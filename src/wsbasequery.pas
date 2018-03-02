unit WSBaseQuery;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, db, fpjson,
  jsonparser, BufDataset, sqltypes, s7stypes, WSConnector, RUtils, jwsjson,jwsconsts,
  BinaryDatapacketReaderEx, s7sdbJsonWriter, helper.stream, jsonscanner;


type

  TApplyUpdateJSON = TS7SDBJsonWriter;

  { TWSBaseQuery }

  TWSBaseQuery = class(TBufDataset)
  private
    { Private declarations }
    FMethod              : String;
    FTableName           : String;
    FMethodType          : TWSMethodType;
    FWSConnetor          : TWSConnector;
    FPacotes             : Integer;
    FDadosEmPacotes      : Boolean;
    FReturnFields        : String;
    FSQL                 : TStringList;
    FParams              : TParams;
    FParamsValid         : Boolean;
    FFieldNameQuoteChars : TQuoteChars;
    FApplyJSON           : TApplyUpdateJSON;
    FQueryBase64         : Boolean;
    FGetData             : Boolean;
    FMergeChangeLog      : Boolean;
    FDataName            : String;
    FData                : String;
    procedure SetSQL(AValue: TStringList);
    procedure SetWSConnetor(AValue: TWSConnector);
    procedure SQLChange(Sender: TObject);
    function SQLToJson(const POpen:Boolean = True; {%H-}PDadosEmPacotes:Boolean = True): String;
    function ParamsToJson: String;
    function IsSelect(ASQL: string): boolean;
    function IsWhere(AClause:String): Boolean;
    //procedure DoOpen;
    procedure CallToDataset(ADataset:TCustomBufDataset; POpen:Boolean = True);
  private
    FOpening: Boolean;
    procedure ApplyUpdates; override; overload;
    procedure ApplyUpdates(MaxErrors: Integer); override; overload;
    Function GetDataStr(const ADataName:String;const AMergeChangeLog:Boolean = True):String; virtual;
  protected
    { Protected declarations }
    procedure SetActive(Value: Boolean); override;
    procedure ApplyRecUpdate(UpdateKind: TUpdateKind); override;
  public
    { Public declarations }
    Function DoExecSQL: Boolean;
    property FieldNameQuoteChars:TQuoteChars  read FFieldNameQuoteChars write FFieldNameQuoteChars;
    constructor Create(AOwner : TComponent); override;
    Destructor destroy; override;
    Function ParamByname(Const AParamName : String) : TParam;

  published
    { Published declarations }
    property DadosEmPacotes     : Boolean      read FDadosEmPacotes      write FDadosEmPacotes;
    property Pacotes            : Integer      read FPacotes             write FPacotes;
    property params             : TParams      read FParams              Write Fparams;
    property QueryBase64        : Boolean      read FQueryBase64         write FQueryBase64;
    property WSConnetor         : TWSConnector read FWSConnetor          write SetWSConnetor;
    property SQL                : TStringList  read FSQL                 write SetSQL;
  end;

  { EWSBaseQuery }

  EWSBaseQuery = class(EDatabaseError)
  public
    constructor Create(aException: TObject);  overload;
  end;

  { TWSQuery }
  TWSQuery = Class(TWSBaseQuery)
    Private
    protected
    Public
      constructor Create(AOwner : TComponent); override;
      Destructor destroy; override;
      //procedure Open;
      Function ExecSQL:Boolean;
    Published
  end;

  { TWSClientDataset }
  TWSClientDataset = class(TWSBaseQuery)
    private
      //FF
    protected

    public
      constructor Create(AOwner : TComponent); override;
      Destructor destroy; override;
      //procedure Open;
      Function ExecSQL:Boolean;
      procedure ApplyUpdates; override; overload;
      procedure ApplyUpdates(MaxErrors: Integer); override; overload;
      Function GetDataStr(const ADataName:String;const AMergeChangeLog:Boolean = True):String; override;
    published
      property TableName        :String       read FTableName      write FTableName;
      //property Method           :string       Read FMethod         write FMethod;
  end;

  { TWSClientMethods }
  TWSClientMethods=Class(TComponent)
    private
      //FHTTPModule  : String;
      FMethodModule: String;
      FMethodName  : String;
      FQueryBase64 : Boolean;
    protected
    public
      Connector : TWSConnector;
      constructor Create(AOwner : TComponent); override;
      Destructor destroy; override;
      Function StrCall(Args: array of variant):String;
      Function JSONCall(Args: array of variant):TJSONObject;
      procedure DatasetCall(ADataset:TDataset;Args: array of variant);
    published
      //property HTTPModule       :string      read FHTTPModule     write FHTTPModule;
      property MethodModule     :string      Read FMethodModule   write FMethodModule;
      property MethodName       :string      Read FMethodName     write FMethodName;
      property Querybase64      :Boolean     Read FQueryBase64    write FQueryBase64;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RPCWS',[TWSClientDataset,TWSQuery]);
end;

{ TWSBaseQuery }

constructor TWSBaseQuery.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  Pacotes         := 0;
  FDadosEmPacotes := False;
  FQueryBase64    := True;
  FReturnFields     := '1'; //1 = retorna fields, 0 = nao retorna fields
  FSQL              := TStringList.Create;
  FSQL.OnChange:=@SQLChange;
  FParams           := TParams.create;
end;

destructor TWSBaseQuery.destroy;
begin
  //Self.Close;
  FSQL.Free;
  FParams.Free;
  inherited Destroy;
end;

function TWSBaseQuery.SQLToJson(const POpen:Boolean = True; PDadosEmPacotes:Boolean = True): String;
var i : Integer;
    AParam : String;
    APosicao: LongInt;
    APacotes: LongInt;
begin
  try
    if (trim(FMethod) = '') then
      raise Exception.Create('Method não pode ser vazio');
    for i := 0 to Pred(params.Count) do
    begin  //valida parametros informados na query
      if pos(':'+params.Items[i].Name,FSQL.Text) = 0 then
        raise Exception.Create('Parâmetro: '+params.Items[i].Name+' '+ ' não informado na SQL')
    end;
    Result:= '{"METHODTYPE":"'+getMethodTypeToStr(FMethodType)+'"';
    if trim(Sql.Text) <> '' then
      Result:= Result + ',"SQL":"'+Trim(FSQL.Text)+'"';
    if trim(FTableName)<> '' then
      Result:= Result + ',"TABLENAME":"'+Trim(FTableName)+'"';
    if FDadosEmPacotes then //se nao for dados em pacotes nao faz sentindo entrar aqui
    begin
      if FPacotes > 0 then APacotes:= FPacotes //busca pacotes definido pelo usuario na query
      else APacotes:= FWSConnetor.GetPacotes; //busca tamanho dos pacotes definido no conector
      if POpen then APosicao:= 0
      else APosicao:= self.RecordCount;
      Result:= Result+',"Pacotes":"'+IntToStr(APacotes)+'"';
      Result:= Result+',"Posicao":"'+IntToStr(APosicao)+'"';
    end;
    if FQueryBase64 then
      Result:= Result + ',"QUERYBASE64":"S"';
    AParam:= ParamsToJson;
    if AParam <> '{}' then
      Result:= Result+',"Params":'+AParam;
    Result:= Result+'}';
    Result:= StringReplace(Result,#13#10,'#LineEnding#',[rfReplaceAll, rfIgnoreCase]);
  finally
  end;
end;

procedure TWSBaseQuery.SetWSConnetor(AValue: TWSConnector);
begin
  if FWSConnetor=AValue then Exit;
  FWSConnetor:=AValue;
end;

procedure TWSBaseQuery.SQLChange(Sender: TObject);
begin
  FParamsValid:=False;
  //params.Clear;
end;

procedure TWSBaseQuery.SetSQL(AValue: TStringList);
begin
  //if FSQL=AValue then Exit;
  //FSQL:=AValue;
  if AValue <> nil then
    FSQL.Assign(AValue);
end;

function TWSBaseQuery.ParamsToJson: String;
Var vJson   : TJSONObject;
    p: TParam;
begin
  Try
    vJson := TJSONObject.create;
    for p in FParams do
    begin
      if p.IsNull then
      begin
        vJson.add(p.name,TJSONNull.Create);
        Continue;
      end;
      Case p.DataType of
        ftSmallint, ftInteger, ftWord,ftAutoInc, ftLargeint:
        begin
          vJson.add(p.name,p.AsInteger);
        end;
        ftBoolean:  vJson.add(p.name,p.asboolean);
        ftFloat,ftCurrency,ftBCD,ftFMTBcd:
        begin
          vJson.add(p.name,p.asfloat);
        end;
        ftDate,ftDateTime,ftTimeStamp,ftTime:
        begin
          if trim(p.asstring) <> '' then
            vJson.add(p.name,p.asstring)
          else vJson.add(p.name,'#nulldate#'); //else vJson.add(p.name,jtNull);
        end;
        ftBlob,ftMemo,ftFmtMemo,ftWideMemo:
        begin
          if Trim(p.AsString) = '' then
            vJson.add(p.name, p.AsString)
          else
            vJson.add(p.name, '#base64#' +StrToBase64(p.AsString));
        end;
        else
          vJson.add(p.name,Trim(p.AsString));
     end;
    end;
    Result := vJson.AsJSON;
  finally
    vJson.free;
    //jNull.Free;
  end;
end;

procedure TWSBaseQuery.ApplyRecUpdate(UpdateKind: TUpdateKind);
begin
  try
    FApplyJSON.ApplyRecUpdate(Self, UpdateKind);
  except
    on e : EDatabaseError do
      raise
    else
      raise EWSBaseQuery.Create(ExceptObject); //do contrário ele não entende q é um erro de DB;
  end;
end;

procedure TWSBaseQuery.ApplyUpdates(MaxErrors: Integer);
var
  aJson: String;
  ADataItem : TJSONData;
  aLogJson : TStringList;
begin
  if trim(FTableName) = '' then
    raise Exception.Create('TableName não pode ser vazio');
  if Self.ChangeCount = 0 then Exit;
  FApplyJSON := TApplyUpdateJSON.Create();
  try
    ManualMergeChangeLog:=True;
    inherited ApplyUpdates(MaxErrors);
    if (FApplyJSON.&Object <> nil) then
    begin
      TFileStream.WriteTo(ClassName+'_ApplyUpdates.txt', FApplyJSON.&Object.AsJSON);
      if FGetData then
        FData := '{"DATANAME":"' + Trim(FDataName) + '"'
      else FData := '{"METHODTYPE":"'+getMethodTypeToStr(wsmtApplyUpdate)+'"';
      FData := FData+',{"TABLENAME":"'+Trim(FTableName)+'"';
      FData := FData+',"DATA":'+FApplyJSON.&Object.AsJSON+'}';

      aLogJson := TStringList.Create;
      aLogJson.Clear;
      aLogJson.Add(FData);
      aLogJson.SaveToFile('LogJson.txt');
      aLogJson.Free;

      if not FGetData then
      begin
        if FWSConnetor.AutoCommit then
        begin
          aJson :=  FWSConnetor.StrCall('', FMethod,['0',FData,FReturnFields],0);
          aJson := FWSConnetor.CheckResult(aJson);
        end
        else
        begin
          FWSConnetor.WSTransaction.AddListCmd:= FData;
          aJson:= JSON_RESULT_OK;
        end;
        ADataItem := GetJSON(aJson);
        aJson := ADataItem.AsString;
        if aJson <> 'OK' then
          raise Exception.CreateFmt('Erro retornado do servidor: %s', [aJson]);
      end;
    end;
    if FMergeChangeLog then
      MergeChangeLog;
  finally
    FApplyJSON.Free;
  end;
end;

function TWSBaseQuery.GetDataStr(const ADataName: String;
  const AMergeChangeLog: Boolean): String;
begin
  FGetData:= True;
  FMergeChangeLog:=AMergeChangeLog;
  if trim(ADataName) = '' then
    FDataName:=Self.Name
  else FDataName:=ADataName;
  Self.ApplyUpdates(-1);
  Result:= FData;
end;

function TWSBaseQuery.ParamByname(const AParamName: String): TParam;
begin
  if not FParamsValid then
  begin
    params.ParseSQL(SQL.Text, True);
    FParamsValid:=True;
  end;
  Result := Params.ParamByName(AParamName);
  //Result := FParams.FindParam(AparamName);
  //if Result = Nil then
  //begin
  //  Result := TParam.create(fParams);
  //  with Result do
  //  begin
  //    Name     := AParamName;
  //    ParamType:= ptInput;
  //    //Result   := FParams.FindParam(AparamName);
  //  end;
  //end
  //Else Result := FParams.FindParam(AparamName);
end;

function TWSBaseQuery.IsSelect(ASQL: string): boolean;
begin
  result := (Pos('SELECT',UpperCase(Copy(ASQL,1,6))) > 0);
end;

function TWSBaseQuery.IsWhere(AClause:String): Boolean;
begin
  Result := (Pos('WHERE',UpperCase(Copy(AClause,1,5))) > 0);
end;

function TWSBaseQuery.DoExecSQL: Boolean;
var vJson      : String;
    vJresult   : TJSONObject;
    vVDataItem : TJSONData;
begin
  if trim(Sql.Text) = '' then
    raise Exception.Create('SQL não pode ser vazio');
  if IsSelect(sql.Text) then
    raise Exception.Create('SQL não pode ser select para ExecSQL');
  try
    FMethodType:= wsmtExecSQL;
    vJson      := SQLToJson(True);
    //vJresult   := FWSConnetor.JsonCall(FMethod,['0',vJson,FReturnFields],0);
    //vVDataItem := vJResult.Items[0];
    if FWSConnetor.AutoCommit then
    begin
      vJresult   := FWSConnetor.JsonCall('', FMethod,['0',vJson,FReturnFields],0);
    end
    else
    begin
      FWSConnetor.WSTransaction.AddListCmd:=vJson;
      with TJSONParser.Create(JSON_RESULT_OK, [joUTF8]) do
      try
        vJresult := Parse as TJSONObject;
      finally
        Free;
      end;
    end;
    vVDataItem := vJResult.Items[0];
    Result     := (vVDataItem.AsString = 'OK');
  finally
    FreeAndNil(vJresult);
  end;
end;
{
procedure TWSBaseQuery.DoOpen;
begin
  CallToDataset(self,True);
end;
}

procedure TWSBaseQuery.CallToDataset(ADataset:TCustomBufDataset; POpen:Boolean = True);
var vjson    : TJSONObject;
    AJson    : String;
begin
  AJson := SQLToJson(POpen,FDadosEmPacotes);
 vJSon := FWSConnetor.JSONCall(FMethod, ['0',AJson,FReturnFields],0);
  try
    //Base64ToDataset(ADataset,vjson);
    if FQueryBase64 then Base64ToDataset(ADataset,vjson)
    else JSONToDataset(ADataset,vjson,true);
  finally
    FreeAndNil(vjson);
  end;
end;

procedure TWSBaseQuery.ApplyUpdates;
begin
  ApplyUpdates(0);
end;

procedure TWSBaseQuery.SetActive(Value: Boolean);
var aFieldDefsCount,aFieldsCount:integer;
begin
  //ShowMessage('TWSBaseQuery.SetActive '+BoolToStr(Value, True));
  //writeln('TWSBaseQuery.SetActive ', Value, ' Opening: ', FOpening);
  //if csDestroying in ComponentState then
  //   Exit;
  if not Value or FOpening or Active then
  begin
    Inherited;
  end
  else
  begin
    FOpening:= True;
    try
       CallToDataset(self,True);
    finally
      FOpening:=False;
    end;
  end;
end;

constructor EWSBaseQuery.Create(aException : TObject);
begin
  if aException = nil then
    inherited Create('')
  else if aException is Exception then
    inherited Create(Exception(aException).Message)
  else
    inherited Create(aException.ClassName);
end;

{ TWSQuery }
constructor TWSQuery.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  Pacotes         := 0;
  FDadosEmPacotes := True;
  FMethod         := 'QuerySQL';
  FMethodType     := wsmtOpen;
end;

destructor TWSQuery.destroy;
begin
  inherited Destroy;
end;


function TWSQuery.ExecSQL:Boolean;
begin
  result:= Self.DoExecSQL;
end;

{ TWSClientDataset }

constructor TWSClientDataset.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FMethodType:= wsmtFind;
  FMethod    := 'QuerySQL';
end;

destructor TWSClientDataset.destroy;
begin
  inherited destroy;
end;

procedure TWSClientDataset.ApplyUpdates(MaxErrors: Integer);
begin
  FGetData:= False;
  FMergeChangeLog:= True;
  inherited ApplyUpdates(MaxErrors);
end;

function TWSClientDataset.GetDataStr(const ADataName: String;
  const AMergeChangeLog: Boolean): String;
begin
  Inherited GetDataStr(ADataName,AMergeChangeLog);
end;

{
procedure TWSClientDataset.Open;
begin
  //if trim(CommandText.Text) = '' then
  //  raise Exception.Create('ComandText não pode ser vazio');
  if (trim(FMethod) = '') then
    raise Exception.Create('Method não pode ser vazio');
  FReOpen:= False;
  FMethodType:= wsmtFind;
  DoOpen;
end;
}

function TWSClientDataset.ExecSQL: Boolean;
begin
  result:= DoExecSQL;
end;

procedure TWSClientDataset.ApplyUpdates;
begin
  inherited ApplyUpdates;
end;

{ TWSClientMethods }

constructor TWSClientMethods.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
end;

destructor TWSClientMethods.destroy;
begin
  inherited Destroy;
end;

//Function TWSClientMethods.CallApplyUpdates(Args: array of variant):String;

Function TWSClientMethods.StrCall(Args: array of variant):String;
begin
  Result:= Connector.StrCall(FMethodModule, FMethodName,Args,0);
end;

Function TWSClientMethods.JSONCall(Args: array of variant):TJSONObject;
begin
  Result:= GetJSONObject(Connector.StrCall(FMethodModule,FMethodName,Args,0), True);
  //Result:= (TJSONParser.Create(Connector.StrCall(AMethod,Args,0)).Parse as TJSONObject);
end;

procedure TWSClientMethods.DatasetCall(ADataset:TDataset;Args: array of variant);
var vjson       : TJSONObject;
begin
  vJSon := JSONCall(Args); //Connector.JSONCall(FMethodModule, FMethodName, ['0',AJson,true],0);
  try
    if FQueryBase64 then
      Base64ToDataset(TCustomBufDataset(ADataset),vjson)
    else
    JSONToDataset(ADataset,vjson,true);
  finally
    FreeAndNil(vjson);
  end;
end;

end.
