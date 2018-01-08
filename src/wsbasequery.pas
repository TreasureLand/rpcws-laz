unit WSBaseQuery;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, db, fpjson,
  jsonparser, BufDataset, sqltypes, s7stypes, WSConnector, RUtils, jwsjson,
  BinaryDatapacketReaderEx, DBJsonWriter, helper.stream;


type

  TApplyUpdateJSON = TDBJsonWriter;

  { TWSBaseQuery }

  TWSBaseQuery = class(TBufDataset)
  private
    { Private declarations }
    FMethod              : String;
    FMethodType          : TWSMethodType;
    FWSConnetor          : TWSConnector;
    FPacotes             : Integer;
    FDadosEmPacotes      : Boolean;
    FReturnFields        : String;
    FSQL                 : TStringList;
    FParams              : TParams;
    FFieldNameQuoteChars : TQuoteChars;
    //FWiterEvent          : TApplyRecUpdateEvent;
    FApplyJSON           :  TApplyUpdateJSON;
    FQueryBase64         : Boolean;
    procedure SetSQL(AValue: TStringList);
    procedure SetWSConnetor(AValue: TWSConnector);
    function SQLToJson(const POpen:Boolean = True; {%H-}PDadosEmPacotes:Boolean = True): String;
    function ParamsToJson: String;
    function IsSelect(ASQL: string): boolean;
    function IsWhere(AClause:String): Boolean;
    Function DoExecSQL: Boolean;
    procedure DoOpen;
    procedure CallToDataset(ADataset:TCustomBufDataset; POpen:Boolean = True);
    procedure CopyFromDataset(DataSet: TDataSet; CopyData: Boolean; CopyFields:Boolean);
  private
    FOpening: Boolean;
  protected
    { Protected declarations }
    procedure SetActive(Value: Boolean); override;
    procedure ApplyRecUpdate(UpdateKind: TUpdateKind); override;
  public
    { Public declarations }
    property FieldNameQuoteChars:TQuoteChars  read FFieldNameQuoteChars write FFieldNameQuoteChars;
    constructor Create(AOwner : TComponent); override;
    Destructor destroy; override;
    Function ParamByname(Const AParamName : String) : TParam;
    procedure ApplyUpdates(MaxErrors: Integer); override; overload;

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
    published
    property Method           :string       Read FMethod         write FMethod;
  end;

  { TWSClientMethods }
  TWSClientMethods=Class(TComponent)
    private
      FMethod      : String;
      FQueryBase64 : Boolean;
    protected
    public
      Connector : TWSConnector;
      constructor Create(AOwner : TComponent); override;
      Destructor destroy; override;
      Function StrCall(const AMethod: String; Args: array of variant):String;
      Function JSONCall(const AMethod: String; Args: array of variant):TJSONObject;
      procedure DatasetCall(AMethod,AFieldName,ATableName:string;ADataset:TDataset;AQueryBase64:Boolean);
    published
      property Method           :string      Read FMethod         write FMethod;
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
  FReturnFields   := '1'; //1 = retorna fields, 0 = nao retorna fields
  FSQL            := TStringList.Create;
  FParams         := TParams.create;
end;

destructor TWSBaseQuery.destroy;
begin
  FreeAndNil(FSQL);
  FreeAndNil(FParams);
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
    Result:= '{"SQL":"'+Trim(FSQL.Text)+'"';
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
    Result:= Result+',"METHODTYPE":"'+getMethodTypeToStr(FMethodType)+'"';
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
          vJson.add(p.name,p.asstring);
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
  FApplyJSON := TApplyUpdateJSON.Create();
  try
    ManualMergeChangeLog:=True;
    inherited ApplyUpdates(MaxErrors);
    if FApplyJSON.&Object <> nil then
    begin
      TFileStream.WriteTo(ClassName+'_ApplyUpdates.txt', FApplyJSON.&Object.AsJSON);
      aJson := '{"METHODTYPE":"'+getMethodTypeToStr(wsmtApplyUpdate)+'"';
      aJson := aJson+',"DATA":'+FApplyJSON.&Object.AsJSON+'}';
      aLogJson      := TStringList.Create;
      aLogJson.Clear;
      aLogJson.Add(aJson);
      aLogJson.SaveToFile('LogJson.txt');
      aLogJson.Free;
      aJson :=  FWSConnetor.StrCall(FMethod,['0',aJson,FReturnFields],0);
      aJson := FWSConnetor.CheckResult(aJson);
      ADataItem := GetJSON(aJson);
      aJson := ADataItem.AsString;
      if aJson <> 'OK' then
        raise Exception.CreateFmt('Erro retornado do servidor: %s', [aJson]);
    end;
    MergeChangeLog;
  finally
    FApplyJSON.Free;
  end;
end;


function TWSBaseQuery.ParamByname(const AParamName: String): TParam;
begin
  if FParams.FindParam(AparamName) = Nil then
  begin
    With TParam.create(fParams) do
    begin
      Name := AParamName;
      ParamType:= ptInput;
      Result := FParams.FindParam(AparamName);
    end;
  end
  Else Result := FParams.FindParam(AparamName);
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
  try
    FMethodType:= wsmtExecSQL;
    vJson      := SQLToJson(True);
    vJresult   := FWSConnetor.JsonCall(FMethod,['0',vJson,FReturnFields],0);
    vVDataItem := vJResult.Items[0];
    Result     := (vVDataItem.AsString = 'OK');
  finally
    FreeAndNil(vJresult);
  end;
end;

procedure TWSBaseQuery.DoOpen;
begin
  CallToDataset(self,True);
end;

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

procedure TWSBaseQuery.CopyFromDataset(DataSet: TDataSet; CopyData: Boolean; CopyFields:Boolean);
Const UseStreams = ftBlobTypes;
Var
  I  : Integer;
  F,F1,F2 : TField;
  L1,L2  : TList;
  N : String;
  OriginalPosition: TBookMark;
  S : TMemoryStream;
begin
  if CopyFields or (Active = false) then
  begin
    Close;
    Fields.Clear;
    FieldDefs.Clear;
    For I:=0 to Dataset.FieldCount-1 do
    begin
      F:=Dataset.Fields[I];
      TFieldDef.Create(FieldDefs,F.FieldName,F.DataType,F.Size,F.Required,F.FieldNo);
    end;
    CreateDataset;
    Open;
  end;
  L1:=Nil;
  L2:=Nil;
  S:=Nil;
  If CopyData then
  begin
    try
      L1:=TList.Create;
      L2:=TList.Create;
      For I:=0 to FieldDefs.Count-1 do
      begin
        N:=FieldDefs[I].Name;
        F1:=FieldByName(N);
        F2:=DataSet.FieldByName(N);
        L1.Add(F1);
        L2.Add(F2);
        If (FieldDefs[I].DataType in UseStreams) and (S=Nil) then
          S:=TMemoryStream.Create;
      end;
      DisableControls;
      Dataset.DisableControls;
      OriginalPosition:=Dataset.GetBookmark;
      Try
        Dataset.Open;
        Dataset.First;
        While not Dataset.EOF do
        begin
          Append;
          For I:=0 to L1.Count-1 do
          begin
            F1:=TField(L1[i]);
            F2:=TField(L2[I]);
            If Not F2.IsNull then
            begin
              Case F1.DataType of
                 ftFixedChar,
                 ftString   : F1.AsString:=F2.AsString;
                 ftFixedWideChar,
                 ftWideString : F1.AsWideString:=F2.AsWideString;
                 ftBoolean  : F1.AsBoolean:=F2.AsBoolean;
                 ftFloat    : F1.AsFloat:=F2.AsFloat;
                 ftAutoInc,
                 ftLargeInt : F1.AsInteger:=F2.AsInteger;
                 ftSmallInt : F1.AsInteger:=F2.AsInteger;
                 ftInteger  : F1.AsInteger:=F2.AsInteger;
                 ftDate     : F1.AsDateTime:=F2.AsDateTime;
                 ftTime     : F1.AsDateTime:=F2.AsDateTime;
                 ftTimestamp,
                 ftDateTime : F1.AsDateTime:=F2.AsDateTime;
                 ftCurrency : F1.AsCurrency:=F2.AsCurrency;
                 ftBCD,
                 ftFmtBCD   : F1.AsBCD:=F2.AsBCD;
              else
              if (F1.DataType in UseStreams) then
              begin
                S.Clear;
                TBlobField(F2).SaveToStream(S);
                S.Position:=0;
                TBlobField(F1).LoadFromStream(S);
              end
              else F1.AsString:=F2.AsString;
            end;
            end;
          end;
          Try
            Post;
          except
            Cancel;
            Raise;
          end;
          Dataset.Next;
          end;
      Finally
        DataSet.GotoBookmark(OriginalPosition); //Return to original record
        Dataset.EnableControls;
        EnableControls;
      end;
    finally
      L2.Free;
      l1.Free;
      S.Free;
    end;
  end;
end;

procedure TWSBaseQuery.SetActive(Value: Boolean);
begin
  //ShowMessage('TWSBaseQuery.SetActive '+BoolToStr(Value, True));
  //writeln('TWSBaseQuery.SetActive ', Value, ' Opening: ', FOpening);
  if not Value or FOpening or Active then
     Inherited
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
  if trim(Sql.Text) = '' then
    raise Exception.Create('SQL não pode ser vazio');
  if IsSelect(sql.Text) then
    raise Exception.Create('SQL não pode ser select para ExecSQL');
  result:= DoExecSQL;
end;

{ TWSClientDataset }

constructor TWSClientDataset.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FMethodType:= wsmtFind;
end;

Destructor TWSClientDataset.destroy;
begin
  inherited destroy;
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

Function TWSClientDataset.ExecSQL:Boolean;
begin
  if (trim(FMethod) = '') then
    raise Exception.Create('Method não pode ser vazio');
  if trim(FSQL.Text) = '' then
    raise Exception.Create('ComandText não pode ser vazio');
  if IsSelect(FSQL.Text) then
    raise Exception.Create('ComandText não pode ser select para ExecSQL');
  Result:= DoExecSQL;
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

Function TWSClientMethods.StrCall(const AMethod: String; Args: array of variant):String;
begin
  Result:= Connector.StrCall(AMethod,Args,0);
end;

Function TWSClientMethods.JSONCall(const AMethod: String; Args: array of variant):TJSONObject;
begin
  Result:= GetJSONObject(Connector.StrCall(AMethod,Args,0), True);
  //Result:= (TJSONParser.Create(Connector.StrCall(AMethod,Args,0)).Parse as TJSONObject);
end;

procedure TWSClientMethods.DatasetCall(AMethod,ATableName,AFieldName:string;ADataset:TDataset;AQueryBase64:Boolean);
var vjson    : TJSONObject;
    AJson    : String;
begin
  if AQueryBase64 and (not (ADataset is TCustomBufDataset)) then
    AQueryBase64:= False;
  AJson:= '{"TABLE":"'+Trim(ATableName)+'"';
  AJson:= AJson + ',"FIELDNAME":"'+Trim(AFieldName)+'"';
  if AQueryBase64 then
    AJson:= AJson + ',"QUERYBASE64":"S"';
  AJson:= AJson+',"METHODTYPE":"'+getMethodTypeToStr(wsmtFind)+'"';
  AJson:= AJson+'}';
  vJSon := Connector.JSONCall('QuerySql', ['0',AJson,true],0);
  try
    if AQueryBase64 then
      Base64ToDataset(TCustomBufDataset(ADataset),vjson)
    else
    JSONToDataset(ADataset,vjson,true);
  finally
    FreeAndNil(vjson);
  end;
end;

procedure TWSClientMethods.DatasetCall(AMethod,ATableName,AFieldName:string;ADataset:TDataset;AQueryBase64:Boolean);
var vjson    : TJSONObject;
    AJson    : String;
begin
  if AQueryBase64 and (not (ADataset is TCustomBufDataset)) then
    AQueryBase64:= False;
  AJson:= '{"TABLE":"'+Trim(ATableName)+'"';
  AJson:= AJson + ',"FIELDNAME":"'+Trim(AFieldName)+'"';
  if AQueryBase64 then
    AJson:= AJson + ',"QUERYBASE64":"S"';
  AJson:= AJson+',"METHODTYPE":"'+getMethodTypeToStr(wsmtFind)+'"';
  AJson:= AJson+'}';
  vJSon := Connector.JSONCall('QuerySql', ['0',AJson,true],0);
  try
    if AQueryBase64 then
      Base64ToDataset(TCustomBufDataset(ADataset),vjson)
    else
    JSONToDataset(ADataset,vjson,true);
  finally
    FreeAndNil(vjson);
  end;
end;

end.
