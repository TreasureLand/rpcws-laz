unit S7SProvider;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, db, fpjson, jsonparser, RUtils, jwsjson, s7stypes,
  servermethods, messages, s7sConn, S7SDBJsonReader, jwsconsts;

type

 { TS7SProvider }
 TS7SProvider = class
   private
     FDBConnector : TS7SDBConnector;
     S7SQuery     : TS7SQuery;
     S7SQueryAux  : TS7SQuery;
     FKeyField    : string;
     FKeyMaster   : String;
     FSQL         : TStringList;
     FTableName   : String;
     FPacotes     : String;
     FPosicao     : String;
     FJsonParams  : TJSONObject;
     FJsonData    : TJSONObject;
     FMethodType  : TWSMethodType;
     FQueryBase64 : Boolean;
     FJSonLoaded  : Boolean;
     FAutoCommit  : Boolean;
   strict private
     function QueryOpen(ASQL:String):String;
     procedure SetQueryBase64(AValue: Boolean);
     procedure SetSQL(AValue: TStringList);
     procedure JsonParamsToQuery(AS7SQuery:TS7SQuery);
     function GetPacotesPos:String;
     function ApplyUpdates:string;
     function Open:string;
     function Find:String;
   protected
   public
     constructor create(ADBConnector:TS7SDBConnector);
     destructor Destroy; override;
     procedure LoadJson(const AJsonObject:String);
     function Execute(const AJsonObject:String):String;
   published
     property DBConnector    : TS7SDBConnector read FDBConnector    write FDBConnector;
     property TableName      : String          read FTableName      write FTableName;
     property KeyField       : string          read FKeyField       write FKeyField;
     property KeyMaster      : String          read FKeyMaster      write FKeyMaster;
     property SQL            : TStringList     read FSQL            write SetSQL;
     property QueryBase64    : Boolean         read FQueryBase64    write SetQueryBase64;
     property AutoCommit     : Boolean         read FAutoCommit     write FAutoCommit;
 end;

implementation

{ TS7SProvider }
constructor TS7SProvider.create(ADBConnector:TS7SDBConnector);
begin
  if ADBConnector = nil then
    raise Exception.Create('Não definido conector para tabela :'+TableName);
  self.DBConnector:= ADBConnector;
  S7SQuery    := TS7SQuery.create(nil);
  S7SQuery.DBConnector := self.DBConnector;
  S7SQueryAux := TS7SQuery.create(nil);
  S7SQueryAux.DBConnector := self.DBConnector;
  FSQL  := TStringList.Create;
  FJsonParams := TJSONObject.Create;
  FJsonData   := TJSONObject.Create;
  FAutoCommit := True;
end;

destructor TS7SProvider.Destroy;
begin
  FreeAndNil(S7SQuery);
  FreeAndNil(S7SQueryAux);
  FreeAndNil(FSQL);
  FreeAndNil(FJsonParams);
  FreeAndNil(FJsonData);
  inherited;
end;

function TS7SProvider.GetPacotesPos:String;
begin
  if (trim(FPacotes) <> '') and (trim(FPosicao) <> '') then
  begin
    if IsSelect(FSQL.Text) then
    begin
      Result := Self.DBConnector.GetPacotesPos(StrToIntDef(FPosicao,0),StrToIntDef(FPacotes,0));
    end;
  end;
end;

procedure TS7SProvider.LoadJson(const AJsonObject: String);
var vJson  : TJSONObject;
    i,k    : Integer;
    vField : String;
    s      : String;
begin
  FJSonLoaded:= False;
  if (trim(AJsonObject) <> '') then
  begin
    try
      FJsonParams.Clear;
      vJson         := (TJSONParser.Create(AJsonObject).Parse as TJSONObject);
      FMethodType   := wsmNone;
      k :=  vJson.Count;
      For i := pred(k) downto 0 do
      begin
        vField:= uppercase(vJson.Names[i]);
        vField:= TrimLeft(vField);
        vField:= TrimRight(vField);
        Case vField of
          'METHODTYPE' : FMethodType        := StrToMethodType(vJson.Items[i].AsString);
          'SQL'        : FSQL.Text          := StringReplace(vJson.Items[i].AsString,'#LineEnding#',#13#10,[rfReplaceAll, rfIgnoreCase]);
          'PARAMS'     :
          begin
            FJsonParams        := TJsonObject(GetJson(vJson.Items[i].AsJson));
          end;
          'QUERYBASE64': FQueryBase64       := (vJson.Items[i].AsString = 'S');
          'TABLENAME'  : FTableName         := vJson.Items[i].AsString;
          'DATA'       :
          begin
            if vJson.Items[i].JSONType = jtObject then
            begin
              //FJsonData := vJson.Extract(i) as TJSONObject;
              s:= vJson.Find('DATA').AsJSON;
              FJsonData:= TJSONParser.Create(s).Parse as TJSONObject;
            end;
          end;
        end;
      end;
      FJSonLoaded:= True;
    finally
      FreeAndNil(vJson);
    end;
  end;
end;

function TS7SProvider.Execute(const AJsonObject:String):String;
begin
  LoadJson(AJsonObject);
  if FJSonLoaded then
  begin
    case FMethodType of
      wsmtOpen,wsmtExecSQL: Result:= Open;
      wsmtFind: result:= Find;
      wsmtApplyUpdate: result:= ApplyUpdates;
    end;
  end;
end;

function TS7SProvider.ApplyUpdates: string;
var
  d: TS7SDBJsonReader; sjson: TStringList;
begin
  try
    sjson:= TStringList.Create;
    sjson.Add(FJsonData.AsJSON);
    sjson.SaveToFile(TableName+'_ApplyUpdates.txt');
    sjson.Free;
    DBConnector.tra.StartTransaction;
    d := TS7SDBJsonReader.Create(S7SQuery);
    try
      d.Execute(FJsonData, TableName);
    finally
      d.Free;
    end;
    Result:= JSON_RESULT_OK;
    DBConnector.tra.CommitRetaining;
  except
    DBConnector.CancelTrans;
    raise;
  end;
end;

function TS7SProvider.Open: string;
begin
  Result:= QueryOpen(sql.Text);
end;

function TS7SProvider.Find: String;
var ASQL : String; //s : tstr
begin //metodo find usado apenas pare retornar registros
      //se passado aqui um comando que nao seja select ele ira sobrescrever o mesmo
  if (trim(sql.Text) = '') or (not IsSelect(sql.Text)) then
  begin
    if trim(FTableName) = '' then
      raise Exception.Create('Table not defined');
    ASQL:= 'select * from '+FTableName;
    if IsWhere(SQL.Text) then ASQL:= ASQL + sql.Text;
  end
  else ASQL:= sql.Text;
  Result:= QueryOpen(ASQL);
end;

function TS7SProvider.QueryOpen(ASQL: String): String;
var APacotesPos: String;
    ASelect: Boolean;
begin
  try
    ASelect:= IsSelect(ASQL);  //a query ja faz isso
    if ASelect and ((OnlyNumbers(FPosicao) <> '') and (OnlyNumbers(FPacotes) <> '')) then
      APacotesPos:= self.DBConnector.GetPacotesPos(StrToIntDef(FPosicao,0),StrToIntDef(FPacotes,0));
    if FAutoCommit then
      DBConnector.startTrans;
    with S7SQuery do
    begin
      Close;
      SQL.Clear;
      SQL.Add(ASQL);
      if trim(APacotesPos) <> '' then
        SQL.Add(APacotesPos);
    end;
    JsonParamsToQuery(S7SQuery);
    //S7SQuery.SQL.SaveToFile('sql.txt');
    if ASelect then
    begin
      S7SQuery.Open;
      if FQueryBase64 then Result := DatasetToBase64(S7SQuery)
      else Result:= DataSetToJSON(S7SQuery,True,True);
    end
    else
    begin
      S7SQuery.ExecSQL;
      Result:= JSON_RESULT_OK;
    end;
    if FAutoCommit then
      DBConnector.CloseTrans;
  except
    on e: Exception do
    begin
      if FAutoCommit then
      begin
        DBConnector.CancelTrans;
        raise exception.Create(e.Message);
      end
      else raise;
    end;
  end;
end;

procedure TS7SProvider.JsonParamsToQuery(AS7SQuery:TS7SQuery);
var k : Integer;
    P : TParam;
    J : TJSONData;
begin
  for k:= 0 to Pred(FJsonParams.Count) do
  begin
    P := AS7SQuery.Params.Items[k];
    J := FJsonParams.Items[k];
    JsonItemToParam(P,J);
  end;
end;

procedure TS7SProvider.SetSQL(AValue: TStringList);
begin
  if FSQL=AValue then Exit;
    FSQL:=AValue;
end;


procedure TS7SProvider.SetQueryBase64(AValue: Boolean);
begin
  if FQueryBase64=AValue then Exit;
    FQueryBase64:=AValue;
end;


end.

