unit s7sConn;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, db, fpjson, jsonparser, RUtils, jwsjson, s7stypes,
  servermethods, DBJsonReader, uDBExecutorSQLdb, messages;
type

  TPathConfDB  = Record
      Ip        : String;
      porta     : String;
      Usuario   : String;
      Senha     : String;
      BaseDados : String;
      Protocolo : String;
      Pacotes   : String;
  end;

  { TS7QDBConnector }
  TS7SDBConnector = class(TPersistent)
    private
    protected
    public
      ConfDB     : TPathConfDB;
      conn       : TSQLConnector;
      tra        : TSQLTransaction;
      DBProtocol : TDBProtocol;
      Pacotes    : Integer;
      constructor create(APathConfDB:String);
      destructor Destroy; override;
      function GetPacotesPos(PPosicao,PPacotes:Integer):String;
      procedure startTrans;
      procedure CloseTrans;
      procedure CancelTrans;
    published
  end;

  { TS7SQuery }
  TS7SQuery = class(TSQLQuery)
    private
      FDBConnector: TS7SDBConnector;
      procedure SetDBConnector(AValue: TS7SDBConnector);

    protected
    public
      constructor create(AOwner : TComponent);
      destructor destroy; override;
    published
      property DBConector : TS7SDBConnector read FDBConnector write SetDBConnector;
  end;

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
     function FactoryQuery(const aSQL: string): TSQLExecutor;
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
     property QueryBase64    : Boolean read FQueryBase64 write SetQueryBase64;
 end;


implementation

{ TDBConector }
constructor TS7SDBConnector.Create(APathConfDB:String);
var Lista : TStrings;
begin
  inherited Create;
  try
    if (trim(APathConfDB) = '') then APathConfDB:= 'PathConfDB.txt';
    If FileExists(APathConfDB) = False then
    begin
      Raise Exception.Create('Não ha como continuar sem o arquivo de configuração: '+APathConfDB);
    end
    Else
    Begin
      Try
        Lista := TStringList.Create;
        Lista.LoadFromFile(APathConfDB);
        With ConfDB do
        begin
          IP          := Lista.Values['IP'];
          Protocolo   := Lista.Values['PROTOCOLO'];
          BaseDados   := Lista.Values['BASEDADOS'];
          Usuario     := Lista.Values['USUARIO'];
          Senha       := Lista.Values['SENHA'];
          Pacotes     := OnlyNumbers(Lista.Values['PACOTES']); //retorna somente numeros
        end;
      finally
        Lista.free;
      end;
    end;
    conn               := TSQLConnector.Create(nil);
    tra                := TSQLTransaction.Create(nil);
    tra.Action         := caCommitRetaining;
    conn.ConnectorType := self.ConfDB.Protocolo;
    conn.HostName      := self.ConfDB.Ip;
    conn.DatabaseName  := self.ConfDB.BaseDados;
    conn.UserName      := self.ConfDB.Usuario;
    conn.Password      := self.ConfDB.Senha;
    conn.Transaction   := tra;
    IF ConfDB.Pacotes <> '' THEN
      Pacotes:= StrToInt(ConfDB.Pacotes)
    else Pacotes:= 10; //se nao achar nada valor default sera 10
    case UpperCase(copy(self.ConfDB.Protocolo,1,5)) of
      'POSTG' : DBProtocol := dbPostGresql;
      'FIREB' : DBProtocol := dbFirebird;
      'MYSQL' : DBProtocol := dbMySql;
      'SQLIT' : DBProtocol := dbSqLite;
    end;
  except
    raise;
  end;
end;

function TS7SDBConnector.GetPacotesPos(PPosicao,PPacotes:Integer):String;
var APosiscao :Integer;
    APacotes  :Integer;
begin
  if (PPacotes > 0) then
    APacotes:= PPacotes //tamanho de pacotes passados pela query
  else if not (PPacotes > 0) and (self.Pacotes > 0) then
    APacotes:= self.Pacotes //pacotes dos parametros do servidor
  else APacotes:= cstPacotes; //pacotes da costante padrao

  if not (PPosicao > 0) then
  begin //quando = 0 siginifica que esta iniciando a query do lado cliente
        //recordcount significa a posicao da query do lado cliente
    case Self.DBProtocol of
      dbFirebird,dbMySql,dbSqLite: APosiscao:= 1;
      dbPostGresql:APosiscao:= 0;
    end;
  end
  else APosiscao:= PPosicao + APacotes;

  case Self.DBProtocol of
    dbFirebird: Result:= ' rows ' +inttostr(APosiscao)+' to '+
                                   inttostr(APosiscao+APacotes-1);
       dbMySql: Result:= ' limit '+IntToStr(APosiscao)+' , '+
                                   IntToStr(APacotes);
    dbPostGresql,
      dbSqLite: Result:= ' limit '+IntToStr(APacotes)+' offset '+
                                   inttostr(APosiscao);
  end;
end;

destructor TS7SDBConnector.Destroy;
begin
  FreeAndNil(self.tra);
  FreeAndNil(self.conn);
  inherited;
end;

procedure TS7SDBConnector.StartTrans;
begin
  if not tra.Active then
  begin
    tra.Action := caCommit;
    tra.StartTransaction;
  end;
end;

procedure TS7SDBConnector.CloseTrans;
begin
  if tra.Active then
  begin
    tra.Action := caCommit;
    tra.Commit;
  end;
end;

procedure TS7SDBConnector.CancelTrans;
begin
  if tra.Active then
  begin
    tra.Action := caRollback;
    tra.Rollback;
  end;
end;

{ TS7SQuery }
procedure TS7SQuery.SetDBConnector(AValue: TS7SDBConnector);
begin
  if FDBConnector=AValue then Exit;
  FDBConnector:=AValue;
  Self.DataBase    := FDBConnector.conn;
  Self.Transaction := FDBConnector.tra;
end;

constructor TS7SQuery.create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TS7SQuery.destroy;
begin
  inherited Destroy;
end;

{ TS7SProvider }
constructor TS7SProvider.create(ADBConnector:TS7SDBConnector);
begin
  if ADBConnector = nil then
    raise Exception.Create('Não definido conector para tabela :'+TableName);
  self.DBConnector:= ADBConnector;
  S7SQuery    := TS7SQuery.create(nil);
  S7SQuery.SetDBConnector(self.DBConnector);
  S7SQueryAux := TS7SQuery.create(nil);
  S7SQueryAux.SetDBConnector(self.DBConnector);
  FSQL  := TStringList.Create;
  FJsonParams := TJSONObject.Create;
  FJsonData   := TJSONObject.Create;
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
    try    //010820176234335 vivo
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
          'PARAMS'     : FJsonParams        := TJsonObject(GetJson(vJson.Items[i].AsJson));
          'QUERYBASE64': FQueryBase64       := (vJson.Items[i].AsString = 'S');
          'TABLE'      : FTableName         := vJson.Items[i].AsString;
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
  d: TDBJsonReader; sjson: TStringList;
begin
  //try
    sjson:= TStringList.Create;
    sjson.Add(FJsonData.AsJSON);
    sjson.SaveToFile('ApplyUpdates.txt');
    sjson.Free;
    DBConnector.tra.StartTransaction;
    d := TDBJsonReader.Create(@FactoryQuery);
    try
      d.Execute(FJsonData, TableName);
    finally
      d.Free;
    end;
    Result:= '{"result":"OK"}';
    DBConnector.tra.CommitRetaining;
  {
  except
    on e: Exception do
    begin
      DBConnector.CancelTrans;
      raise exception.Create(e.Message);
    end;
  end;
  }
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

function TS7SProvider.FactoryQuery(const aSQL: string): TSQLExecutor;
begin
  Result := uDBExecutorSQLdb.TSQLExecutorSQLdb.Create(S7SQuery.SQLConnection,
         S7SQuery.SQLTransaction, aSQL);
end;

function TS7SProvider.QueryOpen(ASQL: String): String;
var APacotesPos: String;
    ASelect: Boolean;
begin
  try
    ASelect:= IsSelect(ASQL);  //a query ja faz isso
    if ASelect and ((OnlyNumbers(FPosicao) <> '') and (OnlyNumbers(FPacotes) <> '')) then
      APacotesPos:= self.DBConnector.GetPacotesPos(StrToIntDef(FPosicao,0),StrToIntDef(FPacotes,0));
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
      Result:= '{"result":"OK"}';
    end;
    DBConnector.CloseTrans;
  except
    on e: Exception do
    begin
      DBConnector.CancelTrans;
      raise exception.Create(e.Message);
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
