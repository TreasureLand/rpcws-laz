unit s7sConn;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, db, fpjson, jsonparser, RUtils, jwsjson, s7stypes,
  servermethods, messages, mysql50,mysql51,mysql57conn,mysql55conn,mysql56conn;
type
  TPathConfDB  = Record
      Ip        : String;
      porta     : String;
      Usuario   : String;
      Senha     : String;
      BaseDados : String;
      Protocolo : String;
      Pacotes   : String;
      CharSet   : String;
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
      property DBConnector : TS7SDBConnector read FDBConnector write SetDBConnector;
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
          CharSet     := Lista.Values['CHARSET'];
          Pacotes     := OnlyNumbers(Lista.Values['PACOTES']); //retorna somente numeros
        end;
      finally
        Lista.free;
      end;
    end;
    conn               := TSQLConnector.Create(nil);
    tra                := TSQLTransaction.Create(nil);
    tra.Action         := caCommitRetaining;
    conn.CharSet       := Self.ConfDB.CharSet;
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


end.
