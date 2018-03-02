{*****************************************************************}
{                 JSON RPC Web Service Library                    }
{                                                                 }
{ by Jose Benedito - josebenedito@gmail.com                       }
{ www.jbsolucoes.net                                              }
{*****************************************************************}

unit jwsmethods;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, jwstypes, fpjson, jsonparser, servermethods, jwsjson, db,
  BufDataSet, sqldb, IBConnection, s7sconn, S7SProvider, RUtils, jwsconsts, s7stypes;

type

  { TJWSMethods }

  TJWSMethods = class(TJWSServerMethods)

  private

  published
    function Disponivel: TJSONStringType;
    function QuerySQL: TJSONStringType;
    function Transaction: TJSONStringType;
    function getIdTable: TJSONStringType;
  end;

implementation

{ TJWSMethods }
function TJWSMethods.Disponivel: TJSONStringType;
begin
  Result:= '{"Result" : "OK"}';
end;

function TJWSMethods.QuerySQL: TJSONStringType;
var ADBConnector : TS7SDBConnector;
    AProvider    : TS7SProvider;
    //sLogQuery    : TStringList;
begin
  try
    ADBConnector := TS7SDBConnector.create('');
    AProvider    := TS7SProvider.create(ADBConnector);
    Result:= AProvider.Execute(Request.Args[1].AsJSON);
  finally
    AProvider.Free;
    AProvider:= nil;
    ADBConnector.Free;
    ADBConnector:= nil;
  end;
end;

function TJWSMethods.Transaction: TJSONStringType;
var ADBConnector : TS7SDBConnector;
    AProvider    : TS7SProvider;
    AJsonObject    : String;
    vJson          : TJSONObject;
    vJsonCommands  : TJSONObject;
    vResultFind    : String;
    i,k    : Integer;
begin
  try
    try
      ADBConnector := TS7SDBConnector.create('');
      AProvider    := TS7SProvider.create(ADBConnector);
      AProvider.AutoCommit:= False;
      AJsonObject   := Request.Args[1].AsJSON;
      vJson         := (TJSONParser.Create(AJsonObject).Parse as TJSONObject);
      vResultFind   :=  vJson.Find('COMMANDS').AsJSON;
      vJsonCommands := TJSONParser.Create(vResultFind).Parse as TJSONObject;
      k:= vJsonCommands.Count;
      AProvider.DBConnector.startTrans;
      for i := 0 to k-1 do
      begin
        vResultFind := vJsonCommands.Find('COMMAND'+IntToStr(i)).AsJSON;
        Result:= AProvider.Execute(vResultFind);
      end;
      AProvider.DBConnector.CloseTrans;
    except
      on e: Exception do
      begin
        if AProvider.AutoCommit then
          ADBConnector.CancelTrans;
        raise exception.Create(e.Message);
      end;
    end;

  finally
    FreeAndNil(AProvider);
    FreeAndNil(ADBConnector);
    vJsonCommands.Free;
    vJson.Free;
  end;
end;

function TJWSMethods.getIdTable: TJSONStringType;
var AQuery: TS7SQuery;
    AUltimo_id:Integer;
    ADtultimo_id : TDateTime;
    ADBConnector : TS7SDBConnector;
    ATabela : String;
    ACampo : String;
    AData: string;
begin   //2018-01-31 17:44:10
  try
    ATabela:= Request.Args[0].AsString;
    ACampo:= Request.Args[1].AsString;
    ADBConnector := TS7SDBConnector.create('');
    if ADBConnector.DBProtocol = dbMySql then
    begin
      ShortDateFormat:='yyyy-mm-dd';
    end;
    AData:=DateTimeToStr(Now);
    ADtultimo_id := Now;//StrToDateTime(AData);
    AQuery:= TS7SQuery.create(nil);
    AQuery.DBConnector := ADBConnector;
    ADBConnector.startTrans;
    with AQuery do
    begin
      SQL.Clear;
      SQL.Add('select ultimo_id from cfgsequencia_unica ');
      SQL.Add('where tabela = '+quotedstr(ATabela));
      SQL.Add('and campo = '+QuotedStr(ACampo));
      open;
      if EOF then
      begin
        Close;
        SQL.Clear;
        SQL.Add('insert into cfgsequencia_unica ');
        SQL.Add('(tabela,campo,ultimo_id,dtultimo_id)');
        SQL.Add('values');
        SQL.Add('(:tabela,:campo,:ultimo_id,:dtultimo_id)');
        ParamByName('tabela').AsString:= ATabela;
        ParamByName('campo').AsString:= ACampo;
        ParamByName('ultimo_id').AsInteger:=1;
        ParamByName('dtultimo_id').AsDateTime:= ADtultimo_id;
        ExecSQL;
        AUltimo_id:= 1;
      end
      else
      begin
        AUltimo_id:= FieldByName('ultimo_id').AsLargeInt + 1;
        Close;
        SQL.Clear;
        SQL.Add('update cfgsequencia_unica set ');
        SQL.Add('ultimo_id = :ultimo_id');
        SQL.Add(',dtultimo_id = :dtultimo_id');
        SQL.Add('where tabela = :tabela');
        SQL.Add('and campo = '+QuotedStr(ACampo));
        ParamByName('ultimo_id').AsLargeInt:= AUltimo_id;
        ParamByName('dtultimo_id').AsDateTime:= ADtultimo_id;
        ParamByName('tabela').AsString:= ATabela;
        ExecSQL;
      end;
    end;
    Result:= '{"id":'+IntToStr(AUltimo_id)+'}';
    AQuery.Close;
    AQuery.Destroy;
    ADBConnector.CloseTrans;
  except
    on e: exception do
    begin
      ADBConnector.CancelTrans;
      Exception.Create('Erro ao gegar id: '+e.Message);
    end;
  end;
end;

end.

