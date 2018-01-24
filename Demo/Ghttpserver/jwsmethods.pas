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
  BufDataSet, sqldb,IBConnection, s7sconn, RUtils;

type

  { TJWSMethods }

  TJWSMethods = class(TJWSServerMethods)

  private

  published

    function Disponivel(Request: TJWSRequestContent): TJSONStringType;
    function QuerySQL(Request: TJWSRequestContent): TJSONStringType;
    function Pessoas(Request: TJWSRequestContent):TJSONStringType;
    function Conta(Request: TJWSRequestContent):TJSONStringType;
  end;

  TPerson = class(TPersistent)
  private
    FId: Int64;
    FName: string;
  published
    property Id: Int64 read FId write FId;
    property Name: string read FName write FName;
  end;
implementation

{ TJWSMethods }
function TJWSMethods.Disponivel(Request: TJWSRequestContent): TJSONStringType;
begin
  Result:= '{"Result" : "OK"}';
end;

function TJWSMethods.Pessoas(Request: TJWSRequestContent):TJSONStringType;
var ADBConnector : TS7SDBConnector;
    AProvider    : TS7SProvider;

begin
  try
    ADBConnector := TS7SDBConnector.create('');
    AProvider    := TS7SProvider.create(ADBConnector);
    AProvider.TableName := 'PESSOAS';
    if Request.Args[0].AsString = '0' then  //chamada feita por uma query e significa que executara o provider
      Result:= AProvider.Execute(Request.Args[1].AsJSON);
  finally
    FreeAndNil(AProvider);
    FreeAndNil(ADBConnector);
  end;
end;

function TJWSMethods.Conta(Request: TJWSRequestContent):TJSONStringType;
var ADBConnector : TS7SDBConnector;
    AProvider    : TS7SProvider;
    sLogQuery    : TStringList;
begin
  try
    ADBConnector := TS7SDBConnector.create('');
    AProvider    := TS7SProvider.create(ADBConnector);
    AProvider.TableName := 'CONTA';
    //if Request.Args[0].AsString = '0' then  //chamada feita por uma query e significa que executara o provider
    sLogQuery    := TStringList.Create;
    sLogQuery.Add(Request.Args[1].AsJSON);
    sLogQuery.SaveToFile('LogConta.txt');
    Result:= AProvider.Execute(Request.Args[1].AsJSON);
    //AProvider.LoadJson(Request.Args[1].AsString);
    AProvider.SQL.Clear;
    //AProvider.Execute('');
  finally
    FreeAndNil(AProvider);
    FreeAndNil(ADBConnector);
    sLogQuery.Free;
  end;
end;

function TJWSMethods.QuerySQL(Request: TJWSRequestContent): TJSONStringType;
var ADBConnector : TS7SDBConnector;
    AProvider    : TS7SProvider;
    //sLogQuery    : TStringList;
begin
  try
    ADBConnector := TS7SDBConnector.create('');
    AProvider    := TS7SProvider.create(ADBConnector);
    //AProvider.TableName := 'pessoas';
    //sLogQuery    := TStringList.Create;
    //sLogQuery.Add(Request.Args[1].AsJSON);
    //sLogQuery.SaveToFile('LogQuerySQL.txt');
    Result:= AProvider.Execute(Request.Args[1].AsJSON);
  finally
    //FreeAndNil(AProvider);
    //FreeAndNil(ADBConnector);
    //sLogQuery.Free;
    AProvider.Free;
    AProvider:= nil;
    ADBConnector.Free;
    ADBConnector:= nil;
  end;
end;

end.

