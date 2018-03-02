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
  Classes, SysUtils, jwstypes, fpjson, jsonparser, servermethods, jwsjson,
  s7sconn, S7SProvider, RUtils, jwsconsts;

type

  { TJWSMethods }

  TJWSMethods = class(TJWSServerMethods)

  private

  published

    function Disponivel(): TJSONStringType;
    function QuerySQL(): TJSONStringType;
    function Transaction(): TJSONStringType;
  end;

implementation

{ TJWSMethods }

function TJWSMethods.Disponivel(): TJSONStringType;
begin
  Result:= '{"Result" : "OK"}';
end;

function TJWSMethods.QuerySQL(): TJSONStringType;
var ADBConnector : TS7SDBConnector;
    AProvider    : TS7SProvider;
begin
  try
    ADBConnector := TS7SDBConnector.create('');
    AProvider    := TS7SProvider.create(ADBConnector);
    Result       := AProvider.Execute(Request.Args[1].AsJSON);
  finally
    AProvider.Free;
    AProvider:= nil;
    ADBConnector.Free;
    ADBConnector:= nil;
  end;
end;

function TJWSMethods.Transaction(): TJSONStringType;
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

end.

