{*****************************************************************}
{                 JSON RPC Web Service Library                    }
{                                                                 }
{ by Fernando Pasqueto fpasqueto@gmail.com                        }
{                                                                 }
{*****************************************************************}

unit wsmethodsViews;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, jwstypes, fpjson, jsonparser, servermethods, jwsjson, db,
  s7sconn, S7SProvider, RUtils, jwsconsts;

type

  { TJWSMethodsViews }

  TJWSMethodsViews = class(TJWSServerMethods)

  private

  published
    function PesquisaPessoas(Request: TJWSRequestContent): TJSONStringType;
  end;

implementation

{ TJWSMethodsViews }

function TJWSMethodsViews.PesquisaPessoas(Request: TJWSRequestContent): TJSONStringType;
var ADBConnector : TS7SDBConnector;
    AQuery       : TS7SQuery;
begin
  try
    ADBConnector := TS7SDBConnector.create('');
    AQuery:= TS7SQuery.create(nil);
    AQuery.DBConnector := ADBConnector;
    with AQuery do
    begin
      Close;
      sql.Clear;
      sql.Add('');
    end;
  finally
    AQuery.Free;
    AQuery:= nil;
    ADBConnector.Free;
    ADBConnector:= nil;
  end;
end;

end.

