{*****************************************************************}
{                 JSON RPC Web Service Library                    }
{                                                                 }
{ by Fernando Pasqueto fpasqueto@gmail.com                        }
{                                                                 }
{*****************************************************************}

unit wsmethods;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, jwstypes, fpjson, jsonparser, servermethods, jwsjson, db,
  s7sconn, S7SProvider, RUtils, jwsconsts;

type

  { TWSMethods }

  TWSMethods = class(TJWSServerMethods)

  private

  published
    function getIdTable(Request: TJWSRequestContent): TJSONStringType;
  end;

implementation

{ TWSMethods }

function TWSMethods.getIdTable(Request: TJWSRequestContent): TJSONStringType;
var AQuery: TS7SQuery;
    AUltimo_id:Integer;
    ADtultimo_id : TDateTime;
    ADBConnector : TS7SDBConnector;
    ATabela : String;
    ACampo : String;
begin
  try
    ADtultimo_id := StrToDateTime(DateToStr(date)+' '+TimeToStr(Time));
    ADBConnector := TS7SDBConnector.create('');
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
        SQL.Add('('+QuotedStr(ATabela)+','+QuotedStr(ACampo)+','+IntToStr(1)+','+quotedstr(DateTimeToStr(ADtultimo_id))+')');
        ExecSQL;
        AUltimo_id:= 1;
      end
      else
      begin
        AUltimo_id:= FieldByName('ultimo_id').AsLargeInt + 1;
        Close;
        SQL.Clear;
        SQL.Add('update cfgsequencia_unica set ');
        SQL.Add('ultimo_id = '+IntToStr(AUltimo_id));
        SQL.Add(',dtultimo_id = '+QuotedStr(DateTimeToStr(ADtultimo_id)));
        SQL.Add('where tabela = '+QuotedStr(ATabela));
        SQL.Add('and campo = '+QuotedStr(ACampo));
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

