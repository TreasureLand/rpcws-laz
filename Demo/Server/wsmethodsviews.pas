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
    function PesquisaPessoas(): TJSONStringType;
  end;

  { TJWSMethodsPessoas }

  TJWSMethodsPessoas = class(TJWSServerMethods)
  private
  published
    function Pesquisa(): TJSONStringType;
    function cadastro(): TJSONStringType;
    function delete(): TJSONStringType;
  end;

implementation

{ TJWSMethodsPessoas }

function TJWSMethodsPessoas.Pesquisa: TJSONStringType;
begin
  {
  with Query do
  begin
    close;
    sql.Clear;
    sql.Add('select * from vw_cadpessoa ');
    if AParam <> '*' then
    begin
      if ((not AIsOnlyNumber) and (AItemIndex <> 1)) then  AParam:= '%'+AParam+'%';
      case AItemIndex of
        0:
        begin
          if not AIsOnlyNumber then
          begin
            sql.Add(' where ((pessoa like '+quotedstr(AParam)+')');
            sql.Add(' or (fantasia like '+quotedstr(AParam)+')');
            //sql.Add(' or (cpfcnpj like '+quotedstr(AParam)+')');
            sql.Add(' or (rginscricaoestadual like '+quotedstr(AParam)+')');
            sql.Add(' or (endprinc_cidade like '+quotedstr(AParam)+')');
            sql.Add(' or (upper(endprinc_estado) like '+quotedstr(AParam)+')');
            sql.Add(' or (endprinc_uf like '+quotedstr(AParam)+'))');
          end
          else
          begin
            if trim(ADocumento) = '' then
            begin
              SQL.Add(' where id = :id ');
              ParamByName('id').AsLargeInt:= StrToInt64(AParam);
            end
            else
            begin
              sql.Add(' where cpfcnpj like '+quotedstr(ADocumento));
            end;
          end;
        end;
        1:sql.Add(' where pessoa like '+quotedstr(AParam));
        2:sql.Add(' where fantasia like '+quotedstr(AParam));
        3:sql.Add(' where cpfcnpj like '+quotedstr(ADocumento));
        4:sql.Add(' where rginscricaoestadual like '+quotedstr(AParam));
        5:sql.Add(' where endprinc_cidade like '+quotedstr(AParam));
        6:sql.Add(' where upper(endprinc_estado) like '+quotedstr(AParam));
        7:sql.Add(' where endprinc_uf like '+quotedstr(AParam));
        8:
        begin
          SQL.Add(' where id = :id ');
          ParamByName('id').AsLargeInt:= StrToInt64(AParam);
        end;
      end;
    end;
    //sql.Add(AdcionaOperador(sql.Text,FiltroAtivo));
    //sql.Add(AdcionaOperador(sql.Text,FiltroPessoa));
    //sql.SaveToFile('vw_cadpessoa.sql');
    open;
  end;

  }

  Result := JSON_RESULT_OK;
end;

function TJWSMethodsPessoas.cadastro: TJSONStringType;
begin
  Result:= JSON_RESULT_OK;
end;

function TJWSMethodsPessoas.delete: TJSONStringType;
begin
  Result:= JSON_RESULT_OK;
end;

{ TJWSMethodsViews }

function TJWSMethodsViews.PesquisaPessoas(): TJSONStringType;
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

