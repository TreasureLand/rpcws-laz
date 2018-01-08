{*****************************************************************}
{                 JSON RPC Web Service Library                    }
{                                                                 }
{ by Jose Benedito - josebenedito@gmail.com                       }
{ www.jbsolucoes.net                                              }
{*****************************************************************}

unit jwsclient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics,
  fpjson, jsonparser, fphttpclient, jwstypes;

type

  { TJWSClient }

  TJWSClient = class(TPersistent)
  private
    FHost: string;
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
    //constructor Create;//(AOwner: TComponent); override;
    //destructor Destroy; override;

    function Call(AMethod: string; Args: array of variant; AID: integer): string; overload;
    function Call(ARequest: TJWSRequestContent): string; overload;
    function JSONCall(AMethod: string; Args: array of variant; AID: integer): TJSONObject;
    function JSONCall(ARequest: TJWSRequestContent): TJSONObject; overload;
  published
    property Host: string read FHost write FHost;
    { Published declarations }
  end;


implementation

uses jwsconsts;

{ TJWSClient }

function TJWSClient.Call(AMethod: string; Args: array of variant; AID: integer): string;
var
  response: TMemoryStream;
  joContent: TJSONObject;
  joArgs: TJSONArray;
  slRes: TStringList;
  httpcli: TFPHTTPClient;
begin
  joArgs := nil;
  try
    httpcli := TFPHTTPClient.Create(nil);//Create(self);
    httpcli.IOTimeout:=500000;
    slRes    := TStringList.Create;
    response := TMemoryStream.Create;
    joContent := TJSONObject.Create;
    //joArgs := TJSONArray.create;
    joContent.Add('jsonrpc', JSONRPC_VERSION);
    joContent.Add('method', AMethod);
    joArgs := GetJSONArray(Args);
    joContent.Add('params', joArgs);
    joContent.Add('id',AID);
    slRes.Add(joContent.FormatJSON);
    slRes.SaveToFile('JsonSaida.txt');
    httpcli.FormPost(Host,joContent.AsJSON, response);

    if (response <> nil) then
    begin
      response.Seek(0, soFromBeginning);

      slRes.LoadFromStream(response);

      Result := slRes.text;
      slRes.SaveToFile('JsonEntrada.txt');
    end;
  finally
    freeandnil(joContent);
    freeandnil(response);
    freeandnil(slRes);
    freeandnil(httpcli);
    //freeandnil(joArgs);
  end;
end;

function TJWSClient.Call(ARequest: TJWSRequestContent): string;
var
  response: TMemoryStream;
  slRes: TStringList;
  httpcli: TFPHTTPClient;
begin
  try
    httpcli := TFPHTTPClient.Create(nil);//(self);

    slRes    := TStringList.Create;
    response := TMemoryStream.Create;

    httpcli.FormPost(Host,ARequest.AsJSON, response);

    if (response <> nil) then
    begin
      response.Seek(0, soFromBeginning);

      slRes.LoadFromStream(response);

      Result := slRes.text;
    end;
  finally
    freeandnil(response);
    freeandnil(slRes);
    freeandnil(httpcli);
  end;

end;

function TJWSClient.JSONCall(AMethod: string; Args: array of variant; AID: integer): TJSONObject;
begin
  Result := (TJSONParser.Create(Call(AMethod,Args,AID)).Parse as TJSONObject); //
end;

function TJWSClient.JSONCall(ARequest: TJWSRequestContent): TJSONObject;
begin
  Result := (TJSONParser.Create(Call(ARequest)).Parse as TJSONObject);
end;



end.
