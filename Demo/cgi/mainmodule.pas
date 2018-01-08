{*****************************************************************}
{                                                                 }
{ by Jose Benedito - josebenedito@gmail.com                       }
{ www.jbsolucoes.net                                              }
{*****************************************************************}

unit mainmodule;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, jwsconsts, jwsmessages, jwstypes, httpdefs, fpHTTP, fpWeb,
  fpjson, jsonparser, jwsmethods, IBConnection, sqldb;

type

  { TMainWebModule }

  TMainWebModule = class(TFPWebModule)
    procedure MainWebActionRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  MainWebModule: TMainWebModule;
  ReqMethods: TJWSMethods;

implementation

{$R *.lfm}

procedure ParseRequest(var ARequest: TJWSRequestContent;
  AJSONString: TJSONStringType);
var
  parser: TJSONParser;
begin
  parser := TJSONParser.Create(AJSONString);
  try
    try
      ARequest := TJWSRequestContent(parser.Parse as TJSONObject);

    except on e:exception do
      begin
        ARequest := nil;
        raise exception.create('ParseRequest '+ e.message);
      end;
    end;
  finally
    parser.free;
    parser := nil;
  end;
end;


procedure SetHeader(var AResponse: TResponse);
begin
  AResponse.CustomHeaders.Clear;
  AResponse.SetCustomHeader('Access-Control-Allow-Origin','*');
  AResponse.SetCustomHeader('Access-Control-Allow-Credentials','true');
  AResponse.CustomHeaders.Add(JSON_HEADER_CONTENT_TYPE);
end;



procedure OnJWSRequest(Sender: TObject; const ARequest: TJWSRequestContent; var AResponse: TJWSResponseContent);
begin
  try
    ReqMethods := TJWSMethods.Create;
    ReqMethods.ProcessRequest(ARequest, AResponse);
  finally
    freeandnil(ReqMethods);
  end;
end;

{ TMainWebModule }

procedure TMainWebModule.MainWebActionRequest(Sender: TObject;
  ARequest: TRequest; AResponse: TResponse; var Handled: Boolean);
var
  jwsRequest: TJWSRequestContent;
  jwsResponse: TJWSResponseContent;
  i, iID: integer;
begin
  Handled := true;
  jwsRequest   := nil;
  jwsResponse  := nil;
  if ARequest.Method = 'GET' then
  begin
    AResponse.Content := MSG_GETRESPONSE;
    exit;
  end;
  try
    try

      addlog('Request header count: '+ inttostr(ARequest.FieldCount));
      for i := 0 to ARequest.FieldCount -1 do
        addlog(ARequest.FieldValues[i] + ': '+ ARequest.FieldValues[i]);
      addlog(ARequest.Content);

      ParseRequest(jwsRequest, ARequest.Content);
      if jwsRequest <> nil then iID := jwsRequest.ID
      else iID := -1;
      OnJWSRequest(Sender,jwsRequest,jwsResponse);
      if not assigned(jwsResponse) then
        jwsResponse := JSONRPCResult(TJSONString.create(MSG_NO_RESPONSE),jwsRequest.ID);
    except on e:exception do
        jwsResponse := JSONRPCError(ERR_REQUEST_ERROR,ERROR_REQUEST_ERROR + '('+ e.message + ')',iID);
    end;
  finally
    AResponse.content := jwsResponse.AsJSON;
    jwsRequest.free;
    jwsRequest := nil;
    jwsResponse.free;
    jwsResponse := nil;
  end;
end;

initialization
  RegisterHTTPModule('TMainWebModule', TMainWebModule);
end.

