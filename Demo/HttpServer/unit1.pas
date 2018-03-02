unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, httpdefs, fpHTTP, fpWeb, jwsconsts, jwsmessages, jwstypes,
  fpjson, jsonparser, jwsmethods, IBConnection, sqldb, strutils;

type

  { TFPWebModule1 }

  TFPWebModule1 = class(TFPWebModule)
    procedure acaoRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure FPWebActionRequestRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FPWebModule1: TFPWebModule1;
  ReqMethods: TJWSMethods;

implementation

{$R *.lfm}

{ TFPWebModule1 }

procedure ParseRequest(var ARequest: TJWSRequestContent;
  AJSONString: TJSONStringType);
var
  parser: TJSONParser;
  lResult: TStringList;
begin
  lResult:= TStringList.Create;
  parser := TJSONParser.Create(AJSONString);
  try
    lResult.Add(AJSONString);
    lResult.SaveToFile('ParseRequest.txt');
    try
      ARequest := TJWSRequestContent(parser.Parse as TJSONObject);
    except on e:exception do
      begin
        ARequest := nil;
        raise exception.create('ParseRequest '+ e.message);
      end;
    end;
  finally
    lResult.Free;
    lResult:= nil;
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
    ReqMethods := TJWSMethods.Create();
    ReqMethods.ProcessRequest(ARequest, AResponse);
  finally
    freeandnil(ReqMethods);
  end;
end;

procedure TFPWebModule1.acaoRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
begin
  AResponse.ContentType:='text/html';
  AResponse.SendRedirect('/files/teste.html');
  Handled:=True;
end;

procedure TFPWebModule1.FPWebActionRequestRequest(Sender: TObject;
  ARequest: TRequest; AResponse: TResponse; var Handled: Boolean);
var
  jwsRequest: TJWSRequestContent;
  jwsResponse: TJWSResponseContent;
  i, iID: integer;
begin

  WriteLn(Format('    Cliente "%s" Data/Hora: %s  Requisicao: %s', [ARequest.RemoteAddr,formatdatetime('dd/mm/yyyy hh:mm:ss:zzz',now),ARequest.QueryFields.Values['QUERY']]));
  WriteLn('');
  WriteLn('');
  Handled := true;
  jwsRequest   := nil;
  jwsResponse  := nil;

  if ARequest.Method = 'GET' then
  begin
    addlog('Get');
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
  WriteLn(Format('    Cliente "%s" Data/Hora da Resposta: %s ', [ARequest.RemoteAddr,formatdatetime('dd/mm/yyyy hh:mm:ss:zzz',now)]));
  WriteLn('');
  WriteLn('');

end;
{
initialization
  RegisterHTTPModule('TFPWebModule1', TFPWebModule1);
  }
end.

