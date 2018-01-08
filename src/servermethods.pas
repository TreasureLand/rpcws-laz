{*****************************************************************}
{                 JSON RPC Web Service Library                    }
{                                                                 }
{ by Jose Benedito - josebenedito@gmail.com                       }
{ www.jbsolucoes.net                                              }
{*****************************************************************}

unit servermethods;

{$mode objfpc}{$H+}

interface
{$M+}

uses
  Classes, SysUtils, variants, jwstypes, fpjson, jsonparser;

type

  TFuncType = function(Request: TJWSRequestContent): TJSONStringType of object;

  { TJWSServerMethods }

  TJWSServerMethods = class
  private
    procedure Call(Request: TJWSRequestContent; var Response: TJWSResponseContent);
  public
    constructor Create;
    destructor Destroy; override;
    procedure ProcessRequest(ARequest: TJWSRequestContent; var Response: TJWSResponseContent);
  published

  end;

  function IsSelect(ASQL: string): Boolean;
  function IsWhere(ASQL: string): Boolean;

implementation

uses
  jwsjson, jwsconsts, jwsmessages;

{ TJWSServerMethods }

constructor TJWSServerMethods.Create;
begin

end;

destructor TJWSServerMethods.Destroy;
begin
  inherited Destroy;
end;

procedure TJWSServerMethods.Call(Request: TJWSRequestContent;
    var Response: TJWSResponseContent);
var
  m: TMethod;
  sResult: string;
  parser: tjsonparser;
begin
  try
    m.Code := Self.MethodAddress(Request.Method); //find method code
    if assigned(m.Code) then
    begin
      m.Data := pointer(Self); //store pointer to object instance
      sResult := TFuncType(m)(Request);
      //addlog(sResult);
      if IsJSON(sResult) then
      begin
        //addlog('result: '+ sResult);
        parser := TJSONParser.Create(sResult);
        try
          Response := JSONRPCResult(parser.parse,Request.ID);
        finally
          parser.free;
          parser := nil;
        end;
      end
      else
      begin
        //addlog('result '+ sResult);
        Response := JSONRPCResult(TJSONString.create(sResult),Request.ID);
      end;
    end
    else
    begin
      if assigned(Response) then
      begin
        Response.free;
        Response := nil;
      end;
      //addlog('call '+ sResult);
      Response := JSONRPCError(ERR_UNKNOW_FUNCTION, ERROR_UNKNOW_FUNCTION);
    end;
  except
    on e: Exception do
    begin
      if assigned(Response) then
      begin
        Response.free;
        Response := nil;
      end;
      Response := JSONRPCError(ERR_INTERNAL_ERROR, e.message, Request.ID);
    end;
  end;
end;

procedure TJWSServerMethods.ProcessRequest(ARequest: TJWSRequestContent; var Response: TJWSResponseContent);
begin
  try
    if ARequest <> nil then
      Call(ARequest, Response)
    else
    begin
      if assigned(Response) then
      begin
        Response.free;
        Response := nil;
      end;
      Response := JSONRPCError(ERR_INVALID_CONTENT, ERROR_INVALID_CONTENT, -1);
    end;
  except
    on e: Exception do
    begin
      if assigned(Response) then
      begin
        Response.free;
        Response := nil;
      end;
      Response := JSONRPCError(ERR_INTERNAL_ERROR, ERROR_INTERNAL_ERROR, -1);
    end;
  end;
end;


function IsSelect(ASQL: string): boolean;
begin
  result := (pos('SELECT',UpperCase(ASQL)) > 0);
end;

function IsWhere(ASQL: string): boolean;
begin
  result := (pos('WHERE',UpperCase(ASQL)) > 0);
end;

end.

