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
  Classes, SysUtils, fgl, variants, strutils, jwstypes, fpjson, jsonparser,
  Helper.Exceptions;

type

  TFuncType = function(): TJSONStringType of object;

  { TJWSServerMethods }

  TJWSServerMethods = class
  private
    FRequest: TJWSRequestContent;
    procedure Call(var Response: TJWSResponseContent);
  public
    constructor Create(aRequest: TJWSRequestContent);
    destructor Destroy; override;
  public
    property Request: TJWSRequestContent read FRequest;
    procedure ProcessRequest(var Response: TJWSResponseContent);
  published

  end;

  TJWSServerMethodsClass = class of TJWSServerMethods;

  { TJWSServerMethodsModules }

  TJWSServerMethodsModules = class
  public
    constructor Create;
    destructor Destroy; override;
  private
    type
      TMapModules = specialize TFPGMap<string, TJWSServerMethodsClass>;
  private
    class var FModules: TMapModules;
  public
    class procedure RegisterClassModule(const aName: string; aClassModule: TJWSServerMethodsClass);overload;
    class procedure RegisterClassModule(aClassModule: TJWSServerMethodsClass); overload;
    class function GetClassModule(const aName: string): TJWSServerMethodsClass; overload;
    class function GetClassMethod(ARequest: TJWSRequestContent): TJWSServerMethodsClass; overload;
  end;

  function IsSelect(ASQL: string): Boolean;
  function IsWhere(ASQL: string): Boolean;

implementation

uses
  jwsjson, jwsconsts, jwsmessages;

{ TJWSServerMethods }

constructor TJWSServerMethods.Create(aRequest: TJWSRequestContent);
begin
  inherited Create;
  FRequest := aRequest;
end;

destructor TJWSServerMethods.Destroy;
begin
  inherited Destroy;
end;

procedure TJWSServerMethods.Call(//Request: TJWSRequestContent;
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
      sResult := TFuncType(m)();//(Request);
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

procedure TJWSServerMethods.ProcessRequest({ARequest: TJWSRequestContent; }var Response: TJWSResponseContent);
begin
  try
    if Request <> nil then
      Call(Response)
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

{ TJWSServerMethodsModules }

constructor TJWSServerMethodsModules.Create;
begin
   inherited Create;
   FModules           := TMapModules.Create;
   FModules.Sorted    := True;
   FModules.Duplicates:=dupError;
end;

destructor TJWSServerMethodsModules.Destroy;
begin
  FModules.Free;
  inherited Destroy;
end;

class procedure TJWSServerMethodsModules.RegisterClassModule(const aName: string;
  aClassModule: TJWSServerMethodsClass);
begin
  try
     FModules.Add(aName.ToUpper(), aClassModule);
  except
    raise EExceptExcept.CreateFmt('Erro registrando módulo(%s), Nome: %s, Classe: %s',
                        [ClassName, aName, aClassModule.ClassName]);
  end;
end;

class procedure TJWSServerMethodsModules.RegisterClassModule(aClassModule: TJWSServerMethodsClass);
var
  s: String;
begin
  s := aClassModule.ClassName;
  if AnsiStartsText('TJWSServerMethods', s) then
     Delete(s, 1, 17);

  RegisterClassModule(s, aClassModule);
end;

class function TJWSServerMethodsModules.GetClassModule(const aName: string): TJWSServerMethodsClass;
begin
  if not FModules.TryGetData(aName.UpperCase(), Result) then
     Result := nil;
end;

class function TJWSServerMethodsModules.GetClassMethod(ARequest: TJWSRequestContent): TJWSServerMethodsClass;
begin
  Result := GetClassModule(ARequest.Module);
end;

end.


