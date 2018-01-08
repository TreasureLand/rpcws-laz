unit WSConnector;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, jsonscanner, LResources, Forms,
  Controls, Graphics, Dialogs, jwsclient, RUtils;

type      

  { TPathConfSRV }

  { TWSServer }

  TWSServer = class(TPersistent)
  private
    FHostName : String;
    FPort     : String;
    FPathCGI  : String;
    procedure SetHostName(AValue: String);
    procedure SetPathCGI(AValue: String);
    procedure SetPort(AValue: String);
  public
    procedure Assign(Source: TPersistent); override;
    //constructor Create;
  published
    property HostName       : String read FHostName write SetHostName;
    property Port           : String read FPort write SetPort;
    property PathCGI        : String read FPathCGI write SetPathCGI;
  end;

  { TWSConnector }

  TWSConnector = class(TComponent)
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;//tem q ter o override
  private
    FWSServer     : TWSServer;
    FJWSClient    : TJWSClient;
    FConnected    : Boolean;
    FHost         : String;
    FResponseTime : String;
    procedure SetConnected(AValue : Boolean);
  protected
    procedure Loaded; override;
  public
  private
    Function DoConnect:Boolean;
    procedure SetResponseTime(AValue: String);
    procedure SetWSServer(AValue: TWSServer);
    function CalcResponseTime(AMilliseconds: LongWord): String;
  public
    function CheckResult(AJresult: TJSONObject): TJSONData; overload;
    function CheckResult(const AJresult: String): string; overload;
  published
    Property Connected    : Boolean      read FConnected    write SetConnected;
    property WSServer     : TWSServer    read FWSServer     write SetWSServer;
  public
    Function StrCall(AMethod: string; Args: array of variant; AID: integer): string;
    Function JSONCall(AMethod: string; Args: array of variant; AID: integer): TJSONObject;
    function GetPacotes:Integer;
    property JWSClient    : TJWSClient   read FJWSClient;
    property ResponseTime : String read FResponseTime write SetResponseTime;
  end;

procedure Register;

implementation

procedure Register;
begin
  //{$I wsconector_icon.lrs}
  RegisterComponents('RPCWS',[TWSConnector]);
end;

{ TWSServer }

procedure TWSServer.SetHostName(AValue: String);
begin
  if FHostName=AValue then Exit;
  FHostName:=AValue;
end;

procedure TWSServer.SetPathCGI(AValue: String);
begin
  if FPathCGI=AValue then Exit;
  FPathCGI:=AValue;
end;

procedure TWSServer.SetPort(AValue: String);
begin
  if FPort=AValue then Exit;
  FPort:=AValue;
end;

procedure TWSServer.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if (Source is TWSServer) then
  begin
    FHostName := TWSServer(Source).HostName;
    Fport     := TWSServer(Source).Port;
    FPathCGI  := TWSServer(Source).PathCGI;
  end;
end;

{ TWSConector }

constructor TWSConnector.Create(aOwner : TComponent);
begin
  inherited Create(aOwner);
  FWSServer   := TWSServer.Create;
  FJWSClient  := TJWSClient.Create;//(self);
end;

destructor TWSConnector.Destroy;
begin
  FWSServer.Free;
  FJWSClient.Free;
  inherited Destroy;
end;

procedure TWSConnector.SetConnected(AValue : Boolean);
begin
  if FConnected <> AValue then
  begin
    if not AValue then
      FConnected := False
    else if csLoading in ComponentState then
      FConnected := True
    else
      FConnected := DoConnect;
  end;
end;

procedure TWSConnector.Loaded;
begin
  inherited Loaded;
  if FConnected then
     Connected:=DoConnect;
end;

function TWSConnector.DoConnect: Boolean;
var  vResult     : TJSONObject;
     vVDataItem  : TJSONData;
begin
  vResult  := JSONCall('Disponivel',[],0);
  try
    vVDataItem := vResult.Items[0];
    Result := (vVDataItem.AsString = 'OK');
  finally
    vResult.Free;
    vResult:= nil;
  end;
end;

procedure TWSConnector.SetResponseTime(AValue: String);
begin
  if FResponseTime=AValue then Exit;
  FResponseTime:=AValue;
end;

function TWSConnector.GetPacotes:Integer;
var ANumeros:String;
begin
  //ANumeros:= OnlyNumbers(ConfSRV.Pacotes);
  Result:= StrToIntDef(ANumeros,0);
end;

procedure TWSConnector.SetWSServer(AValue: TWSServer);
begin
  FWSServer.Assign(AValue);
end;

function TWSConnector.CheckResult(AJresult: TJSONObject): TJSONData;
begin
  Result := AJresult.Find('result');
  if Result <> nil then
    Exit;
  Result := AJresult.Find('error');
  if not (Result is TJSONObject) then
    raise Exception.Create('Resposta desconhecida: ' + sLineBreak + AJresult.AsJSON);
  with TJSONObject(Result) do
    raise Exception.CreateFmt('Erro: %d - %s', [Integers['code'], Strings['message']]);
end;

function TWSConnector.CheckResult(const AJresult: String): string;
var o: TJSONData;
begin
  o := GetJSON(AJresult);
  try
    Result := CheckResult(o as TJSONObject).AsJSON;
  finally
    o.Free;
  end;
end;

function TWSConnector.StrCall(AMethod: string; Args: array of variant;
  AID: integer): string;
  Var TimeIni, TimeFim : LongWord;
begin

  TimeIni := GetTickCount;
  if WSServer.FPathCGI <> '' then FHost:= 'http://'+WSServer.HostName+':'+WSServer.FPort+WSServer.FPathCGI
  else FHost:= 'http://'+WSServer.HostName+':'+WSServer.FPort;
  JWSClient.Host := FHost;
  Result         := JWSClient.Call(AMethod,Args,AID);
  Result         := CheckResult(Result);
  TimeFim        := GetTickCount;
  FResponseTime  := CalcResponseTime(TimeFim - TimeIni);
end;

function TWSConnector.JSONCall(AMethod: string; Args: array of variant;
  AID: integer): TJSONObject;
var
  s: String;
begin
  //Result := (TJSONParser.Create(StrCall(AMethod,Args,AID), [joUTF8]).Parse as TJSONObject); //aki tem uma instancia q não é destruida
  s := StrCall(AMethod,Args,AID);
  with TJSONParser.Create(s, [joUTF8]) do
  try
    Result := Parse as TJSONObject;
  finally
    Free;
  end;
end;

function TWSConnector.CalcResponseTime(AMilliseconds: LongWord): String;
var Mil, Sec, Min : Integer;
begin
  Sec    := AMilliseconds div 1000;
  Mil    := AMilliseconds mod 1000;
  Min    := Sec div 60;
  Sec    := Sec mod 60;
  Result := IntToStr(Min)+':'+ IntTostr(Sec)+':'+IntToStr(Mil)+'ms';
end;

end.
