unit principal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  ExtCtrls, Menus, StdCtrls, ActnList, httpdefs, fpHTTP, fpWeb, fpwebfile,
  fphttpapp, jwsconsts, jwsmessages, jwstypes, WSConnector, fpjson, jsonparser,
  jwsmethods, strutils,fphttpserver
  {$IFDEF UNIX}{$IFDEF UseCThreads}
    ,cthreads
  {$ENDIF}{$ENDIF};

type

  { TFrmPrincipal }

  { TThred }

  TThred = Class(tThread)
  private
    //app: THTTPApplication;
    app: TFPHTTPServer;
  public
    constructor Create(CreateSuspended: Boolean; const StackSize: SizeUInt=DefaultStackSize
      );
    destructor Destroy; override;
    procedure DoTerminate; override;
   protected
     procedure Execute; override;
     procedure DoHandleRequest(Sender: TObject;
                               var ARequest: TFPHTTPConnectionRequest;
                               var AResponse: TFPHTTPConnectionResponse);
  end;

  TFrmPrincipal = class(TForm)
    actAbrir: TAction;
    actFechar: TAction;
    actParar: TAction;
    actIniciar: TAction;
    ActionList1: TActionList;
    bIniciar: TSpeedButton;
    bparar: TSpeedButton;
    MIniciar: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MLog: TMemo;
    PopupMenu1: TPopupMenu;
    Timer1: TTimer;
    TrayIcon1: TTrayIcon;
    procedure actAbrirExecute(Sender: TObject);
    procedure actFecharExecute(Sender: TObject);
    procedure actIniciarExecute(Sender: TObject);
    procedure actPararExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

  { TFPWebModule1 }

  TFPWebM = class(TFPWebModule)
    acao : TFpWebAction;
    FPWebActionRequest : TFpWebAction;
    procedure FPWebActionRequestRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure testeRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
  private
    { private declarations }
  public
    { public declarations }
    constructor createnew(AOwner:TComponent;createmode:Integer); override;
    destructor Destroy; override;
  end;

var
  FrmPrincipal: TFrmPrincipal;
  ReqMethods: TJWSMethods;
  aThred: TThred;
  aFPWebM: TFPWebM;
implementation

{$R *.lfm}

{ TFPWebM }

procedure TFPWebM.testeRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
begin
   aresponse.content := 'acao executada';
   handled := true
end;

constructor TFPWebM.createnew(AOwner: TComponent; createmode: Integer);
begin
  Inherited;
  acao := TFpWebAction.create(actions);
  Acao.name:= 'acao';
  FPWebActionRequest:= TFpWebAction.create(actions);
  FPWebActionRequest.Name:='FPWebActionRequest';
  FPWebActionRequest.Default:=True;
  FPWebActionRequest.OnRequest:= @FPWebActionRequestRequest;
  //Acao.Default:=True;
  acao.OnRequest:=@testeRequest;
end;

destructor TFPWebM.Destroy;
begin
  FreeAndNil(acao);
  inherited Destroy;
end;


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

procedure TFPWebM.FPWebActionRequestRequest(Sender: TObject;
  ARequest: TRequest; AResponse: TResponse; var Handled: Boolean);
var
  jwsRequest: TJWSRequestContent;
  jwsResponse: TJWSResponseContent;
  i, iID: integer;
begin

  FrmPrincipal.MLog.Lines.Add(Format('Cliente "%s" Data/Hora: %s  Requisicao: %s', [ARequest.RemoteAddr,formatdatetime('dd/mm/yyyy hh:mm:ss:zzz',now),ARequest.QueryFields.Values['QUERY']]));
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
  FrmPrincipal.MLog.Lines.Add(Format('Cliente "%s" Data/Hora da Resposta: %s ', [ARequest.RemoteAddr,formatdatetime('dd/mm/yyyy hh:mm:ss:zzz',now)]));
end;


{ Thred }

constructor TThred.Create(CreateSuspended: Boolean; const StackSize: SizeUInt);
begin
  Inherited Create(CreateSuspended);
  //app:= THTTPApplication.Create(nil);
  app:= TFPHTTPServer.create(nil);
  {$IFDEF WINDOWS}
    app.Threaded:=True;
  {$ENDIF};
  app.Port:= 8084;
  app.OnRequest:= @DoHandleRequest;
  //app.Initialize;
end;

procedure TThred.DoTerminate;
begin
  app.Active:=False;
  FreeAndNil(app);
  inherited DoTerminate;
end;

destructor TThred.Destroy;
begin
  inherited Destroy;
end;

procedure TThred.Execute;
begin
  //try
    aFPWebM:= TFPWebM.createnew(nil,1);
    app.Active:=True;
  //finally
  //  FreeAndNil(app);
  //  FreeAndNil(aFPWebM);
  //end;
end;

procedure TThred.DoHandleRequest(Sender: TObject;
var ARequest: TFPHTTPConnectionRequest;
var AResponse: TFPHTTPConnectionResponse);
begin
  //FURL:=Arequest.URL;
  //app.Synchronize(@ShowURL);
  aFPWebM.HandleRequest(ARequest,AResponse);
end;

{ TFrmPrincipal }

procedure TFrmPrincipal.actIniciarExecute(Sender: TObject);
begin
  try
    aThred:= TThred.Create(True);
    aThred.Start;
  finally
    MLog.Lines.Add(Format('Servidor Iniciado Data/Hora: %s ', [formatdatetime('dd/mm/yyyy hh:mm:ss:zzz',now)]));
    actIniciar.Enabled:=false;
    actParar.Enabled:=true;
  end;
end;

procedure TFrmPrincipal.actAbrirExecute(Sender: TObject);
begin
  Self.Show;
  self.WindowState := wsNormal;
  Self.BringToFront;
end;

procedure TFrmPrincipal.actFecharExecute(Sender: TObject);
begin
  actParar.Execute;
  close;
end;

procedure TFrmPrincipal.actPararExecute(Sender: TObject);
begin
  try
    aThred.app.Active:=false;
    aThred.Terminate;
  finally
    MLog.Lines.Add(Format('Servidor Parado Data/Hora: %s ', [formatdatetime('dd/mm/yyyy hh:mm:ss:zzz',now)]));
    actIniciar.Enabled:=True;
    actParar.Enabled:=False;
  end;
end;

procedure TFrmPrincipal.FormCreate(Sender: TObject);
begin
  MLog.Lines.Clear;
  if UpperCase(ParamStr(1)) = '-INICIAR' then
  begin
    Timer1.Enabled:=True;
  end;
end;

procedure TFrmPrincipal.FormWindowStateChange(Sender: TObject);
begin
  if self.WindowState = wsMinimized then
  begin
    self.WindowState := wsNormal;
    self.Hide;
    self.ShowInTaskBar := stNever;
  end;
end;

procedure TFrmPrincipal.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled:= False;
  actIniciar.Execute;
  self.WindowState := wsMinimized;
  //Application.Minimize;
  Self.Hide;
  //self.WindowState := wsMinimized;
  Self.Visible:=false;
  TrayIcon1.Visible:=true;
end;

initialization
  //RegisterHTTPModule(TFPWebM, True);
  //RegisterHTTPModule(TFPWebM, TFPWebModule, True);

end.

