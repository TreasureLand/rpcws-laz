unit principal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  ExtCtrls, Menus, StdCtrls, ActnList, httpdefs, fpHTTP, fpWeb, fpwebfile,
  fphttpapp, jwsconsts, jwsmessages, jwstypes, WSConnector, fpjson, jsonparser,
  jwsmethods, fphttpserver, custweb, fpmimetypes, websession
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
    fMsg: string;
    procedure DoSyncMessage();
    procedure SyncMessage(const s: string);
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
    WSConnector1: TWSConnector;
    procedure actAbrirExecute(Sender: TObject);
    procedure actFecharExecute(Sender: TObject);
    procedure actIniciarExecute(Sender: TObject);
    procedure actPararExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
    Fporta: Integer;
    FGravarLog: Boolean;
  public
    { public declarations }
  published
    property Porta     : Integer read FPorta     write FPorta;
    property GravarLog : Boolean read FGravarLog write FGravarLog;
  end;

  { TFPWebModule }

  TFPWebM = class(TFPWebModule)
  private
    acao : TFpWebAction;
    ActionMethods : TFpWebAction;
    procedure ActionMethodsRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
  private
    { private declarations }
    FADDLogFile : Boolean;
  public
    { public declarations }
    constructor createnew(AOwner:TComponent;createmode:Integer); override;
    destructor Destroy; override;
  published
    property ADDLogFile : boolean Read FADDLogFile write FADDLogFile;
  end;

var
  FrmPrincipal: TFrmPrincipal;
  ReqMethods: TJWSMethods;
  aThred: TThred;
  FHandler: TWebHandler;//TFPWebM;
implementation

{$R *.lfm}

{ TFPWebM }

constructor TFPWebM.createnew(AOwner: TComponent; createmode: Integer);
begin
  Inherited;
  CreateSession:= True;
  FADDLogFile:= False;
  acao := TFpWebAction.create(actions);
  Acao.name:= 'acao';
  ActionMethods:= TFpWebAction.create(actions);
  ActionMethods.Name:='Methods';
  ActionMethods.Default:=True;
  ActionMethods.OnRequest:= @ActionMethodsRequest;
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
    except on
      e:exception do
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

procedure TFPWebM.ActionMethodsRequest(Sender: TObject;
  ARequest: TRequest; AResponse: TResponse; var Handled: Boolean);
var
  jwsRequest: TJWSRequestContent;
  jwsResponse: TJWSResponseContent;
  i, iID: integer;
begin
  //writeln('TFPWebM.FPWebActionRequestRequest ');
  //aThred.SyncMessage('URL '+ARequest.URL);
  aThred.SyncMessage(Format('Cliente "%s" Data/Hora %s Requisicao', [ARequest.RemoteAddr,formatdatetime('dd/mm/yyyy hh:mm:ss:zzz',now),ARequest.QueryFields.Values['QUERY']]));
  Handled := true;
  jwsRequest   := nil;
  jwsResponse  := nil;

  if ARequest.Method <> 'POST' then
  begin
    if FADDLogFile then
      addlog('Get');
    AResponse.Content := MSG_GETRESPONSE;
    handled := true;
  end
  else
  begin
    try
      try
        if FADDLogFile then
        begin
          addlog('Request header count: '+ inttostr(ARequest.FieldCount));
          for i := 0 to ARequest.FieldCount -1 do
            addlog(ARequest.FieldValues[i] + ': '+ ARequest.FieldValues[i]);
          addlog(ARequest.Content);
        end;
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
  aThred.SyncMessage(Format('Cliente "%s" Data/Hora %s Resposta', [ARequest.RemoteAddr,formatdatetime('dd/mm/yyyy hh:mm:ss:zzz',now)]));
end;


{ Thred }

procedure TThred.DoSyncMessage;
begin
  FrmPrincipal.MLog.Lines.Add(fMsg);
end;

procedure TThred.SyncMessage(const s: string);
begin
  fMsg:=s;
  Synchronize(@DoSyncMessage);
end;

constructor TThred.Create(CreateSuspended: Boolean; const StackSize: SizeUInt);
begin
  Inherited Create(CreateSuspended);
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
  app.Active:=True;
end;

procedure TThred.DoHandleRequest(Sender: TObject;
var ARequest: TFPHTTPConnectionRequest;
var AResponse: TFPHTTPConnectionResponse);
begin
  //FURL:=Arequest.URL;
  //app.Synchronize(@ShowURL);
  //SyncMessage('URL 'ARequest.URL);
  FHandler.HandleRequest(ARequest,AResponse);
  //rou
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
  FHandler := TWebHandler.Create(Self);
  //FHandler.ADDLogFile:=true;
  RegisterHTTPModule('', TFPWebM, True);
  //RegisterHTTPModule('methods', TFPWebM, True);
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

end.

