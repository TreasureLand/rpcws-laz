unit principal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  ExtCtrls, Menus, httpdefs, fpHTTP, fpWeb, fpwebfile, fphttpapp
  {$IFDEF UNIX}{$IFDEF UseCThreads}
    ,cthreads
  {$ENDIF}{$ENDIF};

type

  { TFrmPrincipal }

  { TThred }

  TThred = Class(tThread)
  private
    app: THTTPApplication;
  public
    constructor Create(CreateSuspended: Boolean; const StackSize: SizeUInt=DefaultStackSize
      );
    destructor Destroy; override;
   protected
     procedure Execute; override;
  end;

  TFrmPrincipal = class(TForm)
    BitBtn1: TSpeedButton;
    BitBtn2: TSpeedButton;
    PopupMenu1: TPopupMenu;
    TrayIcon1: TTrayIcon;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

  { TFPWebModule1 }

  TFPWebM = class(TFPWebModule)
    acao : TFpWebAction;
    procedure testeRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
  private
    { private declarations }
  public
    { public declarations }
    constructor createnew(AOwner:TComponent;createmode:Integer); override;

  end;

var
  FrmPrincipal: TFrmPrincipal;
  aThred: TThred;
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
  Acao.Default:=True;
  acao.OnRequest:=@testeRequest;
end;

{ TFrmPrincipal }

procedure TFrmPrincipal.BitBtn1Click(Sender: TObject);
begin
  aThred:= TThred.Create(True);
  aThred.Start;
end;

procedure TFrmPrincipal.BitBtn2Click(Sender: TObject);
begin
  aThred.Terminate;
end;

{ Thred }

constructor TThred.Create(CreateSuspended: Boolean; const StackSize: SizeUInt);
begin
  Inherited Create(CreateSuspended);
  app:= THTTPApplication.Create(nil);
  app.Threaded:=True;
  app.Port:= 8084;
  app.Initialize;
end;

destructor TThred.Destroy;
begin
  FreeAndNil(app);
  inherited Destroy;
end;

procedure TThred.Execute;
begin
   app.Run;
end;

initialization
  RegisterHTTPModule(TFPWebM, True);
  //RegisterHTTPModule(TFPWebM, TFPWebModule, True);

end.

