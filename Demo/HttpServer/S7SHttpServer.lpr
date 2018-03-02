program S7SHttpServer;

{$mode objfpc}{$H+}

uses
  fphttpapp, fpmimetypes, fpwebfile, fpWeb, fphttp, Unit1, s7sutils;

begin
  //MimeTypes.AddType('text/html','html');
  //MimeTypes.AddType('text/css','css');
  //MimeTypes.AddType('text/javascript','js');
  //RegisterFileLocation('files','C:\horus\Server\html');
  //RegisterFileLocation('jquery-easyui','C:\horus\Server\html\public\jquery-easyui');
  RegisterHTTPModule('methods', TFPWebModule1);
  Application.Port:=8084;
  Application.Threaded:=True;
  Application.Initialize;
  Application.Run;
end.

