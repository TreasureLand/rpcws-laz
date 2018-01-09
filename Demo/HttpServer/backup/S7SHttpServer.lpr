program S7SHttpServer;

{$mode objfpc}{$H+}

uses
  fphttpapp, Unit1, s7sutils;

begin
  Application.Port:=8084;
  {$ifdef windows}
  Application.Threaded:=True;
  {$endif}
  Application.Initialize;
  Application.Run;
end.

