program S7SHttpServer;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  fphttpapp, Unit1, s7sutils;

begin
  Application.Port:=8084;
  Application.Threaded:=True;
  Application.Initialize;
  Application.Run;
end.

