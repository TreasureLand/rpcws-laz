program jrpcws;

{$mode objfpc}{$H+}

uses
  heaptrc,
  //fpFCGI
  fpCGI
  , mainmodule, jwsconsts, jwsmessages, servermethods,
  jwsmethods, jwsjson;

begin
  //log de memory leak
  //SetHeapTraceOutput(paramstr(0) + '.log');
  Application.Initialize;
  Application.Run;
end.

