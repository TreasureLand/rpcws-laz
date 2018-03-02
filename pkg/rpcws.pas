{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit rpcws;

interface

uses
  WSConnector, WSBaseQuery, Helper.Stream, jwsmessages, servermethods, 
  s7sConn, S7SDBJsonReader, Helper.Exceptions, S7SProvider, S7SDBJsonWriter, 
  jwsmethods, RGUtils, RUtils, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('WSConnector', @WSConnector.Register);
  RegisterUnit('WSBaseQuery', @WSBaseQuery.Register);
end;

initialization
  RegisterPackage('rpcws', @Register);
end.
