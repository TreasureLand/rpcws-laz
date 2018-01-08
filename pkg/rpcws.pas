{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit rpcws;

interface

uses
  WSConnector, WSBaseQuery, RUtils, DBJsonWriter, Helper.Stream, DBJsonReader, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('WSConnector', @WSConnector.Register);
  RegisterUnit('WSBaseQuery', @WSBaseQuery.Register);
end;

initialization
  RegisterPackage('rpcws', @Register);
end.
