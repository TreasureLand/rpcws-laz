unit Helper.Stream;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TFileStreamHelper }

  TFileStreamHelper = class helper for TFileStream
  public
    class function LoadFrom(const aFile: TFilename): AnsiString;
    class procedure WriteTo(const aFile: TFilename; const aData: AnsiString);
  end;

implementation

{ TFileStreamHelper }

class function TFileStreamHelper.LoadFrom(const aFile: TFilename): AnsiString;
begin
  with Self.Create(aFile, fmOpenRead or fmShareDenyNone) do
  try
    SetLength(Result, Size);
    ReadBuffer(Pointer(Result)^, Length(Result));
  finally
    Free;
  end;
end;

class procedure TFileStreamHelper.WriteTo(const aFile: TFilename; const aData: AnsiString);
begin
  with Self.Create(aFile, fmCreate) do
  try
    WriteBuffer(Pointer(aData)^, Length(aData));
  finally
    Free;
  end;
end;

end.

