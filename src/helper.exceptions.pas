unit Helper.Exceptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;
type

  { THelperException }

  THelperException = class helper for Exception
  public
    class function GetMessageFrom(aExceptObject : TObject) : string; overload; //static;
    class function GetMessageFrom() : string; overload; //static;
  end;

  { EExceptExcept }

  EExceptExcept = class(Exception)
  public
    constructor Create(const msg : string; aException : TObject); overload;
    constructor Create(const msg : string); overload;
    constructor CreateFmt(const Msg: string; const Args: array of const);
  end;

implementation

{ THelperException }

class function THelperException.GetMessageFrom(aExceptObject : TObject) : string;
begin
  if aExceptObject = nil then
    Result := '[nula]'
  else if aExceptObject is Exception then
    Result := Exception(aExceptObject).Message
  else
    Result := '['+aExceptObject.ClassName+']';
end;

class function THelperException.GetMessageFrom : string;
begin
  Result := GetMessageFrom(ExceptObject);
end;

{ EExceptExcept }

constructor EExceptExcept.Create(const msg : string; aException : TObject);
begin
  inherited Create(msg + sLineBreak + sLineBreak + Exception.GetMessageFrom(aException));
end;

constructor EExceptExcept.Create(const msg : string);
begin
  Create(msg, ExceptObject);
end;

constructor EExceptExcept.CreateFmt(const Msg: string; const Args: array of const);
begin
  Create(Format(Msg, Args));
end;

end.

