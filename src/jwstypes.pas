{*****************************************************************}
{                 JSON RPC Web Service Library                    }
{                                                                 }
{ by Jose Benedito - josebenedito@gmail.com                       }
{ www.jbsolucoes.net                                              }
{*****************************************************************}

unit jwstypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, variants, fpjson, jsonparser, jsonscanner;

type
  TJWSArgsType = array of variant;

  { TJWSRequestContent }

  TJWSRequestContent = class(TJSONObject)
  private
    FParams: TJSONArray;
    function GetArgs: TJSONArray;
    function GetID: integer;
    function GetMethod: string;
    procedure SetID(AValue: integer);
    procedure SetMethod(AValue: string);
  public
    constructor create;
    destructor destroy; override;

    property ID: integer read GetID write SetID;
    property Method: string read GetMethod write SetMethod;
    property Args: TJSONArray read GetArgs;
  end;

  { TJWSResponseContent }

  TJWSResponseContent = class(TJSONObject)
    private
      function GetID: integer;
      function GetResult: TJSONData;
      procedure SetID(AValue: integer);
      procedure SetResult(AValue: TJSONData);
    public
      procedure SetResultContent(const AResult: TJSONData; const AID: integer);

      destructor Destroy; override;
  end;

  function GetVariantType(const v: variant): string;
  function GetJSONArray(AArray: array of variant): TJSONArray;

  procedure addlog(amsg: string);

  function JSONRPCResult(const AResult: TJSONData; const AID: integer = -1): TJWSResponseContent;
  function JSONRPCError(const ACode: integer; const AMessage: string; AID: integer = -1): TJWSResponseContent;


implementation

uses jwsconsts, jwsjson;



function GetVariantType(const v: variant): string;
begin
  case TVarData(v).vType of
    varEmpty: result := 'Empty';
    varNull: result := 'Null';
    varSmallInt: result := 'SmallInt';
    varInteger: result := 'Integer';
    varSingle: result := 'Single';
    varDouble: result := 'Double';
    varCurrency: result := 'Currency';
    varDate: result := 'Date';
    varOleStr: result := 'OleStr';
    varDispatch: result := 'Dispatch';
    varError: result := 'Error';
    varBoolean: result := 'Boolean';
    varVariant: result := 'Variant';
    varUnknown: result := 'Unknown';
    varByte: result := 'Byte';
    varString: result := 'String';
    varTypeMask: result := 'TypeMask';
    varArray: result := 'Array';
    varByRef: result := 'ByRef';
  end; // case
end;

function GetJSONArray(AArray: array of variant): TJSONArray;
var
  joArray: TJSONArray;
  i, iLen: integer;
  vtype: tvartype;
  vJsonData: TJSONData;
begin
  try
    joArray := TJSONArray.create;
    iLen := High(AArray);
    for i := 0 to iLen do
    begin
      if IsJSON(AArray[i]) then
      begin

        with TJSONParser.Create(AArray[i], [joUTF8]) do
        try
          vJsonData := Parse;
          joArray.Add(vJsonData);
        finally
          Free;
        end;

        //joArray.Add(TJSONParser.create(AArray[i]).Parse);
        result := joArray;
        break;
      end;

      vtype := TVarData(AArray[i]).vType;

      case vtype of
        varEmpty: joArray.Add('');
        varNull: joArray.Add;
        varSmallInt: joArray.Add(integer(AArray[i]));
        varshortint: joArray.Add(integer(AArray[i]));
        varInteger: joArray.Add(integer(AArray[i]));
        varSingle: joArray.Add(integer(AArray[i]));
        varDouble: joArray.Add(TJSONFloat(AArray[i]));
        varCurrency: joArray.Add(TJSONFloat(AArray[i]));
        varDate: joArray.Add(datetostr(AArray[i]));
        varOleStr: joArray.Add(string(AArray[i]));
        varDispatch: joArray.Add(string(AArray[i]));
        varError: joArray.Add('error');
        varBoolean: joArray.Add(boolean(AArray[i]));
        varVariant: joArray.Add(string(AArray[i]));
        varUnknown: joArray.Add(string(AArray[i]));
        varByte: joArray.Add(integer(AArray[i]));
        varString: joArray.Add(string(AArray[i]));
        varTypeMask: joArray.Add(string(AArray[i]));
        varArray: joArray.Add(GetJSONArray(AArray[i]));
        varByRef: joArray.Add(integer(AArray[i]));
      end;
    end;
    result := joArray;
  finally
    //joArray.Free;
  end;
end;

function JSONRPCResult(const AResult: TJSONData; const AID: integer = -1): TJWSResponseContent;
var
  joResult: TJWSResponseContent;
begin
  joResult := TJWSResponseContent.create;

  joResult.Add('jsonrpc',JSONRPC_VERSION);
  joResult.Add('result',AResult);
  joResult.Add('id',AID);

  result := joResult;
end;

function JSONRPCError(const ACode: integer; const AMessage: string; AID: integer = -1): TJWSResponseContent;
var
  jsonerror: TJSONObject;
  joResult: TJWSResponseContent;
begin
  jsonerror := TJSONObject.Create();
  (jsonerror as TJSONObject).Add('code',ACode);
  (jsonerror as TJSONObject).Add('message',AMessage);

  joResult := TJWSResponseContent.create;
  joResult.Add('jsonrpc',JSONRPC_VERSION);
  joResult.Add('error',jsonerror);
  joResult.Add('id',AID);

  result := joResult;
end;



{ TJWSResponseContent }

function TJWSResponseContent.GetID: integer;
begin
  {$WARNINGS OFF}
  if TJSONObject(Self).Find('id') <> nil then
    result := TJSONObject(Self).Find('id').AsInteger
  else
    result := -1;
  {$WARNINGS ON}
end;

function TJWSResponseContent.GetResult: TJSONData;
begin
  {$WARNINGS OFF}
  if TJSONObject(Self).Find('result') <> nil then
    result := TJSONObject(Self).Find('result')
  else
    result := nil;
  {$WARNINGS ON}
end;

procedure TJWSResponseContent.SetID(AValue: integer);
begin
  {$WARNINGS OFF}
  if TJSONObject(Self).Find('id') <> nil then
    TJSONObject(Self).Find('id').AsInteger := AValue
  else
    TJSONObject(Self).Add('id',AValue);
  {$WARNINGS ON}
end;

procedure TJWSResponseContent.SetResult(AValue: TJSONData);
begin
  {$WARNINGS OFF}
  if TJSONObject(Self).Find('result') <> nil then
    TJSONObject(Self).Delete('result');

  TJSONObject(Self).Add('result',AValue as TJSONObject);
  {$WARNINGS ON}
end;

procedure TJWSResponseContent.SetResultContent(const AResult: TJSONData;
  const AID: integer);
begin
  Self.Clear;

  Self.Add('jsonrpc',JSONRPC_VERSION);
  Self.Add('result',AResult);
  Self.Add('id',AID);
end;

destructor TJWSResponseContent.Destroy;
begin

  inherited Destroy;
end;

{ TJWSRequestContent }

function TJWSRequestContent.GetArgs: TJSONArray;
begin
  if Find('params') <> nil then
    result := (Find('params') as TJSONArray)
  else
  begin
    FParams := TJSONArray.Create;

    self.Add('params',FParams);
    result := (Find('params') as TJSONArray);
  end;
end;

function TJWSRequestContent.GetID: integer;
begin
  if Self.Find('id') <> nil then
    result := Self.Find('id').AsInteger
  else
  begin
    Self.Add('id',0);

    result := 0;
  end;
end;

function TJWSRequestContent.GetMethod: string;
begin
  if Self.Find('method') <> nil then
    result := Self.Find('method').AsString
  else
  begin
    Self.Add('method','');

    result := '';
  end;
end;


procedure TJWSRequestContent.SetID(AValue: integer);
begin
  if Self.Find('id') <> nil then
    Self.Find('id').AsInteger := AValue
  else
    Self.Add('id',AValue);
end;

procedure TJWSRequestContent.SetMethod(AValue: string);
begin
  if Self.Find('method') <> nil then
    Self.Find('method').AsString := AValue
  else
    Self.Add('method',AValue);
end;

constructor TJWSRequestContent.create;
begin
  inherited create;

  FParams := TJSONArray.create;

  Self.Add('jsonrpc',JSONRPC_VERSION);
  Self.Add('method','');
  Self.Add('params',FParams);
  Self.Add('id',0);
end;

destructor TJWSRequestContent.destroy;
begin

  inherited destroy;
end;

procedure addlog(amsg: string);
var
  slfile: tstringlist;
begin
  slfile := tstringlist.create;
  try
    if fileexists(paramstr(0) + 'log.txt') then
      slfile.loadfromfile(paramstr(0) + 'log.txt');

    slfile.add(datetimetostr(now) + #9 + amsg);

    slfile.savetofile(paramstr(0) + 'log.txt');
  finally
    freeandnil(slfile);
  end;

end;


end.

