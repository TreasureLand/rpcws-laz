unit S7SDBJsonReader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, base64, strutils, db, s7sConn,
  Helper.Exceptions, s7stypes;


type

  { TS7SDBJsonReader }

  TS7SDBJsonReader = class
  public
    constructor Create(AQuery: TS7SQuery);
    destructor Destroy; override;
  strict private
    FQuery: TS7SQuery;
    Fields: record Keys, Updates: TStrings; end;
    FTabName: string;
    procedure LoadFieldDefs(aKeys, aFields: TJSONArray);
    function GetSQLKeys(): string;
    procedure LoadKeys(aValues: TJSONArray; aParams: TParams);
    procedure LoadValues(aValues: TJSONArray; aParams: TParams);
    procedure Insert(aData: TJSONArray);
    procedure Modify(aData: TJSONArray);
    procedure Delete(aData: TJSONArray);
  public
    procedure Execute(aJson: TJSONObject; const aTabName: string);
  end;

implementation

{ TDBJsonReader }

constructor TS7SDBJsonReader.Create(AQuery: TS7SQuery);
begin
  inherited Create;
  FQuery := AQuery;
  Fields.Keys         := TStringList.Create;
  Fields.Updates      := TStringList.Create;
end;

destructor TS7SDBJsonReader.Destroy;
begin
  Fields.Keys.Free;
  Fields.Updates.Free;
  inherited Destroy;
end;

procedure TS7SDBJsonReader.LoadFieldDefs(aKeys, aFields: TJSONArray);
var
  i : TJSONEnum;
begin
  Fields.Updates.Clear;
  Fields.Keys.Clear;
  for i in aKeys do
    Fields.Keys.Add(i.Value.AsString);
  for i in aFields do
    Fields.Updates.Add(i.Value.AsString);
end;

function TS7SDBJsonReader.GetSQLKeys: string;
var
  i : Integer;
begin
  Result := '';
  for i := 0 to Fields.Keys.Count - 1 do
    Result := Result + Fields.Keys[i] + '=:k' + IntToStr(i) + ' and ';
  SetLength(Result, Length(Result) - 5);//remove last comma
end;

procedure TS7SDBJsonReader.LoadKeys(aValues: TJSONArray; aParams: TParams);
var
  i: Integer;
begin
  for i := 0 to Fields.Keys.Count - 1 do
    aParams.ParamByName('k'+IntToStr(i)).AsString := aValues[i].AsString;
end;

procedure TS7SDBJsonReader.LoadValues(aValues: TJSONArray; aParams: TParams);

  function _Base64(const s: RawByteString; aParam: TParam): Boolean;
  const
    cBase64Signature = '#base64#';
  var
    vStream : TStream;
    Decoder : TBase64DecodingStream;
  begin
    Result := AnsiStartsStr(cBase64Signature, s);
    if not Result then Exit;

    vStream := TStringStream.Create(s);
    try
      vStream.Position := Length(cBase64Signature);//discarde Base64 Signature
      Decoder := TBase64DecodingStream.Create(vStream);
      try
        aParam.LoadFromStream(Decoder, ftBlob);
      finally
        Decoder.Free;
      end;
    finally
      vStream.Free;
    end;
  end;

var
  s : RawByteString;
  i     : Integer;
  vParam: TParam;
  vValue: TJSONData;
  texto : String;
begin
  try
    for i := 0 to Fields.Updates.Count - 1 do
    begin
      vParam := aParams.ParamByName('v'+IntToStr(i));
      vValue := aValues[i];
      JsonItemToParam(vParam,vValue);
      {
      texto:= vParam.Name;
      if vValue.IsNull then
        vParam.Clear
      else
      begin
        texto  := texto + ': '+ vValue.AsString;
        s := vValue.AsString;
        if not _Base64(s, vParam) then
          vParam.AsString := s;
      end;
      WriteLn(texto);
      }
    end;
  except
    //WriteLn(Exception.GetMessageFrom());
    raise EExceptExcept.Create('Valor: '+s);
  end;
end;

procedure TS7SDBJsonReader.Insert(aData: TJSONArray);

  function _GetSql(): String;
  var
    i : Integer;
    s1, s2 : String;
  begin
    s1 := '';
    s2 := '';
    for i := 0 to Fields.Updates.Count - 1 do
    begin
      s1 := s1+Fields.Updates[i]+',';
      s2 := s2+':'+Fields.Updates[i]+',';
    end;
    SetLength(s1, Length(s1) - 1);
    SetLength(s2, Length(s2) - 1);
    Result := 'insert into '+FTabName+'('+s1+') values ('+s2+')';
  end;

var
  i : TJSONEnum;
  vParams: TParams;
begin
  if aData = nil then Exit;
  FQuery.SQL.Text:=_GetSql();
  vParams := FQuery.Params;
  for i in aData do
  begin
    LoadValues(i.Value as TJSONArray, vParams);
    FQuery.ExecSQL;
  end;

end;

procedure TS7SDBJsonReader.Modify(aData: TJSONArray);

  function _GetSql(): String;
  var
    i : Integer;
  begin
    Result := '';
    for i := 0 to Fields.Updates.Count - 1 do
      Result := Result+','+Fields.Updates[i]+'= :'+Fields.Updates[i];
    Result := 'update '+FTabName+' set ' + Copy(Result, 2) + ' where ' + GetSQLKeys();
  end;

var
  vParams : TParams;
  i : TJSONEnum;
  vRec : TJSONObject;
begin
  if aData = nil then Exit;
  FQuery.SQL.Text:=_GetSql();
  vParams := FQuery.Params;
  for i in aData do
  begin
    vRec := i.Value as TJSONObject;
    LoadKeys(vRec.Find('pk') as TJSONArray, vParams);
    LoadValues(vRec.Find('values') as TJSONArray, vParams);
    FQuery.ExecSQL;
  end;

end;

procedure TS7SDBJsonReader.Delete(aData: TJSONArray);
var
  vParams : TParams;
  i : TJSONEnum;
begin
  if aData = nil then Exit;
  FQuery.SQL.Text:='delete from '+FTabName + ' where ' + GetSQLKeys();
  vParams := FQuery.Params;
  for i in aData do
  begin
    LoadKeys(i.Value as TJSONArray, vParams);
    FQuery.ExecSQL;
  end;
end;

procedure TS7SDBJsonReader.Execute(aJson: TJSONObject; const aTabName: string);
var AJsonPKS,AJsonFields: TJSONArray;
begin
  AJsonPKS    := aJson.Find('pks') as TJSONArray;
  AJsonFields := aJson.Find('fields') as TJSONArray;
  FTabName := aTabName;
  LoadFieldDefs(AJsonPKS, AJsonFields);
  Self.Delete(aJson.Find('deletes') as TJSONArray);
  Self.Insert(aJson.Find('inserts') as TJSONArray);
  Self.Modify(aJson.Find('updates') as TJSONArray);
end;

end.

