unit DBJsonReader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uDBExecutor, fpjson, base64, strutils, db;


type

  TSQLExecutor = uDBExecutor.TSQLExecutor;

  { TDBJsonReader }

  TDBJsonReader = class
  public
    constructor Create(aSQLExecutorFactory: TSQLExecutorFactory);
    destructor Destroy; override;
  strict private
    FSQLExecutorFactory: TSQLExecutorFactory;
    Fields: record Keys, Updates: TStrings; end;
    FTabName: string;
    procedure LoadFieldDefs(aKeys, aFields: TJSONArray);          
    function GetSQLKeys(): string;
    function CreateSQLExecutor(const aSQL: string): TSQLExecutor;
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

constructor TDBJsonReader.Create(aSQLExecutorFactory : TSQLExecutorFactory);
begin
  inherited Create;
  FSQLExecutorFactory := aSQLExecutorFactory;
  Fields.Keys         := TStringList.Create;
  Fields.Updates      := TStringList.Create;
end;

destructor TDBJsonReader.Destroy;
begin
  Fields.Keys.Free;
  Fields.Updates.Free;
  inherited Destroy;
end;

procedure TDBJsonReader.LoadFieldDefs(aKeys, aFields: TJSONArray);
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

function TDBJsonReader.GetSQLKeys: string;
var
  i : Integer;
begin
  Result := '';
  for i := 0 to Fields.Keys.Count - 1 do
    Result := Result + Fields.Keys[i] + '=:k' + IntToStr(i) + ' and ';
  SetLength(Result, Length(Result) - 5);//remove last comma
end;

function TDBJsonReader.CreateSQLExecutor(const aSQL: string): TSQLExecutor;
begin
  if not Assigned(FSQLExecutorFactory) then
    raise Exception.Create('SQLExecutorFactory: not Assigned');
  Result := FSQLExecutorFactory(aSQL);
  if Result = nil then
    raise Exception.Create('SQLExecutorFactory: Result=nil');
end;

procedure TDBJsonReader.LoadKeys(aValues: TJSONArray; aParams: TParams);
var
  i: Integer;
begin
  for i := 0 to Fields.Keys.Count - 1 do
    aParams.ParamByName('k'+IntToStr(i)).AsString := aValues[i].AsString;
end;

procedure TDBJsonReader.LoadValues(aValues: TJSONArray; aParams: TParams);

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
      texto:= vParam.Name;
      vValue := aValues[i];
      texto  := texto + ': '+ vValue.AsString;
      WriteLn(texto);
      if vValue.IsNull then
        vParam.Clear
      else
      begin
        s := vValue.AsString;
        if not _Base64(s, vParam) then
          vParam.AsString := s;
      end;
    end;
  except
    on e: Exception do
    begin
      raise exception.Create(e.Message+#13+'Valor: '+s);
    end;
  end;
end;

procedure TDBJsonReader.Insert(aData: TJSONArray);

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
      s2 := s2+':v'+IntToStr(i)+',';
    end;
    SetLength(s1, Length(s1) - 1);
    SetLength(s2, Length(s2) - 1);
    Result := 'insert into '+FTabName+'('+s1+') values ('+s2+')';
  end;

var
  qr: TSQLExecutor;
  i : TJSONEnum;
  vParams: TParams;
begin
  if aData = nil then Exit;
  qr := CreateSQLExecutor(_GetSql());
  try
    vParams := qr.GetParams();
    for i in aData do
    begin
      LoadValues(i.Value as TJSONArray, vParams);
      qr.ExecSQL;
    end;
  finally
    qr.Free;
  end;
end;

procedure TDBJsonReader.Modify(aData: TJSONArray);

  function _GetSql(): String;
  var
    i : Integer;
  begin
    Result := '';
    for i := 0 to Fields.Updates.Count - 1 do
      Result := Result+','+Fields.Updates[i]+'=:v'+IntToStr(i);

    Result := 'update '+FTabName+' set ' + Copy(Result, 2) + ' where ' + GetSQLKeys();
  end;

var
  qr : TSQLExecutor;
  vParams : TParams;
  i : TJSONEnum;
  vRec : TJSONObject;
begin
  if aData = nil then Exit;
  qr := CreateSQLExecutor(_GetSql());
  try
    vParams := qr.GetParams();
    for i in aData do
    begin
      vRec := i.Value as TJSONObject;
      LoadKeys(vRec.Find('pk') as TJSONArray, vParams);
      LoadValues(vRec.Find('values') as TJSONArray, vParams);
      qr.ExecSQL;
    end;
  finally
    qr.Free;
  end;
end;

procedure TDBJsonReader.Delete(aData: TJSONArray);
var
  qr : TSQLExecutor;
  vParams : TParams;
  i : TJSONEnum;
begin
  if aData = nil then Exit;

  qr := CreateSQLExecutor('delete from '+FTabName + ' where ' + GetSQLKeys());
  try
    vParams := qr.GetParams();
    for i in aData do
    begin
      LoadKeys(i.Value as TJSONArray, vParams);
      qr.ExecSQL;
    end;
  finally
    qr.Free;
  end;
end;

procedure TDBJsonReader.Execute(aJson: TJSONObject; const aTabName: string);
var AJsonPKS,AJsonFields: TJSONArray;
    AResult: String;
begin
  AResult:= aJson.Find('pks').AsJSON;
  AJsonPKS    := aJson.Find('pks') as TJSONArray;
  AJsonFields := aJson.Find('fields') as TJSONArray;
  FTabName := aTabName;
  LoadFieldDefs(AJsonPKS, AJsonFields);
  Self.Delete(aJson.Find('deletes') as TJSONArray);
  Self.Insert(aJson.Find('inserts') as TJSONArray);
  Self.Modify(aJson.Find('updates') as TJSONArray);
end;

end.

