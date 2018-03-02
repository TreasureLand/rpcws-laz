unit S7SDBJsonWriter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, base64, db;

type

  { TS7SDBJsonWriter }

  TS7SDBJsonWriter = class
  public
    constructor Create(aObject: TJSONObject); overload;
    constructor Create(); overload;
    destructor Destroy; override;
  strict private
    FOjectOwned: Boolean;
  private
  type
    TArrayField = array of TField;
  private
    Fields: record
              Initialized: Boolean;
              Keys, Updates: TArrayField;
            end;
    FObject: TJSONObject;
    FDeletes, FInserts, FUpdates: TJSONArray;
    procedure WriteFields(aFields: TFields);
    function NewArray(aFields: TArrayField): TJSONArray;
  public
    property &Object: TJSONObject read FObject;
    procedure ApplyRecUpdate(aDataSet: TDataSet; aUpdateKind: TUpdateKind);
  end;

implementation

{ TDBJsonWriter }

constructor TS7SDBJsonWriter.Create(aObject: TJSONObject);
begin
  inherited Create;
  FObject := aObject;
end;

constructor TS7SDBJsonWriter.Create;
begin
  inherited;
  FObject     := TJSONObject.Create();
  FOjectOwned :=True;
end;

destructor TS7SDBJsonWriter.Destroy;
begin
  if FOjectOwned then
    FObject.Free;
  inherited Destroy;
end;

procedure TS7SDBJsonWriter.WriteFields(aFields: TFields);
var
  vpk, vFields: TJSONArray;

  procedure _AddField(f : TField; IsKey: Boolean);

    procedure _Add(var aList: TArrayField);
    var
      i: Integer;
    begin
      i := Length(aList);
      SetLength(aList, i + 1);
      aList[i] := f;
    end;

  begin
    _Add(Fields.Updates);
    if IsKey then
      _Add(Fields.Keys);

    vFields.Add(f.FieldName);
    if IsKey then
      vpk.Add(f.FieldName);
  end;

var
  f : TField;
begin
  SetLength(Fields.Keys, 0);
  SetLength(Fields.Updates, 0);

  vpk     := TJSONArray.Create();
  vFields := TJSONArray.Create();
  FObject.Add('pks', vpk);
  FObject.Add('fields', vFields);
  for f in aFields do
  begin
    if pfInKey in f.ProviderFlags then
      _AddField(f, True)
    else if pfInUpdate in f.ProviderFlags then
      if not f.ReadOnly then
        _AddField(f, False);
  end;

  FDeletes := TJSONArray.Create();
  FInserts := TJSONArray.Create();
  FUpdates := TJSONArray.Create();
  FObject.Add('deletes', FDeletes);
  FObject.Add('inserts', FInserts);
  FObject.Add('updates', FUpdates);
  Fields.Initialized := True;
end;

function TS7SDBJsonWriter.NewArray(aFields: TArrayField) : TJSONArray;
const cBase64Signature = '#base64#';
Var
  vEncode : TStream;
  vStream : TStringStream;
  F       : TField;
begin
  vStream := nil;
  Try
    Result := TJSONArray.Create();
    try
      for F in aFields do
      begin
        if F.IsNull then
          Result.Add(TJSONNull.Create)
        else if F.IsBlob then
        begin
          if vStream = nil then
            vStream := TStringStream.Create('')
          else
          begin
            vStream.Position  := 0;
            vStream.Size      := 0;
          end;
          vEncode := TBase64EncodingStream.Create(vStream);
          try
            (f as TBlobField).SaveToStream(vStream);
          finally
            vEncode.Free;
          end;
          Result.Add(cBase64Signature + vStream.DataString);
        end
        else
        begin
          case f.DataType of
            ftSmallint,
            ftInteger,
            ftWord,
            ftAutoInc,
            ftLargeint: Result.Add(f.AsInteger);

            ftBoolean: Result.Add(F.AsBoolean);

            ftFloat,
            ftCurrency,
            ftBCD,
            ftFMTBcd: Result.Add(F.AsFloat);

            ftDate,
            ftDateTime,
            ftTimeStamp: Result.Add(F.AsDateTime);
            else
              Result.Add(F.AsString);
          end;
        end;
      end;
    except
      Result.Free;
      raise;
    end;
  finally
    vStream.free;
  end;
end;

procedure TS7SDBJsonWriter.ApplyRecUpdate(aDataSet: TDataSet; aUpdateKind: TUpdateKind);

  procedure _Modify();
  var
    o : TJSONObject;
  begin
    o := TJSONObject.Create();
    FUpdates.Add(o);
    o.Add('pk', NewArray(Fields.Keys));
    o.Add('values', NewArray(Fields.Updates));
  end;

begin
  if not Fields.Initialized then
    WriteFields(aDataSet.Fields);
  case aUpdateKind of
    ukModify: _Modify();
    ukInsert: FInserts.Add(NewArray(Fields.Updates));
    ukDelete: FDeletes.Add(NewArray(Fields.Keys));
  end;
end;
end.


