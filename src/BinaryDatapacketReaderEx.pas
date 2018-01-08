unit BinaryDatapacketReaderEx;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BufDataset, db, FmtBCD;


type

  { TBinaryDatapacketReaderEx }

  TBinaryDatapacketReaderEx = class(TFpcBinaryDatapacketReader)
  private
    vNullBitmap: TBytes;
  public
    procedure LoadFieldDefs(var AnAutoIncValue : integer); override;
    procedure RestoreRecord; override;
  end;

implementation

{ TBinaryDatapacketReaderEx }

procedure TBinaryDatapacketReaderEx.LoadFieldDefs(var AnAutoIncValue : integer);
begin
  inherited;
  SetLength(vNullBitmap, (DataSet.FieldDefs.Count + 7) div 8);
end;

procedure TBinaryDatapacketReaderEx.RestoreRecord;
const
  StringFieldTypes = [ftString,ftFixedChar,ftWideString,ftFixedWideChar];
  BlobFieldTypes   = [ftBlob,ftMemo,ftGraphic,ftWideMemo];
  VarLenFieldTypes = StringFieldTypes + BlobFieldTypes + [ftBytes, ftVarBytes];
  DefaultFieldSize : Array [TFieldType] of Integer =
      (
        { ftUnknown}      0,
        { ftString}       0,
        { ftSmallint}     SizeOf(SmallInt),
        { ftInteger}      SizeOf(LongInt),
        { ftWord}         SizeOf(Word),
        { ftBoolean}      SizeOf(wordBool),
        { ftFloat}        SizeOf(Double),
        { ftCurrency}     SizeOf(Double),
        { ftBCD}          SizeOf(system.currency),
        { ftDate}         SizeOf(TDateTime),
        { ftTime}         SizeOf(TDateTime),
        { ftDateTime}     SizeOf(TDateTime),
        { ftBytes}        0,
        { ftVarBytes}     0,
        { ftAutoInc}      SizeOf(LongInt),
        { ftBlob}         0,
        { ftMemo}         0,
        { ftGraphic}      0,
        { ftFmtMemo}      0,
        { ftParadoxOle}   0,
        { ftDBaseOle}     0,
        { ftTypedBinary}  0,
        { ftCursor}       0,
        { ftFixedChar}    0,
        { ftWideString}   0,
        { ftLargeint}     SizeOf(Largeint),
        { ftADT}          0,
        { ftArray}        0,
        { ftReference}    0,
        { ftDataSet}      0,
        { ftOraBlob}      0,
        { ftOraClob}      0,
        { ftVariant}      0,
        { ftInterface}    0,
        { ftIDispatch}    0,
        { ftGuid}         0,
        { ftTimeStamp}    0,
        { ftFMTBcd}       SizeOf(TBCD),
        { ftFixedWideString} 0,
        { ftWideMemo}     0
      );

  function _GetFieldIsNull(NullMask: PByte; x: LongInt): Boolean; //inline;
  begin
    Result := Ord(NullMask[x div 8]) and (1 shl (x mod 8)) > 0;
  end;


var
  vFieldF  : TField;
  vFieldDef: TFieldDef;
  ii: integer;
  L: cardinal;
  B: TBytes;
  s : String;
begin
  if FVersion <> 20 then
  begin
    inherited RestoreRecord;
    Exit;
  end;

  Stream.ReadBuffer(Pointer(vNullBitmap)^, Length(vNullBitmap));
  for ii := 0 to DataSet.FieldDefs.Count-1 do
  begin
    vFieldDef := DataSet.FieldDefs[ii];
    vFieldF   := DataSet.Fields.FieldByNumber(vFieldDef.FieldNo);
    if _GetFieldIsNull(PByte(vNullBitmap), ii) then
    begin
      if vFieldF <> nil then
        vFieldF.SetData(nil);
    end
    else if vFieldDef.DataType in StringFieldTypes then
    begin
      s := Stream.ReadAnsiString;
      if vFieldF <> nil then
        vFieldF.AsString := s;
    end
    else
    begin
      if vFieldDef.DataType in VarLenFieldTypes then
        L := Stream.ReadDWord
      else if vFieldF <> nil then
        L := vFieldF.DataSize
      else
        L := DefaultFieldSize[vFieldDef.DataType];

      SetLength(B, L);
      if L > 0 then
        Stream.ReadBuffer(B[0], L);

      if vFieldF <> nil then
        if vFieldF.DataType in BlobFieldTypes then
          RestoreBlobField(vFieldF, @B[0], L)
        else
          vFieldF.SetData(@B[0], False);
    end;
  end;
end;

initialization
  RegisterDatapacketReader(TBinaryDatapacketReaderEx, TDataPacketFormat.dfBinary);

end.

