unit s7stypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,db,RUtils,BufDataset,fpjson,jsonparser,s7sutils;

type
  TDBProtocol = (dbFirebird,dbPostGresql,dbMySql,dbSqLite);
  TWSMethodType = (wsmNone, wsmtOpen,wsmtExecSQL,wsmtFind,wsmtApplyUpdate,wsmtDelete);
  TTypeSearch = (tsAutomatic,tsStart,tsEnding,tsContaining,tsEqual);
  TDataTypeID = (dtString,dtInteger);
const cstPacotes : Integer = 10;
  function StrToMethodType(const AMethodType:String):TWSMethodType;
  function getMethodTypeToStr(AMethodType:TWSMethodType): String;
  function strToTypeSearch(const ATypeSearch: String): TTypeSearch;
  function getTypeSearchToStr(const ATypeSearch: TTypeSearch): String;
  Function DatasetToBase64(ADataset:TCustomBufDataset):String;
  procedure Base64ToDataset(ADataset:TCustomBufDataset;AJson:TJSONObject);
  procedure Base64ToDataset2(ADataset:TCustomBufDataset;AJson:string);
  procedure JsonItemToParam(AParam:TParam;AJSONItem:TJSONData);
  function GetSQLClause(AListSource: TDataSet;
                        ASQLExecute, AKeyField, AField: String;
                        AText: variant;
                        ATypeSearch: TTypeSearch):string;
implementation

function StrToMethodType(const AMethodType: String): TWSMethodType;
begin
  case AMethodType of
    'wsmtOpen':Result:= wsmtOpen;
    'wsmtExecSQL':Result:= wsmtExecSQL;
    'wsmtFind':Result:= wsmtFind;
    'wsmtApplyUpdate':Result:= wsmtApplyUpdate;
    else raise Exception.CreateFmt('MethodType inválido: "%s"', [AMethodType]);
  end;
end;

function getMethodTypeToStr(AMethodType:TWSMethodType): String;
begin
  case AMethodType of
    wsmtOpen: result:= 'wsmtOpen';
    wsmtExecSQL: Result:= 'wsmtExecSQL';
    wsmtFind: result:= 'wsmtFind';
    wsmtApplyUpdate: result:= 'wsmtApplyUpdate';
  end;
end;

function strToTypeSearch(const ATypeSearch: String): TTypeSearch;
begin
  case ATypeSearch of
    'tsAutomatic': Result:= tsAutomatic;
    'tsStart': Result:= tsStart;
    'tsEnding': Result:= tsEnding;
    'tsContaining': Result:= tsContaining;
    'tsEqual': Result:= tsEqual;
    else raise Exception.CreateFmt('TypeSearch inválido: "%s"', [ATypeSearch]);
  end;
end;

function getTypeSearchToStr(const ATypeSearch: TTypeSearch): String;
begin
  case ATypeSearch of
    tsAutomatic: Result:= 'tsAutomatic';
    tsStart: Result:= 'tsStart';
    tsEnding: Result:= 'tsEnding';
    tsContaining: Result:= 'tsContaining';
    tsEqual: Result:= 'tsEqual';
  end;
end;

function DatasetToBase64(ADataset: TCustomBufDataset): String;
var AStreamIn,AStreamOut: TStream;
begin
  try
    AStreamIn:= TMemoryStream.Create;
    AStreamOut:= TMemoryStream.Create;
    ADataset.SaveToStream(AStreamIn);
    AStreamIn.Position:=0;
    //RUtils.ZCompressStream(AStreamIn,AStreamOut);
    AStreamOut.Position:=0;
    //Result := '{"base64":"'+RUtils.StreamToBase64(AStreamOut)+'"}';
    Result := '{"base64":"'+RUtils.StreamToBase64(AStreamIn)+'"}';
  finally
    FreeAndNil(AStreamIn);
    FreeAndNil(AStreamOut);
  end;
end;

procedure Base64ToDataset(ADataset: TCustomBufDataset; AJson: TJSONObject);
var
    vBin    : TJSONData;
    AStream : TMemoryStream;
begin
  try
    //vjson := (TJSONParser.Create(AJson).Parse as TJSONObject);;
    vBin := ajson.Find('base64');
    if vBin <> nil then
    begin
      AStream:= TMemoryStream.Create;
      Base64ToStream(vBin.AsJSON, AStream);
      AStream.Position:=0;
      ADataset.LoadFromStream(AStream);
      //ADataset.First;
    end;
  finally
    //FreeAndNil(vjson);
    //FreeAndNil(vBin);
    FreeAndNil(AStream);
  end;

end;

procedure Base64ToDataset2(ADataset:TCustomBufDataset;AJson:string);
var vjson   : TJSONObject;
begin
  vjson := (TJSONParser.Create(AJson).Parse as TJSONObject);;
  try
    Base64ToDataset(ADataset, vjson);
  finally
    FreeAndNil(vjson);
  end;
end;

procedure JsonItemToParam(AParam:TParam;AJSONItem:TJSONData);
var S : string;
    F : TMemoryStream;
    ABlob:Boolean;
begin
  try
    ABlob:= False;
    if (AJSONItem is TJSONFloatNumber) then
      AParam.Asfloat := AJSONItem.Asfloat
    else if (AJSONItem is TJSONIntegerNumber) then
      AParam.AsInteger := AJSONItem.Asinteger
    else if (AJSONItem is TJSONInt64Number) then
      AParam.AsLargeInt := AJSONItem.AsInt64
    else if (AJSONItem is TJSONString) then
    begin
      if copy(AJSONItem.AsString ,1,8) = '#base64#' then
      begin
        ABlob:=True;
        S := AJSONItem.AsString;
        Delete(S,1,8);
        F := TMemoryStream.create;
        Base64ToStream(s,f);
        f.Position:=0;
        AParam.LoadFromStream(F,ftblob);
      end
      else if copy(AJSONItem.AsString ,1,10) = '#nulldate#' then
      begin
        AParam.Clear;
      end
      else
      begin
        AParam.AsString := StringReplace(AJSONItem.AsString,'#LineEnding#',#13#10,[rfReplaceAll, rfIgnoreCase]);
      end;
    end
    else if (AJSONItem is TJSONBoolean) then
      AParam.AsBoolean := AJSONItem.AsBoolean
    else if (AJSONItem is TJSONNull) then
      AParam.Clear
    else   //se não for niguem aí de riba
      AParam.AsString := AJSONItem.AsString;
  finally
    if ABlob then
      FreeAndNil(F);
  end;
end;

function GetSQLClause(AListSource: TDataSet;
                    ASQLExecute, AKeyField, AField: String;
                    AText: variant;
                    ATypeSearch: TTypeSearch):string;
var LLocFieldType: TFieldType;
    LTypeSearch  : TTypeSearch;
    i : integer;
    lSql,lFieldName,lSQLExecute: string;
    ADate: String;
    ASoNumero: boolean;
begin
  IF (UpperCase(trim(AField)) <> 'AUTOMATIC') THEN
  begin
    LLocFieldType := AListSource.FieldByName(AField).DataType;
  end;
  LTypeSearch := ATypeSearch;
  lSql:='';
  result := '';

  ASoNumero:= SoNumero(AText);
  if UpperCase(AField) = UpperCase(AKeyField) then
  begin
    if ((ASoNumero)
    and (LLocFieldType IN [ftInteger, ftWord,ftFloat, ftCurrency, ftBCD, ftFMTBcd, ftLargeint])) then
    begin
      LTypeSearch:= tsEqual;
      lFieldName:= AKeyField;
    end
    else LTypeSearch:= tsAutomatic;
  end;
  if trim(AText) <> '' then
  begin
    case LTypeSearch of
      tsAutomatic:
      begin
        for i := 0 to AListSource.Fields.Count-1 do
        begin
          LLocFieldType := AListSource.Fields.Fields[i].DataType;
          lFieldName    := AListSource.Fields.Fields[i].FieldName;
          lSql          := '';
          IF (LLocFieldType IN [ftWideString, ftString]) THEN
            lSql := 'UPPER('+lFieldName+') LIKE '+quotedstr('%'+AText+'%')
          else IF (LLocFieldType IN [ftInteger, ftWord,ftFloat, ftCurrency, ftBCD, ftFMTBcd]) THEN
          begin //SoNumero( //ftDate,  ftTime, ftDateTime
            if SoNumero(AText) then
              lSql := lFieldName+' = '+AText;
          end
          else if LLocFieldType in [ftDate, ftDateTime] then
          begin
            ADate:= AText;
            if DataValida(ADate) then
              lSql := lFieldName+' = '+ADate;
          end;
          if trim(lSql) <> '' then
          begin
            lSql:= '('+lSql+')';
            if trim(lSQLExecute) = '' then lSQLExecute := lSql
            else lSQLExecute := lSQLExecute+' OR '+lSql;
          end;
        end;
      end;
      tsStart:
      begin
        lSQLExecute := AField + ' like '+quotedstr(AText+'%');
      end;
      tsEnding:
      begin
        lSQLExecute := AField + ' like '+quotedstr('%'+AText);
      end;
      tsContaining:
      begin
        lSQLExecute := AField + ' like '+quotedstr('%'+AText+'%')
      end;
      tsEqual:
      BEGIN
        IF (LLocFieldType IN [ftWideString,ftString]) THEN lSQLExecute := lFieldName + ' = '+quotedstr(AText)
        ELSE IF (LLocFieldType IN [ftLargeint,ftInteger, ftWord,ftFloat, ftCurrency, ftBCD, ftFMTBcd]) THEN
        begin
          if SoNumero(AText) then lSQLExecute := lFieldName + ' = ' + AText
          else lSQLExecute := lFieldName + ' = '+quotedstr(AText);
        end;
      end;
    end;
    if trim(ASQLExecute) = '' then exit;
    if trim(lSQLExecute) = '' then exit;
    if (pos('WHERE',trim(uppercase(ASQLExecute))) > 0) then
      ASQLExecute:= ASQLExecute + ' and (' + lSQLExecute +')'
    else ASQLExecute:= ASQLExecute + ' where ' + lSQLExecute;
  end;
end;

end.

