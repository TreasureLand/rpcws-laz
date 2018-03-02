unit wsfuncoes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, s7sConn, S7SProvider;

 function getIdTable(ATabela:String;const ACampo : String = 'id'):String;

implementation

function getIdTable(ATabela: String; const ACampo: String = 'id'): String;
var AQuery: TS7SQuery;
    AUltimo_id:Integer;
    //ADtultimo_id : TDateTime;
    ADtultimo_id : String;
    ADBConnector : TS7SDBConnector;
begin
  ShortDateFormat:='yyyy-mm-dd';
  //ADtultimo_id := StrToDateTime(DateToStr(date)+' '+TimeToStr(Time));
  ADtultimo_id := DateToStr(date)+' '+TimeToStr(Time);
  ADBConnector := TS7SDBConnector.create('');
  AQuery := TS7SQuery.create(nil);
  AQuery.DBConnector := ADBConnector;
  try
    try
      ADBConnector.startTrans;
      with AQuery do
      begin
        SQL.Clear;
        SQL.Add('select ultimo_id from cfgsequencia_unica ');
        SQL.Add('where tabela = '+quotedstr(ATabela));
        SQL.Add('and campo = '+QuotedStr(ACampo));
        open;
        if EOF then
        begin
          Close;
          SQL.Clear;
          SQL.Add('insert into cfgsequencia_unica ');
          SQL.Add('(tabela,campo,ultimo_id,dtultimo_id)');
          SQL.Add('values');
          SQL.Add('('+QuotedStr(ATabela)+','+QuotedStr(ACampo)+','+IntToStr(1)+','+quotedstr(ADtultimo_id)+')');
          ExecSQL;
          AUltimo_id:= 1;
        end
        else
        begin
          AUltimo_id:= FieldByName('ultimo_id').AsLargeInt + 1;
          Close;
          SQL.Clear;
          SQL.Add('update cfgsequencia_unica set ');
          SQL.Add('ultimo_id = '+IntToStr(AUltimo_id));
          SQL.Add(',dtultimo_id = '+QuotedStr(ADtultimo_id));
          SQL.Add('where tabela = '+QuotedStr(ATabela));
          SQL.Add('and campo = '+QuotedStr(ACampo));
          ExecSQL;
        end;
      end;
      Result:= IntToStr(AUltimo_id);
      AQuery.Close;
      ADBConnector.CloseTrans;
    except
      on e: exception do
      begin
        ADBConnector.CancelTrans;
        raise;
      end;
    end;
  finally
    AQuery.Destroy;
    ADBConnector.Destroy;
  end;

end;

end.

