unit s7sutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

Function DataValida(OUT Dado: string): Boolean;
Function RemoveAcentos(Str:String): String;
function TiraAcento(str:String):String;
function SoNumero(const Valor: string): Boolean;
function RetornaSoNumero(const Valor: string): string;
function RetornaSoLetra(const Valor: string): string;
function ifNul(Condicao: String  ): String ;
function Padl(Texto: string; Tamanho: integer; Caracter:String) : string;

implementation


Function DataValida(OUT Dado: string): Boolean;
var
  SalvaFormato,
  DataExterna: string;
  DataInterna: TDateTime;
  Separador: string;
  Dia,Mes,Ano:string;
begin
  Result := True;
  Separador := DateSeparator;
  while Pos(Separador, Dado) > 0 do
    Delete(Dado, Pos(Separador, Dado), 1);
  Dado:= RetornaSoNumero(Dado); //retorna somente numeros
  if Length(Dado) = 6 then
    Dado := Copy(Dado, 1, 2) + Separador +
      Copy(Dado, 3, 2) + Separador + '19' +
      Copy(Dado, 5, 2)
  else
    if Length(Dado) = 8 then
      Dado := Copy(Dado, 1, 2) + Separador +
        Copy(Dado, 3, 2) + Separador +
        Copy(Dado, 5, 4)
    else
      Result := False;
  if Result then
  begin
    Dia:= copy(Dado,1,2);
    Mes:= copy(Dado,4,2);
    Ano:= copy(Dado,7,4);
  end;
  if Result then //testa dia
  begin
    if not (strtoint(Dia) in [1..31])  then result := false;
  end;
  if Result then //testa mes
  begin
    if not (strtoint(Mes) in [1..12])  then result := false;
  end;
  if Result then
  begin //verifica se dia e valido para o mes
    if (strtoint(Mes) in [4,6,9,11]) then
    begin
      if strtoint(Dia) = 31 then Result:= false;
    end
    else if (strtoint(Mes) = 2) then
    begin  //mes de fevereiro so pode ter 29 dias em ano bisexto
      if strtoint(Dia) = 29 then
      begin
        If IsLeapYear(StrToInt(Ano)) then
        begin
          result:= false;
        end;
      end;
    end;
  end;
  if Result then
  begin
    SalvaFormato := ShortDateFormat;
    DataInterna := 0;
    try
      ShortDateFormat := 'd' + Separador + 'm' + Separador + 'y';
      DataInterna := StrToDate(Dado);
    except
      on EConvertError do
      begin
        Result := False;
        ShortDateFormat := SalvaFormato;
      end;
    end;
    if Result then
    begin
      try
        ShortDateFormat := 'dd' + Separador + 'mm' + Separador + 'yyyy';
        DataExterna := DateToStr(DataInterna);
      except
        on EConvertError do
        begin
          Result := False;
          ShortDateFormat := SalvaFormato;
        end;
      end;
      if Result and (DataExterna <> Dado) then
        Result := False;
    end;
    ShortDateFormat := SalvaFormato;
  end;
end;

function TiraAcento(str:String):String;
const
AccentedChars :array[0..25] of string = ('á','à','ã','â','ä','é','è','ê','ë','í','ì','ï','î','ó','ò','õ','ô','ö','ú','ù','ü','û','ç','ñ','ý','ÿ');
NormalChars :array[0..25] of string = ('a','a','a','a','a','e','e','e','e','i','i','i','i','o','o','o','o','o','u','u','u','u','c','n','y','y');
var
i: Integer;
begin
Result := str;
for i := 0 to 25 do
Result := StringReplace(Result, AccentedChars[i], NormalChars[i], [rfReplaceAll]);
end;

Function RemoveAcentos(Str:String): String;
Const ComAcento = 'àâêôûãõáéíóúçüÀÂÊÔÛÃÕÁÉÍÓÚÇÜ';
      SemAcento = 'aaeouaoaeioucuAAEOUAOAEIOUCU';
Var
x : Integer;
Begin
For x := 1 to Length(Str) do
  Begin
  if Pos(Str[x],ComAcento)<>0 Then
  begin
  Str[x] := SemAcento[Pos(Str[x],ComAcento)];
  end;
  end;
Result := Str;
end;

function SoNumero(const Valor: string): Boolean;
var
  i  : Integer;
  C  : Char;
  Ret: string;
begin
  Result:= true;
  if (trim(Valor) = '') then
    Result := false
  else
  begin
    for i := 1 to Length(Valor) do
    begin
      C := Char(Valor[i]);
      if C in ['0'..'9'] then Ret := Ret + C
      else
      begin
        result := false;
        break;
      end;
    end;
  end
end;

function RetornaSoNumero(const Valor: string): string;
var
  i  : Integer;
  C  : Char;
  Ret: string;
begin
  if (trim(Valor) = '') then
    Result := ''
  else
  begin
    for i := 1 to Length(Valor) do
    begin
      C := Char(Valor[i]);
      if C in ['0'..'9'] then Ret := Ret + C;
    end;
    Result := ifNul(Ret);
  end
end;

function RetornaSoLetra(const Valor: string): string;
var
  i  : Integer;
  C  : Char;
  Ret: string;
begin
  if (trim(Valor) = '') then
    Result := ''
  else
  begin
    for i := 1 to Length(Valor) do
    begin
      C := Char(Valor[i]);
      if C in ['A'..'Z','a'..'z'] then
        Ret := Ret + C;
    end;
    Result := Ret;
  end
end;

function ifNul(Condicao: String  ): String ;
begin
  if (Trim(Condicao) = '') then ifNul := '0'
  else
  ifNul := Condicao;
end;

function Padl(Texto: string; Tamanho: integer; Caracter:String) : string;
var Cont: integer;
begin
  Tamanho:=Tamanho-Length(Texto);
  Cont:=1;
  while Cont<=Tamanho do
  begin
    Texto:='0'+Texto;
    Cont:=Cont+1;
  end;
  Padl := Texto
end;

end.

