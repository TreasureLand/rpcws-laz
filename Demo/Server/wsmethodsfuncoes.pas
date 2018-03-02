{*****************************************************************}
{                 JSON RPC Web Service Library                    }
{                                                                 }
{ by Fernando Pasqueto fpasqueto@gmail.com                        }
{                                                                 }
{*****************************************************************}

unit wsmethodsFuncoes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, jwstypes, fpjson, jsonparser, servermethods, jwsjson, db,
  s7sconn, S7SProvider, RUtils, jwsconsts, wsfuncoes, ACBrIBPTax, LConvEncoding,
  FileUtil;

type

  { TJWSMethodsFuncoes }

  TJWSMethodsFuncoes = class(TJWSServerMethods)

  private

  published
    function DataHoraAtual(): TJSONStringType;
    function getIdTable(): TJSONStringType;
    function atualizaibpt() : TJSONStringType;
  end;

implementation

{ TWSMethods }

function TJWSMethodsFuncoes.DataHoraAtual: TJSONStringType;
var
  o: TJSONObject;
begin
  o := TJSONObject.Create;
  try
    o.Add('data', Date);
    o.Add('hora', Time);
    Result := o.AsJSON;
  finally
    o.Free;
  end;
end;

function TJWSMethodsFuncoes.getIdTable(): TJSONStringType;
var ATabela, AUltimo_id, ACampo : String;
begin
  try
    ATabela := Request.Args[0].AsString;
    ACampo := Request.Args[1].AsString;
    AUltimo_id:= wsfuncoes.getIdTable(ATabela,ACampo);
    Result:= '{"id":'+AUltimo_id+'}';
  except
    on e: exception do
    begin
      raise Exception.Create('Erro ao gegar id: '+e.Message);
    end;
  end;
end;

function TJWSMethodsFuncoes.atualizaibpt(): TJSONStringType;
var AQuery       : TS7SQuery;
    ADBConnector : TS7SDBConnector;
    ACBrIBPTax1  : TACBrIBPTax;
    //AJsonObject  : String;
    //vJson        : TJSONObject;
    ADescricao   : String;
    ATabela      : String;
    AUF          : String;
    AId, I, K    : Integer;
    ALista       : TStringList;
    AListaAux    : TStringList;
begin
  ADBConnector       := TS7SDBConnector.create('');
  AQuery             := TS7SQuery.create(nil);
  AQuery.DBConnector := ADBConnector;
  ACBrIBPTax1        := TACBrIBPTax.Create(nil);
  ACBrIBPTax1.IsUTF8 := True;
  //AJsonObject        := Request.Args[0].AsJSON;
  //vJson              := (TJSONParser.Create(AJsonObject).Parse as TJSONObject);
  ATabela            := Request.Args[0].AsString;
  AUF                := Request.Args[1].AsString;
  ALista             := TStringList.Create;
  AListaAux          := TStringList.Create;
  try
    try
      ALista.AddText(CP1252ToUTF8(ATabela));
      k := ALista.Count;
      AListaAux.Clear;
      for i:= 0 to k - 1 do
      begin  //verificar linhas em branco
        if ALista.Strings[i] = '' then
        begin
          AListaAux.Add(IntToStr(i));
        end;
      end;
      for i:= AListaAux.Count - 1 downto 0 do
      begin //apaga linhas em branco da ultima para a primeira para nao perder o indice
        ALista.Delete(StrToInt(AListaAux.Strings[i]));
      end;
      { ativado apenaz para depuracao
      if FileExistsUTF8('Tabela.txt') then
        DeleteFile('Tabela.txt');
      ALista.SaveToFile('Tabela.txt');
      }
      ADBConnector.startTrans;
      if ACBrIBPTax1.AbrirTabela(ALista) then
      begin
        with AQuery do
        begin
          close;
          sql.Clear;
          sql.Add('delete from cadibpt');
          sql.Add('where uf = :uf');
          ParamByName('uf').AsString:= AUF;
          ExecSQL;
        end;
        k:= ACBrIBPTax1.Itens.Count;
        for I := 0 to k - 1 do
        begin
          {
          AListaAux.Clear;
          AListaAux.Add(ACBrIBPTax1.Itens[I].NCM+' - '+ADescricao);
          AListaAux.SaveToFile('Erro_NCM_'+IntToStr(i)+'.txt');
          }
          with AQuery do
          begin
            close;
            sql.Clear;
            sql.Add('insert into cadibpt ');
            sql.Add('(ncm,descricao,ex,tabela,uf,aliqfednacional,');
            sql.Add('aliqfedimportado,aliqestadual,aliqmunicipal)');
            sql.Add('values');
            sql.Add('(:ncm,:descricao,:ex,:tabela,:uf,:aliqfednacional,');
            sql.Add(':aliqfedimportado,:aliqestadual,:aliqmunicipal)');
            ADescricao:= copy(CP1252ToUTF8(ACBrIBPTax1.Itens[I].Descricao),1,255);
            //ParamByName('id').AsInteger             := AId;
            ParamByName('ncm').AsString             := ACBrIBPTax1.Itens[I].NCM;
            ParamByName('descricao').AsString       := ADescricao;
            ParamByName('ex').AsInteger             := StrToIntDef(ACBrIBPTax1.Itens[I].Excecao,0);
            ParamByName('tabela').AsInteger         := Integer(ACBrIBPTax1.Itens[I].Tabela);
            ParamByName('uf').AsString              :=  AUF;
            ParamByName('aliqfednacional').AsFloat  := ACBrIBPTax1.Itens[I].FederalNacional;
            ParamByName('aliqfedimportado').AsFloat := ACBrIBPTax1.Itens[I].FederalImportado;
            ParamByName('aliqestadual').AsFloat     := ACBrIBPTax1.Itens[I].Estadual;
            ParamByName('aliqmunicipal').AsFloat    := ACBrIBPTax1.Itens[I].Municipal;
            ExecSQL;
          end;
        end;
        Result:= JSON_RESULT_OK;
        AQuery.Close;
        ADBConnector.CloseTrans;
      end
      else Result:= JSON_RESULT_NO;
      AQuery.Destroy;
      ADBConnector.Destroy;
    except
      on e: exception do
      begin
        ADBConnector.CancelTrans;
        Exception.Create('Erro ao gravar Tabela IBPT: '+e.Message);
      end;
    end;
  finally
    ACBrIBPTax1.Free;
    FreeAndNil(ALista);
    FreeAndNil(AListaAux);
  end;
end;

end.

