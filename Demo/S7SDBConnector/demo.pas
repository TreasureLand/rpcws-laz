unit Demo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, IBConnection, BufDataset, db, FileUtil, Forms,
  Controls, Graphics, Dialogs, DBGrids, StdCtrls, s7sConn, s7sdbjson, fpjson,
  jsonparser,jsonscanner;

type

  { TFrmDemo }

  TFrmDemo = class(TForm)
    Button1: TButton;
    Button2: TButton;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    Memo1: TMemo;
    SQLConnector1: TSQLConnector;
    SQLQuery1: TSQLQuery;
    SQLQuery1BAIRRO: TStringField;
    SQLQuery1BAIRRO2: TStringField;
    SQLQuery1BLOQUEIADEBITOS: TStringField;
    SQLQuery1BLOQUEIALIMITE: TStringField;
    SQLQuery1CELULAR: TStringField;
    SQLQuery1CEP: TStringField;
    SQLQuery1CEP2: TStringField;
    SQLQuery1CIDADE: TStringField;
    SQLQuery1CIDADE2: TStringField;
    SQLQuery1CIVIL: TStringField;
    SQLQuery1CODEMPRESA: TStringField;
    SQLQuery1CODPESSOA: TStringField;
    SQLQuery1CODUSUARIO: TStringField;
    SQLQuery1CONJUGUE: TStringField;
    SQLQuery1CONTATO: TStringField;
    SQLQuery1CPF_CNPJ: TStringField;
    SQLQuery1CREDITO1: TFloatField;
    SQLQuery1CREDITO2: TFloatField;
    SQLQuery1DATAADM: TDateField;
    SQLQuery1DESC_AUTOMATICO: TFloatField;
    SQLQuery1DTNASC: TDateField;
    SQLQuery1DT_CAD: TDateField;
    SQLQuery1EMAIL: TStringField;
    SQLQuery1ENDERECO: TStringField;
    SQLQuery1ENDERECO2: TStringField;
    SQLQuery1ENDTRAB: TStringField;
    SQLQuery1ESTADO: TStringField;
    SQLQuery1ESTADO2: TStringField;
    SQLQuery1FAX: TStringField;
    SQLQuery1FISJUR: TStringField;
    SQLQuery1FONE: TStringField;
    SQLQuery1FONE2: TStringField;
    SQLQuery1FONEC: TStringField;
    SQLQuery1FONEM: TStringField;
    SQLQuery1FONEP: TStringField;
    SQLQuery1FONETRAB: TStringField;
    SQLQuery1LIMITE: TFloatField;
    SQLQuery1LOCALNASCIMENTO: TStringField;
    SQLQuery1MAE: TStringField;
    SQLQuery1MSN: TStringField;
    SQLQuery1NOME: TStringField;
    SQLQuery1OBS: TStringField;
    SQLQuery1OPERADOR: TStringField;
    SQLQuery1ORGAOEXP: TStringField;
    SQLQuery1PAI: TStringField;
    SQLQuery1PROF: TStringField;
    SQLQuery1RAZAO: TStringField;
    SQLQuery1RENDAMENSAL: TFloatField;
    SQLQuery1RESP_COBRANCA: TStringField;
    SQLQuery1RG_IE: TStringField;
    SQLQuery1SEXO: TStringField;
    SQLQuery1SITE: TStringField;
    SQLQuery1SOMENTEAVISTA: TStringField;
    SQLQuery1STATUS: TStringField;
    SQLQuery1TEMPORESIDENCIA: TStringField;
    SQLQuery1TIPO: TStringField;
    SQLQuery1TIPO2: TStringField;
    SQLQuery1TIPOIMOVEL: TStringField;
    SQLQuery1TRABALHO: TStringField;
    SQLQuery1UFORGAOEXP: TStringField;
    SQLTransaction1: TSQLTransaction;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

const json : string = '{"METHODTYPE" : "wsmtFind", '+
                      '"SQL" : "select * from pessoas '+
                      'where codpessoa = :codpessoa ", '+
                      '"Pacotes" : "10","Posicao" : "0","Params" : '+
                      '{"codpessoa" : "101"}}';
var
  FrmDemo: TFrmDemo;
  S7SProvider  : TS7SProvider;
  S7SProvider2 : TS7SProvider;
  DBConnector  : TS7SDBConnector;         //996448508
implementation

{$R *.lfm}

{ TFrmDemo }

procedure TFrmDemo.Button1Click(Sender: TObject);
var jsonresult: string;
  ALstJson : TStringList;
begin
  ALstJson := TStringList.Create;
  ALstJson.LoadFromFile('LogJson.txt');
  DBConnector := TS7SDBConnector.create('');
  S7SProvider:= TS7SProvider.create(DBConnector);
  S7SProvider.TableName:='pessoas';
  jsonresult:= S7SProvider.Execute(ALstJson.Text);
end;

procedure TFrmDemo.Button2Click(Sender: TObject);
var DBJsonWriter : TDBJsonWriter;
    vjson: TJSONObject;
    ALstJson : TStringList;
begin
  //ukModify, ukInsert, ukDelete
  {
  ALstJson := TStringList.Create;
  ALstJson.LoadFromFile('JsonSaida.txt');
  SQLConnector1.Connected:=True;
  SQLQuery1.Open;
  vjson := (TJSONParser.Create(JsonApplyUpdate).Parse as TJSONObject);
  DBJsonWriter:= TDBJsonWriter.Create(vjson);

  DBJsonWriter.WiterProc(SQLQuery1,ukModify);
  DBJsonWriter.WiterProc(SQLQuery1,ukInsert);
  DBJsonWriter.WiterProc(SQLQuery1,ukDelete);
  Memo1.Text:= DBJsonWriter.JsonObject.AsJSON;
  }
end;

end.

