unit demo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Iphttpbroker, IpHtml, Forms, Controls, Graphics,
  Dialogs, StdCtrls, DBGrids, Menus, DbCtrls, Buttons, ExtCtrls, WSConnector,
  WSBaseQuery, BufDataset, db, sqldb, IBConnection;

type

  { TFormDemo }

  TFormDemo = class(TForm)
    BitBtn1: TBitBtn;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    DataSource1: TDataSource;
    DataSource2: TDataSource;
    DBGrid1: TDBGrid;
    DBGrid2: TDBGrid;
    Image1: TImage;
    lblTimeRequest: TLabel;
    lbRegistros: TLabel;
    WSClientDataset1: TWSClientDataset;
    WSConnector: TWSConnector;
    WSQuery1: TWSQuery;
    WSQuery1BAIRRO: TStringField;
    WSQuery1BAIRRO2: TStringField;
    WSQuery1BLOQUEIADEBITOS: TStringField;
    WSQuery1BLOQUEIALIMITE: TStringField;
    WSQuery1CELULAR: TStringField;
    WSQuery1CEP: TStringField;
    WSQuery1CEP2: TStringField;
    WSQuery1CIDADE: TStringField;
    WSQuery1CIDADE2: TStringField;
    WSQuery1CIVIL: TStringField;
    WSQuery1CODEMPRESA: TStringField;
    WSQuery1CODPESSOA: TStringField;
    WSQuery1CODUSUARIO: TStringField;
    WSQuery1CONJUGUE: TStringField;
    WSQuery1CONTATO: TStringField;
    WSQuery1CPF_CNPJ: TStringField;
    WSQuery1CREDITO1: TFloatField;
    WSQuery1CREDITO2: TFloatField;
    WSQuery1DATAADM: TDateField;
    WSQuery1DESC_AUTOMATICO: TFloatField;
    WSQuery1DTNASC: TDateField;
    WSQuery1DT_CAD: TDateField;
    WSQuery1EMAIL: TStringField;
    WSQuery1ENDERECO: TStringField;
    WSQuery1ENDERECO2: TStringField;
    WSQuery1ENDTRAB: TStringField;
    WSQuery1ESTADO: TStringField;
    WSQuery1ESTADO2: TStringField;
    WSQuery1FAX: TStringField;
    WSQuery1FISJUR: TStringField;
    WSQuery1FONE: TStringField;
    WSQuery1FONE2: TStringField;
    WSQuery1FONEC: TStringField;
    WSQuery1FONEM: TStringField;
    WSQuery1FONEP: TStringField;
    WSQuery1FONETRAB: TStringField;
    WSQuery1LIMITE: TFloatField;
    WSQuery1LOCALNASCIMENTO: TStringField;
    WSQuery1MAE: TStringField;
    WSQuery1MSN: TStringField;
    WSQuery1NOME: TStringField;
    WSQuery1OBS: TStringField;
    WSQuery1OPERADOR: TStringField;
    WSQuery1ORGAOEXP: TStringField;
    WSQuery1PAI: TStringField;
    WSQuery1PROF: TStringField;
    WSQuery1RAZAO: TStringField;
    WSQuery1RENDAMENSAL: TFloatField;
    WSQuery1RESP_COBRANCA: TStringField;
    WSQuery1RG_IE: TStringField;
    WSQuery1SEXO: TStringField;
    WSQuery1SITE: TStringField;
    WSQuery1SOMENTEAVISTA: TStringField;
    WSQuery1STATUS: TStringField;
    WSQuery1TEMPORESIDENCIA: TStringField;
    WSQuery1TIPO: TStringField;
    WSQuery1TIPO2: TStringField;
    WSQuery1TIPOIMOVEL: TStringField;
    WSQuery1TRABALHO: TStringField;
    WSQuery1UFORGAOEXP: TStringField;
    WSQuery2: TWSQuery;
    WSQuery2BRUTO: TFloatField;
    WSQuery2CODEMPRESA: TStringField;
    WSQuery2CODIGO: TStringField;
    WSQuery2CODUSUARIO: TStringField;
    WSQuery2DATA: TDateField;
    WSQuery2DATADIGITACAO: TDateField;
    WSQuery2FINO: TFloatField;
    WSQuery2HISTORICO: TStringField;
    WSQuery2HORARIO: TTimeField;
    WSQuery2IDCONTA: TStringField;
    WSQuery2MIL: TFloatField;
    WSQuery2SUBTOTAL: TBCDField;
    WSQuery2SUBTOTALF: TBCDField;
    WSQuery2SUBTOTALGERALF: TBCDField;
    WSQuery2SUBTOTALGERALR: TBCDField;
    WSQuery2TEOR: TFloatField;
    WSQuery2TIPO1: TStringField;
    WSQuery2TIPO2: TStringField;
    WSQuery2TIPO3: TStringField;
    WSQuery2TIPO4: TStringField;
    WSQuery2VALOR: TBCDField;
    WSQuery3: TWSQuery;
    procedure BitBtn1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure WSQuery1AfterScroll({%H-}DataSet: TDataSet);
    procedure WSQuery2AfterOpen({%H-}DataSet: TDataSet);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FormDemo: TFormDemo;

implementation

{$R *.lfm}

{ TFormDemo }

procedure TFormDemo.Button1Click(Sender: TObject);
begin
  //WSConector.WSServer.HostName:= '127.0.0.1';
  //WSConector.WSServer.Port:= '8084';
  WSConnector.Connected:=True;
end;

procedure TFormDemo.BitBtn1Click(Sender: TObject);
begin
  WSQuery1.ApplyUpdates;
end;

procedure TFormDemo.Button2Click(Sender: TObject);
begin
  WSConnector.Connected:=False;
end;

procedure TFormDemo.Button3Click(Sender: TObject);
begin
  WSQuery1.Prior;
end;

procedure TFormDemo.Button4Click(Sender: TObject);
begin
  WSQuery1.Open;
  lblTimeRequest.Caption:= WSConnector.ResponseTime;
end;

procedure TFormDemo.Button5Click(Sender: TObject);
begin
  WSQuery1.Next;
end;

procedure TFormDemo.Button6Click(Sender: TObject);
begin
  WSQuery2.Open;
  lblTimeRequest.Caption:= WSConnector.ResponseTime;
  //ShowMessage(IntToStr(Screen.FormCount));
end;

procedure TFormDemo.Button7Click(Sender: TObject);
var aStream : TMemoryStream;
begin
  try
    aStream:= TMemoryStream.Create;
    Image1.Picture.SaveToStream(aStream);
    aStream.Position:=0;
    with WSQuery3 do
    begin
      close;
      sql.Clear;
      sql.Add('insert into IMAGENS (imagem) values (:imagem) ');
      ParamByname('imagem').LoadFromStream(AStream,ftBlob);
      ExecSQL;
    end;
  finally
    aStream.Free;
    aStream:= nil;
  end;
end;

procedure TFormDemo.WSQuery1AfterScroll(DataSet: TDataSet);
begin
  {
  if (not WSQuery1.Active) and (WSQuery1CODPESSOA.IsNull) then
    Exit;

  WSQuery2.FilterOptions := [foCaseInsensitive];
  WSQuery2.Filtered      := False;
  WSQuery2.Filter        := 'codigo = '+QuotedStr(WSQuery1CODPESSOA.AsString);
  WSQuery2.Filtered      := True;
  }

  WSQuery2.Close;
  WSQuery2.SQL.Clear;
  wsquery2.SQL.Add('select * from conta where codigo = :codigo');
  WSQuery2.ParamByname('codigo').AsString:= WSQuery1CODPESSOA.AsString;
  wsquery2.Open;
  lblTimeRequest.Caption:= WSConnector.ResponseTime;

end;

procedure TFormDemo.WSQuery2AfterOpen(DataSet: TDataSet);
begin
  lbRegistros.Caption:= 'Registros: '+IntToStr(WSQuery2.RecordCount);
end;

end.

