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
    Button8: TButton;
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
    WSQuery2: TWSQuery;
    WSQuery3: TWSQuery;
    WSQuery4: TWSQuery;
    procedure BitBtn1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
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

procedure TFormDemo.Button8Click(Sender: TObject);
begin
  WSConnector.AutoCommit:=False;
  with WSQuery3 do
  begin
    close;
    sql.Clear;
    sql.Add('insert into pessoas');
    sql.Add('(cod_pessoa,cod_empresa,nome,fantasia)');
    sql.Add('values');
    sql.Add('(:cod_pessoa,:cod_empresa,:nome,:fantasia)');
    ParamByname('cod_pessoa').AsInteger:=9999;
    ParamByname('cod_empresa').AsInteger:=1;
    ParamByname('nome').AsString:= 'FERNANDO FERREIRA GOMES NASCIMENTO';
    ParamByname('fantasia').AsString:= 'PASQUETO';
    ExecSQL;
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
  {
  WSQuery2.Close;
  WSQuery2.SQL.Clear;
  wsquery2.SQL.Add('select * from conta where codigo = :codigo');
  WSQuery2.ParamByname('codigo').AsString:= WSQuery1CODPESSOA.AsString;
  wsquery2.Open;
  lblTimeRequest.Caption:= WSConnector.ResponseTime;
  }

end;

procedure TFormDemo.WSQuery2AfterOpen(DataSet: TDataSet);
begin
  lbRegistros.Caption:= 'Registros: '+IntToStr(WSQuery2.RecordCount);
end;

end.

