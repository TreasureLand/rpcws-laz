unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, BufDataset, sqldb, memds, FileUtil, Forms, Controls,
  Graphics, Dialogs, StdCtrls, DBGrids, WSConnector, WSBaseQuery;

type

  { TForm1 }

  TForm1 = class(TForm)
    BufDataset1: TBufDataset;
    Button1: TButton;
    Button2: TButton;
    CheckBox1: TCheckBox;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    elocCidade: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    MemDataset1: TMemDataset;
    WSClientDataset1: TWSClientDataset;
    WSConnector1: TWSConnector;
    WSQuery1: TWSQuery;
    WSQuery1ATIVO: TStringField;
    WSQuery1CIDADE: TStringField;
    WSQuery1CODIBGE: TStringField;
    WSQuery1ID: TLargeintField;
    WSQuery1TSTE1: TWideStringField;
    WSQuery1UF: TStringField;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure elocCidadeKeyPress(Sender: TObject; var Key: char);
  private
    { private declarations }
  public
    { public declarations }
    function CalcResponseTime(AMilliseconds: LongWord): String;
  end;

var
  Form1: TForm1;
  WSClientMethods : TWSClientMethods;
Var TimeIni, TimeFim : LongWord;
implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.elocCidadeKeyPress(Sender: TObject; var Key: char);
begin
  if key = #13 then
  begin
    with WSQuery1 do
    begin
      params.Clear;
      Close;
      QueryBase64:= CheckBox1.Checked;
      SQL.Clear;
      sql.Add('select * from cadcidade');
      if elocCidade.Text <> '' then
      begin
        sql.Add('where cidade like :cidade');
        ParamByname('cidade').AsString:= '%'+elocCidade.Text+'%';
      end;
      TimeIni := GetTickCount;
      open;
      TimeFim        := GetTickCount;
      Label2.Caption := CalcResponseTime(TimeFim - TimeIni);
      DataSource1.DataSet:= WSQuery1;
    end;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin

 WSClientMethods:= TWSClientMethods.Create(Self);
 WSClientMethods.Connector:= WSConnector1;
 try
   WSClientMethods.Method:='QuerySql';
   TimeIni := GetTickCount;
   WSClientMethods.DatasetCall('QuerySql','','cadcidade',MemDataset1,True);
   TimeFim        := GetTickCount;
   Label2.Caption := CalcResponseTime(TimeFim - TimeIni);
   DataSource1.DataSet:= MemDataset1;
 finally
   FreeAndNil(WSClientMethods);
 end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
 WSClientMethods:= TWSClientMethods.Create(Self);
 WSClientMethods.Connector:= WSConnector1;
 try
   WSClientMethods.Method:='QuerySql';
   TimeIni := GetTickCount;
   WSClientMethods.DatasetCall('QuerySql','','cadcidade',BufDataset1,true);
   TimeFim        := GetTickCount;
   Label2.Caption := CalcResponseTime(TimeFim - TimeIni);
   DataSource1.DataSet:= MemDataset1;
 finally
   FreeAndNil(WSClientMethods);
 end;
end;

function TForm1.CalcResponseTime(AMilliseconds: LongWord): String;
var Mil, Sec, Min : Integer;
begin
  Sec    := AMilliseconds div 1000;
  Mil    := AMilliseconds mod 1000;
  Min    := Sec div 60;
  Sec    := Sec mod 60;
  Result := IntToStr(Min)+':'+ IntTostr(Sec)+':'+IntToStr(Mil)+'ms';
end;

end.

