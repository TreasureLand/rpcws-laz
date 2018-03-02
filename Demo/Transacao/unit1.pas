unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  WSConnector, WSBaseQuery;

type

  { TFDemo }

  TFDemo = class(TForm)
    Button1: TButton;
    WSConnector: TWSConnector;
    WSQuery1: TWSQuery;
    procedure Button1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FDemo: TFDemo;

implementation

{$R *.lfm}

{ TFDemo }

procedure TFDemo.Button1Click(Sender: TObject);
begin
  WSConnector.AutoCommit:=False;
  with WSQuery1 do
  begin
    close;
    sql.Clear;
    sql.Add('insert into TRANS');
    sql.Add('(nome,fantasia)');
    sql.Add('values');
    sql.Add('(:nome,:fantasia)');
    ParamByname('nome').AsString:= 'FERNANDO FERREIRA GOMES NASCIMENTO';
    ParamByname('fantasia').AsString:= 'PASQUETO';
    ExecSQL;
    close;
    sql.Clear;
    sql.Add('insert into TRANS');
    sql.Add('(nome,fantasia)');
    sql.Add('values');
    sql.Add('(:nome,:fantasia)');
    ParamByname('nome').AsString:= 'FERNANDO FERREIRA GOMES NASCIMENTO';
    ParamByname('fantasia').AsString:= 'PASQUETO';
    ExecSQL;
    close;
    sql.Clear;
    sql.Add('insert into TRANS');
    sql.Add('(nome,fantasia)');
    sql.Add('values');
    sql.Add('(:nome,:fantasia)');
    ParamByname('nome').AsString:= 'FERNANDO FERREIRA GOMES NASCIMENTO';
    ParamByname('fantasia').AsString:= 'PASQUETO';
    ExecSQL;
    close;
    sql.Clear;
    sql.Add('insert into TRANS');
    sql.Add('(nome,fantasia)');
    sql.Add('values');
    sql.Add('(:nome,:fantasia)');
    ParamByname('nome').AsString:= 'FERNANDO FERREIRA GOMES NASCIMENTO';
    ParamByname('fantasia').AsString:= 'PASQUETO';
    ExecSQL;
  end;
  WSConnector.Commit;
end;

end.

