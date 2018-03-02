unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  WSConnector, WSBaseQuery, fpjson;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    WSConnector1: TWSConnector;
    WSQuery1: TWSQuery;
    procedure Button1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;
  Cliente: TWSClientMethods;
implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var s: TJSONObject;
begin
  Cliente:= TWSClientMethods.Create(Self);
  Cliente.Connector:= WSConnector1;
  Cliente.MethodModule:='pessoas';
  Cliente.MethodName:='HoraAtual';
  s := Cliente.JSONCall([]);
  try
    //ShowMessage(s.AsJSON);
    ShowMessage(DateTimeToStr(s.Floats['Data']+s.Floats['hora']));
  finally
    s.Free;
  end;
end;

end.

