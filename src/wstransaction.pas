unit WSTransaction;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, jsonscanner, LResources, Forms,
  Controls, Graphics, Dialogs, jwsclient, RUtils;

type

  { TWSTransaction }
  TWSTransaction=class(TPersistent)
    private
      FAddListCmd: String;
      FListCmd : TStringList;
      FCountCmd : Integer;
      procedure setListCmd(AValue:String);
    protected
    public
      constructor Create; //override;
      Destructor destroy; //override;
      function getListCmd: String;
    published
      property AddListCmd: String read GetListCmd write setListCmd;
  end;

implementation

{ TWSTransaction }

constructor TWSTransaction.Create;//(AOwner: TComponent);
begin
  FListCmd:= TStringList.Create;
  FCountCmd := -1;
  //inherited Create(AOwner);
end;

destructor TWSTransaction.destroy;
begin
  FListCmd.Free;
  //inherited destroy;
end;

procedure TWSTransaction.setListCmd(AValue:String);
begin
  IF NOT Assigned(FListCmd) THEN
    FListCmd := TStringList.Create;
  if FCountCmd = -1 then
    FCountCmd:= 0
  else Inc(FCountCmd);
  FListCmd.Add('"COMMAND'+IntToStr(FCountCmd)+'":'+AValue);
end;

function TWSTransaction.getListCmd: String;
var ACommands: String; i: Integer;
begin
  ACommands:= '';
  for i := 0 to FListCmd.Count - 1 do
    ACommands:= ACommands + FListCmd.Strings[i]+',';
  SetLength(ACommands, Length(ACommands) - 1);
  ACommands:= '{"COMMANDS":{'+ACommands+'}}';
  FListCmd.Clear;
  FCountCmd:=-1;
  Result:= ACommands;
end;

end.

