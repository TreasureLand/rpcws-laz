unit uDBExecutor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db;

type

  //{ TDBConector }
//
  //TDBConector = class
  //public
  //  function CreateSQLExecutor(): TSQLExecutor; virtual; abstract;
  //  procedure ExecuteStream(aStream: TStream; const aTabName: string);
  //end;        { TDBStreamReader }


  { TSQLExecutor }

  TSQLExecutor = class
  public
    //procedure SetSQL(); virtual; abstract;
    function GetParams(): TParams; virtual; abstract;
    procedure ExecSQL(); virtual; abstract;
  end;

  TSQLExecutorFactory = function(const aSQL: string): TSQLExecutor of object; //virtual; abstract;


implementation



{ TDBConector }

//procedure TDBConector.ExecuteStream(aStream: TStream; const aTabName: string);
//var
//  r : TGReaderStream;
//begin
//  with TDBStreamReader.Create(Self) do
//  try
//    r := TGReaderStream.Create(aStream);
//    try
//      Execute(r, aTabName);
//    finally
//      r.fre
//    end;
//  finally
//    Free;
//  end;
//end;

{ TDBStreamReader }

//constructor TDBStreamReader.Create(aConnector : TDBConector);
//begin
//  inherited Create;
//  Fcn := aConnector;
//end;

end.

