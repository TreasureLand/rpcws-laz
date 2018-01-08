unit uDBExecutorSQLdb;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, db, uDBExecutor;

type

  { TSQLExecutorSQLdb }

  TSQLExecutorSQLdb = class (TSQLExecutor)
  private
    FQr : TSQLQuery;
  public
    constructor Create(aConnection: TSQLConnection; aTr: TSQLTransaction; const aSql: String);
    destructor Destroy; override;
    function GetParams : TParams; override;
    procedure ExecSQL; override;
  end;

implementation

//uses {%H-}uHelperSQLdb;

{ TSQLExecutorSQLdb }

constructor TSQLExecutorSQLdb.Create(aConnection: TSQLConnection;
  aTr: TSQLTransaction; const aSql: String);
begin
  inherited Create;
  FQr := TSQLQuery.Create(nil);
  FQr.DataBase    := aConnection;
  FQr.Transaction := aTr;
  FQr.SQL.Text    := aSql;
end;

destructor TSQLExecutorSQLdb.Destroy;
begin
  FQr.Free;
  inherited Destroy;
end;

function TSQLExecutorSQLdb.GetParams : TParams;
begin
  Result := FQr.Params;
end;

procedure TSQLExecutorSQLdb.ExecSQL;
begin
  FQr.ExecSQL;
end;

end.

