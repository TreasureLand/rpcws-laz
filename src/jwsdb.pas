{*****************************************************************}
{                 JSON RPC Web Service Library                    }
{                                                                 }
{ by Jose Benedito - josebenedito@gmail.com                       }
{ www.jbsolucoes.net                                              }
{*****************************************************************}

unit jwsdb;

{$mode objfpc}{$H+}

interface

uses SysUtils, Forms, DBGrids, controls, sqldb;

procedure DBGridAdjustColumnWidths(grid: TDBGrid);

implementation

procedure DBGridAdjustColumnWidths(grid: TDBGrid);
const
  DEFBORDER = 10;
var
  temp, n: Integer;
  lmax: array [0..30] of Integer;
begin
  with Grid do
  begin
    Canvas.Font := Font;
    for n := 0 to Columns.Count - 1 do
      //if columns[n].visible then
      lmax[n] := Canvas.TextWidth(columns[n].Field.FieldName) + DEFBORDER;
    grid.DataSource.DataSet.First;
    while not grid.DataSource.DataSet.EOF do
    begin
      for n := 0 to Columns.Count - 1 do
      begin
        //if columns[n].visible then begin
        temp := Canvas.TextWidth(trim(Columns[n].Field.DisplayText)) + DEFBORDER;
        if temp > lmax[n] then lmax[n] := temp;
        //end; { if }
      end; {for}
      grid.DataSource.DataSet.Next;
    end; { while }
    grid.DataSource.DataSet.First;
    for n := 0 to Columns.Count - 1 do
      if lmax[n] > 0 then
        Columns[n].Width := lmax[n];
  end; { With }
end;

end.
