unit DUnitX.TestNameParser;

interface

uses
  DUnitX.Types;

{$I DUnitX.inc}

type
  TTestNameParser = class
  private
    class function GetTestName(const arg : string; var index : integer) : string;
    class function GetSeparator(const arg : string; index : integer) : integer;
  public
    class function Parse(const arg : string) : TStringArray;
  end;

implementation

uses
  SysUtils,
  Classes,
  Generics.Collections;

{ TTestNameParser }

class function TTestNameParser.GetSeparator(const arg: string; index: integer): integer;
var
  nest : integer;
begin
  nest := 0;
  result := -1;
  while index < Length(arg) do
  begin
    case arg[index] of
      ',' :
      begin
        if nest = 0 then
          exit(index);
      end;
      '"' :
      begin
        Inc(index);
        while (index < Length(arg)) and (arg[index] <> '"')  do
          Inc(index);

      end;
      '(','<' : Inc(nest);
      ')','>' : Dec(nest);
    end;
    Inc(index)
  end;

end;

class function TTestNameParser.GetTestName(const arg: string; var index: integer): string;
var
  sep : integer;
begin
  result := '';
  sep := GetSeparator(arg,index);
  if sep > 0 then
  begin
    result := Trim(Copy(arg,index, sep - index));
    index := sep +1;
  end
  else
  begin
    result := Trim(Copy(arg,index, Length(arg)));
    index := Length(arg);
  end;

end;

class function TTestNameParser.Parse(const arg: string): TStringArray;
var
  sList : TStringList;
  i : integer;
  sName : string;
begin
  sList := TStringList.Create;
  try
    if arg <> '' then
    begin
      i := 1;
      while i < Length(arg) do
      begin
        sName := GetTestName(arg,i);
        if sName <> '' then
          sList.Add(sName);
      end;
    end;

    SetLength(result, sList.Count);
    for i := 0 to sList.Count - 1 do
      result[i] := sList[i];
  finally
    sList.Free;
  end;
end;

end.
