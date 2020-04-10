{***************************************************************************}
{                                                                           }
{           DUnitX                                                          }
{                                                                           }
{           Copyright (C) 2015 Vincent Parrett & Contributors               }
{                                                                           }
{           vincent@finalbuilder.com                                        }
{           http://www.finalbuilder.com                                     }
{                                                                           }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{ This unit is largely a port of the NUnit TestNameParser class             }
{ Copyright © 2012-2014 Charlie Poole                                       }
{ License  : http://nunit.org/index.php?p=vsTestAdapterLicense&r=2.6.3      }
{                                                                           }
{***************************************************************************}


unit DUnitX.TestNameParser;

interface

{$I DUnitX.inc}

type
  TTestNameParser = class
  private
    class function GetTestName(const arg : string; var index : integer) : string;
    class function GetSeparator(const arg : string; index : integer) : integer;
  public
    class function Parse(const arg : string) : TArray<string>;
  end;

implementation

uses
  {$IFDEF USE_NS}
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
  {$ELSE}
  Classes,
  SysUtils,
  Generics.Collections,
  {$ENDIF}
  DUnitX.Utils;

{ TTestNameParser }

class function TTestNameParser.GetSeparator(const arg: string; index: integer): integer;
var
  nest : integer;
begin
  nest := 0;
  result := -1;
  {$IFNDEF NEXTGEN}
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
  {$ELSE}
  while index < arg.Length - 1 do
  begin
    case arg.Chars[index] of
      ',' :
      begin
        if nest = 0 then
          exit(index);
      end;
      '"' :
      begin
        Inc(index);
        while (index < arg.Length - 1) and (arg.Chars[index] <> '"')  do
          Inc(index);

      end;
      '(','<' : Inc(nest);
      ')','>' : Dec(nest);
    end;
    Inc(index)
  end;  
  {$ENDIF}
end;

class function TTestNameParser.GetTestName(const arg: string; var index: integer): string;
var
  sep : integer;
begin
  result := '';
  sep := GetSeparator(arg,index);
  {$IFNDEF NEXTGEN}  
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
  {$ELSE}
  if sep > -1 then
  begin
    result := arg.Substring(index, sep - index).Trim;
    index := sep + 1;
  end
  else
  begin
    result := arg.Substring(index).Trim;
    index := arg.Length - 1;
  end; 
  {$ENDIF}
end;

class function TTestNameParser.Parse(const arg: string): TArray<string>;
var
  sList : TList<string>;
  i : integer;
  sName : string;
begin
  sList := TList<string>.Create;
  try
    if arg <> '' then
    begin
      {$IFNDEF NEXTGEN}  
      i := 1;
      while i < Length(arg) do
      {$ELSE}
      i := 0;
      while i < arg.Length - 1 do
      {$ENDIF}
      begin
        sName := GetTestName(arg,i);
        if sName <> '' then
          sList.Add(sName);
      end;
    end;
    result := TListStringUtils.ToArray(sList);
  finally
    sList.Free;
  end;
end;

end.
