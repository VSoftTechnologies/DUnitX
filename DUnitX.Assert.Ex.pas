{***************************************************************************}
{                                                                           }
{           DUnitX                                                          }
{                                                                           }
{           Copyright (C) 2016 Vincent Parrett                              }
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
{***************************************************************************}

unit DUnitX.Assert.Ex;

// This unit should contain DUnitX specific Assert commands and mechanisms
// in order to keep DUnitX.Assert framework neutral and usable in other
// test frameworks such as DUnit.

interface

{$I DUnitX.inc}

uses
  DUnitX.Assert,
  DUnitX.ComparableFormat;

type
  Assert = class(DUnitX.Assert.Assert)
  private
    class procedure FailStrCompare(const expected, actual: string; const compformat: TDUnitXComparableFormatClass = nil; const message : string = ''; const errorAddrs : pointer = nil);
  public
    class procedure AreEqual(const expected, actual : string; const compformat: TDUnitXComparableFormatClass; const ignoreCase : boolean; const message : string = ''); overload;
    class procedure AreEqual(const expected, actual : string; const compformat: TDUnitXComparableFormatClass; const message : string = ''); overload;

    //Reintroduced from Assert to throw ETestFailureStrCompare
    class procedure AreEqual(const expected : string; const actual : string; const ignoreCase : boolean; const message : string = ''); reintroduce; overload;
    //Reintroduced from Assert to throw ETestFailureStrCompare
    class procedure AreEqual(const expected : string; const actual : string; const message : string = ''); reintroduce; overload;
  end;

implementation

uses
  {$IFDEF USE_NS}
  System.SysUtils,
  {$ELSE}
  SysUtils,
  {$ENDIF}
  DUnitX.ResStrs,
  DUnitX.Exceptions;

class procedure Assert.AreEqual(const expected, actual: string; const compformat: TDUnitXComparableFormatClass; const ignoreCase: boolean; const message: string);
begin
  DoAssert;
  if ignoreCase then
  begin
    if not SameText(expected,actual) then
      FailStrCompare(expected, actual, compformat, message, ReturnAddress);
  end
  else if not SameStr(expected,actual) then
    FailStrCompare(expected, actual, compformat, message, ReturnAddress);
end;

class procedure Assert.AreEqual(const expected, actual: string; const compformat: TDUnitXComparableFormatClass; const message: string);
begin
  AreEqual(expected, actual, compformat, IgnoreCaseDefault, message);
end;

class procedure Assert.AreEqual(const expected, actual : string; const ignoreCase : boolean; const message: string);
begin
  AreEqual(expected, actual, nil, ignoreCase, message);
end;

class procedure Assert.AreEqual(const expected : string; const actual : string; const message : string);
begin
  Assert.AreEqual(expected, actual, IgnoreCaseDefault, message);
end;

class procedure Assert.FailStrCompare(const expected, actual: string; const compformat : TDUnitXComparableFormatClass; const message: string; const errorAddrs: pointer);
begin
  //If we have been given a return then use it. (makes the exception appear on level above in the stack)
  if errorAddrs <> nil then
    raise ETestFailureStrCompare.Create(expected, actual, message, compformat) at errorAddrs
  else
    //Otherwise use the return address we can currently get to for where to raise the exception
    raise ETestFailureStrCompare.Create(expected, actual, message, compformat) at ReturnAddress;
end;

end.
