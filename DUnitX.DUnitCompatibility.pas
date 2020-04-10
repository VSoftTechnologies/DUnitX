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
{***************************************************************************}

unit DUnitX.DUnitCompatibility;

/// This unit provides basic compatibilty with DUnit Tests (not the framework!).

interface

{$I DUnitX.inc}

uses
  {$IFDEF USE_NS}
  System.SysUtils,
  {$ELSE}
  SysUtils,
  {$ENDIF}
  DUnitX.TestFramework;

type
  TTestCase = class
  protected
    procedure SetUp; virtual;
    procedure TearDown; virtual;
  public
    procedure Check(const condition: Boolean; const msg: string = '');deprecated  'Use DUnitX.Assert class';
    procedure CheckTrue(const condition: Boolean; const msg: string = '');deprecated  'Use DUnitX.Assert class';
    procedure CheckFalse(const condition: Boolean; const msg: string = '');deprecated  'Use DUnitX.Assert class';
    procedure CheckEquals(const expected, actual: extended; const msg: string = ''); overload;deprecated  'Use DUnitX.Assert class';
    procedure CheckEquals(const expected, actual: extended; const delta: extended; const msg: string = ''); overload;deprecated  'Use DUnitX.Assert class';
    procedure CheckEquals(const expected, actual: integer; const msg: string = ''); overload;deprecated  'Use DUnitX.Assert class';
    procedure CheckEquals(const expected, actual: Cardinal; const msg: string = ''); overload;deprecated  'Use DUnitX.Assert class';
    procedure CheckEquals(const expected, actual: int64; const msg: string = ''); overload;deprecated  'Use DUnitX.Assert class';
    procedure CheckEquals(const expected, actual: UnicodeString; const msg: string = ''); overload;deprecated  'Use DUnitX.Assert class';
{$IFNDEF NEXTGEN}
    procedure CheckEquals(const expected, actual: AnsiString; const msg: string = ''); overload;deprecated  'Use DUnitX.Assert class';
    procedure CheckEquals(const expected, actual: ShortString; const msg: string = ''); overload;deprecated  'Use DUnitX.Assert class';
{$ENDIF}
    procedure CheckEqualsString(const expected, actual: string; const msg: string = '');deprecated  'Use DUnitX.Assert class';
{$IFNDEF NEXTGEN}
    procedure CheckEquals(const expected, actual: WideString; const msg: string = ''); overload;deprecated  'Use DUnitX.Assert class';
    procedure CheckEqualsWideString(const expected, actual: WideString; const msg: string = '');deprecated  'Use DUnitX.Assert class';
{$ENDIF}
    procedure CheckEqualsMem(const expected, actual: pointer; const size:longword; const msg : string='');deprecated  'Use DUnitX.Assert class';
    procedure CheckEquals(const expected, actual: Boolean; const msg: string = ''); overload;deprecated  'Use DUnitX.Assert class';
    procedure CheckEqualsBin(const expected, actual: longword; const msg: string = ''; digits: integer=32);deprecated  'Use DUnitX.Assert class';
    procedure CheckEqualsHex(const expected, actual: longword; const msg: string = ''; digits: integer=8);deprecated  'Use DUnitX.Assert class';

    procedure CheckNotEquals(const expected, actual: integer; const msg: string = ''); overload;deprecated  'Use DUnitX.Assert class';
    procedure CheckNotEquals(const expected, actual: Cardinal; const msg: string = ''); overload;deprecated  'Use DUnitX.Assert class';
    procedure CheckNotEquals(const expected, actual: int64; const msg: string = ''); overload;deprecated  'Use DUnitX.Assert class';
    procedure CheckNotEquals(const expected: extended; const actual: extended; const delta: extended = 0; const msg: string = ''); overload;deprecated  'Use DUnitX.Assert class';
    procedure CheckNotEquals(const expected, actual: string; const msg: string = ''); overload;deprecated  'Use DUnitX.Assert class';
    procedure CheckNotEqualsString(const expected, actual: string; const msg: string = '');deprecated  'Use DUnitX.Assert class';
{$IFNDEF NEXTGEN}
    procedure CheckNotEquals(const expected, actual: WideString; const msg: string = ''); overload;deprecated  'Use DUnitX.Assert class';
    procedure CheckNotEqualsWideString(const expected, actual: WideString; const msg: string = '');deprecated  'Use DUnitX.Assert class';
{$ENDIF}
    procedure CheckNotEqualsMem(const expected, actual: pointer; const size:longword; const msg : string='');deprecated  'Use DUnitX.Assert class';
    procedure CheckNotEquals(const expected, actual: Boolean; const msg: string = ''); overload;deprecated  'Use DUnitX.Assert class';
    procedure CheckNotEqualsBin(const expected, actual: longword; const msg: string = ''; digits: integer=32);deprecated  'Use DUnitX.Assert class';
    procedure CheckNotEqualsHex(const expected, actual: longword; const msg: string = ''; digits: integer=8);deprecated  'Use DUnitX.Assert class';

    procedure CheckNotNull(const obj :IUnknown; const msg :string = ''); overload;deprecated  'Use DUnitX.Assert class';
    procedure CheckNull(const obj: IUnknown; const msg: string = ''); overload;deprecated  'Use DUnitX.Assert class';
    procedure CheckSame(const expected, actual: IInterface; const msg: string = ''); overload;deprecated  'Use DUnitX.Assert class';
    procedure CheckSame(const expected, actual: TObject; const msg: string = ''); overload;deprecated  'Use DUnitX.Assert class';

    procedure CheckNotNull(const obj: TObject; const msg: string = ''); overload;deprecated  'Use DUnitX.Assert class';
    procedure CheckNull(const obj: TObject; const msg: string = ''); overload;deprecated  'Use DUnitX.Assert class';

    procedure CheckException(const AMethod: TTestMethod; const AExceptionClass: ExceptClass; const msg :string = '');deprecated  'Use DUnitX.Assert class';
    procedure CheckEquals(const expected, actual: TClass; const msg: string = ''); overload;deprecated  'Use DUnitX.Assert class';
    procedure CheckInherits(const expected, actual: TClass; const msg: string = ''); overload;deprecated  'Use DUnitX.Assert class';
    procedure CheckIs(const AObject :TObject; const AClass: TClass; const msg: string = ''); overload;deprecated 'Use DUnitX.Assert class';

    procedure Status(const msg : string);
    //Redirect WriteLn to our loggers.
    procedure WriteLn(const msg : string);overload;
    procedure WriteLn;overload;
    [Setup]
    procedure TestSetupMethod;
    [TearDown]
    procedure TestTeardownMethod;
  end;




implementation

uses
  DUnitX.TestRunner;


//Borrowed from DUnit.

function IntToBin(const value, digits: longword): string;
const
  ALL_32_BIT_0 = '00000000000000000000000000000000';
var
  counter: integer;
  pow:     integer;
begin
  Result := ALL_32_BIT_0;
  SetLength(Result, digits);
  pow := 1 shl (digits - 1);
  if value <> 0 then
    for counter := 0 to digits - 1 do
    begin
      if (value and (pow shr counter)) <> 0 then
      begin
      	{$IFDEF NEXTGEN}
        Result.Remove(counter, 1);
        Result.Insert(counter, '1');
	{$ELSE}
	Result[counter+1] := '1';
	{$ENDIF}
      end;
    end;
end;

procedure TTestCase.Check(const condition: Boolean; const msg: string);
begin
  Assert.IsTrue(condition,msg);
end;

procedure TTestCase.CheckTrue(const condition: Boolean; const msg: string);
begin
  Assert.IsTrue(condition,msg);
end;

procedure TTestCase.SetUp;
begin

end;

procedure TTestCase.Status(const msg: string);
begin
  Self.WriteLn(msg);
end;

procedure TTestCase.TearDown;
begin

end;

procedure TTestCase.TestSetupMethod;
begin
  Self.SetUp;
end;

procedure TTestCase.TestTeardownMethod;
begin
  Self.TearDown;
end;

procedure TTestCase.WriteLn(const msg: string);
var
  runner : ITestRunner;
begin
  runner := TDUnitXTestRunner.GetActiveRunner;
  if runner <> nil then
    runner.Log(TLogLevel.Information,msg)
  else
    System.Writeln(msg);
end;

procedure TTestCase.WriteLn;
begin
  Self.WriteLn('');
end;

procedure TTestCase.CheckFalse(const condition: Boolean; const msg: string);
begin
  Assert.IsFalse(condition,msg);
end;

procedure TTestCase.CheckEquals(const expected, actual: extended; const msg: string);
begin
  Assert.AreEqual(expected,actual,0,msg);
end;

procedure TTestCase.CheckEquals(const expected, actual: extended; const delta: extended; const msg: string);
begin
  Assert.AreEqual(expected,actual,delta,msg);
end;

procedure TTestCase.CheckEquals(const expected, actual: integer; const msg: string);
begin
{$IFNDEF DELPHI_XE_DOWN}
  Assert.AreEqual<integer>(expected,actual,msg);
{$ELSE}
  Assert.IsTrue(expected = actual, msg);
{$ENDIF}
end;

procedure TTestCase.CheckEquals(const expected, actual: Cardinal; const msg: string);
begin
{$IFNDEF DELPHI_XE_DOWN}
  Assert.AreEqual<Cardinal>(expected,actual,msg);
{$ELSE}
  Assert.IsTrue(expected = actual, msg);
{$ENDIF}
end;

procedure TTestCase.CheckEquals(const expected, actual: int64; const msg: string);
begin
{$IFNDEF DELPHI_XE_DOWN}
  Assert.AreEqual<Int64>(expected,actual,msg);
{$ELSE}
  Assert.IsTrue(expected = actual, msg);
{$ENDIF}
end;

procedure TTestCase.CheckEquals(const expected, actual: UnicodeString; const msg: string);
begin
{$IFNDEF DELPHI_XE_DOWN}
  Assert.AreEqual(expected, actual, msg);
{$ELSE}
  Assert.IsTrue(expected = actual, msg);
{$ENDIF}
end;

{$IFNDEF NEXTGEN}
procedure TTestCase.CheckEquals(const expected, actual: AnsiString; const msg: string);
begin
{$IFNDEF DELPHI_XE_DOWN}
  Assert.AreEqual<AnsiString>(expected,actual,msg);
{$ELSE}
  Assert.IsTrue(expected = actual, msg);
{$ENDIF}
end;

procedure TTestCase.CheckEquals(const expected, actual: ShortString; const msg: string);
begin
{$IFNDEF DELPHI_XE_DOWN}
  Assert.AreEqual<ShortString>(expected,actual,msg);
{$ELSE}
  Assert.IsTrue(expected = actual, msg);
{$ENDIF}
end;
{$ENDIF}

procedure TTestCase.CheckEqualsString(const expected, actual: string; const msg: string);
begin
  Assert.AreEqual(expected,actual,true,msg);
end;

{$IFNDEF NEXTGEN}
procedure TTestCase.CheckEquals(const expected, actual: WideString; const msg: string);
begin
  Assert.AreEqual(expected,actual,true,msg);
end;

procedure TTestCase.CheckEqualsWideString(const expected, actual: WideString; const msg: string);
begin
  Assert.AreEqual(expected,actual,true,msg);
end;
{$ENDIF}

procedure TTestCase.CheckEqualsMem(const expected, actual: pointer; const size:longword; const msg : string = '');
begin
  Assert.AreEqualMemory(expected,actual,size,msg);
end;

procedure TTestCase.CheckEquals(const expected, actual: Boolean; const msg: string);
begin
{$IFNDEF DELPHI_XE_DOWN}
  Assert.AreEqual<Boolean>(expected,actual,msg);
{$ELSE}
  Assert.IsTrue(expected = actual, msg);
{$ENDIF}
end;

procedure TTestCase.CheckEqualsBin(const expected, actual: longword; const msg: string; digits: integer);
begin
{$IFNDEF DELPHI_XE_DOWN}
  Assert.AreEqual(IntToBin(expected, digits), IntToBin(actual, digits),msg);
{$ELSE}
  Assert.IsTrue(IntToBin(expected, digits) = IntToBin(actual, digits), msg);
{$ENDIF}
end;

procedure TTestCase.CheckEqualsHex(const expected, actual: longword; const msg: string; digits: integer);
begin
  Assert.AreEqual(IntToHex(expected, digits), IntToHex(actual, digits),true,msg);
end;

procedure TTestCase.CheckNotEquals(const expected, actual: integer; const msg: string);
begin
{$IFNDEF DELPHI_XE_DOWN}
  Assert.AreNotEqual<integer>(expected,actual,msg);
{$ELSE}
  Assert.IsFalse(expected = actual, msg);
{$ENDIF}
end;

procedure TTestCase.CheckNotEquals(const expected, actual: Cardinal; const msg: string);
begin
{$IFNDEF DELPHI_XE_DOWN}
  Assert.AreNotEqual<Cardinal>(expected,actual,msg);
{$ELSE}
  Assert.IsFalse(expected = actual, msg);
{$ENDIF}
end;

procedure TTestCase.CheckNotEquals(const expected, actual: int64; const msg: string);
begin
{$IFNDEF DELPHI_XE_DOWN}
  Assert.AreNotEqual<int64>(expected,actual,msg);
{$ELSE}
  Assert.IsFalse(expected = actual, msg);
{$ENDIF}
end;

procedure TTestCase.CheckNotEquals(const expected: extended; const actual: extended; const delta: extended; const msg: string);
begin
  Assert.AreNotEqual(expected,actual,delta,msg);
end;

procedure TTestCase.CheckNotEquals(const expected, actual: string; const msg: string);
begin
  Assert.AreNotEqual(expected,actual,true,msg);
end;

procedure TTestCase.CheckNotEqualsString(const expected, actual: string; const msg: string);
begin
  Assert.AreNotEqual(expected,actual,true,msg);
end;

{$IFNDEF NEXTGEN}
procedure TTestCase.CheckNotEquals(const expected, actual: WideString; const msg: string);
begin
  Assert.AreNotEqual(expected,actual,true,msg);
end;

procedure TTestCase.CheckNotEqualsWideString(const expected, actual: WideString; const msg: string);
begin
  Assert.AreNotEqual(expected,actual,true,msg);
end;
{$ENDIF}

procedure TTestCase.CheckNotEqualsMem(const expected, actual: pointer; const size:longword; const msg:string='');
begin
  Assert.AreNotEqualMemory(expected,actual,size,msg);
end;

procedure TTestCase.CheckNotEquals(const expected, actual: Boolean; const msg: string);
begin
{$IFNDEF DELPHI_XE_DOWN}
  Assert.AreNotEqual<boolean>(expected,actual,msg);
{$ELSE}
  Assert.IsFalse(expected = actual, msg);
{$ENDIF}
end;

procedure TTestCase.CheckNotEqualsBin(const expected, actual: longword; const msg: string; digits: integer);
begin
{$IFNDEF DELPHI_XE_DOWN}
  Assert.AreNotEqual<longword>(expected,actual,msg);
{$ELSE}
  Assert.IsTrue(expected = actual, msg);
{$ENDIF}
end;

procedure TTestCase.CheckNotEqualsHex(const expected, actual: longword; const msg: string; digits: integer);
begin
  Assert.AreNotEqual(IntToHex(expected, digits), IntToHex(actual, digits),true,msg);
end;

procedure TTestCase.CheckNotNull(const obj :IUnknown; const msg :string);
begin
  Assert.IsNotNull(obj,msg);
end;

procedure TTestCase.CheckNull(const obj: IUnknown; const msg: string);
begin
  Assert.IsNull(obj,msg);
end;

procedure TTestCase.CheckSame(const expected, actual: IUnknown; const msg: string);
begin
{$IFNDEF DELPHI_XE_DOWN}
  Assert.AreEqual<IInterface>(expected,actual,msg);
{$ELSE}
  Assert.IsTrue(expected = actual, msg);
{$ENDIF}
end;

procedure TTestCase.CheckSame(const expected, actual: TObject; const msg: string);
begin
{$IFNDEF DELPHI_XE_DOWN}
  Assert.AreEqual<TObject>(expected,actual,msg);
{$ELSE}
  Assert.IsTrue(expected = actual, msg);
{$ENDIF}
end;

procedure TTestCase.CheckNotNull(const obj: TObject; const msg: string);
begin
  Assert.IsNotNull(obj,msg);
end;

procedure TTestCase.CheckNull(const obj: TObject; const msg: string);
begin
  Assert.IsNull(obj,msg);
end;

procedure TTestCase.CheckException(const AMethod: TTestMethod; const AExceptionClass: ExceptClass; const msg :string);
begin
  Assert.WillRaise(AMethod,AExceptionClass,msg);
end;

procedure TTestCase.CheckEquals(const expected, actual: TClass; const msg: string);
begin
  Assert.AreEqual(expected,actual,msg);
end;

procedure TTestCase.CheckInherits(const expected, actual: TClass; const msg: string);
begin
  Assert.InheritsFrom(expected,actual,msg);
end;

procedure TTestCase.CheckIs(const AObject :TObject; const AClass: TClass; const msg: string);
begin
  Assert.InheritsFrom(AObject.ClassType,AClass,msg);
end;

end.
