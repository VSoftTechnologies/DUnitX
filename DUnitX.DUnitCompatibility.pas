{***************************************************************************}
{                                                                           }
{           DUnitX                                                          }
{                                                                           }
{           Copyright (C) 2013 Vincent Parrett                              }
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

uses
  DUnitX.TestFramework,
  SysUtils;

type
  TTestCase = class
  protected
    procedure SetUp; virtual;
    procedure TearDown; virtual;
  public
    procedure Check(condition: Boolean; msg: string = '');deprecated  'Use DUnitX.Assert class';
    procedure CheckTrue(condition: Boolean; msg: string = '');deprecated  'Use DUnitX.Assert class';
    procedure CheckFalse(condition: Boolean; msg: string = '');deprecated  'Use DUnitX.Assert class';
    procedure CheckEquals(expected, actual: extended; msg: string = ''); overload;deprecated  'Use DUnitX.Assert class';
    procedure CheckEquals(expected, actual: extended; delta: extended; msg: string = ''); overload;deprecated  'Use DUnitX.Assert class';
    procedure CheckEquals(expected, actual: integer; msg: string = ''); overload;deprecated  'Use DUnitX.Assert class';
    procedure CheckEquals(expected, actual: Cardinal; msg: string = ''); overload;deprecated  'Use DUnitX.Assert class';
    procedure CheckEquals(expected, actual: int64; msg: string = ''); overload;deprecated  'Use DUnitX.Assert class';
    procedure CheckEquals(expected, actual: UnicodeString; msg: string = ''); overload;deprecated  'Use DUnitX.Assert class';
    procedure CheckEquals(expected, actual: AnsiString; msg: string = ''); overload;deprecated  'Use DUnitX.Assert class';
    procedure CheckEquals(expected, actual: ShortString; msg: string = ''); overload;deprecated  'Use DUnitX.Assert class';
    procedure CheckEqualsString(expected, actual: string; msg: string = '');deprecated  'Use DUnitX.Assert class';
    procedure CheckEquals(expected, actual: WideString; msg: string = ''); overload;deprecated  'Use DUnitX.Assert class';
    procedure CheckEqualsWideString(expected, actual: WideString; msg: string = '');deprecated  'Use DUnitX.Assert class';
    procedure CheckEqualsMem(expected, actual: pointer; size:longword; msg:string='');deprecated  'Use DUnitX.Assert class';
    procedure CheckEquals(expected, actual: Boolean; msg: string = ''); overload;deprecated  'Use DUnitX.Assert class';
    procedure CheckEqualsBin(expected, actual: longword; msg: string = ''; digits: integer=32);deprecated  'Use DUnitX.Assert class';
    procedure CheckEqualsHex(expected, actual: longword; msg: string = ''; digits: integer=8);deprecated  'Use DUnitX.Assert class';

    procedure CheckNotEquals(expected, actual: integer; msg: string = ''); overload;deprecated  'Use DUnitX.Assert class';
    procedure CheckNotEquals(expected, actual: Cardinal; msg: string = ''); overload;deprecated  'Use DUnitX.Assert class';
    procedure CheckNotEquals(expected, actual: int64; msg: string = ''); overload;deprecated  'Use DUnitX.Assert class';
    procedure CheckNotEquals(expected: extended; actual: extended; delta: extended = 0; msg: string = ''); overload;deprecated  'Use DUnitX.Assert class';
    procedure CheckNotEquals(expected, actual: string; msg: string = ''); overload;deprecated  'Use DUnitX.Assert class';
    procedure CheckNotEqualsString(expected, actual: string; msg: string = '');deprecated  'Use DUnitX.Assert class';
    procedure CheckNotEquals(const expected, actual: WideString; msg: string = ''); overload;deprecated  'Use DUnitX.Assert class';
    procedure CheckNotEqualsWideString(const expected, actual: WideString; msg: string = '');deprecated  'Use DUnitX.Assert class';
    procedure CheckNotEqualsMem(expected, actual: pointer; size:longword; msg:string='');deprecated  'Use DUnitX.Assert class';
    procedure CheckNotEquals(expected, actual: Boolean; msg: string = ''); overload;deprecated  'Use DUnitX.Assert class';
    procedure CheckNotEqualsBin(expected, actual: longword; msg: string = ''; digits: integer=32);deprecated  'Use DUnitX.Assert class';
    procedure CheckNotEqualsHex(expected, actual: longword; msg: string = ''; digits: integer=8);deprecated  'Use DUnitX.Assert class';

    procedure CheckNotNull(obj :IUnknown; msg :string = ''); overload;deprecated  'Use DUnitX.Assert class';
    procedure CheckNull(obj: IUnknown; msg: string = ''); overload;deprecated  'Use DUnitX.Assert class';
    procedure CheckSame(expected, actual: IInterface; msg: string = ''); overload;deprecated  'Use DUnitX.Assert class';
    procedure CheckSame(expected, actual: TObject; msg: string = ''); overload;deprecated  'Use DUnitX.Assert class';

    procedure CheckNotNull(obj: TObject; msg: string = ''); overload;deprecated  'Use DUnitX.Assert class';
    procedure CheckNull(obj: TObject; msg: string = ''); overload;deprecated  'Use DUnitX.Assert class';

    procedure CheckException(AMethod: TTestMethod; AExceptionClass: ExceptClass; msg :string = '');deprecated  'Use DUnitX.Assert class';
    procedure CheckEquals(  expected, actual: TClass; msg: string = ''); overload;deprecated  'Use DUnitX.Assert class';
    procedure CheckInherits(expected, actual: TClass; msg: string = ''); overload;deprecated  'Use DUnitX.Assert class';
    procedure CheckIs(AObject :TObject; AClass: TClass; msg: string = ''); overload;deprecated 'Use DUnitX.Assert class';

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
      Result[counter+1] := '1';
  end;
end;

procedure TTestCase.Check(condition: Boolean; msg: string);
begin
  Assert.IsTrue(condition,msg);
end;

procedure TTestCase.CheckTrue(condition: Boolean; msg: string);
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
    runner.Log(TLogLevel.ltInformation,msg)
  else
    System.Writeln(msg);
end;

procedure TTestCase.WriteLn;
begin
  Self.WriteLn('');
end;

procedure TTestCase.CheckFalse(condition: Boolean; msg: string);
begin
  Assert.IsFalse(condition,msg);
end;

procedure TTestCase.CheckEquals(expected, actual: extended; msg: string);
begin
  Assert.AreEqual(expected,actual,0,msg);
end;

procedure TTestCase.CheckEquals(expected, actual: extended; delta: extended; msg: string);
begin
  Assert.AreEqual(expected,actual,delta,msg);
end;

procedure TTestCase.CheckEquals(expected, actual: integer; msg: string);
begin
{$IFDEF DELPHI_XE_UP}
  Assert.AreEqual<integer>(expected,actual,msg);
{$ELSE}
  Assert.IsTrue(expected = actual, msg);
{$ENDIF}
end;

procedure TTestCase.CheckEquals(expected, actual: Cardinal; msg: string);
begin
{$IFDEF DELPHI_XE_UP}
  Assert.AreEqual<Cardinal>(expected,actual,msg);
{$ELSE}
  Assert.IsTrue(expected = actual, msg);
{$ENDIF}
end;

procedure TTestCase.CheckEquals(expected, actual: int64; msg: string);
begin
{$IFDEF DELPHI_XE_UP}
  Assert.AreEqual<Int64>(expected,actual,msg);
{$ELSE}
  Assert.IsTrue(expected = actual, msg);
{$ENDIF}
end;

procedure TTestCase.CheckEquals(expected, actual: UnicodeString; msg: string);
begin
{$IFDEF DELPHI_XE_UP}
  Assert.AreEqual(expected, actual, msg);
{$ELSE}
  Assert.IsTrue(expected = actual, msg);
{$ENDIF}
end;

procedure TTestCase.CheckEquals(expected, actual: AnsiString; msg: string);
begin
{$IFDEF DELPHI_XE_UP}
  Assert.AreEqual<AnsiString>(expected,actual,msg);
{$ELSE}
  Assert.IsTrue(expected = actual, msg);
{$ENDIF}
end;

procedure TTestCase.CheckEquals(expected, actual: ShortString; msg: string);
begin
{$IFDEF DELPHI_XE_UP}
  Assert.AreEqual<ShortString>(expected,actual,msg);
{$ELSE}
  Assert.IsTrue(expected = actual, msg);
{$ENDIF}
end;

procedure TTestCase.CheckEqualsString(expected, actual: string; msg: string);
begin
  Assert.AreEqual(expected,actual,true,msg);
end;

procedure TTestCase.CheckEquals(expected, actual: WideString; msg: string);
begin
  Assert.AreEqual(expected,actual,true,msg);
end;

procedure TTestCase.CheckEqualsWideString(expected, actual: WideString; msg: string);
begin
  Assert.AreEqual(expected,actual,true,msg);
end;

procedure TTestCase.CheckEqualsMem(expected, actual: pointer; size:longword; msg:string='');
begin
  Assert.AreEqualMemory(expected,actual,size,msg);
end;

procedure TTestCase.CheckEquals(expected, actual: Boolean; msg: string);
begin
{$IFDEF DELPHI_XE_UP}
  Assert.AreEqual<Boolean>(expected,actual,msg);
{$ELSE}
  Assert.IsTrue(expected = actual, msg);
{$ENDIF}
end;

procedure TTestCase.CheckEqualsBin(expected, actual: longword; msg: string; digits: integer);
begin
{$IFDEF DELPHI_XE_UP}
  Assert.AreEqual(IntToBin(expected, digits), IntToBin(actual, digits),msg);
{$ELSE}
  Assert.IsTrue(IntToBin(expected, digits) = IntToBin(actual, digits), msg);
{$ENDIF}
end;

procedure TTestCase.CheckEqualsHex(expected, actual: longword; msg: string; digits: integer);
begin
  Assert.AreEqual(IntToHex(expected, digits), IntToHex(actual, digits),true,msg);
end;

procedure TTestCase.CheckNotEquals(expected, actual: integer; msg: string);
begin
{$IFDEF DELPHI_XE_UP}
  Assert.AreNotEqual<integer>(expected,actual,msg);
{$ELSE}
  Assert.IsTrue(expected = actual, msg);
{$ENDIF}
end;

procedure TTestCase.CheckNotEquals(expected, actual: Cardinal; msg: string);
begin
{$IFDEF DELPHI_XE_UP}
  Assert.AreNotEqual<Cardinal>(expected,actual,msg);
{$ELSE}
  Assert.IsTrue(expected = actual, msg);
{$ENDIF}
end;

procedure TTestCase.CheckNotEquals(expected, actual: int64; msg: string);
begin
{$IFDEF DELPHI_XE_UP}
  Assert.AreNotEqual<int64>(expected,actual,msg);
{$ELSE}
  Assert.IsTrue(expected = actual, msg);
{$ENDIF}
end;

procedure TTestCase.CheckNotEquals(expected: extended; actual: extended; delta: extended; msg: string);
begin
  Assert.AreNotEqual(expected,actual,delta,msg);
end;

procedure TTestCase.CheckNotEquals(expected, actual: string; msg: string);
begin
  Assert.AreNotEqual(expected,actual,true,msg);
end;

procedure TTestCase.CheckNotEqualsString(expected, actual: string; msg: string);
begin
  Assert.AreNotEqual(expected,actual,true,msg);
end;

procedure TTestCase.CheckNotEquals(const expected, actual: WideString; msg: string);
begin
  Assert.AreNotEqual(expected,actual,true,msg);
end;

procedure TTestCase.CheckNotEqualsWideString(const expected, actual: WideString; msg: string);
begin
  Assert.AreNotEqual(expected,actual,true,msg);
end;

procedure TTestCase.CheckNotEqualsMem(expected, actual: pointer; size:longword; msg:string='');
begin
  Assert.AreNotEqualMemory(expected,actual,size,msg);
end;

procedure TTestCase.CheckNotEquals(expected, actual: Boolean; msg: string);
begin
{$IFDEF DELPHI_XE_UP}
  Assert.AreNotEqual<boolean>(expected,actual,msg);
{$ELSE}
  Assert.IsTrue(expected = actual, msg);
{$ENDIF}
end;

procedure TTestCase.CheckNotEqualsBin(expected, actual: longword; msg: string; digits: integer);
begin
{$IFDEF DELPHI_XE_UP}
  Assert.AreNotEqual<longword>(expected,actual,msg);
{$ELSE}
  Assert.IsTrue(expected = actual, msg);
{$ENDIF}
end;

procedure TTestCase.CheckNotEqualsHex(expected, actual: longword; msg: string; digits: integer);
begin
  Assert.AreNotEqual(IntToHex(expected, digits), IntToHex(actual, digits),true,msg);
end;

procedure TTestCase.CheckNotNull(obj :IUnknown; msg :string);
begin
  Assert.IsNotNull(obj,msg);
end;

procedure TTestCase.CheckNull(obj: IUnknown; msg: string);
begin
  Assert.IsNull(obj,msg);
end;

procedure TTestCase.CheckSame(expected, actual: IUnknown; msg: string);
begin
{$IFDEF DELPHI_XE_UP}
  Assert.AreEqual<IInterface>(expected,actual,msg);
{$ELSE}
  Assert.IsTrue(expected = actual, msg);
{$ENDIF}
end;

procedure TTestCase.CheckSame(expected, actual: TObject; msg: string);
begin
{$IFDEF DELPHI_XE_UP}
  Assert.AreEqual<TObject>(expected,actual,msg);
{$ELSE}
  Assert.IsTrue(expected = actual, msg);
{$ENDIF}
end;

procedure TTestCase.CheckNotNull(obj: TObject; msg: string);
begin
  Assert.IsNotNull(obj,msg);
end;

procedure TTestCase.CheckNull(obj: TObject; msg: string);
begin
  Assert.IsNull(obj,msg);
end;

procedure TTestCase.CheckException(AMethod: TTestMethod; AExceptionClass: ExceptClass; msg :string);
begin
  Assert.WillRaise(AMethod,AExceptionClass,msg);
end;

procedure TTestCase.CheckEquals(  expected, actual: TClass; msg: string);
begin
  Assert.AreEqual(expected,actual,msg);
end;

procedure TTestCase.CheckInherits(expected, actual: TClass; msg: string);
begin
  Assert.InheritsFrom(expected,actual,msg);
end;

procedure TTestCase.CheckIs(AObject :TObject; AClass: TClass; msg: string);
begin
  Assert.InheritsFrom(AObject.ClassType,AClass,msg);
end;

end.
