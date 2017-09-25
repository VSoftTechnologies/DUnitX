{***************************************************************************}
{                                                                           }
{           DUnitX                                                          }
{                                                                           }
{           Copyright (C) 2017 Vincent Parrett & Contributors               }
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

unit DUnitX.Assert;

// This unit should ONLY contain Assert commands and mechanisms that are
// not directly tied to DUnitX to keep the file neutral and usable in other
// test frameworks such as DUnit.  DUnitX specific implementations should be
// implemented in DUnitX.Assert.Ex.pas

interface

{$I DUnitX.inc}

uses
  {$IFDEF USE_NS}
  System.Classes,
  System.SysUtils;
  {$ELSE}
  Classes,
  SysUtils;
  {$ENDIF}

type
  TTestLocalMethod = TProc;

  TTestMethod = procedure of object;

  Assert = class
  private
    class var fIgnoreCaseDefault: boolean;
    class var fOnAssert: TProc;
    class var fTestFailure: ExceptClass;
    class var fTestPass: ExceptClass;
    class procedure CheckExceptionClass(E: Exception; const exceptionClass: ExceptClass);
    class procedure CheckExceptionClassDescendant(E: Exception; const exceptionClass: ExceptClass);
  protected
    class function StreamsEqual(const stream1, stream2: TStream): boolean;
    class function AddLineBreak(const msg: string): string;
    class procedure DoAssert; inline;
  public
    class procedure Pass(const message : string = '');
    class procedure Fail(const message : string = ''; const errorAddrs : pointer = nil);
    class procedure FailFmt(const message : string; const args: array of const; const errorAddrs : pointer = nil);

    class procedure NotImplemented;

    class procedure AreEqual(const expected : string; const actual : string; const ignoreCase : boolean; const message : string = '');overload;
    class procedure AreEqual(const expected : string; const actual : string; const message : string = '');overload;
    class procedure AreEqual(const expected, actual : Double; const tolerance : Double; const message : string = '');overload;
    class procedure AreEqual(const expected, actual : Double; const message : string = '');overload;
    class procedure AreEqual(const expected, actual : Extended; const tolerance : Extended; const message : string = '');overload;
    class procedure AreEqual(const expected, actual : Extended; const message : string = '');overload;
    class procedure AreEqual(const expected, actual : TClass; const message : string = '');overload;
    class procedure AreEqual(const expected, actual : TStream; const message : string = '');overload;
{$IFNDEF DELPHI_XE_DOWN}
    //Delphi 2010 and XE compiler bug breaks this
    class procedure AreEqual<T>(const expected, actual : T; const message : string = '');overload;
{$ENDIF}
    class procedure AreEqual(const expected, actual : word; const message : string = '');overload;
    class procedure AreEqual(const expected, actual : Integer; const message : string = '');overload;
    class procedure AreEqual(const expected, actual : cardinal; const message : string = '');overload;
    class procedure AreEqual(const expected, actual : boolean; const message : string = '');overload;

    class procedure AreEqual(const expected, actual: TGUID; const message : string = '');overload;

    class procedure AreEqual(const expected, actual: TStrings; const ignoreLines: array of integer; const message : string = '');overload;
    class procedure AreEqual(const expected, actual: TStrings; const message : string = '');overload;

    class procedure AreEqualMemory(const expected : Pointer; const actual : Pointer; const size : Cardinal; const message : string = '');

    class procedure AreNotEqual(const expected : string; const actual : string; const ignoreCase : boolean; const message : string = '');overload;
    class procedure AreNotEqual(const expected : string; const actual : string; const message : string = '');overload;

    class procedure AreNotEqual(const expected, actual : Extended; const tolerance : Extended; const message : string = '');overload;
    class procedure AreNotEqual(const expected, actual : Extended; const message : string = '');overload;

    class procedure AreNotEqual(const expected, actual : Double; const tolerance : double; const message : string = '');overload;
    class procedure AreNotEqual(const expected, actual : Double; const message : string = '');overload;

    class procedure AreNotEqual(const expected, actual : TClass; const message : string = '');overload;

    class procedure AreNotEqual(const expected, actual : TStream; const message : string = '');overload;
{$IFNDEF DELPHI_XE_DOWN}
    //Delphi 2010 and XE compiler bug breaks this
    class procedure AreNotEqual<T>(const expected, actual : T; const message : string = '');overload;
{$ENDIF}
    class procedure AreNotEqual(const expected, actual : Integer; const message : string = '');overload;
    class procedure AreNotEqual(const expected, actual : TGUID; const message : string = '');overload;
    class procedure AreNotEqualMemory(const expected : Pointer; const actual : Pointer; const size : Cardinal; const message : string = '');

    class procedure AreSame(const expected, actual : TObject; const message : string = '');overload;
    class procedure AreSame(const expected, actual : IInterface; const message : string = '');overload;

    class procedure AreNotSame(const expected, actual : TObject; const message : string = '');overload;
    class procedure AreNotSame(const expected, actual : IInterface; const message : string = '');overload;

{$IFDEF DELPHI_XE_UP}
    //Delphi 2010 compiler bug breaks this
    class procedure Contains<T>(const list : IEnumerable<T>; const value : T; const message : string = '');overload;
    class procedure Contains<T>(const arr : array of T; const value : T; const message : string = '');overload;
    class procedure DoesNotContain<T>(const list : IEnumerable<T>; const value : T; const message : string = '');overload;
    class procedure DoesNotContain<T>(const arr : array of T; const value : T; const message : string = '');overload;
{$ENDIF}
    class function Implements<T : IInterface>(value : IInterface; const message : string = '' ) : T;
    class procedure IsTrue(const condition : boolean; const message : string = '');
    class procedure IsFalse(const condition : boolean; const message : string = '');

    class procedure IsNull(const condition : TObject; const message : string = '');overload;
    class procedure IsNull(const condition : Pointer; const message : string = '');overload;
    class procedure IsNull(const condition : IInterface; const message : string = '');overload;
    class procedure IsNull(const condition : Variant; const message : string = '');overload;

    class procedure IsNotNull(const condition : TObject; const message : string = '');overload;
    class procedure IsNotNull(const condition : Pointer; const message : string = '');overload;
    class procedure IsNotNull(const condition : IInterface; const message : string = '');overload;
    class procedure IsNotNull(const condition : Variant; const message : string = '');overload;

    class procedure IsEmpty(const value : string; const message : string = '');overload;
    class procedure IsEmpty(const value : Variant; const message : string = '');overload;
    class procedure IsEmpty(const value : TStrings; const message : string = '');overload;
    class procedure IsEmpty(const value : TList; const message : string = '');overload;
    class procedure IsEmpty(const value : IInterfaceList; const message : string = '');overload;
{$IFDEF DELPHI_XE_UP}
    //Delphi 2010 compiler bug breaks this
    class procedure IsEmpty<T>(const value : IEnumerable<T>; const message : string = '');overload;
{$ENDIF}

    class procedure IsNotEmpty(const value : string; const message : string = '');overload;
    class procedure IsNotEmpty(const value : Variant; const message : string = '');overload;
    class procedure IsNotEmpty(const value : TStrings; const message : string = '');overload;
    class procedure IsNotEmpty(const value : TList; const message : string = '');overload;
    class procedure IsNotEmpty(const value : IInterfaceList; const message : string = '');overload;
{$IFDEF DELPHI_XE_UP}
    //Delphi 2010 compiler bug breaks this
    class procedure IsNotEmpty<T>(const value : IEnumerable<T>; const message : string = '');overload;
{$ENDIF}

    /// <summary>
    ///   Checks that an exception exactly matching ExceptClass will be raised.
    /// </summary>
    class procedure WillRaise(const AMethod : TTestLocalMethod; const exceptionClass : ExceptClass = nil; const msg : string = ''); overload;
    /// <summary>
    ///   Checks that an exception exactly matching ExceptClass and Message will be raised.
    /// </summary>
    class procedure WillRaiseWithMessage(const AMethod : TTestLocalMethod; const exceptionClass : ExceptClass = nil; const exceptionMsg: string = ''; const msg : string = ''); overload;
    /// <summary>
    ///   Checks that an exception exactly matching ExceptClass will be raised.
    /// </summary>
    class procedure WillRaise(const AMethod : TTestMethod; const exceptionClass : ExceptClass = nil; const msg : string = ''); overload;

    /// <summary>
    ///   Checks that an exception that descends from ExceptClass will be raised.
    /// </summary>
    class procedure WillRaiseDescendant(const AMethod : TTestLocalMethod; const exceptionClass : ExceptClass = nil; const msg : string = ''); overload;
    /// <summary>
    ///   Checks that an exception that descends from ExceptClass will be raised.
    /// </summary>
    class procedure WillRaiseDescendant(const AMethod : TTestMethod; const exceptionClass : ExceptClass = nil; const msg : string = ''); overload;

    /// <summary>
    ///   Checks that an exception will be raised.
    /// </summary>
    class procedure WillRaiseAny(const AMethod : TTestLocalMethod; const msg : string = ''); overload;
    /// <summary>
    ///   Checks that an exception will be raised.
    /// </summary>
    class procedure WillRaiseAny(const AMethod : TTestMethod;  const msg : string = ''); overload;


    /// <summary>
    ///   Checks that an exception exactly matching ExceptClass will not be raised.
    /// </summary>
    class procedure WillNotRaise(const AMethod : TTestLocalMethod; const exceptionClass : ExceptClass = nil; const msg : string = ''); overload;
    /// <summary>
    ///   Checks that an exception exactly matching ExceptClass will not be raised.
    /// </summary>
    class procedure WillNotRaise(const AMethod : TTestMethod; const exceptionClass : ExceptClass = nil; const msg : string = ''); overload;

    /// <summary>
    ///   Checks that an exception that descends from ExceptClass not will be raised.
    /// </summary>
    class procedure WillNotRaiseDescendant(const AMethod : TTestLocalMethod; const exceptionClass : ExceptClass = nil; const msg : string = ''); overload;
    /// <summary>
    ///   Checks that an exception that descends from ExceptClass not will be raised.
    /// </summary>
    class procedure WillNotRaiseDescendant(const AMethod : TTestMethod; const exceptionClass : ExceptClass = nil; const msg : string = ''); overload;

    /// <summary>
    ///   Checks that an exception of Any type not will be raised. This method
    ///   is to complement <see cref="DUnitX.TestFramework|Assert.WillRaiseAny(TTestLocalMethod,string)">
    ///   WillRaiseAny</see> method, and is not required, as the default behavior of a test
    ///   is to fail when any exception is raised.
    /// </summary>
    class procedure WillNotRaiseAny(const AMethod : TTestLocalMethod; const msg : string = ''); overload;
    /// <summary>
    ///   Checks that an exception of Any type not will be raised. This method
    ///   is to complement <see cref="DUnitX.TestFramework|Assert.WillRaiseAny(TTestMethod,string)">
    ///   WillRaiseAny</see> method, and is not required, as the default behavior of a test
    ///   is to fail when any exception is raised.
    /// </summary>
    class procedure WillNotRaiseAny(const AMethod : TTestMethod; const msg : string = ''); overload;

    class procedure Contains(const theString : string; const subString : string; const ignoreCase : boolean; const message : string = ''); overload;
    class procedure Contains(const theString : string; const subString : string; const message : string = ''); overload;
    class procedure DoesNotContain(const theString : string; const subString : string; const ignoreCase : boolean; const message : string = ''); overload;
    class procedure DoesNotContain(const theString : string; const subString : string; const message : string = ''); overload;
    class procedure Contains(const theStrings : TStrings; const subString : string; const ignoreCase : boolean; const message : string = ''); overload;
    class procedure Contains(const theStrings : TStrings; const subString : string; const message : string = ''); overload;
    class procedure DoesNotContain(const theStrings : TStrings; const subString : string; const ignoreCase : boolean; const message : string = ''); overload;
    class procedure DoesNotContain(const theStrings : TStrings; const subString : string; const message : string = ''); overload;
    class procedure StartsWith(const subString : string; const theString : string; const ignoreCase : boolean; const message : string = ''); overload;
    class procedure StartsWith(const subString : string; const theString : string; const message : string = ''); overload;
    class procedure EndsWith(const subString : string; const theString : string; const ignoreCase : boolean; const message : string = ''); overload;
    class procedure EndsWith(const subString : string; const theString : string; const message : string = ''); overload;
    class procedure InheritsFrom(const descendant : TClass; const parent : TClass; const message : string = '');
{$IFDEF DELPHI_XE_UP}
    //Delphi 2010 compiler bug breaks this
    class procedure IsType<T>(const value : T; const message : string = '');overload;deprecated 'use inheritsfrom - istype is useless';
{$ENDIF}

    /// <summary>
    ///   Checks that there is no difference between the given strings. If there is a difference, then
    ///   the test will fail with extended informations about the starting position of the difference.
    /// </summary>
    class procedure NoDiff(const expected, actual: string; const ignoreCase : boolean; const message : string = ''); overload;
    /// <summary>
    ///   Checks that there is no difference between the given strings. If there is a difference, then
    ///   the test will fail with extended informations about the starting position of the difference.
    /// </summary>
    class procedure NoDiff(const expected, actual: string; const message : string = ''); overload;

    {$IFDEF SUPPORTS_REGEX}
    class procedure IsMatch(const regexPattern : string; const theString : string; const message : string = '');
    {$ENDIF}

    class property OnAssert: TProc read fOnAssert write fOnAssert;
    class property TestFailure: ExceptClass read fTestFailure write fTestFailure;
    class property TestPass: ExceptClass read fTestPass write fTestPass;
    class property IgnoreCaseDefault: boolean read fIgnoreCaseDefault write fIgnoreCaseDefault;

    class constructor Create;
  end;

{$IFDEF DELPHI_XE_DOWN}
  function ReturnAddress: Pointer; assembler;
{$ENDIF}

implementation

uses
  DUnitX.ResStrs,
  DUnitX.Utils,
  {$IFDEF SUPPORTS_REGEX}
    {$IFDEF USE_TREGEXPR}
    RegExpr,
    {$ELSE}
    System.RegularExpressions,
    {$ENDIF}
  {$ENDIF}
  {$IFDEF USE_NS}
  System.Generics.Collections,
  System.Generics.Defaults,
  System.Math,
  System.Rtti,
  System.StrUtils,
  System.TypInfo,
  System.Variants;
  {$ELSE}
  Generics.Collections,
  Generics.Defaults,
  Math,
  Rtti,
  StrUtils,
  TypInfo,
  Variants;
  {$ENDIF}

{$IFDEF DELPHI_XE_DOWN}
function IsBadPointer(P: Pointer):Boolean;register;
begin
  try
    Result  := (p = nil) or ((Pointer(P^) <> P) and (Pointer(P^) = P));
  except
    Result := true;
  end
end;

function ReturnAddress: Pointer; assembler;
const
  CallerIP = $4;
asm
   mov   eax, ebp
   call  IsBadPointer
   test  eax,eax
   jne   @@Error

   mov   eax, [ebp].CallerIP
   sub   eax, 5   // 5 bytes for call

   push  eax
   call  IsBadPointer
   test  eax,eax
   pop   eax
   je    @@Finish

@@Error:
   xor eax, eax
@@Finish:
end;
{$ENDIF}

{ Assert }

class procedure Assert.AreEqual(const expected, actual, tolerance: Extended; const message: string);
begin
  DoAssert;
  if not {$IFDEF USE_NS}System.Math.{$ENDIF}SameValue(expected,actual,tolerance) then
    FailFmt(SUnexpectedErrorExt ,[expected,actual,message], ReturnAddress);
end;

class procedure Assert.AreEqual(const expected, actual: TClass; const message: string);
var
  msg : string;
begin
  DoAssert;
  if expected <> actual then
  begin
    msg := ' is not equal to ';
    if expected = nil then
      msg := 'nil' + msg
    else
      msg := expected.ClassName + msg;

    if actual = nil then
      msg := msg +  'nil'
    else
      msg := msg + actual.ClassName;

    if message <> '' then
      msg := msg + '. ' + message;

    Fail(msg, ReturnAddress);
  end;
end;

{$IFNDEF DELPHI_XE_DOWN}
    //Delphi 2010 and XE compiler bug breaks this
class procedure Assert.AreEqual<T>(const expected, actual: T; const message: string);
var
  comparer : IComparer<T>;
  expectedValue, actualValue : TValue;
begin
  DoAssert;
  comparer := TComparer<T>.Default;
  if comparer.Compare(actual,expected) <> 0 then
  begin
    expectedValue := TValue.From<T>(expected);
    actualValue := TValue.From<T>(actual);
    FailFmt(SNotEqualErrorStr, [expectedValue.ToString, actualValue.ToString, message], ReturnAddress)
  end;
end;
{$ENDIF}

class function Assert.AddLineBreak(const msg: string): string;
begin
  if msg <> '' then
    Result :=  sLineBreak + msg
  else
    Result := '';
end;

class procedure Assert.AreEqual(const expected, actual: Integer; const message: string);
begin
  DoAssert;
  if expected <> actual then
    FailFmt(SUnexpectedErrorInt ,[expected, actual, message], ReturnAddress);
end;

class procedure Assert.AreEqual(const expected, actual: boolean; const message: string);
begin
  DoAssert;
  if expected <> actual then
    FailFmt(SUnexpectedErrorStr ,[BoolToStr(expected, true), BoolToStr(actual, true), message], ReturnAddress);
end;

class procedure Assert.AreEqual(const expected, actual: cardinal; const message: string);
begin
  DoAssert;
  if expected <> actual then
    FailFmt(SUnexpectedErrorInt ,[expected, actual, message], ReturnAddress);
end;

class procedure Assert.AreEqual(const expected, actual, tolerance: Double; const message: string);
begin
  DoAssert;
  if not {$IFDEF USE_NS}System.Math.{$ENDIF}SameValue(expected,actual,tolerance) then
    FailFmt(SUnexpectedErrorDbl ,[expected,actual,message], ReturnAddress);
end;

class procedure Assert.AreEqual(const expected, actual: Double; const message: string);
var
  tolerance : Double;
begin
  tolerance := 0;
  AreEqual(expected, actual, tolerance, message);
end;

class procedure Assert.AreEqual(const expected, actual: Extended; const message: string);
var
  tolerance : Extended;
begin
  tolerance := 0;
  AreEqual(expected, actual, tolerance, message);
end;

class procedure Assert.AreEqualMemory(const expected : Pointer; const actual : Pointer; const size : Cardinal; const message : string);
begin
  DoAssert;
  if not CompareMem(expected, actual, size) then
    Fail(SMemoryValuesNotEqual + message, ReturnAddress);
end;

class procedure Assert.AreNotEqual(const expected, actual, tolerance: Extended; const message: string);
begin
  DoAssert;
  if {$IFDEF USE_NS}System.Math.{$ENDIF}SameValue(expected, actual, tolerance) then
    FailFmt(SEqualsErrorExt ,[expected,actual,message], ReturnAddress);
end;

class procedure Assert.AreNotEqual(const expected, actual: string;const ignoreCase: boolean; const message: string);

  function AreNotEqualText(const expected, actual: string; const ignoreCase: boolean): boolean;
  begin
    if ignoreCase then
      Result := SameText(expected, actual)
    else
      Result := SameStr(expected, actual);
  end;

begin
  DoAssert;
  if AreNotEqualText(expected, actual, ignoreCase) then
    FailFmt(SEqualsErrorStr, [expected, actual, message], ReturnAddress);
end;

class procedure Assert.AreNotEqual(const expected, actual: TClass; const message: string);
var
  msg : string;
begin
  DoAssert;
  if expected = actual then
  begin
    msg := ' is equal to ';
    if expected = nil then
      msg := 'nil' + msg
    else
      msg := expected.ClassName + msg;

    if actual = nil then
      msg := msg +  'nil'
    else
      msg := msg + actual.ClassName;
    if message <> '' then
      msg := msg + '. ' + message;

    Fail(msg, ReturnAddress);
  end;
end;

{$IFNDEF DELPHI_XE_DOWN}
//Delphi 2010 and XE compiler bug breaks this
class procedure Assert.AreNotEqual<T>(const expected, actual: T; const message: string);
var
  comparer : IComparer<T>;
  expectedValue, actualValue : TValue;
begin
  DoAssert;
  comparer := TComparer<T>.Default;
  if comparer.Compare(actual,expected) = 0 then
  begin
    expectedValue := TValue.From<T>(expected);
    actualValue := TValue.From<T>(actual);

    FailFmt(SEqualsErrorStr2,[expectedValue.ToString, actualValue.ToString, message], ReturnAddress);
  end;
end;
{$ENDIF}

class procedure Assert.AreNotEqual(const expected, actual: Integer; const message: string);
begin
  DoAssert;
  if expected = actual then
    FailFmt(SEqualsErrorInt ,[expected, actual, message], ReturnAddress);
end;

class procedure Assert.AreNotEqual(const expected, actual: Extended; const message: string);
var
  tolerance : Extended;
begin
  tolerance := 0;
  Assert.AreNotEqual(expected,actual,tolerance,message);
end;

class procedure Assert.AreNotEqual(const expected, actual: Double; const message: string);
var
  tolerance : double;
begin
  tolerance := 0;
  Assert.AreNotEqual(expected,actual,tolerance,message);
end;

class procedure Assert.AreNotEqual(const expected, actual, tolerance: double; const message: string);
begin
  DoAssert;
  if {$IFDEF USE_NS}System.Math.{$ENDIF}SameValue(expected, actual, tolerance) then
    FailFmt(SEqualsErrorDbl ,[expected,actual,message], ReturnAddress);
end;

class procedure Assert.AreNotEqualMemory(const expected, actual: Pointer; const size: Cardinal; const message: string);
begin
  DoAssert;
  if CompareMem(expected,actual, size) then
    Fail(SMemoryValuesEqual + message, ReturnAddress);
end;

class procedure Assert.AreNotSame(const expected, actual: TObject; const message: string);
begin
  DoAssert;
  if expected.Equals(actual) then
    FailFmt(SEqualsErrorObj, [expected.ToString,actual.ToString,message], ReturnAddress);
end;

class procedure Assert.AreNotSame(const expected, actual: IInterface; const message: string);
begin
  DoAssert;
  if (expected as IInterface) = (actual as IInterface) then
    FailFmt(SEqualsErrorIntf,[message], ReturnAddress);
end;

class procedure Assert.AreSame(const expected, actual: IInterface; const message: string);
begin
  DoAssert;
  if (expected as IInterface) <> (actual as IInterface) then
    FailFmt(SNotEqualErrorIntf,[message], ReturnAddress);
end;

class procedure Assert.AreSame(const expected, actual: TObject; const message: string);
begin
  DoAssert;
  if not expected.Equals(actual) then
    FailFmt(SNotEqualErrorObj, [expected.ToString,actual.ToString,message], ReturnAddress);
end;

{$IFDEF DELPHI_XE_UP}
//Delphi 2010 compiler bug breaks this
class procedure Assert.Contains<T>(const list: IEnumerable<T>; const value: T; const message: string);
var
  o : T;
  comparer : IComparer<T>;
begin
  DoAssert;
  comparer := TComparer<T>.Default;
  for o in list do
  begin
    if comparer.Compare(o,value) = 0 then
      exit;
  end;

  FailFmt(SValueNotInList,[TValue.From<T>(value).ToString, message], ReturnAddress);
end;

class procedure Assert.Contains<T>(const arr : array of T; const value : T; const message : string = '');
var
  o : T;
  comparer : IComparer<T>;
begin
  DoAssert;
  comparer := TComparer<T>.Default;
  for o in arr do
  begin
    if comparer.Compare(o,value) = 0 then
      exit;
  end;

  FailFmt(SValueNotInList,[TValue.From<T>(value).ToString, message], ReturnAddress);
end;
{$ENDIF}

{$IFDEF DELPHI_XE_UP}
//Delphi 2010 compiler bug breaks this
class procedure Assert.DoesNotContain<T>(const list: IEnumerable<T>; const value: T; const message: string);
var
  o : T;
  comparer : IComparer<T>;
begin
  DoAssert;
  comparer := TComparer<T>.Default;
  for o in list do
  begin
    if comparer.Compare(o,value) = 0 then
      FailFmt(SValueInList,[TValue.From<T>(value).ToString, message], ReturnAddress);
  end;
end;

class procedure Assert.DoesNotContain<T>(const arr : array of T; const value : T; const message : string = '');
var
  o : T;
  comparer : IComparer<T>;
begin
  DoAssert;
  comparer := TComparer<T>.Default;
  for o in arr do
  begin
    if comparer.Compare(o,value) = 0 then
      FailFmt(SValueInList,[TValue.From<T>(value).ToString, message], ReturnAddress);
  end;
end;
{$ENDIF}

class procedure Assert.Fail(const message : string; const errorAddrs : pointer);
begin
  //If we have been given a return then use it. (makes the exception appear on level above in the stack)
  if errorAddrs <> nil then
    raise fTestFailure.Create(message) at errorAddrs
  else
    //Otherwise use the return address we can currently get to for where to raise the exception
    raise fTestFailure.Create(message) at ReturnAddress;
end;

class procedure Assert.FailFmt(const message: string; const args: array of const; const errorAddrs: pointer);
begin
  Fail(Format(message, args), errorAddrs);
end;

class procedure Assert.Pass(const message: string);
begin
  raise fTestPass.Create(message);
end;

class procedure Assert.StartsWith(const subString, theString, message: string);
begin
  Assert.StartsWith(subString, theString, fIgnoreCaseDefault, message);
end;

class function Assert.StreamsEqual(const stream1, stream2: TStream): boolean;
const
  BlockSize = 4096;
var
  Buffer1: array[0..BlockSize - 1] of byte;
  Buffer2: array[0..BlockSize - 1] of byte;
  BufferLen: integer;
begin
  Result := False;

  if stream1.Size = stream2.Size then
  begin
    stream1.Position := 0;
    stream2.Position := 0;

    while stream1.Position < stream1.Size do
    begin
      BufferLen := stream1.Read(Buffer1, BlockSize);
      stream2.Read(Buffer2, BlockSize);
      if not CompareMem(@Buffer1, @Buffer2, BufferLen) then
        exit;
    end;

    Result := True;
  end;
end;

class function Assert.Implements<T>(value: IInterface; const message: string) : T;
begin
  DoAssert;
  if not Supports(value,GetTypeData(TypeInfo(T)).Guid,result) then
    FailFmt(SIntfNotImplemented, [GetTypeName(TypeInfo(T)), message],ReturnAddress);
end;

class procedure Assert.InheritsFrom(const descendant, parent: TClass; const message: string);
var
  msg : string;
begin
  DoAssert;
  if (descendant = nil) or (parent = nil) or (not descendant.InheritsFrom(parent)) then
  begin
    msg := ' does not inherit from ';
    if descendant = nil then
      msg := 'nil' + msg
    else
      msg := descendant.ClassName + msg;
    if parent = nil then
      msg := msg + 'nil'
    else
      msg := msg + parent.ClassName;
    msg := msg + '.';
    if True then
    if message <> '' then
      msg := msg + ' ' + message;

    Fail(msg, ReturnAddress);
  end;
end;

class procedure Assert.IsEmpty(const value: IInterfaceList; const message: string);
begin
  DoAssert;
  if value.Count > 0 then
    FailFmt(SListNotEmpty,[message], ReturnAddress);
end;

class procedure Assert.IsEmpty(const value: TList; const message: string);
begin
  DoAssert;
  if value.Count > 0 then
    FailFmt(SListNotEmpty,[message], ReturnAddress);
end;

class procedure Assert.IsEmpty(const value, message: string);
begin
  DoAssert;
  if Length(value) > 0 then
    FailFmt(SStrNotEmpty,[message], ReturnAddress);
end;

class procedure Assert.IsEmpty(const value: Variant; const message: string);
begin
  DoAssert;
  if VarIsEmpty(value) or VarIsNull(value) then
    FailFmt(SVarNotEmpty,[message], ReturnAddress);
end;

class procedure Assert.IsEmpty(const value: TStrings; const message: string);
begin
  DoAssert;
  if value.Count > 0 then
    FailFmt(SListNotEmpty,[message], ReturnAddress);
end;

{$IFDEF DELPHI_XE_UP}
//Delphi 2010 compiler bug breaks this
class procedure Assert.IsEmpty<T>(const value: IEnumerable<T>; const message: string);
var
  o : T;
  count : integer;
begin
  DoAssert;
  count := 0;
  for o in value do
    Inc(count);

  if count > 0 then
    FailFmt(SListNotEmpty,[message], ReturnAddress);
end;
{$ENDIF}

class procedure Assert.IsFalse(const condition: boolean; const message: string);
begin
  DoAssert;
  if condition then
    FailFmt(SIsFalseError,[message], ReturnAddress);
end;

class procedure Assert.IsNotEmpty(const value: TList; const message: string);
begin
  DoAssert;
  if value.Count = 0 then
    FailFmt(SListEmpty, [message], ReturnAddress);
end;

class procedure Assert.IsNotEmpty(const value: IInterfaceList; const message: string);
begin
  DoAssert;
  if value.Count = 0 then
    FailFmt(SListEmpty, [message], ReturnAddress);
end;

class procedure Assert.IsNotEmpty(const value: TStrings; const message: string);
begin
  DoAssert;
  if value.Count = 0 then
    FailFmt(SListEmpty, [message], ReturnAddress);
end;

class procedure Assert.IsNotEmpty(const value, message: string);
begin
  DoAssert;
  if value = '' then
    FailFmt(SStrEmpty,[message], ReturnAddress);
end;

class procedure Assert.IsNotEmpty(const value: Variant; const message: string);
begin
  DoAssert;
  if VarIsEmpty(value) then
    FailFmt(SVarEmpty,[message], ReturnAddress);
end;

{$IFDEF DELPHI_XE_UP}
//Delphi 2010 compiler bug breaks this
class procedure Assert.IsNotEmpty<T>(const value: IEnumerable<T>; const message: string);
var
  x : T;
  count : integer;
begin
  DoAssert;
  count := 0;
  for x in value do
    Inc(count);

  if count = 0 then
    FailFmt(SListEmpty,[message], ReturnAddress);
end;
{$ENDIF}

class procedure Assert.IsNotNull(const condition: IInterface; const message: string);
begin
  DoAssert;
  if condition = nil then
    FailFmt(SIntfNil,[message], ReturnAddress);
end;

class procedure Assert.IsNotNull(const condition: Pointer; const message: string);
begin
  DoAssert;
  if condition = nil then
    FailFmt(SPointerNil,[message], ReturnAddress);
end;

class procedure Assert.IsNotNull(const condition: TObject; const message: string);
begin
  DoAssert;
  if condition = nil then
    FailFmt(SObjNil,[message], ReturnAddress);
end;

class procedure Assert.IsNotNull(const condition: Variant; const message: string);
begin
  DoAssert;
  if VarIsNull(condition) then
    FailFmt(SVariantNull,[message], ReturnAddress);
end;

class procedure Assert.IsNull(const condition: Variant; const message: string);
begin
  DoAssert;
  if not VarIsNull(condition) then
    FailFmt(SVariantNotNull,[message], ReturnAddress);
end;

class procedure Assert.IsNull(const condition: IInterface; const message: string);
begin
  DoAssert;
  if condition <> nil then
    FailFmt(SIntfNotNil,[message], ReturnAddress);
end;

class procedure Assert.IsNull(const condition: TObject; const message: string);
begin
  DoAssert;
  if condition <> nil then
    FailFmt(SObjNotNil,[message], ReturnAddress);
end;

class procedure Assert.IsNull(const condition: Pointer; const message: string);
begin
  DoAssert;
  if condition <> nil then
    FailFmt(SPointerNotNil,[message], ReturnAddress);
end;

class procedure Assert.IsTrue(const condition: boolean;const message : string);
begin
  DoAssert;
  if not condition then
    FailFmt(SIsTrueError,[message], ReturnAddress);
end;

class procedure Assert.NoDiff(const expected, actual: string; const ignoreCase : boolean; const message: string);
const
  DIFF_LENGTH = 10;
var
  lenExp, lenAct: integer;
  strExp, strAct: string;
  position: Integer;
begin
  DoAssert;
  lenExp := Length(expected);
  lenAct := Length(actual);

  if lenExp <> lenAct then
    FailFmt(SLengthOfStringsNotEqual + ': ' + SUnexpectedErrorInt, [lenExp, lenAct, message])
  else begin
    if ignoreCase then
    begin
      strExp := UpperCase(expected);
      strAct := UpperCase(actual);
    end
    else begin
      strExp := expected;
      strAct := actual;
    end;
    for position := 1 to lenExp do
    begin
      if (position <= lenAct) and (strExp[position] <> strAct[position]) then
        FailFmt(SDiffAtPosition + ': ' + SStrDoesNotMatch, [position,
          TStrUtils.EncodeWhitespace(Copy(expected, position, DIFF_LENGTH)),
          TStrUtils.EncodeWhitespace(Copy(actual, position, DIFF_LENGTH)), message]);
    end;
  end;
end;

class procedure Assert.NoDiff(const expected, actual, message: string);
begin
  NoDiff(expected, actual, false, message);
end;

class procedure Assert.NotImplemented;
begin
  Assert.Fail(SNotImplemented);
end;

{$IFDEF DELPHI_XE_UP}
//Delphi 2010 compiler bug breaks this
class procedure Assert.IsType<T>(const value: T; const message : string);
var
  val : TValue;
begin
  DoAssert;
  val := TValue.From<T>(value);
  if not val.IsType<T> then
    Fail(STypeError, ReturnAddress);
end;
{$ENDIF}

class procedure Assert.WillNotRaise(const AMethod: TTestMethod; const exceptionClass: ExceptClass; const msg: string);
begin
  Assert.WillNotRaise(
    procedure
    begin
      AMethod;
    end,
    exceptionClass, msg);
end;

class procedure Assert.WillNotRaiseAny(const AMethod: TTestLocalMethod;  const msg: string);
begin
  DoAssert;
  try
    AMethod;
  except
    on e : TObject do // For those who throw exceptions not descending from Exception.
    begin
      if e is Exception  then
      begin
        FailFmt(SUnexpectedException, [e.ClassName, exception(e).message], ReturnAddress);
      end
      else
        FailFmt(SUnexpectedExceptionAlt, [e.ClassName], ReturnAddress);
    end;
  end;
end;

class procedure Assert.WillNotRaiseAny(const AMethod: TTestMethod;const msg: string);
begin
  Assert.WillNotRaiseAny(
    procedure
    begin
      AMethod;
    end,
    msg);
end;

class procedure Assert.WillNotRaiseDescendant(const AMethod: TTestLocalMethod;  const exceptionClass: ExceptClass; const msg: string);
begin
  DoAssert;
  try
    AMethod;
  except
    on e : Exception do
    begin
      if exceptionClass <> nil then
      begin
        if e is exceptionClass then
          Fail(SMethodRaisedException + exceptionClass.ClassName + sLineBreak + e.Message + AddLineBreak(msg), ReturnAddress);
      end
      else
        FailFmt(SMethodRaisedExceptionAlt, [e.ClassName, exceptionClass.ClassName, e.message], ReturnAddress);
    end;
  end;
end;

class procedure Assert.WillNotRaiseDescendant(const AMethod: TTestMethod;  const exceptionClass: ExceptClass; const msg: string);
begin
  Assert.WillNotRaise(
    procedure
    begin
      AMethod;
    end,
    exceptionClass, msg);
end;

class procedure Assert.WillRaise(const AMethod : TTestLocalMethod; const exceptionClass : ExceptClass; const msg : string);
begin
  DoAssert;
  try
    AMethod;
  except
    on E: Exception do
    begin
      CheckExceptionClass(e, exceptionClass);
      Exit;
    end;
  end;
  Fail(SNoException + AddLineBreak(msg), ReturnAddress);
end;

class procedure Assert.WillNotRaise(const AMethod : TTestLocalMethod; const exceptionClass : ExceptClass; const msg : string);
begin
  DoAssert;
  try
    AMethod;
  except
    on e : Exception do
    begin
      if exceptionClass <> nil then
      begin
        if e.ClassType = exceptionClass then
          Fail(SMethodRaisedException + exceptionClass.ClassName + sLineBreak + e.Message + AddLineBreak(msg), ReturnAddress);
      end
      else
        FailFmt(SUnexpectedException, [e.ClassName, e.message], ReturnAddress);
    end;
  end;
end;

class procedure Assert.WillRaise(const AMethod: TTestMethod; const exceptionClass: ExceptClass; const msg: string);
begin
  Assert.WillRaise(
    procedure
    begin
      AMethod;
    end,
    exceptionClass, msg);
end;

class procedure Assert.WillRaiseAny(const AMethod: TTestLocalMethod; const msg: string);
begin
  DoAssert;
  try
    AMethod;
  except
    on E: Exception do
    begin
      Exit;
    end;
  end;
  Fail(SNoException + AddLineBreak(msg), ReturnAddress);
end;

class procedure Assert.WillRaiseAny(const AMethod: TTestMethod; const msg: string);
begin
  Assert.WillRaiseAny(
    procedure
    begin
      AMethod;
    end,
    msg);
end;

class procedure Assert.WillRaiseDescendant(const AMethod: TTestLocalMethod;  const exceptionClass: ExceptClass; const msg: string);
begin
  DoAssert;
  try
    AMethod;
  except
    on E: Exception do
    begin
      CheckExceptionClassDescendant(e, exceptionClass);
      Exit;
    end;
  end;
  Fail(SNoException + AddLineBreak(msg), ReturnAddress);
end;

class procedure Assert.WillRaiseDescendant(const AMethod: TTestMethod; const exceptionClass: ExceptClass; const msg: string);
begin
  Assert.WillRaiseDescendant(
    procedure
    begin
      AMethod;
    end,
    exceptionClass, msg);
end;

class procedure Assert.WillRaiseWithMessage(const AMethod: TTestLocalMethod; const exceptionClass: ExceptClass;const exceptionMsg, msg: string);
begin
  DoAssert;
  try
    AMethod;
  except
    on E: Exception do
    begin
      CheckExceptionClass(E, exceptionClass);
      if (exceptionMsg <> '') and (not SameStr(E.Message, exceptionMsg)) then
        FailFmt(SUnexpectedExceptionMessage, [E.ClassName, E.Message, exceptionMsg, msg]);
      Exit;
    end;
  end;
  Fail(SNoException + AddLineBreak(msg), ReturnAddress);
end;

class procedure Assert.AreEqual(const expected : string; const actual : string; const message : string);
begin
  Assert.AreEqual(expected, actual, fIgnoreCaseDefault, message);
end;

class procedure Assert.AreEqual(const expected, actual : string;  const ignoreCase : boolean; const message: string);
begin
  DoAssert;
  if ignoreCase then
  begin
    if not SameText(expected,actual) then
      FailFmt(SNotEqualErrorStr,[expected,actual,message], ReturnAddress);
  end
  else if not SameStr(expected,actual) then
    FailFmt(SNotEqualErrorStr,[expected,actual,message], ReturnAddress);
end;

class procedure Assert.CheckExceptionClass(E: Exception; const exceptionClass: ExceptClass);
begin
  DoAssert;
  if exceptionClass = nil then
    Exit;

  if E.ClassType <> exceptionClass then
    FailFmt(SCheckExceptionClassError, [E.ClassName, exceptionClass.ClassName, E.message], ReturnAddress);
end;

class procedure Assert.CheckExceptionClassDescendant(E: Exception; const exceptionClass: ExceptClass);
begin
  DoAssert;
  if exceptionClass = nil then
    Exit;

  if not (E is exceptionClass) then
    FailFmt(SCheckExceptionClassDescError, [E.ClassName, exceptionClass.ClassName, E.message], ReturnAddress);
end;

class procedure Assert.Contains(const theStrings: TStrings; const subString: string; const ignoreCase: boolean; const message: string);
begin
  Contains(theStrings.Text, subString, ignoreCase, message);
end;

class procedure Assert.Contains(const theStrings: TStrings; const subString, message: string);
begin
  Contains(theStrings.Text, subString, message);
end;

class procedure Assert.Contains(const theString, subString, message: string);
begin
  Contains(theString, subString, fIgnoreCaseDefault, message);
end;

class procedure Assert.Contains(const theString : string; const subString : string; const ignoreCase : boolean; const message : string);
begin
  DoAssert;
  if ignoreCase then
  begin
    if not {$IFDEF USE_NS}System.StrUtils.{$ENDIF}ContainsText(theString,subString) then
      FailFmt(SStrDoesNotContain, [theString,subString,message], ReturnAddress);
  end
  else if not {$IFDEF USE_NS}System.StrUtils.{$ENDIF}ContainsStr(theString,subString) then
    FailFmt(SStrDoesNotContain, [theString,subString,message], ReturnAddress);
end;

class constructor Assert.Create;
begin
  fIgnoreCaseDefault := true;
end;

class procedure Assert.DoAssert;
begin
  if Assigned(fOnAssert) then
    fOnAssert();
end;

class procedure Assert.DoesNotContain(const theString, subString, message: string);
begin
  DoesNotContain(theString, subString, fIgnoreCaseDefault, message);
end;

class procedure Assert.DoesNotContain(const theString, subString: string; const ignoreCase: boolean; const message: string);
begin
  DoAssert;
  if ignoreCase then
  begin
    if {$IFDEF USE_NS}System.{$ENDIF}StrUtils.ContainsText(theString, subString) then
      FailFmt(SStrDoesNotContain,[theString, subString, message], ReturnAddress);
  end
  else if {$IFDEF USE_NS}System.{$ENDIF}StrUtils.ContainsStr(theString, subString) then
    FailFmt(SStrDoesNotContain,[theString, subString, message], ReturnAddress);
end;

class procedure Assert.EndsWith(const subString : string; const theString : string; const ignoreCase : boolean; const message : string);
begin
  DoAssert;
  if ignoreCase then
  begin
    if not {$IFDEF USE_NS}System.StrUtils.{$ENDIF}EndsText(subString,theString) then
      FailFmt(SStrDoesNotEndWith,[theString,subString,message], ReturnAddress);
  end
  else if not {$IFDEF USE_NS}System.StrUtils.{$ENDIF}EndsStr(subString,theString) then
    FailFmt(SStrDoesNotEndWith, [theString,subString,message], ReturnAddress);
end;

{$IFDEF SUPPORTS_REGEX}
class procedure Assert.IsMatch(const regexPattern, theString, message: string);

  {$IFDEF USE_TREGEXPR}
  function PerformIsMatch(): boolean;
  var
    RegExp: TRegExpr;
  begin
    RegExp := TRegExpr.Create;
    try
      RegExp.Expression := regexPattern;
      Result := RegExp.Exec(theString);
    finally
      RegExp.Free;
    end;
  end;
  {$ELSE}
  function PerformIsMatch(): boolean;
  begin
    Result := TRegEx.IsMatch(theString,regexPattern);
  end;
  {$ENDIF}

begin
  DoAssert;

  if not PerformIsMatch then
    FailFmt(SStrDoesNotMatch, [theString,regexPattern,message], ReturnAddress);
end;
{$ENDIF}

class procedure Assert.StartsWith(const subString : string; const theString : string; const ignoreCase : boolean; const message: string);
begin
  DoAssert;

  // passing an empty string into the StartsText results in an accessviolation
  if subString = '' then
    FailFmt(SStrCannotBeEmpty, [], ReturnAddress);

  if ignoreCase then
  begin
    if not {$IFDEF USE_NS}System.StrUtils.{$ENDIF}StartsText(subString,theString) then
      FailFmt(SStrDoesNotStartWith, [theString,subString,message], ReturnAddress);
  end
  else if not {$IFDEF USE_NS}System.StrUtils.{$ENDIF}StartsStr(subString,theString) then
    FailFmt(SStrDoesNotStartWith, [theString,subString,message], ReturnAddress);
end;

class procedure Assert.AreEqual(const expected, actual: TGUID; const message: string);
begin
  DoAssert;
  if not IsEqualGUID(expected, actual) then
    FailFmt(SUnexpectedErrorGUID, [GUIDToString(expected), GUIDToString(actual), message], ReturnAddress);
end;

class procedure Assert.AreEqual(const expected, actual: word; const message: string);
begin
  DoAssert;
  if expected <> actual then
    FailFmt(SUnexpectedErrorInt ,[expected, actual, message], ReturnAddress);
end;

class procedure Assert.AreEqual(const expected, actual: TStream; const message: string);
begin
  DoAssert;
  if not StreamsEqual(expected, actual) then
    FailFmt(SUnexpectedErrorStream, [message], ReturnAddress);
end;

class procedure Assert.AreNotEqual(const expected, actual: TGUID; const message: string);
begin
  DoAssert;
  if IsEqualGUID(expected, actual) then
    FailFmt(SEqualsErrorGUID,[GUIDToString(expected), GUIDToString(actual), message], ReturnAddress);
end;

class procedure Assert.AreNotEqual(const expected, actual: TStream; const message: string);
begin
  DoAssert;
  if StreamsEqual(expected, actual) then
    FailFmt(SEqualsErrorStream, [message], ReturnAddress);
end;

class procedure Assert.AreNotEqual(const expected, actual, message: string);
begin
  AreNotEqual(expected, actual, fIgnoreCaseDefault, message);
end;

class procedure Assert.EndsWith(const subString, theString, message: string);
begin
  Assert.EndsWith(subString, theString, fIgnoreCaseDefault, message);
end;

class procedure Assert.DoesNotContain(const theStrings: TStrings; const subString: string; const ignoreCase: boolean; const message: string);
begin
  DoesNotContain(theStrings.Text, subString, ignoreCase, message);
end;

class procedure Assert.DoesNotContain(const theStrings: TStrings; const subString, message: string);
begin
  DoesNotContain(theStrings.Text, subString, message);
end;

class procedure Assert.AreEqual(const expected, actual: TStrings; const ignoreLines: array of integer; const message: string);
var
  index: Integer;
  lineNumber: integer;
  ignoreLinesDict: TDictionary<Integer, Boolean>;
begin
  DoAssert;
  IsTrue(expected <> actual, message);
  ignoreLinesDict := TDictionary<Integer,Boolean>.Create;
  try
    if (Length(ignoreLines) > 0) then
    begin
      for index := 0 to Length(ignoreLines)-1 do
      begin
        ignoreLinesDict.Add(ignoreLines[index], true);
      end;
    end;

    if (expected.Count <> actual.Count) then
      FailFmt(SNumberOfStringsNotEqual + ': ' + SUnexpectedErrorInt, [expected.Count, actual.Count, message], ReturnAddress);

    for index := 0 to expected.Count - 1 do
    begin
      lineNumber := index + 1;
      if not ignoreLinesDict.ContainsKey(lineNumber) then
      begin
        if (expected[index] <> actual[index]) then
        begin
          NoDiff(expected[index], actual[index], Format('at line %d', [lineNumber]) + ' ' + message);
        end;
      end;
    end;
  finally
    ignoreLinesDict.Free;
  end;
end;

class procedure Assert.AreEqual(const expected, actual: TStrings; const message: string);
begin
  AreEqual(expected, actual, [], message);
end;

end.
