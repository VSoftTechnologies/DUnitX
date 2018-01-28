{***************************************************************************}
{                                                                           }
{           DUnitX                                                          }
{                                                                           }
{           Copyright (C) 2017 Vincent Parrett                              }
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

unit DUnitX.Tests.Assert;

interface

{$I ..\DUnitX.inc}

uses
  DUnitX.TestFramework;

type
  {$M+}
  [TestFixture]
  TTestsAssert = class
  published
    [Test]
    procedure Pass_Throws_ETestPass_Exception;

    [Test]
    procedure Pass_Throws_ETestPass_Exception_With_Message;

    [Test]
    procedure Fail_Throws_ETestFailure_Exception;

    [Test]
    procedure Fail_Throws_ETestFailure_Exception_With_Message;

    [Test]
    procedure Fail_Throws_ETestFailure_Exception_With_Return_Address_Reference;

    [Test]
    procedure Fail_Throws_ETestFailure_Exception_With_Caller_Address_Reference;

    [Test]
    procedure AreEqual_String_Throws_No_Exception_When_Values_Are_Equal;

    [Test]
    procedure AreEqual_Integer_Throws_No_Exception_When_Values_Are_Equal;

    [Test]
    procedure AreEqual_Integer_Throws_ETestFailure_When_Values_Are_NotEqual;

    [Test]
    procedure AreEqual_Extended_Throws_No_Exception_When_Values_Are_Equal;

    [Test]
    procedure AreEqual_Extended_Throws_ETestFailure_When_Values_Are_NotEqual;

    [Test]
    procedure AreEqual_Double_Throws_No_Exception_When_Values_Are_Equal;

    [Test]
    procedure AreEqual_Double_Throws_ETestFailure_When_Values_Are_NotEqual;

    [Test]
    procedure AreEqual_GUID_Throws_No_Exception_When_Values_Are_Equal;

    [Test]
    procedure AreEqual_GUID_Throws_ETestFailure_When_Values_Are_NotEqual;

    [Test]
    procedure AreEqual_Stream_Throws_No_Exception_When_Values_Are_Equal;

    [Test]
    procedure AreEqual_Stream_Throws_ETestFailure_When_Values_Are_NotEqual;

    [Test]
    procedure AreEqual_TClass_Throws_No_Exception_When_Classes_Are_Equal;

    [Test]
    procedure AreEqual_TClass_Throws_ETestFailure_When_Classes_Are_NotEqual;

    [Test]
    procedure NoDiff_Throws_No_Exception_When_Strings_Are_Equal;

    [Test]
    procedure NoDiff_Throws_ETestFailure_When_Strings_Are_NotEqual;

    [Test]
    procedure AreEqual_TStrings_Throws_No_Exception_When_Strings_Are_Equal;

    [Test]
    procedure AreEqual_TStrings_Throws_ETestFailure_When_Strings_Are_NotEqual;

{$IFNDEF DELPHI_XE_DOWN}
    [Test]
    procedure AreEqual_T_Throws_No_Exception_When_Interfaces_Are_Equal;

    [Test]
    procedure AreEqual_T_Throws_ETestFailure_When_Interfaces_Are_NotEqual;

    [Test]
    procedure AreEqual_T_Throws_ETestFailure_When_Interfaces_Are_Nil;

    [Test]
    procedure AreEqual_T_Throws_No_Exception_When_Objects_Are_Equal;

    [Test]
    procedure AreEqual_T_Throws_ETestFailure_When_Objects_Are_NotEqual;

    [Test]
    procedure AreEqual_T_Throws_ETestFailure_When_Objects_Are_Nil;
{$ENDIF}

    [Test]
    procedure AreEqualMemory_Throws_No_Exception_When_Pointers_Are_Equal;

    [Test]
    procedure AreEqualMemory_Throws_ETestFailure_When_Pointers_Are_NotEqual;

    [Test]
    procedure AreEqual_Throws_No_Exception_When_Values_Are_Exactly_Equal;

    [Test]
    procedure AreNotEqual_Integer_Throws_No_Exception_When_Values_Are_NotEqual;

    [Test]
    procedure AreNotEqual_Integer_Throws_Exception_When_Values_Are_Equal;

    [Test]
    procedure AreNotEqual_GUID_Throws_No_Exception_When_Values_Are_NotEqual;

    [Test]
    procedure AreNotEqual_GUID_Throws_Exception_When_Values_Are_Equal;

    [Test]
    procedure WillRaise_Without_Exception_Class_Will_Capture_Any_Exception;

    [Test]
    procedure WillRaiseWithMessage_Exception_And_Message_Will_Check_ExceptionClass_And_Exception_Message;

    [Test]
    procedure WillRaiseWithMessage_Without_Exception_Class_And_Message_Will_Capture_Any_Exception;

    [Test]
    procedure WillRaiseWithMessage_Without_Exception_Class_With_Message_Will_Capture_Any_Exception_With_Message;

    [Test]
    procedure WillRaiseWithMessage_Exception_Not_Thrown_Throws_ETestFailure_Exception;

    [Test]
    procedure WillRaiseDescenadant_With_NonDescendingClass;

    [Test]
    procedure WillRaiseDescenadant_With_DescendingClass;

    [Test]
    procedure WillRaiseDescenadant_With_ExactClass;

    [Test]
    procedure WillRaiseAny;

    [Test]
    procedure WillRaiseAny_NoExecption;

    [Test]
    procedure WillNotRaise_With_ExactClass_Positive;

    [Test]
    procedure WillNotRaise_With_ExactClass_Negative;

    [Test]
    procedure WillNotRaise_With_DescendingClass_Positive;

    [Test]
    procedure WillNotRaise_With_DescendingClass_Negative;

    [Test]
    procedure WillNotRaise_With_NoClass;

    [Test]
    procedure Test_Implements_Will_Fail_If_Not_Implemented;

    [Test]
    procedure Test_Implements_Will_Pass_If_Implemented;

    [Test]
    procedure Test_AreSameOnSameObjectWithDifferentInterfaces_No_Exception;

    [Test]
    procedure Test_AreNotSameOnSameObjectWithDifferentInterfaces_Throws_Exception;

    [Test]
    procedure Contains_ArrayOfT_Throws_No_Exception_When_Value_In_Array;

    [Test]
    procedure Contains_ArrayOfT_Throws_Exception_When_Value_Not_In_Array;

    [Test]
    procedure DoesNotContain_ArrayOfT_Throws_No_Exception_When_Value_Not_In_Array;

    [Test]
    procedure DoesNotContain_ArrayOfT_Throws_Exception_When_Value_In_Array;

    [Test]
    [TestCase( 'substring', 'a str,a string,false' )]
    [TestCase( 'substring - case sensitive', 'a str,a string,true' )]
    procedure StartsWith_SubString_Is_At_The_Start_Of_String(const subString, theString: string; caseSensitive: boolean);

    [Test]
    [TestCase( 'empty substring', ',a string,false' )]
    [TestCase( 'empty substring - case sensitive', ',a string,true' )]
    [TestCase( 'empty string', 'substring,,false' )]
    [TestCase( 'empty string - case sensitive', 'substring,,true' )]
    [TestCase( 'at end of string', 'substring,at the end if the substring,false' )]
    [TestCase( 'at end of string - case sensitive', 'substring,at the end if the substring,true' )]
    [TestCase( 'in the middle of string', 'substring,the substring is in the middle,false' )]
    [TestCase( 'in the middle of string - case sensitive', 'substring,the substring is in the middle,true' )]
    [TestCase( 'not in the string', 'something else,the substring is not here,false' )]
    [TestCase( 'not in the string - case sensitive', 'something else,the substring is not here,true' )]
    procedure StartsWith_SubString_Is_Not_At_Start( const subString, theString: string; caseSensitive: boolean );

    [Test]
    [TestCase( 'substring', 'ing,a string,false' )]
    [TestCase( 'substring - case sensitive', 'ing,a string,true' )]
    procedure EndsWith_SubString_Is_At_The_End_Of_String( const subString, theString: string; caseSensitive: boolean );

    [Test]
    [TestCase( 'empty substring', ',a string,false' )]
    [TestCase( 'empty substring - case sensitive', ',a string,true' )]
    [TestCase( 'empty string', 'substring,,false' )]
    [TestCase( 'empty string - case sensitive', 'substring,,true' )]
    [TestCase( 'at start of string', 'at the,at the end if the substring,false' )]
    [TestCase( 'at start of string - case sensitive', 'at the,at the end if the substring,true' )]
    [TestCase( 'in the middle of string', 'substring,the substring is in the middle,false' )]
    [TestCase( 'in the middle of string - case sensitive', 'substring,the substring is in the middle,true' )]
    [TestCase( 'not in the string', 'something else,the substring is not here,false' )]
    [TestCase( 'not in the string - case sensitive', 'something else,the substring is not here,true' )]
    procedure EndsWith_SubString_Is_Not_At_End( const subString, theString: string; caseSensitive: boolean );

    [Test]
    procedure IgnoreCaseDefault;

{$IFDEF SUPPORTS_REGEX}
    [Test]
    [TestCase('GUID with dash', '[0-9A-F]{8}[-]?([0-9A-F]{4}[-]?){3}[0-9A-F]{12},C687683F-F25B-4F9A-A231-31C52253B6A1')]
    [TestCase('GUID without dash', '[0-9A-F]{8}[-]?([0-9A-F]{4}[-]?){3}[0-9A-F]{12},C687683FF25B4F9AA23131C52253B6A1')]
    procedure IsMatch_True_Will_Not_Raise(const regexPattern, theString: string);

    [Test]
    [TestCase('GUID with dash', '[0-9A-F]{8}[-]([0-9A-F]{4}[-]){3}[0-9A-F]{12},C687683F-F25B-4F9A-A231-31C52253B6A#')]
    [TestCase('GUID without dash', '[0-9A-F]{8}[-]?([0-9A-F]{4}[-]?){3}[0-9A-F]{12},C687683FF25B4F9AA23131C52253B6A#')]
    procedure IsMatch_False_Will_Raise_ETestFailure(const regexPattern, theString: string);
{$ENDIF}
  end;

implementation

uses
  {$IFDEF USE_NS}
  System.SysUtils,
  System.Classes,
  {$ELSE}
  SysUtils,
  Classes,
  {$ENDIF}
  DUnitX.Exceptions,
  DUnitX.Assert;

type
  {$M+}
  IMockInterface = interface
    ['{8DB1A216-0E95-4241-9522-C50DF99AFB71}']
    procedure Stub;
  end;
  {$M-}

  TMockClassOne = class(TObject)
  end;
  TMockClassTwo = class(TObject)
  end;

  IAmImplemented = interface
    ['{4B503CDD-6262-403C-BF68-5D5DE01C3B13}']
  end;

  TImplemented = class(TInterfacedObject,IAmImplemented)
  end;

  //Mask to override the default AssertEx in DUnitX.TestFramework
  Assert = class(DUnitX.Assert.Assert);

procedure TTestsAssert.Fail_Throws_ETestFailure_Exception;
begin
  Assert.WillRaise(
    procedure
    begin
      Assert.Fail
    end, ETestFailure);
end;

procedure TTestsAssert.Fail_Throws_ETestFailure_Exception_With_Caller_Address_Reference;
var
  MyProc : TTestLocalMethod;
begin
  MyProc := procedure
    begin
      Assert.WillRaise(
        procedure
        begin
          Assert.Fail;

          //We will raise at this point, when we return.
        end, ETestFailure);
    end;

  Assert.WillNotRaise(MyProc, ETestFailure);
end;

procedure TTestsAssert.Fail_Throws_ETestFailure_Exception_With_Message;
const
  EXPECTED_EXCEPTION_MSG = 'Failed Message';
begin
  Assert.WillRaise(
    procedure
    begin
      Assert.Fail(EXPECTED_EXCEPTION_MSG);
    end, ETestFailure, EXPECTED_EXCEPTION_MSG);
end;

procedure TTestsAssert.Fail_Throws_ETestFailure_Exception_With_Return_Address_Reference;
var
  MyProc : TTestLocalMethod;
begin
  MyProc := procedure
    begin
      Assert.WillNotRaise(
        procedure
        begin
          Assert.Fail('', @MyProc);

          //We will not raise internally, as we are passing the address of our caller
        end, ETestFailure);

      //We will raise it here, when MyProc returns.
    end;

  Assert.WillRaise(MyProc, ETestFailure);
  MyProc := nil;
end;

procedure TTestsAssert.IgnoreCaseDefault;
const
  cStr1 = 'String';
  cStr2 = 'STRING';
var
  OldVal: Boolean;
begin
  //Don't assume start value
  OldVal := Assert.IgnoreCaseDefault;
  try
    Assert.IgnoreCaseDefault := True;
    Assert.WillNotRaiseAny(procedure begin Assert.AreEqual(cStr1, cStr2) end);
    Assert.WillNotRaiseAny(procedure begin Assert.StartsWith(cStr1, cStr2) end);
    Assert.WillNotRaiseAny(procedure begin Assert.EndsWith(cStr1, cStr2) end);
    Assert.WillRaise(procedure begin Assert.AreNotEqual(cStr1, cStr2) end, ETestFailure);

    Assert.IgnoreCaseDefault := False;
    Assert.WillRaise(procedure begin Assert.AreEqual(cStr1, cStr2) end, ETestFailure);
    Assert.WillRaise(procedure begin Assert.StartsWith(cStr1, cStr2) end, ETestFailure);
    Assert.WillRaise(procedure begin Assert.EndsWith(cStr1, cStr2) end, ETestFailure);
    Assert.WillNotRaiseAny(procedure begin Assert.AreNotEqual(cStr1, cStr2) end);
  finally
    Assert.IgnoreCaseDefault := OldVal;
  end;
end;

procedure TTestsAssert.NoDiff_Throws_ETestFailure_When_Strings_Are_NotEqual;
begin
  Assert.WillRaiseWithMessage(procedure
    begin
      Assert.NoDiff('  '#8, ' ');
    end, ETestFailure, 'Length of strings is not equal: Expected 3 but got 1 ');

  Assert.WillRaiseWithMessage(procedure
    begin
      Assert.NoDiff('lorem ipsum', 'lorem ipsum ', 'characters');
    end,
    ETestFailure, 'Length of strings is not equal: Expected 11 but got 12 characters');

  Assert.WillRaiseWithMessage(procedure
    begin
      Assert.NoDiff('lorem ipsum', 'lorem Ipsum');
    end,
    ETestFailure, 'Difference at position 7: [''ipsum''] does not match [''Ipsum''] ');

  Assert.WillRaiseWithMessage(procedure
    begin
      Assert.NoDiff(#13, #10);
    end,
    ETestFailure, 'Difference at position 1: [#13] does not match [#10] ');

  Assert.WillRaiseWithMessage(procedure
    begin
      Assert.NoDiff('lorem ipsum'#9' ', 'lorem'#13'ipsum'#13#10);
    end,
    ETestFailure, 'Difference at position 6: ['' ipsum''#9'' ''] does not match [#13''ipsum''#13#10] ');
end;

procedure TTestsAssert.NoDiff_Throws_No_Exception_When_Strings_Are_Equal;
begin
  Assert.WillNotRaise(procedure
    begin
      Assert.NoDiff('', '');
    end);
  Assert.WillNotRaise(procedure
    begin
      Assert.NoDiff(#13, #13);
    end);
  Assert.WillNotRaise(procedure
    begin
      Assert.NoDiff('abc', 'abc');
    end);
  Assert.WillNotRaise(procedure
    begin
      Assert.NoDiff('abc', 'ABC', true);
    end);
  Assert.WillNotRaise(procedure
    begin
      Assert.NoDiff('LoReM iPsUm DoLoR sIt AmEt', 'lOrEm IpSuM dOlOr SiT aMeT', true);
    end);
end;

procedure TTestsAssert.Pass_Throws_ETestPass_Exception;
begin
  Assert.WillRaise(
    procedure
    begin
      Assert.Pass;
    end, ETestPass);
end;

procedure TTestsAssert.Pass_Throws_ETestPass_Exception_With_Message;
const
  EXPECTED_EXCEPTION_MSG = 'Passing Message';
begin
  Assert.WillRaise(
    procedure
    begin
      Assert.Pass(EXPECTED_EXCEPTION_MSG);
    end, ETestPass, EXPECTED_EXCEPTION_MSG);
end;

procedure TTestsAssert.Test_Implements_Will_Fail_If_Not_Implemented;
var
  obj : IInterface;
  res : IAmImplemented;
begin
  obj := TInterfacedObject.Create;
  Assert.WillRaise(
  procedure
  begin
    res := Assert.Implements<IAmImplemented>(obj);
  end,
  ETestFailure);
end;

procedure TTestsAssert.Test_Implements_Will_Pass_If_Implemented;
var
  obj : IInterface;
  res : IAmImplemented;
begin
  obj := TImplemented.Create;
  Assert.WillNotRaiseAny(
    procedure
    begin
      res := Assert.Implements<IAmImplemented>(obj);
    end);
  Assert.IsNotNull(res);
end;

procedure TTestsAssert.WillNotRaise_With_DescendingClass_Negative;
begin
 Assert.WillRaise(
  procedure
  begin
    Assert.WillNotRaiseDescendant(
      procedure
      begin
        raise EFilerError.Create('Test');
      end,
      EStreamError);
  end,
  ETestFailure);
end;

procedure TTestsAssert.WillNotRaise_With_DescendingClass_Positive;
begin
    Assert.WillNotRaiseDescendant(
      procedure
      begin
        raise Exception.Create('Test');
      end,
      EStreamError);
end;

procedure TTestsAssert.WillNotRaise_With_ExactClass_Negative;
const
  EXPECTED_EXCEPTION_MSG = 'Passing Message';
begin
 Assert.WillRaise(
 procedure
 begin
   Assert.WillNotRaise(
     procedure
     begin
       raise EStreamError.Create('Error Message');
     end,
     EStreamError,EXPECTED_EXCEPTION_MSG);
 end,
 ETestFailure,
 EXPECTED_EXCEPTION_MSG);
end;

procedure TTestsAssert.WillNotRaise_With_ExactClass_Positive;
begin
 Assert.WillNotRaise(
   procedure
   begin
     // Don't raise an exception we are looking for.
     raise Exception.Create('Error Message');
   end,
   EStreamError,'');
end;

procedure TTestsAssert.WillNotRaise_With_NoClass;
const
  EXPECTED_EXCEPTION_MSG = 'Passing Message';
begin
 Assert.WillRaise(
 procedure
 begin
   Assert.WillNotRaise(
     procedure
     begin
       // Raise an exception
       raise Exception.Create('Error Message');
     end);
 end,
 ETestFailure,
 EXPECTED_EXCEPTION_MSG);
end;

procedure TTestsAssert.WillRaiseAny;
begin
  Assert.WillRaiseAny(
    procedure
    begin
      raise EAbort.Create('Test');
    end);
end;

procedure TTestsAssert.WillRaiseAny_NoExecption;
const
  EXPECTED_EXCEPTION_MSG = 'Failed Message';
begin
  Assert.WillRaise(
    procedure
    begin
       Assert.WillRaiseAny(
               procedure
               begin
                 // Do nothing on purpose.
               end
       );
    end, ETestFailure, EXPECTED_EXCEPTION_MSG);
end;

procedure TTestsAssert.WillRaiseDescenadant_With_DescendingClass;
const
  EXPECTED_EXCEPTION_MSG = 'Failed Message';
begin
 Assert.WillRaiseDescendant(
   procedure
   begin
     raise EFilerError.Create('Test');
   end,
   EStreamError,EXPECTED_EXCEPTION_MSG);

end;

procedure TTestsAssert.WillRaiseDescenadant_With_ExactClass;
const
  EXPECTED_EXCEPTION_MSG = 'Failed Message';
begin
 Assert.WillRaiseDescendant(
   procedure
   begin
     raise EFilerError.Create('Test');
   end,
   EFilerError,EXPECTED_EXCEPTION_MSG);
end;

procedure TTestsAssert.WillRaiseDescenadant_With_NonDescendingClass;
const
  EXPECTED_EXCEPTION_MSG = 'Failed Message';
begin
 Assert.WillRaise(
   procedure
   begin
     Assert.WillRaiseDescendant(
       procedure
       begin
         raise Exception.Create('Test');
       end,
       EStreamError,EXPECTED_EXCEPTION_MSG);
   end,
   ETestFailure,EXPECTED_EXCEPTION_MSG);
end;

procedure TTestsAssert.WillRaiseWithMessage_Exception_And_Message_Will_Check_ExceptionClass_And_Exception_Message;
const
  EXPECTED_EXCEPTION_MSG = 'Passing Message';
begin
  Assert.WillRaiseWithMessage(
    procedure
    begin
      Assert.Pass(EXPECTED_EXCEPTION_MSG);
    end, ETestPass, EXPECTED_EXCEPTION_MSG);
end;

procedure TTestsAssert.WillRaiseWithMessage_Exception_Not_Thrown_Throws_ETestFailure_Exception;
const
  EXPECTED_EXCEPTION_MSG = 'Failed Message';
begin
  Assert.WillRaise(
    procedure
    begin
      Assert.WillRaiseWithMessage(nil, ETestFailure);
    end, ETestFailure, EXPECTED_EXCEPTION_MSG);
end;

procedure TTestsAssert.WillRaiseWithMessage_Without_Exception_Class_And_Message_Will_Capture_Any_Exception;
begin
  Assert.WillRaiseWithMessage(
    procedure
    begin
      raise Exception.Create('Pass');
    end);
end;

procedure TTestsAssert.WillRaiseWithMessage_Without_Exception_Class_With_Message_Will_Capture_Any_Exception_With_Message;
const
  EXPECTED_EXCEPTION_MSG = 'Passing Message';
begin
  Assert.WillRaiseWithMessage(
    procedure
    begin
      raise Exception.Create(EXPECTED_EXCEPTION_MSG);
    end, nil, EXPECTED_EXCEPTION_MSG);
end;

procedure TTestsAssert.WillRaise_Without_Exception_Class_Will_Capture_Any_Exception;
begin
  Assert.WillRaise(
    procedure
    begin
      raise Exception.Create('Test')
    end);
end;

procedure TTestsAssert.AreEqualMemory_Throws_ETestFailure_When_Pointers_Are_NotEqual;
begin
  Assert.Pass;
end;

procedure TTestsAssert.AreEqualMemory_Throws_No_Exception_When_Pointers_Are_Equal;
var
  mock : IInterface;
begin
  mock := TInterfacedObject.Create();

  Assert.WillNotRaise(
    procedure
    begin
      Assert.AreEqualMemory(@mock, @mock, SizeOf(mock));
    end, ETestFailure);
end;

procedure TTestsAssert.AreEqual_Double_Throws_ETestFailure_When_Values_Are_NotEqual;
const
  ACTUAL_DOUBLE : double = 1.19E20;
  EXPECTED_DOUBLE : double = 1.18E20;
  TOLERANCE_DOUBLE : double = 0.001E20;
begin
  Assert.WillRaise(
    procedure
    begin
      Assert.AreEqual(ACTUAL_DOUBLE, EXPECTED_DOUBLE, TOLERANCE_DOUBLE);
    end, ETestFailure, Format('[%e] with in [%e] from [%e]', [ACTUAL_DOUBLE, TOLERANCE_DOUBLE, EXPECTED_DOUBLE]));
end;

procedure TTestsAssert.AreEqual_Double_Throws_No_Exception_When_Values_Are_Equal;
const
  ACTUAL_DOUBLE : double = 1.19E20;
  EXPECTED_DOUBLE : double  = 1.18E20;
  TOLERANCE_DOUBLE : double  = 0.011E20;
begin
  Assert.WillNotRaise(
    procedure
    begin
      Assert.AreEqual(ACTUAL_DOUBLE, EXPECTED_DOUBLE, TOLERANCE_DOUBLE);
    end, Exception);
end;

procedure TTestsAssert.AreEqual_Extended_Throws_ETestFailure_When_Values_Are_NotEqual;
const
  ACTUAL_EXTENDED : extended  = 1.19E20;
  EXPECTED_EXTENDED : extended = 1.18E20;
  TOLERANCE_EXTENDED : extended = 0.001E20;
begin
  Assert.WillRaise(
    procedure
    begin
      Assert.AreEqual(ACTUAL_EXTENDED, EXPECTED_EXTENDED, TOLERANCE_EXTENDED);
    end, ETestFailure, Format('[%e] with in [%e] from [%e]', [ACTUAL_EXTENDED, TOLERANCE_EXTENDED, EXPECTED_EXTENDED]));
end;

procedure TTestsAssert.AreEqual_Extended_Throws_No_Exception_When_Values_Are_Equal;
const
  ACTUAL_EXTENDED : extended = 1.19E20;
  EXPECTED_EXTENDED : extended = 1.18E20;
  TOLERANCE_EXTENDED : extended = 0.011E20;
begin
  Assert.WillNotRaise(
    procedure
    begin
      Assert.AreEqual(ACTUAL_EXTENDED, EXPECTED_EXTENDED, TOLERANCE_EXTENDED);
    end, Exception);
end;

procedure TTestsAssert.AreEqual_GUID_Throws_ETestFailure_When_Values_Are_NotEqual;
const
  EXPECTED_GUID: TGUID = (D1: 0; D2: 0; D3: 0; D4: (0, 0, 0, 0, 0, 0, 0, 0));
  ACTUAL_GUID: TGUID = (D1: 1; D2: 1; D3: 1; D4: (1, 1, 1, 1, 1, 1, 1, 1));
begin
  Assert.WillRaise(
    procedure
    begin
      Assert.AreEqual(EXPECTED_GUID, ACTUAL_GUID);
    end, ETestFailure);
end;

procedure TTestsAssert.AreEqual_GUID_Throws_No_Exception_When_Values_Are_Equal;
const
  EXPECTED_GUID: TGUID = (D1: 0; D2: 0; D3: 0; D4: (0, 0, 0, 0, 0, 0, 0, 0));
  ACTUAL_GUID: TGUID = (D1: 1; D2: 1; D3: 1; D4: (1, 1, 1, 1, 1, 1, 1, 1));
begin
  Assert.WillNotRaise(
    procedure
    begin
      Assert.AreNotEqual(EXPECTED_GUID, ACTUAL_GUID);
    end, Exception);
end;

procedure TTestsAssert.AreEqual_Integer_Throws_ETestFailure_When_Values_Are_NotEqual;
begin
  Assert.WillRaise(
    procedure
    begin
      Assert.AreEqual(1, 2);
    end, ETestFailure, Format('[%d] is Not Equal to [%d] %s', [2, 1, '']));
end;

procedure TTestsAssert.AreEqual_Integer_Throws_No_Exception_When_Values_Are_Equal;
begin
  Assert.WillNotRaise(
    procedure
    begin
      Assert.AreEqual(1, 1);
    end, Exception);
end;

procedure TTestsAssert.AreEqual_Stream_Throws_ETestFailure_When_Values_Are_NotEqual;
const
  TESTSTR_STRING = 'This is a test';
var
  Expected: TStringStream;
  Actual: TStringStream;
begin
  Expected := TStringStream.Create(TESTSTR_STRING);
  Actual := TStringStream.Create('A different value');
  try
    Assert.WillRaise(
      procedure
      begin
        Assert.AreEqual(Expected, Actual);
      end, ETestFailure);
  finally
    Expected.Free();
    Actual.Free();
  end;
end;

procedure TTestsAssert.AreEqual_Stream_Throws_No_Exception_When_Values_Are_Equal;
const
  TESTSTR_STRING = 'This is a test';
var
  Expected: TStringStream;
  Actual: TStringStream;
begin
  Expected := TStringStream.Create(TESTSTR_STRING);
  Actual := TStringStream.Create(TESTSTR_STRING);
  try
    Assert.WillNotRaise(
      procedure
      begin
        Assert.AreEqual(Expected, Actual);
      end, Exception);
  finally
    Expected.Free();
    Actual.Free();
  end;
end;

procedure TTestsAssert.AreEqual_String_Throws_No_Exception_When_Values_Are_Equal;
const
  ACTUAL_AND_EXPECTED_STRING = 'the brown dog jumped something';
begin
  Assert.WillNotRaise(
    procedure
    begin
      Assert.AreEqual(ACTUAL_AND_EXPECTED_STRING, ACTUAL_AND_EXPECTED_STRING);
    end, Exception);
end;

procedure TTestsAssert.AreEqual_TClass_Throws_ETestFailure_When_Classes_Are_NotEqual;
begin
  Assert.WillRaise(
    procedure
    begin
      Assert.AreEqual(TMockClassOne, TMockClassTwo);
    end, ETestFailure);
end;

procedure TTestsAssert.AreEqual_TClass_Throws_No_Exception_When_Classes_Are_Equal;
begin
  Assert.WillNotRaise(
    procedure
    begin
      Assert.AreEqual(TMockClassOne, TMockClassOne);
    end, ETestFailure);
end;

procedure TTestsAssert.AreEqual_Throws_No_Exception_When_Values_Are_Exactly_Equal;
var
  actualAndExpected, tolerance : Extended;
begin
  //Ensure they are the same value.
  actualAndExpected := 1.1;
  tolerance := 0;

  Assert.WillNotRaise(
    procedure
    begin
      Assert.AreEqual(actualAndExpected, actualAndExpected, tolerance);
    end, Exception);
end;

procedure TTestsAssert.AreEqual_TStrings_Throws_ETestFailure_When_Strings_Are_NotEqual;
var
  expected, actual: TStrings;
begin
  expected := TStringList.Create;
  actual := TStringList.Create;
  try
    expected.CommaText := '1,2,3';
    actual.CommaText := '1,2,3,4';
    Assert.WillRaiseWithMessage(procedure
      begin
        Assert.AreEqual(expected, actual);
      end, ETestFailure, 'Number of strings is not equal: Expected 3 but got 4 ');

    expected.CommaText := '"Lorem ipsum dolor sit amet","consectetur adipisici elit","sed eiusmod tempor incidunt"';
    actual.CommaText := '"Lorem ipsum dolor sit amet","consectetur adisipici elit","sed eiusmod tempor incidunt"';
    Assert.WillRaiseWithMessage(procedure
      begin
        Assert.AreEqual(expected, actual);
      end, ETestFailure,
      'Difference at position 16: [''pisici eli''] does not match [''sipici eli''] at line 2 ');
  finally
    expected.Free;
    actual.Free;
  end;
end;

procedure TTestsAssert.AreEqual_TStrings_Throws_No_Exception_When_Strings_Are_Equal;
var
  expected, actual: TStrings;
begin
  expected := TStringList.Create;
  actual := TStringList.Create;
  try
    Assert.WillNotRaise(procedure
      begin
        Assert.AreEqual(expected, actual);
      end, Exception);

    expected.CommaText := '1,2,3';
    actual.CommaText := '1,2,3';
    Assert.WillNotRaise(procedure
      begin
        Assert.AreEqual(expected, actual);
      end, Exception);

    expected.CommaText := '1,2,3';
    actual.CommaText := '1,-,3';
    Assert.WillNotRaise(procedure
      begin
        Assert.AreEqual(expected, actual, [2]);
      end, Exception);
  finally
    expected.Free;
    actual.Free;
  end;
end;

{$IFNDEF DELPHI_XE_DOWN}
procedure TTestsAssert.AreEqual_T_Throws_ETestFailure_When_Interfaces_Are_NotEqual;
var
  mock : IInterface;
  mock2 : IInterface;
begin
  mock := TInterfacedObject.Create();
  mock2 := TInterfacedObject.Create();

  Assert.WillRaise(
    procedure
    begin
      Assert.AreEqual<IInterface>(mock, mock2);
    end, ETestFailure);
end;

procedure TTestsAssert.AreEqual_T_Throws_ETestFailure_When_Objects_Are_Nil;
var
  mock : TObject;
  nilObject : TObject;
begin
  mock := TObject.Create();
  try
    nilObject := nil;

    Assert.WillRaise(
      procedure
      begin
        Assert.AreEqual<TObject>(mock, nilObject);
      end, ETestFailure);

    Assert.WillRaise(
      procedure
      begin
        Assert.AreEqual<TObject>(nilObject, mock);
      end, ETestFailure);
  finally
    FreeAndNil(mock);
  end;
end;

procedure TTestsAssert.AreEqual_T_Throws_ETestFailure_When_Objects_Are_NotEqual;
var
  mock : TObject;
  mock2 : TObject;
begin
  mock := TObject.Create;
  mock2 := TObject.Create;
  try
    Assert.WillRaise(
      procedure
      begin
        Assert.AreEqual<TObject>(mock, mock2);
      end, ETestFailure);
  finally
    FreeAndNil(mock);
    FreeAndNil(mock2);
  end;
end;

procedure TTestsAssert.AreEqual_T_Throws_ETestFailure_When_Interfaces_Are_Nil;
var
  mock : IInterface;
begin
  mock := TInterfacedObject.Create();

  Assert.WillRaise(
    procedure
    begin
      Assert.AreEqual<IInterface>(mock, nil);
    end, ETestFailure);

  Assert.WillRaise(
    procedure
    begin
      Assert.AreEqual<IInterface>(nil, mock);
    end, ETestFailure);
end;

procedure TTestsAssert.AreEqual_T_Throws_No_Exception_When_Interfaces_Are_Equal;
var
  mock : IInterface;
begin
  mock := TInterfacedObject.Create();

  Assert.WillNotRaise(
    procedure
    begin
      Assert.AreEqual<IInterface>(mock, mock);
    end, ETestFailure);
end;

procedure TTestsAssert.AreEqual_T_Throws_No_Exception_When_Objects_Are_Equal;
var
  mock : TObject;
begin
  mock := TObject.Create;
  try
    Assert.WillNotRaise(
      procedure
      begin
        Assert.AreEqual<TObject>(mock, mock);
      end, ETestFailure);
  finally
    FreeAndNil(mock);
  end;
end;
{$ENDIF}

procedure TTestsAssert.Test_AreSameOnSameObjectWithDifferentInterfaces_No_Exception;
var
  myObject  : IInterfaceList;
begin
  myObject := TInterfaceList.Create;
  Assert.WillNotRaise(
    procedure
    begin
      Assert.AreSame(myObject, myObject as IInterface);
    end, ETestFailure);
end;

procedure TTestsAssert.Test_AreNotSameOnSameObjectWithDifferentInterfaces_Throws_Exception;
var
  myObject  : IInterfaceList;
begin
  myObject := TInterfaceList.Create;
  Assert.WillRaise(
    procedure
    begin
      Assert.AreNotSame(myObject, myObject as IInterface);
    end, ETestFailure);
end;

procedure TTestsAssert.AreNotEqual_Integer_Throws_No_Exception_When_Values_Are_NotEqual;
begin
  Assert.WillNotRaise(
    procedure
    begin
      Assert.AreNotEqual(1, 2);
    end, ETestFailure);
end;

procedure TTestsAssert.AreNotEqual_GUID_Throws_Exception_When_Values_Are_Equal;
const
  EXPECTED_GUID: TGUID = (D1: 0; D2: 0; D3: 0; D4: (0, 0, 0, 0, 0, 0, 0, 0));
  ACTUAL_GUID: TGUID = (D1: 0; D2: 0; D3: 0; D4: (0, 0, 0, 0, 0, 0, 0, 0));
begin
  Assert.WillRaise(
    procedure
    begin
      Assert.AreNotEqual(EXPECTED_GUID, ACTUAL_GUID);
    end, ETestFailure);
end;

procedure TTestsAssert.AreNotEqual_GUID_Throws_No_Exception_When_Values_Are_NotEqual;
const
  EXPECTED_GUID: TGUID = (D1: 0; D2: 0; D3: 0; D4: (0, 0, 0, 0, 0, 0, 0, 0));
  ACTUAL_GUID: TGUID = (D1: 1; D2: 1; D3: 1; D4: (1, 1, 1, 1, 1, 1, 1, 1));
begin
  Assert.WillNotRaise(
    procedure
    begin
      Assert.AreNotEqual(EXPECTED_GUID, ACTUAL_GUID);
    end, ETestFailure);
end;

procedure TTestsAssert.AreNotEqual_Integer_Throws_Exception_When_Values_Are_Equal;
begin
  Assert.WillRaise(
    procedure
    begin
      Assert.AreNotEqual(1, 1);
    end, ETestFailure);
end;

procedure TTestsAssert.Contains_ArrayOfT_Throws_No_Exception_When_Value_In_Array;
begin
  Assert.WillNotRaise(
    procedure
    begin
      Assert.Contains<string>(['x', 'y', 'z'], 'x');
    end, ETestFailure);
end;

procedure TTestsAssert.Contains_ArrayOfT_Throws_Exception_When_Value_Not_In_Array;
begin
  Assert.WillRaise(
    procedure
    begin
      Assert.Contains<string>(['x', 'y', 'z'], 'a');
    end, ETestFailure);
end;

procedure TTestsAssert.DoesNotContain_ArrayOfT_Throws_No_Exception_When_Value_Not_In_Array;
begin
  Assert.WillNotRaise(
    procedure
    begin
      Assert.DoesNotContain<string>(['x', 'y', 'z'], 'a');
    end, ETestFailure);
end;

procedure TTestsAssert.DoesNotContain_ArrayOfT_Throws_Exception_When_Value_In_Array;
begin
  Assert.WillRaise(
    procedure
    begin
      Assert.DoesNotContain<string>(['x', 'y', 'z'], 'x');
    end, ETestFailure);
end;

procedure TTestsAssert.StartsWith_SubString_Is_At_The_Start_Of_String(const subString, theString: string; caseSensitive: boolean);
begin
  Assert.WillNotRaise(
    procedure
    begin
      Assert.StartsWith( subString, theString, caseSensitive );
    end, ETestFailure);
end;

procedure TTestsAssert.StartsWith_SubString_Is_Not_At_Start( const subString, theString: string; caseSensitive: boolean );
begin
  Assert.WillRaise(
    procedure
    begin
      Assert.StartsWith( subString, theString, caseSensitive );
    end, ETestFailure);
end;

procedure TTestsAssert.EndsWith_SubString_Is_At_The_End_Of_String(const subString, theString: string; caseSensitive: boolean);
begin
  Assert.WillNotRaise(
    procedure
    begin
      Assert.EndsWith( subString, theString, caseSensitive );
    end, ETestFailure);
end;

procedure TTestsAssert.EndsWith_SubString_Is_Not_At_End( const subString, theString: string; caseSensitive: boolean );
begin
  Assert.WillRaise(
    procedure
    begin
      Assert.EndsWith( subString, theString, caseSensitive );
    end, ETestFailure);
end;

{$IFDEF SUPPORTS_REGEX}
procedure TTestsAssert.IsMatch_True_Will_Not_Raise(const regexPattern, theString: string);
begin
  Assert.WillNotRaiseAny(
    procedure
    begin
      Assert.IsMatch(regexPattern, theString);
    end
  );
end;

procedure TTestsAssert.IsMatch_False_Will_Raise_ETestFailure(const regexPattern, theString: string);
begin
  Assert.WillRaise(
    procedure
    begin
      Assert.IsMatch(regexPattern, theString);
    end, ETestFailure);
end;
{$ENDIF}

initialization
  TDUnitX.RegisterTestFixture(TTestsAssert);

end.
