{***************************************************************************}
{                                                                           }
{           DUnitX                                                          }
{                                                                           }
{           Copyright (C) 2012 Vincent Parrett                              }
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

uses
  DUnitX.TestFramework;


type
  {+M}
  [TestFixture]
  TTestsAssert = class
  private
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
    procedure AreEqual_String_Throws_ETestFailure_When_Values_Are_NotEqual;
    [Test]
    procedure AreEqual_Extended_Throws_No_Exception_When_Values_Are_Equal;
    [Test]
    procedure AreEqual_Extended_Throws_ETestFailure_When_Values_Are_NotEqual;
    [Test]
    procedure AreEqual_TClass_Throws_No_Exception_When_Classes_Are_Equal;
    [Test]
    procedure AreEqual_TClass_Throws_ETestFailure_When_Classes_Are_NotEqual;
    [Test]
    procedure AreEqual_T_Throws_No_Exception_When_Classes_Are_Equal;
    [Test]
    procedure AreEqual_T_Throws_ETestFailure_When_Classes_Are_NotEqual;

    [Test]
    procedure AreEqual_T_Throws_ETestFailure_When_Interfaces_Are_NotEqual_OrNil;

    [Test]
    procedure Warn_Throws_ETestWarning_Exception;
    [Test]
    procedure AreEqual_Throws_No_Exception_When_Values_Are_Exactly_Equal;
  end;

implementation

uses
  Delphi.Mocks,
  SysUtils;

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

{ TTestAssert }

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

procedure TTestsAssert.Warn_Throws_ETestWarning_Exception;
begin
  Assert.WillRaise(
    procedure
    begin
      Assert.Warn;
    end, ETestWarning);
end;

procedure TTestsAssert.AreEqual_Extended_Throws_ETestFailure_When_Values_Are_NotEqual;
const
  ACTUAL_EXTENDED = 1.19E20;
  EXPECTED_EXTENDED = 1.18E20;
  TOLERANCE_EXTENDED = 0.001E20;
begin
  Assert.WillRaise(
    procedure
    begin
      Assert.AreEqual(ACTUAL_EXTENDED, EXPECTED_EXTENDED, TOLERANCE_EXTENDED);
    end, ETestFailure, Format('[%e] with in [%e] from [%e]', [ACTUAL_EXTENDED, TOLERANCE_EXTENDED, EXPECTED_EXTENDED]));

end;

procedure TTestsAssert.AreEqual_Extended_Throws_No_Exception_When_Values_Are_Equal;
const
  ACTUAL_EXTENDED = 1.19E20;
  EXPECTED_EXTENDED = 1.18E20;
  TOLERANCE_EXTENDED = 0.011E20;
begin
  Assert.WillNotRaise(
    procedure
    begin
      Assert.AreEqual(ACTUAL_EXTENDED, EXPECTED_EXTENDED, TOLERANCE_EXTENDED);
    end, Exception);
end;

procedure TTestsAssert.AreEqual_String_Throws_ETestFailure_When_Values_Are_NotEqual;
const
  ACTUAL_STRING = 'the brown dog jumped something';
  EXPECTED_STRING = 'SOMETHING JUMPED THE BROWN DOG';
begin
  Assert.WillRaise(
    procedure
    begin
      Assert.AreEqual(ACTUAL_STRING, EXPECTED_STRING);
    end, ETestFailure, Format('[%s] is Not Equal to [%s] %s', [ACTUAL_STRING, EXPECTED_STRING, '']));
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

procedure TTestsAssert.AreEqual_T_Throws_ETestFailure_When_Classes_Are_NotEqual;
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

  //TODO: Fix generic are equals invalid cast error. TValue of a class does not allow AsString
end;


procedure TTestsAssert.AreEqual_T_Throws_ETestFailure_When_Interfaces_Are_NotEqual_OrNil;
var
  mock : IInterface;
begin
  mock := TInterfacedObject.Create();

  Assert.WillRaise(
    procedure
    begin
      Assert.AreEqual<IInterface>(mock, nil);
    end, ETestFailure);

  //TODO: Fix generic are equals invalid cast error. TValue of a class does not allow AsString
end;


procedure TTestsAssert.AreEqual_T_Throws_No_Exception_When_Classes_Are_Equal;
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

initialization
  TDUnitX.RegisterTestFixture(TTestsAssert);
end.
