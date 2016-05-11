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

unit DUnitX.Tests.TestFixture;

interface

uses
  DUnitX.TestFramework,
  DUnitX.TestFixture;

const
  TIMES_RUN = 3;
  TIMES_RUN_ANYWAY = 5;
  TIMES_RUN_TEST_CASE = 2;

type
  {$M+}
  [TestFixture]
  TTestClassWithNonPublicSetup = class
  private
    FSetupRun : Boolean;
  protected
    [Setup]
    procedure Setup;
  public
    constructor Create;
    property SetupRun : Boolean read FSetupRun;
  end;
  {$M-}

  MockTestSourceAttribute = class(CustomTestCaseSourceAttribute)
  protected
    function GetCaseInfoArray : TestCaseInfoArray; override;
  end;

  {$M+}
  [TestFixture]
  TTestClassWithTestSource = class
  public
    [Test]
    [MockTestSource]
    procedure DataTest(Value : Integer);
  end;
  {$M-}

  {$M+}
  [TestFixture]
  TTestRepeatAttribute = class
  public
    [SetUpFixture]
    procedure SetUpFixture;

    [TearDownFixture]
    procedure TearDownFixture;

    [Test]
    [RepeatTest(TIMES_RUN)]
    procedure TestRepeat3Times;

    [RepeatTest(TIMES_RUN_TEST_CASE)]
    [Test]
    [TestCase('Sum Case', '1,2,3')]
    procedure Sum(const A, B, Expected: Integer);

    [Test]
    [RepeatTest(0)]
    procedure IgnoreMeWhenRepeatIsZero;
  published
    [RepeatTest(TIMES_RUN_ANYWAY)]
    procedure TestRepeat5TimesAnyWay;

    [Test]
    [RepeatTest(0)]
    procedure IgnoreMeAnyWayWhenRepeatIsZero;
  end;
  {$M-}

  {$M+}
  [TestFixture]
  TTestMaxTimeAttribute = class
  public
    [Test]
    [MaxTime(0)]
    procedure MaxTimeIsZero;

    //We ignore the timeout test for the moment. We require a way to test timeout without calling it to fail the test itself.
    [Test, Ignore]
    [MaxTime(100)]
    procedure TestTimeout;

    [Test]
    [MaxTime(100)]
    procedure TestDoesNotTimeout;
  end;
  {$M-}

implementation

uses
  {$IFDEF USE_NS}
  System.Math,
  System.SysUtils;
  {$ELSE}
  Math,
  SysUtils;
  {$ENDIF}

var
  _TimesRun: Integer;
  _TimesRunAnyWay: Integer;
  _TimesRunTestCase: Integer;

{ TDUnitXTestFixtureTests }

{ TTestClassWithNonPublicSetup }

constructor TTestClassWithNonPublicSetup.Create;
begin
  inherited Create;
  FSetupRun := False;
end;

procedure TTestClassWithNonPublicSetup.Setup;
begin
  //Optimised out as the method is not used internally;
  FSetupRun := True;
end;

{ TTestSourceAttribute }

function MockTestSourceAttribute.GetCaseInfoArray: TestCaseInfoArray;
var
 I : Integer;
begin
  SetLength(result,3);
  for I := 0 to 2 do
  begin
     result[I].Name := 'DataTest' + IntToStr(I);
     SetLength(result[I].Values,1);
     result[I].Values[0] := I;
  end;
end;

{ TTestClassWithTestSource }

procedure TTestClassWithTestSource.DataTest(Value: Integer);
begin
  TDUnitX.CurrentRunner.Status(Format('DataTest(%d) Called',[Value]));
  Assert.IsTrue(InRange(Value,0,2));
end;

{ TTestRepeatAttribute }

procedure TTestRepeatAttribute.TestRepeat3Times;
begin
  Inc(_TimesRun);
  Assert.Pass;
end;

procedure TTestRepeatAttribute.TestRepeat5TimesAnyWay;
begin
  Inc(_TimesRunAnyWay);
  Sleep(5);
  Assert.Pass;
end;

procedure TTestRepeatAttribute.IgnoreMeAnyWayWhenRepeatIsZero;
begin
  Assert.IsTrue(false,'I should not have been called!');
end;

procedure TTestRepeatAttribute.IgnoreMeWhenRepeatIsZero;
begin
  Assert.IsTrue(false,'I should not have been called!');
end;

procedure TTestRepeatAttribute.SetUpFixture;
begin
  _TimesRun := 0;
  _TimesRunAnyWay := 0;
  _TimesRunTestCase := 0;
end;

procedure TTestRepeatAttribute.Sum(const A, B, Expected: Integer);
begin
  Assert.AreEqual(Expected, A + B);
  Inc(_TimesRunTestCase);
end;

procedure TTestRepeatAttribute.TearDownFixture;
begin
  Assert.AreEqual(TIMES_RUN, _TimesRun, 'TimesRun');
  Assert.AreEqual(TIMES_RUN_ANYWAY, _TimesRunAnyWay, 'TimesRunAnyway');
  Assert.AreEqual(TIMES_RUN_TEST_CASE, _TimesRunTestCase, 'TimesRunTestCase');
end;

{ TTestMaxTimeAttribute }

procedure TTestMaxTimeAttribute.MaxTimeIsZero;
begin
  Sleep( 100 );
  Assert.Pass;  // even after a delay,
                // the test should not timeout, as zero means no timeout
end;

procedure TTestMaxTimeAttribute.TestTimeout;
begin
  Sleep( 150 );
  Assert.Fail('Timeout should catch this test.');  // the test should time out before this point
end;

procedure TTestMaxTimeAttribute.TestDoesNotTimeout;
begin
  Assert.Pass;  // the test should NOT time out before this point
end;


initialization
  TDUnitX.RegisterTestFixture(TTestClassWithNonPublicSetup);
  TDUnitX.RegisterTestFixture(TTestClassWithTestSource);
  TDUnitX.RegisterTestFixture(TTestRepeatAttribute);
  TDUnitX.RegisterTestFixture(TTestMaxTimeAttribute);

end.
