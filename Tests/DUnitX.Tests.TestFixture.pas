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
  TIMES_RUN         = 3;
  TIMES_RUN_ANYWAY  = 5;
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

  MockTestSourceAttribute = class(CustomTestCaseGeneratorAttribute)
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
    [NamedTestCase('Sum Case', '1,2,3')]
    procedure Sum(const A, B, Expected : Integer);

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

    //Ignored, as a timeout fails the test itself.  TMaxTimeTests runs timed-out tests through a runner of its own instead.
    [Test, Ignore]
    [MaxTime(100)]
    procedure TestTimeout;

    [Test]
    [MaxTime(100)]
    procedure TestDoesNotTimeout;
  end;
{$M-}

{$M+}
  [TestFixture]
  TTearDownFixtureFailureTests = class
  public
    [Test]
    procedure TearDownFixture_Failure_Is_Reported_Once;
  end;

  [TestFixture]
  TFixtureResultDurationTests = class
  public
    [Test]
    procedure Duration_Of_A_Fixture_With_Children_Includes_Its_Own_Tests;
  end;

  [TestFixture]
  TRuntimeIgnoreTests = class
  public
    [Test]
    procedure Ignored_At_Run_Time_Is_Reported_Ignored_With_Its_Reason;
    [Test]
    procedure Assert_Ignore_Is_Reported_Ignored_With_Its_Reason;
    [Test]
    procedure Ignored_At_Run_Time_Keeps_What_The_Test_Logged;
    [Test]
    procedure Ignored_In_Setup_Is_Reported_Ignored_And_The_Test_Does_Not_Run;
    [Test]
    procedure Ignored_In_SetupFixture_Is_Reported_Ignored_For_Every_Test;
    [Test]
    procedure TearDown_Error_After_A_Runtime_Ignore_Is_Reported;
    [Test]
    procedure Ignored_In_A_WillRaise_Test_Is_Reported_Ignored;
    [Test]
    procedure Ignored_In_A_WillRaise_Descendant_Test_Is_Reported_Ignored;
    [Test]
    procedure Ignored_At_Run_Time_Is_Written_As_Ignored_By_The_XML_Loggers;
  end;
{$M-}

{$IFDEF MSWINDOWS}
{$M+}
  [TestFixture]
  TMaxTimeTests = class
  public
    [Test]
    procedure Overrun_In_A_Busy_Loop_Is_Interrupted_And_Fails;
    [Test]
    procedure Overrun_In_A_Sleep_Loop_Is_Interrupted_And_Fails;
    [Test]
    procedure Blocked_In_A_Synchronous_Read_Is_Interrupted_And_Fails;
    [Test]
    procedure Blocked_In_An_Alertable_Wait_Is_Interrupted_And_Fails;
    [Test]
    procedure The_Next_Test_Runs_And_Its_Alertable_Wait_Is_Not_Cut_Short;
    [Test]
    procedure A_Caught_Interruption_Replaced_By_A_Failure_Is_Reported_As_The_Timeout;
    [Test]
    procedure A_Caught_Interruption_Replaced_By_A_Pass_Is_Reported_As_The_Timeout;
    [Test]
    procedure A_Caught_Interruption_The_Test_Returns_From_Is_Reported_As_The_Timeout;
    [Test]
    procedure An_Interrupted_WillRaise_Test_Is_Reported_As_The_Timeout;
    [Test]
    procedure An_Interrupted_WillRaise_Descendant_Test_Is_Reported_As_The_Timeout;
    [Test]
    procedure An_Interrupted_Test_That_Expects_ETimedOut_Passes;
    [Test]
    procedure An_Interruption_Caught_By_Assert_WillRaise_Is_Reported_As_The_Timeout;
    [Test]
    procedure A_Caught_Interruption_The_Test_Ignores_Is_Ignored;
    [Test]
    procedure Within_Its_MaxTime_Passes;
    [Test]
    procedure A_Caught_Interruption_Is_Followed_By_Another;
    [Test]
    procedure A_TestCase_Overrun_Is_Interrupted_And_Fails;
    [Test]
    procedure An_Interrupted_WillRaise_TestCase_Is_Reported_As_The_Timeout;
    [Test]
    procedure An_Interrupted_TestCase_That_Expects_ETimedOut_Passes;
  end;
{$M-}
{$ENDIF}

  TRaisingDestructorClass = class
  public
    destructor Destroy; override;
    procedure TearDown;
  end;

  TTestCaseAttribute = class
  public
    procedure ProcedureWithUntypedParameter(const Param1);
  end;

{$M+}
  [TestFixture]
  TDUnitXTestFixtureTests = class
  public
    [Test]
    procedure WhenATestCaseHasAUntypedParameterCantRaiseAnyError;
    [Test]
    procedure TestCase_With_Too_Few_Values_Errors;
    [Test]
    procedure TestError_Separates_The_Result_And_Exception_Messages;
    [Test]
    procedure RaisingDestructorTearDown_Runs_The_Destructor_Once;
    [Test]
    procedure FixtureResult_Namespace_Is_The_Fixture_Namespace;
    [Test]
    procedure WillRaiseTest_Releases_The_Context_After_Execute;
    [Test]
    procedure TestError_Of_A_StrCompare_Descendant_Is_Comparable;
    [Test]
    procedure NameFilter_Fixture_Name_Does_Not_Select_A_Longer_Fixture_Name;
    [Test]
    procedure NameFilter_Test_Name_Does_Not_Select_A_Longer_Test_Name;
    [Test]
    procedure NameFilter_Namespace_Selects_Whole_Segments_Only;
    [Test]
    procedure NameFilter_Generic_Fixture_Name_Selects_Its_Instances;
  end;
{$M-}

implementation

uses
{$IFDEF MSWINDOWS}
  WinAPI.Windows,
{$ENDIF}
  System.Math,
  System.SysUtils,
  System.Rtti,
  System.Classes,
  DUnitX.Types,
  DUnitX.Exceptions,
  DUnitX.Extensibility,
  DUnitX.Filters,
  DUnitX.FixtureProvider,
  DUnitX.FixtureResult,
  DUnitX.InternalInterfaces,
  DUnitX.Loggers.XML.JUnit,
  DUnitX.Loggers.XML.NUnit,
  DUnitX.ResStrs,
  DUnitX.RunResults,
  DUnitX.TestResult,
  DUnitX.TestRunner;

var
  _TimesRun : Integer;
  _TimesRunAnyWay : Integer;
  _TimesRunTestCase : Integer;
  _RaisingDestructorCalls : Integer;
  _RaiseContextFreed : Boolean;

type
  TRaiseContextSpy = class(TInterfacedObject, ITestExecuteContext)
  public
    destructor Destroy; override;
    procedure RecordFixture(const fixtureResult : IFixtureResult);
    procedure RecordResult(const fixtureResult : IFixtureResult; const testResult : ITestResult);
    procedure RollupResults;
  end;

  TWillRaiseClass = class
  public
    procedure RaisesAbort;
  end;

  TTearDownFixtureRunnerAccess = class(TDUnitXTestRunner);

  TTearDownFixtureFails = class
  public
    procedure PassesOne;
    procedure PassesTwo;
    procedure TearDownFixture;
  end;

  TTooFewValuesClass = class
  public type
    TTooFewValuesEnum = (tfOne, tfTwo);
  public
    procedure TwoValues(const a : integer; const b : string);
    procedure OneString(const value : string);
    procedure OneEnum(const value : TTooFewValuesEnum);
  end;

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

function MockTestSourceAttribute.GetCaseInfoArray : TestCaseInfoArray;
var
  I : Integer;
begin
  SetLength(result, 3);
  for I := 0 to 2 do
  begin
    result[I].Name := 'DataTest' + IntToStr(I);
    SetLength(result[I].Values, 1);
    result[I].Values[0] := I;
  end;
end;

{ TTestClassWithTestSource }

procedure TTestClassWithTestSource.DataTest(Value : Integer);
begin
  TDUnitX.CurrentRunner.Status(Format('DataTest(%d) Called', [Value]));
  Assert.IsTrue(InRange(Value, 0, 2));
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
  Assert.IsTrue(false, 'I should not have been called!');
end;

procedure TTestRepeatAttribute.IgnoreMeWhenRepeatIsZero;
begin
  Assert.IsTrue(false, 'I should not have been called!');
end;

procedure TTestRepeatAttribute.SetUpFixture;
begin
  _TimesRun := 0;
  _TimesRunAnyWay := 0;
  _TimesRunTestCase := 0;
end;

procedure TTestRepeatAttribute.Sum(const A, B, Expected : Integer);
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
  Sleep(100);
  Assert.Pass;                          // even after a delay,
  // the test should not timeout, as zero means no timeout
end;

procedure TTestMaxTimeAttribute.TestTimeout;
begin
  Sleep(150);
  Assert.Fail('Timeout should catch this test.'); // the test should time out before this point
end;

procedure TTestMaxTimeAttribute.TestDoesNotTimeout;
begin
  Assert.Pass;                          // the test should NOT time out before this point
end;

{ TDUnitXTestFixtureTests }

procedure TDUnitXTestFixtureTests.WhenATestCaseHasAUntypedParameterCantRaiseAnyError;
var
  Context : TRttiContext;
  MyClass : TRttiType;
  MyMethod : TRttiMethod;
  TestFixture : ITestFixture;
  args : TValueArray;
begin
  Context := TRttiContext.Create;
  MyClass := Context.GetType(TTestCaseAttribute);
  MyMethod := MyClass.GetMethod('ProcedureWithUntypedParameter');
  TestFixture := TDUnitXTestFixture.Create(MyClass.Name, EmptyStr, TTestCaseAttribute, MyClass.UnitName) as ITestFixture;

  Assert.WillNotRaise(
    procedure
    begin
      SetLength(args, 1);
      args[0] := 0;
      TestFixture.AddTestCase(MyMethod.Name, 'My test case', 'My name', 'My category', MyMethod, True, args);
    end);

  TestFixture := nil;

  Context.Free;
end;

procedure TDUnitXTestFixtureTests.TestCase_With_Too_Few_Values_Errors;
var
  rttiContext : TRttiContext;
  rttiType : TRttiType;
  fixture : ITestFixture;
  args : TValueArray;
  test : ITestExecute;
begin
  rttiContext := TRttiContext.Create;
  rttiType := rttiContext.GetType(TTooFewValuesClass);
  fixture := TDUnitXTestFixture.Create('ATooFewValuesFixture', '', TTooFewValuesClass, 'AUnit') as ITestFixture;

  SetLength(args, 2);
  args[0] := '2';
  args[1] := 'two';
  test := fixture.AddTestCase('TwoValues', 'Two', 'TwoValues.Two', '', rttiType.GetMethod('TwoValues'), True, args) as ITestExecute;
  Assert.WillNotRaiseAny(
    procedure
    begin
      test.Execute(nil);
    end, 'two values for two parameters');

  SetLength(args, 1);
  test := fixture.AddTestCase('TwoValues', 'One', 'TwoValues.One', '', rttiType.GetMethod('TwoValues'), True, args) as ITestExecute;
  Assert.WillRaiseWithMessage(
    procedure
    begin
      test.Execute(nil);
    end, ETestFrameworkException, 'Too few values: 1 value for 2 parameters', 'one value for two parameters');

  SetLength(args, 0);
  test := fixture.AddTestCase('TwoValues', 'None', 'TwoValues.None', '', rttiType.GetMethod('TwoValues'), True, args) as ITestExecute;
  Assert.WillRaiseWithMessage(
    procedure
    begin
      test.Execute(nil);
    end, ETestFrameworkException, 'Too few values: 0 values for 2 parameters', 'no values for two parameters');

  //[NamedTestCase('Name', '')] gives no values at all
  test := fixture.AddTestCase('OneString', 'Empty', 'OneString.Empty', '', rttiType.GetMethod('OneString'), True, args) as ITestExecute;
  Assert.WillNotRaiseAny(
    procedure
    begin
      test.Execute(nil);
    end, 'no values for one string parameter');

  //...which is one empty value, and an empty value is not an enum name
  test := fixture.AddTestCase('OneEnum', 'Empty', 'OneEnum.Empty', '', rttiType.GetMethod('OneEnum'), True, args) as ITestExecute;
  Assert.WillRaise(
    procedure
    begin
      test.Execute(nil);
    end, ETestFrameworkException, 'no values for one enum parameter');
end;

procedure TDUnitXTestFixtureTests.FixtureResult_Namespace_Is_The_Fixture_Namespace;
var
  fixture : ITestFixture;
  fixtureResult : IFixtureResult;
begin
  fixture := TDUnitXTestFixture.Create('My.Namespace.AFixture', '', TTestCaseAttribute, 'AUnit') as ITestFixture;
  fixtureResult := TDUnitXFixtureResult.Create(nil, fixture as ITestFixtureInfo);
  Assert.AreEqual('My.Namespace', fixtureResult.Namespace);
end;

procedure TDUnitXTestFixtureTests.TestError_Separates_The_Result_And_Exception_Messages;
var
  rttiContext : TRttiContext;
  fixture : ITestFixture;
  instance : TTearDownFixtureFails;
  testInfo : ITestInfo;
  thrown : Exception;
  error : ITestError;
begin
  rttiContext := TRttiContext.Create;
  fixture := TDUnitXTestFixture.Create('AMessageFixture', '', TTearDownFixtureFails, 'AUnit') as ITestFixture;
  instance := fixture.FixtureInstance as TTearDownFixtureFails;
  testInfo := fixture.AddTest('PassesOne', instance.PassesOne, 'PassesOne', '',
    rttiContext.GetType(TTearDownFixtureFails).GetMethod('PassesOne')) as ITestInfo;

  thrown := Exception.Create('exception message');
  try
    error := TDUnitXTestError.Create(testInfo, TTestResultType.Error, thrown, nil, 'result message', nil);
    Assert.AreEqual('result message' + sLineBreak + 'exception message', error.ExceptionMessage, 'two different messages');
    error := TDUnitXTestError.Create(testInfo, TTestResultType.Error, thrown, nil, '', nil);
    Assert.AreEqual('exception message', error.ExceptionMessage, 'no result message');
    error := TDUnitXTestError.Create(testInfo, TTestResultType.Error, thrown, nil, thrown.Message, nil);
    Assert.AreEqual('exception message', error.ExceptionMessage, 'the same message twice');
  finally
    thrown.Free;
  end;
end;

procedure TDUnitXTestFixtureTests.RaisingDestructorTearDown_Runs_The_Destructor_Once;
var
  fixture : ITestFixture;
  instance : TRaisingDestructorClass;
  calls : Integer;
begin
  _RaisingDestructorCalls := 0;
  fixture := TDUnitXTestFixture.Create('ARaisingFixture', '', TRaisingDestructorClass, 'AUnit') as ITestFixture;
  instance := fixture.FixtureInstance as TRaisingDestructorClass;
  fixture.SetTearDownFixtureMethod('Destroy', instance.TearDown, true);

  Assert.WillRaise(
    procedure
    begin
      fixture.ExecuteFixtureTearDown;
    end, EInvalidOpException);
  fixture := nil;

  calls := _RaisingDestructorCalls;
  //A destructor that raises does not free the instance, so release its memory here.
  if calls = 1 then
    instance.FreeInstance;
  Assert.AreEqual(1, calls);
end;

procedure TDUnitXTestFixtureTests.WillRaiseTest_Releases_The_Context_After_Execute;
var
  rttiContext : TRttiContext;
  fixture : ITestFixture;
  instance : TWillRaiseClass;
  test : ITestExecute;
  context : ITestExecuteContext;
begin
  _RaiseContextFreed := False;
  rttiContext := TRttiContext.Create;
  fixture := TDUnitXTestFixture.Create('AWillRaiseFixture', '', TWillRaiseClass, 'AUnit') as ITestFixture;
  instance := fixture.FixtureInstance as TWillRaiseClass;
  test := fixture.AddTest('RaisesAbort', instance.RaisesAbort, 'RaisesAbort', '',
    rttiContext.GetType(TWillRaiseClass).GetMethod('RaisesAbort'), True, False, '', 0, EAbort) as ITestExecute;
  context := TRaiseContextSpy.Create;
  test.Execute(context);
  context := nil;
  Assert.IsTrue(_RaiseContextFreed, 'The [WillRaise] test still holds the context after Execute');
end;

type
  //A custom string comparison failure, as a user's own assertion might raise.
  ETestFailureStrCompareDescendant = class(ETestFailureStrCompare);

procedure TDUnitXTestFixtureTests.TestError_Of_A_StrCompare_Descendant_Is_Comparable;
var
  rttiContext : TRttiContext;
  fixture : ITestFixture;
  instance : TTearDownFixtureFails;
  testInfo : ITestInfo;
  thrown : Exception;
  error : ITestError;
begin
  rttiContext := TRttiContext.Create;
  fixture := TDUnitXTestFixture.Create('ACompareFixture', '', TTearDownFixtureFails, 'AUnit') as ITestFixture;
  instance := fixture.FixtureInstance as TTearDownFixtureFails;
  testInfo := fixture.AddTest('PassesOne', instance.PassesOne, 'PassesOne', '',
    rttiContext.GetType(TTearDownFixtureFails).GetMethod('PassesOne')) as ITestInfo;

  thrown := ETestFailureStrCompareDescendant.Create('expected text', 'actual text', 'compare message', nil);
  try
    error := TDUnitXTestError.Create(testInfo, TTestResultType.Failure, thrown, nil, thrown.Message, nil);
  finally
    thrown.Free;
  end;
  Assert.IsTrue(error.IsComparable, 'a descendant of ETestFailureStrCompare is not comparable');
  Assert.AreEqual('expected text', error.Expected, false, 'expected value not carried');
  Assert.AreEqual('actual text', error.Actual, false, 'actual value not carried');
end;

function NameFilterTestMethod(const AContext : TRttiContext) : TRttiMethod;
begin
  result := AContext.GetType(TTestCaseAttribute).GetMethod('ProcedureWithUntypedParameter');
end;

procedure TDUnitXTestFixtureTests.NameFilter_Fixture_Name_Does_Not_Select_A_Longer_Fixture_Name;
var
  context : TRttiContext;
  method : TRttiMethod;
  foo, fooBar : ITestFixture;
  fooTest, fooBarTest : ITest;
  filter : ITestFilter;
begin
  context := TRttiContext.Create;
  method := NameFilterTestMethod(context);
  foo := TDUnitXTestFixture.Create('Unit.TFoo', '', TTestCaseAttribute, 'AUnit') as ITestFixture;
  fooBar := TDUnitXTestFixture.Create('Unit.TFooBar', '', TTestCaseAttribute, 'AUnit') as ITestFixture;
  fooTest := foo.AddTest(method.Name, nil, 'Test1', '', method);
  fooBarTest := fooBar.AddTest(method.Name, nil, 'Test1', '', method);

  filter := TNameFilter.Create('Unit.TFoo');
  Assert.IsTrue(filter.Match(fooTest), 'a test of the named fixture');
  Assert.IsFalse(filter.Match(fooBarTest), 'a test of a fixture whose name only starts with the named one');

  filter := TNameFilter.Create('unit.tfoo');
  Assert.IsTrue(filter.Match(fooTest), 'names match ignoring case');
end;

procedure TDUnitXTestFixtureTests.NameFilter_Test_Name_Does_Not_Select_A_Longer_Test_Name;
var
  context : TRttiContext;
  method : TRttiMethod;
  args : TValueArray;
  foo, repeated : ITestFixture;
  test1, test10, namedCase, valuesCase, repeatedTest : ITest;
  filter : ITestFilter;
begin
  context := TRttiContext.Create;
  method := NameFilterTestMethod(context);
  SetLength(args, 1);
  args[0] := 0;
  foo := TDUnitXTestFixture.Create('Unit.TFoo', '', TTestCaseAttribute, 'AUnit') as ITestFixture;
  test1 := foo.AddTest(method.Name, nil, 'Test1', '', method);
  test10 := foo.AddTest(method.Name, nil, 'Test10', '', method);
  namedCase := foo.AddTestCase(method.Name, 'CaseA', 'Test1', '', method, True, args);
  valuesCase := foo.AddTestCase(method.Name, '(1,2)', 'Test1', '', method, True, args);
  //[RepeatTest] puts the repetitions of Test10 in a child fixture named after the method.
  repeated := foo.AddChildFixture(TTestCaseAttribute, 'Unit.TFoo.Test10', '');
  repeatedTest := repeated.AddTest(method.Name, nil, 'Test10-1-of-2', '', method);

  filter := TNameFilter.Create('Unit.TFoo.Test1');
  Assert.IsTrue(filter.Match(test1), 'the named test');
  Assert.IsFalse(filter.Match(test10), 'a test whose name only starts with the named one');
  Assert.IsFalse(filter.Match(repeatedTest), 'a repetition of a test whose name only starts with the named one');
  Assert.IsTrue(filter.Match(namedCase), 'a named test case of the named test');
  Assert.IsTrue(filter.Match(valuesCase), 'a test case of the named test, named from its values');

  filter := TNameFilter.Create('Unit.TFoo.Test10');
  Assert.IsTrue(filter.Match(repeatedTest), 'a repetition of the named test');

  filter := TNameFilter.Create('Unit.TFoo.Test1.CaseA');
  Assert.IsTrue(filter.Match(namedCase), 'the named test case');
  Assert.IsFalse(filter.Match(valuesCase), 'another test case of the same test');
end;

procedure TDUnitXTestFixtureTests.NameFilter_Namespace_Selects_Whole_Segments_Only;
var
  context : TRttiContext;
  method : TRttiMethod;
  fixture : ITestFixture;
  test : ITest;
  filter : ITestFilter;
begin
  context := TRttiContext.Create;
  method := NameFilterTestMethod(context);
  fixture := TDUnitXTestFixture.Create('My.Namespace.TFoo', '', TTestCaseAttribute, 'AUnit') as ITestFixture;
  test := fixture.AddTest(method.Name, nil, 'Test1', '', method);

  filter := TNameFilter.Create('My');
  Assert.IsTrue(filter.Match(test), 'the outer namespace');
  filter := TNameFilter.Create('My.Namespace');
  Assert.IsTrue(filter.Match(test), 'the whole namespace');
  filter := TNameFilter.Create('My.Name');
  Assert.IsFalse(filter.Match(test), 'part of a namespace segment');
  filter := TNameFilter.Create('Namespace.TFoo');
  Assert.IsFalse(filter.Match(test), 'a name that does not start the full name');
end;

procedure TDUnitXTestFixtureTests.NameFilter_Generic_Fixture_Name_Selects_Its_Instances;
var
  context : TRttiContext;
  method : TRttiMethod;
  foo, fooBar : ITestFixture;
  fooTest, fooBarTest : ITest;
  filter : ITestFilter;
begin
  context := TRttiContext.Create;
  method := NameFilterTestMethod(context);
  //A generic fixture is named from its class name, type arguments included.
  foo := TDUnitXTestFixture.Create('Unit.TFoo<System.Integer>', '', TTestCaseAttribute, 'AUnit') as ITestFixture;
  fooBar := TDUnitXTestFixture.Create('Unit.TFooBar<System.Integer>', '', TTestCaseAttribute, 'AUnit') as ITestFixture;
  fooTest := foo.AddTest(method.Name, nil, 'Test1', '', method);
  fooBarTest := fooBar.AddTest(method.Name, nil, 'Test1', '', method);

  filter := TNameFilter.Create('Unit.TFoo');
  Assert.IsTrue(filter.Match(fooTest), 'an instance of the named generic fixture');
  Assert.IsFalse(filter.Match(fooBarTest), 'an instance of a generic fixture whose name only starts with the named one');
  filter := TNameFilter.Create('Unit.TFoo<System.Integer>');
  Assert.IsTrue(filter.Match(fooTest), 'the named instance');
end;

{ TRaiseContextSpy }

destructor TRaiseContextSpy.Destroy;
begin
  _RaiseContextFreed := True;
  inherited;
end;

procedure TRaiseContextSpy.RecordFixture(const fixtureResult : IFixtureResult);
begin
end;

procedure TRaiseContextSpy.RecordResult(const fixtureResult : IFixtureResult; const testResult : ITestResult);
begin
end;

procedure TRaiseContextSpy.RollupResults;
begin
end;

{ TWillRaiseClass }

procedure TWillRaiseClass.RaisesAbort;
begin
  raise EAbort.Create('expected');
end;

{ TRaisingDestructorClass }

destructor TRaisingDestructorClass.Destroy;
begin
  Inc(_RaisingDestructorCalls);
  if _RaisingDestructorCalls = 1 then
    raise EInvalidOpException.Create('Destructor failed');
  inherited;
end;

procedure TRaisingDestructorClass.TearDown;
begin
end;

{ TTestCaseAttribute }

procedure TTestCaseAttribute.ProcedureWithUntypedParameter(const Param1);
begin
  Assert.IsTrue(True);
end;

{ TTooFewValuesClass }

procedure TTooFewValuesClass.TwoValues(const a : integer; const b : string);
begin
  Assert.AreEqual(2, a, 'a');
  Assert.AreEqual('two', b, 'b');
end;

procedure TTooFewValuesClass.OneString(const value : string);
begin
  Assert.AreEqual('', value, 'value');
end;

procedure TTooFewValuesClass.OneEnum(const value : TTooFewValuesEnum);
begin
  Assert.Fail('A case with no value for its enum parameter must not run');
end;

{ TTearDownFixtureFails }

procedure TTearDownFixtureFails.PassesOne;
begin
  Assert.Pass;
end;

procedure TTearDownFixtureFails.PassesTwo;
begin
  Assert.Pass;
end;

procedure TTearDownFixtureFails.TearDownFixture;
begin
  raise EInvalidOpException.Create('TearDownFixture failed');
end;

{ TTearDownFixtureFailureTests }

//Runs one fixture through a runner of its own. A runner registers itself for its thread, so it gets a thread of its own.
//ATestNames lists the name of each result's test, read while the runner is alive, as ',name,name,'.
function RunFixtureOnItsOwnThread(const AFixture : ITestFixture; out ATestNames : string) : IRunResults;
var
  thread : TThread;
  results : IRunResults;
  failure : string;
  names : string;
begin
  results := nil;
  failure := '';
  names := ',';
  thread := TThread.CreateAnonymousThread(
    procedure
    var
      runner : TDUnitXTestRunner;
      runnerIntf : ITestRunner;
      fixtures : ITestFixtureList;
      context : ITestExecuteContext;
      testResult : ITestResult;
    begin
      try
        runner := TDUnitXTestRunner.Create;
        runnerIntf := runner;
        fixtures := TTestFixtureList.Create;
        fixtures.Add(AFixture);
        results := TDUnitXRunResults.Create;
        context := results as ITestExecuteContext;
        TTearDownFixtureRunnerAccess(runner).ExecuteFixtures(nil, context, TThread.CurrentThread.ThreadID, fixtures);
        context.RollupResults;
        for testResult in results.GetAllTestResults do
          if testResult.Test <> nil then
            names := names + testResult.Test.Name + ','
          else
            names := names + '(no test),';
      except
        on e : Exception do
          failure := e.ClassName + ': ' + e.Message;
      end;
    end);
  thread.FreeOnTerminate := false;
  try
    thread.Start;
    thread.WaitFor;
  finally
    thread.Free;
  end;
  Assert.AreEqual('', failure, 'the run itself raised');
  ATestNames := names;
  result := results;
end;

procedure TTearDownFixtureFailureTests.TearDownFixture_Failure_Is_Reported_Once;
var
  rttiContext : TRttiContext;
  rttiType : TRttiType;
  fixture : ITestFixture;
  instance : TTearDownFixtureFails;
  results : IRunResults;
  testResult : ITestResult;
  tearDownErrors : integer;
  testNames : string;
begin
  rttiContext := TRttiContext.Create;
  rttiType := rttiContext.GetType(TTearDownFixtureFails);
  fixture := TDUnitXTestFixture.Create('ATearDownFixtureFails', '', TTearDownFixtureFails, 'AUnit') as ITestFixture;
  instance := fixture.FixtureInstance as TTearDownFixtureFails;
  fixture.AddTest('PassesOne', instance.PassesOne, 'PassesOne', '', rttiType.GetMethod('PassesOne'));
  fixture.AddTest('PassesTwo', instance.PassesTwo, 'PassesTwo', '', rttiType.GetMethod('PassesTwo'));
  fixture.SetTearDownFixtureMethod('TearDownFixture', instance.TearDownFixture, false);

  results := RunFixtureOnItsOwnThread(fixture, testNames);

  tearDownErrors := 0;
  for testResult in results.GetAllTestResults do
    if (testResult.ResultType = TTestResultType.Error) and (Pos('TearDownFixture failed', testResult.Message) > 0) then
      Inc(tearDownErrors);
  Assert.AreEqual(2, results.PassCount, 'both tests passed');
  Assert.AreEqual(3, results.TestCount, 'two tests and one TearDownFixture error, no test reported twice');
  Assert.AreEqual(1, tearDownErrors, 'the TearDownFixture failure is reported once');
  Assert.AreEqual(',PassesOne,PassesTwo,TearDownFixture,', testNames, 'the results and their tests');
end;

type
  //Not a fixture: the class of the fixtures whose results the test below times.
  TDurationFixtureClass = class
  end;

{ TFixtureResultDurationTests }

procedure TFixtureResultDurationTests.Duration_Of_A_Fixture_With_Children_Includes_Its_Own_Tests;
var
  parentFixture : ITestFixture;
  childFixture : ITestFixture;
  parentResult : IFixtureResult;
  childResult : IFixtureResult;
begin
  //A fixture's [RepeatTest] methods are child fixtures; its other tests are its own.
  parentFixture := TDUnitXTestFixture.Create('AParentFixture', '', TDurationFixtureClass, 'AUnit') as ITestFixture;
  childFixture := TDUnitXTestFixture.Create('AChildFixture', '', TDurationFixtureClass, 'AUnit') as ITestFixture;
  parentResult := TDUnitXFixtureResult.Create(nil, parentFixture as ITestFixtureInfo);
  //The fixture's own tests take this long.
  Sleep(100);
  childResult := TDUnitXFixtureResult.Create(parentResult, childFixture as ITestFixtureInfo);
  (childResult as IFixtureResultBuilder).RollUpResults;
  (parentResult as IFixtureResultBuilder).RollUpResults;
  Assert.IsTrue(parentResult.Duration.TotalMilliseconds >= 90,
    Format('the fixture''s duration is %.0f ms, its own tests took 100 ms', [parentResult.Duration.TotalMilliseconds]));
  Assert.IsTrue(parentResult.FinishTime >= childResult.FinishTime, 'the fixture finishes after its children');
end;

const
  RUNTIME_IGNORE_REASON = 'the resource this test needs is not available';
  RUNTIME_IGNORE_LOGGED = 'logged before the test ignored itself';

var
  _RuntimeIgnoreSetupRan : Boolean;
  _RuntimeIgnoreTestRan : Boolean;
  _RuntimeIgnoreCodeAfterIgnoreRan : Boolean;
  _RuntimeIgnoreTearDownRan : Boolean;
  _RuntimeIgnoreTearDownFixtureRan : Boolean;

type
  //Not a fixture: the class of the fixtures the tests below run through a runner of their own.
  TRuntimeIgnoreClass = class
  public
    procedure SetupFixtureIgnores;
    procedure TearDownFixture;
    procedure Setup;
    procedure SetupIgnores;
    procedure TearDown;
    procedure TearDownRaises;
    procedure IgnoresItself;
    procedure IgnoresItselfInsideWillRaise;
    procedure IgnoresAfterLogging;
    procedure IgnoresWithAssertIgnore;
    procedure Passes;
  end;

  ERuntimeIgnoreExpected = class(Exception);

  TRuntimeIgnoreTest = (ritIgnoresItself, ritIgnoresItselfInsideWillRaise, ritIgnoresItselfInsideWillRaiseDescendant,
    ritIgnoresAfterLogging, ritIgnoresWithAssertIgnore, ritPasses);

  TRuntimeIgnoreOption = (rioSetupIgnores, rioTearDownRaises, rioSetupFixtureIgnores);
  TRuntimeIgnoreOptions = set of TRuntimeIgnoreOption;

//Skips the test the way a test does when it finds at run time that it cannot run.
procedure IgnoreThisTest(const reason : string);
begin
  raise ETestIgnored.Create(reason);
end;

{ TRuntimeIgnoreClass }

procedure TRuntimeIgnoreClass.SetupFixtureIgnores;
begin
  IgnoreThisTest(RUNTIME_IGNORE_REASON);
end;

procedure TRuntimeIgnoreClass.TearDownFixture;
begin
  _RuntimeIgnoreTearDownFixtureRan := True;
end;

procedure TRuntimeIgnoreClass.Setup;
begin
  _RuntimeIgnoreSetupRan := True;
end;

procedure TRuntimeIgnoreClass.SetupIgnores;
begin
  _RuntimeIgnoreSetupRan := True;
  IgnoreThisTest(RUNTIME_IGNORE_REASON);
end;

procedure TRuntimeIgnoreClass.TearDown;
begin
  _RuntimeIgnoreTearDownRan := True;
end;

procedure TRuntimeIgnoreClass.TearDownRaises;
begin
  _RuntimeIgnoreTearDownRan := True;
  raise EInvalidOpException.Create('TearDown failed');
end;

procedure TRuntimeIgnoreClass.IgnoresItself;
begin
  _RuntimeIgnoreTestRan := True;
  IgnoreThisTest(RUNTIME_IGNORE_REASON);
  _RuntimeIgnoreCodeAfterIgnoreRan := True;
end;

procedure TRuntimeIgnoreClass.IgnoresItselfInsideWillRaise;
begin
  _RuntimeIgnoreTestRan := True;
  IgnoreThisTest(RUNTIME_IGNORE_REASON);
  raise ERuntimeIgnoreExpected.Create('the exception the test is expected to raise');
end;

procedure TRuntimeIgnoreClass.IgnoresAfterLogging;
begin
  _RuntimeIgnoreTestRan := True;
  TDUnitX.CurrentRunner.Log(TLogLevel.Warning, RUNTIME_IGNORE_LOGGED);
  IgnoreThisTest(RUNTIME_IGNORE_REASON);
end;

procedure TRuntimeIgnoreClass.IgnoresWithAssertIgnore;
begin
  _RuntimeIgnoreTestRan := True;
  Assert.Ignore(RUNTIME_IGNORE_REASON);
  _RuntimeIgnoreCodeAfterIgnoreRan := True;
end;

procedure TRuntimeIgnoreClass.Passes;
begin
  _RuntimeIgnoreTestRan := True;
end;

procedure AddRuntimeIgnoreTest(const AFixture : ITestFixture; const ARttiType : TRttiType; const ATest : TRuntimeIgnoreTest);
var
  instance : TRuntimeIgnoreClass;
begin
  instance := AFixture.FixtureInstance as TRuntimeIgnoreClass;
  case ATest of
    ritIgnoresItself :
      AFixture.AddTest('IgnoresItself', instance.IgnoresItself, 'IgnoresItself', '', ARttiType.GetMethod('IgnoresItself'));
    ritIgnoresItselfInsideWillRaise :
      AFixture.AddTest('IgnoresItselfInsideWillRaise', instance.IgnoresItselfInsideWillRaise, 'IgnoresItselfInsideWillRaise', '',
        ARttiType.GetMethod('IgnoresItselfInsideWillRaise'), True, False, '', 0, ERuntimeIgnoreExpected);
    ritIgnoresItselfInsideWillRaiseDescendant :
      AFixture.AddTest('IgnoresItselfInsideWillRaise', instance.IgnoresItselfInsideWillRaise, 'IgnoresItselfInsideWillRaise', '',
        ARttiType.GetMethod('IgnoresItselfInsideWillRaise'), True, False, '', 0, ERuntimeIgnoreExpected, exDescendant);
    ritIgnoresAfterLogging :
      AFixture.AddTest('IgnoresAfterLogging', instance.IgnoresAfterLogging, 'IgnoresAfterLogging', '', ARttiType.GetMethod('IgnoresAfterLogging'));
    ritIgnoresWithAssertIgnore :
      AFixture.AddTest('IgnoresWithAssertIgnore', instance.IgnoresWithAssertIgnore, 'IgnoresWithAssertIgnore', '',
        ARttiType.GetMethod('IgnoresWithAssertIgnore'));
    ritPasses :
      AFixture.AddTest('Passes', instance.Passes, 'Passes', '', ARttiType.GetMethod('Passes'));
  end;
end;

//A fixture of the given tests, with a setup and a tear down (each of which can ignore or raise, as AOptions says).
function CreateRuntimeIgnoreFixture(const ATests : array of TRuntimeIgnoreTest; const AOptions : TRuntimeIgnoreOptions) : ITestFixture;
var
  rttiContext : TRttiContext;
  rttiType : TRttiType;
  instance : TRuntimeIgnoreClass;
  i : integer;
begin
  _RuntimeIgnoreSetupRan := False;
  _RuntimeIgnoreTestRan := False;
  _RuntimeIgnoreCodeAfterIgnoreRan := False;
  _RuntimeIgnoreTearDownRan := False;
  _RuntimeIgnoreTearDownFixtureRan := False;
  rttiContext := TRttiContext.Create;
  rttiType := rttiContext.GetType(TRuntimeIgnoreClass);
  result := TDUnitXTestFixture.Create('ARuntimeIgnoreFixture', '', TRuntimeIgnoreClass, 'AUnit') as ITestFixture;
  instance := result.FixtureInstance as TRuntimeIgnoreClass;
  if rioSetupFixtureIgnores in AOptions then
  begin
    result.SetSetupFixtureMethod('SetupFixtureIgnores', instance.SetupFixtureIgnores);
    result.SetTearDownFixtureMethod('TearDownFixture', instance.TearDownFixture, false);
  end;
  if rioSetupIgnores in AOptions then
    result.SetSetupTestMethod('SetupIgnores', instance.SetupIgnores)
  else
    result.SetSetupTestMethod('Setup', instance.Setup);
  if rioTearDownRaises in AOptions then
    result.SetTearDownTestMethod('TearDownRaises', instance.TearDownRaises)
  else
    result.SetTearDownTestMethod('TearDown', instance.TearDown);
  for i := Low(ATests) to High(ATests) do
    AddRuntimeIgnoreTest(result, rttiType, ATests[i]);
end;

//Checks that AResults holds ACount results, each an ignored one carrying RUNTIME_IGNORE_REASON, and returns the last.
function CheckReportedIgnored(const AResults : IRunResults; const ACount : integer = 1) : ITestResult;
var
  testResult : ITestResult;
begin
  Assert.AreEqual(ACount, AResults.TestCount, 'the number of tests');
  Assert.AreEqual(ACount, AResults.IgnoredCount, 'the tests are ignored');
  Assert.AreEqual(0, AResults.PassCount, 'a test is passed');
  Assert.AreEqual(0, AResults.FailureCount, 'a test is failed');
  Assert.AreEqual(0, AResults.ErrorCount, 'a test is errored');
  result := nil;
  for testResult in AResults.GetAllTestResults do
  begin
    Assert.AreEqual(Ord(TTestResultType.Ignored), Ord(testResult.ResultType), 'the result type');
    Assert.AreEqual(RUNTIME_IGNORE_REASON, testResult.Message, false, 'the result does not carry the reason the test was ignored');
    result := testResult;
  end;
  Assert.IsNotNull(result, 'no result');
end;

{ TRuntimeIgnoreTests }

procedure TRuntimeIgnoreTests.Ignored_At_Run_Time_Is_Reported_Ignored_With_Its_Reason;
var
  results : IRunResults;
  testNames : string;
begin
  results := RunFixtureOnItsOwnThread(CreateRuntimeIgnoreFixture([ritIgnoresItself], []), testNames);
  CheckReportedIgnored(results);
  Assert.IsTrue(_RuntimeIgnoreSetupRan, 'the setup ran');
  Assert.IsTrue(_RuntimeIgnoreTestRan, 'the test ran up to the ignore');
  Assert.IsFalse(_RuntimeIgnoreCodeAfterIgnoreRan, 'the test ran on after it was ignored');
  Assert.IsTrue(_RuntimeIgnoreTearDownRan, 'the tear down still runs for a test ignored at run time');
end;

procedure TRuntimeIgnoreTests.Assert_Ignore_Is_Reported_Ignored_With_Its_Reason;
var
  results : IRunResults;
  testNames : string;
begin
  results := RunFixtureOnItsOwnThread(CreateRuntimeIgnoreFixture([ritIgnoresWithAssertIgnore], []), testNames);
  CheckReportedIgnored(results);
  Assert.IsTrue(_RuntimeIgnoreTestRan, 'the test ran up to Assert.Ignore');
  Assert.IsFalse(_RuntimeIgnoreCodeAfterIgnoreRan, 'the test ran on after Assert.Ignore');
  Assert.IsTrue(_RuntimeIgnoreTearDownRan, 'the tear down still runs after Assert.Ignore');
end;

procedure TRuntimeIgnoreTests.Ignored_At_Run_Time_Keeps_What_The_Test_Logged;
var
  results : IRunResults;
  testNames : string;
  testResult : ITestResult;
  logMessages : TLogMessageArray;
  found : boolean;
  i : integer;
begin
  results := RunFixtureOnItsOwnThread(CreateRuntimeIgnoreFixture([ritIgnoresAfterLogging], []), testNames);
  testResult := CheckReportedIgnored(results);
  logMessages := testResult.LogMessages;
  found := false;
  for i := 0 to High(logMessages) do
    if logMessages[i].Msg = RUNTIME_IGNORE_LOGGED then
      found := true;
  Assert.IsTrue(found, 'the result lost what the test logged before it ignored itself');
end;

procedure TRuntimeIgnoreTests.Ignored_In_Setup_Is_Reported_Ignored_And_The_Test_Does_Not_Run;
var
  results : IRunResults;
  testNames : string;
begin
  results := RunFixtureOnItsOwnThread(CreateRuntimeIgnoreFixture([ritPasses], [rioSetupIgnores]), testNames);
  CheckReportedIgnored(results);
  Assert.IsTrue(_RuntimeIgnoreSetupRan, 'the setup ran');
  Assert.IsFalse(_RuntimeIgnoreTestRan, 'the test ran although its setup ignored it');
  Assert.IsTrue(_RuntimeIgnoreTearDownRan, 'the tear down still runs after the setup ignored the test');
end;

procedure TRuntimeIgnoreTests.Ignored_In_SetupFixture_Is_Reported_Ignored_For_Every_Test;
var
  results : IRunResults;
  testNames : string;
begin
  results := RunFixtureOnItsOwnThread(CreateRuntimeIgnoreFixture([ritPasses, ritIgnoresItself], [rioSetupFixtureIgnores]), testNames);
  CheckReportedIgnored(results, 2);
  Assert.IsFalse(_RuntimeIgnoreSetupRan, 'a test''s setup ran although the fixture setup ignored the fixture');
  Assert.IsFalse(_RuntimeIgnoreTestRan, 'a test ran although the fixture setup ignored the fixture');
  //As after a fixture setup that errors.
  Assert.IsFalse(_RuntimeIgnoreTearDownFixtureRan, 'the fixture tear down ran although the fixture setup did not finish');
end;

procedure TRuntimeIgnoreTests.TearDown_Error_After_A_Runtime_Ignore_Is_Reported;
var
  results : IRunResults;
  testNames : string;
  testResult : ITestResult;
begin
  //The ignore is not a failure to diagnose, so the tear down error is not hidden behind it.
  results := RunFixtureOnItsOwnThread(CreateRuntimeIgnoreFixture([ritIgnoresItself], [rioTearDownRaises]), testNames);
  Assert.IsTrue(_RuntimeIgnoreTearDownRan, 'the tear down ran');
  Assert.AreEqual(1, results.TestCount, 'one test ran');
  Assert.AreEqual(1, results.ErrorCount, 'the tear down error is reported');
  Assert.AreEqual(0, results.IgnoredCount, 'the tear down error is hidden behind the ignore');
  for testResult in results.GetAllTestResults do
    Assert.Contains(testResult.Message, 'TearDown failed', false, 'the result is not the tear down error');
end;

procedure TRuntimeIgnoreTests.Ignored_In_A_WillRaise_Test_Is_Reported_Ignored;
var
  results : IRunResults;
  testNames : string;
begin
  //The ignore is the test's outcome, not an exception of the wrong class.
  results := RunFixtureOnItsOwnThread(CreateRuntimeIgnoreFixture([ritIgnoresItselfInsideWillRaise], []), testNames);
  CheckReportedIgnored(results);
  Assert.IsTrue(_RuntimeIgnoreTearDownRan, 'the tear down ran');
end;

procedure TRuntimeIgnoreTests.Ignored_In_A_WillRaise_Descendant_Test_Is_Reported_Ignored;
var
  results : IRunResults;
  testNames : string;
begin
  results := RunFixtureOnItsOwnThread(CreateRuntimeIgnoreFixture([ritIgnoresItselfInsideWillRaiseDescendant], []), testNames);
  CheckReportedIgnored(results);
end;

procedure TRuntimeIgnoreTests.Ignored_At_Run_Time_Is_Written_As_Ignored_By_The_XML_Loggers;
var
  results : IRunResults;
  testNames : string;
  nunitStream : TStringStream;
  junitStream : TStringStream;
  logger : ITestLogger;
begin
  results := RunFixtureOnItsOwnThread(CreateRuntimeIgnoreFixture([ritIgnoresItself], []), testNames);
  junitStream := nil;
  nunitStream := TStringStream.Create('', TEncoding.UTF8);
  try
    junitStream := TStringStream.Create('', TEncoding.UTF8);
    logger := TDUnitXXMLNUnitLogger.Create(nunitStream);
    logger.OnTestingEnds(results);
    logger := TDUnitXXMLJUnitLogger.Create(junitStream);
    logger.OnTestingEnds(results);
    logger := nil;
    Assert.Contains(nunitStream.DataString, 'name="IgnoresItself" executed="False" result="Ignored"', false, 'the NUnit XML');
    Assert.Contains(nunitStream.DataString, RUNTIME_IGNORE_REASON, false, 'the NUnit XML does not carry the reason');
    Assert.Contains(junitStream.DataString, '<skipped/>', false, 'the JUnit XML');
  finally
    logger := nil;
    junitStream.Free;
    nunitStream.Free;
  end;
end;

{$IFDEF MSWINDOWS}
const
  //The [MaxTime] of the tests that should be interrupted.
  MAX_TIME_LIMIT = 100;
  //How long those tests would take if they were not interrupted.  A test stopped in under half of it was interrupted;
  //one that took longer ran on until its wait or loop ended, which is not stopping a hung test.
  MAX_TIME_UNINTERRUPTED = 3000;
  //The [MaxTime] of the test that should pass: far more than it takes, however slow the machine.
  MAX_TIME_GENEROUS = 10000;

var
  _MaxTimeTestRanToTheEnd : Boolean;
  _MaxTimeAlertableWaitResult : Cardinal;
  _MaxTimePipeRead : THandle;
  _MaxTimePipeWrite : THandle;
  _MaxTimeStopBusyLoop : Boolean;
  _MaxTimeInterruptionsCaught : Integer;

type
  //Not a fixture: the class of the fixtures TMaxTimeTests runs through a runner of its own.
  TMaxTimeClass = class
  public
    procedure BusyLoop;
    procedure SleepLoop;
    procedure SynchronousRead;
    procedure AlertableWait;
    procedure WaitsAlertably;
    procedure Passes;
    procedure CatchesTheInterruptionThenHangsAgain;
    procedure CatchesTheInterruptionAndFails;
    procedure CatchesTheInterruptionAndPasses;
    procedure CatchesTheInterruptionAndReturns;
    procedure CatchesTheInterruptionWithAssertWillRaise;
    procedure CatchesTheInterruptionAndIgnores;
  end;

  //Not fixtures: each holds one [TestCase] that loops until it is interrupted, generated as a suite's would be.
  TMaxTimeCaseClass = class
  public
    [Test]
    [TestCase('1')]
    [MaxTime(MAX_TIME_LIMIT)]
    procedure BusyLoop(const AValue : Integer);
  end;

  TMaxTimeWillRaiseCaseClass = class
  public
    [Test]
    [TestCase('1')]
    [MaxTime(MAX_TIME_LIMIT)]
    [WillRaise(EConvertError)]
    procedure BusyLoop(const AValue : Integer);
  end;

  TMaxTimeExpectsTimeoutCaseClass = class
  public
    [Test]
    [TestCase('1')]
    [MaxTime(MAX_TIME_LIMIT)]
    [WillRaise(ETimedOut)]
    procedure BusyLoop(const AValue : Integer);
  end;

  TMaxTimeFixtureProviderAccess = class(TDUnitXFixtureProvider);

{ TMaxTimeClass }

//Stores its parameter in its home slot, at the top of the caller's stack.
procedure LeaveJunkAtTheTopOfTheStack(AJunk : NativeInt);
var
  junk : NativeInt;
begin
  junk := AJunk;
  if junk = 0 then
    _MaxTimeStopBusyLoop := True;
end;

//Loops without calling anything, so it is interrupted in its own code, with junk rather than a return address at the
//top of its stack (on Win64 the interruption must supply the return address itself).
procedure TMaxTimeClass.BusyLoop;
begin
  LeaveJunkAtTheTopOfTheStack(NativeInt($DEAD0BAD));
  while not _MaxTimeStopBusyLoop do
    ;
  _MaxTimeTestRanToTheEnd := True;
end;

//Spends its time inside Sleep, so it is interrupted inside a system call, as a test polling a connection would be.
procedure TMaxTimeClass.SleepLoop;
var
  start : Cardinal;
begin
  start := GetTickCount;
  repeat
    Sleep(10);
  until GetTickCount - start >= MAX_TIME_UNINTERRUPTED;
  _MaxTimeTestRanToTheEnd := True;
end;

//Blocked reading a pipe nothing is written to until MAX_TIME_UNINTERRUPTED has passed.
procedure TMaxTimeClass.SynchronousRead;
var
  buffer : Byte;
  bytesRead : Cardinal;
begin
  ReadFile(_MaxTimePipeRead, buffer, 1, bytesRead, nil);
  _MaxTimeTestRanToTheEnd := True;
end;

//Blocked in an alertable wait, as a blocking Winsock call is.
procedure TMaxTimeClass.AlertableWait;
begin
  SleepEx(MAX_TIME_UNINTERRUPTED, True);
  _MaxTimeTestRanToTheEnd := True;
end;

//Runs after an interrupted test, on the same thread: an alertable wait must last its full time.
procedure TMaxTimeClass.WaitsAlertably;
begin
  _MaxTimeAlertableWaitResult := SleepEx(200, True);
end;

procedure TMaxTimeClass.Passes;
begin
  _MaxTimeTestRanToTheEnd := True;
end;

//Loops until the safety net ends it.
procedure LoopUntilStopped;
begin
  while not _MaxTimeStopBusyLoop do
    ;
end;

//Catches the first interruption, as a test retrying a connection might, then hangs again.
procedure TMaxTimeClass.CatchesTheInterruptionThenHangsAgain;
begin
  try
    LoopUntilStopped;
  except
    on ETimedOut do
      Inc(_MaxTimeInterruptionsCaught);
  end;
  LoopUntilStopped;
  _MaxTimeTestRanToTheEnd := True;
end;

//Catches the interruption and fails with a message of its own.
procedure TMaxTimeClass.CatchesTheInterruptionAndFails;
begin
  try
    LoopUntilStopped;
  except
    on ETimedOut do
      Assert.Fail('the test failed itself');
  end;
end;

//Catches the interruption and passes.
procedure TMaxTimeClass.CatchesTheInterruptionAndPasses;
begin
  try
    LoopUntilStopped;
  except
    on ETimedOut do
      Assert.Pass;
  end;
end;

//Lets Assert.WillRaiseDescendant catch the interruption, which it takes for the expected exception.
procedure TMaxTimeClass.CatchesTheInterruptionWithAssertWillRaise;
begin
  Assert.WillRaiseDescendant(LoopUntilStopped, Exception);
end;

//Catches the interruption and ignores itself.
procedure TMaxTimeClass.CatchesTheInterruptionAndIgnores;
begin
  try
    LoopUntilStopped;
  except
    on ETimedOut do
      Assert.Ignore('too slow here');
  end;
end;

//Catches the interruption and returns as if nothing had happened.
procedure TMaxTimeClass.CatchesTheInterruptionAndReturns;
begin
  try
    LoopUntilStopped;
  except
    on ETimedOut do
      ;
  end;
end;

//A fixture of the named tests of TMaxTimeClass, in order, each with the [MaxTime] at the same index.
function CreateMaxTimeFixture(const ATestNames : array of string; const AMaxTimes : array of cardinal;
  const AExpectedException : ExceptClass = nil; const AExceptionInheritance : TExceptionInheritance = exExact) : ITestFixture;
var
  rttiContext : TRttiContext;
  rttiType : TRttiType;
  rttiMethod : TRttiMethod;
  instance : TMaxTimeClass;
  method : TMethod;
  i : Integer;
begin
  _MaxTimeTestRanToTheEnd := False;
  rttiContext := TRttiContext.Create;
  rttiType := rttiContext.GetType(TMaxTimeClass);
  result := TDUnitXTestFixture.Create('AMaxTimeFixture', '', TMaxTimeClass, 'AUnit') as ITestFixture;
  instance := result.FixtureInstance as TMaxTimeClass;
  for i := 0 to High(ATestNames) do
  begin
    rttiMethod := rttiType.GetMethod(ATestNames[i]);
    method.Code := rttiMethod.CodeAddress;
    method.Data := instance;
    result.AddTest(ATestNames[i], TTestMethod(method), ATestNames[i], '', rttiMethod, True, False, '', AMaxTimes[i],
      AExpectedException, AExceptionInheritance);
  end;
end;

//Runs the fixture and returns how long it took, in milliseconds.  If a test is not interrupted, a thread ends its loop or
//its read once MAX_TIME_UNINTERRUPTED has passed, so a failure is reported instead of the run hanging.
function RunMaxTimeFixture(const AFixture : ITestFixture; out AResults : IRunResults; out ATestNames : string) : Cardinal;
var
  start : Cardinal;
  stopSafetyNet : THandle;
  safetyNet : TThread;
begin
  _MaxTimeStopBusyLoop := False;
  if not CreatePipe(_MaxTimePipeRead, _MaxTimePipeWrite, nil, 0) then
    RaiseLastOSError;
  stopSafetyNet := CreateEvent(nil, True, False, nil);
  safetyNet := TThread.CreateAnonymousThread(
    procedure
    var
      buffer : Byte;
      bytesWritten : Cardinal;
    begin
      if WaitForSingleObject(stopSafetyNet, MAX_TIME_UNINTERRUPTED) = WAIT_TIMEOUT then
      begin
        _MaxTimeStopBusyLoop := True;
        buffer := 0;
        WriteFile(_MaxTimePipeWrite, buffer, 1, bytesWritten, nil);
      end;
    end);
  safetyNet.FreeOnTerminate := False;
  try
    safetyNet.Start;
    start := GetTickCount;
    AResults := RunFixtureOnItsOwnThread(AFixture, ATestNames);
    result := GetTickCount - start;
  finally
    SetEvent(stopSafetyNet);
    safetyNet.WaitFor;
    safetyNet.Free;
    CloseHandle(stopSafetyNet);
    CloseHandle(_MaxTimePipeWrite);
    CloseHandle(_MaxTimePipeRead);
  end;
end;

//Checks that the one test in AResults failed with the timeout (not errored, as an access violation would be), and that
//it was stopped promptly rather than running on to its end.
procedure CheckInterrupted(const AResults : IRunResults; const AElapsed : Cardinal;
  const AStoppedWithin : Cardinal = MAX_TIME_UNINTERRUPTED div 2);
var
  testResult : ITestResult;
  resultMessage : string;
begin
  resultMessage := '';
  for testResult in AResults.GetAllTestResults do
    resultMessage := testResult.Message;
  Assert.AreEqual(1, AResults.TestCount, 'the number of tests');
  Assert.AreEqual(0, AResults.ErrorCount, 'the overrun errored: ' + resultMessage);
  Assert.AreEqual(0, AResults.PassCount, 'the overrun passed');
  Assert.AreEqual(1, AResults.FailureCount, 'the overrun did not fail');
  Assert.AreEqual(SOperationTimedOut, resultMessage, false, 'the failure is not the timeout');
  Assert.IsFalse(_MaxTimeTestRanToTheEnd, 'the test ran to its end instead of being interrupted');
  Assert.IsTrue(AElapsed < AStoppedWithin, Format('the test was stopped after %d ms, not soon after its [MaxTime] of %d ms',
    [AElapsed, MAX_TIME_LIMIT]));
end;

procedure TMaxTimeCaseClass.BusyLoop(const AValue : Integer);
begin
  while not _MaxTimeStopBusyLoop do
    ;
  _MaxTimeTestRanToTheEnd := True;
end;

procedure TMaxTimeWillRaiseCaseClass.BusyLoop(const AValue : Integer);
begin
  while not _MaxTimeStopBusyLoop do
    ;
  _MaxTimeTestRanToTheEnd := True;
end;

procedure TMaxTimeExpectsTimeoutCaseClass.BusyLoop(const AValue : Integer);
begin
  while not _MaxTimeStopBusyLoop do
    ;
  _MaxTimeTestRanToTheEnd := True;
end;

//A fixture of AClass's tests, generated from their attributes by the fixture provider, as a suite's are.
function GenerateMaxTimeFixture(const AClass : TClass) : ITestFixture;
var
  provider : TDUnitXFixtureProvider;
  providerIntf : IFixtureProvider;
begin
  _MaxTimeTestRanToTheEnd := False;
  result := TDUnitXTestFixture.Create(AClass.ClassName, '', AClass, 'AUnit') as ITestFixture;
  provider := TDUnitXFixtureProvider.Create;
  providerIntf := provider;
  TMaxTimeFixtureProviderAccess(provider).GenerateTests(nil, result);
end;

{ TMaxTimeTests }

procedure TMaxTimeTests.Overrun_In_A_Busy_Loop_Is_Interrupted_And_Fails;
var
  results : IRunResults;
  testNames : string;
  elapsed : Cardinal;
begin
  elapsed := RunMaxTimeFixture(CreateMaxTimeFixture(['BusyLoop'], [MAX_TIME_LIMIT]), results, testNames);
  CheckInterrupted(results, elapsed);
end;

procedure TMaxTimeTests.Overrun_In_A_Sleep_Loop_Is_Interrupted_And_Fails;
var
  results : IRunResults;
  testNames : string;
  elapsed : Cardinal;
begin
  elapsed := RunMaxTimeFixture(CreateMaxTimeFixture(['SleepLoop'], [MAX_TIME_LIMIT]), results, testNames);
  CheckInterrupted(results, elapsed);
end;

procedure TMaxTimeTests.Blocked_In_A_Synchronous_Read_Is_Interrupted_And_Fails;
var
  results : IRunResults;
  testNames : string;
  elapsed : Cardinal;
begin
  elapsed := RunMaxTimeFixture(CreateMaxTimeFixture(['SynchronousRead'], [MAX_TIME_LIMIT]), results, testNames);
  CheckInterrupted(results, elapsed);
end;

procedure TMaxTimeTests.Blocked_In_An_Alertable_Wait_Is_Interrupted_And_Fails;
var
  results : IRunResults;
  testNames : string;
  elapsed : Cardinal;
begin
  elapsed := RunMaxTimeFixture(CreateMaxTimeFixture(['AlertableWait'], [MAX_TIME_LIMIT]), results, testNames);
  CheckInterrupted(results, elapsed);
end;

procedure TMaxTimeTests.The_Next_Test_Runs_And_Its_Alertable_Wait_Is_Not_Cut_Short;
var
  results : IRunResults;
  testNames : string;
begin
  _MaxTimeAlertableWaitResult := WAIT_FAILED;
  RunMaxTimeFixture(CreateMaxTimeFixture(['BusyLoop', 'WaitsAlertably'], [MAX_TIME_LIMIT, MAX_TIME_GENEROUS]), results, testNames);
  Assert.AreEqual(',BusyLoop,WaitsAlertably,', testNames, 'the tests run, in order');
  Assert.AreEqual(1, results.FailureCount, 'the interrupted test did not fail');
  Assert.AreEqual(1, results.PassCount, 'the test after the interrupted one did not pass');
  Assert.AreEqual(Cardinal(0), _MaxTimeAlertableWaitResult, 'SleepEx in the next test ended early (WAIT_IO_COMPLETION is 192)');
end;

procedure TMaxTimeTests.A_Caught_Interruption_Replaced_By_A_Failure_Is_Reported_As_The_Timeout;
var
  results : IRunResults;
  testNames : string;
  elapsed : Cardinal;
begin
  elapsed := RunMaxTimeFixture(CreateMaxTimeFixture(['CatchesTheInterruptionAndFails'], [MAX_TIME_LIMIT]), results,
    testNames);
  CheckInterrupted(results, elapsed);
end;

procedure TMaxTimeTests.A_Caught_Interruption_Replaced_By_A_Pass_Is_Reported_As_The_Timeout;
var
  results : IRunResults;
  testNames : string;
  elapsed : Cardinal;
begin
  elapsed := RunMaxTimeFixture(CreateMaxTimeFixture(['CatchesTheInterruptionAndPasses'], [MAX_TIME_LIMIT]), results,
    testNames);
  CheckInterrupted(results, elapsed);
end;

procedure TMaxTimeTests.A_Caught_Interruption_The_Test_Returns_From_Is_Reported_As_The_Timeout;
var
  results : IRunResults;
  testNames : string;
  elapsed : Cardinal;
begin
  elapsed := RunMaxTimeFixture(CreateMaxTimeFixture(['CatchesTheInterruptionAndReturns'], [MAX_TIME_LIMIT]), results,
    testNames);
  CheckInterrupted(results, elapsed);
end;

//The expected-exception check catches the interruption and replaces it with "expected EConvertError, got ETimedOut".
procedure TMaxTimeTests.An_Interrupted_WillRaise_Test_Is_Reported_As_The_Timeout;
var
  results : IRunResults;
  testNames : string;
  elapsed : Cardinal;
begin
  elapsed := RunMaxTimeFixture(CreateMaxTimeFixture(['BusyLoop'], [MAX_TIME_LIMIT], EConvertError), results, testNames);
  CheckInterrupted(results, elapsed);
end;

//ETimedOut is an Exception, so [WillRaise(Exception, exDescendant)] takes the interruption for the expected exception.
procedure TMaxTimeTests.An_Interrupted_WillRaise_Descendant_Test_Is_Reported_As_The_Timeout;
var
  results : IRunResults;
  testNames : string;
  elapsed : Cardinal;
begin
  elapsed := RunMaxTimeFixture(CreateMaxTimeFixture(['BusyLoop'], [MAX_TIME_LIMIT], Exception, exDescendant), results,
    testNames);
  CheckInterrupted(results, elapsed);
end;

procedure TMaxTimeTests.An_Interrupted_Test_That_Expects_ETimedOut_Passes;
var
  results : IRunResults;
  testNames : string;
  elapsed : Cardinal;
begin
  elapsed := RunMaxTimeFixture(CreateMaxTimeFixture(['BusyLoop'], [MAX_TIME_LIMIT], ETimedOut), results, testNames);
  Assert.AreEqual(1, results.TestCount, 'the number of tests');
  Assert.AreEqual(1, results.PassCount, 'a test that expects the timeout did not pass');
  Assert.IsFalse(_MaxTimeTestRanToTheEnd, 'the test ran to its end instead of being interrupted');
  Assert.IsTrue(elapsed < MAX_TIME_UNINTERRUPTED div 2, Format('the test was stopped after %d ms, not soon after its ' +
    '[MaxTime] of %d ms', [elapsed, MAX_TIME_LIMIT]));
end;

procedure TMaxTimeTests.An_Interruption_Caught_By_Assert_WillRaise_Is_Reported_As_The_Timeout;
var
  results : IRunResults;
  testNames : string;
  elapsed : Cardinal;
begin
  elapsed := RunMaxTimeFixture(CreateMaxTimeFixture(['CatchesTheInterruptionWithAssertWillRaise'], [MAX_TIME_LIMIT]),
    results, testNames);
  CheckInterrupted(results, elapsed);
end;

//Assert.Ignore is the test's own choice, so it is not replaced by the timeout.
procedure TMaxTimeTests.A_Caught_Interruption_The_Test_Ignores_Is_Ignored;
var
  results : IRunResults;
  testNames : string;
begin
  RunMaxTimeFixture(CreateMaxTimeFixture(['CatchesTheInterruptionAndIgnores'], [MAX_TIME_LIMIT]), results, testNames);
  Assert.AreEqual(1, results.TestCount, 'the number of tests');
  Assert.AreEqual(1, results.IgnoredCount, 'a test that ignored itself was not reported ignored');
  Assert.AreEqual(0, results.FailureCount, 'a test that ignored itself was reported as failing');
end;

procedure TMaxTimeTests.Within_Its_MaxTime_Passes;
var
  results : IRunResults;
  testNames : string;
begin
  RunMaxTimeFixture(CreateMaxTimeFixture(['Passes'], [MAX_TIME_GENEROUS]), results, testNames);
  Assert.AreEqual(1, results.TestCount, 'the number of tests');
  Assert.AreEqual(1, results.PassCount, 'a test within its [MaxTime] did not pass');
  Assert.IsTrue(_MaxTimeTestRanToTheEnd, 'the test did not run');
end;

procedure TMaxTimeTests.A_Caught_Interruption_Is_Followed_By_Another;
var
  results : IRunResults;
  testNames : string;
  elapsed : Cardinal;
begin
  _MaxTimeInterruptionsCaught := 0;
  elapsed := RunMaxTimeFixture(CreateMaxTimeFixture(['CatchesTheInterruptionThenHangsAgain'], [MAX_TIME_LIMIT]), results,
    testNames);
  Assert.AreEqual(1, _MaxTimeInterruptionsCaught, 'the test did not catch the first interruption');
  //Interrupted again about a second after the first: well before the safety net ends the loop.
  CheckInterrupted(results, elapsed, MAX_TIME_UNINTERRUPTED - 500);
end;

procedure TMaxTimeTests.A_TestCase_Overrun_Is_Interrupted_And_Fails;
var
  results : IRunResults;
  testNames : string;
  elapsed : Cardinal;
begin
  elapsed := RunMaxTimeFixture(GenerateMaxTimeFixture(TMaxTimeCaseClass), results, testNames);
  CheckInterrupted(results, elapsed);
end;

procedure TMaxTimeTests.An_Interrupted_WillRaise_TestCase_Is_Reported_As_The_Timeout;
var
  results : IRunResults;
  testNames : string;
  elapsed : Cardinal;
begin
  elapsed := RunMaxTimeFixture(GenerateMaxTimeFixture(TMaxTimeWillRaiseCaseClass), results, testNames);
  CheckInterrupted(results, elapsed);
end;

procedure TMaxTimeTests.An_Interrupted_TestCase_That_Expects_ETimedOut_Passes;
var
  results : IRunResults;
  testNames : string;
  elapsed : Cardinal;
begin
  elapsed := RunMaxTimeFixture(GenerateMaxTimeFixture(TMaxTimeExpectsTimeoutCaseClass), results, testNames);
  Assert.AreEqual(1, results.TestCount, 'the number of tests');
  Assert.AreEqual(1, results.PassCount, 'a case that expects the timeout did not pass');
  Assert.IsFalse(_MaxTimeTestRanToTheEnd, 'the case ran to its end instead of being interrupted');
  Assert.IsTrue(elapsed < MAX_TIME_UNINTERRUPTED div 2, Format('the case was stopped after %d ms, not soon after its ' +
    '[MaxTime] of %d ms', [elapsed, MAX_TIME_LIMIT]));
end;
{$ENDIF}

initialization
  TDUnitX.RegisterTestFixture(TTestClassWithNonPublicSetup);
  TDUnitX.RegisterTestFixture(TTestClassWithTestSource);
  TDUnitX.RegisterTestFixture(TTestRepeatAttribute);
  TDUnitX.RegisterTestFixture(TTestMaxTimeAttribute);
  TDUnitX.RegisterTestFixture(TDUnitXTestFixtureTests);
  TDUnitX.RegisterTestFixture(TTearDownFixtureFailureTests);
  TDUnitX.RegisterTestFixture(TFixtureResultDurationTests);
  TDUnitX.RegisterTestFixture(TRuntimeIgnoreTests);
{$IFDEF MSWINDOWS}
  TDUnitX.RegisterTestFixture(TMaxTimeTests);
{$ENDIF}

end.

