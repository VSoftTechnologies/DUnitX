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

    //We ignore the timeout test for the moment. We require a way to test timeout without calling it to fail the test itself.
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
{$M-}

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
  end;
{$M-}

implementation

uses
{$IFDEF USE_NS}
  System.Math,
  System.SysUtils,
  System.Rtti,
  System.Classes,
{$ELSE}
  Math,
  SysUtils,
  Rtti,
  Classes,
{$ENDIF}
  DUnitX.Types,
  DUnitX.Exceptions,
  DUnitX.Extensibility,
  DUnitX.FixtureResult,
  DUnitX.InternalInterfaces,
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

  //[TestCase('Name', '')] gives no values at all
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

initialization
  TDUnitX.RegisterTestFixture(TTestClassWithNonPublicSetup);
  TDUnitX.RegisterTestFixture(TTestClassWithTestSource);
  TDUnitX.RegisterTestFixture(TTestRepeatAttribute);
  TDUnitX.RegisterTestFixture(TTestMaxTimeAttribute);
  TDUnitX.RegisterTestFixture(TDUnitXTestFixtureTests);
  TDUnitX.RegisterTestFixture(TTearDownFixtureFailureTests);
  TDUnitX.RegisterTestFixture(TFixtureResultDurationTests);

end.

