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
  DUnitX.Loggers.XML.JUnit,
  DUnitX.Loggers.XML.NUnit,
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

initialization
  TDUnitX.RegisterTestFixture(TTestClassWithNonPublicSetup);
  TDUnitX.RegisterTestFixture(TTestClassWithTestSource);
  TDUnitX.RegisterTestFixture(TTestRepeatAttribute);
  TDUnitX.RegisterTestFixture(TTestMaxTimeAttribute);
  TDUnitX.RegisterTestFixture(TDUnitXTestFixtureTests);
  TDUnitX.RegisterTestFixture(TTearDownFixtureFailureTests);
  TDUnitX.RegisterTestFixture(TFixtureResultDurationTests);
  TDUnitX.RegisterTestFixture(TRuntimeIgnoreTests);

end.

