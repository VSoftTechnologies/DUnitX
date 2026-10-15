unit DUnitX.Tests.Framework;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TTearDownFailureTests = class
  public
    [Test]
    procedure TearDown_Error_Does_Not_Replace_The_Test_Failure;
    [Test]
    procedure TearDown_Error_Does_Not_Replace_The_Setup_Error;
    [Test]
    procedure TearDown_Error_Still_Fails_A_Passing_Test;
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
  System.Rtti,
  DUnitX.Extensibility,
  DUnitX.InternalInterfaces,
  DUnitX.TestFixture,
  DUnitX.RunResults,
  DUnitX.TestRunner;

type
  TTearDownRunnerAccess = class(TDUnitXTestRunner);

  TTearDownFails = class
  public
    procedure Setup;
    procedure SetupFails;
    procedure TearDownFails;
    procedure FailsItself;
    procedure Passes;
  end;

{ TTearDownFails }

procedure TTearDownFails.Setup;
begin
end;

procedure TTearDownFails.SetupFails;
begin
  raise EInvalidOpException.Create('the Setup error');
end;

procedure TTearDownFails.TearDownFails;
begin
  raise EInvalidOpException.Create('the TearDown error');
end;

procedure TTearDownFails.FailsItself;
begin
  Assert.Fail('the test failure');
end;

procedure TTearDownFails.Passes;
begin
  Assert.Pass;
end;

//Runs one test, whose TearDown raises, through a runner of its own. A runner registers itself for its thread, so it gets a
//thread of its own.
function RunWithFailingTearDown(const ATestMethod : string; const ASetupFails : boolean) : ITestResult;
var
  thread : TThread;
  results : IRunResults;
  failure : string;
  testResult : ITestResult;
begin
  results := nil;
  failure := '';
  thread := TThread.CreateAnonymousThread(
    procedure
    var
      runner : TDUnitXTestRunner;
      runnerIntf : ITestRunner;
      rttiContext : TRttiContext;
      rttiType : TRttiType;
      fixture : ITestFixture;
      instance : TTearDownFails;
      method : TMethod;
      fixtures : ITestFixtureList;
      context : ITestExecuteContext;
    begin
      try
        runner := TDUnitXTestRunner.Create;
        runnerIntf := runner;
        rttiContext := TRttiContext.Create;
        rttiType := rttiContext.GetType(TTearDownFails);
        fixture := TDUnitXTestFixture.Create('ATearDownFailsFixture', '', TTearDownFails, 'AUnit') as ITestFixture;
        instance := fixture.FixtureInstance as TTearDownFails;
        if ASetupFails then
          fixture.SetSetupTestMethod('SetupFails', instance.SetupFails)
        else
          fixture.SetSetupTestMethod('Setup', instance.Setup);
        fixture.SetTearDownTestMethod('TearDownFails', instance.TearDownFails);
        method.Code := rttiType.GetMethod(ATestMethod).CodeAddress;
        method.Data := instance;
        fixture.AddTest(ATestMethod, TTestMethod(method), ATestMethod, '', rttiType.GetMethod(ATestMethod));
        fixtures := TTestFixtureList.Create;
        fixtures.Add(fixture);
        results := TDUnitXRunResults.Create;
        context := results as ITestExecuteContext;
        TTearDownRunnerAccess(runner).ExecuteFixtures(nil, context, TThread.CurrentThread.ThreadID, fixtures);
        context.RollupResults;
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
  Assert.AreEqual(1, results.TestCount, 'one test ran');
  result := nil;
  for testResult in results.GetAllTestResults do
    result := testResult;
end;

{ TTearDownFailureTests }

procedure TTearDownFailureTests.TearDown_Error_Does_Not_Replace_The_Test_Failure;
var
  testResult : ITestResult;
begin
  testResult := RunWithFailingTearDown('FailsItself', false);
  Assert.Contains(testResult.Message, 'the test failure', 'the result keeps the test''s own failure');
  Assert.AreEqual(Ord(TTestResultType.Failure), Ord(testResult.ResultType), 'a failure, not the TearDown error');
end;

procedure TTearDownFailureTests.TearDown_Error_Does_Not_Replace_The_Setup_Error;
var
  testResult : ITestResult;
begin
  testResult := RunWithFailingTearDown('Passes', true);
  Assert.Contains(testResult.Message, 'the Setup error', 'the result keeps the Setup error');
  Assert.AreEqual(Ord(TTestResultType.Error), Ord(testResult.ResultType), 'an error');
end;

procedure TTearDownFailureTests.TearDown_Error_Still_Fails_A_Passing_Test;
var
  testResult : ITestResult;
begin
  testResult := RunWithFailingTearDown('Passes', false);
  Assert.Contains(testResult.Message, 'the TearDown error', 'a passing test reports the TearDown error');
  Assert.AreEqual(Ord(TTestResultType.Error), Ord(testResult.ResultType), 'an error');
end;

initialization
  TDUnitX.RegisterTestFixture(TTearDownFailureTests);

end.
