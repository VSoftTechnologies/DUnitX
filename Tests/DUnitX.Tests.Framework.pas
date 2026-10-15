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

  [TestFixture]
  TTestLogTests = class
  public
    [Test]
    procedure Results_Carry_Their_Own_Log_Including_Setup;
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

type
  TLogsInSetup = class
  public
    procedure Setup;
    procedure TestOne;
    procedure TestTwo;
  end;

var
  logSetupCalls : integer;

{ TLogsInSetup }

procedure TLogsInSetup.Setup;
begin
  Inc(logSetupCalls);
  TDUnitX.CurrentRunner.Log('logged in setup');
  //TestOne fails, then TestTwo fails in Setup.
  if logSetupCalls = 2 then
    raise EInvalidOpException.Create('the second Setup fails');
end;

procedure TLogsInSetup.TestOne;
begin
  TDUnitX.CurrentRunner.Log('logged by TestOne');
  Assert.Fail('TestOne fails');
end;

procedure TLogsInSetup.TestTwo;
begin
  TDUnitX.CurrentRunner.Log('logged by TestTwo');
  Assert.Fail('TestTwo fails');
end;

//Runs TLogsInSetup's two tests through a runner of its own, on a thread of its own, and returns each result's log.
procedure RunLogsInSetup(out ALogOne : string; out ALogTwo : string);
var
  thread : TThread;
  logOne : string;
  logTwo : string;
  failure : string;
begin
  logOne := '';
  logTwo := '';
  failure := '';
  logSetupCalls := 0;
  thread := TThread.CreateAnonymousThread(
    procedure
    var
      runner : TDUnitXTestRunner;
      runnerIntf : ITestRunner;
      rttiContext : TRttiContext;
      rttiType : TRttiType;
      fixture : ITestFixture;
      instance : TLogsInSetup;
      fixtures : ITestFixtureList;
      results : IRunResults;
      context : ITestExecuteContext;
      testResult : ITestResult;
      logMessage : TLogMessage;
      log : string;
    begin
      try
        runner := TDUnitXTestRunner.Create;
        runnerIntf := runner;
        rttiContext := TRttiContext.Create;
        rttiType := rttiContext.GetType(TLogsInSetup);
        fixture := TDUnitXTestFixture.Create('ALogsInSetupFixture', '', TLogsInSetup, 'AUnit') as ITestFixture;
        instance := fixture.FixtureInstance as TLogsInSetup;
        fixture.SetSetupTestMethod('Setup', instance.Setup);
        fixture.AddTest('TestOne', instance.TestOne, 'TestOne', '', rttiType.GetMethod('TestOne'));
        fixture.AddTest('TestTwo', instance.TestTwo, 'TestTwo', '', rttiType.GetMethod('TestTwo'));
        fixtures := TTestFixtureList.Create;
        fixtures.Add(fixture);
        results := TDUnitXRunResults.Create;
        context := results as ITestExecuteContext;
        TTearDownRunnerAccess(runner).ExecuteFixtures(nil, context, TThread.CurrentThread.ThreadID, fixtures);
        context.RollupResults;
        for testResult in results.GetAllTestResults do
        begin
          log := '';
          for logMessage in testResult.LogMessages do
            log := log + logMessage.Msg + '|';
          if testResult.Test.Name = 'TestOne' then
            logOne := log
          else
            logTwo := log;
        end;
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
  ALogOne := logOne;
  ALogTwo := logTwo;
end;

{ TTestLogTests }

procedure TTestLogTests.Results_Carry_Their_Own_Log_Including_Setup;
var
  logOne : string;
  logTwo : string;
begin
  RunLogsInSetup(logOne, logTwo);
  Assert.DoesNotContain(logTwo, 'logged by TestOne', false, 'TestTwo''s result carries TestOne''s log: ' + logTwo);
  Assert.Contains(logTwo, 'logged in setup', false, 'TestTwo''s result: ' + logTwo);
  Assert.Contains(logOne, 'logged in setup', false, 'TestOne''s result: ' + logOne);
  Assert.Contains(logOne, 'logged by TestOne', false, 'TestOne''s result: ' + logOne);
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
  TDUnitX.RegisterTestFixture(TTestLogTests);

end.
