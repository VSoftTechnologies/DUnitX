{***************************************************************************}
{                                                                           }
{           DUnitX                                                          }
{                                                                           }
{           Copyright (C) 2015 Vincent Parrett & Contributors               }
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

unit DUnitX.Tests.ParallelFixtures;

interface

uses
  DUnitX.TestFramework;

type
  //Fixtures run in parallel when the runner's MaxThreads is above 1 (issue #532).  Each test runs a tree of probe
  //fixtures through a runner of its own and checks what the run recorded.
  [TestFixture]
  TParallelFixtureTests = class
  public
    [Test]
    procedure Fixtures_Run_Concurrently;
    [Test]
    procedure Each_Result_Carries_Only_Its_Own_Log;
    [Test]
    procedure No_Asserts_Check_Is_Per_Test;
    [Test]
    procedure Run_Totals_Match_The_Results;
    [Test]
    procedure Fixture_Setup_And_TearDown_Run_Once_Per_Fixture;
    [Test]
    procedure Loggers_Are_Never_Called_Concurrently;
    [Test]
    procedure Results_Are_In_The_Serial_Order;
    [Test]
    procedure NUnit_Xml_Is_The_Same_As_A_Serial_Run;
    [Test]
    procedure JUnit_Xml_Is_The_Same_As_A_Serial_Run;
    [Test]
    procedure Parallelism_0_Runs_On_The_Runner_Thread;
    [Test]
    procedure Parallelism_1_Runs_On_The_Runner_Thread;
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
  System.Rtti,
  System.SyncObjs,
  System.RegularExpressions,
  System.Generics.Collections,
  DUnitX.Extensibility,
  DUnitX.InternalInterfaces,
  DUnitX.TestFixture,
  DUnitX.RunResults,
  DUnitX.TestRunner,
  DUnitX.Loggers.Null,
  DUnitX.Loggers.XML.NUnit,
  DUnitX.Loggers.XML.JUnit;

const
  PROBE_FIXTURES = 6;
  LOG_LINES = 25;

type
  TParallelRunnerAccess = class(TDUnitXTestRunner);

  EProbeError = class(Exception);

  //The fixture class the probe fixtures are made from.  It has no [TestFixture] attribute, so the suite itself does not
  //pick it up; each fixture made from it has its own instance, whose Tag is the fixture's name.
  TParallelProbe = class
  public
    Tag : string;
    procedure SetupFixture;
    procedure TearDownFixture;
    procedure Rendezvous;
    procedure Passes;
    procedure Fails;
    procedure Errors;
    procedure IgnoresAtRunTime;
    procedure LogsItsOwnName;
    procedure AssertsMany;
    procedure MakesNoAssert;
  end;

  //Counts its callers inside it at once, to show that the runner never calls a logger from two threads at the same time.
  TOverlapLogger = class(TDUnitXNullLogger)
  private
    FInside : integer;
    FCalls : integer;
    FOverlaps : integer;
    FWrongThreadIds : integer;
    FSpin : integer;
    procedure Enter(const threadId : TThreadID);
    procedure Leave;
  protected
    procedure OnBeginTest(const threadId : TThreadID; const Test : ITestInfo); override;
    procedure OnEndTest(const threadId : TThreadID; const Test : ITestResult); override;
    procedure OnExecuteTest(const threadId : TThreadID; const Test : ITestInfo); override;
    procedure OnLog(const logType : TLogLevel; const msg : string); override;
    procedure OnStartTestFixture(const threadId : TThreadID; const fixture : ITestFixtureInfo); override;
    procedure OnEndTestFixture(const threadId : TThreadID; const results : IFixtureResult); override;
    procedure OnTestSuccess(const threadId : TThreadID; const Test : ITestResult); override;
  end;

  TProbeRun = record
    Results : IRunResults;
    RunnerThread : TThreadID;
  end;

var
  probeLock : TObject;
  probeSetups : TDictionary<string, integer>;
  probeTearDowns : TDictionary<string, integer>;
  probeThreads : TDictionary<string, TThreadID>;
  probeGate : TCountdownEvent;

procedure ResetProbes(const AGateCount : integer);
begin
  MonitorEnter(probeLock);
  try
    probeSetups.Clear;
    probeTearDowns.Clear;
    probeThreads.Clear;
    FreeAndNil(probeGate);
    if AGateCount > 0 then
      probeGate := TCountdownEvent.Create(AGateCount);
  finally
    MonitorExit(probeLock);
  end;
end;

procedure CountProbe(const ACounts : TDictionary<string, integer>; const AKey : string);
var
  count : integer;
begin
  MonitorEnter(probeLock);
  try
    if not ACounts.TryGetValue(AKey, count) then
      count := 0;
    ACounts.AddOrSetValue(AKey, count + 1);
  finally
    MonitorExit(probeLock);
  end;
end;

procedure RecordProbeThread(const AKey : string);
begin
  MonitorEnter(probeLock);
  try
    probeThreads.AddOrSetValue(AKey, TThread.Current.ThreadID);
  finally
    MonitorExit(probeLock);
  end;
end;

function DistinctProbeThreads : integer;
var
  threads : TList<TThreadID>;
  threadId : TThreadID;
begin
  threads := TList<TThreadID>.Create;
  try
    for threadId in probeThreads.Values do
      if not threads.Contains(threadId) then
        threads.Add(threadId);
    result := threads.Count;
  finally
    threads.Free;
  end;
end;

{ TParallelProbe }

procedure TParallelProbe.SetupFixture;
begin
  CountProbe(probeSetups, Tag);
end;

procedure TParallelProbe.TearDownFixture;
begin
  CountProbe(probeTearDowns, Tag);
end;

procedure TParallelProbe.Rendezvous;
begin
  RecordProbeThread(Tag + '.Rendezvous');
  probeGate.Signal;
  Assert.IsTrue(probeGate.WaitFor(10000) = wrSignaled, Tag + ' never had the other fixtures running alongside it');
end;

procedure TParallelProbe.Passes;
begin
  RecordProbeThread(Tag + '.Passes');
  Assert.IsTrue(True);
end;

procedure TParallelProbe.Fails;
begin
  Assert.Fail(Tag + ' fails');
end;

procedure TParallelProbe.Errors;
begin
  raise EProbeError.Create(Tag + ' errors');
end;

procedure TParallelProbe.IgnoresAtRunTime;
begin
  Assert.Ignore(Tag + ' is ignored');
end;

procedure TParallelProbe.LogsItsOwnName;
var
  i : integer;
begin
  RecordProbeThread(Tag + '.LogsItsOwnName');
  for i := 1 to LOG_LINES do
  begin
    TDUnitX.CurrentRunner.Log(Tag + '.' + TDUnitX.CurrentRunner.CurrentTestName);
    Sleep(0);
  end;
  Assert.AreEqual('LogsItsOwnName', TDUnitX.CurrentRunner.CurrentTestName, false);
end;

procedure TParallelProbe.AssertsMany;
var
  i : integer;
begin
  RecordProbeThread(Tag + '.AssertsMany');
  for i := 1 to 50 do
  begin
    Assert.IsTrue(True);
    Sleep(0);
  end;
end;

procedure TParallelProbe.MakesNoAssert;
begin
  RecordProbeThread(Tag + '.MakesNoAssert');
  Sleep(20);
end;

{ TOverlapLogger }

procedure TOverlapLogger.Enter(const threadId : TThreadID);
var
  i : integer;
begin
  TInterlocked.Increment(FCalls);
  if TInterlocked.Increment(FInside) > 1 then
    TInterlocked.Increment(FOverlaps);
  if threadId <> TThread.Current.ThreadID then
    TInterlocked.Increment(FWrongThreadIds);
  //Stay inside long enough for a second caller to arrive, if the runner lets one in.
  for i := 1 to 20000 do
    TInterlocked.Add(FSpin, 1);
end;

procedure TOverlapLogger.Leave;
begin
  TInterlocked.Decrement(FInside);
end;

procedure TOverlapLogger.OnBeginTest(const threadId : TThreadID; const Test : ITestInfo);
begin
  Enter(threadId);
  Leave;
end;

procedure TOverlapLogger.OnEndTest(const threadId : TThreadID; const Test : ITestResult);
begin
  Enter(threadId);
  Leave;
end;

procedure TOverlapLogger.OnExecuteTest(const threadId : TThreadID; const Test : ITestInfo);
begin
  Enter(threadId);
  Leave;
end;

procedure TOverlapLogger.OnLog(const logType : TLogLevel; const msg : string);
begin
  Enter(TThread.Current.ThreadID);
  Leave;
end;

procedure TOverlapLogger.OnStartTestFixture(const threadId : TThreadID; const fixture : ITestFixtureInfo);
begin
  Enter(threadId);
  Leave;
end;

procedure TOverlapLogger.OnEndTestFixture(const threadId : TThreadID; const results : IFixtureResult);
begin
  Enter(threadId);
  Leave;
end;

procedure TOverlapLogger.OnTestSuccess(const threadId : TThreadID; const Test : ITestResult);
begin
  Enter(threadId);
  Leave;
end;

//Gives a probe fixture its fixture setup and teardown and the named tests; AIgnoredTest adds one test ignored when built.
procedure ConfigureProbe(const AFixture : ITestFixture; const ATests : array of string; const AIgnoredTest : boolean);
var
  rttiContext : TRttiContext;
  rttiType : TRttiType;
  rttiMethod : TRttiMethod;
  instance : TParallelProbe;
  testName : string;
  method : TMethod;
begin
  rttiContext := TRttiContext.Create;
  rttiType := rttiContext.GetType(TParallelProbe);
  instance := AFixture.FixtureInstance as TParallelProbe;
  instance.Tag := AFixture.Name;
  AFixture.SetSetupFixtureMethod('SetupFixture', instance.SetupFixture);
  AFixture.SetTearDownFixtureMethod('TearDownFixture', instance.TearDownFixture, false);
  for testName in ATests do
  begin
    rttiMethod := rttiType.GetMethod(testName);
    method.Code := rttiMethod.CodeAddress;
    method.Data := instance;
    AFixture.AddTest(testName, TTestMethod(method), testName, '', rttiMethod);
  end;
  if AIgnoredTest then
    AFixture.AddTest('Passes', instance.Passes, 'IgnoredWhenBuilt', '', rttiType.GetMethod('Passes'), true, true, 'ignored when built');
end;

//Builds a tree as the fixture provider does from unit names: a namespace-like container holding ACount - 1 probes, then one probe at the top level.
function BuildProbes(const ACount : integer; const ATests : array of string; const AIgnoredTest : boolean = false) : ITestFixtureList;
var
  container : ITestFixture;
  fixture : ITestFixture;
  i : integer;
begin
  result := TTestFixtureList.Create;
  container := TDUnitXTestFixture.Create('ProbeContainer', '', TObject, 'AUnit') as ITestFixture;
  result.Add(container);
  for i := 1 to ACount - 1 do
    ConfigureProbe(container.AddChildFixture(TParallelProbe, Format('Probe%d', [i]), ''), ATests, AIgnoredTest);
  fixture := TDUnitXTestFixture.Create(Format('Probe%d', [ACount]), '', TParallelProbe, 'AUnit') as ITestFixture;
  ConfigureProbe(fixture, ATests, AIgnoredTest);
  result.Add(fixture);
end;

//Runs the fixtures as Execute does, through a runner of its own.  A runner registers itself for its thread, so it gets a
//thread of its own.
function RunProbes(const AFixtures : ITestFixtureList; const AMaxThreads : integer; const AFailsOnNoAsserts : boolean; const ALoggers :
  TArray<ITestLogger>) : TProbeRun;
var
  thread : TThread;
  results : IRunResults;
  runnerThread : TThreadID;
  failure : string;
begin
  results := nil;
  runnerThread := 0;
  failure := '';
  thread := TThread.CreateAnonymousThread(
    procedure
    var
      runner : TDUnitXTestRunner;
      runnerIntf : ITestRunner;
      context : ITestExecuteContext;
      logger : ITestLogger;
    begin
      try
        runnerThread := TThread.Current.ThreadID;
        runner := TDUnitXTestRunner.Create;
        runnerIntf := runner;
        for logger in ALoggers do
          runnerIntf.AddLogger(logger);
        runnerIntf.UseThreadPool := AMaxThreads > 1;
        runnerIntf.MaxThreads := AMaxThreads;
        runnerIntf.FailsOnNoAsserts := AFailsOnNoAsserts;
        results := TDUnitXRunResults.Create;
        context := results as ITestExecuteContext;
        TParallelRunnerAccess(runner).Loggers_TestingStarts(runnerThread, 0, 0);
        TParallelRunnerAccess(runner).ExecuteFixtures(nil, context, runnerThread, AFixtures);
        context.RollupResults;
        TParallelRunnerAccess(runner).Loggers_TestingEnds(results);
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
  result.Results := results;
  result.RunnerThread := runnerThread;
end;

function ResultsOfType(const AResults : IRunResults; const AType : TTestResultType) : string;
var
  testResult : ITestResult;
begin
  result := '';
  for testResult in AResults.GetAllTestResults do
    if testResult.ResultType = AType then
      result := result + testResult.Test.FullName + ': ' + testResult.Message + '; ';
end;

function FullNames(const AResults : IRunResults) : string;
var
  testResult : ITestResult;
begin
  result := '';
  for testResult in AResults.GetAllTestResults do
    result := result + testResult.Test.FullName + #13#10;
end;

//What the XML loggers write, without the times and dates that differ from run to run.
function LoggedXml(const AFixtures : ITestFixtureList; const AMaxThreads : integer; const AJUnit : boolean) : string;
var
  stream : TStringStream;
  logger : ITestLogger;
begin
  stream := TStringStream.Create('', TEncoding.UTF8);
  try
    if AJUnit then
      logger := TDUnitXXMLJUnitLogger.Create(stream)
    else
      logger := TDUnitXXMLNUnitLogger.Create(stream);
    RunProbes(AFixtures, AMaxThreads, false, [logger]);
    logger := nil;
    result := stream.DataString;
  finally
    stream.Free;
  end;
  result := TRegEx.Replace(result, '\s(time|date|timestamp|duration|start-time|end-time)="[^"]*"', '');
end;

function CountOf(const ASubText : string; const AText : string) : integer;
var
  index : integer;
begin
  result := 0;
  index := Pos(ASubText, AText);
  while index > 0 do
  begin
    Inc(result);
    index := Pos(ASubText, AText, index + Length(ASubText));
  end;
end;

{ TParallelFixtureTests }

procedure TParallelFixtureTests.Fixtures_Run_Concurrently;
var
  run : TProbeRun;
  threadId : TThreadID;
begin
  //Each fixture waits until all four have arrived, so the run passes only if all four run at once.
  ResetProbes(4);
  run := RunProbes(BuildProbes(4, ['Rendezvous']), 4, false, nil);
  Assert.AreEqual(4, run.Results.PassCount, 'every fixture met the others: ' + ResultsOfType(run.Results, TTestResultType.Failure));
  Assert.AreEqual(4, DistinctProbeThreads, 'four fixtures ran on four threads');
  for threadId in probeThreads.Values do
    Assert.AreNotEqual(run.RunnerThread, threadId, 'no fixture ran on the runner''s own thread');
end;

procedure TParallelFixtureTests.Each_Result_Carries_Only_Its_Own_Log;
var
  run : TProbeRun;
  testResult : ITestResult;
  expected : string;
  logMessage : TLogMessage;
  checked : integer;
begin
  ResetProbes(0);
  run := RunProbes(BuildProbes(PROBE_FIXTURES, ['LogsItsOwnName', 'AssertsMany']), 4, false, nil);
  Assert.AreEqual(2 * PROBE_FIXTURES, run.Results.PassCount, ResultsOfType(run.Results, TTestResultType.Failure) +
    ResultsOfType(run.Results, TTestResultType.Error));
  Assert.IsTrue(DistinctProbeThreads > 1, 'the fixtures ran on more than one thread');
  checked := 0;
  for testResult in run.Results.GetAllTestResults do
  begin
    if testResult.Test.Name <> 'LogsItsOwnName' then
      Continue;
    expected := testResult.Test.Fixture.Name + '.LogsItsOwnName';
    Assert.AreEqual(LOG_LINES, Length(testResult.LogMessages), testResult.Test.FullName + ' carries every line it logged, and no other');
    for logMessage in testResult.LogMessages do
      Assert.AreEqual(expected, logMessage.Msg, false, testResult.Test.FullName + ' carries only its own log');
    Inc(checked);
  end;
  Assert.AreEqual(PROBE_FIXTURES, checked);
end;

procedure TParallelFixtureTests.No_Asserts_Check_Is_Per_Test;
var
  run : TProbeRun;
  testResult : ITestResult;
begin
  ResetProbes(0);
  run := RunProbes(BuildProbes(PROBE_FIXTURES, ['AssertsMany', 'MakesNoAssert']), 4, true, nil);
  Assert.IsTrue(DistinctProbeThreads > 1, 'the fixtures ran on more than one thread');
  for testResult in run.Results.GetAllTestResults do
  begin
    if testResult.Test.Name = 'AssertsMany' then
      Assert.AreEqual(Ord(TTestResultType.Pass), Ord(testResult.ResultType), testResult.Test.FullName + ': ' + testResult.Message)
    else
      Assert.AreEqual(Ord(TTestResultType.Failure), Ord(testResult.ResultType), testResult.Test.FullName +
        ' made no assertion, whatever other threads asserted');
  end;
  Assert.AreEqual(2 * PROBE_FIXTURES, run.Results.TestCount);
end;

procedure TParallelFixtureTests.Run_Totals_Match_The_Results;
var
  run : TProbeRun;
  fixtureResult : IFixtureResult;
  passes, failures, errors, ignored : integer;
begin
  ResetProbes(0);
  run := RunProbes(BuildProbes(PROBE_FIXTURES, ['Passes', 'Fails', 'Errors', 'IgnoresAtRunTime', 'MakesNoAssert'], true), 4, true, nil);
  //Each probe: 1 pass, 2 failures (Fails and MakesNoAssert), 1 error, 2 ignored (at run time and when built).
  Assert.AreEqual(6 * PROBE_FIXTURES, run.Results.TestCount, 'TestCount');
  Assert.AreEqual(1 * PROBE_FIXTURES, run.Results.PassCount, 'PassCount');
  Assert.AreEqual(2 * PROBE_FIXTURES, run.Results.FailureCount, 'FailureCount');
  Assert.AreEqual(1 * PROBE_FIXTURES, run.Results.ErrorCount, 'ErrorCount');
  Assert.AreEqual(2 * PROBE_FIXTURES, run.Results.IgnoredCount, 'IgnoredCount');
  Assert.AreEqual(0, run.Results.MemoryLeakCount, 'MemoryLeakCount');
  Assert.IsFalse(run.Results.AllPassed, 'AllPassed');
  Assert.AreEqual(2, run.Results.FixtureCount, 'the container and the top-level probe');
  passes := 0;
  failures := 0;
  errors := 0;
  ignored := 0;
  for fixtureResult in run.Results.FixtureResults do
  begin
    Inc(passes, fixtureResult.PassCount);
    Inc(failures, fixtureResult.FailureCount);
    Inc(errors, fixtureResult.ErrorCount);
    Inc(ignored, fixtureResult.IgnoredCount);
  end;
  Assert.AreEqual(run.Results.PassCount, passes, 'the fixture results roll up the passes');
  Assert.AreEqual(run.Results.FailureCount, failures, 'the fixture results roll up the failures');
  Assert.AreEqual(run.Results.ErrorCount, errors, 'the fixture results roll up the errors');
  Assert.AreEqual(run.Results.IgnoredCount, ignored, 'the fixture results roll up the ignored');
end;

procedure TParallelFixtureTests.Fixture_Setup_And_TearDown_Run_Once_Per_Fixture;
var
  i : integer;
  count : integer;
begin
  ResetProbes(0);
  RunProbes(BuildProbes(PROBE_FIXTURES, ['Passes', 'AssertsMany']), 4, false, nil);
  Assert.AreEqual(PROBE_FIXTURES, probeSetups.Count, 'every fixture ran its setup');
  Assert.AreEqual(PROBE_FIXTURES, probeTearDowns.Count, 'every fixture ran its teardown');
  for i := 1 to PROBE_FIXTURES do
  begin
    Assert.IsTrue(probeSetups.TryGetValue(Format('Probe%d', [i]), count) and (count = 1), Format('Probe%d setup ran once', [i]));
    Assert.IsTrue(probeTearDowns.TryGetValue(Format('Probe%d', [i]), count) and (count = 1), Format('Probe%d teardown ran once', [i]));
  end;
end;

procedure TParallelFixtureTests.Loggers_Are_Never_Called_Concurrently;
var
  logger : TOverlapLogger;
  loggerIntf : ITestLogger;
  run : TProbeRun;
begin
  ResetProbes(0);
  logger := TOverlapLogger.Create;
  loggerIntf := logger;
  run := RunProbes(BuildProbes(PROBE_FIXTURES, ['LogsItsOwnName', 'AssertsMany']), 4, false, [loggerIntf]);
  Assert.AreEqual(2 * PROBE_FIXTURES, run.Results.PassCount);
  Assert.IsTrue(DistinctProbeThreads > 1, 'the fixtures ran on more than one thread');
  Assert.IsTrue(logger.FCalls > 0, 'the logger was called');
  Assert.AreEqual(0, logger.FOverlaps, 'no logger call overlapped another');
  Assert.AreEqual(0, logger.FWrongThreadIds, 'every event names the thread it came from');
end;

procedure TParallelFixtureTests.Results_Are_In_The_Serial_Order;
var
  serial : string;
  parallel : string;
begin
  ResetProbes(0);
  serial := FullNames(RunProbes(BuildProbes(PROBE_FIXTURES, ['Passes', 'Fails', 'LogsItsOwnName']), 0, false, nil).Results);
  ResetProbes(0);
  parallel := FullNames(RunProbes(BuildProbes(PROBE_FIXTURES, ['Passes', 'Fails', 'LogsItsOwnName']), 4, false, nil).Results);
  Assert.AreEqual(3 * PROBE_FIXTURES, CountOf(#13#10, serial));
  Assert.AreEqual(serial, parallel, false, 'a parallel run lists its results in the order a serial run does');
end;

procedure TParallelFixtureTests.NUnit_Xml_Is_The_Same_As_A_Serial_Run;
var
  serial : string;
  parallel : string;
begin
  ResetProbes(0);
  serial := LoggedXml(BuildProbes(PROBE_FIXTURES, ['Passes', 'Fails', 'Errors', 'IgnoresAtRunTime', 'LogsItsOwnName'], true), 0, false);
  ResetProbes(0);
  parallel := LoggedXml(BuildProbes(PROBE_FIXTURES, ['Passes', 'Fails', 'Errors', 'IgnoresAtRunTime', 'LogsItsOwnName'], true), 4, false);
  Assert.AreEqual(6 * PROBE_FIXTURES, CountOf('<test-case ', serial), 'the serial run logs every test');
  Assert.AreEqual(serial, parallel, false, 'a parallel run logs the same NUnit XML as a serial run');
end;

procedure TParallelFixtureTests.JUnit_Xml_Is_The_Same_As_A_Serial_Run;
var
  serial : string;
  parallel : string;
begin
  ResetProbes(0);
  serial := LoggedXml(BuildProbes(PROBE_FIXTURES, ['Passes', 'Fails', 'Errors', 'IgnoresAtRunTime', 'LogsItsOwnName'], true), 0, true);
  ResetProbes(0);
  parallel := LoggedXml(BuildProbes(PROBE_FIXTURES, ['Passes', 'Fails', 'Errors', 'IgnoresAtRunTime', 'LogsItsOwnName'], true), 4, true);
  Assert.AreEqual(6 * PROBE_FIXTURES, CountOf('<testcase ', serial), 'the serial run logs every test');
  Assert.AreEqual(serial, parallel, false, 'a parallel run logs the same JUnit XML as a serial run');
end;

//With MaxThreads 0 or 1, every test runs on the runner's thread, as before parallel fixtures.
procedure CheckRunsOnTheRunnerThread(const AMaxThreads : integer);
var
  run : TProbeRun;
  threadId : TThreadID;
begin
  ResetProbes(0);
  run := RunProbes(BuildProbes(PROBE_FIXTURES, ['Passes', 'LogsItsOwnName']), AMaxThreads, false, nil);
  Assert.AreEqual(2 * PROBE_FIXTURES, run.Results.PassCount);
  Assert.AreEqual(2 * PROBE_FIXTURES, probeThreads.Count);
  for threadId in probeThreads.Values do
    Assert.AreEqual(run.RunnerThread, threadId, 'every test ran on the runner''s thread');
end;

procedure TParallelFixtureTests.Parallelism_0_Runs_On_The_Runner_Thread;
begin
  CheckRunsOnTheRunnerThread(0);
end;

procedure TParallelFixtureTests.Parallelism_1_Runs_On_The_Runner_Thread;
begin
  CheckRunsOnTheRunnerThread(1);
end;

initialization
  probeLock := TObject.Create;
  probeSetups := TDictionary<string, integer>.Create;
  probeTearDowns := TDictionary<string, integer>.Create;
  probeThreads := TDictionary<string, TThreadID>.Create;
  TDUnitX.RegisterTestFixture(TParallelFixtureTests);

finalization
  probeGate.Free;
  probeThreads.Free;
  probeTearDowns.Free;
  probeSetups.Free;
  probeLock.Free;

end.
