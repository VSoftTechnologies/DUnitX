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

unit DUnitX.TestRunner;

interface

{$I DUnitX.inc}

uses
  System.Classes,
  System.SysUtils,
  System.Rtti,
  System.Generics.Collections,
  DUnitX.TestFramework,
  DUnitX.Extensibility,
  DUnitX.InternalInterfaces,
  DUnitX.Generics,
  DUnitX.WeakReference,
  DUnitX.Filters,
  DUnitX.Exceptions;

type
  ///  What the runner keeps for the test that a thread is running.  When fixtures run in parallel, each fixture has its
  ///  own, so that tests on different threads never share a log.
  TDUnitXTestThreadState = class
  public
    LogMessages : TStringList;
    LogMessagesEx : TLogMessageArray;
    CurrentTestName : string;
    //The results recorded while a fixture runs in parallel, in the order they were recorded; nil otherwise.
    Results : TList<ITestResult>;
    constructor Create(const ACollectResults : boolean);
    destructor Destroy; override;
  end;

  ///  Note - we rely on the fact that there will only ever be 1 testrunner
  ///  per thread, if this changes then handling of WriteLn will need to change
  TDUnitXTestRunner = class(TWeakReferencedObject, ITestRunner)
  private
    class var
      FRttiContext : TRttiContext;
  public
    class var
      FActiveRunners : TDictionary<TThreadID, IWeakReference<ITestRunner>>;
  private
    FLoggers : TList<ITestLogger>;
    FUseRTTI : boolean;
    FFixtureClasses : TDictionary<TClass, string>;

    FFixtureList : ITestFixtureList;

    FFailsOnNoAsserts : boolean;
    //The results of failed fixture teardowns hold their tests weakly, so the runner keeps the tests.
    FTearDownFixtureTests : TList<ITest>;

    FUseThreadPool : boolean;
    FMaxThreads : integer;
    //True while fixtures are running in parallel.
    FRunningInParallel : boolean;
    //While fixtures run in parallel, the fixtures to run (a TList<TParallelFixture>) and the index of the last one taken.
    FParallelFixtures : TObject;
    FLastParallelFixture : integer;
    //The state of the test running on the runner's own thread; see State.
    FMainState : TObject;
    //Serialises the calls to the loggers, so that a logger is never called from two threads at once.
    FLoggersLock : TObject;
    //Serialises recording results, which fixtures running in parallel share.
    FResultsLock : TObject;
  protected
    //The log and test name of the test running on the calling thread.  The runner's own thread uses FMainState; a
    //worker thread uses the state of the fixture it is running.
    function State : TDUnitXTestThreadState;
    procedure CountAndFilterTests(const fixtureList : ITestFixtureList; var count, active : Cardinal);
    //Logger calls - sequence ordered
    procedure Loggers_TestingStarts(const threadId : TThreadID; testCount, testActiveCount : Cardinal);

    procedure Loggers_StartTestFixture(const threadId : TThreadID; const fixture : ITestFixtureInfo);

    procedure Loggers_SetupFixture(const threadId : TThreadID; const fixture : ITestFixtureInfo);
    procedure Loggers_EndSetupFixture(const threadId : TThreadID; const fixture : ITestFixtureInfo);

    procedure Loggers_BeginTest(const threadId : TThreadID; const Test : ITestInfo);

    procedure Loggers_SetupTest(const threadId : TThreadID; const Test : ITestInfo);
    procedure Loggers_EndSetupTest(const threadId : TThreadID; const Test : ITestInfo);

    procedure Loggers_ExecuteTest(const threadId : TThreadID; const Test : ITestInfo);

    procedure Loggers_AddSuccess(const threadId : TThreadID; const Test : ITestResult);
    procedure Loggers_AddError(const threadId : TThreadID; const Error : ITestError);
    procedure Loggers_AddFailure(const threadId : TThreadID; const Failure : ITestError);
    procedure Loggers_AddIgnored(const threadId : TThreadID; const AIgnored : ITestResult);
    procedure Loggers_AddMemoryLeak(const threadId : TThreadID; const Test : ITestResult);

    procedure Loggers_EndTest(const threadId : TThreadID; const Test : ITestResult);
    procedure Loggers_TeardownTest(const threadId : TThreadID; const Test : ITestInfo);
    procedure Loggers_EndTeardownTest(const aThreadId : TThreadID; const aTest : ITestInfo);

    procedure Loggers_TeardownFixture(const threadId : TThreadID; const fixture : ITestFixtureInfo);
    procedure Loggers_EndTearDownFixture(const aThreadId : TThreadID; const aFixture : ITestFixtureInfo);

    procedure Loggers_EndTestFixture(const threadId : TThreadID; const results : IFixtureResult);

    procedure Loggers_TestingEnds(const RunResults : IRunResults);

    //ITestRunner
    procedure AddLogger(const value : ITestLogger);
    function Execute : IRunResults;

    procedure ExecuteFixtures(const parentFixtureResult : IFixtureResult; const context : ITestExecuteContext; const threadId : TThreadID; const fixtures :
      ITestFixtureList);
    procedure ExecuteFixture(const context : ITestExecuteContext; const threadId : TThreadID; const fixture : ITestFixture; const fixtureResult :
      IFixtureResult);
{$IFDEF PARALLEL_FIXTURES}
    procedure ExecuteFixturesInParallel(const context : ITestExecuteContext; const threadId : TThreadID; const fixtures : ITestFixtureList);
    function ParallelWorker(const context : ITestExecuteContext; const runnerRef : IWeakReference<ITestRunner>) : TProc;
    procedure ExecuteParallelFixtures(const context : ITestExecuteContext; const runnerRef : IWeakReference<ITestRunner>);
{$ENDIF}
    function ExecuteSetupFixtureMethod(const context : ITestExecuteContext; const threadId : TThreadID; const fixture : ITestFixture; const fixtureResult :
      IFixtureResult) : boolean;
    function ExecuteTestSetupMethod(const context : ITestExecuteContext; const threadId : TThreadID; const fixture : ITestFixture; const test : ITest; out
      errorResult : ITestResult; const memoryAllocationProvider : IMemoryLeakMonitor) : boolean;

    procedure ExecuteTests(const context : ITestExecuteContext; const threadId : TThreadID; const fixture : ITestFixture; const fixtureResult :
      IFixtureResult);
    //Starts the log that the next result carries.
    procedure ClearLogMessages;

    function ExecuteTest(const context : ITestExecuteContext; const threadId : TThreadID; const test : ITest; const memoryAllocationProvider :
      IMemoryLeakMonitor) : ITestResult;
    function ExecuteSuccessfulResult(const context : ITestExecuteContext; const threadId : TThreadID; const test : ITest; const aMessage : string; const
      aLogMessages : TLogMessageArray) : ITestResult;
    function ExecuteFailureResult(const context : ITestExecuteContext; const threadId : TThreadID; const test : ITest; const exception : Exception; const
      aLogMessages : TLogMessageArray) : ITestError;
    function ExecuteTimedOutResult(const context : ITestExecuteContext; const threadId : TThreadID; const test : ITest; const exception : Exception; const
      aLogMessages : TLogMessageArray) : ITestError;
    function ExecuteErrorResult(const context : ITestExecuteContext; const threadId : TThreadID; const test : ITest; const exception : Exception; const
      aLogMessages : TLogMessageArray) : ITestError;
    function ExecuteIgnoredResult(const context : ITestExecuteContext; const threadId : TThreadID; const test : ITest; const ignoreReason : string) :
      ITestResult;
    procedure InvalidateTestsInFixture(const aFixture : ITestFixture; const aContext : ITestExecuteContext; const aThreadId : TThreadID; const aException :
      Exception; const aFixtureResult : IFixtureResult);

    function CheckMemoryAllocations(const test : ITest; out errorResult : ITestResult; const memoryAllocationProvider : IMemoryLeakMonitor) : boolean;

    function ExecuteTestTearDown(const context : ITestExecuteContext; const threadId : TThreadID; const fixture : ITestFixture; const test : ITest; out
      errorResult : ITestResult; const memoryAllocationProvider : IMemoryLeakMonitor) : boolean;
    procedure ExecuteTearDownFixtureMethod(const context : ITestExecuteContext; const threadId : TThreadID; const fixture : ITestFixture; const fixtureResult :
      IFixtureResult);

    procedure RecordResult(const context : ITestExecuteContext; const threadId : TThreadID; const fixtureResult : IFixtureResult; const testResult :
      ITestResult);
    procedure RecordResultInContext(const context : ITestExecuteContext; const fixtureResult : IFixtureResult; const testResult : ITestResult);

    function GetUseRTTI : Boolean;
    procedure SetUseRTTI(const value : Boolean);
    function GetFailsOnNoAsserts : boolean;
    procedure SetFailsOnNoAsserts(const value : boolean);

    function GetUseThreadPool : boolean;
    procedure SetUseThreadPool(value : boolean);

    function GetMaxThreads : integer;
    procedure SetMaxThreads(value : integer);

    procedure Log(const logType : TLogLevel; const msg : string); overload;
    procedure Log(const msg : string); overload;

    //for backwards compatibility with DUnit tests.
    procedure Status(const msg : string); overload;

    //redirects WriteLn to our loggers.
    procedure WriteLn(const msg : string); overload;
    procedure WriteLn; overload;
    function CurrentTestName : string;

    //internals
    procedure RTTIDiscoverFixtureClasses;
    function BuildFixtures : IInterface;


    function ShouldRunThisTest(const test : ITest) : boolean;

    class constructor Create;
    class destructor Destroy;
  public
    constructor Create; overload;
    constructor Create(const AListener : ITestLogger); overload;
    constructor Create(const AListeners : array of ITestLogger); overload;
    destructor Destroy; override;
    class function GetActiveRunner : ITestRunner;
    class function GetCurrentTestName : string;
  end;

implementation

uses
  System.TypInfo,
  System.StrUtils,
  System.Types,
  System.SyncObjs,
  DUnitX.Attributes,
  DUnitX.CommandLine.Options,
  DUnitX.TestFixture,
  DUnitX.RunResults,
  DUnitX.TestResult,
  DUnitX.FixtureResult,
  DUnitX.Utils,
  DUnitX.ServiceLocator,
  DUnitX.ResStrs,
  DUnitX.FixtureBuilder,
  DUnitX.Test;

threadvar
  //The runner whose fixture this worker thread is running, and that fixture's state (see TDUnitXTestRunner.State).
  WorkerThreadRunner : Pointer;
  WorkerThreadState : Pointer;

{ TDUnitXTestThreadState }

constructor TDUnitXTestThreadState.Create(const ACollectResults : boolean);
begin
  inherited Create;
  LogMessages := TStringList.Create;
  if ACollectResults then
    Results := TList<ITestResult>.Create;
end;

destructor TDUnitXTestThreadState.Destroy;
begin
  Results.Free;
  LogMessages.Free;
  inherited;
end;

{ TDUnitXTestRunner }

procedure TDUnitXTestRunner.Log(const msg : string);
begin
  Self.Log(TLogLevel.Information, msg);
end;

procedure TDUnitXTestRunner.Loggers_AddError(const threadId : TThreadID; const Error : ITestError);
var
  logger : ITestLogger;
begin
  MonitorEnter(FLoggersLock);
  try
    for logger in FLoggers do
    begin
      logger.OnTestError(threadId, Error);
    end;
  finally
    MonitorExit(FLoggersLock);
  end;
end;

procedure TDUnitXTestRunner.Loggers_AddFailure(const threadId : TThreadID; const Failure : ITestError);
var
  logger : ITestLogger;
begin
  MonitorEnter(FLoggersLock);
  try
    for logger in FLoggers do
    begin
      logger.OnTestFailure(threadId, Failure);
    end;
  finally
    MonitorExit(FLoggersLock);
  end;
end;

procedure TDUnitXTestRunner.Loggers_AddIgnored(const threadId : TThreadID; const AIgnored : ITestResult);
var
  logger : ITestLogger;
begin
  MonitorEnter(FLoggersLock);
  try
    for logger in FLoggers do
    begin
      logger.OnTestIgnored(threadId, AIgnored);
    end;
  finally
    MonitorExit(FLoggersLock);
  end;
end;

procedure TDUnitXTestRunner.Loggers_AddMemoryLeak(const threadId : TThreadID; const Test : ITestResult);
var
  logger : ITestLogger;
begin
  MonitorEnter(FLoggersLock);
  try
    for logger in FLoggers do
    begin
      logger.OnTestMemoryLeak(threadId, Test);
    end;
  finally
    MonitorExit(FLoggersLock);
  end;
end;

procedure TDUnitXTestRunner.AddLogger(const value : ITestLogger);
begin
  if not FLoggers.Contains(value) then
    FLoggers.Add(value);
end;

procedure TDUnitXTestRunner.Loggers_AddSuccess(const threadId : TThreadID; const Test : ITestResult);
var
  logger : ITestLogger;
begin
  MonitorEnter(FLoggersLock);
  try
    for logger in FLoggers do
    begin
      logger.OnTestSuccess(threadId, Test);
    end;
  finally
    MonitorExit(FLoggersLock);
  end;
end;

function TDUnitXTestRunner.BuildFixtures : IInterface;
var
  fixtureBuilder : IFixtureBuilder;
begin
  fixtureBuilder := TDUnitXFixtureBuilder.Create(FUseRTTI);

  FFixtureList := fixtureBuilder.BuildFixtureList;
  FFixtureList.Sort;

  result := FFixtureList;
end;

class constructor TDUnitXTestRunner.Create;
begin
  FRttiContext := TRttiContext.Create;
  FActiveRunners := TDictionary < TThreadID, IWeakReference<ITestRunner> > .Create;
end;

function TDUnitXTestRunner.CheckMemoryAllocations(const test : ITest; out errorResult : ITestResult; const memoryAllocationProvider : IMemoryLeakMonitor) :
  boolean;
var
  LSetUpMemoryAllocated : Int64;
  LTearDownMemoryAllocated : Int64;
  LTestMemoryAllocated : Int64;
  LMsg : string;
  LMemoryAllocationProvider2 : IMemoryLeakMonitor2;
  testInfo : ITestInfo;
begin
  Result := True;
  errorResult := nil;

  if (test.IgnoreMemoryLeaks) then
    Exit;

  LSetUpMemoryAllocated := memoryAllocationProvider.SetUpMemoryAllocated;
  LTestMemoryAllocated := memoryAllocationProvider.TestMemoryAllocated;
  LTearDownMemoryAllocated := memoryAllocationProvider.TearDownMemoryAllocated;

  //Only net growth is a leak: a test may free memory that was allocated before it ran.
  if (LSetUpMemoryAllocated + LTestMemoryAllocated + LTearDownMemoryAllocated <= 0) then
    Exit(True);

  if memoryAllocationProvider.QueryInterface(IMemoryLeakMonitor2, LMemoryAllocationProvider2) = 0 then
    LMsg := LMemoryAllocationProvider2.GetReport;

  testInfo := test as ITestInfo;
  if (LTestMemoryAllocated = 0) then
  begin
    // The leak occurred in the setup/teardown
    Result := False;
    LMsg := Format(SSetupTeardownBytesLeaked, [LSetUpMemoryAllocated + LTearDownMemoryAllocated]) + LMsg;

    errorResult := TDUnitXTestResult.Create(testInfo, TTestResultType.MemoryLeak, LMsg);
  end
  else if (LSetUpMemoryAllocated + LTearDownMemoryAllocated = 0) then
  begin
    // The leak occurred in the test only
    Result := False;
    LMsg := Format(STestBytesLeaked, [LTestMemoryAllocated]) + LMsg;
    errorResult := TDUnitXTestResult.Create(testInfo, TTestResultType.MemoryLeak, LMsg);
  end
  else
  begin
    // The leak occurred in the setup/teardown/test
    Result := False;
    LMsg := Format(SSetupTestTeardownBytesLeaked, [LSetUpMemoryAllocated + LTestMemoryAllocated + LTearDownMemoryAllocated]) + LMsg;
    errorResult := TDUnitXTestResult.Create(testInfo, TTestResultType.MemoryLeak, LMsg);
  end;
end;

procedure TDUnitXTestRunner.ClearLogMessages;
var
  threadState : TDUnitXTestThreadState;
begin
  threadState := State;
  threadState.LogMessages.Clear;
  SetLength(threadState.LogMessagesEx, 0);
end;

constructor TDUnitXTestRunner.Create;
begin
  inherited;
  FLoggers := TList<ITestLogger>.Create;
  FFixtureClasses := TDictionary<TClass, string>.Create;
  FUseRTTI := False;
  FMainState := TDUnitXTestThreadState.Create(false);
  FLoggersLock := TObject.Create;
  FResultsLock := TObject.Create;
  FTearDownFixtureTests := TList<ITest>.Create;
  //Defaulting to using the options here to make working with existing code/testinsight easier.
  FMaxThreads := TDUnitX.Options.MaxDegreeOfParallelism;
  FUseThreadPool := FMaxThreads > 1;
  MonitorEnter(TDUnitXTestRunner.FActiveRunners);
  try
    TDUnitXTestRunner.FActiveRunners.Add(TThread.CurrentThread.ThreadID, TWeakReference<ITestRunner>.Create(Self));
  finally
    MonitorExit(TDUnitXTestRunner.FActiveRunners);
  end;
end;

constructor TDUnitXTestRunner.Create(const AListener : ITestLogger);
begin
  Create;

  if AListener <> nil then
    FLoggers.Add(AListener);
end;

constructor TDUnitXTestRunner.Create(const AListeners : array of ITestLogger);
begin
  Create;

  FLoggers.AddRange(AListeners);
end;

function TDUnitXTestRunner.CurrentTestName : string;
begin
  result := State.CurrentTestName;
end;

destructor TDUnitXTestRunner.Destroy;
var
  tId : TThreadID;
begin
  MonitorEnter(TDUnitXTestRunner.FActiveRunners);
  try
    tId := TThread.CurrentThread.ThreadID;
    if TDUnitXTestRunner.FActiveRunners.ContainsKey(tId) then
      TDUnitXTestRunner.FActiveRunners.Remove(tId);
  finally
    MonitorExit(TDUnitXTestRunner.FActiveRunners);
  end;

  FTearDownFixtureTests.Free;
  FResultsLock.Free;
  FLoggersLock.Free;
  FMainState.Free;
  FLoggers.Free;
  FFixtureClasses.Free;
  inherited;
end;

class destructor TDUnitXTestRunner.Destroy;
begin
  FActiveRunners.Free;
  FRttiContext.Free;
end;

procedure TDUnitXTestRunner.RecordResult(const context : ITestExecuteContext; const threadId : TThreadID; const fixtureResult : IFixtureResult; const testResult
  : ITestResult);
begin
  case testResult.ResultType of
    TTestResultType.Pass :
      begin
        RecordResultInContext(context, fixtureResult, testResult);
        Self.Loggers_AddSuccess(threadId, testResult);
      end;
    TTestResultType.Failure :
      begin
        Log(TLogLevel.Error, STestFailed + testResult.Test.Name + ' : ' + testResult.Message);
        RecordResultInContext(context, fixtureResult, testResult);
        Self.Loggers_AddFailure(threadId, ITestError(testResult));
      end;
    TTestResultType.Error :
      begin
        Log(TLogLevel.Error, STestError + testResult.Test.Name + ' : ' + testResult.Message);
        RecordResultInContext(context, fixtureResult, testResult);
        Self.Loggers_AddError(threadId, ITestError(testResult));
      end;
    TTestResultType.Ignored :
      begin
        Log(TLogLevel.Error, STestIgnored + testResult.Test.Name + ' : ' + testResult.Message);
        RecordResultInContext(context, fixtureResult, testResult);
        Self.Loggers_AddIgnored(threadId, testResult);
      end;
    TTestResultType.MemoryLeak :
      begin
        Log(TLogLevel.Error, STestLeaked + testResult.Test.Name + ' : ' + testResult.Message);
        RecordResultInContext(context, fixtureResult, testResult);
        Self.Loggers_AddMemoryLeak(threadId, testResult);
      end;

  end;
end;

procedure TDUnitXTestRunner.RecordResultInContext(const context : ITestExecuteContext; const fixtureResult : IFixtureResult; const testResult :
  ITestResult);
var
  results : TList<ITestResult>;
begin
  MonitorEnter(FResultsLock);
  try
    context.RecordResult(fixtureResult, testResult);
  finally
    MonitorExit(FResultsLock);
  end;
  results := State.Results;
  if results <> nil then
    results.Add(testResult);
end;

procedure TDUnitXTestRunner.RTTIDiscoverFixtureClasses;
var
  types : TArray<TRttiType>;
  rType : TRttiType;
  attributes : TArray<TCustomAttribute>;
  attribute : TCustomAttribute;
  sName : string;
begin
  types := FRttiContext.GetTypes;
  for rType in types do
  begin
    //try and keep the iteration down as much as possible
    if (rType.TypeKind = TTypeKind.tkClass) and (not rType.InheritsFrom(TPersistent)) then
    begin
      attributes := rType.GetAttributes;
      if Length(attributes) > 0 then
        for attribute in attributes do
        begin
          if attribute.ClassType = TestFixtureAttribute then
          begin
            sName := TestFixtureAttribute(attribute).Name;
            if sName = '' then
              sName := TRttiInstanceType(rType).MetaclassType.ClassName;
            if not FFixtureClasses.ContainsKey(TRttiInstanceType(rType).MetaclassType) then
              FFixtureClasses.Add(TRttiInstanceType(rType).MetaclassType, sName);
          end;
        end;
    end;
  end;
end;

procedure TDUnitXTestRunner.Loggers_EndSetupFixture(const threadId : TThreadID; const fixture : ITestFixtureInfo);
var
  logger : ITestLogger;
begin
  MonitorEnter(FLoggersLock);
  try
    for logger in FLoggers do
      logger.OnEndSetupFixture(threadId, fixture);
  finally
    MonitorExit(FLoggersLock);
  end;
end;

procedure TDUnitXTestRunner.Loggers_EndSetupTest(const threadId : TThreadID; const Test : ITestInfo);
var
  logger : ITestLogger;
begin
  MonitorEnter(FLoggersLock);
  try
    for logger in FLoggers do
    begin
      try
        logger.OnEndSetupTest(threadid, Test);
      except
        //Hmmmm what to do with errors here. This kinda smells.
        on e : Exception do
        begin
          try
            logger.OnLog(TLogLevel.Error, SOnEndSetupEventError + e.Message);
          except
            on e : Exception do
              System.Write(SOnEndSetupTestLogError + e.Message);
          end;
        end;
      end;
    end;
  finally
    MonitorExit(FLoggersLock);
  end;
end;

procedure TDUnitXTestRunner.Loggers_EndTearDownFixture(const aThreadId : TThreadID; const aFixture : ITestFixtureInfo);
var
  lLogger : ITestLogger;
begin
  MonitorEnter(FLoggersLock);
  try
    for lLogger in FLoggers do
      lLogger.OnEndTearDownFixture(aThreadId, aFixture);
  finally
    MonitorExit(FLoggersLock);
  end;
end;

procedure TDUnitXTestRunner.Loggers_EndTeardownTest(const aThreadId : TThreadID; const aTest : ITestInfo);
var
  lLogger : ITestLogger;
begin
  MonitorEnter(FLoggersLock);
  try
    for lLogger in FLoggers do
      lLogger.OnEndTeardownTest(aThreadId, aTest);
  finally
    MonitorExit(FLoggersLock);
  end;
end;

procedure TDUnitXTestRunner.Loggers_EndTest(const threadId : TThreadID; const Test : ITestResult);
var
  logger : ITestLogger;
begin
  MonitorEnter(FLoggersLock);
  try
    for logger in FLoggers do
      logger.OnEndTest(threadId, Test);
  finally
    MonitorExit(FLoggersLock);
  end;
end;

procedure TDUnitXTestRunner.Loggers_EndTestFixture(const threadId : TThreadID; const results : IFixtureResult);
var
  logger : ITestLogger;
begin
  MonitorEnter(FLoggersLock);
  try
    for logger in FLoggers do
    begin
      logger.OnEndTestFixture(threadId, results);
    end;
  finally
    MonitorExit(FLoggersLock);
  end;
end;

procedure TDUnitXTestRunner.Loggers_ExecuteTest(const threadId : TThreadID; const Test : ITestInfo);
var
  logger : ITestLogger;
begin
  MonitorEnter(FLoggersLock);
  try
    for logger in FLoggers do
      logger.OnExecuteTest(threadId, Test);
  finally
    MonitorExit(FLoggersLock);
  end;
end;

procedure TDUnitXTestRunner.CountAndFilterTests(const fixtureList : ITestFixtureList; var count : Cardinal; var active : Cardinal);
var
  fixture : ITestFixture;
  test : ITest;
begin
  for fixture in fixtureList do
  begin
    for test in fixture.Tests do
    begin
      if TDUnitX.Filter <> nil then
        test.Enabled := TDUnitX.Filter.Match(test);

      if test.Enabled then
      begin
        Inc(count);
        if not test.Ignored then
          Inc(active);
      end;
    end;
    if fixture.HasChildFixtures then
      CountAndFilterTests(fixture.children, count, active);

    //    if not (fixture.HasTests or fixture.HasChildTests) then
    //      fixture.Enabled := false;
  end;
end;

//TODO - this needs to be thread aware so we can run tests in threads.
function TDUnitXTestRunner.Execute : IRunResults;
var
  fixtureList : ITestFixtureList;
  context : ITestExecuteContext;
  threadId : TThreadID;
  testCount : Cardinal;
  testActiveCount : Cardinal;
  fixtures : IInterface;
begin
  result := nil;

  if FFixtureList = nil then
  begin
    fixtures := BuildFixtures;
    fixtureList := fixtures as ITestFixtureList;
  end
  else
    fixtureList := FFixtureList;
  if fixtureList.Count = 0 then
    raise ENoTestsRegistered.Create(SNoFixturesFound);

  testCount := 0;
  //TODO: Count the active tests that we have.
  testActiveCount := 0;

  //TODO : Filter tests here?

  CountAndFilterTests(fixtureList, testCount, testActiveCount);

  //TODO: Move to the fixtures class
  result := TDUnitXRunResults.Create;
  context := result as ITestExecuteContext;

  //TODO: Record Test metrics.. runtime etc.
  threadId := TThread.CurrentThread.ThreadID;
  Self.Loggers_TestingStarts(threadId, testCount, testActiveCount);
  try
    ExecuteFixtures(nil, context, threadId, fixtureList);
    context.RollupResults;              //still need this for overall time.
  finally
    Self.Loggers_TestingEnds(result);
  end;
end;

function TDUnitXTestRunner.ExecuteErrorResult(const context : ITestExecuteContext; const threadId : TThreadID; const test : ITest; const exception : Exception;
  const aLogMessages : TLogMessageArray) : ITestError;
var
  testInfo : ITestInfo;
begin
  testInfo := test as ITestInfo;
  Result := TDUnitXTestError.Create(testInfo, TTestResultType.Error, exception, ExceptAddr, exception.Message, aLogMessages);
end;

class function TDUnitXTestRunner.GetActiveRunner : ITestRunner;
var
  ref : IWeakReference<ITestRunner>;
begin
  result := nil;
  MonitorEnter(FActiveRunners);
  try
    if not FActiveRunners.TryGetValue(TThread.CurrentThread.ThreadId, ref) then
      exit;
  finally
    MonitorExit(FActiveRunners);
  end;
  result := ref.Data;
end;

class function TDUnitXTestRunner.GetCurrentTestName : string;
var
  runner : ITestRunner;
begin
  runner := GetActiveRunner;
  if runner <> nil then
    result := runner.CurrentTestName;
end;

function TDUnitXTestRunner.GetFailsOnNoAsserts : boolean;
begin
  Result := FFailsOnNoAsserts;
end;

function TDUnitXTestRunner.ExecuteFailureResult(const context : ITestExecuteContext; const threadId : TThreadID; const test : ITest; const exception :
  Exception; const aLogMessages : TLogMessageArray) : ITestError;
var
  testInfo : ITestInfo;
begin
  testInfo := test as ITestInfo;
  //TODO: Does test failure require its own results interface and class?
  Result := TDUnitXTestError.Create(testInfo, TTestResultType.Failure, exception, ExceptAddr, exception.Message, aLogMessages);
end;

procedure TDUnitXTestRunner.ExecuteFixtures(const parentFixtureResult : IFixtureResult; const context : ITestExecuteContext; const threadId : TThreadID; const
  fixtures : ITestFixtureList);
var
  fixture : ITestFixture;
  fixtureResult : IFixtureResult;
begin
{$IFDEF PARALLEL_FIXTURES}
  //Fixtures run in parallel from the top of the tree; a fixture's own children then run on its thread.
  if (parentFixtureResult = nil) and FUseThreadPool and (FMaxThreads > 1) and (not FRunningInParallel) then
  begin
    ExecuteFixturesInParallel(context, threadId, fixtures);
    exit;
  end;
{$ENDIF}
  for fixture in fixtures do
  begin
    if not fixture.Enabled then
      System.continue;

    if (not fixture.HasTests) and (not fixture.HasChildTests) then
      System.Continue;

    fixtureResult := TDUnitXFixtureResult.Create(parentFixtureResult, fixture as ITestFixtureInfo);
    if parentFixtureResult = nil then
      context.RecordFixture(fixtureResult);

    ExecuteFixture(context, threadId, fixture, fixtureResult);
  end;
end;

procedure TDUnitXTestRunner.ExecuteFixture(const context : ITestExecuteContext; const threadId : TThreadID; const fixture : ITestFixture; const
  fixtureResult : IFixtureResult);
var
  setupOk : boolean;
begin
  setupOk := True;
  Self.Loggers_StartTestFixture(threadId, fixture as ITestFixtureInfo);
  try
    //Initialize the fixture as it may have been destroyed in a previous run (when using gui runner).
    if fixture.HasTests then
      fixture.InitFixtureInstance;
    //only run the setup method if there are actually tests
    if fixture.HasActiveTests and Assigned(fixture.SetupFixtureMethod) then
      //returns false if an exception happens in the setup.
      setupOk := ExecuteSetupFixtureMethod(context, threadId, fixture, fixtureResult);

    if setupOk and fixture.HasTests then
      ExecuteTests(context, threadId, fixture, fixtureResult);

    if fixture.HasChildFixtures then
      ExecuteFixtures(fixtureResult, context, threadId, fixture.Children);

    // teardown will not run if the fixture setup errored.
    if setupOk and fixture.HasActiveTests and Assigned(fixture.TearDownFixtureMethod) then
      //TODO: Tricker yet each test above us requires errors that occur here
      ExecuteTearDownFixtureMethod(context, threadId, fixture, fixtureResult);

    //rollup durations - needs to be done here to ensure correct timing.
    (fixtureResult as IFixtureResultBuilder).RollUpResults;
  finally
    Self.Loggers_EndTestFixture(threadId, fixtureResult);
  end;
end;

{$IFDEF PARALLEL_FIXTURES}

type
  TParallelFixture = record
    Fixture : ITestFixture;
    FixtureResult : IFixtureResult;
    State : TDUnitXTestThreadState;
  end;

//Walks the fixtures as ExecuteFixtures does, creating their results in the same order.  A fixture with no tests of its
//own and no fixture setup or teardown to run, such as a namespace, goes to AContainers and its children are walked in turn;
//every other fixture goes to AUnits, and later runs on a worker thread with its children, as ExecuteFixture runs it.
procedure CollectParallelFixtures(const ARunner : TDUnitXTestRunner; const AParentResult : IFixtureResult; const AContext : ITestExecuteContext;
  const AThreadId : TThreadID; const AFixtures : ITestFixtureList; const AContainers, AUnits : TList<TParallelFixture>);
var
  fixture : ITestFixture;
  item : TParallelFixture;
begin
  for fixture in AFixtures do
  begin
    if not fixture.Enabled then
      System.Continue;

    if (not fixture.HasTests) and (not fixture.HasChildTests) then
      System.Continue;

    item.Fixture := fixture;
    item.FixtureResult := TDUnitXFixtureResult.Create(AParentResult, fixture as ITestFixtureInfo);
    item.State := nil;
    if AParentResult = nil then
      AContext.RecordFixture(item.FixtureResult);

    if (not fixture.HasTests) and fixture.HasChildFixtures and
      not (fixture.HasActiveTests and (Assigned(fixture.SetupFixtureMethod) or Assigned(fixture.TearDownFixtureMethod))) then
    begin
      ARunner.Loggers_StartTestFixture(AThreadId, fixture as ITestFixtureInfo);
      AContainers.Add(item);
      CollectParallelFixtures(ARunner, item.FixtureResult, AContext, AThreadId, fixture.Children, AContainers, AUnits);
    end
    else
    begin
      item.State := TDUnitXTestThreadState.Create(true);
      AUnits.Add(item);
    end;
  end;
end;

procedure TDUnitXTestRunner.ExecuteFixturesInParallel(const context : ITestExecuteContext; const threadId : TThreadID; const fixtures : ITestFixtureList);
var
  containers : TList<TParallelFixture>;
  units : TList<TParallelFixture>;
  item : TParallelFixture;
  i : integer;
  workers : TList<TThread>;
  worker : TThread;
  runnerRef : IWeakReference<ITestRunner>;
  orderedResults : TList<ITestResult>;
  workerError : string;
begin
  workerError := '';
  containers := TList<TParallelFixture>.Create;
  units := TList<TParallelFixture>.Create;
  try
    try
      CollectParallelFixtures(Self, nil, context, threadId, fixtures, containers, units);
      //no point in threads for one fixture.
      if units.Count = 1 then
        ExecuteFixture(context, threadId, units[0].Fixture, units[0].FixtureResult)
      else if units.Count > 1 then
      begin
        //Worker threads register this runner for themselves through one reference, made here on the runner's thread.
        runnerRef := TWeakReference<ITestRunner>.Create(Self);
        FParallelFixtures := units;
        FLastParallelFixture := -1;
        FRunningInParallel := True;
        workers := TList<TThread>.Create;
        try
          //Each worker takes the next fixture not yet taken until none are left, so at most MaxThreads run at once.
          for i := 1 to FMaxThreads do
          begin
            if i > units.Count then
              Break;
            worker := TThread.CreateAnonymousThread(ParallelWorker(context, runnerRef));
            worker.FreeOnTerminate := false;
            workers.Add(worker);
            worker.Start;
          end;
          for worker in workers do
          begin
            worker.WaitFor;
            if (worker.FatalException is Exception) and (workerError = '') then
              workerError := Exception(worker.FatalException).ClassName + ': ' + Exception(worker.FatalException).Message;
          end;
        finally
          for worker in workers do
            worker.Free;
          workers.Free;
          FRunningInParallel := False;
          FParallelFixtures := nil;
        end;

        //The results were recorded as the fixtures finished; list them in the order a serial run records them.
        if (context as TObject) is TDUnitXRunResults then
        begin
          orderedResults := TList<ITestResult>.Create;
          try
            for item in units do
              orderedResults.AddRange(item.State.Results);
            TDUnitXRunResults(context as TObject).ReorderLastResults(orderedResults.ToArray);
          finally
            orderedResults.Free;
          end;
        end;
      end;
    finally
      for i := containers.Count - 1 downto 0 do
      begin
        (containers[i].FixtureResult as IFixtureResultBuilder).RollUpResults;
        Self.Loggers_EndTestFixture(threadId, containers[i].FixtureResult);
      end;
      for item in units do
        item.State.Free;
    end;
  finally
    units.Free;
    containers.Free;
  end;

  //A fixture's own failures are its results; this is an error in the runner or a logger, which stopped a worker.
  if workerError <> '' then
    raise ETestFrameworkException.Create(workerError);
end;

function TDUnitXTestRunner.ParallelWorker(const context : ITestExecuteContext; const runnerRef : IWeakReference<ITestRunner>) : TProc;
begin
  result := procedure
    begin
      ExecuteParallelFixtures(context, runnerRef);
    end;
end;

procedure TDUnitXTestRunner.ExecuteParallelFixtures(const context : ITestExecuteContext; const runnerRef : IWeakReference<ITestRunner>);
var
  units : TList<TParallelFixture>;
  index : integer;
  workerThreadId : TThreadID;
  registered : boolean;
begin
  units := TList<TParallelFixture>(FParallelFixtures);
  workerThreadId := TThread.Current.ThreadID;
  //Register the runner for this thread, so that TDUnitX.CurrentRunner, Log and the assert counters find it.
  MonitorEnter(TDUnitXTestRunner.FActiveRunners);
  try
    registered := not TDUnitXTestRunner.FActiveRunners.ContainsKey(workerThreadId);
    if registered then
      TDUnitXTestRunner.FActiveRunners.Add(workerThreadId, runnerRef);
  finally
    MonitorExit(TDUnitXTestRunner.FActiveRunners);
  end;
  WorkerThreadRunner := Self;
  try
    index := TInterlocked.Increment(FLastParallelFixture);
    while index < units.Count do
    begin
      WorkerThreadState := units[index].State;
      //The result was made when the fixtures were collected; time the fixture from when it starts.
      TDUnitXFixtureResult(units[index].FixtureResult as TObject).RestartTiming;
      ExecuteFixture(context, workerThreadId, units[index].Fixture, units[index].FixtureResult);
      index := TInterlocked.Increment(FLastParallelFixture);
    end;
  finally
    WorkerThreadRunner := nil;
    WorkerThreadState := nil;
    if registered then
    begin
      MonitorEnter(TDUnitXTestRunner.FActiveRunners);
      try
        TDUnitXTestRunner.FActiveRunners.Remove(workerThreadId);
      finally
        MonitorExit(TDUnitXTestRunner.FActiveRunners);
      end;
    end;
  end;
end;

{$ENDIF}

function TDUnitXTestRunner.ExecuteIgnoredResult(const context : ITestExecuteContext; const threadId : TThreadID; const test : ITest; const ignoreReason :
  string) : ITestResult;
begin
  result := TDUnitXTestResult.Create(test as ITestInfo, TTestResultType.Ignored, ignoreReason, State.LogMessagesEx);
end;

function TDUnitXTestRunner.ExecuteSetupFixtureMethod(const context : ITestExecuteContext; const threadId : TThreadID; const fixture : ITestFixture; const
  fixtureResult : IFixtureResult) : boolean;
begin
  ClearLogMessages;
  try
    Self.Loggers_SetupFixture(threadid, fixture as ITestFixtureInfo);
    fixture.SetupFixtureMethod;
    Self.Loggers_EndSetupFixture(threadid, fixture as ITestFixtureInfo);
    result := true;
  except
    on e : Exception do
    begin
      result := false;
      // Log error (or the ignore) into each test
      InvalidateTestsInFixture(fixture, context, threadId, e, fixtureResult);
      //A fixture setup that ignores the fixture (Assert.Ignore) is not an error: each test is reported ignored.
      if not (e is ETestIgnored) then
      begin
        Log(TLogLevel.Error, Format(SFixtureSetupError, [fixture.Name, e.Message]));
        Log(TLogLevel.Error, SSkippingFixture);
      end;

    end;
  end;
end;

function TDUnitXTestRunner.ExecuteSuccessfulResult(const context : ITestExecuteContext; const threadId : TThreadID; const test : ITest; const aMessage :
  string; const aLogMessages : TLogMessageArray) : ITestResult;
var
  testInfo : ITestInfo;
begin
  testInfo := test as ITestInfo;
  Result := TDUnitXTestResult.Create(testInfo, TTestResultType.Pass, aMessage, aLogMessages);
end;

procedure TDUnitXTestRunner.ExecuteTearDownFixtureMethod(const context : ITestExecuteContext; const threadId : TThreadID; const fixture : ITestFixture; const
  fixtureResult : IFixtureResult);
var
  tearDownTest : ITest;
  tearDownResult : ITestResult;
begin
  ClearLogMessages;
  try
    Self.Loggers_TeardownFixture(threadId, fixture as ITestFixtureInfo);
    fixture.ExecuteFixtureTearDown;     //deals with destructors in a way that stops memory leak reporting from reporting bogus leaks.
    Self.Loggers_EndTearDownFixture(threadId, fixture as ITestFixtureInfo);
  except
    on e : Exception do
    begin
      //The fixture's tests have already run and been recorded, so the failure is recorded once, as a result of its own.
      tearDownTest := TDUnitXTest.Create(fixture, fixture.TearDownFixtureMethodName, fixture.TearDownFixtureMethodName, '', fixture.TearDownFixtureMethod,
        true);
      MonitorEnter(FResultsLock);
      try
        FTearDownFixtureTests.Add(tearDownTest);
      finally
        MonitorExit(FResultsLock);
      end;
      Self.Loggers_BeginTest(threadId, tearDownTest as ITestInfo);
      tearDownResult := ExecuteErrorResult(context, threadId, tearDownTest, e, State.LogMessagesEx);
      RecordResult(context, threadId, fixtureResult, tearDownResult);
      Self.Loggers_EndTest(threadId, tearDownResult);
      Log(TLogLevel.Error, Format(SFixtureTeardownError, [fixture.Name, e.Message]));
    end;
  end;
end;

function TDUnitXTestRunner.ExecuteTest(const context : ITestExecuteContext; const threadId : TThreadID; const test : ITest; const memoryAllocationProvider :
  IMemoryLeakMonitor) : ITestResult;
var
  testExecute : ITestExecute;
  assertBeforeCount : Cardinal;
  assertAfterCount : Cardinal;
begin
  if Supports(test, ITestExecute, testExecute) then
  begin
    State.CurrentTestName := test.Name;
    try
      Self.Loggers_ExecuteTest(threadId, test as ITestInfo);
      assertBeforeCount := 0;           //to shut the compiler up;
      if FFailsOnNoAsserts then
        assertBeforeCount := TDUnitX.GetAssertCount(threadId);
      memoryAllocationProvider.PreTest;
      try
        testExecute.Execute(context);
      finally
        memoryAllocationProvider.PostTest;
      end;

      if FFailsOnNoAsserts then
      begin
        assertAfterCount := TDUnitX.GetAssertCount(threadId);
        if (assertBeforeCount = assertAfterCount) then
          raise ENoAssertionsMade.Create(SNoAssertions);
      end;

      Result := ExecuteSuccessfulResult(context, threadId, test, State.LogMessages.Text, State.LogMessagesEx);
      ClearLogMessages;
    finally
      State.CurrentTestName := '';
    end;
  end
  else
  begin
    //This will be handled by the caller as a test error.
    raise Exception.CreateFmt(SITestExecuteNotSupported, [test.Name]);
  end;
end;

procedure TDUnitXTestRunner.ExecuteTests(const context : ITestExecuteContext; const threadId : TThreadID; const fixture : ITestFixture; const fixtureResult :
  IFixtureResult);
var
  tests : IEnumerable<ITest>;
  test : ITest;
  testResult : ITestResult;
  setupResult : ITestResult;
  tearDownResult : ITestResult;
  memoryAllocationResult : ITestResult;
  memoryAllocationProvider : IMemoryLeakMonitor;
begin
  tests := fixture.Tests;
  for test in tests do
  begin

    if not ShouldRunThisTest(test) then
      System.Continue;

    memoryAllocationProvider := TDUnitXServiceLocator.DefaultContainer.Resolve<IMemoryLeakMonitor>();

    //Start a fresh for this test. If we had an exception last execute of the
    //setup or tear down that may have changed this execution. Therefore we try
    //again to see if things have changed. Remember setup, test, tear down can
    //hold state
    setupResult := nil;
    testResult := nil;
    tearDownResult := nil;
    memoryAllocationResult := nil;
    //A result carries what was logged from its test's Setup on, and nothing from the test before it.
    ClearLogMessages;

    Self.Loggers_BeginTest(threadId, test as ITestInfo);

    //If the setup fails then we need to show this as the result.
    if Assigned(fixture.SetupMethod) and (not test.Ignored) then
      if not (ExecuteTestSetupMethod(context, threadId, fixture, test, setupResult, memoryAllocationProvider)) then
        testResult := setupResult;

    try
      try
        if test.Ignored then
          testResult := ExecuteIgnoredResult(context, threadId, test, test.IgnoreReason)
            //If we haven't already failed, then run the test.
        else if testResult = nil then
          testResult := ExecuteTest(context, threadId, test, memoryAllocationProvider);

      except
        //Handle the results which are raised in the test.
        on e : ETestIgnored do
          testResult := ExecuteIgnoredResult(context, threadId, test, e.Message);
        on e : ETestPass do
          testResult := ExecuteSuccessfulResult(context, threadId, test, e.Message, State.LogMessagesEx);
        on e : ETestFailure do
          testResult := ExecuteFailureResult(context, threadId, test, e, State.LogMessagesEx);
        on e : ETimedOut do
          testResult := ExecuteTimedOutResult(context, threadId, test, e, State.LogMessagesEx);
        on e : Exception do
          testResult := ExecuteErrorResult(context, threadId, test, e, State.LogMessagesEx);
      end;

      //If the tear down fails then we need to show this as the test result, unless the setup or the test has already
      //failed: that failure is the one to diagnose, so it is kept and the tear down error is logged. A test ignored at
      //run time has nothing to diagnose, so the tear down error replaces it, as it does a pass.
      if Assigned(fixture.TearDownMethod) and (not test.Ignored) then
        if not (ExecuteTestTearDown(context, threadId, fixture, test, tearDownResult, memoryAllocationProvider)) then
        begin
          if testResult.ResultType in [TTestResultType.Pass, TTestResultType.Ignored] then
            testResult := tearDownResult
          else
            Log(TLogLevel.Error, Format(STearDownErrorAfterFailure, [test.Name, tearDownResult.Message]));
        end;

      //A leak monitor measures the whole process, so it cannot tell a test's allocations from those of the tests running
      //alongside it: leaks are not checked while fixtures run in parallel.
      if (testResult.ResultType = TTestResultType.Pass) and (not FRunningInParallel) then
        if (not CheckMemoryAllocations(test, memoryAllocationResult, memoryAllocationProvider)) then
          testResult := memoryAllocationResult;
    finally
      RecordResult(context, threadId, fixtureResult, testResult);
      Self.Loggers_EndTest(threadId, testResult);
    end;
  end;
end;

function TDUnitXTestRunner.ExecuteTestSetupMethod(const context : ITestExecuteContext; const threadId : TThreadID; const fixture : ITestFixture; const test :
  ITest; out errorResult : ITestResult; const memoryAllocationProvider : IMemoryLeakMonitor) : boolean;
begin
  Result := False;
  errorResult := nil;

  //Setup method is called before each test method.
  if Assigned(fixture.SetupMethod) then
  begin
    try
      Self.Loggers_SetupTest(threadId, test as ITestInfo);

      memoryAllocationProvider.PreSetup;

      fixture.SetupMethod;

      memoryAllocationProvider.PostSetUp;

      Self.Loggers_EndSetupTest(threadId, test as ITestInfo);
      Result := True;
    except
      //A setup that ignores the test (Assert.Ignore) ignores it, rather than erroring it.
      on e : ETestIgnored do
        errorResult := ExecuteIgnoredResult(context, threadId, test, e.Message);
      on e : System.SysUtils.Exception do
      begin
        errorResult := ExecuteErrorResult(context, threadId, test, e, State.LogMessagesEx);
      end;
    end;
  end;
end;

function TDUnitXTestRunner.ExecuteTestTearDown(const context :
  ITestExecuteContext; const threadId : TThreadID; const fixture : ITestFixture; const test : ITest; out errorResult : ITestResult; const
  memoryAllocationProvider : IMemoryLeakMonitor) : boolean;
begin
  Result := False;
  errorResult := nil;

  try
    Self.Loggers_TeardownTest(threadId, test as ITestInfo);

    memoryAllocationProvider.PreTearDown;

    fixture.TearDownMethod;

    memoryAllocationProvider.PostTearDown;

    Self.Loggers_EndTeardownTest(threadId, test as ITestInfo);
    result := true;
  except
    on e : System.SysUtils.Exception do
    begin
      errorResult := ExecuteErrorResult(context, threadId, test, e, State.LogMessagesEx);
    end;
  end;
end;

function TDUnitXTestRunner.ExecuteTimedOutResult(const context : ITestExecuteContext; const threadId : TThreadID; const test : ITest; const exception :
  Exception; const aLogMessages : TLogMessageArray) : ITestError;
begin
  Result := TDUnitXTestError.Create(test as ITestInfo, TTestResultType.Failure, exception, ExceptAddr, exception.Message, aLogMessages);
end;

function TDUnitXTestRunner.GetUseRTTI : Boolean;
begin
  result := FUseRTTI;
end;

procedure TDUnitXTestRunner.InvalidateTestsInFixture(const aFixture : ITestFixture; const aContext : ITestExecuteContext; const aThreadId : TThreadID; const
  aException : Exception; const aFixtureResult : IFixtureResult);
var
  lTest : ITest;
  lTestResult : ITestResult;
begin
  for lTest in aFixture.Tests do
  begin
    if not ShouldRunThisTest(lTest) then
      Continue;

    Self.Loggers_BeginTest(aThreadId, lTest as ITestInfo);
    if aException is ETestIgnored then
      lTestResult := ExecuteIgnoredResult(aContext, aThreadId, lTest, aException.Message)
    else
      lTestResult := ExecuteErrorResult(aContext, aThreadId, lTest, aException, State.LogMessagesEx);
    RecordResult(aContext, aThreadId, aFixtureResult, lTestResult);
    Self.Loggers_EndTest(aThreadId, lTestResult);
  end;
end;

procedure TDUnitXTestRunner.SetFailsOnNoAsserts(const value : boolean);
begin
  FFailsOnNoAsserts := value;
end;

function TDUnitXTestRunner.State : TDUnitXTestThreadState;
begin
  if (WorkerThreadRunner <> nil) and (WorkerThreadRunner = Pointer(Self)) then
    result := TDUnitXTestThreadState(WorkerThreadState)
  else
    result := TDUnitXTestThreadState(FMainState);
end;

function TDUnitXTestRunner.GetUseThreadPool : boolean;
begin
  result := FUseThreadPool;
end;

procedure TDUnitXTestRunner.SetUseThreadPool(value : boolean);
begin
  FUseThreadPool := value;
end;

function TDUnitXTestRunner.GetMaxThreads : integer;
begin
  result := FMaxThreads;
end;

procedure TDUnitXTestRunner.SetMaxThreads(value : integer);
begin
  FMaxThreads := value;
end;

procedure TDUnitXTestRunner.SetUseRTTI(const value : Boolean);
begin
  FUseRTTI := value;
end;

procedure TDUnitXTestRunner.Status(const msg : string);
begin
  Self.Log(TLogLevel.Information, msg);
end;

procedure TDUnitXTestRunner.WriteLn;
begin
  Self.Log(TLogLevel.Information, '');
end;

procedure TDUnitXTestRunner.WriteLn(const msg : string);
begin
  Self.Log(TLogLevel.Information, msg);
end;

procedure TDUnitXTestRunner.Loggers_SetupFixture(const threadId : TThreadID; const fixture : ITestFixtureInfo);
var
  logger : ITestLogger;
begin
  MonitorEnter(FLoggersLock);
  try
    for logger in FLoggers do
      logger.OnSetupFixture(threadId, fixture);
  finally
    MonitorExit(FLoggersLock);
  end;
end;

procedure TDUnitXTestRunner.Loggers_SetupTest(const threadId : TThreadID; const Test : ITestInfo);
var
  logger : ITestLogger;
begin
  MonitorEnter(FLoggersLock);
  try
    for logger in FLoggers do
      logger.OnSetupTest(threadId, Test);
  finally
    MonitorExit(FLoggersLock);
  end;
end;

procedure TDUnitXTestRunner.Loggers_BeginTest(const threadId : TThreadID; const Test : ITestInfo);
var
  logger : ITestLogger;
begin
  MonitorEnter(FLoggersLock);
  try
    for logger in FLoggers do
      logger.OnBeginTest(threadId, Test);
  finally
    MonitorExit(FLoggersLock);
  end;
end;

procedure TDUnitXTestRunner.Loggers_StartTestFixture(const threadId : TThreadID; const fixture : ITestFixtureInfo);
var
  logger : ITestLogger;
begin
  MonitorEnter(FLoggersLock);
  try
    for logger in FLoggers do
      logger.OnStartTestFixture(threadId, fixture);
  finally
    MonitorExit(FLoggersLock);
  end;
end;

procedure TDUnitXTestRunner.Loggers_TeardownFixture(const threadId : TThreadID; const fixture : ITestFixtureInfo);
var
  logger : ITestLogger;
begin
  MonitorEnter(FLoggersLock);
  try
    for logger in FLoggers do
      logger.OnTearDownFixture(threadId, fixture);
  finally
    MonitorExit(FLoggersLock);
  end;
end;

procedure TDUnitXTestRunner.Loggers_TeardownTest(const threadId : TThreadID; const Test : ITestInfo);
var
  logger : ITestLogger;
begin
  MonitorEnter(FLoggersLock);
  try
    for logger in FLoggers do
      logger.OnTeardownTest(threadId, Test);
  finally
    MonitorExit(FLoggersLock);
  end;
end;

procedure TDUnitXTestRunner.Loggers_TestingEnds(const RunResults : IRunResults);
var
  logger : ITestLogger;
begin
  MonitorEnter(FLoggersLock);
  try
    for logger in FLoggers do
      logger.OnTestingEnds(RunResults);
  finally
    MonitorExit(FLoggersLock);
  end;
end;

procedure TDUnitXTestRunner.Loggers_TestingStarts(const threadId : TThreadID; testCount, testActiveCount : Cardinal);
var
  logger : ITestLogger;
begin
  MonitorEnter(FLoggersLock);
  try
    for logger in FLoggers do
      logger.OnTestingStarts(threadId, testCount, testActiveCount);
  finally
    MonitorExit(FLoggersLock);
  end;
end;

procedure TDUnitXTestRunner.Log(const logType : TLogLevel; const msg : string);
var
  logger : ITestLogger;
  NewIdx : integer;
  threadState : TDUnitXTestThreadState;
begin
  if logType >= TDUnitX.Options.LogLevel then
  begin
    //TODO : Need to get this to the current test result.
    threadState := State;
    threadState.LogMessages.Add(msg);
    NewIdx := Length(threadState.LogMessagesEx);
    SetLength(threadState.LogMessagesEx, NewIdx + 1);
    threadState.LogMessagesEx[NewIdx].Level := logType;
    threadState.LogMessagesEx[NewIdx].Msg := msg;
    MonitorEnter(FLoggersLock);
    try
      for logger in FLoggers do
        logger.OnLog(logType, msg);
    finally
      MonitorExit(FLoggersLock);
    end;
  end;
end;

function TDUnitXTestRunner.ShouldRunThisTest(const test : ITest) : boolean;
begin
  Result := True;

  if not test.Enabled then
    Result := False;

  if test.Ignored and TDUnitX.Options.DontShowIgnored then
    Result := False;

end;

end.

