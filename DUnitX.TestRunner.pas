{***************************************************************************}
{                                                                           }
{           DUnitX                                                          }
{                                                                           }
{           Copyright (C) 2013 Vincent Parrett                              }
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

uses
  DUnitX.TestFramework,
  Generics.Collections,
  DUnitX.InternalInterfaces,
  DUnitX.Generics,
  DUnitX.WeakReference,
  classes,
  SysUtils,
  Rtti;

{$I DUnitX.inc}


type
  ///  Note - we rely on the fact that there will only ever be 1 testrunner
  ///  per thread, if this changes then handling of WriteLn will need to change
  TDUnitXTestRunner = class(TInterfacedObject, ITestRunner)
  private class var
    FRttiContext : TRttiContext;
  public class var
    FActiveRunners : TDictionary<Cardinal,ITestRunner>;
  private
    FLoggers        : TList<ITestLogger>;
    FUseCommandLine : boolean;
    FUseRTTI        : boolean;
    FExitBehavior   : TRunnerExitBehavior;
    FFixtureClasses : TDictionary<TClass,string>;

    FFixtureList    : ITestFixtureList;
    FLogMessages    : TStringList;
  protected
    //Logger calls - sequence ordered
    procedure Loggers_TestingStarts(const threadId, testCount, testActiveCount : Cardinal);

    procedure Loggers_StartTestFixture(const threadId : Cardinal; const fixture : ITestFixtureInfo);

    procedure Loggers_SetupFixture(const threadId : Cardinal; const fixture : ITestFixtureInfo);
    procedure Loggers_EndSetupFixture(const threadId : Cardinal; const fixture : ITestFixtureInfo);

    procedure Loggers_BeginTest(const threadId : Cardinal; const Test: ITestInfo);

    procedure Loggers_SetupTest(const threadId : Cardinal; const Test: ITestInfo);
    procedure Loggers_EndSetupTest(const threadId : Cardinal; const Test: ITestInfo);

    procedure Loggers_ExecuteTest(const threadId : Cardinal; const Test: ITestInfo);

    procedure Loggers_AddSuccess(const threadId : Cardinal; const Test: ITestResult);
    procedure Loggers_AddError(const threadId : Cardinal; const Error: ITestError);
    procedure Loggers_AddFailure(const threadId : Cardinal; const Failure: ITestError);
    procedure Loggers_AddIgnored(const threadId : Cardinal; const AIgnored: ITestResult);
    procedure Loggers_AddMemoryLeak(const threadId: Cardinal; const Test: ITestResult);

    procedure Loggers_EndTest(const threadId : Cardinal; const Test: ITestResult);
    procedure Loggers_TeardownTest(const threadId : Cardinal; const Test: ITestInfo);

    procedure Loggers_TeardownFixture(const threadId : Cardinal; const fixture : ITestFixtureInfo);

    procedure Loggers_EndTestFixture(const threadId : Cardinal; const results : IFixtureResult);

    procedure Loggers_TestingEnds(const RunResults: IRunResults);

    //ITestRunner
    procedure AddLogger(const value: ITestLogger);
    function Execute: IRunResults;

    procedure ExecuteFixtures(const parentFixtureResult : IFixtureResult; const context: ITestExecuteContext; const threadId: Cardinal; const fixtures: ITestFixtureList);
    procedure ExecuteSetupFixtureMethod(const threadid: cardinal; const fixture: ITestFixture);
    function  ExecuteTestSetupMethod(const context : ITestExecuteContext; const threadid: cardinal; const fixture: ITestFixture; const test: ITest; out errorResult: ITestResult; const memoryAllocationProvider : IMemoryLeakMonitor): boolean;

    procedure ExecuteTests(const context : ITestExecuteContext; const threadId: Cardinal; const fixture: ITestFixture; const fixtureResult : IFixtureResult);

    function ExecuteTest(const context: ITestExecuteContext; const threadId: cardinal; const test: ITest; const memoryAllocationProvider : IMemoryLeakMonitor) : ITestResult;
    function ExecuteSuccessfulResult(const context: ITestExecuteContext; const threadId: cardinal; const test: ITest; const TimesRun : Cardinal;const message: string = '') : ITestResult;
    function ExecuteFailureResult(const context: ITestExecuteContext; const threadId: cardinal; const test: ITest;const TimesRun : Cardinal; const exception : Exception) : ITestError;
    function ExecuteErrorResult(const context: ITestExecuteContext; const threadId: cardinal; const test: ITest; const TimesRun : Cardinal;const exception : Exception) : ITestError;
    function ExecuteIgnoredResult(const context: ITestExecuteContext; const threadId: cardinal; const test: ITest; const ignoreReason : string) : ITestResult;

    function CheckMemoryAllocations(const test: ITest; out errorResult: ITestResult; const memoryAllocationProvider : IMemoryLeakMonitor) : boolean;

    function ExecuteTestTearDown(const context: ITestExecuteContext; const threadId: Cardinal; const fixture: ITestFixture; const test: ITest; out errorResult: ITestResult; const memoryAllocationProvider : IMemoryLeakMonitor) : boolean;
    procedure ExecuteTearDownFixtureMethod(const context: ITestExecuteContext; const threadId: Cardinal; const fixture: ITestFixture);

    procedure RecordResult(const context: ITestExecuteContext; const threadId: cardinal; const fixtureResult : IFixtureResult; const testResult: ITestResult);

    function GetExitBehavior: TRunnerExitBehavior;
    function GetUseCommandLineOptions: Boolean;
    function GetUseRTTI: Boolean;
    procedure SetExitBehavior(const value: TRunnerExitBehavior);
    procedure SetUseCommandLineOptions(const value: Boolean);
    procedure SetUseRTTI(const value: Boolean);
    procedure Log(const logType : TLogLevel; const msg : string);overload;
    procedure Log(const msg : string);overload;

    //for backwards compatibilty with DUnit tests.
    procedure Status(const msg : string);overload;

    //redirects WriteLn to our loggers.
    procedure WriteLn(const msg : string);overload;
    procedure WriteLn;overload;

    //internals
    procedure RTTIDiscoverFixtureClasses;
    function BuildFixtures : IInterface;

    procedure AddStatus(const threadId; const msg : string);

    class constructor Create;
    class destructor Destroy;
  public
    constructor Create(const useCommandLineOptions : boolean; const AListener : ITestLogger);
    destructor Destroy;override;
    class function GetActiveRunner : ITestRunner;
  end;

implementation

uses
  DUnitX.TestFixture,
  DUnitX.RunResults,
  DUnitX.TestResult,
  DUnitX.FixtureResult,
  DUnitX.Utils,
  DUnitX.IoC,
  TypInfo,
  StrUtils,
  Types;

{ TDUnitXTestRunner }

procedure TDUnitXTestRunner.Log(const msg: string);
begin
  Self.Log(TLogLevel.ltInformation,msg);
end;

procedure TDUnitXTestRunner.Loggers_AddError(const threadId : Cardinal; const Error: ITestError);
var
  logger : ITestLogger;
begin
  for logger in FLoggers do
  begin
    logger.OnTestError(threadId, Error);
  end;
end;

procedure TDUnitXTestRunner.Loggers_AddFailure(const threadId : Cardinal; const Failure: ITestError);
var
  logger : ITestLogger;
begin
  for logger in FLoggers do
  begin
    logger.OnTestFailure(threadId, Failure);
  end;
end;

procedure TDUnitXTestRunner.Loggers_AddIgnored(const threadId: Cardinal; const AIgnored: ITestResult);
var
  logger : ITestLogger;
begin
  for logger in FLoggers do
  begin
    logger.OnTestIgnored(threadId,AIgnored);
  end;
end;

procedure TDUnitXTestRunner.Loggers_AddMemoryLeak(const threadId: Cardinal; const Test: ITestResult);
var
  logger : ITestLogger;
begin
  for logger in FLoggers do
  begin
    logger.OnTestMemoryLeak(threadId,Test);
  end;
end;

procedure TDUnitXTestRunner.AddLogger(const value: ITestLogger);
begin
  if not FLoggers.Contains(value) then
    FLoggers.Add(value);
end;

procedure TDUnitXTestRunner.Loggers_AddSuccess(const threadId : Cardinal; const Test: ITestResult);
var
  logger : ITestLogger;
begin
  for logger in FLoggers do
  begin
    logger.OnTestSuccess(threadId,Test);
  end;
end;


procedure TDUnitXTestRunner.AddStatus(const threadId; const msg: string);
begin

end;

function TDUnitXTestRunner.BuildFixtures  : IInterface;
var
  fixture : ITestFixture;
  parentFixture : ITestFixture;
  pair : TPair<TClass,string>;
  uName : string;
  namespaces : TStringDynArray;
  namespace : string;
  parentNamespace : string;
  fixtureNamespace : string;
  tmpFixtures : TDictionary<string,ITestFixture>;
begin
  if FFixtureList <> nil then
  begin
    result := FFixtureList;
    exit;
  end;

  FFixtureList := TTestFixtureList.Create;


  if FUseRTTI then
    RTTIDiscoverFixtureClasses;

  for pair in TDUnitX.RegisteredFixtures do
  begin
    if not FFixtureClasses.ContainsValue(pair.Value) then
      FFixtureClasses.AddOrSetValue(pair.Key, pair.Value);
  end;

  //Build up a fixture hierarchy based on unit names.
  tmpFixtures := TDictionary<string,ITestFixture>.Create;
  try
    for pair in FFixtureClasses do
    begin
      uName := pair.Key.UnitName;
      namespaces := SplitString(uName,'.');
      //if the unit name has no namespaces the just add the tests.
      fixtureNamespace := '';
      parentNameSpace := '';

      parentFixture := nil;
      fixture := nil;

      for namespace in namespaces do
      begin
        if fixtureNamespace <> '' then
          fixtureNamespace := fixtureNamespace + '.' + namespace
        else
          fixtureNamespace := namespace;

        //first time through the loop it will be empty.
        if parentNamespace = '' then
          parentNamespace := fixtureNamespace
        else
        begin
          if not tmpFixtures.TryGetValue(parentNamespace,parentFixture) then
          begin
            parentFixture := TDUnitXTestFixture.Create(parentNamespace, TObject);
            FFixtureList.Add(parentFixture);
            tmpFixtures.Add(parentNamespace,parentFixture);
          end;

          if not tmpFixtures.TryGetValue(fixtureNamespace,fixture) then
          begin
            fixture := TDUnitXTestFixture.Create(fixtureNamespace, TObject);
            parentFixture.Children.Add(fixture);
            tmpFixtures.Add(fixtureNamespace,fixture);
          end;

          parentFixture := fixture;
          parentNamespace := fixtureNamespace;
        end;
      end;


      fixtureNamespace := fixtureNamespace + '.' + pair.Value;
      fixture := TDUnitXTestFixture.Create(fixtureNamespace, pair.Key);

      if parentFixture = nil then
        FFixtureList.Add(fixture)
      else
        parentFixture.Children.Add(fixture);
    end;
  finally
    tmpFixtures.Free;
  end;
  result := FFixtureList;
end;

class constructor TDUnitXTestRunner.Create;
begin
  FRttiContext := TRttiContext.Create;
  FActiveRunners := TDictionary<Cardinal,ITestRunner>.Create;
end;

function TDUnitXTestRunner.CheckMemoryAllocations(const test: ITest; out errorResult: ITestResult; const memoryAllocationProvider: IMemoryLeakMonitor): boolean;
var
  LSetUpMemoryAllocated: Int64;
  LTearDownMemoryAllocated: Int64;
  LTestMemoryAllocated: Int64;
begin
  Result := True;
  errorResult := nil;

  if (test.IgnoreMemoryLeaks) then
    Exit;

  LSetUpMemoryAllocated := memoryAllocationProvider.SetUpMemoryAllocated;
  LTestMemoryAllocated := memoryAllocationProvider.TestMemoryAllocated;
  LTearDownMemoryAllocated := memoryAllocationProvider.TearDownMemoryAllocated;

  if (LSetUpMemoryAllocated + LTestMemoryAllocated + LTearDownMemoryAllocated = 0) then
    Exit(True);

  if (LTestMemoryAllocated = 0) then
  begin
    // The leak occurred in the setup/teardown
    Result := False;
    errorResult := TDUnitXTestResult.Create(test as ITestInfo, TTestResultType.MemoryLeak,1, Format('%d bytes were leaked in the setup/teardown methods', [LSetUpMemoryAllocated + LTearDownMemoryAllocated]));
  end
  else if (LSetUpMemoryAllocated + LTearDownMemoryAllocated = 0) then
  begin
    // The leak occurred in the test only
    Result := False;
    errorResult := TDUnitXTestResult.Create(test as ITestInfo, TTestResultType.MemoryLeak,1, Format('%d bytes were leaked in the test method', [LTestMemoryAllocated]));
  end
  else
  begin
    // The leak occurred in the setup/teardown/test
    Result := False;
    errorResult := TDUnitXTestResult.Create(test as ITestInfo, TTestResultType.MemoryLeak,1, Format('%d bytes were leaked in the setup/test/teardown methods', [LSetUpMemoryAllocated + LTestMemoryAllocated + LTearDownMemoryAllocated]));
  end;
end;

constructor TDUnitXTestRunner.Create(const useCommandLineOptions: boolean; const AListener: ITestLogger);
begin
  FLoggers := TList<ITestLogger>.Create;
  if AListener <> nil then
    FLoggers.Add(AListener);
  FFixtureClasses := TDictionary<TClass,string>.Create;
  FUseCommandLine := useCommandLineOptions;
  FUseRTTI := False;
  FLogMessages    := TStringList.Create;
  MonitorEnter(TDUnitXTestRunner.FActiveRunners);
  try
    TDUnitXTestRunner.FActiveRunners.Add(TThread.CurrentThread.ThreadID, Self);
  finally
    MonitorExit(TDUnitXTestRunner.FActiveRunners);
  end;
end;

destructor TDUnitXTestRunner.Destroy;
var
  tId : Cardinal;
begin
  MonitorEnter(TDUnitXTestRunner.FActiveRunners);
  try
    tId := TThread.CurrentThread.ThreadID;
    if TDUnitXTestRunner.FActiveRunners.ContainsKey(tId) then
      TDUnitXTestRunner.FActiveRunners.Remove(tId);
  finally
    MonitorExit(TDUnitXTestRunner.FActiveRunners);
  end;
  FLogMessages.Free;
  FLoggers.Free;
  FFixtureClasses.Free;

  inherited;
end;

class destructor TDUnitXTestRunner.Destroy;
begin
  FActiveRunners.Free;
end;

procedure TDUnitXTestRunner.RecordResult(const context: ITestExecuteContext; const threadId: cardinal; const fixtureResult : IFixtureResult; const testResult: ITestResult);
begin
  case testResult.ResultType of
    TTestResultType.Pass:
      begin
        context.RecordResult(fixtureResult,testResult);
        Self.Loggers_AddSuccess(threadId, testResult);
      end;
    TTestResultType.Failure:
      begin
        Log(TLogLevel.ltError, 'Test failed : ' + testResult.Test.Name + ' : ' + testResult.Message);
        context.RecordResult(fixtureResult, testResult);
        Self.Loggers_AddFailure(threadId, ITestError(testResult));
      end;
    TTestResultType.Error:
      begin
        Log(TLogLevel.ltError, 'Test Error : ' + testResult.Test.Name + ' : ' + testResult.Message);
        context.RecordResult(fixtureResult, testResult);
        Self.Loggers_AddError(threadId, ITestError(testResult));
      end;
    TTestResultType.Ignored :
      begin
        Log(TLogLevel.ltError, 'Test Ignored : ' + testResult.Test.Name + ' : ' + testResult.Message);
        context.RecordResult(fixtureResult,testResult);
        Self.Loggers_AddIgnored(threadId, testResult);
      end;
    TTestResultType.MemoryLeak :
      begin
        Log(TLogLevel.ltError, 'Test Leaked Memory : ' + testResult.Test.Name + ' : ' + testResult.Message);
        context.RecordResult(fixtureResult,testResult);
        Self.Loggers_AddMemoryLeak(threadId, testResult);
      end;

  end;
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
          if attribute.ClassType =  TestFixtureAttribute then
          begin
            sName := TestFixtureAttribute(attribute).Name;
            if sName = '' then
              sName := TRttiInstanceType(rType).MetaclassType.ClassName;
            if not FFixtureClasses.ContainsKey(TRttiInstanceType(rType).MetaclassType) then
              FFixtureClasses.Add(TRttiInstanceType(rType).MetaclassType,sName);
          end;
        end;
    end;
  end;
end;

procedure TDUnitXTestRunner.Loggers_EndSetupFixture(const threadId: Cardinal; const fixture: ITestFixtureInfo);
var
  logger : ITestLogger;
begin
  for logger in FLoggers do
     logger.OnEndSetupFixture(threadId,fixture);
end;

procedure TDUnitXTestRunner.Loggers_EndSetupTest(const threadId: Cardinal; const Test: ITestInfo);
var
  logger : ITestLogger;
begin
  for logger in FLoggers do
  begin
    try
      logger.OnEndSetupTest(threadid,Test);
    except
      //Hmmmm what to do with errors here. This kinda smells.
      on e : Exception do
      begin
        try
           logger.OnLog(TLogLevel.ltError,'Error in OnEndSetupEvent : ' + e.Message);
        except
          on e : Exception do
            System.Write('unable to log error in OnEndSetupTest event : ' + e.Message);
        end;
      end;
    end;
  end;

end;

procedure TDUnitXTestRunner.Loggers_EndTest(const threadId : Cardinal; const Test: ITestResult);
var
  logger : ITestLogger;
begin
  for logger in FLoggers do
    logger.OnEndTest(threadId,Test);

end;

procedure TDUnitXTestRunner.Loggers_EndTestFixture(const threadId : Cardinal; const results: IFixtureResult);
var
  logger : ITestLogger;
begin
  for logger in FLoggers do
  begin
    logger.OnEndTestFixture(threadId,results);
  end;
end;

procedure TDUnitXTestRunner.Loggers_ExecuteTest(const threadId: Cardinal; const Test: ITestInfo);
var
  logger : ITestLogger;
begin
  for logger in FLoggers do
    logger.OnExecuteTest(threadId, Test);
end;

//TODO - this needs to be thread aware so we can run tests in threads.
function TDUnitXTestRunner.Execute: IRunResults;

procedure CountTests(const fixtureList : ITestFixtureList; var count : Cardinal; var active : Cardinal);
var
  fixture  : ITestFixture;
  test     : ITest;
begin
  for fixture in fixtureList do
  begin
    for test in fixture.Tests do
    begin
      if test.Enabled then
      begin
        Inc(count);
        if not test.Ignored then
          Inc(active);
      end;
    end;
    if fixture.HasChildFixtures then
      CountTests(fixture.children,count,active);
  end;



end;

var
  fixtures : ITestFixtureList;
  context : ITestExecuteContext;
  threadId : Cardinal;
  testCount : Cardinal;
  testActiveCount : Cardinal;
begin
  result := nil;
  fixtures := BuildFixtures as ITestFixtureList;
  if fixtures.Count = 0 then
    raise ENoTestsRegistered.Create('No Test Fixtures found');

  testCount := 0;
  //TODO: Count the active tests that we have.
  testActiveCount := 0;

  CountTests(fixtures,testCount,testActiveCount);

  //TODO: Move to the fixtures class

  //TODO: Need a simple way of converting one list to another list of a supported interface. Generics should help here.
  result := TDUnitXRunResults.Create(fixtures.AsFixtureInfoList);
  context := result as ITestExecuteContext;

  //TODO: Record Test metrics.. runtime etc.
  threadId := TThread.CurrentThread.ThreadID;
  Self.Loggers_TestingStarts(threadId, testCount, testActiveCount);
  try
    ExecuteFixtures(nil,context, threadId, fixtures);
    //make sure each fixture includes it's child fixture result counts.
    context.RollupResults;
  finally
    Self.Loggers_TestingEnds(result);
  end;
end;

function TDUnitXTestRunner.ExecuteErrorResult(
  const context: ITestExecuteContext; const threadId: cardinal;
  const test: ITest; const TimesRun : Cardinal;const exception: Exception) : ITestError;
begin
  Result := TDUnitXTestError.Create(test as ITestInfo, TTestResultType.Error, exception, ExceptAddr, TimesRun, exception.Message);
end;

class function TDUnitXTestRunner.GetActiveRunner: ITestRunner;
begin
  result := nil;
  FActiveRunners.TryGetValue(TThread.CurrentThread.ThreadId,result)
end;

function TDUnitXTestRunner.ExecuteFailureResult(
  const context: ITestExecuteContext; const threadId: cardinal;
  const test: ITest; const TimesRun : Cardinal; const exception : Exception) : ITestError;
begin
  //TODO: Does test failure require its own results interface and class?
  Result := TDUnitXTestError.Create(test as ITestInfo, TTestResultType.Failure, exception, ExceptAddr, TimesRun, exception.Message);
end;

procedure TDUnitXTestRunner.ExecuteFixtures(const parentFixtureResult : IFixtureResult; const context: ITestExecuteContext; const threadId: Cardinal; const fixtures: ITestFixtureList);
var
  fixture: ITestFixture;
  fixtureResult : IFixtureResult;
begin
  for fixture in fixtures do
  begin
    if not fixture.Enabled then
      System.continue;

    fixtureResult := TDUnitXFixtureResult.Create(parentFixtureResult, fixture as ITestFixtureInfo);
    if parentFixtureResult = nil then
      context.RecordFixture(fixtureResult);

    Self.Loggers_StartTestFixture(threadId, fixture as ITestFixtureInfo);
    try
      //only run the setup method if there are actually tests
      if fixture.HasTests and Assigned(fixture.SetupFixtureMethod) then
        //TODO: Errors from here need to be logged into each test below us
        ExecuteSetupFixtureMethod(threadId, fixture);

      if fixture.HasTests then
        ExecuteTests(context, threadId, fixture,fixtureResult);

      if fixture.HasChildFixtures then
        ExecuteFixtures(fixtureResult, context, threadId, fixture.Children);

      if fixture.HasTests and Assigned(fixture.TearDownFixtureMethod) then
        //TODO: Tricker yet each test above us requires errors that occur here
        ExecuteTearDownFixtureMethod(context, threadId, fixture);
    finally
      Self.Loggers_EndTestFixture(threadId, fixtureResult);
    end;
  end;
end;

function TDUnitXTestRunner.ExecuteIgnoredResult(const context: ITestExecuteContext; const threadId: cardinal; const test: ITest; const ignoreReason: string): ITestResult;
begin
  result := TDUnitXTestResult.Create(test as ITestInfo, TTestResultType.Ignored, 0,ignoreReason);
end;

procedure TDUnitXTestRunner.ExecuteSetupFixtureMethod(const threadid: cardinal; const fixture : ITestFixture);
begin
  try
    Self.Loggers_SetupFixture(threadid, fixture as ITestFixtureInfo);
    fixture.SetupFixtureMethod;
    Self.Loggers_EndSetupFixture(threadid, fixture as ITestFixtureInfo);
  except
    on e: Exception do
    begin
      Log(TLogLevel.ltError, 'Error in Fixture SetupError : ' + fixture.Name + ' : ' + e.Message);
      Log(TLogLevel.ltError, 'Skipping Fixture.');

      raise;
    end;
  end;
end;

function TDUnitXTestRunner.ExecuteSuccessfulResult(const context: ITestExecuteContext; const threadId: cardinal; const test: ITest; const TimesRun : Cardinal;const message: string) : ITestResult;
begin
  Result := TDUnitXTestResult.Create(test as ITestInfo, TTestResultType.Pass, TimesRun,message);
end;

procedure TDUnitXTestRunner.ExecuteTearDownFixtureMethod(
  const context: ITestExecuteContext; const threadId: Cardinal;
  const fixture: ITestFixture);
begin
  try
    Self.Loggers_TeardownFixture(threadId, fixture as ITestFixtureInfo);
    fixture.TearDownFixtureMethod;
    fixture.OnMethodExecuted(fixture.TearDownFixtureMethod);
  except
    on e: Exception do
    begin
      //TODO: ExecuteErrorResult(context, threadId, test, 'Test does not support ITestExecute');
    end;
  end;
end;

function TDUnitXTestRunner.ExecuteTest(const context: ITestExecuteContext; const threadId: cardinal; const test: ITest; const memoryAllocationProvider : IMemoryLeakMonitor) : ITestResult;
var
  testExecute: ITestExecute;
begin
  if Supports(test, ITestExecute, testExecute) then
  begin
    FLogMessages.Clear;
    Self.Loggers_ExecuteTest(threadId, test as ITestInfo);

    memoryAllocationProvider.PreTest;
    try
      testExecute.Execute(context);
    finally
      memoryAllocationProvider.PostTest;
    end;

    Result := ExecuteSuccessfulResult(context, threadId, test,1,FLogMessages.Text);
    FLogMessages.Clear;
  end
  else
  begin
    //This will be handled by the caller as a test error.
    raise Exception.CreateFmt('%s does not support ITestExecute', [test.Name]);
  end;
end;

procedure TDUnitXTestRunner.ExecuteTests(const context : ITestExecuteContext; const threadId: Cardinal; const fixture: ITestFixture; const fixtureResult : IFixtureResult);
var
  tests : IEnumerable<ITest>;
  test : ITest;
  testResult : ITestResult;
  setupResult : ITestResult;
  tearDownResult : ITestResult;
  memoryAllocationResult : ITestResult;
  memoryAllocationProvider : IMemoryLeakMonitor;
  runCount : Cardinal;
  runIndex : Cardinal;
  successMsg : TStringBuilder;
begin
  tests := fixture.Tests;
  for test in tests do
  begin
    if not test.Enabled then
      System.Continue;

    memoryAllocationProvider := TDUnitXIoC.DefaultContainer.Resolve<IMemoryLeakMonitor>();

    //Start a fresh for this test. If we had an exception last execute of the
    //setup or tear down that may have changed this execution. Therefore we try
    //again to see if things have changed. Remember setup, test, tear down can
    //hold state
    setupResult := nil;
    testResult := nil;
    tearDownResult := nil;
    memoryAllocationResult := nil;

    Self.Loggers_BeginTest(threadId, test as ITestInfo);

    //If the setup fails then we need to show this as the result.
    if Assigned(fixture.SetupMethod) and (not test.Ignored) then
    if not (ExecuteTestSetupMethod(context, threadId, fixture, test, setupResult, memoryAllocationProvider)) then
        testResult := setupResult;

    try
      runCount := 0;
      successMsg := TStringBuilder.Create;
      try
      try
        if test.Ignored then
            testResult :=  ExecuteIgnoredResult(context,threadId,test,test.IgnoreReason)
       //If we haven't already failed, then run the test.
        else if testResult = nil then
        begin
           for runIndex := 1 to test.RepeatCount do
           begin
              inc(runCount);
              try
                testResult := ExecuteTest(context, threadId, test, memoryAllocationProvider);
                successMsg.Append(testResult.Message).AppendLine;
              except
                on e: ETestPass do
                begin
                   successMsg.Append(E.Message).AppendLine;
                   // if last time through on sucess re-raise same message.
                   if runIndex = test.RepeatCount then
                       raise;
                end
                else raise;
              end;
           end;
        end;

      except
        //Handle the results which are raised in the test.
        on e: ETestPass do
          testResult := ExecuteSuccessfulResult(context, threadId, test,runCount, successMsg.ToString );
        on e: ETestFailure do
          testResult := ExecuteFailureResult(context, threadId, test, runCount,e);
        on e: Exception do
          testResult := ExecuteErrorResult(context, threadId, test,runCount, e);
      end;
      finally
        successMsg.Free;
      end;

      //If the tear down fails then we need to show this as the test result.
      if Assigned(fixture.TearDownMethod) and (not test.Ignored) then
      if not (ExecuteTestTearDown(context, threadId, fixture, test, tearDownResult, memoryAllocationProvider)) then
          testResult := tearDownResult;

      if(testResult.ResultType = TTestResultType.Pass) then
        if (not CheckMemoryAllocations(test, memoryAllocationResult, memoryAllocationProvider)) then
          testResult := memoryAllocationResult;
    finally
      RecordResult(context, threadId, fixtureResult, testResult);
      Self.Loggers_EndTest(threadId, testResult);
    end;
  end;
end;

function TDUnitXTestRunner.ExecuteTestSetupMethod(const context : ITestExecuteContext; const threadid: cardinal; const fixture: ITestFixture; const test: ITest; out errorResult: ITestResult; const memoryAllocationProvider : IMemoryLeakMonitor): boolean;
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
      on e: SysUtils.Exception do
      begin
        errorResult := ExecuteErrorResult(context, threadId, test, 1, e);
      end;
    end;
  end;
end;

function TDUnitXTestRunner.ExecuteTestTearDown(const context:
  ITestExecuteContext; const threadId: Cardinal; const fixture: ITestFixture; const test: ITest; out errorResult: ITestResult; const memoryAllocationProvider : IMemoryLeakMonitor): boolean;
begin
  Result := False;
  errorResult := nil;

  try
    Self.Loggers_TeardownTest(threadId, test as ITestInfo);

    memoryAllocationProvider.PreTearDown;

    fixture.TearDownMethod;

    memoryAllocationProvider.PostTearDown;

    result := true;
  except
    on e: SysUtils.Exception do
    begin
      errorResult := ExecuteErrorResult(context, threadId, test, 1, e);
    end;
  end;
end;

function TDUnitXTestRunner.GetExitBehavior: TRunnerExitBehavior;
begin
  result := FExitBehavior;
end;

function TDUnitXTestRunner.GetUseCommandLineOptions: Boolean;
begin
  result := FUseCommandLine;
end;

function TDUnitXTestRunner.GetUseRTTI: Boolean;
begin
  result := FUseRTTI;
end;


procedure TDUnitXTestRunner.SetExitBehavior(const value: TRunnerExitBehavior);
begin
  FExitBehavior := value;
end;

procedure TDUnitXTestRunner.SetUseCommandLineOptions(const value: Boolean);
begin
  FUseCommandLine := value;
end;

procedure TDUnitXTestRunner.SetUseRTTI(const value: Boolean);
begin
  FUseRTTI := value;
end;


procedure TDUnitXTestRunner.Status(const msg: string);
begin
  Self.Log(TLogLevel.ltInformation,msg);

end;

procedure TDUnitXTestRunner.WriteLn;
begin
  Self.Log(TLogLevel.ltInformation,'');
end;

procedure TDUnitXTestRunner.WriteLn(const msg: string);
begin
  Self.Log(TLogLevel.ltInformation,msg);
end;

procedure TDUnitXTestRunner.Loggers_SetupFixture(const threadId: Cardinal; const fixture: ITestFixtureInfo);
var
  logger : ITestLogger;
begin
  for logger in FLoggers do
    logger.OnSetupFixture(threadId,fixture);
end;

procedure TDUnitXTestRunner.Loggers_SetupTest(const threadId: Cardinal; const Test: ITestInfo);
var
  logger : ITestLogger;
begin
  for logger in FLoggers do
    logger.OnSetupTest(threadId,Test);
end;

procedure TDUnitXTestRunner.Loggers_BeginTest(const threadId : Cardinal; const Test: ITestInfo);
var
  logger : ITestLogger;
begin
  for logger in FLoggers do
    logger.OnBeginTest(threadId, Test);
end;

procedure TDUnitXTestRunner.Loggers_StartTestFixture(const threadId : Cardinal; const fixture: ITestFixtureInfo);
var
  logger : ITestLogger;
begin
  for logger in FLoggers do
    logger.OnStartTestFixture(threadId, fixture);
end;

procedure TDUnitXTestRunner.Loggers_TeardownFixture(const threadId: Cardinal; const fixture: ITestFixtureInfo);
var
  logger : ITestLogger;
begin
  for logger in FLoggers do
    logger.OnTearDownFixture(threadId, fixture);
end;

procedure TDUnitXTestRunner.Loggers_TeardownTest(const threadId: Cardinal; const Test: ITestInfo);
var
  logger : ITestLogger;
begin
  for logger in FLoggers do
    logger.OnTeardownTest(threadId, Test);
end;

procedure TDUnitXTestRunner.Loggers_TestingEnds(const RunResults: IRunResults);
var
  logger : ITestLogger;
begin
  for logger in FLoggers do
    logger.OnTestingEnds(RunResults);
end;

procedure TDUnitXTestRunner.Loggers_TestingStarts(const threadId, testCount, testActiveCount : Cardinal);
var
  logger : ITestLogger;
begin
  for logger in FLoggers do
    logger.OnTestingStarts(threadId, testCount, testActiveCount);
end;

procedure TDUnitXTestRunner.Log(const logType: TLogLevel; const msg: string);
var
  logger : ITestLogger;
begin
  if logType >= TDUnitX.CommandLine.LogLevel then
  begin
    //TODO : Need to get this to the current test result.
    FLogMessages.Add(msg);
    for logger in FLoggers do
      logger.OnLog(logType,msg);
  end;
end;

end.

