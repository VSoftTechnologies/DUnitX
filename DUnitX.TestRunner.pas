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
  {$IFDEF USE_NS}
  System.Classes,
  System.SysUtils,
  System.Rtti,
  System.Generics.Collections,
  {$ELSE}
  Classes,
  SysUtils,
  Rtti,
  Generics.Collections,
  {$ENDIF}
  DUnitX.TestFramework,
  DUnitX.Extensibility,
  DUnitX.InternalInterfaces,
  DUnitX.Generics,
  DUnitX.WeakReference,
  DUnitX.Filters,
  DUnitX.Exceptions;

type
  ///  Note - we rely on the fact that there will only ever be 1 testrunner
  ///  per thread, if this changes then handling of WriteLn will need to change
  TDUnitXTestRunner = class(TWeakReferencedObject, ITestRunner)
  private class var
    FRttiContext : TRttiContext;
  public class var
    FActiveRunners : TDictionary<TThreadID,IWeakReference<ITestRunner>>;
  private
    FLoggers          : TList<ITestLogger>;
    FUseRTTI          : boolean;
    FFixtureClasses   : TDictionary<TClass,string>;

    FFixtureList      : ITestFixtureList;
    FLogMessages      : TStringList;
    FLogMessagesEx    : TLogMessageArray;

    FFailsOnNoAsserts : boolean;

  protected
    procedure CountAndFilterTests(const fixtureList: ITestFixtureList; var count, active: Cardinal);
    //Logger calls - sequence ordered
    procedure Loggers_TestingStarts(const threadId: TThreadID; testCount, testActiveCount: Cardinal);

    procedure Loggers_StartTestFixture(const threadId: TThreadID; const fixture: ITestFixtureInfo);

    procedure Loggers_SetupFixture(const threadId: TThreadID; const fixture: ITestFixtureInfo);
    procedure Loggers_EndSetupFixture(const threadId: TThreadID; const fixture: ITestFixtureInfo);

    procedure Loggers_BeginTest(const threadId: TThreadID; const Test: ITestInfo);

    procedure Loggers_SetupTest(const threadId: TThreadID; const Test: ITestInfo);
    procedure Loggers_EndSetupTest(const threadId: TThreadID; const Test: ITestInfo);

    procedure Loggers_ExecuteTest(const threadId: TThreadID; const Test: ITestInfo);

    procedure Loggers_AddSuccess(const threadId: TThreadID; const Test: ITestResult);
    procedure Loggers_AddError(const threadId: TThreadID; const Error: ITestError);
    procedure Loggers_AddFailure(const threadId: TThreadID; const Failure: ITestError);
    procedure Loggers_AddIgnored(const threadId: TThreadID; const AIgnored: ITestResult);
    procedure Loggers_AddMemoryLeak(const threadId: TThreadID; const Test: ITestResult);

    procedure Loggers_EndTest(const threadId: TThreadID; const Test: ITestResult);
    procedure Loggers_TeardownTest(const threadId: TThreadID; const Test: ITestInfo);

    procedure Loggers_TeardownFixture(const threadId: TThreadID; const fixture: ITestFixtureInfo);

    procedure Loggers_EndTestFixture(const threadId: TThreadID; const results: IFixtureResult);

    procedure Loggers_TestingEnds(const RunResults: IRunResults);

    //ITestRunner
    procedure AddLogger(const value: ITestLogger);
    function Execute: IRunResults;

    procedure ExecuteFixtures(const parentFixtureResult: IFixtureResult; const context: ITestExecuteContext; const threadId: TThreadID; const fixtures: ITestFixtureList);
    procedure ExecuteSetupFixtureMethod(const context: ITestExecuteContext; const threadId: TThreadID; const fixture: ITestFixture; const fixtureResult: IFixtureResult);
    function  ExecuteTestSetupMethod(const context: ITestExecuteContext; const threadId: TThreadID; const fixture: ITestFixture; const test: ITest; out errorResult: ITestResult; const memoryAllocationProvider: IMemoryLeakMonitor): boolean;

    procedure ExecuteTests(const context: ITestExecuteContext; const threadId: TThreadID; const fixture: ITestFixture; const fixtureResult: IFixtureResult);

    function ExecuteTest(const context: ITestExecuteContext; const threadId: TThreadID; const test: ITest; const memoryAllocationProvider: IMemoryLeakMonitor): ITestResult;
    function ExecuteSuccessfulResult(const context: ITestExecuteContext; const threadId: TThreadID; const test: ITest; const aMessage: string; const aLogMessages: TLogMessageArray): ITestResult;
    function ExecuteFailureResult(const context: ITestExecuteContext; const threadId: TThreadID; const test: ITest; const exception: Exception; const aLogMessages: TLogMessageArray): ITestError;
    function ExecuteTimedOutResult(const context: ITestExecuteContext; const threadId: TThreadID; const test: ITest; const exception: Exception; const aLogMessages: TLogMessageArray) : ITestError;
    function ExecuteErrorResult(const context: ITestExecuteContext; const threadId: TThreadID; const test: ITest; const exception: Exception; const aLogMessages: TLogMessageArray): ITestError;
    function ExecuteIgnoredResult(const context: ITestExecuteContext; const threadId: TThreadID; const test: ITest; const ignoreReason: string): ITestResult;

    function CheckMemoryAllocations(const test: ITest; out errorResult: ITestResult; const memoryAllocationProvider: IMemoryLeakMonitor): boolean;

    function ExecuteTestTearDown(const context: ITestExecuteContext; const threadId: TThreadID; const fixture: ITestFixture; const test: ITest; out errorResult: ITestResult; const memoryAllocationProvider: IMemoryLeakMonitor): boolean;
    procedure ExecuteTearDownFixtureMethod(const context: ITestExecuteContext; const threadId: TThreadID; const fixture: ITestFixture);

    procedure RecordResult(const context: ITestExecuteContext; const threadId: TThreadID; const fixtureResult: IFixtureResult; const testResult: ITestResult);

    function GetUseRTTI: Boolean;
    procedure SetUseRTTI(const value: Boolean);
    function GetFailsOnNoAsserts: boolean;
    procedure SetFailsOnNoAsserts(const value: boolean);

    procedure Log(const logType: TLogLevel; const msg: string); overload;
    procedure Log(const msg: string); overload;

    //for backwards compatibilty with DUnit tests.
    procedure Status(const msg: string); overload;

    //redirects WriteLn to our loggers.
    procedure WriteLn(const msg: string); overload;
    procedure WriteLn; overload;

    //internals
    procedure RTTIDiscoverFixtureClasses;
    function BuildFixtures: IInterface;

    procedure AddStatus(const threadId; const msg: string);

    function CreateFixture(const AInstance: TObject; const AFixtureClass: TClass; const AName: string; const ACategory: string): ITestFixture;

    function ShouldRunThisTest(const test: ITest): boolean;

    class constructor Create;
    class destructor Destroy;
  public
    constructor Create; overload;
    constructor Create(const AListener: ITestLogger); overload;
    constructor Create(const AListeners: array of ITestLogger); overload;
    destructor Destroy; override;
    class function GetActiveRunner: ITestRunner;
  end;

implementation

uses
  {$IFDEF USE_NS}
  System.TypInfo,
  System.StrUtils,
  System.Types,
  {$ELSE}
  TypInfo,
  StrUtils,
  Types,  
  {$ENDIF}
  DUnitX.Attributes,
  DUnitX.CommandLine.Options,
  DUnitX.TestFixture,
  DUnitX.RunResults,
  DUnitX.TestResult,
  DUnitX.FixtureResult,
  DUnitX.Utils,
  DUnitX.IoC,
  DUnitX.Extensibility.PluginManager,
  DUnitX.ResStrs;

{ TDUnitXTestRunner }

procedure TDUnitXTestRunner.Log(const msg: string);
begin
  Self.Log(TLogLevel.Information,msg);
end;

procedure TDUnitXTestRunner.Loggers_AddError(const threadId: TThreadID; const Error: ITestError);
var
  logger : ITestLogger;
begin
  for logger in FLoggers do
  begin
    logger.OnTestError(threadId, Error);
  end;
end;

procedure TDUnitXTestRunner.Loggers_AddFailure(const threadId: TThreadID; const Failure: ITestError);
var
  logger : ITestLogger;
begin
  for logger in FLoggers do
  begin
    logger.OnTestFailure(threadId, Failure);
  end;
end;

procedure TDUnitXTestRunner.Loggers_AddIgnored(const threadId: TThreadID; const AIgnored: ITestResult);
var
  logger : ITestLogger;
begin
  for logger in FLoggers do
  begin
    logger.OnTestIgnored(threadId,AIgnored);
  end;
end;

procedure TDUnitXTestRunner.Loggers_AddMemoryLeak(const threadId: TThreadID; const Test: ITestResult);
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

procedure TDUnitXTestRunner.Loggers_AddSuccess(const threadId: TThreadID; const Test: ITestResult);
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
    //TODO : What should be here???
end;

function TDUnitXTestRunner.BuildFixtures  : IInterface;
var
  pluginManager : IPluginManager;
begin
  result := FFixtureList;
  if FFixtureList <> nil then
    exit;

  FFixtureList := TTestFixtureList.Create;


  pluginManager := TPluginManager.Create(Self.CreateFixture,FUseRTTI);
  pluginManager.Init;//loads the plugin features.

  //generate the fixtures. The plugin Manager calls back into CreateFixture
  pluginManager.CreateFixtures;
  FFixtureList.Sort;

  result := FFixtureList;
end;

class constructor TDUnitXTestRunner.Create;
begin
  FRttiContext := TRttiContext.Create;
  FActiveRunners := TDictionary<TThreadID,IWeakReference<ITestRunner>>.Create;
end;

function TDUnitXTestRunner.CheckMemoryAllocations(const test: ITest; out errorResult: ITestResult; const memoryAllocationProvider: IMemoryLeakMonitor): boolean;
var
  LSetUpMemoryAllocated: Int64;
  LTearDownMemoryAllocated: Int64;
  LTestMemoryAllocated: Int64;
  LMsg: string;
  LMemoryAllocationProvider2: IMemoryLeakMonitor2;
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

  if memoryAllocationProvider.QueryInterface(IMemoryLeakMonitor2, LMemoryAllocationProvider2) = 0 then
    LMsg := LMemoryAllocationProvider2.GetReport;

  if (LTestMemoryAllocated = 0) then
  begin
    // The leak occurred in the setup/teardown
    Result := False;
    LMsg := Format(SSetupTeardownBytesLeaked, [LSetUpMemoryAllocated + LTearDownMemoryAllocated]) + LMsg;

    errorResult := TDUnitXTestResult.Create(test as ITestInfo, TTestResultType.MemoryLeak, LMsg);
  end
  else if (LSetUpMemoryAllocated + LTearDownMemoryAllocated = 0) then
  begin
    // The leak occurred in the test only
    Result := False;
    LMsg := Format(STestBytesLeaked, [LTestMemoryAllocated]) + LMsg;
    errorResult := TDUnitXTestResult.Create(test as ITestInfo, TTestResultType.MemoryLeak, LMsg);
  end
  else
  begin
    // The leak occurred in the setup/teardown/test
    Result := False;
    LMsg := Format(SSetupTestTeardownBytesLeaked, [LSetUpMemoryAllocated + LTestMemoryAllocated + LTearDownMemoryAllocated]) + LMsg;
    errorResult := TDUnitXTestResult.Create(test as ITestInfo, TTestResultType.MemoryLeak, LMsg);
  end;
end;

constructor TDUnitXTestRunner.Create;
begin
  inherited;
  FLoggers := TList<ITestLogger>.Create;
  FFixtureClasses := TDictionary<TClass,string>.Create;
  FUseRTTI := False;
  FLogMessages := TStringList.Create;
  MonitorEnter(TDUnitXTestRunner.FActiveRunners);
  try
    TDUnitXTestRunner.FActiveRunners.Add(TThread.CurrentThread.ThreadID, TWeakReference<ITestRunner>.Create(Self));
  finally
    MonitorExit(TDUnitXTestRunner.FActiveRunners);
  end;
end;

constructor TDUnitXTestRunner.Create(const AListener: ITestLogger);
begin
  Create;

  if AListener <> nil then
    FLoggers.Add(AListener);
end;

constructor TDUnitXTestRunner.Create(const AListeners: array of ITestLogger);
begin
  Create;

  FLoggers.AddRange(AListeners);
end;

function TDUnitXTestRunner.CreateFixture(const AInstance : TObject;const AFixtureClass: TClass; const AName: string; const ACategory : string): ITestFixture;
begin
  if AInstance <> nil then
    result := TDUnitXTestFixture.Create(AName,ACategory, AInstance,AInstance.ClassType.UnitName)
  else
    result := TDUnitXTestFixture.Create(AName, ACategory, AFixtureClass,AFixtureClass.UnitName);
  FFixtureList.Add(Result);
end;


destructor TDUnitXTestRunner.Destroy;
var
  tId: TThreadID;
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
  FRttiContext.Free;
end;

procedure TDUnitXTestRunner.RecordResult(const context: ITestExecuteContext; const threadId: TThreadID; const fixtureResult : IFixtureResult; const testResult: ITestResult);
begin
  case testResult.ResultType of
    TTestResultType.Pass:
      begin
        context.RecordResult(fixtureResult,testResult);
        Self.Loggers_AddSuccess(threadId, testResult);
      end;
    TTestResultType.Failure:
      begin
        Log(TLogLevel.Error, STestFailed + testResult.Test.Name + ' : ' + testResult.Message);
        context.RecordResult(fixtureResult, testResult);
        Self.Loggers_AddFailure(threadId, ITestError(testResult));
      end;
    TTestResultType.Error:
      begin
        Log(TLogLevel.Error, STestError + testResult.Test.Name + ' : ' + testResult.Message);
        context.RecordResult(fixtureResult, testResult);
        Self.Loggers_AddError(threadId, ITestError(testResult));
      end;
    TTestResultType.Ignored :
      begin
        Log(TLogLevel.Error, STestIgnored + testResult.Test.Name + ' : ' + testResult.Message);
        context.RecordResult(fixtureResult,testResult);
        Self.Loggers_AddIgnored(threadId, testResult);
      end;
    TTestResultType.MemoryLeak :
      begin
        Log(TLogLevel.Error, STestLeaked + testResult.Test.Name + ' : ' + testResult.Message);
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

procedure TDUnitXTestRunner.Loggers_EndSetupFixture(const threadId: TThreadID; const fixture: ITestFixtureInfo);
var
  logger : ITestLogger;
begin
  for logger in FLoggers do
     logger.OnEndSetupFixture(threadId,fixture);
end;

procedure TDUnitXTestRunner.Loggers_EndSetupTest(const threadId: TThreadID; const Test: ITestInfo);
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
           logger.OnLog(TLogLevel.Error, SOnEndSetupEventError + e.Message);
        except
          on e : Exception do
            System.Write(SOnEndSetupTestLogError + e.Message);
        end;
      end;
    end;
  end;

end;

procedure TDUnitXTestRunner.Loggers_EndTest(const threadId: TThreadID; const Test: ITestResult);
var
  logger : ITestLogger;
begin
  for logger in FLoggers do
    logger.OnEndTest(threadId,Test);

end;

procedure TDUnitXTestRunner.Loggers_EndTestFixture(const threadId: TThreadID; const results: IFixtureResult);
var
  logger : ITestLogger;
begin
  for logger in FLoggers do
  begin
    logger.OnEndTestFixture(threadId,results);
  end;
end;

procedure TDUnitXTestRunner.Loggers_ExecuteTest(const threadId: TThreadID; const Test: ITestInfo);
var
  logger : ITestLogger;
begin
  for logger in FLoggers do
    logger.OnExecuteTest(threadId, Test);
end;

procedure TDUnitXTestRunner.CountAndFilterTests(const fixtureList : ITestFixtureList; var count : Cardinal; var active : Cardinal);
var
  fixture  : ITestFixture;
  test     : ITest;
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
      CountAndFilterTests(fixture.children,count,active);

//    if not (fixture.HasTests or fixture.HasChildTests) then
//      fixture.Enabled := false;
  end;
end;


//TODO - this needs to be thread aware so we can run tests in threads.
function TDUnitXTestRunner.Execute: IRunResults;
var
  fixtureList : ITestFixtureList;
  context : ITestExecuteContext;
  threadId : TThreadID;
  testCount : Cardinal;
  testActiveCount : Cardinal;
  fixtures : IInterface;
begin
  result := nil;


  fixtures := BuildFixtures;
  fixtureList := fixtures as ITestFixtureList;
  if fixtureList.Count = 0 then
    raise ENoTestsRegistered.Create(SNoFixturesFound);

  testCount := 0;
  //TODO: Count the active tests that we have.
  testActiveCount := 0;

  //TODO : Filter tests here?


  CountAndFilterTests(fixtureList,testCount,testActiveCount);

  //TODO: Move to the fixtures class
  result := TDUnitXRunResults.Create;
  context := result as ITestExecuteContext;

  //TODO: Record Test metrics.. runtime etc.
  threadId := TThread.CurrentThread.ThreadID;
  Self.Loggers_TestingStarts(threadId, testCount, testActiveCount);
  try
    ExecuteFixtures(nil,context, threadId, fixtureList);
    context.RollupResults;
  finally
    Self.Loggers_TestingEnds(result);
  end;
end;

function TDUnitXTestRunner.ExecuteErrorResult(const context: ITestExecuteContext; const threadId: TThreadID; const test: ITest; const exception: Exception; const aLogMessages: TLogMessageArray) : ITestError;
begin
  Result := TDUnitXTestError.Create(test as ITestInfo, TTestResultType.Error, exception, ExceptAddr, exception.Message, aLogMessages);
end;

class function TDUnitXTestRunner.GetActiveRunner: ITestRunner;
var
  ref : IWeakReference<ITestRunner>;
begin
  result := nil;
  if FActiveRunners.TryGetValue(TThread.CurrentThread.ThreadId,ref) then
    result := ref.Data;
end;

function TDUnitXTestRunner.GetFailsOnNoAsserts: boolean;
begin
  Result := FFailsOnNoAsserts;
end;

function TDUnitXTestRunner.ExecuteFailureResult(const context: ITestExecuteContext; const threadId: TThreadID; const test: ITest; const exception : Exception; const aLogMessages: TLogMessageArray) : ITestError;
begin
  //TODO: Does test failure require its own results interface and class?
  Result := TDUnitXTestError.Create(test as ITestInfo, TTestResultType.Failure, exception, ExceptAddr, exception.Message, aLogMessages);
end;

procedure TDUnitXTestRunner.ExecuteFixtures(const parentFixtureResult : IFixtureResult; const context: ITestExecuteContext; const threadId: TThreadID; const fixtures: ITestFixtureList);
var
  fixture: ITestFixture;
  fixtureResult : IFixtureResult;
begin
  for fixture in fixtures do
  begin
    if not fixture.Enabled then
      System.continue;

    if (not fixture.HasTests) and (not fixture.HasChildTests) then
      System.Continue;

    fixtureResult := TDUnitXFixtureResult.Create(parentFixtureResult, fixture as ITestFixtureInfo);
    if parentFixtureResult = nil then
      context.RecordFixture(fixtureResult);

    Self.Loggers_StartTestFixture(threadId, fixture as ITestFixtureInfo);
    try
      //Initialize the fixture as it may have been destroyed in a previous run (when using gui runner).
      if fixture.HasTests then
        fixture.InitFixtureInstance;
      //only run the setup method if there are actually tests
      if fixture.HasTests and Assigned(fixture.SetupFixtureMethod) then
        ExecuteSetupFixtureMethod(context, threadId, fixture, fixtureResult);

      if fixture.HasTests then
        ExecuteTests(context, threadId, fixture, fixtureResult);

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

function TDUnitXTestRunner.ExecuteIgnoredResult(const context: ITestExecuteContext; const threadId: TThreadID; const test: ITest; const ignoreReason: string): ITestResult;
begin
  result := TDUnitXTestResult.Create(test as ITestInfo, TTestResultType.Ignored, ignoreReason);
end;

procedure TDUnitXTestRunner.ExecuteSetupFixtureMethod(const context: ITestExecuteContext; const threadId: TThreadID; const fixture: ITestFixture; const fixtureResult: IFixtureResult);
var
  testResult : ITestResult;
  tests : IEnumerable<ITest>;
  test : ITest;
begin
  try
    Self.Loggers_SetupFixture(threadid, fixture as ITestFixtureInfo);
    fixture.SetupFixtureMethod;
    Self.Loggers_EndSetupFixture(threadid, fixture as ITestFixtureInfo);
  except
    on e: Exception do
    begin
      tests := fixture.Tests;
      // Log error into each test
      for test in tests do
      begin
        if not ShouldRunThisTest(test) then
          System.Continue;

        Self.Loggers_BeginTest(threadId, test as ITestInfo);
        testResult := ExecuteErrorResult(context, threadId, test, e, FLogMessagesEx);
        RecordResult(context, threadId, fixtureResult, testResult);
        Self.Loggers_EndTest(threadId, testResult);
      end;
      Log(TLogLevel.Error, Format(SFixtureSetupError, [fixture.Name, e.Message]));
      Log(TLogLevel.Error, SSkippingFixture);
    end;
  end;
end;

function TDUnitXTestRunner.ExecuteSuccessfulResult(const context: ITestExecuteContext; const threadId: TThreadID; const test: ITest; const aMessage: string; const aLogMessages: TLogMessageArray): ITestResult;
begin
  Result := TDUnitXTestResult.Create(test as ITestInfo, TTestResultType.Pass, aMessage, aLogMessages);
end;

procedure TDUnitXTestRunner.ExecuteTearDownFixtureMethod(const context: ITestExecuteContext; const threadId: TThreadID; const fixture: ITestFixture);
begin
  try
    Self.Loggers_TeardownFixture(threadId, fixture as ITestFixtureInfo);
    fixture.ExecuteFixtureTearDown; //deals with destructors in a way that stops memory leak reporting from reporting bogus leaks.
  except
    on e: Exception do
    begin
      //TODO: ExecuteErrorResult(context, threadId, test, 'Test does not support ITestExecute');
      Log(TLogLevel.Error, Format(SFixtureTeardownError, [fixture.Name,e.Message]));
      raise;
    end;
  end;
end;

function TDUnitXTestRunner.ExecuteTest(const context: ITestExecuteContext; const threadId: TThreadID; const test: ITest; const memoryAllocationProvider : IMemoryLeakMonitor) : ITestResult;
var
  testExecute: ITestExecute;
  assertBeforeCount : Cardinal;
  assertAfterCount : Cardinal;
begin
  if Supports(test, ITestExecute, testExecute) then
  begin
    FLogMessages.Clear;
    SetLength(FLogMessagesEx, 0);
    Self.Loggers_ExecuteTest(threadId, test as ITestInfo);
    assertBeforeCount := 0;//to shut the compiler up;
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
      if (assertBeforeCount = assertAfterCount)  then
        raise ENoAssertionsMade.Create(SNoAssertions);
    end;

    Result := ExecuteSuccessfulResult(context, threadId, test, FLogMessages.Text, FLogMessagesEx);
    FLogMessages.Clear;
    SetLength(FLogMessagesEx, 0);
  end
  else
  begin
    //This will be handled by the caller as a test error.
    raise Exception.CreateFmt(SITestExecuteNotSupported, [test.Name]);
  end;
end;

procedure TDUnitXTestRunner.ExecuteTests(const context : ITestExecuteContext; const threadId: TThreadID; const fixture: ITestFixture; const fixtureResult : IFixtureResult);
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
      try
        if test.Ignored  then
            testResult :=  ExecuteIgnoredResult(context,threadId,test,test.IgnoreReason)
       //If we haven't already failed, then run the test.
        else if testResult = nil then
           testResult := ExecuteTest(context, threadId, test, memoryAllocationProvider);

      except
        //Handle the results which are raised in the test.
        on e: ETestPass do
          testResult := ExecuteSuccessfulResult(context, threadId, test, e.Message, FLogMessagesEx);
        on e: ETestFailure do
          testResult := ExecuteFailureResult(context, threadId, test, e, FLogMessagesEx);
        on e: ETimedOut do
          testResult := ExecuteTimedOutResult(context, threadId, test, e, FLogMessagesEx);
        on e: Exception do
          testResult := ExecuteErrorResult(context, threadId, test, e, FLogMessagesEx);
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

function TDUnitXTestRunner.ExecuteTestSetupMethod(const context : ITestExecuteContext; const threadId: TThreadID; const fixture: ITestFixture; const test: ITest; out errorResult: ITestResult; const memoryAllocationProvider : IMemoryLeakMonitor): boolean;
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
      on e: {$IFDEF USE_NS}System.SysUtils.{$ENDIF}Exception do
      begin
        errorResult := ExecuteErrorResult(context, threadId, test, e, FLogMessagesEx);
      end;
    end;
  end;
end;

function TDUnitXTestRunner.ExecuteTestTearDown(const context:
  ITestExecuteContext; const threadId: TThreadID; const fixture: ITestFixture; const test: ITest; out errorResult: ITestResult; const memoryAllocationProvider : IMemoryLeakMonitor): boolean;
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
    on e: {$IFDEF USE_NS}System.SysUtils.{$ENDIF}Exception do
    begin
      errorResult := ExecuteErrorResult(context, threadId, test, e, FLogMessagesEx);
    end;
  end;
end;



function TDUnitXTestRunner.ExecuteTimedOutResult(const context: ITestExecuteContext; const threadId: TThreadID; const test: ITest; const exception: Exception; const aLogMessages: TLogMessageArray): ITestError;
begin
  Result := TDUnitXTestError.Create(test as ITestInfo, TTestResultType.Failure, exception, ExceptAddr, exception.Message, aLogMessages);
end;

function TDUnitXTestRunner.GetUseRTTI: Boolean;
begin
  result := FUseRTTI;
end;

procedure TDUnitXTestRunner.SetFailsOnNoAsserts(const value: boolean);
begin
  FFailsOnNoAsserts := value;
end;

procedure TDUnitXTestRunner.SetUseRTTI(const value: Boolean);
begin
  FUseRTTI := value;
end;

procedure TDUnitXTestRunner.Status(const msg: string);
begin
  Self.Log(TLogLevel.Information,msg);
end;

procedure TDUnitXTestRunner.WriteLn;
begin
  Self.Log(TLogLevel.Information,'');
end;

procedure TDUnitXTestRunner.WriteLn(const msg: string);
begin
  Self.Log(TLogLevel.Information,msg);
end;

procedure TDUnitXTestRunner.Loggers_SetupFixture(const threadId: TThreadID; const fixture: ITestFixtureInfo);
var
  logger : ITestLogger;
begin
  for logger in FLoggers do
    logger.OnSetupFixture(threadId,fixture);
end;

procedure TDUnitXTestRunner.Loggers_SetupTest(const threadId: TThreadID; const Test: ITestInfo);
var
  logger : ITestLogger;
begin
  for logger in FLoggers do
    logger.OnSetupTest(threadId,Test);
end;

procedure TDUnitXTestRunner.Loggers_BeginTest(const threadId: TThreadID; const Test: ITestInfo);
var
  logger : ITestLogger;
begin
  for logger in FLoggers do
    logger.OnBeginTest(threadId, Test);
end;

procedure TDUnitXTestRunner.Loggers_StartTestFixture(const threadId: TThreadID; const fixture: ITestFixtureInfo);
var
  logger : ITestLogger;
begin
  for logger in FLoggers do
    logger.OnStartTestFixture(threadId, fixture);
end;

procedure TDUnitXTestRunner.Loggers_TeardownFixture(const threadId: TThreadID; const fixture: ITestFixtureInfo);
var
  logger : ITestLogger;
begin
  for logger in FLoggers do
    logger.OnTearDownFixture(threadId, fixture);
end;

procedure TDUnitXTestRunner.Loggers_TeardownTest(const threadId: TThreadID; const Test: ITestInfo);
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

procedure TDUnitXTestRunner.Loggers_TestingStarts(const threadId: TThreadID; testCount, testActiveCount: Cardinal);
var
  logger : ITestLogger;
begin
  for logger in FLoggers do
    logger.OnTestingStarts(threadId, testCount, testActiveCount);
end;

procedure TDUnitXTestRunner.Log(const logType: TLogLevel; const msg: string);
var
  logger : ITestLogger;
  NewIdx: integer;
begin
  if logType >= TDUnitX.Options.LogLevel then
  begin
    //TODO : Need to get this to the current test result.
    FLogMessages.Add(msg);
    NewIdx := Length(FLogMessagesEx);
    SetLength(FLogMessagesEx, NewIdx + 1);
    FLogMessagesEx[NewIdx].Level := logType;
    FLogMessagesEx[NewIdx].Msg := msg;
    for logger in FLoggers do
      logger.OnLog(logType, msg);
  end;
end;

function TDUnitXTestRunner.ShouldRunThisTest(const test: ITest): boolean;
begin
  Result := True;

  if not test.Enabled then
    Result := False;

  if test.Ignored and TDUnitX.Options.DontShowIgnored then
    Result := False;

end;

end.

