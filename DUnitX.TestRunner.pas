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
  SysUtils,
  Rtti;

{$I DUnitX.inc}


type
  ///  Note - we rely on the fact that there will only ever be 1 testrunner
  ///  per thread, if this changes then handling of WriteLn will need to change
  TDUnitXTestRunner = class(TWeakReferencedObject, ITestRunner)
  private class var
    FRttiContext : TRttiContext;
  public class var
    FActiveRunners : TDictionary<Cardinal,ITestRunner>;
  private
    FLoggers      : TList<ITestLogger>;
    FUseCommandLine : boolean;
    FUseRTTI        : boolean;
    FExitBehavior   : TRunnerExitBehavior;
    FFixtureClasses : TDictionary<string,TClass>;

    FFixtureList    : ITestFixtureList;

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
    procedure Loggers_AddWarning(const threadId : Cardinal; const AWarning: ITestResult);
    procedure Loggers_AddIgnored(const threadId : Cardinal; const AIgnored: ITestResult);

    procedure Loggers_EndTest(const threadId : Cardinal; const Test: ITestResult);
    procedure Loggers_TeardownTest(const threadId : Cardinal; const Test: ITestInfo);

    procedure Loggers_TeardownFixture(const threadId : Cardinal; const fixture : ITestFixtureInfo);

    procedure Loggers_EndTestFixture(const threadId : Cardinal; const results : IFixtureResult);

    procedure Loggers_TestingEnds(const TestResult: ITestResults);

    //ITestRunner
    procedure AddLogger(const value: ITestLogger);
    function Execute: ITestResults;

    procedure ExecuteFixtures(const context: ITestExecuteContext; const threadId: Cardinal; const fixtures: ITestFixtureList);
    procedure ExecuteSetupFixtureMethod(const threadid: cardinal; const fixture: ITestFixture);
    function  ExecuteTestSetupMethod(const context : ITestExecuteContext; const threadid: cardinal; const fixture: ITestFixture; const test: ITest; out errorResult: ITestResult): boolean;

    procedure ExecuteTests(const context : ITestExecuteContext; const threadId: Cardinal; const fixture: ITestFixture);

    function ExecuteTest(const context: ITestExecuteContext; const threadId: cardinal; const test: ITest) : ITestResult;
    function ExecuteSuccessfulResult(const context: ITestExecuteContext; const threadId: cardinal; const test: ITest; const message: string = '') : ITestResult;
    function ExecuteFailureResult(const context: ITestExecuteContext; const threadId: cardinal; const test: ITest; const exception : Exception) : ITestError;
    function ExecuteWarningResult(const context: ITestExecuteContext; const threadId: cardinal; const test: ITest; const exception : Exception) : ITestResult;
    function ExecuteErrorResult(const context: ITestExecuteContext; const threadId: cardinal; const test: ITest; const exception : Exception) : ITestError;
    function ExecuteIgnoredResult(const context: ITestExecuteContext; const threadId: cardinal; const test: ITest; const ignoreReason : string) : ITestResult;


    function ExecuteTestTearDown(const context: ITestExecuteContext; const threadId: Cardinal; const fixture: ITestFixture; const test: ITest; out errorResult: ITestResult) : boolean;
    procedure ExecuteTearDownFixtureMethod(const context: ITestExecuteContext; const threadId: Cardinal; const fixture: ITestFixture);

    procedure RecordResult(const context: ITestExecuteContext; const threadId: cardinal; const testResult: ITestResult);

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
  DUnitX.TestResults,
  DUnitX.TestResult,
  DUnitX.Utils,
  TypInfo,
  StrUtils,
  Types,
  classes;

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

procedure TDUnitXTestRunner.Loggers_AddWarning(const threadId : Cardinal; const AWarning: ITestResult);
var
  logger : ITestLogger;
begin
  for logger in FLoggers do
  begin
    logger.OnTestWarning(threadId,AWarning);
  end;
end;

procedure TDUnitXTestRunner.AddStatus(const threadId; const msg: string);
begin

end;

function TDUnitXTestRunner.BuildFixtures  : IInterface;
var
  fixture : ITestFixture;
  parentFixture : ITestFixture;
  pair : TPair<string,TClass>;
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
      uName := pair.Value.UnitName;
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


      fixtureNamespace := fixtureNamespace + '.' + pair.Key;
      fixture := TDUnitXTestFixture.Create(fixtureNamespace, pair.Value);

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

constructor TDUnitXTestRunner.Create(const useCommandLineOptions: boolean; const AListener: ITestLogger);
begin
  FLoggers := TList<ITestLogger>.Create;
  if AListener <> nil then
    FLoggers.Add(AListener);
  FFixtureClasses := TDictionary<string,TClass>.Create;
  FUseCommandLine := useCommandLineOptions;
  FUseRTTI := False;
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
  FLoggers.Free;
  FFixtureClasses.Free;
  inherited;
end;

class destructor TDUnitXTestRunner.Destroy;
begin
  FActiveRunners.Free;
end;

procedure TDUnitXTestRunner.RecordResult(const context: ITestExecuteContext; const threadId: cardinal; const testResult: ITestResult);
begin
  case testResult.ResultType of
    Pass:
      begin
        context.RecordResult(testResult);
        Self.Loggers_AddSuccess(threadId, testResult);
      end;
    Failure:
      begin
        Log(TLogLevel.ltError, 'Test failed : ' + testResult.Test.Name + ' : ' + testResult.Message);
        context.RecordResult(testResult);
        Self.Loggers_AddFailure(threadId, ITestError(testResult));
      end;
    Warning:
      begin
        Log(TLogLevel.ltWarning, 'Test warning : ' + testResult.Test.Name + ' : ' + testResult.Message);
        context.RecordResult(testResult);
        Self.Loggers_AddWarning(threadId, testResult);
      end;
    Error:
      begin
        Log(TLogLevel.ltError, 'Test Error : ' + testResult.Test.Name + ' : ' + testResult.Message);
        context.RecordResult(testResult);
        Self.Loggers_AddError(threadId, ITestError(testResult));
      end;
    Ignored :
      begin
        Log(TLogLevel.ltError, 'Test Ignored : ' + testResult.Test.Name + ' : ' + testResult.Message);
        context.RecordResult(testResult);
        Self.Loggers_AddIgnored(threadId, testResult);
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
            if not FFixtureClasses.ContainsValue(TRttiInstanceType(rType).MetaclassType) then
              FFixtureClasses.Add(sName,TRttiInstanceType(rType).MetaclassType);
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
function TDUnitXTestRunner.Execute: ITestResults;
var
  fixtures : ITestFixtureList;
  fixture  : ITestFixture;
  test     : ITest;
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

  //TODO: Move to the fixtures class
  for fixture in fixtures do
    for test in fixture.Tests do
      Inc(testCount);

  //TODO: Need a simple way of converting one list to another list of a supported interface. Generics should help here.
  result := TDUnitXTestResults.Create(fixtures.AsFixtureInfoList);
  context := result as ITestExecuteContext;

  //TODO: Record Test metrics.. runtime etc.
  threadId := TThread.CurrentThread.ThreadID;
  Self.Loggers_TestingStarts(threadId, testCount, testActiveCount);
  try
    ExecuteFixtures(context, threadId, fixtures);
  finally
    //TODO: Actully pass the results for all fixtures and tests here.
    Self.Loggers_TestingEnds(result);
  end;
end;

function TDUnitXTestRunner.ExecuteErrorResult(
  const context: ITestExecuteContext; const threadId: cardinal;
  const test: ITest; const exception: Exception) : ITestError;
begin
  Result := TDUnitXTestError.Create(test as ITestInfo, TTestResultType.Error, exception, ExceptAddr, exception.Message);
end;

class function TDUnitXTestRunner.GetActiveRunner: ITestRunner;
begin
  result := nil;
  FActiveRunners.TryGetValue(TThread.CurrentThread.ThreadId,result)
end;

function TDUnitXTestRunner.ExecuteFailureResult(
  const context: ITestExecuteContext; const threadId: cardinal;
  const test: ITest; const exception : Exception) : ITestError;
begin
  //TODO: Does test failure require its own results interface and class?
  Result := TDUnitXTestError.Create(test as ITestInfo, TTestResultType.Failure, exception, ExceptAddr, exception.Message);
end;

procedure TDUnitXTestRunner.ExecuteFixtures(const context: ITestExecuteContext; const threadId: Cardinal; const fixtures: ITestFixtureList);
var
  fixture: ITestFixture;
  testResult : ITestResult;
begin
  for fixture in fixtures do
  begin
    if not fixture.Enabled then
      System.continue;

    Self.Loggers_StartTestFixture(threadId, fixture as ITestFixtureInfo);
    try
      if Assigned(fixture.SetupFixtureMethod) then
        //TODO: Errors from here need to be logged into each test below us
        ExecuteSetupFixtureMethod(threadId, fixture);

      ExecuteTests(context, threadId, fixture);

      if fixture.HasChildFixtures then
        ExecuteFixtures(context, threadId, fixture.Children);

      if Assigned(fixture.TearDownFixtureMethod) then
        //TODO: Tricker yet each test above us requires errors that occur here
        ExecuteTearDownFixtureMethod(context, threadId, fixture);

    finally
      //TODO: Actully pass the results for the fixture here
      Self.Loggers_EndTestFixture(threadId, nil);
    end;
  end;
end;

function TDUnitXTestRunner.ExecuteIgnoredResult(const context: ITestExecuteContext; const threadId: cardinal; const test: ITest; const ignoreReason: string): ITestResult;
begin
  result := TDUnitXTestResult.Create(test as ITestInfo, TTestResultType.Ignored, ignoreReason);
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

function TDUnitXTestRunner.ExecuteSuccessfulResult(const context: ITestExecuteContext; const threadId: cardinal; const test: ITest; const message: string) : ITestResult;
begin
  Result := TDUnitXTestResult.Create(test as ITestInfo, TTestResultType.Pass, message);
end;

procedure TDUnitXTestRunner.ExecuteTearDownFixtureMethod(
  const context: ITestExecuteContext; const threadId: Cardinal;
  const fixture: ITestFixture);
begin
  try
    Self.Loggers_TeardownFixture(threadId, fixture as ITestFixtureInfo);
    fixture.TearDownFixtureMethod;
  except
    on e: Exception do
    begin
      //TODO: ExecuteErrorResult(context, threadId, test, 'Test does not support ITestExecute');
    end;
  end;
end;

function TDUnitXTestRunner.ExecuteTest(const context: ITestExecuteContext; const threadId: cardinal; const test: ITest) : ITestResult;
var
  testExecute: ITestExecute;
  testResult: ITestResult;
begin
  if Supports(test, ITestExecute, testExecute) then
  begin
    Self.Loggers_ExecuteTest(threadId, test as ITestInfo);
    testExecute.Execute(context);
    Result := ExecuteSuccessfulResult(context, threadId, test);
  end
  else
  begin
    //This will be handled by the caller as a test error.
    raise Exception.CreateFmt('%s does not support ITestExecute', [test.Name]);
  end;
end;

procedure TDUnitXTestRunner.ExecuteTests(const context : ITestExecuteContext; const threadId: Cardinal; const fixture: ITestFixture);
var
  tests : IEnumerable<ITest>;
  test : ITest;
  testResult : ITestResult;
  setupResult : ITestResult;
  tearDownResult : ITestResult;
begin
  tests := fixture.Tests;
  for test in tests do
  begin
    if not test.Enabled then
      System.Continue;

    //Start a freash for this test. If we had an exception last execute of the
    //setup or tear down that may have changed this execution. Therefore we try
    //again to see if things have changed. Remember setup, test, tear down can
    //hold state
    setupResult := nil;
    testResult := nil;
    tearDownResult := nil;

    Self.Loggers_BeginTest(threadId, test as ITestInfo);

    //If the setup fails then we need to show this as the result.
    if Assigned(fixture.SetupMethod) and (not test.Ignored) then
      if not (ExecuteTestSetupMethod(context, threadId, fixture, test, setupResult)) then
        testResult := setupResult;

    try
      try
        if test.Ignored then
            testResult :=  ExecuteIgnoredResult(context,threadId,test,test.IgnoreReason)
       //If we haven't already failed, then run the test.
        else if testResult = nil then
           testResult := ExecuteTest(context, threadId, test);
        
      except
        //Handle the results which are raised in the test.
        on e: ETestPass do
          testResult := ExecuteSuccessfulResult(context, threadId, test, e.Message);
        on e: ETestFailure do
          testResult := ExecuteFailureResult(context, threadId, test, e);
        on e: ETestWarning do
          testResult := ExecuteWarningResult(context, threadId, test, e);
        on e: Exception do
          testResult := ExecuteErrorResult(context, threadId, test, e);
      end;

      //If the tear down fails then we need to show this as the test result.
      if Assigned(fixture.TearDownMethod) and (not test.Ignored) then
        if not (ExecuteTestTearDown(context, threadId, fixture, test, tearDownResult)) then
          testResult := tearDownResult;

    finally
      RecordResult(context, threadId, testResult);
      Self.Loggers_EndTest(threadId, testResult);
    end;
  end;
end;

function TDUnitXTestRunner.ExecuteTestSetupMethod(const context : ITestExecuteContext; const threadid: cardinal; const fixture: ITestFixture; const test: ITest; out errorResult: ITestResult): boolean;
begin
  Result := False;
  errorResult := nil;

  //Setup method is called before each test method.
  if Assigned(fixture.SetupMethod) then
  begin
    try
      Self.Loggers_SetupTest(threadId, test as ITestInfo);
      fixture.SetupMethod;
      Self.Loggers_EndSetupTest(threadId, test as ITestInfo);
      Result := True;
    except
      on e: SysUtils.Exception do
      begin
        errorResult := ExecuteErrorResult(context, threadId, test, e);
      end;
    end;
  end;
end;

function TDUnitXTestRunner.ExecuteTestTearDown(const context: ITestExecuteContext; const threadId: Cardinal; const fixture: ITestFixture; const test: ITest; out errorResult: ITestResult): boolean;
begin
  Result := False;
  errorResult := nil;

  try
    Self.Loggers_TeardownTest(threadId, test as ITestInfo);
    fixture.TearDownMethod;
    result := true;
  except
    on e: SysUtils.Exception do
    begin
      errorResult := ExecuteErrorResult(context, threadId, test, e);
    end;
  end;
end;

function TDUnitXTestRunner.ExecuteWarningResult(const context: ITestExecuteContext; const threadId: cardinal; const test: ITest; const exception: Exception) : ITestResult;
begin
  //TODO: Does test warning require its own results interface and class?
  result := TDUnitXTestResult.Create(test as ITestInfo, TTestResultType.Warning, exception.Message);
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

procedure TDUnitXTestRunner.Loggers_TestingEnds(const TestResult: ITestResults);
var
  logger : ITestLogger;
begin
  for logger in FLoggers do
    logger.OnTestingEnds(TestResult);
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
    for logger in FLoggers do
      logger.OnLog(logType,msg);
  end;
end;

end.
