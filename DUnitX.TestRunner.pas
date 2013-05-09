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

unit DUnitX.TestRunner;

interface

uses
  DUnitX.TestFramework,
  Generics.Collections,
  DUnitX.InternalInterfaces,
  DUnitX.Generics,
  DUnitX.WeakReference,
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
  protected
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

    procedure Loggers_EndTest(const threadId : Cardinal; const Test: ITestResult);
    procedure Loggers_TeardownTest(const threadId : Cardinal; const Test: ITestInfo);

    procedure Loggers_TeardownFixture(const threadId : Cardinal; const fixture : ITestFixtureInfo);

    procedure Loggers_EndTestFixture(const threadId : Cardinal; const results : IFixtureResult);


    procedure Loggers_TestingEnds(const TestResult: ITestResults);

    //ITestRunner;
    procedure AddLogger(const value: ITestLogger);
    function Execute: ITestResults;
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
    function BuildFixtures : IList<ITestFixture>;
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
  DUnitX.TestResult,
  TypInfo,
  SysUtils,
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
    logger.OnTestError(threadId,Error);
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

function TDUnitXTestRunner.BuildFixtures  : IList<ITestFixture>;
var
  fixture : ITestFixture;
  pair : TPair<string,TClass>;
begin
  result := TDUnitXList<ITestFixture>.Create;

  if FUseRTTI then
    RTTIDiscoverFixtureClasses;

  for pair in TDUnitX.RegisteredFixtures do
  begin
    if not FFixtureClasses.ContainsValue(pair.Value) then
      FFixtureClasses.AddOrSetValue(pair.Key, pair.Value);
  end;

  for pair in FFixtureClasses do
  begin
    fixture := TDUnitXTestFixture.Create(pair.Key, pair.Value);
    result.Add(fixture);
  end;
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
  FUseRTTI := True;
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
              FFixtureClasses.Add(TRttiInstanceType(rType).MetaclassType.ClassName,TRttiInstanceType(rType).MetaclassType);
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
  fixtures : IList<ITestFixture>;
  fixture  : ITestFixture;
  tests    : IEnumerable<ITest>;
  test     : ITest;
  testExecute : ITestExecute;
  context : ITestExecuteContext;

  threadId : Cardinal;
  testResult : ITestResult;
  testError : ITestError;

  testCount : Cardinal;
  testActiveCount : Cardinal;
begin
  result := nil;
  fixtures := BuildFixtures;
  if fixtures.Count = 0 then
    raise ENoTestsRegistered.Create('No Test Fixtures found');

  testCount := 0;
  //TODO: Count the active tests that we have.
  testActiveCount := 0;

  //TODO: Move to the fixtures class
  for fixture in fixtures do
    for test in fixture.Tests do
      Inc(testCount);

  //TODO: Record Test metrics.. runtime etc.
  threadId := TThread.CurrentThread.ThreadID;
  Self.Loggers_TestingStarts(threadId, testCount, testActiveCount);
  try
    for fixture in fixtures do
    begin
      Self.Loggers_StartTestFixture(threadId,fixture as ITestFixtureInfo);
      try
        if Assigned(fixture.SetupFixtureMethod)  then
        begin
          try
            Self.Loggers_SetupFixture(threadid, fixture as ITestFixtureInfo);
            fixture.SetupFixtureMethod;
            Self.Loggers_EndSetupFixture(threadid, fixture as ITestFixtureInfo);
          except
            on e : Exception do
            begin
              Log(TLogLevel.ltError,'Error in Fixture SetupError : ' + fixture.Name + ' : ' + e.Message);
              Log(TLogLevel.ltError,'Skipping Fixture.');
              System.Continue;
            end;
          end;
        end;

        try
          tests := fixture.Tests;
          for test in tests do
          begin
            testResult := nil;
            testError := nil;

            Self.Loggers_BeginTest(threadId,test as ITestInfo);
            //Setup method is called before each test method.
            if Assigned(fixture.SetupMethod)  then
            begin
              try
                Self.Loggers_SetupTest(threadId,test as ITestInfo);
                fixture.SetupMethod;
                Self.Loggers_EndSetupTest(threadId,test as ITestInfo);
              except
                on e : Exception do
                begin
                  testResult := TDUnitXTestResult.Create(test as ITestInfo, TTestResultType.Error, e.Message);
                  Log(TLogLevel.ltError,'Error running test Setup method : ' + e.Message);
                  Log(TLogLevel.ltError,'Skipping test.');
                  System.Continue;
                end;
              end;
            end;

            try
              try
                if Supports(test, ITestExecute, testExecute) then
                begin
                  Self.Loggers_ExecuteTest(threadId, test as ITestInfo);
                  testExecute.Execute(context);
                  testResult := TDUnitXTestResult.Create(test as ITestInfo, TTestResultType.Success);
                  Self.Loggers_AddSuccess(threadId, testResult);
                end;
              except
                on e : ETestFailure do
                begin
                  //TODO: Does test failure require its own results interface and class?
                  Log(TLogLevel.ltError, 'Test failed : ' + test.Name + ' : ' + e.Message);
                  testError := TDUnitXTestError.Create(test as ITestInfo, TTestResultType.Failure, e, ExceptAddr);
                  Self.Loggers_AddFailure(threadId, testError);
                end;
                on e : ETestWarning do
                begin
                  //TODO: Does test warning require its own results interface and class?
                  Log(TLogLevel.ltWarning, 'Test warning : ' + test.Name + ' : ' + e.Message);
                  testResult := TDUnitXTestResult.Create(test as ITestInfo, TTestResultType.Warning, e.Message);
                  Self.Loggers_AddWarning(threadId, testResult);
                end;
                on e : Exception do
                begin
                  Log(TLogLevel.ltError, 'Test Error : ' + test.Name + ' : ' + e.Message);
                  testError := TDUnitXTestError.Create(test as ITestInfo, TTestResultType.Error, e, ExceptAddr);
                  Self.Loggers_AddError(threadId, testError);
                end;
              end;

              if Assigned(fixture.TearDownMethod)  then
              begin
                try
                  Self.Loggers_TeardownTest(threadId, test as ITestInfo);
                  fixture.TearDownMethod;
                except
                  //TODO: Report test tear down exceptions to the user
                end;
              end;
            finally
              Self.Loggers_EndTest(threadId,nil);
            end;
          end;

        except
          //TODO: Report system failures to user
          //TODO: Remove raise, however for the moment while testing I need to see these.

        end;

        if Assigned(fixture.TearDownFixtureMethod)  then
        begin
          try
            Self.Loggers_TeardownFixture(threadId,fixture as ITestFixtureInfo);
            fixture.TearDownFixtureMethod;
          except
            on e : Exception do
            begin
               //TODO: Report fixture tear down exceptions to the user
               //TODO: Remove raise, however for the moment while testing I need to see these.

            end;
          end;
        end;

      finally
        Self.Loggers_EndTestFixture(threadId,nil);
      end;
    end;
  finally
    //TODO: need test results.
    Self.Loggers_TestingEnds(nil);
  end;
end;

class function TDUnitXTestRunner.GetActiveRunner: ITestRunner;
begin
  result := nil;
  FActiveRunners.TryGetValue(TThread.CurrentThread.ThreadId,result)
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
