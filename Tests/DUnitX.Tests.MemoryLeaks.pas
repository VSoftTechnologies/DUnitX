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

unit DUnitX.Tests.MemoryLeaks;

interface

uses
  DUnitX.TestFramework;

type
{$M+}
  [TestFixture]
  [IgnoreMemoryLeaks]
  TTestsNoMemoryLeaksReported = class
  published
    [Test]
    procedure No_Reporting_Of_Memory_Leaks;
    [Test]
    [IgnoreMemoryLeaks(False)]
    procedure Override_Reporting_Of_Memory_Leaks;
  end;

  [TestFixture]
  // if the "IgnoreMemoryLeaks" attribute is not specified, then all memory leaks will be reported.
  [IgnoreMemoryLeaks(False)]            // Report all leaks. This is the same as not specifying this attribute.
  TTestsMemoryLeaksReported = class
  published
    [Test]
    [IgnoreMemoryLeaks(True)]
    procedure Override_No_Reporting_Of_Memory_Leaks;

    [Test]
    // if the "IgnoreMemoryLeaks" attribute is not specified, it will use the fixtures setting.
    procedure Reporting_Of_Memory_Leaks;
  end;

  [TestFixture]
  TTestsLeakCheck = class
  public
    [Test]
    procedure Memory_Freed_By_A_Test_Is_Not_A_Leak;
    //Memory a test's setup or the test allocates and something frees between the phases (another thread, say) is
    //not a leak.  Here a logger frees it, from the callbacks that come between the phases.
    [Test]
    procedure Memory_Freed_Between_Phases_Is_Not_A_Leak;
    //What a logger keeps from the callbacks that come between a test's phases is the logger's, not the test's, and
    //what it frees is its own, so it cannot hide a real leak.
    [Test]
    procedure Logger_State_Is_Not_A_Leak;
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
  System.Rtti,
  System.TypInfo,
  DUnitX.Extensibility,
  DUnitX.InternalInterfaces,
  DUnitX.Loggers.Null,
  DUnitX.MemoryLeakMonitor.Default,
  DUnitX.RunResults,
  DUnitX.ServiceLocator,
  DUnitX.TestFixture,
  DUnitX.TestRunner;

var
  //The heap that TCountingMonitor measures, so a test can say exactly what is allocated and when.
  GCounted : Int64;
  //What TLeakRunFixture's setup and its Passes test allocate.
  GSetupAllocates : Int64;
  GTestAllocates : Int64;
  //What TGapFreeingLogger frees between the phases.
  GFreeBeforeTest : Int64;
  GFreeBeforeTearDown : Int64;

type
  TLeakCheckRunnerAccess = class(TDUnitXTestRunner);

  //Reports fixed memory growth for each phase of a test.
  TFixedGrowthMonitor = class(TInterfacedObject, IMemoryLeakMonitor)
  private
    FSetUp : Int64;
    FTest : Int64;
    FTearDown : Int64;
  public
    constructor Create(const ASetUp, ATest, ATearDown : Int64);
    procedure PreSetup;
    procedure PostSetUp;
    procedure PreTest;
    procedure PostTest;
    procedure PreTearDown;
    procedure PostTearDown;
    function SetUpMemoryAllocated : Int64;
    function TearDownMemoryAllocated : Int64;
    function TestMemoryAllocated : Int64;
  end;

  TLeakCheckClass = class
  public
    procedure Method;
  end;

  //Measures GCounted instead of the heap.
  TCountingMonitor = class(TInterfacedObject, IMemoryLeakMonitor)
  private
    FPreSetup : Int64;
    FPostSetup : Int64;
    FPreTest : Int64;
    FPostTest : Int64;
    FPreTearDown : Int64;
    FPostTearDown : Int64;
  public
    procedure PreSetup;
    procedure PostSetUp;
    procedure PreTest;
    procedure PostTest;
    procedure PreTearDown;
    procedure PostTearDown;
    function SetUpMemoryAllocated : Int64;
    function TearDownMemoryAllocated : Int64;
    function TestMemoryAllocated : Int64;
  end;

  //Run by the leak check tests, measured by TCountingMonitor.
  TLeakRunFixture = class
  public
    procedure Setup;
    procedure TearDown;
    procedure Passes;
    procedure LeaksForReal;
  end;

  //Frees what TLeakRunFixture's setup allocated as the test starts, and what the test allocated as teardown starts.
  TGapFreeingLogger = class(TDUnitXNullLogger)
  protected
    procedure OnExecuteTest(const threadId : TThreadID; const Test : ITestInfo); override;
    procedure OnTeardownTest(const threadId : TThreadID; const Test : ITestInfo); override;
  end;

  //Keeps 40 bytes as setup ends, and 40 more as the test starts, which it frees as teardown starts.
  TStateKeepingLogger = class(TDUnitXNullLogger)
  protected
    procedure OnEndSetupTest(const threadId : TThreadID; const Test : ITestInfo); override;
    procedure OnExecuteTest(const threadId : TThreadID; const Test : ITestInfo); override;
    procedure OnTeardownTest(const threadId : TThreadID; const Test : ITestInfo); override;
  end;

{ TTestsNoMemoryLeaksReported }

procedure TTestsNoMemoryLeaksReported.No_Reporting_Of_Memory_Leaks;
begin
  AllocMem(1024);
  Assert.Pass;
end;

procedure TTestsNoMemoryLeaksReported.Override_Reporting_Of_Memory_Leaks;
begin
  AllocMem(512);
  Assert.Pass;
end;

{ TTestsMemoryLeaksReported }

procedure TTestsMemoryLeaksReported.Override_No_Reporting_Of_Memory_Leaks;
begin
  AllocMem(256);
  Assert.Pass;
end;

procedure TTestsMemoryLeaksReported.Reporting_Of_Memory_Leaks;
begin
  AllocMem(128);
  Assert.Pass;
end;

{ TFixedGrowthMonitor }

constructor TFixedGrowthMonitor.Create(const ASetUp, ATest, ATearDown : Int64);
begin
  inherited Create;
  FSetUp := ASetUp;
  FTest := ATest;
  FTearDown := ATearDown;
end;

procedure TFixedGrowthMonitor.PreSetup;
begin
end;

procedure TFixedGrowthMonitor.PostSetUp;
begin
end;

procedure TFixedGrowthMonitor.PreTest;
begin
end;

procedure TFixedGrowthMonitor.PostTest;
begin
end;

procedure TFixedGrowthMonitor.PreTearDown;
begin
end;

procedure TFixedGrowthMonitor.PostTearDown;
begin
end;

function TFixedGrowthMonitor.SetUpMemoryAllocated : Int64;
begin
  result := FSetUp;
end;

function TFixedGrowthMonitor.TearDownMemoryAllocated : Int64;
begin
  result := FTearDown;
end;

function TFixedGrowthMonitor.TestMemoryAllocated : Int64;
begin
  result := FTest;
end;

{ TLeakCheckClass }

procedure TLeakCheckClass.Method;
begin
end;

{ TCountingMonitor }

procedure TCountingMonitor.PreSetup;
begin
  FPreSetup := GCounted;
end;

procedure TCountingMonitor.PostSetUp;
begin
  FPostSetup := GCounted;
end;

procedure TCountingMonitor.PreTest;
begin
  FPreTest := GCounted;
end;

procedure TCountingMonitor.PostTest;
begin
  FPostTest := GCounted;
end;

procedure TCountingMonitor.PreTearDown;
begin
  FPreTearDown := GCounted;
end;

procedure TCountingMonitor.PostTearDown;
begin
  FPostTearDown := GCounted;
end;

function TCountingMonitor.SetUpMemoryAllocated : Int64;
begin
  result := FPostSetup - FPreSetup;
end;

function TCountingMonitor.TearDownMemoryAllocated : Int64;
begin
  result := FPostTearDown - FPreTearDown;
end;

function TCountingMonitor.TestMemoryAllocated : Int64;
begin
  result := FPostTest - FPreTest;
end;

{ TLeakRunFixture }

procedure TLeakRunFixture.Setup;
begin
  Inc(GCounted, GSetupAllocates);
end;

procedure TLeakRunFixture.TearDown;
begin
end;

procedure TLeakRunFixture.Passes;
begin
  Inc(GCounted, GTestAllocates);
  Assert.IsTrue(true);
end;

procedure TLeakRunFixture.LeaksForReal;
begin
  Inc(GCounted, 32);
  Assert.IsTrue(true);
end;

{ TGapFreeingLogger }

procedure TGapFreeingLogger.OnExecuteTest(const threadId : TThreadID; const Test : ITestInfo);
begin
  Dec(GCounted, GFreeBeforeTest);
end;

procedure TGapFreeingLogger.OnTeardownTest(const threadId : TThreadID; const Test : ITestInfo);
begin
  Dec(GCounted, GFreeBeforeTearDown);
end;

{ TStateKeepingLogger }

procedure TStateKeepingLogger.OnEndSetupTest(const threadId : TThreadID; const Test : ITestInfo);
begin
  Inc(GCounted, 40);
end;

procedure TStateKeepingLogger.OnExecuteTest(const threadId : TThreadID; const Test : ITestInfo);
begin
  Inc(GCounted, 40);
end;

procedure TStateKeepingLogger.OnTeardownTest(const threadId : TThreadID; const Test : ITestInfo);
begin
  Dec(GCounted, 40);
end;

//Runs the named tests of TLeakRunFixture, with its setup and teardown when AWithSetup, measured by TCountingMonitor,
//and returns '<test>=<result>: <message>' for each, one per line.
function RunLeakFixture(const ATests : array of string; const AWithSetup : boolean; const ALogger : ITestLogger) : string;
var
  thread : TThread;
  testNames : TArray<string>;
  i : integer;
  outcome : string;
  failure : string;
begin
  SetLength(testNames, Length(ATests));
  for i := 0 to High(ATests) do
    testNames[i] := ATests[i];
  outcome := '';
  failure := '';
  //The registration is replaced and then restored to the default, so a suite built with another monitor skips this.
  if not (TDUnitXServiceLocator.DefaultContainer.Resolve<IMemoryLeakMonitor>() as TObject is TDUnitXDefaultMemoryLeakMonitor) then
    Assert.Ignore('Needs the default memory leak monitor');
  TDUnitXServiceLocator.DefaultContainer.RegisterType<IMemoryLeakMonitor>(
    function : IMemoryLeakMonitor
    begin
      result := TCountingMonitor.Create;
    end);
  try
    thread := TThread.CreateAnonymousThread(
      procedure
      var
        runner : TDUnitXTestRunner;
        runnerIntf : ITestRunner;
        rttiContext : TRttiContext;
        rttiType : TRttiType;
        fixture : ITestFixture;
        instance : TLeakRunFixture;
        fixtures : ITestFixtureList;
        results : IRunResults;
        context : ITestExecuteContext;
        testResult : ITestResult;
        testName : string;
        method : TMethod;
      begin
        try
          runner := TDUnitXTestRunner.Create;
          runnerIntf := runner;
          runnerIntf.AddLogger(ALogger);
          rttiContext := TRttiContext.Create;
          rttiType := rttiContext.GetType(TLeakRunFixture);
          fixture := TDUnitXTestFixture.Create('ALeakRunFixture', '', TLeakRunFixture, 'AUnit') as ITestFixture;
          instance := fixture.FixtureInstance as TLeakRunFixture;
          if AWithSetup then
          begin
            fixture.SetSetupTestMethod('Setup', instance.Setup);
            fixture.SetTearDownTestMethod('TearDown', instance.TearDown);
          end;
          for testName in testNames do
          begin
            method.Code := rttiType.GetMethod(testName).CodeAddress;
            method.Data := instance;
            fixture.AddTest(testName, TTestMethod(method), testName, '', rttiType.GetMethod(testName));
          end;
          fixtures := TTestFixtureList.Create;
          fixtures.Add(fixture);
          results := TDUnitXRunResults.Create;
          context := results as ITestExecuteContext;
          TLeakCheckRunnerAccess(runner).ExecuteFixtures(nil, context, TThread.CurrentThread.ThreadID, fixtures);
          for testResult in results.GetAllTestResults do
            outcome := outcome + testResult.Test.Name + '=' + GetEnumName(TypeInfo(TTestResultType), Ord(testResult.ResultType)) + ': ' +
              testResult.Message + sLineBreak;
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
  finally
    //The suite runs with the default monitor; put it back.
    DUnitX.MemoryLeakMonitor.Default.RegisterDefaultProvider;
  end;
  Assert.AreEqual('', failure, 'the run itself raised');
  result := outcome;
end;

//Resets what TLeakRunFixture and the loggers allocate and free.
procedure ResetLeakRun(const ASetupAllocates, ATestAllocates, AFreeBeforeTest, AFreeBeforeTearDown : Int64);
begin
  GCounted := 0;
  GSetupAllocates := ASetupAllocates;
  GTestAllocates := ATestAllocates;
  GFreeBeforeTest := AFreeBeforeTest;
  GFreeBeforeTearDown := AFreeBeforeTearDown;
end;

{ TTestsLeakCheck }

//Runs the runner's leak check for the given growth. A runner registers itself for its thread, so it gets a thread of its own.
function LeakCheckPasses(const ASetUp, ATest, ATearDown : Int64; out AMessage : string) : boolean;
var
  thread : TThread;
  passed : boolean;
  msg : string;
  failure : string;
begin
  passed := false;
  msg := '';
  failure := '';
  thread := TThread.CreateAnonymousThread(
    procedure
    var
      runner : TDUnitXTestRunner;
      runnerIntf : ITestRunner;
      rttiContext : TRttiContext;
      fixture : ITestFixture;
      test : ITest;
      errorResult : ITestResult;
    begin
      try
        runner := TDUnitXTestRunner.Create;
        runnerIntf := runner;
        rttiContext := TRttiContext.Create;
        fixture := TDUnitXTestFixture.Create('ALeakCheckFixture', '', TLeakCheckClass, 'AUnit') as ITestFixture;
        test := fixture.AddTest('Method', TLeakCheckClass(fixture.FixtureInstance).Method, 'Method', '',
          rttiContext.GetType(TLeakCheckClass).GetMethod('Method'));
        passed := TLeakCheckRunnerAccess(runner).CheckMemoryAllocations(test, errorResult,
          TFixedGrowthMonitor.Create(ASetUp, ATest, ATearDown));
        if errorResult <> nil then
          msg := errorResult.Message;
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
  Assert.AreEqual('', failure, 'the leak check itself raised');
  AMessage := msg;
  result := passed;
end;

procedure TTestsLeakCheck.Memory_Freed_By_A_Test_Is_Not_A_Leak;
var
  msg : string;
begin
  Assert.IsTrue(LeakCheckPasses(0, -64, 0, msg), 'the test freed 64 bytes allocated before it: ' + msg);
  Assert.IsTrue(LeakCheckPasses(-32, 0, -32, msg), 'setup and teardown freed memory: ' + msg);
  Assert.IsTrue(LeakCheckPasses(64, -64, 0, msg), 'the test freed what its setup allocated: ' + msg);
  Assert.IsFalse(LeakCheckPasses(0, 64, 0, msg), 'a test that keeps 64 bytes still leaks');
  Assert.IsFalse(LeakCheckPasses(-64, 128, 0, msg), 'a net growth of 64 bytes still leaks');
end;

procedure TTestsLeakCheck.Memory_Freed_Between_Phases_Is_Not_A_Leak;
var
  outcome : string;
begin
  //Setup allocates 500, freed before the test starts; the test allocates 1000, freed before teardown starts.
  ResetLeakRun(500, 1000, 500, 1000);
  outcome := RunLeakFixture(['Passes'], true, TGapFreeingLogger.Create);
  Assert.AreEqual('Passes=Pass: ' + sLineBreak, outcome);
  //Setup's 500, freed before the test starts, is not part of a real leak.
  ResetLeakRun(500, 0, 500, 0);
  outcome := RunLeakFixture(['LeaksForReal'], true, TGapFreeingLogger.Create);
  Assert.StartsWith('LeaksForReal=MemoryLeak: 32 bytes were leaked', outcome);
end;

procedure TTestsLeakCheck.Logger_State_Is_Not_A_Leak;
var
  outcome : string;
begin
  ResetLeakRun(0, 0, 0, 0);
  outcome := RunLeakFixture(['Passes', 'LeaksForReal'], true, TStateKeepingLogger.Create);
  Assert.AreEqual('Passes=Pass: ' + sLineBreak + 'LeaksForReal=MemoryLeak: 32 bytes were leaked in the test method' +
    sLineBreak, outcome);
end;

initialization
  TDUnitX.RegisterTestFixture(TTestsNoMemoryLeaksReported);
  TDUnitX.RegisterTestFixture(TTestsMemoryLeaksReported);
  TDUnitX.RegisterTestFixture(TTestsLeakCheck);

end.

