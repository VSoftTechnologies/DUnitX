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
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
  System.Rtti,
  DUnitX.Extensibility,
  DUnitX.TestFixture,
  DUnitX.TestRunner;

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

initialization
  TDUnitX.RegisterTestFixture(TTestsNoMemoryLeaksReported);
  TDUnitX.RegisterTestFixture(TTestsMemoryLeaksReported);
  TDUnitX.RegisterTestFixture(TTestsLeakCheck);

end.

