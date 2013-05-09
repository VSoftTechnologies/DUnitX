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

unit DUnitX.InternalInterfaces;

interface

uses
  TimeSpan,
  DUnitX.TestFrameWork;

{$I DUnitX.inc}

type
  //NOTE : Do not use interface inheritence here, will cause problems
  //with GetTestFixture;

  //These interfaces mirror the Info classes in the framework but
  //expose stuff we need for runtime.

  ITestFixture = interface;

  ///
  ///  Describes the Test at runtime.
  ///
  ITest = interface
    ['{0CCCE0C7-9AD1-4C3A-86EF-E882D3A839AB}']
    function GetName : string;
    function GetTestMethod : TTestMethod;
    function GetTestFixture : ITestFixture;
    function GetTestStartTime : TDateTime;
    function GetTestEndTime : TDateTime;
    function GetTestDuration : TTimeSpan;
    property Name : string read GetName;
    property Fixture : ITestFixture read GetTestFixture;
    property TestMethod : TTestMethod read GetTestMethod;
  end;


  ///
  ///  Describes the Test Fixture at runtime.
  ///
  ITestFixture = interface
    ['{B2F140C3-1D6A-4C09-B4C6-0D6AFC99BC87}']
    function GetName  : string;
    function GetTests : IEnumerable<ITest>;
    function GetTestClass : TClass;
    function GetSetupMethod : TTestMethod;
    function GetSetupMethodName : string;
    function GetSetupFixtureMethod : TTestMethod;
    function GetSetupFixtureMethodName : string;
    function GetTearDownMethod : TTestMethod;
    function GetTearDownMethodName : string;
    function GetTearDownFixtureMethod : TTestMethod;
    function GetTearDownFixtureMethodName : string;
    function GetTestInOwnThread : boolean;

    property Name                       : string read GetName;
    property TestClass                  : TClass read GetTestClass;
    property Tests                      : IEnumerable<ITest> read GetTests;
    property SetupMethod                : TTestMethod read GetSetupMethod;
    property SetupMethodName            : string read GetSetupMethodName;
    property SetupFixtureMethod         : TTestMethod read GetSetupFixtureMethod;
    property SetupFixtureMethodName     : string read GetSetupFixtureMethodName;
    property TearDownMethod             : TTestMethod read GetTearDownMethod;
    property TearDownMethodName         : string read GetTearDownMethodName;
    property TearDownFixtureMethod      : TTestMethod read GetTearDownFixtureMethod;
    property TearDownFixtureMethodName  : string read GetTearDownFixtureMethodName;
    property TestInOwnThread            : boolean read GetTestInOwnThread;
  end;

  ISetTestResult = interface
  ['{B50D50E9-3609-40BF-847D-53B5BF19B5C7}']
    procedure SetResult(const value : ITestResult);
  end;

  //Used by the TestExecute method.
  ITestExecuteContext = interface
    ['{DE4ADB3F-3B5B-4B90-8659-0BFA578977CC}']
    procedure RecordResult(const testResult : ITestResult);
  end;


  ITestFixtureContext = interface
    ['{C3B85C73-1FE8-4558-8AB0-7E8075821D35}']
  end;


  ITestExecute = interface
  ['{C59443A9-8C7D-46CE-83A1-E40309A1B384}']
    procedure Execute(const context : ITestExecuteContext);
  end;

  ITestCaseExecute = interface(ITestExecute)
    ['{49781E22-C127-4BED-A9D5-84F9AAACE96C}']
    function GetCaseName : string;
  end;

implementation

end.
