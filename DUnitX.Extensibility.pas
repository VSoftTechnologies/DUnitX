{***************************************************************************}
{                                                                           }
{           DUnitX                                                          }
{                                                                           }
{           Copyright (C) 2017 Vincent Parrett & Contributors               }
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

unit DUnitX.Extensibility;

interface

{$I DUnitX.inc}

uses
  {$IFDEF USE_NS}
  System.TimeSpan,
  System.Rtti,
  System.Generics.Collections,
  System.SysUtils,
  {$ELSE}
  TimeSpan,
  Rtti,
  Generics.Collections,
  SysUtils,
  {$ENDIF}
  DUnitX.Types,
  DUnitX.Generics;

type
  TTestMethod = procedure of object;


  //These interfaces mirror the Info classes in the framework but expose stuff we need for runtime.
  ITestFixture = interface;

  ///
  ///  Describes the Test at runtime.
  ///
  ITest = interface
    ['{0CCCE0C7-9AD1-4C3A-86EF-E882D3A839AB}']
    function GetName : string;
    function GetFullName : string;
    function GetMethodName : string;
    function GetCategories : TList<string>;
    function GetTestMethod : TTestMethod;
    function GetTestFixture : ITestFixture;
    function GetTestStartTime : TDateTime;
    function GetTestEndTime : TDateTime;
    function GetTestDuration : TTimeSpan;
    function GetEnabled : boolean;
    procedure SetEnabled(const value : boolean);
    function GetIgnored : boolean;
    function GetIgnoreReason : string;
    function GetIgnoreMemoryLeaks() : Boolean;
    procedure SetIgnoreMemoryLeaks(const AValue : Boolean);
    function GetMaxTime: cardinal;
    procedure SetMaxTime(const AValue: cardinal);
    function GetTimedOut: Boolean;
    procedure SetTimedOut(const AValue: Boolean);

    property Name : string read GetName;
    property FullName : string read GetFullName;
    property MethodName : string read GetMethodName;
    property Categories : TList<string> read GetCategories;
    property Enabled : boolean read GetEnabled write SetEnabled;
    property Fixture : ITestFixture read GetTestFixture;
    property Ignored : boolean read GetIgnored;
    property IgnoreReason : string read GetIgnoreReason;
    property TestMethod : TTestMethod read GetTestMethod;
    property IgnoreMemoryLeaks : Boolean read GetIgnoreMemoryLeaks write SetIgnoreMemoryLeaks;
    property MaxTime: cardinal read GetMaxTime write SetMaxTime;
    property TimedOut: Boolean read GetTimedOut write SetTimedOut;
  end;

  ITestList = interface(IList<ITest>)
    ['{83ABC05F-5762-4FD2-9791-E32F5A9A4D06}']
  end;

  ITestFixtureList = interface;

  ///
  ///  Describes the Test Fixture at runtime.
  ///
  ITestFixture = interface
    ['{B2F140C3-1D6A-4C09-B4C6-0D6AFC99BC87}']
    function GetName  : string;
    function GetFullName : string;
    function GetUnitName : string;
    function GetDescription : string;
    function GetCategories : TList<string>;
    function GetTests : ITestList;
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
    function GetEnabled : boolean;
    procedure SetEnabled(const value : boolean);
    function GetChildren : ITestFixtureList;
    function GetHasChildren : boolean;
    function GetNameSpace : string;
    function GetHasTests : boolean;
    function GetHasChildTests : boolean;
    function IsNameSpaceOnly : boolean;
    procedure OnMethodExecuted(const AMethod : TTestMethod);
    function GetFixtureInstance : TObject;

    function AddTest(const AMethodName : string; const AMethod : TTestMethod; const AName : string; const ACategory : string; const AEnabled : boolean = true;const AIgnored : boolean = false; const AIgnoreReason : string = ''; const AMaxTime :cardinal = 0; AExpectedException: ExceptClass = nil; const AExceptionInheritance: TExceptionInheritance = exExact) : ITest;
    function AddTestCase(const AMethodName : string; const ACaseName : string; const AName : string; const ACategory : string; const AMethod : TRttiMethod; const AEnabled : boolean; const AArgs : TValueArray) : ITest;

    function AddChildFixture(const ATestClass : TClass; const AName : string; const ACategory : string) : ITestFixture;overload;
    function AddChildFixture(const AInstance : TObject; const AName : string; const ACategory : string) : ITestFixture;overload;

    procedure SetSetupTestMethod(const AMethodName : string; const AMethod : TTestMethod);
    procedure SetSetupFixtureMethod(const AMethodName : string; const AMethod : TTestMethod);
    procedure SetTearDownTestMethod(const AMethodName : string; const AMethod : TTestMethod);
    procedure SetTearDownFixtureMethod(const AMethodName : string; const AMethod : TTestMethod; const AIsDestructor : boolean);
    procedure SetTestInOwnThread(const value : boolean);

    procedure ExecuteFixtureTearDown;
    procedure InitFixtureInstance;

    property Name                       : string read GetName;
    property NameSpace                  : string read GetNameSpace;
    property FullName                   : string read GetFullName;
    property UnitName                   : string read GetUnitName;
    property Categories                 : TList<string> read GetCategories;
    property Children                   : ITestFixtureList read GetChildren;
    property Description                : string read GetDescription;
    property Enabled                    : boolean read GetEnabled write SetEnabled;
    property FixtureInstance            : TObject read GetFixtureInstance;
    property HasChildFixtures           : boolean read GetHasChildren;
    property HasTests                   : boolean read GetHasTests;
    property HasChildTests              : boolean read GetHasChildTests;
    property TestClass                  : TClass read GetTestClass;
    property Tests                      : ITestList read GetTests;
    property SetupMethod                : TTestMethod read GetSetupMethod;
    property SetupMethodName            : string read GetSetupMethodName;
    property SetupFixtureMethod         : TTestMethod read GetSetupFixtureMethod;
    property SetupFixtureMethodName     : string read GetSetupFixtureMethodName;
    property TearDownMethod             : TTestMethod read GetTearDownMethod;
    property TearDownMethodName         : string read GetTearDownMethodName;
    property TearDownFixtureMethod      : TTestMethod read GetTearDownFixtureMethod;
    property TearDownFixtureMethodName  : string read GetTearDownFixtureMethodName;
    property TestInOwnThread            : boolean read GetTestInOwnThread write SetTestInOwnThread;
  end;

  ITestFixtureList = interface(IList<ITestFixture>)
    ['{BB78BD03-4818-4CF4-B40C-AD037DF2EFB9}']
  end;

  TTestList = class(TDUnitXList<ITest>, ITestList)
  end;

  TTestFixtureList = class(TDUnitXList<ITestFixture>, ITestFixtureList)
  end;



  IFixtureProviderContext = interface
    ['{933F8442-77F1-4574-BB5E-2F3D0B8E6E6F}']
    function CreateFixture(const AFixtureClass : TClass; const AName : string; const ACategory : string) : ITestFixture;overload;
    function CreateFixture(const AInstance : TObject; const AName : string; const ACategory : string) : ITestFixture;overload;
    function GetUseRtti : boolean;
    //The runner UseRtti property exposed for plugin use.
    property UseRtti : boolean read GetUseRtti;
  end;


  IFixtureProvider = interface
    ['{48D58C88-1236-4B96-9D5F-5DD81DB71504}']
    procedure Execute(const context : IFixtureProviderContext);
  end;


  IPluginLoadContext = interface
    ['{0A60FE65-C3E3-4E98-9686-8BB6A793810B}']
    procedure RegisterFixtureProvider(const provider : IFixtureProvider);
    //procedure RegisterSomeOtherFeature(const provider : ISomeOtherFeature);
  end;


  IPlugin = interface
    ['{C706DD67-58D0-4B66-92A6-6FBE2AF065A4}']
    procedure GetPluginFeatures(const context : IPluginLoadContext);
  end;


  IFixtureFilter = interface
    ['{0FBC270E-2DC0-4135-8724-C2AD567A009A}']
    procedure InitFromOptions(const ARun : string; const AInclude : string; const AExclude : string);
  end;



implementation

end.
