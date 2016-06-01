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

unit DUnitX.InternalInterfaces;

interface

{$I DUnitX.inc}

uses
  {$IFDEF USE_NS}
  System.TimeSpan,
  {$ELSE}
  TimeSpan,
  {$ENDIF}
  DUnitX.Generics,
  DUnitX.Extensibility,
  DUnitX.TestFrameWork;

type
  //These interfaces mirror the Info classes in the framework but expose stuff we need for runtime.

  ISetTestResult = interface
    ['{B50D50E9-3609-40BF-847D-53B5BF19B5C7}']
    procedure SetResult(const value : ITestResult);
  end;

  //Used by the TestExecute method.
  ITestExecuteContext = interface
    ['{DE4ADB3F-3B5B-4B90-8659-0BFA578977CC}']
    procedure RecordFixture(const fixtureResult : IFixtureResult);
    procedure RecordResult(const fixtureResult : IFixtureResult; const testResult : ITestResult);
    procedure RollupResults;
  end;

  ITestFixtureContext = interface
    ['{C3B85C73-1FE8-4558-8AB0-7E8075821D35}']
  end;

  ITestExecute = interface
    ['{C59443A9-8C7D-46CE-83A1-E40309A1B384}']
    procedure Execute(const context : ITestExecuteContext);
    procedure UpdateInstance(const fixtureInstance : TObject);
  end;

  ITestCaseExecute = interface(ITestExecute)
    ['{49781E22-C127-4BED-A9D5-84F9AAACE96C}']
    function GetCaseName : string;
  end;

  IFixtureResultBuilder = interface
    ['{2604E655-349D-4379-9796-1C708CAD7307}']
    procedure AddTestResult(const AResult : ITestResult);
    procedure AddChild(const AFixtureResult : IFixtureResult);
    procedure RollUpResults;
   // function Combine(const AFixtureResult : IFixtureResult) : IFixtureResult;
   // function AreEqual(const AFixtureResult : IFixtureResult) : boolean;
  end;

implementation

end.
