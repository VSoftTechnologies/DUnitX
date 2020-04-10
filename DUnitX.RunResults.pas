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

unit DUnitX.RunResults;

interface

{$I DUnitX.inc}

uses
  {$IFDEF USE_NS}
  System.Classes,
  System.TimeSpan,
  System.Diagnostics,
  System.Generics.Collections,
  {$ELSE}
  Classes,
  TimeSpan,
  Diagnostics,
  Generics.Collections,
  {$ENDIF}
  DUnitX.TestFramework,
  DUnitX.InternalInterfaces,
  DUnitX.Generics;


type
  TDUnitXRunResults = class(TInterfacedObject, IRunResults, ITestExecuteContext)
  private
    FFixtures : IList<ITestFixtureInfo>;
    FAllPassed : boolean;
    FErrorCount : integer;
    FFailureCount : integer;
    FPassCount : integer;
    FIgnoredCount : integer;
    FMemoryLeakCount : Integer;
    FTotalCount : integer;

    FStopWatch: TStopwatch;
    FStartTime: TDateTime;
    FFinishTime: TDateTime;
    FDuration: TTimeSpan;
    FFixtureResults : IList<IFixtureResult>;
    FAllTestResults : IList<ITestResult>;
  protected
    function GetFixtureCount: Integer;
    function GetAllPassed: Boolean;
    function GetTestCount: Integer;
    function GetErrorCount: Integer;
    function GetFailureCount: Integer;
    function GetMemoryLeakCount : Integer;
    function GetFixtures: IEnumerable<ITestFixtureInfo>;
    function GetFixtureResults: IEnumerable<IFixtureResult>;
    function GetAllTestResults : IEnumerable<ITestResult>;

    function GetPassCount: Integer;
    function GetIgnoredCount: Integer;

    function GetSuccessRate : integer;
    function GetStartTime: TDateTime;
    function GetFinishTime: TDateTime;
    function GetDuration: TTimeSpan;

    //ITestExecuteContext
    procedure RecordFixture(const fixtureResult : IFixtureResult);
    procedure RecordResult(const fixtureResult : IFixtureResult; const testResult : ITestResult);
    //called when all is done.
    procedure RollupResults;
  public
    constructor Create;
    destructor Destroy;override;
    function ToString : string;override;
  end;


implementation

uses
  DUnitX.ResStrs,
  {$IFDEF USE_NS}
  System.DateUtils,
  System.SysUtils;
  {$ELSE}
  DateUtils,
  SysUtils;
  {$ENDIF}

{ TDUnitXTestResults }

constructor TDUnitXRunResults.Create;
begin
  FFixtureResults := TDUnitXList<IFixtureResult>.Create;
  FAllTestResults := TDUnitXList<ITestResult>.Create;
  FAllPassed := True;
  FErrorCount := 0;
  FPassCount := 0;
  FFailureCount := 0;
  FStartTime := Now;
  FFinishTime := FStartTime;
  FDuration := TTimeSpan.Zero;
  FStopWatch := TStopwatch.StartNew;
end;

destructor TDUnitXRunResults.Destroy;
begin
  //not required, but makes debugging easier.
  FFixtures := nil;
  FAllTestResults := nil;
  inherited;
end;

function TDUnitXRunResults.GetAllPassed: Boolean;
begin
  result := FAllPassed;
end;

function TDUnitXRunResults.GetAllTestResults: IEnumerable<ITestResult>;
begin
  result := FAllTestResults;
end;

function TDUnitXRunResults.GetTestCount: Integer;
begin
  result := FTotalCount;
end;

function TDUnitXRunResults.GetErrorCount: Integer;
begin
  result := FErrorCount;
end;

function TDUnitXRunResults.GetFailureCount: Integer;
begin
  result := FFailureCount;
end;

function TDUnitXRunResults.GetFinishTime: TDateTime;
begin
  result := FFinishTime;
end;

function TDUnitXRunResults.GetFixtures: IEnumerable<ITestFixtureInfo>;
begin
  result := FFixtures;
end;

function TDUnitXRunResults.GetIgnoredCount: Integer;
begin
  result := FIgnoredCount;
end;

function TDUnitXRunResults.GetMemoryLeakCount: Integer;
begin
  Result := FMemoryLeakCount;
end;

function TDUnitXRunResults.GetFixtureCount: Integer;
begin
  result := FFixtureResults.Count;
end;

function TDUnitXRunResults.GetFixtureResults: IEnumerable<IFixtureResult>;
begin
  result := FFixtureResults;
end;

function TDUnitXRunResults.GetDuration: TTimeSpan;
begin
  result := FDuration;
end;

function TDUnitXRunResults.GetStartTime: TDateTime;
begin
  result := FStartTime;
end;

function TDUnitXRunResults.GetPassCount: Integer;
begin
  result := FPassCount;
end;

function TDUnitXRunResults.GetSuccessRate: integer;
var
  successRate : integer;
begin
  if FTotalCount <> 0 then
    successRate :=  Trunc((FTotalCount - FFailureCount - FErrorCount) / FTotalCount) * 100
  else
    successRate := 100;

  Result := successRate;
end;


procedure TDUnitXRunResults.RecordFixture(const fixtureResult: IFixtureResult);
begin
  FFixtureResults.Add(fixtureResult);
end;

procedure TDUnitXRunResults.RecordResult(const fixtureResult : IFixtureResult; const testResult : ITestResult);
begin
  Inc(FTotalCount);
  case testResult.ResultType of
    TTestResultType.Pass    : Inc(FPassCount);
    TTestResultType.Failure : Inc(FFailureCount);
    TTestResultType.Error   : Inc(FErrorCount);
    TTestResultType.Ignored : Inc(FIgnoredCount);
    TTestResultType.MemoryLeak : Inc(FMemoryLeakCount);
  end;

  //Note, don't treat ignored as errors here!
  if not (testResult.ResultType in [TTestResultType.Pass, TTestResultType.Ignored]) then
    FAllPassed := False;

  (fixtureResult as IFixtureResultBuilder).AddTestResult(testResult);

  FAllTestResults.Add(testResult);
end;

procedure TDUnitXRunResults.RollupResults;
var
  fixtureResult : IFixtureResult;

begin
  FFinishTime := Now;
  FDuration := FStopWatch.Elapsed;
  for fixtureResult in FFixtureResults do
    (fixtureResult as IFixtureResultBuilder).RollUpResults;

  //TODO: Make sure the fixture results are unique.
end;

function TDUnitXRunResults.ToString: string;
begin
  result := Format(STestsPassed +#13#10,[FPassCount]);
end;

end.
