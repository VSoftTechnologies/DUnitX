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

unit DUnitX.FixtureResult;

interface

{$I DUnitX.inc}

uses
  {$IFDEF USE_NS}
  System.Classes,
  System.TimeSpan,
  System.Diagnostics,
  {$ELSE}
  Classes,
  TimeSpan,
  Diagnostics,
  {$ENDIF}
  DUnitX.Generics,
  DUnitX.TestFramework,
  DUnitX.InternalInterfaces;


type
  TDUnitXFixtureResult = class(TInterfacedObject,IFixtureResult,IFixtureResultBuilder)
  private
    FChildren     : IList<IFixtureResult>;
    FTestResults  : IList<ITestResult>;
    FFixture      : ITestFixtureInfo;

    FAllPassed    : boolean;
    FErrorCount   : integer;
    FFailureCount : integer;
    FPassCount    : integer;
    FIgnoredCount : integer;
    FMemoryLeakCount   : Integer;
    FTotalCount   : integer;

    FStopWatch    : TStopwatch;
    FStartTime    : TDateTime;
    FFinishTime   : TDateTime;
    FDuration     : TTimeSpan;

    FName         : string;
    FNameSpace    : string;
    FCanReduce    : boolean;

  protected
    procedure Reduce;


    function GetChildCount : Integer;
    function GetChildren : IList<IFixtureResult>;
    function GetErrorCount : Integer;
    function GetErrors : IList<ITestError>;
    function GetFailureCount : Integer;
    function GetFailures : IList<ITestResult>;
    function GetFixture : ITestFixtureInfo;
    function GetHasFailures : Boolean;
    function GetPassCount : Integer;
    function GetPasses : IList<ITestResult>;
    function GetTestResultCount : Integer;
    function GetTestResults : IList<ITestResult>;
    function GetIgnoredCount : Integer;

    function GetStartTime: TDateTime;
    function GetFinishTime: TDateTime;
    function GetDuration: TTimeSpan;
    function GetName : string;
    function GetNamespace : string;


    procedure AddChild(const AFixtureResult: IFixtureResult);
    procedure AddTestResult(const AResult: ITestResult);
    procedure RecordTestResult(const AResult : ITestResult);
    procedure RollUpResults;
  public
    constructor Create(const AParentResult : IFixtureResult; const AFixture : ITestFixtureInfo);
  end;

implementation

uses
  {$IFDEF USE_NS}
  System.DateUtils,
  System.SysUtils;
  {$ELSE}
  DateUtils,
  SysUtils;
  {$ENDIF}

const
  UNDEFINED_DATETIME = 0;

{ TDUnitXFixtureResult }

procedure TDUnitXFixtureResult.AddChild(const AFixtureResult: IFixtureResult);
begin
  if FChildren = nil then
    FChildren := TDUnitXList<IFixtureResult>.Create;
  FChildren.Add(AFixtureResult);
end;

procedure TDUnitXFixtureResult.AddTestResult(const AResult: ITestResult);
begin
  if FTestResults = nil then
    FTestResults := TDUnitXList<ITestResult>.Create;
  FTestResults.Add(AResult);
  RecordTestResult(AResult);
end;

constructor TDUnitXFixtureResult.Create(const AParentResult : IFixtureResult;const AFixture: ITestFixtureInfo);
begin
  FFixture := AFixture;
  FStartTime := Now;
  FFinishTime := UNDEFINED_DATETIME;
  FStopWatch := TStopwatch.StartNew;
  //Don't create collections here.. we'll lazy create;
  FChildren := nil;
  FTestResults := nil;

  FName := AFixture.Name;
  FNameSpace := AFixture.NameSpace;


  if AParentResult <> nil then
  begin
    (AParentResult as IFixtureResultBuilder).AddChild(Self);
    FCanReduce := True;
  end;

end;


function TDUnitXFixtureResult.GetChildCount: Integer;
begin
  if FChildren <> nil then
    result := FChildren.Count
  else
    result := 0;
end;

function TDUnitXFixtureResult.GetChildren: IList<IFixtureResult>;
begin
  //Don't pass nill back???
  if FChildren = nil then
    FChildren := TDUnitXList<IFixtureResult>.Create;
  result := FChildren;
end;

function TDUnitXFixtureResult.GetDuration: TTimeSpan;
begin
  result := FDuration;
end;

function TDUnitXFixtureResult.GetErrorCount: Integer;
begin
  result := FErrorCount;
end;


function TDUnitXFixtureResult.GetErrors: IList<ITestError>;
var
  test : ITestResult;
  error : ITestError;
begin
  result := TDUnitXList<ITestError>.Create;
  if FTestResults = nil then
    exit;

  for test in FTestResults do
  begin
    if Supports(test,ITestError,error) then
      result.Add(error);
  end;
end;

function TDUnitXFixtureResult.GetFailureCount: Integer;
begin
  result := FFailureCount;
end;

function TDUnitXFixtureResult.GetFailures: IList<ITestResult>;
var
  test : ITestResult;
begin
  result := TDUnitXList<ITestResult>.Create;
  if FTestResults = nil then
    exit;

  for test in FTestResults do
  begin
    if test.ResultType = TTestResultType.Failure then
      result.Add(test);
  end;
end;

function TDUnitXFixtureResult.GetFinishTime: TDateTime;
begin
  result := FFinishTime;
end;

function TDUnitXFixtureResult.GetFixture: ITestFixtureInfo;
begin
  result := FFixture;
end;

function TDUnitXFixtureResult.GetHasFailures: Boolean;
begin
  result := FFailureCount > 0;
end;

function TDUnitXFixtureResult.GetIgnoredCount: Integer;
begin
  result := FIgnoredCount;
end;

function TDUnitXFixtureResult.GetName: string;
begin
  result := FName;
end;

function TDUnitXFixtureResult.GetNamespace: string;
begin
  result := FName;
end;

function TDUnitXFixtureResult.GetPassCount: Integer;
begin
  result := FPassCount;
end;

function TDUnitXFixtureResult.GetPasses: IList<ITestResult>;
var
  test : ITestResult;
begin
  result := TDUnitXList<ITestResult>.Create;
  if FTestResults = nil then
    exit;

  for test in FTestResults do
  begin
    if test.ResultType = TTestResultType.Pass then
      result.Add(test);
  end;
end;

function TDUnitXFixtureResult.GetStartTime: TDateTime;
begin
  Result := FStartTime;
end;

function TDUnitXFixtureResult.GetTestResultCount: Integer;
begin
  if FTestResults = nil then
    Exit(0);
  result := FTestResults.Count;
end;

function TDUnitXFixtureResult.GetTestResults: IList<DUnitX.TestFramework.ITestResult>;
begin
  if FTestResults = nil then
    FTestResults := TDUnitXList<ITestResult>.Create;
  result := FTestResults;
end;

function Max(const a, b : TDateTime) : TDateTime;
begin
  if a > b then
    result := a
  else
    result := b;
end;


procedure TDUnitXFixtureResult.RecordTestResult(const AResult: ITestResult);
begin
  Inc(FTotalCount);
  case AResult.ResultType of
    TTestResultType.Pass    : Inc(FPassCount);
    TTestResultType.Failure : Inc(FFailureCount);
    TTestResultType.Error   : Inc(FErrorCount);
    TTestResultType.Ignored : Inc(FIgnoredCount);
    TTestResultType.MemoryLeak : Inc(FMemoryLeakCount);
  end;

  if AResult.ResultType <> TTestResultType.Pass then
    FAllPassed := False;
end;

procedure TDUnitXFixtureResult.Reduce;
var
  fixtureRes : IFixtureResult;
begin
  if (FChildren <> nil) and (FChildren.Count > 0) then
  begin
    //Reduce the children first.
    for fixtureRes in FChildren do
      fixtureRes.Reduce;

    //if we have no tests and only one child, then we reduce to that child.
    if FCanReduce and (FChildren.Count = 1) and ((FTestResults = nil) or (FTestResults.Count = 0)) then
    begin
       fixtureRes := FChildren[0];

       FNameSpace := FNameSpace + '.' + FName;
       FName := fixtureRes.Name;

       FFixture := fixtureRes.Fixture;
       if FTestResults = nil then
         FTestResults := TDUnitXList<ITestResult>.Create;
       FTestResults.AddRange(fixtureRes.TestResults);
       FChildren.Clear;
       if fixtureRes.ChildCount > 0 then
         FChildren.AddRange(fixtureRes.Children)
       else
        FChildren.Clear;
    end;
  end;
end;

procedure TDUnitXFixtureResult.RollUpResults;
var
  fixture : IFixtureResult;
begin
  if FChildren <> nil then
  begin
    FDuration := TTimeSpan.Zero;
    FFinishTime := FStartTime;
    for fixture in FChildren do
    begin
      (fixture as IFixtureResultBuilder).RollUpResults;
      Inc(FErrorCount,fixture.ErrorCount);
      Inc(FFailureCount,fixture.FailureCount);
      Inc(FIgnoredCount,fixture.IgnoredCount);
      Inc(FPassCount,fixture.PassCount);
      FAllPassed := FAllPassed and (not fixture.HasFailures);
      FFinishTime := Max(FFinishTime,fixture.FinishTime);
      FDuration := FDuration.Add(fixture.Duration);
    end;
  end
  else if (FFinishTime = UNDEFINED_DATETIME) then
  begin
    FFinishTime := Now;
    FStopWatch.Stop;
    FDuration := FStopWatch.Elapsed;
  end;
end;

end.
