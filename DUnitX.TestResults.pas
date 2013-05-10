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

unit DUnitX.TestResults;

interface

uses
  System.TimeSpan,
  DUnitX.TestFramework,
  DUnitX.InternalInterfaces,
  Generics.Collections,
  DUnitX.Generics,
  classes;

{$I DUnitX.inc}


type
  TDUnitXTestResults = class(TInterfacedObject, ITestResults, ITestExecuteContext)
  private
    FResults : TList<ITestResult>;
    FFixtures : IList<ITestFixtureInfo>;
    FAllPassed : boolean;
    FErrorCount : integer;
    FFailureCount : integer;
    FSuccessCount : integer;
    FWarningCount : integer;

    FStartTime: TDateTime;
    FFinishTime: TDateTime;
    FDuration: TTimeSpan;
  protected
    function GetAllPassed: Boolean;
    function GetCount: Integer;
    function GetErrorCount: Integer;
    function GetFailureCount: Integer;
    function GetFixtures: IEnumerable<DUnitX.TestFramework.ITestFixtureInfo>;
    function GetResults: IEnumerable<DUnitX.TestFramework.ITestResult>;
    function GetSuccessCount: Integer;
    function GetWarningCount: Integer;

    function GetSuccessRate : integer;
    function GetStartTime: TDateTime;
    function GetFinishTime: TDateTime;
    function GetTestDuration: TTimeSpan;

    //ITestExecuteContext
    procedure RecordResult(const testResult: ITestResult);
  public
    constructor Create(const fixtures : IList<ITestFixtureInfo>);
    destructor Destroy;override;
  end;


implementation

uses
  SysUtils,
  //TODO: Need to to remove by getting a system independant performance counter.
  Windows;

{ TDUnitXTestResults }

constructor TDUnitXTestResults.Create(const fixtures : IList<ITestFixtureInfo>);
begin
  FResults := TList<ITestResult>.Create;
  FFixtures := fixtures;
  FAllPassed := True;
  FErrorCount := 0;
  FSuccessCount := 0;
  FFailureCount := 0;
  FWarningCount := 0;

  FStartTime := Now;
  FFinishTime := FStartTime;
  FDuration := TTimeSpan.Zero;
end;

destructor TDUnitXTestResults.Destroy;
begin
  FResults.Free;
  inherited;
end;

function TDUnitXTestResults.GetAllPassed: Boolean;
begin
  result := FAllPassed;
end;

function TDUnitXTestResults.GetCount: Integer;
begin
  result := FResults.Count;
end;

function TDUnitXTestResults.GetErrorCount: Integer;
begin
  result := FErrorCount;
end;

function TDUnitXTestResults.GetFailureCount: Integer;
begin
  result := FFailureCount;
end;

function TDUnitXTestResults.GetFinishTime: TDateTime;
begin
  result := FFinishTime;
end;

function TDUnitXTestResults.GetFixtures: System.IEnumerable<DUnitX.TestFramework.ITestFixtureInfo>;
begin
  result := FFixtures;
end;

function TDUnitXTestResults.GetResults: System.IEnumerable<DUnitX.TestFramework.ITestResult>;
begin
  result := nil;
end;

function TDUnitXTestResults.GetTestDuration: TTimeSpan;
begin
  result := FDuration;
end;

function TDUnitXTestResults.GetStartTime: TDateTime;
begin
  result := FStartTime;
end;

function TDUnitXTestResults.GetSuccessCount: Integer;
begin
  result := 0;
end;

function TDUnitXTestResults.GetSuccessRate: integer;
var
  successRate : integer;
begin
  if FResults.Count <> 0 then
    successRate :=  Trunc((FResults.Count - FFailureCount - FErrorCount) / FResults.Count) * 100
  else
    successRate := 100;

  Result := successRate;
end;

function TDUnitXTestResults.GetWarningCount: Integer;
begin
  result := 0;
end;

procedure TDUnitXTestResults.RecordResult(const testResult: ITestResult);
begin
  case testResult.ResultType of
    TTestResultType.Success : Inc(FSuccessCount);
    TTestResultType.Failure : Inc(FFailureCount);
    TTestResultType.Warning : Inc(FWarningCount);
    TTestResultType.Error   : Inc(FErrorCount);
  end;

  if testResult.ResultType <> Success then
    FAllPassed := False;

  FResults.Add(testResult);
end;

end.
