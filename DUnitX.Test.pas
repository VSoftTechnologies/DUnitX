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

unit DUnitX.Test;

interface

uses
  DUnitX.TestFramework,
  DUnitX.InternalInterfaces,
  DUnitX.WeakReference,
  TimeSpan,
  Rtti;

{$I DUnitX.inc}


type
  TDUnitXTest = class(TWeakReferencedObject,ITest,ITestInfo,ISetTestResult,ITestExecute)
  private
    FName : string;
    FMethod : TTestMethod;
    FFixture : IWeakReference<ITestFixture>;
    FStartTime : TDateTime;
    FEndTime   : TDateTime;
    FDuration  : TTimeSpan;
  protected
    procedure SetResult(const value: ITestResult);
    function GetName: string;virtual;
    function GetTestFixture: ITestFixture;
    function ITestInfo.GetTestFixture = ITestInfo_GetTestFixture;
    function ITestInfo_GetTestFixture : ITestFixtureInfo;
    function GetTestMethod: TTestMethod;
    function GetTestStartTime : TDateTime;
    function GetTestEndTime : TDateTime;
    function GetTestDuration: TTimeSpan;
    procedure Execute(const context : ITestExecuteContext);virtual;
  public
    constructor Create(const AFixture : ITestFixture; const AName : string; const AMethod : TTestMethod);
  end;

  TDUnitXTestCase = class(TDUnitXTest,ITestExecute)
  private
    FCaseName : string;
    FArgs : TValueArray;
    FRttiMethod : TRttiMethod;
    FInstance : TObject;
  protected
    function GetName: string;override;
    procedure Execute(const context : ITestExecuteContext);override;
  public
    constructor Create(const AInstance : TObject; const AFixture : ITestFixture; const ACaseName : string; const AName : string; const AMethod : TRttiMethod; const AArgs : TValueArray);reintroduce;
  end;


implementation

uses
  SysUtils,
  DUnitX.Utils;

{ TDUnitXTest }

constructor TDUnitXTest.Create(const AFixture: ITestFixture; const AName: string; const AMethod: TTestMethod);
begin
  FFixture := TWeakReference<ITestFixture>.Create(AFixture);
  FName := AFixture.Name + '.' + AName;
  FMethod := AMethod;
end;

procedure TDUnitXTest.Execute(const context : ITestExecuteContext);
begin
  FStartTime := Now();
  try
    FMethod();
  finally
    FEndTime := Now();

    FDuration := TTimeSpan.Subtract(FEndTime,FStartTime);
    end;
end;

function TDUnitXTest.GetName: string;
begin
  result := FName;
end;

function TDUnitXTest.GetTestDuration: TTimeSpan;
begin
  result := FDuration;
end;

function TDUnitXTest.GetTestEndTime: TDateTime;
begin
  result := FEndTime;
end;

function TDUnitXTest.GetTestFixture: ITestFixture;
begin
  if FFixture.IsAlive then
    result := FFixture.Data
  else
    result := nil;
end;

function TDUnitXTest.GetTestMethod: TTestMethod;
begin
  result := FMethod;
end;

function TDUnitXTest.GetTestStartTime: TDateTime;
begin
  result := FStartTime;
end;

function TDUnitXTest.ITestInfo_GetTestFixture: ITestFixtureInfo;
begin
  if FFixture.IsAlive then
    result := FFixture.Data as ITestFixtureInfo
  else
    result := nil;

end;

procedure TDUnitXTest.SetResult(const value: ITestResult);
begin

end;

{ TDUnitXTestCase }

constructor TDUnitXTestCase.Create(const AInstance : TObject; const AFixture: ITestFixture; const ACaseName : string;
                                   const AName: string; const AMethod: TRttiMethod; const AArgs : TValueArray);
var
  len : integer;
  i   : integer;
  parameters : TArray<TRttiParameter>;
  tmp : TValue;
begin
  inherited Create(AFixture,AName,nil);
  FInstance := AInstance;
  FRttiMethod := AMethod;
  FCaseName := ACaseName;

  parameters := FRttiMethod.GetParameters();

  len := Length(AArgs);
  if len > 0 then
  begin
    SetLength(FArgs,len);
    for i := 0 to len-1 do
    begin
      if AArgs[i].TryConvert(parameters[i].ParamType.Handle, tmp) then
        FArgs[i] := tmp;
    end;
  end;
end;


procedure TDUnitXTestCase.Execute(const context : ITestExecuteContext);
begin
  FStartTime := Now();
  try
    FRttiMethod.Invoke(FInstance,FArgs);
  finally
    FEndTime := Now();
    FDuration := TTimeSpan.Subtract(FEndTime,FStartTime);
  end;
end;

function TDUnitXTestCase.GetName: string;
begin
  result := FName + ' (Test Case : ' + FCaseName +')';
end;

end.
