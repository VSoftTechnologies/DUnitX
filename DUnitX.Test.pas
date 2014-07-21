{***************************************************************************}
{                                                                           }
{           DUnitX                                                          }
{                                                                           }
{           Copyright (C) 2013 Vincent Parrett                              }
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
  DUnitX.Extensibility,
  DUnitX.InternalInterfaces,
  DUnitX.WeakReference,
  DUnitX.TestFramework,
  TimeSpan,
  Rtti;

{$I DUnitX.inc}


type
  TDUnitXTest = class(TWeakReferencedObject, ITest, ITestInfo, ISetTestResult, ITestExecute)
  private
    FName         : string;
    FMethod       : TTestMethod;
    FFixture      : IWeakReference<ITestFixture>;
    FStartTime    : TDateTime;
    FEndTime      : TDateTime;
    FDuration     : TTimeSpan;
    FEnabled      : boolean;
    FIgnored      : boolean;
    FIgnoreReason : string;
    FIgnoreMemoryLeaks : Boolean;
  protected
    //ITest
    function GetName: string; virtual;
    function GetFullName : string;virtual;
    function GetTestFixture: ITestFixture;
    function GetTestMethod: TTestMethod;
    function GetTestStartTime : TDateTime;
    function GetTestEndTime : TDateTime;
    function GetTestDuration: TTimeSpan;
    function GetIgnoreMemoryLeaks() : Boolean;
    procedure SetIgnoreMemoryLeaks(const AValue : Boolean);

    //ITestInfo
    function GetActive : boolean;
    function ITestInfo.GetTestFixture = ITestInfo_GetTestFixture;
    function ITestInfo_GetTestFixture: ITestFixtureInfo;
    function GetEnabled: Boolean;
    procedure SetEnabled(const value: Boolean);
    function GetIgnored : boolean;
    function GetIgnoreReason : string;

    //ISetTestResult
    procedure SetResult(const value: ITestResult);

    //ITestExecute
    procedure Execute(const context : ITestExecuteContext);virtual;
  public
    constructor Create(const AFixture : ITestFixture; const AName : string; const AMethod : TTestMethod; const AEnabled : boolean;
                       const AIgnored : boolean = false; const AIgnoreReason : string = '');

    //property Name : string read GetName;
//    property Fixture : ITestFixture read GetTestFixture;
//    property TestMethod : TTestMethod read GetTestMethod;
  end;

  TDUnitXTestCase = class(TDUnitXTest, ITestExecute)
  private
    FCaseName : string;
    FArgs : TValueArray;
    FRttiMethod : TRttiMethod;
    FInstance : TObject;
  protected
    function GetName: string; override;
    procedure Execute(const context : ITestExecuteContext); override;
  public
    constructor Create(const AInstance : TObject; const AFixture : ITestFixture; const ACaseName : string; const AName : string; const AMethod : TRttiMethod;
                       const AEnabled : boolean; const AArgs : TValueArray);reintroduce;

//    property Name : string read GetName;
//    property Fixture : ITestFixture read GetTestFixture;
//    property TestMethod : TTestMethod read GetTestMethod;
  end;


implementation

uses
  SysUtils,
  DUnitX.Utils;

{ TDUnitXTest }

constructor TDUnitXTest.Create(const AFixture: ITestFixture; const AName: string; const AMethod: TTestMethod; const AEnabled : boolean; const AIgnored : boolean; const AIgnoreReason : string);
begin
  FFixture := TWeakReference<ITestFixture>.Create(AFixture);
  FName := AName;
  FMethod := AMethod;
  FEnabled := AEnabled;
  FIgnored := AIgnored;
  FIgnoreReason := AIgnoreReason;
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

function TDUnitXTest.GetActive: boolean;
begin
  //TODO: Need to set the internal active state
  result := True;
end;

function TDUnitXTest.GetEnabled: Boolean;
begin
  result := FEnabled;
end;

function TDUnitXTest.GetFullName: string;
begin
  result := FFixture.Data.FullName + '.' + Self.GetName;
end;

function TDUnitXTest.GetIgnored: boolean;
begin
  result := FIgnored;
end;

function TDUnitXTest.GetIgnoreMemoryLeaks: Boolean;
begin
  Result := FIgnoreMemoryLeaks;
end;

function TDUnitXTest.GetIgnoreReason: string;
begin
  result := FIgnoreReason;
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

procedure TDUnitXTest.SetEnabled(const value: Boolean);
begin
  FEnabled := value;
end;

procedure TDUnitXTest.SetIgnoreMemoryLeaks(const AValue: Boolean);
begin
  FIgnoreMemoryLeaks := AValue;
end;

procedure TDUnitXTest.SetResult(const value: ITestResult);
begin

end;

{ TDUnitXTestCase }

constructor TDUnitXTestCase.Create(const AInstance : TObject; const AFixture : ITestFixture; const ACaseName : string;
                                   const AName : string; const AMethod : TRttiMethod; const AEnabled : boolean;
                                   const AArgs : TValueArray);
var
  len : integer;
  index   : integer;
  parameters : TArray<TRttiParameter>;
  tmp : TValue;
begin
  inherited Create(AFixture, AName, nil,AEnabled);
  FInstance := AInstance;
  FRttiMethod := AMethod;
  FCaseName := ACaseName;

  parameters := FRttiMethod.GetParameters();

  //Work with the params as the limiter.
  len := Length(parameters);

  if len > 0 then
  begin
    //Only keep as many arguements as there are params
    SetLength(FArgs, len);
    for index := 0 to Pred(len) do
    begin
      if index <= high(AArgs) then
        if AArgs[index].TryConvert(parameters[index].ParamType.Handle, tmp) then
          FArgs[index] := tmp;

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
{  for index := low(FArgs) to high(FArgs) do
  begin
    printableArgsList := printableArgsList + FArgs[index].ToString;

    if index <> high(FArgs) then
      printableArgsList := printableArgsList + ', ';
  end;

  Result := Format(TESTCASE_NAME_FORMAT, [FName, printableArgsList, FCaseName]);
  }
  Result := FName;
end;

end.

