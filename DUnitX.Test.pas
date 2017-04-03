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

unit DUnitX.Test;

interface

{$I DUnitX.inc}

uses
  {$IFDEF USE_NS}
  System.Generics.Collections,
  System.TimeSpan,
  System.Rtti,
  System.SysUtils,
  {$ELSE}
  Generics.Collections,
  TimeSpan,
  Rtti,
  SysUtils,
  {$ENDIF}
  DUnitX.Types,
  DUnitX.Extensibility,
  DUnitX.InternalInterfaces,
  DUnitX.WeakReference,
  DUnitX.TestFramework;

type
  TDUnitXTest = class(TWeakReferencedObject, ITest, ITestInfo, ISetTestResult, ITestExecute)
  private
    FName         : string;
    FMethodName   : string;
    FCategories   : TList<string>;
    FMethod       : TTestMethod;
    FFixture      : IWeakReference<ITestFixture>;
    FStartTime    : TDateTime;
    FEndTime      : TDateTime;
    FDuration     : TTimeSpan;
    FEnabled      : boolean;
    FIgnored      : boolean;
    FIgnoreReason : string;
    FIgnoreMemoryLeaks : Boolean;
    FMaxTime      : cardinal; // milliseconds for timeout
    FTimedOut     : Boolean;
  protected
    //ITest
    function GetName: string; virtual;
    function GetMethodName : string;
    function GetCategories : TList<string>;
    function GetFullName : string;virtual;
    function GetTestFixture: ITestFixture;
    function GetTestMethod: TTestMethod;
    function GetTestStartTime : TDateTime;
    function GetTestEndTime : TDateTime;
    function GetTestDuration: TTimeSpan;
    function GetIgnoreMemoryLeaks() : Boolean;
    procedure SetIgnoreMemoryLeaks(const AValue : Boolean);
    function GetMaxTime: cardinal;
    procedure SetMaxTime(const AValue: cardinal);
    function GetTimedOut: Boolean;
    procedure SetTimedOut(const AValue: Boolean);

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
    procedure UpdateInstance(const fixtureInstance : TObject);virtual;
  public
    constructor Create(const AFixture : ITestFixture; const AMethodName : string; const AName : string; const ACategory  : string; const AMethod : TTestMethod; const AEnabled : boolean;
                       const AIgnored : boolean = false; const AIgnoreReason : string = ''; const AMaxTime : Cardinal = 0);
    destructor Destroy;override;
  end;

  TDUnitXExceptionTest = class(TDUnitXTest, ITestExecute)
  private
    FExpectedException : ExceptClass;
    FExceptionInheritance : TExceptionInheritance;
    FRaiseContext : ITestExecuteContext;
  protected
    procedure RaiseMethod;
    procedure Execute(const context : ITestExecuteContext); override;
  public
    constructor Create(const AFixture : ITestFixture; const AMethodName : string; const AName : string; const ACategory  : string;
                       const AMethod : TTestMethod; const AEnabled : boolean; const AIgnored : boolean = false;
                       const AIgnoreReason : string = ''; const AMaxTime : Cardinal = 0;
                       AExpectedException : ExceptClass = nil; const AExceptionInheritance : TExceptionInheritance = exExact);
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
    procedure UpdateInstance(const fixtureInstance : TObject);override;
  public
    constructor Create(const AInstance : TObject; const AFixture : ITestFixture; const AMethodName : string; const ACaseName : string; const AName : string; const ACategory  : string; const AMethod : TRttiMethod;
                       const AEnabled : boolean; const AArgs : TValueArray);reintroduce;
    destructor Destroy;override;
  end;


implementation

uses
  {$IFDEF USE_NS}
  System.Generics.Defaults,
  {$ELSE}
  Generics.Defaults,
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  DUnitX.Timeout,
  {$ENDIF}
  DUnitX.Utils;

{ TDUnitXTest }

constructor TDUnitXTest.Create(const AFixture: ITestFixture; const AMethodName : string; const AName: string; const ACategory  : string; const AMethod: TTestMethod; const AEnabled : boolean; const AIgnored : boolean; const AIgnoreReason : string; const AMaxTime : Cardinal);
var
  categories : TArray<string>;
  cat        : string;
begin
  inherited Create;
  FFixture := TWeakReference<ITestFixture>.Create(AFixture);
  FMethodName := AMethodName;
  FName := AName;

  FCategories := TList<string>.Create(TComparer<string>.Construct(
    function(const Left, Right : string) : integer
    begin
      result := AnsiCompareText(Left,Right);
    end));

  if ACategory <> '' then
  begin
    categories := TStrUtils.SplitString(ACategory,',');
    for cat in categories do
      FCategories.Add(Trim(cat));
  end;

  FMethod := AMethod;
  FEnabled := AEnabled;
  FIgnored := AIgnored;
  FIgnoreReason := AIgnoreReason;
  FMaxTime := AMaxTime;
  FTimedOut := false;
end;

destructor TDUnitXTest.Destroy;
begin
  FCategories.Free;
  inherited;
end;

procedure TDUnitXTest.Execute(const context : ITestExecuteContext);
{$IFDEF MSWINDOWS}
var
  timeout : ITimeout;
{$ENDIF}
begin
  FStartTime := Now();
  try
    {$IFDEF MSWINDOWS}
    if FMaxTime > 0 then
      timeout := InitialiseTimeOut( FMaxTime );
    {$ENDIF}
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

function TDUnitXTest.GetCategories: TList<string>;
begin
  result := FCategories;
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

function TDUnitXTest.GetMethodName: string;
begin
  result := FMethodName;
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

function TDUnitXTest.GetMaxTime: cardinal;
begin
  Result := FMaxTime;
end;
procedure TDUnitXTest.SetMaxTime(const AValue: cardinal);
begin
  FMaxTime := AValue;
end;

function TDUnitXTest.GetTimedOut: Boolean;
begin
  Result := FTimedOut;
end;
procedure TDUnitXTest.SetTimedOut(const AValue: Boolean);
begin
  FTimedOut := AValue;
end;

procedure TDUnitXTest.UpdateInstance(const fixtureInstance: TObject);
begin
  TMethod(FMethod).Data := fixtureInstance;
end;

procedure TDUnitXTest.SetResult(const value: ITestResult);
begin
    //TODO : what was meant to happen here?? Is this called?
end;

{ TDUnitXTestCase }

constructor TDUnitXTestCase.Create(const AInstance : TObject; const AFixture : ITestFixture; const AMethodName : string; const ACaseName : string;
                                   const AName : string; const ACategory  : string; const AMethod : TRttiMethod; const AEnabled : boolean;
                                   const AArgs : TValueArray);
var
  len : integer;
  index   : integer;
  parameters : TArray<TRttiParameter>;
  tmp : TValue;
begin
  inherited Create(AFixture, AMethodName, AName, ACategory, nil,AEnabled);
  FInstance := AInstance;
  FRttiMethod := AMethod;
  FCaseName := ACaseName;

  parameters := FRttiMethod.GetParameters();

  //Work with the params as the limiter.
  len := Length(parameters);

  if len > 0 then
  begin
    //Only keep as many arguments as there are params
    SetLength(FArgs, len);
    for index := 0 to Pred(len) do
    begin
      if index <= high(AArgs) then
        if AArgs[index].TryConvert(parameters[index].ParamType.Handle, tmp) then
          FArgs[index] := tmp;

    end;
  end;
end;


destructor TDUnitXTestCase.Destroy;
begin

  inherited;
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
  Result := FName + '.' + FCaseName;
end;

procedure TDUnitXTestCase.UpdateInstance(const fixtureInstance: TObject);
begin
  inherited;
  FInstance := fixtureInstance;
end;

{ TDUnitXExceptionTest }

constructor TDUnitXExceptionTest.Create(const AFixture: ITestFixture;
  const AMethodName, AName, ACategory: string; const AMethod: TTestMethod;
  const AEnabled, AIgnored: boolean; const AIgnoreReason: string;
  const AMaxTime: Cardinal; AExpectedException: ExceptClass; const AExceptionInheritance: TExceptionInheritance);
begin
  inherited Create(AFixture, AMethodName, AName, ACategory, AMethod, AEnabled, AIgnored, AIgnoreReason, AMaxTime);
  FExpectedException := AExpectedException;
  FExceptionInheritance := AExceptionInheritance;
end;

procedure TDUnitXExceptionTest.Execute(const context: ITestExecuteContext);
begin
  FRaiseContext := context;
  if FExceptionInheritance = exDescendant then
    Assert.WillRaiseDescendant(RaiseMethod, FExpectedException)
  else
    Assert.WillRaise(RaiseMethod, FExpectedException);
end;

procedure TDUnitXExceptionTest.RaiseMethod;
begin
  inherited Execute(FRaiseContext);
end;

end.
