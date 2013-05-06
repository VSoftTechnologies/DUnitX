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

unit DUnitX.TestResult;

interface

uses
  DUnitX.TestFramework,
  DUnitX.InternalInterfaces,
  DUnitX.WeakReference,
  SysUtils;

{$I DUnitX.inc}

type
  TDUnitXTestResult = class(TInterfacedObject,ITestResult)
  private
    FMessage : string;
    FResultType : TTestResultType;
    FTest : IWeakReference<ITestInfo>;

  protected
    function GetMessage: string;
    function GetResult: Boolean;
    function GetResultType: TTestResultType;
    function GetTest: ITestInfo;
  public
    constructor Create(const ATest : ITestInfo; const AType : TTestResultType; const AMessage : string = '');
  end;

  TDUnitXTestError = class(TDUnitXTestResult,ITestError)
  private
    FExceptionClass : ExceptClass;
  protected
    function GetExceptionClass: ExceptClass;
  public
    constructor Create(const ATest : ITestInfo; const AType : TTestResultType; const AMessage : string; const AExceptClass : ExceptClass);reintroduce;
  end;

implementation

{ TDUnitXTestResult }

constructor TDUnitXTestResult.Create(const ATest: ITestInfo;const AType: TTestResultType; const AMessage: string);
begin
  FTest := TWeakReference<ITestInfo>.Create(ATest);
  FResultType := AType;
  FMessage := AMessage;
end;

function TDUnitXTestResult.GetMessage: string;
begin
  result := FMessage;
end;

function TDUnitXTestResult.GetResult: Boolean;
begin
   result := GetResultType = TTestResultType.Success;
end;

function TDUnitXTestResult.GetResultType: TTestResultType;
begin
  result := FResultType;
end;

function TDUnitXTestResult.GetTest: ITestInfo;
begin
  if FTest.IsAlive then
    result := FTest.Data
  else
    result := nil;
end;

{ TDUnitXTestError }

constructor TDUnitXTestError.Create(const ATest: ITestInfo;const AType: TTestResultType; const AMessage: string;const AExceptClass: ExceptClass);
begin
  inherited Create(ATest,AType,AMessage);
  FExceptionClass := AExceptClass;
end;

function TDUnitXTestError.GetExceptionClass: ExceptClass;
begin
  result := FExceptionClass;
end;


end.
