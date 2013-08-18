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
  Timespan,
  DUnitX.TestFramework,
  DUnitX.WeakReference,
  DUnitX.InternalInterfaces,
  SysUtils;

{$I DUnitX.inc}

type
  TDUnitXTestResult = class(TInterfacedObject, ITestResult)
  private
    //Keeping message as the user passed message. Not used for internal functionality like exception messages.
    FMessage : string;
    FResultType : TTestResultType;
    FTest : IWeakReference<ITestInfo>;
  protected
    function GetMessage: string;
    function GetResult: Boolean;
    function GetResultType: TTestResultType;
    function GetTest: ITestInfo;
    function GetTestStartTime : TDateTime;
    function GetTestEndTime : TDateTime;
    function GetTestDuration : TTimeSpan;
  public
    constructor Create(const ATestInfo : ITestInfo; const AType : TTestResultType; const AMessage : string = '');
  end;

  TDUnitXTestError = class(TDUnitXTestResult, ITestError)
  private
    FStackTrace : string;
    FExceptionClass : ExceptClass;
    FExceptionMessage : string;
    FExceptionAddress : Pointer;
  protected
    function GetExceptionClass : ExceptClass;
    function GetExceptionLocationInfo : string;
    function GetExceptionAddressInfo : string;
    function GetExceptionMessage : string;
  public
    constructor Create(const ATestInfo : ITestInfo; const AType : TTestResultType; const AThrownException: Exception; const Addrs: Pointer; const AMessage : string = '');reintroduce;
  end;

implementation

uses
  {$IFDEF MSWINDOWS}
    {$if CompilerVersion < 23 }
      Windows,
    {$else}
      WinAPI.Windows, // Delphi XE2 (CompilerVersion 23) added scopes in front of unit names
    {$ifend}
  {$ENDIF}
  DUnitX.IoC;

{ TDUnitXTestResult }

constructor TDUnitXTestResult.Create(const ATestInfo : ITestInfo; const AType: TTestResultType; const AMessage: string);
begin
  FTest := TWeakReference<ITestInfo>.Create(ATestInfo);

  FResultType := AType;
  FMessage := AMessage;
end;

function TDUnitXTestResult.GetMessage: string;
begin
  result := FMessage;
end;

function TDUnitXTestResult.GetResult: Boolean;
begin
   result := GetResultType = TTestResultType.Pass;
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

function TDUnitXTestResult.GetTestDuration: TTimeSpan;
begin
  if FTest.IsAlive then
    Result := FTest.Data.GetTestDuration
  else
    Result := TTimeSpan.Zero;
end;

function TDUnitXTestResult.GetTestEndTime: TDateTime;
begin
  if FTest.IsAlive then
    Result := FTest.Data.GetTestEndTime
  else
    Result := 0;
end;

function TDUnitXTestResult.GetTestStartTime: TDateTime;
begin
  if FTest.IsAlive then
    Result := FTest.Data.GetTestStartTime
  else
    Result := 0;
end;

{ TDUnitXTestError }

constructor TDUnitXTestError.Create(const ATestInfo : ITestInfo; const AType: TTestResultType; const AThrownException: Exception; const Addrs: Pointer; const AMessage: string = '');
var
  stackTraceProvider : IStacktraceProvider;
begin
  inherited Create(ATestInfo, AType, AMessage);

  FExceptionClass := ExceptClass(AThrownException.ClassType);

  FExceptionMessage := AMessage + AThrownException.Message;
  FExceptionAddress := Addrs;

  {$IFDEF DELPHI_XE_UP}
  stackTraceProvider := TDUnitXIoC.DefaultContainer.Resolve<IStacktraceProvider>();

  if stackTraceProvider <> nil then
    FStackTrace := stackTraceProvider.GetStackTrace(AThrownException,Addrs);
  {$ENDIF}
end;

function TDUnitXTestError.GetExceptionAddressInfo: string;
var
  stackTraceProvider : IStacktraceProvider;
begin
  {$IFDEF DELPHI_XE_UP}
  stackTraceProvider := TDUnitXIoc.DefaultContainer.Resolve<IStacktraceProvider>();
  if stackTraceProvider <> nil then
    Result := stackTraceProvider.PointerToAddressInfo(FExceptionAddress)
  else
  {$ENDIF}
    Result := '';
end;

function TDUnitXTestError.GetExceptionClass: ExceptClass;
begin
  result := FExceptionClass;
end;

function TDUnitXTestError.GetExceptionLocationInfo: string;
var
  stackTraceProvider : IStacktraceProvider;
begin
  {$IFDEF DELPHI_XE_UP}
  stackTraceProvider := TDUnitXIoc.DefaultContainer.Resolve<IStacktraceProvider>();
  if stackTraceProvider <> nil then
    Result := stackTraceProvider.PointerToLocationInfo(FExceptionAddress)
  else
  {$ENDIF}
    Result := '';
end;

function TDUnitXTestError.GetExceptionMessage: string;
begin
  Result := FExceptionMessage;
end;

end.
