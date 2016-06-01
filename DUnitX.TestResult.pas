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

unit DUnitX.TestResult;

interface

{$I DUnitX.inc}

uses
  {$IFDEF USE_NS}
  System.SysUtils,
  System.Timespan,
  {$ELSE}
  SysUtils,
  Timespan,
  {$ENDIF}
  DUnitX.TestFramework,
  DUnitX.WeakReference,
  DUnitX.InternalInterfaces,
  DUnitX.ComparableFormat,
  DUnitX.Exceptions;

type
  TDUnitXTestResult = class(TInterfacedObject, ITestResult)
  private
    //Keeping message as the user passed message. Not used for internal functionality like exception messages.
    FMessage : string;
    FLogMessages: TLogMessageArray;
    FResultType : TTestResultType;
    FTest : IWeakReference<ITestInfo>;
    FStackTrace : string;
  protected
    function GetMessage: string;
    function GetLogMessages: TLogMessageArray;
    function GetResult: Boolean;
    function GetResultType: TTestResultType;
    function GetTest: ITestInfo;
    function GetStartTime : TDateTime;
    function GetFinishTime : TDateTime;
    function GetDuration : TTimeSpan;
    function GetStackTrace : string;
  public
    constructor Create(const ATestInfo : ITestInfo; const AType : TTestResultType; const AMessage: string; const ALogMessages : TLogMessageArray); overload;
    constructor Create(const ATestInfo : ITestInfo; const AType : TTestResultType; const AMessage: string = ''); overload;
  end;

  TDUnitXTestError = class(TDUnitXTestResult, ITestError)
  private
    FExceptionClass : ExceptClass;
    FExceptionMessage : string;
    FExceptionAddress : Pointer;
    FIsComparable: boolean;
    FExpected: string;
    FActual: string;
    FFormat: TDUnitXComparableFormatClass;
  protected
    function GetExceptionClass : ExceptClass;
    function GetExceptionLocationInfo : string;
    function GetExceptionAddressInfo : string;
    function GetExceptionMessage : string;
    function GetExceptionAddress : Pointer;
    function GetIsComparable: boolean;
    function GetExpected: string;
    function GetActual: string;
    function GetFormat: TDUnitXComparableFormatClass;
  public
    constructor Create(const ATestInfo : ITestInfo; const AType: TTestResultType; const AThrownException: Exception; const Addrs: Pointer; const AMessage: string; const AMessageExList : TLogMessageArray); reintroduce;
  end;

implementation

uses
  DUnitX.IoC;

constructor TDUnitXTestResult.Create(const ATestInfo : ITestInfo; const AType : TTestResultType; const AMessage: string; const ALogMessages : TLogMessageArray);
begin
  FTest := TWeakReference<ITestInfo>.Create(ATestInfo);
  FResultType := AType;
  FMessage := AMessage;
  FLogMessages := ALogMessages;
end;

function TDUnitXTestResult.GetMessage: string;
begin
  result := FMessage;
end;

function TDUnitXTestResult.GetLogMessages: TLogMessageArray;
begin
  result := FLogMessages;
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

constructor TDUnitXTestResult.Create(const ATestInfo: ITestInfo; const AType: TTestResultType; const AMessage: string);
var
  LogMessages: TLogMessageArray;
begin
  SetLength(LogMessages, 0);
  Create(ATestInfo, AType, AMessage, LogMessages);
end;

function TDUnitXTestResult.GetDuration: TTimeSpan;
begin
  if FTest.IsAlive then
    Result := FTest.Data.GetTestDuration
  else
    Result := TTimeSpan.Zero;
end;

function TDUnitXTestResult.GetFinishTime: TDateTime;
begin
  if FTest.IsAlive then
    Result := FTest.Data.GetTestEndTime
  else
    Result := 0;
end;

function TDUnitXTestResult.GetStackTrace: string;
begin
  result := FStackTrace;
end;

function TDUnitXTestResult.GetStartTime: TDateTime;
begin
  if FTest.IsAlive then
    Result := FTest.Data.GetTestStartTime
  else
    Result := 0;
end;

{ TDUnitXTestError }

constructor TDUnitXTestError.Create(const ATestInfo : ITestInfo; const AType: TTestResultType; const AThrownException: Exception; const Addrs: Pointer;  const AMessage: string; const AMessageExList : TLogMessageArray);
{$IFDEF DELPHI_XE_UP}
var
  stackTraceProvider : IStacktraceProvider;
{$ENDIF}
var
  StrCompareEx: ETestFailureStrCompare;
begin
  inherited Create(ATestInfo, AType, AMessage, AMessageExList);

  FExceptionClass := ExceptClass(AThrownException.ClassType);

  FExceptionMessage := FMessage;
  if FMessage <> AThrownException.Message then
    FExceptionMessage := FExceptionMessage + AThrownException.Message;
  FExceptionAddress := Addrs;

  if FExceptionClass = ETestFailureStrCompare then
  begin
    StrCompareEx := AThrownException as ETestFailureStrCompare;
    FIsComparable := True;
    FExpected := StrCompareEx.Expected;
    FActual := StrCompareEx.Actual;
    FFormat := StrCompareEx.Format;
  end
  else FIsComparable := False;

  {$IFDEF DELPHI_XE_UP}
  stackTraceProvider := TDUnitXIoC.DefaultContainer.Resolve<IStacktraceProvider>();

  if stackTraceProvider <> nil then
    FStackTrace := stackTraceProvider.GetStackTrace(AThrownException,Addrs);
  {$ENDIF}
end;

function TDUnitXTestError.GetActual: string;
begin
  Result := FActual;
end;

function TDUnitXTestError.GetExceptionAddress: Pointer;
begin
  Result := FExceptionAddress;
end;

function TDUnitXTestError.GetExceptionAddressInfo: string;
{$IFDEF DELPHI_XE_UP}
var
  stackTraceProvider : IStacktraceProvider;
{$ENDIF}
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
{$IFDEF DELPHI_XE_UP}
var
  stackTraceProvider : IStacktraceProvider;
{$ENDIF}
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

function TDUnitXTestError.GetExpected: string;
begin
  Result := FExpected;
end;

function TDUnitXTestError.GetFormat: TDUnitXComparableFormatClass;
begin
  Result := FFormat;
end;

function TDUnitXTestError.GetIsComparable: boolean;
begin
  Result := FIsComparable;
end;

end.
