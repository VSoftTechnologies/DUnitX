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

unit DUnitX.Tests.Loggers.XML.NUnit;


interface

{$I DUnitX.inc}

uses
  {$IFDEF USE_NS}
  System.Classes,
  {$ELSE}
  Classes,
  {$ENDIF}
  DUnitX.TestFramework,
  DUnitX.Loggers.XML.NUnit;


type
  {$M+}
  [TestFixture]
  TDUnitX_LoggerXMLNUnitTests = class
  public
    [Test(false)]
    procedure OnTestingStarts_Fills_The_Start_Of_The_Stream_With_Header_Info;
  //  [Test(false)]
//    procedure OnTestingEnds_Fills_The_End_Of_The_Stream_With_Testing_Result_Info;
{$IFNDEF DELPHI_XE_DOWN}
    [Test(false)]
    procedure OnTestWarning_Adds_Warnings_To_Be_Written_Out_On_Next_Error;
    procedure OnTestWarning_Adds_Warnings_To_Be_Written_Out_On_Next_Success;
{$ENDIF}
  end;

implementation

uses
  {$IFDEF USE_NS}
  System.Rtti,
  System.SysUtils,
  System.TimeSpan,
  System.DateUtils,
  {$ELSE}
  Rtti,
  SysUtils,
  TimeSpan,
  DateUtils,
  {$ENDIF}
{$IFNDEF DELPHI_XE_DOWN}
  Delphi.Mocks,
{$ENDIF}
  DUnitX.Generics,
  DUnitX.RunResults;

const
  CRLF = #13#10;

{ TDUnitX_LoggerXMLNUnit }

{
procedure TDUnitX_LoggerXMLNUnitTests.OnTestingEnds_Fills_The_End_Of_The_Stream_With_Testing_Result_Info;
var
  logger : ITestLogger;
  mockStream : TStringStream;
  mockResults : TMock<ITestResults>;

  sExpectedEnding : string;

  TempStartTime: TDateTime;
  TempFinishTime: TDateTime;
  StartTimeStr: string;
  FinishTimeStr: string;

begin
  mockStream := TStringStream.Create('', TEncoding.UTF8);

  mockResults := TMock<ITestResults>.Create;
  mockResults.Setup.WillReturn(6).When.Count;
  mockResults.Setup.WillReturn(3).When.FailureCount;
  mockResults.Setup.WillReturn(1).When.ErrorCount;
  mockResults.Setup.WillReturn(50).When.SuccessRate;
    mockResults.Setup.WillReturn(3).When.IgnoredCount;

  TempStartTime := EncodeDateTime(2000, 2, 1, 11, 32, 50, 0);
  TempFinishTime := EncodeDateTime(2000, 2, 28, 12, 34, 56, 0);
  StartTimeStr := DateTimeToStr(TempStartTime);
  FinishTimeStr := DateTimeToSTr(TempFinishTime);


  mockResults.Setup.WillReturn(TempStartTime).When.StartTime;
  mockResults.Setup.WillReturn(TempFinishTime).When.FinishTime;
  mockResults.Setup.WillReturn(TValue.From<TTimeSpan>(TTimeSpan.FromMilliseconds(80129120))).When.TestDuration;

  logger := TDUnitXXMLNUnitLogger.Create(mockStream);
  logger.OnTestingEnds(mockResults);

  sExpectedEnding :=  '<statistics>' + CRLF +
                  Format('<stat name="tests" value="%d" />', [6]) + CRLF +
                  Format('<stat name="failures" value="%d" />', [3]) + CRLF +
                  Format('<stat name="errors" value="%d" />', [1]) + CRLF +
                  Format('<stat name="ignored" value="%d" />', [3]) + CRLF +
                  Format('<stat name="success-rate" value="%d%%" />', [50]) + CRLF +
                  Format('<stat name="started-at" value="%s" />', [StartTimeStr]) + CRLF +
                  Format('<stat name="finished-at" value="%s" />', [FinishTimeStr]) + CRLF +
                  Format('<stat name="runtime" value="%1.3f"/>', [80129.120]) + CRLF +
                  '</statistics>' + CRLF +
              '</test-results>';

  Assert.AreEqual<string>(mockStream.DataString, sExpectedEnding);
end;
 }
procedure TDUnitX_LoggerXMLNUnitTests.OnTestingStarts_Fills_The_Start_Of_The_Stream_With_Header_Info;
var
  sUnicodePreamble: string;
  logger : ITestLogger;
  mockStream : TStringStream;
  sOnTestingStartsText : string;
  sHeader: string;
  sResults: string;
  sAppName: string;
  sExpectedResults : string;
  sExpectedAppName : string;
  iPrevLastChar: Integer;
const
  EXPECTED_HEADER = '<?xml version="1.0" encoding="UTF-8" standalone="yes" ?>';
  EXPECTED_RESULTS_FORMAT_STR = '<test-results total="%d" notrun="%d" date="%s" time="%s" >';
  EXPECTED_APP_NAME_FORMAT_STR = '<application name="%s" />';
begin
  //TODO: Break this unit tests into three separate specific tests for each of the header sections.
  mockStream := TStringStream.Create('', TEncoding.UTF8);
  logger := TDUnitXXMLNUnitLogger.Create(mockStream);
  logger.OnTestingStarts(0, 40, 30);

  sOnTestingStartsText := mockStream.DataString;

  //Expected results
  //TODO: Fix this dangerous tests around "NOW"
  sExpectedResults := Format(EXPECTED_RESULTS_FORMAT_STR,
                         [40, 40 - 30, DateToStr(Now), TimeToStr(Now)]);

  sExpectedAppName := Format(EXPECTED_APP_NAME_FORMAT_STR,
                         [ExtractFileName(ParamStr(0))]);

  iPrevLastChar := 0;
  //Check the preamble
  sUnicodePreamble := TEncoding.UTF8.GetString(TEncoding.UTF8.GetPreamble);

  Assert.AreEqual(Copy(sOnTestingStartsText, iPrevLastChar, Length(sUnicodePreamble)), sUnicodePreamble);
  iPrevLastChar := iPrevLastChar + Length(sUnicodePreamble);

  //Check the header
  sHeader := Copy(sOnTestingStartsText, Succ(iPrevLastChar), Length(EXPECTED_HEADER));
  iPrevLastChar := iPrevLastChar + Length(EXPECTED_HEADER);

  sResults := Copy(sOnTestingStartsText, Succ(iPrevLastChar), Length(sExpectedResults));
  iPrevLastChar := iPrevLastChar + Length(sExpectedResults);

  sAppName := Copy(sOnTestingStartsText, Succ(iPrevLastChar), Length(sExpectedAppName));

  Assert.AreEqual(sHeader, EXPECTED_HEADER);

  Assert.AreEqual(sResults, sExpectedResults);

  Assert.AreEqual(sAppName, sExpectedAppName);
end;

{$IFNDEF DELPHI_XE_DOWN}
procedure TDUnitX_LoggerXMLNUnitTests.OnTestWarning_Adds_Warnings_To_Be_Written_Out_On_Next_Error;
var
  logger : ITestLogger;
  mockStream : TStringStream;
  mockWarning : TMock<ITestResult>;
  mockFixture : TMock<ITestFixtureInfo>;
  mockTest : TMock<ITestInfo>;
  mockError: TMock<ITestError>;

  sExceptedWarning : string;
  iPositionOfWarning: Integer;
begin
  //Mocks
  mockStream := TStringStream.Create('', TEncoding.UTF8);
  mockWarning := TMock<ITestResult>.Create;
  mockFixture := TMock<ITestFixtureInfo>.Create;
  mockTest := TMock<ITestInfo>.Create;
  mockError := TMock<ITestError>.Create;

  //System under test
  logger := TDUnitXXMLNUnitLogger.Create(mockStream);

  //Setup
  //TODO: Would be nice in Delphi.Mocks to have a auto mock of interface and object properties.
  mockFixture.Setup.WillReturn('WarningFixture').When.Name;
  mockTest.Setup.WillReturn('WarningTest').When.Name;
  mockTest.Setup.WillReturn('WarningTest').When.FullName;
  mockTest.Setup.WillReturn(True).When.Active;
  mockWarning.Setup.WillReturn('!!WarningMessage!!').When.Message;
  mockWarning.Setup.WillReturn(mockTest.InstanceAsValue).When.Test;
  mockError.Setup.WillReturn(mockTest.InstanceAsValue).When.Test;
  mockError.Setup.WillReturn(TValue.From<TTimeSpan>(TTimeSpan.FromMilliseconds(0))).When.Duration;
  mockError.Setup.WillReturn(Exception).When.ExceptionClass;
  mockError.Setup.WillReturn('').When.ExceptionLocationInfo;
  mockError.Setup.WillReturn('').When.ExceptionMessage;

  sExceptedWarning := Format('WARNING: %s: %s', [mockTest.Instance.Name, mockWarning.Instance.Message]);

  //Call
  logger.OnTestError(0, mockError);

  //Verify
  iPositionOfWarning := Pos(sExceptedWarning, mockStream.DataString);
  Assert.IsTrue(iPositionOfWarning > 0);
  Assert.AreEqual(Copy(mockStream.DataString, iPositionOfWarning, Length(sExceptedWarning)), sExceptedWarning);
end;

procedure TDUnitX_LoggerXMLNUnitTests.OnTestWarning_Adds_Warnings_To_Be_Written_Out_On_Next_Success;
var
  logger : ITestLogger;
  mockStream : TStringStream;
  mockWarning : TMock<ITestResult>;
  mockTest : TMock<ITestInfo>;
  mockSuccess: TMock<ITestError>;

  sExceptedWarning : string;
  iPositionOfWarning: Integer;
begin
  //Mocks
  mockStream := TStringStream.Create('', TEncoding.UTF8);
  mockWarning := TMock<ITestResult>.Create;
  mockTest := TMock<ITestInfo>.Create;
  mockSuccess := TMock<ITestError>.Create;

  //System under test
  logger := TDUnitXXMLNUnitLogger.Create(mockStream);

  //Setup
  mockTest.Setup.WillReturn('SuccessfulTest').When.Name;
  mockWarning.Setup.WillReturn('Warning').When.Message;

  //TODO: Would be nice in Delphi.Mocks to have a auto mock of interface and object properties.
  mockWarning.Setup.WillReturn(mockTest.InstanceAsValue).When.Test;

  sExceptedWarning := Format('WARNING: %s: %s', [mockTest.Instance.Name, mockWarning.Instance.Message]);

  //Call
  logger.OnTestSuccess(0, mockSuccess);

  //Verify
  iPositionOfWarning := Pos(sExceptedWarning, mockStream.DataString);
  Assert.IsTrue(iPositionOfWarning > 0);
  Assert.AreEqual(Copy(mockStream.DataString, iPositionOfWarning, Length(sExceptedWarning)), sExceptedWarning);
end;
{$ENDIF}

initialization
  TDUnitX.RegisterTestFixture(TDUnitX_LoggerXMLNUnitTests);
end.

