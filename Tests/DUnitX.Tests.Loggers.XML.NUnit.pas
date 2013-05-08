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

uses
  Classes,
  DUnitX.TestFramework,
  DUnitX.Loggers.XML.NUnit;

type
  {$M+}
  [TestFixture]
  TDUnitX_LoggerXMLNUnitTests = class
  public
    [Test]
    procedure CreateTest;
    [Test]
    procedure After_Creation_Filename_Is_Set;
    [Test]
    procedure OnTestingStarts_Fills_The_Start_Of_The_Stream_With_Header_Info;
    [Test]
    procedure OnTestingEnds_Fills_The_End_Of_The_Stream_With_Testing_Result_Info;
  end;

implementation

uses
  SysUtils,
  DUnitX.Generics,
  DUnitX.TestResults,
  Delphi.Mocks;

const
  CRLF = #13#10;

{ TDUnitX_LoggerXMLNUnit }

procedure TDUnitX_LoggerXMLNUnitTests.CreateTest;
var
  logger : IDUnitXXMLNUnitLogger;
  mockStream : TMock<TFileStream>;
begin
  mockStream := TMock<TFileStream>.Create;

  logger := TDUnitXXMLNUnitLogger.Create(mockStream);

  Assert.IsNotNull(mockStream);
end;

procedure TDUnitX_LoggerXMLNUnitTests.OnTestingEnds_Fills_The_End_Of_The_Stream_With_Testing_Result_Info;
var
  logger : IDUnitXXMLNUnitLogger;
  mockStream : TStringStream;
  mockResults : TMock<ITestResults>;

  sExpectedEnding : string;
begin
  mockStream := TStringStream.Create('', TEncoding.UTF8);

  mockResults := TMock<ITestResults>.Create;
  mockResults.Setup.WillReturn(6).When.Count;
  mockResults.Setup.WillReturn(3).When.FailureCount;
  mockResults.Setup.WillReturn(1).When.ErrorCount;
  mockResults.Setup.WillReturn(50).When.SuccessRate;
  mockResults.Setup.WillReturn(StrToDateTime('1/02/2000 11:32:50 AM')).When.StartTime;
  mockResults.Setup.WillReturn(StrToDateTime('28/02/2000 12:34:55 PM')).When.FinishTime;
  mockResults.Setup.WillReturn(80129.120).When.RunTime;

  logger := TDUnitXXMLNUnitLogger.Create(mockStream);
  logger.OnTestingEnds(mockResults);

  sExpectedEnding :=  '<statistics>' + CRLF +
                  Format('<stat name="tests" value="%d" />', [6]) + CRLF +
                  Format('<stat name="failures" value="%d" />', [3]) + CRLF +
                  Format('<stat name="errors" value="%d" />', [1]) + CRLF +
                  Format('<stat name="success-rate" value="%d%%" />', [50]) + CRLF +
                  Format('<stat name="started-at" value="%s" />', ['1/02/2000 11:32:50 AM']) + CRLF +
                  Format('<stat name="finished-at" value="%s" />', ['28/02/2000 12:34:55 PM']) + CRLF +
                  Format('<stat name="runtime" value="%1.3f"/>', [80129.120]) + CRLF +
                  '</statistics>' + CRLF +
              '</test-results>';

  Assert.AreEqual<string>(mockStream.DataString, sExpectedEnding);
end;

procedure TDUnitX_LoggerXMLNUnitTests.OnTestingStarts_Fills_The_Start_Of_The_Stream_With_Header_Info;
var
{$IFDEF UNICODE}
  sUnicodePreamble: string;
{$ENDIF}
  logger : IDUnitXXMLNUnitLogger;
  mockStream : TStringStream;
  sOnTestingStartsText : string;
  sHeader: string;
  sResults: string;
  sAppName: string;
  sExpectedResults : string;
  sExpectedAppName : string;
  iPrevLastChar: Integer;
const
{$IFDEF UNICODE}
  EXPECTED_HEADER = '<?xml version="1.0" encoding="UTF-8" standalone="yes" ?>';
{$ELSE}
  EXPECTED_HEADER = '<?xml version="1.0" encoding="ISO-8859-1" standalone="yes" ?>';
{$ENDIF}
  EXPECTED_RESULTS_FORMAT_STR = '<test-results total="%d" notrun="%d" date="%s" time="%s" >';
  EXPECTED_APP_NAME_FORMAT_STR = '<application name="%s" />';
begin
  //TODO: Break this unit tests into three seperate specific tests for each of the header sections.
  //TODO: Break unicode specific test out into its own test, and compile in/out the whole test.
{$IFDEF UNICODE}
  mockStream := TStringStream.Create('', TEncoding.UTF8);
{$ELSE}
  mockStream := TStringStream.Create('', TEncoding.ANSI);
{$ENDIF}
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
{$IFDEF UNICODE}
  //Check the preamble
  sUnicodePreamble := TEncoding.UTF8.GetString(TEncoding.UTF8.GetPreamble);

  Assert.AreEqual<string>(Copy(sOnTestingStartsText, iPrevLastChar, Length(sUnicodePreamble)), sUnicodePreamble);
  iPrevLastChar := iPrevLastChar + Length(sUnicodePreamble);

  //Check the header
  sHeader := Copy(sOnTestingStartsText, Succ(iPrevLastChar), Length(EXPECTED_HEADER));
  iPrevLastChar := iPrevLastChar + Length(EXPECTED_HEADER);

  sResults := Copy(sOnTestingStartsText, Succ(iPrevLastChar), Length(sExpectedResults));
  iPrevLastChar := iPrevLastChar + Length(sExpectedResults);

  sAppName := Copy(sOnTestingStartsText, Succ(iPrevLastChar), Length(sExpectedAppName));
{$ELSE}
  sHeader := Copy(sOnTestingStartsText, Succ(iPrevLastChar), Length(EXPECTED_HEADER));
  iPrevLastChar := iPrevLastChar + Length(EXPECTED_HEADER);

  sResults := Copy(sOnTestingStartsText, Succ(iPrevLastChar), Length(sExpectedResults));
  iPrevLastChar := iPrevLastChar + Length(sExpectedResults);

  sAppName := Copy(sOnTestingStartsText, Succ(iPrevLastChar), Length(sExpectedAppName));
{$ENDIF}

  Assert.AreEqual<string>(sHeader, EXPECTED_HEADER);

  Assert.AreEqual<string>(sResults, sExpectedResults);

  Assert.AreEqual<string>(sAppName, sExpectedAppName);
end;

procedure TDUnitX_LoggerXMLNUnitTests.After_Creation_Filename_Is_Set;
var
  logger : IDUnitXXMLNUnitLogger;
  mockStream : TMock<TFileStream>;
const
  TEST_FILE_NAME = 'DUnitX_TestFile';
begin
  mockStream := TMock<TFileStream>.Create;

  //TODO: Delphi Mocks needs to handle property overrides.
  // mockStream.Setup.WillReturn(TEST_FILE_NAME).When.FileName;

  logger := TDUnitXXMLNUnitLogger.Create(mockStream);

  //TODO: Create decendant of TDUnitXXMLNUnitLogger which is a filestream version
  // Assert.AreEqual(logger.Filename, TEST_FILE_NAME);
end;



initialization
  TDUnitX.RegisterTestFixture(TDUnitX_LoggerXMLNUnitTests);
end.
