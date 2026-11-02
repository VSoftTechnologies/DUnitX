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

unit DUnitX.Tests.Loggers.XML.JUnit;

interface

{$I DUnitX.inc}

uses
  System.Classes,
  DUnitX.TestFramework,
  DUnitX.Loggers.XML.JUnit;

type
{$M+}
  [TestFixture]
  TDUnitX_LoggerXMLJUnitTests = class
  public
    [Test]
    procedure FileLogger_Accepts_A_Bare_File_Name;
    [Test(false)]
    procedure OnTestingStarts_Fills_The_Start_Of_The_Stream_With_Header_Info;
    //  [Test(false)]
  //    procedure OnTestingEnds_Fills_The_End_Of_The_Stream_With_Testing_Result_Info;
    [Test]
    procedure OnTestingEnds_Counts_A_Leak_As_An_Error_In_The_Testsuite;
    [Test(false)]
    procedure OnTestWarning_Adds_Warnings_To_Be_Written_Out_On_Next_Error;
    procedure OnTestWarning_Adds_Warnings_To_Be_Written_Out_On_Next_Success;
    [Test]
    procedure OnTestingEnds_Writes_An_ISO_8601_Timestamp;
    [Test]
    procedure OnTestingEnds_Counts_A_Leak_As_An_Error_In_The_Header;
    [Test]
    procedure OnTestingEnds_Names_The_Suite_By_Removing_Only_The_Trailing_Fixture_Name;
    [Test]
    procedure OnTestingEnds_Writes_The_Stack_Trace_In_The_Failure_Body;
  private
    function LogOneResult(const fixtureName, fixtureNamespace : string; const resultType : TTestResultType) : string;
  end;

implementation

uses
  System.Rtti,
  System.SysUtils,
  System.Generics.Collections,
  System.TimeSpan,
  System.DateUtils,
  Delphi.Mocks,
  DUnitX.Generics,
  DUnitX.FixtureResult,
  DUnitX.InternalInterfaces,
  DUnitX.RunResults;

const
  CRLF              = #13#10;

  { TDUnitX_LoggerXMLNUnit }

  {
  procedure TDUnitX_LoggerXMLJUnitTests.OnTestingEnds_Fills_The_End_Of_The_Stream_With_Testing_Result_Info;
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
procedure TDUnitX_LoggerXMLJUnitTests.OnTestingStarts_Fills_The_Start_Of_The_Stream_With_Header_Info;
var
  sUnicodePreamble : string;
  logger : ITestLogger;
  mockStream : TStringStream;
  sOnTestingStartsText : string;
  sHeader : string;
  sResults : string;
  sAppName : string;
  sExpectedResults : string;
  sExpectedAppName : string;
  iPrevLastChar : Integer;
const
  EXPECTED_HEADER   = '<?xml version="1.0" encoding="UTF-8" standalone="yes" ?>';
  EXPECTED_RESULTS_FORMAT_STR = '<test-results total="%d" notrun="%d" date="%s" time="%s" >';
  EXPECTED_APP_NAME_FORMAT_STR = '<application name="%s" />';
begin
  //TODO: Break this unit tests into three separate specific tests for each of the header sections.
  mockStream := TStringStream.Create('', TEncoding.UTF8);
  logger := TDUnitXXMLJUnitLogger.Create(mockStream);
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

procedure TDUnitX_LoggerXMLJUnitTests.OnTestingEnds_Counts_A_Leak_As_An_Error_In_The_Testsuite;
var
  logger : ITestLogger;
  stream : TStringStream;
  categories : TList<string>;
  mockFixture : TMock<ITestFixtureInfo>;
  mockTest : TMock<ITestInfo>;
  mockResult : TMock<ITestResult>;
  runResults : TDUnitXRunResults;
  runResultsIntf : IRunResults;
  context : ITestExecuteContext;
  fixtureResult : IFixtureResult;
  xml : string;
begin
  categories := TList<string>.Create;
  stream := TStringStream.Create('', TEncoding.UTF8);
  try
    mockFixture := TMock<ITestFixtureInfo>.Create;
    mockFixture.Setup.WillReturn('AFixture').When.Name;
    mockFixture.Setup.WillReturn('My.Namespace').When.NameSpace;
    mockFixture.Setup.WillReturn('My.Namespace.AFixture').When.FullName;
    mockFixture.Setup.WillReturn(TValue.From<TClass>(TStringList)).When.TestClass;
    mockFixture.Setup.WillReturn(TValue.From<TList<string>>(categories)).When.Categories;

    mockTest := TMock<ITestInfo>.Create;
    mockTest.Setup.WillReturn('ATest').When.Name;
    mockTest.Setup.WillReturn('My.Namespace.AFixture.ATest').When.FullName;
    mockTest.Setup.WillReturn(TValue.From<TList<string>>(categories)).When.Categories;
    mockTest.Setup.WillReturn(mockFixture.InstanceAsValue).When.Fixture;

    mockResult := TMock<ITestResult>.Create;
    mockResult.Setup.WillReturn(TValue.From<TTestResultType>(TTestResultType.MemoryLeak)).When.ResultType;
    mockResult.Setup.WillReturn(mockTest.InstanceAsValue).When.Test;
    mockResult.Setup.WillReturn(TValue.From<TTimeSpan>(TTimeSpan.FromMilliseconds(0))).When.Duration;
    mockResult.Setup.WillReturn('result message').When.Message;
    mockResult.Setup.WillReturn('result stack').When.StackTrace;

    runResults := TDUnitXRunResults.Create;
    runResultsIntf := runResults;
    context := runResults;
    fixtureResult := TDUnitXFixtureResult.Create(nil, mockFixture.Instance);
    context.RecordFixture(fixtureResult);
    context.RecordResult(fixtureResult, mockResult.Instance);
    context.RollupResults;

    logger := TDUnitXXMLJUnitLogger.Create(stream);
    logger.OnTestingEnds(runResultsIntf);
    xml := stream.DataString;

    xml := Copy(xml, Pos('<testsuite name=', xml), MaxInt);
    xml := Copy(xml, 1, Pos('>', xml));
    Assert.Contains(xml, 'tests="1" disabled="0" errors="1" failures="0"');
  finally
    logger := nil;
    stream.Free;
    categories.Free;
  end;
end;

procedure TDUnitX_LoggerXMLJUnitTests.OnTestWarning_Adds_Warnings_To_Be_Written_Out_On_Next_Error;
var
  logger : ITestLogger;
  mockStream : TStringStream;
  mockWarning : TMock<ITestResult>;
  mockFixture : TMock<ITestFixtureInfo>;
  mockTest : TMock<ITestInfo>;
  mockError : TMock<ITestError>;

  sExceptedWarning : string;
  iPositionOfWarning : Integer;
begin
  //Mocks
  mockStream := TStringStream.Create('', TEncoding.UTF8);
  mockWarning := TMock<ITestResult>.Create;
  mockFixture := TMock<ITestFixtureInfo>.Create;
  mockTest := TMock<ITestInfo>.Create;
  mockError := TMock<ITestError>.Create;

  //System under test
  logger := TDUnitXXMLJUnitLogger.Create(mockStream);

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

procedure TDUnitX_LoggerXMLJUnitTests.OnTestWarning_Adds_Warnings_To_Be_Written_Out_On_Next_Success;
var
  logger : ITestLogger;
  mockStream : TStringStream;
  mockWarning : TMock<ITestResult>;
  mockTest : TMock<ITestInfo>;
  mockSuccess : TMock<ITestError>;

  sExceptedWarning : string;
  iPositionOfWarning : Integer;
begin
  //Mocks
  mockStream := TStringStream.Create('', TEncoding.UTF8);
  mockWarning := TMock<ITestResult>.Create;
  mockTest := TMock<ITestInfo>.Create;
  mockSuccess := TMock<ITestError>.Create;

  //System under test
  logger := TDUnitXXMLJUnitLogger.Create(mockStream);

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

procedure TDUnitX_LoggerXMLJUnitTests.OnTestingEnds_Writes_An_ISO_8601_Timestamp;
var
  logger : ITestLogger;
  stream : TStringStream;
  categories : TList<string>;
  mockFixture : TMock<ITestFixtureInfo>;
  mockTest : TMock<ITestInfo>;
  mockResult : TMock<ITestResult>;
  runResults : TDUnitXRunResults;
  runResultsIntf : IRunResults;
  context : ITestExecuteContext;
  fixtureResult : IFixtureResult;
  xml : string;
  stamp : string;
  oldTimeSeparator : Char;
begin
  categories := TList<string>.Create;
  stream := TStringStream.Create('', TEncoding.UTF8);
  oldTimeSeparator := FormatSettings.TimeSeparator;
  try
    mockFixture := TMock<ITestFixtureInfo>.Create;
    mockFixture.Setup.WillReturn('AFixture').When.Name;
    mockFixture.Setup.WillReturn('My.Namespace').When.NameSpace;
    mockFixture.Setup.WillReturn('My.Namespace.AFixture').When.FullName;
    mockFixture.Setup.WillReturn(TValue.From<TClass>(TStringList)).When.TestClass;
    mockFixture.Setup.WillReturn(TValue.From<TList<string>>(categories)).When.Categories;

    mockTest := TMock<ITestInfo>.Create;
    mockTest.Setup.WillReturn('ATest').When.Name;
    mockTest.Setup.WillReturn('My.Namespace.AFixture.ATest').When.FullName;
    mockTest.Setup.WillReturn(TValue.From<TList<string>>(categories)).When.Categories;
    mockTest.Setup.WillReturn(mockFixture.InstanceAsValue).When.Fixture;

    mockResult := TMock<ITestResult>.Create;
    mockResult.Setup.WillReturn(TValue.From<TTestResultType>(TTestResultType.Failure)).When.ResultType;
    mockResult.Setup.WillReturn(mockTest.InstanceAsValue).When.Test;
    mockResult.Setup.WillReturn(TValue.From<TTimeSpan>(TTimeSpan.FromMilliseconds(0))).When.Duration;
    mockResult.Setup.WillReturn('failure message').When.Message;
    mockResult.Setup.WillReturn('failure stack').When.StackTrace;

    runResults := TDUnitXRunResults.Create;
    runResultsIntf := runResults;
    context := runResults;
    fixtureResult := TDUnitXFixtureResult.Create(nil, mockFixture.Instance);
    context.RecordFixture(fixtureResult);
    context.RecordResult(fixtureResult, mockResult.Instance);
    context.RollupResults;

    FormatSettings.TimeSeparator := '.';
    logger := TDUnitXXMLJUnitLogger.Create(stream);
    logger.OnTestingEnds(runResultsIntf);
    xml := stream.DataString;

    Assert.IsTrue(Pos('timestamp="', xml) > 0, xml);
    stamp := Copy(xml, Pos('timestamp="', xml) + Length('timestamp="'), 19);
    Assert.AreEqual('T', Copy(stamp, 11, 1), stamp);
    Assert.AreEqual(':', Copy(stamp, 14, 1), stamp);
    Assert.AreEqual(':', Copy(stamp, 17, 1), stamp);
  finally
    FormatSettings.TimeSeparator := oldTimeSeparator;
    logger := nil;
    stream.Free;
    categories.Free;
  end;
end;

procedure TDUnitX_LoggerXMLJUnitTests.OnTestingEnds_Counts_A_Leak_As_An_Error_In_The_Header;
var
  logger : ITestLogger;
  stream : TStringStream;
  categories : TList<string>;
  mockFixture : TMock<ITestFixtureInfo>;
  mockTest : TMock<ITestInfo>;
  mockResult : TMock<ITestResult>;
  runResults : TDUnitXRunResults;
  runResultsIntf : IRunResults;
  context : ITestExecuteContext;
  fixtureResult : IFixtureResult;
  xml : string;
begin
  categories := TList<string>.Create;
  stream := TStringStream.Create('', TEncoding.UTF8);
  try
    mockFixture := TMock<ITestFixtureInfo>.Create;
    mockFixture.Setup.WillReturn('AFixture').When.Name;
    mockFixture.Setup.WillReturn('My.Namespace').When.NameSpace;
    mockFixture.Setup.WillReturn('My.Namespace.AFixture').When.FullName;
    mockFixture.Setup.WillReturn(TValue.From<TClass>(TStringList)).When.TestClass;
    mockFixture.Setup.WillReturn(TValue.From<TList<string>>(categories)).When.Categories;

    mockTest := TMock<ITestInfo>.Create;
    mockTest.Setup.WillReturn('ATest').When.Name;
    mockTest.Setup.WillReturn('My.Namespace.AFixture.ATest').When.FullName;
    mockTest.Setup.WillReturn(TValue.From<TList<string>>(categories)).When.Categories;
    mockTest.Setup.WillReturn(mockFixture.InstanceAsValue).When.Fixture;

    mockResult := TMock<ITestResult>.Create;
    mockResult.Setup.WillReturn(TValue.From<TTestResultType>(TTestResultType.MemoryLeak)).When.ResultType;
    mockResult.Setup.WillReturn(mockTest.InstanceAsValue).When.Test;
    mockResult.Setup.WillReturn(TValue.From<TTimeSpan>(TTimeSpan.FromMilliseconds(0))).When.Duration;
    mockResult.Setup.WillReturn('result message').When.Message;
    mockResult.Setup.WillReturn('result stack').When.StackTrace;

    runResults := TDUnitXRunResults.Create;
    runResultsIntf := runResults;
    context := runResults;
    fixtureResult := TDUnitXFixtureResult.Create(nil, mockFixture.Instance);
    context.RecordFixture(fixtureResult);
    context.RecordResult(fixtureResult, mockResult.Instance);
    context.RollupResults;

    logger := TDUnitXXMLJUnitLogger.Create(stream);
    logger.OnTestingEnds(runResultsIntf);
    xml := stream.DataString;

    xml := Copy(xml, Pos('<testsuites ', xml), MaxInt);
    xml := Copy(xml, 1, Pos('>', xml));
    Assert.Contains(xml, 'errors="1" failures="0"');
  finally
    logger := nil;
    stream.Free;
    categories.Free;
  end;
end;

function TDUnitX_LoggerXMLJUnitTests.LogOneResult(const fixtureName, fixtureNamespace : string; const resultType : TTestResultType) : string;
var
  logger : ITestLogger;
  stream : TStringStream;
  categories : TList<string>;
  mockFixture : TMock<ITestFixtureInfo>;
  mockTest : TMock<ITestInfo>;
  mockResult : TMock<ITestResult>;
  runResults : TDUnitXRunResults;
  runResultsIntf : IRunResults;
  context : ITestExecuteContext;
  fixtureResult : IFixtureResult;
begin
  categories := TList<string>.Create;
  stream := TStringStream.Create('', TEncoding.UTF8);
  try
    mockFixture := TMock<ITestFixtureInfo>.Create;
    mockFixture.Setup.WillReturn(fixtureName).When.Name;
    mockFixture.Setup.WillReturn(fixtureNamespace).When.NameSpace;
    mockFixture.Setup.WillReturn(fixtureNamespace + '.' + fixtureName).When.FullName;
    mockFixture.Setup.WillReturn(TValue.From<TClass>(TStringList)).When.TestClass;
    mockFixture.Setup.WillReturn(TValue.From<TList<string>>(categories)).When.Categories;

    mockTest := TMock<ITestInfo>.Create;
    mockTest.Setup.WillReturn('ATest').When.Name;
    mockTest.Setup.WillReturn(fixtureNamespace + '.' + fixtureName + '.ATest').When.FullName;
    mockTest.Setup.WillReturn(TValue.From<TList<string>>(categories)).When.Categories;
    mockTest.Setup.WillReturn(mockFixture.InstanceAsValue).When.Fixture;

    mockResult := TMock<ITestResult>.Create;
    mockResult.Setup.WillReturn(TValue.From<TTestResultType>(resultType)).When.ResultType;
    mockResult.Setup.WillReturn(mockTest.InstanceAsValue).When.Test;
    mockResult.Setup.WillReturn(TValue.From<TTimeSpan>(TTimeSpan.FromMilliseconds(0))).When.Duration;
    mockResult.Setup.WillReturn('result message').When.Message;
    mockResult.Setup.WillReturn('result stack').When.StackTrace;

    runResults := TDUnitXRunResults.Create;
    runResultsIntf := runResults;
    context := runResults;
    fixtureResult := TDUnitXFixtureResult.Create(nil, mockFixture.Instance);
    context.RecordFixture(fixtureResult);
    context.RecordResult(fixtureResult, mockResult.Instance);
    context.RollupResults;

    logger := TDUnitXXMLJUnitLogger.Create(stream);
    logger.OnTestingEnds(runResultsIntf);
    result := stream.DataString;
  finally
    logger := nil;
    stream.Free;
    categories.Free;
  end;
end;

procedure TDUnitX_LoggerXMLJUnitTests.OnTestingEnds_Names_The_Suite_By_Removing_Only_The_Trailing_Fixture_Name;
var
  xml : string;
begin
  //The namespace contains '.TFoo' too; only the trailing '.TFoo' may be removed from the full name.
  xml := LogOneResult('TFoo', 'My.TFooTests', TTestResultType.Pass);
  xml := Copy(xml, Pos('<testsuite ', xml), MaxInt);
  xml := Copy(xml, 1, Pos('>', xml));
  Assert.Contains(xml, 'name="My.TFooTests"', false);
end;

procedure TDUnitX_LoggerXMLJUnitTests.OnTestingEnds_Writes_The_Stack_Trace_In_The_Failure_Body;
var
  xml : string;
begin
  xml := LogOneResult('AFixture', 'My.Namespace', TTestResultType.Failure);
  Assert.Contains(xml, '<failure message="result message">', false);
  Assert.Contains(xml, '<![CDATA[ result stack ]]>', false);
end;

procedure TDUnitX_LoggerXMLJUnitTests.FileLogger_Accepts_A_Bare_File_Name;
const
  BARE_FILE_NAME = 'dunitx-junit-bare-file-name-test.xml';
var
  oldDir : string;
begin
  //A file name with no directory part must not make the logger call ForceDirectories('')
  oldDir := GetCurrentDir;
  SetCurrentDir(GetEnvironmentVariable('TEMP'));
  try
    Assert.WillNotRaiseAny(
      procedure
      var
        logger : ITestLogger;
      begin
        logger := TDUnitXXMLJUnitFileLogger.Create(BARE_FILE_NAME);
      end);
    Assert.IsTrue(FileExists(BARE_FILE_NAME), 'The log file was not created in the current directory');
  finally
    DeleteFile(BARE_FILE_NAME);
    SetCurrentDir(oldDir);
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TDUnitX_LoggerXMLJUnitTests);
end.

