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
  System.Classes,
  DUnitX.TestFramework,
  DUnitX.Loggers.XML.NUnit;

type
{$M+}
  [TestFixture]
  TDUnitX_LoggerXMLNUnitTests = class
  public
    [Test]
    procedure FileLogger_Accepts_A_Bare_File_Name;
    [Test(false)]
    procedure OnTestingStarts_Fills_The_Start_Of_The_Stream_With_Header_Info;
    [Test]
    procedure OnTestingEnds_Escapes_Category_Names;
    //  [Test(false)]
  //    procedure OnTestingEnds_Fills_The_End_Of_The_Stream_With_Testing_Result_Info;
    [Test]
    procedure OnTestingEnds_Writes_The_Assembly_Time_As_A_Duration;
    [Test(false)]
    procedure OnTestWarning_Adds_Warnings_To_Be_Written_Out_On_Next_Error;
    procedure OnTestWarning_Adds_Warnings_To_Be_Written_Out_On_Next_Success;
    [Test]
    procedure OnTestingEnds_Reports_An_Errored_Fixture_As_Failed;
    [Test]
    procedure OnTestingEnds_Counts_A_Leak_As_A_Failure_In_The_Header;
  end;

  [TestFixture]
  TDUnitX_LoggerTextTests = class
  public
    [Test]
    procedure Issue_List_Keeps_Names_With_Equals_And_Empty_Messages;
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
  DUnitX.Loggers.Text,
  DUnitX.RunResults;

const
  CRLF              = #13#10;

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

procedure TDUnitX_LoggerXMLNUnitTests.OnTestingEnds_Writes_The_Assembly_Time_As_A_Duration;
var
  logger : ITestLogger;
  stream : TStringStream;
  categories : TList<string>;
  sTime : string;
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
    mockResult.Setup.WillReturn(TValue.From<TTestResultType>(TTestResultType.Failure)).When.ResultType;
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

    logger := TDUnitXXMLNUnitLogger.Create(stream);
    logger.OnTestingEnds(runResultsIntf);
    xml := stream.DataString;

    xml := Copy(xml, Pos('<test-suite type="Assembly"', xml), MaxInt);
    Assert.IsTrue(xml <> '', stream.DataString);
    sTime := Copy(xml, Pos('time="', xml) + Length('time="'), MaxInt);
    sTime := Copy(sTime, 1, Pos('"', sTime) - 1);
    Assert.AreEqual(Format('%.3f', [runResultsIntf.Duration.TotalSeconds]), sTime);
  finally
    logger := nil;
    stream.Free;
    categories.Free;
  end;
end;

procedure TDUnitX_LoggerXMLNUnitTests.OnTestWarning_Adds_Warnings_To_Be_Written_Out_On_Next_Error;
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

procedure TDUnitX_LoggerXMLNUnitTests.OnTestingEnds_Escapes_Category_Names;
var
  logger : ITestLogger;
  stream : TStringStream;
  categories : TList<string>;
  testCategories : TList<string>;
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
  testCategories := TList<string>.Create;
  testCategories.Add('Slow & Steady');
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
    mockTest.Setup.WillReturn(TValue.From<TList<string>>(testCategories)).When.Categories;
    mockTest.Setup.WillReturn(mockFixture.InstanceAsValue).When.Fixture;

    mockResult := TMock<ITestResult>.Create;
    mockResult.Setup.WillReturn(TValue.From<TTestResultType>(TTestResultType.Failure)).When.ResultType;
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

    logger := TDUnitXXMLNUnitLogger.Create(stream);
    logger.OnTestingEnds(runResultsIntf);
    xml := stream.DataString;

    Assert.Contains(xml, '<category name="Slow &amp; Steady" />');
  finally
    logger := nil;
    stream.Free;
    testCategories.Free;
    categories.Free;
  end;
end;

procedure TDUnitX_LoggerXMLNUnitTests.OnTestingEnds_Reports_An_Errored_Fixture_As_Failed;
var
  logger : ITestLogger;
  stream : TStringStream;
  categories : TList<string>;
  mockFixture : TMock<ITestFixtureInfo>;
  mockTest : TMock<ITestInfo>;
  mockError : TMock<ITestResult>;
  runResults : TDUnitXRunResults;
  runResultsIntf : IRunResults;
  context : ITestExecuteContext;
  fixtureResult : IFixtureResult;
begin
  categories := TList<string>.Create;
  stream := TStringStream.Create('', TEncoding.UTF8);
  try
    mockFixture := TMock<ITestFixtureInfo>.Create;
    mockFixture.Setup.WillReturn('ErroredFixture').When.Name;
    mockFixture.Setup.WillReturn('My.Namespace').When.NameSpace;
    mockFixture.Setup.WillReturn('My.Namespace.ErroredFixture').When.FullName;
    mockFixture.Setup.WillReturn(TValue.From<TClass>(TStringList)).When.TestClass;
    mockFixture.Setup.WillReturn(TValue.From<TList<string>>(categories)).When.Categories;

    mockTest := TMock<ITestInfo>.Create;
    mockTest.Setup.WillReturn('ATest').When.Name;
    mockTest.Setup.WillReturn('My.Namespace.ErroredFixture.ATest').When.FullName;
    mockTest.Setup.WillReturn(TValue.From<TList<string>>(categories)).When.Categories;
    mockTest.Setup.WillReturn(mockFixture.InstanceAsValue).When.Fixture;

    mockError := TMock<ITestResult>.Create;
    mockError.Setup.WillReturn(TValue.From<TTestResultType>(TTestResultType.Error)).When.ResultType;
    mockError.Setup.WillReturn(mockTest.InstanceAsValue).When.Test;
    mockError.Setup.WillReturn(TValue.From<TTimeSpan>(TTimeSpan.FromMilliseconds(0))).When.Duration;
    mockError.Setup.WillReturn('error message').When.Message;
    mockError.Setup.WillReturn('error stack').When.StackTrace;

    runResults := TDUnitXRunResults.Create;
    runResultsIntf := runResults;
    context := runResults;
    fixtureResult := TDUnitXFixtureResult.Create(nil, mockFixture.Instance);
    context.RecordFixture(fixtureResult);
    context.RecordResult(fixtureResult, mockError.Instance);
    context.RollupResults;

    logger := TDUnitXXMLNUnitLogger.Create(stream);
    logger.OnTestingEnds(runResultsIntf);

    Assert.Contains(stream.DataString, 'name="ErroredFixture" executed="True" result="Failure" success="False"');
  finally
    logger := nil;
    stream.Free;
    categories.Free;
  end;
end;

procedure TDUnitX_LoggerXMLNUnitTests.OnTestingEnds_Counts_A_Leak_As_A_Failure_In_The_Header;
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

    logger := TDUnitXXMLNUnitLogger.Create(stream);
    logger.OnTestingEnds(runResultsIntf);
    xml := stream.DataString;

    xml := Copy(xml, Pos('<test-results ', xml), MaxInt);
    xml := Copy(xml, 1, Pos('>', xml));
    Assert.Contains(xml, 'errors="0" failures="1"');
  finally
    logger := nil;
    stream.Free;
    categories.Free;
  end;
end;

procedure TDUnitX_LoggerXMLNUnitTests.FileLogger_Accepts_A_Bare_File_Name;
const
  BARE_FILE_NAME = 'dunitx-nunit-bare-file-name-test.xml';
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
        logger := TDUnitXXMLNUnitFileLogger.Create(BARE_FILE_NAME);
      end);
    Assert.IsTrue(FileExists(BARE_FILE_NAME), 'The log file was not created in the current directory');
  finally
    DeleteFile(BARE_FILE_NAME);
    SetCurrentDir(oldDir);
  end;
end;

procedure TDUnitX_LoggerTextTests.Issue_List_Keeps_Names_With_Equals_And_Empty_Messages;
var
  logger : ITestLogger;
  stream : TStringStream;
  mockTestA : TMock<ITestInfo>;
  mockTestB : TMock<ITestInfo>;
  mockFailure : TMock<ITestError>;
  mockIgnored : TMock<ITestResult>;
  runResults : IRunResults;
  text : string;
begin
  stream := TStringStream.Create('', TEncoding.UTF8);
  try
    mockTestA := TMock<ITestInfo>.Create;
    mockTestA.Setup.WillReturn('Fixture.Test(a=b)').When.FullName;
    mockTestB := TMock<ITestInfo>.Create;
    mockTestB.Setup.WillReturn('Fixture.Other').When.FullName;

    mockFailure := TMock<ITestError>.Create;
    mockFailure.Setup.WillReturn(mockTestA.InstanceAsValue).When.Test;
    mockFailure.Setup.WillReturn('x=y').When.Message;
    mockIgnored := TMock<ITestResult>.Create;
    mockIgnored.Setup.WillReturn(mockTestB.InstanceAsValue).When.Test;
    mockIgnored.Setup.WillReturn('').When.Message;

    runResults := TDUnitXRunResults.Create;
    logger := TDunitXTextLogger.Create(stream);
    logger.OnTestFailure(0, mockFailure.Instance);
    logger.OnTestIgnored(0, mockIgnored.Instance);
    logger.OnTestingEnds(runResults);
    text := stream.DataString;

    Assert.Contains(text, '1 Fixture.Test(a=b)', False, 'a test name containing = was split');
    Assert.Contains(text, 'x=y', False, 'the message was lost');
    Assert.Contains(text, '2 Fixture.Other', False, 'an issue with an empty message was dropped');
  finally
    logger := nil;
    stream.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TDUnitX_LoggerXMLNUnitTests);
  TDUnitX.RegisterTestFixture(TDUnitX_LoggerTextTests);
end.

