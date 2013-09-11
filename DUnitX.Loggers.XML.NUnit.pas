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

unit DUnitX.Loggers.XML.NUnit;

interface

uses
  DUnitX.TestFramework,
  classes;

{$I DUnitX.inc}

type
  TDUnitXXMLNUnitLogger = class(TInterfacedObject, ITestLogger)
  private
    FOutputStream : TStream;

    FLogList : TStringList;
    FWarningList : TStringList;

    procedure WriteXMLLine(const AXMLLine: string);
    procedure WriteInfoAndWarningsXML;

    function HasInfoOrWarnings: Boolean;

  protected
    procedure OnTestingStarts(const threadId, testCount, testActiveCount: Cardinal);

    procedure OnStartTestFixture(const threadId: Cardinal; const fixture: ITestFixtureInfo);

    procedure OnSetupFixture(const threadId: Cardinal; const fixture: ITestFixtureInfo);
    procedure OnEndSetupFixture(const threadId: Cardinal; const fixture: ITestFixtureInfo);

    procedure OnBeginTest(const threadId: Cardinal; const Test: ITestInfo);

    procedure OnSetupTest(const threadId: Cardinal; const Test: ITestInfo);
    procedure OnEndSetupTest(const threadId: Cardinal; const Test: ITestInfo);

    procedure OnExecuteTest(const threadId: Cardinal; const Test: ITestInfo);

    procedure OnTestSuccess(const threadId: Cardinal; const Success: ITestResult);
    procedure OnTestWarning(const threadId: Cardinal; const Warning: ITestResult);
    procedure OnTestError(const threadId: Cardinal; const Error: ITestError);
    procedure OnTestFailure(const threadId: Cardinal; const Failure: ITestError);
    procedure OnTestIgnored(const threadId: Cardinal; const Ignored: ITestResult);

    procedure OnLog(const logType: TLogLevel; const msg: string);

    procedure OnTeardownTest(const threadId: Cardinal; const Test: ITestInfo);
    procedure OnEndTeardownTest(const threadId: Cardinal; const Test: ITestInfo);

    procedure OnEndTest(const threadId: Cardinal; const Test: ITestResult);

    procedure OnTearDownFixture(const threadId: Cardinal; const fixture: ITestFixtureInfo);
    procedure OnEndTearDownFixture(const threadId: Cardinal; const fixture: ITestFixtureInfo);

    procedure OnEndTestFixture(const threadId: Cardinal; const results: IFixtureResult);

    procedure OnTestingEnds(const TestResults: ITestResults);
  public
    constructor Create(const AOutputStream : TStream);
    destructor Destroy;override;
  end;

  TDUnitXXMLNUnitFileLogger = class(TDUnitXXMLNUnitLogger)
  private
    FXMLFileStream : TFileStream;
  public
    constructor Create(const AFilename: string = '');
  end;

implementation

uses
  {$IFDEF MSWINDOWS}
    {$if CompilerVersion < 23 }
      Forms,
      Windows,
    {$else}
      Vcl.Forms,
      WinAPI.Windows, // Delphi XE2 (CompilerVersion 23) added scopes in front of unit names
    {$ifend}
  {$ENDIF}
  DUnitX.Utils.XML,
  SysUtils;

const
  NUNIT_LOGGER_CRLF = #13#10;

{ TDUnitXTextFileLogger }

procedure TDUnitXXMLNUnitLogger.WriteInfoAndWarningsXML;
var
  log : string;
  warning : string;
begin
  if FLogList.Count > 0 then
  begin
    for log in FLogList do
      WriteXMLLine('<status>' + EscapeForXML(log, false) + '</status>');
  end;

  if FWarningList.Count > 0 then
  begin
    for warning in FWarningList do
      WriteXMLLine('<warning>' + EscapeForXML(warning, false) + '</warning>');
  end;
end;

constructor TDUnitXXMLNUnitLogger.Create(const AOutputStream: TStream);
begin
  //We are given this stream to use as we see fit, would pass in an interface but there is none for streams.
  FOutputStream := AOutputStream;
  FLogList := TStringList.Create;
  FWarningList := TStringList.Create;
end;

destructor TDUnitXXMLNUnitLogger.Destroy;
begin
  FOutputStream.Free;
  FLogList.Free;
  FWarningList.Free;

  inherited;
end;

function TDUnitXXMLNUnitLogger.HasInfoOrWarnings: Boolean;
begin
  Result := (FLogList.Count > 0) or (FWarningList.Count > 0);
end;

procedure TDUnitXXMLNUnitLogger.OnBeginTest(const threadId: Cardinal; const Test: ITestInfo);
begin
  FLogList.Clear;
  FWarningList.Clear;
end;

procedure TDUnitXXMLNUnitLogger.OnEndSetupFixture(const threadId: Cardinal; const fixture: ITestFixtureInfo);
begin

end;

procedure TDUnitXXMLNUnitLogger.OnEndSetupTest(const threadId: Cardinal; const Test: ITestInfo);
begin

end;

procedure TDUnitXXMLNUnitLogger.OnEndTearDownFixture(const threadId: Cardinal; const fixture: ITestFixtureInfo);
begin

end;

procedure TDUnitXXMLNUnitLogger.OnEndTeardownTest(const threadId: Cardinal; const Test: ITestInfo);
begin

end;

procedure TDUnitXXMLNUnitLogger.OnEndTest(const threadId: Cardinal; const Test: ITestResult);
begin

end;

procedure TDUnitXXMLNUnitLogger.OnEndTestFixture(const threadId: Cardinal; const results: IFixtureResult);
begin
   WriteXMLLine('</results>');
   WriteXMLLine('</test-suite>');
end;

procedure TDUnitXXMLNUnitLogger.OnExecuteTest(const threadId: Cardinal; const Test: ITestInfo);
begin

end;

procedure TDUnitXXMLNUnitLogger.OnLog(const logType: TLogLevel; const msg: string);
begin
  FLogList.Add(Format('STATUS: %s: %s', [TLogLevelDesc[logType], msg]));
end;

procedure TDUnitXXMLNUnitLogger.OnSetupFixture(const threadId: Cardinal; const fixture: ITestFixtureInfo);
begin

end;

procedure TDUnitXXMLNUnitLogger.OnSetupTest(const threadId: Cardinal; const Test: ITestInfo);
begin

end;

procedure TDUnitXXMLNUnitLogger.OnStartTestFixture(const threadId: Cardinal; const fixture: ITestFixtureInfo);
var
  sType : string;
begin
  if fixture.Tests.Count > 0 then
    sType := 'TestFixture'
  else
    sType := 'Namespace';

  WriteXMLLine(Format('<test-suite type="%s" name="%s" total="%d" notrun="%d">', [sType,fixture.Name, fixture.TestCount, fixture.TestCount - fixture.ActiveTestCount]));
  WriteXMLLine('<results>');
end;

procedure TDUnitXXMLNUnitLogger.OnTearDownFixture(const threadId: Cardinal; const fixture: ITestFixtureInfo);
begin

end;

procedure TDUnitXXMLNUnitLogger.OnTeardownTest(const threadId: Cardinal; const Test: ITestInfo);
begin

end;

procedure TDUnitXXMLNUnitLogger.OnTestError(const threadId: Cardinal; const Error: ITestError);
begin
  //TODO: Getting Test, and Fixture from Error is painful for testing. Therefore its painful for setup, and use?

  WriteXMLLine(Format('<test-case name="%s" executed="%s" success="False" time="%1.3f" result="Error">',
                    [EscapeForXML(Error.Test.FullName), BoolToStr(Error.Test.Active, True),
                      Error.TestDuration.TotalMilliseconds / 1000]));

  WriteXMLLine(Format('<failure name="%s" location="%s">', [EscapeForXML(error.ExceptionClass.ClassName), EscapeForXML(error.ExceptionLocationInfo)]));
  WriteXMLLine(Format('<message>%s</message>', [EscapeForXML(error.ExceptionMessage, false)]));
  WriteXMLLine('</failure>');
  WriteInfoAndWarningsXML;
  WriteXMLLine('</test-case>');
end;

procedure TDUnitXXMLNUnitLogger.OnTestFailure(const threadId: Cardinal; const Failure: ITestError);
begin
  WriteXMLLine(Format('<test-case name="%s" executed="%s" success="False" time="%1.3f" result="Failure">',
                    [EscapeForXML(Failure.Test.FullName), BoolToStr(Failure.Test.Active, True),
                    Failure.TestDuration.Milliseconds / 1000]));
  WriteXMLLine(Format('<failure name="%s" location="%s">', [EscapeForXML(Failure.ExceptionClass.ClassName), EscapeForXML(Failure.ExceptionLocationInfo)]));
  WriteXMLLine(Format('<message>%s</message>', [EscapeForXML(Failure.ExceptionMessage, false)]));
  WriteXMLLine('</failure>');
  WriteInfoAndWarningsXML;
  WriteXMLLine('</test-case>');
end;

procedure TDUnitXXMLNUnitLogger.OnTestIgnored(const threadId: Cardinal; const Ignored: ITestResult);
var
  endTag : string;
  fixture: ITestFixtureInfo;
begin
  if Ignored.Message <> '' then
    endTag := '>'
  else
    endTag := '/>';

  fixture := Ignored.Test.Fixture;

  WriteXMLLine(Format('<test-case name="%s" executed="False" result="Ignored" %s',
                     [EscapeForXML(Ignored.Test.FullName),endTag]));

  if Ignored.Message <> '' then
  begin
    WriteXMLLine('<status>' + EscapeForXML(Ignored.Message, false) + '</status>');
    WriteXMLLine('</test-case>');
  end;
end;

procedure TDUnitXXMLNUnitLogger.OnTestingEnds(const TestResults: ITestResults);
begin
  WriteXMLLine('<statistics>' + NUNIT_LOGGER_CRLF +
                  Format('<stat name="tests" value="%d" />', [TestResults.Count]) + NUNIT_LOGGER_CRLF +
                  Format('<stat name="failures" value="%d" />', [TestResults.FailureCount]) + NUNIT_LOGGER_CRLF +
                  Format('<stat name="errors" value="%d" />', [TestResults.ErrorCount]) + NUNIT_LOGGER_CRLF +
                  Format('<stat name="ignored" value="%d" />', [TestResults.IgnoredCount]) + NUNIT_LOGGER_CRLF +
                  Format('<stat name="success-rate" value="%d%%" />', [TestResults.SuccessRate]) + NUNIT_LOGGER_CRLF +
                  Format('<stat name="started-at" value="%s" />', [DateTimeToStr(TestResults.StartTime)]) + NUNIT_LOGGER_CRLF +
                  Format('<stat name="finished-at" value="%s" />', [DateTimeToStr(TestResults.FinishTime)]) + NUNIT_LOGGER_CRLF +
                  Format('<stat name="runtime" value="%1.3f"/>', [TestResults.TestDuration.TotalMilliseconds / 1000]) + NUNIT_LOGGER_CRLF +
                  '</statistics>' + NUNIT_LOGGER_CRLF +
              '</test-results>');

  //TODO: Do we need to write to the console here?
end;

procedure TDUnitXXMLNUnitLogger.OnTestingStarts(const threadId, testCount, testActiveCount: Cardinal);
var
  unicodePreamble: TBytes;
  dtNow: TDateTime;
begin
   //write the byte order mark
   unicodePreamble := TEncoding.UTF8.GetPreamble;

   if Length(unicodePreamble) > 0 then
      FOutputStream.WriteBuffer(unicodePreamble[0], Length(unicodePreamble));

   WriteXMLLine('<?xml version="1.0" encoding="UTF-8" standalone="yes" ?>');

   dtNow := Now;
   WriteXMLLine(Format('<test-results total="%d" notrun="%d" date="%s" time="%s" >',
                         [testCount,
                           testCount - testActiveCount,
                             DateToStr(dtNow),
                               TimeToStr(dtNow)]));

   WriteXMLLine(Format('<application name="%s" />',[ExtractFileName(ParamStr(0))]));
end;

procedure TDUnitXXMLNUnitLogger.OnTestSuccess(const threadId: Cardinal; const Success: ITestResult);
var
  endTag : string;
  fixture: ITestFixtureInfo;
begin
  if HasInfoOrWarnings then
    endTag := '>'
  else
    endTag := '/>';

  fixture := Success.Test.Fixture;

  WriteXMLLine(Format('<test-case name="%s" executed="%s" success="True" time="%1.3f" result="Pass" %s',
                     [EscapeForXML(Success.Test.FullName),  BoolToStr(Success.Test.Active, True),
                     Success.TestDuration.TotalMilliseconds / 1000, endTag]));

  if HasInfoOrWarnings then
  begin
    WriteInfoAndWarningsXML;
    WriteXMLLine('</test-case>');
  end;
end;

procedure TDUnitXXMLNUnitLogger.OnTestWarning(const threadId: Cardinal; const Warning: ITestResult);
begin
  FWarningList.Add(Format('WARNING: %s: %s', [Warning.Test.Name, Warning.Message]));
end;


procedure TDUnitXXMLNUnitLogger.WriteXMLLine(const AXMLLine: string);
var
  btUTF8Buffer : TBytes;
  sLine: string;
begin
  sLine := AXMLLine + NUNIT_LOGGER_CRLF;
  if FOutputStream <> nil then
  begin
    btUTF8Buffer := TEncoding.UTF8.GetBytes(AXMLLine);
    FOutputStream.WriteBuffer(btUTF8Buffer[0],Length(btUTF8Buffer));
  end
  else
    WriteLn(AXMLLine);
end;

{ TDUnitXXMLNUnitLoggerFile }

constructor TDUnitXXMLNUnitFileLogger.Create(const AFilename: string = '');
var
  sXmlFilename: string;
const
  DEFAULT_NUNIT_FILE_NAME = 'dunit-report.xml';
begin
  sXmlFilename := AFilename;

  if sXmlFilename = '' then
    sXmlFilename := ExtractFilePath(Application.ExeName) + DEFAULT_NUNIT_FILE_NAME;

  FXMLFileStream := TFileStream.Create(sXmlFilename, fmCreate);

  //The stream class will take care of cleaning this up for us.
  inherited Create(FXMLFileStream);
end;

end.
