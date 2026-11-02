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

unit DUnitX.Loggers.XML.JUnit;

interface

{$I DUnitX.inc}

uses
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
  DUnitX.TestFramework,
  DUnitX.Loggers.Null;

type
  TDUnitXXMLJUnitLogger = class(TDUnitXNullLogger)
  private
    FOutputStream : TStream;
    FOwnsStream : boolean;
    FIndent : integer;
    FFormatSettings : TFormatSettings;
  protected
    procedure Indent;
    procedure Outdent;
    procedure WriteXMLLine(const value : string);

    procedure OnTestingEnds(const RunResults : IRunResults); override;

    procedure WriteCategoryNodes(const ACategoryList : TList<string>);
    procedure WriteFixtureResult(const fixtureResult : IFixtureResult);
    procedure WriteTestResult(const testResult : ITestResult);

    function Format(const Format : string; const Args : array of const) : string;
  public
    constructor Create(const AOutputStream : TStream; const AOwnsStream : boolean = false);
    destructor Destroy; override;
  end;

  TDUnitXXMLJUnitFileLogger = class(TDUnitXXMLJUnitLogger)
  public
    constructor Create(const AFilename : string = '');
  end;

implementation

uses
  DUnitX.Utils.XML,
  System.StrUtils,
  System.TypInfo;

{ TDUnitXXMLJUnitLogger }

constructor TDUnitXXMLJUnitLogger.Create(const AOutputStream : TStream; const AOwnsStream : boolean = false);
var
  preamble : TBytes;
begin
  inherited Create;
  FFormatSettings := TFormatSettings.Create;
  FFormatSettings.ThousandSeparator := ',';
  FFormatSettings.DecimalSeparator := '.';
  FOutputStream := AOutputStream;
  FOwnsStream := AOwnsStream;

  Preamble := TEncoding.UTF8.GetPreamble;
  FOutputStream.WriteBuffer(preamble[0], Length(preamble));

end;

destructor TDUnitXXMLJUnitLogger.Destroy;
begin
  if FOwnsStream then
    FOutputStream.Free;
  inherited;
end;

function TDUnitXXMLJUnitLogger.Format(const Format : string; const Args : array of const) : string;
begin
  Result := System.SysUtils.Format(Format, Args, FFormatSettings);
end;

procedure TDUnitXXMLJUnitLogger.Indent;
begin
  Inc(FIndent, 2);
end;

procedure TDUnitXXMLJUnitLogger.OnTestingEnds(const RunResults : IRunResults);

  procedure LogFixture(const fixture : IFixtureResult; level : integer);
  var
    child : IFixtureResult;
    sLevel : string;
  begin
    sLevel := StringOfChar(' ', level * 2);
    System.WriteLn(sLevel + fixture.Fixture.NameSpace + ':' + fixture.Fixture.Name + Format(' [Tests: %d] [Children: %d] [Passed : %d]', [fixture.ResultCount,
      fixture.ChildCount, fixture.PassCount]));

    Inc(level);
    for child in fixture.Children do
    begin
      LogFixture(child, level);
    end;

  end;

var
  fixtureRes : IFixtureResult;
  sExeName : string;
  sTime : string;
  //totalTests  : integer;
begin

  { first things first, rollup the namespaces.
    So, where parent fixtures have no tests, or only one child fixture, combine into a single fixture.
    }
  for fixtureRes in RunResults.FixtureResults do
  begin
    fixtureRes.Reduce;
    //    LogFixture(fixtureRes,0);
  end;

  //JUnit reports the total without the Ignored.
//  totalTests := RunResults.TestCount - RunResults.IgnoredCount;

  sExeName := ParamStr(0);
  FIndent := 0;
  sTime := Format('%.3f', [RunResults.Duration.TotalSeconds]);

  WriteXMLLine('<?xml version="1.0" encoding="UTF-8"?>');

  // Global overview
  // There is only a "disabled" attribute on the top level, but "disabled" and "skipped" on fixture level. Return ignored tests as "disabled"
  WriteXMLLine(Format('<testsuites name="%s" tests="%d" disabled="%d" errors="%d" failures="%d" time="%s">',
      [sExeName, RunResults.TestCount, RunResults.IgnoredCount, RunResults.ErrorCount + RunResults.MemoryLeakCount, RunResults.FailureCount, sTime]));
  //A leaked test is written as <error>, so it is counted as one.

  Indent;

  for fixtureRes in RunResults.FixtureResults do
    WriteFixtureResult(fixtureRes);

  Outdent;
  WriteXMLLine('</testsuites>');
end;

procedure TDUnitXXMLJUnitLogger.Outdent;
begin
  Dec(FIndent, 2);
end;

procedure TDUnitXXMLJUnitLogger.WriteFixtureResult(const fixtureResult : IFixtureResult);
var
  sTime : string;
  sDate : string;
  child : IFixtureResult;
  testResult : ITestResult;
  sExecuted : string;
  sName : string;
  ignoredCount : integer;
  errorCount : integer;
  failureCount : integer;
begin
  //its a real fixture if the class is not TObject.
  if (not fixtureResult.Fixture.TestClass.ClassNameIs('TObject')) then
  begin
    //if there were no tests then just ignore this fixture.
    if fixtureResult.ResultCount = 0 then
      exit;
    sExecuted := BoolToStr(fixtureResult.ResultCount > 0, true);
    sExecuted := EscapeForXML(sExecuted);

    //The suite name is the full name without its trailing '.Name' (not the first '.Name' found).
    sName := fixtureResult.Fixture.FullName;
    if EndsStr('.' + fixtureResult.Fixture.Name, sName) then
      SetLength(sName, Length(sName) - Length(fixtureResult.Fixture.Name) - 1);
    sTime := Format('%.3f', [fixtureResult.Duration.TotalSeconds]);

    sName := EscapeForXML(sName);
    sTime := EscapeForXML(sTime);
    sDate := FormatDateTime('yyyy-MM-dd"T"hh":"nn":"ss', fixtureResult.StartTime);

    //Count this suite's own test cases, as tests="" does; child fixtures are written as suites of their own.
    //A leaked test is written as <error>, so it is counted as one.
    ignoredCount := 0;
    errorCount := 0;
    failureCount := 0;
    for testResult in fixtureResult.TestResults do
      case testResult.ResultType of
        TTestResultType.Ignored : Inc(ignoredCount);
        TTestResultType.Error, TTestResultType.MemoryLeak : Inc(errorCount);
        TTestResultType.Failure : Inc(failureCount);
      end;

    // There is only a "disabled" attribute on the top level, but "disabled" and "skipped" on fixture level. Return ignored tests as "disabled"
    WriteXMLLine(Format('<testsuite name="%s" tests="%d" disabled="%d" errors="%d" failures="%d" time="%s" timestamp="%s">',
        [sName, fixtureResult.TestResults.Count, ignoredCount, errorCount, failureCount, sTime, sDate]));
    //WriteCategoryNodes(fixtureResult.Fixture.Categories);

    for testResult in fixtureResult.TestResults do
    begin
      WriteTestResult(testResult);
    end;
    WriteXMLLine('</testsuite>');

    for child in fixtureResult.Children do
    begin
      WriteFixtureResult(child);
    end;
  end
  else
  begin
    //It's a Namespace.

    if fixtureResult.ChildCount > 0 then
    begin
      for child in fixtureResult.Children do
      begin
        WriteFixtureResult(child);
      end;
    end;
  end;
end;

procedure TDUnitXXMLJUnitLogger.WriteTestResult(const testResult : ITestResult);
var
  sTime : string;
  sName : string;
  sClassName : string;
begin
  Indent;
  try
    sTime := Format('%.3f', [testResult.Duration.TotalSeconds]);

    sName := EscapeForXML(testResult.Test.Name);
    sClassName := EscapeForXML(testResult.Test.Fixture.Name);
    sTime := EscapeForXML(sTime);

    WriteXMLLine(Format('<testcase classname="%s" name="%s" time="%s">', [sClassName, sName, sTime]));
    //WriteCategoryNodes(testResult.Test.Categories);
    case testResult.ResultType of
      TTestResultType.Failure :
        begin
          Indent;
          WriteXMLLine(Format('<failure message="%s">', [EscapeForXML(testResult.Message)]));
          Indent;
          WriteXMLLine(Format('<![CDATA[ %s ]]>', [EscapeForXML(testResult.StackTrace, False, True)]));
          Outdent;
          WriteXMLLine('</failure>');
          Outdent;
        end;
      TTestResultType.MemoryLeak,
        TTestResultType.Error :
        begin
          Indent;
          WriteXMLLine(Format('<error message="%s">', [EscapeForXML(testResult.Message)]));
          Indent;
          WriteXMLLine(Format('<![CDATA[ %s ]]>', [EscapeForXML(testResult.StackTrace, False, True)]));
          Outdent;
          WriteXMLLine('</error>');
          Outdent;
        end;
      TTestResultType.Ignored :
        begin
          Indent;
          WriteXMLLine('<skipped/>');
          Outdent;
        end;
      TTestResultType.Pass :
        begin
          //        if testResult.Test.Categories.Count = 0 then
          //        begin
          //          exit;
          //        end;
          //        Indent;
        end;
    end;
    WriteXMLLine('</testcase>');

  finally
    Outdent;
  end;
end;

procedure TDUnitXXMLJUnitLogger.WriteCategoryNodes(const ACategoryList : TList<string>);
var
  sCategory : string;
begin
  if ACategoryList.Count > 0 then
  begin
    Indent;
    WriteXMLLine('<categories>');
    Indent;
    for sCategory in ACategoryList do
      WriteXMLLine(Format('<category name="%s" />', [sCategory]));
    Outdent;
    WriteXMLLine('</categories>');
    Outdent;
  end;
end;

procedure TDUnitXXMLJUnitLogger.WriteXMLLine(const value : string);
var
  bytes : TBytes;
  s : string;
begin
  s := StringOfChar(' ', FIndent) + value + #13#10;
  bytes := TEncoding.UTF8.GetBytes(s);
  FOutputStream.Write(bytes[0], Length(bytes));
end;

{ TDUnitXXMLJUnitFileLogger }

constructor TDUnitXXMLJUnitFileLogger.Create(const AFilename : string);
var
  sXmlFilename : string;
  fileStream : TFileStream;
  lXmlDirectory : string;
const
  DEFAULT_JUNIT_FILE_NAME = 'dunitx-results.xml';
begin
  sXmlFilename := AFilename;

  if sXmlFilename = '' then
    sXmlFilename := ExtractFilePath(ParamStr(0)) + DEFAULT_JUNIT_FILE_NAME;

  lXmlDirectory := ExtractFilePath(sXmlFilename);
  //A bare file name has no directory part, and ForceDirectories('') raises
  if lXmlDirectory <> '' then
    ForceDirectories(lXmlDirectory);

  fileStream := TFileStream.Create(sXmlFilename, fmCreate);

  //base class will destroy the stream;
  inherited Create(fileStream, true);
end;

end.

