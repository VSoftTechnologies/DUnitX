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

unit DUnitX.Loggers.XML.NUnit;

interface

{$I DUnitX.inc}

uses
  {$IFDEF USE_NS}
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
  {$ELSE}
  Classes,
  SysUtils,
  Generics.Collections,
  {$ENDIF}
  DUnitX.TestFramework,
  DUnitX.Loggers.Null;

type
  TDUnitXXMLNUnitLogger = class(TDUnitXNullLogger)
  private
    FOutputStream : TStream;
    FOwnsStream   : boolean;
    FIndent       : integer;
    FFormatSettings : TFormatSettings;
  protected
    procedure Indent;
    procedure Outdent;
    procedure WriteXMLLine(const value : string);


    procedure OnTestingEnds(const RunResults: IRunResults); override;

    procedure WriteCategoryNodes(const ACategoryList: TList<string>);
    procedure WriteFixtureResult(const fixtureResult : IFixtureResult);
    procedure WriteTestResult(const testResult : ITestResult);

    function Format(const Format: string; const Args: array of const): String;
  public
    constructor Create(const AOutputStream : TStream; const AOwnsStream : boolean = false);
    destructor Destroy;override;
  end;

  TDUnitXXMLNUnitFileLogger = class(TDUnitXXMLNUnitLogger)
  public
    constructor Create(const AFilename: string = '');
  end;


implementation

uses
  {$IFDEF USE_NS}
  System.TypInfo;
  {$ELSE}
  TypInfo;
  {$ENDIF}

function IsValidXMLChar(wc: WideChar): Boolean;
begin
  case Word(wc) of
    $0009, $000A, $000C, $000D,
      $0020..$D7FF,
      $E000..$FFFD, // Standard Unicode chars below $FFFF
      $D800..$DBFF, // High surrogate of Unicode character  = $10000 - $10FFFF
      $DC00..$DFFF: // Low surrogate of Unicode character  = $10000 - $10FFFF
      result := True;
  else
    result := False;
  end;
end;

function StripInvalidXML(const s: string): string;
var
  i, count: Integer;
begin
  {$IFNDEF NEXTGEN}
  count := Length(s);
  setLength(result, count);
  for i := 1 to Count do // Iterate
  begin
    if IsValidXMLChar(WideChar(s[i])) then
      result[i] := s[i]
    else
      result[i] := ' ';
  end; // for}
  {$ELSE}
  count := s.Length;
  SetLength(result, count);
  for i := 0 to count - 1 do // Iterate
  begin
    if IsValidXMLChar(s.Chars[i]) then
    begin
      result := result.Remove(i, 1);
      result := result.Insert(i, s.Chars[i]);
    end
    else
    begin
      result := result.Remove(i, 1);
      result := result.Insert(i, s.Chars[i]);
    end;
  end; // for}
  {$ENDIF}
end;
function EscapeForXML(const value: string; const isAttribute: boolean = True; const isCDATASection : Boolean = False): string;
begin
  result := StripInvalidXML(value);
  {$IFNDEF NEXTGEN}
  if isCDATASection  then
  begin
    Result := StringReplace(Result, ']]>', ']>',[rfReplaceAll]);
    exit;
  end;

  //note we are avoiding replacing &amp; with &amp;amp; !!
  Result := StringReplace(result, '&amp;', '[[-xy-amp--]]',[rfReplaceAll]);
  Result := StringReplace(result, '&', '&amp;',[rfReplaceAll]);
  Result := StringReplace(result, '[[-xy-amp--]]', '&amp;amp;',[rfReplaceAll]);
  Result := StringReplace(result, '<', '&lt;',[rfReplaceAll]);
  Result := StringReplace(result, '>', '&gt;',[rfReplaceAll]);

  if isAttribute then
  begin
    Result := StringReplace(result, '''', '&#39;',[rfReplaceAll]);
    Result := StringReplace(result, '"', '&quot;',[rfReplaceAll]);
  end;
  {$ELSE}
  if isCDATASection  then
  begin
    Result := Result.Replace(']]>', ']>', [rfReplaceAll]);
    exit;
  end;

  //note we are avoiding replacing &amp; with &amp;amp; !!
  Result := Result.Replace('&amp;', '[[-xy-amp--]]',[rfReplaceAll]);
  Result := Result.Replace('&', '&amp;',[rfReplaceAll]);
  Result := Result.Replace('[[-xy-amp--]]', '&amp;amp;',[rfReplaceAll]);
  Result := Result.Replace('<', '&lt;',[rfReplaceAll]);
  Result := Result.Replace('>', '&gt;',[rfReplaceAll]);

  if isAttribute then
  begin
    Result := Result.Replace('''', '&#39;',[rfReplaceAll]);
    Result := Result.Replace('"', '&quot;',[rfReplaceAll]);
  end;
  {$ENDIF}
end;

{ TDUnitXXMLNUnitLogger }

constructor TDUnitXXMLNUnitLogger.Create(const AOutputStream: TStream; const AOwnsStream : boolean = false);
var
  preamble: TBytes;
  {$IFNDEF DELPHI_XE_UP}
  oldThousandSeparator: Char;
  oldDecimalSeparator: Char;
  {$ENDIF}
begin
  inherited Create;
  {$IFDEF DELPHI_XE_UP }
  FFormatSettings := TFormatSettings.Create;
  FFormatSettings.ThousandSeparator := ',';
  FFormatSettings.DecimalSeparator := '.';
  {$ELSE}
  oldThousandSeparator        := {$IFDEF USE_NS}System.{$ENDIF}SysUtils.ThousandSeparator;
  oldDecimalSeparator         := {$IFDEF USE_NS}System.{$ENDIF}DecimalSeparator;
  try
    SysUtils.ThousandSeparator := ',';
    SysUtils.DecimalSeparator := '.';
  {$ENDIF}
  FOutputStream := AOutputStream;
  FOwnsStream   := AOwnsStream;

  Preamble := TEncoding.UTF8.GetPreamble;
  FOutputStream.WriteBuffer(preamble[0], Length(preamble));
  {$IFNDEF DELPHI_XE_UP}
  finally
    {$IFDEF USE_NS}System.{$ENDIF}SysUtils.ThousandSeparator := oldThousandSeparator;
    {$IFDEF USE_NS}System.{$ENDIF}SysUtils.DecimalSeparator  := oldDecimalSeparator;
  end;
  {$ENDIF}

end;

destructor TDUnitXXMLNUnitLogger.Destroy;
begin
  if FOwnsStream then
    FOutputStream.Free;
  inherited;
end;

function TDUnitXXMLNUnitLogger.Format(const Format: string; const Args: array of const): String;
begin
  Result := {$IFDEF USE_NS}System.{$ENDIF}SysUtils.Format(Format, Args, FFormatSettings);
end;

procedure TDUnitXXMLNUnitLogger.Indent;
begin
  Inc(FIndent,2);
end;

procedure TDUnitXXMLNUnitLogger.OnTestingEnds(const RunResults: IRunResults);


procedure LogFixture(const fixture : IFixtureResult; level : integer);
var
  child : IFixtureResult;
  sLevel : string;
begin
  sLevel := StringOfChar(' ', level * 2 );
  System.WriteLn(sLevel + fixture.Fixture.NameSpace + ':' + fixture.Fixture.Name + Format(' [Tests: %d] [Children: %d] [Passed : %d]',[fixture.ResultCount,fixture.ChildCount,fixture.PassCount]));

  Inc(level);
  for child in fixture.Children do
  begin
    LogFixture(child,level);
  end;

end;


var
  fixtureRes  : IFixtureResult;
  sExeName    : string;
  sResult     : string;
  sTime       : string;
  sDate       : string;
  totalTests  : integer;
begin

{ first things first, rollup the namespaces.
  So, where parent fixtures have no tests, or only one child fixture, combine into a single fixture.
  }
  for fixtureRes in RunResults.FixtureResults do
  begin
    fixtureRes.Reduce;
//    LogFixture(fixtureRes,0);
  end;

  //NUnit reports the total without the Ignored.
  totalTests := RunResults.TestCount - RunResults.IgnoredCount;

  sExeName := ParamStr(0);
  FIndent := 0;
  sTime := Format('%.3f',[RunResults.Duration.TotalSeconds]);
  sDate := FormatDateTime('yyyy-MM-dd',RunResults.StartTime);

  WriteXMLLine('<?xml version="1.0" encoding="UTF-8" standalone="yes" ?>');
  WriteXMLLine(Format('<test-results name="%s" total="%d" errors="%d" failures="%d" ignored="%d" inconclusive="0" not-run="%d" skipped="0" invalid="0" date="%s" time="%s">',
                      [sExeName,totalTests,RunResults.ErrorCount,RunResults.FailureCount,RunResults.IgnoredCount,RunResults.IgnoredCount,sDate,sTime]));
  sExeName := ExtractFileName(sExeName);

  if RunResults.AllPassed then
    sResult := 'Success'
  else
    sResult := 'Failure';

  Indent;
  //TODO: Populate these properly.
//  WriteXMLLine('<environment nunit-version="DUnitX" clr-version="2.0.0.0" os-version="6.1.0.0" platform="Windows" cwd="" machine-name="" user="" user-domain=""  />');
  WriteXMLLine('<culture-info current-culture="en" current-uiculture="en" />');
  WriteXMLLine(Format('<test-suite type="Assembly" name="%s" executed="true" result="%s" success="%s" time="%s" asserts="0">',[sExeName,sResult,BoolToStr(RunResults.AllPassed,true),sTime]));
  Indent;
  WriteXMLLine('<results>');

  Indent;
  for fixtureRes in RunResults.FixtureResults do
    WriteFixtureResult(fixtureRes);
  Outdent;
  WriteXMLLine('</results>');
  Outdent;
  WriteXMLLine('</test-suite>');
  Outdent;
  WriteXMLLine('</test-results>');

end;

procedure TDUnitXXMLNUnitLogger.Outdent;
begin
  Dec(FIndent,2);
end;

procedure TDUnitXXMLNUnitLogger.WriteFixtureResult(const fixtureResult: IFixtureResult);
var
  sResult : string;
  sTime   : string;
  sLineEnd : string;
  child : IFixtureResult;
  testResult : ITestResult;
  sExecuted : string;
  sName: string;
  sSuccess: string;
begin
  Indent;
  try
    if not fixtureResult.HasFailures then
      sResult := 'Success'
    else
      sResult := 'Failure';
    sTime := Format('%.3f',[fixtureResult.Duration.TotalSeconds]);

    sName := EscapeForXML(fixtureResult.Fixture.Name);
    sResult := EscapeForXML(sResult);
    sSuccess := EscapeForXML(BoolToStr(not fixtureResult.HasFailures,true));
    sTime := EscapeForXML(sTime);

    //its a real fixture if the class is not TObject.
    if (not fixtureResult.Fixture.TestClass.ClassNameIs('TObject'))  then
    begin
      //if there were no tests then just ignore this fixture.
      if fixtureResult.ResultCount = 0 then
        exit;
      sExecuted := BoolToStr(fixtureResult.ResultCount > 0,true);
      sExecuted := EscapeForXML(sExecuted);

      WriteXMLLine(Format('<test-suite type="Fixture" name="%s" executed="%s" result="%s" success="%s" time="%s" >',[sName, sExecuted, sResult, sSuccess, sTime]));
        WriteCategoryNodes(fixtureResult.Fixture.Categories);
          Indent;
          WriteXMLLine('<results>');
          for testResult in fixtureResult.TestResults do
          begin
            WriteTestResult(testResult);
          end;

          for child in fixtureResult.Children do
          begin
            WriteFixtureResult(child);
          end;
          WriteXMLLine('</results>');
        Outdent;
        WriteXMLLine('</test-suite>');
    end
    else
    begin
      if fixtureResult.ChildCount = 0 then
        sLineEnd := '/';
      //It's a Namespace.

      WriteXMLLine(Format('<test-suite type="Namespace" name="%s" executed="true" result="%s" success="%s" time="%s" asserts="0" %s>',[sName, sResult, sSuccess, sTime, sLineEnd]));

      if fixtureResult.ChildCount > 0 then
      begin
        Indent;
        WriteXMLLine('<results>');
        Indent;
        for child in fixtureResult.Children do
        begin
            WriteFixtureResult(child);
        end;
        Outdent;
        WriteXMLLine('</results>');
        Outdent;
        WriteXMLLine('</test-suite>');
      end;
    end;
  finally
    Outdent;
  end;
end;

function ResultTypeToString(const value : TTestResultType) : string;
begin
  case value of
    TTestResultType.Pass: result := 'Success';
    TTestResultType.Failure : result := 'Failure';
    TTestResultType.Error   : result := 'Error';
    TTestResultType.Ignored : result := 'Ignored';
    TTestResultType.MemoryLeak : result := 'Failure'; //NUnit xml doesn't understand memory leak
  else
    result := GetEnumName(TypeInfo(TTestResultType),Ord(value));
  end;
end;

procedure TDUnitXXMLNUnitLogger.WriteTestResult(const testResult: ITestResult);
var
  sLineEnd : string;
  sResult  : string;
  sTime : string;
  sExecuted : string;
  sSuccess : string;
  sName : string;
begin
  Indent;
  try
    sTime := Format('%.3f',[testResult.Duration.TotalSeconds]);
    sResult := ResultTypeToString(testResult.ResultType);
    if (testResult.ResultType = TTestResultType.Pass) and (testResult.Test.Categories.Count = 0)  then
      sLineEnd := '/';
    sExecuted := BoolToStr(testResult.ResultType <> TTestResultType.Ignored,true);

    if testResult.ResultType <> TTestResultType.Ignored then
      sSuccess := Format('success="%s"',[EscapeForXML(BoolToStr(testResult.ResultType = TTestResultType.Pass, true))])
    else
      sSuccess := '';

    sName := EscapeForXML(testResult.Test.Name);
    sExecuted := EscapeForXML(sExecuted);
    sResult := EscapeForXML(sResult);
    sTime := EscapeForXML(sTime);

    WriteXMLLine(Format('<test-case name="%s" executed="%s" result="%s" %s time="%s" asserts="0" %s>', [sName, sExecuted, sResult, sSuccess, sTime, sLineEnd]));
    WriteCategoryNodes(testResult.Test.Categories);
    case testResult.ResultType of
      TTestResultType.MemoryLeak,
      TTestResultType.Failure,
      TTestResultType.Error:
      begin
        Indent;
        WriteXMLLine('<failure>');
        Indent;
          WriteXMLLine('<message>');
          Indent;
            WriteXMLLine(Format('<![CDATA[ %s ]]>',[EscapeForXML(testResult.Message, False, True)]));
          Outdent;
          WriteXMLLine('</message>');
        Outdent;
        Indent;
          WriteXMLLine('<stack-trace>');
          Indent;
            WriteXMLLine(Format('<![CDATA[ %s ]]>',[EscapeForXML(testResult.StackTrace, False, True)]));
          Outdent;
          WriteXMLLine('</stack-trace>');
        Outdent;
        WriteXMLLine('</failure>');
      end;
      TTestResultType.Ignored:
      begin
        Indent;
        WriteXMLLine('<reason>');
        Indent;
          WriteXMLLine('<message>');
          Indent;
            WriteXMLLine(Format('<![CDATA[ %s ]]>',[EscapeForXML(testResult.Message, False, True)]));
          Outdent;
          WriteXMLLine('</message>');
        Outdent;
        WriteXMLLine('</reason>');
      end;
      TTestResultType.Pass :
      begin
        if testResult.Test.Categories.Count = 0 then
        begin
          exit;
        end;
        Indent;
      end;
    end;
    Outdent;
    WriteXMLLine('</test-case>');

  finally
    Outdent;
  end;
end;

procedure TDUnitXXMLNUnitLogger.WriteCategoryNodes(const ACategoryList: TList<string>);
var
  sCategory: string;
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


procedure TDUnitXXMLNUnitLogger.WriteXMLLine(const value: string);
var
  bytes : TBytes;
  s : string;
begin
  s := StringOfChar(' ',FIndent) + value + #13#10;
  bytes := TEncoding.UTF8.GetBytes(s);
  FOutputStream.Write(bytes[0],Length(bytes));
end;

{ TDUnitXXMLNUnitFileLogger }

constructor TDUnitXXMLNUnitFileLogger.Create(const AFilename: string);
var
  sXmlFilename  : string;
  fileStream    : TFileStream;
  lXmlDirectory: string;
const
  DEFAULT_NUNIT_FILE_NAME = 'dunitx-results.xml';
begin
  sXmlFilename := AFilename;

  if sXmlFilename = '' then
    sXmlFilename := ExtractFilePath(ParamStr(0)) + DEFAULT_NUNIT_FILE_NAME;

  lXmlDirectory := ExtractFilePath(sXmlFilename);
  ForceDirectories(lXmlDirectory);

  fileStream := TFileStream.Create(sXmlFilename, fmCreate);

  //base class will destroy the stream;
  inherited Create(fileStream,true);
end;

end.
