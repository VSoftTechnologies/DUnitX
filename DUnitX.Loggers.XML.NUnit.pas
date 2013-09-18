unit DUnitX.Loggers.XML.NUnit;

interface

uses
  classes,
  SysUtils,
  DUnitX.TestFramework,
  DUnitX.Loggers.Null;

//TODO : Rework https://github.com/VSoftTechnologies/Delphi-Fluent-XML so it doesn't use msxml and use it here?

type
  TDUnitXXMLNUnitLogger = class(TDUnitXNullLogger)
  private
    FOutputStream : TStream;
    FOwnsStream   : boolean;
    FIndent       : integer;
  protected
    procedure Indent;
    procedure Outdent;
    procedure WriteXMLLine(const value : string);


    procedure OnTestingEnds(const RunResults: IRunResults); override;

    procedure WriteFixtureResult(const fixtureResult : IFixtureResult);
    procedure WriteTestResult(const testResult : ITestResult);


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
  TypInfo;


{ TDUnitXXMLNUnitLogger }

constructor TDUnitXXMLNUnitLogger.Create(const AOutputStream: TStream; const AOwnsStream : boolean = false);
var
  preamble: TBytes;
begin
  FOutputStream := AOutputStream;
  FOwnsStream   := AOwnsStream;

  Preamble := TEncoding.UTF8.GetPreamble;
  FOutputStream.WriteBuffer(preamble[0], Length(preamble));

end;

destructor TDUnitXXMLNUnitLogger.Destroy;
begin
  if FOwnsStream then
    FOutputStream.Free;
  inherited;
end;

procedure TDUnitXXMLNUnitLogger.Indent;
begin
  Inc(FIndent,1);
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
  fixtureRes : IFixtureResult;
  sExeName : string;
  sResult : string;
  sTime   : string;
  sDate   : string;
begin

{ first things first, rollup the namespaces.
  So, where parent fixtures have no tests, or only one child fixture, combine into a single fixture.
  }
  for fixtureRes in RunResults.FixtureResults do
  begin
    fixtureRes.Reduce;
    LogFixture(fixtureRes,0);
  end;


  sExeName := ParamStr(0);
  FIndent := 0;
  sTime := Format('%.3f',[RunResults.Duration.TotalSeconds]);
  sDate := FormatDateTime('yyyy-MM-dd',RunResults.StartTime);

  WriteXMLLine(Format('<test-results name="%s" total="%d" errors="%d" failures="%d" ignored="%d" inconclusive="0" not-run="%d" skipped="0" invalid="0" date="%s" time="%s">',
                      [sExeName,RunResults.TestCount,RunResults.ErrorCount,RunResults.FailureCount,RunResults.IgnoredCount,RunResults.IgnoredCount,sDate,sTime]));
  sExeName := ChangeFileExt(ExtractFileName(sExeName),'');

  if RunResults.AllPassed then
    sResult := 'Success'
  else
    sResult := 'Failure';

  Indent;
  //TODO: Populate these properly.
  WriteXMLLine('<environment nunit-version="DUnitX" clr-version="2.0.0.0" os-version="6.1.0.0" platform="Windows" cwd="c:\test" machine-name="mymachine" user="vincent" user-domain="finalbuilder.com"  />');
  WriteXMLLine('<culture-info current-culture="en" current-uiculture="en" />');
  Outdent;

  Indent;
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
  Dec(FIndent,1);
end;

procedure TDUnitXXMLNUnitLogger.WriteFixtureResult(const fixtureResult: IFixtureResult);
var
  sResult : string;
  sTime   : string;
  sLineEnd : string;
  child : IFixtureResult;
  testResult : ITestResult;
  sExecuted : string;
begin
  Indent;
  try
    if not fixtureResult.HasFailures then
      sResult := 'Success'
    else
      sResult := 'Failure';
    sTime := Format('%.3f',[fixtureResult.Duration.TotalSeconds]);


    //its a real fixture if the class is not TObject.
    if (not fixtureResult.Fixture.TestClass.ClassNameIs('TObject'))  then
    begin
      //if there were no tests then just ignore this fixture.
      if fixtureResult.ResultCount = 0 then
        exit;
      sExecuted := BoolToStr(fixtureResult.ResultCount > 0,true);

      WriteXMLLine(Format('<test-suite type="Fixture" name="%s" executed="%s" result="%s" success="%s" time="%s" >',[fixtureResult.Fixture.Name, sResult,sExecuted,BoolToStr(not fixtureResult.HasFailures,true),sTime]));
      Indent;
      WriteXMLLine('<results>');
      for testResult in fixtureResult.TestResults do
      begin
        WriteTestResult(testResult);
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
      WriteXMLLine(Format('<test-suite type="Namespace" name="%s" executed="true" result="%s" success="%s" time="%s" asserts="0" %s>',[fixtureResult.Fixture.Name, sResult,BoolToStr(not fixtureResult.HasFailures,true),sTime,sLineEnd]));
      if fixtureResult.ChildCount > 0 then
      begin
        WriteXMLLine('<results>');
        for child in fixtureResult.Children do
        begin
            WriteFixtureResult(child);
        end;
        WriteXMLLine('</results>');
        WriteXMLLine('</test-suite>');
      end;
    end;
  finally
    Outdent;
  end;
end;

function ResultTypeToString(const value : TTestResultType) : string;
begin
  result := GetEnumName(TypeInfo(TTestResultType),Ord(value));
end;

procedure TDUnitXXMLNUnitLogger.WriteTestResult(const testResult: ITestResult);
var
  sLineEnd : string;
  sResult  : string;
  sTime : string;
  sExecuted : string;
begin
  Indent;
  try
    sTime := Format('%.3f',[testResult.Duration.TotalSeconds]);
    sResult := ResultTypeToString(testResult.ResultType);
    if testResult.ResultType = TTestResultType.Pass then
      sLineEnd := '/';
    sExecuted := BoolToStr(testResult.ResultType <> Ignored,true);

    WriteXMLLine(Format('<test-case name="%s" executed="%s" result="%s" time="%s" asserts="0" %s>',[testResult.Test.FullName, sExecuted, sResult,sTime,sLineEnd]));
    if testResult.ResultType <> TTestResultType.Pass then
    begin
      Indent;
      case testResult.ResultType of
        Failure, Error:
        begin
          Indent;
          WriteXMLLine('<failure>');
          Indent;
            WriteXMLLine('<message>');
            Indent;
              WriteXMLLine(Format('<![CDATA[ %s ]]>',[testResult.Message]));
            Outdent;
            WriteXMLLine('</message>');
          Outdent;
          Indent;
            WriteXMLLine('<stack-trace>');
            Indent;
              WriteXMLLine(Format('<![CDATA[ %s ]]>',[testResult.StackTrace]));
            Outdent;
            WriteXMLLine('</stack-trace>');
          Outdent;
          WriteXMLLine('</failure>');
          Outdent;
        end;
        Ignored:
        begin
          Indent;
          WriteXMLLine('<reason>');
          Indent;
            WriteXMLLine('<message>');
            Indent;
              WriteXMLLine(Format('<![CDATA[ %s ]]>',[testResult.Message]));
            Outdent;
            WriteXMLLine('</message>');
          Outdent;
          WriteXMLLine('</reason>');
          Outdent;
        end;
      end;



      Outdent;
      WriteXMLLine('</test-case>');
    end;

  finally
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
const
  DEFAULT_NUNIT_FILE_NAME = 'dunitx-results.xml';
begin
  sXmlFilename := AFilename;

  if sXmlFilename = '' then
    sXmlFilename := ExtractFilePath(ParamStr(0)) + DEFAULT_NUNIT_FILE_NAME;

  fileStream := TFileStream.Create(sXmlFilename, fmCreate);

  //base class will destroy the stream;
  inherited Create(fileStream,true);
end;

end.
