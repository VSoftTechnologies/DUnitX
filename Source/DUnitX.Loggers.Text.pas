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

unit DUnitX.Loggers.Text;

interface

{$I DUnitX.inc}

uses
{$IFDEF USE_NS}   // since Delphi XE2
  WinAPI.Windows,
  System.SysUtils,
  System.Classes,
  System.TimeSpan,
  System.Generics.Collections,
{$ELSE}
  Windows,
  SysUtils,
  Classes,
  TimeSpan,
  Generics.Collections,
{$ENDIF}
  DUnitX.TestFramework,
  DUnitX.Loggers.Null;

type
  // Simple text file logger.
  TDunitXTextLogger = class(TDUnitXNullLogger)
  strict private
    const
      fIndentStep = 2;
      fStartSuffix = ' - START';
      fStopSuffix  = ' - STOP';
    var
      fOutput:   TStream;
      fOwnOut:   Boolean;
      fIndent:   Integer;
      fIssues:   TStringList;
      fEncoding: TEncoding;

    function GetFixtureName(const Fixture : ITestFixtureInfo) : string;
    function GetTestName(const Test : ITestInfo) : string;
    function GetTimeSuffix(const Caption : string) : string;

    procedure IncIndent(const Steps : Integer = 1);
    procedure DecIndent(const Steps : Integer = 1);

    procedure WriteLine(const text : string = ''); overload;
    procedure WriteLine(const textFmt : string; textArgs: array of const); overload;
    procedure WriteLine(const text : string; const duration : TTimeSpan); overload;

  protected
    procedure OnTestingStarts(const threadId : TThreadID; testCount, testActiveCount : Cardinal); override;

    procedure OnStartTestFixture(const threadId : TThreadID; const fixture : ITestFixtureInfo); override;

    procedure OnBeginTest(const threadId : TThreadID; const Test : ITestInfo); override;

    procedure OnTestSuccess(const threadId : TThreadID; const TestResult : ITestResult); override;
    procedure OnTestError(const threadId : TThreadID; const Error : ITestError); override;
    procedure OnTestFailure(const threadId : TThreadID; const Failure : ITestError); override;
    procedure OnTestIgnored(const threadId : TThreadID; const Ignored : ITestResult); override;
    procedure OnTestMemoryLeak(const threadId : TThreadID; const TestResult : ITestResult); override;

    procedure OnEndTest(const threadId : TThreadID; const TestResult : ITestResult); override;

    procedure OnEndTestFixture(const threadId : TThreadID; const results : IFixtureResult); override;

    procedure OnTestingEnds(const RunResults : IRunResults); override;

  public
    constructor Create(const outputStream : TStream; const ownsStream : Boolean = False; encoding : TEncoding = nil);
    destructor Destroy(); override;
  end;

  TDunitXTextFileLogger = class(TDunitXTextLogger)
  public
    constructor Create(const fileName : string = ''; const overwrite : Boolean = True; const encoding : TEncoding = nil);
  end;

implementation

{$REGION 'TDunitXTextLogger'}

constructor TDunitXTextLogger.Create(const outputStream : TStream; const ownsStream : Boolean; encoding : TEncoding);
begin
  inherited Create();
  fIssues := TStringList.Create();

  fOutput := outputStream;
  fOwnOut := ownsStream;

  if encoding <> nil then
    fEncoding := encoding
  else
    fEncoding := TEncoding.UTF8;
end;

procedure TDunitXTextLogger.DecIndent(const Steps : Integer);
begin
  Dec(fIndent, Steps * fIndentStep)
end;

destructor TDunitXTextLogger.Destroy;
begin
  fIssues.Free();
  if fOwnOut then
    fOutput.Free();

  inherited;
end;

function TDunitXTextLogger.GetFixtureName(const Fixture : ITestFixtureInfo) : string;
begin
  if  SameText(Fixture.UnitName, 'System')  then
    Result := Fixture.FullName
  else
    Result := Format('%s (%s)', [Fixture.FullName, Fixture.UnitName])
end;

function TDunitXTextLogger.GetTestName(const Test: ITestInfo): string;
begin
  if Test.Name <> Test.MethodName then
    Result := Format('%s (%s)', [Test.MethodName, Test.Name])
  else
    Result := Test.MethodName
end;

function TDunitXTextLogger.GetTimeSuffix(const Caption : string) : string;
begin
  Result := Caption + ' ' + FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now())
end;

procedure TDunitXTextLogger.IncIndent(const Steps : Integer);
begin
  Inc(fIndent, Steps * fIndentStep)
end;

procedure  TDunitXTextLogger.OnBeginTest(const threadId : TThreadID; const Test : ITestInfo);
begin
  inherited;

  WriteLine(GetTestName(Test) + GetTimeSuffix(fStartSuffix));
  IncIndent()
end;

procedure TDunitXTextLogger.OnEndTest(const threadId : TThreadID; const TestResult : ITestResult);
begin
  inherited;
  WriteLine(GetTestName(TestResult.Test) + GetTimeSuffix(fStopSuffix), TestResult.Duration);
end;

procedure TDunitXTextLogger.OnEndTestFixture(const threadId : TThreadID; const results : IFixtureResult);
begin
  inherited;
  WriteLine(GetFixtureName(results.Fixture) + GetTimeSuffix(fStopSuffix), results.Duration);
end;

procedure TDunitXTextLogger.OnStartTestFixture(const threadId : TThreadID; const fixture : ITestFixtureInfo);
begin
  inherited;
  WriteLine(GetFixtureName(fixture) + GetTimeSuffix(fStartSuffix));
  WriteLine();
  IncIndent()
end;

procedure TDunitXTextLogger.OnTestError(const threadId : TThreadID; const Error : ITestError);
begin
  inherited;
  WriteLine('- Status: ERROR %s', [Error.Message]);
  fIssues.Values[Error.Test.FullName] := Error.Message
end;

procedure TDunitXTextLogger.OnTestFailure(const threadId : TThreadID; const Failure : ITestError);
begin
  inherited;
  WriteLine('- Status: FAILURE %s', [Failure.Message]);
  fIssues.Values[Failure.Test.FullName] := Failure.Message
end;

procedure TDunitXTextLogger.OnTestIgnored(const threadId : TThreadID; const Ignored : ITestResult);
begin
  inherited;
  WriteLine('- Status: IGNORED %s', [Ignored.Message]);
  fIssues.Values[Ignored.Test.FullName] := Ignored.Message
end;

procedure TDunitXTextLogger.OnTestingEnds(const RunResults : IRunResults);
var
  i: Integer;
begin
  inherited;

  DecIndent();

  WriteLine('Testing finished');

  if fIssues.Count <> 0 then
  begin
    WriteLine('- Issues:');
    IncIndent();
    for i := 0 to fIssues.Count - 1  do
      begin
        WriteLine('%d %s', [i + 1, fIssues.Names[i]]);
        IncIndent(2);
        WriteLine(fIssues.ValueFromIndex[i]);
        DecIndent(2)
      end;
    DecIndent();
    WriteLine()
  end;

  WriteLine('- Fixture Count: %d', [RunResults.FixtureCount]);
  WriteLine('- Test Count:    %d', [RunResults.TestCount]);
  WriteLine('- Pass Count:    %d', [RunResults.PassCount]);
  if RunResults.FailureCount <> 0 then
    WriteLine('- Failure Count: %d', [RunResults.FailureCount]);
  if RunResults.ErrorCount <> 0 then
    WriteLine('- Error Count:   %d', [RunResults.ErrorCount]);
  if RunResults.IgnoredCount <> 0 then
    WriteLine('- Ignored Count: %d', [RunResults.IgnoredCount]);
  if RunResults.MemoryLeakCount <> 0 then
    WriteLine('- Memory Leaks:  %d', [RunResults.MemoryLeakCount]);
  if RunResults.AllPassed then
    WriteLine('- All PASSED');
  WriteLine()
end;

procedure TDunitXTextLogger.OnTestingStarts(const threadId : TThreadID; testCount, testActiveCount : Cardinal);
var
  buf : string;
begin
  inherited;

  buf := 'Starting to perform %u test';
  if testCount <> 1 then
    buf := buf + 's';
  if testCount <> testActiveCount then
    buf := buf + ', %u active';

  buf := Format(buf, [testCount, testActiveCount]);
  WriteLine(buf);
  WriteLine(StringOfChar('~', Length(buf)));
  WriteLine();

  IncIndent()
end;

procedure TDunitXTextLogger.OnTestMemoryLeak(const threadId : TThreadID; const TestResult : ITestResult);
begin
  inherited;
  WriteLine('- Status: MEMORY LEAK %s', [TestResult.Message]);
  fIssues.Values[TestResult.Test.FullName] := TestResult.Message
end;

procedure TDunitXTextLogger.OnTestSuccess(const threadId : TThreadID; const TestResult : ITestResult);
begin
  inherited;
  WriteLine('- Status: SUCCESS')
end;

function TTimeSpanToString( value : TTimeSpan): string;
var
  Fmt: string;
  Days, SubSecondTicks: Integer;
  LTicks: Int64;
begin
  Fmt := '%1:.2d:%2:.2d:%3:.2d'; // do not localize
  Days :=  value.Ticks div TTimeSpan.TicksPerDay;
  LTicks := value.Ticks mod TTimeSpan.TicksPerDay;
  if value.Ticks < 0 then
    LTicks := -LTicks;
  if value.Days <> 0 then
    Fmt := '%0:d.' + Fmt; // do not localize
  SubSecondTicks := LTicks mod TTimeSpan.TicksPerSecond;
  if SubSecondTicks <> 0 then
    Fmt := Fmt + '.%4:.7d'; // do not localize
  Result := Format(Fmt,
     [Days,
    (LTicks div TTimeSpan.TicksPerHour) mod 24,
    (LTicks div TTimeSpan.TicksPerMinute) mod 60,
    (LTicks div TTimeSpan.TicksPerSecond) mod 60,
    SubSecondTicks]);
end;

procedure TDunitXTextLogger.WriteLine(const text : string; const duration : TTimeSpan);
var
  buf : string;
begin
  DecIndent();

  WriteLine(text);

  if duration.Ticks <> 0 then
    begin
      buf := TTimeSpanToString(duration);

      while Copy(buf, 1, 3) = '00:' do
        Delete(buf, 1, 3);
      if (Length(buf) >= 2) and (buf[1] = '0') and CharInSet(buf[2], ['0'..'9']) then
        Delete(buf, 1, 1);

      while (buf <> '') and (buf[Length(buf)] = '0') do
        SetLength(buf, Length(buf) - 1);
      if (buf <> '') and (buf[Length(buf)] = '.') then
        SetLength(buf, Length(buf) - 1);

      if buf <> '' then
        WriteLine('- elapsed time: ' + buf);
    end;

  WriteLine()
end;

procedure TDunitXTextLogger.WriteLine(const textFmt : string; textArgs : array of const);
begin
  WriteLine(Format(textFmt, textArgs))
end;

procedure TDunitXTextLogger.WriteLine(const text: string);

var
  bufS : string;
  bufB : TBytes;

begin
  if Trim(text) <> '' then
    bufS := StringOfChar(' ', fIndent) + Trim(text) + sLineBreak
  else
    bufS := sLineBreak;
  {-}
  bufB := fEncoding.GetBytes(bufS);
  fOutput.Write(bufB, Length(bufB))
end;

{$ENDREGION 'TDunitXTextLogger'}


{$REGION 'TDunitXTextFileLogger'}

constructor TDunitXTextFileLogger.Create(const fileName : string; const overwrite : Boolean; const encoding : TEncoding);

type
  {$IF Declared(TBufferedFileStream)}
    TOutputStream = TBufferedFileStream;
  {$ELSE}
    TOutputStream = TFileStream;
  {$IFEND}

var
  outName : string;
  outStream : TOutputStream;
  outEncoding : TEncoding;
  bufBOM : TBytes;
  len : Integer;

begin
  outName := FileName;
  if outName = '' then
    outName := ChangeFileExt(ParamStr(0), '.log');

  if overwrite or not FileExists(outName) then
  begin
    outStream := TOutputStream.Create(outName, fmCreate);

    if encoding <> nil then
      outEncoding := encoding
    else
      outEncoding := TEncoding.UTF8;

    bufBOM := outEncoding.GetPreamble();
    if bufBOM <> nil then
      outStream.Write(bufBOM, Length(bufBOM))
  end
  else
  begin
    outStream := TOutputStream.Create(outName, fmOpenReadWrite);

    outEncoding := encoding;
    if outEncoding = nil then
    begin
      // A standard BOM consists of 2 to 4 bytes.
      // In the case of exotic encodings, such as UTF-7, the BOM may consist of 5 bytes.
      SetLength(bufBOM, 5);
      len := outStream.Read(bufBOM, 5);
      SetLength(bufBOM, len);
      TEncoding.GetBufferEncoding(bufBOM, outEncoding, TEncoding.UTF8)
    end;

    outStream.Seek(0, soFromEnd)
  end;

  inherited Create(outStream, True, outEncoding)
end;

{$ENDREGION 'TDunitXTextFileLogger'}


end.

