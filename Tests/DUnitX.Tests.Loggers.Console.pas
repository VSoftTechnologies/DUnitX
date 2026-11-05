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

unit DUnitX.Tests.Loggers.Console;

interface

{$I DUnitX.inc}

uses
  DUnitX.TestFramework,
  DUnitX.ConsoleWriter.Base;

type
  //Keeps everything the console logger writes as one string, each line end as #10.
  TCapturingConsoleWriter = class(TInterfacedObject, IDUnitXConsoleWriter)
  private
    FText : string;
    FIndent : Integer;
  public
    function GetIndent : Integer;
    procedure SetIndent(const count : Integer);
    procedure SetColour(const foreground : TConsoleColour; const background : TConsoleColour = ccDefault);
    procedure WriteLn(const s : string); overload;
    procedure WriteLn; overload;
    procedure Write(const s : string);
    procedure Indent(const value : integer = 1);
    procedure Outdent(const value : integer = 1);
    property Text : string read FText;
  end;

  TConsoleWriterClass = class of TDUnitXConsoleWriterBase;

{$M+}
  [TestFixture]
  TDUnitX_ConsoleLoggerTests = class
  private
    FWriter : TCapturingConsoleWriter;
    FWriterRef : IDUnitXConsoleWriter;
    FSavedWriterClass : TConsoleWriterClass;
    //Runs the script XPXFXEIXPM (X execute, P pass, F failure, E error, I ignored, M leak) through a quiet
    //logger, which writes the ten marks ...F.EI..M, and returns what it wrote.  A row width of MaxInt
    //uses the constructor's default.
    function RunQuietScript(const ARowWidth : Integer; const AEndRun : Boolean = False) : string;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure RowWidth_Ends_A_Row_After_Exactly_That_Many_Marks;
    [Test]
    procedure RowWidth_Of_One_Puts_Each_Mark_On_Its_Own_Row;
    [Test]
    [NamedTestCase('Default', '2147483647')]
    [NamedTestCase('Zero', '0')]
    [NamedTestCase('Negative', '-5')]
    procedure No_RowWidth_Keeps_Every_Mark_On_One_Row(const ARowWidth : Integer);
    //'|' in AExpectedMarks stands for a line end.
    [Test]
    [NamedTestCase('RowJustEnded', '5,...F.|EI..M|')]
    [NamedTestCase('RowPartlyFilled', '4,...F|.EI.|.M')]
    [NamedTestCase('NoWidth', '2147483647,...F.EI..M')]
    procedure Summary_Follows_The_Marks_After_One_Blank_Line(const ARowWidth : Integer; const AExpectedMarks : string);
  end;
{$M-}

implementation

uses
  System.SysUtils,
  DUnitX.Loggers.Console,
  DUnitX.ResStrs,
  DUnitX.RunResults,
  DUnitX.ServiceLocator;

{ TCapturingConsoleWriter }

function TCapturingConsoleWriter.GetIndent : Integer;
begin
  Result := FIndent;
end;

procedure TCapturingConsoleWriter.Indent(const value : integer);
begin
  SetIndent(FIndent + value);
end;

procedure TCapturingConsoleWriter.Outdent(const value : integer);
begin
  SetIndent(FIndent - value);
end;

procedure TCapturingConsoleWriter.SetColour(const foreground, background : TConsoleColour);
begin
  //Colour is not part of what these tests compare.
end;

procedure TCapturingConsoleWriter.SetIndent(const count : Integer);
begin
  if count < 0 then
    FIndent := 0
  else
    FIndent := count;
end;

procedure TCapturingConsoleWriter.Write(const s : string);
begin
  FText := FText + s;
end;

procedure TCapturingConsoleWriter.WriteLn(const s : string);
begin
  FText := FText + s + #10;
end;

procedure TCapturingConsoleWriter.WriteLn;
begin
  FText := FText + #10;
end;

{ TDUnitX_ConsoleLoggerTests }

procedure TDUnitX_ConsoleLoggerTests.Setup;
var
  current : IDUnitXConsoleWriter;
begin
  //Remember the platform's console writer so TearDown can put it back.
  current := TDUnitXServiceLocator.DefaultContainer.Resolve<IDUnitXConsoleWriter>();
  FSavedWriterClass := TConsoleWriterClass((current as TObject).ClassType);
  current := nil;

  FWriter := TCapturingConsoleWriter.Create;
  FWriterRef := FWriter;
  TDUnitXServiceLocator.DefaultContainer.RegisterType<IDUnitXConsoleWriter>(
    function : IDUnitXConsoleWriter
    begin
      Result := FWriterRef;
    end);
end;

procedure TDUnitX_ConsoleLoggerTests.TearDown;
var
  writerClass : TConsoleWriterClass;
begin
  writerClass := FSavedWriterClass;
  TDUnitXServiceLocator.DefaultContainer.RegisterType<IDUnitXConsoleWriter>(
    function : IDUnitXConsoleWriter
    begin
      Result := writerClass.Create;
    end);
  FWriter := nil;
  FWriterRef := nil;
end;

function TDUnitX_ConsoleLoggerTests.RunQuietScript(const ARowWidth : Integer; const AEndRun : Boolean) : string;
const
  SCRIPT = 'XPXFXEIXPM';
var
  logger : ITestLogger;
  results : IRunResults;
  i : Integer;
begin
  if ARowWidth = MaxInt then
    logger := TDUnitXConsoleLogger.Create(True)
  else
    logger := TDUnitXConsoleLogger.Create(True, ARowWidth);
  for i := 1 to Length(SCRIPT) do
    case SCRIPT[i] of
      'X' : logger.OnExecuteTest(MainThreadID, nil);
      'P' : logger.OnTestSuccess(MainThreadID, nil);
      'F' : logger.OnTestFailure(MainThreadID, nil);
      'E' : logger.OnTestError(MainThreadID, nil);
      'I' : logger.OnTestIgnored(MainThreadID, nil);
      'M' : logger.OnTestMemoryLeak(MainThreadID, nil);
    end;
  if AEndRun then
  begin
    results := TDUnitXRunResults.Create;
    logger.OnTestingEnds(results);
  end;
  Result := FWriter.Text;
end;

procedure TDUnitX_ConsoleLoggerTests.RowWidth_Ends_A_Row_After_Exactly_That_Many_Marks;
begin
  Assert.AreEqual('...F'#10'.EI.'#10'.M', RunQuietScript(4), False);
end;

procedure TDUnitX_ConsoleLoggerTests.RowWidth_Of_One_Puts_Each_Mark_On_Its_Own_Row;
begin
  Assert.AreEqual('.'#10'.'#10'.'#10'F'#10'.'#10'E'#10'I'#10'.'#10'.'#10'M'#10, RunQuietScript(1), False);
end;

procedure TDUnitX_ConsoleLoggerTests.No_RowWidth_Keeps_Every_Mark_On_One_Row(const ARowWidth : Integer);
begin
  Assert.AreEqual('...F.EI..M', RunQuietScript(ARowWidth), False);
end;

procedure TDUnitX_ConsoleLoggerTests.Summary_Follows_The_Marks_After_One_Blank_Line(const ARowWidth : Integer; const AExpectedMarks : string);
var
  expected : string;
begin
  //A row still open needs its line end, then one blank line; a row already ended needs only the blank line.
  expected := StringReplace(AExpectedMarks, '|', #10, [rfReplaceAll]);
  if Copy(expected, Length(expected), 1) <> #10 then
    expected := expected + #10;
  expected := expected + #10 + Format(STestsFound, [0]);
  Assert.AreEqual(expected, Copy(RunQuietScript(ARowWidth, True), 1, Length(expected)), False);
end;

end.
