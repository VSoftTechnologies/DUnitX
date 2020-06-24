program DUnitXTestProject;

{$IFNDEF GUI}
{$APPTYPE CONSOLE}
{$ENDIF}

{$STRONGLINKTYPES ON}

uses
  SysUtils,
  DUnitX.TestFramework,
  DUnitX.Loggers.XML.NUnit,
  {$IFDEF TESTINSIGHT}
    TestInsight.DUnitX,
  {$ELSE}
    {$IFDEF GUI}
      {$IFDEF MSWINDOWS}
        DUnitX.Loggers.GUI.VCL,
      {$ENDIF }
    {$ELSE}
      DUnitX.AutoDetect.Console,
      DUnitX.Loggers.Console,
    {$ENDIF }
  {$ENDIF }
  DUnitX.Tests.Assert in 'DUnitX.Tests.Assert.pas',
  DUnitX.Tests.DUnitCompatibility in 'DUnitX.Tests.DUnitCompatibility.pas',
  DUnitX.Tests.Example in 'DUnitX.Tests.Example.pas',
  DUnitX.Tests.Framework in 'DUnitX.Tests.Framework.pas',
  DUnitX.Tests.IoC in 'DUnitX.Tests.IoC.pas',
  DUnitX.Tests.TestFixture in 'DUnitX.Tests.TestFixture.pas',
  DUnitX.Tests.Utils.XML in 'DUnitX.Tests.Utils.XML.pas',
  DUnitX.Tests.WeakReference in 'DUnitX.Tests.WeakReference.pas',
  DUnitX.Tests.Loggers.XML.NUnit in 'DUnitX.Tests.Loggers.XML.NUnit.pas',
  DUnitX.SingleNameSpace in 'DUnitX.SingleNameSpace.pas',
  DUnitX.Tests.MemoryLeaks in 'DUnitX.Tests.MemoryLeaks.pas',
  DUnitX.Tests.CommandLineParser in 'DUnitX.Tests.CommandLineParser.pas',
  DUnitX.Tests.CategoryParser in 'DUnitX.Tests.CategoryParser.pas',
  DUnitX.Tests.TestNameParser in 'DUnitX.Tests.TestNameParser.pas',
  DUnitX.Tests.Inheritance in 'DUnitX.Tests.Inheritance.pas',
  DUnitX.Tests.ConsoleWriter.Base in 'DUnitX.Tests.ConsoleWriter.Base.pas',
  DUnitX.Tests.TestDataProvider in 'DUnitX.Tests.TestDataProvider.pas',
  DUnitX.Tests.IgnoreFixture in 'DUnitX.Tests.IgnoreFixture.pas',
  DUnitX.Tests.Utils in 'DUnitX.Tests.Utils.pas';

var
  runner : ITestRunner;
  results : IRunResults;
  logger : ITestLogger;
  nunitLogger : ITestLogger;
begin
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnitX.RunRegisteredTests;
  Exit;
  {$ENDIF}

  {$IFDEF GUI}
    DUnitX.Loggers.GUI.VCL.Run;
    exit;
  {$ENDIF}

  try
    TDUnitX.CheckCommandLine;
    //Create the runner
    runner := TDUnitX.CreateRunner;
    runner.UseRTTI := True;
    runner.FailsOnNoAsserts := True; //Assertions must be made during tests;

    //tell the runner how we will log things

    if TDUnitX.Options.ConsoleMode <> TDunitXConsoleMode.Off then
    begin
      logger := TDUnitXConsoleLogger.Create(TDUnitX.Options.ConsoleMode = TDunitXConsoleMode.Quiet);
      runner.AddLogger(logger);
    end;

    nunitLogger := TDUnitXXMLNUnitFileLogger.Create(TDUnitX.Options.XMLOutputFile);
    runner.AddLogger(nunitLogger);

    logger := nil;
    nunitLogger := nil;
    //Run tests
    results := runner.Execute;
    runner := nil;
    //Let the CI Server know that something failed.

    {$IFDEF CI}
    if not results.AllPassed then
      System.ExitCode := EXIT_ERRORS;
    {$ELSE}
    //We don;t want this happening when running under CI.
    if TDUnitX.Options.ExitBehavior = TDUnitXExitBehavior.Pause then
    begin
      System.Write('Done...  Press <Enter> key to quit.');
      System.Readln;
    end;
    {$ENDIF}
    results := nil;

  except
    on E: Exception do
    begin
      System.Writeln(E.ClassName, ': ', E.Message);
      {$IFNDEF CI}
      System.Readln;
      {$ENDIF}
    end;
  end;
end.
