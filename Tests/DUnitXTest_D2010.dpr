program DUnitXTest_D2010;

{$IFNDEF GUI}
{$APPTYPE CONSOLE}
{$ENDIF}

{$STRONGLINKTYPES OFF}

uses
  SysUtils,
  DUnitX.Loggers.GUI.VCL in '..\Source\DUnitX.Loggers.GUI.VCL.pas' {GUIVCLTestRunner},
  DUnitX.Loggers.Console in '..\Source\DUnitX.Loggers.Console.pas',
  DUnitX.Loggers.Text in '..\Source\DUnitX.Loggers.Text.pas',
  DUnitX.MacOS.Console in '..\Source\DUnitX.MacOS.Console.pas',
  DUnitX.Windows.Console in '..\Source\DUnitX.Windows.Console.pas',
  DUnitX.ConsoleWriter.Base in '..\Source\DUnitX.ConsoleWriter.Base.pas',
  DUnitX.Loggers.XML.xUnit in '..\Source\DUnitX.Loggers.XML.xUnit.pas',
  DUnitX.Generics in '..\Source\DUnitX.Generics.pas',
  DUnitX.Utils in '..\Source\DUnitX.Utils.pas',
  DUnitX.WeakReference in '..\Source\DUnitX.WeakReference.pas',
  DUnitX.Test in '..\Source\DUnitX.Test.pas',
  DUnitX.TestFixture in '..\Source\DUnitX.TestFixture.pas',
  DUnitX.TestResult in '..\Source\DUnitX.TestResult.pas',
  DUnitX.RunResults in '..\Source\DUnitX.RunResults.pas',
  DUnitX.TestRunner in '..\Source\DUnitX.TestRunner.pas',
  DUnitX.InternalInterfaces in '..\Source\DUnitX.InternalInterfaces.pas',
  DUnitX.TestFramework in '..\Source\DUnitX.TestFramework.pas',
  DUnitX.DUnitCompatibility in '..\Source\DUnitX.DUnitCompatibility.pas',
  DUnitX.IoC in '..\Source\DUnitX.IoC.pas',
  DUnitX.Utils.XML in '..\Source\DUnitX.Utils.XML.pas',
  DUnitX.StackTrace.JCL in '..\Source\DUnitX.StackTrace.JCL.pas',
  DUnitX.StackTrace.MadExcept3 in '..\Source\DUnitX.StackTrace.MadExcept3.pas',
  DUnitX.StackTrace.MadExcept4 in '..\Source\DUnitX.StackTrace.MadExcept4.pas',
  DUnitX.StackTrace.EurekaLog7 in '..\Source\DUnitX.StackTrace.EurekaLog7.pas',
  DUnitX.Loggers.Null in '..\Source\DUnitX.Loggers.Null.pas',
  DUnitX.FixtureResult in '..\Source\DUnitX.FixtureResult.pas',
  DUnitX.Tests.Assert in 'DUnitX.Tests.Assert.pas',
  DUnitX.Tests.DUnitCompatibility in 'DUnitX.Tests.DUnitCompatibility.pas',
  DUnitX.Tests.Example in 'DUnitX.Tests.Example.pas',
  DUnitX.Tests.Framework in 'DUnitX.Tests.Framework.pas',
  DUnitX.Tests.IoC in 'DUnitX.Tests.IoC.pas',
  DUnitX.Tests.TestFixture in 'DUnitX.Tests.TestFixture.pas',
  DUnitX.Tests.Utils.XML in 'DUnitX.Tests.Utils.XML.pas',
  DUnitX.Tests.WeakReference in 'DUnitX.Tests.WeakReference.pas',
  DUnitX.Tests.Loggers.XML.NUnit in 'DUnitX.Tests.Loggers.XML.NUnit.pas',
  DUnitX.Loggers.XML.NUnit in '..\Source\DUnitX.Loggers.XML.NUnit.pas',
  DUnitX.SingleNameSpace in 'DUnitX.SingleNameSpace.pas',
  DUnitX.MemoryLeakMonitor.Default in '..\Source\DUnitX.MemoryLeakMonitor.Default.pas',
  DUnitX.MemoryLeakMonitor.FastMM4 in '..\Source\DUnitX.MemoryLeakMonitor.FastMM4.pas',
  DUnitX.Tests.MemoryLeaks in 'DUnitX.Tests.MemoryLeaks.pas',
  DUnitX.Extensibility in '..\Source\DUnitX.Extensibility.pas',
  DUnitX.Extensibility.PluginManager in '..\Source\DUnitX.Extensibility.PluginManager.pas',
  DUnitX.FixtureProviderPlugin in '..\Source\DUnitX.FixtureProviderPlugin.pas',
  DUnitX.Tests.CommandLineParser in 'DUnitX.Tests.CommandLineParser.pas',
  DUnitX.Filters in '..\Source\DUnitX.Filters.pas',
  DUnitX.CategoryExpression in '..\Source\DUnitX.CategoryExpression.pas',
  DUnitX.Tests.CategoryParser in 'DUnitX.Tests.CategoryParser.pas',
  DUnitX.TestNameParser in '..\Source\DUnitX.TestNameParser.pas',
  DUnitX.Tests.TestNameParser in 'DUnitX.Tests.TestNameParser.pas',
  DUnitX.AutoDetect.Console in '..\Source\DUnitX.AutoDetect.Console.pas',
  DUnitX.CommandLine.OptionDef in '..\Source\DUnitX.CommandLine.OptionDef.pas',
  DUnitX.CommandLine.Options in '..\Source\DUnitX.CommandLine.Options.pas',
  DUnitX.CommandLine.Parser in '..\Source\DUnitX.CommandLine.Parser.pas',
  DUnitX.OptionsDefinition in '..\Source\DUnitX.OptionsDefinition.pas',
  DUnitX.FilterBuilder in '..\Source\DUnitX.FilterBuilder.pas',
  DUnitX.Tests.Inheritance in 'DUnitX.Tests.Inheritance.pas',
  DUnitX.Tests.ConsoleWriter.Base in 'DUnitX.Tests.ConsoleWriter.Base.pas',
  DUnitX.Assert in '..\Source\DUnitX.Assert.pas',
  DUnitX.Types in '..\Source\DUnitX.Types.pas',
  DUnitX.Tests.Utils in 'DUnitX.Tests.Utils.pas';

var
  runner : ITestRunner;
  results : IRunResults;
  logger : ITestLogger;
  nunitLogger : ITestLogger;
begin
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
    TDUnitX.Options.ExitBehavior := TDUnitXExitBehavior.Pause;

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
