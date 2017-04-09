program DUnitXExamples_XE7;

{$IFDEF CI}
  {$APPTYPE CONSOLE}
{$ELSE}
  {$IFNDEF GUI}
    {$IFNDEF TESTINSIGHT}
      {$APPTYPE CONSOLE}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

{$STRONGLINKTYPES ON}

uses
  SysUtils,
  DUnitX.Loggers.GUI.VCL in '..\DUnitX.Loggers.GUI.VCL.pas' {GUIVCLTestRunner},
  DUnitX.Examples.General in 'DUnitX.Examples.General.pas',
  DUnitX.Loggers.Text in '..\DUnitX.Loggers.Text.pas',
  DUnitX.Loggers.XML.NUnit in '..\DUnitX.Loggers.XML.NUnit.pas',
  DUnitX.Loggers.XML.xUnit in '..\DUnitX.Loggers.XML.xUnit.pas',
  DUnitX.MacOS.Console in '..\DUnitX.MacOS.Console.pas',
  DUnitX.Test in '..\DUnitX.Test.pas',
  DUnitX.TestFixture in '..\DUnitX.TestFixture.pas',
  DUnitX.TestFramework in '..\DUnitX.TestFramework.pas',
  DUnitX.TestResult in '..\DUnitX.TestResult.pas',
  DUnitX.RunResults in '..\DUnitX.RunResults.pas',
  DUnitX.TestRunner in '..\DUnitX.TestRunner.pas',
  DUnitX.Utils in '..\DUnitX.Utils.pas',
  DUnitX.Utils.XML in '..\DUnitX.Utils.XML.pas',
  DUnitX.WeakReference in '..\DUnitX.WeakReference.pas',
  DUnitX.Windows.Console in '..\DUnitX.Windows.Console.pas',
  DUnitX.StackTrace.EurekaLog7 in '..\DUnitX.StackTrace.EurekaLog7.pas',
  NonNamespacedExample in 'NonNamespacedExample.pas',
  DUnitX.Examples.EqualityAsserts in 'DUnitX.Examples.EqualityAsserts.pas',
  DUnitX.Loggers.Null in '..\DUnitX.Loggers.Null.pas',
  DUnitX.MemoryLeakMonitor.Default in '..\DUnitX.MemoryLeakMonitor.Default.pas',
  DUnitX.AutoDetect.Console in '..\DUnitX.AutoDetect.Console.pas',
  DUnitX.ConsoleWriter.Base in '..\DUnitX.ConsoleWriter.Base.pas',
  DUnitX.DUnitCompatibility in '..\DUnitX.DUnitCompatibility.pas',
  DUnitX.Extensibility in '..\DUnitX.Extensibility.pas',
  DUnitX.Extensibility.PluginManager in '..\DUnitX.Extensibility.PluginManager.pas',
  DUnitX.FixtureProviderPlugin in '..\DUnitX.FixtureProviderPlugin.pas',
  DUnitX.FixtureResult in '..\DUnitX.FixtureResult.pas',
  DUnitX.Generics in '..\DUnitX.Generics.pas',
  DUnitX.InternalInterfaces in '..\DUnitX.InternalInterfaces.pas',
  DUnitX.IoC in '..\DUnitX.IoC.pas',
  DUnitX.Loggers.Console in '..\DUnitX.Loggers.Console.pas',
  DUnitX.CommandLine.OptionDef in '..\DUnitX.CommandLine.OptionDef.pas',
  DUnitX.CommandLine.Options in '..\DUnitX.CommandLine.Options.pas',
  DUnitX.CommandLine.Parser in '..\DUnitX.CommandLine.Parser.pas',
  DUnitX.OptionsDefinition in '..\DUnitX.OptionsDefinition.pas',
  DUnitX.Banner in '..\DUnitX.Banner.pas',
  DUnitX.FilterBuilder in '..\DUnitX.FilterBuilder.pas',
  DUnitX.Filters in '..\DUnitX.Filters.pas',
  DUnitX.CategoryExpression in '..\DUnitX.CategoryExpression.pas',
  DUnitX.TestNameParser in '..\DUnitX.TestNameParser.pas',
  DUnitX.Assert in '..\DUnitX.Assert.pas',
  DUnitX.Attributes in '..\DUnitX.Attributes.pas',
  DUnitX.Types in '..\DUnitX.Types.pas',
  DUnitX.Timeout in '..\DUnitX.Timeout.pas';

var
  runner : ITestRunner;
  results : IRunResults;
  logger : ITestLogger;
  nunitLogger : ITestLogger;

begin
  Assert.IgnoreCaseDefault := False;

{$IFNDEF CI}
  {$IFDEF GUI}
    DUnitX.Loggers.GUI.VCL.Run;
    exit;
  {$ENDIF}

  {$IFDEF TESTINSIGHT}
    TestInsight.DUnitX.RunRegisteredTests;
    exit;
  {$ENDIF}
{$ENDIF}

  try
    //Check command line options, will exit if invalid
    TDUnitX.CheckCommandLine;
    //Create the test runner
    runner := TDUnitX.CreateRunner;
    //Tell the runner to use RTTI to find Fixtures
    runner.UseRTTI := True;
    //tell the runner how we will log things
    //Log to the console window
    logger := TDUnitXConsoleLogger.Create(False);
    runner.AddLogger(logger);
    //Generate an NUnit compatible XML File
    nunitLogger := TDUnitXXMLNUnitFileLogger.Create(TDUnitX.Options.XMLOutputFile);
    runner.AddLogger(nunitLogger);
    runner.FailsOnNoAsserts := True; //When true, Assertions must be made during tests;

    //Run tests
    results := runner.Execute;
    if not results.AllPassed then
      System.ExitCode := EXIT_ERRORS;
  except
    on E: Exception do
      System.Writeln(E.ClassName, ': ', E.Message);
  end;

  {$IFNDEF CI}
  //We don't want this happening when running under CI.
  if TDUnitX.Options.ExitBehavior = TDUnitXExitBehavior.Pause then
  begin
    System.Write('Done...  Press <Enter> key to quit.');
    System.Readln;
  end;
  {$ENDIF}
end.
