program DUnitXTest_D10Berlin;

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
  System.SysUtils,
  DUnitX.Loggers.GUI.VCL in '..\DUnitX.Loggers.GUI.VCL.pas' {GUIVCLTestRunner},
  DUnitX.Loggers.Console in '..\DUnitX.Loggers.Console.pas',
  DUnitX.Loggers.Text in '..\DUnitX.Loggers.Text.pas',
  DUnitX.MacOS.Console in '..\DUnitX.MacOS.Console.pas',
  DUnitX.Windows.Console in '..\DUnitX.Windows.Console.pas',
  DUnitX.ConsoleWriter.Base in '..\DUnitX.ConsoleWriter.Base.pas',
  DUnitX.Loggers.XML.xUnit in '..\DUnitX.Loggers.XML.xUnit.pas',
  DUnitX.Generics in '..\DUnitX.Generics.pas',
  DUnitX.Utils in '..\DUnitX.Utils.pas',
  DUnitX.WeakReference in '..\DUnitX.WeakReference.pas',
  DUnitX.Test in '..\DUnitX.Test.pas',
  DUnitX.TestFixture in '..\DUnitX.TestFixture.pas',
  DUnitX.TestResult in '..\DUnitX.TestResult.pas',
  DUnitX.RunResults in '..\DUnitX.RunResults.pas',
  DUnitX.TestRunner in '..\DUnitX.TestRunner.pas',
  DUnitX.InternalInterfaces in '..\DUnitX.InternalInterfaces.pas',
  DUnitX.TestFramework in '..\DUnitX.TestFramework.pas',
  DUnitX.DUnitCompatibility in '..\DUnitX.DUnitCompatibility.pas',
  DUnitX.IoC in '..\DUnitX.IoC.pas',
  DUnitX.Utils.XML in '..\DUnitX.Utils.XML.pas',
  DUnitX.StackTrace.JCL in '..\DUnitX.StackTrace.JCL.pas',
  DUnitX.StackTrace.MadExcept3 in '..\DUnitX.StackTrace.MadExcept3.pas',
  DUnitX.StackTrace.MadExcept4 in '..\DUnitX.StackTrace.MadExcept4.pas',
  DUnitX.StackTrace.EurekaLog7 in '..\DUnitX.StackTrace.EurekaLog7.pas',
  DUnitX.Loggers.Null in '..\DUnitX.Loggers.Null.pas',
  DUnitX.FixtureResult in '..\DUnitX.FixtureResult.pas',
  DUnitX.Tests.Assert in 'DUnitX.Tests.Assert.pas',
  DUnitX.Tests.DUnitCompatibility in 'DUnitX.Tests.DUnitCompatibility.pas',
  DUnitX.Tests.Example in 'DUnitX.Tests.Example.pas',
  DUnitX.Tests.Framework in 'DUnitX.Tests.Framework.pas',
  DUnitX.Tests.IoC in 'DUnitX.Tests.IoC.pas',
  DUnitX.Tests.TestFixture in 'DUnitX.Tests.TestFixture.pas',
  DUnitX.Tests.Utils.XML in 'DUnitX.Tests.Utils.XML.pas',
  DUnitX.Tests.WeakReference in 'DUnitX.Tests.WeakReference.pas',
  DUnitX.Tests.Loggers.XML.NUnit in 'DUnitX.Tests.Loggers.XML.NUnit.pas',
  DUnitX.Loggers.XML.NUnit in '..\DUnitX.Loggers.XML.NUnit.pas',
  DUnitX.SingleNameSpace in 'DUnitX.SingleNameSpace.pas',
  DUnitX.MemoryLeakMonitor.Default in '..\DUnitX.MemoryLeakMonitor.Default.pas',
  DUnitX.MemoryLeakMonitor.FastMM4 in '..\DUnitX.MemoryLeakMonitor.FastMM4.pas',
  DUnitX.Tests.MemoryLeaks in 'DUnitX.Tests.MemoryLeaks.pas',
  DUnitX.Extensibility in '..\DUnitX.Extensibility.pas',
  DUnitX.Extensibility.PluginManager in '..\DUnitX.Extensibility.PluginManager.pas',
  DUnitX.FixtureProviderPlugin in '..\DUnitX.FixtureProviderPlugin.pas',
  DUnitX.Tests.CommandLineParser in 'DUnitX.Tests.CommandLineParser.pas',
  DUnitX.Filters in '..\DUnitX.Filters.pas',
  DUnitX.CategoryExpression in '..\DUnitX.CategoryExpression.pas',
  DUnitX.Tests.CategoryParser in 'DUnitX.Tests.CategoryParser.pas',
  DUnitX.TestNameParser in '..\DUnitX.TestNameParser.pas',
  DUnitX.Tests.TestNameParser in 'DUnitX.Tests.TestNameParser.pas',
  DUnitX.AutoDetect.Console in '..\DUnitX.AutoDetect.Console.pas',
  DUnitX.CommandLine.OptionDef in '..\DUnitX.CommandLine.OptionDef.pas',
  DUnitX.CommandLine.Options in '..\DUnitX.CommandLine.Options.pas',
  DUnitX.CommandLine.Parser in '..\DUnitX.CommandLine.Parser.pas',
  DUnitX.OptionsDefinition in '..\DUnitX.OptionsDefinition.pas',
  DUnitX.FilterBuilder in '..\DUnitX.FilterBuilder.pas',
  DUnitX.Tests.Inheritance in 'DUnitX.Tests.Inheritance.pas',
  DUnitX.Tests.ConsoleWriter.Base in 'DUnitX.Tests.ConsoleWriter.Base.pas',
  DUnitX.Assert in '..\DUnitX.Assert.pas',
  DUnitX.Types in '..\DUnitX.Types.pas',
  DUnitX.Utils.Files in '..\DUnitX.Utils.Files.pas',
  DUnitX.Tests.Utils in 'DUnitX.Tests.Utils.pas',
  DUnitX.Tests.Utils.Files in 'DUnitX.Tests.Utils.Files.pas',
  DUnitX.Exceptions in '..\DUnitX.Exceptions.pas',
  DUnitX.Init in '..\DUnitX.Init.pas',
  DUnitX.Timeout in '..\DUnitX.Timeout.pas',
  DUnitX.Helpers in '..\DUnitX.Helpers.pas',
  DUnitX.ResStrs in '..\DUnitX.ResStrs.pas',
  DUnitX.Constants in '..\DUnitX.Constants.pas',
  DUnitX.Attributes in '..\DUnitX.Attributes.pas';

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
