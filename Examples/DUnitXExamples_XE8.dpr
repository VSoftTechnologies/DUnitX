program DUnitXExamples_XE8;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  DUnitX.Examples.General in 'DUnitX.Examples.General.pas',
  DUnitX.ConsoleWriter.Base in '..\Source\DUnitX.ConsoleWriter.Base.pas',
  DUnitX.DUnitCompatibility in '..\Source\DUnitX.DUnitCompatibility.pas',
  DUnitX.Generics in '..\Source\DUnitX.Generics.pas',
  DUnitX.InternalInterfaces in '..\Source\DUnitX.InternalInterfaces.pas',
  DUnitX.IoC in '..\Source\DUnitX.IoC.pas',
  DUnitX.Loggers.Console in '..\Source\DUnitX.Loggers.Console.pas',
  DUnitX.Loggers.Text in '..\Source\DUnitX.Loggers.Text.pas',
  DUnitX.Loggers.XML.NUnit in '..\Source\DUnitX.Loggers.XML.NUnit.pas',
  DUnitX.Loggers.XML.xUnit in '..\Source\DUnitX.Loggers.XML.xUnit.pas',
  DUnitX.MacOS.Console in '..\Source\DUnitX.MacOS.Console.pas',
  DUnitX.Test in '..\Source\DUnitX.Test.pas',
  DUnitX.TestFixture in '..\Source\DUnitX.TestFixture.pas',
  DUnitX.TestFramework in '..\Source\DUnitX.TestFramework.pas',
  DUnitX.TestResult in '..\Source\DUnitX.TestResult.pas',
  DUnitX.RunResults in '..\Source\DUnitX.RunResults.pas',
  DUnitX.TestRunner in '..\Source\DUnitX.TestRunner.pas',
  DUnitX.Utils in '..\Source\DUnitX.Utils.pas',
  DUnitX.Utils.XML in '..\Source\DUnitX.Utils.XML.pas',
  DUnitX.WeakReference in '..\Source\DUnitX.WeakReference.pas',
  DUnitX.Windows.Console in '..\Source\DUnitX.Windows.Console.pas',
  DUnitX.StackTrace.EurekaLog7 in '..\Source\DUnitX.StackTrace.EurekaLog7.pas',
  NonNamespacedExample in 'NonNamespacedExample.pas',
  DUnitX.Examples.EqualityAsserts in 'DUnitX.Examples.EqualityAsserts.pas',
  DUnitX.FixtureResult in '..\Source\DUnitX.FixtureResult.pas',
  DUnitX.Loggers.Null in '..\Source\DUnitX.Loggers.Null.pas',
  DUnitX.MemoryLeakMonitor.Default in '..\Source\DUnitX.MemoryLeakMonitor.Default.pas',
  DUnitX.Extensibility in '..\Source\DUnitX.Extensibility.pas',
  DUnitX.CommandLine.OptionDef in '..\Source\DUnitX.CommandLine.OptionDef.pas',
  DUnitX.CommandLine.Options in '..\Source\DUnitX.CommandLine.Options.pas',
  DUnitX.CommandLine.Parser in '..\Source\DUnitX.CommandLine.Parser.pas',
  DUnitX.FixtureProviderPlugin in '..\Source\DUnitX.FixtureProviderPlugin.pas';

var
  runner : ITestRunner;
  results : IRunResults;
  logger : ITestLogger;
  nunitLogger : ITestLogger;
begin
  try
    //Create the runner
    runner := TDUnitX.CreateRunner;
    runner.UseRTTI := True;

    //tell the runner how we will log things

    if TDUnitX.Options.ConsoleMode <> TDunitXConsoleMode.Off then
    begin
      logger := TDUnitXConsoleLogger.Create(TDUnitX.Options.ConsoleMode = TDunitXConsoleMode.Quiet);
      runner.AddLogger(logger);
    end;

    nunitLogger := TDUnitXXMLNUnitFileLogger.Create(TDUnitX.Options.XMLOutputFile);
    runner.AddLogger(nunitLogger);

    //Run tests
    results := runner.Execute;

    System.Write('Done.. press <Enter> key to quit.');
    System.Readln;
  except
    on E: Exception do
      System.Writeln(E.ClassName, ': ', E.Message);
  end;
end.
