program DUnitXExamples_XE2;

{$APPTYPE CONSOLE}

uses
  SysUtils,
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
  DUnitX.CommandLine in '..\DUnitX.CommandLine.pas',
  DUnitX.ConsoleWriter.Base in '..\DUnitX.ConsoleWriter.Base.pas',
  DUnitX.DUnitCompatibility in '..\DUnitX.DUnitCompatibility.pas',
  DUnitX.Extensibility in '..\DUnitX.Extensibility.pas',
  DUnitX.Extensibility.PluginManager in '..\DUnitX.Extensibility.PluginManager.pas',
  DUnitX.FixtureProviderPlugin in '..\DUnitX.FixtureProviderPlugin.pas',
  DUnitX.FixtureResult in '..\DUnitX.FixtureResult.pas',
  DUnitX.Generics in '..\DUnitX.Generics.pas',
  DUnitX.InternalInterfaces in '..\DUnitX.InternalInterfaces.pas',
  DUnitX.IoC in '..\DUnitX.IoC.pas',
  DUnitX.Loggers.Console in '..\DUnitX.Loggers.Console.pas';

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
    logger := TDUnitXConsoleLogger.Create(true);
    nunitLogger := TDUnitXXMLNUnitFileLogger.Create;
    runner.AddLogger(logger);
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
