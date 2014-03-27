program DUnitXTest_XE2;

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}
uses
  SysUtils,
  DUnitX.Loggers.Console in '..\DUnitX.Loggers.Console.pas',
  DUnitX.Loggers.Text in '..\DUnitX.Loggers.Text.pas',
  DUnitX.MacOS.Console in '..\DUnitX.MacOS.Console.pas',
  DUnitX.Windows.Console in '..\DUnitX.Windows.Console.pas',
  DUnitX.ConsoleWriter.Base in '..\DUnitX.ConsoleWriter.Base.pas',
  DUnitX.Loggers.XML.xUnit in '..\DUnitX.Loggers.XML.xUnit.pas',
  DUnitX.Generics in '..\DUnitX.Generics.pas',
  DUnitX.Utils in '..\DUnitX.Utils.pas',
  DUnitX.WeakReference in '..\DUnitX.WeakReference.pas',
  DUnitX.CommandLine in '..\DUnitX.CommandLine.pas',
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
  DUnitX.Loggers.GUI in '..\DUnitX.Loggers.GUI.pas' {Form1},
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
  DUnitX.FixtureProviderPlugin in '..\DUnitX.FixtureProviderPlugin.pas';

{$R *.res}

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
    logger := TDUnitXConsoleLogger.Create(false);
    nunitLogger := TDUnitXXMLNUnitFileLogger.Create;
    runner.AddLogger(logger);
    runner.AddLogger(nunitLogger);

    logger := nil;
    nunitLogger := nil;

    //Run tests
    results := runner.Execute;

    {$IFDEF CI}
    //Let the CI Server know that something failed.
    if not results.AllPassed then
      System.ExitCode := 1;
    {$ELSE}
    //We don;t want this happening when running under CI.
    System.Write('Done.. press <Enter> key to quit.');
    System.Readln;
    {$ENDIF}
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
