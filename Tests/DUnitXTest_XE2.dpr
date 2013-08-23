program DUnitXTest_XE2;

{$APPTYPE CONSOLE}
{\\$STRONGLINKTYPES ON}
uses
  SysUtils,
  DUnitX.Tests.IoC in 'DUnitX.Tests.IoC.pas',
  DUnitX.Loggers.Console in '..\DUnitX.Loggers.Console.pas',
  DUnitX.Loggers.Text in '..\DUnitX.Loggers.Text.pas',
  DUnitX.MacOS.Console in '..\DUnitX.MacOS.Console.pas',
  DUnitX.Windows.Console in '..\DUnitX.Windows.Console.pas',
  DUnitX.ConsoleWriter.Base in '..\DUnitX.ConsoleWriter.Base.pas',
  DUnitX.Loggers.XML.NUnit in '..\DUnitX.Loggers.XML.NUnit.pas',
  DUnitX.Loggers.XML.xUnit in '..\DUnitX.Loggers.XML.xUnit.pas',
  DUnitX.Detour in '..\DUnitX.Detour.pas',
  DUnitX.Generics in '..\DUnitX.Generics.pas',
  DUnitX.Utils in '..\DUnitX.Utils.pas',
  DUnitX.WeakReference in '..\DUnitX.WeakReference.pas',
  DUnitX.CommandLine in '..\DUnitX.CommandLine.pas',
  DUnitX.Test in '..\DUnitX.Test.pas',
  DUnitX.TestFixture in '..\DUnitX.TestFixture.pas',
  DUnitX.TestResult in '..\DUnitX.TestResult.pas',
  DUnitX.TestResults in '..\DUnitX.TestResults.pas',
  DUnitX.TestRunner in '..\DUnitX.TestRunner.pas',
  DUnitX.InternalInterfaces in '..\DUnitX.InternalInterfaces.pas',
  DUnitX.TestFramework in '..\DUnitX.TestFramework.pas',
  DUnitX.DUnitCompatibility in '..\DUnitX.DUnitCompatibility.pas',
  DUnitX.IoC.Internal in '..\DUnitX.IoC.Internal.pas',
  DUnitX.IoC in '..\DUnitX.IoC.pas',
  DUnitX.Tests.Assert in 'DUnitX.Tests.Assert.pas',
  DUnitX.Tests.DUnitCompatibility in 'DUnitX.Tests.DUnitCompatibility.pas',
  DUnitX.Tests.Example in 'DUnitX.Tests.Example.pas',
  DUnitX.Tests.Loggers.XML.NUnit in 'DUnitX.Tests.Loggers.XML.NUnit.pas',
  DUnitX.Utils.XML in '..\DUnitX.Utils.XML.pas',
  DUnitX.Tests.Utils.XML in 'DUnitX.Tests.Utils.XML.pas',
  DUnitX.Tests.WeakReference in 'DUnitX.Tests.WeakReference.pas',
  DUnitX.StackTrace.JCL in '..\DUnitX.StackTrace.JCL.pas',
  DUnitX.StackTrace.MadExcept3 in '..\DUnitX.StackTrace.MadExcept3.pas',
  DUnitX.StackTrace.MadExcept4 in '..\DUnitX.StackTrace.MadExcept4.pas',
  DUnitX.Loggers.GUI in '..\DUnitX.Loggers.GUI.pas' {Form1},
  DUnitX.StackTrace.EurekaLog7 in '..\DUnitX.StackTrace.EurekaLog7.pas',
  DUnitX.Tests.TestResultCollector in 'DUnitX.Tests.TestResultCollector.pas';

var
  runner : ITestRunner;
  results : ITestResults;
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
