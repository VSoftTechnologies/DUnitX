program DUnitXTest;

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}
uses
  SysUtils,
  DUnitX.TestFramework in 'DUnitX.TestFramework.pas',
  DUnitX.Tests.Example in 'Tests\DUnitX.Tests.Example.pas',
  DUnitX.Tests.Assert in 'Tests\DUnitX.Tests.Assert.pas',
  DUnitX.DUnitCompatibility in 'DUnitX.DUnitCompatibility.pas',
  DUnitX.Tests.IoC in 'Tests\DUnitX.Tests.IoC.pas',
  DUnitX.Tests.DUnitCompatibility in 'Tests\DUnitX.Tests.DUnitCompatibility.pas',
  DUnitX.Loggers.Console in 'Loggers\DUnitX.Loggers.Console.pas',
  DUnitX.Loggers.Text in 'Loggers\DUnitX.Loggers.Text.pas',
  DUnitX.MacOS.Console in 'Loggers\DUnitX.MacOS.Console.pas',
  DUnitX.Windows.Console in 'Loggers\DUnitX.Windows.Console.pas',
  DUnitX.IoC.Internal in 'IoC\DUnitX.IoC.Internal.pas',
  DUnitX.IoC in 'IoC\DUnitX.IoC.pas',
  DUnitX.ConsoleWriter.Base in 'Loggers\DUnitX.ConsoleWriter.Base.pas',
  DUnitX.Loggers.XML.NUnit in 'Loggers\DUnitX.Loggers.XML.NUnit.pas',
  DUnitX.Loggers.XML.xUnit in 'Loggers\DUnitX.Loggers.XML.xUnit.pas',
  DUnitX.Detour in 'Utils\DUnitX.Detour.pas',
  DUnitX.Generics in 'Utils\DUnitX.Generics.pas',
  DUnitX.Utils in 'Utils\DUnitX.Utils.pas',
  DUnitX.WeakReference in 'Utils\DUnitX.WeakReference.pas',
  DUnitX.CommandLine in 'CmdLine\DUnitX.CommandLine.pas',
  DUnitX.InternalInterfaces in 'Internals\DUnitX.InternalInterfaces.pas',
  DUnitX.Test in 'Runtime\DUnitX.Test.pas',
  DUnitX.TestFixture in 'Runtime\DUnitX.TestFixture.pas',
  DUnitX.TestResult in 'Runtime\DUnitX.TestResult.pas',
  DUnitX.TestResults in 'Runtime\DUnitX.TestResults.pas',
  DUnitX.TestRunner in 'Runtime\DUnitX.TestRunner.pas';

var
  runner : ITestRunner;
  results : ITestResults;
begin
  try
    //Create the runner
    runner := TDUnitX.CreateRunner;
    runner.UseRTTI := True;
    //tell the runner how we will log things
    runner.AddLogger(TDUnitXConsoleLogger.Create);
    //note - text logger not implemented yet
    //runner.AddLogger(TDUnitXTextFileLogger.Create('c:\temp\log.txt'));
    //run the tests
    TDUnitX.CommandLine.HideBanner := true;
    results := runner.Execute;
//    if not results.AllPassed then
//      System.ExitCode := 1;
    //todo : results really should have some stats already available.
    System.Writeln('Done.. press any key to quit.');
    ReadLn;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
