program DUnitXTest;

{$APPTYPE CONSOLE}
{\\$STRONGLINKTYPES ON}
uses
  SysUtils,
  DUnitX.Tests.IoC in 'Tests\DUnitX.Tests.IoC.pas',
  DUnitX.Loggers.Console in 'DUnitX.Loggers.Console.pas',
  DUnitX.Loggers.Text in 'DUnitX.Loggers.Text.pas',
  DUnitX.MacOS.Console in 'DUnitX.MacOS.Console.pas',
  DUnitX.Windows.Console in 'DUnitX.Windows.Console.pas',
  DUnitX.ConsoleWriter.Base in 'DUnitX.ConsoleWriter.Base.pas',
  DUnitX.Loggers.XML.NUnit in 'DUnitX.Loggers.XML.NUnit.pas',
  DUnitX.Loggers.XML.xUnit in 'DUnitX.Loggers.XML.xUnit.pas',
  DUnitX.Detour in 'DUnitX.Detour.pas',
  DUnitX.Generics in 'DUnitX.Generics.pas',
  DUnitX.Utils in 'DUnitX.Utils.pas',
  DUnitX.WeakReference in 'DUnitX.WeakReference.pas',
  DUnitX.CommandLine in 'DUnitX.CommandLine.pas',
  DUnitX.Test in 'DUnitX.Test.pas',
  DUnitX.TestFixture in 'DUnitX.TestFixture.pas',
  DUnitX.TestResult in 'DUnitX.TestResult.pas',
  DUnitX.TestResults in 'DUnitX.TestResults.pas',
  DUnitX.TestRunner in 'DUnitX.TestRunner.pas',
  DUnitX.InternalInterfaces in 'DUnitX.InternalInterfaces.pas',
  DUnitX.TestFramework in 'DUnitX.TestFramework.pas',
  DUnitX.DUnitCompatibility in 'DUnitX.DUnitCompatibility.pas',
  DUnitX.IoC.Internal in 'DUnitX.IoC.Internal.pas',
  DUnitX.IoC in 'DUnitX.IoC.pas',
  DUnitX.Tests.Assert in 'Tests\DUnitX.Tests.Assert.pas',
  DUnitX.Tests.DUnitCompatibility in 'Tests\DUnitX.Tests.DUnitCompatibility.pas',
  DUnitX.Tests.Example in 'Tests\DUnitX.Tests.Example.pas';

var
  runner : ITestRunner;
  results : ITestResults;
  logger : ITestLogger;
begin
  try
    //Create the runner
    runner := TDUnitX.CreateRunner;
    runner.UseRTTI := True;
    //tell the runner how we will log things
    logger := TDUnitXConsoleLogger.Create;
    runner.AddLogger(logger);
    //note - text logger not implemented yet
    //runner.AddLogger(TDUnitXTextFileLogger.Create('c:\temp\log.txt'));
    //run the tests
//    TDUnitX.CommandLine.HideBanner := true;
  //  results := runner.Execute;
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
