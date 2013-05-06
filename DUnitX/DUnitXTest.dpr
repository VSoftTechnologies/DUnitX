program DUnitXTest;

{$APPTYPE CONSOLE}
{/$STRONGLINKTYPES ON}
uses
  SysUtils,
  DUnitX.TestFramework in 'DUnitX.TestFramework.pas',
  DUnitX.Tests.Example in 'Tests\DUnitX.Tests.Example.pas',
  DUnitX.TestRunner in 'DUnitX.TestRunner.pas',
  DUnitX.Loggers.Text in 'DUnitX.Loggers.Text.pas',
  DUnitX.Loggers.XML in 'DUnitX.Loggers.XML.pas',
  DUnitX.TestResults in 'DUnitX.TestResults.pas',
  DUnitX.TestResult in 'DUnitX.TestResult.pas',
  DUnitX.WeakReference in 'DUnitX.WeakReference.pas',
  DUnitX.TestFixture in 'DUnitX.TestFixture.pas',
  DUnitX.Test in 'DUnitX.Test.pas',
  DUnitX.Generics in 'DUnitX.Generics.pas',
  DUnitX.Utils in 'DUnitX.Utils.pas',
  DUnitX.InternalInterfaces in 'DUnitX.InternalInterfaces.pas',
  DUnitX.Tests.Assert in 'Tests\DUnitX.Tests.Assert.pas',
  DUnitX.DUnitCompatibility in 'DUnitX.DUnitCompatibility.pas',
  DUnitX.Loggers.Console in 'DUnitX.Loggers.Console.pas',
  DUnitX.Windows.Console in 'DUnitX.Windows.Console.pas',
  DUnitX.ConsoleWriter.Base in 'DUnitX.ConsoleWriter.Base.pas',
  DUnitX.IoC in 'DUnitX.IoC.pas',
  DUnitX.IoC.Internal in 'DUnitX.IoC.Internal.pas',
  DUnitX.Tests.IoC in 'Tests\DUnitX.Tests.IoC.pas',
  DUnitX.MacOS.Console in 'DUnitX.MacOS.Console.pas',
  DUnitX.CommandLine in 'DUnitX.CommandLine.pas',
  DUnitX.Tests.DUnitCompatibility in 'Tests\DUnitX.Tests.DUnitCompatibility.pas';

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
