program DUnitXExamples;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  DUnitX.CommandLine in '..\DUnitX.CommandLine.pas',
  DUnitX.ConsoleWriter.Base in '..\DUnitX.ConsoleWriter.Base.pas',
  DUnitX.Detour in '..\DUnitX.Detour.pas',
  DUnitX.DUnitCompatibility in '..\DUnitX.DUnitCompatibility.pas',
  DUnitX.Generics in '..\DUnitX.Generics.pas',
  DUnitX.InternalInterfaces in '..\DUnitX.InternalInterfaces.pas',
  DUnitX.IoC.Internal in '..\DUnitX.IoC.Internal.pas',
  DUnitX.IoC in '..\DUnitX.IoC.pas',
  DUnitX.Loggers.Console in '..\DUnitX.Loggers.Console.pas',
  DUnitX.Loggers.Text in '..\DUnitX.Loggers.Text.pas',
  DUnitX.Loggers.XML.NUnit in '..\DUnitX.Loggers.XML.NUnit.pas',
  DUnitX.Loggers.XML.xUnit in '..\DUnitX.Loggers.XML.xUnit.pas',
  DUnitX.MacOS.Console in '..\DUnitX.MacOS.Console.pas',
  DUnitX.Test in '..\DUnitX.Test.pas',
  DUnitX.TestFixture in '..\DUnitX.TestFixture.pas',
  DUnitX.TestFramework in '..\DUnitX.TestFramework.pas',
  DUnitX.TestResult in '..\DUnitX.TestResult.pas',
  DUnitX.TestResults in '..\DUnitX.TestResults.pas',
  DUnitX.TestRunner in '..\DUnitX.TestRunner.pas',
  DUnitX.Utils in '..\DUnitX.Utils.pas',
  DUnitX.WeakReference in '..\DUnitX.WeakReference.pas',
  DUnitX.Windows.Console in '..\DUnitX.Windows.Console.pas',
  DUnitX.Examples.EqualityAsserts in 'DUnitX.Examples.EqualityAsserts.pas';

var
  runner : ITestRunner;
  results : ITestResults;
  logger : ITestLogger;
begin
  try
    //Create the runner
    runner := TDUnitX.CreateRunner;
    runner.UseRTTI := True;
    //Tell the runner how we will log things
    logger := TDUnitXConsoleLogger.Create;
    runner.AddLogger(logger);

    //Run the tests
    results := runner.Execute;

    System.Writeln('Done.. press any key to quit.');
    ReadLn;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
