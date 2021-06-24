program Project1;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  FastMM4,
  System.SysUtils,
  DUnitX.Attributes,
  DUnitX.Loggers.Console,
  DUnitX.TestFramework;

type

  [TestFixture]
  TUnitTests = class
  [Test, WillRaiseAttribute(Exception)]
    procedure TestWillRaise;
  end;

  { TUnitTests }

procedure TUnitTests.TestWillRaise;
begin
  raise Exception.Create('Error Message');
end;

var
  runner: ITestRunner;
  results: IRunResults;
  logger: ITestLogger;
  nunitLogger: ITestLogger;

begin
  try
    TDUnitX.RegisterTestFixture(TUnitTests);
    // Check command line options, will exit if invalid
    TDUnitX.CheckCommandLine;
    // Create the test runner
    runner := TDUnitX.CreateRunner;
    // Tell the runner to use RTTI to find Fixtures
    runner.UseRTTI := True;
    // tell the runner how we will log things
    // Log to the console window
    logger := TDUnitXConsoleLogger.Create();
    runner.AddLogger(logger);
    runner.FailsOnNoAsserts := False; // When true, Assertions must be made during tests;

    results := runner.Execute;
    if not results.AllPassed then
      System.ExitCode := EXIT_ERRORS;

{$IFNDEF CI}
    // We don't want this happening when running under CI.
    if TDUnitX.Options.ExitBehavior = TDUnitXExitBehavior.Pause then
    begin
      System.Write('Done.. press <Enter> key to quit.');
      System.Readln;
    end;
{$ENDIF}
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
