unit DUnitX.Expert.CodeGen.Templates;

interface

resourcestring

 STestDPR = 'program %0:s;'#13#10 +
 #13#10 +
 '{$IFNDEF TESTINSIGHT}'#13#10 +
 '{$APPTYPE CONSOLE}'#13#10 +
 '{$ENDIF}'+
 '{$STRONGLINKTYPES ON}'#13#10 +
 'uses'#13#10 +
 '  SysUtils,'#13#10 +
 '{$IFDEF TESTINSIGHT}'#13#10 +
 '  TestInsight.DUnitX,'#13#10 +
 '{$ENDIF}'#13#10 +
 '  DUnitX.Loggers.Console,'#13#10 +
 '  DUnitX.Loggers.Xml.NUnit,'#13#10 +
 '  DUnitX.TestFramework;'#13#10 +
 #13#10 +
 'var'#13#10 +
 '  runner : ITestRunner;'#13#10 +
 '  results : IRunResults;'#13#10 +
 '  logger : ITestLogger;'#13#10 +
 '  nunitLogger : ITestLogger;'#13#10 +
 'begin'#13#10 +
 '{$IFDEF TESTINSIGHT}'#13#10 +
 '  TestInsight.DUnitX.RunRegisteredTests;'#13#10 +
 '  exit;'#13#10 +
 '{$ENDIF}'#13#10 +
 '  try'#13#10 +
 '    //Check command line options, will exit if invalid'#13#10 +
 '    TDUnitX.CheckCommandLine;'#13#10 +
 '    //Create the test runner'#13#10 +
 '    runner := TDUnitX.CreateRunner;'#13#10 +
 '    //Tell the runner to use RTTI to find Fixtures'#13#10 +
 '    runner.UseRTTI := True;'#13#10 +
 '    //tell the runner how we will log things'#13#10 +
 '    //Log to the console window'#13#10 +
 '    logger := TDUnitXConsoleLogger.Create(true);'#13#10 +
 '    runner.AddLogger(logger);'#13#10 +
 '    //Generate an NUnit compatible XML File'#13#10 +
 '    nunitLogger := TDUnitXXMLNUnitFileLogger.Create(TDUnitX.Options.XMLOutputFile);'#13#10 +
 '    runner.AddLogger(nunitLogger);'#13#10 +
 '    runner.FailsOnNoAsserts := False; //When true, Assertions must be made during tests;'#13#10 +
 #13#10 +
 '    //Run tests'#13#10 +
 '    results := runner.Execute;'#13#10 +
 '    if not results.AllPassed then'#13#10 +
 '      System.ExitCode := EXIT_ERRORS;'#13#10 +
 #13#10 +
 '    {$IFNDEF CI}'#13#10 +
 '    //We don''t want this happening when running under CI.'#13#10 +
 '    if TDUnitX.Options.ExitBehavior = TDUnitXExitBehavior.Pause then'#13#10 +
 '    begin'#13#10 +
 '      System.Write(''Done.. press <Enter> key to quit.'');'#13#10 +
 '      System.Readln;'#13#10 +
 '    end;'#13#10 +
 '    {$ENDIF}'#13#10 +
 '  except'#13#10 +
 '    on E: Exception do'#13#10 +
 '      System.Writeln(E.ClassName, '': '', E.Message);'#13#10 +
 '  end;'#13#10 +
 'end.'#13#10;

 // 0 - Unit Name
 // 1 - Class Name
 // 2 - Setup/TearDown Methods - Interface
 // 3 - Sample Methods - Interface
 // 4 - Setup/TearDown Methods - Implementation
 // 5 - Sample Methods - Implementation
 STestUnit = 'unit %0:s;'#13#10 +
 #13#10 +
 'interface'#13#10 +
 'uses'#13#10 +
 '  DUnitX.TestFramework;'#13#10 +
 #13#10 +
 'type'#13#10 +
 #13#10 +
 '  [TestFixture]'#13#10 +
 '  %1:s = class(TObject) '#13#10 +
 '  public'#13#10 +
 '%2:s%3:s' +
 '  end;'#13#10 +
 #13#10 +
 'implementation'#13#10 +
 #13#10 +
 '%4:s%5:s' +
 #13#10 +
 'initialization'#13#10 +
 '  TDUnitX.RegisterTestFixture(%1:s);'#13#10 +
 'end.'#13#10;

 SSetupTearDownIntf =
 '    [Setup]'#13#10 +
 '    procedure Setup;'#13#10  +
 '    [TearDown]'#13#10  +
 '    procedure TearDown;'#13#10;

 // 0 - Class Name
 SSetupTearDownImpl =
 'procedure %0:s.Setup;'#13#10  +
 'begin'#13#10 +
 'end;'#13#10 +
 #13#10 +
 'procedure %0:s.TearDown;'#13#10 +
 'begin'#13#10 +
 'end;'#13#10 +
 #13#10;


 SSampleMethodsIntf =
 '    // Sample Methods'#13#10 +
 '    // Simple single Test'#13#10 +
 '    [Test]'#13#10 +
 '    procedure Test1;'#13#10 +
 '    // Test with TestCase Atribute to supply parameters.'#13#10 +
 '    [Test]'#13#10 +
 '    [TestCase(''TestA'',''1,2'')]'#13#10 +
 '    [TestCase(''TestB'',''3,4'')]'#13#10 +
 '    procedure Test2(const AValue1 : Integer;const AValue2 : Integer);'#13#10;

 // 0 - Class Name
 //TODO: Show Examples of calling Assert
 SSampleMethodsImpl =
 'procedure %0:s.Test1;'#13#10 +
 'begin'#13#10 +
 'end;'#13#10 +
 #13#10 +
 'procedure %0:s.Test2(const AValue1 : Integer;const AValue2 : Integer);'#13#10 +
 'begin'#13#10 +
 'end;'#13#10;

 SDefaultClassName = 'TMyTestObject';


implementation

end.
