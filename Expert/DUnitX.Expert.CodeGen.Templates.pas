{***************************************************************************}
{                                                                           }
{           DUnitX                                                          }
{                                                                           }
{           Copyright (C) 2015 Vincent Parrett & Contributors               }
{                                                                           }
{           vincent@finalbuilder.com                                        }
{           http://www.finalbuilder.com                                     }
{                                                                           }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}

unit DUnitX.Expert.CodeGen.Templates;

interface

{$I DUnitX.inc}

resourcestring

 { Delphi template code }

 STestDPR = 'program %0:s;'#13#10 +
 #13#10 +
 '{$IFNDEF TESTINSIGHT}'#13#10 +
 '{$APPTYPE CONSOLE}'#13#10 +
 '{$ENDIF}'+
 '{$STRONGLINKTYPES ON}'#13#10 +
 'uses'#13#10 +
 {$IFDEF USE_NS}
 '  System.SysUtils,'#13#10 +
 {$ELSE}
 '  SysUtils,'#13#10 +
 {$ENDIF}
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
 '    // Test with TestCase Attribute to supply parameters.'#13#10 +
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

 { C++Builder template code }

 STestCBPROJ = '#include <System.Sysutils.hpp>'#13#10 +
 '#include <DUnitX.Loggers.Console.hpp>'#13#10 +
 '#include <DUnitX.Loggers.Xml.NUnit.hpp>'#13#10 +
 '#include <DUnitX.TestFramework.hpp>'#13#10 +
 '#include <stdio.h>'#13#10 +
 #13#10 +
 'int main()'#13#10 +
 '{'#13#10 +
 '  try'#13#10 +
 '  {'#13#10 +
 '    TDUnitX::CheckCommandLine();'#13#10 +
 '    _di_ITestRunner runner = TDUnitX::CreateRunner();'#13#10 +
 '    _di_ITestLogger logger(*new TDUnitXConsoleLogger(true));'#13#10 +
 '    runner->AddLogger(logger);'#13#10 +
 #13#10 +
 '    _di_ITestLogger nunitLogger(*new TDUnitXXMLNUnitFileLogger(TDUnitX::Options->XMLOutputFile));'#13#10 +
 '    runner->AddLogger(nunitLogger);'#13#10 +
 #13#10 +
 '    _di_IRunResults results = runner->Execute();'#13#10 +
 #13#10 +
 '#if !defined(CI)'#13#10 +
 '    if (TDUnitX::Options->ExitBehavior == TDUnitXExitBehavior::Pause)'#13#10 +
 '    {'#13#10 +
 '      printf("Done.. press <Enter> key to quit.");'#13#10 +
 '      getchar();'#13#10 +
 '    }'#13#10 +
 '#endif'#13#10 +
 #13#10 +
 '    return results->AllPassed ? EXIT_SUCCESS : EXIT_FAILURE;'#13#10 +
 '  }'#13#10 +
 '  catch(System::Sysutils::Exception& Ex)'#13#10 +
 '  {'#13#10 +
 '    printf("Exception: ''''%s''''\n", AnsiString(Ex.Message).c_str());'#13#10 +
 '  }'#13#10 +
 '  return EXIT_FAILURE;'#13#10 +
 '}';

 STestCPPUnit = '#include <DUnitX.TestFramework.hpp>'#13#10 +
 '#include <stdio.h>'#13#10 +
 #13#10 +
 '#pragma option --xrtti'#13#10 +
 #13#10 +
 'class __declspec(delphirtti) %1:s : public TObject'#13#10 +
 '{'#13#10 +
 'public:'#13#10 +
 '%2:s' +
 #13#10 +
 '__published:'#13#10 +
 '%3:s' +
 '};'#13#10 +
 #13#10 +
 #13#10 +
 '%4:s' +
 #13#10 +
 '%5:s' +
 #13#10 +
 'static void registerTests()'#13#10 +
 '{'#13#10 +
 '  TDUnitX::RegisterTestFixture(__classid(%1:s));'#13#10 +
 '}'#13#10 +
 '#pragma startup registerTests 33';

 SSetupTearDownCPPIntf =
 '  virtual void __fastcall SetUp();'#13#10 +
 '  virtual void __fastcall TearDown();'#13#10;

 SSetupTearDownCPPImpl =
 'void __fastcall %0:s::SetUp()'#13#10 +
 '{'#13#10 +
 '}'#13#10 +
 #13#10 +
 'void __fastcall %0:s::TearDown()'#13#10 +
 '{'#13#10 +
 '}'#13#10;

 SSampleMethodsCPPIntf =
 '  void __fastcall Test1();'#13#10 +
 '  void __fastcall Test2();'#13#10;

 SSampleMethodsCPPImpl =
 'void __fastcall %0:s::Test1()'#13#10 +
 '{'#13#10 +
 '  // TODO'#13#10 +
 '  String s("Hello");'#13#10 +
 '  Dunitx::Testframework::Assert::IsTrue(s == "Hello");'#13#10 +
 '}'#13#10 +
 #13#10 +
 'void __fastcall %0:s::Test2()'#13#10 +
 '{'#13#10 +
 '  // TODO'#13#10 +
 '  String s("Hello");'#13#10 +
 '  Dunitx::Testframework::Assert::IsTrue(s == "Bonjour"); // This fails for illustrative purposes'#13#10 +
 '}'#13#10;


implementation

end.
