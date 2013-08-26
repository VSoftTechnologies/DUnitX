{***************************************************************************}
{                                                                           }
{           DUnitX                                                          }
{                                                                           }
{           Copyright (C) 2013 Vincent Parrett                              }
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

unit DUnitX.TestFramework;

interface

uses
  classes,
  SysUtils,
  Rtti,
  TimeSpan,
  DUnitX.Generics,
  Generics.Collections;

//TODO: Automatic support for https://code.google.com/p/delphi-code-coverage/ would be cool

{$I DUnitX.inc}

type
  ///	<summary>
  ///	  A class decorated with this attribute will be tested. The parameters
  ///	  allow you to control which methods are treated as tests. By default 
  ///	  only methods decorated with the Test attribute are run as tests.
  ///	</summary>
  TestFixtureAttribute = class(TCustomAttribute)
  private
    FName : string;
    FDescription : string;
  public
    constructor Create;overload;
    constructor Create(const AName : string);overload;
    constructor Create(const AName : string; const ADescription : string);overload;
    property Name : string read FName;
    property Description : string read FDescription;
  end;


  ///	<summary>
  ///	  A TestFixture decorated with this attribute will be tested using it's
  ///	  own thread.  This can speed up unit testing when fixtures do not
  ///	  compete for resources and the test machine has enough cores to service
  ///	  the tests.
  ///	</summary>
  ///	<remarks>
  ///	  NOTE - CURRENTLY PLANNED BUT NOT IMPLEMENTED!!!
  ///	</remarks>
  TestInOwnThreadAttribute = class(TCustomAttribute);

  ///	<summary>
  ///	  A method marked with this attribute will run before any tests in.  Note
  ///	  that if more than one method is decorated with this attribute the first
  ///	  method found will be executed (not recommended!).
  ///	</summary>
  SetupFixtureAttribute = class(TCustomAttribute)
  end;


  ///	<summary>
  ///	  A method on a test fixture decorated with this attribute will run
  ///	  before each test method is run. Note that if more than one method is
  ///	  decorated with this attribute the first method found will be executed
  ///	  (not recommended!).
  ///	</summary>
  SetupAttribute = class(TCustomAttribute)
  end;


  ///	<summary>
  ///	  A method on a test fixture class decorated with this attribute will be
  ///	  run after each test method is run. Note that if more than one method is
  ///	  decorated with this attribute the first method found will be executed
  ///	  (not recommended!).
  ///	</summary>
  TearDownAttribute = class(TCustomAttribute)
  end;


  ///	<summary>
  ///	  A method marked with this attribute can contain a teardown method that
  ///	  will be run after each all tests in the fixture have executed.  Note
  ///	  that if more than one method is decorated with this attribute the first
  ///	  method found will be executed (not recommended!).
  ///	</summary>
  TearDownFixtureAttribute = class(TCustomAttribute)
  end;



  ///	<summary>
  ///	  This attribute marks a method as a test method
  ///	</summary>
  TestAttribute = class(TCustomAttribute)
  private
    FEnabled : boolean;
  public
    constructor Create;overload;
    constructor Create(const AEnabled : boolean);overload;
    property Enabled : boolean read FEnabled;
  end;


  ///	<summary>
  ///	  Marks a test method to be repeated count times.
  ///	</summary>
  ///	<remarks>
  ///	  NOT IMPLEMENTED
  ///	</remarks>
  RepeatAttribute = class(TCustomAttribute)
  private
    FCount : Cardinal;
  public
    constructor Create(const ACount : Cardinal);
    property Count : Cardinal read FCount;
  end;

  TValueArray = array of TValue;


  ///	<summary>
  ///	  The TestCaseAttribute allows you to specify the name of a function that
  ///	  returns a TValueArray which will be passed into a function that takes
  ///	  parameters. This is really only needed to work around the problens with
  ///	  the TestCaseAttribute. 
  ///	</summary>
  ///	<remarks>
  ///	  Note that the types in the TConstArray should match the parameters of
  ///	  the method we are testing.
  ///	</remarks>
  TestCaseAttribute = class(TCustomAttribute)
  private
    FCaseName : string;
    FValues : TValueArray;
  public
    constructor Create(const ACaseName : string; const AValues : string);overload;
    property Name : string read FCaseName;
    property Values : TValueArray read FValues;
  end;

  TTestMethod = procedure of object;
  TTestLocalMethod = reference to procedure;

  TLogLevel = (ltInformation, ltWarning, ltError);

const
  TLogLevelDesc : array[TLogLevel] of string = ('Info', 'Warn', 'Err');

type
{$IFDEF DELPHI_XE2_UP}
  ///	<summary>
  ///	  This helper class is intended to redirect the writeln method to a test
  ///	  runner so that it is logged correctly.
  ///	</summary>
  TTestFixtureHelper = class helper for TObject
  public
    procedure Log(const logType : TLogLevel; const msg : string);overload;
    procedure Log(const msg : string);overload;
    //for backwards compatibilty with DUnit tests.
    procedure Status(const msg : string);
    //redirects WriteLn to our loggers.
    procedure WriteLn(const msg : string);overload;
    procedure WriteLn;overload;
  end;
{$ENDIF}



  Assert = class
  public
    class procedure Pass(const message : string = '');
    class procedure Fail(const message : string = ''; const errorAddrs : pointer = nil);

    //TODO: Make more use of warnings. Currently none in use.
    class procedure Warn(const message : string = ''; const errorAddrs : pointer = nil);

    class procedure AreEqual(const left : string; const right : string; const ignoreCase : boolean = true; const message : string = '');overload;
    class procedure AreEqual(const left : string; const right : string; const message : string = '');overload;
    class procedure AreEqual(const left, right : Extended; const tolerance : Extended; const message : string = '');overload;
    class procedure AreEqual(const left, right : Extended; const message : string = '');overload;
    class procedure AreEqual(const left, right : TClass; const message : string = '');overload;
{$IFDEF DELPHI_XE_UP}
    //Delphi 2010 compiler bug breaks this
    class procedure AreEqual<T>(const left, right : T; const message : string = '');overload;
{$ENDIF}
    class procedure AreEqual(const left, right : Integer; const message : string = '');overload;

    class procedure AreEqualMemory(const left : Pointer; const right : Pointer; const size : Cardinal; message : string = '');

    class procedure AreNotEqual(const left : string; const right : string; const ignoreCase : boolean = true; const message : string = '');overload;
    class procedure AreNotEqual(const left, right : Extended; const tolerance : Extended; const message : string = '');overload;
    class procedure AreNotEqual(const left, right : TClass; const message : string = '');overload;
{$IFDEF DELPHI_XE_UP}
    //Delphi 2010 compiler bug breaks this
    class procedure AreNotEqual<T>(const left, right : T; const message : string = '');overload;
{$ELSE}
    class procedure AreNotEqual(const left, right : Integer; const message : string = '');overload;
{$ENDIF}
    class procedure AreNotEqualMemory(const left : Pointer; const right : Pointer; const size : Cardinal; message : string = '');

    class procedure AreSame(const left, right : TObject; const message : string = '');overload;
    class procedure AreSame(const left, right : IInterface; const message : string = '');overload;

    class procedure AreNotSame(const left, right : TObject; const message : string = '');overload;
    class procedure AreNotSame(const left, right : IInterface; const message : string = '');overload;

{$IFDEF DELPHI_XE_UP}
    //Delphi 2010 compiler bug breaks this
    class procedure Contains<T>(const list : IEnumerable<T>; const value : T; const message : string = '');overload;
    class procedure DoesNotContain<T>(const list : IEnumerable<T>; const value : T; const message : string = '');overload;
{$ENDIF}

    class procedure IsTrue(const condition : boolean; const message : string = '');
    class procedure IsFalse(const condition : boolean; const message : string = '');

    class procedure IsNull(const condition : TObject; const message : string = '');overload;
    class procedure IsNull(const condition : Pointer; const message : string = '');overload;
    class procedure IsNull(const condition : IInterface; const message : string = '');overload;
    class procedure IsNull(const condition : Variant; const message : string = '');overload;

    class procedure IsNotNull(const condition : TObject; const message : string = '');overload;
    class procedure IsNotNull(const condition : Pointer; const message : string = '');overload;
    class procedure IsNotNull(const condition : IInterface; const message : string = '');overload;
    class procedure IsNotNull(const condition : Variant; const message : string = '');overload;

    class procedure IsEmpty(const value : string; const message : string = '');overload;
    class procedure IsEmpty(const value : Variant; const message : string = '');overload;
    class procedure IsEmpty(const value : TStrings; const message : string = '');overload;
    class procedure IsEmpty(const value : TList; const message : string = '');overload;
    class procedure IsEmpty(const value : IInterfaceList; const message : string = '');overload;
{$IFDEF DELPHI_XE_UP}
    //Delphi 2010 compiler bug breaks this
    class procedure IsEmpty<T>(const value : IEnumerable<T>; const message : string = '');overload;
{$ENDIF}

    class procedure IsNotEmpty(const value : string; const message : string = '');overload;
    class procedure IsNotEmpty(const value : Variant; const message : string = '');overload;
    class procedure IsNotEmpty(const value : TStrings; const message : string = '');overload;
    class procedure IsNotEmpty(const value : TList; const message : string = '');overload;
    class procedure IsNotEmpty(const value : IInterfaceList; const message : string = '');overload;
{$IFDEF DELPHI_XE_UP}
    //Delphi 2010 compiler bug breaks this
    class procedure IsNotEmpty<T>(const value : IEnumerable<T>; const message : string = '');overload;
{$ENDIF}

    class procedure WillRaise(const AMethod : TTestLocalMethod; const exceptionClass : ExceptClass = nil; const msg : string = ''); overload;
    class procedure WillRaise(const AMethod : TTestMethod; const exceptionClass : ExceptClass = nil; const msg : string = ''); overload;
    class procedure WillNotRaise(const AMethod : TTestLocalMethod; const exceptionClass : ExceptClass = nil; const msg : string = ''); overload;
    class procedure WillNotRaise(const AMethod : TTestMethod; const exceptionClass : ExceptClass = nil; const msg : string = ''); overload;

    class procedure Contains(const theString : string; const subString : string; const ignoreCase : boolean = true; const message : string = '');overload;
    class procedure StartsWith(const theString : string; const subString : string;const ignoreCase : boolean = true; const message : string = '');
    class procedure EndsWith(const theString : string; const subString : string;const ignoreCase : boolean = true; const message : string = '');
    class procedure InheritsFrom(const descendant : TClass; const parent : TClass; const message : string = '');
{$IFDEF DELPHI_XE_UP}
    //Delphi 2010 compiler bug breaks this
    class procedure IsType<T>(const value : T; const message : string = '');overload;
{$ENDIF}

    {$IFDEF SUPPORTS_REGEX}
    class procedure IsMatch(const regexPattern : string; const theString : string; const message : string = '');
    {$ENDIF}
  end;

  {$M+}
  ITestFixtureInfo = interface;
  {$M-}

  {$M+}
  ITestInfo = interface
    ['{FF61A6EB-A76B-4BE7-887A-598EBBAE5611}']
    function GetName : string;
    function GetFullName : string;
    function GetActive : boolean;
    function GetTestFixture : ITestFixtureInfo;

    function GetTestStartTime : TDateTime;
    function GetTestEndTime : TDateTime;
    function GetTestDuration : TTimeSpan;

    function GetEnabled : boolean;
    procedure SetEnabled(const value : boolean);

    property Name : string read GetName;
    property FullName : string read GetFullName;
    property Enabled : boolean read GetEnabled write SetEnabled;

    property Active : boolean read GetActive;
    property Fixture : ITestFixtureInfo read GetTestFixture;
  end;

  ITestInfoList = interface(IList<ITestInfo>)
    ['{1C614DC2-537F-4229-863D-669E4211074E}']
  end;

  TTestInfoList = class(TDUnitXList<ITestInfo>, ITestInfoList);
  {$M-}

  {$M+}
  ITestFixtureInfo = interface
    ['{9E98B1E8-583A-49FC-B409-9B6937E22E81}']
    function GetName  : string;
    function GetNameSpace : string;
    function GetFullName : string;
    function GetDescription : string;
    function GetTests : IList<ITestInfo>;
    function GetTestClass : TClass;
    function GetSetupMethodName : string;
    function GetSetupFixtureMethodName : string;
    function GetTearDownMethodName : string;
    function GetTearDownFixtureMethodName : string;
    function GetTestInOwnThread : boolean;
    function GetHasChildren : boolean;

    function GetTestCount : cardinal;
    function GetActiveTestCount : cardinal;

    property Name                       : string read GetName;
    property NameSpace                  : string read GetNameSpace;
    property FullName                   : string read GetFullName;
    property Description                : string read GetDescription;
    property HasChildFixtures           : boolean read GetHasChildren;
    property TestClass                  : TClass read GetTestClass;
    property Tests                      : IList<ITestInfo> read GetTests;
    property SetupMethodName            : string read GetSetupMethodName;
    property SetupFixtureMethodName     : string read GetSetupFixtureMethodName;
    property TearDownMethodName         : string read GetTearDownMethodName;
    property TearDownFixtureMethodName  : string read GetTearDownFixtureMethodName;
    property TestInOwnThread            : boolean read GetTestInOwnThread;

    property TestCount                  : cardinal read GetTestCount;
    property ActiveTestCount            : cardinal read GetActiveTestCount;
  end;

  ITestFixtureInfoList = interface(IList<ITestFixtureInfo>)
    ['{DEE229E7-1450-4DC1-BEEA-562461439084}']
  end;

  TTestFixtureInfoList = class(TDUnitXList<ITestFixtureInfo>, ITestFixtureInfoList);
  {$M-}

  TTestResultType = (Pass,Failure,Warning,Error);

  {$M+}
  ITestResult = interface
  ['{EFD44ABA-4F3E-435C-B8FC-1F8EB4B35A3B}']
    function GetTest : ITestInfo;
    function GetResult : boolean;
    function GetResultType : TTestResultType;
    function GetMessage : string;
    function GetTestStartTime : TDateTime;
    function GetTestEndTime : TDateTime;
    function GetTestDuration : TTimeSpan;

    //Test
    property Test : ITestInfo read GetTest;

    //Results
    property Result : boolean read GetResult;
    property ResultType : TTestResultType read GetResultType;
    property Message : string read GetMessage;

    //Timing
    property TestStartTime : TDateTime read GetTestStartTime;
    property TestEndTime : TDateTime read GetTestEndTime;
    property TestDuration : TTimeSpan read GetTestDuration;
  end;
  {$M-}

  ITestError = interface(ITestResult)
    function GetExceptionClass : ExceptClass;
    function GetExceptionMessage : string;
    function GetExceptionLocationInfo : string;
    function GetExceptionAddressInfo : string;

    property ExceptionClass : ExceptClass read GetExceptionClass;
    property ExceptionMessage : string read GetExceptionMessage;
    property ExceptionLocationInfo : string read GetExceptionLocationInfo;
    property ExceptionAddressInfo : string read GetExceptionAddressInfo;
  end;

  IFixtureResult = interface(IEnumerable<ITestResult>)
  ['{2ED7CD6D-AF17-4A56-9ECF-7528A1583B30}']
    function GetErrorCount : integer;
    function GetFailureCount : integer;
    function GetWarningCount : integer;
    function GetSuccessCount : integer;
    function GetHasFailures : boolean;
    function GetFixture : ITestFixtureInfo;
    function GetResult(index : integer) : ITestResult;
    function GetCount : integer;

    function GetFailures  : IEnumerable<ITestResult>;
    function GetWarnings  : IEnumerable<ITestResult>;
    function GetErrors    : IEnumerable<ITestError>;
    function GetSuccesses : IEnumerable<ITestResult>;

    function GetResults   : IEnumerable<ITestResult>;

    property Fixture      : ITestFixtureInfo read GetFixture;
    property HasFailures  : Boolean read GetHasFailures;
    property ErrorCount   : integer read GetErrorCount;
    property WarningCount : integer read GetWarningCount;
    property SuccessCount : integer read GetSuccessCount;
    property ResultCount  : integer read GetCount;

    property Result[index : integer] : ITestResult read GetResult;
    property Results      : IEnumerable<ITestResult> read GetResults;
    property Failures     : IEnumerable<ITestResult> read GetFailures;
    property Warnings     : IEnumerable<ITestResult> read GetWarnings;
    property Errors       : IEnumerable<ITestError> read GetErrors;
    property Successes    : IEnumerable<ITestResult> read GetWarnings;
  end;

  {$M+}
  ITestResults = interface
  ['{4A335B76-33E3-48FD-87DF-9462428C60DA}']
    function GetCount : integer;
    function GetAllPassed : boolean;
    function GetFailureCount : integer;
    function GetErrorCount : integer;
    function GetWarningCount : integer;
    function GetPassCount : integer;
    function GetSuccessRate : integer;
    function GetStartTime: TDateTime;
    function GetFinishTime: TDateTime;
    function GetTestDuration: TTimeSpan;

    function GetFixtures : IEnumerable<ITestFixtureInfo>;
    function GetResults  : IEnumerable<ITestResult>;

    function ToString : string;

    property Count : integer read GetCount;
    property FailureCount : integer read GetFailureCount;
    property ErrorCount : integer read GetErrorCount;
    property WarningCount : integer read GetWarningCount;
    property PassCount : integer read GetPassCount;

    property StartTime : TDateTime read GetStartTime;
    property FinishTime: TDateTime read GetFinishTime;
    property TestDuration : TTimeSpan read GetTestDuration;

    property SuccessRate : integer read GetSuccessRate;
    property AllPassed : boolean read GetAllPassed;
  end;
  {$M-}

  ITestLogger = interface
    ['{AADCA392-421C-4060-8D47-79D7CAAB0EEF}']

    ///	<summary>
    ///	  Called at the start of testing. The default console logger prints the
    ///	  DUnitX banner.
    ///	</summary>
    procedure OnTestingStarts(const threadId, testCount, testActiveCount : Cardinal);

    ///	<summary>
    ///	  //Called before a Fixture is run.
    ///	</summary>
    procedure OnStartTestFixture(const threadId : Cardinal; const fixture : ITestFixtureInfo);

    ///	<summary>
    ///	  //Called before a fixture Setup method is run
    ///	</summary>
    procedure OnSetupFixture(const threadId : Cardinal; const fixture : ITestFixtureInfo);

    ///	<summary>
    ///	  Called after a fixture setup method is run.
    ///	</summary>
    procedure OnEndSetupFixture(const threadId : Cardinal; const fixture : ITestFixtureInfo);

    ///	<summary>
    ///	  Called before a Test method is run.
    ///	</summary>
    procedure OnBeginTest(const threadId : Cardinal; Test: ITestInfo);

    ///	<summary>
    ///	  Called before a test setup method is run.
    ///	</summary>
    procedure OnSetupTest(const threadId : Cardinal; Test: ITestInfo);

    ///	<summary>
    ///	  Called after a test setup method is run.
    ///	</summary>
    procedure OnEndSetupTest(const threadId : Cardinal; Test: ITestInfo);

    ///	<summary>
    ///	  Called before a Test method is run.
    ///	</summary>
    procedure OnExecuteTest(const threadId : Cardinal; Test: ITestInfo);

    ///	<summary>
    ///	  Called when a test succeeds
    ///	</summary>
    procedure OnTestSuccess(const threadId : Cardinal; Test: ITestResult);

    ///	<summary>
    ///	  Called when a test errors.
    ///	</summary>
    procedure OnTestError(const threadId : Cardinal; Error: ITestError);

    ///	<summary>
    ///	  Called when a test fails.
    ///	</summary>
    procedure OnTestFailure(const threadId : Cardinal; Failure: ITestError);

    ///	<summary>
    ///	  //called when a test results in a warning.
    ///	</summary>
    procedure OnTestWarning(const threadId : Cardinal; AWarning: ITestResult);

    ///	<summary>
    ///	  //allows tests to write to the log.
    ///	</summary>
    procedure OnLog(const logType : TLogLevel; const msg : string);

    ///	<summary>
    ///	  //called before a Test Teardown method is run.
    ///	</summary>
    procedure OnTeardownTest(const threadId : Cardinal; Test: ITestInfo);

    ///	<summary>
    ///	  //called after a test teardown method is run.
    ///	</summary>
    procedure OnEndTeardownTest(const threadId : Cardinal; Test: ITestInfo);

    ///	<summary>
    ///	  //called after a test method and teardown is run.
    ///	</summary>
    procedure OnEndTest(const threadId : Cardinal; Test: ITestResult);

    ///	<summary>
    ///	  //called before a Fixture Teardown method is called.
    ///	</summary>
    procedure OnTearDownFixture(const threadId : Cardinal; const fixture : ITestFixtureInfo);

    ///	<summary>
    ///	  //called after a Fixture Teardown method is called.
    ///	</summary>
    procedure OnEndTearDownFixture(const threadId : Cardinal; const fixture : ITestFixtureInfo);

    ///	<summary>
    ///	  //called after a Fixture has run.
    ///	</summary>
    procedure OnEndTestFixture(const threadId : Cardinal; const results : IFixtureResult);

    ///	<summary>
    ///	  //called after all fixtures have run.
    ///	</summary>
    procedure OnTestingEnds(const TestResults: ITestResults);
  end;

  TRunnerExitBehavior = (Continue, //The runner will exit normally
                         Pause, //The runner will pause after displaying it's results
                         HaltOnFailures //??
                         );

  ITestRunner = interface
    ['{06C0D8D2-B2D7-42F9-8D23-8F2D8A75263F}']
    procedure AddLogger(const value : ITestLogger);
    function GetUseCommandLineOptions : boolean;
    procedure SetUseCommandLineOptions(const value : boolean);
    function GetExitBehavior : TRunnerExitBehavior;
    procedure SetExitBehavior(const value : TRunnerExitBehavior);
    function GetUseRTTI : boolean;
    procedure SetUseRTTI(const value : boolean);

    //This is exposed for the GUI Runner cast as ITestFixtureList.
    function BuildFixtures : IInterface;

    function Execute : ITestResults;

    property ExitBehavior : TRunnerExitBehavior read GetExitBehavior write SetExitBehavior;
    property UseCommandLineOptions : boolean read GetUseCommandLineOptions write SetUseCommandLineOptions;

    procedure Log(const logType : TLogLevel; const msg : string);overload;
    procedure Log(const msg : string);overload;
    //for backwards compatibilty with DUnit tests.
    procedure Status(const msg : string);
    //redirects WriteLn to our loggers.
    procedure WriteLn(const msg : string);overload;
    procedure WriteLn;overload;


    ///	<summary>
    ///	  When true, test fixtures will be found by using RTTI to search for
    ///	  classes decorated as TestFixtures.  Note for this to work you may
    ///	  need to use {$STRONGLINKTYPES ON} otherwise the classes may not get
    ///	  linked as they are not referenced. When False you will need to
    ///	  register the fixtures using TDUnitX.RegisterTestFixture
    ///	</summary>
    property UseRTTI : boolean read GetUseRTTI write SetUseRTTI;
  end;

  ICommandLine = interface
    ['{A86D66B3-2CD3-4E11-B0A8-9602EB5790CB}']
    function GetHideBanner : boolean;
    procedure SetHideBanner(const value : boolean);
    function GetLogLevel : TLogLevel;

    function HasOption(const optionName : string) : boolean;
    function GetOptionValue(const optionName : string) : string;

    //standard options
    property HideBanner : boolean read GetHideBanner write SetHideBanner; // -b
    property LogLevel : TLogLevel read GetLogLevel;  // -l:e -l:w -l:i
  end;

  TDUnitX = class
  public class var
    RegisteredFixtures : TDictionary<string,TClass>;
  public
    class constructor Create;
    class destructor Destroy;
    class function CreateRunner : ITestRunner;overload;
    class function CreateRunner(const useCommandLineOptions : boolean) : ITestRunner;overload;
    class function CreateRunner(const ALogger : ITestLogger) : ITestRunner;overload;
    class function CreateRunner(const useCommandLineOptions : boolean; const ALogger : ITestLogger) : ITestRunner;overload;
    class procedure RegisterTestFixture(const AClass : TClass; const AName : string = '' );
    class function CommandLine : ICommandLine;
    class function CurrentRunner : ITestRunner;
  end;

  // Register an implementation via TDUnitXIoC.DefaultContainer
  IStacktraceProvider = interface
  ['{382288B7-932C-4B6E-8417-660FFCA849EB}']
    function GetStackTrace(const ex: Exception; const exAddressAddress: Pointer) : string;
    function PointerToLocationInfo(const Addrs: Pointer): string;
    function PointerToAddressInfo(Addrs: Pointer): string;
  end;

  ETestFrameworkException = class(Exception);

  ENotImplemented = class(ETestFrameworkException);

  //base exception for any internal exceptions which cause the test to stop
  EAbort = class(ETestFrameworkException);

  ETestFailure = class(EAbort);
  ETestPass = class(EAbort);
  ETestWarning = class(EABort);
  ENoTestsRegistered = class(ETestFrameworkException);

{$IFDEF DELPHI_XE_DOWN}
  function ReturnAddress: Pointer; assembler;
{$ENDIF}

implementation

uses
  DUnitX.TestRunner,
  DUnitX.Utils,
  DUnitX.Commandline,
  Variants,
  Math,
  StrUtils,
  Types,
  {$IFDEF SUPPORTS_REGEX}
  RegularExpressions,
  {$ENDIF}
  Generics.Defaults;

function IsBadPointer(P: Pointer):Boolean;register;
begin
  try
    Result  := (p = nil) or ((Pointer(P^) <> P) and (Pointer(P^) = P));
  except
    Result := true;
  end
end;

{$IFDEF DELPHI_XE_DOWN}
function ReturnAddress: Pointer; assembler;
const
  CallerIP = $4;
asm
   mov   eax, ebp
   call  IsBadPointer
   test  eax,eax
   jne   @@Error

   mov   eax, [ebp].CallerIP
   sub   eax, 5   // 5 bytes for call

   push  eax
   call  IsBadPointer
   test  eax,eax
   pop   eax
   je    @@Finish

@@Error:
   xor eax, eax
@@Finish:
end;
{$ENDIF}

{ TestFixture }

constructor TestFixtureAttribute.Create(const AName: string);
begin
  FName := AName;
end;

constructor TestFixtureAttribute.Create;
begin

end;


constructor TestFixtureAttribute.Create(const AName: string; const ADescription : string);
begin
  FName := AName;
  FDescription := ADescription;
end;

{ Assert }

class procedure Assert.AreEqual(const left, right, tolerance: Extended; const message: string);
begin
  if not Math.SameValue(left,right,tolerance) then
    Fail(Format('left %g but got %g - %s' ,[left,right,message]), ReturnAddress);
end;


class procedure Assert.AreEqual(const left, right: TClass; const message: string);
var
  msg : string;
begin
  if left <> right then
  begin
    msg := ' is not equal to ';
    if left = nil then
      msg := 'nil' + msg
    else
      msg := left.ClassName + msg;

    if right = nil then
      msg := msg +  'nil'
    else
      msg := msg + right.ClassName;

    if message <> '' then
      msg := msg + '. ' + message;

    Fail(msg, ReturnAddress);
  end;
end;

{$IFDEF DELPHI_XE_UP}
    //Delphi 2010 compiler bug breaks this
class procedure Assert.AreEqual<T>(const left, right: T; const message: string);
var
  comparer : IComparer<T>;
  leftvalue, rightvalue : TValue;
begin
  comparer := TComparer<T>.Default;
  if comparer.Compare(right,left) <> 0 then
  begin
    leftValue := TValue.From<T>(left);
    rightValue := TValue.From<T>(right);

    Fail(Format('left %s but got %s', [leftValue.AsString, rightValue.AsString]), ReturnAddress);
  end;
end;
{$ENDIF}
class procedure Assert.AreEqual(const left, right: Integer; const message: string);
begin
  if left <> right then
    Fail(Format('left %d but got %d - %s' ,[left, right, message]), ReturnAddress);
end;


class procedure Assert.AreEqual(const left, right, message: string);
begin
  AreEqual(left, right, True, message);
end;

class procedure Assert.AreEqual(const left, right: Extended; const message: string);
begin
  AreEqual(left, right, 0, message);
end;

class procedure Assert.AreEqualMemory(const left : Pointer; const right : Pointer; const size : Cardinal; message : string);
begin
  if not CompareMem(left, right, size) then
    Fail('Memory values are not equal. ' + message, ReturnAddress);
end;

class procedure Assert.AreNotEqual(const left, right, tolerance: Extended; const message: string);
begin
  if not Math.SameValue(left, right, tolerance) then
    Fail(Format('%g equals right %g %s' ,[left,right,message]), ReturnAddress);
end;


class procedure Assert.AreNotEqual(const left, right: string;const ignoreCase: boolean; const message: string);

  function AreNotEqualText(const left, right: string; const ignoreCase: boolean): boolean;
  begin
    if ignoreCase then
      Result := SameText(left, right)
    else
      Result := SameStr(left, right);

  end;
begin
  if AreNotEqualText(left, right, ignoreCase) then
     Fail(Format('[%s] is Equal to [%s] %s', [left, right, message]), ReturnAddress);
end;

class procedure Assert.AreNotEqual(const left, right: TClass; const message: string);
var
  msg : string;
begin
  if left = right then
  begin
    msg := ' is equal to ';
    if left = nil then
      msg := 'nil' + msg
    else
      msg := left.ClassName + msg;

    if right = nil then
      msg := msg +  'nil'
    else
      msg := msg + right.ClassName;
    if message <> '' then
      msg := msg + '. ' + message;

    Fail(msg, ReturnAddress);
  end;
end;

{$IFDEF DELPHI_XE_UP}
//Delphi 2010 compiler bug breaks this
class procedure Assert.AreNotEqual<T>(const left, right: T; const message: string);
var
  comparer : IComparer<T>;
  leftValue, rightValue : TValue;
begin
  comparer := TComparer<T>.Default;
  if comparer.Compare(right,left) = 0 then
  begin
    leftValue := TValue.From<T>(left);
    rightValue := TValue.From<T>(right);

    Fail(Format('left %s Not Equal To %s',[leftValue.AsString,rightValue.AsString]), ReturnAddress);
  end;
end;
{$ELSE}
class procedure Assert.AreNotEqual(const left, right: Integer; const message: string);
begin
  if left = right then
    Fail(Format('%d equals right %d %s' ,[left, right, message]), ReturnAddress);
end;
{$ENDIF}

class procedure Assert.AreNotEqualMemory(const left, right: Pointer; const size: Cardinal; message: string);
begin
  if CompareMem(left,right, size) then
    Fail('Memory values are equal. ' + message, ReturnAddress);
end;

class procedure Assert.AreNotSame(const left, right: TObject; const message: string);
begin
  if left.Equals(right) then
    Fail(Format('Object [%s] Equals Object [%s] %s',[left.ToString,right.ToString,message]), ReturnAddress);
end;

class procedure Assert.AreNotSame(const left, right: IInterface; const message: string);
begin
  if left = right then
    Fail(Format('references are the same. %s',[message]), ReturnAddress);
end;

class procedure Assert.AreSame(const left, right: IInterface; const message: string);
begin
  if left <> right then
    Fail(Format('references are Not the same. %s',[message]), ReturnAddress);
end;

class procedure Assert.AreSame(const left, right: TObject; const message: string);
begin
  if not left.Equals(right) then
    Fail(Format('Object [%s] Not Object [%s] %s',[left.ToString,right.ToString,message]), ReturnAddress);
end;

{$IFDEF DELPHI_XE_UP}
//Delphi 2010 compiler bug breaks this
class procedure Assert.Contains<T>(const list: IEnumerable<T>; const value: T; const message: string);
var
  o : T;
  comparer : IComparer<T>;
begin
  comparer := TComparer<T>.Default;
  for o in list do
  begin
    if comparer.Compare(o,value) = 0 then
      exit;
  end;

  Fail(Format('List does not contain value. %s',[message]), ReturnAddress);
end;
{$ENDIF}

{$IFDEF DELPHI_XE_UP}
//Delphi 2010 compiler bug breaks this
class procedure Assert.DoesNotContain<T>(const list: IEnumerable<T>; const value: T; const message: string);
var
  o : T;
  comparer : IComparer<T>;
begin
  comparer := TComparer<T>.Default;
  for o in list do
  begin
    if comparer.Compare(o,value) = 0 then
      Fail(Format('List contains value. %s',[message]), ReturnAddress);
  end;
end;
{$ENDIF}

class procedure Assert.Fail(const message : string; const errorAddrs : pointer);
begin
  //If we have been given a return then use it. (makes the exception appear on level above in the stack)
  if errorAddrs <> nil then
    raise ETestFailure.Create(message) at errorAddrs
  else
    //Otherwise use the return address we can currently get to for where to raise the exception
    raise ETestFailure.Create(message) at ReturnAddress;

end;


class procedure Assert.Pass(const message: string);
begin
  raise ETestPass.Create(message);
end;

class procedure Assert.InheritsFrom(const descendant, parent: TClass; const message: string);
var
  msg : string;

begin
  if (descendant = nil) or (parent = nil) or (not descendant.InheritsFrom(parent)) then
  begin
    msg := ' does not inherit from ';
    if descendant = nil then
      msg := 'nil' + msg
    else
      msg := descendant.ClassName + msg;
    if parent = nil then
      msg := msg + 'nil'
    else
      msg := parent.ClassName + msg;
    msg := msg + '.';
    if True then
    if message <> '' then
      msg := msg + ' ' + message;

    Fail(msg, ReturnAddress);
  end;

end;

class procedure Assert.IsEmpty(const value: IInterfaceList; const message: string);
begin
  if value.Count > 0 then
    Fail(Format('List is Not empty. %s',[message]), ReturnAddress);
end;

class procedure Assert.IsEmpty(const value: TList; const message: string);
begin
  if value.Count > 0 then
    Fail(Format('List is Not empty. %s',[message]), ReturnAddress);
end;

class procedure Assert.IsEmpty(const value, message: string);
begin
  if Length(value) > 0 then
    Fail(Format('String is Not empty. %s',[message]), ReturnAddress);
end;

class procedure Assert.IsEmpty(const value: Variant; const message: string);
begin
  if VarIsEmpty(value) or VarIsNull(value) then
    Fail(Format('Variant is Not empty. %s',[message]), ReturnAddress);
end;

class procedure Assert.IsEmpty(const value: TStrings; const message: string);
begin
  if value.Count > 0 then
    Fail(Format('List is Not empty. %s',[message]), ReturnAddress);
end;

{$IFDEF DELPHI_XE_UP}
//Delphi 2010 compiler bug breaks this
class procedure Assert.IsEmpty<T>(const value: IEnumerable<T>; const message: string);
var
  o : T;
  count : integer;
begin
  count := 0;
  for o in value do
    Inc(count);

  if count > 0 then
    Fail(Format('List is Not empty. %s',[message]), ReturnAddress);
end;
{$ENDIF}

class procedure Assert.IsFalse(const condition: boolean; const message: string);
begin
  if condition then
   Fail(Format('Condition is True when False expected. %s',[message]), ReturnAddress);
end;

class procedure Assert.IsNotEmpty(const value: TList; const message: string);
begin
  if value.Count = 0 then
   Fail(Format('List is Empty. %s',[message]), ReturnAddress);
end;

class procedure Assert.IsNotEmpty(const value: IInterfaceList; const message: string);
begin
  if value.Count = 0 then
   Fail(Format('List is Empty. %s',[message]), ReturnAddress);
end;

class procedure Assert.IsNotEmpty(const value: TStrings; const message: string);
begin
  if value.Count = 0 then
   Fail(Format('List is Empty. %s',[message]), ReturnAddress);
end;

class procedure Assert.IsNotEmpty(const value, message: string);
begin
  if value = '' then
   Fail(Format('Variant is Empty. %s',[message]), ReturnAddress);
end;

class procedure Assert.IsNotEmpty(const value: Variant; const message: string);
begin
  if VarIsEmpty(value) then
    Fail(Format('Variant is Empty. %s',[message]), ReturnAddress);
end;

{$IFDEF DELPHI_XE_UP}
//Delphi 2010 compiler bug breaks this
class procedure Assert.IsNotEmpty<T>(const value: IEnumerable<T>; const message: string);
var
  x : T;
  count : integer;
begin
  count := 0;
  for x in value do
    Inc(count);

  if count = 0 then
    Fail(Format('List is Empty when Not empty expected. %s',[message]), ReturnAddress);
end;
{$ENDIF}

class procedure Assert.IsNotNull(const condition: IInterface; const message: string);
begin
  if condition = nil then
    Fail(Format('Interface is Nil when not nil expected. %s',[message]), ReturnAddress);
end;

class procedure Assert.IsNotNull(const condition: Pointer; const message: string);
begin
  if condition = nil then
    Fail(Format('Pointer is Nil when not Nil expected. %s',[message]), ReturnAddress);
end;

class procedure Assert.IsNotNull(const condition: TObject; const message: string);
begin
  if condition = nil then
    Fail(Format('Object is Nil when Not Nil expected. %s',[message]), ReturnAddress);
end;

class procedure Assert.IsNotNull(const condition: Variant; const message: string);
begin
  if VarIsNull(condition) then
    Fail(Format('Variant is Null when Not Null expcted. %s',[message]), ReturnAddress);
end;

class procedure Assert.IsNull(const condition: Variant; const message: string);
begin
  if not VarIsNull(condition) then
    Fail(Format('Variant is Not Null when Null expected. [%s]',[message]), ReturnAddress);
end;

class procedure Assert.IsNull(const condition: IInterface; const message: string);
begin
  if condition <> nil then
    Fail(Format('Interface is not Nil when nil expected. [%s]',[message]), ReturnAddress);
end;

class procedure Assert.IsNull(const condition: TObject; const message: string);
begin
  if condition <> nil then
    Fail(Format('Object is not nil when nil expected. [%s]',[message]), ReturnAddress);
end;

class procedure Assert.IsNull(const condition: Pointer; const message: string);
begin
  if condition <> nil then
    Fail(Format('Pointer is not Nil when nil expected. [%s]',[message]), ReturnAddress);
end;

class procedure Assert.IsTrue(const condition: boolean;const message : string);
begin
  if not condition then
    Fail(Format('Condition is False when True expected. [%s]',[message]), ReturnAddress);
end;

{$IFDEF DELPHI_XE_UP}
//Delphi 2010 compiler bug breaks this
class procedure Assert.IsType<T>(const value: T; const message : string);
var
  val : TValue;
begin
  begin
    val := TValue.From<T>(value);
    if not val.IsType<T> then
      Fail('value is not of type T', ReturnAddress);
  end;
end;
{$ENDIF}

class procedure Assert.WillNotRaise(const AMethod: TTestMethod; const exceptionClass: ExceptClass; const msg: string);
begin
  Assert.WillNotRaise(
    procedure
    begin
      AMethod;
    end,
    exceptionClass, msg);
end;

class procedure Assert.WillRaise(const AMethod : TTestLocalMethod; const exceptionClass : ExceptClass; const msg : string);
  function GetMsg : string;
  begin
    if msg <> '' then
      result :=  #13#10 + msg
    else
      result := '';
  end;
begin
  try
    AMethod;
  except
    //we expect an exception to be thrown.
    on e : Exception do
    begin
      if exceptionClass <> nil then
      begin
        if e.ClassType <> exceptionClass then
          Fail(Format('Method raised [%s] was expecting [%s]. %s', [e.ClassName, exceptionClass.ClassName, e.message]), ReturnAddress)
        else
          exit;
      end;
    end;
  end;
  Fail('Method did not throw any excpetions.' + GetMsg, ReturnAddress);
end;

class procedure Assert.Warn(const message : string; const errorAddrs : pointer);
begin
  if errorAddrs = nil then
    raise ETestWarning.Create(message) at ReturnAddress
  else
    raise ETestWarning.Create(message) at errorAddrs;
end;

class procedure Assert.WillNotRaise(const AMethod : TTestLocalMethod; const exceptionClass : ExceptClass; const msg : string);
  function GetMsg : string;
  begin
    if msg <> '' then
      result :=  #13#10 + msg
    else
      result := '';
  end;
begin
  try
    AMethod;
  except
    on e : Exception do
    begin
      if exceptionClass <> nil then
      begin
        if e.ClassType = exceptionClass then
           Fail('Method raised an exception of type : ' + exceptionClass.ClassName + #13#10 + e.Message + GetMsg, ReturnAddress);
      end
      else
        Fail(Format('Method raised [%s] was expecting not to raise [%s]. %s', [e.ClassName, exceptionClass.ClassName, e.message]), ReturnAddress);
    end;
  end;
end;

class procedure Assert.WillRaise(const AMethod: TTestMethod; const exceptionClass: ExceptClass; const msg: string);
begin
  Assert.WillRaise(
    procedure
    begin
      AMethod;
    end,
    exceptionClass, msg);
end;

class procedure Assert.AreEqual(const left, right : string;  const ignoreCase : boolean; const message: string);
begin
  if ignoreCase then
  begin
    if not SameText(left,right) then
      Fail(Format('[%s] is Not Equal to [%s] %s',[left,right,message]), ReturnAddress);
  end
  else if not SameStr(left,right) then
      Fail(Format('[%s] is Not Equal to [%s] %s',[left,right,message]), ReturnAddress);
end;

class procedure Assert.Contains(const theString : string; const subString : string; const ignoreCase : boolean; const message : string);
begin
  if ignoreCase then
  begin
    if not StrUtils.ContainsText(theString,subString) then
      Fail(Format('[%s] does not contain [%s] %s',[theString,subString,message]), ReturnAddress);
  end
  else if not StrUtils.ContainsStr(theString,subString) then
    Fail(Format('[%s] does not contain [%s] %s',[theString,subString,message]), ReturnAddress);
end;

class procedure Assert.EndsWith(const theString : string; const subString : string; const ignoreCase : boolean; const message : string);
begin
  if ignoreCase then
  begin
    if not StrUtils.EndsText(theString,subString) then
      Fail(Format('[%s] does not end with [%s] %s',[theString,subString,message]), ReturnAddress);
  end
  else if not StrUtils.EndsStr(theString,subString) then
    Fail(Format('[%s] does not end with [%s] %s',[theString,subString,message]), ReturnAddress);
end;
{$IFDEF SUPPORTS_REGEX}
class procedure Assert.IsMatch(const regexPattern, theString, message: string);
begin
  if not TRegEx.IsMatch(theString,regexPattern) then
    Fail(Format('[%s] does not match [%s] %s',[theString,regexPattern,message]), ReturnAddress);
end;
{$ENDIF}

class procedure Assert.StartsWith(const theString : string; const subString : string; const ignoreCase : boolean; const message: string);
begin
  if ignoreCase then
  begin
    if not StrUtils.StartsText(theString,subString) then
      Fail(Format('[%s] does Not Start with [%s] %s',[theString,subString,message]), ReturnAddress);
  end
  else if not StrUtils.StartsStr(theString,subString) then
    Fail(Format('[%s] does Not Start with [%s] %s',[theString,subString,message]), ReturnAddress);
end;

{ Test }

constructor TestAttribute.Create;
begin
  FEnabled := True;
end;

constructor TestAttribute.Create(const AEnabled: boolean);
begin
  FEnabled := AEnabled;
end;

{ TDUnit3 }

class function TDUnitX.CreateRunner: ITestRunner;
begin
  result := TDUnitXTestRunner.Create(false,nil);
end;

class function TDUnitX.CreateRunner(const ALogger: ITestLogger): ITestRunner;
begin
  result := TDUnitXTestRunner.Create(false,ALogger);

end;

class function TDUnitX.CreateRunner(const useCommandLineOptions: boolean): ITestRunner;
begin
  result := TDUnitXTestRunner.Create(useCommandLineOptions,nil);

end;

class function TDUnitX.CommandLine: ICommandLine;
begin
  result := DUnitX.CommandLine.CommandLine;
end;

class constructor TDUnitX.Create;
begin
  RegisteredFixtures := TDictionary<string,TClass>.Create;
end;

class function TDUnitX.CreateRunner(const useCommandLineOptions: boolean; const ALogger: ITestLogger): ITestRunner;
begin
  result := TDUnitXTestRunner.Create(useCommandLineOptions,ALogger);
end;

class function TDUnitX.CurrentRunner: ITestRunner;

begin
  if not TDUnitXTestRunner.FActiveRunners.TryGetValue(TThread.CurrentThread.ThreadId,result) then
    raise Exception.Create('No Runner found for current thread');

end;

class destructor TDUnitX.Destroy;
begin
  RegisteredFixtures.Free;
end;

class procedure TDUnitX.RegisterTestFixture(const AClass: TClass; const AName : string);
var
  sName : string;
  attrib : TestFixtureAttribute;
  rType : TRttiType;
begin
  // If a name is passed in, then that's what we'll use, otherwise see if there
  // is a TestFixture attribute and if it's name is set then use it. If no
  // attribute then fallback to classname
  if AName <> '' then
    sName := AName
  else
  begin
    rType := DUnitX.Utils.GetRttiType(AClass);
    if rType <> nil then
    begin
      if rType.TryGetAttributeOfType<TestFixtureAttribute>(attrib) then
        sName := attrib.Name;
    end;


    if sName = '' then
      sName := AClass.ClassName;


  end;




  if not RegisteredFixtures.ContainsValue(AClass) then
    RegisteredFixtures.Add(sName, AClass);
end;

{ TestCaseAttribute }


constructor TestCaseAttribute.Create(const ACaseName: string; const AValues: string);
var
  i: Integer;
  l : integer;
  lValues : TStringDynArray;
begin
  FCaseName := ACaseName;
  lValues := SplitString(AValues,',');
  l := Length(lValues);
  SetLength(FValues,l);
  for i := 0 to l -1 do
    FValues[i] := TValue.From<string>(lValues[i]);
end;


{$IFDEF DELPHI_XE2_UP}

{ TTestFixtureHelper }

procedure TTestFixtureHelper.Log(const msg: string);
begin
  Self.Log(TLogLevel.ltInformation,msg);
end;

procedure TTestFixtureHelper.Log(const logType : TLogLevel; const msg: string);
var
  runner : ITestRunner;
begin
  if TDUnitXTestRunner.FActiveRunners.TryGetValue(TThread.CurrentThread.ThreadId,runner) then
    runner.Log(logType,msg)
  else
    System.Writeln(msg);
end;

procedure TTestFixtureHelper.Status(const msg: string);
begin
  Self.Log(TLogLevel.ltInformation,msg);
end;

procedure TTestFixtureHelper.WriteLn;
begin
  Self.Log(TLogLevel.ltInformation,'');
end;

procedure TTestFixtureHelper.WriteLn(const msg: string);
begin
  Self.Log(TLogLevel.ltInformation,msg);
end;
{$ENDIF}

{ RepeatAttribute }

constructor RepeatAttribute.Create(const ACount: Cardinal);
begin
  FCount := ACount;
end;

end.
