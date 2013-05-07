{***************************************************************************}
{                                                                           }
{           DUnitX                                                          }
{                                                                           }
{           Copyright (C) 2012 Vincent Parrett                              }
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
  Generics.Collections;

{$I DUnitX.inc}

type

  ///  A class decorated with this attribute will be tested. The parameters
  ///  allow you to control which methods are treated as tests. By default
  ///  only methods decorated with the Test attribute are run as tests.
  TestFixtureAttribute = class(TCustomAttribute)
  private
    FName : string;
    FRunAllMethods : boolean;
  public
    constructor Create;overload;
    constructor Create(const AName : string);overload;
    constructor Create(const ARunAllMethods : boolean);overload;
    constructor Create(const AName : string; const ARunAllMethods : boolean);overload;
    property Name : string read FName;
  end;

  ///  A TestFixture decorated with this attribute will be tested using it's own thread
  ///  This can speed up unit testing when fixtures do not compete for resources and the
  ///  test machine has enough cores to service the tests.
  ///  NOTE - CURRENTLY PLANNED BUT NOT IMPLEMENTED!!!
  TestInOwnThreadAttribute = class(TCustomAttribute);

  ///  A method marked with this attribute will run before any tests in.
  ///  Note that if more than one method is decorated with this attribute
  ///  the first method found will be executed (notrecommended!).
  SetupFixtureAttribute = class(TCustomAttribute)
  end;


  ///  A method on a test fixture decorated with this attribute will
  ///  run before each test method is run. Note that if more than one method
  ///  is decorated with this attribute the first method found will be executed (not
  ///  recommended!).
  SetupAttribute = class(TCustomAttribute)
  end;

  ///  A method on a test fixture class decorated with this attribute will be
  ///  run after each test method is run. Note that if more than one method
  ///  is decorated with this attribute the first method found will be executed (not
  ///  recommended!).
  TearDownAttribute = class(TCustomAttribute)
  end;

  ///  A method marked with this attribute can contain a teardown method that
  ///  will be run after each all tests in the fixture have executed.
  ///  Note that if more than one method is decorated with this attribute the
  //   first method found will be executed (not recommended!).
  TearDownFixtureAttribute = class(TCustomAttribute)
  end;


  //Marks a method as a test method
  TestAttribute = class(TCustomAttribute)
  private
    FEnabled : boolean;
  public
    constructor Create;overload;
    constructor Create(const AEnabled : boolean);overload;
    property Enabled : boolean read FEnabled;
  end;

  //Marks a test method to be repeated count times.
  //NOT IMPLEMENTED
  RepeatAttribute = class(TCustomAttribute)
  private
    FCount : Cardinal;
  public
    constructor Create(const ACount : Cardinal);
    property Count : Cardinal read FCount;
  end;

  TValueArray = array of TValue;

  ///  The TestCaseAttribute allows you to specify the name of a function that returns a
  ///  TValueArray which will be passed into a function that takes parameters. This is really only
  ///  needed to work around the problens with the TestCaseAttribute
  ///  Note that the types in the TConstArray should match the parameters of the method we are testing.
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

  TLogLevel = (ltInformation,ltWarning,ltError);

{$IFDEF DELPHI_XE2_UP}
  ///  This helper class is intended to redirect the writeln
  ///  method to a test runner so that it is logged correctly.
  //
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
    class procedure Fail(const message : string = '');
    class procedure Warn(const message : string = '');


    class procedure AreEqual(const left : string; const right : string; const ignoreCase : boolean = true; const message : string = '');overload;
    class procedure AreEqual(const left, right : Extended; const tolerance : Extended; const message : string = '');overload;
    class procedure AreEqual(const left, right : TClass; const message : string = '');overload;
    class procedure AreEqual<T>(const left, right : T; const message : string = '');overload;
    class procedure AreEqualMemory(const left : Pointer; const right : Pointer; const size : Cardinal; message : string = '');

    class procedure AreNotEqual(const left : string; const right : string; const ignoreCase : boolean = true; const message : string = '');overload;
    class procedure AreNotEqual(const left, right : Extended; const tolerance : Extended; const message : string = '');overload;
    class procedure AreNotEqual(const left, right : TClass; const message : string = '');overload;
    class procedure AreNotEqual<T>(const left, right : T; const message : string = '');overload;
    class procedure AreNotEqualMemory(const left : Pointer; const right : Pointer; const size : Cardinal; message : string = '');

    class procedure AreSame(const left, right : TObject; const message : string = '');overload;
    class procedure AreSame(const left, right : IInterface; const message : string = '');overload;

    class procedure AreNotSame(const left, right : TObject; const message : string = '');overload;
    class procedure AreNotSame(const left, right : IInterface; const message : string = '');overload;


    class procedure Contains<T>(const list : IEnumerable<T>; const value : T; const message : string = '');overload;
    class procedure DoesNotContain<T>(const list : IEnumerable<T>; const value : T; const message : string = '');overload;

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
    class procedure IsEmpty<T>(const value : IEnumerable<T>; const message : string = '');overload;

    class procedure IsNotEmpty(const value : string; const message : string = '');overload;
    class procedure IsNotEmpty(const value : Variant; const message : string = '');overload;
    class procedure IsNotEmpty(const value : TStrings; const message : string = '');overload;
    class procedure IsNotEmpty(const value : TList; const message : string = '');overload;
    class procedure IsNotEmpty(const value : IInterfaceList; const message : string = '');overload;
    class procedure IsNotEmpty<T>(const value : IEnumerable<T>; const message : string = '');overload;

    class procedure WillRaise(const AMethod : TTestMethod; const exceptionClass : ExceptClass = nil; const msg : string = '');
    class procedure WillNotRaise(const AMethod : TTestMethod; const exceptionClass : ExceptClass = nil; const msg : string = '');

    class procedure Contains(const theString : string; const subString : string; const ignoreCase : boolean = true; const message : string = '');overload;
    class procedure StartsWith(const theString : string; const subString : string;const ignoreCase : boolean = true; const message : string = '');
    class procedure EndsWith(const theString : string; const subString : string;const ignoreCase : boolean = true; const message : string = '');
    class procedure InheritsFrom(const descendant : TClass; const parent : TClass; const message : string = '');
    class procedure IsType<T>(const value : T; const message : string = '');overload;
    {$IFDEF SUPPORTS_REGEX}
    class procedure IsMatch(const regexPattern : string; const theString : string; const message : string = '');
    {$ENDIF}
  end;

  ITestFixtureInfo = interface;

  ITestInfo = interface
    ['{FF61A6EB-A76B-4BE7-887A-598EBBAE5611}']
    function GetName : string;
    function GetTestFixture : ITestFixtureInfo;
    function GetTestStartTime : TDateTime;
    function GetTestEndTime : TDateTime;
    function GetTestDuration : TTimeSpan;
    property Name : string read GetName;
    property Fixture : ITestFixtureInfo read GetTestFixture;
    property TestStartTime : TDateTime read GetTestStartTime;
    property TestEndTime : TDateTime read GetTestEndTime;
    property TestDuration : TTimeSpan read GetTestDuration;

  end;



  ITestFixtureInfo = interface
    ['{9E98B1E8-583A-49FC-B409-9B6937E22E81}']
    function GetName  : string;
    function GetTests : IEnumerable<ITestInfo>;
    function GetTestClass : TClass;
    function GetSetupMethodName : string;
    function GetSetupFixtureMethodName : string;
    function GetTearDownMethodName : string;
    function GetTearDownFixtureMethodName : string;
    function GetTestInOwnThread : boolean;

    property Name                       : string read GetName;
    property TestClass                  : TClass read GetTestClass;
    property Tests                      : IEnumerable<ITestInfo> read GetTests;
    property SetupMethodName            : string read GetSetupMethodName;
    property SetupFixtureMethodName     : string read GetSetupFixtureMethodName;
    property TearDownMethodName         : string read GetTearDownMethodName;
    property TearDownFixtureMethodName  : string read GetTearDownFixtureMethodName;
    property TestInOwnThread            : boolean read GetTestInOwnThread;
  end;





  TTestResultType = (Success,Failure,Warning,Error);
  ITestResult = interface
  ['{EFD44ABA-4F3E-435C-B8FC-1F8EB4B35A3B}']
    function GetTest : ITestInfo;
    function GetResult : boolean;
    function GetResultType : TTestResultType;
    function GetMessage : string;
    property Test : ITestInfo read GetTest;
    property Result : boolean read GetResult;
    property ResultType : TTestResultType read GetResultType;
    property Message : string read GetMessage;
  end;

  ITestError = interface(ITestResult)
    function GetExceptionClass : ExceptClass;
    property ExceptionClass : ExceptClass read GetExceptionClass;
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


  ITestResults = interface
  ['{4A335B76-33E3-48FD-87DF-9462428C60DA}']
    function GetCount : integer;
    function GetAllPassed : boolean;
    function GetFailureCount : integer;
    function GetErrorCount : integer;
    function GetWarningCount : integer;
    function GetSuccessCount : integer;

    function GetFixtures : IEnumerable<ITestFixtureInfo>;
    function GetResults  : IEnumerable<ITestResult>;

    property Count : integer read GetCount;

    property AllPassed : boolean read GetAllPassed;
  end;

  ITestLogger = interface
    ['{AADCA392-421C-4060-8D47-79D7CAAB0EEF}']
    //Called at the start of testing. The default console logger prints the DUnitX banner.
    procedure OnTestingStarts(const threadId : Cardinal);

    //Called before a Fixture is run.
    procedure OnStartTestFixture(const threadId : Cardinal; const fixture : ITestFixtureInfo);

    //Called before a fixture Setup method is run.
    procedure OnSetupFixture(const threadId : Cardinal; const fixture : ITestFixtureInfo);
    //Called afgter a ficture setup method is run.
    procedure OnEndSetupFixture(const threadId : Cardinal; const fixture : ITestFixtureInfo);

    //called before a Test method is run.
    procedure OnBeginTest(const threadId : Cardinal; Test: ITestInfo);

    //called before a test setup method is run.
    procedure OnSetupTest(const threadId : Cardinal; Test: ITestInfo);
    //called after a test setup method is run.
    procedure OnEndSetupTest(const threadId : Cardinal; Test: ITestInfo);

    //called before a Test method is run.
    procedure OnExecuteTest(const threadId : Cardinal; Test: ITestInfo);


    //called when a test succeeds.
    procedure OnTestSuccess(const threadId : Cardinal; Test: ITestResult);
    //called when a test errors.
    procedure OnTestError(const threadId : Cardinal; Error: ITestError);
    //called when a test fails.
    procedure OnTestFailure(const threadId : Cardinal; Failure: ITestResult);
    //called when a test results in a warning.
    procedure OnTestWarning(const threadId : Cardinal; AWarning: ITestResult);

    //allows tests to write to the log.
    procedure OnLog(const logType : TLogLevel; const msg : string);

    //called before a Test Teardown method is run.
    procedure OnTeardownTest(const threadId : Cardinal; Test: ITestInfo);
    //called after a test teardown method is run.
    procedure OnEndTeardownTest(const threadId : Cardinal; Test: ITestInfo);
    //called after a test method and teardown is run.
    procedure OnEndTest(const threadId : Cardinal; Test: ITestResult);

    //called before a Fixture Teardown method is called.
    procedure OnTearDownFixture(const threadId : Cardinal; const fixture : ITestFixtureInfo);
    //called after a Fixture Teardown method is called.
    procedure OnEndTearDownFixture(const threadId : Cardinal; const fixture : ITestFixtureInfo);

    //called after a Fixture has run.
    procedure OnEndTestFixture(const threadId : Cardinal; const results : IFixtureResult);

    //called after all fixtures have run.
    procedure OnTestingEnds(const TestResult: ITestResults);
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


    //When true, test fixtures will be found by using RTTI to search for classes decorated as TestFixtures
    //Note for this to work you may need to use {$STRONGLINKTYPES ON} otherwise the classes may not get
    //linked as they are not referenced. When False you will need to register the fixtures using
    //TDUnitX.RegisterTestFixture
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
    class procedure RegisterTestFixture(const AClass : TClass;const AName : string = '' );
    class function CommandLine : ICommandLine;
    class function CurrentRunner : ITestRunner;
  end;

  ETestFrameworkException = class(Exception);

  ENotImplemented = class(ETestFrameworkException);
  EAssertionFailure = class(ETestFrameworkException);

  //base exception for any internal exceptions which cause the test to stop
  EAbort = class(ETestFrameworkException);
  ETestFailure = class(EAbort);
  ETestPass = class(EAbort);
  ETestWarning = class(EABort);
  ENoTestsRegistered = class(ETestFrameworkException);


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


{ TestFixture }

constructor TestFixtureAttribute.Create(const AName: string);
begin
  FName := AName;
  FRunAllMethods := False;
end;

constructor TestFixtureAttribute.Create;
begin
  FRunAllMethods := False;
end;

constructor TestFixtureAttribute.Create(const ARunAllMethods: boolean);
begin
  FRunAllMethods := ARunAllMethods;
end;

constructor TestFixtureAttribute.Create(const AName: string; const ARunAllMethods: boolean);
begin
  FName := AName;
  FRunAllMethods := ARunAllMethods;
end;

{ Assert }


class procedure Assert.AreEqual(const left, right, tolerance: Extended; const message: string);
begin
  if not Math.SameValue(left,right,tolerance) then
    raise EAssertionFailure.Create(Format('left %g but got %g - %s' ,[left,right,message]));
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
    raise EAssertionFailure.Create(msg);
  end;
end;

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
    raise EAssertionFailure.Create(Format('left %s but got %s',[leftValue.AsString,rightValue.AsString]));
  end;
end;

class procedure Assert.AreEqualMemory(const left : Pointer; const right : Pointer; const size : Cardinal; message : string);
begin
  if not CompareMem(left,right, size) then
    raise EAssertionFailure.Create('Memory values are not equal. ' + message);
end;

class procedure Assert.AreNotEqual(const left, right, tolerance: Extended; const message: string);
begin
  if Math.SameValue(left,right,tolerance) then
    raise EAssertionFailure.Create(Format('%g equals right %g %s' ,[left,right,message]));
end;


class procedure Assert.AreNotEqual(const left, right: string;const ignoreCase: boolean; const message: string);
begin
  if ignoreCase then
  begin
    if SameText(left,right) then
      raise EAssertionFailure.Create(Format('[%s] is Equal to [%s] %s',[left,right,message]));
  end
  else if SameStr(left,right) then
      raise EAssertionFailure.Create(Format('[%s] is Equal to [%s] %s',[left,right,message]));
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
    raise EAssertionFailure.Create(msg);
  end;

end;

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
    raise EAssertionFailure.Create(Format('left %s Not Equal To %s',[leftValue.AsString,rightValue.AsString]));
  end;
end;

class procedure Assert.AreNotEqualMemory(const left, right: Pointer; const size: Cardinal; message: string);
begin
  if CompareMem(left,right, size) then
    raise EAssertionFailure.Create('Memory values are equal. ' + message);
end;

class procedure Assert.AreNotSame(const left, right: TObject; const message: string);
begin
  if left.Equals(right) then
    raise EAssertionFailure.Create(Format('Object [%s] Equals Object [%s] %s',[left.ToString,right.ToString,message]));
end;

class procedure Assert.AreNotSame(const left, right: IInterface; const message: string);
begin
  if left = right then
    raise EAssertionFailure.Create(Format('references are the same. %s',[message]));
end;

class procedure Assert.AreSame(const left, right: IInterface; const message: string);
begin
  if left <> right then
    raise EAssertionFailure.Create(Format('references are Not the same. %s',[message]));
end;

class procedure Assert.AreSame(const left, right: TObject; const message: string);
begin
  if not left.Equals(right) then
    raise EAssertionFailure.Create(Format('Object [%s] Not Object [%s] %s',[left.ToString,right.ToString,message]));
end;


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
  raise EAssertionFailure.Create(Format('List does not contain value. %s',[message]));
end;



class procedure Assert.DoesNotContain<T>(const list: IEnumerable<T>; const value: T; const message: string);
var
  o : T;
  comparer : IComparer<T>;
begin
  comparer := TComparer<T>.Default;
  for o in list do
  begin
    if comparer.Compare(o,value) = 0 then
      raise EAssertionFailure.Create(Format('List contains value. %s',[message]));
  end;
end;

class procedure Assert.Fail(const message: string);
begin
  raise ETestFailure.Create(Format('Fail. %s',[message]));
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
    raise EAssertionFailure.Create(msg);
  end;

end;

class procedure Assert.IsEmpty(const value: IInterfaceList; const message: string);
begin
  if value.Count > 0 then
    raise EAssertionFailure.Create(Format('List is Not empty. %s',[message]));
end;

class procedure Assert.IsEmpty(const value: TList; const message: string);
begin
  if value.Count > 0 then
    raise EAssertionFailure.Create(Format('List is Not empty. %s',[message]));
end;

class procedure Assert.IsEmpty(const value, message: string);
begin
  if Length(value) > 0 then
    raise EAssertionFailure.Create(Format('String is Not empty. %s',[message]));
end;

class procedure Assert.IsEmpty(const value: Variant; const message: string);
begin
  if VarIsEmpty(value) or VarIsNull(value) then
    raise EAssertionFailure.Create(Format('Variant is Not empty. %s',[message]));
end;

class procedure Assert.IsEmpty(const value: TStrings; const message: string);
begin
  if value.Count > 0 then
    raise EAssertionFailure.Create(Format('List is Not empty. %s',[message]));
end;


class procedure Assert.IsEmpty<T>(const value: IEnumerable<T>; const message: string);
var
  o : T;
  count : integer;
begin
  count := 0;
  for o in value do
    Inc(count);
  if count > 0 then
   raise EAssertionFailure.Create(Format('List is Not empty. %s',[message]));
end;

class procedure Assert.IsFalse(const condition: boolean; const message: string);
begin
  if condition then
   raise EAssertionFailure.Create(Format('Condition is True when False expected. %s',[message]));
end;

class procedure Assert.IsNotEmpty(const value: TList; const message: string);
begin
  if value.Count = 0 then
   raise EAssertionFailure.Create(Format('List is Empty. %s',[message]));
end;

class procedure Assert.IsNotEmpty(const value: IInterfaceList; const message: string);
begin
  if value.Count = 0 then
   raise EAssertionFailure.Create(Format('List is Empty. %s',[message]));
end;

class procedure Assert.IsNotEmpty(const value: TStrings; const message: string);
begin
  if value.Count = 0 then
   raise EAssertionFailure.Create(Format('List is Empty. %s',[message]));
end;

class procedure Assert.IsNotEmpty(const value, message: string);
begin
  if value = '' then
   raise EAssertionFailure.Create(Format('Variant is Empty. %s',[message]));
end;

class procedure Assert.IsNotEmpty(const value: Variant; const message: string);
begin
  if VarIsEmpty(value) then
    raise EAssertionFailure.Create(Format('Variant is Empty. %s',[message]));
end;


class procedure Assert.IsNotEmpty<T>(const value: IEnumerable<T>; const message: string);
var
  x : T;
  count : integer;
begin
  count := 0;
  for x in value do
    Inc(count);
  if count = 0 then
    raise EAssertionFailure.Create(Format('List is Empty when Not empty expected. %s',[message]));
end;

class procedure Assert.IsNotNull(const condition: IInterface; const message: string);
begin
  if condition = nil then
    raise EAssertionFailure.Create(Format('Interface is Nil when not nil expected. %s',[message]));
end;

class procedure Assert.IsNotNull(const condition: Pointer; const message: string);
begin
  if condition = nil then
    raise EAssertionFailure.Create(Format('Pointer is Nil when not Nil expected. %s',[message]));
end;

class procedure Assert.IsNotNull(const condition: TObject; const message: string);
begin
  if condition = nil then
    raise EAssertionFailure.Create(Format('Object is Nil when Not Nil expected. %s',[message]));
end;

class procedure Assert.IsNotNull(const condition: Variant; const message: string);
begin
  if VarIsNull(condition) then
    raise EAssertionFailure.Create(Format('Variant is Null when Not Null expcted. %s',[message]));
end;

class procedure Assert.IsNull(const condition: Variant; const message: string);
begin
  if not VarIsNull(condition) then
    raise EAssertionFailure.Create(Format('Variant is Not Null when Null expected. [%s]',[message]));
end;

class procedure Assert.IsNull(const condition: IInterface; const message: string);
begin
  if condition <> nil then
    raise EAssertionFailure.Create(Format('Interface is not Nil when nil expected. [%s]',[message]));
end;

class procedure Assert.IsNull(const condition: TObject; const message: string);
begin
  if condition <> nil then
    raise EAssertionFailure.Create(Format('Object is not nil when nil expected. [%s]',[message]));
end;

class procedure Assert.IsNull(const condition: Pointer; const message: string);
begin
  if condition <> nil then
    raise EAssertionFailure.Create(Format('Pointer is not Nil when nil expected. [%s]',[message]));
end;

class procedure Assert.IsTrue(const condition: boolean;const message : string);
begin
  if not condition then
    raise EAssertionFailure.Create(Format('Condition is False when True expected. [%s]',[message]));
end;


class procedure Assert.IsType<T>(const value: T; const message : string);
var
  val : TValue;
begin
  begin
    val := TValue.From<T>(value);
    if not val.IsType<T> then
      raise EAssertionFailure.Create('value is not of type T');
  end;
end;

class procedure Assert.WillRaise(const AMethod : TTestMethod; const exceptionClass : ExceptClass; const msg : string);
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
          raise EAssertionFailure.Create('Method did not throw exception of type : ' + exceptionClass.ClassName + GetMsg );
      end
      else
        exit;
    end;
  end;
  raise EAssertionFailure.Create('Method did not throw any excpetions.' + GetMsg);
end;

class procedure Assert.Warn(const message: string);
begin
  raise ETestWarning.Create(Format('Warning. %s',[message]));
end;

class procedure Assert.WillNotRaise(const AMethod : TTestMethod; const exceptionClass : ExceptClass; const msg : string);
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
          raise EAssertionFailure.Create('Method raised an exception of type : ' + exceptionClass.ClassName + #13#10 + e.Message + GetMsg);
      end
      else
        raise EAssertionFailure.Create('Method raised and exception of type : ' + e.ClassName + #13#10 + e.Message + GetMsg);
    end;
  end;
end;

class procedure Assert.AreEqual(const left, right : string;  const ignoreCase : boolean; const message: string);
begin
  if ignoreCase then
  begin
    if not SameText(left,right) then
      raise EAssertionFailure.Create(Format('[%s] is Not Equal to [%s] %s',[left,right,message]));
  end
  else if not SameStr(left,right) then
      raise EAssertionFailure.Create(Format('[%s] is Not Equal to [%s] %s',[left,right,message]));
end;

class procedure Assert.Contains(const theString : string; const subString : string; const ignoreCase : boolean; const message : string);
begin
  if ignoreCase then
  begin
    if not StrUtils.ContainsText(theString,subString) then
      raise EAssertionFailure.Create(Format('[%s] does not contain [%s] %s',[theString,subString,message]));
  end
  else if not StrUtils.ContainsStr(theString,subString) then
    raise EAssertionFailure.Create(Format('[%s] does not contain [%s] %s',[theString,subString,message]));
end;

class procedure Assert.EndsWith(const theString : string; const subString : string; const ignoreCase : boolean; const message : string);
begin
  if ignoreCase then
  begin
    if not StrUtils.EndsText(theString,subString) then
      raise EAssertionFailure.Create(Format('[%s] does not end with [%s] %s',[theString,subString,message]));
  end
  else if not StrUtils.EndsStr(theString,subString) then
    raise EAssertionFailure.Create(Format('[%s] does not end with [%s] %s',[theString,subString,message]));
end;
{$IFDEF SUPPORTS_REGEX}
class procedure Assert.IsMatch(const regexPattern, theString, message: string);
begin
  if not TRegEx.IsMatch(theString,regexPattern) then
    raise EAssertionFailure.Create(Format('[%s] does not match [%s] %s',[theString,regexPattern,message]));
end;
{$ENDIF}

class procedure Assert.StartsWith(const theString : string; const subString : string; const ignoreCase : boolean; const message: string);
begin
  if ignoreCase then
  begin
    if not StrUtils.StartsText(theString,subString) then
      raise EAssertionFailure.Create(Format('[%s] does Not Start with [%s] %s',[theString,subString,message]));
  end
  else if not StrUtils.StartsStr(theString,subString) then
    raise EAssertionFailure.Create(Format('[%s] does Not Start with [%s] %s',[theString,subString,message]));
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

class procedure TDUnitX.RegisterTestFixture(const AClass: TClass;const AName : string);
var
  sName : string;
begin
  if AName <> '' then
    sName := AName
  else
    sName := AClass.ClassName;
  if not RegisteredFixtures.ContainsValue(AClass) then
    RegisteredFixtures.Add(sName,AClass);
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
