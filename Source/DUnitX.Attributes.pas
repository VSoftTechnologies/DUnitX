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

unit DUnitX.Attributes;

interface

{$I DUnitX.inc}

uses
{$IFDEF USE_NS}
  System.Rtti,
  System.SysUtils,
{$ELSE}
  Rtti,
  SysUtils,
{$ENDIF}
  DUnitX.Types,
  DUnitX.InternalDataProvider;

type
  /// <summary>
  ///   A class decorated with this attribute will be tested. The parameters
  ///   allow you to control which methods are treated as tests. By default�
  ///   only methods decorated with the Test attribute are run as tests.
  /// </summary>
  TestFixtureAttribute = class(TCustomAttribute)
  private
    FName : string;
    FDescription : string;
  public
    constructor Create; overload;
    constructor Create(const AName : string); overload;
    constructor Create(const AName : string; const ADescription : string); overload;
    property Name : string read FName;
    property Description : string read FDescription;
  end;

  /// <summary>
  ///   A TestFixture decorated with this attribute will be tested using it's
  ///   own thread. �This can speed up unit testing when fixtures do not
  ///   compete for resources and the test machine has enough cores to service
  ///   the tests.
  /// </summary>
  /// <remarks>
  ///   NOTE - CURRENTLY PLANNED BUT NOT IMPLEMENTED!!!
  /// </remarks>
  TestInOwnThreadAttribute = class(TCustomAttribute)
  end;

  /// <summary>
  ///   A method marked with this attribute will run before any tests in.� Note
  ///   that if more than one method is decorated with this attribute�the first
  ///   method found will be executed (not recommended!).
  /// </summary>
  SetupFixtureAttribute = class(TCustomAttribute)
  end;

  /// <summary>
  ///   A method on a test fixture decorated with this attribute will run
  ///   before each test method is run. Note that if more than one method is
  ///   decorated with this attribute the first method found will be executed
  ///   (not recommended!).
  /// </summary>
  SetupAttribute = class(TCustomAttribute)
  end;

  /// <summary>
  ///   A method on a test fixture class decorated with this attribute will be
  ///   run after each test method is run. Note that if more than one method is
  ///   decorated with this attribute the first method found will be executed
  ///   (not recommended!).
  /// </summary>
  TearDownAttribute = class(TCustomAttribute)
  end;

  /// <summary>
  ///   A method marked with this attribute can contain a teardown method that
  ///   will be run after each all tests in the fixture have executed.� Note
  ///   that if more than one method is decorated with this attribute the first
  ///   method found will be executed (not recommended!).
  /// </summary>
  TearDownFixtureAttribute = class(TCustomAttribute)
  end;

  /// <summary>
  ///   This attribue is applied to test methods. If a test is successful and
  ///   produces a memory leak it will be reported.�� If you do not want the
  ///   leak reported, then you can add this attribute to the test method.
  /// </summary>
  IgnoreMemoryLeaks = class(TCustomAttribute)
  private
    FIgnoreMemoryLeaks : Boolean;
  public
    constructor Create(const AIgnoreMemoryLeaks : Boolean = True);
    property IgnoreLeaks : Boolean read FIgnoreMemoryLeaks;
  end;

  ///	<summary>
  ///	  Marks a test method to fail if it takes longer than the time specified.
  ///	</summary>
  ///	<remarks>
  ///	  If [MaxTime(1000)] used then the test will fail if the
  ///   test takes longer than 1000ms.  The time is checked once the test
  ///   returns: the test is not interrupted, so one that never returns is
  ///   not stopped.
  ///	</remarks>
  MaxTimeAttribute = class(TCustomAttribute)
  private
    FMaxTime : Cardinal;
  public
    constructor Create(const AMaxTime : Cardinal);
    property MaxTime : Cardinal read FMaxTime;
  end;

  /// <summary>
  ///   This attribute marks a method as a test method
  /// </summary>
  TestAttribute = class(TCustomAttribute)
  private
    FEnabled : boolean;
  public
    constructor Create; overload;
    constructor Create(const AEnabled : boolean); overload;
    property Enabled : boolean read FEnabled;
  end;

  /// <summary>
  ///   This attribute allows you to specify a test Category which can be used
  ///   when filtering the tests to run.
  /// </summary>
  CategoryAttribute = class(TCustomAttribute)
  private
    FCategory : string;
  public
    constructor Create(const ACategory : string);
    property Category : string read FCategory;
  end;

  /// <summary>
  ///   This attribute will prevent a test from being run.�� It will still show
  ///   up in the lists of tests, and reported as an Ignored test
  /// </summary>
  /// <remarks>
  ///   This is useful when you need to temporarily stop a test from running.
  /// </remarks>
  IgnoreAttribute = class(TCustomAttribute)
  private
    FReason : string;
  public
    constructor Create(const AReason : string = '');
    property Reason : string read FReason;
  end;

  /// <summary>
  ///   Marks a test method to be repeated count times.
  /// </summary>
  /// <remarks>
  ///   If [RepeatTest(0)] used then the test will be skipped and behaves like
  ///   IgnoreAttribute
  /// </remarks>
  RepeatTestAttribute = class(TCustomAttribute)
  private
    FCount : Cardinal;
  public
    constructor Create(const ACount : Cardinal);
    property Count : Cardinal read FCount;
  end;

  /// <summary>
  ///   This attribute marks a method as a test method which will raise an exception.
  /// </summary>
  /// <remarks>
  ///   If [WillRaise(ERangeError)] is used then the test will fail if it
  ///   does not raise an ERangeError.
  /// </remarks>
  WillRaiseAttribute = class(TCustomAttribute)
  private
    FExpectedException : ExceptClass;
    FExceptionInheritance : TExceptionInheritance;
  public
    constructor Create(AExpectedException : ExceptClass; const AInheritance : TExceptionInheritance = exExact);
    property ExpectedException : ExceptClass read FExpectedException;
    property ExceptionInheritance : TExceptionInheritance read FExceptionInheritance;
  end;

  /// <summary>
  ///   Internal Structure used for those implementing CustomTestCase or
  ///   CustomTestCaseGenerator descendants.
  /// </summary>
  TestCaseInfo = record

    /// <summary>
    ///   Name of the Test Case
    /// </summary>
    Name : string;

    /// <summary>
    ///   Values that will be passed to the method being tested.
    /// </summary>
    Values : TValueArray;
  end;

  TestCaseInfoArray = TArray<TestCaseInfo>;

  /// <summary>
  ///   Base class for all Test Case Attributes.���
  /// </summary>
  /// <remarks>
  ///   Class is abstract and should never be, used to annotate a class as a
  ///   attribute.�� Instead use a descendant, that implements the GetCaseInfo
  ///   method.
  /// </remarks>
  CustomTestCaseAttribute = class abstract(TCustomAttribute)
  protected
    function GetCaseInfo : TestCaseInfo; virtual; abstract;
  public
    property CaseInfo : TestCaseInfo read GetCaseInfo;
  end;

  /// <summary>
  ///   Base class for all Test Case Generator Attributes.���
  /// </summary>
  /// <remarks>
  ///   <para>
  ///     Class is abstract and should never be, used to annotate a class as
  ///     a attribute.�� Instead use a descendant, that implements the
  ///     GetCaseInfoArray method.����
  ///   </para>
  ///   <para>
  ///     Note: If a method is annotated with a descendant of
  ///     CustomTestCaseGeneratorAttribute and returns an empty
  ///     TestCaseInfoArray, then no test will be shown for the method.
  ///   </para>
  ///   <para>
  ///     This was CustomTestCaseSourceAttribute, which is kept as a
  ///     deprecated alias.
  ///   </para>
  /// </remarks>
  CustomTestCaseGeneratorAttribute = class abstract(TCustomAttribute)
  protected
    function GetCaseInfoArray : TestCaseInfoArray; virtual; abstract;
  public
    property CaseInfoArray : TestCaseInfoArray read GetCaseInfoArray;
  end;

  /// <summary>
  ///   The TestCaseAttribute allows you to pass values to a test function.
  ///   Each value is delimited in the string, by default the delimiter is ','
  ///   The case name is generated by concatenating the test method name with (values),
  ///   as MethodName(1,2).  Use NamedTestCaseAttribute to give a case a name of its own.
  /// </summary>
  /// <remarks>
  ///   A separator that contains a letter or digit errors the case when it runs: it is
  ///   almost certainly a [TestCase('Name', 'values')] written when TestCase took a name
  ///   first, which still compiles, as the values 'Name' split at the characters of 'values'.
  /// </remarks>
  TestCaseAttribute = class(CustomTestCaseAttribute)
  protected
    FCaseInfo : TestCaseInfo;
    FValues : string;
    FValuesError : string;
    function GetCaseInfo : TestCaseInfo; override;
    function GetName : string;
    function GetValues : TValueArray;
    function GetValuesText : string;
    constructor InternalCreate(const ACaseName : string; const AValues : string; const ASeparator : string; const ATrimValues : boolean);
  public
    constructor Create(const AValues : string; const ASeparator : string = ','; const ATrimValues : boolean = false);
    property Name : string read GetName;
    property Values : TValueArray read GetValues;
    property ValuesText : string read GetValuesText;
    //Set when the values cannot be what was meant.  The case reports it as an error when it runs: raising from an
    //attribute's constructor would stop every attribute of the fixture being read.
    property ValuesError : string read FValuesError;
  end;

  /// <summary>
  ///   The NamedTestCaseAttribute allows you to pass values to a test function,
  ///   as TestCaseAttribute does, under a name of your choosing.
  ///   Each value is delimited in the string, by default the delimiter is ','
  ///   This is what TestCaseAttribute did before it named its cases from their values.
  /// </summary>
  NamedTestCaseAttribute = class(TestCaseAttribute)
  public
    constructor Create(const ACaseName : string; const AValues : string; const ASeparator : string = ','; const ATrimValues : boolean = false);
  end;

  /// <summary>
  ///   Deprecated: TestCaseAttribute now names its cases from their values, as this did.
  /// </summary>
  AutoNameTestCaseAttribute = TestCaseAttribute deprecated 'Use TestCase, which now names its cases from their values';

  /// <summary>
  ///   Deprecated: renamed CustomTestCaseGeneratorAttribute, to keep it apart from TestCaseSourceAttribute.
  /// </summary>
  CustomTestCaseSourceAttribute = CustomTestCaseGeneratorAttribute deprecated 'Use CustomTestCaseGeneratorAttribute';

  /// <summary>
  /// TestCaseProvider Attribute allows you, to pass a registered
  /// Class to the Test, that provides the test function width the
  /// needed data.
  /// </summary>
  TestCaseProviderAttribute = class(TCustomAttribute)
  protected
    FName : string;
    FClass : TTestDataProviderClass;
  public
    constructor Create(const providerName : string); overload;
    constructor Create(const AClass : TTestDataProviderClass); overload;
    property ProviderName : string read Fname;
    property ProviderClass : TTestDataProviderClass read FClass;
  end;

  /// <summary>
  ///   TestCaseSource Attribute names a class function that supplies the
  ///   cases for the test: of the fixture class, or of another class.
  ///   The function takes no parameters and returns an array with one
  ///   element per case: an array of the case's values (TArray&lt;TArray&lt;Integer&gt;&gt;,
  ///   or TArray&lt;TArray&lt;TValue&gt;&gt; for values of different types), or, for a
  ///   method with one parameter, the value itself (TArray&lt;string&gt;).
  ///   Each case is named from its values, as MethodName(1, 2, 3).
  ///   A source that is missing, cannot be called, raises, or returns no
  ///   cases errors the test.
  /// </summary>
  TestCaseSourceAttribute = class(TCustomAttribute)
  private
    FSourceMethodName : string;
    FSourceClass : TClass;
  public
    constructor Create(const ASourceMethodName : string); overload;
    constructor Create(const ASourceClass : TClass; const ASourceMethodName : string); overload;
    property SourceMethodName : string read FSourceMethodName;
    //nil for a method of the fixture class
    property SourceClass : TClass read FSourceClass;
  end;

implementation

uses
{$IFDEF USE_NS}
  System.Types,
  System.StrUtils,
{$ELSE}
  Types,
  StrUtils,
{$ENDIF}
  DUnitX.Utils;

{ TestFixture }

constructor TestFixtureAttribute.Create;
begin
  inherited;
end;

constructor TestFixtureAttribute.Create(const AName : string);
begin
  inherited Create;
  FName := AName;
end;

constructor TestFixtureAttribute.Create(const AName : string; const ADescription : string);
begin
  inherited Create;
  FName := AName;
  FDescription := ADescription;
end;

{ IgnoreMemoryLeaks }

constructor IgnoreMemoryLeaks.Create(const AIgnoreMemoryLeaks : Boolean);
begin
  inherited Create;
  FIgnoreMemoryLeaks := AIgnoreMemoryLeaks;
end;

{ TestAttribute }

constructor TestAttribute.Create;
begin
  inherited;
  FEnabled := True;
end;

constructor TestAttribute.Create(const AEnabled : boolean);
begin
  inherited Create;
  FEnabled := AEnabled;
end;

{ CategoryAttribute }

constructor CategoryAttribute.Create(const ACategory : string);
begin
  inherited Create;
  FCategory := ACategory;
end;

{ IgnoreAttribute }

constructor IgnoreAttribute.Create(const AReason : string);
begin
  inherited Create;
  FReason := AReason;
end;

{ RepeatTestAttribute }

constructor RepeatTestAttribute.Create(const ACount : Cardinal);
begin
  inherited Create;
  FCount := ACount;
end;

{ TestCaseAttribute }

constructor TestCaseAttribute.InternalCreate(const ACaseName : string; const AValues : string; const ASeparator : string; const ATrimValues : boolean);
var
  i : Integer;
  l : integer;
  lValues : TStringDynArray;
begin
  inherited Create;
  FCaseInfo.Name := ACaseName;
  FValues := AValues;
  lValues := SplitString(AValues, ASeparator);
  l := Length(lValues);
  SetLength(FCaseInfo.Values, l);
  for i := 0 to l - 1 do
  begin
    if ATrimValues then
      FCaseInfo.Values[i] := TValue.From<string>(Trim(lValues[i]))
    else
      FCaseInfo.Values[i] := TValue.From<string>(lValues[i]);
  end;
end;

function TestCaseAttribute.GetCaseInfo : TestCaseInfo;
begin
  Result := FCaseInfo;
end;

function TestCaseAttribute.GetName : string;
begin
  Result := FCaseInfo.Name;
end;

function TestCaseAttribute.GetValues : TValueArray;
begin
  Result := FCaseInfo.Values;
end;

function TestCaseAttribute.GetValuesText : string;
begin
  result := FValues;
end;

constructor TestCaseAttribute.Create(const AValues : string; const ASeparator : string; const ATrimValues : boolean);
var
  c : Char;
begin
  InternalCreate('', AValues, ASeparator, ATrimValues);
  //Until TestCase named its cases from their values, it took a name first: an old [TestCase('Name', 'values')] still
  //compiles, as the values 'Name' split at every character of 'values'.  A real separator has no letter or digit in it.
  for c in ASeparator do
    if CharInSet(c, ['0'..'9', 'A'..'Z', 'a'..'z']) then
    begin
      FValuesError := Format('TestCase separator ''%s'' contains a letter or digit: this looks like [TestCase(''%s'', ''%s'')] ' +
        'written for the old TestCase, which is now [NamedTestCase(''%s'', ''%s'')]', [ASeparator, AValues, ASeparator, AValues, ASeparator]);
      break;
    end;
end;

{ MaxTimeAttribute }

constructor MaxTimeAttribute.Create(const AMaxTime : Cardinal);
begin
  inherited Create;
  FMaxTime := AMaxTime;
end;

{ WillRaiseAttribute }

constructor WillRaiseAttribute.Create(AExpectedException : ExceptClass; const AInheritance : TExceptionInheritance);
begin
  inherited Create;
  FExpectedException := AExpectedException;
  FExceptionInheritance := AInheritance;
end;

{ TestCaseProviderAttribute }

constructor TestCaseProviderAttribute.Create(const providerName : string);
begin
  inherited Create;
  FName := ProviderName;
  FClass := nil;
end;

constructor TestCaseProviderAttribute.Create(const AClass : TTestDataProviderClass);
begin
  inherited Create;
  FName := '';
  FClass := AClass;
end;

{ NamedTestCaseAttribute }

constructor NamedTestCaseAttribute.Create(const ACaseName : string; const AValues : string; const ASeparator : string; const ATrimValues : boolean);
begin
  inherited InternalCreate(ACaseName, AValues, ASeparator, ATrimValues);
end;

{ TestCaseSourceAttribute }

constructor TestCaseSourceAttribute.Create(const ASourceMethodName : string);
begin
  inherited Create;
  FSourceClass := nil;
  FSourceMethodName := ASourceMethodName;
end;

constructor TestCaseSourceAttribute.Create(const ASourceClass : TClass; const ASourceMethodName : string);
begin
  inherited Create;
  FSourceClass := ASourceClass;
  FSourceMethodName := ASourceMethodName;
end;

end.

