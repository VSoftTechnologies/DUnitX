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
  DUnitX.Types;

type
  /// <summary>
  ///   A class decorated with this attribute will be tested. The parameters
  ///   allow you to control which methods are treated as tests. By default 
  ///   only methods decorated with the Test attribute are run as tests.
  /// </summary>
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

  /// <summary>
  ///   A TestFixture decorated with this attribute will be tested using it's
  ///   own thread.  This can speed up unit testing when fixtures do not
  ///   compete for resources and the test machine has enough cores to service
  ///   the tests.
  /// </summary>
  /// <remarks>
  ///   NOTE - CURRENTLY PLANNED BUT NOT IMPLEMENTED!!!
  /// </remarks>
  TestInOwnThreadAttribute = class(TCustomAttribute)
  end;

  /// <summary>
  ///   A method marked with this attribute will run before any tests in.  Note
  ///   that if more than one method is decorated with this attribute the first
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
  ///   will be run after each all tests in the fixture have executed.  Note
  ///   that if more than one method is decorated with this attribute the first
  ///   method found will be executed (not recommended!).
  /// </summary>
  TearDownFixtureAttribute = class(TCustomAttribute)
  end;

  /// <summary>
  ///   This attribue is applied to test methods. If a test is successful and
  ///   produces a memory leak it will be reported.   If you do not want the
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
  ///	  Marks a test method to fail after the time specified.
  ///  Currently only support on Win32 & Win64
  ///	</summary>
  ///	<remarks>
  ///	  If [MaxTime(1000]] used then the test will fail if the
  ///   test takes longer than 1000ms
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
    constructor Create;overload;
    constructor Create(const AEnabled : boolean);overload;
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
  ///   This attribute will prevent a test from being run.   It will still show
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
    FExceptionInheritance: TExceptionInheritance;
  public
    constructor Create(AExpectedException : ExceptClass; const AInheritance : TExceptionInheritance = exExact);
    property ExpectedException : ExceptClass read FExpectedException;
    property ExceptionInheritance : TExceptionInheritance read FExceptionInheritance;
  end;

  /// <summary>
  ///   Internal Structure used for those implementing CustomTestCase or
  ///   CustomTestCaseSource descendants.
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
  ///   Base class for all Test Case Attributes.   
  /// </summary>
  /// <remarks>
  ///   Class is abstract and should never be, used to annotate a class as a
  ///   attribute.   Instead use a descendant, that implements the GetCaseInfo
  ///   method.
  /// </remarks>
  CustomTestCaseAttribute = class abstract(TCustomAttribute)
  protected
    function GetCaseInfo : TestCaseInfo;  virtual; abstract;
  public
    property CaseInfo : TestCaseInfo read GetCaseInfo;
  end;

  /// <summary>
  ///   Base class for all Test Case Source Attributes.   
  /// </summary>
  /// <remarks>
  ///   <para>
  ///     Class is abstract and should never be, used to annotate a class as
  ///     a attribute.   Instead use a descendant, that implements the
  ///     GetCaseInfoArray method.    
  ///   </para>
  ///   <para>
  ///     Note: If a method is annotated with a decendant of
  ///     TestCaseSourceAttribute and returns an empty TestCaseInfoArray,
  ///     then no test will be shown for the method.
  ///   </para>
  /// </remarks>
  CustomTestCaseSourceAttribute = class abstract(TCustomAttribute)
  protected
    function GetCaseInfoArray : TestCaseInfoArray; virtual; abstract;
  public
    property CaseInfoArray : TestCaseInfoArray read GetCaseInfoArray;
  end;

  /// <summary>
  ///   The TestCaseAttribute allows you to pass values to a test function.
  ///   Each value is delimited in the string, by default the delimiter is ','
  /// </summary>
  TestCaseAttribute = class(CustomTestCaseAttribute)
  protected
    FCaseInfo : TestCaseInfo;
    function GetCaseInfo : TestCaseInfo; Override;
    function GetName: String;
    function GetValues: TValueArray;
  public
    constructor Create(const ACaseName : string; const AValues : string;const ASeparator : string = ',');overload;
    property Name : String read GetName;
    property Values : TValueArray read GetValues;
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

constructor TestFixtureAttribute.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
end;

constructor TestFixtureAttribute.Create(const AName: string; const ADescription : string);
begin
  inherited Create;
  FName := AName;
  FDescription := ADescription;
end;

{ IgnoreMemoryLeaks }

constructor IgnoreMemoryLeaks.Create(const AIgnoreMemoryLeaks: Boolean);
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

constructor TestAttribute.Create(const AEnabled: boolean);
begin
  inherited Create;
  FEnabled := AEnabled;
end;

{ CategoryAttribute }

constructor CategoryAttribute.Create(const ACategory: string);
begin
  inherited Create;
  FCategory := ACategory;
end;

{ IgnoreAttribute }

constructor IgnoreAttribute.Create(const AReason: string);
begin
  inherited Create;
  FReason := AReason;
end;

{ RepeatTestAttribute }

constructor RepeatTestAttribute.Create(const ACount: Cardinal);
begin
  inherited Create;
  FCount := ACount;
end;

{ TestCaseAttribute }

constructor TestCaseAttribute.Create(const ACaseName: string; const AValues: string;const ASeparator : string);
var
  i: Integer;
  l : integer;
  lValues : TStringDynArray;
begin
  inherited Create;
  FCaseInfo.Name := ACaseName;
  lValues := SplitString(AValues,ASeparator);
  l := Length(lValues);
  SetLength(FCaseInfo.Values,l);
  for i := 0 to l -1 do
    FCaseInfo.Values[i] := TValue.From<string>(lValues[i]);
end;

function TestCaseAttribute.GetCaseInfo: TestCaseInfo;
begin
  Result := FCaseInfo;
end;

function TestCaseAttribute.GetName: String;
begin
  Result := FCaseInfo.Name;
end;

function TestCaseAttribute.GetValues: TValueArray;
begin
  Result := FCaseInfo.Values;
end;

{ MaxTimeAttribute }

constructor MaxTimeAttribute.Create(const AMaxTime : Cardinal);
begin
  FMaxTime := AMaxTime;
end;

{ WillRaiseAttribute }

constructor WillRaiseAttribute.Create(AExpectedException: ExceptClass; const AInheritance: TExceptionInheritance);
begin
  FExpectedException := AExpectedException;
  FExceptionInheritance := AInheritance;
end;

end.
