{***************************************************************************}
{                                                                           }
{           DUnitX                                                          }
{                                                                           }
{           Copyright (C) 2017 Vincent Parrett & Contributors               }
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

unit DUnitX.FixtureProvider;

interface

{$I DUnitX.inc}

uses
  System.Rtti,
  System.Generics.Collections,
  DUnitX.Extensibility,
  DUnitX.Types,
  DUnitX.TestDataProvider;

type
  TDUnitXFixtureProvider = class(TInterfacedObject, IFixtureProvider)
  private class var
      FRttiContext : TRttiContext;
  private
    FFixtureClasses : TDictionary<TClass, string>;

  protected
    function FormatTestName(const AName : string; const ATimes, ACount : Integer) : string;
    function FormatCaseName(const AName : string; Nr : integer) : string;
    function TryGetAttributeOfType<T : class>(const attributes : TArray<TCustomAttribute>; var attribute : T) : boolean;
    procedure RTTIDiscoverFixtureClasses;
    procedure GenerateTests(const context : IFixtureProviderContext; const fixture : ITestFixture);
    procedure Execute(const context : IFixtureProviderContext);
  public
    class constructor Create;
    class destructor Destroy;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  System.TypInfo,
  System.Classes,
  System.Types,
  System.StrUtils,
  System.SysUtils,
  DUnitX.Attributes,
  DUnitX.Test,
  DUnitX.Utils,
  DUnitX.TestFramework,
  DUnitX.ResStrs,
  DUnitX.InternalInterfaces,
  DUnitX.InternalDataProvider,
  DUnitX.ServiceLocator;

//A [TestCase] value that contains the separator is split at it, which gives the case more values than parameters
function TooManyValuesMessage(const AValueCount, AParameterCount : Integer; const ASplit : boolean = true) : string;
const
  PLURAL : array[Boolean] of string = ('s', '');
begin
  Result := Format('Too many values: %d value%s for %d parameter%s',
    [AValueCount, PLURAL[AValueCount = 1], AParameterCount, PLURAL[AParameterCount = 1]]);
  if ASplit then
    Result := Result + ' (does a value contain the separator?)';
end;

const
  PLURAL_S : array[Boolean] of string = ('s', '');

//The case reports the error when it runs, so only that case errors
procedure SetArgsError(const ATest : ITest; const AMessage : string);
begin
  if (ATest as TObject) is TDUnitXTestCase then
    TDUnitXTestCase(ATest as TObject).ArgsError := AMessage;
end;

//Data from a provider or a generator is written for the method, so a value it has no parameter for is a mistake
procedure CheckNotTooManyValues(const ATest : ITest; const AValues : TValueArray; const AMethod : TRttiMethod);
begin
  if Length(AValues) > Length(AMethod.GetParameters()) then
    SetArgsError(ATest, TooManyValuesMessage(Length(AValues), Length(AMethod.GetParameters()), false));
end;

//A TArray<TValue> holds its values as TValues that hold TValues
function Unwrapped(const AValue : TValue) : TValue;
begin
  Result := AValue;
  while Result.TypeInfo = TypeInfo(TValue) do
    Result := Result.AsType<TValue>;
end;

//The values of one case from a [TestCaseSource]: an array is the case's values, anything else its one value.  For a
//method whose one parameter is an array, the array is that value.
function CaseValues(const AValue : TValue; const AOneArrayParameter : boolean) : TValueArray;
var
  value : TValue;
  i : Integer;
begin
  value := Unwrapped(AValue);
  if value.IsArray and not AOneArrayParameter then
    Result := value.ToArray
  else
  begin
    SetLength(Result, 1);
    Result[0] := value;
  end;
  for i := 0 to High(Result) do
    Result[i] := Unwrapped(Result[i]);
end;

//The name of a case from a [TestCaseSource]: its values, as (1, 2, 3)
function FormatValues(const AValues : TValueArray) : string;
var
  i : Integer;
begin
  Result := '(';
  for i := 0 to High(AValues) do
  begin
    if i > 0 then
      Result := Result + ', ';
    Result := Result + AValues[i].ToString;
  end;
  Result := Result + ')';
end;

{ TDUnitXFixtureProvider }

constructor TDUnitXFixtureProvider.Create;
begin
  FFixtureClasses := TDictionary<TClass, string>.Create;
end;

class constructor TDUnitXFixtureProvider.Create;
begin
  FRttiContext := TRttiContext.Create;
end;

destructor TDUnitXFixtureProvider.Destroy;
begin
  FFixtureClasses.Free;
  inherited;
end;

class destructor TDUnitXFixtureProvider.Destroy;
begin
  FRttiContext.Free;
end;

procedure TDUnitXFixtureProvider.Execute(const context : IFixtureProviderContext);
var
  pair : TPair<TClass, string>;
  fixture : ITestFixture;
  parentFixture : ITestFixture;
  uName : string;
  namespaces : TArray<string>;
  fixtureNamespaces : TArray<string>;
  namespace : string;
  parentNamespace : string;
  fixtureNamespace : string;
  fixtureName : string;
  tmpFixtures : TDictionary<string, ITestFixture>;
  fixtureList : ITestFixtureList;
  rType : TRttiType;
  categoryAttrib : CategoryAttribute;
  category : string;
begin
  if context.UseRtti then
    RTTIDiscoverFixtureClasses;

  for pair in TDUnitX.RegisteredFixtures do
  begin
    if not FFixtureClasses.ContainsKey(pair.Key) then
      FFixtureClasses.AddOrSetValue(pair.Key, pair.Value);
  end;

  //Build up a fixture hierarchy based on unit names.
  tmpFixtures := TDictionary<string, ITestFixture>.Create;
  fixtureList := TTestFixtureList.Create;
  try
    for pair in FFixtureClasses do
    begin
      rType := FRttiContext.GetType(pair.Key);
      if rType.TryGetAttributeOfType<CategoryAttribute>(categoryAttrib) then
        category := categoryAttrib.Category
      else
        category := '';

      uName := pair.Key.UnitName;

      namespaces := TStrUtils.SplitString(uName, '.');
      //check if the fixture name has namespaces (possible via testfixtureattribute)
      fixtureNamespaces := TStrUtils.SplitString(pair.Value, '.');
      if length(fixtureNamespaces) > 1 then
      begin
        fixtureName := fixtureNamespaces[Length(fixtureNamespaces) - 1];
        TArrayHelper.Delete<string>(fixtureNamespaces, Length(fixtureNamespaces) - 1, 1);

        namespaces := TArrayHelper.Concat<string>([namespaces, fixtureNamespaces]);
      end
      else
        fixtureName := pair.Value;

      //if the unit name has no namespaces the just add the tests.
      fixtureNamespace := '';
      parentNameSpace := '';

      parentFixture := nil;
      fixture := nil;

      for namespace in namespaces do
      begin
        if fixtureNamespace <> '' then
          fixtureNamespace := fixtureNamespace + '.' + namespace
        else
          fixtureNamespace := namespace;

        //first time through the loop it will be empty.
        if parentNamespace = '' then
        begin
          if not tmpFixtures.TryGetValue(fixtureNamespace, parentFixture) then
          begin
            parentFixture := context.CreateFixture(TObject, fixtureNamespace, ''); //<< Should this not take category??
            tmpFixtures.Add(fixtureNamespace, parentFixture);
            fixtureList.Add(parentFixture);
          end;
          parentNamespace := fixtureNamespace;
          continue;
        end
        else
        begin
          if not tmpFixtures.TryGetValue(parentNamespace, parentFixture) then
          begin
            parentFixture := context.CreateFixture(TObject, parentNamespace, '');
            tmpFixtures.Add(parentNamespace, parentFixture);
            fixtureList.Add(parentFixture);
          end;

          if not tmpFixtures.TryGetValue(fixtureNamespace, fixture) then
          begin
            fixture := parentFixture.AddChildFixture(TObject, fixtureNamespace, '');
            tmpFixtures.Add(fixtureNamespace, fixture);
          end;
          parentFixture := fixture;
          parentNamespace := fixtureNamespace;
        end;
      end;

      fixtureNamespace := fixtureNamespace + '.' + fixtureName;

      //per issue #253 - looking at the code above, parentFixture should always be assigned by the time we get here.
      System.Assert(Assigned(parentFixture));
      parentFixture.AddChildFixture(pair.Key, fixtureNamespace, category);
    end;
    for fixture in fixtureList do
    begin
      GenerateTests(context, fixture);
    end;

  finally
    fixtureList := nil;
    tmpFixtures.Free;
  end;
end;

function TDUnitXFixtureProvider.FormatCaseName(const AName : string;
  Nr : integer) : string;
begin
  result := Format('%s (%.2d)', [AName, nr]);
end;

function TDUnitXFixtureProvider.FormatTestName(const AName : string; const ATimes, ACount : Integer) : string;
begin
  Result := AName;

  if (ACount > 1) then
  begin
    Result := Result + Format('-%d-of-%d', [ATimes, ACount]);
  end;
end;

procedure TDUnitXFixtureProvider.GenerateTests(const context : IFixtureProviderContext; const fixture : ITestFixture);
var
  childFixture : ITestFixture;

  rType : TRttiType;
  methods : TArray<TRttiMethod>;
  method : TRttiMethod;
  meth : TMethod;

  tearDownFixtureIsDestructor : boolean;
  setupMethod : TTestMethod;
  tearDownMethod : TTestMethod;
  setupFixtureMethod : TTestMethod;
  tearDownFixtureMethod : TTestMethod;

  setupAttrib : SetupAttribute;
  setupFixtureAttrib : SetupFixtureAttribute;
  tearDownAttrib : TearDownAttribute;
  tearDownFixtureAttrib : TearDownFixtureAttribute;
  testAttrib : TestAttribute;
  categoryAttrib : CategoryAttribute;
  ignoredAttrib : IgnoreAttribute;
  willRaiseAttrib : WillRaiseAttribute;
  testCases : TArray<CustomTestCaseAttribute>;
  testCaseAttrib : CustomTestCaseAttribute;
  testCaseGenerators : TArray<CustomTestCaseGeneratorAttribute>;
  testCaseGeneratorAttrib : CustomTestCaseGeneratorAttribute;
  testCaseSources : TArray<TestCaseSourceAttribute>;
  testCaseSourceAttrib : TestCaseSourceAttribute;
  testCaseData : TestCaseInfo;
  testEnabled : boolean;
  isTestMethod : boolean;
  repeatAttrib : RepeatTestAttribute;
{$IFDEF MSWINDOWS}
  maxTimeAttrib : MaxTimeAttribute;
{$ENDIF}
  tstProviderAttribs : TArray<TestCaseProviderAttribute>;
  tstProviderAttrib : TestCaseProviderAttribute;
  iProvider : ITestDataProvider;
  count, x : integer;
  Params : TValueArray;
  caseName : string;
  test : ITest;

  category : string;
  ignoredTest : boolean;
  ignoredReason : string;
  maxTime : cardinal;
  willRaise : ExceptClass;
  willRaiseInherit : TExceptionInheritance;

  repeatCount : Cardinal;
  i : Integer;
  currentFixture : ITestFixture;
  addedTestCode : TArray<Pointer>;
  autoCaseNames : TArray<string>;

  //An override declared as a test of its own (in a published section, or with [Test] again) and the attributed method
  //it overrides both run the override, so the second is not added.
  function IsAddedTestCode(const code : Pointer) : boolean;
  var
    added : Pointer;
  begin
    result := false;
    for added in addedTestCode do
      if added = code then
        exit(true);
    SetLength(addedTestCode, Length(addedTestCode) + 1);
    addedTestCode[High(addedTestCode)] := code;
  end;

  //A case named from its values is numbered when the method already has a case of that name: [TestCase]s with the
  //same values would otherwise give tests with the same name.  The number depends only on the order of the cases.
  function UniqueCaseName(const AName : string) : string;
  var
    used : string;
    found : boolean;
    n : integer;
  begin
    result := AName;
    n := 1;
    repeat
      found := false;
      for used in autoCaseNames do
        if used = result then
        begin
          found := true;
          Inc(n);
          result := Format('%s #%d', [AName, n]);
          break;
        end;
    until not found;
    SetLength(autoCaseNames, Length(autoCaseNames) + 1);
    autoCaseNames[High(autoCaseNames)] := result;
  end;

  //Adds the cases a [TestCaseSource] supplies, or, when the source cannot be used, a test that errors saying why, so it
  //is not mistaken for a source with no cases.
  procedure AddSourceCases(const ASourceAttrib : TestCaseSourceAttribute);
  var
    sourceClass : TClass;
    sourceType : TRttiType;
    sourceMethod : TRttiMethod;
    sourceName : string;
    rows : TValue;
    problem : string;
    row : integer;
    values : TValueArray;
    name : string;
    n : Cardinal;
    caseTest : ITest;
    candidate : TRttiMethod;
    parameters : TArray<TRttiParameter>;
    oneArrayParameter : boolean;
  begin
    problem := '';
    sourceMethod := nil;
    sourceClass := ASourceAttrib.SourceClass;
    if sourceClass = nil then
      sourceClass := fixture.TestClass;
    sourceName := sourceClass.ClassName + '.' + ASourceAttrib.SourceMethodName;
    sourceType := FRttiContext.GetType(sourceClass);
    if sourceType <> nil then
      //Of overloads, the one without parameters
      for candidate in sourceType.GetMethods(ASourceAttrib.SourceMethodName) do
        if (sourceMethod = nil) or (Length(candidate.GetParameters) = 0) then
        begin
          sourceMethod := candidate;
          if Length(candidate.GetParameters) = 0 then
            break;
        end;
    if sourceMethod = nil then
      problem := Format('TestCaseSource method ''%s'' not found on %s (it must be public)', [ASourceAttrib.SourceMethodName, sourceClass.ClassName])
    else if (not sourceMethod.IsClassMethod) or (Length(sourceMethod.GetParameters) > 0) or (sourceMethod.ReturnType = nil) or
      not (sourceMethod.ReturnType.TypeKind in [tkArray, tkDynArray]) then
      problem := Format('TestCaseSource method %s must be a class function with no parameters that returns an array', [sourceName])
    else
    begin
      try
        rows := sourceMethod.Invoke(sourceClass, []);
      except
        //Reported as the test's error, which is where it belongs
        on e : Exception do
          problem := Format('TestCaseSource method %s raised %s: %s', [sourceName, e.ClassName, e.Message]);
      end;
      if (problem = '') and (rows.GetArrayLength = 0) then
        problem := Format('TestCaseSource method %s returned no cases', [sourceName]);
    end;

    if problem <> '' then
    begin
      //Named after the source, so two sources that cannot be used give two tests
      name := UniqueCaseName(ASourceAttrib.SourceMethodName);
      for n := 1 to repeatCount do
      begin
        caseTest := currentFixture.AddTestCase(method.Name, name, FormatTestName(method.Name, n, repeatCount), category, method, testEnabled, nil,
          willRaise, willRaiseInherit);
        SetArgsError(caseTest, problem);
      end;
      exit;
    end;

    parameters := method.GetParameters();
    oneArrayParameter := (Length(parameters) = 1) and Assigned(parameters[0].ParamType) and
      (parameters[0].ParamType.TypeKind in [tkArray, tkDynArray]);
    for row := 0 to rows.GetArrayLength - 1 do
    begin
      values := CaseValues(rows.GetArrayElement(row), oneArrayParameter);
      name := UniqueCaseName(FormatValues(values));
      for n := 1 to repeatCount do
      begin
        caseTest := currentFixture.AddTestCase(method.Name, name, FormatTestName(method.Name, n, repeatCount), category, method, testEnabled, values,
          willRaise, willRaiseInherit);
        //A source's rows are written for the method, so a row with a value it has no parameter for is a mistake.  An empty
        //row would otherwise give a single parameter an empty string.
        if Length(values) > Length(parameters) then
          SetArgsError(caseTest, TooManyValuesMessage(Length(values), Length(parameters), false))
        else if (Length(values) = 0) and (Length(parameters) > 0) then
          SetArgsError(caseTest, Format('Too few values: 0 values for %d parameter%s', [Length(parameters), PLURAL_S[Length(parameters) = 1]]));
      end;
    end;
  end;

begin
  //  WriteLn('Generating Tests for : ' + fixture.FullName);

  if fixture.HasChildFixtures then
  begin
    for childFixture in fixture.Children do
      GenerateTests(context, childFixture);
  end;

  rType := FRttiContext.GetType(fixture.TestClass);
  System.Assert(rType <> nil);

  //it's a dummy namespace fixture, don't bother with the rest.
  if rType.Handle = TypeInfo(TObject) then
    exit;

  tearDownFixtureIsDestructor := False;
  setupMethod := nil;
  tearDownMethod := nil;
  setupFixtureMethod := nil;
  tearDownFixtureMethod := nil;

  //Note : Relying on the order of the methods return, ie current type, then up the heirachy
  methods := rType.GetMethods;
  for method in methods do
  begin
    ignoredTest := false;
    ignoredReason := '';

    category := TStrUtils.Join(TListStringUtils.ToArray(fixture.Categories), ','); //default to the fixture's category
    categoryAttrib := nil;
    testEnabled := true;
    setupAttrib := nil;
    setupFixtureAttrib := nil;
    tearDownAttrib := nil;
    tearDownFixtureAttrib := nil;
    ignoredAttrib := nil;
    testAttrib := nil;
    categoryAttrib := nil;
    willRaiseAttrib := nil;
    isTestMethod := false;
    repeatCount := 1;
{$IFDEF MSWINDOWS}
    maxTimeAttrib := nil;
{$ENDIF}
    maxTime := 0;
    willRaise := nil;
    willRaiseInherit := exExact;
    currentFixture := fixture;
    autoCaseNames := nil;

    meth.Code := method.CodeAddress;
    //Call a virtual method through the fixture class's virtual method table: an override does not inherit the attributes
    //of the method it overrides, so it is the attributed ancestor method that is found, and it must run the override.
    if method.DispatchKind = dkVtable then
      meth.Code := PPointer(PByte(fixture.TestClass) + method.VirtualIndex * SizeOf(Pointer))^;
    meth.Data := fixture.FixtureInstance;

    //if the test has a category attribute then we'll use it to override the fixtures's category.
    if method.TryGetAttributeOfType<CategoryAttribute>(categoryAttrib) then
      category := categoryAttrib.Category;

    if method.TryGetAttributeOfType<RepeatTestAttribute>(repeatAttrib) then
    begin
      if (repeatAttrib.Count = 0) then
      begin
        ignoredTest := True;
        ignoredReason := STestIgnoredRepeatSet;
      end
      else if (repeatAttrib.Count > 1) then
      begin
        repeatCount := repeatAttrib.Count;
        currentFixture := fixture.AddChildFixture(fixture.TestClass, Format('%s.%s', [currentFixture.FullName, method.Name]), category);
        //setup and teardown are given to the child once all the methods have been seen, below.
        //don't assign setupfixture or teardown fixture as the parent fixture's methods will still be run.
      end;
    end;

    if (not Assigned(setupFixtureMethod)) and method.TryGetAttributeOfType<SetupFixtureAttribute>(setupFixtureAttrib) then
    begin
      setupFixtureMethod := TTestMethod(meth);
      currentFixture.SetSetupFixtureMethod(method.Name, setupFixtureMethod);
      continue;
    end;

    //if there is a Destructor then we will use it as the fixture
    //Teardown method.
    if (not Assigned(tearDownFixtureMethod)) and method.IsDestructor and (Length(method.GetParameters) = 0) then
    begin
      tearDownFixtureMethod := TTestMethod(meth);
      currentFixture.SetTearDownFixtureMethod(method.Name, TTestMethod(meth), true);
      tearDownFixtureIsDestructor := true;
      continue;
    end;

    //if we had previously assigned a destructor as the teardownfixturemethod, then we can still override that with an attributed one.
    if ((not Assigned(tearDownFixtureMethod)) or tearDownFixtureIsDestructor) and method.TryGetAttributeOfType<TearDownFixtureAttribute>(tearDownFixtureAttrib)
      then
    begin
      tearDownFixtureMethod := TTestMethod(meth);
      currentFixture.SetTearDownFixtureMethod(method.Name, tearDownFixtureMethod, false);
      tearDownFixtureIsDestructor := false;
      continue;
    end;

    if (not Assigned(setupMethod)) and method.TryGetAttributeOfType<SetupAttribute>(setupAttrib) then
    begin
      setupMethod := TTestMethod(meth);
      currentFixture.SetSetupTestMethod(method.Name, setupMethod);
      continue;
    end;

    if (not Assigned(tearDownMethod)) and method.TryGetAttributeOfType<TearDownAttribute>(tearDownAttrib) then
    begin
      tearDownMethod := TTestMethod(meth);
      currentFixture.SetTearDownTestMethod(method.Name, tearDownMethod);
      continue;
    end;

    if fixture.Ignored then
    begin
      ignoredTest := true;
      ignoredReason := fixture.IgnoreReason;
    end
    else if method.TryGetAttributeOfType<IgnoreAttribute>(ignoredAttrib) then
    begin
      ignoredTest := true;
      ignoredReason := ignoredAttrib.Reason;
    end;

    if method.TryGetAttributeOfType<WillRaiseAttribute>(willRaiseAttrib) then
    begin
      willRaise := willRaiseAttrib.ExpectedException;
      willRaiseInherit := willRaiseAttrib.ExceptionInheritance;
    end;

    if method.TryGetAttributeOfType<TestAttribute>(testAttrib) then
    begin
      testEnabled := testAttrib.Enabled;
      isTestMethod := true;
    end;

{$IFDEF MSWINDOWS}
    if method.TryGetAttributeOfType<MaxTimeAttribute>(maxTimeAttrib) then
      maxTime := maxTimeAttrib.MaxTime;
{$ENDIF}
    if method.IsDestructor or method.IsConstructor then
      continue;

    //if a test case is disabled then just ignore it.
    if testEnabled then
    begin
      //find out if the test fixture has test cases.
      testCases := method.GetAttributesOfType<CustomTestCaseAttribute>;
      //find out if the test has test case generators
      testCaseGenerators := method.GetAttributesOfType<CustomTestCaseGeneratorAttribute>;
      //find out if the test has test case sources
      testCaseSources := method.GetAttributesOfType<TestCaseSourceAttribute>;
      //Find out if we got a TestProviderAttribute
      tstProviderAttribs := Method.GetAttributesOfType<TestCaseProviderAttribute>;

      if (Length(testCases) > 0) or (Length(testCaseGenerators) > 0) or (Length(testCaseSources) > 0) or (Length(tstProviderAttribs) > 0) then
      begin
        if not ignoredTest then
        begin
          //Add the Providertests first
          for tstProviderAttrib in tstProviderAttribs do
          begin
            if (tstProviderAttrib.ProviderClass <> nil) then
              iProvider := TestDataProviderManager.GetProvider(tstProviderAttrib.ProviderClass)
            else
              iProvider := TestDataProviderManager.GetProvider(tstProviderAttrib.ProviderName);
            if (iProvider <> nil) then
            begin
              count := iProvider.GetCaseCount(method.name);
              for x := 0 to count - 1 do
              begin
                caseName := iProvider.GetCaseName(method.name, x);
                params := iProvider.GetCaseParams(method.name, x);
                for i := 1 to repeatCount do
                begin
                  test := currentFixture.AddTestCase(method.Name, FormatCaseName(caseName, x), FormatTestName(method.Name, i, repeatCount), category,
                    method, testEnabled, params, willRaise, willRaiseInherit);
                  CheckNotTooManyValues(test, params, method);
                end;
              end;
              iProvider := nil;
            end;
          end;
          // Add individual test cases
          for testCaseAttrib in testCases do
          begin
            //A [TestCase] is named from its values; a [NamedTestCase], or another kind of test case, has a name
            if (testCaseAttrib is TestCaseAttribute) and (testCaseAttrib.CaseInfo.Name = '') then
              caseName := UniqueCaseName('(' + TestCaseAttribute(testCaseAttrib).ValuesText + ')')
            else
              caseName := testCaseAttrib.CaseInfo.Name;
            for i := 1 to repeatCount do
            begin
              test := currentFixture.AddTestCase(method.Name, caseName, FormatTestName(method.Name, i, repeatCount), category, method,
                testEnabled, testCaseAttrib.CaseInfo.Values, willRaise, willRaiseInherit);
              //Values split from text get the separator hint; another kind of test case is checked as a generator is
              if testCaseAttrib is TestCaseAttribute then
              begin
                if TestCaseAttribute(testCaseAttrib).ValuesError <> '' then
                  SetArgsError(test, TestCaseAttribute(testCaseAttrib).ValuesError)
                else if Length(testCaseAttrib.CaseInfo.Values) > Length(method.GetParameters()) then
                  SetArgsError(test, TooManyValuesMessage(Length(testCaseAttrib.CaseInfo.Values), Length(method.GetParameters())));
              end
              else
                CheckNotTooManyValues(test, testCaseAttrib.CaseInfo.Values, method);
            end;
          end;
          // Add test cases from test case sources
          for testCaseSourceAttrib in testCaseSources do
            AddSourceCases(testCaseSourceAttrib);
          // Add test cases from test case generators
          if Length(testCaseGenerators) > 0 then
          begin
            for testCaseGeneratorAttrib in testCaseGenerators do
            begin
              if Length(testCaseGeneratorAttrib.CaseInfoArray) > 0 then
              begin
                for testCaseData in testCaseGeneratorAttrib.CaseInfoArray do
                begin
                  for i := 1 to repeatCount do
                  begin
                    test := currentFixture.AddTestCase(method.Name, TestCaseData.Name, FormatTestName(method.Name, i, repeatCount), category, method,
                      testEnabled, TestCaseData.Values, willRaise, willRaiseInherit);
                    CheckNotTooManyValues(test, TestCaseData.Values, method);
                  end;
                end;
              end;
            end;
          end;
        end
        else
        begin
          //if a testcase is ignored, just add it as a regular test.
          currentFixture.AddTest(method.Name, TTestMethod(meth), method.Name, category, method, true, true, ignoredReason, maxTime);
        end;
        continue;
      end;
    end;

    if isTestMethod and testEnabled then
    begin
      if IsAddedTestCode(meth.Code) then
        continue;
      for i := 1 to repeatCount do
      begin
        currentFixture.AddTest(method.Name, TTestMethod(meth), FormatTestName(method.Name, i, repeatCount), category, method, true, ignoredTest, ignoredReason,
          maxTime, willRaise, willRaiseInherit);
      end;
      continue;
    end;

    //finally.. if it's a pulished method
    if (method.Visibility = TMemberVisibility.mvPublished) and (testEnabled) then
    begin
      if IsAddedTestCode(meth.Code) then
        continue;
      // Add Published Method that has no Attributes
      for i := 1 to repeatCount do
      begin
        currentFixture.AddTest(method.Name, TTestMethod(meth), FormatTestName(method.Name, i, repeatCount), category, method, true, ignoredTest, ignoredReason,
          maxTime);
      end;
    end;
  end;

  //The setup and teardown of the fixture may be declared after a [RepeatTest] method, or in a base class (whose methods
  //come after the class's own), so give the repeat child fixtures the fixture's final ones.
  if fixture.HasChildFixtures then
  begin
    for childFixture in fixture.Children do
    begin
      if childFixture.TestClass = fixture.TestClass then
      begin
        childFixture.SetSetupTestMethod(fixture.SetupMethodName, fixture.SetupMethod);
        childFixture.SetTearDownTestMethod(fixture.TearDownMethodName, fixture.TearDownMethod);
      end;
    end;
  end;
end;

function TDUnitXFixtureProvider.TryGetAttributeOfType<T>(const attributes : TArray<TCustomAttribute>; var attribute : T) : boolean;
var
  LAttribute : TCustomAttribute;
begin
  attribute := Default(T);
  Result := false;
  for LAttribute in attributes do
  begin
    if LAttribute.InheritsFrom(T) then
    begin
      attribute := T(LAttribute);
      result := true;
      Break;
    end;
  end;
end;

procedure TDUnitXFixtureProvider.RTTIDiscoverFixtureClasses;
var
  types : TArray<TRttiType>;
  rType : TRttiType;
  attributes : TArray<TCustomAttribute>;
  sName : string;
  fixtureAttribute : TestFixtureAttribute;
begin
  types := FRttiContext.GetTypes;
  for rType in types do
  begin
    fixtureAttribute := nil;
    //try and keep the iteration down as much as possible
    if (rType.TypeKind = TTypeKind.tkClass) and (not rType.InheritsFrom(TPersistent)) then
    begin
      attributes := rType.GetAttributes;
      if Length(attributes) > 0 then
      begin
        if TryGetAttributeOfType<TestFixtureAttribute>(attributes, fixtureAttribute) then
        begin
          sName := fixtureattribute.Name;
          if sName = '' then
            sName := TRttiInstanceType(rType).MetaclassType.ClassName;
          if not FFixtureClasses.ContainsKey(TRttiInstanceType(rType).MetaclassType) then
            FFixtureClasses.Add(TRttiInstanceType(rType).MetaclassType, sName);
        end;
      end;
    end;
  end;
end;

end.

