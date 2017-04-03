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

unit DUnitX.FixtureProviderPlugin;

interface

{$I DUnitX.inc}

uses
  {$IFDEF USE_NS}
  System.Rtti,
  System.Generics.Collections,
  {$ELSE}
  Rtti,
  Generics.Collections,
  {$ENDIF}
  DUnitX.Extensibility;

type
  TDUnitXFixtureProviderPlugin = class(TInterfacedObject,IPlugin)
  protected
    procedure GetPluginFeatures(const context: IPluginLoadContext);
  end;


  TDUnitXFixtureProvider = class(TInterfacedObject,IFixtureProvider)
  private class var
    FRttiContext : TRttiContext;
  private
    FFixtureClasses : TDictionary<TClass,string>;

  protected
    function FormatTestName(const AName: string; const ATimes, ACount: Integer): string;
    function TryGetAttributeOfType<T : class>(const attributes: TArray<TCustomAttribute>; var attribute: T): boolean;
    procedure RTTIDiscoverFixtureClasses(const context: IFixtureProviderContext);
    procedure GenerateTests(const context: IFixtureProviderContext; const fixture : ITestFixture);
    procedure Execute(const context: IFixtureProviderContext);
  public
    class constructor Create;
    class destructor Destroy;
    constructor Create;
    destructor Destroy;override;
  end;


implementation
uses
  {$IFDEF USE_NS}
  System.TypInfo,
  System.Classes,
  System.Types,
  System.StrUtils,
  System.SysUtils,
  {$ELSE}
  TypInfo,
  Classes,
  Types,
  StrUtils,
  SysUtils,
  {$ENDIF}
  DUnitX.Attributes,
  DUnitX.Utils,
  DUnitX.TestFramework,
  DUnitX.ResStrs,
  DUnitX.Types;

{ TDUnitXFixtureProvider }

constructor TDUnitXFixtureProvider.Create;
begin
  FFixtureClasses := TDictionary<TClass,string>.Create;
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

procedure TDUnitXFixtureProvider.Execute(const context: IFixtureProviderContext);
var
  pair : TPair<TClass,string>;
  fixture : ITestFixture;
  parentFixture : ITestFixture;
  uName : string;
  namespaces : TStringDynArray;
  namespace : string;
  parentNamespace : string;
  fixtureNamespace : string;
  tmpFixtures : TDictionary<string,ITestFixture>;
  fixtureList : ITestFixtureList;
  rType : TRttiType;
  categoryAttrib : CategoryAttribute;
  category : string;
begin
  if context.UseRtti then
    RTTIDiscoverFixtureClasses(context);

  for pair in TDUnitX.RegisteredFixtures do
  begin
     if not FFixtureClasses.ContainsValue(pair.Value) then
      FFixtureClasses.AddOrSetValue(pair.Key, pair.Value);
  end;



  //Build up a fixture hierarchy based on unit names.
  tmpFixtures := TDictionary<string,ITestFixture>.Create;
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

      namespaces := SplitString(uName,'.');
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
          if not tmpFixtures.TryGetValue(fixtureNamespace,fixture) then
          begin
            parentFixture := context.CreateFixture(TObject,fixtureNamespace,'');
            tmpFixtures.Add(fixtureNamespace,parentFixture);
            fixtureList.Add(parentFixture);
          end;
          parentNamespace := fixtureNamespace;
          continue;
        end
        else
        begin
          if not tmpFixtures.TryGetValue(parentNamespace,parentFixture) then
          begin
            parentFixture := context.CreateFixture(TObject,parentNamespace,'');
            tmpFixtures.Add(parentNamespace,parentFixture);
            fixtureList.Add(parentFixture);
          end;

          if not tmpFixtures.TryGetValue(fixtureNamespace,fixture) then
          begin
            fixture := parentFixture.AddChildFixture(TObject,fixtureNamespace,'');
            tmpFixtures.Add(fixtureNamespace,fixture);
          end;
          parentFixture := fixture;
          parentNamespace := fixtureNamespace;
        end;
      end;

      fixtureNamespace := fixtureNamespace + '.' + pair.Value;

      if parentFixture = nil then
      begin
        fixture := context.CreateFixture(pair.Key,fixtureNamespace,category);
        fixtureList.Add(fixture);
      end
      else
        parentFixture.AddChildFixture(pair.Key,fixtureNamespace,category);
    end;
    for fixture in fixtureList do
    begin
      GenerateTests(context,fixture);
    end;

  finally
    fixtureList := nil;
    tmpFixtures.Free;
  end;
end;

function TDUnitXFixtureProvider.FormatTestName(const AName: string; const ATimes, ACount: Integer): string;
begin
  Result := AName;

  if (ACount > 1) then
  begin
    Result := Result + Format('-%d-of-%d', [ATimes, ACount]);
  end;
end;

procedure TDUnitXFixtureProvider.GenerateTests(const context: IFixtureProviderContext; const fixture: ITestFixture);
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
  ignoredAttrib   : IgnoreAttribute;
  willRaiseAttrib : WillRaiseAttribute;
  testCases       : TArray<CustomTestCaseAttribute>;
  testCaseAttrib  : CustomTestCaseAttribute;
  testCaseSources : TArray<CustomTestCaseSourceAttribute>;
  testCaseSourceAttrb : CustomTestCaseSourceAttribute;
  testCaseData    : TestCaseInfo;
  testEnabled     : boolean;
  isTestMethod    : boolean;
  repeatAttrib    : RepeatTestAttribute;
  maxTimeAttrib   : MaxTimeAttribute;

  category        : string;
  ignoredTest     : boolean;
  ignoredReason   : string;
  maxTime         : cardinal;
  willRaise       : ExceptClass;
  willRaiseInherit: TExceptionInheritance;

  repeatCount: Cardinal;
  i: Integer;
  currentFixture: ITestFixture;
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

    category := TStrUtils.Join(TListStringUtils.ToArray(fixture.Categories),','); //default to the fixture's category
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
    maxTimeAttrib := nil;
    maxTime := 0;
    willRaise := nil;
    willRaiseInherit := exExact;
    currentFixture := fixture;

    meth.Code := method.CodeAddress;
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
      else
      if (repeatAttrib.Count > 1) then
      begin
        repeatCount := repeatAttrib.Count;
        currentFixture := fixture.AddChildFixture(fixture.TestClass, Format('%s.%s', [currentFixture.FullName,method.Name]),category);
        //setup and teardown might have already been set.. so take them from the parent fixture.
        currentFixture.SetSetupTestMethod(fixture.SetupMethodName,fixture.SetupMethod);
        currentFixture.SetTearDownTestMethod(fixture.TearDownMethodName,fixture.TearDownMethod);
        //don't assign setupfixture or teardown fixture as the parent fixture's methods will still be run.
      end;
    end;

    if (not Assigned(setupFixtureMethod)) and method.TryGetAttributeOfType<SetupFixtureAttribute>(setupFixtureAttrib) then
    begin
       setupFixtureMethod := TTestMethod(meth);
       currentFixture.SetSetupFixtureMethod(method.Name,setupFixtureMethod);
       continue;
    end;

    {$IFDEF DELPHI_XE_UP}
    //if there is a Destructor then we will use it as the fixture
    //Teardown method.
    if (not Assigned(tearDownFixtureMethod)) and method.IsDestructor and (Length(method.GetParameters) = 0) then
    begin
      tearDownFixtureMethod := TTestMethod(meth);
      currentFixture.SetTearDownFixtureMethod(method.Name,TTestMethod(meth),true);
      tearDownFixtureIsDestructor := true;
      continue;
    end;
    {$ENDIF}

    //if we had previously assigned a destructor as the teardownfixturemethod, then we can still override that with an attributed one.
    if ((not Assigned(tearDownFixtureMethod)) or tearDownFixtureIsDestructor) and method.TryGetAttributeOfType<TearDownFixtureAttribute>(tearDownFixtureAttrib) then
    begin
       tearDownFixtureMethod := TTestMethod(meth);
       currentFixture.SetTearDownFixtureMethod(method.Name,tearDownFixtureMethod,false);
       tearDownFixtureIsDestructor := false;
       continue;
    end;

    if (not Assigned(setupMethod)) and method.TryGetAttributeOfType<SetupAttribute>(setupAttrib) then
    begin
      setupMethod := TTestMethod(meth);
      currentFixture.SetSetupTestMethod(method.Name,setupMethod);
      continue;
    end;

    if (not Assigned(tearDownMethod)) and  method.TryGetAttributeOfType<TearDownAttribute>(tearDownAttrib) then
    begin
      tearDownMethod := TTestMethod(meth);
      currentFixture.SetTearDownTestMethod(method.Name,tearDownMethod);
      continue;
    end;

    if method.TryGetAttributeOfType<IgnoreAttribute>(ignoredAttrib) then
    begin
      ignoredTest   := true;
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
      //find out if the test has test sources
      testCaseSources := method.GetAttributesOfType<CustomTestCaseSourceAttribute>;

      if (Length(testCases) > 0) or (Length(testCaseSources) > 0) then
      begin
        if not ignoredTest then
        begin
          // Add individual test cases first
          for testCaseAttrib in testCases do
          begin
            for i := 1 to repeatCount do
            begin
              currentFixture.AddTestCase(method.Name, testCaseAttrib.CaseInfo.Name, FormatTestName(method.Name, i, repeatCount), category, method, testEnabled, testCaseAttrib.CaseInfo.Values);
            end;
          end;
          // Add test case from test \case sources
          if Length(testCaseSources) > 0 then
          begin
            for testCaseSourceAttrb in testCaseSources do
            begin
              if Length(testCaseSourceAttrb.CaseInfoArray) > 0 then
              begin
                for testCaseData in testCaseSourceAttrb.CaseInfoArray do
                begin
                  for i := 1 to repeatCount do
                  begin
                    currentFixture.AddTestCase(method.Name, TestCaseData.Name, FormatTestName(method.Name, i, repeatCount), category, method, testEnabled,TestCaseData.Values);
                  end;
                end;
              end;
            end;
          end;
        end
        else
        begin
          //if a testcase is ignored, just add it as a regular test.
          currentFixture.AddTest(method.Name, TTestMethod(meth), method.Name, category, true, true, ignoredReason, maxTime);
        end;
        continue;
      end;
    end;

    if isTestMethod and testEnabled then
    begin
      for i := 1 to repeatCount do
      begin
        currentFixture.AddTest(method.Name, TTestMethod(meth), FormatTestName(method.Name, i, repeatCount), category, true, ignoredTest, ignoredReason, maxTime, willRaise, willRaiseInherit);
      end;
      continue;
    end;

    //finally.. if it's a pulished method
    if (method.Visibility = TMemberVisibility.mvPublished) and (testEnabled)  then
    begin
      // Add Published Method that has no Attributes
      for i := 1 to repeatCount do
      begin
        currentFixture.AddTest(method.Name, TTestMethod(meth), FormatTestName(method.Name, i, repeatCount), category, true, ignoredTest, ignoredReason, maxTime);
      end;
    end;
  end;
end;

function TDUnitXFixtureProvider.TryGetAttributeOfType<T>(const attributes : TArray<TCustomAttribute>; var attribute : T) : boolean;
var
  LAttribute: TCustomAttribute;
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

procedure TDUnitXFixtureProvider.RTTIDiscoverFixtureClasses(const context: IFixtureProviderContext);
var
  types : TArray<TRttiType>;
  rType : TRttiType;
  attributes : TArray<TCustomAttribute>;
  sName : string;
  fixtureAttribute : TestFixtureAttribute;
  categoryAttrib : CategoryAttribute;
  sNameSpace : string;
  sCategory : string;
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
        if TryGetAttributeOfType<TestFixtureAttribute>(attributes,fixtureAttribute) then
        begin
          sName := fixtureattribute.Name;
          if sName = '' then
            sName := TRttiInstanceType(rType).MetaclassType.ClassName;
          sNameSpace := TRttiInstanceType(rType).MetaclassType.UnitName;
          if TryGetAttributeOfType<CategoryAttribute>(attributes,categoryAttrib) then
            sCategory := categoryAttrib.Category
          else
            sCategory := '';

          if not FFixtureClasses.ContainsKey(TRttiInstanceType(rType).MetaclassType) then
            FFixtureClasses.Add(TRttiInstanceType(rType).MetaclassType,sName);
        end;
      end;
    end;
  end;
end;

{ TDUnitXFixtureProviderPlugin }

procedure TDUnitXFixtureProviderPlugin.GetPluginFeatures(const context: IPluginLoadContext);
begin
  context.RegisterFixtureProvider(TDUnitXFixtureProvider.Create);
end;

end.
