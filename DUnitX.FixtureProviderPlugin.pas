unit DUnitX.FixtureProviderPlugin;

interface

uses
  Rtti,
  Generics.Collections,
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
    procedure RTTIDiscoverFixtureClasses;
    procedure GenerateTests(const fixture : ITestFixture);
    procedure Execute(const context: IFixtureProviderContext);
  public
    class constructor Create;
    class destructor Destroy;
    constructor Create;
    destructor Destroy;override;
  end;


implementation
uses
  TypInfo,
  Classes,
  Types,
  StrUtils,
  DUnitX.Utils,
  DUnitX.TestFramework;

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
begin
  if context.UseRtti then
    RTTIDiscoverFixtureClasses;
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
          parentNamespace := fixtureNamespace
        else
        begin
          if not tmpFixtures.TryGetValue(parentNamespace,parentFixture) then
          begin
            parentFixture := context.CreateFixture(TObject,parentNamespace);
            tmpFixtures.Add(parentNamespace,parentFixture);
            fixtureList.Add(parentFixture);
          end;

          if not tmpFixtures.TryGetValue(fixtureNamespace,fixture) then
          begin
            fixture := parentFixture.AddChildFixture(TObject,fixtureNamespace);
            tmpFixtures.Add(fixtureNamespace,fixture);
          end;

          parentFixture := fixture;
          parentNamespace := fixtureNamespace;
        end;
      end;

      fixtureNamespace := fixtureNamespace + '.' + pair.Value;

      if parentFixture = nil then
      begin
        fixture := context.CreateFixture(pair.Key,fixtureNamespace);
        fixtureList.Add(fixture);
      end
      else
        parentFixture.AddChildFixture(pair.Key,fixtureNamespace);
    end;
    for fixture in fixtureList do
    begin
      GenerateTests(fixture);
    end;

  finally
    fixtureList := nil;
    tmpFixtures.Free;
  end;
end;

procedure TDUnitXFixtureProvider.GenerateTests(const fixture: ITestFixture);
var
  childFixture : ITestFixture;

  rType : TRttiType;
  rBaseType : TRttiType;
  methods : TArray<TRttiMethod>;
  method : TRttiMethod;
  attribute : TCustomAttribute;
  meth : TMethod;
  fixtureAttrib   : TestFixtureAttribute;

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
  ignoredAttrib   : IgnoreAttribute;
  testCases       : TArray<CustomTestCaseAttribute>;
  testCaseAttrib  : CustomTestCaseAttribute;
  testCaseSources : TArray<CustomTestCaseSourceAttribute>;
  testCaseSourceAttrb : CustomTestCaseSourceAttribute;
  testCaseData    : TestCaseInfo;
  testEnabled     : boolean;
  isTestMethod    : boolean;


  ignoredTest     : boolean;
  ignoredReason   : string;


begin
  // WriteLn('Generating Tests for : ' + fixture.FullName);
  // SBD: The above line was commented out because it is wrong.
  // TODO: Insert some code to achieve what-ever was intended by the WriteLn.
  if fixture.HasChildFixtures then
  begin
    for childFixture in fixture.Children do
      GenerateTests(childFixture);
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

  //important to use declared here.. otherwise we are looking at TObject as well.
  methods := rType.GetDeclaredMethods;
  for method in methods do
  begin
    ignoredTest := false;
    ignoredReason := '';
    testEnabled := true;
    setupAttrib := nil;
    setupFixtureAttrib := nil;
    tearDownAttrib := nil;
    tearDownFixtureAttrib := nil;
    ignoredAttrib := nil;
    testAttrib := nil;
    isTestMethod := false;

    meth.Code := method.CodeAddress;
    meth.Data := fixture.FixtureInstance;

    {$IFDEF DELPHI_XE_UP}
    //if there is a Destructor then we will use it as the fixture
    //Teardown method.
    if method.IsDestructor and (Length(method.GetParameters) = 0) then
    begin
      fixture.SetTearDownFixtureMethod(TTestMethod(meth),method.Name,true);
      tearDownFixtureIsDestructor := true;
      tearDownFixtureMethod := TTestMethod(meth);
      continue;
    end;
    {$ENDIF}

    if method.TryGetAttributeOfType<SetupAttribute>(setupAttrib) then
    begin
      setupMethod := TTestMethod(meth);
      fixture.SetSetupTestMethod(method.Name,setupMethod);
      continue;
    end;

    if method.TryGetAttributeOfType<TearDownAttribute>(tearDownAttrib) then
    begin
      tearDownMethod := TTestMethod(meth);
      fixture.SetTearDownTestMethod(method.Name,tearDownMethod);
      continue;
    end;

    if method.TryGetAttributeOfType<SetupFixtureAttribute>(setupFixtureAttrib) then
    begin
       setupFixtureMethod := TTestMethod(meth);
       fixture.SetSetupFixtureMethod(method.Name,setupFixtureMethod);
       continue;
    end;

    if (not tearDownFixtureIsDestructor) and method.TryGetAttributeOfType<TearDownFixtureAttribute>(tearDownFixtureAttrib) then
    begin
       tearDownFixtureMethod := TTestMethod(meth);
       fixture.SetTearDownFixtureMethod(method.Name,tearDownFixtureMethod,false);
       continue;
    end;

    if method.TryGetAttributeOfType<IgnoreAttribute>(ignoredAttrib) then
    begin
       ignoredTest   := true;
       ignoredReason := ignoredAttrib.Reason;
    end;


    if method.TryGetAttributeOfType<TestAttribute>(testAttrib) then
    begin
       testEnabled := testAttrib.Enabled;
       isTestMethod := true;
    end;

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
            fixture.AddTestCase(testCaseAttrib.CaseInfo.Name, method.Name, method, testEnabled,testCaseAttrib.CaseInfo.Values);
          // Add test case from test case sources
          for testCaseSourceAttrb in testCaseSources do
          begin
            for testCaseData in testCaseSourceAttrb.CaseInfoArray do
                fixture.AddTestCase(TestCaseData.Name,method.Name, method, testEnabled,TestCaseData.Values);
          end;
        end
        else
        begin
          //if a testcase is ignored, just add it as a regular test.
          fixture.AddTest(TTestMethod(meth),method.Name,true,true,ignoredReason);
        end;
        continue;
      end;
    end;

    if isTestMethod and testEnabled then
    begin
      fixture.AddTest(TTestMethod(meth),method.Name,true,ignoredTest,ignoredReason);
      continue;
    end;

    //finally.. if it's a pulished method
    if (method.Visibility = TMemberVisibility.mvPublished) and (testEnabled)  then
    begin
      // Add Published Method that has no Attributes
      fixture.AddTest(TTestMethod(meth),method.Name,true,ignoredTest,ignoredReason);
    end;
  end;


  if (not Assigned(setupMethod)) or (not Assigned(setupFixtureMethod))
     or (not Assigned(tearDownMethod))  or (not Assigned(tearDownFixtureMethod))then
  begin

    rBaseType := rType.BaseType;
    while Assigned(rBaseType) do
    begin
      if not rBaseType.TryGetAttributeOfType<TestFixtureAttribute>(fixtureAttrib) then
      begin
        methods := rBaseType.GetDeclaredMethods;
        for method in methods do
        begin
          meth.Code := method.CodeAddress;
          meth.Data := fixture.FixtureInstance;

          if not Assigned(setupMethod) then
          begin
            attribute := method.GetAttributeOfType<SetupAttribute>;
            if Assigned(attribute) then
            begin
              setupMethod := TTestMethod(meth);
              fixture.SetSetupTestMethod(method.Name,setupMethod);
            end;
          end;

          if not Assigned(setupFixtureMethod) then
          begin
            attribute := method.GetAttributeOfType<SetupFixtureAttribute>;
            if Assigned(attribute) then
            begin
              setupFixtureMethod := TTestMethod(meth);
              fixture.SetSetupFixtureMethod(method.Name,setupFixtureMethod);
            end;
          end;

          if not Assigned(tearDownMethod) then
          begin
            attribute := method.GetAttributeOfType<TearDownAttribute>;
            if Assigned(attribute) then
            begin
              tearDownMethod := TTestMethod(meth);
              fixture.SetTearDownTestMethod(method.Name,tearDownMethod);
            end;
          end;

          if not Assigned(tearDownFixtureMethod) then
          begin
            attribute := method.GetAttributeOfType<TearDownFixtureAttribute>;
            if Assigned(attribute) then
            begin
              tearDownFixtureMethod := TTestMethod(meth);
              fixture.SetTearDownFixtureMethod(method.Name,tearDownFixtureMethod,false);
            end;
          end;
        end;
      end;
      rBaseType := rBaseType.BaseType;
    end;
  end;

end;

procedure TDUnitXFixtureProvider.RTTIDiscoverFixtureClasses;
var
  types : TArray<TRttiType>;
  rType : TRttiType;
  attributes : TArray<TCustomAttribute>;
  attribute : TCustomAttribute;
  sName : string;
begin
  types := FRttiContext.GetTypes;
  for rType in types do
  begin
    //try and keep the iteration down as much as possible
    if (rType.TypeKind = TTypeKind.tkClass) and (not rType.InheritsFrom(TPersistent)) then
    begin
      attributes := rType.GetAttributes;
      if Length(attributes) > 0 then
        for attribute in attributes do
        begin
          if attribute.ClassType =  TestFixtureAttribute then
          begin
            sName := TestFixtureAttribute(attribute).Name;
            if sName = '' then
              sName := TRttiInstanceType(rType).MetaclassType.ClassName;
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

