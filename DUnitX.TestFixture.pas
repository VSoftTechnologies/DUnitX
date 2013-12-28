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

unit DUnitX.TestFixture;

interface

uses
  DUnitX.TestFramework,
  DUnitX.InternalInterfaces,
  DUnitX.WeakReference,
  DUnitX.Generics,
  Rtti;

{$I DUnitX.inc}

type
  TDUnitXTestFixture = class(TWeakReferencedObject, ITestFixture, ITestFixtureInfo)
  class var
    FRttiContext  : TRttiContext;
  private
    FTestClass    : TClass;
    FName         : string;
    FNameSpace    : string;
    FDescription  : string;
    FEnabled      : boolean;
    FTests        : IList<ITest>;
    FTestInfos              : IList<ITestInfo>;
    FSetupMethod            : TTestMethod;
    FSetupFixtureMethod     : TTestMethod;
    FTearDownMethod         : TTestMethod;
    FTearDownFixtureMethod  : TTestMethod;
    FFixtureInstance        : TObject;
    FTestInOwnThread        : boolean;
    FSetupFixtureMethodName : string;
    FSetupMethodName        : string;
    FTearDownMethodName     : string;
    FTearDownFixtureMethodName  : string;
    FChildren : ITestFixtureList;
    FTearDownFixtureIsDestructor : boolean;
    FIgnoreMemoryLeaks : Boolean;

  protected
    //uses RTTI to buid the fixture & tests
    procedure GenerateFixtureFromClass;
    function GetEnabled: Boolean;
    procedure SetEnabled(const value: Boolean);


    function GetName: string;
    function GetNameSpace : string;
    function GetFullName : string;
    function GetDescription : string;
    function GetTests: IEnumerable<ITest>;
    function ITestFixtureInfo.GetTests = ITestFixtureInfo_GetTests;
    function ITestFixtureInfo_GetTests : IList<ITestInfo>;
    function GetTestClass : TClass;
    function GetSetupMethod : TTestMethod;
    function GetSetupFixtureMethod : TTestMethod;
    function GetTearDownMethod : TTestMethod;
    function GetTearDownFixtureMethod : TTestMethod;
    function GetTestInOwnThread : boolean;
    function GetSetupFixtureMethodName: string;
    function GetSetupMethodName: string;
    function GetTearDownMethodName: string;
    function GetTearDownFixtureMethodName: string;

    function GetTestCount : cardinal;
    function GetActiveTestCount : cardinal;

    function GetChildren: ITestFixtureList;
    function GetHasChildren : boolean;
    function GetHasTests : boolean;
    procedure OnMethodExecuted(const AMethod : TTestMethod);
  public
    constructor Create(const AName : string; const AClass : TClass);
    destructor Destroy;override;
    class constructor Create;
  end;

implementation

uses
  TypInfo,
  SysUtils,
  DUnitX.Test,
  DUnitX.Utils,
  System.Generics.Collections;

{ TDUnitXTestFixture }

constructor TDUnitXTestFixture.Create(const AName : string; const AClass : TClass);
var
  IgnoreMemoryLeak: IgnoreMemoryLeaks;
  rType : TRttiType;
  i : integer;
begin
  FTestClass := AClass;
  FTests := TDUnitXList<ITest>.Create;

  i := LastDelimiter('.',AName);
  if i <> 0 then
  begin
    FNameSpace := Copy(AName,1,i -1);
    FName := Copy(AName,i+1,Length(AName));
  end
  else
  begin
    FName := AName;
    FNameSpace := AName;
  end;

  FEnabled := True;

  FIgnoreMemoryLeaks := False;
  rType := FRttiContext.GetType(FTestClass);
  if rType.TryGetAttributeOfType<IgnoreMemoryLeaks>(IgnoreMemoryLeak) then
    FIgnoreMemoryLeaks := IgnoreMemoryLeak.IgnoreMemoryLeaks;

  //TODO: Constructor doing "work" makes this harder to test and handle errors if the class isn't of the correct structure
  GenerateFixtureFromClass;
end;

destructor TDUnitXTestFixture.Destroy;
begin
  if FFixtureInstance <> nil then
    FFixtureInstance.Free;

  FTests := nil;
  inherited;
end;



procedure TDUnitXTestFixture.GenerateFixtureFromClass;
var
  rType : TRttiType;
  rBaseType : TRttiType;
  methods : TArray<TRttiMethod>;
  method : TRttiMethod;
  attributes : TArray<TCustomAttribute>;
  attribute : TCustomAttribute;
  meth : TMethod;
  newTest : ITest;

  ignoreFixtureSetup : boolean;

  fixtureAttrib   : TestFixtureAttribute;
  testCases       : TArray<TestCaseAttribute>;
  testCaseAttrib  : TestCaseAttribute;
  testEnabled     : boolean;
  ignoredAttrib   : IgnoreAttribute;
  IgnoreMemoryLeak: IgnoreMemoryLeaks;
begin
  rType := FRttiContext.GetType(FTestClass);
  System.Assert(rType <> nil);

  //it's a dummy namespace fixture, don't bother with the rest.
  if rType.Handle = TypeInfo(TObject) then
  begin
    FFixtureInstance := FTestClass.Create;
    exit;
  end;

  //If the fixture class was decorated with [TestFixture] then use it for the description.
  fixtureAttrib := nil;
  if rType.TryGetAttributeOfType<TestFixtureAttribute>(fixtureAttrib) then
    FDescription := fixtureAttrib.Description;

  ignoreFixtureSetup := false;
  {$IFDEF DELPHI_XE_UP}
  //NOTE: Causes Delphi 2010 to be inconsistent with produced exe. Will sometimes crash with AV when generating fixtures.
  //If there is a parameterless constructor declared then we will use that as the
  //fixture Setup method.
  if rType.TryGetConstructor(method) then
  begin
    ignoreFixtureSetup := true;
    FFixtureInstance := method.Invoke(TRttiInstanceType(rtype).MetaclassType, []).AsObject;
  end
  else
  {$ENDIF}
    FFixtureInstance := FTestClass.Create;



  //important to use declared here.. otherwise we are looking at TObject as well.
  methods := rType.GetDeclaredMethods;
  for method in methods do
  begin
    meth.Code := method.CodeAddress;
    meth.Data := FFixtureInstance;

    {$IFDEF DELPHI_XE_UP}
    //if there is a Destructor then we will use it as the fixture
    //Teardown method.
    if method.IsDestructor and (Length(method.GetParameters) = 0) then
    begin
      FTearDownFixtureMethod := TTestMethod(meth);
      FTearDownFixtureMethodName := method.Name;
      FTearDownFixtureIsDestructor := True;
    end;
    {$ENDIF}

    attributes := method.GetAttributes;
    if Length(attributes) > 0 then
    begin
      for attribute in attributes do
      begin
        //first check if the method is our setup or teardown.
        if attribute.ClassType = SetupAttribute then
        begin
          FSetupMethod := TTestMethod(meth);
          FSetupMethodName := method.Name;
        end
        //If we found a parameterless constructor then that was used.
        else if (not ignoreFixtureSetup) and (attribute.ClassType = SetupFixtureAttribute) then
        begin
          FSetupFixtureMethod := TTestMethod(meth);
          FSetupFixtureMethodName := method.Name;
        end
        else if attribute.ClassType = TearDownAttribute then
        begin
          FTearDownMethod := TTestMethod(meth);
          FTearDownMethodName := method.Name;
        end
        else if (not FTearDownFixtureIsDestructor) and (attribute.ClassType = TearDownFixtureAttribute) then
        begin
          FTearDownFixtureMethod := TTestMethod(meth);
          FTearDownFixtureMethodName := method.Name;
        end
        else if attribute.ClassType = TestInOwnThreadAttribute then
          FTestInOwnThread := true
        //TODO: Should add tests to the list even though they aren't enabled.
        else if ((attribute.ClassType = TestAttribute)) or
                ((attribute.ClassType <> TestAttribute) and (method.Visibility = TMemberVisibility.mvPublished) and (not method.HasAttributeOfType<TestAttribute>)) then
        begin
          ignoredAttrib := method.GetAttributeOfType<IgnoreAttribute>;
          if attribute.ClassType = TestAttribute then
          begin
            testEnabled := TestAttribute(attribute).Enabled;

            if testEnabled and (ignoredAttrib = nil) then
              //find out if the test fixture has test cases.
              testCases := method.GetAttributesOfType<TestCaseAttribute>;

            if Length(testCases) > 0 then
            begin
              for testCaseAttrib in testCases do
              begin
                newTest := TDUnitXTestCase.Create(FFixtureInstance, Self, testCaseAttrib.Name, method.Name, method, testEnabled, testCaseAttrib.Values);
                newTest.Enabled := testEnabled;

                if method.TryGetAttributeOfType<IgnoreMemoryLeaks>(IgnoreMemoryLeak) then
                  newTest.IgnoreMemoryLeaks := IgnoreMemoryLeak.IgnoreMemoryLeaks
                else
                  newTest.IgnoreMemoryLeaks := FIgnoreMemoryLeaks;

                FTests.Add(newTest);
              end;
            end
            else
            begin
              if ignoredAttrib <> nil then
                newTest := TDUnitXTest.Create(Self, method.Name, TTestMethod(meth),testEnabled,true,ignoredAttrib.Reason)
              else
                newTest := TDUnitXTest.Create(Self, method.Name, TTestMethod(meth),testEnabled);

              if method.TryGetAttributeOfType<IgnoreMemoryLeaks>(IgnoreMemoryLeak) then
                newTest.IgnoreMemoryLeaks := IgnoreMemoryLeak.IgnoreMemoryLeaks
              else
                newTest.IgnoreMemoryLeaks := FIgnoreMemoryLeaks;

              FTests.Add(newTest);
            end;
          end
          else
          begin
            if ignoredAttrib <> nil then
              newTest := TDUnitXTest.Create(Self, method.Name, TTestMethod(meth),true,true,ignoredAttrib.Reason)
            else
              newTest := TDUnitXTest.Create(Self, method.Name, TTestMethod(meth),true);

            if method.TryGetAttributeOfType<IgnoreMemoryLeaks>(IgnoreMemoryLeak) then
              newTest.IgnoreMemoryLeaks := IgnoreMemoryLeak.IgnoreMemoryLeaks
            else
              newTest.IgnoreMemoryLeaks := FIgnoreMemoryLeaks;

            FTests.Add(newTest);
          end;
        end;
      end;
    end
    else if method.Visibility = TMemberVisibility.mvPublished then
    begin
      newTest := TDUnitXTest.Create(Self,method.Name,TTestMethod(meth),true);
      FTests.Add(newTest);
    end;
  end;


  if (not Assigned(FSetupMethod)) or (not Assigned(FSetupFixtureMethod))
     or (not Assigned(FTearDownMethod))  or (not Assigned(FTearDownFixtureMethod))then begin

    rBaseType := rType.BaseType;
    while Assigned(rBaseType) do begin
      if not rBaseType.TryGetAttributeOfType<TestFixtureAttribute>(fixtureAttrib) then
      begin
        methods := rBaseType.GetDeclaredMethods;
        for method in methods do
        begin
          meth.Code := method.CodeAddress;
          meth.Data := FFixtureInstance;


          if not Assigned(FSetupMethod) then begin
            attribute := method.GetAttributeOfType<SetupAttribute>;
            if Assigned(attribute) then begin
              FSetupMethod := TTestMethod(meth);
              FSetupMethodName := method.Name;
            end;
          end;

          if not Assigned(FSetupFixtureMethod) then begin
            attribute := method.GetAttributeOfType<SetupFixtureAttribute>;
            if Assigned(attribute) then begin
              FSetupFixtureMethod := TTestMethod(meth);
              FSetupFixtureMethodName := method.Name;
            end;
          end;

          if not Assigned(FTearDownMethod) then begin
            attribute := method.GetAttributeOfType<TearDownAttribute>;
            if Assigned(attribute) then begin
              FTearDownMethod := TTestMethod(meth);
              FTearDownMethodName := method.Name;
            end;
          end;

          if not Assigned(FTearDownFixtureMethod) then begin
            attribute := method.GetAttributeOfType<TearDownFixtureAttribute>;
            if Assigned(attribute) then begin
              FTearDownFixtureMethod := TTestMethod(meth);
              FTearDownFixtureMethodName := method.Name;
            end;
          end;
        end;
      end;
      rBaseType := rBaseType.BaseType;
    end;
  end;
end;

function TDUnitXTestFixture.GetActiveTestCount: cardinal;
begin
  //TODO: Return the active count, currently fudged to be the count.
  Result := GetTestCount;
end;

function TDUnitXTestFixture.GetChildren: ITestFixtureList;
begin
  if FChildren = nil then
    FChildren := TTestFixtureList.Create;
  result := FChildren;
end;

function TDUnitXTestFixture.GetDescription: string;
begin
  result := FDescription;
end;

function TDUnitXTestFixture.GetEnabled: Boolean;
begin
  result := FEnabled;
end;

function TDUnitXTestFixture.GetFullName: string;
begin
  if FName <> FNameSpace then
    result := FNameSpace + '.' + FName
  else
    Result := FName;
end;

function TDUnitXTestFixture.GetHasChildren: boolean;
begin
  result := (FChildren <> nil) and (FChildren.Count > 0);
end;


function TDUnitXTestFixture.GetHasTests: boolean;
begin
  result := (FTests <> nil) and (FTests.Count > 0);
end;

function TDUnitXTestFixture.GetName: string;
begin
  result := FName;
end;

function TDUnitXTestFixture.GetNameSpace: string;
begin
  result := FNameSpace;
end;

function TDUnitXTestFixture.GetSetupFixtureMethod: TTestMethod;
begin
  result := FSetupFixtureMethod;
end;

function TDUnitXTestFixture.GetSetupFixtureMethodName: string;
begin
  result := FSetupFixtureMethodName;
end;

function TDUnitXTestFixture.GetSetupMethod: TTestMethod;
begin
  result := FSetupMethod;
end;

function TDUnitXTestFixture.GetSetupMethodName: string;
begin
  result := FSetupMethodName;
end;

function TDUnitXTestFixture.GetTearDownFixtureMethod: TTestMethod;
begin
  result := FTearDownFixtureMethod;
end;

function TDUnitXTestFixture.GetTearDownFixtureMethodName: string;
begin
  result := FTearDownFixtureMethodName;
end;

function TDUnitXTestFixture.GetTearDownMethod: TTestMethod;
begin
  result  := FTearDownMethod;
end;

function TDUnitXTestFixture.GetTearDownMethodName: string;
begin
  result := FTearDownMethodName;
end;

function TDUnitXTestFixture.GetTestClass: TClass;
begin
  result := FTestClass;
end;

function TDUnitXTestFixture.GetTestCount: cardinal;
begin
  Result := FTests.Count;
end;

function TDUnitXTestFixture.GetTestInOwnThread: boolean;
begin
  result := FTestInOwnThread;
end;

function TDUnitXTestFixture.GetTests: IEnumerable<ITest>;
begin
  result := FTests;
end;

function TDUnitXTestFixture.ITestFixtureInfo_GetTests: IList<ITestInfo>;
var
  test : ITest;
begin
  //TODO: Need to test that this isn't accessed between updates to FTests.
  if FTestInfos = nil then
  begin
    FTestInfos := TDUnitXList<ITestInfo>.Create;
    for test in FTests do
      FTestInfos.Add(test as ITestInfo);
  end;
  result := FTestInfos;
end;


procedure TDUnitXTestFixture.OnMethodExecuted(const AMethod: TTestMethod);
begin
  if FTearDownFixtureIsDestructor then
  begin
    if TMethod(AMethod).Code = TMethod(FTearDownFixtureMethod).Code then
      FFixtureInstance := nil;
  end;
  
end;

procedure TDUnitXTestFixture.SetEnabled(const value: Boolean);
begin
  FEnabled := value;
end;

class constructor TDUnitXTestFixture.Create;
begin
  FRttiContext := TRttiContext.Create;
end;

end.

