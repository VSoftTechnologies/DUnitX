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

unit DUnitX.TestFixture;

interface

{$I DUnitX.inc}

uses
  {$IFDEF USE_NS}
  System.Generics.Collections,
  System.Rtti,
  System.SysUtils,
  {$ELSE}
  Generics.Collections,
  Rtti,
  SysUtils,
  {$ENDIF}
  DUnitX.Types,
  DUnitX.Attributes,
  DUnitX.TestFramework,
  DUnitX.Extensibility,
  DUnitX.InternalInterfaces,
  DUnitX.WeakReference,
  DUnitX.Generics;

type
  TDUnitXTestFixture = class(TWeakReferencedObject, ITestFixture,ITestFixtureInfo)
  private class var
    FRttiContext  : TRttiContext;
  private
    FFixtureType  : TRttiType;
    FTestClass    : TClass;
    FUnitName     : string;
    FName         : string;
    FNameSpace    : string;
    FDescription  : string;
    FCategories   : TList<string>;
    FEnabled      : boolean;
    FTests        : ITestList;
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

    FIgnoreFixtureSetup : boolean;
  protected
    //used by GenerateFixtureFromClass to be tests from TestCaseInfo
    function CreateTestFromTestCase(const ACaseInfo : TestCaseInfo; const ACategory : string; const AMethod : TRttiMethod; const ATestEnabled : Boolean) : ITest;
    //used by GenerateFixtureFromClass to be tests from a method with out test cases
    function CreateTestFromMethod(const AMethod : TRttiMethod; const ACategory : string; const ATestEnabled : Boolean;const AIgnored : Boolean;const AIgnoredReason: String) : ITest;
    // Check overriding attribute, other wise uses returns fixture value
    function GetIgnoreMemoryLeaksForMethod(AMethod : TRttiMethod) : Boolean;


    function GetEnabled: Boolean;
    procedure SetEnabled(const value: Boolean);


    function GetName: string;
    function GetNameSpace : string;
    function GetFullName : string;
    function GetUnitName : string;
    function GetDescription : string;
    function GetCategories : TList<string>;
    function GetTests: ITestList;
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
    function GetFixtureInstance : TObject;
    function GetTestCount : cardinal;
    function GetActiveTestCount : cardinal;

    function GetChildren: ITestFixtureList;
    function GetHasChildren : boolean;
    function GetHasTests : boolean;
    function GetHasChildTests: Boolean;
    function IsNameSpaceOnly : boolean;
    procedure OnMethodExecuted(const AMethod : TTestMethod);

    procedure ExecuteFixtureTearDown;
    procedure InitFixtureInstance;
    procedure InternalInitFixtureInstance(const isConstructing : boolean);

    function AddTest(const AMethodName : string; const AMethod : TTestMethod; const AName : string; const ACategory  : string; const AEnabled : boolean = true;const AIgnored : boolean = false; const AIgnoreReason : string = ''; const AMaxTime :cardinal = 0; AExpectedException: ExceptClass = nil; const AExceptionInheritance: TExceptionInheritance = exExact) : ITest;
    function AddTestCase(const AMethodName : string; const ACaseName : string; const AName : string; const ACategory  : string; const AMethod : TRttiMethod; const AEnabled : boolean; const AArgs : TValueArray) : ITest;

    function AddChildFixture(const ATestClass : TClass; const AName : string; const ACategory : string) : ITestFixture;overload;
    function AddChildFixture(const AInstance : TObject; const AName : string; const ACategory : string) : ITestFixture;overload;

    procedure SetSetupTestMethod(const AMethodName : string; const AMethod : TTestMethod);
    procedure SetSetupFixtureMethod(const AMethodName : string; const AMethod : TTestMethod);
    procedure SetTearDownTestMethod(const AMethodName : string; const AMethod : TTestMethod);
    procedure SetTearDownFixtureMethod(const AMethodName : string; const AMethod : TTestMethod; const AIsDestructor : boolean);
    procedure SetTestInOwnThread(const value: Boolean);
    class constructor Create;
    class destructor Destroy;
  public
    constructor Create(const AName : string; const ACategory : string; const AInstance : TObject; const AUnitName : string);overload;
    constructor Create(const AName : string; const ACategory : string; const AClass : TClass; const AUnitName : string);overload;
    destructor Destroy;override;

  end;

implementation

uses
  {$IFDEF USE_NS}
  System.TypInfo,
  System.Generics.Defaults,
  {$ELSE}
  TypInfo,
  Generics.Defaults,
  {$ENDIF}
  DUnitX.Test,
  DUnitX.Utils;

{ TDUnitXTestFixture }

constructor TDUnitXTestFixture.Create(const AName : string; const ACategory : string; const AClass : TClass; const AUnitName : string);
var
  fixtureAttrib   : TestFixtureAttribute;
  IgnoreMemoryLeakAttrib: IgnoreMemoryLeaks;
  i : integer;
  categories : TArray<string>;
  cat : string;
begin
  inherited Create;
  FTestClass := AClass;
  FUnitName := AUnitName;
  FTests := TTestList.Create;
  FCategories := TList<string>.Create(TComparer<string>.Construct(
    function(const Left, Right : string) : integer
    begin
      result := AnsiCompareText(Left,Right);
    end));

  if ACategory <> '' then
  begin
    categories := TStrUtils.SplitString(ACategory,',');
    for cat in categories do
    begin
      FCategories.Add(Trim(cat));
    end;
  end;



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
  FFixtureType := FRttiContext.GetType(FTestClass);
  if FFixtureType.TryGetAttributeOfType<IgnoreMemoryLeaks>(IgnoreMemoryLeakAttrib) then
    FIgnoreMemoryLeaks := IgnoreMemoryLeakAttrib.IgnoreLeaks;

  fixtureAttrib := nil;
  if FFixtureType.TryGetAttributeOfType<TestFixtureAttribute>(fixtureAttrib) then
    FDescription := fixtureAttrib.Description;

  InternalInitFixtureInstance(true);
end;

destructor TDUnitXTestFixture.Destroy;
begin
  if FFixtureInstance <> nil then
    FFixtureInstance.Free;
  if FChildren <> nil then
    FChildren.Clear;
  FChildren := nil;

  FCategories.Free;
  FTests.Clear;
  FTests := nil;
  inherited;
end;




procedure TDUnitXTestFixture.ExecuteFixtureTearDown;
begin
  if Assigned(FTearDownFixtureMethod) then
  begin
    if FTearDownFixtureIsDestructor then
    begin
      FFixtureInstance.Free;
      FFixtureInstance := nil;
    end
    else
      FTearDownFixtureMethod();
  end;
end;

function TDUnitXTestFixture.GetActiveTestCount: cardinal;
begin
  //TODO: Return the active count, currently fudged to be the count.
  Result := GetTestCount;
end;

function TDUnitXTestFixture.GetCategories : TList<string>;
begin
  result := FCategories;
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

function TDUnitXTestFixture.GetFixtureInstance: TObject;
begin
  result := FFixtureInstance;
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


function TDUnitXTestFixture.GetHasChildTests: Boolean;
var
  fixture: ITestFixture;
begin
  result := GetHasChildren;
  if result then
  begin
    result := false;
    for fixture in FChildren do
    begin
      result := fixture.HasTests;
      if result then
        exit;
      result := fixture.HasChildTests;
      if result then
        exit;
    end;
  end;
end;

function TDUnitXTestFixture.GetHasTests: boolean;
var
  test : ITest;
begin
  result := false;
  if FTests <> nil then
    for test in FTests do
    begin
      if test.Enabled then
        exit(true);
    end;
end;

function TDUnitXTestFixture.GetIgnoreMemoryLeaksForMethod(
  AMethod: TRttiMethod): Boolean;
var
 IgnoreMemoryLeak: IgnoreMemoryLeaks;
begin
  if AMethod.TryGetAttributeOfType<IgnoreMemoryLeaks>(IgnoreMemoryLeak) then
    result := IgnoreMemoryLeak.IgnoreLeaks
  else
    result := FIgnoreMemoryLeaks;
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

function TDUnitXTestFixture.GetTests: ITestList;
begin
  result := FTests;
end;

function TDUnitXTestFixture.GetUnitName: string;
begin
  result := FUnitName;
end;

procedure TDUnitXTestFixture.InitFixtureInstance;
begin
  InternalInitFixtureInstance(false);
end;

procedure TDUnitXTestFixture.InternalInitFixtureInstance(const isConstructing: boolean);
var
  test : ITest;
{$IFDEF DELPHI_XE_UP}
   method : TRttiMethod;
{$ENDIF}

begin
  if FFixtureInstance = nil then
  begin
    //it's a dummy namespace fixture, don't bother with the rest.
    if FFixtureType.Handle = TypeInfo(TObject) then
    begin
      FFixtureInstance := FTestClass.Create;
      exit;
    end;


    FIgnoreFixtureSetup := false;
    {$IFDEF DELPHI_XE_UP}
    //NOTE: Causes Delphi 2010 to be inconsistent with produced exe. Will sometimes crash with AV when generating fixtures.
    //If there is a parameterless constructor declared then we will use that as the
    //fixture Setup method.
    if FFixtureType.TryGetConstructor(method) then
    begin
      FIgnoreFixtureSetup := true;
      FFixtureInstance := method.Invoke(TRttiInstanceType(FFixtureType).MetaclassType, []).AsObject;
    end
    else
    {$ENDIF}
      FFixtureInstance := FTestClass.Create;

    //Don't do this if we are called from the constructor as it's not needed.
    if not isConstructing then
    begin
      //The fixture instance has changed, need to update the tests to run on the new instance.
      for test in FTests do
        (test as ITestExecute).UpdateInstance(FFixtureInstance);
    end;
  end;
end;

function TDUnitXTestFixture.IsNameSpaceOnly: boolean;
begin
  result := FTestClass = TObject;
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
  if FTearDownFixtureIsDestructor and (TMethod(AMethod).Code = TMethod(FTearDownFixtureMethod).Code) then
      FFixtureInstance := nil;
end;

procedure TDUnitXTestFixture.SetEnabled(const value: Boolean);
begin
  FEnabled := value;
end;

procedure TDUnitXTestFixture.SetSetupFixtureMethod(const AMethodName: string; const AMethod: TTestMethod);
begin
  if not FIgnoreFixtureSetup then
  begin
    FSetupFixtureMethodName := AMethodName;
    FSetupFixtureMethod := AMethod;
  end;
end;

procedure TDUnitXTestFixture.SetSetupTestMethod(const AMethodName: string; const AMethod: TTestMethod);
begin
  FSetupMethodName := AMethodName;
  FSetupMethod := AMethod;
end;

procedure TDUnitXTestFixture.SetTearDownFixtureMethod(const AMethodName: string; const AMethod: TTestMethod; const AIsDestructor : boolean);
begin
  FTearDownFixtureMethodName := AMethodName;
  FTearDownFixtureMethod := AMethod;
  FTearDownFixtureIsDestructor := AIsDestructor;
end;

procedure TDUnitXTestFixture.SetTearDownTestMethod(const AMethodName: string; const AMethod: TTestMethod);
begin
  FTearDownMethodName := AMethodName;
  FTearDownMethod := AMethod;
end;

procedure TDUnitXTestFixture.SetTestInOwnThread(const value: Boolean);
begin
  FTestInOwnThread := value;
end;

function TDUnitXTestFixture.AddChildFixture(const ATestClass: TClass; const AName: string; const ACategory : string): ITestFixture;
begin
  result := TDUnitXTestFixture.Create(AName,ACategory, ATestClass,ATestClass.UnitName);
  if FChildren = nil then
    FChildren := TTestFixtureList.Create;
  FChildren.Add(result);
end;

function TDUnitXTestFixture.AddChildFixture(const AInstance: TObject; const AName: string; const ACategory : string): ITestFixture;
begin
  result := TDUnitXTestFixture.Create(AName,ACategory, AInstance,AInstance.ClassType.UnitName);
  if FChildren = nil then
    FChildren := TTestFixtureList.Create;
  FChildren.Add(result);
end;

function TDUnitXTestFixture.AddTest(const AMethodName : string; const AMethod : TTestMethod; const AName : string; const ACategory  : string; const AEnabled : boolean;const AIgnored : boolean; const AIgnoreReason : string; const AMaxTime :cardinal; AExpectedException: ExceptClass; const AExceptionInheritance: TExceptionInheritance): ITest;
begin
  if AExpectedException = nil then
    result  := TDUnitXTest.Create(Self, AMethodName, AName, ACategory, AMethod, AEnabled, AIgnored, AIgnoreReason, AMaxTime)
  else
    result  := TDUnitXExceptionTest.Create(Self, AMethodName, AName, ACategory, AMethod, AEnabled, AIgnored, AIgnoreReason, AMaxTime, AExpectedException, AExceptionInheritance);
  FTests.Add(Result);
end;

function TDUnitXTestFixture.AddTestCase(const AMethodName : string; const ACaseName, AName: string; const ACategory  : string;const AMethod: TRttiMethod; const AEnabled: boolean; const AArgs: TValueArray): ITest;
begin
  result := TDUnitXTestCase.Create(FFixtureInstance, Self, AMethodName, ACaseName, AName, ACategory, AMethod, AEnabled, AArgs);
  FTests.Add(result);
end;

class constructor TDUnitXTestFixture.Create;
begin
  FRttiContext := TRttiContext.Create;
end;

class destructor TDUnitXTestFixture.Destroy;
begin
  FRttiContext.Free;
end;


constructor TDUnitXTestFixture.Create(const AName: string; const ACategory : string; const AInstance: TObject; const AUnitName : string);
begin
  FFixtureInstance := AInstance;
  Create(AName, ACategory, AInstance.ClassType,AUnitName);
end;

function TDUnitXTestFixture.CreateTestFromMethod(const AMethod: TRttiMethod; const ACategory : string;  const ATestEnabled, AIgnored: Boolean; const AIgnoredReason: String): ITest;
var
  Meth : TMethod;
begin
  meth.Code := AMethod.CodeAddress;
  meth.Data := FFixtureInstance;
  result  := TDUnitXTest.Create(Self, AMethod.Name, AMethod.Name, ACategory, TTestMethod(meth),ATestEnabled,AIgnored,AIgnoredReason);
  result.IgnoreMemoryLeaks := GetIgnoreMemoryLeaksForMethod(AMethod);
end;

function TDUnitXTestFixture.CreateTestFromTestCase(const ACaseInfo : TestCaseInfo; const ACategory : string;  const AMethod : TRttiMethod; const ATestEnabled : Boolean) : ITest;
begin
  result := TDUnitXTestCase.Create(FFixtureInstance, Self, AMethod.Name, ACaseInfo.Name,  AMethod.Name, ACategory, AMethod, ATestEnabled, ACaseInfo.Values);
  result.IgnoreMemoryLeaks := getIgnoreMemoryLeaksForMethod(AMethod);
end;


end.

