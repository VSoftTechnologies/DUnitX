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

unit DUnitX.Tests.WeakReference;

(*

The idea behind this unit is provide a similar lifecycle to reference counted objects
in delphi as WeakReference does in .NET.

Reference counted objects in delphi have some limitations when it comes to circular references,
where for example TParent references it's children (via IChild), and TChild references it's parent
(via IParent). If we remove any external references to our IParent and IChild instances without first
getting the child to remove it's reference to IParent, we would end up with orphaned objects. This
is because our IChild and IParent instances are holding references to each other, and thus they never
get released.

This unit was borrowed from FinalBuilder 7(with permission), it has been extensively used with
Delphi 2010 and has so far proven to be very reliable.

*)

interface

uses
  DUnitX.TestFramework,
  DUnitX.WeakReference;

type
  ISimpleInterface = interface
    ['{1A84030F-EEC5-447F-981D-94C4339D8800}']
    function GetName : string;
  end;

  TSimpleInterfacedObject = class(TWeakReferencedObject, ISimpleInterface)
    function GetName : string;
  end;

  IInternalUseInterface = interface
    ['{B2A2406D-C17D-478C-A194-59AACF40D279}']
    function GetName : string;
    function GetInternalUseOnly : string;
    property Name : string read GetName;
    property InternalUseOnly : string read GetInternalUseOnly;
  end;

  IExternalUseInterface = interface
    ['{907365C0-2A20-41D0-927B-8B36FB4CB0FD}']
    function GetName : string;
    property Name : string read GetName;
  end;

  TExposedObject = class(TWeakReferencedObject, IInternalUseInterface, IExternalUseInterface)
  private
    FName : string;
  protected
    function GetName : string;
    function GetInternalUseOnly : string;
  public
    property Name : string read GetName;
    constructor Create(const ANewName: string);
  end;

  {$M+}
  [TestFixture]
  TDUnitX_WeakReferenceXMLNUnitTests = class
  public
    [Test]
    procedure After_Being_Assigned_To_Another_Interface_Properties_Can_Be_Called;
    [Test]
    procedure After_Being_Created_Interface_Properties_Can_Be_Called;
    [Test]
    procedure After_The_Reference_Is_Freed_The_WeakReference_Data_Is_Nill;
    [Test]
    procedure After_The_Reference_Is_Freed_The_WeakReference_Data_Is_Not_Alive;
  end;

implementation

procedure TDUnitX_WeakReferenceXMLNUnitTests.After_Being_Assigned_To_Another_Interface_Properties_Can_Be_Called;
var
  mockInternalObj : IInternalUseInterface;
  mockExternalObj : IExternalUseInterface;
  weakRef : IWeakReference<IExternalUseInterface>;
const
  EXPECTED_NAME = 'We can see this exposed!';
begin
  //Make sure to create the object and store under the internal interface
  mockInternalObj := TExposedObject.Create(EXPECTED_NAME);
  //As we know the exposed object stored in internal object interface supports external interface just cast.
  mockExternalObj := (mockInternalObj as IExternalUseInterface);

  //Get a weak reference to the exposed interace
  weakRef := TWeakReference<IExternalUseInterface>.Create(mockExternalObj);

  //Check that we have a valid external reference.
  Assert.AreEqual(weakRef.Data.Name, EXPECTED_NAME);
end;

procedure TDUnitX_WeakReferenceXMLNUnitTests.After_Being_Created_Interface_Properties_Can_Be_Called;
var
  mockInterface : ISimpleInterface;
  weakRef : IWeakReference<ISimpleInterface>;
begin
  //Setup
  mockInterface := TSimpleInterfacedObject.Create;
  weakRef := TWeakReference<ISimpleInterface>.Create(mockInterface);

  //Check
  Assert.AreEqual(weakRef.Data.GetName, mockInterface.GetName);
end;


procedure TDUnitX_WeakReferenceXMLNUnitTests.After_The_Reference_Is_Freed_The_WeakReference_Data_Is_Nill;
var
  mockInterface : ISimpleInterface;
  weakRef : IWeakReference<ISimpleInterface>;
begin
  //Setup
  mockInterface := TSimpleInterfacedObject.Create;
  weakRef := TWeakReference<ISimpleInterface>.Create(mockInterface);

  //Test
  mockInterface := nil;

  //Check
  Assert.IsNull(weakRef.Data);
end;

procedure TDUnitX_WeakReferenceXMLNUnitTests.After_The_Reference_Is_Freed_The_WeakReference_Data_Is_Not_Alive;
var
  mockInterface : ISimpleInterface;
  weakRef : IWeakReference<ISimpleInterface>;
begin
  //Setup
  mockInterface := TSimpleInterfacedObject.Create;
  weakRef := TWeakReference<ISimpleInterface>.Create(mockInterface);

  //Test
  mockInterface := nil;

  //Check
  Assert.IsTrue(not weakRef.IsAlive);
end;

{ TExposedObject }

function TExposedObject.GetName: string;
begin
  Result := FName;
end;

constructor TExposedObject.Create(const ANewName: string);
begin
  inherited Create;
  FName := ANewName;
end;

function TExposedObject.GetInternalUseOnly: string;
begin
  Result := 'Only here for completeness!';
end;

{ TSimpleInterfacedObject }

function TSimpleInterfacedObject.GetName: string;
begin
  Result := Self.ClassName;
end;

initialization
  TDUnitX.RegisterTestFixture(TDUnitX_WeakReferenceXMLNUnitTests);
end.
