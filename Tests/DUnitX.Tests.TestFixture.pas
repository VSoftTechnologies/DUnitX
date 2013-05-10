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

unit DUnitX.Tests.TestFixture;

interface

uses
  DUnitX.TestFramework,
  DUnitX.TestFixture;

type
  {$M+}
  [TestFixture]
  TTestClassWithNonPublicSetup = class
  private
    FSetupRun : Boolean;
  protected
    [Setup]
    procedure Setup;
  public
    constructor Create;
    property SetupRun : Boolean read FSetupRun;
  end;
  {$M-}

  {$M+}
  [TestFixture]
  TDUnitXTestFixtureTests = class
  protected
    procedure Create_TDUnitXTestFixtureTests_Fixture;
  public
    [Test]
    procedure Setup_Attribute_On_Compiled_Out_Procedure_Raises_Exception;
  end;
  {$M-}
implementation

uses
  SysUtils;
{ TDUnitXTestFixtureTests }

procedure TDUnitXTestFixtureTests.Create_TDUnitXTestFixtureTests_Fixture;
var
  testFixture : ITestFixtureInfo;
begin
  testFixture := TDUnitXTestFixture.Create(TDUnitXTestFixtureTests.ClassName, TDUnitXTestFixtureTests);
end;

procedure TDUnitXTestFixtureTests.Setup_Attribute_On_Compiled_Out_Procedure_Raises_Exception;
var
  testFixture : ITestFixtureInfo;
begin
  //TODO: Make this raise an exception of sort.
  Assert.WillRaise(Create_TDUnitXTestFixtureTests_Fixture, Exception);
end;

{ TTestClassWithNonPublicSetup }

constructor TTestClassWithNonPublicSetup.Create;
begin
  inherited Create;
  FSetupRun := False;
end;

procedure TTestClassWithNonPublicSetup.Setup;
begin
  //Optimised out as the method is not used internally;
  FSetupRun := True;
end;

initialization
  TDUnitX.RegisterTestFixture(TDUnitXTestFixtureTests);
end.
