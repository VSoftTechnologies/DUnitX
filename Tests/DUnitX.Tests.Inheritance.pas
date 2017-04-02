{***************************************************************************}
{                                                                           }
{           DUnitX                                                          }
{                                                                           }
{           Copyright (C) 2017 Vincent Parrett                              }
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

unit DUnitX.Tests.Inheritance;

interface

{$I ..\DUnitX.inc}

uses
  DUnitX.TestFramework;

type
{$M+}
  TMyBaseTestClass = class
  private
    FBlah: string;
  protected
    function GetSomething: string; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    [TearDownFixture]
    procedure TearDownFixture;

    [Test]
    procedure TestInBase;
  end;

  TMyDerivedTest = class(TMyBaseTestClass)
  protected
    function GetSomething: string; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

constructor TMyBaseTestClass.Create;
begin
  FBlah := 'base';
end;

destructor TMyBaseTestClass.Destroy;
begin
  //Empty
  inherited;
end;

function TMyBaseTestClass.GetSomething: string;
begin
  Result := 'base';
end;

procedure TMyBaseTestClass.TearDownFixture;
begin
  FBlah := '';
end;

procedure TMyBaseTestClass.TestInBase;
begin
  Writeln(GetSomething);
  Assert.Pass;
end;

constructor TMyDerivedTest.Create;
begin
  inherited;
  FBlah := 'derived';
end;

destructor TMyDerivedTest.Destroy;
begin
  //Empty
  inherited;
end;

function TMyDerivedTest.GetSomething: string;
begin
  Result := 'derived';
  Assert.Pass;
end;

initialization
  TDUnitX.RegisterTestFixture(TMyDerivedTest);

end.
