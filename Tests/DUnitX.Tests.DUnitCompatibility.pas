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

unit DUnitX.Tests.DUnitCompatibility;

interface

uses
  DUnitX.TestFramework,
  DUnitX.DUnitCompatibility;

type
  // a typical DUnit like class
  {$M+}
  TMyDUnitTest = class(TTestCase)
  published
    procedure ATest;
  end;

  {$M+}
  TMyDUnitTestSetup = class(TTestCase)
  protected
    FObject: TObject;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure ATest;
  end;


implementation

{ TMyDUnitTest }

procedure TMyDUnitTest.ATest;
begin
  Status('Testing Status Redirect');
{$WARN SYMBOL_DEPRECATED OFF}
  CheckTrue(true,'true is always true!');
{$WARN SYMBOL_DEPRECATED ON}
end;

{ TMyDUnitTestSetup }

procedure TMyDUnitTestSetup.ATest;
begin
  {$WARN SYMBOL_DEPRECATED OFF}
  CheckNotNull(FObject);
  {$WARN SYMBOL_DEPRECATED ON}
end;

procedure TMyDUnitTestSetUp.SetUp;
begin
  inherited;
  FObject := TObject.Create;
end;

procedure TMyDUnitTestSetup.TearDown;
begin
  FObject.Free;
  inherited;
end;

initialization
  TDUnitX.RegisterTestFixture(TMyDUnitTest);
  TDUnitX.RegisterTestFixture(TMyDUnitTestSetup);

end.
