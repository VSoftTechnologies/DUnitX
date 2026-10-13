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
    procedure CheckInherits_Passes_When_Actual_Descends_From_Expected;
    procedure CheckInherits_Fails_When_Expected_Descends_From_Actual;
  end;

{$M+}
  TMyDUnitTestSetup = class(TTestCase)
  protected
    FObject : TObject;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure ATest;
    procedure CheckEquals_String_Is_Case_Sensitive;
    procedure CheckEqualsString_Is_Case_Sensitive;
    procedure CheckNotEquals_String_Passes_When_Only_Case_Differs;
    procedure Test_TestName;
    procedure CheckEqualsBin_Fails_When_Values_Differ_Above_Digits;
  end;

implementation

{ TMyDUnitTest }

procedure TMyDUnitTest.ATest;
begin
  Status('Testing Status Redirect');
{$WARN SYMBOL_DEPRECATED OFF}
  CheckTrue(true, 'true is always true!');
{$WARN SYMBOL_DEPRECATED ON}
end;

procedure TMyDUnitTest.CheckInherits_Passes_When_Actual_Descends_From_Expected;
begin
  //DUnit: CheckInherits(expected, actual) checks actual.InheritsFrom(expected)
{$WARN SYMBOL_DEPRECATED OFF}
  CheckInherits(TObject, TTestCase);
{$WARN SYMBOL_DEPRECATED ON}
end;

procedure TMyDUnitTest.CheckInherits_Fails_When_Expected_Descends_From_Actual;
begin
  Assert.WillRaise(
    procedure
    begin
{$WARN SYMBOL_DEPRECATED OFF}
      CheckInherits(TTestCase, TObject);
{$WARN SYMBOL_DEPRECATED ON}
    end, Assert.TestFailure);
end;

{ TMyDUnitTestSetup }

procedure TMyDUnitTestSetup.ATest;
begin
{$WARN SYMBOL_DEPRECATED OFF}
  CheckNotNull(FObject);
{$WARN SYMBOL_DEPRECATED ON}
end;

procedure TMyDUnitTestSetup.CheckEquals_String_Is_Case_Sensitive;
var
  lower, upper : string;
begin
  //DUnit's string checks are case-sensitive
  lower := 'abc';
  upper := 'ABC';
  Assert.WillRaiseDescendant(
    procedure
    begin
{$WARN SYMBOL_DEPRECATED OFF}
      CheckEquals(lower, upper);
{$WARN SYMBOL_DEPRECATED ON}
    end, Assert.TestFailure);
end;

procedure TMyDUnitTestSetup.CheckEqualsString_Is_Case_Sensitive;
begin
  Assert.WillRaiseDescendant(
    procedure
    begin
{$WARN SYMBOL_DEPRECATED OFF}
      CheckEqualsString('abc', 'ABC');
{$WARN SYMBOL_DEPRECATED ON}
    end, Assert.TestFailure);
end;

procedure TMyDUnitTestSetup.CheckNotEquals_String_Passes_When_Only_Case_Differs;
var
  lower, upper : string;
begin
  lower := 'abc';
  upper := 'ABC';
{$WARN SYMBOL_DEPRECATED OFF}
  CheckNotEquals(lower, upper);
  CheckNotEqualsString(lower, upper);
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

procedure TMyDUnitTestSetup.Test_TestName;
begin
  Status('The Test Name is : ' + Self.Name);
  CheckEqualsString('Test_TestName', Self.Name, 'Test Name not working');
end;

procedure TMyDUnitTestSetup.CheckEqualsBin_Fails_When_Values_Differ_Above_Digits;
begin
  //DUnit compares the values; digits only sets how many binary digits the failure message shows
  Assert.WillRaiseDescendant(
    procedure
    begin
{$WARN SYMBOL_DEPRECATED OFF}
      CheckEqualsBin($100, $000, '', 8);
{$WARN SYMBOL_DEPRECATED ON}
    end, Assert.TestFailure);
end;

initialization
  TDUnitX.RegisterTestFixture(TMyDUnitTest);
  TDUnitX.RegisterTestFixture(TMyDUnitTestSetup);

end.

