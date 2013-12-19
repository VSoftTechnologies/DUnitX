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


type
  {$M+}
  [TestFixture]
   TTestRepeatAttr3Times = class
   private
     FTimesRun : Cardinal;
   public
     [Setup]
     procedure Setup;
     [Teardown]
     procedure TearDown;
     [Test]
     [RepeatTest(3)]
     procedure TestRepeat3TimesNoAssert;
     [Test]
     [RepeatTest(3)]
     procedure TestRepeat3TimesWithPassAssert;
   end;

   TTestRepeatAttrFailure = class
   private
     FTimesRun : Cardinal;
   public
     [Setup]
     procedure Setup;
     [Test]
     [RepeatTest(3)]
     procedure TestShouldFailOnFirst;
     [Test]
     [RepeatTest(3)]
     procedure TestShouldFailOnLast;
   end;

  {$M-}

implementation

uses
  SysUtils;
{ TDUnitXTestFixtureTests }

const
   RUN_3TIMES : Cardinal = 3;

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


{ TTestRepeatAttr3Times }

procedure TTestRepeatAttr3Times.Setup;
begin
  FTimesRun := 0;
end;

procedure TTestRepeatAttr3Times.TearDown;
begin
  Assert.AreEqual(RUN_3TIMES,FTimesRun);
end;


procedure TTestRepeatAttr3Times.TestRepeat3TimesNoAssert;
begin
  inc(FTimesRun);
end;

procedure TTestRepeatAttr3Times.TestRepeat3TimesWithPassAssert;
begin
  inc(FTimesRun);
  Assert.Pass('Passed');
end;


{ TTestRepeatAttrFailure }

procedure TTestRepeatAttrFailure.Setup;
begin

end;

procedure TTestRepeatAttrFailure.TestShouldFailOnFirst;
begin
  inc(FTimesRun);
  if FTimesRun = 1 then
     Assert.Fail('EXPECTED Failure')
  else
     Assert.Fail('Ran More Times that expected.')
end;

procedure TTestRepeatAttrFailure.TestShouldFailOnLast;
begin
  inc(FTimesRun);
  if FTimesRun = 3 then
     Assert.Fail('EXPECTED Failure')
end;

initialization
  TDUnitX.RegisterTestFixture(TTestClassWithNonPublicSetup);
  TDUnitX.RegisterTestFixture(TTestRepeatAttr3Times);
  {$IFNDEF CI}
  TDUnitX.RegisterTestFixture(TTestRepeatAttrFailure);
  {$ENDIF}


end.
