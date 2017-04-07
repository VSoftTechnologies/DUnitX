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

unit DUnitX.Tests.Utils;

interface

{$I ..\DUnitX.inc}

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TTestsUtils = class(TObject)
  public
    [Test]
    procedure Test_Constructor_Detection_Non_Explicit;
    [Test]
    procedure Test_Constructor_Detection_Simple;
    [Test]
    procedure Test_Constructor_Detection_Complex;
  end;

implementation

uses
  {$IFDEF USE_NS}
  System.Rtti,
  {$ELSE}
  Rtti,
  {$ENDIF}
  DUnitX.Utils;

type
  TNonExplicit = class(TObject);

  TSimple = class(TObject)
  public
    constructor ThisIsNotIt(const Param: string);
    constructor ThisIsIt;
  end;

  TComplex1 = class(TSimple);

  TComplex2 = class(TSimple)
  public
    constructor ThisIsNotIt2(const Param: string);
    constructor ThisIsIt2;
  end;

procedure TTestsUtils.Test_Constructor_Detection_Non_Explicit;
var
  FixtureType: TRttiType;
  RttiContext: TRttiContext;
  Method: TRttiMethod;
begin
  RttiContext := TRttiContext.Create;
  try
    FixtureType := RttiContext.GetType(TNonExplicit);
    Assert.IsTrue(FixtureType.TryGetConstructor(Method, True));
    Assert.AreEqual('Create', Method.Name);
  finally
    RttiContext.Free;
  end;
end;

procedure TTestsUtils.Test_Constructor_Detection_Simple;
var
  FixtureType: TRttiType;
  RttiContext: TRttiContext;
  Method: TRttiMethod;
begin
  RttiContext := TRttiContext.Create;
  try
    FixtureType := RttiContext.GetType(TSimple);
    Assert.IsTrue(FixtureType.TryGetConstructor(Method));
    Assert.AreEqual('ThisIsIt', Method.Name);
  finally
    RttiContext.Free;
  end;
end;

procedure TTestsUtils.Test_Constructor_Detection_Complex;
var
  FixtureType: TRttiType;
  RttiContext: TRttiContext;
  Method: TRttiMethod;
begin
  RttiContext := TRttiContext.Create;
  try
    FixtureType := RttiContext.GetType(TComplex1);
    Assert.IsFalse(FixtureType.TryGetConstructor(Method, False));

    FixtureType := RttiContext.GetType(TComplex1);
    Assert.IsTrue(FixtureType.TryGetConstructor(Method, True));
    Assert.AreEqual('ThisIsIt', Method.Name);

    FixtureType := RttiContext.GetType(TComplex2);
    Assert.IsTrue(FixtureType.TryGetConstructor(Method, False));
    Assert.AreEqual('ThisIsIt2', Method.Name);

    FixtureType := RttiContext.GetType(TComplex2);
    Assert.IsTrue(FixtureType.TryGetConstructor(Method, True));
    Assert.AreEqual('ThisIsIt2', Method.Name);
  finally
    RttiContext.Free;
  end;
end;

constructor TSimple.ThisIsIt;
begin
  //empty
end;

constructor TSimple.ThisIsNotIt(const Param: string);
begin
  //empty
end;

constructor TComplex2.ThisIsIt2;
begin
  //empty
end;

constructor TComplex2.ThisIsNotIt2(const Param: string);
begin
  //empty
end;

initialization
  TDUnitX.RegisterTestFixture(TTestsUtils);

end.
