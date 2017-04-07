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

unit DUnitX.Tests.Utils.Files;

interface

{$I ..\DUnitX.inc}

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TTestFileUtils = class(TObject)
  public
    [Test]
    procedure FileExists_Should_Be_True;
    [Test]
    procedure FileExists_Should_Be_False;
    [Test]
    procedure DirectoryExists_Should_Be_True;
    [Test]
    procedure DirectoryExists_Should_Be_False;
    [Test]
    procedure TempDirectory_Test;
  end;

implementation

uses
  DUnitX.Utils.Files,
  {$IFDEF USE_NS}
  System.SysUtils;
  {$ELSE}
  SysUtils;
  {$ENDIF}

procedure TTestFileUtils.DirectoryExists_Should_Be_False;
begin
  Assert.IsFalse(FileUtils.DirectoryExists('x:\dir_that_does_not_exist\'));
end;

procedure TTestFileUtils.DirectoryExists_Should_Be_True;
begin
  Assert.IsTrue(FileUtils.DirectoryExists(ExtractFilePath(ParamStr(0))));
end;

procedure TTestFileUtils.FileExists_Should_Be_False;
begin
  Assert.IsFalse(FileUtils.FileExists('x:\file_that_does_not_exist.xyz'));
end;

procedure TTestFileUtils.FileExists_Should_Be_True;
begin
  //Checks for this exe
  Assert.IsTrue(FileUtils.FileExists(ParamStr(0)));
end;

procedure TTestFileUtils.TempDirectory_Test;
begin
  Assert.IsNotEmpty(FileUtils.TempDirectory);
  FileAssert.DirectoryDoesExist(FileUtils.TempDirectory);
  Assert.EndsWith('\', FileUtils.TempDirectory);
end;

initialization
  TDUnitX.RegisterTestFixture(TTestFileUtils);

end.
