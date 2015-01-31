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

unit DUnitX.Tests.MemoryLeaks;

interface

uses
  DUnitX.TestFramework;

type
  {$M+}
  [TestFixture]
  [IgnoreMemoryLeaks]
  TTestsNoMemoryLeaksReported = class
  published
    [Test]
    procedure No_Reporting_Of_Memory_Leaks;
    [Test]
    [IgnoreMemoryLeaks(False)]
    procedure Override_Reporting_Of_Memory_Leaks;
  end;

  [TestFixture]
  // if the "IgnoreMemoryLeaks" attribute is not specified, then all memory leaks will be reported.
  [IgnoreMemoryLeaks(False)]  // Report all leaks. This is the same as not specifying this attribute.
  TTestsMemoryLeaksReported = class
  published
    [Test]
    [IgnoreMemoryLeaks(True)]
    procedure Override_No_Reporting_Of_Memory_Leaks;

    [Test]
    // if the "IgnoreMemoryLeaks" attribute is not specified, it will use the fixtures setting.
    procedure Reporting_Of_Memory_Leaks;
  end;

implementation

{ TTestsNoMemoryLeaksReported }

procedure TTestsNoMemoryLeaksReported.No_Reporting_Of_Memory_Leaks;
begin
  AllocMem(1024);
  Assert.Pass;
end;

procedure TTestsNoMemoryLeaksReported.Override_Reporting_Of_Memory_Leaks;
begin
  AllocMem(512);
  Assert.Pass;
end;

{ TTestsMemoryLeaksReported }

procedure TTestsMemoryLeaksReported.Override_No_Reporting_Of_Memory_Leaks;
begin
  AllocMem(256);
  Assert.Pass;
end;

procedure TTestsMemoryLeaksReported.Reporting_Of_Memory_Leaks;
begin
  AllocMem(128);
  Assert.Pass;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestsNoMemoryLeaksReported);
  TDUnitX.RegisterTestFixture(TTestsMemoryLeaksReported);

end.
