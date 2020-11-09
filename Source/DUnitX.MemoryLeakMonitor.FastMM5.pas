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

unit DUnitX.MemoryLeakMonitor.FastMM5;

interface

{$I DUnitX.inc}

uses DUnitX.TestFramework;

implementation

uses System.SysUtils, DUnitX.IoC, FastMM5;

type
  TDUnitXFastMM5MemoryLeakMonitor = class(TInterfacedObject, IMemoryLeakMonitor)
  private
    FPreSetupAllocation: Int64;
    FPostSetupAllocation: Int64;
    FPreTestAllocation: Int64;
    FPostTestAllocation: Int64;
    FPreTearDownAllocation: Int64;
    FPostTearDownAllocation: Int64;

    function GetMemoryAllocated: Int64;
  public
    function SetUpMemoryAllocated: Int64;
    function TearDownMemoryAllocated: Int64;
    function TestMemoryAllocated: Int64;

    procedure PostSetUp;
    procedure PostTearDown;
    procedure PostTest;
    procedure PreSetup;
    procedure PreTearDown;
    procedure PreTest;
  end;

{ TDUnitXFastMM5MemoryLeakMonitor }

function TDUnitXFastMM5MemoryLeakMonitor.GetMemoryAllocated: Int64;
begin
  Result := FastMM_GetUsageSummary.AllocatedBytes;
end;

procedure TDUnitXFastMM5MemoryLeakMonitor.PostSetUp;
begin
  FPostSetupAllocation := GetMemoryAllocated;
end;

procedure TDUnitXFastMM5MemoryLeakMonitor.PostTearDown;
begin
  FPostTearDownAllocation := GetMemoryAllocated;
end;

procedure TDUnitXFastMM5MemoryLeakMonitor.PostTest;
begin
  FPostTestAllocation := GetMemoryAllocated;
end;

procedure TDUnitXFastMM5MemoryLeakMonitor.PreSetup;
begin
  FPreSetupAllocation := GetMemoryAllocated;
end;

procedure TDUnitXFastMM5MemoryLeakMonitor.PreTearDown;
begin
  FPreTearDownAllocation := GetMemoryAllocated;
end;

procedure TDUnitXFastMM5MemoryLeakMonitor.PreTest;
begin
  FPreTestAllocation := GetMemoryAllocated;
end;

function TDUnitXFastMM5MemoryLeakMonitor.SetUpMemoryAllocated: Int64;
begin
  Result := FPostSetupAllocation - FPreSetupAllocation;
end;

function TDUnitXFastMM5MemoryLeakMonitor.TearDownMemoryAllocated: Int64;
begin
  Result := FPostTearDownAllocation - FPreTearDownAllocation;
end;

function TDUnitXFastMM5MemoryLeakMonitor.TestMemoryAllocated: Int64;
begin
  Result := FPostTestAllocation - FPreTestAllocation;
end;

initialization
  TDUnitXIoC.DefaultContainer.RegisterType<IMemoryLeakMonitor>(
    function: IMemoryLeakMonitor
    begin
      if FastMM_GetInstallationState <> mmisInstalled then
        raise Exception.Create('You must add FastMM5.pas in your project as the first''s unit in .dpr! Check and try to use again!');
      
      Result := TDUnitXFastMM5MemoryLeakMonitor.Create;
    end);

end.
