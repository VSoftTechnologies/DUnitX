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

unit DUnitX.MemoryLeakMonitor.FastMM4;

interface

{$I DUnitX.inc}


uses
  {$IFDEF USE_NS}
  System.Classes,
  {$ELSE}
  Classes,
  {$ENDIF}
  DUnitX.TestFramework;

implementation

uses
  DUnitX.MemoryLeakMonitor.Default,
  DUnitX.IoC;

type
  TDUnitXFastMM4MemoryLeakMonitor = class(TInterfacedObject,IMemoryLeakMonitor)
  private
    FPreSetupAllocation : Int64;
    FPostSetupAllocation : Int64;
    FPreTestAllocation : Int64;
    FPostTestAllocation : Int64;
    FPreTearDownAllocation : Int64;
    FPostTearDownAllocation : Int64;

    function GetMemoryAllocated() : Int64;
  public
    procedure PreSetup;
    procedure PostSetUp;
    procedure PreTest;
    procedure PostTest;
    procedure PreTearDown;
    procedure PostTearDown;

    function SetUpMemoryAllocated: Int64;
    function TearDownMemoryAllocated: Int64;
    function TestMemoryAllocated: Int64;
  end;



{ TDUnitXFastMM4MemoryLeakMonitor }

function TDUnitXFastMM4MemoryLeakMonitor.GetMemoryAllocated: Int64;
var
  st: TMemoryManagerState;
  sb: TSmallBlockTypeState;
begin
  GetMemoryManagerState(st);

  Result := st.TotalAllocatedMediumBlockSize + st.TotalAllocatedLargeBlockSize;

  for sb in st.SmallBlockTypeStates do
  begin
    Result := Result + Int64(sb.UseableBlockSize * sb.AllocatedBlockCount);
  end;
end;

procedure TDUnitXFastMM4MemoryLeakMonitor.PostSetUp;
begin
  FPostSetupAllocation := GetMemoryAllocated();
end;

procedure TDUnitXFastMM4MemoryLeakMonitor.PostTearDown;
begin
  FPostTearDownAllocation := GetMemoryAllocated();
end;

procedure TDUnitXFastMM4MemoryLeakMonitor.PostTest;
begin
  FPostTestAllocation := GetMemoryAllocated();
end;

procedure TDUnitXFastMM4MemoryLeakMonitor.PreSetup;
begin
  FPreSetupAllocation := GetMemoryAllocated();
end;

procedure TDUnitXFastMM4MemoryLeakMonitor.PreTearDown;
begin
  FPreTearDownAllocation := GetMemoryAllocated();
end;

procedure TDUnitXFastMM4MemoryLeakMonitor.PreTest;
begin
  FPreTestAllocation := GetMemoryAllocated();
end;

function TDUnitXFastMM4MemoryLeakMonitor.SetUpMemoryAllocated: Int64;
begin
  Result := FPostSetupAllocation - FPreSetupAllocation;
end;

function TDUnitXFastMM4MemoryLeakMonitor.TearDownMemoryAllocated: Int64;
begin
  Result := FPostTearDownAllocation - FPreTearDownAllocation;
end;

function TDUnitXFastMM4MemoryLeakMonitor.TestMemoryAllocated: Int64;
begin
  Result := FPostTestAllocation - FPreTestAllocation;
end;

initialization
{$IFDEF USE_FASTMM4_LEAK_MONITOR}
  TDUnitXIoC.DefaultContainer.RegisterType<IMemoryLeakMonitor>(
    function : IMemoryLeakMonitor
    begin
      result := TDUnitXFastMM4MemoryLeakMonitor.Create;
    end);
{$ENDIF}

end.

