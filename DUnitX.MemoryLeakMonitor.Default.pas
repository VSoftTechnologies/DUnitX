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

unit DUnitX.MemoryLeakMonitor.Default;

interface

{$I DUnitX.inc}

uses
  {$IFDEF USE_NS}
  System.Classes,
  {$ELSE}
  Classes,
  {$ENDIF}
  DUnitX.TestFramework;

type
  TDUnitXDefaultMemoryLeakMonitor = class(TInterfacedObject,IMemoryLeakMonitor)
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



  procedure RegisterDefaultProvider;

implementation

uses
  DUnitX.IoC;


procedure RegisterDefaultProvider;
begin
  TDUnitXIoC.DefaultContainer.RegisterType<IMemoryLeakMonitor, TDUnitXDefaultMemoryLeakMonitor>;
end;


{ TDUnitXDefaultMemoryLeakMonitor }

procedure TDUnitXDefaultMemoryLeakMonitor.PostSetUp;
begin

end;

procedure TDUnitXDefaultMemoryLeakMonitor.PostTearDown;
begin

end;

procedure TDUnitXDefaultMemoryLeakMonitor.PostTest;
begin

end;

procedure TDUnitXDefaultMemoryLeakMonitor.PreSetup;
begin

end;

procedure TDUnitXDefaultMemoryLeakMonitor.PreTearDown;
begin

end;

procedure TDUnitXDefaultMemoryLeakMonitor.PreTest;
begin

end;

function TDUnitXDefaultMemoryLeakMonitor.SetUpMemoryAllocated: Int64;
begin
  Result := 0;
end;

function TDUnitXDefaultMemoryLeakMonitor.TearDownMemoryAllocated: Int64;
begin
  Result := 0;
end;

function TDUnitXDefaultMemoryLeakMonitor.TestMemoryAllocated: Int64;
begin
  Result := 0;
end;

end.
