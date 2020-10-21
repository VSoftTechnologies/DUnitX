{***************************************************************************}
{                                                                           }
{           DUnitX                                                          }
{                                                                           }
{           Copyright (C) 2020 Vincent Parrett & Contributors               }
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

unit DUnitX.StackTrace.MadExcept5;

interface

{$I DUnitX.inc}

uses
{$IFDEF USE_NS}
  System.SysUtils,
  System.Classes,
{$ELSE}
  SysUtils,
  Classes,
{$ENDIF}
{$IFDEF USE_MADEXCEPT5}
  madStackTrace,
{$ENDIF}
  DUnitX.TestFramework;

type
  TMadExcept5StackTraceProvider = class(TInterfacedObject,IStacktraceProvider)
  public
    function GetStackTrace(const ex: Exception; const exAddressAddress: Pointer): string;
    function PointerToLocationInfo(const Addrs: Pointer): string;
    function PointerToAddressInfo(Addrs: Pointer): string;
  end;



implementation

uses
  DUnitX.IoC;

{ TMadExcept5StackTraceProvider }

function TMadExcept5StackTraceProvider.GetStackTrace(const ex: Exception; const exAddressAddress: Pointer): string;
begin
  result := '';
  {$IFDEF USE_MADEXCEPT5}
  Result := madStackTrace.StackTrace( false, false, false, nil, nil,
                                           exAddressAddress, false,
                                           false, 0, 0, nil,
                                           @exAddressAddress);
  {$ENDIF}
end;

function TMadExcept5StackTraceProvider.PointerToAddressInfo(Addrs: Pointer): string;
begin
  Result := '';
  {$IFDEF USE_MADEXCEPT5}
  Result := String(StackAddrToStr(Addrs));
  {$ENDIF}
end;

function TMadExcept5StackTraceProvider.PointerToLocationInfo(const Addrs: Pointer): string;
begin
   Result := '';
  {$IFDEF USE_MADEXCEPT5}
  Result := String(StackAddrToStr(Addrs));
  {$ENDIF}
end;

initialization
{$IFDEF USE_MADEXCEPT5}

  {$IFDEF DELPHI_XE_UP}
    TDUnitXIoC.DefaultContainer.RegisterType<IStacktraceProvider,TMadExcept5StackTraceProvider>(true);
  {$ELSE}
    //D2010 bug prevents using above method.
    TDUnitXIoC.DefaultContainer.RegisterType<IStacktraceProvider>(true,
     function : IStacktraceProvider
        begin
          result := TMadExcept5StackTraceProvider.Create;
        end
     );
  {$ENDIF}
{$ENDIF}

end.
