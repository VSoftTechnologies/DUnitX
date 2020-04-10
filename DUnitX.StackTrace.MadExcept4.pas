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

unit DUnitX.StackTrace.MadExcept4;

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
{$IFDEF USE_MADEXCEPT4}
  madStackTrace,
{$IFDEF CPUx64}
  madExcept,
{$ENDIF}
{$ENDIF}
  DUnitX.TestFramework;

type
  TMadExcept4StackTraceProvider = class(TInterfacedObject,IStacktraceProvider)
  public
    function GetStackTrace(const ex: Exception; const exAddressAddress: Pointer): string;
    function PointerToLocationInfo(const Addrs: Pointer): string;
    function PointerToAddressInfo(Addrs: Pointer): string;
  end;



implementation

uses
  DUnitX.IoC;

{ TMadExcept4StackTraceProvider }

function TMadExcept4StackTraceProvider.GetStackTrace(const ex: Exception; const exAddressAddress: Pointer): string;
{$IFDEF USE_MADEXCEPT4}
{$IFDEF CPUx64}
var
  stackTrace: TStackTrace;
{$ENDIF}
{$ENDIF}
begin
  result := '';
  {$IFDEF USE_MADEXCEPT4}
  {$IFDEF CPUx64}
    Result := madExcept.GetCrashStackTrace(false, true, true, @stackTrace); 
  {$ELSE}
    Result := madStackTrace.StackTrace( false, false, false, nil, nil,
                                             exAddressAddress, false,
                                             false, 0, 0, nil,
                                             @exAddressAddress);
  {$ENDIF}
  {$ENDIF}
end;

function TMadExcept4StackTraceProvider.PointerToAddressInfo(Addrs: Pointer): string;
begin
  {$IFDEF USE_MADEXCEPT4}
  Result := String(StackAddrToStr(Addrs));
  {$ELSE}
  Result := '';
  {$ENDIF}
end;

function TMadExcept4StackTraceProvider.PointerToLocationInfo(const Addrs: Pointer): string;
begin
  {$IFDEF USE_MADEXCEPT4}
  Result := String(StackAddrToStr(Addrs));
  {$ELSE}
  Result := '';
  {$ENDIF}
end;

initialization
{$IFDEF USE_MADEXCEPT4}

  {$IFDEF DELPHI_XE_UP}
    TDUnitXIoC.DefaultContainer.RegisterType<IStacktraceProvider,TMadExcept4StackTraceProvider>(true);
  {$ELSE}
    //D2010 bug prevents using above method.
    TDUnitXIoC.DefaultContainer.RegisterType<IStacktraceProvider>(true,
     function : IStacktraceProvider
        begin
          result := TMadExcept4StackTraceProvider.Create;
        end
     );
  {$ENDIF}
{$ENDIF}

end.
