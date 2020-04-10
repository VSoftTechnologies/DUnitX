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

unit DUnitX.StackTrace.JCL;

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
{$IFDEF USE_JCL}
  JclDebug,
{$ENDIF}
  DUnitX.TestFramework;

type
  TJCLStackTraceProvider = class(TInterfacedObject,IStacktraceProvider)
  protected
    function GetStackTrace(const ex: Exception; const exAddressAddress: Pointer): string;
    function PointerToLocationInfo(const Addrs: Pointer): string;
    function PointerToAddressInfo(Addrs: Pointer): string;
  end;



implementation

uses
  DUnitX.IoC;

{ TJCLStackTraceProvider }

function TJCLStackTraceProvider.GetStackTrace(const ex: Exception; const exAddressAddress: Pointer): string;
{$IFDEF USE_JCL}
var
  traceList: TStrings;
{$ENDIF}
begin
  result := '';
  {$IFDEF USE_JCL}
  traceList := TStringList.Create;
  try
    JclDebug.JclLastExceptStackListToStrings(traceList, true);
    Result := traceList.Text;
  finally
    traceList.Free;
  end;
  {$ENDIF}
end;

function TJCLStackTraceProvider.PointerToAddressInfo(Addrs: Pointer): string;
{$IFDEF USE_JCL}
var
  _file,
  _module,
  _proc: string;
  _line: integer;
{$ENDIF}
begin
  Result := '';
{$IFDEF USE_JCL}
  JclDebug.MapOfAddr(Addrs, _file, _module, _proc, _line);
  Result := Format('%s$%p', [_proc, Addrs]);
{$ENDIF}
end;

//Borrowed from DUnit.
function TJCLStackTraceProvider.PointerToLocationInfo(const Addrs: Pointer): string;
{$IFDEF USE_JCL}
var
  _file,
  _module,
  _proc: string;
  _line: integer;
{$ENDIF}
begin
  Result := '';
{$IFDEF USE_JCL}
  JclDebug.MapOfAddr(Addrs, _file, _module, _proc, _line);

  if _file <> '' then
    Result   := Format('%s:%d', [_file, _line])
  else
    Result   := _module;
{$ENDIF}
end;

initialization
{$IFDEF USE_JCL}

  {$IFDEF DELPHI_XE_UP}
    TDUnitXIoC.DefaultContainer.RegisterType<IStacktraceProvider,TJCLStackTraceProvider>(true);
  {$ELSE}
    //D2010 bug prevents using above method.
    TDUnitXIoC.DefaultContainer.RegisterType<IStacktraceProvider>(true,
     function : IStacktraceProvider
        begin
          result := TJCLStackTraceProvider.Create;
        end
     );
  {$ENDIF}
{$ENDIF}

end.
