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


unit DUnitX.MacOS.Console;

interface

uses
  classes,
  DUnitX.ConsoleWriter.Base;

type
  ///TODO : No idea if MacOS supports console coloring..!!! It does help to have
  ///  this here when figuring out the abstraction though.

  TDUnitXMacOSConsoleWriter = class(TDUnitXConsoleWriterBase)
  private
  protected
    procedure InternalWriteLn(const s : String); override;
  public
    procedure SetColour(const foreground: TConsoleColour; const background: TConsoleColour = ccDefault); override;
    constructor Create;override;
    destructor Destroy; override;
  end;


implementation

uses
  DUnitX.IoC;

{ TDUnitXMacOSConsoleWriter }

constructor TDUnitXMacOSConsoleWriter.Create;
begin
  inherited;

end;

destructor TDUnitXMacOSConsoleWriter.Destroy;
begin

  inherited;
end;

procedure TDUnitXMacOSConsoleWriter.InternalWriteLn(const s: String);
begin
  inherited;

end;

procedure TDUnitXMacOSConsoleWriter.SetColour(const foreground, background: TConsoleColour);
begin
  inherited;

end;

initialization
  {$IFDEF MACOSX}
  TDUnitXIoC.RegisterType<IDUnitXConsoleWriter,TDUnitXMacOSConsoleWriter>();
  {$ENDIF}

end.
