{***************************************************************************}
{                                                                           }
{           DUnitX                                                          }
{                                                                           }
{           Copyright (C) 2017 Vincent Parrett & Contributors               }
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

unit DUnitX.Init;

// In order to workaround the Delphi XE3 Bug noted at
// https://github.com/VSoftTechnologies/DUnitX/issues/117),
// you need to add the unit DUnitX.Init to your test projects - all
// other versions do this in the initialization of DUnitX.TestFramework.
// This file is safe to include in ALL projects as it with IFDEF out
// if not running under XE3.

interface

{$I DUnitX.inc}

{$IFDEF DELPHI_XE3}

uses
  DUnitX.TestFramework,
  DUnitX.FixtureProviderPlugin;

{$ENDIF}

implementation

{$IFDEF DELPHI_XE3}

uses
  DUnitX.Exceptions;

procedure InitAssert;
begin
  DUnitX.TestFramework.Assert.TestFailure := ETestFailure;
  DUnitX.TestFramework.Assert.TestPass := ETestPass;
end;

initialization
  TDUnitX.RegisterPlugin(TDUnitXFixtureProviderPlugin.Create);
  InitAssert;

{$ENDIF}

end.
