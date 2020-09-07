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

unit DUnitX.Exceptions;

interface

{$I DUnitX.inc}

uses
  {$IFDEF USE_NS}
  System.SysUtils,
  {$ELSE}
  SysUtils,
  {$ENDIF}
  DUnitX.ComparableFormat;

type
  ETestFrameworkException = class(Exception);

  ENotImplemented = class(ETestFrameworkException);

  //base exception for any internal exceptions which cause the test to stop
  EAbort = class(ETestFrameworkException);

  ETestFailure = class(EAbort);

  ETestFailureStrCompare = class(ETestFailure)
  private
    FActual: string;
    FExpected: string;
    FMsg: string;
    FFormat: TDUnitXComparableFormatClass;
  public
    property Actual: string read FActual;
    property Expected: string read FExpected;
    property Msg: string read FMsg;
    property Format: TDUnitXComparableFormatClass read FFormat;

    constructor Create(const aExpected, aActual, aMessage: string; const aFormat: TDUnitXComparableFormatClass); reintroduce;
  end;

  ETestPass = class(EAbort);
  ETimedOut = class(EAbort);
  ENoAssertionsMade = class(ETestFailure);

  ENoTestsRegistered = class(ETestFrameworkException);
  ECommandLineError = class(ETestFrameworkException);

implementation

uses
  DUnitX.ResStrs;

constructor ETestFailureStrCompare.Create(const aExpected, aActual, aMessage: string; const aFormat: TDUnitXComparableFormatClass);
begin
  FExpected := aExpected;
  FActual := aActual;
  FMsg := aMessage;
  FFormat := aFormat;
  inherited Create({$IFDEF USE_NS}System.{$ENDIF}SysUtils.Format(SNotEqualErrorStr,[aExpected, aActual, aMessage]));
end;

end.
