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
{ This unit is contains ideas borrowed largely from NUnit                   }
{ Copyright © 2012-2014 Charlie Poole                                       }
{ License  : http://nunit.org/index.php?p=vsTestAdapterLicense&r=2.6.3      }
{                                                                           }
{***************************************************************************}

unit DUnitX.Types;

{$I DUnitX.inc}

interface

uses
{$IFDEF USE_NS}
  System.Rtti;
{$ELSE}
  Rtti;
{$ENDIF}

type
  {$IFDEF DELPHI_XE_UP}
  TValueArray = TArray<TValue>;
  {$ELSE}
  TValueArray = array of TValue;
  {$ENDIF}

  TExceptionInheritance = (exExact, exDescendant);

implementation

end.
