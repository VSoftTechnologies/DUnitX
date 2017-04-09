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

 //Basic Version of Compiler Supported
{$IFDEF CONDITIONALEXPRESSIONS}  //Started being defined with D2009
  {$IF CompilerVersion < 21.0} // Before RAD Studio 2010
    {$DEFINE UNSUPPORTED_COMPILER_VERSION}
  {$IFEND}
{$ELSE}
  {$DEFINE UNSUPPORTED_COMPILER_VERSION}
{$ENDIF}

{$IFDEF UNSUPPORTED_COMPILER_VERSION}
  Unsupported Compiler Version (Delphi 2010 or later required!)
{$ENDIF}

// Pre-define all "and down" defs
{$DEFINE DELPHI_XE102_TOKYO_DOWN}
{$DEFINE DELPHI_XE101_BERLIN_DOWN}
{$DEFINE DELPHI_XE10_SEATTLE_DOWN}
{$DEFINE DELPHI_XE8_DOWN}
{$DEFINE DELPHI_XE7_DOWN}
{$DEFINE DELPHI_XE6_DOWN}
{$DEFINE DELPHI_XE5_DOWN}
{$DEFINE DELPHI_XE4_DOWN}
{$DEFINE DELPHI_XE3_DOWN}
{$DEFINE DELPHI_XE2_DOWN}
{$DEFINE DELPHI_XE_DOWN}
{$DEFINE DELPHI_2010_DOWN}

// *** VERSIONS ***
// 2010
{$IF CompilerVersion >= 21.0}
  {$IF CompilerVersion = 21.0}
    {$DEFINE DELPHI_2010}
  {$IFEND}
  {$DEFINE DELPHI_2010_UP}
{$IFEND}

// XE
{$IF CompilerVersion >= 22.0}
  {$IF CompilerVersion = 22.0}
    {$DEFINE DELPHI_XE}
  {$IFEND}
  {$DEFINE DELPHI_XE_UP}
  {$UNDEF DELPHI_2010_DOWN}
{$IFEND}

// XE2
{$IF CompilerVersion >= 23.0}
  {$IF CompilerVersion = 23.0}
    {$DEFINE DELPHI_XE2}
  {$IFEND}
  {$DEFINE DELPHI_XE2_UP}
  {$UNDEF DELPHI_XE_DOWN}
{$IFEND}

// XE3
{$IF CompilerVersion >= 24.0}
  {$IF CompilerVersion = 24.0}
    {$DEFINE DELPHI_XE3}
  {$IFEND}
  {$DEFINE DELPHI_XE3_UP}
  {$UNDEF DELPHI_XE2_DOWN}
{$IFEND}

// XE4
{$IF CompilerVersion >= 25.0}
  {$IF CompilerVersion = 25.0}
    {$DEFINE DELPHI_XE4}
  {$IFEND}
  {$DEFINE DELPHI_XE4_UP}
  {$UNDEF DELPHI_XE3_DOWN}
{$IFEND}

// XE5
{$IF CompilerVersion >= 26.0}
  {$IF CompilerVersion = 26.0}
    {$DEFINE DELPHI_XE5}
  {$IFEND}
  {$DEFINE DELPHI_XE5_UP}
  {$UNDEF DELPHI_XE4_DOWN}
{$IFEND}

// XE6
{$IF CompilerVersion >= 27.0}
  {$IF CompilerVersion = 27.0}
    {$DEFINE DELPHI_XE6}
  {$IFEND}
  {$DEFINE DELPHI_XE6_UP}
  {$UNDEF DELPHI_XE5_DOWN}
{$IFEND}

// XE7
{$IF CompilerVersion >= 28.0}
  {$IF CompilerVersion = 28.0}
    {$DEFINE DELPHI_XE7}
  {$IFEND}
  {$DEFINE DELPHI_XE7_UP}
  {$UNDEF DELPHI_XE6_DOWN}
{$IFEND}

// XE8
{$IF CompilerVersion >= 29.0}
  {$IF CompilerVersion = 29.0}
    {$DEFINE DELPHI_XE8}
  {$IFEND}
  {$DEFINE DELPHI_XE8_UP}
  {$UNDEF DELPHI_XE7_DOWN}
{$IFEND}

// XE 10.0 Seattle
{$IF CompilerVersion >= 30.0}
  {$IF CompilerVersion = 30.0}
    {$DEFINE DELPHI_XE10_SEATTLE}
  {$IFEND}
  {$DEFINE DELPHI_XE10_SEATTLE_UP}
  {$UNDEF DELPHI_XE8_DOWN}
{$IFEND}

// XE 10.1 Berlin
{$IF CompilerVersion >= 31.0}
  {$IF CompilerVersion = 31.0}
    {$DEFINE DELPHI_XE101_BERLIN}
  {$IFEND}
  {$DEFINE DELPHI_XE101_BERLIN_UP}
  {$UNDEF DELPHI_XE10_SEATTLE_DOWN}
{$IFEND}

// 10.2 Tokyo
{$IF CompilerVersion >= 32.0}
  {$IF CompilerVersion = 32.0}
    {$DEFINE DELPHI_XE102_TOKYO}
  {$IFEND}
  {$DEFINE DELPHI_XE102_TOKYO_UP}
  {$UNDEF DELPHI_XE101_BERLIN_DOWN}
{$IFEND}

// *** SPECIAL SETTINGS by version ***

// CPUX86 for XE, 2010
{$IFDEF DELPHI_XE_DOWN}
  {$DEFINE CPUX86}
{$ENDIF}

// Namespaces and RegEx XE2 and up
{$IFDEF DELPHI_XE2_UP}
  {$DEFINE USE_NS} //Use namespaces
  {$DEFINE SUPPORTS_REGEX}
{$ENDIF}

// Optional RegEx support via TRegExpr in XE, 2010
{$IFNDEF SUPPORTS_REGEX}
  {$IFDEF USE_TREGEXPR}
    {$DEFINE SUPPORTS_REGEX}
  {$ENDIF}
{$ENDIF}

// *** OTHER INCLUDES ***

{$I DUnitX.Stacktrace.inc}
{$I DUnitX.MemoryLeaks.inc}

// *** DEBUGGING ***
//turn off debug info so you do not step into DUNITX when debugging your tests

//Note from Ryan McGinty 04/25/2016 - I had legacy code in conditional IFDEF DEBUG
//using XE.  When I put in a directive of DUNITX-DEBUG in my project, it started
//making the tests for IFDEF DEBUG true as well.  I tested with an _ and had same
//result, so added one without a spacer and it seems to work.  Kept both values
//as to not break any backward compatbilities.
{$IFNDEF DUNITX-DEBUG}
  {$IFNDEF DUNITXDEBUG}
    {$D-}
  {$ENDIF}
{$ENDIF}
