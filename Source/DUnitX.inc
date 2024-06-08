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
  {$IF CompilerVersion > 24.0 } //XE4 or later
    {$LEGACYIFEND ON}
  {$IFEND}
{$ELSE}
  {$DEFINE UNSUPPORTED_COMPILER_VERSION}
{$ENDIF}

{$IFDEF UNSUPPORTED_COMPILER_VERSION}
  Unsupported Compiler Version (Delphi 2010 or later required!)
{$ENDIF}


{$IF CompilerVersion = 21.0} // 2010
  {$DEFINE DELPHI_2010}
{$IFEND}


{$IF CompilerVersion > 21.0} // XE up
  {$DEFINE DELPHI_XE_UP}
  {$DEFINE SUPPORTS_TARRAY}
{$IFEND}


{$IF CompilerVersion > 22.0} // XE2 up
  {$DEFINE DELPHI_XE2_UP}
  {$DEFINE USE_NS}
  {$DEFINE SUPPORTS_REGEX}
{$IFEND}

{$IF CompilerVersion < 23.0} // XE down
  {$DEFINE CPUX86} //only defined in XE2 or later
{$IFEND}

{$IF CompilerVersion > 23.0 } //XE3 or later
  {$DEFINE DELPHI_XE3_UP}
{$IFEND}

{$IF CompilerVersion = 24.0 }
  {$DEFINE DELPHI_XE3}
{$IFEND}

{$IF CompilerVersion > 24.0} // XE4 up
  {$DEFINE DELPHI_XE4_UP}
{$IFEND}

{$IF CompilerVersion > 26.0} // XE6 up
  {$DEFINE DELPHI_XE6_UP}
{$IFEND}



{$IF CompilerVersion > 28.0} // XE8 up
  {$DEFINE DELPHI_XE8_UP}
{$IFEND}

{$IF CompilerVErsion > 29.0}
  {$DEFINE DELPHIX_SEATTLE_UP} //used in expert
{$IFEND}

{$IF CompilerVersion > 32.0} // 10.3 up
  {$DEFINE DELPHI_XE103_UP}
{$IFEND}

{$IF CompilerVersion > 31.0} // 10.1 up
  {$DEFINE DELPHI_XE101_UP}
{$IFEND}


{$I DUnitX.Stacktrace.inc}
{$I DUnitX.MemoryLeaks.inc}

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

