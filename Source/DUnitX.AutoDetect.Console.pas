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

/// <summary>
///   Using this unit will auto detect the platform you are compiling for and
///   register the correct console unit.
/// </summary>
/// <remarks>
///   <para>
///     It is really just adding the correct unit DUnitX."PlatformName".Console
///     Unit to the project via a compiler define.
///   </para>
///   <para>
///     This cleans up the DPR uses by removing the need for {$IFDEF}
///     statements in the project uses clause. If a {$IFDEF} is in the project
///     uses clause and you add a new file Delphi does not know which area to
///     put it in and has to guess.
///   </para>
///   <para>
///     In versions prior to XE3 it would just remove {$IFDEF} statements
///     completely from your code.
///     <see href="http://qc.embarcadero.com/wc/qcmain.aspx?d=6294'" />
///   </para>
/// </remarks>
/// <seealso cref="DUnitX.Windows.Console" />
/// <seealso cref="DUnitX.MacOS.Console" />
/// <seealso_ cref="DUnitX.Linux.Console" />
/// <seealso cref="DUnitX.Loggers.Console" />

unit DUnitX.AutoDetect.Console;

interface
uses
 {$IF Defined(MSWINDOWS)}
     DUnitX.Windows.Console;
 {$ELSEIF Defined(MACOS) or Defined(OSX32)}
     // Simplification as MacOS console supports Ansi, and other terminals
     // on platforms other than windows typically support some form of
     // ANSI colors.
     DUnitX.MacOS.Console;
 {$ELSEIF Defined(LINUX) or Defined(ANDROID)}
     DUnitX.Linux.Console;
 {$ELSE}
     {$MESSAGE Error 'Unknown Platform for Console Writer'}
 {$IFEND}

implementation

end.
