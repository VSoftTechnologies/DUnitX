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




/// <summary>
///   Adding this unit to your project will register console support for the
///   Mac OS.
/// </summary>
/// <remarks>
///   It is recommended to add the <see cref="DUnitX.AutoDetect.Console" /> to
///   your project uses instead of this unit directly.
/// </remarks>
/// <seealso cref="DUnit.AutoDetect.Console" />
/// <seealso cref="DUnit.Windows.Console" />
/// <seealso cref="DUnitX.Loggers.Console" />
unit DUnitX.MacOS.Console;

interface

{$I DUnitX.inc}

uses
  {$IFDEF USE_NS}
  System.Classes,
  {$ELSE}
  Classes,
  {$ENDIF}
  DUnitX.ConsoleWriter.Base;


type
   /// <summary>
   ///   Internal Class and should not be created directly. Adding this unit to
   ///   the uses will automatically register this class as a
   ///   <see cref="DUnitX.ConsoleWriter.Base|IDUnitXConsoleWriter">IDUnitXConsoleWriter</see>
   ///    in the <see cref="TDUnitXIoC|TDUnitXIoC">DUnitX IOC Container</see>.
   /// </summary>
   TDUnitXMacOSConsoleWriter = class(TDUnitXConsoleWriterBase)
  private
  protected
    procedure InternalWriteLn(const s : String); override;
    procedure InternalWrite(const s : string); override;
  public
    procedure SetColour(const foreground: TConsoleColour; const background: TConsoleColour = ccDefault); override;
  end;




implementation

uses
 DUnitX.IoC;


const

// AT = Ansi Terminal
// FG = Foreground
// BG = Background'

 AT_NO_ATTRIB  = #27 + '[0m';

 AT_BOLD       = #27 + '[1m';

 AT_FG_BLACK   = #27 + '[30m';
 AT_FG_RED     = #27 + '[31m';
 AT_FG_GREEN   = #27 + '[32m';
 AT_FG_YELLOW  = #27 + '[33m';
 AT_FG_BLUE    = #27 + '[34m';
 AT_FG_MAGENTA = #27 + '[35m';
 AT_FG_CYAN    = #27 + '[36m';
 AT_FG_WHITE   = #27 + '[37m';

 AT_BG_BLACK   = #27 + '[40m';
 AT_BG_RED     = #27 + '[41m';
 AT_BG_GREEN   = #27 + '[42m';
 AT_BG_YELLOW  = #27 + '[43m';
 AT_BG_BLUE    = #27 + '[44m';
 AT_BG_MAGENTA = #27 + '[45m';
 AT_BG_CYAN    = #27 + '[46m';
 AT_BG_WHITE   = #27 + '[47m';


{ TDUnitXMacOSConsoleWriter }

procedure TDUnitXMacOSConsoleWriter.InternalWrite(const s: string);
begin
  System.Write(s);
end;

procedure TDUnitXMacOSConsoleWriter.InternalWriteLn(const s: String);
begin
  System.Writeln(s);
end;

procedure TDUnitXMacOSConsoleWriter.SetColour(const foreground, background: TConsoleColour);
begin
  // Background colors on behave strangely with writeln
  // So I decided to always reset the attributes and ignore the background request
  // A great side effect is that themed terminals behave as expected,
  // as the background color can vary.
  // Also some colors to map to the default Ansi constant, so those
  // color request are ignored.

  // Note if you use a themed terminal with a red or green background it's going
  // to look bad.

  System.Write(AT_NO_ATTRIB);
  // If a Bright color set Bold
  if foreground in [ccBrightRed,ccBrightBlue,ccBrightGreen,ccBrightYellow,
                    ccBrightAqua,ccBrightAqua,ccBrightPurple,ccBrightWhite] then
     System.Write(AT_BOLD);

  case foreground of
    ccBrightRed,
    ccDarkRed      : System.Write(AT_FG_RED);
    ccBrightBlue,
    ccDarkBlue     : System.Write(AT_FG_BLUE);
    ccBrightGreen,
    ccDarkGreen    : System.Write(AT_FG_GREEN);
    ccBrightYellow,
    ccDarkYellow   : System.Write(AT_FG_YELLOW);
    ccBrightAqua,
    ccDarkAqua     : System.Write(AT_FG_CYAN);
    ccBrightPurple,
    ccDarkPurple   : System.Write(AT_FG_MAGENTA);

    // Not Set:  default background in terminal.app is white and a white
    //           background on white FG is hard to read.
    //   ccBrightWhite,
    //     ccWhite        : System.Write(AT_FG_WHITE);
  end;

end;

{$IF Defined(MACOS) or Defined(OSX32)}
 initialization
  TDUnitXIoC.DefaultContainer.RegisterType<IDUnitXConsoleWriter,TDUnitXMacOSConsoleWriter>();
{$IFEND}

end.
