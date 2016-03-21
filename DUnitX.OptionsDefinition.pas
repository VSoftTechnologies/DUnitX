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

unit DUnitX.OptionsDefinition;

interface

{$I DUnitX.inc}

uses
  DUnitX.TestFramework;

//Defines the command line options for DUnitX. Add any new options definitions

implementation

uses
  DUnitX.CommandLine.Options;


procedure RegisterOptions;
var
  def : IOptionDefintion;
begin
  //enable passing an options file containing options.
  def := TOptionsRegistry.RegisterOption<string>('options','opt','Options File',nil);
  def.IsOptionFile := true;

  def := TOptionsRegistry.RegisterOption<boolean>('hidebanner','b','Hide the License Banner',
                                          procedure(value : boolean)
                                          begin
                                             TDUnitX.Options.HideBanner := value;
                                          end);
  def.HasValue := false;

  TOptionsRegistry.RegisterOption<string>('xmlfile','xml','XML output file path',
                                          procedure(value :string)
                                          begin
                                             TDUnitX.Options.XMLOutputFile := value;
                                          end);

  TOptionsRegistry.RegisterOption<string>('runlist','rl','Specify the name of a file which lists the tests to run.',
                                          procedure(value :string)
                                          begin
                                             TDUnitX.Options.RunListFile := value;
                                          end);

  def := TOptionsRegistry.RegisterOption<string>('run','r','Specify the tests to run, separate by commas',
                                          procedure(value :string)
                                          begin
                                             TDUnitX.Options.Run.Add(value);
                                          end);
  def.AllowMultiple := true;

  TOptionsRegistry.RegisterOption<string>('include','i','Specify the categories to include',
                                          procedure(value :string)
                                          begin
                                             TDUnitX.Options.Include := value;
                                          end);

  TOptionsRegistry.RegisterOption<string>('exclude','e','Specify the categories to exclude',
                                          procedure(value :string)
                                          begin
                                             TDUnitX.Options.Exclude := value;
                                          end);

  def := TOptionsRegistry.RegisterOption<boolean>('dontshowignored','dsi','Don''t show ignored tests',
                                          procedure(value : boolean)
                                          begin
                                             TDUnitX.Options.DontShowIgnored := true;
                                          end);
  def.HasValue := false;



  TOptionsRegistry.RegisterOption<TLogLevel>('loglevel','l','Logging Level - Information, Warning, Error',
                                          procedure(value : TLogLevel)
                                          begin
                                             TDUnitX.Options.LogLevel := value
                                          end);

  TOptionsRegistry.RegisterOption<TDUnitXExitBehavior>('exitbehavior','exit','Exit behavior - Continue, Pause',
                                          procedure(value : TDUnitXExitBehavior)
                                          begin
                                             TDUnitX.Options.ExitBehavior := value
                                          end);

  def := TOptionsRegistry.RegisterOption<boolean>('h','?','Show Usage',
                                          procedure(value : boolean)
                                          begin
                                             TDUnitX.Options.ShowUsage := value;
                                          end);
  def.HasValue := False;

  def := TOptionsRegistry.RegisterUnNamedOption<string>('',
                                                        procedure(value :string)
                                                        begin
                                                           TDUnitX.Options.XMLOutputFile := value;
                                                        end);
  def.Hidden := True;

end;

initialization
  RegisterOptions;
end.
