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

unit DUnitX.Tests.CommandLine;

interface

uses
  DUnitX.TestFramework,
  Classes;

type
  {$M+}
  [TestFixture]
  TDUnitX_CommandLineTests = class
  public
    [Test]
    procedure Passing_Fixture_Param_On_CommandLine_Fills_Fixtures_To_Run_List_With_Fixtures_Supplied;
  end;

implementation

uses
  SysUtils,
  DUnitX.CommandLine;

procedure TDUnitX_CommandLineTests.Passing_Fixture_Param_On_CommandLine_Fills_Fixtures_To_Run_List_With_Fixtures_Supplied;
begin
  {$Message 'TODO: Test the command line class'}
  //Currently its not possible to test the command line class. The reason for this
  //is that we can only access a single global variable which is the one used by
  //the testing framework. This would mean changing the level of exposure for the
  //the TCommandLine class. Its either that or some conditional trickery when testing
  //ourselves. Testing the runner external interface isn't enough in this case
  //as the runner uses the singleton being used to run the tests.

  //  commandLineSUT := TCommandLine.Create;
  //  try
  //  finally
  //    FreeAndNil(commandLineSUT);
  //  end;
end;



initialization
  TDUnitX.RegisterTestFixture(TDUnitX_CommandLineTests);
end.
