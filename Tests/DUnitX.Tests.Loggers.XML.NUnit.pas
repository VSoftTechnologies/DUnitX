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

unit DUnitX.Tests.Loggers.XML.NUnit;


interface

uses
  Classes,
  DUnitX.TestFramework,
  DUnitX.Loggers.XML.NUnit;

type
  {$M+}
  TMockFileStream = class(TFileStream)
  private
    FFileName : string;
  public
    constructor Create(const AFileName: string);
    property Filename : string read FFilename;
  end;

  [TestFixture]
  TDUnitX_LoggerXMLNUnitTests = class
  public
    [Test]
    procedure CreateTest;
  end;

implementation


{ TDUnitX_LoggerXMLNUnit }

procedure TDUnitX_LoggerXMLNUnitTests.CreateTest;
var
  sut : IDUnitXXMLNUnitLogger;
  mockStream : TMockFileStream;
const
  TEST_FILE_NAME = 'DUnitX_TestFile';
begin
  mockStream := TMockFileStream.Create(TEST_FILE_NAME);
  sut := TDUnitXXMLNUnitLogger.Create(mockStream);

  Assert.IsNotNull(sut);
end;

{ TMockFileStream }

constructor TMockFileStream.Create(const AFileName: string);
begin
  FFileName := AFileName;
end;

initialization
  TDUnitX.RegisterTestFixture(TDUnitX_LoggerXMLNUnitTests);
end.
