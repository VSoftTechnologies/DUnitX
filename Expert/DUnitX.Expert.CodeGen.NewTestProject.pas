{***************************************************************************}
{                                                                           }
{           DUnitX                                                          }
{                                                                           }
{           Copyright (C) 2013 Vincent Parrett                              }
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

unit DUnitX.Expert.CodeGen.NewTestProject;

interface
uses
  ToolsAPI,
  DUnitX.Expert.CodeGen.NewProject;

type
  TTestProjectFile = class(TNewProject)
  protected
    function NewProjectSource(const ProjectName: string): IOTAFile; override;
  public
    constructor Create;
  end;

implementation
uses
  DUnitX.Expert.CodeGen.SourceFile,
  DunitX.Expert.CodeGen.Templates;

{ TProjectFile }

constructor TTestProjectFile.Create;
begin
 //TODO: Figure out how to make this be TestProjectX where X is the next available.
 //Return Blank and the project will be 'ProjectX.dpr' where X is the next available number
  FFileName := '';
end;

function TTestProjectFile.NewProjectSource(const ProjectName: string): IOTAFile;
begin
  result := TSourceFile.Create(STestDPR,[ProjectName]);
end;

end.
