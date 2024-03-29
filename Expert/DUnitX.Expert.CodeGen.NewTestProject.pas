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

unit DUnitX.Expert.CodeGen.NewTestProject;

interface

{$I DUnitX.inc}

uses
  ToolsAPI,
  DUnitX.Expert.CodeGen.NewProject;

type
  TReportLeakOptions = (rloNone, rloFastMM4, rloFastMM5);

  TTestProjectFile = class({$IFNDEF DELPHI_SEATTLE_UP}TNewProject{$ELSE}TNewProjectEx{$ENDIF})
  private
    FReportLeakOptions: TReportLeakOptions;
  protected
    function NewProjectSource(const ProjectName: string): IOTAFile; override;
  public
    constructor Create(const ReportLeakOptions: TReportLeakOptions); overload;
    constructor Create(const APersonality: String; const ReportLeakOptions: TReportLeakOptions); overload;
  end;

implementation

uses
  DUnitX.Expert.CodeGen.SourceFile,
  DunitX.Expert.CodeGen.Templates,
  {$IFDEF USE_NS}
  System.SysUtils;
  {$ELSE}
  SysUtils;
  {$ENDIF}

const
  REPORT_LEAK_DECLARATION: array[TReportLeakOptions] of String = ('', '  FastMM4,'#13#10'  DUnitX.MemoryLeakMonitor.FastMM4,'#13#10, '  FastMM5,'#13#10'  DUnitX.MemoryLeakMonitor.FastMM5,'#13#10);

{ TProjectFile }

constructor TTestProjectFile.Create(const ReportLeakOptions: TReportLeakOptions);
begin
 //TODO: Figure out how to make this be TestProjectX where X is the next available.
 //Return Blank and the project will be 'ProjectX.dpr' where X is the next available number
  FFileName := '';
  FReportLeakOptions := ReportLeakOptions;
end;

constructor TTestProjectFile.Create(const APersonality: String; const ReportLeakOptions: TReportLeakOptions);
begin
  Create(ReportLeakOptions);
  {$IFDEF DELPHI_SEATTLE_UP}
  Personality := APersonality;
  {$ENDIF}
end;

function TTestProjectFile.NewProjectSource(const ProjectName: string): IOTAFile;
{$IFDEF DELPHI_SEATTLE_UP}
var
  TestProjectCode: string;
{$ENDIF}
begin
  {$IFNDEF DELPHI_SEATTLE_UP}
  result := TSourceFile.Create(STestDPR,[ProjectName, REPORT_LEAK_DECLARATION[FReportLeakOptions]]);
  {$ELSE}
  if Personality.isEmpty or SameText(Personality, sDelphiPersonality) then
    TestProjectCode := STestDPR
  else
    if SameText(Personality, sCBuilderPersonality) then
      TestProjectCode := STestCBPROJ;

  result := TSourceFile.Create(TestProjectCode, [ProjectName, REPORT_LEAK_DECLARATION[FReportLeakOptions]]);
  {$ENDIF}
end;

end.
