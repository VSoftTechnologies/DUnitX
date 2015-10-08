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

unit DUnitX.Expert.CodeGen.NewTestUnit;

interface

{$I DUnitX.inc}

uses
  ToolsApi,
  DunitX.Expert.CodeGen.NewUnit;

type
  TNewTestUnit = class(TNewUnit)
  protected
    FCreateSetupTearDown : Boolean;
    FCreateSampleMethods : Boolean;
    FTestFixureClassName : String;
    function NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile; override;
  public
    constructor Create(ACreateSetupTearDown : boolean; ACreateSampleMethods : boolean;ATestFixureClassName : String);
  end;

implementation

uses
  {$IFDEF USE_NS}
  System.SysUtils,
  VCL.Dialogs,
  {$ELSE}
  SysUtils,
  Dialogs,
  {$ENDIF}
  DUnitX.Expert.CodeGen.Templates,
  DUnitX.Expert.CodeGen.SourceFile;

{ TNewTestUnit }

constructor TNewTestUnit.Create(ACreateSetupTearDown,
  ACreateSampleMethods: boolean;ATestFixureClassName : String);
begin
  Assert(Length(ATestFixureClassName) > 0);
  FAncestorName := '';
  FFormName := '';
  FImplFileName := '';
  FIntfFileName := '';
  FTestFixureClassName := ATestFixureClassName;
  FCreateSetupTearDown := ACreateSetupTearDown;
  FCreateSampleMethods := ACreateSampleMethods;
end;

function TNewTestUnit.NewImplSource(const ModuleIdent, FormIdent,
  AncestorIdent: string): IOTAFile;
var
  lSetupTearDownIntf : string;
  lSetupTearDownImpl : string;
  lSampleIntf : string;
  lSampleImpl : string;
  lUnitIdent, lFormName, lFileName : String;
begin
   if FCreateSetupTearDown then
   begin
     lSetupTearDownIntf := SSetupTearDownIntf;
     lSetupTearDownImpl := Format(SSetupTearDownImpl,[FTestFixureClassName]);
   end
   else
   begin
     lSetupTearDownIntf := '';
     lSetupTearDownImpl := '';
   end;

   if FCreateSampleMethods then
   begin
     lSampleIntf := SSampleMethodsIntf;
     lSampleImpl := Format(SSampleMethodsImpl,[FTestFixureClassName]);
   end
   else
   begin
     lSampleIntf := '';
     lSampleImpl := '';
   end;

   //ModuleIdent is blank for some reason.
   // http://stackoverflow.com/questions/4196412/how-do-you-retrieve-a-new-unit-name-from-delphis-open-tools-api
   // So using method mentioned by Marco Cantu.
   (BorlandIDEServices as IOTAModuleServices).GetNewModuleAndClassName( '', lUnitIdent, lFormName, lFileName);
   result := TSourceFile.Create(STestUnit,[lUnitIdent,FTestFixureClassName,lSetupTearDownIntf,
                                           lSampleIntf,lSetupTearDownImpl,lSampleImpl]);

end;

end.
