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
    constructor Create(const ACreateSetupTearDown : boolean; const ACreateSampleMethods : boolean;const ATestFixureClassName : String; const APersonality : String = '' );
  end;

  {$IFDEF DELPHIX_SEATTLE_UP}
  TNewTestUnitEx = class(TNewTestUnit)
    function NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile; override;
  end;
  {$ENDIF}


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

constructor TNewTestUnit.Create(const ACreateSetupTearDown : boolean; const ACreateSampleMethods : boolean;const ATestFixureClassName : String; const APersonality : String = '' );
begin
  Assert(Length(ATestFixureClassName) > 0);
  FAncestorName := '';
  FFormName := '';
  FImplFileName := '';
  FIntfFileName := '';
  FTestFixureClassName := ATestFixureClassName;
  FCreateSetupTearDown := ACreateSetupTearDown;
  FCreateSampleMethods := ACreateSampleMethods;
  {$IFDEF DELPHIX_SEATTLE_UP}
  Personality := APersonality;
  {$ENDIF}
end;

function TNewTestUnit.NewImplSource(const ModuleIdent, FormIdent,  AncestorIdent: string): IOTAFile;
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

{$IFDEF DELPHIX_SEATTLE_UP}
function TNewTestUnitEx.NewImplSource(const ModuleIdent, FormIdent,  AncestorIdent: string): IOTAFile;
var
  lSetupTearDownIntf : string;
  lSetupTearDownImpl : string;
  lSampleIntf : string;
  lSampleImpl : string;
  lUnitIdent, lFormName, lFileName, lTestUnit : String;
 begin
   if FCreateSetupTearDown then
    if Personality.isEmpty or SameText(Personality, sDelphiPersonality) then
    begin
     lTestUnit := STestUnit;

     lSetupTearDownIntf := SSetupTearDownIntf;
     lSetupTearDownImpl := Format(SSetupTearDownImpl,[FTestFixureClassName]);

     lSampleIntf := SSampleMethodsIntf;
     lSampleImpl := Format(SSampleMethodsImpl,[FTestFixureClassName]);
    end
    else
    if SameText(Personality, sCBuilderPersonality) then
    begin
      lTestUnit := STestCPPUnit;
      lSetupTearDownIntf := SSetupTearDownCPPIntf;
      lSetupTearDownImpl := Format(SSetupTearDownCPPImpl,[FTestFixureClassName]);
      lSampleIntf := SSampleMethodsCPPIntf;
      lSampleImpl := Format(SSampleMethodsCPPImpl,[FTestFixureClassName]);
    end;

    if not FCreateSetupTearDown then
    begin
      lSetupTearDownIntf := '';
      lSetupTearDownImpl := '';
    end;

   if not FCreateSampleMethods then
   begin
      lSampleIntf := '';
      lSampleImpl := '';
   end;
    // http://stackoverflow.com/questions/4196412/how-do-you-retrieve-a-new-unit-name-from-delphis-open-tools-api
    // So using method mentioned by Marco Cantu.
    (BorlandIDEServices as IOTAModuleServices).GetNewModuleAndClassName( '', lUnitIdent, lFormName, lFileName);
   result := TSourceFile.Create(lTestUnit,[lUnitIdent,FTestFixureClassName,lSetupTearDownIntf,
                                            lSampleIntf,lSetupTearDownImpl,lSampleImpl]);

end;
{$ENDIF}


end.
