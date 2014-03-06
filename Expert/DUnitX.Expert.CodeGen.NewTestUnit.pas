unit DUnitX.Expert.CodeGen.NewTestUnit;

interface
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
  SysUtils,
  Dialogs,
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
