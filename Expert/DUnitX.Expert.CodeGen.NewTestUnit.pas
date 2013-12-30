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
begin
   if FCreateSetupTearDown then
   begin
     ShowMessage('1');
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
     ShowMessage('2');
     lSampleIntf := SSampleMethodsIntf;
     lSampleImpl := Format(SSampleMethodsImpl,[FTestFixureClassName]);
   end
   else
   begin
     lSampleIntf := '';
     lSampleImpl := '';
   end;

   ShowMessage('3');
   TSourceFile.Create(STestUnit,[ModuleIdent,FTestFixureClassName,lSetupTearDownIntf,
                                 lSampleIntf,lSetupTearDownImpl,lSampleImpl]);
   ShowMessage('4');
end;

end.
