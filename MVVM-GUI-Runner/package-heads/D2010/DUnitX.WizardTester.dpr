program DUnitX.WizardTester;

uses
  Forms,
  DUnitX.mfmWizardTester in '..\..\design-time\pascal\DUnitX.mfmWizardTester.pas' {mfmWizardTester},
  DUnitX.fmProjectWizard in '..\..\design-time\pascal\DUnitX.fmProjectWizard.pas' {fmProjectWizard},
  DUnitX.IoC in '..\..\..\DUnitX.IoC.pas',
  DUnitX.Generics in '..\..\..\DUnitX.Generics.pas',
  DUnitX.frmMainDetailsPage in '..\..\design-time\pascal\DUnitX.frmMainDetailsPage.pas' {frmMainDetailsPage: TFrame},
  DUnitX.WizardPageIntf in '..\..\design-time\pascal\DUnitX.WizardPageIntf.pas',
  DUnitX.LaunchWizardForm in '..\..\design-time\pascal\DUnitX.LaunchWizardForm.pas',
  DUnitX.frmMultipleChoicePage in '..\..\design-time\pascal\DUnitX.frmMultipleChoicePage.pas' {frmMultipleChoice: TFrame},
  DUnitX.WizardStates in '..\..\design-time\pascal\DUnitX.WizardStates.pas',
  DUnitX.frmPathPage in '..\..\design-time\pascal\DUnitX.frmPathPage.pas' {frmPathPage: TFrame},
  DUnitX.frmReadMemoPage in '..\..\design-time\pascal\DUnitX.frmReadMemoPage.pas' {frmReadMemo: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'DUnitX Wizard Tester App';
  Application.CreateForm(TmfmWizardTester, mfmWizardTester);
  Application.CreateForm(TfmProjectWizard, fmProjectWizard);
  Application.Run;
end.
