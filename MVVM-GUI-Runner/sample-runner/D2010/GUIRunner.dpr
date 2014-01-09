program GUIRunner;
// This module was dynamically generated by the DUnitX GUI Runner Wizard.

uses
  Forms,
  DUnitX.Generics in '..\..\..\DUnitX.Generics.pas',
  DUnitX.GUIRunnerForm in '..\..\run-time\view\DUnitX.GUIRunnerForm.pas' {mfmGUIRunner},
  DUnitX.BaseExecutive in '..\..\run-time\backplane\DUnitX.BaseExecutive.pas',
  DUnitX.uExecutive in '..\DUnitX.uExecutive.pas',
  DUnitX.ViewModel_VCLForms in '..\..\run-time\view-model\DUnitX.ViewModel_VCLForms.pas',
  DUnitX.ViewModel_Tree in '..\..\run-time\view-model\DUnitX.ViewModel_Tree.pas',
  DUnitX.viewModel_LoggerContainer in '..\..\run-time\view-model\DUnitX.viewModel_LoggerContainer.pas',
  DUnitX.uTestSuiteVirtualTree in '..\..\run-time\view\plug-ins\trees\DUnitX.VirtualStringTree\DUnitX.uTestSuiteVirtualTree.pas',
  DUnitX.WeakReference in '..\..\..\DUnitX.WeakReference.pas',
  DUnitX.InternalInterfaces in '..\..\..\DUnitX.InternalInterfaces.pas',
  DUnitX.MemoryLeakMonitor.Default in '..\..\..\DUnitX.MemoryLeakMonitor.Default.pas',
  DUnitX.IoC in '..\..\..\DUnitX.IoC.pas',
  DUnitX.Test in '..\..\..\DUnitX.Test.pas',
  DUnitX.TestRunner in '..\..\..\DUnitX.TestRunner.pas',
  DUnitX.TestFixture in '..\..\..\DUnitX.TestFixture.pas',
  DUnitX.TestFramework in '..\..\..\DUnitX.TestFramework.pas',
  DUnitX.TestResult in '..\..\..\DUnitX.TestResult.pas',
  DUnitX.Utils in '..\..\..\DUnitX.Utils.pas',
  DUnitX.Detour in '..\..\..\DUnitX.Detour.pas',
  DUnitX.RunResults in '..\..\..\DUnitX.RunResults.pas',
  DUnitX.FixtureResult in '..\..\..\DUnitX.FixtureResult.pas',
  DUnitX.CommandLine in '..\..\..\DUnitX.CommandLine.pas',
  DUnitX.DUnitCompatibility in '..\..\..\DUnitX.DUnitCompatibility.pas',
  DUnitX.VirtualTrees in '..\..\External-libraries\VirtualTreeView\Source\DUnitX.VirtualTrees.pas',
  DUnitX.Utils.XML2 in '..\..\..\DUnitX.Utils.XML2.pas',
  DUnitX.Extensions in '..\..\..\DUnitX.Extensions.pas',
  DUnitX.Examples.General in '..\..\..\Examples\DUnitX.Examples.General.pas',
  DUnitX.udmVirtualTreeNonVisualSupport in '..\..\run-time\view\plug-ins\trees\DUnitX.udmVirtualTreeNonVisualSupport.pas' {dmVirtualTreeNonVisualSupport: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TmfmGUIRunner, mfmGUIRunner);
  Application.Run;
end.
