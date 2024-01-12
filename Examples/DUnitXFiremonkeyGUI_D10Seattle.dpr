program DUnitXFiremonkeyGUI_D10Seattle;

uses
  FMX.Forms,
  DUNitX.Loggers.GUIX in '..\Source\DUNitX.Loggers.GUIX.pas' {GUIXTestRunner},
  DUnitX.Examples.EqualityAsserts in 'DUnitX.Examples.EqualityAsserts.pas',
  DUnitX.Examples.General in 'DUnitX.Examples.General.pas',
  NonNamespacedExample in 'NonNamespacedExample.pas',
  DUnitX.Generics in '..\Source\DUnitX.Generics.pas',
  DUnitX.InternalInterfaces in '..\Source\DUnitX.InternalInterfaces.pas',
  DUnitX.WeakReference in '..\Source\DUnitX.WeakReference.pas',
  DUnitX.FixtureResult in '..\Source\DUnitX.FixtureResult.pas',
  DUnitX.RunResults in '..\Source\DUnitX.RunResults.pas',
  DUnitX.Test in '..\Source\DUnitX.Test.pas',
  DUnitX.TestFixture in '..\Source\DUnitX.TestFixture.pas',
  DUnitX.TestFramework in '..\Source\DUnitX.TestFramework.pas',
  DUnitX.TestResult in '..\Source\DUnitX.TestResult.pas',
  DUnitX.TestRunner in '..\Source\DUnitX.TestRunner.pas',
  DUnitX.Utils in '..\Source\DUnitX.Utils.pas',
  DUnitX.IoC in '..\Source\DUnitX.IoC.pas',
  DUnitX.MemoryLeakMonitor.Default in '..\Source\DUnitX.MemoryLeakMonitor.Default.pas',
  DUnitX.DUnitCompatibility in '..\Source\DUnitX.DUnitCompatibility.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGUIXTestRunner, GUIXTestRunner);
  Application.Run;
end.
