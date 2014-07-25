program DUnitXFiremonkeyGUI_XE5;

uses
  FMX.Forms,
  DUNitX.Loggers.GUIX in '..\DUNitX.Loggers.GUIX.pas' {GUIXTestRunner},
  DUnitX.Examples.EqualityAsserts in 'DUnitX.Examples.EqualityAsserts.pas',
  DUnitX.Examples.General in 'DUnitX.Examples.General.pas',
  NonNamespacedExample in 'NonNamespacedExample.pas',
  DUnitX.Generics in '..\DUnitX.Generics.pas',
  DUnitX.InternalInterfaces in '..\DUnitX.InternalInterfaces.pas',
  DUnitX.WeakReference in '..\DUnitX.WeakReference.pas',
  DUnitX.FixtureResult in '..\DUnitX.FixtureResult.pas',
  DUnitX.RunResults in '..\DUnitX.RunResults.pas',
  DUnitX.Test in '..\DUnitX.Test.pas',
  DUnitX.TestFixture in '..\DUnitX.TestFixture.pas',
  DUnitX.TestFramework in '..\DUnitX.TestFramework.pas',
  DUnitX.TestResult in '..\DUnitX.TestResult.pas',
  DUnitX.TestRunner in '..\DUnitX.TestRunner.pas',
  DUnitX.Utils in '..\DUnitX.Utils.pas',
  DUnitX.IoC in '..\DUnitX.IoC.pas',
  DUnitX.MemoryLeakMonitor.Default in '..\DUnitX.MemoryLeakMonitor.Default.pas',
  DUnitX.DUnitCompatibility in '..\DUnitX.DUnitCompatibility.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGUIXTestRunner, GUIXTestRunner);
  Application.Run;
end.
