program DUnitXGuiTest;

uses
  SysUtils,
  Forms,
  DUnitX.Loggers.Console in '..\DUnitX.Loggers.Console.pas',
  DUnitX.Loggers.Text in '..\DUnitX.Loggers.Text.pas',
  DUnitX.MacOS.Console in '..\DUnitX.MacOS.Console.pas',
  DUnitX.Windows.Console in '..\DUnitX.Windows.Console.pas',
  DUnitX.ConsoleWriter.Base in '..\DUnitX.ConsoleWriter.Base.pas',
  DUnitX.Loggers.XML.NUnit in '..\DUnitX.Loggers.XML.NUnit.pas',
  DUnitX.Loggers.XML.xUnit in '..\DUnitX.Loggers.XML.xUnit.pas',
  DUnitX.Detour in '..\DUnitX.Detour.pas',
  DUnitX.Generics in '..\DUnitX.Generics.pas',
  DUnitX.Utils in '..\DUnitX.Utils.pas',
  DUnitX.WeakReference in '..\DUnitX.WeakReference.pas',
  DUnitX.CommandLine in '..\DUnitX.CommandLine.pas',
  DUnitX.Test in '..\DUnitX.Test.pas',
  DUnitX.TestFixture in '..\DUnitX.TestFixture.pas',
  DUnitX.TestResult in '..\DUnitX.TestResult.pas',
  DUnitX.TestResults in '..\DUnitX.TestResults.pas',
  DUnitX.TestRunner in '..\DUnitX.TestRunner.pas',
  DUnitX.InternalInterfaces in '..\DUnitX.InternalInterfaces.pas',
  DUnitX.TestFramework in '..\DUnitX.TestFramework.pas',
  DUnitX.DUnitCompatibility in '..\DUnitX.DUnitCompatibility.pas',
  DUnitX.IoC.Internal in '..\DUnitX.IoC.Internal.pas',
  DUnitX.IoC in '..\DUnitX.IoC.pas',
  DUnitX.Utils.XML in '..\DUnitX.Utils.XML.pas',
  DUnitX.StackTrace.JCL in '..\DUnitX.StackTrace.JCL.pas',
  DUnitX.StackTrace.MadExcept3 in '..\DUnitX.StackTrace.MadExcept3.pas',
  DUnitX.StackTrace.MadExcept4 in '..\DUnitX.StackTrace.MadExcept4.pas',
  DUnitX.Loggers.GUI in '..\DUnitX.Loggers.GUI.pas' {DUnitXGuiLoggerForm},
  DUnitX.StackTrace.EurekaLog7 in '..\DUnitX.StackTrace.EurekaLog7.pas',
  DUnitX.Examples.EqualityAsserts in '..\Examples\DUnitX.Examples.EqualityAsserts.pas',
  DUnitX.Examples.General in '..\Examples\DUnitX.Examples.General.pas';

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TDUnitXGuiLoggerForm, DUnitXGuiLoggerForm);
  Application.Run;

end.
