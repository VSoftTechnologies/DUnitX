program DUnitXVCLGUI_D10Berlin;

{$R *.res}

uses
  Vcl.Forms,
  System.SysUtils,
  DUnitX.Examples.General in 'DUnitX.Examples.General.pas',
  DUnitX.Loggers.Text in '..\Source\DUnitX.Loggers.Text.pas',
  DUnitX.Loggers.XML.NUnit in '..\Source\DUnitX.Loggers.XML.NUnit.pas',
  DUnitX.Loggers.XML.xUnit in '..\Source\DUnitX.Loggers.XML.xUnit.pas',
  DUnitX.MacOS.Console in '..\Source\DUnitX.MacOS.Console.pas',
  DUnitX.Test in '..\Source\DUnitX.Test.pas',
  DUnitX.TestFixture in '..\Source\DUnitX.TestFixture.pas',
  DUnitX.TestFramework in '..\Source\DUnitX.TestFramework.pas',
  DUnitX.TestResult in '..\Source\DUnitX.TestResult.pas',
  DUnitX.RunResults in '..\Source\DUnitX.RunResults.pas',
  DUnitX.TestRunner in '..\Source\DUnitX.TestRunner.pas',
  DUnitX.Utils in '..\Source\DUnitX.Utils.pas',
  DUnitX.Utils.XML in '..\Source\DUnitX.Utils.XML.pas',
  DUnitX.WeakReference in '..\Source\DUnitX.WeakReference.pas',
  DUnitX.Windows.Console in '..\Source\DUnitX.Windows.Console.pas',
  DUnitX.StackTrace.EurekaLog7 in '..\Source\DUnitX.StackTrace.EurekaLog7.pas',
  NonNamespacedExample in 'NonNamespacedExample.pas',
  DUnitX.Examples.EqualityAsserts in 'DUnitX.Examples.EqualityAsserts.pas',
  DUnitX.Loggers.Null in '..\Source\DUnitX.Loggers.Null.pas',
  DUnitX.MemoryLeakMonitor.Default in '..\Source\DUnitX.MemoryLeakMonitor.Default.pas',
  DUnitX.AutoDetect.Console in '..\Source\DUnitX.AutoDetect.Console.pas',
  DUnitX.ConsoleWriter.Base in '..\Source\DUnitX.ConsoleWriter.Base.pas',
  DUnitX.DUnitCompatibility in '..\Source\DUnitX.DUnitCompatibility.pas',
  DUnitX.Extensibility in '..\Source\DUnitX.Extensibility.pas',

  DUnitX.FixtureProvider in '..\Source\DUnitX.FixtureProvider.pas',
  DUnitX.FixtureResult in '..\Source\DUnitX.FixtureResult.pas',
  DUnitX.Generics in '..\Source\DUnitX.Generics.pas',
  DUnitX.InternalInterfaces in '..\Source\DUnitX.InternalInterfaces.pas',
  DUnitX.ServiceLocator in '..\Source\DUnitX.ServiceLocator.pas',
  DUnitX.Loggers.Console in '..\Source\DUnitX.Loggers.Console.pas',
  DUnitX.CommandLine.OptionDef in '..\Source\DUnitX.CommandLine.OptionDef.pas',
  DUnitX.CommandLine.Options in '..\Source\DUnitX.CommandLine.Options.pas',
  DUnitX.CommandLine.Parser in '..\Source\DUnitX.CommandLine.Parser.pas',
  DUnitX.OptionsDefinition in '..\Source\DUnitX.OptionsDefinition.pas',
  DUnitX.Banner in '..\Source\DUnitX.Banner.pas',
  DUnitX.FilterBuilder in '..\Source\DUnitX.FilterBuilder.pas',
  DUnitX.Filters in '..\Source\DUnitX.Filters.pas',
  DUnitX.CategoryExpression in '..\Source\DUnitX.CategoryExpression.pas',
  DUnitX.TestNameParser in '..\Source\DUnitX.TestNameParser.pas',
  DUnitX.Loggers.GUI.VCL in '..\Source\DUnitX.Loggers.GUI.VCL.pas' {GUIVCLTestRunner},
  DUnitX.Examples.AssertFailureCompare in 'DUnitX.Examples.AssertFailureCompare.pas',
  DUnitX.Timeout in '..\Source\DUnitX.Timeout.pas',
  DUnitX.Exceptions in '..\Source\DUnitX.Exceptions.pas',
  DUnitX.ResStrs in '..\Source\DUnitX.ResStrs.pas',
  DUnitX.Examples.UITest in 'DUnitX.Examples.UITest.pas' {frmUITest};

begin
  Application.Initialize;
  Application.CreateForm(TGUIVCLTestRunner, GUIVCLTestRunner);
  Application.Run;
end.
