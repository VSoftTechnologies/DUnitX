program InternalError;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  DUnitX.Detour in 'Utils\DUnitX.Detour.pas',
  DUnitX.Generics in 'Utils\DUnitX.Generics.pas',
  DUnitX.Utils in 'Utils\DUnitX.Utils.pas',
  DUnitX.WeakReference in 'Utils\DUnitX.WeakReference.pas',
  DUnitX.ConsoleWriter.Base in 'Loggers\DUnitX.ConsoleWriter.Base.pas',
  DUnitX.Loggers.XML.xUnit in 'Loggers\DUnitX.Loggers.XML.xUnit.pas',
  DUnitX.IoC.Internal in 'IoC\DUnitX.IoC.Internal.pas',
  DUnitX.IoC in 'IoC\DUnitX.IoC.pas';

begin
  try
    { TODO -oUser -cConsole Main : Insert code here }
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
