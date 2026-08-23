program DUnitXConsoleFMX;

(*
  FMX pseudo-console example for DUnitX.

  This is an FMX application - not {$APPTYPE CONSOLE}.
  Define nothing special for console mode. Android/iOS (and any FMX target
  without a real console) can run the same suite and see console-style
  output in a code-created memo host.

  See Docs/FMX-Pseudo-Console.md and Source/DUnitX.Loggers.Console.FMX.pas.
*)

uses
  System.StartUpCopy,
  FMX.Forms,
  DUnitX.Loggers.Console.FMX in '..\Source\DUnitX.Loggers.Console.FMX.pas',
  DUnitX.Examples.Console.FMX in 'DUnitX.Examples.Console.FMX.pas';


begin
  DUnitX.Loggers.Console.FMX.Run;
end.
