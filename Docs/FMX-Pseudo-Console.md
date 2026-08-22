# FMX Pseudo-Console

## Purpose

Android and iOS have no console applications (Embarcadero still has no mobile
console; Quality Portal RSP-17631). This branch adds a **console stand-in** so
the same DUnitX suite can run on a device and show console-style output.

This is **not** a GUI test explorer. TestInsight remains the IDE path.
MobileGUI / GUIX / VCL GUI are left alone.

## Shape (intentionally tiny)

1. `TDUnitXFMXConsoleLogger` (`Source/DUnitX.Loggers.Console.FMX.pas`) -
   an `ITestLogger` that appends console-style lines to a `TStrings` (typically
   a `TMemo.Lines`). Message text mirrors `TDUnitXConsoleLogger` (quiet/verbose).
   It never calls `Write`/`WriteLn` or `TDUnitXConsoleLogger`.
2. `TDUnitXFMXConsoleHost` + `Run` in the same unit - a form built entirely with
   `TForm.CreateNew` + `TMemo` + status `TLabel` (+ optional "Run again").
   Auto-runs the registered suite on first show.

## Why there is no `.fmx`

FMX designer/form files break across Delphi versions. Related history:

| Issue | Note |
|-------|------|
| #19, #39, #153 | FMX GUI was a contribution; never really finished. Vincent will not maintain FMX. |
| #154 | VCL GUI wiring uses `{$IFNDEF GUI}{$IFNDEF TESTINSIGHT}{$APPTYPE CONSOLE}` and `DUnitX.Loggers.GUI.VCL.Run`. |
| #217 | "Run on android" (closed): console apps unsupported on Android. |
| #172 | NUnit file logger + Android path/`TMonitor` issues. Optional XML (not in this change) should use documents path. |
| #230 | Tokyo FMX GUI example crashes; still broken in 10.4+. Do not extend those example projects. |
| #327 | Repro already uses `TForm` + `TMemo` and dumps results after the run - the UX we want, as a live `ITestLogger`. |
| #214 / #324 | I/O 105 when writing to a console that is not a console app. |
| #383 | Vincent: happy to take PRs; support many Delphi versions; avoid IDE-edited `.fmx` files. |

## Example

`Examples/DUnitXConsoleFMX.dpr` - one modern FMX multi-device example
(Win32 + Android). Not a version matrix. Include path points at `Source`.

## Usage

```delphi
uses
  DUnitX.Loggers.Console.FMX;

begin
  DUnitX.Loggers.Console.FMX.Run;
end.
```

Or host your own memo and attach `TDUnitXFMXConsoleLogger.Create(Memo.Lines)`.
