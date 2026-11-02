Breaking Changes
================

Changes in behaviour that can make an existing test suite report differently after an upgrade.

Next major version
------------------

### `[TestCase]` names its cases from their values; `[NamedTestCase]` takes a name (#532)

Thinking up a name for every case is tedious when there are many, so `[TestCase]` now takes only the values and names each case from them, as `[AutoNameTestCase]` did: `[TestCase('1,2')]` on `Add` is the test `Add(1,2)`.  A case that needs a name of its own uses the new `[NamedTestCase]`, which takes the arguments `[TestCase]` used to take.

| Before | Now |
|---|---|
| `[TestCase('Name', '1,2')]` | `[NamedTestCase('Name', '1,2')]`, or `[TestCase('1,2')]` |
| `[TestCase('Name', '1;2', ';', true)]` | `[NamedTestCase('Name', '1;2', ';', true)]`, or `[TestCase('1;2', ';', true)]` |
| `[AutoNameTestCase('1,2')]` | `[TestCase('1,2')]` (`AutoNameTestCase` is kept as a deprecated alias) |
| `class(CustomTestCaseSourceAttribute)` | `class(CustomTestCaseGeneratorAttribute)` (the old name is kept as a deprecated alias) |

- **Converting.**  Replacing `[TestCase(` with `[NamedTestCase(` wherever a name is given keeps every test's name, and so any `--run` filter or result history that uses it.  Dropping the name instead renames the test.
- **Case names.**  A `[TestCase]` is named `Method(values)`, with the values exactly as written, before they are split or trimmed.  Cases of one method with the same values are numbered in the order they are declared, `Add(1,2)`, `Add(1,2) #2`, so every name is unique and the same from run to run.  A `[TestCaseSource]` case is named from the values it supplies, as `Add(1, 2, 3)`, written with `TValue.ToString`: keep source values to numbers, strings, booleans and enumerations, because an object's name includes its address, which changes every run, and a float or date is written in the machine's locale.
- **An old `[TestCase]` that still compiles errors.**  Written for the old attribute, a three- or four-argument `[TestCase]` no longer compiles, but a two-argument one does, with a new meaning: `[TestCase('Name', 'Bob')]` is now the value 'Name' with the separator 'Bob', which would run, and could pass, with the wrong value.  A separator is hardly ever a letter or digit, so a `[TestCase]` whose separator has one (A-Z, a-z or 0-9) errors when it runs: `TestCase separator 'Bob' contains a letter or digit: this looks like [TestCase('Name', 'Bob')] written for the old TestCase, which is now [NamedTestCase('Name', 'Bob')]`.  `[NamedTestCase]` takes any separator, so a case that really is separated by a letter, such as `[TestCase('1x2', 'x')]`, is written `[NamedTestCase('Name', '1x2', 'x')]`.
  - The check does **not** catch an old two-argument form whose values contain no ASCII letter or digit, such as `[TestCase('Name', ',')]`, `[TestCase('Name', '-')]` or `[TestCase('Empty', '')]`, the usual way to write an empty-string case: each is now the value 'Name' (or 'Empty'), split at those characters.  On a method with more than one parameter such a case usually errors anyway, as having too few values (#518); on a method with one `string` parameter it runs with 'Name', and a test that expected an empty string may pass or fail for the wrong reason.  Values made only of non-ASCII letters (`'ü'`) are not caught either.  Search for these by hand when converting.
- **`[TestCaseSource]`, new.**  `[TestCaseSource('MethodName')]` takes the cases from a class function of the fixture, and `[TestCaseSource(TSomeClass, 'MethodName')]` from one of another class.  The function is public, takes no parameters and returns an array with one element per case: an array of the case's values (`TArray<TArray<Integer>>`, or `TArray<TArray<TValue>>` for values of different types), or, for a method with one parameter, the value itself (`TArray<string>`).  For a method whose one parameter is an array, each element is that array.  Of overloads of the function, the one without parameters is used.  A source that is not found, has another signature, raises, or returns no cases errors the test, named `Method.SourceName`, rather than leaving it with no cases.  Each case is checked for too few and too many values (an empty row is too few), and its values are converted as other cases' are.  Objects a source returns are not freed.
- **`CustomTestCaseGeneratorAttribute`** is `CustomTestCaseSourceAttribute` renamed, to keep it apart from `[TestCaseSource]`; it works as before.  A descendant of `TestCaseAttribute` that passed a name to `inherited Create` calls the protected `InternalCreate` instead: `inherited Create(AName, AValues)` still compiles, as values and separator, and is caught only when `AValues` contains a letter or digit.

### Bad `[TestCase]` data now errors the test case

Until now, a `[TestCase]` value that could not be converted to its parameter's type was silently replaced: by 0, an empty value, a truncated number or 30/12/1899.  The test then ran with data nobody wrote, and could pass.  Such a case now **errors**, with a message naming the value and the parameter.  A test that errors after the upgrade had data that did not mean what it said: fix the data.  The safest form for test data is the invariant one: '.' as the decimal separator and ISO 8601 (`yyyy-mm-dd`, `yyyy-mm-ddThh:nn:ss`) for dates and times.

`[TestCaseProvider]` / `ITestDataProvider` data and `CustomTestCaseGeneratorAttribute` (formerly `CustomTestCaseSourceAttribute`) data are not affected by these checks, except the ones for too few and too many values.  `[NamedTestCase]` is checked as `[TestCase]` is, and `[TestCaseSource]` data as described above.

- **Fewer values than parameters** (#518).  A `[TestCase]` with fewer values than the method has parameters ran, each missing parameter taking its type's empty value (0, '', False, the first enum value), not the parameter's default: RTTI cannot see default values.  It now errors, as does provider or generator data with too few values.  Give every value, including one for a parameter that has a default.  A method with one parameter that is given no values at all, as `[TestCase('')]` gives it, still receives one empty string: right for a `string` parameter, while for any other type it errors as a value that does not convert (an `Integer` used to run with 0).
- **More values than parameters** (#520).  A value that contains the separator was split at it, and the extra values were dropped, so `[TestCase('Smith, John')]` ran with 'Smith'.  It now errors.  Use another separator: `[TestCase('Smith, John', '|')]`.  Provider and generator cases with more values than the method has parameters error too: the extra values used to be dropped.
- **Integers** (#522).  A value that does not parse ('abc'), or does not fit its type ('300' for a `Byte`, '-1' for a `Cardinal`), was read as 0 or truncated.  It now errors.  A hex value (`$FF`, `0xFF`) that fits the type's unsigned width is read as its bit pattern, as in code: '$FFFFFFFF' is the `Integer` -1.  `TColor` and `TAlphaColor` keep their existing parsing.
- **Floats** (#524).  A value that does not parse was read as 0.  It now errors.  Values are read with the invariant format, so '2.5' reads the same on every machine; a decimal comma ('2,5') is still accepted.
- **Dates and times** (#528).  A `TDate`, `TDateTime` or `TTime` value was read in the machine's locale first, then loosely as ISO 8601, and one that read in neither became 30/12/1899 (or midnight).  Now:
  - ISO 8601 is read first, strictly, and reads the same on every machine.
  - Otherwise the value is read with both the invariant format and the locale.  If they read **different** dates, the value is ambiguous and the case errors: '01/02/2020' errors on a UK machine (it is 1 February there, 2 January in the invariant format).  A day-first date with a day of 12 or less is therefore refused even where the locale wrote it.
  - A value neither reads errors the case.
  - A `TDateTime` whose year would be taken for an hour (on a year-first locale, '22/06/2020 10:30') errors instead of becoming 20 June 2022 at 20:00.
  - A date that only the locale reads ('22/06/2020' on a UK machine, '22.06.2020' on a German one) reads as before.
  - A date whose year is not written in full errors (#543): '22-06-20' is 22 June 2020 read day first and 20 June 2022 read year first, and nothing in the text says which, so '22.06.20' and '6/22/20' error on every machine too.  Write the year with four digits, or better, write the date as ISO 8601 (`2020-06-22`), which the error message suggests.  A date with no year at all ('6/22', '22.06') errors as well: it used to take the current year, so the test meant something different every year.  A time on its own has no year and still converts, except where the locale also reads it as a date (under fi-FI '10.06' is 10 June, not 10:06), which errors; write such a time as ISO 8601 (`10:06`).

### `Assert.WillNotRaise` lets another exception through (#526)

`Assert.WillNotRaise( method, SomeClass )` (and `WillNotRaiseWithMessage`) passed when the method raised an exception of a class it was not asked about, which was swallowed.  That exception now propagates, so the test errors with the real exception's class and message.  A test that relied on the swallowing should either name the class it expects or not raise.

### A test that ignores itself at run time is reported ignored (#396)

A test can ignore itself at run time with the new `Assert.Ignore(reason)`, or, since #386, by raising `ETestIgnored`.  For a test that raises `ETestIgnored` itself, these things change:

- The result carries the exception's message as the reason, and what the test logged.  The reason used to be empty, and the log was dropped.
- An error in `TearDown` after it is reported as the test's error, as after a pass.  It used to be logged as an error "after the test had failed", and the test stayed ignored.
- Raised from `Setup`, it ignores the test, which does not run.  It used to error the test.
- Raised from `SetupFixture`, it ignores every test in the fixture, none of which runs; `TearDownFixture` does not run, as after a `SetupFixture` error.  It used to error every test.
- Raised inside `Assert.WillRaise`, `WillRaiseAny`, `WillRaiseDescendant`, `WillNotRaise`, `WillNotRaiseAny`, `WillNotRaiseDescendant` or a `[WillRaise]` test, it is the test's outcome, as `Assert.Pass` already is, unless the class they expect is `ETestIgnored` or one of its ancestors (other than `Exception`).  An exact-class `WillRaise` form expecting an ancestor reports a class mismatch, as it does for `Assert.Pass`'s `ETestPass`.  It used to be taken for the exception being checked, so the test failed, or passed when any exception was expected.

A suite that does not raise `ETestIgnored` is not affected.

### A test that catches a `[MaxTime]` interruption is still reported as timed out (#538)

When `[MaxTime]` interrupts a test, the test fails with `ETimedOut` even if the interruption was caught on its way out: by the test's own `except`, by `Assert.WillRaise`, or by `[WillRaise]`.  It used to report whatever replaced the interruption: a pass (`Assert.Pass`, or the test returning), its own failure, an error (its handler raising another exception), or `expected X, got ETimedOut`.  `Assert.Ignore` is still reported as ignored.  A test that expects to time out says so with `[WillRaise(ETimedOut)]`, and passes; `[WillRaise(Exception, exDescendant)]` does not count, as it expects any exception.

### `[MaxTime]` applies to `[TestCase]` tests (#544)

`[MaxTime]` on a method with `[TestCase]`, `[NamedTestCase]`, `[TestCaseSource]`, a test case generator or a `[TestCaseProvider]` had no effect: every case ran for as long as it took.  Each case now has the method's `[MaxTime]`, as a plain test has, so a case that overruns fails with `ETimedOut`, a case still running is interrupted, and the rules above for a caught interruption and `[WillRaise(ETimedOut)]` apply to it.  A case that used to pass only because its limit was ignored now fails: raise the limit or speed the case up.

### JUnit XML: suite names and failure bodies (#547)

The JUnit logger named a suite by deleting the first `.FixtureName` it found in the fixture's full name, so a namespace that contains the fixture's name was mangled: fixture `TFoo` in unit `My.TFooTests` was the suite `MyTests.TFoo`.  It now deletes only the trailing one, and that suite is `My.TFooTests`.  A CI server that keys its test history by suite name sees such a suite as new.  The body of a `<failure>` element now holds the stack trace, as `<error>` does, instead of repeating the `message` attribute.

### Fixtures can run in parallel (#532)

The new option `TDUnitX.Options.MaxDegreeOfParallelism`, set on the command line with `-m:x` (`--mdop:x`), runs up to x fixtures at once, each on a worker thread.  It defaults to 0, and 0 or 1 runs the fixtures one at a time on the runner's thread exactly as before.  It needs Delphi 11 or later; earlier compilers ignore it.

- **`ITestRunner` has new members:** `UseThreadPool` and `MaxThreads`, with their `Get`/`Set` methods.  `UseThreadPool` means "run the fixtures on worker threads" (the threads are not a `TThreadPool`).  A class of your own that implements `ITestRunner` no longer compiles until it adds them.  A suite that only uses the runner `TDUnitX.CreateRunner` returns is not affected.
- **Every runner reads the option when it is created.**  `TDUnitXTestRunner` takes `MaxThreads` from `TDUnitX.Options.MaxDegreeOfParallelism` in its constructor, so the option must be set before the runner is created, and with `-m:x` a runner created by a GUI or IDE runner, or inside a test, runs its fixtures in parallel too.  Set its `UseThreadPool` to false to keep one serial.
- **New public members on implementation classes:** `TDUnitXRunResults.ReorderLastResults` and `TDUnitXFixtureResult.RestartTiming`, which the runner uses.  A descendant that declares members with those names hides them.

With x above 1:

- **Fixtures run off the main thread.**  Fixtures, and their `SetupFixture`, `TearDownFixture`, `Setup` and `TearDown`, run on worker threads: COM is not initialised there (ADO, MSXML and other COM objects need `CoInitialize`), the VCL must not be used, and code that checks for the main thread (`TThread.CurrentThread.ThreadID = MainThreadID`) sees a different answer.  `TThread.Synchronize` and `Queue` from a test still work: they run on the main thread, which, when the runner runs there, processes them while it waits for the fixtures.  Tests within one fixture, and its child fixtures, still run one at a time on one thread.
- **Fixtures must not share unguarded state.**  They run at the same time as other fixtures.  Fixtures that share globals, files, ports or a database need a lock, or need running without `-m`.
- **Loggers are called from the worker threads.**  The runner calls one logger method at a time, so a logger never sees two calls at once, but a logger that assumes the main thread (a VCL GUI logger) must not be used with `-m`.  The `threadId` passed to each event is the thread that runs the fixture.  Events from different fixtures interleave.  A fixture with no tests of its own, such as a namespace, gets `OnStartTestFixture` before any of its children start and `OnEndTestFixture` after all fixtures in the run have finished, in reverse order of their starts.
- **Verbose console output interleaves** between fixtures, so its indentation no longer follows the fixture tree.  Use `-cm:quiet` with `-m`.
- **Memory leaks are not checked** by any runner while fixtures run in parallel in any runner.  A leak monitor measures the whole process, so it cannot tell one test's allocations from those of the tests running alongside it.
- **An error that is not a test result stops its worker.**  An exception from a logger, say, ends that worker thread; the others finish their fixtures, and the run then raises `ETestFrameworkException` with the first such error's class and message.  Fixtures that no worker reached have no results.
- **`[MaxTime]` on Win64 can crash the run.**  Its timeout already ends upstream's `TestMaxTime` with an access violation on Win64 when run serially; with `-m:4` about one run in twelve of upstream's suite on Win64 ended with the process killed by an access violation, and none did with that fixture left out (25 runs each).  Win32 is not affected.  The cause is not the parallel code: the `[MaxTime]` watchdog suspends the test's thread and sets its instruction pointer to `RaiseTimeOutException`, and on Win64 `Ctx.Rip := Cardinal(@RaiseTimeOutException)` (`DUnitX.Timeout.pas`) truncates the 64-bit address, so the thread jumps to a wrong address.  Serially the runner happens to catch the access violation; on a worker thread it can take the process down.  A fix for `[MaxTime]` is offered separately.
- **Results keep the serial order.**  `IRunResults`, the fixture results and the NUnit and JUnit XML list the fixtures and tests in the order a serial run does; only times and durations differ.
