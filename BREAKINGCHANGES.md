Breaking Changes
================

Changes in behaviour that can make an existing test suite report differently after an upgrade.

Next major version
------------------

### `[MaxTime]` no longer interrupts the test

A test that ran past its `[MaxTime]` was stopped by a watchdog thread, which suspended the test thread, rewrote its instruction pointer to jump into a `raise ETimedOut` and resumed it.  On Win64 that crashed: the code address was truncated to 32 bits, so the thread jumped to an unmapped address and the test errored with an access violation instead of failing.  Correcting the address would not have made it safe, because x64 exception handling unwinds from tables that an injected frame has no entry in.

The test is now timed, and checked once it returns.  These things change:

- A test that runs over its `[MaxTime]` is not interrupted: it runs to its end, then fails with "Operation Timed Out".  Code after the point where it used to be stopped now runs.
- A test that never returns is no longer stopped by `[MaxTime]`, so the run waits for it.
- A test cannot catch the timeout itself (`except on ETimedOut`), as the example `TestMaxTime` did, because it is raised after the test returns.
- An overrun that ends in `Assert.Pass` fails.  An overrun that ends in a failed assertion or another exception reports that, as it would without `[MaxTime]`.  The timeout is checked only when the test ends normally or with `Assert.Pass`, so a `[WillRaise]` test that overruns and then raises the exception it expects passes.  (As before, a `[WillRaise]` test expecting `EAbort`, or `Exception` with `exDescendant`, takes the timeout's `ETimedOut` for its exception.)
- The test fails when its elapsed time, in whole milliseconds, is more than its `[MaxTime]`: a test taking exactly its `[MaxTime]` passes, as the attribute's documentation says.  The watchdog checked every 20 ms, so an overrun of up to 20 ms or so could pass.
- On Win64 an overrunning test fails with the timeout instead of erroring with an access violation.
- `[MaxTime]` no longer needs Windows, so it now applies on every platform; it was ignored elsewhere.

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
