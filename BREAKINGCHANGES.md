Breaking Changes
================

Changes in behaviour that can make an existing test suite report differently after an upgrade.

Next major version
------------------

### Bad `[TestCase]` data now errors the test case

Until now, a `[TestCase]` value that could not be converted to its parameter's type was silently replaced: by 0, an empty value, a truncated number or 30/12/1899.  The test then ran with data nobody wrote, and could pass.  Such a case now **errors**, with a message naming the value and the parameter.  A test that errors after the upgrade had data that did not mean what it said: fix the data.  The safest form for test data is the invariant one: '.' as the decimal separator and ISO 8601 (`yyyy-mm-dd`, `yyyy-mm-ddThh:nn:ss`) for dates and times.

`[TestCaseProvider]` / `ITestDataProvider` data and `TestCaseSource` data are not affected by these checks, except the one for too few values.

- **Fewer values than parameters** (#518).  A `[TestCase]` with fewer values than the method has parameters ran, each missing parameter taking its type's empty value (0, '', False, the first enum value), not the parameter's default: RTTI cannot see default values.  It now errors, as does provider or `TestCaseSource` data with too few values.  Give every value, including one for a parameter that has a default.  A method with one parameter that is given no values at all, as `[TestCase('Name', '')]` gives it, still receives one empty string: right for a `string` parameter, while for any other type it errors as a value that does not convert (an `Integer` used to run with 0).
- **More values than parameters** (#520).  A value that contains the separator was split at it, and the extra values were dropped, so `[TestCase('Name', 'Smith, John')]` ran with 'Smith'.  It now errors.  Use another separator: `[TestCase('Name', 'Smith, John', '|')]`.
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
