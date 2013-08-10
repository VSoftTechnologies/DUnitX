{***************************************************************************}
{                                                                           }
{           DUnitX                                                          }
{                                                                           }
{           Copyright (C) 2012 Vincent Parrett                              }
{                                                                           }
{           vincent@finalbuilder.com                                        }
{           http://www.finalbuilder.com                                     }
{                                                                           }
{ Contributors : Vincent Parrett                                            }
{                Jason Smith                                                }
{                Nick Hodges                                                }
{                Nicholas Ring                                              }
{                                                                           }
{***************************************************************************}

DUnitX is an attempt to create a new test framework, taking ideas from DUnit,
NUnit and other test frameworks. It is designed to work with Delphi 2010 or
later, it makes use of language/RTL features that are not available in older
versions of delphi.

Planned Features

* Attribute driven. Any class can be a unit test fixture.

* Extensive Assert class.

* Limited backwards compatibility with DUnit test classes (not the
  framework). Use DUnitX.DUnitCompatibility

* Data driven test cases - the ability to provide a test method with a data
  source and test each entry in the data source. The data source will be
  virtualised so it can be anything (text file, db table etc).

* Setup and TearDown per test method and per test fixture.

* Remote logging - make it simple to run unit tests on a remote machine -
  - not thought this through yet - just an idea.


* XML Logging which produces output compatible with NUnit (this will make
  it compatible with CI servers like ContinuaCI which can process NUnit xml)

* Multi-threaded tests - the ability to have test fixtures run in their own
  threads.
