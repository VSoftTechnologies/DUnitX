DUnitX Overview
===============

DUnitX is a new test framework, taking ideas from DUnit, NUnit and other test frameworks. It is designed to work with Delphi 2010 or later, it makes use of language/RTL features that are not available in older versions of Delphi.


DUnitX Features
===============

 * Any class can contain tests
 * Attribute based testing
 * An extensive Assert Class
 * Setup and TearDown per test method and per test fixture.
 * API Documented using Xml-Doc
 * Console Based Runner
 * XML Logging which produces output compatible with NUnit (this will make it compatible with CI servers like [ContinuaCI](http://www.finalbuilder.com/continua-ci) which can process NUnit xml)
 * Cross platform currently supporting:
    * Win32,Win64 and OSX Compilers.
 * Limited backwards compatibility with DUnit test classes. 
 * [Wizard](http://www.finalbuilder.com/Resources/Blogs/PostId/702/dunitx-has-a-wizard) for creating new tests.

DUnitX Planned Features
=======================

This is far from a complete list, but a few planned features are listed here to help indicate future direction.

 * ~~GUI Test Runner~~ - Use TestInsight
 * Multi-threaded tests - the ability to have test fixtures run in their own
  threads.
 * Remote logging - Simple way to run tests on remote machines (just an idea at this point) 
 * Data driven test cases - the ability to provide a test method with a data source and test each entry in the data source. The data source will be virtualised so it can be anything (text file, db table etc).

Tips and Tricks
===========
* In order to workaround the [Delphi XE3 Bug](https://github.com/VSoftTechnologies/DUnitX/issues/117), you need to add the unit DUnitX.Init to your test projects.

Support
=======

A [Google+ Community](https://plus.google.com/communities/110602661860791972403) has been setup to discuss DUnitX.

Contributors
============ 

* Vincent Parrett                                            
* Jason Smith                                               
* Nick Hodges                                               
* Nicholas Ring
* Robert Love              

A [recent list of contributors](https://github.com/VSoftTechnologies/DUnitX/graphs/contributors) can always be obtained on GitHub.                              

License
========
Apache Version 2.0 
Copyright (C) 2012-2016 Vincent Parrett
vincent@finalbuilder.com                                         
http://www.finalbuilder.com

See license.txt for details.


