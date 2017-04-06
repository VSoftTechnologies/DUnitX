{***************************************************************************}
{                                                                           }
{           DUnitX                                                          }
{                                                                           }
{           Copyright (C) 2017 Vincent Parrett & Contributors               }
{                                                                           }
{           vincent@finalbuilder.com                                        }
{           http://www.finalbuilder.com                                     }
{                                                                           }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}
{ Resource strings contributed by Embarcadero                               }
{***************************************************************************}

unit DUnitX.ResStrs;

interface

resourcestring
  STestRunComplete = 'Test Run Complete';

  SUnexpectedErrorExt = 'Expected %g but got %g %s';
  SUnexpectedErrorInt = 'Expected %d but got %d %s';
  SUnexpectedErrorStr = 'Expected %s but got %s %s';
  SUnexpectedErrorDbl = 'Expected %g but got %g %s';
  SUnexpectedErrorGUID = 'Expected %s but got %s %s';
  SUnexpectedErrorStream = 'Streams are not equal %s';
  SNotEqualErrorStr = 'Expected %s is not equal to actual %s %s';
  SMemoryValuesNotEqual = 'Memory values are not equal. ';
  SFileDoesNotExist = 'File [%s] does not exist';
  SFileDoesExist = 'File [%s] does exist';
  SFileUnexpectedErrorCreation = 'Expected creation datetime [%s] but got [%s] %s';
  SEqualsErrorExt = '%g equals actual %g %s';
  SEqualsErrorStr = '[%s] is equal to [%s] %s';
  SEqualsErrorStr2 = 'Expected %s equals actual %s %s';
  SEqualsErrorInt = 'Expected %d equals actual %d %s';
  SEqualsErrorDbl = '%g equals actual %g %s';
  SEqualsErrorObj = 'Object [%s] Equals Object [%s] %s';
  SEqualsErrorGUID = 'Expected %s equals actual %s %s';
  SEqualsErrorStream = 'Streams are equal %s';
  SEqualsErrorIntf = 'references are the same. %s';
  SMemoryValuesEqual = 'Memory values are equal. ';
  SNotEqualErrorIntf = 'references are Not the same. %s';
  SNotEqualErrorObj = 'Object [%s] Not Object [%s] %s';
  SValueNotInList = 'List does not contain value %s. %s';
  SValueInList = 'List contains value %s. %s';
  SIntfNotImplemented = 'value does not implement %s. %s';
  SListNotEmpty = 'List is Not empty. %s';
  SStrNotEmpty = 'String is Not empty. %s';
  SVarNotEmpty = 'Variant is Not empty. %s';
  SIsFalseError = 'Condition is True when False expected. %s';
  SListEmpty = 'List is Empty when Not empty expected. %s';
  SStrEmpty = 'String is Empty. %s';
  SVarEmpty = 'Variant is Empty. %s';
  SIntfNil = 'Interface is Nil when not nil expected. %s';
  SPointerNil = 'Pointer is Nil when not Nil expected. %s';
  SObjNil = 'Object is Nil when Not Nil expected. %s';
  SVariantNull = 'Variant is Null when Not Null expcted. %s';
  SVariantNotNull = 'Variant is Not Null when Null expected. [%s]';
  SIntfNotNil = 'Interface is not Nil when nil expected. [%s]';
  SObjNotNil = 'Object is not nil when nil expected. [%s]';
  SPointerNotNil = 'Pointer is not Nil when nil expected. [%s]';
  SIsTrueError = 'Condition is False when True expected. [%s]';
  STypeError = 'value is not of type T';
  SUnexpectedException = 'Method raised [%s] was expecting not to raise Any exception. %s';
  SUnexpectedExceptionAlt = 'Method raised [%s] was expecting not to raise Any exception.';
  SMethodRaisedException = 'Method raised an exception of type : ';
  SMethodRaisedExceptionAlt = 'Method raised [%s] was expecting not to raise [%s]. %s';
  SNoException = 'Method did not throw any exceptions.';
  SUnexpectedExceptionMessage = 'Exception [%s] was raised with message [%s] was expecting [%s] %s';
  SCheckExceptionClassError = 'Method raised [%s] was expecting [%s]. %s';
  SCheckExceptionClassDescError = 'Method raised [%s] was expecting a descendant of [%s]. %s';
  SStrDoesNotContain = '[%s] does not contain [%s] %s';
  SStrDoesNotEndWith = '[%s] does not end with [%s] %s';
  SStrDoesNotMatch = '[%s] does not match [%s] %s';
  SStrCannotBeEmpty = 'subString cannot be empty';
  SStrDoesNotStartWith = '[%s] does Not Start with [%s] %s';
  SNumberOfStringsNotEqual = 'Number of strings is not equal';
  SLengthOfStringsNotEqual = 'Length of strings is not equal';
  SDiffAtPosition = 'Difference at position %d';

  SInvalidValueBool = 'Invalid value, not boolean';
  SInvalidOptionType = 'Invalid Option type - only string, integer, float, boolean, enum and sets are supported';
  SInvalidEnum = 'Invalid Enum Value : ';
  SInvalidOpt = 'invalid option type';

  SNameRequired = 'Name required - use RegisterUnamed to register unamed options';
  SOptionAlreadyRegistered = 'Options : %s already registered';
  SUnknownOptionStart = 'Unknown option start : ';

  SOptionExpectedValue = 'Option [ %s ] expected a following :value but none was found';
  SParameterFileDoesNotExist = 'Parameter File [%s] does not exist';
  SErrorParsingParameterFile = 'Error parsing Parameter File [%s] : ';
  SErrorSettingOption = 'Error setting option : %s to %s : ';
  SUnknownCommandLineOption = 'Unknown command line option : ';
  SOptionNotSpecified = 'Required Option [%s] was not specified';

  STestIgnoredRepeatSet = 'Repeat Set to 0. Test Ignored.';

  SRegisteredImplementationError = 'The implementation registered (%s) does not implement %s';
  SImplementationAlreadyRegistered = 'An implementation for type %s with name %s is already registered with IoC';
  SNoImplementationRegistered = 'No implementation registered for type %s';
  SNoInstance = 'The activator delegate failed to return an instance %s';

  SNoConsoleWriterClassRegistered = 'No ConsoleWriter Class is registered. You will need to include DUnitX.Windows.Console, DUnitX.MacOS.Console or DUnitX.Linux.Console in you application';
  SExecutingTest = 'Executing Test : ';
  SRunningFixtureSetup = 'Running Fixture Setup Method : ';
  SRunningSetup = 'Running Setup for : ';
  STest = 'Test : ';
  SFixture = 'Fixture : ';
  SSuccess = 'Success.';
  SRunningFixtureTeardown = 'Running Fixture Teardown Method : ';
  SRunningTestTeardown = 'Running Teardown for Test : ';
  SDoneTesting = 'Done testing.';
  STestsFound = 'Tests Found   : %d';
  STestsIgnored = 'Tests Ignored : %d';
  STestsPassed = 'Tests Passed  : %d';
  STestsLeaked = 'Tests Leaked  : %d';
  STestsFailed = 'Tests Failed  : %d';
  STestsErrored = 'Tests Errored : %d';
  STestsWarning = 'Tests with Warnings : %d';
  SFailingTests = 'Failing Tests';
  SMessage = '  Message: ';
  STestsWithErrors = 'Tests With Errors';
  STestsWithLeak = 'Tests With Memory Leak';
  SStartingTests = 'DUnitX - [%s] - Starting Tests.';
  SApplicationName = 'DUnitX - [%s]';

  SRunning = 'Running ';

  SUsage = 'Usage : %s options';
  SOptions = ' Options :';
  SNoRunner = 'No Runner found for current thread';
  SNilPlugin = 'Nil plugin registered!';

  SSetupTeardownBytesLeaked = '%d bytes were leaked in the setup/teardown methods';
  STestBytesLeaked = '%d bytes were leaked in the test method';
  SSetupTestTeardownBytesLeaked = '%d bytes were leaked in the setup/test/teardown methods';
  STestFailed = 'Test failed : ';
  STestError = 'Test Error : ';
  STestIgnored = 'Test Ignored : ';
  STestLeaked = 'Test Leaked Memory : ';
  SOnEndSetupEventError = 'Error in OnEndSetupEvent : ';
  SOnEndSetupTestLogError = 'unable to log error in OnEndSetupTest event : ';
  SNoFixturesFound = 'No Test Fixtures found';
  SFixtureSetupError = 'Error in Fixture Setup. Fixture: %s Error: %s';
  SSkippingFixture = 'Skipping Fixture.';
  SFixtureTeardownError = 'Error in Fixture TearDown. Fixture: %s Error: %s';
  SNoAssertions = 'No assertions were made during the test';
  SITestExecuteNotSupported = '%s does not support ITestExecute';

  SWeakReferenceError = 'TWeakReference can only be used with objects derived from TWeakReferencedObject';

  SNotImplemented = 'Not Implemented!';

  SOperationTimedOut = 'Operation Timed Out';

  SCouldNotFindResultsForTest = 'Could not find results for test.';

  SGUIStatusIdle = 'Idle';
  SGUIStatusRunning = 'Running (%d of %d)';

implementation

end.
