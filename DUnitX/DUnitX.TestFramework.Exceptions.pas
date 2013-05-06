unit DUnitX.TestFramework.Exceptions;

interface

uses
  SysUtils;

type
  ETestFrameworkException = class(Exception);

  ENotImplemented = class(ETestFrameworkException);

  //base exception for any internal exceptions which cause the test to stop
  EAbort = class(ETestFrameworkException);

  ETestFailure = class(EAbort);
  ETestPass = class(EAbort);
  ENoTestsRegistered = class(ETestFrameworkException);

implementation

end.
