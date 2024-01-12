unit DUnitX.InternalDataProvider;

interface
uses
  DUnitX.Types;

type
  ITestDataProvider = interface
    ['{1BFC9318-AF52-41BB-82D4-58166429885A}']
    function GetCaseCount(const methodName : string): integer;
    function GetCaseName(const methodName : string; const caseNumber : integer) : string;
    function GetCaseParams(const methodName : string; const caseNumber : integer) : TValuearray;
  end;

  TTestDataProvider = class(TInterfacedObject,ITestDataProvider)
  public
    constructor Create;virtual;abstract;
    function GetCaseCount(const methodName : string) : integer;virtual;abstract;
    function GetCaseName(const methodName : string; const caseNumber : integer) : string;virtual;abstract;
    function GetCaseParams(const methodName : string ; const caseNumber : integer) : TValuearray;virtual;abstract;
  End;

  TTestDataProviderClass = class of TTestDataProvider;


implementation

end.
