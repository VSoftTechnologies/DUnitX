unit DUnitX.InternalDataProvider;

interface
uses
  DUnitX.Types;

Type
  ITestDataProvider = interface
    ['{1BFC9318-AF52-41BB-82D4-58166429885A}']
    function GetCaseCount(Methodname:string):integer;
    function GetCaseName(Methodname:string):String;
    function GetCaseParams(Methodname:string;casenr:integer):TValuearray;
  end;

  TTestDataProviderBase = Class abstract (TInterfacedObject,ITestDataProvider)
    private
    protected
    public
      Constructor Create;virtual;Abstract;
      function GetCaseCount(Methodname:string):integer;Virtual;Abstract;
      function GetCaseName(Methodname:string):String;Virtual;Abstract;
      function GetCaseParams(Methodname:string;casenr:integer):TValuearray;Virtual;Abstract;
      Destructor Destroy;virtual;Abstract;
  End;
  TTestDataProviderBaseClass = class of TTestDataProviderbase;


implementation

end.
