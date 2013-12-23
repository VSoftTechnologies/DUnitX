unit DUnitX.viewModel_LoggerContainer;
interface
uses DUnitX.TestFramework, Generics.Collections;

type

ILoggerContainerFactory = interface
  ['{A0FBAF69-FA90-42E6-BFBB-4F8786065CCE}']
    function  ClassDescriptor: string;
    function  CreateLogger( var Logger: ITestLogger; var Properties: IInterface; var DisplayName: string): boolean;
    procedure DisplayProperties( const Logger: ITestLogger; const Properties: IInterface);
  end;


ILoggerCentral = interface
  ['{CEE386E2-36D1-4D40-AB47-1AE1B2234A65}']
    function Loggers: TList<ILoggerContainerFactory>;
  end;

implementation

end.
