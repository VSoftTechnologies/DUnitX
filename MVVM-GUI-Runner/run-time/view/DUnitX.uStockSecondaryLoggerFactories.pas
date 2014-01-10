unit DUnitX.uStockSecondaryLoggerFactories;
interface
uses DUnitX.viewModel_LoggerContainer, DUnitX.TestFramework,
     Generics.Collections, DUnitX.Loggers.Console, DUnitX.Loggers.Text;

type
TBaseFactory = class( TInterfacedObject, ILoggerContainerFactory)
  private
    FClassDescriptor: string;
    function  ClassDescriptor: string;
  protected
    function  CreateLogger( var Logger: ITestLogger; var Properties: IInterface; var DisplayName: string): boolean;  virtual; abstract;
    procedure DisplayProperties( const Logger: ITestLogger; const Properties: IInterface);                           virtual;
    constructor Create( const ClassDescriptor1: string);
  end;

TConsoleFactory = class( TBaseFactory)
  protected
    function  CreateLogger( var Logger: ITestLogger; var Properties: IInterface; var DisplayName: string): boolean;  override;
  end;

TTextFactory = class( TBaseFactory)
  protected
    function  CreateLogger( var Logger: ITestLogger; var Properties: IInterface; var DisplayName: string): boolean;  override;
    procedure DisplayProperties( const Logger: ITestLogger; const Properties: IInterface);                           override;
  end;


procedure AppendStockFactories( Collection: TList<ILoggerContainerFactory>);

implementation





uses DUnitX.IoC, DUnitX.ConsoleWriter.Base, Dialogs, IOUtils, SysUtils
  {$if CompilerVersion >= 23}
    // XE2+
    , System.UITypes
  {$else}
    // D2010, XE
    , Controls
  {$ifend}
     ;

procedure AppendStockFactories( Collection: TList<ILoggerContainerFactory>);
begin
if TDUnitXIoC.DefaultContainer.Resolve<IDUnitXConsoleWriter>() <> nil then
  Collection.Add( TConsoleFactory.Create( 'Console'));
Collection.Add( TTextFactory.Create( 'Text'))
end;


{ TBaseFactory }

function TBaseFactory.ClassDescriptor: string;
begin
result := FClassDescriptor
end;

constructor TBaseFactory.Create( const ClassDescriptor1: string);
begin
FClassDescriptor := ClassDescriptor1
end;

procedure TBaseFactory.DisplayProperties(
  const Logger: ITestLogger; const Properties: IInterface);
begin
end;


function TConsoleFactory.CreateLogger(
  var Logger: ITestLogger; var Properties: IInterface; var DisplayName: string): boolean;
var
  quietMode: boolean;
begin
// TODO: Inquire user value for quietMode and store in Properties
result := True;
quietMode := True;
Logger := TDUnitXConsoleLogger.Create( quietMode);
DisplayName := 'Console'
end;

type
ITextLoggerMetaData = interface
  ['{5E15809F-CDE4-41B0-AA08-E37782DF25BD}']
    function FileName: string;
    function Overwrite: boolean;
  end;
TTextLoggerMetaData = class( TInterfacedObject, ITextLoggerMetaData)
  private
    FFileName: string;
    FOverwrite: boolean;
    function FileName: string;
    function Overwrite: boolean;
  public
    constructor Create( const FileName1: string; Overwrite1: boolean);
  end;

function TTextFactory.CreateLogger(
  var Logger: ITestLogger; var Properties: IInterface; var DisplayName: string): boolean;
var
  FileName: string;
  Overwrite: boolean;
begin
Overwrite := False;
result := Dialogs.PromptForFileName( FileName, 'text file|*.txt', '.txt', 'Select filename to log to', '', True);
if not result then exit;
case Dialogs.MessageDlg( 'Allow the logger to overwrite (instead of append)?', mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
  mrYes:    Overwrite := True;
  mrNo:     Overwrite := False;
  mrCancel: result := False;
  end;
if not result then exit;
Properties := TTextLoggerMetaData.Create( FileName, Overwrite);
Logger := TDUnitXTextFileLogger.Create( FileName, Overwrite);
DisplayName := TPath.GetFileName( FileName)
end;

procedure TTextFactory.DisplayProperties(
  const Logger: ITestLogger; const Properties: IInterface);
var
  Props: ITextLoggerMetaData;
begin
if Supports( Properties, ITextLoggerMetaData, Props) then
  ShowMessageFmt( 'File="%s"; Overwrite=%s',
    [Props.FileName, BoolToStr( Props.Overwrite, True)])
end;


constructor TTextLoggerMetaData.Create(
  const FileName1: string; Overwrite1: boolean);
begin
FFileName  := FileName1;
FOverwrite := Overwrite1
end;

function TTextLoggerMetaData.FileName: string;
begin
result := FFileName
end;

function TTextLoggerMetaData.Overwrite: boolean;
begin
result := FOverwrite
end;

end.
