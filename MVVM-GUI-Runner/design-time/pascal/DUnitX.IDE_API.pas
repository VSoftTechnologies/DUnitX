unit DUnitX.IDE_API;
interface
uses ToolsAPI, DUnitX.IoC, Classes;

type
  IIDE_API = interface
    ['{685503B0-C46C-446E-9107-B65EEBF4EC9C}']
      procedure RegisterPackageWizard( const Wizard: IOTAWizard);
      function  GetActiveProject: IOTAProject;
      function  BorlandIDEServices: IBorlandIDEServices;
      function  SplashScreenServices: IOTASplashScreenServices;
      function  IOTAModuleServices: IOTAModuleServices;
      function  IOTAGalleryCategoryManager: IOTAGalleryCategoryManager;
      function  DecodeEnvironmentVariable( const PropName: string): string;
      procedure GetLibraryPaths( const sPlatform: string; var Paths: TStrings);
      function  GetDelphiSuffix: string;
      procedure ReportError( const Msg: string);
      procedure ReportInformation( const Msg: string);
      function  Confirm( const Question, AutomationId: string): boolean;
    end;

procedure RegisterStockDevEnviroService( const ServiceContainer: TDUnitXIoC);

implementation






uses SysUtils, Windows, Registry, IOUtils
  {$if CompilerVersion >= 23}
    // XE2+
   , Vcl.Dialogs, System.UITypes
  {$else}
   , Dialogs, Controls
  {$ifend}
   ;

type
TIDE = class( TInterfacedObject, IIDE_API)
  private
    FEnviroVars: TStrings;
    procedure RegisterPackageWizard( const Wizard: IOTAWizard);
    function  GetActiveProject: IOTAProject;
    function  BorlandIDEServices: IBorlandIDEServices;
    function  SplashScreenServices: IOTASplashScreenServices;
    function  IOTAModuleServices: IOTAModuleServices;
    function  IOTAGalleryCategoryManager: IOTAGalleryCategoryManager;
    function  DecodeEnvironmentVariable( const PropName: string): string;
    procedure GetLibraryPaths( const sPlatform: string; var Paths: TStrings);
    function  GetDelphiSuffix: string;
    procedure ReportError( const Msg: string);
    procedure ReportInformation( const Msg: string);
    function  Confirm( const Question, AutomationId: string): boolean;
  public
    constructor Create;
    destructor Destroy; override;
  end;

procedure RegisterStockDevEnviroService( const ServiceContainer: TDUnitXIoC);
begin
ServiceContainer.RegisterType<IIDE_API>( function: IIDE_API begin result := TIDE.Create end)
end;


function RecursivelyReplaceProperties( EnviroDefs: TStrings; const PropValue: string): string;
var
  Used: TStrings;

  function ReplaceProperties( const PropValue: string; UsedProps: TStrings): string;
  var
    k: integer;
    NativeKey, NativeValue: string;
    PropFormat: string;
    NeedsSubstitution: boolean;
    SubsCount: integer;
    Subs, NewUsedProps: TStrings;
  begin
  result := PropValue;
  SubsCount := 0;
  Subs := TStringList.Create;
  try
    for k := 0 to EnviroDefs.Count - 1 do
      begin
      NativeKey := EnviroDefs.Names[k];
      NativeValue := EnviroDefs.ValueFromIndex[k];
      if UsedProps.IndexOf( NativeKey) <> -1 then exit; // Avoid infinite recursion
      PropFormat := Format( '$(%s)', [NativeKey]);
      NeedsSubstitution := Pos( PropFormat, result) > 0;
      if NeedsSubstitution then
        begin
        Subs.Add( NativeKey);
        result := StringReplace( result, PropFormat, NativeValue, [rfReplaceAll, rfIgnoreCase]);
        Inc( SubsCount)
        end;
      end;
    if (SubsCount > 0) and (Pos( '$(', result) > 0) then
      begin
      NewUsedProps := TStringList.Create;
      try
        NewUsedProps.AddStrings( UsedProps);
        NewUsedProps.AddStrings( Subs);
        result := ReplaceProperties( Result, NewUsedProps)
      finally
        NewUsedProps.Free
        end
      end
  finally
    Subs.Free
    end
  end;

begin
Used := TStringList.Create;
try
  result := ReplaceProperties( PropValue, Used)
finally
  Used.Free
end
end;


procedure GetEnvironmentVariables( Strings: TStrings);
var
  P: PChar;
  Native: TStrings;
  j: integer;
  Name, Value: string;
begin
  // This body thanks to T.Ondrej .
  P := nil;
  Native := TStringList.Create;
  Native.BeginUpdate;
  try
    Native.Clear;
    P := GetEnvironmentStrings;
    repeat
      Native.Add( P);
      P := StrEnd( P);
      Inc( P);
    until P^ = #0;
  finally
    if Assigned( P) then
      FreeEnvironmentStrings( P);
    Native.EndUpdate;
  end;
  Strings.BeginUpdate;
  try
    Strings.Clear;
    for j := 0 to Native.Count - 1 do
      begin
      Name  := Native.Names[j];
      if Name = '' then Continue; // Strangely, this can happen.
                                  // You can get a Name/Value like '=C:=C:\Windows\system32'
      Value := Native.ValueFromIndex[j];
      {$if CompilerVersion >= 23}
        // In Delphi 2010, Environment Variables weren't allowed to be
        //  recursive. In XE3+ apparently, this can be so.
        //  This needs to be examined more closely and tested.
        if Pos( '$(', Value) > 0 then
          begin
          Value := RecursivelyReplaceProperties( Native, Value)
          end;
      {$ifend}
      Strings.Values[ Name] := Value
      end
  finally
    Strings.EndUpdate;
    Native.Free
    end
end;


function TIDE.Confirm( const Question, AutomationId: string): boolean;
begin
result := {$if CompilerVersion >= 23}Vcl.{$ifend}Dialogs.MessageDlg(
  Question, mtConfirmation, [mbYes, mbNo], 0) = mrYes
end;

constructor TIDE.Create;
begin
FEnviroVars := TStringList.Create;
GetEnvironmentVariables( FEnviroVars)
end;

destructor TIDE.Destroy;
begin
FEnviroVars.Free;
inherited
end;

procedure TIDE.RegisterPackageWizard( const Wizard: IOTAWizard);
begin
ToolsAPI.RegisterPackageWizard( Wizard)
end;

procedure TIDE.ReportError( const Msg: string);
begin
{$if CompilerVersion >= 23}Vcl.{$ifend}Dialogs.MessageDlg( Msg, mtError, [mbOK], 0)
end;

procedure TIDE.ReportInformation( const Msg: string);
begin
{$if CompilerVersion >= 23}Vcl.{$ifend}Dialogs.MessageDlg( Msg, mtInformation, [mbOK], 0)
end;

function TIDE.DecodeEnvironmentVariable( const PropName: string): string;
begin
result := FEnviroVars.Values[ PropName]
end;

function TIDE.GetActiveProject: IOTAProject;
begin
result := ToolsAPI.GetActiveProject
end;

function TIDE.GetDelphiSuffix: string;
begin
{$if CompilerVersion = 21}
  result := '_D2010'
{$ifend}
{$if CompilerVersion = 22}
  result := '_XE'
{$ifend}
{$if CompilerVersion = 23}
  result := '_XE2'
{$ifend}
{$if CompilerVersion = 24}
  result := '_XE3'
{$ifend}
{$if CompilerVersion = 25}
  result := '_XE4'
{$ifend}
{$if CompilerVersion = 26}
  result := '_XE5'
{$ifend}
end;

procedure TIDE.GetLibraryPaths( const sPlatform: string; var Paths: TStrings);
var
  sRegKey: string;
  Reg: TRegistry;
  sPaths, sPath2: string;
  RawPaths: TStrings;
begin
//	For XE3 the Key\@Name is "HKCU\Software\Embarcadero\BDS\10.0\Library\$(Platform)\@Search Path"
//	For D2010 it is "HKCU\Software\CodeGear\BDS\7.0\Library\@Search Path"
{$if CompilerVersion = 21}
  sRegKey := 'Software\CodeGear\BDS\7.0';
{$ifend}
{$if CompilerVersion = 22}
  sRegKey := 'Software\Embarcadero\BDS\8.0';
{$ifend}
{$if CompilerVersion = 23}
  sRegKey := 'Software\Embarcadero\BDS\9.0';
{$ifend}
{$if CompilerVersion = 24}
  sRegKey := 'Software\Embarcadero\BDS\10.0';
{$ifend}
{$if CompilerVersion = 25}
  sRegKey := 'Software\Embarcadero\BDS\11.0';
{$ifend}
{$if CompilerVersion = 26}
  sRegKey := 'Software\Embarcadero\BDS\12.0';
{$ifend}

sRegKey := sRegKey + '\Library';
{$if CompilerVersion >= 23}
  sRegKey := sRegKey + '\' + sPlatform;
{$ifend}
Reg := TRegistry.Create;
try
  Reg.RootKey := HKEY_CURRENT_USER;
  if Reg.OpenKeyReadOnly( sRegKey) then
      sPaths := Reg.ReadString( 'Search Path')
    else
      sPaths := ''
finally
  Reg.Free
  end;
RawPaths := TStringList.Create;
RawPaths.StrictDelimiter := True;
RawPaths.Delimiter := ';';
Paths.BeginUpdate;
try
  RawPaths.DelimitedText := sPaths;
  Paths.Clear;
  for sPaths in RawPaths do
    begin
    sPath2 := Trim( sPaths);
    if sPath2 = '' then continue;
    sPath2 := RecursivelyReplaceProperties( FEnviroVars, sPath2);
    sPath2 := ExcludeTrailingPathDelimiter( sPath2);
    if (Paths.IndexOf( sPath2) = -1) and
       TDirectory.Exists( sPath2) then
      Paths.Add( sPath2)
    end
finally
  Paths.EndUpdate;
  RawPaths.Free
  end
end;

function TIDE.BorlandIDEServices: IBorlandIDEServices;
begin
result := ToolsAPI.BorlandIDEServices
end;

function TIDE.SplashScreenServices: IOTASplashScreenServices;
begin
result := ToolsAPI.SplashScreenServices
end;

function TIDE.IOTAModuleServices: IOTAModuleServices;
begin
result := self.BorlandIDEServices() as ToolsAPI.IOTAModuleServices
end;

function TIDE.IOTAGalleryCategoryManager: IOTAGalleryCategoryManager;
begin
result := self.BorlandIDEServices() as ToolsAPI.IOTAGalleryCategoryManager
end;


end.
