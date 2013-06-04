unit DUnitX.StackTrace.JCL;

interface

{$I DUnitX.inc}

uses
  SysUtils,
  classes,
{$IFDEF USE_JCL}
  JclDebug,
{$ENDIF}
  DUnitX.TestFramework;

type
  TJCLStackTraceProvider = class(TInterfacedObject,IStacktraceProvider)
  protected
    function GetStackTrace(const ex: Exception; const exAddressAddress: Pointer): string;
    function PointerToLocationInfo(const Addrs: Pointer): string;
    function PointerToAddressInfo(Addrs: Pointer): string;
  end;



implementation

uses
  DUnitX.IoC;

{ TJCLStackTraceProvider }

function TJCLStackTraceProvider.GetStackTrace(const ex: Exception; const exAddressAddress: Pointer): string;
{$IFDEF USE_JCL}
var
  traceList: TStrings;
{$ENDIF}
begin
  result := '';
  {$IFDEF USE_JCL}
  traceList := TStringList.Create;
  try
    JclDebug.JclLastExceptStackListToStrings(traceList, true);
    Result := traceList.Text;
  finally
    traceList.Free;
  end;
  {$ENDIF}
end;

function TJCLStackTraceProvider.PointerToAddressInfo(Addrs: Pointer): string;
{$IFDEF USE_JCL}
var
  _file,
  _module,
  _proc: string;
  _line: integer;
{$ENDIF}
begin
  Result := '';
{$IFDEF USE_JCL}
  JclDebug.MapOfAddr(Addrs, _file, _module, _proc, _line);
  Result := Format('%s$%p', [_proc, Addrs]);
{$ENDIF}
end;

//Borrowed from DUnit.
function TJCLStackTraceProvider.PointerToLocationInfo(const Addrs: Pointer): string;
{$IFDEF USE_JCL}
var
  _file,
  _module,
  _proc: string;
  _line: integer;
{$ENDIF}
begin
  Result := '';
{$IFDEF USE_JCL}
  JclDebug.MapOfAddr(Addrs, _file, _module, _proc, _line);

  if _file <> '' then
    Result   := Format('%s:%d', [_file, _line])
  else
    Result   := _module;
{$ENDIF}
end;

initialization
{$IFDEF USE_JCL}

  {$IFDEF DELPHI_XE_UP}
    TDUnitXIoC.DefaultContainer.RegisterType<IStacktraceProvider,TJCLStackTraceProvider>(true);
  {$ELSE}
    //D2010 bug prevents using above method.
    TDUnitXIoC.DefaultContainer.RegisterType<IStacktraceProvider>(true,
     function : IStacktraceProvider
        begin
          result := TJCLStackTraceProvider.Create;
        end
     );
  {$ENDIF}
{$ENDIF}

end.
