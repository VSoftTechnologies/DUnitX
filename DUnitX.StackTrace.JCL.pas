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
  public
    function GetStackTrace(const ex: Exception; const exAddressAddress: Pointer): string;
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
