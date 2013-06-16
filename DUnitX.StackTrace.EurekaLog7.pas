unit DUnitX.StackTrace.EurekaLog7;

interface

{$I DUnitX.inc}

uses
  SysUtils,
  classes,
{$IFDEF USE_EUREKALOG7}
  ECallStack,
  EDebugInfo,
{$ENDIF}
  DUnitX.TestFramework;

type
  TEurekaLog7StackTraceProvider = class(TInterfacedObject,IStacktraceProvider)
  public
    function GetStackTrace(const ex: Exception; const exAddressAddress: Pointer): string;
    function PointerToLocationInfo(const Addrs: Pointer): string;
    function PointerToAddressInfo(Addrs: Pointer): string;
  end;



implementation

uses
  DUnitX.IoC;

{ TEurekaLog7StackTraceProvider }

function TEurekaLog7StackTraceProvider.GetStackTrace(const ex: Exception; const exAddressAddress: Pointer): string;
{$IFDEF USE_EUREKALOG7}
var
  LEurekaBaseStackList : TEurekaStackListV7;
{$ENDIF}
begin
  result := '';
  {$IFDEF USE_EUREKALOG7}
  LEurekaBaseStackList := nil;
  try
    LEurekaBaseStackList := TEurekaStackListV7.Create(exAddressAddress);
    Result := LEurekaBaseStackList.ToString;
  finally
    LEurekaBaseStackList.Free;
  end;
  {$ENDIF}
end;

function TEurekaLog7StackTraceProvider.PointerToAddressInfo(Addrs: Pointer): string;
begin
  {$IFDEF USE_EUREKALOG7}
  Result := GetLocationInfoStr(Addrs);
  {$ELSE}
  Result := '';
  {$ENDIF}
end;

function TEurekaLog7StackTraceProvider.PointerToLocationInfo(const Addrs: Pointer): string;
begin
  {$IFDEF USE_EUREKALOG7}
  Result := GetLocationInfoStr(Addrs);
  {$ELSE}
  Result := '';
  {$ENDIF}
end;

initialization
{$IFDEF USE_EUREKALOG7}

  {$IFDEF DELPHI_XE_UP}
    TDUnitXIoC.DefaultContainer.RegisterType<IStacktraceProvider,TEurekaLog7StackTraceProvider>(true);
  {$ELSE}
    //D2010 bug prevents using above method.
    TDUnitXIoC.DefaultContainer.RegisterType<IStacktraceProvider>(true,
     function : IStacktraceProvider
        begin
          result := TEurekaLog7StackTraceProvider.Create;
        end
     );
  {$ENDIF}
{$ENDIF}

end.
