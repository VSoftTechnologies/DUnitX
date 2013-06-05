unit DUnitX.StackTrace.MadExcept4;

interface

{$I DUnitX.inc}

uses
  SysUtils,
  classes,
{$IFDEF USE_MADEXCEPT4}
  madStackTrace,
{$ENDIF}
  DUnitX.TestFramework;

type
  TMadExcept4StackTraceProvider = class(TInterfacedObject,IStacktraceProvider)
  public
    function GetStackTrace(const ex: Exception; const exAddressAddress: Pointer): string;
    function PointerToLocationInfo(const Addrs: Pointer): string;
    function PointerToAddressInfo(Addrs: Pointer): string;
  end;



implementation

uses
  DUnitX.IoC;

{ TMadExcept4StackTraceProvider }

function TMadExcept4StackTraceProvider.GetStackTrace(const ex: Exception; const exAddressAddress: Pointer): string;
begin
  result := '';
  {$IFDEF USE_MADEXCEPT4}
  Result := madStackTrace.StackTrace( false, false, false, nil, nil,
                                           exAddressAddress, false,
                                           false, 0, 0, nil,
                                           @exAddressAddress);

  {$ENDIF}
end;

function TMadExcept4StackTraceProvider.PointerToAddressInfo(Addrs: Pointer): string;
begin
  {$IFDEF USE_MADEXCEPT4}
  Result := String(StackAddrToStr(Addrs));
  {$ELSE}
  Result := '';
  {$ENDIF}
end;

function TMadExcept4StackTraceProvider.PointerToLocationInfo(const Addrs: Pointer): string;
begin
  {$IFDEF USE_MADEXCEPT4}
  Result := String(StackAddrToStr(Addrs));
  {$ELSE}
  Result := '';
  {$ENDIF}
end;

initialization
{$IFDEF USE_MADEXCEPT4}

  {$IFDEF DELPHI_XE_UP}
    TDUnitXIoC.DefaultContainer.RegisterType<IStacktraceProvider,TMadExcept4StackTraceProvider>(true);
  {$ELSE}
    //D2010 bug prevents using above method.
    TDUnitXIoC.DefaultContainer.RegisterType<IStacktraceProvider>(true,
     function : IStacktraceProvider
        begin
          result := TMadExcept4StackTraceProvider.Create;
        end
     );
  {$ENDIF}
{$ENDIF}

end.
