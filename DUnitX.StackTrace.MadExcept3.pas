unit DUnitX.StackTrace.MadExcept3;

interface

{$I DUnitX.inc}

uses
  SysUtils,
  classes,
{$IFDEF USE_MADEXCEPT3}
  madStackTrace,
{$ENDIF}
  DUnitX.TestFramework;

type
  TMadExcept3StackTraceProvider = class(TInterfacedObject,IStacktraceProvider)
  public
    function GetStackTrace(const ex: Exception; const exAddressAddress: Pointer): string;
    function PointerToLocationInfo(const Addrs: Pointer): string;
    function PointerToAddressInfo(Addrs: Pointer): string;
  end;



implementation

uses
  DUnitX.IoC;

{ TJCLStackTraceProvider }

function TMadExcept3StackTraceProvider.GetStackTrace(const ex: Exception; const exAddressAddress: Pointer): string;
begin
  result := '';
  {$IFDEF USE_MADEXCEPT3}
  Result := string(madStackTrace.StackTrace( false, false, false, nil,
                                           exAddressAddress, false,
                                           false, 0, 0, nil,
                                           @exAddressAddress));

  {$ENDIF}
end;

function TMadExcept3StackTraceProvider.PointerToAddressInfo(Addrs: Pointer): string;
begin
 Result := String(StackAddrToStr( Addrs ));
end;

function TMadExcept3StackTraceProvider.PointerToLocationInfo(const Addrs: Pointer): string;
begin

end;

initialization
{$IFDEF USE_MADEXCEPT3}

  {$IFDEF DELPHI_XE_UP}
    TDUnitXIoC.DefaultContainer.RegisterType<IStacktraceProvider,TMadExcept3StackTraceProvider>(true);
  {$ELSE}
    //D2010 bug prevents using above method.
    TDUnitXIoC.DefaultContainer.RegisterType<IStacktraceProvider>(true,
     function : IStacktraceProvider
        begin
          result := TMadExcept3StackTraceProvider.Create;
        end
     );
  {$ENDIF}
{$ENDIF}

end.
