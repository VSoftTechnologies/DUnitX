unit DUnitX.StackTrace.MadExcept;

interface

{$I DUnitX.inc}

uses
  SysUtils,
  classes,
{$IFDEF USE_MADEXCEPT}
  madStackTrace,
{$ENDIF}
  DUnitX.TestFramework;

type
  TMadExceptStackTraceProvider = class(TInterfacedObject,IStacktraceProvider)
  public
    function GetStackTrace(const ex: Exception; const exAddressAddress: Pointer): string;
  end;



implementation

uses
  DUnitX.IoC;

{ TJCLStackTraceProvider }

function TMadExceptStackTraceProvider.GetStackTrace(const ex: Exception; const exAddressAddress: Pointer): string;
begin
  result := '';
  {$IFDEF USE_MADEXCEPT}
  Result := string(madStackTrace.StackTrace( false, false, false, nil,
                                           exAddressAddress, false,
                                           false, 0, 0, nil,
                                           @exAddressAddress));

  {$ENDIF}
end;

initialization
{$IFDEF USE_MADEXCEPT}

  {$IFDEF DELPHI_XE_UP}
    TDUnitXIoC.DefaultContainer.RegisterType<IStacktraceProvider,TMadExceptStackTraceProvider>(true);
  {$ELSE}
    //D2010 bug prevents using above method.
    TDUnitXIoC.DefaultContainer.RegisterType<IStacktraceProvider>(true,
     function : IStacktraceProvider
        begin
          result := TMadExceptStackTraceProvider.Create;
        end
     );
  {$ENDIF}
{$ENDIF}

end.
