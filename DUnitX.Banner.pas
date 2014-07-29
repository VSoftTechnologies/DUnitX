unit DUnitX.Banner;

interface


procedure ShowBanner;

implementation

uses
  DUnitX.ConsoleWriter.Base,
  DUnitX.IoC;


procedure ShowBanner;
var
  consoleWriter : IDUnitXConsoleWriter;

  procedure WriteLine(const value : string);
  begin
    if consoleWriter <> nil then
      consoleWriter.WriteLn(value)
    else
      System.Writeln(value);
  end;


begin
  consoleWriter := TDUnitXIoC.DefaultContainer.Resolve<IDUnitXConsoleWriter>();
  if consoleWriter <> nil then
    consoleWriter.SetColour(ccBrightWhite, ccDefault);

  WriteLine('**********************************************************************');
  WriteLine('*              DUnitX - (c) 2013 Vincent Parrett                     *');
  WriteLine('*                    vincent@finalbuilder.com                        *');
  WriteLine('*                                                                    *');
  WriteLine('*        License - http://www.apache.org/licenses/LICENSE-2.0        *');
  WriteLine('**********************************************************************');
  WriteLine('');
  if consoleWriter <> nil then
    consoleWriter.SetColour(ccDefault);



end;

end.
