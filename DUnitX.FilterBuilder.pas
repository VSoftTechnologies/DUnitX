{***************************************************************************}
{                                                                           }
{           DUnitX                                                          }
{                                                                           }
{           Copyright (C) 2015 Vincent Parrett & Contributors               }
{                                                                           }
{           vincent@finalbuilder.com                                        }
{           http://www.finalbuilder.com                                     }
{                                                                           }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}

unit DUnitX.FilterBuilder;

interface

{$I DUnitX.inc}

uses
  DUnitX.TestFrameWork,
  DUnitX.Filters;


type
  TDUnitXFilterBuilder = class
    class function BuildFilter(const options : TDUnitXOptions) : ITestFilter;
  end;


implementation

uses
  {$IFDEF USE_NS}
  System.Classes,
  System.SysUtils,
  {$ELSE}
  Classes,
  SysUtils,
  {$ENDIF}
  DUnitX.CategoryExpression,
  DUnitX.TestNameParser,
  DUnitX.Exceptions;

{ TDUnitXFilterBuilder }

class function TDUnitXFilterBuilder.BuildFilter(const options: TDUnitXOptions): ITestFilter;
var
  nameFilter    : INameFilter;
  includeFilter : ITestFilter;
  excludeFilter : ITestFilter;
  i             : integer;
  name          : string;
  sList         : TStringList;
begin
  result := TEmptyFilter.Create;
  nameFilter := TNameFilter.Create;

  if options.Run.Count > 0 then
  begin
    for i := 0 to options.Run.Count -1 do
    begin
      for name in TTestNameParser.Parse(options.Run[i]) do
        nameFilter.Add(name);
    end;
    result := nameFilter;
  end;

  if options.RunListFile <> '' then
  begin
    if not FileExists(options.RunListFile) then
      raise ECommandLineError.Create('RunList File does not exist : ' + options.RunListFile);
    try
      sList := TStringList.Create;
      try
        sList.LoadFromFile(options.RunListFile);
        for i := 0 to sList.Count -1 do
        begin
          for name in TTestNameParser.Parse(sList[i]) do
            nameFilter.Add(name);
        end;
        result := nameFilter;
      finally
        sList.Free;
      end;
    except
      on e : Exception do
        raise ECommandLineError.Create(e.Message);
    end;
  end;

  if Trim(options.Include) <> '' then
  begin
    includeFilter := TCategoryExpression.CreateFilter(options.Include);
    if result.IsEmpty then
      result := includeFilter
    else
      result := TAndFilter.Create(TArray<ITestFilter>.Create(result,includeFilter));
  end;

  if Trim(options.Exclude) <> '' then
  begin
    excludeFilter := TNotFilter.Create(TCategoryExpression.CreateFilter(options.Exclude));
    if result.IsEmpty then
      result := excludeFilter
    else
      result := TAndFilter.Create(TArray<ITestFilter>.Create(result,excludeFilter));
  end;

  if Supports(result,INotFilter) then
    (result as INotFilter).TopLevel := true;

end;

end.
