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

unit DUnitX.Filters;

//Filters are modelled on NUnit's Test filters

interface

{$I DUnitX.inc}

uses
  {$IFDEF USE_NS}
  System.Classes,
  System.Generics.Collections,
  {$ELSE}
  Classes,
  Generics.Collections,
  {$ENDIF}
  DUnitX.Extensibility;

type
	/// <summary>
	/// Interface to be implemented by filters applied to tests.
	/// The filter applies when running the test, after it has been
	/// loaded, since this is the only time an ITest exists.
	/// </summary>
  ITestFilter = interface
  ['{FE8D59F6-E47A-4F05-BA15-0471BFCCA004}']
		/// <summary>
		/// Indicates whether this is the EmptyFilter
		/// </summary>
    function IsEmpty : boolean;
    /// <summary>
		/// Determine whether the test itself matches the filter criteria,
		/// without examining either parents or descendants.
		/// </summary>
		/// <param name="test">The test to which the filter is applied</param>
		/// <returns>True if the filter matches the any parent of the test</returns>
    function Match(const test : ITest) : boolean;
  end;

  /// <summary>
	/// Name filter selects tests based on their name
	/// </summary>
  INameFilter = interface(ITestFilter)
  ['{74E76CBC-6CC3-4393-A66C-09A14974B7EB}']
    procedure Add(const name : string);overload;
    procedure Add(const names : TArray<string>);overload;
  end;

  /// <summary>
	/// Combines multiple filters so that a test must pass all
	/// of them in order to pass this filter.
	/// </summary>
  IAndFilter = interface(ITestFilter)
  ['{EE95054F-06DC-47F7-BB25-77F5162A26FD}']
    procedure Add(const filter : ITestFilter);overload;
    procedure Add(const filters : TArray<ITestFilter>);overload;
    function Filters : TList<ITestFilter>;
  end;

  /// <summary>
	/// Combines multiple filters so that a test must pass one
	/// of them in order to pass this filter.
	/// </summary>
  IOrFilter = interface(ITestFilter)
    ['{617B3F85-BF00-4F1E-9454-180618E32868}']
    procedure Add(const filter : ITestFilter);overload;
    procedure Add(const filters : TArray<ITestFilter>);overload;
    function Filters : TList<ITestFilter>;
  end;

  /// <summary>
	/// NotFilter negates the operation of another filter
	/// </summary>
  INotFilter = interface(ITestFilter)
  ['{4F0C6588-A98A-47C4-8CD6-2AF62BBBB7E5}']
    function GetBaseFilter : ITestFilter;
    function GetTopLevel  : boolean;
    procedure SetTopLevel(const value : boolean);

    property BaseFilter : ITestFilter read GetBaseFilter;
    property TopLevel : boolean read GetTopLevel write SetTopLevel;
  end;

  ICategoryFilter = interface(ITestFilter)
  ['{3D11F89D-9E39-4E03-80AB-F1AC9C6F3065}']
    procedure Add(const name : string);overload;
    procedure Add(const names : TArray<string>);overload;
    function Categories : TList<string>;
  end;



  TTestFilter = class(TInterfacedObject, ITestFilter)
  protected
    function IsEmpty: Boolean;
    function Match(const test: ITest): Boolean;virtual;abstract;
  end;

  TEmptyFilter = class(TTestFilter)
  protected
    function Match(const test: ITest): Boolean;override;
  end;

  TNameFilter = class(TTestFilter, ITestFilter,INameFilter)
  private
    FNames : TList<string>;
  protected
    procedure Add(const name : string);overload;
    procedure Add(const names : TArray<string>);overload;
    procedure Add(const names : TStrings);overload;
    function Match(const test: ITest): Boolean;override;
  public
    constructor Create;overload;
    constructor Create(const AName : string);overload;
    constructor Create(const ANames : TArray<string>);overload;
    constructor Create(const ANames : TStrings);overload;
    destructor Destroy;override;
  end;

  TCategoryFilter = class(TNameFilter,ITestFilter,ICategoryFilter)
  protected
    function Match(const test: ITest): Boolean;override;
    function Categories : TList<string>;
  end;

  TAndFilter = class(TTestFilter,ITestFilter,IAndFilter)
  private
    FFilters : TList<ITestFilter>;
  protected
    procedure Add(const filter : ITestFilter);overload;
    procedure Add(const filters : TArray<ITestFilter>);overload;
    function Match(const test: ITest): Boolean;override;
    function Filters : TList<ITestFilter>;
  public
    constructor Create(const filter : ITestFilter);overload;
    constructor Create(const filters : TArray<ITestFilter>);overload;
    destructor Destroy;override;
  end;

  TOrFilter = class(TTestFilter, ITestFilter,IOrFilter)
  private
    FFilters : TList<ITestFilter>;
  protected
    procedure Add(const filter : ITestFilter);overload;
    procedure Add(const filters : TArray<ITestFilter>);overload;
    function Match(const test: ITest): Boolean;override;
    function Filters : TList<ITestFilter>;
  public
    constructor Create(const filter : ITestFilter);overload;
    constructor Create(const filters : TArray<ITestFilter>);overload;
    destructor Destroy;override;
  end;

  TNotFilter = class(TTestFilter, ITestFilter,INotFilter)
  private
    FTopLevel : boolean;
    FBaseFilter : ITestFilter;
  protected
    function GetBaseFilter: ITestFilter;
    function GetTopLevel: Boolean;
    procedure SetTopLevel(const value : boolean);
    function Match(const test: ITest): Boolean;override;

  public
    constructor Create(const baseFilter : ITestFilter);overload;
    constructor Create(const baseFilter : ITestFilter; const topLevel : boolean);overload;
  end;


implementation

uses
  {$IFDEF USE_NS}
  System.Generics.Defaults,
  System.StrUtils,
  System.SysUtils;
  {$ELSE}
  Generics.Defaults,
  StrUtils,
  SysUtils;
  {$ENDIF}

{ TTestFilter }

function TTestFilter.IsEmpty: Boolean;
begin
  result := Self is TEmptyFilter;
end;


{ TEmptyFilter }

function TEmptyFilter.Match(const test: ITest): Boolean;
begin
  result := True;
end;


{ TNameFilter }

procedure TNameFilter.Add(const names: TArray<string>);
var
  name: string;
begin
  for name in names do
    FNames.Add(name);
end;

constructor TNameFilter.Create;
begin
  inherited;
  FNames := TList<string>.Create(TComparer<string>.Construct(
  function(const Left, Right : string) : integer
  begin
    result := AnsiCompareText(Left,Right);
  end));
end;

procedure TNameFilter.Add(const name: string);
begin
  if name <> '' then
  begin
    if not FNames.Contains(name) then
      FNames.Add(name)
  end;
end;

procedure TNameFilter.Add(const names: TStrings);
var
  name: string;
begin
  for name in names do
    FNames.Add(name);
end;

constructor TNameFilter.Create(const ANames: TStrings);
begin
  Create;
  Add(ANames);
end;

constructor TNameFilter.Create(const ANames: TArray<string>);
begin
  Create;
  Add(ANames);
end;

constructor TNameFilter.Create(const AName: string);
begin
  Create;
  Add(AName);
end;

destructor TNameFilter.Destroy;
begin
  FNames.Free;
  inherited;
end;

function TNameFilter.Match(const test: ITest): Boolean;
var
  name: string;
begin
  result := false;
  for name in FNames do
  begin
    result := StartsText(name,test.Fixture.FullName);
    if result then
      exit;
  end;

  if not result then
    result := FNames.IndexOf(test.FullName) <> -1;
end;

{ TAndFilter }

procedure TAndFilter.Add(const filters: TArray<ITestFilter>);
begin
  FFilters.AddRange(filters);
end;

procedure TAndFilter.Add(const filter: ITestFilter);
begin
  FFilters.Add(filter);
end;

constructor TAndFilter.Create(const filters: TArray<ITestFilter>);
begin
  inherited Create;
  FFilters := TList<ITestFilter>.Create;
  Add(filters);
end;

destructor TAndFilter.Destroy;
begin
  FFilters.Free;
  inherited;
end;

function TAndFilter.Filters: TList<ITestFilter>;
begin
  result := FFilters;
end;

constructor TAndFilter.Create(const filter: ITestFilter);
begin
  inherited Create;
  FFilters := TList<ITestFilter>.Create;
  Add(filter);
end;

function TAndFilter.Match(const test: ITest): Boolean;
var
  filter: ITestFilter;
begin
  for filter in FFilters do
  begin
    if not filter.Match(test) then
      Exit(false);
  end;
  result := true;
end;


{ TOrFilter }

procedure TOrFilter.Add(const filters: TArray<ITestFilter>);
begin
  FFilters.AddRange(filters);
end;

procedure TOrFilter.Add(const filter: ITestFilter);
begin
  FFilters.Add(filter);
end;

constructor TOrFilter.Create(const filters: TArray<ITestFilter>);
begin
  inherited Create;
  FFilters := TList<ITestFilter>.Create;
  Add(filters);
end;

destructor TOrFilter.Destroy;
begin
  FFilters.Free;
  inherited;
end;

function TOrFilter.Filters: TList<ITestFilter>;
begin
  result := FFilters;
end;

constructor TOrFilter.Create(const filter: ITestFilter);
begin
  inherited Create;
  FFilters := TList<ITestFilter>.Create;
  Add(filter);
end;

function TOrFilter.Match(const test: ITest): Boolean;
var
  filter: ITestFilter;
begin
  result := false;
  for filter in FFilters do
  begin
    if filter.Match(test) then
      Exit(true);
  end;
end;


{ TNotFilter }

constructor TNotFilter.Create(const baseFilter: ITestFilter);
begin
  Create(baseFilter,false);
end;

constructor TNotFilter.Create(const baseFilter: ITestFilter; const topLevel: boolean);
begin
  inherited Create;
  FBaseFilter := baseFilter;
  FTopLevel := topLevel;
end;

function TNotFilter.GetBaseFilter: ITestFilter;
begin
  result := FBaseFilter;
end;

function TNotFilter.GetTopLevel: Boolean;
begin
  result := FTopLevel;
end;

function TNotFilter.Match(const test: ITest): Boolean;
begin
  Result := not FBaseFilter.Match(test);
end;

procedure TNotFilter.SetTopLevel(const value: boolean);
begin
  FTopLevel := value;
end;

{ TCategoryFilter }

function TCategoryFilter.Categories: TList<string>;
begin
  result := FNames;
end;

function TCategoryFilter.Match(const test: ITest): Boolean;
var
  i : integer;
begin
  result := False;
  if test.Categories.Count = 0 then
    exit;

  for i := 0 to FNames.Count -1 do
  begin
    if test.Categories.Contains(FNames[i]) then
      exit(true);
  end;
end;

end.
