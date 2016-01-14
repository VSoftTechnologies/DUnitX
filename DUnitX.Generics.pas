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

unit DUnitX.Generics;

interface

{$I DUnitX.inc}

uses
  {$IFDEF USE_NS}
  System.Generics.Defaults,
  System.Generics.Collections;
  {$ELSE}
  Generics.Defaults,
  Generics.Collections;
  {$ENDIF}


type
  //Delphi does not have reference counted collection types.. so we created one here.
  //This will typically be used where we return IEnumerbable<T> from a function
  //TODO: need unit tests!!!

  IReadOnlyList<T> = interface(IEnumerable<T>)
    function GetCapacity : integer;
    function GetCount : integer;
    function GetItem(index : integer) : T;
    function GetOnNotify : TCollectionNotifyEvent<T>;
    procedure SetOnNotify(value : TCollectionNotifyEvent<T>);
    function First: T;
    function Last: T;
    function Contains(const Value: T): Boolean;
    function IndexOf(const Value: T): Integer;
    function LastIndexOf(const Value: T): Integer;
    function BinarySearch(const Item: T; out Index: Integer): Boolean; overload;
    function BinarySearch(const Item: T; out Index: Integer; const AComparer: IComparer<T>): Boolean; overload;
    property Capacity: Integer read GetCapacity;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: T read GetItem; default;
    property OnNotify: TCollectionNotifyEvent<T> read GetOnNotify write SetOnNotify;

  end;

  IList<T> = interface(IReadOnlyList<T>)
    procedure SetCapacity(const value : integer);
    procedure SetCount(const value : integer);
    procedure SetItem(index : integer; value : T);


    function Add(const Value: T): Integer;
    procedure AddRange(const Values: array of T); overload;
    procedure AddRange(const Collection: IEnumerable<T>); overload;
    procedure AddRange(Collection: TEnumerable<T>); overload;

    procedure Insert(Index: Integer; const Value: T);

    procedure InsertRange(Index: Integer; const Values: array of T); overload;
    procedure InsertRange(Index: Integer; const Collection: IEnumerable<T>); overload;
    procedure InsertRange(Index: Integer; const Collection: TEnumerable<T>); overload;

    function Remove(const Value: T): Integer;
    procedure Delete(Index: Integer);
    procedure DeleteRange(AIndex, ACount: Integer);
    function Extract(const Value: T): T;

    procedure Exchange(Index1, Index2: Integer);
    procedure Move(CurIndex, NewIndex: Integer);


    procedure Clear;


    procedure Reverse;

    procedure Sort; overload;
    procedure Sort(const AComparer: IComparer<T>); overload;

    procedure TrimExcess;

    function ToArray: TArray<T>;

    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount write SetCount;
    property Items[Index: Integer]: T read GetItem write SetItem; default;
    property OnNotify: TCollectionNotifyEvent<T> read GetOnNotify write SetOnNotify;
  end;

  //Trying to implement IEnumerable<T> in the same class as IEnumerable does not
  //seem to be impossible in delphi.. it's and excercise in extreme frustration!

  //Simple base IEnumerable Base implementation
  TDUnitXEnumerable = class(TInterfacedObject, IEnumerable)
  protected
    function IEnumerable.GetEnumerator = GetNonGenEnumerator;
    function GetNonGenEnumerator: IEnumerator; virtual; abstract;
  end;

  TDUnitXList<T> = class(TDUnitXEnumerable, IList<T>,IEnumerable<T>)
  private
    FList : TList<T>;
  protected
    //IEnumerable<T>
    function GetEnumerator: IEnumerator<T>;overload;
    function GetNonGenEnumerator: IEnumerator;override;

    //IList<T>
    function GetCapacity : integer;
    procedure SetCapacity(const value : integer);
    function GetCount : integer;
    procedure SetCount(const value : integer);
    function GetItem(index : integer) : T;
    procedure SetItem(index : integer; value : T);

    function GetOnNotify : TCollectionNotifyEvent<T>;
    procedure SetOnNotify(value : TCollectionNotifyEvent<T>);

    function Add(const Value: T): Integer;
    procedure AddRange(const Values: array of T); overload;
    procedure AddRange(const Collection: IEnumerable<T>); overload;
    procedure AddRange(Collection: TEnumerable<T>); overload;

    procedure Insert(Index: Integer; const Value: T);

    procedure InsertRange(Index: Integer; const Values: array of T); overload;
    procedure InsertRange(Index: Integer; const Collection: IEnumerable<T>); overload;
    procedure InsertRange(Index: Integer; const Collection: TEnumerable<T>); overload;

    function Remove(const Value: T): Integer;
    procedure Delete(Index: Integer);
    procedure DeleteRange(AIndex, ACount: Integer);
    function Extract(const Value: T): T;

    procedure Exchange(Index1, Index2: Integer);
    procedure Move(CurIndex, NewIndex: Integer);

    function First: T;
    function Last: T;

    procedure Clear;

    function Contains(const Value: T): Boolean;
    function IndexOf(const Value: T): Integer;
    function LastIndexOf(const Value: T): Integer;

    procedure Reverse;

    procedure Sort; overload;
    procedure Sort(const AComparer: IComparer<T>); overload;
    function BinarySearch(const Item: T; out Index: Integer): Boolean; overload;
    function BinarySearch(const Item: T; out Index: Integer; const AComparer: IComparer<T>): Boolean; overload;

    procedure TrimExcess;

    function ToArray: TArray<T>;
  public
    constructor Create; overload;
    constructor Create(const AComparer: IComparer<T>); overload;
    constructor Create(Collection: TEnumerable<T>); overload;
    destructor Destroy; override;
  end;

  TDUnitXIEnumerator<T> = class(TInterfacedObject,IEnumerator<T>,IEnumerator)
  private
    FList : IList<T>;
    FIndex : integer;
  protected
    function GetCurrentGen: T;
    function IEnumerator<T>.GetCurrent = GetCurrentGen;
    function GetCurrent: TObject;
    function MoveNext: Boolean;
    procedure Reset;
  public
    constructor Create(const AList : IList<T>);
  end;

implementation


{ TListFactory }


{ TDUnitList<T> }

function TDUnitXList<T>.Add(const Value: T): Integer;
begin
  result := FList.Add(Value);
end;

procedure TDUnitXList<T>.AddRange(const Collection: IEnumerable<T>);
begin
  FList.AddRange(collection);
end;

procedure TDUnitXList<T>.AddRange(Collection: TEnumerable<T>);
begin
  FList.AddRange(collection);
end;

procedure TDUnitXList<T>.AddRange(const Values: array of T);
begin
  FList.AddRange(Values);
end;

function TDUnitXList<T>.BinarySearch(const Item: T; out Index: Integer; const AComparer: IComparer<T>): Boolean;
begin
  result := FList.BinarySearch(Item,Index,AComparer);
end;

function TDUnitXList<T>.BinarySearch(const Item: T; out Index: Integer): Boolean;
begin
  result := FList.BinarySearch(Item,Index);
end;

procedure TDUnitXList<T>.Clear;
begin
  FList.Clear;
end;

function TDUnitXList<T>.Contains(const Value: T): Boolean;
begin
  result := FList.Contains(Value);
end;

constructor TDUnitXList<T>.Create;
begin
  FList := TList<T>.Create;
end;

constructor TDUnitXList<T>.Create(Collection: TEnumerable<T>);
begin
  FList := TList<T>.Create(collection);
end;

constructor TDUnitXList<T>.Create(const AComparer: IComparer<T>);
begin
  FList := TList<T>.Create(AComparer);
end;

procedure TDUnitXList<T>.Delete(Index: Integer);
begin
  FList.Delete(index);
end;

procedure TDUnitXList<T>.DeleteRange(AIndex, ACount: Integer);
begin
  FList.DeleteRange(AIndex,ACount);
end;

destructor TDUnitXList<T>.Destroy;
begin
  FList.Free;
  inherited;
end;

procedure TDUnitXList<T>.Exchange(Index1, Index2: Integer);
begin
  FList.Exchange(Index1,Index2);
end;

function TDUnitXList<T>.Extract(const Value: T): T;
begin
  result := FList.Extract(Value);
end;

function TDUnitXList<T>.First: T;
begin
  result := FList.First;
end;

function TDUnitXList<T>.GetCapacity: integer;
begin
  result := FList.Capacity;
end;

function TDUnitXList<T>.GetCount: integer;
begin
  result := FList.Count;
end;


function TDUnitXList<T>.GetEnumerator: IEnumerator<T>;
begin
  result := TDUnitXIEnumerator<T>.Create(Self);
end;


function TDUnitXList<T>.GetItem(index: integer): T;
begin
  result := FList.Items[index];
end;

function TDUnitXList<T>.GetNonGenEnumerator: IEnumerator;
begin
  result := nil;
end;

function TDUnitXList<T>.GetOnNotify: TCollectionNotifyEvent<T>;
begin
  result := FList.OnNotify;
end;

function TDUnitXList<T>.IndexOf(const Value: T): Integer;
begin
  result := FList.IndexOf(Value);
end;

procedure TDUnitXList<T>.Insert(Index: Integer; const Value: T);
begin
  FList.Insert(Index,Value);
end;

procedure TDUnitXList<T>.InsertRange(Index: Integer; const Collection: TEnumerable<T>);
begin
  FList.InsertRange(Index,Collection);
end;

procedure TDUnitXList<T>.InsertRange(Index: Integer; const Values: array of T);
begin
  FList.InsertRange(Index,Values);
end;

procedure TDUnitXList<T>.InsertRange(Index: Integer; const Collection: IEnumerable<T>);
begin
  FList.InsertRange(Index,Collection);
end;

function TDUnitXList<T>.Last: T;
begin
  result := FList.Last;
end;

function TDUnitXList<T>.LastIndexOf(const Value: T): Integer;
begin
  result := FList.LastIndexOf(Value);
end;

procedure TDUnitXList<T>.Move(CurIndex, NewIndex: Integer);
begin
  FList.Move(CurIndex,NewIndex);
end;

function TDUnitXList<T>.Remove(const Value: T): Integer;
begin
  result := FList.Remove(Value);
end;

procedure TDUnitXList<T>.Reverse;
begin
  FList.Reverse;
end;

procedure TDUnitXList<T>.SetCapacity(const value: integer);
begin
  FList.Capacity := value;
end;

procedure TDUnitXList<T>.SetCount(const value: integer);
begin
  FList.Count := value;
end;

procedure TDUnitXList<T>.SetItem(index: integer; value: T);
begin
  FList.Items[index] := value;
end;

procedure TDUnitXList<T>.SetOnNotify(value: TCollectionNotifyEvent<T>);
begin
  FList.OnNotify := value;
end;

procedure TDUnitXList<T>.Sort(const AComparer: IComparer<T>);
begin
  FList.Sort(AComparer);
end;

procedure TDUnitXList<T>.Sort;
begin
  FList.Sort;
end;

function TDUnitXList<T>.ToArray: TArray<T>;
begin
  result := nil; //FList.ToArray<T>;
end;

procedure TDUnitXList<T>.TrimExcess;
begin
  FList.TrimExcess;
end;

{ TDUnitIEnumerator<T> }

constructor TDUnitXIEnumerator<T>.Create(const AList: IList<T>);
begin
  FList := AList;
  FIndex := -1;
end;

function TDUnitXIEnumerator<T>.GetCurrent: TObject;
begin
  result := nil;
end;

function TDUnitXIEnumerator<T>.GetCurrentGen: T;
begin
  Result := FList[FIndex];
end;

function TDUnitXIEnumerator<T>.MoveNext: Boolean;
begin
  if FIndex >= FList.Count then
    Exit(False);
  Inc(FIndex);
  Result := FIndex < FList.Count;
end;

procedure TDUnitXIEnumerator<T>.Reset;
begin
  FIndex := -1;
end;


end.
