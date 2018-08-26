(*
****************************************************************
* Unit:		      DUnitX.TestDataProvider
* Area:	      	TestCases
* Author:	      Uwe Rupprecht
* Purpose:      Base Class and Factory for TestDataProvider
* Version:	    1.0.0.0
* Last Change:	16.08.2018 15:25
* History:      -
****************************************************************
* This Source Code Form is subject to the terms of the
* Mozilla Public License, v. 2.0. If a copy of the MPL was not
* distributed with this file, You can obtain one at
* https://mozilla.org/MPL/2.0/.
****************************************************************
*)
unit DUnitX.TestDataProvider;

interface
uses
  System.Classes,
  System.Generics.Collections,
  DUnitX.Types,
  DUnitX.InternalDataProvider;

TYPE

  TestDataProviderManager = Class
    private
      class var flist : TDictionary<STRING,TClass>;
    protected
    public
       Class Constructor Create;
       Class Destructor Destroy;

       Class Procedure RegisterProvider(Name:string;AClass : TTestDataProviderBaseClass);
       Class Procedure UnregisterProvider(name:string);

       Class function GetProvider(Name:string):ITestDataProvider;overload;
       Class function GetProvider(AClass:TTestDataProviderBaseClass):ITestDataProvider;overload;
    published
  End;
implementation

{ TestDataProviderManager }

class constructor TestDataProviderManager.Create;
begin
  flist := TDictionary<STRING,TClass>.Create;
end;

class Destructor TestDataProviderManager.Destroy;
begin
  flist.Free;
end;

class function TestDataProviderManager.GetProvider(AClass: TTestDataProviderBaseClass): ITestDataProvider;
var
  key : string;
begin
  result := NIL;
  if (flist.ContainsValue(AClass)) then
  begin
    for key in flist.keys do
    begin
      if (flist[key] = AClass) then
      begin
        result := TTestDataProviderBaseClass(flist[key]).Create;
        break;
      end;
    end;
  end;
end;

Class function TestDataProviderManager.GetProvider(Name: string): ITestDataProvider;
begin
  result := NIL;
  if (flist.ContainsKey(Name)) then
    result := TTestDataProviderBaseClass(flist[Name]).Create;
end;

Class procedure TestDataProviderManager.RegisterProvider(Name: string;
  AClass: TTestDataProviderBaseClass);
begin
  if (not flist.ContainsKey(Name)) then
    flist.add(Name,AClass);
end;

Class procedure TestDataProviderManager.UnregisterProvider(name: string);
begin
 if (flist.ContainsKey(name)) then
    flist.Remove(Name);
end;

end.
