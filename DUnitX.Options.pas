unit DUnitX.Options;

interface

uses
  DUnitX.TestFramework,
  System.Generics.Collections;

type
  TOptions = class(TObject)
  private
    FCategoryList : TDictionary<string, IOptionsProvider>;
  protected
    constructor Create;
    destructor Destroy; override;
  public
    function Category<TInterface : IOptionsBase> : TInterface;
    function RegisterCategory(const ACategoryProvider : IOptionsProvider) : boolean;
    function UnregisterCategory(const ACategoryProvider : IOptionsProvider) : boolean;
  end;

function Options : TOptions;

implementation

uses
  SysUtils,
  TypInfo,
  Rtti;

var
  _options : TOptions;

function Options : TOptions;
begin
  if _options = nil then
    _options := TOptions.Create;

  result := _options;
end;

{ TOptionsGenerator }

constructor TOptions.Create;
begin
  FCategoryList := TDictionary<string, IOptionsProvider>.Create;
end;

destructor TOptions.Destroy;
begin
  FreeAndNil(FCategoryList);
  inherited;
end;

function TOptions.RegisterCategory(const ACategoryProvider: IOptionsProvider): boolean;
begin
  FCategoryList.Add(ACategoryProvider.Name, ACategoryProvider);
  result := true;
end;

function TOptions.UnregisterCategory(const ACategoryProvider: IOptionsProvider): boolean;
begin
  FCategoryList.Remove(ACategoryProvider.Name);
  result := true;
end;

function TOptions.Category<TInterface> : TInterface;
var
  iOptionsIndx: Integer;

  TypeInfoOfT: PTypeInfo;
  RttiContext: TRttiContext;
  RttiInterfaceType: TRttiInterfaceType;
  RttiType: TRttiType;

  optionsProvider : IOptionsProvider;
  sName: string;
  provider: IOptionsProvider;
begin
  Result := nil;

  TypeInfoOfT := TypeInfo(TInterface);
  RttiContext := TRttiContext.Create;
  RttiType := RttiContext.GetType(TypeInfoOfT);

  if not (RttiType is TRttiInterfaceType) then
    raise Exception.Create('Options type needs to be an interface type.');

  RttiInterfaceType := RttiType as TRttiInterfaceType;

  //Thought I could get the name from the interface by creating it on the fly.
  //  optionsProvider := (RttiInterfaceType.ClassType.Create as IOptionsProvider);
  //  try
  //    sName := optionsProvider.Name;
  //  finally
  //    optionsProvider := nil;
  //  end;

  for provider in FCategoryList.Values do
  begin
    if provider.GetOptionsInterface.QueryInterface(RttiInterfaceType.GUID, Result) = 0 then
      Exit;
  end;
end;

end.

