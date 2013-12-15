unit DUnitX.SBD.uServiceProviderImpl;
interface
uses DUnitX.SBD.uServiceProvider, Classes, Generics.Collections, Generics.Defaults
     {$REGION ', Rtti'}
     // Rtti fix for defect as reported by QC report #109247
     //  (http://qc.embarcadero.com/wc/qcmain.aspx?d=109247)
     {$if                             RTLVersion < 23.00 } ,            Rtti       {$ifend}
     {$if (RTLVersion >= 23.00) and  (RTLVersion < 24.00)} , DUnitX.SBD.Rtti_XE2  {$ifend}
     {$if (RTLVersion >= 24.00) and  (RTLVersion < 25.00)} , DUnitX.SBD.Rtti_XE3  {$ifend}
     {$if  RTLVersion >= 25.00                           } ,            Rtti       {$ifend}
     {$ENDREGION}
     ;

type
RServiceKey = record
    FServiceIID: TGUID;
    FConfig: string;
    end;

RService = record
    FLiveService: IInterface;
    FFlyweightCreateBase: TClass;
    FFlyweightFactory: TServiceFactory;
    FCookie: Integer;
    FIdentifier: string;
    procedure Create;
  end;

RGroupOfServices = record
  private type
    TServiceList = class( TList<RService>)
    protected
      FisPooling: boolean;
    public
      procedure CheckPoolable( Idx: integer; const LiveService: IInterface);
      constructor Create;
    end;
  public
    FAffinity: TServiceAffinity;
    FMembers: TServiceList;
    FActiveIndex: integer;
    FisPooled: boolean;
    procedure Create;
    procedure Free;
  end;

IServiceProviderEx = interface( IServiceProvider)
  ['{CE9DD9B6-7F2D-4F83-BC9C-F49111EC75BC}']
    function GetServices: TDictionary<RServiceKey,RGroupOfServices>;
  end;

{$WARNINGS OFF}
TServiceProvider = class( TInterfacedObject, IServiceProvider, IEqualityComparer<RServiceKey>, IServiceProviderEx)
  strict private
    FisConstructing: boolean;

  private
    FServices: TDictionary<RServiceKey,RGroupOfServices>;
    FdoFreeGroupOnRemoval: boolean;
    FRtti: TRttiContext;

    function  GetServiceAffinity( const ServiceIID: TGUID; const Config: string): TServiceAffinity;
    procedure SetServiceAffinity( const ServiceIID: TGUID; const Config: string; Value: TServiceAffinity);
    function  GetActiveArrayMemberCookie( const ServiceIID: TGUID; const Config: string): integer;
    procedure SetActiveArrayMemberByCookie( const ServiceIID: TGUID; const Config: string; Cookie: integer);
    function  GetActiveArrayMemberName( const ServiceIID: TGUID; const Config: string): string;
    procedure SetActiveArrayMemberByName( const ServiceIID: TGUID; const Config, Name: string);
    function  GetNameByCookie( Cookie: integer): string;
    function  GetIsPooled( const ServiceIID: TGUID; const Config: string): boolean;
    procedure SetIsPooled( const ServiceIID: TGUID; const Config: string; Value: boolean);
    function  GetGroupCount( const ServiceIID: TGUID; const Config: string): integer;
    function  GetCookies( const ServiceIID: TGUID; const Config: string): TIntegerList;
    function  GetCookieByName( const ServiceIID: TGUID; const Config, Name: string): integer;
    function  GetCookieOfLiveService( const ServiceIID: TGUID; const Service: IInterface; const Config: string): integer;
    function  GetCookiesOfServiceClass( Registrant: TClass; const ServiceIID: TGUID): TIntegerList;
    procedure DeregisterServiceByCookie( Cookie: integer);
    procedure DeregisterServicesByCookie( Cookies: TIntegerList);
    procedure DeregisterLiveServiceByName( const ServiceIID: TGUID; const Config: string = ''; const SelectionArrayMemberName: string = '');
    procedure DeregisterServicesOfClass( Registrant: TClass; const ServiceIID: TGUID);



    function  RegisterLiveService   ( const ServiceIID: TGUID; const Service: IInterface;
      const Config, SelectionArrayMemberName: string): integer;
    function  RegisterFlyweightService( const ServiceIID: TGUID; CreateBase: TClass; Factory: TServiceFactory;
      const Config, SelectionArrayMemberName: string): integer;
    procedure RegisterServiceClass( Registrant: TClass; const ServiceIID: TGUID; var Cookies: TIntegerList); overload;
    procedure RegisterServiceClass( Registrant: TClass; const ServiceIID: TGUID                           ); overload;
    function  Acquire( const Client: IInterface; const ServiceIID: TGUID; out Intf; const Config: string; const InjectionOverride: IServiceProvider): boolean;
    function  Equals( const Left, Right: RServiceKey): Boolean;
    function  GetHashCode( const Value: RServiceKey): Integer;
    function  NextCookie: integer;
    function  FindKey( Cookie: Integer; out Key: RServiceKey; out Idx: integer): boolean;
    procedure ShutDown;
    function  Clone: IServiceProvider;
    procedure Merge( const Source: IServiceProvider); // Add Source's services to this.

    function  AcquireGang( const Client: IInterface; const ServiceIID: TGUID; var Gang: IInterfaceList; const Config: string = ''; const InjectionOverride: IServiceProvider = nil): boolean;

    procedure RemoveGroup( const Key: RServiceKey);
    procedure OnServiceNotify( Sender: TObject; const Item: RGroupOfServices; Action: TCollectionNotification);

    constructor CreateAsClone( SourceServices: TDictionary<RServiceKey,RGroupOfServices>);
    function GetServices: TDictionary<RServiceKey,RGroupOfServices>;
    procedure AddServices( Addend: TDictionary<RServiceKey,RGroupOfServices>);

  private type TServiceProc = reference to function (
                const ServiceIID: TGUID; const Config, Id: string;
                RegCls: TClass; ConstructorMethod: TRttiMethod): integer;

  private
    procedure ParseServiceClass(
      Registrant: TClass; const ServiceIID: TGUID;
      var Cookies: TIntegerList;
      ParseAction: TServiceProvider.TServiceProc);

  public
    constructor Create;
    destructor  Destroy; override;
  end;
{$WARNINGS ON}

implementation





uses SysUtils, TypInfo;


{ TServiceProvider }

function TServiceProvider.AcquireGang( const Client: IInterface;
  const ServiceIID: TGUID; var Gang: IInterfaceList;
  const Config: string; const InjectionOverride: IServiceProvider): boolean;
begin
result := (GetServiceAffinity( ServiceIID, Config) = afCooperative) and
          Acquire( Client, ServiceIID, Gang, Config, InjectionOverride)
end;

procedure TServiceProvider.AddServices(
  Addend: TDictionary<RServiceKey, RGroupOfServices>);
var
  Pair: TPair<RServiceKey, RGroupOfServices>;
  NewGroup, OldGroup: RGroupOfServices;
  NewCookie: integer;
  SaveIsConstr: boolean;

  procedure MergeMemberServices( const SourceGroup: RGroupOfServices);
  var
    AddendService, NewService: RService;
  begin
   for AddendService in SourceGroup.FMembers do
     begin
     NewService := AddendService;
     if NewService.FCookie <> -1 then
       NewService.FCookie := NewCookie;
     Inc( NewCookie);
     NewGroup.FMembers.Add( NewService)
     end;
  end;

begin
SaveIsConstr          := FisConstructing;
FisConstructing       := True;
FdoFreeGroupOnRemoval := True;
try
  NewCookie := NextCookie;
  for Pair in Addend do
    begin
    NewGroup.Create;
    NewGroup.FAffinity    := Pair.Value.FAffinity;
    NewGroup.FActiveIndex := Pair.Value.FActiveIndex;
    NewGroup.FisPooled    := Pair.Value.FisPooled;
    MergeMemberServices( Pair.Value);
    if FServices.ContainsKey( Pair.Key) then
        begin
        OldGroup := FServices[ Pair.Key];
        NewGroup.FActiveIndex := OldGroup.FActiveIndex;
        if (NewGroup.FAffinity <> OldGroup.FAffinity) or
           (NewGroup.FisPooled <> OldGroup.FisPooled) then
          raise Exception.Create(
            'Invalid merge of Service Providers with services with ' +
            'incompatible affinities or pool capability.');
        MergeMemberServices( OldGroup);
        FServices[ Pair.Key] := NewGroup
        end
      else
        FServices.Add( Pair.Key, NewGroup)
    end
finally
  FisConstructing       := SaveIsConstr;
  FdoFreeGroupOnRemoval := False
  end
end;

constructor TServiceProvider.Create;
begin
FisConstructing := True;
FRtti.Create;
FServices := TDictionary<RServiceKey,RGroupOfServices>.Create( Self);
FServices.OnValueNotify := OnServiceNotify;
FdoFreeGroupOnRemoval := False;
RegisterFlyweightService( IServiceProvider, nil,
  function( Obj: TObject; const Config: string;
  const ServiceProvider: IServiceProvider): IInterface
    begin
    result := self as IServiceProvider
    end, '', '');
RegisterFlyweightService( IServiceProvider, nil,
  function( Obj: TObject; const Config: string;
  const ServiceProvider: IServiceProvider): IInterface
    begin
    result := StandardServiceProvider
    end, 'New', '');
FisConstructing := False
end;

constructor TServiceProvider.CreateAsClone(
  SourceServices: TDictionary<RServiceKey, RGroupOfServices>);
begin
FisConstructing := True;
FServices := TDictionary<RServiceKey,RGroupOfServices>.Create( Self);
FServices.OnValueNotify := OnServiceNotify;
FdoFreeGroupOnRemoval := False;
AddServices( SourceServices);
FisConstructing := False
end;

destructor TServiceProvider.Destroy;
begin
FServices.Free;
FRtti.Free;
inherited
end;

type
TSuiteInjector = class;
IInjectableMember = interface
  ['{EB594A82-EAAE-4555-BAA0-33F86E6B5D8D}']
    function  IsInjectable: boolean;
    function  AsRttiMember: TRttiMember;
    function  MemberType: TRttiType;
    procedure SetValue( const Inject: TValue);
  end;

TInjectableMember = class( TInterfacedObject, IInjectableMember)
  protected
    FOwner: TSuiteInjector;

    function  IsInjectable: boolean;   virtual; abstract;
    function  AsRttiMember: TRttiMember;  virtual; abstract;
    function  MemberType: TRttiType;   virtual; abstract;
    procedure SetValue( const Inject: TValue);    virtual; abstract;
  public
    constructor Create( Owner1: TSuiteInjector);
  end;

ISuiteInjector = interface
  ['{72088119-43E6-4273-928B-0C84E0F84791}']
    procedure   Inject( const Provider: IServiceProvider);
  end;

TSuiteInjector = class( TInterfacedObject, ISuiteInjector)
  public
    FInjectionSubject: TObject;
    FServiceType : TRttiType;
    FClient: IInterface;
    FInjectionsClient: IInterface;
    FInjectionOverride: IServiceProvider;

    constructor Create( InjectionSubject1: TObject;
                        ServiceType1 : TRttiType;
                        const Client1: IInterface;
                        const InjectionOverride: IServiceProvider);
    destructor  Destroy; override;
    procedure   Inject( const Provider: IServiceProvider);

  protected
    FMembers: TEnumerable<IInjectableMember>;
    function  GetMembers: TEnumerable<IInjectableMember>;   virtual; abstract;

  private
    function GetInjectionMetaData(
                const Provider: IServiceProvider;
                const Member: IInjectableMember;
                var   Providers: TList<IServiceProvider>;
                var   InjectableGUID: TGUID;
                var   sInjectionConfig: string;
                var   Affinity: TServiceAffinity): boolean;
  end;
TSuiteInjectorClass = class of TSuiteInjector;

TDataMemberInjector = class( TSuiteInjector)
  protected
    function  GetMembers: TEnumerable<IInjectableMember>;          override;
  end;

TWritablePropertyInjector = class( TSuiteInjector)
  protected
    function  GetMembers: TEnumerable<IInjectableMember>;          override;
  end;

TInjectableDataMember = class( TInjectableMember)
  private
    FMember: TRttiField;
  protected
    function  IsInjectable: boolean;              override;
    function  AsRttiMember: TRttiMember;          override;
    function  MemberType: TRttiType;              override;
    procedure SetValue( const Inject: TValue);    override;
  public
    constructor Create( Owner1: TSuiteInjector; Member1: TRttiField);
  end;

TInjectablePropMember = class( TInjectableMember)
  private
    FMember: TRttiProperty;
  protected
    function  IsInjectable: boolean;              override;
    function  AsRttiMember: TRttiMember;          override;
    function  MemberType: TRttiType;              override;
    procedure SetValue( const Inject: TValue);    override;
  public
    constructor Create( Owner1: TSuiteInjector; Member1: TRttiProperty);
  end;


TMemberEnumable<MemberT> = class( TEnumerable<IInjectableMember>)
  private
    FMembers: TArray<MemberT>;
    FSuite: TSuiteInjector;
  public
    constructor Create( Suite1: TSuiteInjector);     virtual;
  end;

TPropEnumerable = class( TMemberEnumable<TRttiProperty>)
  protected
    function DoGetEnumerator: TEnumerator<IInjectableMember>; override;
  private
    constructor Create( Suite1: TSuiteInjector);     override;
  end;

TFieldEnumerable = class( TMemberEnumable<TRttiField>)
  protected
    function DoGetEnumerator: TEnumerator<IInjectableMember>; override;
  private
    constructor Create( Suite1: TSuiteInjector);     override;
  end;

TMemberEnumator<MemberT> = class( TEnumerator<IInjectableMember>)
  protected
    FOwner: TMemberEnumable<MemberT>;
    FIndex: integer;
    function DoMoveNext: Boolean; override;
  public
    constructor Create( Owner1: TMemberEnumable<MemberT>);
  end;

TPropEnumator = class( TMemberEnumator<TRttiProperty>)
  protected
    function DoGetCurrent: IInjectableMember; override;
  public
    constructor Create( Owner1: TMemberEnumable<TRttiProperty>);
  end;

TFieldEnumator = class( TMemberEnumator<TRttiField>)
  protected
    function DoGetCurrent: IInjectableMember; override;
  public
    constructor Create( Owner1: TMemberEnumable<TRttiField>);
  end;

function TServiceProvider.Acquire(
  const Client: IInterface; const ServiceIID: TGUID;
  out Intf; const Config: string; const InjectionOverride: IServiceProvider): boolean;
const
  Suites: array[0..1] of TSuiteInjectorClass = (TDataMemberInjector, TWritablePropertyInjector);
var
  Key: RServiceKey;
  Value: RService;
  Group: RGroupOfServices;
  // Rtti: TRttiContext;
  Gang: IInterfaceList;
  GangMember: IInterface;
  isNewbie: boolean;
  Idx: integer;

  function AcquireOneService( out Intf1; const Value1: RService; var isInception: boolean): boolean;
  var
    Cls: TClass;
    Obj: TObject;
    ServiceType: TRttiType;
    SuiteCls: TSuiteInjectorClass;
    Hold: IInterface;
  begin
  isInception := False;
  IInterface( Intf1) := Value1.FLiveService;
  result := assigned( IInterface( Intf1));
  if result then exit;
  Obj := nil;
  try
    if assigned( Value1.FFlyweightFactory) then
      begin
      Cls := Value1.FFlyweightCreateBase;
      if assigned( Cls) then
          begin
          Obj  := Cls.NewInstance;
          Supports( Obj, IInterface, Hold);
          // ^-- Needed for reference counted objects which do not use
          //  the BeforeConstruction property to set the reference count
          //  to one.
          ServiceType := FRtti.GetType( Cls);
          for SuiteCls in Suites do
            (SuiteCls.Create( Obj, ServiceType, Client, InjectionOverride) as ISuiteInjector)
              .Inject( self)
          end
        else
          Obj := nil;
      IInterface( Intf1) := Value1.FFlyweightFactory( Obj, Config, Self);
      result := assigned( IInterface( Intf1));
      isInception := result;
      Hold   := nil
      end
    else
      result := False
  except
    begin
    Obj.Free;
    raise
    end
  end end;

begin
Key.FServiceIID := ServiceIID;
Key.FConfig     := Config;
result := FServices.TryGetValue( Key, Group);
if not result then exit;
case Group.FAffinity of
  afCooperative:
    begin
    Gang := TInterfaceList.Create;
    IInterface( Intf) := Gang;
    for Idx := 0 to Group.FMembers.Count - 1 do
      begin
      Value := Group.FMembers[ Idx];
      if AcquireOneService( GangMember, Value, isNewbie) then
        begin
        Gang.Add( GangMember);
        if isNewbie and Group.FisPooled then
          Group.FMembers.CheckPoolable( Idx, GangMember)
        end
      end
    end;
  afCompetitive:
    begin
    Idx := Group.FActiveIndex;
    result := (Idx >= 0) and (Idx < Group.FMembers.Count);
    if not result then exit;
    result := AcquireOneService( Intf, Group.FMembers[Idx], isNewbie);
    if isNewbie and Group.FisPooled then
      Group.FMembers.CheckPoolable( Idx, IInterface( Intf))
    end
  end
end;


function TServiceProvider.Equals( const Left, Right: RServiceKey): Boolean;
begin
Result := IsEqualGUID( Left.FServiceIID, Right.FServiceIID) and
          (Left.FConfig = Right.FConfig)
end;

function TServiceProvider.FindKey( Cookie: Integer; out Key: RServiceKey; out Idx: integer): boolean;
var
  Pair: TPair<RServiceKey,RGroupOfServices>;
  Service: RService;
begin
result := False;
for Pair in FServices do
  begin
  Idx := 0;
  for Service in Pair.Value.FMembers do
    begin
    result := Service.FCookie = Cookie;
    if result then Break;
    Inc( Idx)
    end;
  if not result then continue;
  Key := Pair.Key;
  break
  end;
end;

function TServiceProvider.GetActiveArrayMemberCookie(
  const ServiceIID: TGUID; const Config: string): integer;
var
  Key: RServiceKey;
  Group: RGroupOfServices;
  Members: RGroupOfServices.TServiceList;
begin
Key.FServiceIID := ServiceIID;
Key.FConfig     := Config;
Members := nil;
if FServices.ContainsKey( Key) then
    begin
    Group   := FServices.Items[Key];
    Members := Group.FMembers;
    result  := Group.FActiveIndex
    end
  else
    result := -1;
if (result >= 0) and (result < Members.Count) then
    result := Members[result].FCookie
  else
    result := -1
end;


function TServiceProvider.GetActiveArrayMemberName(
  const ServiceIID: TGUID; const Config: string): string;
var
  Key: RServiceKey;
  Idx: integer;
  Group: RGroupOfServices;
  Members: RGroupOfServices.TServiceList;
begin
Key.FServiceIID := ServiceIID;
Key.FConfig     := Config;
Members := nil;
if FServices.ContainsKey( Key) then
    begin
    Group   := FServices.Items[Key];
    Members := Group.FMembers;
    Idx     := Group.FActiveIndex
    end
  else
    Idx := -1;
if (Idx >= 0) and (Idx < Members.Count) then
    result := Members[Idx].FIdentifier
  else
    result := ''
end;

function TServiceProvider.GetCookieByName(
  const ServiceIID: TGUID; const Config, Name: string): integer;
var
  Key: RServiceKey;
  Service: RService;
begin
Key.FServiceIID := ServiceIID;
Key.FConfig     := Config;
result := -1;
if FServices.ContainsKey( Key) then
  for Service in FServices.Items[Key].FMembers do
    begin
    if Service.FIdentifier <> Name then continue;
    result := Service.FCookie;
    break
    end
end;

function TServiceProvider.GetCookieOfLiveService(
  const ServiceIID: TGUID;
  const Service: IInterface; const Config: string): integer;
var
  Key: RServiceKey;
  ServiceRec: RService;
begin
Key.FServiceIID := ServiceIID;
Key.FConfig     := Config;
result := -1;
if FServices.ContainsKey( Key) then
  for ServiceRec in FServices.Items[Key].FMembers do
    begin
    if ServiceRec.FLiveService <> Service then continue;
    result := ServiceRec.FCookie;
    break
    end
end;

function TServiceProvider.GetCookies(
  const ServiceIID: TGUID; const Config: string): TIntegerList;
var
  Key: RServiceKey;
  Service: RService;
begin
Key.FServiceIID := ServiceIID;
Key.FConfig     := Config;
result := TIntegerList.Create;
if FServices.ContainsKey( Key) then
  for Service in FServices.Items[Key].FMembers do
    if Service.FCookie <> -1 then
      result.Add( Service.FCookie)
end;

function TServiceProvider.GetCookiesOfServiceClass(
  Registrant: TClass; const ServiceIID: TGUID): TIntegerList;
begin
result := nil;
ParseServiceClass( Registrant, ServiceIID, result,
  function (
    const ServiceIID: TGUID; const Config, Id: string;
    RegCls: TClass; ConstructorMethod: TRttiMethod): integer
  begin
  result := GetCookieByName( ServiceIID, Config, Id)
  end
  )
end;

function TServiceProvider.GetGroupCount(
  const ServiceIID: TGUID; const Config: string): integer;
var
  Key: RServiceKey;
begin
Key.FServiceIID := ServiceIID;
Key.FConfig     := Config;
if FServices.ContainsKey( Key) then
    result := FServices.Items[Key].FMembers.Count
  else
    result := 0
end;

function TServiceProvider.GetHashCode( const Value: RServiceKey): Integer;
var
  L: integer;
begin
result := BobJenkinsHash( Value.FServiceIID, SizeOf( Value.FServiceIID), 0);
L := Length( Value.FConfig);
if L > 0 then
  result := BobJenkinsHash( Value.FConfig[1], L * SizeOf( Value.FConfig[1]), result)
end;

function TServiceProvider.GetIsPooled(
  const ServiceIID: TGUID; const Config: string): boolean;
var
  Key: RServiceKey;
begin
Key.FServiceIID := ServiceIID;
Key.FConfig     := Config;
result := FServices.ContainsKey( Key) and FServices.Items[Key].FisPooled
end;

function TServiceProvider.GetNameByCookie( Cookie: integer): string;
var
  Key: RServiceKey;
  Idx: integer;
begin
if FindKey( Cookie, Key, Idx) then
    result := FServices.Items[Key].FMembers[Idx].FIdentifier
  else
    result := ''
end;


function TServiceProvider.GetServiceAffinity(
  const ServiceIID: TGUID; const Config: string): TServiceAffinity;
var
  Key: RServiceKey;
begin
Key.FServiceIID := ServiceIID;
Key.FConfig     := Config;
if FServices.ContainsKey( Key) then
    result := FServices.Items[Key].FAffinity
  else
    result := afCompetitive
end;


function TServiceProvider.GetServices: TDictionary<RServiceKey, RGroupOfServices>;
begin
result := FServices
end;

function TServiceProvider.Clone: IServiceProvider;
begin
result := TServiceProvider.CreateAsClone( FServices)
end;


procedure TServiceProvider.Merge( const Source: IServiceProvider);
begin
AddServices( (Source as IServiceProviderEx).GetServices)
end;

function TServiceProvider.NextCookie: integer;
var
  Group: RGroupOfServices;
  Value: RService;
begin
result := -1;
for Group in FServices.Values do
  for Value in Group.FMembers do
    if Value.FCookie > result then
      result := Value.FCookie;
Inc( result)
end;

procedure TServiceProvider.OnServiceNotify( Sender: TObject;
  const Item: RGroupOfServices; Action: TCollectionNotification);
begin
if FdoFreeGroupOnRemoval and (Action = cnRemoved) then
  Item.Free
end;

function TServiceProvider.RegisterFlyweightService(
   const ServiceIID: TGUID;
   CreateBase: TClass; Factory: TServiceFactory;
   const Config, SelectionArrayMemberName: string): integer;
var
  Key: RServiceKey;
  Group: RGroupOfServices;
  Value: RService;
begin
Key.FServiceIID := ServiceIID;
Key.FConfig     := Config;
if FServices.ContainsKey( Key) then
    Group := FServices.Items[ Key]
  else
    Group.Create;
Value.FLiveService := nil;
Value.FFlyweightCreateBase := CreateBase;
Value.FFlyweightFactory := Factory;
Value.FIdentifier := SelectionArrayMemberName;
if FisConstructing then
    result := -1
  else
    result := NextCookie;
Value.FCookie := result;
Group.FMembers.Add( Value);
if Group.FActiveIndex = -1 then
   Group.FActiveIndex := 0;
FServices.AddOrSetValue( Key, Group)
end;

function TServiceProvider.RegisterLiveService(
  const ServiceIID: TGUID;
  const Service: IInterface;
  const Config, SelectionArrayMemberName: string): integer;
var
  Key: RServiceKey;
  Group: RGroupOfServices;
  Value: RService;
begin
Key.FServiceIID := ServiceIID;
Key.FConfig     := Config;
if FServices.ContainsKey( Key) then
    Group := FServices.Items[ Key]
  else
    Group.Create;
Value.FFlyweightCreateBase := nil;
Value.FFlyweightFactory := nil;
Value.FLiveService := Service;
Value.FIdentifier := SelectionArrayMemberName;
if FisConstructing then
    result := -1
  else
    result := NextCookie;
Value.FCookie := result;
Group.FMembers.Add( Value);
if Group.FActiveIndex = -1 then
   Group.FActiveIndex := 0;
FServices.AddOrSetValue( Key, Group)
end;

procedure TServiceProvider.RegisterServiceClass(
  Registrant: TClass; const ServiceIID: TGUID);
var
  Cookies: TIntegerList;
begin
Cookies := nil;
try
  RegisterServiceClass( Registrant, ServiceIID, Cookies)
finally
  Cookies.Free
end
end;

type TServiceProc = reference to function (
    const ServiceIID: TGUID; const Config, Id: string;
    RegCls: TClass; ConstructorMethod: TRttiMethod): integer;

procedure TServiceProvider.ParseServiceClass(
  Registrant: TClass; const ServiceIID: TGUID;
  var Cookies: TIntegerList;
  ParseAction: TServiceProvider.TServiceProc);
var
  // Context: TRTTIContext;
  ServiceType: TRttiType;
  A: TCustomAttribute;
  M: TRttiMethod;
  ConfigAttrib: Configuration;
  j: integer;
  ConstructorParamCount: integer;
  Params: TArray<TRttiParameter>;
  Configs, Blacklist: TStrings;
  sConfig: string;
  Cookie: integer;
begin
if not assigned( Cookies) then
  Cookies := TIntegerList.Create;
if assigned( Registrant) and Supports( Registrant, ServiceIID) then
  begin
  // Context := TRTTIContext.Create;
  Configs := TStringList.Create;
  Blacklist := TStringList.Create;
  ServiceType := FRtti.GetType( Registrant);
  for A in ServiceType.GetAttributes do
    if A is BlacklistConfigs then
      Blacklist.AddStrings( BlacklistConfigs(A).FConfigs);
  for M in ServiceType.GetMethods do
    begin
    if not M.IsConstructor then continue;
    ConstructorParamCount := -1;
    Params := M.GetParameters;
    if Length( Params) = 0 then
        ConstructorParamCount := 0
      else if (Length( Params) = 1) and
              (Params[0].ParamType.TypeKind = tkUString) and
              (Params[0].Flags = [pfConst]) then
        ConstructorParamCount := 1;
    if ConstructorParamCount = -1 then continue;
    for A in M.GetAttributes do
      begin
      if not (A is Configuration) then continue;
      ConfigAttrib := Configuration(A);
      for j := 0 to ConfigAttrib.FConfigs.Count - 1 do
        begin
        sConfig := ConfigAttrib.FConfigs[j];
        if (Configs  .IndexOf( sConfig) <> -1) or
           (Blacklist.IndexOf( sConfig) <> -1) then
        continue;
        Configs.Add( sConfig);
        if assigned( ParseAction) then
            Cookie := ParseAction( ServiceIID, sConfig, ConfigAttrib.FId, Registrant, M)
          else
            Cookie := -1;
        if Cookie <> -1 then
          Cookies.Add( Cookie)
        end
      end
    end;
  Configs.Free;
  Blacklist.Free
  end
end;

procedure TServiceProvider.RegisterServiceClass(
  Registrant: TClass; const ServiceIID: TGUID; var Cookies: TIntegerList);
var
  PreCount: integer;
begin
if assigned( Cookies) then
    PreCount := Cookies.Count
  else
    PreCount := 0;
ParseServiceClass( Registrant, ServiceIID, Cookies,
  function (
    const ServiceIID: TGUID; const Config, Id: string;
    RegCls: TClass; ConstructorMethod: TRttiMethod): integer
  type
    AnonRegister_ActRec = record
      FServiceIID: TGUID;
      // FContext: TRttiContext;
      FConstructor: TRttiMethod;
      FCls: TClass;
      end;
  var
    Closure: AnonRegister_ActRec;
    s: string;
  begin
  Closure.FConstructor := ConstructorMethod;
  Closure.FCls         := RegCls;
  Closure.FServiceIID  := ServiceIID;
  result := RegisterFlyweightService(
    ServiceIID, Registrant,
    function( Obj: TObject; const Config: string;
              const ServiceProvider: IServiceProvider): IInterface
      begin
      if Length( Closure.FConstructor.GetParameters) = 0 then
        Closure.FConstructor.Invoke( Obj, [])
      else
        Closure.FConstructor.Invoke( Obj, [Config]);
      Obj.AfterConstruction;
      if not Supports( Obj, Closure.FServiceIID, result) then
        Obj.Free
      end,
    Config, Id)
  end
  );
if Cookies.Count = PreCount then
  raise Exception.CreateFmt( 'TServiceProvider.RegisterServiceClass - ' +
    '%s not validly decorated for registration.<br/>', [Registrant.ClassName])
end;


procedure TServiceProvider.SetActiveArrayMemberByCookie(
  const ServiceIID: TGUID; const Config: string; Cookie: integer);
var
  Key: RServiceKey;
  Group: RGroupOfServices;
  Idx: Integer;
begin
if Cookie = -1 then exit;
Key.FServiceIID := ServiceIID;
Key.FConfig     := Config;
if not FServices.ContainsKey( Key) then exit;
Group := FServices.Items[ Key];
for Idx := 0 to Group.FMembers.Count - 1 do
  begin
  if Group.FMembers[Idx].FCookie <> Cookie then continue;
  if Group.FActiveIndex <> Idx then
    begin
    Group.FActiveIndex := Idx;
    FServices.Items[ Key] := Group
    end;
  break
  end
end;

procedure TServiceProvider.SetActiveArrayMemberByName(
  const ServiceIID: TGUID; const Config, Name: string);
var
  Key: RServiceKey;
  Group: RGroupOfServices;
  Value: RService;
  Cookie: integer;
begin
Key.FServiceIID := ServiceIID;
Key.FConfig     := Config;
if not FServices.ContainsKey( Key) then exit;
Group := FServices.Items[ Key];
Cookie := -1;
for Value in Group.FMembers do
  begin
  if Value.FIdentifier <> Name then continue;
  Cookie := Value.FCookie;
  break
  end;
if Cookie <> -1 then
  SetActiveArrayMemberByCookie( ServiceIID, Config, Cookie)
end;

procedure TServiceProvider.SetIsPooled(
  const ServiceIID: TGUID; const Config: string; Value: boolean);
var
  Key: RServiceKey;
  Group: RGroupOfServices;
begin
Key.FServiceIID := ServiceIID;
Key.FConfig     := Config;
if FServices.ContainsKey( Key) then
    begin
    Group := FServices.Items[Key];
    if Group.FisPooled <> Value then
      begin
      Group.FisPooled := Value;
      FServices.Items[Key] := Group
      end
    end
  else if Value then
    raise Exception.Create('Cannot set isPooled on unregistered service');
end;

procedure TServiceProvider.SetServiceAffinity(
  const ServiceIID: TGUID; const Config: string; Value: TServiceAffinity);
var
  Key: RServiceKey;
  Old: TServiceAffinity;
  Group: RGroupOfServices;
begin
Old := GetServiceAffinity( ServiceIID, Config);
if Value = Old then exit;
Key.FServiceIID := ServiceIID;
Key.FConfig     := Config;
if FServices.ContainsKey( Key) then
    begin
    Group := FServices.Items[ Key];
    Group.FAffinity := Value;
    if (Value = afCompetitive) and ((Group.FActiveIndex < 0) or
                                    (Group.FActiveIndex >= Group.FMembers.Count)) then
      Group.FActiveIndex := 0;
    FServices.Items[ Key] := Group
    end
  else
    begin
    Group.Create;
    Group.FAffinity := Value;
    FServices.Add( Key, Group)
    end
end;

procedure TServiceProvider.ShutDown;
begin
FdoFreeGroupOnRemoval := True;
FServices.Clear;
FdoFreeGroupOnRemoval := False
end;

procedure TServiceProvider.RemoveGroup( const Key: RServiceKey);
begin
FdoFreeGroupOnRemoval := True;
FServices.Remove( Key);
FdoFreeGroupOnRemoval := False
end;

procedure TServiceProvider.DeregisterLiveServiceByName(
  const ServiceIID: TGUID; const Config, SelectionArrayMemberName: string);
begin
DeregisterServiceByCookie( GetCookieByName( ServiceIID, Config, SelectionArrayMemberName))
end;

procedure TServiceProvider.DeregisterServiceByCookie( Cookie: integer);
var
  Key: RServiceKey;
  Idx: Integer;
  Group: RGroupOfServices;
begin
if (Cookie <> -1) and FindKey( Cookie, Key, Idx) then
  begin
  Group := FServices.Items[ Key];
  Group.FMembers.Delete( Idx);
  if Group.FActiveIndex >= Group.FMembers.Count then
     Group.FActiveIndex := Group.FMembers.Count - 1;
  if Group.FMembers.Count = 0 then
      RemoveGroup( Key)
    else
      FServices.Items[Key] := Group
  end;
end;


procedure TServiceProvider.DeregisterServicesByCookie( Cookies: TIntegerList);
var
  Cookie: integer;
begin
for Cookie in Cookies do
  DeregisterServiceByCookie( Cookie)
end;


procedure TServiceProvider.DeregisterServicesOfClass(
  Registrant: TClass; const ServiceIID: TGUID);
var
  Cookies: TIntegerList;
begin
Cookies := GetCookiesOfServiceClass( Registrant, ServiceIID);
try
  DeregisterServicesByCookie( Cookies)
finally
  Cookies.Free
  end
end;


type TSBDUnit_BaseAttributeClass = class of TSBDUnit_BaseAttribute;

function IsMemberDecorated( const Memb: IInjectableMember; AttriCls: TSBDUnit_BaseAttributeClass;
                            var Attri: TSBDUnit_BaseAttribute): boolean;
var
  Run: TCustomAttribute;
begin
result := False;
for Run in Memb.AsRttiMember.GetAttributes do
  begin
  result := Run.InheritsFrom( AttriCls);
  if not result then continue;
  Attri := Run as TSBDUnit_BaseAttribute;
  break
  end;
end;

constructor TSuiteInjector.Create(
  InjectionSubject1: TObject; ServiceType1: TRttiType;
  const Client1: IInterface; const InjectionOverride: IServiceProvider);
begin
FInjectionSubject  := InjectionSubject1;
FServiceType       := ServiceType1;
FClient            := Client1;
FInjectionOverride := InjectionOverride;
Supports( FInjectionSubject, IInterface, FInjectionsClient)
end;


destructor TSuiteInjector.Destroy;
begin
FMembers.Free;
inherited
end;



function TSuiteInjector.GetInjectionMetaData(
          const Provider: IServiceProvider;
          const Member: IInjectableMember;
          var   Providers: TList<IServiceProvider>;
          var   InjectableGUID: TGUID;
          var   sInjectionConfig: string;
          var   Affinity: TServiceAffinity): boolean;
var
  MemberType1: TRttiType;
  InjectionAttri: TSBDUnit_BaseAttribute;
  ProvisionWithMethod: TRttiMethod;
  MemSP: TSBDUnit_BaseAttribute;

  procedure IncludeProviderIfUseful( const AddendProv: IServiceProvider);
  begin
  if assigned( AddendProv) and (AddendProv.GetGroupCount( InjectableGUID, sInjectionConfig) > 0) then
    Providers.Add( AddendProv)
  end;

begin
MemberType1 := Member.MemberType;
sInjectionConfig := '';
InjectionAttri := nil;
result := IsMemberDecorated( Member, Injection, InjectionAttri) and
          (MemberType1.TypeKind = tkInterface) and
          (MemberType1 is TRttiInterfaceType) and
          Member.IsInjectable;
if not result then exit;
if assigned( Providers) then
    Providers.Clear
  else
    Providers := TList<IServiceProvider>.Create;
sInjectionConfig := (InjectionAttri as Injection).FConfig;
if IsMemberDecorated( Member, ProvisionWith, MemSP) then
    ProvisionWithMethod := FServiceType.GetMethod( ProvisionWith(MemSP).Name)
  else
    ProvisionWithMethod := nil;
if assigned( ProvisionWithMethod) and
   (
     ProvisionWithMethod.IsConstructor or
     ProvisionWithMethod.IsDestructor or
     ProvisionWithMethod.IsClassMethod or
     (Length( ProvisionWithMethod.GetParameters) <> 0) or
     (not assigned( ProvisionWithMethod.ReturnType)) or
     (ProvisionWithMethod.ReturnType.TypeKind <> tkInterface) or
     (not (ProvisionWithMethod.ReturnType is TRttiInterfaceType)) or
     (not IsEqualGUID( TRttiInterfaceType( ProvisionWithMethod.ReturnType).GUID, IServiceProvider))
   ) then
   ProvisionWithMethod := nil;
InjectableGUID := TRttiInterfaceType( MemberType1).GUID;
IncludeProviderIfUseful( FInjectionOverride);
if assigned( ProvisionWithMethod) then
  IncludeProviderIfUseful( ProvisionWithMethod.Invoke( FInjectionSubject, []).AsInterface as IServiceProvider);
IncludeProviderIfUseful( Provider);
if Providers.Count > 0 then
    Affinity := Providers[0].GetServiceAffinity( InjectableGUID, sInjectionConfig)
  else
    result := False
end;

type
TTargetFinder = class abstract
  public
    class function FindTargetMember(
      SuiteInjector: TSuiteInjector;
      const BaseMember: IInjectableMember;
      BaseMemberIndex: integer;
      var TargetMember: IInjectableMember): boolean; virtual; abstract;
  end;
TTargetFinderClass = class of TTargetFinder;

TCompetitiveTargetFinder = class sealed( TTargetFinder)
  public
    class function FindTargetMember(
      SuiteInjector: TSuiteInjector;
      const BaseMember: IInjectableMember;
      BaseMemberIndex: integer;
      var TargetMember: IInjectableMember): boolean; override;
  end;

TCooperativeTargetFinder = class sealed( TTargetFinder)
  public
    class function FindTargetMember(
      SuiteInjector: TSuiteInjector;
      const BaseMember: IInjectableMember;
      BaseMemberIndex: integer;
      var TargetMember: IInjectableMember): boolean; override;
  end;

class function TCompetitiveTargetFinder.FindTargetMember(
      SuiteInjector: TSuiteInjector;
      const BaseMember: IInjectableMember;
      BaseMemberIndex: integer;
      var TargetMember: IInjectableMember): boolean;
begin
TargetMember := BaseMember;
result := assigned( TargetMember)
end;

class function TCooperativeTargetFinder.FindTargetMember(
      SuiteInjector: TSuiteInjector;
      const BaseMember: IInjectableMember;
      BaseMemberIndex: integer;
      var TargetMember: IInjectableMember): boolean;
var
  Mem2Number: integer;
  Member2: IInjectableMember;
  MemberType2: TRttiType;
  isGang: boolean;
  GangAttri: TSBDUnit_BaseAttribute;
  GangGUID: TGUID;
begin
TargetMember := nil;
Mem2Number := -1;
for Member2 in SuiteInjector.GetMembers do
  begin
  Inc( Mem2Number);
  MemberType2 := Member2.MemberType;
  isGang := (BaseMemberIndex < Mem2Number) and IsMemberDecorated( Member2, GangHere, GangAttri);
  if (not isGang) or (BaseMemberIndex >= Mem2Number) or
     (MemberType2.TypeKind <> tkInterface) or
     (not (MemberType2 is TRttiInterfaceType)) then continue;
  GangGUID := TRttiInterfaceType( MemberType2).GUID;
  if not IsEqualGUID( GangGUID, IInterfaceList) then continue;
  TargetMember := Member2;
  break
  end;
result := assigned( TargetMember)
end;

const
  TargetFinders: array[TServiceAffinity] of TTargetFinderClass =
    (TCompetitiveTargetFinder, TCooperativeTargetFinder);

procedure TSuiteInjector.Inject( const Provider: IServiceProvider);
var
  Member, TargetMember: IInjectableMember;
  sInjectionConfig: string;
  Inject: IInterface;
  InjectAsRttiValue: TValue;
  InjectableGUID: TGUID;
  Affinity: TServiceAffinity;
  Mem1Number: integer;
  Providers: TList<IServiceProvider>;
  Provider1: IServiceProvider;
  Addends: IInterfaceList;
  Addend: IInterface;
  doInject: boolean;

  function Acquire( const FromProvider: IServiceProvider;  const ServiceIID: TGUID; out Intf): boolean;
  begin
     result := FromProvider.Acquire( FInjectionsClient, ServiceIID, Intf, sInjectionConfig, nil)
  end;

begin
Mem1Number := -1;
Providers  := nil;
try
  for Member in GetMembers do
    begin
    Inc( Mem1Number);
    Inject := nil;
    doInject := False;
    if GetInjectionMetaData( Provider, Member,
                             Providers, InjectableGUID, sInjectionConfig, Affinity) and
       TargetFinders[Affinity].FindTargetMember( self, Member, Mem1Number, TargetMember) then
         begin
         if IsEqualGUID( InjectableGUID, IInterface) and
            (sInjectionConfig = sClientRef) then
             begin
             Inject   := FClient;
             doInject := assigned( Inject)
             end
           else if (Affinity = afCompetitive) or (Providers.Count = 1) then
                doInject := Acquire( Providers[0], InjectableGUID, Inject)
           else
             begin
             for Provider1 in Providers do
               begin
               if not assigned( Inject) then
                   doInject := Acquire( Provider1, InjectableGUID, Inject)
                 else if Acquire( Provider1, IInterfaceList, Addends) then
                   begin
                   doInject := True;
                   for Addend in (Addends as IInterfaceListEx) do
                     IInterfaceList( Inject).Add( Addend)
                   end;
                end;
             end
        end;
    if doInject then
      begin
      TValue.Make( @Inject, TargetMember.MemberType.Handle, InjectAsRttiValue);
      TargetMember.SetValue( InjectAsRttiValue)
      end
    end
finally
  Providers.Free
  end
end;

{ TDataMemberInjector }

function TDataMemberInjector.GetMembers: TEnumerable<IInjectableMember>;
begin
if not Assigned( FMembers) then
  FMembers := TFieldEnumerable.Create( self);
result := FMembers
end;


{ TWritablePropertyInjector }

function TWritablePropertyInjector.GetMembers: TEnumerable<IInjectableMember>;
begin
if not Assigned( FMembers) then
  FMembers := TPropEnumerable.Create( self);
result := FMembers
end;


{ TInjectableMember }

constructor TInjectableMember.Create( Owner1: TSuiteInjector);
begin
FOwner := Owner1
end;

{ TInjectableDataMember }

function TInjectableDataMember.AsRttiMember: TRttiMember;
begin
result := FMember
end;

constructor TInjectableDataMember.Create(
  Owner1: TSuiteInjector; Member1: TRttiField);
begin
inherited Create( Owner1);
FMember := Member1
end;


function TInjectableDataMember.IsInjectable: boolean;
begin
result := True
end;


function TInjectableDataMember.MemberType: TRttiType;
begin
result := FMember.FieldType
end;


procedure TInjectableDataMember.SetValue( const Inject: TValue);
begin
FMember.SetValue( FOwner.FInjectionSubject, Inject)
end;

{ TInjectablePropMember }

function TInjectablePropMember.AsRttiMember: TRttiMember;
begin
result := FMember
end;

constructor TInjectablePropMember.Create(
  Owner1: TSuiteInjector; Member1: TRttiProperty);
begin
inherited Create( Owner1);
FMember := Member1
end;

function TInjectablePropMember.IsInjectable: boolean;
begin
result := FMember.IsWritable and (FMember is TRttiInstanceProperty)
end;

function TInjectablePropMember.MemberType: TRttiType;
begin
result := FMember.PropertyType
end;

procedure TInjectablePropMember.SetValue( const Inject: TValue);
begin
FMember.SetValue( FOwner.FInjectionSubject, Inject)
end;

{ TMemberEnumable<MemberT> }

constructor TMemberEnumable<MemberT>.Create( Suite1: TSuiteInjector);
begin
FSuite := Suite1
end;

{ TPropEnumerable }

constructor TPropEnumerable.Create( Suite1: TSuiteInjector);
begin
inherited Create( Suite1);
FMembers := FSuite.FServiceType.GetProperties
end;

function TPropEnumerable.DoGetEnumerator: TEnumerator<IInjectableMember>;
begin
result := TPropEnumator.Create( self)
end;

{ TFieldEnumerable }

constructor TFieldEnumerable.Create( Suite1: TSuiteInjector);
begin
inherited Create( Suite1);
FMembers := FSuite.FServiceType.GetFields
end;

function TFieldEnumerable.DoGetEnumerator: TEnumerator<IInjectableMember>;
begin
result := TFieldEnumator.Create( self)
end;

{ TMemberEnumator<MemberT> }

constructor TMemberEnumator<MemberT>.Create( Owner1: TMemberEnumable<MemberT>);
begin
FOwner := Owner1;
FIndex := -1
end;

function TMemberEnumator<MemberT>.DoMoveNext: Boolean;
begin
result := FIndex < Length( FOwner.FMembers);
if not result then exit;
inc( FIndex);
result := FIndex < Length( FOwner.FMembers)
end;

{ TPropEnumator<MemberT> }

constructor TPropEnumator.Create( Owner1: TMemberEnumable<TRttiProperty>);
begin
inherited Create( Owner1)
end;

function TPropEnumator.DoGetCurrent: IInjectableMember;
begin
result := TInjectablePropMember.Create( FOwner.FSuite, FOwner.FMembers[FIndex])
end;

{ TFieldEnumator }

constructor TFieldEnumator.Create(Owner1: TMemberEnumable<TRttiField>);
begin
inherited Create( Owner1)
end;

function TFieldEnumator.DoGetCurrent: IInjectableMember;
begin
result := TInjectableDataMember.Create( FOwner.FSuite, FOwner.FMembers[FIndex])
end;


{ RService }

procedure RService.Create;
begin
FLiveService := nil;
FFlyweightCreateBase := nil;
FFlyweightFactory := nil;
FCookie := -1;
FIdentifier := ''
end;


{ RGroupOfServices }

procedure RGroupOfServices.Create;
begin
FAffinity    := afCompetitive;
FMembers     := TServiceList.Create;
FIsPooled    := False;
FActiveIndex := -1
end;

procedure RGroupOfServices.Free;
begin
FMembers.Free
end;

{ RGroupOfServices.TServiceList }

procedure RGroupOfServices.TServiceList.CheckPoolable(
  Idx: integer; const LiveService: IInterface);
var
  Member: RService;
begin
Member := Items[Idx];
if assigned( Member.FLiveService) or (not assigned( LiveService)) or
   (Idx < 0) or (Idx >= Count) then exit;
FisPooling := True;
try
  Member.FLiveService := LiveService;
  Items[ Idx] := Member
finally
  FisPooling := False
  end
end;

constructor RGroupOfServices.TServiceList.Create;
begin
inherited;
FisPooling := False;
end;



end.
