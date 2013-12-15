unit DUnitX.SBD.uServiceProvider;
interface
uses Classes, Generics.Collections;

const
  CSI = '\';   // Config string Control Sequence Introducer. Control Sequences are \\ or \,
  Sep = ',';   // Config string list item seperator.
  sClientRef = 'client'; // Use: decorate an IInterface data member/property with [Injection('client')]

type
IServiceProvider = interface;
TServiceFactory = reference to function( Obj: TObject; const Config: string;
  const ServiceProvider: IServiceProvider): IInterface;

TServiceAffinity = (
  afcompetitive,     // A single service or an array of competing services with
                     //  the same interface and config. Select one, the 'active'
                     //  service, like radio buttons to be the one acquired.
  afCooperative);    // A gang of services with the same interface and config,
                     //  working co-operatively together to solve a shared problem.


TIntegerList = TList<integer>;

IServiceProvider = interface
  ['{4E38BEF6-2768-46E3-8740-25B8C526C912}']
    {$REGION 'property accessors'}
    function  GetServiceAffinity( const ServiceIID: TGUID; const Config: string): TServiceAffinity;
    procedure SetServiceAffinity( const ServiceIID: TGUID; const Config: string; Value: TServiceAffinity);
    function  GetActiveArrayMemberCookie( const ServiceIID: TGUID; const Config: string): integer;
    procedure SetActiveArrayMemberByCookie( const ServiceIID: TGUID; const Config: string; Cooki: integer);
    function  GetActiveArrayMemberName( const ServiceIID: TGUID; const Config: string): string;
    procedure SetActiveArrayMemberByName( const ServiceIID: TGUID; const Config, Name: string);
    function  GetNameByCookie( Cookie: integer): string;
    function  GetIsPooled( const ServiceIID: TGUID; const Config: string): boolean;
    procedure SetIsPooled( const ServiceIID: TGUID; const Config: string; Value: boolean);
    {$ENDREGION}

    // Lookup methods
    function GetGroupCount( const ServiceIID: TGUID; const Config: string): integer;
    function GetCookies( const ServiceIID: TGUID; const Config: string): TIntegerList;
    function GetCookieByName( const ServiceIID: TGUID; const Config, Name: string): integer;
    function GetCookieOfLiveService( const ServiceIID: TGUID; const Service: IInterface; const Config: string): integer;
    function GetCookiesOfServiceClass( Registrant: TClass; const ServiceIID: TGUID): TIntegerList;
    property  ActiveArrayMemberCookie[ const ServiceIID: TGUID; const Config: string]: integer
                    read GetActiveArrayMemberCookie write SetActiveArrayMemberByCookie;
    property  ActiveArrayMemberName[ const ServiceIID: TGUID; const Config: string]: string
                    read GetActiveArrayMemberName write SetActiveArrayMemberByName;
    property  NameByCookie[ Cookie: integer]: string   read GetNameByCookie;

    // Registration methods
    function  RegisterLiveService( const ServiceIID: TGUID; const Service: IInterface;
                                   const Config: string = ''; const SelectionArrayMemberName: string = ''): integer;
    function  RegisterFlyweightService( const ServiceIID: TGUID; CreateBase: TClass; Factory: TServiceFactory;
                                        const Config: string = ''; const SelectionArrayMemberName: string = ''): integer;
    procedure RegisterServiceClass( Registrant: TClass; const ServiceIID: TGUID; var Cookies: TIntegerList); overload;
    procedure RegisterServiceClass( Registrant: TClass; const ServiceIID: TGUID                           ); overload;

    // Deregistration methods
    procedure DeregisterServiceByCookie( Cookie: integer);
    procedure DeregisterServicesByCookie( Cookies: TIntegerList);
    procedure DeregisterLiveServiceByName( const ServiceIID: TGUID; const Config: string = ''; const SelectionArrayMemberName: string = '');
    procedure DeregisterServicesOfClass( Registrant: TClass; const ServiceIID: TGUID);

    // Acquisition methods
    function  Acquire( const Client: IInterface; const ServiceIID: TGUID; out Intf; const Config: string = ''; const InjectionOverride: IServiceProvider = nil): boolean;
    function  AcquireGang( const Client: IInterface; const ServiceIID: TGUID; var Gang: IInterfaceList; const Config: string = ''; const InjectionOverride: IServiceProvider = nil): boolean;

    // Life-cycle methods
    procedure ShutDown;
    function  Clone: IServiceProvider;
    procedure Merge( const Source: IServiceProvider); // Add Source's services to this.

    // Main properties of service acquisition
    property  ServiceAffinity[ const ServiceIID: TGUID; const Config: string]: TServiceAffinity   read GetServiceAffinity write SetServiceAffinity;
    property  IsPooled[ const ServiceIID: TGUID; const Config: string]: boolean
                    read GetIsPooled write SetIsPooled;
  end;


TSBDUnit_BaseAttribute = class( TCustomAttribute) end;

Injection = class( TSBDUnit_BaseAttribute)
  public
    FConfig: string;

    constructor Create;                           overload;
    constructor Create( const Config1: string);   overload;
  end;

GangHere = class( TSBDUnit_BaseAttribute)
  public
    constructor Create;
  end;

Configuration = class( TSBDUnit_BaseAttribute)
   public
     FConfigs: TStrings;
     FMaxPoolSize: integer; // 0 for unlimited.
     FId: string;

     constructor Create;                             overload;
     constructor Create( const Config1: string);     overload;
     destructor Destroy; override;
   end;

BlacklistConfigs = class( TSBDUnit_BaseAttribute)
   public
     FConfigs: TStrings;

     constructor Create( const Config1: string);     overload;
     destructor Destroy; override;
   end;

ProvisionWith = class( TSBDUnit_BaseAttribute)
  // Used to decorate function X: IServiceProvider;
  public
    Name: string;
    constructor Create( const Name1: string);
  end;


function StandardServiceProvider: IServiceProvider;

implementation





uses SysUtils, DUnitX.SBD.uServiceProviderImpl;
{ Configuration }

constructor Configuration.Create;
begin
FConfigs := TStringList.Create;
FConfigs.Add('');
FId := '';
FMaxPoolSize := 0// 0 for unlimited.
end;

function CommaSplit( const CommaSeparedList: string): TStrings;
var
  Left, Right: string;
  p, q: integer;
  FoundSep: boolean;
begin
 // Comma separated string.
 // Escape sequence:  \,  ==>  ,
 // Escape sequence:  \\  ==>  \
result := TStringList.Create;
Right := CommaSeparedList;
repeat
  Left := '';
  FoundSep := False;
  while (Right <> '') and (not FoundSep) do
    begin
    p := Pos( Sep, Right);
    q := Pos( CSI, Right);
    if (p = 0) or ((q <> 0) and (q < p)) then
      p := q;
    if p > 0 then
        begin
        if Right[p] = Sep then
            begin
            Left  := Left + Copy( Right, 1, p-1);
            Delete( Right, 1, p);
            FoundSep := True;
            end
          else if (p < Length(Right)) and ((Right[p+1]=Sep) or (Right[p+1]=CSI)) then
            begin
            Left  := Left + Copy( Right, 1, p-1) + Right[p+1];
            Delete( Right, 1, p+1)
            end
          else
            begin
            Left  := Left + Copy( Right, 1, p);
            Delete( Right, 1, p)
            end
        end
      else
        begin
        Left  := Left + Right;
        Right := '';
        FoundSep := True;
        end
    end;
  if result.IndexOf( Left) = -1 then
    result.Add( Left)
until Right = '';
if result.Count = 0 then
  result.Add( '')
end;


constructor Configuration.Create( const Config1: string);
const
  IdIntro = 'Id=';
var
  j: integer;
begin
FConfigs := CommaSplit( Config1);
for j := FConfigs.Count - 1 downto 0 do
  if SameText( Copy( Trim( FConfigs[j]), 1, Length( IdIntro)), IdIntro) then
    begin
    FId := Trim( FConfigs[j]);
    Delete( FId, 1, Length( IdIntro));
    FId := Trim( FId);
    FConfigs.Delete( j)
    end;
if FConfigs.Count = 0 then
  FConfigs.Add( '');
FMaxPoolSize := 0// 0 for unlimited.
end;


destructor Configuration.Destroy;
begin
FConfigs.Free;
inherited
end;


{ Injection }

constructor Injection.Create;
begin
FConfig := ''
end;

constructor Injection.Create( const Config1: string);
begin
FConfig := Config1
end;

{ BlacklistConfigs }

constructor BlacklistConfigs.Create( const Config1: string);
begin
FConfigs := CommaSplit( Config1)
end;

destructor BlacklistConfigs.Destroy;
begin
FConfigs.Free;
inherited
end;

{ GangHere }

constructor GangHere.Create;
begin
end;


constructor ProvisionWith.Create( const Name1: string);
begin
Name := Name1
end;


function StandardServiceProvider: IServiceProvider;
begin
result := DUnitX.SBD.uServiceProviderImpl.TServiceProvider.Create as IServiceProvider
end;

end.
