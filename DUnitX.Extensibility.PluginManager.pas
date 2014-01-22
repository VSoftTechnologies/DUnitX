unit DUnitX.Extensibility.PluginManager;

interface

uses
  DUnitX.Generics,
  DUnitX.Extensibility;
type
  IPluginManager = interface
    ['{0AD83588-CF0F-4185-B5F8-093893150BB3}']
    procedure Init;
    procedure CreateFixtures;
  end;

  TCreateFixtureProc = function(const AInstance : TObject; const AFixtureClass: TClass; const AName: string): ITestFixture of object;

  TPluginManager = class(TInterfacedObject,IPluginManager,IPluginLoadContext,IFixtureProviderContext)
  private
    FFixtureProviders : IList<IFixtureProvider>;
    FCreateFixtureProc : TCreateFixtureProc;
    FUseRtti : boolean;
  protected
    //IPluginLoader
    procedure Init;
    procedure CreateFixtures;

    //IPluginLoadContext
    procedure RegisterFixtureProvider(const provider: IFixtureProvider);

    //IFixtureProviderContext
    function CreateFixture(const AFixtureClass: TClass; const AName: string): ITestFixture;overload;
    function CreateFixture(const AInstance : TObject; const AName: string): ITestFixture;overload;
    function GetUseRtti: Boolean;
  public
    constructor Create(const ACreateFixtureProc : TCreateFixtureProc; const AUseRtti : boolean);
    destructor Destroy;override;


  end;

implementation

uses
  DUnitX.TestFramework;

{ TPluginManager }

constructor TPluginManager.Create(const ACreateFixtureProc : TCreateFixtureProc; const AUseRtti : boolean);
begin
  FFixtureProviders := TDUnitXList<IFixtureProvider>.Create;
  FCreateFixtureProc := ACreateFixtureProc;
  FUseRtti := AUseRtti;
end;

function TPluginManager.CreateFixture(const AFixtureClass: TClass; const AName: string): ITestFixture;
begin
  //delegate back to the runner so it can own the fixture.
  result := FCreateFixtureProc(nil,AFixtureClass,AName);
end;

function TPluginManager.CreateFixture(const AInstance: TObject; const AName: string): ITestFixture;
begin
  //delegate back to the runner so it can own the fixture.
  result := FCreateFixtureProc(AInstance,AInstance.ClassType,AName);

end;

procedure TPluginManager.CreateFixtures;
var
  provider : IFixtureProvider;
begin
  for provider in FFixtureProviders do
  begin
    provider.Execute(Self);
  end;
end;

destructor TPluginManager.Destroy;
begin
  FFixtureProviders := nil;//not needed, just aids debugging;
  inherited;
end;

function TPluginManager.GetUseRtti: Boolean;
begin
  result := FUseRtti;
end;

procedure TPluginManager.Init;
var
  plugin : IPlugin;
begin
  for plugin in TDUnitX.RegisteredPlugins do
  begin
    plugin.GetPluginFeatures(Self);
  end;
end;

procedure TPluginManager.RegisterFixtureProvider(const provider: IFixtureProvider);
begin
  FFixtureProviders.Add(provider);
end;

end.
