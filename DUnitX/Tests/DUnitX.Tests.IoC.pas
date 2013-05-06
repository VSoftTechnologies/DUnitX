unit DUnitX.Tests.IoC;

interface

uses
  DUnitX.TestFramework,
  DUnitX.IoC;

type
  {$M+}
  IFoo = interface
  ['{FA01B68C-0D64-4251-91C6-F38617A56B33}']
    procedure Bar;
  end;

  {$M+}
  ISingleton = interface
  ['{615BDB28-13D0-4CAF-B84A-3C77B479B9AE}']
    function HowMany : integer;
  end;

  TDUnitX_IoCTests = class
  public
    [SetupFixture]
     procedure Setup;

    [Test]
    procedure Test_Implementation_Non_Singleton;

    [Test]
    procedure Test_Implementation_Singleton;


    [Test]
    procedure Test_Activator_Non_Singleton;

    [Test]
    procedure Test_Activator_Singleton;


  end;

implementation

type
  TFoo = class(TInterfacedObject,IFoo)
  protected
    procedure Bar;
  end;




{ TDUnitX_IoCTests }

procedure TDUnitX_IoCTests.Setup;
begin
  TDUnitXIoC.RegisterType<IFoo>(
      function : IFoo
      begin
        result := TFoo.Create;
      end);

  TDUnitXIoC.RegisterType<IFoo,TFoo>('test');

//singletons
  TDUnitXIoC.RegisterType<IFoo>(true,
      function : IFoo
      begin
        result := TFoo.Create;
      end,
      'activator_singleton');

  TDUnitXIoC.RegisterType<IFoo,TFoo>(true,'impl_singleton');

end;

procedure TDUnitX_IoCTests.Test_Activator_Non_Singleton;
var
  foo1 : IFoo;
  foo2 : IFoo;
begin
  foo1 := TDUnitXIoC.Resolve<IFoo>();
  foo2 := TDUnitXIoC.Resolve<IFoo>();
  Assert.AreNotSame(foo1,foo2);

end;

procedure TDUnitX_IoCTests.Test_Activator_Singleton;
var
  foo1 : IFoo;
  foo2 : IFoo;
begin
  foo1 := TDUnitXIoC.Resolve<IFoo>('activator_singleton');
  foo2 := TDUnitXIoC.Resolve<IFoo>('activator_singleton');
  Assert.AreSame(foo1,foo2);
  foo1 := TDUnitXIoC.Resolve<IFoo>('impl_singleton');
  Assert.AreNotSame(foo1,foo2);

end;

procedure TDUnitX_IoCTests.Test_Implementation_Non_Singleton;
var
  foo1 : IFoo;
  foo2 : IFoo;
begin
  foo1 := TDUnitXIoC.Resolve<IFoo>('test');
  foo2 := TDUnitXIoC.Resolve<IFoo>('test');
  Assert.AreNotSame(foo1,foo2);

end;

procedure TDUnitX_IoCTests.Test_Implementation_Singleton;
var
  foo1 : IFoo;
  foo2 : IFoo;
begin
  foo1 := TDUnitXIoC.Resolve<IFoo>('impl_singleton');
  foo2 := TDUnitXIoC.Resolve<IFoo>('impl_singleton');
  Assert.AreSame(foo1,foo2);
end;

{ TFoo }

procedure TFoo.Bar;
begin
  Write('Bar');
end;

initialization
  TDUnitX.RegisterTestFixture(TDUnitX_IoCTests);
end.
