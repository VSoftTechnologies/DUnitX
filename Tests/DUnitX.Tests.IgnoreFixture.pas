unit DUnitX.Tests.IgnoreFixture;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  [Ignore('Ignore complete Fixture.')]
  TIgnoredTest = class
  public
    [Setup]
    procedure Setup;
    [SetupFixture]
    procedure SetupFixture;
    [Teardown]
    procedure Teardown;
    [TeardownFixture]
    procedure TeardownFixture;
    [Test]
    procedure Test_A;
    [Test]
    procedure Test_B;
  end;

  [TestFixture]
  TNoActiveTests = class
  public
    [Setup]
    procedure Setup;
    [SetupFixture]
    procedure SetupFixture;
    [Teardown]
    procedure Teardown;
    [TeardownFixture]
    procedure TeardownFixture;
    [Test]
    [Ignore('Ignore Test.')]
    procedure IgnoredTest_A;
    [Test]
    [Ignore('Ignore Test.')]
    procedure IgnoredTest_B;
  end;

implementation

uses
  System.SysUtils;

{ TIgnoredTest }

procedure TIgnoredTest.Setup;
begin
  raise Exception.Create('Setup');
end;

procedure TIgnoredTest.SetupFixture;
begin
  raise Exception.Create('SetupFixture');
end;

procedure TIgnoredTest.Teardown;
begin
  raise Exception.Create('Teardown');
end;

procedure TIgnoredTest.TeardownFixture;
begin
  raise Exception.Create('TeardownFixture');
end;

procedure TIgnoredTest.Test_A;
begin
  raise Exception.Create('Test_A');
end;

procedure TIgnoredTest.Test_B;
begin
  raise Exception.Create('Test_B');
end;

{ TNoActiveTests }

procedure TNoActiveTests.IgnoredTest_A;
begin
  raise Exception.Create('IgnoredTest_A');
end;

procedure TNoActiveTests.IgnoredTest_B;
begin
  raise Exception.Create('IgnoredTest_B');
end;

procedure TNoActiveTests.Setup;
begin
  raise Exception.Create('Setup');
end;

procedure TNoActiveTests.SetupFixture;
begin
  raise Exception.Create('SetupFixture');
end;

procedure TNoActiveTests.Teardown;
begin
  raise Exception.Create('Teardown');
end;

procedure TNoActiveTests.TeardownFixture;
begin
  raise Exception.Create('TeardownFixture');
end;

initialization
  TDUnitX.RegisterTestFixture(TIgnoredTest);
  TDUnitX.RegisterTestFixture(TNoActiveTests);

end.
