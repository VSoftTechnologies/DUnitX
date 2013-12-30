unit DUnitX.Expert.CodeGen.Templates;

interface

resourcestring

 SSimpleDPR = 'program %0:s;'#13#10 +
 #13#10 +
 'begin'#13#10 +
 'end.'#13#10;

 // 0 - Unit Name
 // 1 - Class Name
 // 2 - Setup/TearDown Methods - Interface
 // 3 - Sample Methods - Interface
 // 4 - Setup/TearDown Methods - Implementation
 // 5 - Sample Methods - Implementation
 STestUnit = 'unit %0:s;#13#10 + ' +
 #13#10 +
 'interface'#13#10 +
 'uses'#1310 +
 '  DUnitX.TestFramework'#13#10 +
 #13#10 +
 'type'#13#10 +
 '  [TestFixture]'#13#10 +
 '  %1s = class(TObject) '#13#10 +
 '  public'#1310 +
 '%2s%3s' +
 '  end;'#13#10 +
 'implementation'#13#10 +
 #13#10 +
 '%4s%5s' +
 #13#10 +
 'end.'#13#10;

 SSetupTearDownIntf =
 '  [Setup]'#13#10 +
 '  procedure Setup;'#13#10  +
 '  [TearDown]'#13#10  +
 '  procedure TearDown;'#13#10;

 // 0 - Class Name
 SSetupTearDownImpl =
 'procedure %0s.Setup;'#13#10  +
 'begin'#13#10 +
 'end;'#13#10 +
 #13#10 +
 'procedure %0s.TearDown;'#13#10 +
 'begin'#13#10 +
 'end;'#13#10 +
 #13#10;


 SSampleMethodsIntf =
 '  [Test]'#13#10 +
 '  procedure Test1;'#13#10 +
 '  [Test]'#13#10 +
 '  [TestCase(''TestA'',''1,2'')]'#13#10 +
 '  [TestCase(''TestA'',''3,4'')]'#13#10 +
 '  procedure Test2(const AValue1 : Integer;const AValue2 : Integer);'#13#10;

 // 0 - Class Name
 //TODO: Show Examples of calling Assert
 SSampleMethodsImpl =
 'procedure %0s.Test1;'#13#10 +
 'begin'#13#10 +
 'end;'#13#10 +
 #13#10 +
 'procedure %0s.Test2(const AValue1 : Integer;const AValue2 : Integer);'#13#10 +
 'begin'#13#10 +
 'end;'#13#10;

 SDefaultClassName = 'TMyTestObject';


implementation

end.
