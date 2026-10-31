unit DUnitX.Tests.Utils;

{$I DUnitX.inc}

interface

uses
  DUnitX.TestFramework,
{$IFDEF USE_NS}
  System.SysUtils,
  System.UITypes;
{$ELSE}
  SysUtils,
  UITypes;
{$ENDIF}

type
  [TestFixture]
  [Category('Utils')]
  TArrayHelperTests = class
  public
    [Test]
    procedure Delete_Removes_The_Given_Elements;
  end;

  //A signed type that does not cover its whole width
  TSignedSubrange = -10..10;

  [TestFixture('TValueHelper')]
  [Category('Utils')]
  TValueHelperTests = class
  private var
{$IFDEF DELPHI_XE_UP}
    originalFormatSettings : TFormatSettings;
{$ELSE}
    originalLCID : Integer;
{$ENDIF}
  protected var
    expectedDate : TDate;
    expectedTime : TTime;
  protected
    procedure setFormatSettings(const locale : string);
    procedure revertFormatSettings();
{$IFDEF DELPHI_2010}
    function GetLCIDFromLocale(const locale : string) : integer;
{$ENDIF}
  public
    // Delphi 2010 does support calling of constructor
{$IFDEF DELPHI_XE_UP}
    constructor Create;
{$ELSE}
    procedure AfterConstruction; override;
{$ENDIF}
    destructor Destroy; override;
    [Test]
    procedure ToString_Formats_A_Nil_Object_Or_Interface;
    [Test]
    [TestCase('EN-US local format', 'EN-US,06/22/2020')]
    [TestCase('EN-US iso8601 format', 'EN-US,2020-06-22')]
    [TestCase('EN-GB local format', 'EN-GB,22/06/2020')]
    [TestCase('EN-GB iso8601 format', 'EN-GB,2020-06-22')]
    [TestCase('DE local format', 'DE,22.06.2020')]
    [TestCase('DE local format, no leading zeroes', 'DE,22.6.2020')]
    [TestCase('DE local format, no explicit century', 'DE,22.06.20')]
    [TestCase('DE iso8601 format', 'DE,2020-06-22')]
    procedure TestDateConversion(const locale : string; const text : string);

    [Test]
    [TestCase('EN-US local format, no leading zero, no seconds', 'EN-US,6:36 pm')]
    [TestCase('EN-US local format, with seconds', 'EN-US,06:36:00 PM')]
    [TestCase('DE local format, no seconds', 'DE,18:36')]
    [TestCase('DE local format, with seconds', 'DE,18:36:00')]
    [TestCase('DE iso8601 format', 'DE,18:36:00.000')]
    [TestCase('EN-GB local 12 h format', 'EN-GB,06:36 pm')]
    [TestCase('EN-GB local 24 h format', 'EN-GB,18:36')]
    procedure TestTimeConversion(const locale : string; const text : string);

    [Test]
    [TestCase('EN-US local format, verbose', 'EN-US,06/22/2020 06:36:00 pm')]
    [TestCase('EN-US local format short', 'EN-US,6/22/2020 6:36 pm')]
    [TestCase('EN-US iso 8601 format', 'EN-US,2020-06-22 18:36:00')]
    [TestCase('EN-US iso 8601 format, verbose', 'EN-US,2020-06-22T18:36:00.000Z')]
    [TestCase('EN-GB local format, 24 h, verbose', 'EN-GB,22/06/2020 18:36:00')]
    [TestCase('EN-GB local format, 12 h, verbose', 'EN-GB,22/06/2020 06:36:00 pm')]
    [TestCase('EN-GB iso8601 format', 'EN-GB,2020-06-22 18:36')]
    [TestCase('EN-GB iso8601 format, verbose', 'EN-GB,2020-06-22T18:36:00+00')]
    [TestCase('DE local format, verbose', 'DE,22.06.2020 18:36:00.000')]
    [TestCase('DE local format, short', 'DE,22.6.20 18:36')]
    [TestCase('DE iso8601 format', 'DE,2020-06-22 18:36:00')]
    procedure TestDateTimeConversion(const locale : string; const text : string);

    [Test]
    [TestCase('EN-US local format, verbose', 'EN-US,06/22/2020 06:36:00 pm')]
    [TestCase('EN-US local format short', 'EN-US,6/22/2020 6:36 pm')]
    [TestCase('EN-GB local format, 24 h, verbose', 'EN-GB,22/06/2020 18:36:00')]
    [TestCase('EN-GB local format, 12 h, verbose', 'EN-GB,22/06/2020 06:36:00 pm')]
    [TestCase('DE local format, verbose', 'DE,22.06.2020 18:36:00.000')]
    [TestCase('DE local format, short', 'DE,22.6.20 18:36')]
    // ISO-8601 belongs on TestDateTimeConversion (TValue.TryConvert).
    // RTL StrToDateTime does not parse those strings (Delphi 13.1, EN-US/EN-GB/DE).
    // The previous {$IFDEF DELPHI_X8_UP} never compiled (DUnitX.inc defines DELPHI_XE8_UP).
    procedure TestDateTimeConversion2(const locale : string; const text : string);

    [Test]
    [TestCase('claRed = xFFFF0000', 'claRed,xFFFF0000')]
    [TestCase('Red = xFFFF0000', 'Red,xFFFF0000')]
    [TestCase('red = xFFFF0000', 'red,xFFFF0000')]
    [TestCase('$FFFF0000 = xFFFF0000', '$FFFF0000,xFFFF0000')]
    [TestCase('xFFFF0000 = xFFFF0000', 'xFFFF0000,xFFFF0000')]
    procedure TestAlphaColorTestCase(const AColor : TAlphaColor; const AColorOrd : Cardinal);

    [Test]
    [TestCase('clRed = 255', 'clRed,255')]
    [TestCase('clred = 255', 'clred,255')]
    [TestCase('clRed = $000000FF', 'clRed,$000000FF')]
    [TestCase('clBlue = $00FF0000', 'clBlue,$00FF0000')]
    [TestCase('$00FF0000 = $00FF0000', '$00FF0000,$00FF0000')]
    [TestCase('$000000FF = $000000FF', '$000000FF,$000000FF')]
    procedure TestColorTestCase(const AColor : TColor; const AColorOrd : Integer);

    [Test]
    procedure Unparseable_Or_Out_Of_Range_Integer_Does_Not_Convert;

    [Test]
    procedure Hex_Integer_Is_Read_As_A_Bit_Pattern_Of_Its_Type;
  end;

  [TestFixture]
  [Category('Utils')]
  TDUnitXListTests = class
  public
    [Test]
    procedure ToArray_Returns_The_Items;
    [Test]
    procedure NonGeneric_Enumerator_Yields_The_Objects;
  end;



implementation

uses

{$IFDEF USE_NS}
  System.DateUtils,
  System.Rtti,
  WinApi.Windows,
{$ELSE}
  DateUtils,
  Rtti,
  Windows,
{$ENDIF}
  DUnitX.Generics,
  DUnitX.Utils;

{ TArrayHelperTests }

procedure TArrayHelperTests.Delete_Removes_The_Given_Elements;
var
  values : TArray<Integer>;
begin
  SetLength(values, 5);
  values[0] := 1;
  values[1] := 2;
  values[2] := 3;
  values[3] := 4;
  values[4] := 5;

  TArrayHelper.Delete<Integer>(values, 0, 1);
  Assert.AreEqual(4, Length(values), 'Length after deleting the first element');
  Assert.AreEqual(2, values[0], 'First element after deleting the first element');

  TArrayHelper.Delete<Integer>(values, 1, 2);
  Assert.AreEqual(2, Length(values), 'Length after deleting from the middle');
  Assert.AreEqual(2, values[0]);
  Assert.AreEqual(5, values[1]);

  TArrayHelper.Delete<Integer>(values, 1, 10);
  Assert.AreEqual(1, Length(values), 'Length after deleting more than there is');
  Assert.AreEqual(2, values[0]);
end;

{ TValueHelperTests }

{$IFDEF DELPHI_XE_UP}
constructor TValueHelperTests.Create;

{$ELSE}
procedure TValueHelperTests.AfterConstruction;
{$ENDIF}
begin
  inherited;
{$IFDEF DELPHI_XE_UP}
  originalFormatSettings := {$IFDEF USE_NS}System.{$ENDIF}SysUtils.FormatSettings;
{$ELSE}
  originalLCID := GetThreadLocale;
{$ENDIF}

  expectedDate := EncodeDate(2020, 06, 22);
  expectedTime := EncodeTime(18, 36, 00, 000);
end;

destructor TValueHelperTests.Destroy;
begin
  revertFormatSettings();
  inherited;
end;

procedure TValueHelperTests.revertFormatSettings();
begin
{$IFDEF DELPHI_XE_UP}
{$IFDEF USE_NS}System.{$ENDIF}SysUtils.FormatSettings := originalFormatSettings;
{$ELSE}
  SetThreadLocale(originalLCID);
  GetFormatSettings;
{$ENDIF}
end;

{$IFDEF DELPHI_2010}
// I could not find a better way than hardcode them for D2010..
// Full list: https://docs.microsoft.com/en-us/openspecs/office_standards/ms-oe376/6c085406-a698-4e12-9d4d-c3b0ee3dbc4a
function TValueHelperTests.GetLCIDFromLocale(const locale : string) : integer;
begin
  if locale = 'EN-US' then
    result := 1033
  else if locale = 'EN-GB' then
    result := 2057
  else if locale = 'DE' then
    result := 1031
  else
    raise Exception.Create('Unsupported locale passed to GetLCIDFromLocale function');
end;
{$ENDIF}

procedure TValueHelperTests.setFormatSettings(const locale : string);
{$IFNDEF DELPHI_XE3_UP}
var
  _lcid : LCID;
{$ENDIF}
begin
{$IFNDEF DELPHI_XE3_UP}
{$IFDEF DELPHI_2010}
  _lcid := GetLCIDFromLocale(locale);
  SetThreadLocale(_lcid);
  GetFormatSettings;
{$ELSE}
  _lcid := LocaleNameToLCID(PChar(locale), 0);
  GetLocaleFormatSettings(_lcid, FormatSettings);
{$ENDIF}
{$ELSE}
  FormatSettings := TFormatSettings.Create(locale);
{$ENDIF}
end;

procedure TValueHelperTests.ToString_Formats_A_Nil_Object_Or_Interface;
var
  objectText : string;
  interfaceText : string;
begin
  Assert.WillNotRaiseAny(
    procedure
    begin
      objectText := TValue.ToString(TValue.From<TObject>(nil));
    end, 'object');
  Assert.AreEqual('nil', objectText, 'object');
  Assert.WillNotRaiseAny(
    procedure
    begin
      interfaceText := TValue.ToString(TValue.From<IInterface>(nil));
    end, 'interface');
  Assert.AreEqual('nil', interfaceText, 'interface');
end;

procedure TValueHelperTests.TestAlphaColorTestCase(const AColor : TAlphaColor;
  const AColorOrd : Cardinal);
begin
  Assert.AreEqual(AColorOrd, AColor);
end;

procedure TValueHelperTests.TestColorTestCase(const AColor : TColor;
  const AColorOrd : Integer);
begin
  Assert.AreEqual(AColorOrd, AColor);
end;

procedure TValueHelperTests.Unparseable_Or_Out_Of_Range_Integer_Does_Not_Convert;
var
  asTValue : TValue;
begin
  //'abc' used to convert to 0, and '300' to the Byte 44
  Assert.IsFalse(TValue.From<string>('abc').TryConvert<Integer>(asTValue), '''abc'' to Integer');
  Assert.IsFalse(TValue.From<string>('300').TryConvert<Byte>(asTValue), '''300'' to Byte');
  Assert.IsFalse(TValue.From<string>('-1').TryConvert<Cardinal>(asTValue), '''-1'' to Cardinal');
  Assert.IsFalse(TValue.From<string>('2147483648').TryConvert<Integer>(asTValue), '''2147483648'' to Integer');
  Assert.IsFalse(TValue.From<string>('$100').TryConvert<Byte>(asTValue), '''$100'' to Byte');
  Assert.IsFalse(TValue.From<string>('$100').TryConvert<ShortInt>(asTValue), '''$100'' to ShortInt');
  Assert.IsFalse(TValue.From<string>('11').TryConvert<TSignedSubrange>(asTValue), '''11'' to -10..10');
  Assert.IsFalse(TValue.From<string>('$FF').TryConvert<TSignedSubrange>(asTValue), '''$FF'' to -10..10');
  Assert.IsFalse(TValue.From<string>('notacolour').TryConvert<TColor>(asTValue), '''notacolour'' to TColor');

  Assert.IsTrue(TValue.From<string>('255').TryConvert<Byte>(asTValue), '''255'' to Byte');
  Assert.AreEqual(255, asTValue.AsInteger, '''255'' to Byte');
  Assert.IsTrue(TValue.From<string>('4294967295').TryConvert<Cardinal>(asTValue), '''4294967295'' to Cardinal');
  Assert.AreEqual(Int64(4294967295), asTValue.AsInt64, '''4294967295'' to Cardinal');
end;

procedure TValueHelperTests.Hex_Integer_Is_Read_As_A_Bit_Pattern_Of_Its_Type;
var
  asTValue : TValue;
begin
  //As in code: a hex value that fits the type's width is its bit pattern
  Assert.IsTrue(TValue.From<string>('$FFFFFFFF').TryConvert<Integer>(asTValue), '''$FFFFFFFF'' to Integer');
  Assert.AreEqual(-1, asTValue.AsInteger, '''$FFFFFFFF'' to Integer');
  Assert.IsTrue(TValue.From<string>('$FF').TryConvert<ShortInt>(asTValue), '''$FF'' to ShortInt');
  Assert.AreEqual(-1, asTValue.AsInteger, '''$FF'' to ShortInt');
  Assert.IsTrue(TValue.From<string>('0xFFFF').TryConvert<SmallInt>(asTValue), '''0xFFFF'' to SmallInt');
  Assert.AreEqual(-1, asTValue.AsInteger, '''0xFFFF'' to SmallInt');
  Assert.IsTrue(TValue.From<string>('x7F').TryConvert<ShortInt>(asTValue), '''x7F'' to ShortInt');
  Assert.AreEqual(127, asTValue.AsInteger, '''x7F'' to ShortInt');
  Assert.IsTrue(TValue.From<string>('$FF').TryConvert<Byte>(asTValue), '''$FF'' to Byte');
  Assert.AreEqual(255, asTValue.AsInteger, '''$FF'' to Byte');
  Assert.IsTrue(TValue.From<string>('$FFFFFFFF').TryConvert<Cardinal>(asTValue), '''$FFFFFFFF'' to Cardinal');
  Assert.AreEqual(Int64(4294967295), asTValue.AsInt64, '''$FFFFFFFF'' to Cardinal');
  Assert.IsTrue(TValue.From<string>(' $FFFFFFFF').TryConvert<Integer>(asTValue), ''' $FFFFFFFF'' to Integer');
  Assert.AreEqual(-1, asTValue.AsInteger, ''' $FFFFFFFF'' to Integer');
  Assert.IsTrue(TValue.From<string>('+$FFFFFFFF').TryConvert<Integer>(asTValue), '''+$FFFFFFFF'' to Integer');
  Assert.AreEqual(-1, asTValue.AsInteger, '''+$FFFFFFFF'' to Integer');
  //A subrange does not cover its type's width, so its hex values are only range-checked
  Assert.IsTrue(TValue.From<string>('$A').TryConvert<TSignedSubrange>(asTValue), '''$A'' to -10..10');
  Assert.AreEqual(10, asTValue.AsInteger, '''$A'' to -10..10');
end;

procedure TValueHelperTests.TestDateConversion(const locale, text : string);
var
  asTValue : TValue;
  actual : TDate;
begin
  setFormatSettings(locale);
  Assert.IsTrue( TValue.From(text).TryConvert<TDate>(asTValue), 'TryConvert<TDate>' );
  Assert.IsTrue( asTValue.IsType<TDate>(), 'IsType TDate' );

  actual := asTValue.AsType<TDate>();
  Assert.IsSameDate(expectedDate, actual, 'SameDate(..)');
end;

procedure TValueHelperTests.TestDateTimeConversion(const locale, text : string);
var
  asTValue : TValue;
  expected, actual : TDateTime;
begin
  setFormatSettings(locale);
  Assert.IsTrue(TValue.From(text).TryConvert<TDateTime>(asTValue), 'TryConvert<TDateTime>');
  Assert.IsTrue(asTvalue.IsType<TDateTime>(), 'IsType TDateTime');

  expected := (expectedDate + expectedTime);
  actual := asTValue.AsType<TDateTime>();
  Assert.IsSameDateTime(expected, actual, 'SameDateTime(..)');
end;

procedure TValueHelperTests.TestDateTimeConversion2(const locale, text : string);
var
  expected, actual : TDateTime;
begin
  setFormatSettings(locale);

  expected := (expectedDate + expectedTime);
  actual := StrToDateTime(text);
  Assert.IsSameDateTime(expected, actual, 'SameDateTime(..)');
end;

procedure TValueHelperTests.TestTimeConversion(const locale, text : string);
var
  asTValue : TValue;
  actual : TTime;
begin
  setFormatSettings(locale);
  Assert.IsTrue(TValue.From(text).TryConvert<TTime>(asTValue), 'TryConvert<TTime>');
  Assert.IsTrue(asTvalue.IsType<TTime>(), 'IsType TTime');

  actual := asTValue.AsType<TTime>();
  Assert.IsSameTime(expectedTime, actual, 'SameTime(..)');
end;

{ TDUnitXListTests }

procedure TDUnitXListTests.ToArray_Returns_The_Items;
var
  list : IList<Integer>;
  items : TArray<Integer>;
begin
  list := TDUnitXList<Integer>.Create;
  list.Add(3);
  list.Add(1);
  list.Add(2);
  items := list.ToArray;
  Assert.AreEqual(3, Length(items), 'Length');
  Assert.AreEqual(3, items[0]);
  Assert.AreEqual(1, items[1]);
  Assert.AreEqual(2, items[2]);
end;

procedure TDUnitXListTests.NonGeneric_Enumerator_Yields_The_Objects;
var
  list : IList<TObject>;
  first, second : TObject;
  enumerable : IEnumerable;
  enumerator : IEnumerator;
begin
  first := TObject.Create;
  second := TObject.Create;
  try
    list := TDUnitXList<TObject>.Create;
    list.Add(first);
    list.Add(second);
    enumerable := list;
    enumerator := enumerable.GetEnumerator;
    Assert.IsNotNull(enumerator, 'GetEnumerator');
    Assert.IsTrue(enumerator.MoveNext, 'MoveNext 1');
    Assert.AreSame(first, enumerator.Current, 'Current 1');
    Assert.IsTrue(enumerator.MoveNext, 'MoveNext 2');
    Assert.AreSame(second, enumerator.Current, 'Current 2');
    Assert.IsFalse(enumerator.MoveNext, 'MoveNext 3');
  finally
    second.Free;
    first.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TArrayHelperTests);
  TDUnitX.RegisterTestFixture(TValueHelperTests);
  TDUnitX.RegisterTestFixture(TDUnitXListTests);

end.

