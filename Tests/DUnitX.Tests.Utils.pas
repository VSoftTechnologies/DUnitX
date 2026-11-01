unit DUnitX.Tests.Utils;

{$I DUnitX.inc}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.UITypes;

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
    originalFormatSettings : TFormatSettings;
  protected var
    expectedDate : TDate;
    expectedTime : TTime;
  protected
    procedure setFormatSettings(const locale : string);
    procedure revertFormatSettings();
  public
    // Delphi 2010 does support calling of constructor
    constructor Create;
    destructor Destroy; override;
    [Test]
    procedure ToString_Formats_A_Nil_Object_Or_Interface;
    [Test]
    procedure Unparseable_Float_Does_Not_Convert;
    [Test]
    [NamedTestCase('EN-US local format', 'EN-US,06/22/2020')]
    [NamedTestCase('EN-US iso8601 format', 'EN-US,2020-06-22')]
    [NamedTestCase('EN-GB local format', 'EN-GB,22/06/2020')]
    [NamedTestCase('EN-GB iso8601 format', 'EN-GB,2020-06-22')]
    [NamedTestCase('DE local format', 'DE,22.06.2020')]
    [NamedTestCase('DE local format, no leading zeroes', 'DE,22.6.2020')]
    [NamedTestCase('DE local format, no explicit century', 'DE,22.06.20')]
    [NamedTestCase('DE iso8601 format', 'DE,2020-06-22')]
    procedure TestDateConversion(const locale : string; const text : string);

    [Test]
    [NamedTestCase('EN-US local format, no leading zero, no seconds', 'EN-US,6:36 pm')]
    [NamedTestCase('EN-US local format, with seconds', 'EN-US,06:36:00 PM')]
    [NamedTestCase('DE local format, no seconds', 'DE,18:36')]
    [NamedTestCase('DE local format, with seconds', 'DE,18:36:00')]
    [NamedTestCase('DE iso8601 format', 'DE,18:36:00.000')]
    [NamedTestCase('EN-GB local 12 h format', 'EN-GB,06:36 pm')]
    [NamedTestCase('EN-GB local 24 h format', 'EN-GB,18:36')]
    procedure TestTimeConversion(const locale : string; const text : string);

    [Test]
    [NamedTestCase('EN-US local format, verbose', 'EN-US,06/22/2020 06:36:00 pm')]
    [NamedTestCase('EN-US local format short', 'EN-US,6/22/2020 6:36 pm')]
    [NamedTestCase('EN-US iso 8601 format', 'EN-US,2020-06-22 18:36:00')]
    [NamedTestCase('EN-US iso 8601 format, verbose', 'EN-US,2020-06-22T18:36:00.000Z')]
    [NamedTestCase('EN-GB local format, 24 h, verbose', 'EN-GB,22/06/2020 18:36:00')]
    [NamedTestCase('EN-GB local format, 12 h, verbose', 'EN-GB,22/06/2020 06:36:00 pm')]
    [NamedTestCase('EN-GB iso8601 format', 'EN-GB,2020-06-22 18:36')]
    [NamedTestCase('EN-GB iso8601 format, verbose', 'EN-GB,2020-06-22T18:36:00+00')]
    [NamedTestCase('DE local format, verbose', 'DE,22.06.2020 18:36:00.000')]
    [NamedTestCase('DE local format, short', 'DE,22.6.20 18:36')]
    [NamedTestCase('DE iso8601 format', 'DE,2020-06-22 18:36:00')]
    procedure TestDateTimeConversion(const locale : string; const text : string);

    [Test]
    [NamedTestCase('EN-US local format, verbose', 'EN-US,06/22/2020 06:36:00 pm')]
    [NamedTestCase('EN-US local format short', 'EN-US,6/22/2020 6:36 pm')]
    [NamedTestCase('EN-GB local format, 24 h, verbose', 'EN-GB,22/06/2020 18:36:00')]
    [NamedTestCase('EN-GB local format, 12 h, verbose', 'EN-GB,22/06/2020 06:36:00 pm')]
    [NamedTestCase('DE local format, verbose', 'DE,22.06.2020 18:36:00.000')]
    [NamedTestCase('DE local format, short', 'DE,22.6.20 18:36')]
    // ISO-8601 belongs on TestDateTimeConversion (TValue.TryConvert).
    // RTL StrToDateTime does not parse those strings (Delphi 13.1, EN-US/EN-GB/DE).
    // The previous {$IFDEF DELPHI_X8_UP} never compiled (DUnitX.inc defines DELPHI_XE8_UP).
    procedure TestDateTimeConversion2(const locale : string; const text : string);

    [Test]
    [NamedTestCase('EN-US 01/02/2020, both readings agree', 'EN-US,01/02/2020,2020,1,2')]
    [NamedTestCase('EN-GB 01/02/2020, the readings disagree', 'EN-GB,01/02/2020,0,0,0')]
    [NamedTestCase('EN-GB 22/06/2020, only the locale reads it', 'EN-GB,22/06/2020,2020,6,22')]
    [NamedTestCase('DE 01/02/2020, only the invariant format reads it', 'DE,01/02/2020,2020,1,2')]
    [NamedTestCase('DE 22.06.2020', 'DE,22.06.2020,2020,6,22')]
    [NamedTestCase('NL 22-06-20 is not ISO 8601', 'nl-NL,22-06-20,2020,6,22')]
    [NamedTestCase('NL iso8601 format', 'nl-NL,2020-06-22,2020,6,22')]
    [NamedTestCase('JA 22/06/2020, not in the locale order', 'ja-JP,22/06/2020,0,0,0')]
    procedure Date_Converts_Only_When_Its_Meaning_Is_Unambiguous(const locale : string; const text : string;
      const year, month, day : Word);

    [Test]
    [NamedTestCase('EN-US 01/02/2020 10:30, both readings agree', 'EN-US,01/02/2020 10:30,2020,1,2')]
    [NamedTestCase('EN-GB 01/02/2020 10:30, the readings disagree', 'EN-GB,01/02/2020 10:30,0,0,0')]
    [NamedTestCase('NL 22-06-20 10:30 is not ISO 8601', 'nl-NL,22-06-20 10:30,2020,6,22')]
    [NamedTestCase('NL iso8601 format, verbose', 'nl-NL,2020-06-22T10:30:00Z,2020,6,22')]
    [NamedTestCase('DE iso8601 format, verbose', 'DE,2020-06-22T10:30:00.000Z,2020,6,22')]
    [NamedTestCase('EN-GB iso8601, zone hh:mm', 'EN-GB,2020-06-22T10:30+02:00,2020,6,22')]
    [NamedTestCase('EN-GB iso8601, zone hhmm', 'EN-GB,2020-06-22T10:30:00-0500,2020,6,22')]
    [NamedTestCase('EN-GB iso8601, zone hh', 'EN-GB,2020-06-22 10:30:00.000-05,2020,6,22')]
    [NamedTestCase('JA 22/06/2020, the year is not an hour', 'ja-JP,22/06/2020,0,0,0')]
    [NamedTestCase('JA 22/06/2020 10:30, not in the locale order', 'ja-JP,22/06/2020 10:30,0,0,0')]
    [NamedTestCase('ZH 22/06/2020 10:30, not in the locale order', 'zh-CN,22/06/2020 10:30,0,0,0')]
    [NamedTestCase('SV 22-06-2020, the year is not an hour', 'sv-SE,22-06-2020,0,0,0')]
    [NamedTestCase('SV 1-2-2020, the year is not an hour', 'sv-SE,1-2-2020,0,0,0')]
    procedure DateTime_Converts_Only_When_Its_Meaning_Is_Unambiguous(const locale : string; const text : string;
      const year, month, day : Word);

    [Test]
    [NamedTestCase('FI 10.30, only the locale reads it', 'fi-FI,10.30,10,30')]
    [NamedTestCase('EN-GB 10:30 pm', 'EN-GB,10:30 pm,22,30')]
    [NamedTestCase('DE iso8601 format', 'DE,10:30:00.000,10,30')]
    procedure Time_Converts_In_Every_Locale(const locale : string; const text : string; const hour, minute : Word);

    [Test]
    [NamedTestCase('EN-US', 'EN-US')]
    [NamedTestCase('EN-GB', 'EN-GB')]
    [NamedTestCase('DE', 'DE')]
    [NamedTestCase('NL', 'nl-NL')]
    [NamedTestCase('JA', 'ja-JP')]
    [NamedTestCase('ZH', 'zh-CN')]
    [NamedTestCase('FI', 'fi-FI')]
    [NamedTestCase('SV', 'sv-SE')]
    [NamedTestCase('KO, the afternoon marker before the time', 'ko-KR')]
    procedure Date_And_Time_Formatted_In_The_Locale_Read_Back(const locale : string);

    [Test]
    procedure Unparseable_Date_Or_Time_Does_Not_Convert;

    [Test]
    [NamedTestCase('Text inside', '2020-06-22 foo 18:36')]
    [NamedTestCase('Text after the time', '2020-06-22 18:36 extra')]
    [NamedTestCase('Text after a space', '2020-06-22 xyz')]
    [NamedTestCase('Text after the date', '2020-06-22X')]
    [NamedTestCase('One-digit month', '2020-6-22')]
    [NamedTestCase('Hour only', '2020-06-22 18')]
    [NamedTestCase('No such day', '2020-02-30')]
    [NamedTestCase('No such hour', '2020-06-22T25:00')]
    [NamedTestCase('One-digit zone', '2020-06-22T18:36+2')]
    [NamedTestCase('No digit after the point', '2020-06-22T18:36:00.')]
    procedure Iso8601_Lookalike_Does_Not_Convert(const text : string);

    [Test]
    [NamedTestCase('One digit', '2020-06-22T18:36:00.5,500')]
    [NamedTestCase('Two digits', '2020-06-22T18:36:00.25,250')]
    [NamedTestCase('Three digits', '2020-06-22T18:36:00.123,123')]
    [NamedTestCase('Six digits', '2020-06-22 18:36:00.123456,123')]
    procedure Iso8601_Fraction_Is_A_Fraction_Of_A_Second(const text : string; const milliseconds : Word);

    [Test]
    [NamedTestCase('Point', '18:36:00.5;500', ';')]
    [NamedTestCase('Comma', '18:36:00,25;250', ';')]
    [NamedTestCase('Zone', '18:36:00.5Z;500', ';')]
    procedure Iso8601_Time_Fraction_Is_A_Fraction_Of_A_Second(const text : string; const milliseconds : Word);

    [Test]
    [NamedTestCase('Point', '18:36:00.5;500', ';')]
    [NamedTestCase('Zone', '18:36:00.25Z;250', ';')]
    procedure Iso8601_Time_As_DateTime_Fraction_Is_A_Fraction_Of_A_Second(const text : string; const milliseconds : Word);

    [Test]
    [NamedTestCase('No such hour', '25:00')]
    [NamedTestCase('No such minute', '18:60')]
    procedure Time_Out_Of_Range_Does_Not_Convert(const text : string);

    [Test]
    [NamedTestCase('claRed = xFFFF0000', 'claRed,xFFFF0000')]
    [NamedTestCase('Red = xFFFF0000', 'Red,xFFFF0000')]
    [NamedTestCase('red = xFFFF0000', 'red,xFFFF0000')]
    [NamedTestCase('$FFFF0000 = xFFFF0000', '$FFFF0000,xFFFF0000')]
    [NamedTestCase('xFFFF0000 = xFFFF0000', 'xFFFF0000,xFFFF0000')]
    procedure TestAlphaColorTestCase(const AColor : TAlphaColor; const AColorOrd : Cardinal);

    [Test]
    [NamedTestCase('clRed = 255', 'clRed,255')]
    [NamedTestCase('clred = 255', 'clred,255')]
    [NamedTestCase('clRed = $000000FF', 'clRed,$000000FF')]
    [NamedTestCase('clBlue = $00FF0000', 'clBlue,$00FF0000')]
    [NamedTestCase('$00FF0000 = $00FF0000', '$00FF0000,$00FF0000')]
    [NamedTestCase('$000000FF = $000000FF', '$000000FF,$000000FF')]
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

  System.DateUtils,
  System.Rtti,
  WinApi.Windows,
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

constructor TValueHelperTests.Create;

begin
  inherited;
  originalFormatSettings := System.SysUtils.FormatSettings;

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
  System.SysUtils.FormatSettings := originalFormatSettings;
end;

procedure TValueHelperTests.setFormatSettings(const locale : string);
{$IFNDEF DELPHI_XE3_UP}
var
  _lcid : LCID;
{$ENDIF}
begin
{$IFNDEF DELPHI_XE3_UP}
  _lcid := LocaleNameToLCID(PChar(locale), 0);
  GetLocaleFormatSettings(_lcid, FormatSettings);
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

procedure TValueHelperTests.Unparseable_Float_Does_Not_Convert;
var
  asTValue : TValue;
begin
  //These used to convert to 0
  Assert.IsFalse(TValue.From<string>('abc').TryConvert<Double>(asTValue), '''abc'' to Double');
  Assert.IsFalse(TValue.From<string>('1.5x').TryConvert<Double>(asTValue), '''1.5x'' to Double');

  Assert.IsTrue(TValue.From<string>('1.5').TryConvert<Double>(asTValue), '''1.5'' to Double');
  Assert.AreEqual(1.5, asTValue.AsExtended, 0, '''1.5'' to Double');
  Assert.IsTrue(TValue.From<string>('-2.5E3').TryConvert<Single>(asTValue), '''-2.5E3'' to Single');
  Assert.AreEqual(-2500, asTValue.AsExtended, 0, '''-2.5E3'' to Single');
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

//A year of 0 means the value must not convert: the invariant format and the locale read it as different dates
procedure TValueHelperTests.Date_Converts_Only_When_Its_Meaning_Is_Unambiguous(const locale, text : string;
  const year, month, day : Word);
var
  asTValue : TValue;
  converted : Boolean;
begin
  setFormatSettings(locale);
  try
    converted := TValue.From(text).TryConvert<TDate>(asTValue);
  finally
    revertFormatSettings();
  end;
  if year = 0 then
    Assert.IsFalse(converted, 'An ambiguous or misread date must not convert')
  else
  begin
    Assert.IsTrue(converted, 'TryConvert<TDate>');
    Assert.IsSameDate(EncodeDate(year, month, day), asTValue.AsType<TDate>(), 'TDate');
  end;
end;

//A year of 0 means the value must not convert; the time of every other case is 10:30
procedure TValueHelperTests.DateTime_Converts_Only_When_Its_Meaning_Is_Unambiguous(const locale, text : string;
  const year, month, day : Word);
var
  asTValue : TValue;
  converted : Boolean;
begin
  setFormatSettings(locale);
  try
    converted := TValue.From(text).TryConvert<TDateTime>(asTValue);
  finally
    revertFormatSettings();
  end;
  if year = 0 then
    Assert.IsFalse(converted, 'An ambiguous or misread date must not convert')
  else
  begin
    Assert.IsTrue(converted, 'TryConvert<TDateTime>');
    Assert.IsSameDateTime(EncodeDateTime(year, month, day, 10, 30, 0, 0), asTValue.AsType<TDateTime>(), 'TDateTime');
  end;
end;

procedure TValueHelperTests.Time_Converts_In_Every_Locale(const locale, text : string; const hour, minute : Word);
var
  asTValue : TValue;
  converted : Boolean;
begin
  setFormatSettings(locale);
  try
    converted := TValue.From(text).TryConvert<TTime>(asTValue);
  finally
    revertFormatSettings();
  end;
  Assert.IsTrue(converted, 'TryConvert<TTime>');
  Assert.IsSameTime(EncodeTime(hour, minute, 0, 0), asTValue.AsType<TTime>(), 'TTime');
end;

//What the locale itself writes has to read back, as the same value
procedure TValueHelperTests.Date_And_Time_Formatted_In_The_Locale_Read_Back(const locale : string);
var
  expected : TDateTime;
  asTValue : TValue;
begin
  expected := EncodeDateTime(2020, 6, 22, 18, 36, 0, 0);
  setFormatSettings(locale);
  try
    Assert.IsTrue(TValue.From(DateTimeToStr(expected)).TryConvert<TDateTime>(asTValue), DateTimeToStr(expected));
    Assert.IsSameDateTime(expected, asTValue.AsType<TDateTime>(), DateTimeToStr(expected));
    Assert.IsTrue(TValue.From(DateToStr(expected)).TryConvert<TDate>(asTValue), DateToStr(expected));
    Assert.IsSameDate(expected, asTValue.AsType<TDate>(), DateToStr(expected));
    Assert.IsTrue(TValue.From(TimeToStr(expected)).TryConvert<TTime>(asTValue), TimeToStr(expected));
    Assert.IsSameTime(expected, asTValue.AsType<TTime>(), TimeToStr(expected));
  finally
    revertFormatSettings();
  end;
end;

procedure TValueHelperTests.Unparseable_Date_Or_Time_Does_Not_Convert;
var
  asTValue : TValue;
begin
  //These used to convert to 0 (30/12/1899), so the test case ran with a date nobody wrote
  Assert.IsFalse(TValue.From<string>('notadate').TryConvert<TDate>(asTValue), 'TDate');
  Assert.IsFalse(TValue.From<string>('notadate').TryConvert<TDateTime>(asTValue), 'TDateTime');
  Assert.IsFalse(TValue.From<string>('notatime').TryConvert<TTime>(asTValue), 'TTime');
end;

//Each starts with a four-digit year and '-' but is not ISO 8601, and no UK format reads it either: it must not convert,
//rather than be read in part
procedure TValueHelperTests.Iso8601_Lookalike_Does_Not_Convert(const text : string);
var
  asTValue : TValue;
  dateConverted : Boolean;
  dateTimeConverted : Boolean;
begin
  setFormatSettings('EN-GB');
  try
    dateConverted := TValue.From(text).TryConvert<TDate>(asTValue);
    dateTimeConverted := TValue.From(text).TryConvert<TDateTime>(asTValue);
  finally
    revertFormatSettings();
  end;
  Assert.IsFalse(dateConverted, 'TDate');
  Assert.IsFalse(dateTimeConverted, 'TDateTime');
end;

//The digits after the point are a decimal fraction of a second: '.5' is half a second, not 5 milliseconds
procedure TValueHelperTests.Iso8601_Fraction_Is_A_Fraction_Of_A_Second(const text : string; const milliseconds : Word);
var
  asTValue : TValue;
begin
  Assert.IsTrue(TValue.From(text).TryConvert<TDateTime>(asTValue), 'TryConvert<TDateTime>');
  Assert.AreEqual(Integer(milliseconds), Integer(MilliSecondOf(asTValue.AsType<TDateTime>())), 'Milliseconds');
  Assert.IsSameDateTime(EncodeDateTime(2020, 6, 22, 18, 36, 0, milliseconds), asTValue.AsType<TDateTime>(), 'TDateTime');
end;

//The same for a time on its own
procedure TValueHelperTests.Iso8601_Time_Fraction_Is_A_Fraction_Of_A_Second(const text : string;
  const milliseconds : Word);
var
  asTValue : TValue;
  converted : Boolean;
begin
  setFormatSettings('EN-GB');
  try
    converted := TValue.From(text).TryConvert<TTime>(asTValue);
  finally
    revertFormatSettings();
  end;
  Assert.IsTrue(converted, 'TryConvert<TTime>');
  Assert.AreEqual(Integer(milliseconds), Integer(MilliSecondOf(asTValue.AsType<TTime>())), 'Milliseconds');
  Assert.IsSameTime(EncodeTime(18, 36, 0, milliseconds), asTValue.AsType<TTime>(), 'TTime');
end;

//The same for a time on its own read as a date and time
procedure TValueHelperTests.Iso8601_Time_As_DateTime_Fraction_Is_A_Fraction_Of_A_Second(const text : string;
  const milliseconds : Word);
var
  asTValue : TValue;
  converted : Boolean;
begin
  setFormatSettings('EN-GB');
  try
    converted := TValue.From(text).TryConvert<TDateTime>(asTValue);
  finally
    revertFormatSettings();
  end;
  Assert.IsTrue(converted, 'TryConvert<TDateTime>');
  Assert.AreEqual(Integer(milliseconds), Integer(MilliSecondOf(asTValue.AsType<TDateTime>())), 'Milliseconds');
  Assert.IsSameDateTime(EncodeTime(18, 36, 0, milliseconds), asTValue.AsType<TDateTime>(), 'TDateTime');
end;

//Used to convert to midnight
procedure TValueHelperTests.Time_Out_Of_Range_Does_Not_Convert(const text : string);
var
  asTValue : TValue;
  converted : Boolean;
begin
  setFormatSettings('EN-GB');
  try
    converted := TValue.From(text).TryConvert<TTime>(asTValue);
  finally
    revertFormatSettings();
  end;
  Assert.IsFalse(converted, 'TTime');
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

