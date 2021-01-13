unit DUnitX.Tests.Utils;
{$I ..\Source\DUnitX.inc}

interface

uses
  DUnitX.TestFramework,
  {$IFDEF USE_NS}
  System.SysUtils;
  {$ELSE}
  SysUtils;
  {$ENDIF}

type
  [TestFixture('TValueHelper')]
  [Category('Utils')]
  TValueHelperTests = class
  private var
  	originalFormatSettings: TFormatSettings;
  protected var
	expectedDate: TDate;
	expectedTime: TTime;
  protected
	procedure setFormatSettings(const locale: String);
	procedure revertFormatSettings();
  public
	constructor Create();
	destructor Destroy(); override;
	[Test]
	[TestCase('EN-US local format',                   'EN-US,06/22/2020')]
	[TestCase('EN-US iso8601 format',                 'EN-US,2020-06-22')]
	[TestCase('EN-GB local format',                   'EN-GB,22/06/2020')]
	[TestCase('EN-GB iso8601 format',                 'EN-GB,2020-06-22')]
	[TestCase('DE local format',                      'DE,22.06.2020')]
	[TestCase('DE local format, no leading zeroes',   'DE,22.6.2020')]
	[TestCase('DE local format, no explicit century', 'DE,22.06.20')]
	[TestCase('DE iso8601 format',                    'DE,2020-06-22')]
	procedure TestDateConversion(const locale: String; const text: String);

	[Test]
	[TestCase('EN-US local format, no leading zero, no seconds',   'EN-US,6:36 pm')]
	[TestCase('EN-US local format, with seconds',                  'EN-US,06:36:00 PM')]
	[TestCase('DE local format, no seconds',                       'DE,18:36')]
	[TestCase('DE local format, with seconds',                     'DE,18:36:00')]
	[TestCase('DE iso8601 format',                                 'DE,18:36:00.000')]
	[TestCase('EN-GB local 12 h format',                           'EN-GB,06:36 pm')]
	[TestCase('EN-GB local 24 h format',                           'EN-GB,18:36')]
	procedure TestTimeConversion(const locale: String; const text: String);

    [Test]
	[TestCase('EN-US local format, verbose',        'EN-US,06/22/2020 06:36:00 pm')]
	[TestCase('EN-US local format short',           'EN-US,6/22/2020 6:36 pm')]
	[TestCase('EN-US iso 8601 format',              'EN-US,2020-06-22 18:36:00')]
	[TestCase('EN-US iso 8601 format, verbose',     'EN-US,2020-06-22T18:36:00.000Z')]
	[TestCase('EN-GB local format, 24 h, verbose',  'EN-GB,22/06/2020 18:36:00')]
	[TestCase('EN-GB local format, 12 h, verbose',  'EN-GB,22/06/2020 06:36:00 pm')]
	[TestCase('EN-GB iso8601 format',               'EN-GB,2020-06-22 18:36')]
	[TestCase('EN-GB iso8601 format, verbose',      'EN-GB,2020-06-22T18:36:00+00')]
	[TestCase('DE local format, verbose',           'DE,22.06.2020 18:36:00.000')]
	[TestCase('DE local format, short',             'DE,22.6.20 18:36')]
	[TestCase('DE iso8601 format',                  'DE,2020-06-22 18:36:00')]
	procedure TestDateTimeConversion(const locale: String; const text: String);
  end;

implementation uses
  {$IFDEF USE_NS}
  System.DateUtils,
  System.Rtti,
  WinApi.Windows,
  DUnitX.Utils;
  {$ELSE}
  DateUtils,
  Rtti,
  Windows,
  Utils;
  {$ENDIF}

{ TMyExampleTests }

constructor TValueHelperTests.Create();
begin
	inherited;
	originalFormatSettings := System.SysUtils.FormatSettings;
	expectedDate := EncodeDate(2020, 06, 22);
	expectedTime := EncodeTime(18, 36, 00, 000);
end;

destructor TValueHelperTests.Destroy();
begin
  revertFormatSettings();
  inherited;
end;

procedure TValueHelperTests.revertFormatSettings();
begin
	System.SysUtils.FormatSettings := originalFormatSettings;
end;

procedure TValueHelperTests.setFormatSettings(const locale: String);
{$If Defined(XE2_DOWN)}
var
	_lcid: LCID;
{$IFEND}
begin
	{$If Defined(XE2_DOWN)}
	_lcid := LocaleNameToLCID(PChar(locale), 0);
	GetLocaleFormatSettings(_lcid, FormatSettings);
	{$Else}
	FormatSettings := TFormatSettings.Create(locale);
	{$IFEND}
end;

procedure TValueHelperTests.TestDateConversion(const locale, text: String);
var
	asTValue: TValue;
	actual: TDate;
begin
	setFormatSettings(locale);
	Assert.IsTrue( TValue.From(text).TryConvert<TDate>(asTValue), 'TryConvert<TDate>' );
	Assert.IsTrue( asTValue.IsType<TDate>(), 'IsType TDate' );

	actual := asTValue.AsType<TDate>();
	Assert.IsTrue( SameDate(expectedDate, actual), 'SameDate(..)' );
end;

procedure TValueHelperTests.TestDateTimeConversion(const locale, text: String);
var
	asTValue: TValue;
	expected, actual: TDateTime;
begin
	setFormatSettings(locale);
	Assert.IsTrue( TValue.From(text).TryConvert<TDateTime>(asTValue), 'TryConvert<TDateTime>' );
	Assert.IsTrue( asTvalue.IsType<TDateTime>(), 'IsType TDateTime' );

	expected := (expectedDate + expectedTime);
	actual := asTValue.AsType<TDateTime>();
	Assert.IsTrue( SameDateTime(expected, actual), 'SameDateTime(..)' );
end;

procedure TValueHelperTests.TestTimeConversion(const locale, text: String);
var
	asTValue: TValue;
	actual: TTime;
begin
	setFormatSettings(locale);
	Assert.IsTrue( TValue.From(text).TryConvert<TTime>(asTValue), 'TryConvert<TTime>' );
	Assert.IsTrue( asTvalue.IsType<TTime>(), 'IsType TTime' );

	actual := asTValue.AsType<TTime>();
	Assert.IsTrue( SameTime(expectedTime, actual), 'SameTime(..)' );
end;

end.
