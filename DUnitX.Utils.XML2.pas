unit DUnitX.Utils.XML2;
interface
uses XMLIntf, Classes, FMTBcd, Generics.Defaults, IniFiles;

type
  TMatchResult = (mNoMatch, mMatch);
  TMatchResultSet = set of TMatchResult;
  TPatternFunc = reference to function( const Haystack: string; var Position: integer): integer;
  // returns length of next match.
  RMatchFragment = record
    FResult: TMatchResult;
    FFragment: string;
  end;

{$REGION 'summary'}
type
/// <summary>
///   Helper functions for  IXMLDocument
/// </summary>
{$ENDREGION}
TXMLDoc = class
  public
    class function  NewEmtpy: IXMLDocument;
    class function  FromString( const psXMLStr: string): IXMLDocument;
    class function  FromUTF8String( const psXMLStr: utf8string): IXMLDocument;
    class function  FromFile( const psXMLFN: string): IXMLDocument;
    class function  FromStream( const Stream: TStream; EncodingType: TXMLEncodingType = xetUnknown): IXMLDocument;
    class procedure DeclareSelectionNamespaces( const Doc: IXMLDocument; const Namespaces: string {space separated list of namespace declarations});
  end;


{$REGION 'summary'}
type
/// <summary>
///   Helper functions for XML data type xs:boolean
/// </summary>
{$ENDREGION}
Txs_boolean = class
  public
    class function  Encode( pbValue: boolean): string;
    class function  Decode( const psEncodedValue: string; Default: boolean): boolean;
  end;

{$REGION 'summary'}
type
/// <summary>
///  Helper functions for XML data type xs:float and xs:double
/// </summary>
{$ENDREGION}
Txs_float = class
  public
    class function Encode( pnValue: double): string;        overload;
    class function Encode( pnValue: single): string;        overload;
    class function Decode( const psEncodedValue: string; var pnDecoded: double): boolean;   overload;
    class function Decode( const psEncodedValue: string; var pnDecoded: single): boolean;   overload;
  end;

{$REGION 'summary'}
type
/// <summary>
///  Helper functions for XML data type xs:decimal
/// </summary>
{$ENDREGION}
Txs_decimal = class
  public
    class function Encode( const pnValue: TBcd): string;
    class function Decode( psEncodedValue: string; var pnDecoded: TBcd): boolean;
  end;

{$REGION 'summary'}
type
/// <summary>
///  Helper functions for XML data type xs:integer
/// </summary>
{$ENDREGION}
Txs_integer = class
  public
    class function Encode( pnValue: int64): string;         overload;
    class function Encode( pnValue: uint64): string;        overload;
    class function Decode( const psEncodedValue: string; var pnDecoded: int64): boolean;    overload;
    class function Decode( const psEncodedValue: string; var pnDecoded: uint64): boolean;   overload;
  end;

{$REGION 'summary'}
type
/// <summary>
///  Helper functions for XML data type xs:duration
/// </summary>
{$ENDREGION}
Txs_duration = record
    bPositive: boolean;
    nYears   : cardinal;
    nMonths  : cardinal;
    nDays    : cardinal;
    nHours   : cardinal;
    nMinutes : cardinal;
    nSeconds : double;    // Maximum number of digits, when encoded is 18.

    procedure Zeroise;
    procedure Negate;
    procedure Normalise;
    function  isNormalForm: Boolean;
    function  Encode: string;
    function  Decode( const psEncodedValue: string): boolean;
    class function GetEqualityComparer: IEqualityComparer<Txs_duration>;  static;
    function  Equals( const Right: Txs_duration): boolean;
    function  GetHashCode: integer;
    function  isZero: boolean;
    procedure DiffDateTime( a, b: TDateTime);
    class operator Add( const a, b: Txs_duration): Txs_duration;
    class operator Add( const a: Txs_duration; b: TDateTime): TDateTime;
    class operator Add( a: TDateTime; const b: Txs_duration): TDateTime;
    class operator Subtract( const a, b: Txs_duration): Txs_duration;
    class operator Multiply( const b: Txs_duration; Factor: integer): Txs_duration;
  end;


{$REGION 'summary'}
type
/// <summary>
///  Helper functions for XML data type xs:time, xs:date and xs:dateTime
/// </summary>
{$ENDREGION}
Txs_dateTime = class
  public
    class function EncodeTime( pdValue: TTime; pbIncludeTZ: boolean; TZMinutes: integer): string;
    class function DecodeTime( const psEncodedValue: string; var pdDecoded: TTime;
                               var pbIncludeTZ: boolean; var TZMinutes: integer): boolean;

    class function EncodeDate( pdValue: TDate): string;
    class function DecodeDate( const psEncodedValue: string; var pdDecoded: TDate;
                               var pbIncludeTZ: boolean; var TZMinutes: integer): boolean;

    class function Encode( pdValue: TDateTime; pbIncludeTZ: boolean; TZMinutes: integer): string;
    class function Decode( const psEncodedValue: string; var pdDecoded: TDateTime;
                           var pbIncludeTZ: boolean; var TZMinutes: integer): boolean;
  end;

TDateHelper = class
  public
    class function  EncodeDate( Year: integer; Month, Day: word): TDate;
    class procedure DecodeDate( Value: TDate; var Year: integer; var Month, Day: word);
  end;

Txs_gYearMonth = class
  public
    class function Encode( pdYear: integer; pdMonth: word; pbIncludeTZ: boolean; TZMinutes: integer): string;
    class function Decode( const psEncodedValue: string; var pdYear: integer; var pdMonth: word;
                               var pbIncludeTZ: boolean; var TZMinutes: integer): boolean;
  end;

Txs_gMonth = class
  public
    class function Encode( pdMonth: word; pbIncludeTZ: boolean; TZMinutes: integer): string;
    class function Decode( const psEncodedValue: string; var pdMonth: word;
                               var pbIncludeTZ: boolean; var TZMinutes: integer): boolean;
  end;

Txs_gYear = class
  public
    class function Encode( pdYear: integer; pbIncludeTZ: boolean; TZMinutes: integer): string;
    class function Decode( const psEncodedValue: string; var pdYear: integer;
                               var pbIncludeTZ: boolean; var TZMinutes: integer): boolean;
  end;

Txs_gMonthDay = class
  public
    class function Encode( pdMonth, pdDay: word; pbIncludeTZ: boolean; TZMinutes: integer): string;
    class function Decode( const psEncodedValue: string; var pdMonth, pdDay: word;
                               var pbIncludeTZ: boolean; var TZMinutes: integer): boolean;
  end;

Txs_gDay = class
  public
    class function Encode( pdDay: word; pbIncludeTZ: boolean; TZMinutes: integer): string;
    class function Decode( const psEncodedValue: string; var pdDay: word;
                               var pbIncludeTZ: boolean; var TZMinutes: integer): boolean;
  end;


Txs_Binary = class
  public
    class function EncodeHex( poData: TStream): string;
    class function DecodeHex( const psEncodedValue: string; var poData: TStream): boolean;
    class function EncodeBase64( poData: TStream): string;
    class function DecodeBase64( const psEncodedValue: string; var poData: TStream): boolean;
  end;


{$REGION 'summary'}
type
/// <summary>
///   Helper functions for MS XML's XPath 1.0 engine
/// </summary>
{$ENDREGION}
TXPath = class
  public
    class function SelectFirst( const poFocusNode: IXMLNode; const psXPath: string; var poSelectedNode: IXMLNode): boolean;
    class function Select     ( const poFocusNode: IXMLNode; const psXPath: string): TArray<IXMLNode>;
    class function Enum( const poNodes: TArray<IXMLNode>) : IEnumerable<IXMLNode>;

    class function SelectString( const poFocusNode: IXMLNode; const psXPath: string; var psValue: string): boolean;
    class function SelectedString( const poFocusNode: IXMLNode; const psXPath: string): string;
    class function SelectedTrimString( const poFocusNode: IXMLNode; const psXPath: string): string;
    class function SelectBoolean( const poFocusNode: IXMLNode; const psXPath: string; pbDefault: boolean): boolean;
    class function SelectSingle( const poFocusNode: IXMLNode; const psXPath: string; var pnValue: single): boolean;
    class function SelectDouble( const poFocusNode: IXMLNode; const psXPath: string; var pnValue: double): boolean;
    class function SelectBCD( const poFocusNode: IXMLNode; const psXPath: string; var pnValue: TBcd): boolean;
    class function SelectInt32 ( const poFocusNode: IXMLNode; const psXPath: string; var pnValue: integer): boolean;
    class function SelectUInt32( const poFocusNode: IXMLNode; const psXPath: string; var pnValue: cardinal): boolean;
    class function SelectInt64 ( const poFocusNode: IXMLNode; const psXPath: string; var pnValue: int64): boolean;
    class function SelectUInt64( const poFocusNode: IXMLNode; const psXPath: string; var pnValue: uint64): boolean;
    class function SelectDuration( const poFocusNode: IXMLNode; const psXPath: string; var pdValue: Txs_duration): boolean;
    class function SelectDateTime( const poFocusNode: IXMLNode; const psXPath: string; var pdValue: TDateTime; var pbIncludeTZ: boolean; var TZMinutes: integer): boolean;
    class function SelectDate( const poFocusNode: IXMLNode; const psXPath: string; var pdValue: TDate; var pbIncludeTZ: boolean; var TZMinutes: integer): boolean;
    class function SelectTime( const poFocusNode: IXMLNode; const psXPath: string; var pdValue: TTime; var pbIncludeTZ: boolean; var TZMinutes: integer): boolean;
    class function SelectHexBinary( const poFocusNode: IXMLNode; const psXPath: string; var poData: TStream): boolean;
    class function SelectBase64Binary( const poFocusNode: IXMLNode; const psXPath: string; var poData: TStream): boolean;

  end;



function StringValue( poNode: IXMLNode): string;

type TDeclKind = (kXMLdecl, kTextDecl);
function DemarcEncoding( const Encoding: string; Decl: TDeclKind; XMLVer: string): string;

function XMLTrim( const Value: string): string;          overload;
function XMLTrim( const Value: utf8string): utf8string;  overload;

type
IStyleSheetParameter = interface
  ['{29A0F508-7EF1-4CAC-ABC0-50ABCC81707B}']
    {$REGION 'property accessors'}
    function  GetURI: string;
    function  GetName: string;
    function  GetValue: string;
    procedure SetValue( const Value: string);
    function  GetIsNull: boolean;
    procedure SetIsNull( Value: boolean);
    {$ENDREGION}
    property URI: string   read GetURI;
    property ParamterName: string  read GetName;
    property ParameterValue: string read GetValue write SetValue;
    property isNull: boolean read GetIsNull write SetIsNull;
end;

IStyleSheetParameterSet = interface
  ['{3481BD34-7220-4436-8C5B-7759F7BF05F1}']
    function Param( const URI, Name: string): IStyleSheetParameter;
    function GetEnumerator: IEnumerator<IStyleSheetParameter>;
end;
TParametersSetProc = reference to procedure( const poParams: IStyleSheetParameterSet);

function Contains  ( const sHaystack, sPattern: string; isCaseInsensitive: boolean = False): boolean;
function StartsWith( const sHaystack, sPattern: string; isCaseInsensitive: boolean = False): boolean;
function EndsWith  ( const sHaystack, sPattern: string; isCaseInsensitive: boolean = False): boolean;
function CharIn( Ch: Char; const List: string): boolean;
function NormalizeXMLSpace( const Raw: string): string;

function Substring( const sHaystack: string; Pos: Integer; Len: integer = -1): string;
// The substring function returns the substring of sHaystack
//  starting at the position specified by Pos
//  with length specified by Len.
// For example, substring("12345",2,3) returns "234".
// If the Len is -1, it returns the substring starting at the Pos and
//  continuing to the end of the string.
// For example, substring("12345",2) returns "2345".

function SubstringBefore( const sHaystack, sSubstring: string; isCaseInsensitive: boolean = False): string;
// The SubstringBefore function returns the substring of sHaystack that
//  precedes the first occurrence of the sSubstring in sHaystack,
// or the empty string if sHaystack does not contain sSubstring.
// For example, SubstringBefore("1999/04/01","/") returns 1999.

function SubstringAfter( const sHaystack, sSubstring: string; isCaseInsensitive: boolean = False): string;
// The SubstringAfter function returns the substring of sHaystack that
//  follows the first occurrence of the sSubstring in sHaystack,
// or the empty string if sHaystack does not contain sSubstring.

function StringLength( const sValue: string): integer;
// Length of the string in unicode character. Not the length of Char.

function Translate( const sContent, sPattern, sReplacement: string): string;
// The translate function returns sContent with occurrences of characters
//  in sPattern replaced by the character at the corresponding position in sReplacement.
// For example, Tnslate("bar","abc","ABC") returns the string BAr.
// If there is a character in sPattern with no character at a corresponding position in sReplacement
// (because sPattern is longer than sReplacement), then occurrences of that character in sContent are removed.
// For example, Translate("--aaa--","abc-","ABC") returns "AAA".
// If a character occurs more than once in sPattern, then the first occurrence determines the replacement character.
// If the sReplacement string is longer than the sPattern string,
//  then excess characters are ignored.
// Note: This works on a unicode character basis, not a Char basis.

function Tokenize( const Haystack: string; ReturnSet: TMatchResultSet; Pattern: TPatternFunc): IEnumerable<RMatchFragment>;

type
TTransformOptions = (
  oSimpleXSLT1,
  oPrettyPrint,
  oAsNow
  );

TTransformOptionSet = set of TTransformOptions;

TXMLVersionInstruction = (xmlDefault, xml10, xml11);
TXSDVersionInstruction = (xsdDefault, xsd10, xsd11);

TStyleSheet = class( TComponent)
  private
    oContent: TStrings;
    oOptions: TTransformOptionSet;
    sSaxonDir: string;
    sContentName: string;
    hasConvertedResource: boolean;
    sWorkDir: string;
    sURI: string;
    oFilesToDelete: TStrings;
    sCatalog: string;
    nRepeats: cardinal;
    eXMLVer: TXMLVersionInstruction;
    eXSDVer: TXSDVersionInstruction;
    dtAsNow: TDateTime;
    nAsNowTZ: integer;

    procedure SetContent( Value: TStrings);
    function  ContentAsUtf8: UTF8String;
    function  NewTempFile( bDeleteWhenFinished: boolean; const Ext: string = ''): string;
    procedure PurgeTemporaryFiles;

  published
    property Content: TStrings  read oContent write SetContent;
    property ContentAsResource: string  read sContentName write sContentName;
    property Options: TTransformOptionSet  read oOptions write oOptions;
    property URI: string  read sURI write sURI;
    property Catalog: string   read sCatalog write sCatalog;
    property RepeatCount: cardinal     read nRepeats write nRepeats default 1;
    property XMLVer: TXMLVersionInstruction   read eXMLVer write eXMLVer default xmlDefault;
    property XSDVer: TXSDVersionInstruction   read eXSDVer write eXSDVer default xsdDefault;
    // Event to create and acquire a temporary file.

  public
    property SaxonDirectory: string    read sSaxonDir write sSaxonDir;
    property WorkingDirectory: string  read sWorkDir write sWorkDir;
    property AsNow: TDateTime          read dtAsNow  write dtAsNow;
    property AsNowTZ_Minutes: integer  read nAsNowTZ write nAsNowTZ;

    constructor Create( AOwner: TComponent); override;
    destructor Destroy; override;
    function Parameters: IStyleSheetParameterSet;
    function Transform(
      InputDocumentFN : string;
      OutputDocumentFN: string;
      var ErrorOutput: string;
      const WithParams: IStyleSheetParameterSet): boolean;
    procedure LoadedAtRunTime;

  protected
    procedure Loaded; override;

  private
    procedure SetStaticDefaults;
    procedure SetDynamicDefaults;
  end;

  XFocus = record  // Record containing the focus node for an XPATH expression.
      N: IXMLNode;
      class operator Explicit( const N1: IXMLNode): XFocus;
      class operator Divide( const a: XFocus; const b: string): IEnumerable<IXMLNode>;
      class operator IntDivide( const a: XFocus; const b: string): IXMLNode;
//      class operator In( const a: string; const b: XFocus): boolean;
//      class operator Multiply( const a: XFocus; StyleSheet: TStyleSheet): IXMLDocument;  overload;
    end;



{$REGION 'Internal'}
TTokenizer = class( TInterfacedObject, IEnumerable<RMatchFragment>)
  private
    sHaystack: string;
    oReturnSet: TMatchResultSet;
    oPattern: TPatternFunc;
    function GetEnumerator: IEnumerator;
    function GetEnumeratorIntf: IEnumerator<RMatchFragment>;
    function IEnumerable<RMatchFragment>.GetEnumerator = GetEnumeratorIntf;

  private type
    TTokenCursor = class( TInterfacedObject, IEnumerator<RMatchFragment>)
      private
        nPosition: integer;
        oTokenizer: TTokenizer;
        oHold: IInterface;
        oCurrentRec: RMatchFragment;
        bBufferedMatch: boolean;
        function  GetCurrent: TObject;
        function  MoveNext: Boolean;
        procedure Reset;
        function  GetCurrentRec: RMatchFragment;
        function  IEnumerator<RMatchFragment>.GetCurrent = GetCurrentRec;
        constructor Create( poTokenizer: TTokenizer);
      end;

  public
    constructor Create( const Haystack: string; ReturnSet: TMatchResultSet; Pattern: TPatternFunc);
  end;
{$ENDREGION}


type
UniChar = UCS4Char;

RCodePoint = record
    FIndex: integer;
    FCurrentAsUniChar: UniChar;
    FCurrentAsString: string;
  end;

ICodePointEnumerator = interface
  ['{BC104326-7481-4E41-BFF8-A2BF6759AC66}']
    function MoveNext: boolean;
    function GetCurrent: RCodePoint;
    property Current: RCodePoint  read GetCurrent;
  end;

RUnicodeParser = record
   private
    FValue: string;

   public
    function GetEnumerator: ICodePointEnumerator;
    class function ByCodePoint( const Value: string): RUnicodeParser; static;
  end;

 ITransform = interface
   ['{1004AE9A-D4AE-40E1-956D-AD98801AF7C1}']
     procedure SetInputDocumentFileName ( const sValue: string);
     procedure SetOutputDocumentFileName( const sValue: string);
     procedure AddParameter( const sParamBaseName, sParamValue, sNamespaceURI: string);
     procedure Transform;

     property InputDocumentFileName : string    write SetInputDocumentFileName;
     property OutputDocumentFileName: string    write SetOutputDocumentFileName;
   end;

XSelfTransform = record
    csLiteralStyleSheet: string;
    cParamsProc: TParametersSetProc;
    bOk: boolean;
    class operator Explicit( const psLiteralStyleSheet: string): XSelfTransform;
    class operator Add( const This: XSelfTransform; ParamsProc: TParametersSetProc): XSelfTransform;
    class operator Multiply( const sDocumentFileName: string; const This: XSelfTransform): XSelfTransform; overload;
    class operator Multiply( const This: XSelfTransform; const sDocumentFileName: string): XSelfTransform; overload;
  end;

function ResourceName_to_Utf8( const ResourceName: string): UTF8String;
function MS_Transform( const sStylesheet: string; doPrettyPrint: boolean): ITransform;

type
  TUtilsFilenameToUriOptions = set of (fuSetLocalhost, fuPlainColon);
// Copied from UriUtils
function FilenameToUriStr( const Path: string; const Opt: TUtilsFilenameToUriOptions): string;

var
  sGlobalSaxonDir: string;

const
  DefaultNamespaceBase = 'https://github.com/SeanBDurkin/DUnitX/stylesheet';

implementation



































uses XMLDoc, xmldom, msxmldom, msxml, SysUtils, StrUtils, Math, Character,
     Generics.Collections, Windows, Contnrs, IOUtils, ShlObj, EncdDecd;


type
IXMLDOMDocument2 = interface(IXMLDOMDocument)
  ['{2933BF95-7B36-11D2-B20E-00C04F983E60}']
  function Get_namespaces: IXMLDOMSchemaCollection; safecall;
  function Get_schemas: OleVariant; safecall;
  procedure _Set_schemas(otherCollection: OleVariant); safecall;
  function validate: IXMLDOMParseError; safecall;
  procedure setProperty(const name: WideString; value: OleVariant); safecall;
  function getProperty(const name: WideString): OleVariant; safecall;
  property namespaces: IXMLDOMSchemaCollection read Get_namespaces;
  property schemas: OleVariant read Get_schemas write _Set_schemas;
end;

TXMLDocProc = reference to procedure( const poDoc: IXMLDocument);

RParamKey = record
    sURI: string;
    sName: string;
  end;

TParameter = class( TInterfacedObject, IStyleSheetParameter)
  private
    oParamName: RParamKey;
    sValue: string;
    bIsNull: boolean;
    function  GetURI: string;
    function  GetName: string;
    function  GetValue: string;
    procedure SetValue( const Value: string);
    function  GetIsNull: boolean;
    procedure SetIsNull( Value: boolean);
  public
    constructor Create( const poParamName: RParamKey);
  end;

TStyleSheetParameterSet = class( TInterfacedObject, IStyleSheetParameterSet)
  private type
    TKeyComparer = class( TInterfacedObject, IEqualityComparer<RParamKey>)
      private
        function CmprEquals( const Left, Right: RParamKey): boolean;
        function IEqualityComparer<RParamKey>.Equals = CmprEquals;
        function CmprGetHashCode( const Value: RParamKey): integer;
        function IEqualityComparer<RParamKey>.GetHashCode = CmprGetHashCode;
      end;
  private type
    TParamDict = class( TDictionary<RParamKey,TParameter>)
      protected
        procedure ValueNotify(const Value: TParameter; Action: Generics.Collections.TCollectionNotification); override;
      public
        constructor Create( poStyleSheet: TStylesheet);
      end;
  private
    coStyleSheet: TStyleSheet;
    coParameters: TParamDict;
    function Param( const URI, Name: string): IStyleSheetParameter;
    function GetEnumerator: IEnumerator<IStyleSheetParameter>;

  public
    constructor Create( poStyleSheet: TStyleSheet);
    destructor Destroy; override;
  end;


TParameterCursor = class( TInterfacedObject, IEnumerator<IStyleSheetParameter>)
  private
    oParamSet: TStyleSheetParameterSet;
    nIndex   : TEnumerator<TParameter>;
    oHold    : IInterface;
    function  GetCurrent: TObject;
    function  MoveNext: Boolean;
    procedure Reset;
    function  GetCurrentIntf: IStyleSheetParameter;
    function  IEnumerator<IStyleSheetParameter>.GetCurrent = GetCurrentIntf;
  public
    constructor Create( poParamSet: TStyleSheetParameterSet);
    destructor Destroy; override;
  end;



function CyclicDiv( Quotient: int64; Divisor: integer): integer;
begin
  Assert( Divisor > 0);
  if Quotient >= 0 then
      result := Quotient div Divisor
    else
      result := -( (Divisor - Quotient - 1) div Divisor)
end;


function DaysPerYear( y: integer): integer;
begin
	result := (365*y) + CyclicDiv( y, 4) - CyclicDiv( y, 100) + CyclicDiv( y, 400)
end;

// http://alcor.concordia.ca/~gpkatch/gdate-algorithm.html
function g( y,m,d: integer): integer;
begin
  result :=
       d - 1 + // Day component.
       ((306 * ((m  + 9) mod 12) + 5) div 10) + // Month component.
       DaysPerYear( y)
end;


procedure d( g: integer; var y, m, d: integer);
var
   diff: integer;
   mi: integer;
   g64: Int64;
   miPlus2: integer;

   procedure ComputeDiff;
   begin
   diff := g - DaysPerYear( y)
   end;

begin
g64 := g;
y := CyclicDiv( (10000*g64 + 14780), 3652425);
ComputeDiff;
if diff < 0 then
  begin
  Dec( y);
  ComputeDiff
  end;
mi := CyclicDiv( 100 * diff + 52, 3060);
miPlus2 := mi + 2;
m := (miPlus2 mod 12) + 1;
Inc( y, miPlus2 div 12);
d := diff - ((mi*306 + 5) div 10) + 1
end;


const Delphi_To_1Mar1BC_Offset = 693899;


class function TDateHelper.EncodeDate( Year: integer; Month, Day: word): TDate;
// Replacement function for SysUtils.SBDEnoodeDate()
begin
  result := g( Year, Month, Day) - Delphi_To_1Mar1BC_Offset
end;

class procedure TDateHelper.DecodeDate( Value: TDate; var Year: integer; var Month, Day: Word);
// Replacement procedure for SysUtils.DecodeDate()
var
  iMonth, iDay: integer;
begin
  d( Math.Floor( Value) + Delphi_To_1Mar1BC_Offset, Year, iMonth, iDay);
  Month := iMonth;
  Day   := iDay
end;


procedure SetMSXMLProperty( const poDoc: IXMLDocument; const PropName, PropValue: string; doForce: boolean);
var
  XMLDOMNodeRef: IXMLDOMNodeRef;
  Dom2: IXMLDOMDocument2;
begin
if Supports( poDoc.DOMDocument, IXMLDOMNodeRef, XMLDOMNodeRef) and
   Supports( XMLDOMNodeRef.GetXMLDOMNode, IXMLDOMDocument2, Dom2) and
   (doForce or (Dom2.getProperty( PropName) <> PropValue)) then
    Dom2.setProperty( PropName, PropValue)
end;

function  CreateXMLDoc( OnLoad: TXMLDocProc): IXMLDocument;
var
  oDoc: TXMLDocument;
begin
  oDoc  := TXMLDocument.Create( nil);
  oDoc.Options := [doNodeAutoCreate, doNodeAutoIndent, doAttrNull,
                   doAutoPrefix, doNamespaceDecl];
  oDoc.DOMVendor := GetDOMVendor( 'MSXML');
  result := oDoc;
  if assigned( OnLoad) then
    OnLoad( result);
  result.Active := True;
  SetMSXMLProperty( Result, 'SelectionLanguage', 'XPath', False);
end;



class function TXMLDoc.FromString( const psXMLStr: string): IXMLDocument;
begin
result := CreateXMLDoc(
  procedure( const poDoc: IXMLDocument)
  begin
    poDoc.LoadFromXML( psXMLStr)
  end);
end;

class function TXMLDoc.FromUTF8String( const psXMLStr: utf8string): IXMLDocument;
begin
result := CreateXMLDoc(
  procedure( const poDoc: IXMLDocument)
  begin
    poDoc.LoadFromXML( UTF8ToString( psXMLStr))
  end);
end;


class function TXMLDoc.NewEmtpy: IXMLDocument;
begin
result := CreateXMLDoc( nil)
end;

class function TXMLDoc.FromFile( const psXMLFN: string): IXMLDocument;
begin
result := CreateXMLDoc(
  procedure( const poDoc: IXMLDocument)
  begin
    poDoc.LoadFromFile( psXMLFN)
  end);
end;

class function TXMLDoc.FromStream( const Stream: TStream; EncodingType: TXMLEncodingType): IXMLDocument;
begin
result := CreateXMLDoc(
  procedure( const poDoc: IXMLDocument)
  begin
    poDoc.LoadFromStream( Stream, EncodingType)
  end);
end;

class procedure TXMLDoc.DeclareSelectionNamespaces( const Doc: IXMLDocument; const Namespaces: string);
begin
  SetMSXMLProperty( Doc, 'SelectionNamespaces', Namespaces, True);
end;

class function TXPath.SelectFirst( const poFocusNode: IXMLNode; const psXPath: string; var poSelectedNode: IXMLNode): boolean;
var
  oDomNodeSelect: IDomNodeSelect;
  oDOMNode      : IDomNode;
  oDocAccess    : IXmlDocumentAccess;
  oDoc          : TXmlDocument;
begin
  poSelectedNode := nil;
  oDOMNode       := nil;
  oDoc           := nil;
  if assigned( poFocusNode) and
     Supports( poFocusNode.DOMNode, IDomNodeSelect, oDomNodeSelect) then
    oDOMNode := oDomNodeSelect.selectNode( psXPath);
  if assigned( poFocusNode) and Supports( poFocusNode.GetOwnerDocument, IXmlDocumentAccess, oDocAccess) then
    oDoc := oDocAccess.DocumentObject;
  if assigned( oDOMNode) then
    poSelectedNode := TXmlNode.Create( oDOMNode, nil, oDoc);
  result := assigned( poSelectedNode)
end;

class function TXPath.SelectString(
  const poFocusNode: IXMLNode; const psXPath: string; var psValue: string): boolean;
var
  Node: IXMLNode;
begin
  result := SelectFirst( poFocusNode, psXPath, Node);
  if result then
    psValue := StringValue( Node)
end;

class function TXPath.SelectedString(
  const poFocusNode: IXMLNode; const psXPath: string): string;
begin
  if not SelectString( poFocusNode, psXPath, result) then
    result := ''
end;

class function TXPath.SelectedTrimString(
  const poFocusNode: IXMLNode; const psXPath: string): string;
begin
  result := XMLTrim( SelectedString( poFocusNode, psXPath))
end;


class function TXPath.SelectHexBinary(const poFocusNode: IXMLNode;
  const psXPath: string; var poData: TStream): boolean;
begin
  // TODO
  result := False
end;

class function TXPath.SelectInt32(
  const poFocusNode: IXMLNode; const psXPath: string; var pnValue: integer): boolean;
var
  sValue: string;
  Code: integer;
begin
  sValue := SelectedTrimString( poFocusNode, psXPath);
  Val( sValue, pnValue, Code);
  result := (sValue <> '') and (Code = 0)
end;

class function TXPath.SelectInt64(
  const poFocusNode: IXMLNode; const psXPath: string; var pnValue: int64): boolean;
var
  sValue: string;
  Code: integer;
begin
  sValue := SelectedTrimString( poFocusNode, psXPath);
  Val( sValue, pnValue, Code);
  result := (sValue <> '') and (Code = 0)
end;

class function TXPath.SelectSingle(
  const poFocusNode: IXMLNode; const psXPath: string; var pnValue: single): boolean;
begin
  result := Txs_float.Decode(  SelectedTrimString( poFocusNode, psXPath), pnValue)
end;

class function TXPath.SelectTime(
  const poFocusNode: IXMLNode; const psXPath: string; var pdValue: TTime; var pbIncludeTZ: boolean;
  var TZMinutes: integer): boolean;
begin
  result := Txs_dateTime.DecodeTime( SelectedTrimString( poFocusNode, psXPath), pdValue, pbIncludeTZ, TZMinutes)
end;

class function TXPath.SelectUInt32(const poFocusNode: IXMLNode;
  const psXPath: string; var pnValue: cardinal): boolean;
var
  sValue: string;
  Code: integer;
begin
  sValue := SelectedTrimString( poFocusNode, psXPath);
  Val( sValue, pnValue, Code);
  result := (sValue <> '') and (Code = 0)
end;

class function TXPath.SelectUInt64(const poFocusNode: IXMLNode;
  const psXPath: string; var pnValue: uint64): boolean;
var
  sValue: string;
  Code: integer;
begin
  sValue := SelectedTrimString( poFocusNode, psXPath);
  Val( sValue, pnValue, Code);
  result := (sValue <> '') and (Code = 0)
end;

class function TXPath.Select( const poFocusNode: IXMLNode; const psXPath: string): TArray<IXMLNode>;
var
  oDomNodeSelect: IDomNodeSelect;
  oDOMNode      : IDomNode;
  oDocAccess    : IXmlDocumentAccess;
  oDoc          : TXmlDocument;
  oDOMNodes     : IDOMNodeList;
  iDOMNode      : integer;
begin
  oDOMNodes := nil;
  SetLength( result, 0);
  if assigned( poFocusNode) and
     Supports( poFocusNode.DOMNode, IDomNodeSelect, oDomNodeSelect) then
      oDOMNodes := oDomNodeSelect.SelectNodes( psXPath);
  if not assigned( oDOMNodes) then exit;
  SetLength( result, oDOMNodes.Length);
  for iDOMNode := 0 to oDOMNodes.Length - 1 do
  begin
    oDoc := nil;
    oDOMNode := oDOMNodes.item[iDOMNode];
    if Supports( oDOMNode, IXmlDocumentAccess, oDocAccess) then
      oDoc := oDocAccess.DocumentObject;
    result[ iDOMNode] := TXmlNode.Create( oDOMNode, nil, oDoc) as IXMLNode
  end
end;


class function TXPath.SelectBase64Binary(
  const poFocusNode: IXMLNode; const psXPath: string; var poData: TStream): boolean;
var
  Node: IXMLNode;
begin
  result := SelectFirst( poFocusNode, psXPath, Node) and
            Txs_Binary.DecodeBase64( Node.Text, poData)
end;

class function TXPath.SelectBCD(
  const poFocusNode: IXMLNode; const psXPath: string; var pnValue: TBcd): boolean;
begin
  result := Txs_decimal.Decode( SelectedTrimString( poFocusNode, psXPath), pnValue)
end;

class function TXPath.SelectBoolean(
  const poFocusNode: IXMLNode; const psXPath: string; pbDefault: boolean): boolean;
begin
  result := Txs_boolean.Decode( SelectedTrimString( poFocusNode, psXPath), pbDefault)
end;

class function TXPath.SelectDate(
  const poFocusNode: IXMLNode; const psXPath: string; var pdValue: TDate; var pbIncludeTZ: boolean;
  var TZMinutes: integer): boolean;
begin
  result := Txs_dateTime.DecodeDate( SelectedTrimString( poFocusNode, psXPath), pdValue, pbIncludeTZ, TZMinutes)
end;

class function TXPath.SelectDateTime(const poFocusNode: IXMLNode;
  const psXPath: string; var pdValue: TDateTime; var pbIncludeTZ: boolean;
  var TZMinutes: integer): boolean;
begin
  result := Txs_dateTime.Decode( SelectedTrimString( poFocusNode, psXPath), pdValue, pbIncludeTZ, TZMinutes)
end;

class function TXPath.SelectDouble(const poFocusNode: IXMLNode;
  const psXPath: string; var pnValue: double): boolean;
begin
  result := Txs_float.Decode( SelectedTrimString( poFocusNode, psXPath), pnValue)
end;

class function TXPath.SelectDuration(
  const poFocusNode: IXMLNode; const psXPath: string; var pdValue: Txs_duration): boolean;
begin
  result := pdValue.Decode( SelectedTrimString( poFocusNode, psXPath))
end;

class function Txs_boolean.Encode( pbValue: boolean): string;
//http://www.w3.org/TR/xmlschema-2/#boolean
//3.2.2.1 Lexical representation
//An instance of a datatype that is defined as ·boolean· can have the following legal literals {true, false, 1, 0}.
begin
  result := IfThen( pbValue, 'true', 'false')
end;


class function Txs_boolean.Decode( const psEncodedValue: string; Default: boolean): boolean;
begin
  if psEncodedValue = '' then
      result := Default
    else
      result := Pos( LowerCase( Copy( psEncodedValue + ' ', 1, 1)), 't1') > 0
end;

class function Txs_float.Decode( const psEncodedValue: string; var pnDecoded: single): boolean;
//The special values positive and negative infinity and not-a-number have lexical representations INF, -INF and NaN, respectively. Lexical representations for zero may take a positive or negative sign.
//For example, -1E4, 1267.43233E12, 12.78e-2, 12 , -0, 0 and INF are all legal literals for float.
var
  nValueAsDouble: double;
begin
  result := Decode( psEncodedValue, nValueAsDouble);
  if Result then
    pnDecoded := nValueAsDouble
end;

class function Txs_float.Decode( const psEncodedValue: string; var pnDecoded: double): boolean;
begin
  result := True;
  if psEncodedValue = 'INF' then
    pnDecoded := Infinity
  else if psEncodedValue = '-INF' then
    pnDecoded := NegInfinity
  else if psEncodedValue = 'NaN' then
    pnDecoded := NaN
  else
    result := TryStrToFloat( psEncodedValue, pnDecoded)
end;

class function Txs_float.Encode( pnValue: single): string;
var
  nValueAsDouble: double;
begin
  nValueAsDouble := pnValue;
  result := Encode( nValueAsDouble)
end;


function FloatToStrEx( Value: double): string;
// Copied from TypeTrans
var
  FormatSettings : TFormatSettings;
begin
  if IsNan( Value) then
    result := 'NaN'
  else if IsInfinite( Value) then
  begin
    if ((PInt64( @Value)^ and $8000000000000000) = $8000000000000000) then
      result := '-INF'
    else
      result := 'INF';
  end
  else
  begin
    FormatSettings.DecimalSeparator := '.';
    result := FloatToStr( Value, FormatSettings);
  end;
end;

class function Txs_float.Encode( pnValue: double): string;
begin
  result := FloatToStrEx( pnValue)
end;

{ Txs_decimal }

class function Txs_decimal.Decode(
  psEncodedValue: string; var pnDecoded: TBcd): boolean;
begin
  result := FMTBcd.TryStrToBcd( psEncodedValue, pnDecoded)
end;

class function Txs_decimal.Encode( const pnValue: TBcd): string;
begin
  result := FMTBcd.BcdToStr( pnValue)
end;

{$R-}
function TryStrToUINT64( StrValue:String; var uValue:UInt64 ):Boolean;
// Thanks to Warren P (http://stackoverflow.com/users/84704/warren-p)
// http://stackoverflow.com/questions/6077258
var
  Start,Base,Digit:Integer;
  n:Integer;
  Nextvalue:UInt64;
begin
  result := false;
  Digit := 0;
  Base := 10;
  Start := 1;
  StrValue := UpperCase( StrValue);
  if StrValue='' then
    exit;
  if StrValue[1]='-' then
    exit;
  if StrValue[1]='$' then
  begin
    Base := 16;
    Start := 2;
    if Length(StrValue)>17 then // $+16 hex digits = max hex length.
        exit;
  end;
  uValue := 0;
  for n := Start to Length(StrValue) do
  begin
      if Character.IsDigit(StrValue[n]) then
          Digit := Ord(StrValue[n])-Ord('0')
      else if  (Base=16) and (StrValue[n] >= 'A') and (StrValue[n] <= 'F') then
          Digit := (Ord(StrValue[n])-Ord('A'))+10
      else
          exit;// invalid digit.

      Nextvalue := (uValue*base)+digit;
      if (Nextvalue<uValue) then
          exit;
      uValue := Nextvalue;
  end;
  result := true; // success.
end;
{$R+}

class function Txs_integer.Decode(
  const psEncodedValue: string; var pnDecoded: int64): boolean;
begin
  result := TryStrToInt64( psEncodedValue, pnDecoded)
end;

class function Txs_integer.Decode(const psEncodedValue: string;
  var pnDecoded: uint64): boolean;
begin
  result := TryStrToUINT64( psEncodedValue, pnDecoded)
end;

class function Txs_integer.Encode( pnValue: int64): string;
begin
  result := IntToStr( pnValue)
end;

class function Txs_integer.Encode( pnValue: uint64): string;
begin
  // result := UIntToStr( pnValue)
  FmtStr( result, '%u', [pnValue])
end;

function FloorFrac( x: double): double;
begin
  result := x - Math.Floor( x)
end;



class function Txs_dateTime.Encode( pdValue: TDateTime; pbIncludeTZ: boolean; TZMinutes: integer): string;
//  '-'? yyyy '-' mm '-' dd 'T' hh ':' mm ':' ss ('.' s+)? (zzzzzz)?,
begin
  result := Txs_dateTime.EncodeDate( pdValue) + 'T' +
            Txs_dateTime.EncodeTime( FloorFrac( pdValue), pbIncludeTZ, TZMinutes)
end;


function daysInMonth( m: word; isLeap: boolean): Word;
begin
  case m of
    2: if isLeap then
           result := 29
         else
           result := 28;
    4, 6, 9, 11: result := 30;
    else         result := 31
  end;
end;


function ReadWord( const psEncoded: string; StartPos, Length: integer; MinVal, MaxVal: word; var Val1: word): boolean;
var
  Code: integer;
begin
  Val( Copy( psEncoded, StartPos, Length), Val1, Code);
  result := (Code = 0) and (Val1 >= MinVal) and (Val1 <= MaxVal);
end;



function Txs_dateTime_DecodeTimeZone(
  const psEncodedValue: string; var pbIncludeTZ: boolean; var TZMinutes: integer): boolean;
var
  L: integer;
  ch: char;
  TZHour, TZMin: word;

  function ReadWord( StartPos, Length: integer; MinVal, MaxVal: word; var Val1: word): boolean;
  begin
    result := DUnitX.Utils.XML2.ReadWord( psEncodedValue, StartPos, Length, MinVal, MaxVal, Val1);
  end;

begin
  result := True;
  TZMinutes := 0;
  L := Length( psEncodedValue);
  if L > 0 then
      ch := psEncodedValue[1]
    else
      ch := ' ';
  case ch of
    '+', '-': begin
           pbIncludeTZ := True;
           result := result and ReadWord( 2, 2, 0, 99, TZHour) and
                                ReadWord( 5, 2, 0, 59, TZMin);
           TZMinutes := TZHour * 60 + TZMin;
           result := result and (TZMinutes <= 840);
           if ch = '-' then
             TZMinutes := - TZMinutes
         end;
    'Z': begin
           pbIncludeTZ := True
         end;
    else pbIncludeTZ := False
  end;
end;


class function Txs_dateTime.DecodeDate(
  const psEncodedValue: string; var pdDecoded: TDate;
  var pbIncludeTZ: boolean; var TZMinutes: integer): boolean;
var
  sEncoded: string;
  L: integer;
  isNeg: boolean;
  P: integer;
  Year, Code: integer;
  Month, Day: word;
  isLeap: boolean;
  MaxDay: word;

  function ReadWord( StartPos, Length: integer; MinVal, MaxVal: word; var Val1: word): boolean;
  begin
    result := DUnitX.Utils.XML2.ReadWord( sEncoded, StartPos, Length, MinVal, MaxVal, Val1);
  end;

begin
  pdDecoded   := 0.0;
  Year        := 0;
  Month       := 1;
  Day         := 1;
  // '-9998-01-01'
  sEncoded := psEncodedValue;
  L := Length( sEncoded);
  isNeg := (L > 0) and (sEncoded[1] = '-');
  if isNeg then
    Delete( sEncoded, 1, 1);
  // '9998-01-01'
  P := Pos( '-', sEncoded);
  if P > 0 then
    Val( Copy( sEncoded, 1, P-1), Year, Code);
  result := (P > 0) and (P <= 5) and (Code=0) and (Year >= 0) and (Year <= 9999);
  if isNeg then
    Year := -Year;
  Delete( sEncoded, 1, P);
  // '01-01'
  result := result and ReadWord( 1, 2, 1, 12, Month);
  isLeap := IsLeapYear( Year);
  MaxDay := daysInMonth( Month, isLeap);
  result := result and ReadWord( 4, 2, 1, MaxDay, Day);
  Delete( sEncoded, 1, 5);
  if result and ((Year <= -9999) or (Year >= 10000)) then
    result := False;  // Minimun supported date is 1-Jan-9999 BC. This is xs:dateTime('-9998-01-01T00:00:00') in xml schema v1.1
  if result then
    begin
      pdDecoded := TDateHelper.EncodeDate( Year, Month, Day);
      result    := Txs_dateTime_DecodeTimeZone( sEncoded, pbIncludeTZ, TZMinutes)
    end
end;


function Txs_dateTime_DecodeTimeWithDayBump(
  const psEncodedValue: string; var DayBump: word; var pdDecoded: TTime; var pbIncludeTZ: boolean; var TZMinutes: integer): boolean;
var
  sEncoded: string;
  L: integer;
  P, P1: integer;
  Code: integer;
  Hour, Minute, Sec: Word;
  Milli: double;

  function ReadWord( StartPos, Length: integer; MinVal, MaxVal: word; var Val1: word): boolean;
  begin
    result := DUnitX.Utils.XML2.ReadWord( sEncoded, StartPos, Length, MinVal, MaxVal, Val1);
  end;

begin
  pdDecoded   := 0.0;
  TZMinutes   := 0;
  Hour        := 0;
  Minute      := 0;
  Sec         := 0;
  Milli       := 0.0;
  // TZHour      := 0;
  // TZMin       := 0;
  DayBump     := 0;
  // '00:00:00'
  sEncoded := psEncodedValue;
  result := ReadWord( 1, 2, 0, 24, Hour) and
            ReadWord( 4, 2, 0, 59, Minute) and
            ReadWord( 7, 2, 0, 59, Sec);
  Delete( sEncoded, 1, 8);
  // '.124+11:00'
  L  := Length( sEncoded);
  P  := Pos( '+', sEncoded);
  P1 := Pos( '-', sEncoded);
  if (P = 0) or (P1 < P) then
    P := P1;
  P1 := Pos( 'Z', sEncoded);
  if (P = 0) or (P1 < P) then
    P := P1;
  if P = 0 then
    P := L + 1;
  if (L > 0) and (sEncoded[1]='.') and (P >= 3) then
    begin
    Val( '0' + Copy( sEncoded, 1, P - 1), Milli, Code);
    result := result and (Code = 0);
    Delete( sEncoded, 1, P - 1)
    end;
  // '+11:00'
  if result and (Hour = 24) then
    begin
      result  := (Minute = 0) and (Sec = 0) and (Milli = 0.0);
      Hour    := 0;
      DayBump := 1
    end;
  result := result and Txs_dateTime_DecodeTimeZone( sEncoded, pbIncludeTZ, TZMinutes);
  if result then
    pdDecoded := SysUtils.EncodeTime( Hour, Minute, Sec, Round( Milli * 1000))
end;


class function Txs_dateTime.DecodeTime(
  const psEncodedValue: string;
  var pdDecoded: TTime; var pbIncludeTZ: boolean;
  var TZMinutes: integer): boolean;
var
  DayBump: word;
begin
  result := Txs_dateTime_DecodeTimeWithDayBump(
    psEncodedValue, DayBump, pdDecoded, pbIncludeTZ, TZMinutes)
end;



class function Txs_dateTime.Decode(
  const psEncodedValue: string; var pdDecoded: TDateTime; var pbIncludeTZ: boolean; var TZMinutes: integer): boolean;
var
  dDate: TDate;
  dTime: TTime;
  sDatePart, sTimePart: string;
  dDayBump: word;
  P: integer;
  iYear, iMonth, iDay: integer;
begin
  P := Pos( 'T', psEncodedValue);
  if P > 0 then
      begin
        sDatePart := Copy( psEncodedValue, 1, P - 1);
        sTimePart := Copy( psEncodedValue, P + 1, MaxInt)
      end
    else
      begin
        sDatePart := psEncodedValue;
        sTimePart := ''
      end;
  result := Txs_dateTime.DecodeDate( sDatePart, dDate, pbIncludeTZ, TZMinutes);
  if result and (sTimePart <> '') then
      result := Txs_dateTime_DecodeTimeWithDayBump( sTimePart, dDayBump, dTime, pbIncludeTZ, TZMinutes)
    else
      begin
        dDayBump := 0;
        dTime    := 0.0
      end;
  if result then
  begin
    dDate := dDate + dDayBump;
    // Earliest supported date is 1-Jan-9999 BC. This is xs:dateTime('-9998-01-01T00:00:00') in xml schema v1.1
    d( Math.Floor( dDate) + Delphi_To_1Mar1BC_Offset, iYear, iMonth, iDay);
    result := (iYear >= -9998) and (iYear <= 9999)
  end;
  if result then
    pdDecoded := dDate + dTime;
end;



class function Txs_dateTime.EncodeDate( pdValue: TDate): string;
var
  Year: integer;
  Month, Day: word;
begin
  TDateHelper.DecodeDate( pdValue, Year, Month, Day);
  // Don't use FormatDateTime() because it does not work for our target range of dates.
  result := Format( '%.4d-%.2d-%.2d', [Abs( Year), Month, Day]);
  if Year < 0 then
    result := '-' + result;
end;


function Txs_dateTime_EncodeTimeZone( TZMinutes: integer): string;
begin
  if TZMinutes = 0 then
      result := 'Z'
    else
      begin
      result := Format( '%.2d:%.2d', [Abs( TZMinutes) div 60, Abs( TZMinutes) mod 60]);
      if TZMinutes >= 0 then
          result := '+' + result
        else
          result := '-' + result
      end;
end;

class function Txs_dateTime.EncodeTime( pdValue: TTime; pbIncludeTZ: boolean; TZMinutes: integer): string;
var
  Hour, Min, Sec, Milli: word;
  L: integer;
  LastCh: Char;
begin
  SysUtils.DecodeTime( pdValue, Hour, Min, Sec, Milli);
  result := result + Format('%.2d:%.2d:%.2d', [Hour, Min, Sec] );
  if Milli > 0 then
    begin
      result := result + Format('.%.3d', [Milli]);
      L := Length( Result);
      repeat
        LastCh := result[L];
        if (LastCh = '0') or (LastCh = '.') then
          begin
            Dec( L);
            SetLength( result, L)
          end
      until (LastCh <> '0') or (L = 0)
    end;
  if pbIncludeTZ then
    result := result + Txs_dateTime_EncodeTimeZone( TZMinutes)
end;


function StringValue( poNode: IXMLNode): string;
var
  oChild: IXMLNode;
begin
  if not assigned( poNode) then
      result := ''
    else if poNode.NodeType in [ntElement, ntDocument, ntDocFragment] then
    begin
      result := '';
      oChild := poNode.ChildNodes.First;
      while assigned( oChild) do
      begin
        result := result + StringValue( oChild);
        oChild := oChild.NextSibling;
      end;
    end
    else
      result := poNode.text
end;


function isWhiteChar( Utf16CodeUnit: Char): boolean; inline;
begin
result := (Utf16CodeUnit = #$0020) or (Utf16CodeUnit = #$0009) or (Utf16CodeUnit = #$000D) or (Utf16CodeUnit = #$000A)
end;

function isWhiteCharA( Utf16CodeUnit: AnsiChar): boolean; inline;
begin
result := (Utf16CodeUnit = #$0020) or (Utf16CodeUnit = #$0009) or (Utf16CodeUnit = #$000D) or (Utf16CodeUnit = #$000A)
end;

function XMLTrim( const Value: string): string;
var
  L, Base: integer;
begin
result := Value;
L      := Length( result);
Base   := 0;
while L > 0  do
  begin
  if isWhiteChar( result[1]) then
      Base := 1
    else if isWhiteChar( result[L]) then
      Base := L
    else
      break;
  Delete( result, Base, 1);
  Dec( L)
  end
end;

function XMLTrim( const Value: utf8string): utf8string;
var
  L, Base: integer;
begin
result := Value;
L      := Length( result);
Base   := 0;
while L > 0  do
  begin
  if isWhiteCharA( result[1]) then
      Base := 1
    else if isWhiteCharA( result[L]) then
      Base := L
    else
      break;
  Delete( result, Base, 1);
  Dec( L)
  end
end;

function DemarcEncoding( const Encoding: string; Decl: TDeclKind; XMLVer: string): string;
begin

end;

function Contains( const sHaystack, sPattern: string; isCaseInsensitive: boolean = False): boolean;
var
  P: integer;
begin
  if isCaseInsensitive then
      P := Pos( LowerCase( sPattern), LowerCase( sHaystack))
    else
      P := Pos( sPattern, sHaystack);
  result := P > 0
end;

function StartsWith( const sHaystack, sPattern: string; isCaseInsensitive: boolean = False): boolean;
begin
  if isCaseInsensitive then
      result := SameText( Copy( sHaystack, 1, Length( sPattern)), sPattern)
    else
      result := Copy( sHaystack, 1, Length( sPattern)) = sPattern
end;

function EndsWith( const sHaystack, sPattern: string; isCaseInsensitive: boolean = False): boolean;
begin
  if isCaseInsensitive then
      result := SameText( Copy( sHaystack, Length( sHaystack) - Length( sPattern) + 1, Length( sPattern)), sPattern)
    else
      result := Copy( sHaystack, Length( sHaystack) - Length( sPattern) + 1, Length( sPattern)) = sPattern
end;

function CharIn( Ch: Char; const List: string): boolean;
begin
  result := Pos( Ch, List) > 0
end;

function NormalizeXMLSpace( const Raw: string): string;
var
  bIsWhite, bPrevWhite: boolean;
  ch: Char;
  iRaw, j: integer;
begin
  result := Raw;
  bPrevWhite := True;
  j := 1;
  for iRaw := 1 to Length( Raw) do
  begin
    ch := Raw[ iRaw];
    bIsWhite := IsWhiteChar( ch);
    if bPrevWhite and bIsWhite then continue;
    result[ j] := ch;
    Inc( j);
    bPrevWhite := bIsWhite
  end;
  if bPrevWhite and (j >= 2) then
    Dec( j);
  SetLength( result, j - 1)
end;

function Substring( const sHaystack: string; Pos: Integer; Len: integer = -1): string;
// The substring function returns the substring of sHaystack
//  starting at the position specified by Pos
//  with length specified by Len.
// For example, substring("12345",2,3) returns "234".
// If the Len is -1, it returns the substring starting at the Pos and
//  continuing to the end of the string.
// For example, substring("12345",2) returns "2345".
begin
  if Len <> -1 then
      result := Copy( sHaystack, Pos, Len)
    else
      result := Copy( sHaystack, Pos, MaxInt)
end;

function SubstringBefore( const sHaystack, sSubstring: string; isCaseInsensitive: boolean = False): string;
// The SubstringBefore function returns the substring of sHaystack that
//  precedes the first occurrence of the sSubstring in sHaystack,
// or the empty string if sHaystack does not contain sSubstring.
// For example, SubstringBefore("1999/04/01","/") returns 1999.
var
  P: integer;
  sHay, sSub: string;
begin
  if isCaseInsensitive then
      begin
        sHay := sHaystack;
        sSub := sSubstring;
      end
    else
      begin
        sHay := Lowercase( sHaystack);
        sSub := Lowercase( sSubstring);
      end ;
  P := Pos( sSub, sHay);
  if P > 0 then
      result := Copy( sHaystack, 1, P - 1)
    else
      result := ''
end;

function SubstringAfter( const sHaystack, sSubstring: string; isCaseInsensitive: boolean = False): string;
// The SubstringAfter function returns the substring of sHaystack that
//  follows the first occurrence of the sSubstring in sHaystack,
// or the empty string if sHaystack does not contain sSubstring.
var
  P: integer;
  sHay, sSub: string;
begin
  if isCaseInsensitive then
      begin
        sHay := sHaystack;
        sSub := sSubstring;
      end
    else
      begin
        sHay := Lowercase( sHaystack);
        sSub := Lowercase( sSubstring);
      end ;
  P := Pos( sSub, sHay);
  if P > 0 then
      result := Copy( sHaystack, P + Length( sSubstring), MaxInt)
    else
      result := ''
end;

function StringLength( const sValue: string): integer;
var
  CodePoint: RCodePoint;
begin
  result := 0;
  for CodePoint in RUnicodeParser.ByCodePoint( sValue) do
    Inc( result)
end;

function Translate( const sContent, sPattern, sReplacement: string): string;
var
  Dict: TDictionary<string,string>;
  CodePoint: RCodePoint;
  ReplacerCursor: ICodePointEnumerator;
  sReplac: string;
begin
  Dict := TDictionary<string,string>.Create;
  ReplacerCursor := RUnicodeParser.ByCodePoint( sReplacement).GetEnumerator;
  for CodePoint in RUnicodeParser.ByCodePoint( sPattern) do
    begin
      if assigned( ReplacerCursor) and ReplacerCursor.MoveNext then
          sReplac := ReplacerCursor.Current.FCurrentAsString
        else
          begin
            ReplacerCursor := nil;
            sReplac   := ''
          end;
      Dict.AddOrSetValue( CodePoint.FCurrentAsString, sReplac)
    end;
  result := '';
  for CodePoint in RUnicodeParser.ByCodePoint( sPattern) do
  begin
    if Dict.ContainsKey( CodePoint.FCurrentAsString) then
        sReplac := Dict[ CodePoint.FCurrentAsString]
      else
        sReplac := CodePoint.FCurrentAsString;
    result := result + sReplac
  end;
  Dict.Free
end;


function Tokenize( const Haystack: string; ReturnSet: TMatchResultSet; Pattern: TPatternFunc): IEnumerable<RMatchFragment>;
begin
  result := TTokenizer.Create( Haystack, ReturnSet, Pattern)
end;

type TNodeArrayEnumerable = class( TInterfacedObject, IEnumerable<IXMLNode>)
  private
    coNodes: TArray<IXMLNode>;
    function GetEnumerator: IEnumerator;
    function GetNodeEnumerator: IEnumerator<IXMLNode>;
    function IEnumerable<IXMLNode>.GetEnumerator = GetNodeEnumerator;
  private type
    TCursor = class( TInterfacedObject, IEnumerator<IXMLNode>)
      private
        coNodes: TArray<IXMLNode>;
        ciIdx  : integer;
        function  GetCurrent: TObject;
        function  MoveNext: Boolean;
        procedure Reset;
        function  GetCurrentNode: IXMLNode;
        function  IEnumerator<IXMLNode>.GetCurrent = GetCurrentNode;
        constructor Create( const poNodes: TArray<IXMLNode>);
      end;
  public
    constructor Create( const poNodes: TArray<IXMLNode>);
  end;

class function TXPath.Enum( const poNodes: TArray<IXMLNode>): IEnumerable<IXMLNode>;
begin
  result := TNodeArrayEnumerable.Create( poNodes)
end;

class operator XFocus.Divide(
  const a: XFocus; const b: string): IEnumerable<IXMLNode>;
begin
  result := TXPath.Enum( TXPath.Select( a.N, b))
end;

class operator XFocus.Explicit( const N1: IXMLNode): XFocus;
begin
  result.N := N1
end;

class operator XFocus.IntDivide(const a: XFocus; const b: string): IXMLNode;
begin
if not TXpath.SelectFirst( a.N, b, result) then
  result := nil
end;

//class operator XFocus.Multiply(
//  const a: XFocus; StyleSheet: TStyleSheet): IXMLDocument;
//begin
//result := NewDocument_MSXML;
//a.N.TransformNode( LoadDocument_MSXML_FromString( StyleSheet).Node, result)
//end;
//
//class operator XFocus.In( const a: string; const b: XFocus): boolean;
//var
//  Dummy: IXMLNode;
//begin
//  result := TXPath.SelectFirst( b.N, a, Dummy)
//end;


{ Txs_duration }

function Txs_duration_Normalise( const psUnnormal: Txs_duration): Txs_duration;
var
  nTemp: UInt64;
begin
  result := psUnnormal;
  if (result.nSeconds < 0.0) and (result.nMinutes = 0) and
      (result.nHours = 0) and  (result.nDays = 0) and
      (result.nMonths = 0) and (result.nYears = 0) then
    begin
      result.nSeconds  := - result.nSeconds;
      result.bPositive := not result.bPositive
    end;
  if result.nSeconds >= 60.0 then
  begin
    nTemp := Trunc( result.nSeconds / 60.0);
    result.nSeconds := result.nSeconds - (nTemp * 60);
    result.nMinutes := result.nMinutes + nTemp
  end;
  if result.nMinutes >= 60 then
  begin
    nTemp := result.nMinutes;
    result.nMinutes := nTemp mod 60;
    result.nHours := result.nHours + (nTemp div 60)
  end;
  if result.nHours >= 24 then
  begin
    nTemp := result.nHours;
    result.nHours := nTemp mod 24;
    result.nDays := result.nDays + (nTemp div 24)
  end;
  if result.nMonths >= 12 then
  begin
    nTemp := result.nMonths;
    result.nMonths := nTemp mod 12;
    result.nYears := result.nYears + (nTemp div 12)
  end;
  if (result.nSeconds = 0.0) and (result.nMinutes = 0) and
      (result.nHours = 0) and  (result.nDays = 0) and
      (result.nMonths = 0) and (result.nYears = 0) and
      (not result.bPositive) then
    result.bPositive := True
end;


class operator Txs_duration.Add( const a, b: Txs_duration): Txs_duration;
var
  NegB: Txs_duration;
begin
  if a.bPositive = b.bPositive then
      begin
        result.bPositive := a.bPositive;
        result.nYears    := a.nYears  + b.nYears;
        result.nMonths   := a.nMonths  + b.nMonths;
        result.nDays     := a.nDays    + b.nDays;
        result.nHours    := a.nHours   + b.nHours;
        result.nMinutes  := a.nMinutes + b.nMinutes;
        Result.nSeconds  := a.nSeconds + b.nSeconds;
        result.Normalise
      end
    else
      begin
        NegB := b;
        NegB.Negate;
        result := a - NegB
      end
end;


class operator Txs_duration.Add( const a: Txs_duration; b: TDateTime): TDateTime;
var
  Year: integer;
  Month, Day: word;
begin
  TDateHelper.DecodeDate( b, Year, Month, Day);
  if a.bPositive then
    begin
      {$WARNINGS OFF} // Operands will be coerced into int64 and then back to int32.
                      //  This is safe.
      Year := Year + a.nYears + (a.nMonths div 12);
      {$WARNINGS ON}
      Month := Month + (a.nMonths mod 12);
      if Month > 12 then
        begin
        Dec( Month, 12);
        Inc( Year)
        end;
      result := TDateHelper.EncodeDate( Year, Month, Day) + FloorFrac( b) + a.nDays +
                (a.nHours + ((a.nMinutes + (a.nSeconds / 60.0)) / 60.0) / 24.0);
    end
  else
    begin
      {$WARNINGS OFF} // Operands will be coerced into int64 and then back to int32.
                     // This is safe.
      Year := Year - a.nYears - (a.nMonths div 12);
      {$WARNINGS ON}
      if Month <= (a.nMonths mod 12) then
        begin
          Inc( Month, 12 - (a.nMonths mod 12));
          Dec( Year);
        end
        else
          Month := Month - (a.nMonths mod 12);
      result := TDateHelper.EncodeDate( Year, Month, Day) + FloorFrac( b) - a.nDays -
                (a.nHours + ((a.nMinutes + (a.nSeconds / 60.0)) / 60.0) / 24.0);
    end;
end;

class operator Txs_duration.Add( a: TDateTime; const b: Txs_duration): TDateTime;
begin
  result := b + a
end;


function Txs_duration.Decode( const psEncodedValue: string): boolean;
// The lexical representation for duration is the [ISO 8601] extended format PnYnMnDTnHnMnS
type
  TParticle = (eNull, eYear, eMonth, eDay, eTime, eHour, eMinute, eSecond);
var
  s: string;
  L: integer;
  p: integer;
  ch: char;
  EndP: integer;
  nDigits, nDots: integer;
  Done: TParticle;
  nDimension: integer;
begin
  s := psEncodedValue;
  L := Length( s);
  Done := eNull;
  Zeroise;
  bPositive := (L = 0) or (s[1] <> '-');
  if not bPositive then
    begin
    Delete( s, 1, 1);
    Dec( L)
    end;
  result := (L > 0) and (s[1]='P');
  if not result then exit;
  Delete( s, 1, 1);
  Dec( L);
  while result and (L > 0) do
  begin
    EndP := L;
    nDigits := 0;
    nDots   := 0;
    ch      := #1;
    for p := 1 to L do
    begin
      ch := s[p];
      if Character.IsDigit( ch) then
          Inc( nDigits)
        else if ch = '.' then
          Inc( nDots)
        else if ch = 'T' then
          begin
            EndP := p;
            break
          end
        else
          begin
            EndP := p - 1;
            break
          end
    end;
    if (nDigits > 0) and (nDots = 0) then
      begin
        result := (Pos( ch, 'YMDH') > 0) and TryStrToInt( Copy( s, 1, EndP), nDimension);
        Delete( s, 1, EndP + 1);
        Dec( L, EndP + 1);
        if L < 0 then
          L := 0;
        if result then
          case ch of
            'Y': begin
                   result := Done < eYear;
                   Done   := eYear;
                   nYears := nDimension
                 end;
            'M': begin
                   result := Done in [eNull, eYear, eTime, eHour];
                   if Done in [eNull, eYear] then
                       begin
                         Done := eMonth;
                         nMonths := nDimension
                       end
                     else
                       begin
                         Done := eMinute;
                         nMinutes := nDimension
                       end
                 end;
            'D': begin
                   result := Done < eDay;
                   Done   := eDay;
                   nDays := nDimension
                 end;
            'H':
                 begin
                   result := Done = eTime;
                   Done   := eHour;
                   nHours := nDimension
                 end;
            'S':
                 begin
                   result := Done in [eTime, eHour, eMinute];
                   Done := eSecond
                 end;
            end;
      end
    else if (nDigits > 0) and (nDots = 1) then
      begin
        result := (Done in [eTime, eHour, eMinute]) and
                  (L > EndP) and
                  (s[EndP+1] = 'S') and
                  TryStrToFloat( Copy( s, 1, EndP), nSeconds);
        Delete( s, 1, EndP + 1);
        Dec( L, EndP + 1);
        Done := eSecond
      end
    else if (nDigits = 0) and (nDots = 0) and (ch = 'T') then
      begin
        result := Done < eTime;
        Done   := eTime
      end
    else
      result := False
  end;
  if result then
    Normalise
end;

procedure Txs_duration.DiffDateTime( a, b: TDateTime);
var
  v: double;
begin
  v := b - a;
  Zeroise;
  bPositive := v >= 0;
  if not bPositive then
    v := -v;
  nDays := Trunc( v);
  nSeconds := Frac( v) * 24 * 60 * 60;
  Normalise
end;

function Txs_duration.Encode: string;
var
  V: Txs_duration;
begin
  V := Txs_duration_Normalise( self);
  result := 'P';
  if V.nYears <> 0 then
    result := result + IntToStr( V.nYears) + 'Y';
  if V.nMonths <> 0 then
    result := result + IntToStr( V.nMonths) + 'M';
  if V.nDays <> 0 then
    result := result + IntToStr( V.nDays) + 'D';
  if (V.nHours <> 0) or (V.nMinutes <> 0) or (V.nSeconds > 0.0) then
    result := result + 'T';
  if V.nHours <> 0 then
    result := result + IntToStr( V.nHours) + 'H';
  if V.nMinutes <> 0 then
    result := result + IntToStr( V.nMinutes) + 'M';
  if V.nSeconds <> 0 then
    result := Format( '%s%fS', [result, V.nSeconds]);
  if result = 'P' then
    result := 'P0D';
  if not V.bPositive then
    result := '-' + result
end;

function Txs_duration.Equals( const Right: Txs_duration): boolean;
var
  L, R: Txs_duration;
begin
  L := Txs_duration_Normalise( self);
  R := Txs_duration_Normalise( Right);
  result := (L.nSeconds  = R.nSeconds ) and
            (L.nMinutes  = R.nMinutes ) and
            (L.nHours    = R.nHours   ) and
            (L.nDays     = R.nDays    ) and
            (L.nMonths   = R.nMonths  ) and
            (L.nYears    = R.nYears   ) and
            (L.bPositive = R.bPositive)
end;

type
Txs_duration_GetEqualityComparer = class( TInterfacedObject, IEqualityComparer<Txs_duration>)
  private
    function CmprEquals( const Left, Right: Txs_duration): boolean;
    function IEqualityComparer<Txs_duration>.Equals = CmprEquals;
    function CmprGetHashCode( const Value: Txs_duration): integer;
    function IEqualityComparer<Txs_duration>.GetHashCode = CmprGetHashCode;
  public
    constructor Create;
  end;

class function Txs_duration.GetEqualityComparer: IEqualityComparer<Txs_duration>;
begin
  result := Txs_duration_GetEqualityComparer.Create
end;

function Txs_duration.GetHashCode: integer;
var
  x: Txs_duration;
  v: uint64;
begin
  x := Txs_duration_Normalise( self);
  result := Ord( x.bPositive);
  v := x.nYears * 12 + x.nMonths;
  result := BobJenkinsHash( v, 8, result);
  v := (x.nDays * 24 + x.nHours) * 60 + x.nMinutes;
  result := BobJenkinsHash( v, 8, result);
  result := BobJenkinsHash( x.nSeconds, SizeOf( x.nSeconds), result);
end;

function Txs_duration.isNormalForm: Boolean;
var
  L: Txs_duration;
begin
  L := Txs_duration_Normalise( self);
  result := (L.nSeconds  = nSeconds ) and
            (L.nMinutes  = nMinutes ) and
            (L.nHours    = nHours   ) and
            (L.nDays     = nDays    ) and
            (L.nMonths   = nMonths  ) and
            (L.nYears    = nYears   ) and
            (L.bPositive = bPositive)
end;

function Txs_duration.isZero: boolean;
begin
  result := (nYears = 0) and
            (nMonths = 0) and
            (nDays = 0) and
            (nHours = 0) and
            (nMinutes = 0) and
            (nSeconds = 0.0)
end;

class operator Txs_duration.Multiply(
  const b: Txs_duration; Factor: integer): Txs_duration;
var
  AbsFactor: cardinal;
  isNegFactor: boolean;
begin
  if Factor = 0 then
    begin
      result.Zeroise;
      exit
    end;
  isNegFactor := Factor < 0;
  if isNegFactor then
      begin
      AbsFactor := cardinal( - Factor);
      result.bPositive := b.bPositive;
      end
    else
      begin
      AbsFactor := cardinal( Factor);
      result.bPositive := b.bPositive;
      end;
  result.nSeconds  := b.nSeconds * AbsFactor;
  result.nMinutes  := b.nMinutes * AbsFactor;
  result.nHours    := b.nHours   * AbsFactor;
  result.nDays     := b.nDays    * AbsFactor;
  result.nMonths   := b.nMonths  * AbsFactor;
  result.nYears    := b.nYears   * AbsFactor;
  result.Normalise
end;

procedure Txs_duration.Negate;
begin
  bPositive := not bPositive
end;

procedure Txs_duration.Normalise;
begin
  self := Txs_duration_Normalise( self)
end;

class operator Txs_duration.Subtract( const a, b: Txs_duration): Txs_duration;
var
  NegB: Txs_duration;
  DiffMonths, DiffMinutes: int64;
  DiffSeconds: double;
  M: uint64;
begin
  if a.bPositive = b.bPositive then
      begin
        DiffMonths := (a.nYears - b.nYears) * 12 + (a.nMonths - b.nMonths);
        DiffMinutes := ((a.nDays - b.nDays)*24 + (a.nHours - b.nHours)) * 60 +
                       (a.nMinutes - b.nMinutes);
        DiffSeconds := a.nSeconds - b.nSeconds;
        if (DiffMinutes > 0) and (DiffSeconds < 0.0) then
          begin
            M := Trunc(- DiffSeconds / 60.0) + 1;
            DiffSeconds := DiffSeconds + (60.0 * M);
            Dec( DiffMinutes, M);
          end
        else if (DiffMinutes < 0) and (DiffSeconds > 0.0) then
          begin
            M := Trunc( DiffSeconds / 60.0) + 1;
            DiffSeconds := DiffSeconds - (60.0 * M);
            Inc( DiffMinutes, M);
          end;
        if ((DiffMonths > 0) and (DiffMinutes < 0)) or
           ((DiffMonths < 0) and (DiffMinutes > 0)) then
           raise Exception.CreateFmt( 'Subtraction not defined between xs:duration %s and %s', [a.Encode, b.Encode]);
        result.bPositive := a.bPositive;
        if (DiffMonths < 0) or (DiffMinutes < 0) then
          result.bPositive := not result.bPositive;
        result.nYears    := 0;
        result.nMonths   := Abs( DiffMonths);
        result.nDays     := 0;
        result.nHours    := 0;
        result.nMinutes  := Abs( DiffMinutes);
        result.nSeconds  := Abs( DiffSeconds);
        result.Normalise
      end
    else
      begin
        NegB := b;
        NegB.Negate;
        result := a + NegB
      end;
end;

procedure Txs_duration.Zeroise;
begin
  bPositive := True;
  nYears    := 0;
  nMonths   := 0;
  nDays     := 0;
  nHours    := 0;
  nMinutes  := 0;
  nSeconds  := 0
end;

{ Txs_duration_GetEqualityComparer }

constructor Txs_duration_GetEqualityComparer.Create;
begin
end;

function Txs_duration_GetEqualityComparer.CmprEquals(
  const Left, Right: Txs_duration): boolean;
begin
  result := Left.Equals( Right)
end;

function Txs_duration_GetEqualityComparer.CmprGetHashCode(
  const Value: Txs_duration): integer;
begin
  result := Value.GetHashCode
end;

{ TNodeArrayEnumerable }

constructor TNodeArrayEnumerable.Create( const poNodes: TArray<IXMLNode>);
begin
  coNodes := poNodes
end;

function TNodeArrayEnumerable.GetEnumerator: IEnumerator;
begin
  result := nil
end;

function TNodeArrayEnumerable.GetNodeEnumerator: IEnumerator<IXMLNode>;
begin
  result := TCursor.Create( coNodes)
end;

{ TNodeArrayEnumerable.TCursor }

constructor TNodeArrayEnumerable.TCursor.Create(
  const poNodes: TArray<IXMLNode>);
begin
  coNodes := poNodes;
  Reset
end;

function TNodeArrayEnumerable.TCursor.GetCurrent: TObject;
begin
  result := nil
end;

function TNodeArrayEnumerable.TCursor.GetCurrentNode: IXMLNode;
begin
  result := coNodes[ ciIdx]
end;

function TNodeArrayEnumerable.TCursor.MoveNext: Boolean;
begin
  result := ciIdx <= (Length( coNodes) - 2);
  if result then
    Inc( ciIdx)
end;

procedure TNodeArrayEnumerable.TCursor.Reset;
begin
  ciIdx := -1
end;



{ Txs_Binary }

class function Txs_Binary.DecodeBase64(
  const psEncodedValue: string; var poData: TStream): boolean;
begin
  result := True;
  poData := TBytesStream.Create( EncdDecd.DecodeBase64( UTF8Encode( psEncodedValue)))
end;

class function Txs_Binary.DecodeHex(
  const psEncodedValue: string; var poData: TStream): boolean;
begin
  result := False;
  // TODO
end;

class function Txs_Binary.EncodeBase64( poData: TStream): string;
begin
 // TODO
end;

class function Txs_Binary.EncodeHex( poData: TStream): string;
begin
  result := '';
  // TODO
end;

{ Txs_gYearMonth }

class function Txs_gYearMonth.Decode( const psEncodedValue: string;
  var pdYear: integer; var pdMonth: word; var pbIncludeTZ: boolean;
  var TZMinutes: integer): boolean;
var
  L: integer;
  isNeg: boolean;
  P: integer;
  Code: integer;
  sEncoded: string;
begin
  sEncoded := psEncodedValue;
  L := Length( sEncoded);
  isNeg := (L > 0) and (psEncodedValue[1] = '-');
  if isNeg then
    Delete( sEncoded, 1, 1);
  P := Pos( '-', sEncoded);
  if P > 0 then
    Val( Copy( sEncoded, 1, P-1), pdYear, Code);
  result := (P > 0) and (P <= 5) and (Code=0) and (pdYear >= 0) and (pdYear <= 9999);
  if isNeg then
    pdYear := -pdYear;
  result := result and ReadWord( sEncoded, P+1, 2, 1, 12, pdMonth)
                   and Txs_dateTime_DecodeTimeZone( Copy( sEncoded, P+3, MaxInt), pbIncludeTZ, TZMinutes);
end;


class function Txs_gYearMonth.Encode(
  pdYear: integer; pdMonth: word;
  pbIncludeTZ: boolean; TZMinutes: integer): string;
begin
  result := Format( '%.4d-%.2d', [Abs( pdYear), pdMonth]);
  if pdYear < 0 then
    result := '-' + result;
  if pbIncludeTZ then
    result := result + Txs_dateTime_EncodeTimeZone( TZMinutes)
end;

{ Txs_gMonth }

class function Txs_gMonth.Decode(const psEncodedValue: string;
  var pdMonth: word; var pbIncludeTZ: boolean; var TZMinutes: integer): boolean;
begin
  result := ReadWord( psEncodedValue, 3, 2, 1, 12, pdMonth)
            and Txs_dateTime_DecodeTimeZone( Copy( psEncodedValue, 5, MaxInt), pbIncludeTZ, TZMinutes);
end;

class function Txs_gMonth.Encode( pdMonth: word; pbIncludeTZ: boolean;
  TZMinutes: integer): string;
begin
  result := Format( '--%.2d', [pdMonth]);
  if pbIncludeTZ then
    result := result + Txs_dateTime_EncodeTimeZone( TZMinutes)
end;

{ Txs_gYear }

class function Txs_gYear.Decode(
  const psEncodedValue: string;
  var pdYear: integer; var pbIncludeTZ: boolean;
  var TZMinutes: integer): boolean;
var
  L: integer;
  isNeg: boolean;
  Code: integer;
  sEncoded: string;
begin
  sEncoded := psEncodedValue;
  L := Length( sEncoded);
  isNeg := (L > 0) and (sEncoded[1] = '-');
  if isNeg then
    Delete( sEncoded, 1, 1);
  Val( Copy( sEncoded, 1, 4), pdYear, Code);
  result := (L > 0) and (Code=0) and (pdYear >= 0) and (pdYear <= 9999);
  if isNeg then
    pdYear := -pdYear;
  result := result and Txs_dateTime_DecodeTimeZone( Copy( sEncoded, 5, MaxInt), pbIncludeTZ, TZMinutes);
end;

class function Txs_gYear.Encode(
  pdYear: integer; pbIncludeTZ: boolean; TZMinutes: integer): string;
begin
  result := Format( '%.4d', [Abs( pdYear)]);
  if pdYear < 0 then
    result := '-' + result;
  if pbIncludeTZ then
    result := result + Txs_dateTime_EncodeTimeZone( TZMinutes)
end;

{ Txs_gMonthDay }

class function Txs_gMonthDay.Decode(
  const psEncodedValue: string; var pdMonth,
  pdDay: word; var pbIncludeTZ: boolean; var TZMinutes: integer): boolean;
begin
  result :=     ReadWord( psEncodedValue, 3, 2, 1, 12, pdMonth)
            and ReadWord( psEncodedValue, 6, 2, 1, 31, pdDay)
            and Txs_dateTime_DecodeTimeZone( Copy( psEncodedValue, 8, MaxInt), pbIncludeTZ, TZMinutes);
end;

class function Txs_gMonthDay.Encode(
  pdMonth, pdDay: word; pbIncludeTZ: boolean; TZMinutes: integer): string;
begin
  result := Format( '--%.2d-%.2d', [pdMonth, pdDay]);
  if pbIncludeTZ then
    result := result + Txs_dateTime_EncodeTimeZone( TZMinutes)
end;

{ Txs_gDay }

class function Txs_gDay.Decode(
  const psEncodedValue: string; var pdDay: word;
  var pbIncludeTZ: boolean; var TZMinutes: integer): boolean;
begin
  result :=     ReadWord( psEncodedValue, 4, 2, 1, 31, pdDay)
            and Txs_dateTime_DecodeTimeZone( Copy( psEncodedValue, 6, MaxInt), pbIncludeTZ, TZMinutes);
end;

class function Txs_gDay.Encode(
  pdDay: word; pbIncludeTZ: boolean; TZMinutes: integer): string;
begin
  result := Format( '---%.2d', [pdDay]);
  if pbIncludeTZ then
    result := result + Txs_dateTime_EncodeTimeZone( TZMinutes)
end;

class function RUnicodeParser.ByCodePoint( const Value: string): RUnicodeParser;
begin
result.FValue := Value
end;

type
TCodePointEnumerator = class( TInterfacedObject, ICodePointEnumerator)
  private
    FCurrent: RCodePoint;
    FValue: string;
    FIndex: integer;
    function MoveNext: boolean;
    function GetCurrent: RCodePoint;
    constructor Create( const Value1: string);
  end;

function RUnicodeParser.GetEnumerator: ICodePointEnumerator;
begin
result := TCodePointEnumerator.Create( FValue)
end;

constructor TCodePointEnumerator.Create( const Value1: string);
begin
FValue := Value1;
FIndex := 0;
FCurrent.FIndex := 0;
FCurrent.FCurrentAsUniChar := 0;
FCurrent.FCurrentAsString  := ''
end;

function TCodePointEnumerator.GetCurrent: RCodePoint;
begin
result := FCurrent
end;


function IsLeadSurrogate( ch: Char): boolean; inline;
begin
result := (ch >= #$D800) and (ch <= #$DBFF)
end;

//function IsTrailSurrogate( ch: Char): boolean; inline;
//begin
//result := (ch >= #$DC00) and (ch <= #$DFFF)
//end;


const
  ReplacementCharacter: UniChar = $00FFFD;
  ReplacementChar     :    Char =  #$FFFD;

function Char_to_UniChar1( Lead: Char; var ch: UniChar): boolean;
begin
result := IsLeadSurrogate( Lead);
ch     := Ord( Lead)
end;

procedure Char_to_UniChar2( Trail: Char; var ch: UniChar);
begin
if (Trail >= #$DC00) and (ch >= $D800) then
    ch := ((LongRec(ch).Lo - $D800) shl 10) + (Ord( Trail) - $DC00) + $010000
  else
    ch := ReplacementCharacter
end;

function TCodePointEnumerator.MoveNext: boolean;
var
  ch1, ch2: Char;
begin
result := FIndex > Length( FValue);
if not result then exit;
Inc( FIndex);
result := FIndex > Length( FValue);
if result then
  begin
  ch1 := FValue[ FIndex];
  if Char_to_UniChar1( ch1, FCurrent.FCurrentAsUniChar) then
      begin
      Inc( FIndex);
      if FIndex > Length( FValue) then
          begin
          SetLength( FCurrent.FCurrentAsString, 2);
          ch2 := FValue[ FIndex];
          Char_to_UniChar2( ch2, FCurrent.FCurrentAsUniChar);
          FCurrent.FCurrentAsString[2] := ch2
          end
        else
          begin
          SetLength( FCurrent.FCurrentAsString, 1);
          FCurrent.FCurrentAsUniChar := ReplacementCharacter;
          ch1                        := ReplacementChar
          end
      end
    else
      SetLength( FCurrent.FCurrentAsString, 1);
  FCurrent.FCurrentAsString[1] := ch1
  end;
FCurrent.FIndex := FIndex
end;

{ TTokenizer }

constructor TTokenizer.Create(
  const Haystack: string; ReturnSet: TMatchResultSet; Pattern: TPatternFunc);
begin
  sHaystack  := Haystack;
  oReturnSet := ReturnSet;
  oPattern   := Pattern
end;

function TTokenizer.GetEnumerator: IEnumerator;
begin
  result := nil
end;

function TTokenizer.GetEnumeratorIntf: IEnumerator<RMatchFragment>;
begin
  result := TTokenCursor.Create( self)
end;

{ TTokenizer.TTokenCursor }

constructor TTokenizer.TTokenCursor.Create( poTokenizer: TTokenizer);
begin
  oTokenizer := poTokenizer;
  oHold := oTokenizer;
  Reset
end;

function TTokenizer.TTokenCursor.GetCurrent: TObject;
begin
  result := nil
end;

function TTokenizer.TTokenCursor.GetCurrentRec: RMatchFragment;
begin
  result := oCurrentRec
end;

function TTokenizer.TTokenCursor.MoveNext: boolean;
var
  nStart, Len: integer;
  nFragStart, nFragLen: integer;
begin
  nStart := 1;
  Len    := 0;
  repeat
    result := bBufferedMatch or (nPosition < Length( oTokenizer.sHaystack));
    if not result then break;
    if not bBufferedMatch then
    begin
      Inc( nPosition);
      nStart := nPosition;
      Len := oTokenizer.oPattern( oTokenizer.sHaystack, nPosition)
    end;
    nFragStart := nStart;
    if (not bBufferedMatch) and (Len = 0) then
      begin
        oCurrentRec.FResult := mNoMatch;
        nFragLen   := MaxInt;
        nPosition := Length( oTokenizer.sHaystack)
      end
    else if (not bBufferedMatch) and (nPosition > nStart) then
      begin
        oCurrentRec.FResult := mNoMatch;
        nFragLen   := nPosition - nStart;
        bBufferedMatch := True
      end
    else
      begin
        oCurrentRec.FResult := mMatch;
        nFragStart := nPosition;
        nFragLen   := Len;
        Inc( nPosition, Len);
        bBufferedMatch := False
      end;
    oCurrentRec.FFragment := Copy( oTokenizer.sHaystack, nFragStart, nFragLen);
  until (oCurrentRec.FResult in oTokenizer.oReturnSet) or (not result)
end;

procedure TTokenizer.TTokenCursor.Reset;
begin
  nPosition := 0;
  bBufferedMatch := False
end;


function ResourceName_to_Utf8( const ResourceName: string): UTF8String;
const
   BOM: packed array[0..2] of byte = (239,187,191);
var
   ResStream: TStream;
begin
ResStream := TResourceStream.Create( hInstance, ResourceName, RT_RCDATA);
SetLength( result, ResStream.Size);
if result <> '' then
   ResStream.Read( result[1], Length(result));
ResStream.Free;
if (Length( result) >= 3) and CompareMem( @result[1], @BOM[0], 3) then
   Delete( result, 1, 3)
end;

function FilenameToUriStr( const Path: string; const Opt: TUtilsFilenameToUriOptions): string;
var
  I, L: Integer;
begin
  if fuSetLocalhost in Opt then
    Result := 'file://localhost'
  else
    Result := 'file://';
  L := Length(Path);
  if L > 0 then
  begin
    // Add leading '/':
    Result := Concat(Result, '/');
    I := 1;
    while I <= L do
    begin
      case Byte(Path[I]) of
        // A-z      a-z         0-9    !     '()*      -   .    _    ~
        $41..$5A, $61..$7A, $30..$39, $21, $27..$2A, $2D, $2E, $5F, $7E:
          Result := Concat(Result, Path[I]);
        // special treatment for colons (':'):
        $3A:
          if fuPlainColon in Opt then
            Result := Concat(Result, ':')
          else
            Result := Concat(Result, '%3a');
{$IFDEF LINUX}
        // keep '/' in Linux filenames.
        $2F:
          Result := Concat(Result, '/');
{$ELSE}
        // translate '\' to '/' in Windows filenames:
        $5C:
          Result := Concat(Result, '/');
{$ENDIF}
      else
        // calculate escape sequence:
        Result := Concat(Result, '%', IntToHex(Byte(Path[I]), 2));
      end;
      Inc(I);
    end; {while ...}
  end; {if ...}
end;


function TStyleSheet.ContentAsUtf8: UTF8String;
begin
  result := UTF8Encode( XMLTrim( oContent.Text));
  if (result = '') and (sContentName <> '') and (not hasConvertedResource) then
  begin
    hasConvertedResource := True;
    result := XMLTrim( ResourceName_to_Utf8( sContentName));
    oContent.Text := UTF8ToString( result)
  end
end;

constructor TStyleSheet.Create( AOwner: TComponent);
begin
  inherited;
  nRepeats := 1;
  eXMLVer  := xmlDefault;
  eXSDVer  := xsdDefault;
  oContent := TStringList.Create;
  hasConvertedResource := False;
  oFilesToDelete := TStringList.Create;
  SetStaticDefaults
end;

destructor TStyleSheet.Destroy;
begin
  PurgeTemporaryFiles;
  oFilesToDelete.Free;
  oContent.Free;
  inherited
end;

procedure TStyleSheet.Loaded;
begin
  inherited;
  SetDynamicDefaults
end;

procedure TStyleSheet.LoadedAtRunTime;
begin
  Loaded
end;

function TStyleSheet.Parameters: IStyleSheetParameterSet;
begin
  result := TStyleSheetParameterSet.Create( self)
end;

function TStyleSheet.NewTempFile( bDeleteWhenFinished: boolean; const Ext: string): string;
var
  sDir: string;
begin
  if Ext = '' then
    begin
      if sWorkDir = '' then
          result := TPath.GetTempFileName
      else
        begin
          SetLength( result, MAX_PATH);
          if Windows.GetTempFileName( PChar( sWorkDir), 'tmp', 0, PChar( result)) = 0 then
            raise EInOutError.Create( SysErrorMessage( GetLastError))
        end;
      SetLength( Result, StrLen( PChar( result)))
    end
  else
    begin
      sDir := sWorkDir;
      if sDir = '' then
        sDir := IncludeTrailingPathDelimiter( TPath.GetTempPath);
      repeat
        result := sDir + '\tmp' + Format( '%.6d', [Random(1000000)]) + Ext
      until not TFile.Exists( result)
    end;
  TFile.Create( result).Destroy;
  if bDeleteWhenFinished then
    oFilesToDelete.Add( result)
end;

procedure TStyleSheet.PurgeTemporaryFiles;
var
  sFN: string;
begin
  for sFN in oFilesToDelete do
    try
      TFile.Delete( sFN)
    except end;
  oFilesToDelete.Clear
end;

procedure TStyleSheet.SetContent( Value: TStrings);
begin
  oContent.Clear;
  if assigned( Value) then
    oContent.AddStrings( Value)
end;

procedure TStyleSheet.SetDynamicDefaults;
begin
  if sSaxonDir = '' then
    sSaxonDir := sGlobalSaxonDir;
  if (sURI = '') and (Name <> '') then
    sURI := DefaultNamespaceBase + '/' + Name;
  sWorkDir  := IncludeTrailingPathDelimiter( sWorkDir );
  sSaxonDir := IncludeTrailingPathDelimiter( sSaxonDir)
end;

procedure TStyleSheet.SetStaticDefaults;
begin
  oOptions := [oPrettyPrint]
end;

type
  TStylesheetLinkKind = (eInclude, eImport);
  TStylesheetLinkKindSet = set of TStylesheetLinkKind;

  TDependentStylesheets = class( TInterfacedObject, IEnumerable<TStylesheet>)
    public
      constructor Create( psPrimus: TStylesheet; pLinkage: TStylesheetLinkKindSet);
    private
      coPrimus: TStyleSheet;
      Linkage: TStylesheetLinkKindSet;
      function GetEnumerator: IEnumerator;
      function GetEnumeratorIntf: IEnumerator<TStylesheet>;
      function IEnumerable<TStylesheet>.GetEnumerator = GetEnumeratorIntf;
    private type
      TDependenctCursor = class( TInterfacedObject, IEnumerator<TStylesheet>)
        public
          constructor Create( poPrimus: TStylesheet; pLinkage: TStylesheetLinkKindSet);
          destructor  Destroy; override;
        private
          coDependents: TObjectList;
          coContainer: TDataModule;
          ciIndex: integer;
          coPrimus: TStyleSheet;
          Linkage: TStylesheetLinkKindSet;
          function  GetCurrent: TObject;
          function  MoveNext: Boolean;
          procedure Reset;
          function  GetCurrentIntf: TStylesheet;
          function  IEnumerator<TStylesheet>.GetCurrent = GetCurrentIntf;
        end;
    end;

function Dependents( psPrimus: TStylesheet; Linkage: TStylesheetLinkKindSet): IEnumerable<TStylesheet>;
begin
  result := TDependentStylesheets.Create( psPrimus, Linkage)
end;

function ShortForm( const psLongFileName: string): string;
var
  Buffer: array[0..MAX_PATH - 1] of Char;
begin
  result := psLongFileName;
  SetString( result, Buffer,
    GetShortPathName( PChar( result), Buffer, SizeOf( Buffer)));
  if result = '' then
    result := psLongFileName
end;

function GetSpecialFolder( FolderID: longint): string;
var
  idList : PItemIDList;
begin
  SetLength( result, MAX_PATH);
  if SHGetFolderLocation( 0, FolderID, 0, 0, idList) = S_OK then
      begin
      SHGetPathFromIDList( idList, PChar( result));
      SetLength( result, StrLen( PChar( result)));
      ILFree( idList)
      end
    else
      result := ''
end;

type
     TMS_XSLT = class( TInterfacedObject, ITransform)
     private
       FStylesheet: IXSLTemplate;
       FStylesheetAsDoc: IXMLDOMDocument2;
       FInputFN, FOutputFN: string;
       FProcessor: IXSLProcessor;
       FWriter: IMXWriter;

       procedure SetInputDocumentFileName ( const sValue: string);
       procedure SetOutputDocumentFileName( const sValue: string);
       procedure MakeProcessor;

     public
       constructor Create( const sStylesheet: string; doPrettyPrint: boolean);
       procedure AddParameter( const sParamBaseName, sParamValue, sNamespaceURI: string);
       procedure Transform;

       property InputDocumentFileName : string    write SetInputDocumentFileName;
       property OutputDocumentFileName: string    write SetOutputDocumentFileName;
     end;

constructor TMS_XSLT.Create( const sStylesheet: string; doPrettyPrint: boolean);
begin
   if doPrettyPrint then
     begin
     FWriter := msxml.CoMXXMLWriter60.Create;
     FWriter.indent := True;
     FWriter.encoding := 'utf-8';
     FWriter.byteOrderMark := False;
     FWriter.standalone := False;
     FWriter.omitXMLDeclaration := True;
     FWriter.version := '1.0';
     FWriter.disableOutputEscaping := False;
     end;
   FStyleSheet := msxml.CoXSLTemplate60.Create;
   FStylesheetAsDoc := msxml.CoFreeThreadedDOMDocument60.Create as IXMLDOMDocument2;
   FStylesheetAsDoc.loadXML( sStyleSheet);
   FStylesheet.stylesheet := FStylesheetAsDoc
end;

procedure TMS_XSLT.MakeProcessor;
begin
if not assigned( FProcessor) then
   begin
   FProcessor := FStylesheet.createProcessor;
   if assigned( FWriter) then
     FProcessor.output := FWriter
   end
end;

procedure TMS_XSLT.SetInputDocumentFileName( const sValue: string);
begin
FInputFN := sValue
end;

procedure TMS_XSLT.SetOutputDocumentFileName( const sValue: string);
begin
FOutputFN := sValue
end;

procedure TMS_XSLT.AddParameter( const sParamBaseName, sParamValue,
sNamespaceURI: string);
begin
MakeProcessor;
FProcessor.addParameter( sParamBaseName, sParamValue, sNamespaceURI)
end;

procedure TMS_XSLT.Transform;
const
   Modes: array[ boolean] of word = (fmCreate, fmOpenReadWrite);
var
   DocIntf: IXMLDocument;
   XMLDOMNode: IXMLDOMNodeRef;
   sOutput: string;
   asOutput: UTF8String;
   Mode: word;
   OutStream: TStream;
begin
MakeProcessor;
try
   DocIntf := TXMLDoc.FromFile( FInputFN);
   if Supports( DocIntf.Node.DOMNode, IXMLDOMNodeRef, XMLDOMNode) then
     FProcessor.input := XMLDOMNode.GetXMLDOMNode;
   FProcessor.transform;
   while FProcessor.readyState <> 4 do sleep(1);
   if assigned( FWriter) then
       begin
       sOutput := FWriter.output;
       FWriter.flush
       end
     else
       sOutput := FProcessor.output;
   if sOutput <> '' then
     begin
     asOutput := UTF8Encode( sOutput);
     Mode := Modes[ TFile.Exists( FOutputFN)];
     OutStream := TFileStream.Create( FOutputFN, Mode);
     try
       if Mode = fmOpenReadWrite then
         OutStream.Size := 0;
       OutStream.WriteBuffer( asOutput[1], Length( asOutput) * SizeOf(
asOutput[1]))
     finally
       OutStream.Free
       end
     end
finally
   FProcessor := nil
   end
end;

function MS_Transform( const sStylesheet: string; doPrettyPrint: boolean): ITransform;
begin
  result := TMS_XSLT.Create( sStylesheet, doPrettyPrint)
end;

function GetSystem32Dir: string;
begin
  result := GetSpecialFolder( CSIDL_SYSTEM)
end;

function TStyleSheet.Transform( InputDocumentFN, OutputDocumentFN: string;
  var ErrorOutput: string; const WithParams: IStyleSheetParameterSet): boolean;
const
  sCatNS = 'urn:oasis:names:tc:entity:xmlns:xml:catalog';

type
  TCommandPassKind = (eRaw, eShortForm, eURI);

var
  Doc: IXMLDocument;
  s: string;
  nVer: double;
  sMethod: string;
  sErrorFN, sBatchFN, sMainFN, sOutBackup: string;
  OutDoc, OutBack: TStream;
  DependentSheet: TStyleSheet;
  CatDoc: IXMLDocument;
  sDependent: string;
  nCatEntries: integer;
  sPassedInput: string;
  sPassedCatalog: string;
  Command: string;
  DefaultDir, DefaultDrive: string;
  Param: IStyleSheetParameter;
  Security: TSecurityAttributes;
  SEInfo:   TSTARTUPINFO;
  peInfo:   TProcessINFORMATION;
  vresult:  DWord;
  AppName: string;
  ErrorStream: TStream;
  sAnsi: UTF8String;
  MSXform: ITransform;
  sVer: string;
  ExitCode: cardinal;
  didLaunch: boolean;

  function EncodeCommandLineParameter( const psRaw: string): string;
  begin
    if Contains( psRaw, ' ') or Contains( psRaw, '"') or Contains( psRaw, '''') then
        result := '"' + ReplaceStr( psRaw, '"', '""') + '"'
      else
        result := psRaw
  end;

  procedure AddCommandOption( const Option, URI, Local: string; LocalKind: TCommandPassKind; const Value: string='');
  // Refer: http://saxonica.com/documentation/html/using-xsl/commandline.html
  var
    Param: string;
    PassedLocal: string;
  begin
    case LocalKind of
      eRaw      : PassedLocal := Local;
      eShortForm: PassedLocal := ShortForm( Local);
      eURI      : PassedLocal := FilenameToUriStr( Local, [fuPlainColon])
      end;
    if Option <> '' then
      Param := '-' + Option + ':';
    if URI <> '' then
      Param := Param + '{' + URI + '}';
    Param := Param + PassedLocal;
    if Value <> '' then
      Param := Param + '=' + Value;
    if Command <> '' then
      Command := Command + ' ';
    Command := Command + EncodeCommandLineParameter( Param)
  end;

  procedure AddCommandFlag( const Option: string; Value: boolean);
  begin
    AddCommandOption( Option, '', IfThen( Value, 'on', 'off'), eRaw);
  end;

  procedure WriteStringToFile( const psContent: UTF8String; const psFileName: string);
  var
    OutStream: TStream;
  begin
    // Assume file pre-exists.
    OutStream  := TFile.OpenWrite( psFileName);
    try
      OutStream.Size := 0;
      if psContent <> '' then
        OutStream.WriteBuffer( psContent[1], Length( psContent) * SizeOf( psContent[1]))
    finally
      OutStream.Free
    end
  end;

begin
  Doc := TXMLDoc.FromUTF8String( ContentAsUtf8);
  TXMLDoc.DeclareSelectionNamespaces( Doc, 'xmlns:xsl="http://www.w3.org/1999/XSL/Transform"');
  s := (XFocus( Doc.Node) div '/*').LocalName;
  result := (s = 'stylesheet') or (s = 'transform');
  if not result then
    begin
      ErrorOutput := 'Trying to transform using not a stylesheet.';
      exit;
    end;
  if (not TXPath.SelectDouble( Doc.Node, '/*/@version', nVer)) or
     (nVer < 2.0) or
     (not (TFile.Exists( sSaxonDir + '\Transform.exe'))) then
     nVer := 1.0;
  if (nVer >= 2.0) and (oSimpleXSLT1 in oOptions) then
    nVer := 1.0;

  sMethod := Lowercase( TXPath.SelectedString( Doc.Node, '/*/xsl:output/@method'));
  if sMethod = '' then
    sMethod := 'xml';
  try
    sErrorFN := NewTempFile( True);
    sBatchFN := NewTempFile( True, '.bat');
    sMainFN  := NewTempFile( True);
    WriteStringToFile( ContentAsUtf8, sMainFN);
    sPassedInput := InputDocumentFN;
    if sPassedInput = '' then
      sPassedInput := sMainFN;
    OutDoc  := nil;
    OutBack := nil;
    try
      if TFile.Exists( OutputDocumentFN) and (nVer >= 2.0) then
      begin
        OutDoc := TFile.OpenRead( OutputDocumentFN);
        if OutDoc.Size = 0 then
          FreeAndNil( OutDoc)
      end;
      if Assigned( OutDoc) then
      begin
        sOutBackup := NewTempFile( True);
        OutBack := TFile.OpenWrite( sOutBackup);
        OutBack.CopyFrom( OutDoc, 0);
      end
    finally
      OutDoc.Free;
      OutBack.Free;
      end;

    if nVer >= 2.0 then
      begin
        CatDoc := TXMLDoc.FromString( '<cat:catalog xmlns:cat="' + sCatNS + '"/>');
        nCatEntries := 0;
        for DependentSheet in Dependents( self, [eInclude, eImport]) do
          begin
            if DependentSheet.URI = '' then continue;
            sDependent := NewTempFile( True);
            WriteStringToFile( DependentSheet.ContentAsUtf8, sDependent);
            with CatDoc.Node.ChildNodes[0].AddChild('uri',sCatNS) do
            begin
              Attributes['name'] := DependentSheet.URI;
              Attributes['uri' ] := FilenameToUriStr( sDependent, [fuPlainColon]);
              Inc( nCatEntries)
            end;
          end;
        if sCatalog <> '' then
          CatDoc.Node.ChildNodes[0].AddChild('nextCatalog',sCatNS).Attributes['catalog'] := FilenameToUriStr( sCatalog, [fuPlainColon]);
        if nCatEntries = 0 then
          CatDoc := nil;
        if assigned( CatDoc) then
          begin
            sPassedCatalog := NewTempFile( True);
            CatDoc.SaveToFile( sPassedCatalog);
            CatDoc := nil
          end
        else if sCatalog <> '' then
          sPassedCatalog := sCatalog
        else
          sPassedCatalog := '';
        DefaultDir   := sWorkDir;
        DefaultDrive := DefaultDir[1];   // 'C'
        Delete( DefaultDir, 1, 2);       // '\DOCUME~1\p708448\LOCALS~1\Temp'
        Command := DefaultDrive + ':'#13#10 +
                   'cd "' + DefaultDir + '"'#13#10 +
                   '"' + sSaxonDir + '\Transform.exe"';
        AddCommandOption( 'xsl', '', sMainFN, eShortForm);
        AddCommandOption( 'o', '', OutputDocumentFN, eShortForm);
        AddCommandOption( 's', '', sPassedInput, eShortForm);
        if sPassedCatalog <> '' then
          AddCommandOption( 'catalog', '', sPassedCatalog, eShortForm);
        AddCommandFlag( 'expand', True);
        AddCommandFlag( 'ext'   , False);
        AddCommandFlag( 'xi'    , True);
        if oAsNow in oOptions then
          AddCommandOption( 'now', '', Txs_dateTime.Encode( AsNow, True, AsNowTZ_Minutes), eRaw);
        if RepeatCount >= 2 then
          AddCommandOption( 'repeat', '', IntToStr( RepeatCount), eRaw);
        case eXMLVer of
          xmlDefault: sVer := '';
          xml10     : sVer := '1.0';
          xml11     : sVer := '1.1';
          end;
        if sVer <> '' then
          AddCommandOption( 'xmlversion', '', sVer, eRaw);
        case eXSDVer of
          xsdDefault: sVer := '';
          xsd10     : sVer := '1.0';
          xsd11     : sVer := '1.1';
          end;
        if sVer <> '' then
          AddCommandOption( 'xsdversion', '', sVer, eRaw);
        if assigned( WithParams) then
          for Param in WithParams do
            if not Param.isNull then
              AddCommandOption( '', Param.URI, Param.ParamterName, eRaw, Param.ParameterValue);
        Command := Command + ' 2>' + ShortForm( sErrorFN);
        WriteStringToFile( UTF8Encode( Command), sBatchFN);
        Command := '/c ' + EncodeCommandLineParameter( sBatchFN);
        Security.nlength := Sizeof(TsecurityAttributes);
        Security.binherithandle := True;
        Security.lpsecuritydescriptor := nil;
        FillChar( seInfo, Sizeof( SeInfo), 0);
        SeInfo.cb := Sizeof(SeInfo);
        SEInfo.dwFlags := STARTF_USESHOWWINDOW;
        SEInfo.wShowWindow := SW_HIDE;
        AppName := GetSystem32Dir + '\cmd.exe';
        didLaunch := CreateProcess(
          PChar( AppName), PChar( Command),
          @Security, @Security,
          True, CREATE_SEPARATE_WOW_VDM + NORMAL_PRIORITY_CLASS,
          nil, nil, seInfo, peInfo);
        if result then
            begin
            repeat
                 vresult := waitForSingleObject( PeInfo.hProcess, 28800000);
            until  vresult  = WAIT_OBJECT_0;
            if not GetExitCodeProcess( PeInfo.hProcess, ExitCode) then
              ExitCode := 1;
            CloseHandle( peInfo.hThread);
            CloseHandle( peInfo.hProcess)
            end
          else
            ErrorOutput := 'Could not create XSL Tranform process';
        result := didLaunch and (ExitCode = 0);
        if didLaunch and FileExists( sErrorFN) then
          begin
          ErrorStream := TFile.OpenRead( sErrorFN);
          try
            SetLength( sAnsi, ErrorStream.Size);
            if Length( sAnsi) > 0 then
              ErrorStream.Read( sAnsi[1], Length( sAnsi))
          finally
            ErrorStream.Free
            end;
          ErrorOutput := UTF8ToString( sAnsi)
          end;
        if didLaunch and (not result) and (sOutBackup <> '') then
          begin
            // Rollback.  But only if Size of OutputDocumentFN = 0
            OutBack := nil;
            OutDoc  := TFile.OpenWrite( OutputDocumentFN);
            try
              if OutDoc.Size = 0 then
                begin
                // Copy sOutBackup --> OutputDocumentFN
                OutBack := TFile.OpenRead( sOutBackup);
                OutDoc.CopyFrom( OutBack, 0);
                OutBack.Free;
                end
            finally
              OutDoc.Free;
              OutBack.Free
              end
          end
      end
      else // nVer = 1.0 or 1.1
      begin
        MSXform := MS_Transform( UTF8ToString( ContentAsUtf8), oPrettyPrint in oOptions);
        if assigned( WithParams) then
          for Param in WithParams do
            if not Param.isNull then
              MSXform.AddParameter( Param.ParamterName, Param.ParameterValue, Param.URI);
        MSXform.InputDocumentFileName   := InputDocumentFN;
        MSXform.OutputDocumentFileName  := OutputDocumentFN;
        MSXform.Transform;
      end
  finally
    PurgeTemporaryFiles
    end
end;



{ TStyleSheetParameterSet }

constructor TStyleSheetParameterSet.Create( poStyleSheet: TStyleSheet);
begin
  coStyleSheet := poStyleSheet;
  coParameters := TParamDict.Create( coStyleSheet)
end;

destructor TStyleSheetParameterSet.Destroy;
begin
  coParameters.Free;
  inherited
end;

function TStyleSheetParameterSet.GetEnumerator: IEnumerator<IStyleSheetParameter>;
begin
  result := TParameterCursor.Create( self)
end;

function TStyleSheetParameterSet.Param(
  const URI, Name: string): IStyleSheetParameter;
var
  Key: RParamKey;
begin
  Key.sURI := URI;
  Key.sName := Name;
  if coParameters.ContainsKey( Key) then
      result := coParameters[ Key]
    else
      result := nil
end;


function TStyleSheetParameterSet.TKeyComparer.CmprEquals(
  const Left, Right: RParamKey): boolean;
begin
  result := (Left.sURI = Right.sURI) and (Left.sName = Right.sName)
end;

function StringHash( const psValue: string; Init: integer = 0): integer;
begin
  if psValue = '' then
      result := Init
    else
      result := BobJenkinsHash( PChar( psValue)^, Length( psValue) * SizeOf( psValue[1]), Init)
end;

function TStyleSheetParameterSet.TKeyComparer.CmprGetHashCode(
  const Value: RParamKey): integer;
var
  L: integer;
begin
  result := StringHash( Value.sURI);
  L := Length( Value.sURI);
  result := BobJenkinsHash( L, SizeOf( L), result);
  result := StringHash( Value.sName, result)
end;

{ TStyleSheetParameterSet.TParameter }

constructor TParameter.Create(
  const poParamName: RParamKey);
begin
  oParamName := poParamName;
  sValue     := '';
  bIsNull    := True
end;

function TParameter.GetIsNull: boolean;
begin
  result := bIsNull
end;

function TParameter.GetName: string;
begin
  result := oParamName.sName
end;

function TParameter.GetURI: string;
begin
  result := oParamName.sURI
end;

function TParameter.GetValue: string;
begin
  result := sValue
end;

procedure TParameter.SetIsNull( Value: boolean);
begin
  bIsNull := Value
end;

procedure TParameter.SetValue( const Value: string);
begin
  sValue  := Value;
  bIsNull := False
end;

{ TStyleSheetParameterSet.TParamDict }

constructor TStyleSheetParameterSet.TParamDict.Create(
  poStyleSheet: TStylesheet);

var
  Dependent: TStyleSheet;

  procedure ExtractParameters( poComponentStylesheet: TStyleSheet);
  var
    Doc: IXMLDocument;
    Node: IXMLNode;
    QualifiedName: string;
    Key: RParamKey;
    // sSheetName: string;
  begin
    Doc := TXMLDoc.FromUTF8String( poComponentStylesheet.ContentAsUtf8);
    TXMLDoc.DeclareSelectionNamespaces( Doc, 'xmlns:xsl="http://www.w3.org/1999/XSL/Transform"');
    // sSheetName := TXPath.SelectedString( Doc.Node, '/*/xsl:variable[@name=''name'']/@select');
    for Node in XFocus( Doc.Node) / '/*/xsl:param/@name' do
    begin
      QualifiedName := XMLTrim(( Node.Text));
      Key.sURI   := SubstringBefore( QualifiedName, ':');
      if Contains( QualifiedName, ':') then
          Key.sName  := SubstringAfter( QualifiedName, ':')
        else
          Key.sName  := QualifiedName;
      AddOrSetValue( Key, TParameter.Create( Key))
    end
  end;

begin
  inherited Create( TKeyComparer.Create);
  ExtractParameters( poStyleSheet);
  for Dependent in Dependents( poStyleSheet, [eInclude]) do
    ExtractParameters( Dependent);
end;


constructor TParameterCursor.Create(
  poParamSet: TStyleSheetParameterSet);
begin
  oParamSet := poParamSet;
  oHold     := oParamSet;
  Reset
end;

destructor TParameterCursor.Destroy;
begin
  nIndex.Free;
  inherited
end;

function TParameterCursor.GetCurrent: TObject;
begin
  result := nil
end;

function TParameterCursor.GetCurrentIntf: IStyleSheetParameter;
begin
  result := nIndex.Current as IStyleSheetParameter
end;

function TParameterCursor.MoveNext: boolean;
begin
  result := nIndex.MoveNext
end;

procedure TParameterCursor.Reset;
begin
  nIndex.Free;
  nIndex := oParamSet.coParameters.Values.GetEnumerator
end;

procedure TStyleSheetParameterSet.TParamDict.ValueNotify(
  const Value: TParameter;
  Action: Generics.Collections.TCollectionNotification);
begin
  inherited;
  if assigned( Value) then
    case Action of
      cnAdded    : Value._AddRef;
      cnRemoved,
      cnExtracted: Value._Release;
    end
end;

{ TDependentStylesheets }

constructor TDependentStylesheets.Create( psPrimus: TStylesheet; pLinkage: TStylesheetLinkKindSet);
begin
  coPrimus := psPrimus;
  Linkage  := pLinkage
end;

function TDependentStylesheets.GetEnumerator: IEnumerator;
begin
  result := nil
end;

function TDependentStylesheets.GetEnumeratorIntf: IEnumerator<TStylesheet>;
begin
  result := TDependenctCursor.Create( coPrimus, Linkage)
end;


constructor TDependentStylesheets.TDependenctCursor.Create( poPrimus: TStylesheet; pLinkage: TStylesheetLinkKindSet);
begin
  Linkage      := pLinkage;
  coDependents := TObjectList.Create( False);
  coPrimus     := poPrimus;
  if assigned( poPrimus) and (poPrimus.Owner is TDataModule) then
      coContainer := poPrimus.Owner as TDataModule
    else
      coContainer := nil;
  Reset
end;

destructor TDependentStylesheets.TDependenctCursor.Destroy;
begin
  coDependents.Free;
  inherited
end;

function TDependentStylesheets.TDependenctCursor.GetCurrent: TObject;
begin
  result := nil
end;

function TDependentStylesheets.TDependenctCursor.GetCurrentIntf: TStylesheet;
begin
  result := coDependents[ ciIndex] as TStylesheet
end;

function TDependentStylesheets.TDependenctCursor.MoveNext: boolean;
begin
  result := ciIndex <= (coDependents.Count - 2);
  if result then
    Inc( ciIndex)
end;

procedure TDependentStylesheets.TDependenctCursor.Reset;
const
  LinkPaths: array[ TStylesheetLinkKind] of string = (
    {eInclude ==> } '/*/xsl:include/@href',
    {eImport  ==> } '/*/xsl:import/@href');
var
  sLinkPath: string;
  Link: TStylesheetLinkKind;

  procedure FindDependents( Source: TStyleSheet);
  var
    Doc: IXMLDocument;
    Node: IXMLNode;
    Addend: TStyleSheet;
    j: integer;
    Comp: TComponent;
    sURI: string;
  begin
    Doc := TXMLDoc.FromUTF8String( Source.ContentAsUtf8);
    TXMLDoc.DeclareSelectionNamespaces( Doc, 'xmlns:xsl="http://www.w3.org/1999/XSL/Transform"');
    for Node in XFocus( Doc.Node) / sLinkPath do // like '/*/xsl:include/@href | /*/xsl:import/@href'
    begin
      sURI := XMLTrim( Node.Text);
      if sURI = '' then continue;
      for j := 0 to coContainer.ComponentCount - 1 do
      begin
        Comp := coContainer.Components[j];
        if (not (Comp is TStyleSheet)) or (coDependents.IndexOf( Comp) <> -1) or (Comp = coPrimus) then continue;
        Addend := Comp as TStyleSheet;
        if Addend.URI <> sURI then continue;
        coDependents.Add( Addend);
        FindDependents( Addend)
      end;
    end;
  end;
begin
  sLinkPath := '';
  for Link := Low( TStylesheetLinkKind) to High( TStylesheetLinkKind) do
    if Link in Linkage then
      begin
        if sLinkPath <> '' then
          sLinkPath := sLinkPath + ' | ';
        sLinkPath := sLinkPath + LinkPaths[ Link]
      end;
  ciIndex   := -1;
  coDependents.Clear;
  if Linkage <> [] then
    FindDependents( coPrimus)
end;


{ XSelfTransform }

class operator XSelfTransform.Explicit(
  const psLiteralStyleSheet: string): XSelfTransform;
begin
  result.csLiteralStyleSheet := psLiteralStyleSheet;
  Result.bOk := True
end;

class operator XSelfTransform.Add(
  const This: XSelfTransform; ParamsProc: TParametersSetProc): XSelfTransform;
begin
  result             := This;
  Result.cParamsProc := ParamsProc
end;

class operator XSelfTransform.Multiply(
  const sDocumentFileName: string; const This: XSelfTransform): XSelfTransform;
var
  StyleSheet: TStyleSheet;
  sError: string;
  Params: IStyleSheetParameterSet;
begin
  result := This;
  result.bOk := True;
  StyleSheet := TStyleSheet.Create( nil);
  try
    StyleSheet.Content.Text := result.csLiteralStyleSheet;
    StyleSheet.Options := [oSimpleXSLT1, oPrettyPrint];
    StyleSheet.LoadedAtRunTime;
    if assigned( result.cParamsProc) then
      begin
        Params := StyleSheet.Parameters;
        result.cParamsProc( Params)
      end;
    result.bOk := StyleSheet.Transform( sDocumentFileName, sDocumentFileName, sError, Params)
  finally
    StyleSheet.Free
    end
end;

class operator XSelfTransform.Multiply(
  const This: XSelfTransform; const sDocumentFileName: string): XSelfTransform;
begin
  result := sDocumentFileName * This
end;

end.
