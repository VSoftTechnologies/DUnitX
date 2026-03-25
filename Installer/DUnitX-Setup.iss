; ***************************************************************************
;
;  DUnitX IDE Expert InnoSetup Installer
;
;  Copyright (c) 2026 Jim McKeeth
;  Licensed under the Apache License, Version 2.0
;
;  Compile with Inno Setup 6.x from the Installer\ sub-folder:
;    iscc DUnitX-Setup.iss
;
;  Command-line parameters accepted at run time:
;    /DIR=<path>        Override the default installation directory
;    /VERSIONS=<spec>   Non-interactive version selection:
;                         all        - every detected Delphi version
;                         12,13      - comma-separated generation numbers
;                                      (matched against registry version or
;                                       name substring, e.g. "13" matches
;                                       "Delphi 13 Florence")
;    /SKIPTS            Skip the TestInsight check entirely
;    /SKIPBACKUP        Skip the per-version registry backup
;    /PLATFORMS=<spec>  Non-interactive platform selection:
;                         all            - every platform (default)
;                         Win32,Win64    - comma-separated platform names
;                       Valid names: Win32 Win64 Win64x Android Android64
;                                    OSX64 OSXARM64 iOSDevice64
;                                    iOSSimARM64 Linux64
;
;  A log is always written to %TEMP%\Setup Log *.txt
;    /SILENT            Unattended install; selects all detected versions
;    /VERYSILENT        Same as /SILENT with no progress window
;
; ***************************************************************************

#define AppName      "DUnitX"
#define AppVersion   "v0.4.2"
#define AppPublisher "VSoftTechnologies"
#define AppURL       "https://github.com/VSoftTechnologies/DUnitX"

; ---------------------------------------------------------------------------
[Setup]
; ---------------------------------------------------------------------------
AppId={{B3F2A1D0-C4E5-4F67-89AB-CD1234567EF0}
AppName={#AppName}
AppVersion={#AppVersion}
AppPublisher={#AppPublisher}
AppPublisherURL={#AppURL}
AppSupportURL={#AppURL}/issues
AppUpdatesURL={#AppURL}/releases
DefaultDirName={autopf}\DUnitX
DisableProgramGroupPage=yes
LicenseFile=..\LICENSE.txt
OutputDir=.
OutputBaseFilename=DUnitX-Setup
Compression=lzma2
SolidCompression=yes
WizardStyle=modern
; No elevation needed - all registry writes go to HKCU
PrivilegesRequired=lowest
MinVersion=6.1sp1
; Write a log to %TEMP%\Setup Log *.txt on every run
SetupLogging=yes

; ---------------------------------------------------------------------------
[Files]
; ---------------------------------------------------------------------------

; Expert project files (one .dproj per supported Delphi version)
Source: "..\Expert\*.pas";   DestDir: "{app}\Expert"; Flags: ignoreversion
Source: "..\Expert\*.dfm";   DestDir: "{app}\Expert"; Flags: ignoreversion
Source: "..\Expert\*.dpk";   DestDir: "{app}\Expert"; Flags: ignoreversion
Source: "..\Expert\*.dproj"; DestDir: "{app}\Expert"; Flags: ignoreversion
Source: "..\Expert\*.res";   DestDir: "{app}\Expert"; Flags: ignoreversion
Source: "..\Expert\*.dres";  DestDir: "{app}\Expert"; Flags: ignoreversion
Source: "..\Expert\*.rc";    DestDir: "{app}\Expert"; Flags: ignoreversion
Source: "..\Expert\*.ico";   DestDir: "{app}\Expert"; Flags: ignoreversion

; DUnitX source files
Source: "..\source\*.pas";   DestDir: "{app}\source"; Flags: ignoreversion
Source: "..\source\*.inc";   DestDir: "{app}\source"; Flags: ignoreversion
Source: "..\source\*.fmx";   DestDir: "{app}\source"; Flags: ignoreversion
Source: "..\source\*.dfm";   DestDir: "{app}\source"; Flags: ignoreversion
Source: "..\source\*.vlb";   DestDir: "{app}\source"; Flags: ignoreversion

; Runtime package project files
; Note: the VCL package filename is DUnitX_VLC (not VCL) - that is the
; actual name in the repository.
Source: "..\source\Packages\DUnitX_VLC.dpk";    DestDir: "{app}\source\Packages"; Flags: ignoreversion
Source: "..\source\Packages\DUnitX_VLC.dproj";  DestDir: "{app}\source\Packages"; Flags: ignoreversion
Source: "..\source\Packages\DUnitX_VLC.res";    DestDir: "{app}\source\Packages"; Flags: ignoreversion
Source: "..\source\Packages\DUnitX_FMX.dpk";    DestDir: "{app}\source\Packages"; Flags: ignoreversion
Source: "..\source\Packages\DUnitX_FMX.dproj";  DestDir: "{app}\source\Packages"; Flags: ignoreversion
Source: "..\source\Packages\DUnitX_FMX.res";    DestDir: "{app}\source\Packages"; Flags: ignoreversion

; ---------------------------------------------------------------------------
[Run]
; ---------------------------------------------------------------------------

; Offer to launch the newest installed Delphi from the Finish page.
; GetBdsExe / ShouldOfferLaunch are defined in [Code] below.
; postinstall  - adds a labelled checkbox to the Finish page (ticked by default)
; nowait       - don't hold up Setup waiting for the IDE to close
; skipifsilent - suppressed in /SILENT and /VERYSILENT mode
Filename: "{code:GetBdsExe}"; Description: "Launch Delphi after installation"; Flags: postinstall nowait skipifsilent; Check: ShouldOfferLaunch

; ---------------------------------------------------------------------------
[Code]

// --------------------------------------------------------------------------
// Type declarations
// --------------------------------------------------------------------------

type
  TDelphiVersion = record
    RegVer:       String;   // registry subkey, e.g. '23.0'
    Name:         String;   // display name, e.g. 'Delphi 12 Athens'
    OldBplSuffix: String;   // legacy BPL suffix, e.g. '360'
    DprojFile:    String;   // Expert .dproj filename
    RootDir:      String;   // populated by DetectVersions
    Detected:     Boolean;
  end;

// --------------------------------------------------------------------------
// Global state
// --------------------------------------------------------------------------

var
  VersionTable:         array of TDelphiVersion;
  DetectedIndices:      array of Integer;   // indices into VersionTable
  VersionPage:          TWizardPage;
  VersionCheckList:     TNewCheckListBox;
  BackupCheckBox:       TNewCheckBox;
  PlatformPage:         TWizardPage;
  PlatformCheckList:    TNewCheckListBox;
  NewestInstalledBdsExe: String;  // set during ssPostInstall; used by [Run]

const
  BDS_KEY          = 'Software\Embarcadero\BDS';
  FMX_PLAT_COUNT   = 10;

// --------------------------------------------------------------------------
// String / list helpers
// --------------------------------------------------------------------------

// Remove one entry (exact, case-insensitive) from a ';'-separated list.
function SemiListRemove(const S, Entry: String): String;
var Src, Part: String;
    p: Integer;
begin
  Result := '';
  Src := S;
  while Src <> '' do begin
    p := Pos(';', Src);
    if p = 0 then begin Part := Src;               Src := ''; end
    else           begin Part := Copy(Src, 1, p-1); Src := Copy(Src, p+1, MaxInt); end;
    if not SameText(Part, Entry) then begin
      if Result <> '' then Result := Result + ';';
      Result := Result + Part;
    end;
  end;
end;

// Check whether an entry already exists in a ';'-separated list.
function SemiListContains(const S, Entry: String): Boolean;
var Src, Part: String;
    p: Integer;
begin
  Result := False;
  Src := S;
  while Src <> '' do begin
    p := Pos(';', Src);
    if p = 0 then begin Part := Src;               Src := ''; end
    else           begin Part := Copy(Src, 1, p-1); Src := Copy(Src, p+1, MaxInt); end;
    if SameText(Part, Entry) then begin Result := True; Exit; end;
  end;
end;

// --------------------------------------------------------------------------
// Registry helpers (HKCU only)
// --------------------------------------------------------------------------

procedure RegAddToSemiList(const SubKey, ValueName, ToAdd: String);
var Current, New_: String;
begin
  if not RegQueryStringValue(HKCU, SubKey, ValueName, Current) then Current := '';
  if SemiListContains(Current, ToAdd) then Exit;
  if Current = '' then New_ := ToAdd else New_ := Current + ';' + ToAdd;
  RegWriteStringValue(HKCU, SubKey, ValueName, New_);
end;

procedure RegRemoveFromSemiList(const SubKey, ValueName, ToRemove: String);
var Current, Cleaned: String;
begin
  if not RegQueryStringValue(HKCU, SubKey, ValueName, Current) then Exit;
  Cleaned := SemiListRemove(Current, ToRemove);
  if Cleaned <> Current then
    RegWriteStringValue(HKCU, SubKey, ValueName, Cleaned);
end;

// --------------------------------------------------------------------------
// Version table
// --------------------------------------------------------------------------

procedure BuildVersionTable;
var i: Integer;
begin
  SetArrayLength(VersionTable, 17);
  i := 0;
  VersionTable[i].RegVer:='7.0';  VersionTable[i].Name:='Delphi 2010';
    VersionTable[i].OldBplSuffix:='210'; VersionTable[i].DprojFile:='DUnitX_IDE_Expert_2010.dproj';         Inc(i);
  VersionTable[i].RegVer:='8.0';  VersionTable[i].Name:='Delphi XE';
    VersionTable[i].OldBplSuffix:='220'; VersionTable[i].DprojFile:='DUnitX_IDE_Expert_XE.dproj';           Inc(i);
  VersionTable[i].RegVer:='9.0';  VersionTable[i].Name:='Delphi XE2';
    VersionTable[i].OldBplSuffix:='230'; VersionTable[i].DprojFile:='DUnitX_IDE_Expert_XE2.dproj';          Inc(i);
  VersionTable[i].RegVer:='10.0'; VersionTable[i].Name:='Delphi XE3';
    VersionTable[i].OldBplSuffix:='240'; VersionTable[i].DprojFile:='DUnitX_IDE_Expert_XE3.dproj';          Inc(i);
  VersionTable[i].RegVer:='11.0'; VersionTable[i].Name:='Delphi XE4';
    VersionTable[i].OldBplSuffix:='250'; VersionTable[i].DprojFile:='DUnitX_IDE_Expert_XE4.dproj';          Inc(i);
  VersionTable[i].RegVer:='12.0'; VersionTable[i].Name:='Delphi XE5';
    VersionTable[i].OldBplSuffix:='260'; VersionTable[i].DprojFile:='DUnitX_IDE_Expert_XE5.dproj';          Inc(i);
  VersionTable[i].RegVer:='13.0'; VersionTable[i].Name:='Delphi XE6';
    VersionTable[i].OldBplSuffix:='270'; VersionTable[i].DprojFile:='DUnitX_IDE_Expert_XE6.dproj';          Inc(i);
  VersionTable[i].RegVer:='14.0'; VersionTable[i].Name:='Delphi XE7';
    VersionTable[i].OldBplSuffix:='280'; VersionTable[i].DprojFile:='DUnitX_IDE_Expert_XE7.dproj';          Inc(i);
  VersionTable[i].RegVer:='15.0'; VersionTable[i].Name:='Delphi XE8';
    VersionTable[i].OldBplSuffix:='290'; VersionTable[i].DprojFile:='DUnitX_IDE_Expert_XE8.dproj';          Inc(i);
  VersionTable[i].RegVer:='17.0'; VersionTable[i].Name:='Delphi 10 Seattle';
    VersionTable[i].OldBplSuffix:='300'; VersionTable[i].DprojFile:='DUnitX_IDE_Expert_D10Seattle.dproj';   Inc(i);
  VersionTable[i].RegVer:='18.0'; VersionTable[i].Name:='Delphi 10.1 Berlin';
    VersionTable[i].OldBplSuffix:='310'; VersionTable[i].DprojFile:='DUnitX_IDE_Expert_D10Berlin.dproj';    Inc(i);
  VersionTable[i].RegVer:='19.0'; VersionTable[i].Name:='Delphi 10.2 Tokyo';
    VersionTable[i].OldBplSuffix:='320'; VersionTable[i].DprojFile:='DUnitX_IDE_Expert_D10Tokyo.dproj';     Inc(i);
  VersionTable[i].RegVer:='20.0'; VersionTable[i].Name:='Delphi 10.3 Rio';
    VersionTable[i].OldBplSuffix:='330'; VersionTable[i].DprojFile:='DUnitX_IDE_Expert_D10Rio.dproj';       Inc(i);
  VersionTable[i].RegVer:='21.0'; VersionTable[i].Name:='Delphi 10.4 Sydney';
    VersionTable[i].OldBplSuffix:='340'; VersionTable[i].DprojFile:='DUnitX_IDE_Expert_Sydney.dproj';       Inc(i);
  VersionTable[i].RegVer:='22.0'; VersionTable[i].Name:='Delphi 11 Alexandria';
    VersionTable[i].OldBplSuffix:='350'; VersionTable[i].DprojFile:='DUnitX_IDE_Expert_D11Alexandria.dproj';Inc(i);
  VersionTable[i].RegVer:='23.0'; VersionTable[i].Name:='Delphi 12 Athens';
    VersionTable[i].OldBplSuffix:='360'; VersionTable[i].DprojFile:='DUnitX_IDE_Expert_D12Athens.dproj';    Inc(i);
  VersionTable[i].RegVer:='37.0'; VersionTable[i].Name:='Delphi 13 Florence';
    VersionTable[i].OldBplSuffix:='370'; VersionTable[i].DprojFile:='DUnitX_IDE_Expert_D13Florence.dproj';
end;

function DetectVersions: Integer;
var i, n: Integer;
    RootDir: String;
begin
  BuildVersionTable;
  n := 0;
  SetArrayLength(DetectedIndices, 0);
  for i := 0 to GetArrayLength(VersionTable) - 1 do begin
    VersionTable[i].Detected := False;
    VersionTable[i].RootDir  := '';
    if RegQueryStringValue(HKCU, BDS_KEY + '\' + VersionTable[i].RegVer,
                           'RootDir', RootDir) then begin
      RootDir := RemoveBackslash(RootDir);
      if (RootDir <> '') and DirExists(RootDir) then begin
        VersionTable[i].Detected := True;
        VersionTable[i].RootDir  := RootDir;
        SetArrayLength(DetectedIndices, n + 1);
        DetectedIndices[n] := i;
        Inc(n);
      end;
    end;
  end;
  Result := n;
end;

// --------------------------------------------------------------------------
// Build helpers
// --------------------------------------------------------------------------

// Run rsvars.bat and capture the BDSCOMMONDIR environment variable.
function GetBDSCommonDir(const RsvarsBat: String;
                         var BDSCommonDir: String): Boolean;
var TempBat, TempOut, Line: String;
    Content: AnsiString;
    ResultCode, p: Integer;
begin
  Result      := False;
  TempBat     := ExpandConstant('{tmp}\dunitx_rsvars.bat');
  TempOut     := ExpandConstant('{tmp}\dunitx_rsvars.txt');

  SaveStringToFile(TempBat,
    '@echo off'                             + #13#10 +
    'call "' + RsvarsBat + '" >nul 2>&1'    + #13#10 +
    'echo BDSCOMMONDIR=%BDSCOMMONDIR%'       + #13#10,
    False);

  Exec(ExpandConstant('{cmd}'),
       '/c ""' + TempBat + '" > "' + TempOut + '" 2>&1"',
       '', SW_HIDE, ewWaitUntilTerminated, ResultCode);

  if LoadStringFromFile(TempOut, Content) then begin
    Line := String(Content);
    p := Pos('BDSCOMMONDIR=', Line);
    if p > 0 then begin
      Line := Copy(Line, p + Length('BDSCOMMONDIR='), MaxInt);
      p := Pos(#13, Line); if p > 0 then Line := Copy(Line, 1, p - 1);
      p := Pos(#10, Line); if p > 0 then Line := Copy(Line, 1, p - 1);
      BDSCommonDir := Trim(Line);
      Result := BDSCommonDir <> '';
    end;
  end;

  DeleteFile(TempBat);
  DeleteFile(TempOut);
end;

// Build a .dproj via msbuild, loading the Delphi environment from rsvars.bat.
// Returns True on success (msbuild exit code 0).
function BuildDproj(const RsvarsBat, DprojPath, Config, Plat: String): Boolean;
var TempBat: String;
    ResultCode: Integer;
begin
  TempBat := ExpandConstant('{tmp}\dunitx_build.bat');

  SaveStringToFile(TempBat,
    '@echo off'                                          + #13#10 +
    'call "' + RsvarsBat + '"'                           + #13#10 +
    'msbuild "' + DprojPath + '"'                        +
      ' /p:Config=' + Config + ' /p:Platform=' + Plat    +
      ' /nologo /v:minimal'                              + #13#10,
    False);

  Exec(ExpandConstant('{cmd}'), '/c "' + TempBat + '"',
       '', SW_HIDE, ewWaitUntilTerminated, ResultCode);

  DeleteFile(TempBat);
  Result := (ResultCode = 0);
end;

// Return the i-th FMX platform name (0-based).
function FmxPlatform(const i: Integer): String;
begin
  case i of
    0: Result := 'Win32';
    1: Result := 'Win64';
    2: Result := 'Win64x';
    3: Result := 'Android';
    4: Result := 'Android64';
    5: Result := 'OSX64';
    6: Result := 'OSXARM64';
    7: Result := 'iOSDevice64';
    8: Result := 'iOSSimARM64';
    9: Result := 'Linux64';
  else
    Result := '';
  end;
end;

// --------------------------------------------------------------------------
// Platform selection resolution
// --------------------------------------------------------------------------

function GetSelectedPlatforms: array of String;
var PlatformsParam: String;
    n, i, p: Integer;
    Token, Src: String;
begin
  SetArrayLength(Result, 0);
  n := 0;
  PlatformsParam := Trim(ExpandConstant('{param:PLATFORMS|}'));

  // Silent without /PLATFORMS, or /PLATFORMS=all → every platform
  if (WizardSilent and (PlatformsParam = '')) or SameText(PlatformsParam, 'all') then begin
    SetArrayLength(Result, FMX_PLAT_COUNT);
    for i := 0 to FMX_PLAT_COUNT - 1 do
      Result[i] := FmxPlatform(i);
    Exit;
  end;

  // /PLATFORMS=Win32,Win64 → parse comma-separated list
  if PlatformsParam <> '' then begin
    Src := PlatformsParam;
    while Src <> '' do begin
      p := Pos(',', Src);
      if p = 0 then begin Token := Trim(Src); Src := ''; end
      else           begin Token := Trim(Copy(Src, 1, p-1));
                           Src   := Trim(Copy(Src, p+1, MaxInt)); end;
      for i := 0 to FMX_PLAT_COUNT - 1 do
        if SameText(FmxPlatform(i), Token) then begin
          SetArrayLength(Result, n + 1);
          Result[n] := FmxPlatform(i);
          Inc(n);
          Break;
        end;
    end;
    Exit;
  end;

  // Interactive: read checklist
  if Assigned(PlatformCheckList) then
    for i := 0 to FMX_PLAT_COUNT - 1 do
      if PlatformCheckList.Checked[i] then begin
        SetArrayLength(Result, n + 1);
        Result[n] := FmxPlatform(i);
        Inc(n);
      end;
end;

// --------------------------------------------------------------------------
// Per-version installation
// --------------------------------------------------------------------------

function ShouldDoBackup: Boolean;
begin
  if WizardSilent or not Assigned(BackupCheckBox) then
    Result := Trim(ExpandConstant('{param:SKIPBACKUP|}')) = ''
  else
    Result := BackupCheckBox.Checked;
end;

procedure InstallForVersion(const VerIdx: Integer; const AppDir: String);
var
  Ver: TDelphiVersion;
  RsvarsBat, ExpertDproj, SourceDir, PkgDir: String;
  BDSCommonDir, BplName, BplPath: String;
  KnownPkgsKey, OldBplName: String;
  EnvVarsKey, LibKey: String;
  BackupFile: String;
  SubkeyNames: TArrayOfString;
  VclDproj, FmxDproj: String;
  Platforms: array of String;
  ResultCode, i: Integer;
begin
  Ver        := VersionTable[VerIdx];
  RsvarsBat  := AddBackslash(Ver.RootDir) + 'bin\rsvars.bat';
  ExpertDproj:= AddBackslash(AppDir) + 'Expert\' + Ver.DprojFile;
  SourceDir  := RemoveBackslash(AppDir) + '\source';
  PkgDir     := SourceDir + '\Packages';

  WizardForm.StatusLabel.Caption :=
    'Preparing ' + Ver.Name + '...';

  // ---- Registry backup --------------------------------------------------
  // Skipped when /SKIPBACKUP is passed on the command line or the checkbox
  // is unticked in the interactive wizard.
  if ShouldDoBackup then begin
    BackupFile := Ver.RegVer;
    StringChangeEx(BackupFile, '.', '_', False);
    BackupFile :=
      ExpandConstant('{tmp}') + '\DUnitX_BDS_' + BackupFile + '_' +
      GetDateTimeString('yyyymmdd_hhnnss', '-', ':') + '.reg';
    Exec(ExpandConstant('{sys}') + '\reg.exe',
         'export "HKCU\Software\Embarcadero\BDS\' + Ver.RegVer +
         '" "' + BackupFile + '" /y',
         '', SW_HIDE, ewWaitUntilTerminated, ResultCode);
    if ResultCode = 0 then
      Log(Ver.Name + ': registry backup saved to ' + BackupFile)
    else
      Log(Ver.Name + ': registry backup failed (continuing)');
  end else
    Log(Ver.Name + ': registry backup skipped');

  // ---- Validate prerequisites -------------------------------------------
  if not FileExists(ExpertDproj) then begin
    Log('SKIP ' + Ver.Name + ': .dproj not found: ' + ExpertDproj); Exit;
  end;
  if not FileExists(RsvarsBat) then begin
    Log('SKIP ' + Ver.Name + ': rsvars.bat not found: ' + RsvarsBat); Exit;
  end;

  // ---- Determine BDSCOMMONDIR -------------------------------------------
  if not GetBDSCommonDir(RsvarsBat, BDSCommonDir) then begin
    Log('SKIP ' + Ver.Name + ': could not read BDSCOMMONDIR from rsvars.bat');
    Exit;
  end;

  // ---- Build Expert BPL -------------------------------------------------
  WizardForm.StatusLabel.Caption :=
    'Building Expert BPL for ' + Ver.Name + '...';

  BplName := ChangeFileExt(Ver.DprojFile, '.bpl');
  BplPath := AddBackslash(BDSCommonDir) + 'dcp\Win32\Release\' + BplName;

  if not BuildDproj(RsvarsBat, ExpertDproj, 'Release', 'Win32') then
    Log('WARN ' + Ver.Name + ': Expert BPL msbuild returned non-zero');

  if not FileExists(BplPath) then begin
    Log('FAIL ' + Ver.Name + ': BPL not found at expected path: ' + BplPath);
    Exit;
  end;
  Log(Ver.Name + ': Expert BPL built: ' + BplPath);

  // ---- Register Known Package -------------------------------------------
  KnownPkgsKey := BDS_KEY + '\' + Ver.RegVer + '\Known Packages';
  OldBplName   := '$(BDS)\bin\DUnitXIDEExpert' + Ver.OldBplSuffix + '.bpl';
  RegDeleteValue(HKCU, KnownPkgsKey, OldBplName);
  RegDeleteValue(HKCU, KnownPkgsKey,
    '$(BDSBIN)\DUnitXIDEExpert' + Ver.OldBplSuffix + '.bpl');
  RegWriteStringValue(HKCU, KnownPkgsKey, BplPath, 'DUnitX - IDE Expert');
  Log(Ver.Name + ': Known Package registered');

  // ---- Set DUNITX IDE environment variable ------------------------------
  EnvVarsKey := BDS_KEY + '\' + Ver.RegVer + '\Environment Variables';
  RegWriteStringValue(HKCU, EnvVarsKey, 'DUNITX', SourceDir);
  Log(Ver.Name + ': DUNITX = ' + SourceDir);

  Platforms := GetSelectedPlatforms;

  // ---- Build VCL package ------------------------------------------------
  // The repository file is named DUnitX_VLC.dproj (not VCL — that is the
  // actual filename in the repo).  VCL only supports Win32 and Win64.
  VclDproj := PkgDir + '\DUnitX_VLC.dproj';
  if FileExists(VclDproj) then begin
    WizardForm.StatusLabel.Caption :=
      'Building DUnitX VCL packages for ' + Ver.Name + '...';
    for i := 0 to GetArrayLength(Platforms) - 1 do
      if SameText(Platforms[i], 'Win32') or SameText(Platforms[i], 'Win64') then begin
        if not BuildDproj(RsvarsBat, VclDproj, 'Release', Platforms[i]) then
          Log('WARN ' + Ver.Name + ': VCL ' + Platforms[i] + '/Release failed');
        if not BuildDproj(RsvarsBat, VclDproj, 'Debug',   Platforms[i]) then
          Log('WARN ' + Ver.Name + ': VCL ' + Platforms[i] + '/Debug failed');
      end;
  end else
    Log(Ver.Name + ': DUnitX_VLC.dproj not found, skipping VCL build');

  // ---- Build FMX packages -----------------------------------------------
  FmxDproj := PkgDir + '\DUnitX_FMX.dproj';
  if FileExists(FmxDproj) then begin
    for i := 0 to GetArrayLength(Platforms) - 1 do begin
      WizardForm.StatusLabel.Caption :=
        'Building DUnitX FMX/' + Platforms[i] + ' for ' + Ver.Name + '...';
      // Failures on non-Windows platforms are expected if SDKs are absent
      BuildDproj(RsvarsBat, FmxDproj, 'Release', Platforms[i]);
      BuildDproj(RsvarsBat, FmxDproj, 'Debug',   Platforms[i]);
    end;
  end else
    Log(Ver.Name + ': DUnitX_FMX.dproj not found, skipping FMX build');

  // ---- Update library paths for every platform subkey -------------------
  WizardForm.StatusLabel.Caption :=
    'Updating library paths for ' + Ver.Name + '...';

  LibKey := BDS_KEY + '\' + Ver.RegVer + '\Library';
  if RegGetSubkeyNames(HKCU, LibKey, SubkeyNames) then begin
    for i := 0 to GetArrayLength(SubkeyNames) - 1 do begin
      // Remove the legacy $(BDS)\source\DUnitX browsing path entry
      RegRemoveFromSemiList(LibKey + '\' + SubkeyNames[i],
                            'Browsing Path', '$(BDS)\source\DUnitX');
      RegAddToSemiList(LibKey + '\' + SubkeyNames[i],
                       'Search Path',    '$(DUNITX)\Packages\$(Platform)\Release');
      RegAddToSemiList(LibKey + '\' + SubkeyNames[i],
                       'Browsing Path',  '$(DUNITX)\Source');
      RegAddToSemiList(LibKey + '\' + SubkeyNames[i],
                       'Debug DCU Path', '$(DUNITX)\Packages\$(Platform)\Debug');
    end;
  end;

  Log(Ver.Name + ': installation complete');
end;

// --------------------------------------------------------------------------
// TestInsight helpers
// --------------------------------------------------------------------------

function HasTestInsight(const RegVer: String): Boolean;
var ExpertsKey: String;
    ValueNames: TArrayOfString;
    i: Integer;
begin
  Result     := False;
  ExpertsKey := BDS_KEY + '\' + RegVer + '\Experts';
  if RegGetValueNames(HKCU, ExpertsKey, ValueNames) then
    for i := 0 to GetArrayLength(ValueNames) - 1 do
      if Pos('TestInsight', ValueNames[i]) > 0 then begin
        Result := True; Exit;
      end;
end;

procedure DownloadAndRunTestInsight;
var ZipUrl, ZipPath, ExtractDir, SetupExe: String;
    ResultCode: Integer;
begin
  ZipUrl     := 'https://files.spring4d.com/TestInsight/latest/TestInsightSetup.zip';
  ZipPath    := ExpandConstant('{tmp}\TestInsightSetup.zip');
  ExtractDir := ExpandConstant('{tmp}\TestInsightSetup');

  WizardForm.StatusLabel.Caption := 'Downloading TestInsight...';

  // Use PowerShell to download (no extra plugin required)
  Exec('powershell.exe',
       '-NoProfile -NonInteractive -Command ' +
       '"Invoke-WebRequest -Uri ''' + ZipUrl +
       ''' -OutFile ''' + ZipPath + ''' -UseBasicParsing"',
       '', SW_HIDE, ewWaitUntilTerminated, ResultCode);

  if not FileExists(ZipPath) then begin
    MsgBox(
      'Could not download the TestInsight installer.' + #13#10 +
      'Please install TestInsight manually from:' + #13#10 +
      'https://bitbucket.org/sglienke/testinsight',
      mbInformation, MB_OK);
    Exit;
  end;

  WizardForm.StatusLabel.Caption := 'Extracting TestInsight...';

  if DirExists(ExtractDir) then
    DelTree(ExtractDir, True, True, True);

  Exec('powershell.exe',
       '-NoProfile -NonInteractive -Command ' +
       '"Expand-Archive -Path ''' + ZipPath +
       ''' -DestinationPath ''' + ExtractDir + ''' -Force"',
       '', SW_HIDE, ewWaitUntilTerminated, ResultCode);

  SetupExe := ExtractDir + '\TestInsightSetup.exe';
  if not FileExists(SetupExe) then begin
    MsgBox(
      'TestInsightSetup.exe was not found in the downloaded archive.' + #13#10 +
      'Please install TestInsight manually.',
      mbInformation, MB_OK);
    DeleteFile(ZipPath);
    Exit;
  end;

  WizardForm.StatusLabel.Caption := 'Running TestInsight installer...';
  Exec(SetupExe, '', '', SW_SHOWNORMAL, ewWaitUntilTerminated, ResultCode);
  Log('TestInsight installer exited with code: ' + IntToStr(ResultCode));
  DeleteFile(ZipPath);
end;

// --------------------------------------------------------------------------
// Version selection resolution
// --------------------------------------------------------------------------

// Returns an array of VersionTable indices to install, resolved from either
// the /VERSIONS command-line parameter, silent mode, or the wizard checklist.
function GetSelectedVersionIndices: array of Integer;
var VersionsParam: String;
    n, i, j, p: Integer;
    Token, Src: String;
begin
  SetArrayLength(Result, 0);
  n := 0;
  VersionsParam := Trim(ExpandConstant('{param:VERSIONS|}'));

  // /SILENT or /VERYSILENT without explicit /VERSIONS → select all detected
  if WizardSilent and (VersionsParam = '') then begin
    SetArrayLength(Result, GetArrayLength(DetectedIndices));
    for i := 0 to GetArrayLength(DetectedIndices) - 1 do
      Result[i] := DetectedIndices[i];
    Exit;
  end;

  // /VERSIONS=all → select all detected
  if SameText(VersionsParam, 'all') then begin
    SetArrayLength(Result, GetArrayLength(DetectedIndices));
    for i := 0 to GetArrayLength(DetectedIndices) - 1 do
      Result[i] := DetectedIndices[i];
    Exit;
  end;

  // /VERSIONS=12,13 → match each comma-separated token against name or regver
  if VersionsParam <> '' then begin
    Src := VersionsParam;
    while Src <> '' do begin
      p := Pos(',', Src);
      if p = 0 then begin Token := Trim(Src); Src := ''; end
      else           begin Token := Trim(Copy(Src, 1, p-1));
                           Src   := Trim(Copy(Src, p+1, MaxInt)); end;
      for i := 0 to GetArrayLength(DetectedIndices) - 1 do begin
        j := DetectedIndices[i];
        // Match registry version ('37.0') or name substring ('Florence', '13')
        if SameText(VersionTable[j].RegVer, Token) or
           (Pos(Token, VersionTable[j].Name) > 0) then begin
          SetArrayLength(Result, n + 1);
          Result[n] := j;
          Inc(n);
          Break;
        end;
      end;
    end;
    Exit;
  end;

  // Interactive mode: read the version selection checklist
  if Assigned(VersionCheckList) then
    for i := 0 to GetArrayLength(DetectedIndices) - 1 do
      if VersionCheckList.Checked[i] then begin
        SetArrayLength(Result, n + 1);
        Result[n] := DetectedIndices[i];
        Inc(n);
      end;
end;

// --------------------------------------------------------------------------
// [Run] helpers — launch newest Delphi on the Finish page
// --------------------------------------------------------------------------

function GetBdsExe(Param: String): String;
begin
  Result := NewestInstalledBdsExe;
end;

function ShouldOfferLaunch: Boolean;
begin
  Result := NewestInstalledBdsExe <> '';
end;

// --------------------------------------------------------------------------
// Wizard event handlers
// --------------------------------------------------------------------------

function InitializeSetup: Boolean;
begin
  Result := True;
  if DetectVersions = 0 then begin
    MsgBox(
      'No supported Delphi installation was found in the registry.' + #13#10 +
      'Install Delphi 2010 or later and try again.',
      mbError, MB_OK);
    Result := False;
  end;
end;

procedure InitializeWizard;
var i: Integer;
begin
  VersionPage := CreateCustomPage(
    wpSelectDir,
    'Select Delphi Versions',
    'Choose the Delphi versions to install DUnitX into.');

  VersionCheckList              := TNewCheckListBox.Create(VersionPage);
  VersionCheckList.Parent       := VersionPage.Surface;
  VersionCheckList.Left         := 0;
  VersionCheckList.Top          := 0;
  VersionCheckList.Width        := VersionPage.SurfaceWidth;
  // Leave room for the backup checkbox at the bottom of the page
  VersionCheckList.Height       := VersionPage.SurfaceHeight - ScaleY(32);

  for i := 0 to GetArrayLength(DetectedIndices) - 1 do
    VersionCheckList.AddCheckBox(
      VersionTable[DetectedIndices[i]].Name +
        '  (' + VersionTable[DetectedIndices[i]].RootDir + ')',
      '', 0, True, True, False, True, nil);

  BackupCheckBox         := TNewCheckBox.Create(VersionPage);
  BackupCheckBox.Parent  := VersionPage.Surface;
  BackupCheckBox.Left    := 0;
  BackupCheckBox.Top     := VersionCheckList.Top + VersionCheckList.Height + ScaleY(8);
  BackupCheckBox.Width   := VersionPage.SurfaceWidth;
  BackupCheckBox.Height  := ScaleY(17);
  BackupCheckBox.Caption := 'Back up registry settings before making changes';
  // Default: checked, unless /SKIPBACKUP was already passed on the command line
  BackupCheckBox.Checked := Trim(ExpandConstant('{param:SKIPBACKUP|}')) = '';

  // ---- Platform selection page ------------------------------------------
  PlatformPage := CreateCustomPage(
    VersionPage.ID,
    'Select Target Platforms',
    'Choose the platforms to compile DUnitX units for:');

  PlatformCheckList        := TNewCheckListBox.Create(PlatformPage);
  PlatformCheckList.Parent := PlatformPage.Surface;
  PlatformCheckList.Left   := 0;
  PlatformCheckList.Top    := 0;
  PlatformCheckList.Width  := PlatformPage.SurfaceWidth;
  PlatformCheckList.Height := PlatformPage.SurfaceHeight;

  for i := 0 to FMX_PLAT_COUNT - 1 do
    PlatformCheckList.AddCheckBox(FmxPlatform(i), '', 0, True, True, False, True, nil);
end;

// Skip custom pages when their CLI equivalent is supplied or in silent mode.
function ShouldSkipPage(PageID: Integer): Boolean;
begin
  Result := False;
  if PageID = VersionPage.ID then
    Result := WizardSilent or
              (Trim(ExpandConstant('{param:VERSIONS|}')) <> '');
  if PageID = PlatformPage.ID then
    Result := WizardSilent or
              (Trim(ExpandConstant('{param:PLATFORMS|}')) <> '');
end;

// Require at least one version to be ticked before proceeding.
function NextButtonClick(CurPageID: Integer): Boolean;
var i, n: Integer;
begin
  Result := True;
  if CurPageID = VersionPage.ID then begin
    n := 0;
    for i := 0 to GetArrayLength(DetectedIndices) - 1 do
      if VersionCheckList.Checked[i] then Inc(n);
    if n = 0 then begin
      MsgBox('Please select at least one Delphi version.', mbError, MB_OK);
      Result := False;
    end;
  end;
end;

// Main post-install step: build BPLs and configure the IDE registry.
procedure CurStepChanged(CurStep: TSetupStep);
var
  Selected: array of Integer;
  AppDir: String;
  i: Integer;
  SkipTS: Boolean;
  MissingTS: String;
  Ver: TDelphiVersion;
  BdsExe: String;
begin
  if CurStep <> ssPostInstall then Exit;

  AppDir   := WizardDirValue;
  Selected := GetSelectedVersionIndices;

  if GetArrayLength(Selected) = 0 then begin
    Log('DUnitX installer: no versions selected.');
    Exit;
  end;

  // --- Build and register each selected Delphi version ---
  for i := 0 to GetArrayLength(Selected) - 1 do
    InstallForVersion(Selected[i], AppDir);

  // --- Find newest installed bds.exe for the Finish-page launch checkbox ---
  NewestInstalledBdsExe := '';
  for i := GetArrayLength(Selected) - 1 downto 0 do begin
    BdsExe := AddBackslash(VersionTable[Selected[i]].RootDir) + 'bin\bds.exe';
    if FileExists(BdsExe) then begin
      NewestInstalledBdsExe := BdsExe;
      Break;
    end;
  end;

  // --- TestInsight check -------------------------------------------------
  // /SKIPTS suppresses the check entirely.
  SkipTS := Trim(ExpandConstant('{param:SKIPTS|}')) <> '';

  if not SkipTS then begin
    MissingTS := '';
    for i := 0 to GetArrayLength(Selected) - 1 do begin
      Ver := VersionTable[Selected[i]];
      if not HasTestInsight(Ver.RegVer) then begin
        if MissingTS <> '' then MissingTS := MissingTS + ', ';
        MissingTS := MissingTS + Ver.Name;
      end;
    end;

    if MissingTS <> '' then begin
      if WizardSilent then begin
        // Silent mode without /SKIPTS → install automatically
        DownloadAndRunTestInsight;
      end else begin
        if MsgBox(
          'TestInsight is not installed for: ' + MissingTS + '.' + #13#10#13#10 +
          'TestInsight displays real-time test results inside the IDE.' + #13#10#13#10 +
          'Download and run the TestInsight installer now?',
          mbConfirmation, MB_YESNO) = IDYES then
          DownloadAndRunTestInsight;
      end;
    end;
  end;

  WizardForm.StatusLabel.Caption := 'DUnitX installation complete.';
end;
