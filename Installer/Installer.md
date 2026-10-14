# DUnitX IDE Expert Installer

`DUnitX-Setup.iss` is an [Inno Setup 6](https://jrsoftware.org/isinfo.php) project that automates the installation of [DUnitX](https://github.com/VSoftTechnologies/DUnitX/) into one or more versions of Delphi/RAD Studio.

The installer is based on the [manual steps described in the wiki](https://github.com/VSoftTechnologies/DUnitX/wiki/Wizard-Installation).

1. Detects installed Delphi versions, allows user selection
2. Copies the DUnitX source tree to the chosen installation directory
3. _(Optional)_ backup the registry settings for the selected Delphi version
4. Removes the old Embarcadero version of the DUnitX package if it exists
5. Removes `$(BDS)\source\DUnitX` from the IDE Browsing path
6. Builds the Expert BPL for the selected Delphi versions using the command-line compiler
7. Uses VCL and FMX packages to build debug and release versions of the units for all appropriate compilers
8. Sets the `$(DUNITX)` IDE environment variable
9. Updates the IDE's Search paths to the precompiled release units for each platform `$(DUNITX)\packages\$(Platform)\Release`
10. Updates the IDE's Debug DCU paths to the precompiled debug units for each platform `$(DUNITX)\packages\$(Platform)\Debug`
11. Adds the source to the IDE's Browsing path to the DUnitX source code `$(DUNITX)\source`
12. _(Optional)_ download and install TestInsight if it isn't already installed
13. _(Optional)_ launch the newest version of Delphi

---

## Requirements

- [Inno Setup 6.x](https://jrsoftware.org/isdl.php) (to compile the installer)
- One or more supported Delphi versions installed (but _not running_)
- No elevated privileges required — all registry writes go to `HKCU`

---

## Building the Installer EXE

Compile `DUnitX-Setup.iss` from the `Installer\` sub-folder:

```cmd
iscc DUnitX-Setup.iss
```

This produces `Installer\DUnitX-Setup.exe`.
The script references source files via relative paths (`..\..\Expert\`, `..\source\`, etc.) so it **must** be compiled from within the repository.

---

## Running the Installer

### Interactive (GUI)

Double-click `DUnitX-Setup.exe`. The wizard guides you through:

1. Accepting the license
2. Choosing an installation directory
3. Selecting which detected Delphi versions to install into
4. Installation (file copy + build + registry configuration)
5. Optional TestInsight download

### Unattended / Command-line

```cmd
DUnitX-Setup.exe [/DIR=<path>] [/VERSIONS=<spec>] [/SKIPTS] [/SKIPBACKUP] [/SILENT | /VERYSILENT]
```

| Parameter          | Description                                                                    |
| ------------------ | ------------------------------------------------------------------------------ |
| `/DIR=<path>`      | Override the default installation directory.                                   |
| `/VERSIONS=<spec>` | Non-interactive version selection (see below). Skips the selection page.       |
| `/SKIPTS`          | Skip the TestInsight check entirely.                                           |
| `/SKIPBACKUP`      | Skip the per-version registry backup.                                          |
| `/SILENT`          | Run with a progress window but no wizard pages; selects all detected versions. |
| `/VERYSILENT`      | Run completely silently; selects all detected versions.                        |

### `/VERSIONS` values

| Value   | Meaning                                                                      |
| ------- | ---------------------------------------------------------------------------- |
| `all`   | Install into every detected Delphi version.                                  |
| `13`    | Install into the version whose name contains `13` (e.g. Delphi 13 Florence). |
| `12,13` | Install into Delphi 12 Athens and Delphi 13 Florence.                        |
| `37.0`  | Match by registry version string exactly.                                    |

### Examples

```cmd
REM Interactive — wizard prompts for everything
DUnitX-Setup.exe

REM All detected versions, no prompts, no TestInsight
DUnitX-Setup.exe /VERSIONS=all /SKIPTS /VERYSILENT

REM Delphi 13 Florence only, interactive TestInsight prompt
DUnitX-Setup.exe /VERSIONS=13

REM Custom install location, Athens + Florence, skip TestInsight
DUnitX-Setup.exe /DIR=C:\Libraries\DUnitX /VERSIONS=12,13 /SKIPTS

REM Skip registry backup (faster CI installs)
DUnitX-Setup.exe /VERSIONS=all /SKIPBACKUP /VERYSILENT
```

---

## How It Works

### 1. Detect Installed Versions

On startup the installer scans `HKCU:\Software\Embarcadero\BDS\` for subkeys matching the known version table. For each subkey it:

- Reads the `RootDir` registry value
- Verifies that the directory exists on disk
- Skips with a warning if the version is unrecognised or the directory is missing

If no recognised Delphi installation is found, setup aborts immediately with an error message.

### 2. Version Selection

When running interactively, a wizard page lists every detected installation as a checked checkbox (all ticked by default). The user can untick versions to skip.

When `/VERSIONS=<spec>` is supplied or `/SILENT`/`/VERYSILENT` is used, this page is skipped and the selection is resolved programmatically.

### 3. File Installation

All necessary source files are extracted from the installer to the chosen `{app}` directory:

```text
{app}\
├── Expert\           ← .dproj/.dpk/.pas/.dfm/resources for every version
└── source\
    ├── *.pas / *.inc ← DUnitX core source
    └── Packages\
        ├── DUnitX_VLC.dproj   ← VCL runtime package
        └── DUnitX_FMX.dproj   ← FMX runtime package
```

### 4. Registry Backup (optional)

Before making any changes for a given Delphi version the installer exports its entire BDS registry key to a timestamped `.reg` file in `%TEMP%`. Pass `/SKIPBACKUP` to suppress this step.

```cmd
%TEMP%\DUnitX_BDS_23_0_20250101_120000.reg
```

To restore, double-click the `.reg` file or run:

```cmd
reg import %TEMP%\DUnitX_BDS_23_0_20250101_120000.reg
```

### 5. Remove Old Known Package Entry

The Embarcadero-shipped stub package is removed before registering the new one:

```text
HKCU\Software\Embarcadero\BDS\{ver}\Known Packages
  → delete value: $(BDS)\bin\DUnitXIDEExpert{OldSuffix}.bpl
```

### 6. Remove Legacy Browsing Path Entry

The old source path that Embarcadero's installer added is removed from every platform's Browsing Path:

```text
HKCU\Software\Embarcadero\BDS\{ver}\Library\{platform}\Browsing Path
  → remove: $(BDS)\source\DUnitX
```

### 7. Build the Expert BPL

For each selected version:

```text
{app}\Expert\{DprojFile}   →   msbuild /p:Config=Release /p:Platform=Win32
```

The build is driven by `rsvars.bat` (found at `{RootDir}\bin\rsvars.bat`), which sets up the Delphi build environment including `BDSCOMMONDIR`. The installer captures `BDSCOMMONDIR` and uses it to locate the built BPL:

```cmd
{BDSCOMMONDIR}\dcp\Win32\Release\{PackageName}.bpl
```

The new BPL path is then registered as a Known Package:

```text
HKCU\Software\Embarcadero\BDS\{ver}\Known Packages
  {full path to built .bpl}  =  "DUnitX - IDE Expert"
```

If the BPL is not found after a successful build, the version is skipped and the failure is logged.

### 8. Build VCL and FMX Runtime Packages

```text
{app}\source\Packages\DUnitX_VLC.dproj  →  Win32 + Win64  ×  Release + Debug
{app}\source\Packages\DUnitX_FMX.dproj  →  all enabled platforms  ×  Release + Debug
```

> **Note:** the VCL package filename in the repository is `DUnitX_VLC.dproj` (not `VCL`) — that is the actual name on disk.

FMX platforms attempted: `Win32`, `Win64`, `Win64x`, `Android`, `Android64`, `OSX64`, `OSXARM64`, `iOSDevice64`, `iOSSimARM64`, `Linux64`.

Build failures (e.g. missing mobile SDK) are reported as warnings and do not abort the overall installation.

### 9. Set the `DUNITX` Environment Variable

```text
HKCU\Software\Embarcadero\BDS\{ver}\Environment Variables
  DUNITX = {app}\source
```

This makes `$(DUNITX)` available as an IDE macro in all project and library path settings, resolving to the `source\` folder of the installed DUnitX tree.

### 10. Update Search Paths

For every platform subkey under `HKCU\Software\Embarcadero\BDS\{ver}\Library\{platform}\`:

```text
Search Path  +=  $(DUNITX)\Packages\$(Platform)\Release
```

This gives the IDE (and MSBuild) the compiled `.dcp` files for each target platform.

### 11. Update Debug DCU Paths

```text
Debug DCU Path  +=  $(DUNITX)\Packages\$(Platform)\Debug
```

Allows the debugger to step into DUnitX source code using the debug-configuration DCUs.

### 12. Update Browsing Paths

```text
Browsing Path  +=  $(DUNITX)
```

`$(DUNITX)` and `$(Platform)` are IDE macros written literally to the registry and expanded by the IDE at runtime. Each entry is only appended if not already present.

### 13. TestInsight Check (optional)

After all selected versions are processed the installer checks whether [TestInsight](https://bitbucket.org/sglienke/testinsight/wiki/Home) is registered in the `Experts` key for each successfully installed version:

```reg
HKCU\Software\Embarcadero\BDS\{ver}\Experts
```

If any version is missing a value whose name contains `TestInsight`, the installer:

1. Lists the affected versions
2. Prompts `[Yes / No]` to download and run the installer (interactive mode), or installs automatically (silent mode)
3. Downloads `TestInsightSetup.zip` from `https://files.spring4d.com/TestInsight/latest/`
4. Extracts it to `%TEMP%\TestInsightSetup\`
5. Runs `TestInsightSetup.exe` and waits for it to finish

Pass `/SKIPTS` to skip this step entirely.

### 14. Launch Delphi (optional)

After installation the Finish page shows a **"Launch Delphi after installation"** checkbox (ticked by default). If left ticked, clicking **Finish** launches `bds.exe` for the newest Delphi version that was successfully installed.

- The checkbox only appears when at least one `bds.exe` was found on disk.
- The IDE is launched without waiting for it to close (`nowait`).
- In `/SILENT` or `/VERYSILENT` mode the checkbox is suppressed (`skipifsilent`).

---

## Version Table

| Delphi name          | Reg ver | BPL suffix | Expert `.dproj`                         |
| -------------------- | ------- | ---------- | --------------------------------------- |
| Delphi 2010          | 7.0     | 210        | `DUnitX_IDE_Expert_2010.dproj`          |
| Delphi XE            | 8.0     | 220        | `DUnitX_IDE_Expert_XE.dproj`            |
| Delphi XE2           | 9.0     | 230        | `DUnitX_IDE_Expert_XE2.dproj`           |
| Delphi XE3           | 10.0    | 240        | `DUnitX_IDE_Expert_XE3.dproj`           |
| Delphi XE4           | 11.0    | 250        | `DUnitX_IDE_Expert_XE4.dproj`           |
| Delphi XE5           | 12.0    | 260        | `DUnitX_IDE_Expert_XE5.dproj`           |
| Delphi XE6           | 13.0    | 270        | `DUnitX_IDE_Expert_XE6.dproj`           |
| Delphi XE7           | 14.0    | 280        | `DUnitX_IDE_Expert_XE7.dproj`           |
| Delphi XE8           | 15.0    | 290        | `DUnitX_IDE_Expert_XE8.dproj`           |
| Delphi 10 Seattle    | 17.0    | 300        | `DUnitX_IDE_Expert_D10Seattle.dproj`    |
| Delphi 10.1 Berlin   | 18.0    | 310        | `DUnitX_IDE_Expert_D10Berlin.dproj`     |
| Delphi 10.2 Tokyo    | 19.0    | 320        | `DUnitX_IDE_Expert_D10Tokyo.dproj`      |
| Delphi 10.3 Rio      | 20.0    | 330        | `DUnitX_IDE_Expert_D10Rio.dproj`        |
| Delphi 10.4 Sydney   | 21.0    | 340        | `DUnitX_IDE_Expert_Sydney.dproj`        |
| Delphi 11 Alexandria | 22.0    | 350        | `DUnitX_IDE_Expert_D11Alexandria.dproj` |
| Delphi 12 Athens     | 23.0    | 360        | `DUnitX_IDE_Expert_D12Athens.dproj`     |
| Delphi 13 Florence   | 37.0    | 370        | `DUnitX_IDE_Expert_D13Florence.dproj`   |

Any detected registry version not in this table is skipped with a warning in the Inno Setup log.

---

## Verification

After running the installer:

1. Open **Registry Editor** and confirm:
   - `HKCU\...\Known Packages` — new value whose name is the full BPL path and whose data is `DUnitX - IDE Expert`
   - `HKCU\...\Environment Variables` — `DUNITX` = `{app}\source`
   - `HKCU\...\Library\Win32\Search Path` — contains `$(DUNITX)\Packages\$(Platform)\Release`
   - `HKCU\...\Library\Win32\Browsing Path` — contains `$(DUNITX)`, does **not** contain `$(BDS)\source\DUnitX`
   - `HKCU\...\Library\Win32\Debug DCU Path` — contains `$(DUNITX)\Packages\$(Platform)\Debug`
2. Launch RAD Studio and confirm the DUnitX wizard appears under **File > New**.

---

## Installed Folder Layout

```text
{app}\                           ← chosen installation directory
├── Expert\
│   ├── DUnitX_IDE_Expert_2010.dproj
│   │   ... (one .dproj/.dpk per supported version)
│   └── DUnitX_IDE_Expert_D13Florence.dproj
└── source\                      ← $(DUNITX) points here
    ├── DUnitX.TestFramework.pas
    ├── ...
    └── Packages\
        ├── DUnitX_VLC.dproj
        ├── DUnitX_FMX.dproj
        ├── Win32\
        │   ├── Release\         ← VCL/FMX DCUs/BPLs (Search Path)
        │   └── Debug\           ← VCL/FMX DCUs (Debug DCU Path)
        └── [other platforms]\
```

---

## License

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

https://www.apache.org/licenses/LICENSE-2.0
