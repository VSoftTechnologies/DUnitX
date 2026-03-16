# DUnitX IDE Expert Installer

`Install-DUnitX.ps1` automates the installation of [DUnitX](https://github.com/VSoftTechnologies/DUnitX/) into one or more versions of Delphi/RAD Studio. It replicates the manual steps described in the [wiki](https://github.com/VSoftTechnologies/DUnitX/wiki/Wizard-Installation): build the Expert BPL and the VCL/FMX runtime packages, register the Expert under Known Packages, set the `$(DUNITX)` IDE environment variable, and update the IDE's Library paths so projects can reference DUnitX packages directly.

## Requirements

- PowerShell 5.1 or later
- One or more supported Delphi versions installed (but _not running_)
- No elevated privileges required — all registry writes go to `HKCU`

## Usage

```powershell
powershell -ExecutionPolicy Bypass -File .\Install\Install-DUnitX.ps1 [-Versions <spec>] [-InstallTestInsight]
```

The script can be run from any working directory; it locates the `Expert\` and `source\` folders relative to its own location using `$PSScriptRoot`.

### Parameters

| Parameter             | Type   | Description                                                                                                                                                                                                                                                                                        |
| --------------------- | ------ | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `-Versions`           | String | Skip the interactive prompt and select versions non-interactively. Use `All` to install into every detected version, or supply one or more Delphi generation numbers separated by commas (e.g. `13` for Florence, `12,13` for Athens and Florence). When omitted the script prompts interactively. |
| `-InstallTestInsight` | Switch | Automatically download and run the TestInsight installer for any selected version that does not already have it registered, without prompting.                                                                                                                                                     |

### Examples

```powershell
# Interactive — prompts for version selection and TestInsight
powershell -ExecutionPolicy Bypass -File .\Install\Install-DUnitX.ps1

# Install into every detected Delphi version, no prompts
powershell -ExecutionPolicy Bypass -File .\Install\Install-DUnitX.ps1 -Versions All -InstallTestInsight

# Install into Delphi 13 Florence only
powershell -ExecutionPolicy Bypass -File .\Install\Install-DUnitX.ps1 -Versions 13
```

---

## How It Works

### 1. Detect Installed Versions

The script scans `HKCU:\Software\Embarcadero\BDS\` for subkeys. For each subkey it:

- Reads the `RootDir` registry value
- Verifies that the directory exists on disk
- Matches the version number against the known version table (see below)
- Skips with a warning if the version is unrecognised or the directory is missing

### 2. Interactive Selection

A numbered list of detected installs is displayed. The user enters:

- A comma-separated list of numbers (e.g. `1,3`)
- Or `all` to install into every detected version

Pressing **Enter** without typing anything defaults to `all`.

### 3. Registry Backup

Before making any changes for a given Delphi version, the script exports its entire BDS registry key to a timestamped `.reg` file in `%TEMP%`:

```cmd
%TEMP%\DUnitX_BDS_23_0_20250101_120000.reg
```

The backup path is printed to the console. To restore, double-click the `.reg` file or run:

```cmd
reg import %TEMP%\DUnitX_BDS_23_0_20250101_120000.reg
```

### 4. Build the Expert BPL

For each selected version:

```text
Expert\{DprojFile}   →   msbuild /p:Config=Release /p:Platform=Win32
```

The build is driven by `rsvars.bat` (found at `{RootDir}\bin\rsvars.bat`), which sets up the Delphi build environment including the `BDSCOMMONDIR` variable. The script captures those environment variables and uses `BDSCOMMONDIR` to locate the built BPL:

```cmd
{BDSCOMMONDIR}\dcp\Win32\Release\{PackageName}.bpl
```

### 5. Register the BPL (Known Packages)

```registry
HKCU\Software\Embarcadero\BDS\{ver}\Known Packages
```

| Value name                    | Value data            |
| ----------------------------- | --------------------- |
| Full path to the built `.bpl` | `DUnitX - IDE Expert` |

If found, it removes the existing BPL matching

```cmd
$(BDS)\bin\DUnitXIDEExpert{OldBplSuffix}.bpl
```

### 5b. Set the `DUNITX` Environment Variable

```registry
HKCU\Software\Embarcadero\BDS\{ver}\Environment Variables
  DUNITX = {repo}\source
```

This makes `$(DUNITX)` available as an IDE macro in all project and library path settings, resolving to the `source\` folder of the DUnitX repository.

### 5c. Build VCL Runtime Packages

```text
source\packages\DUnitX_VCL.dproj   →   Win32 + Win64  ×  Release + Debug
```

Output goes to `source\packages\{Platform}\{Config}\` as determined by the dproj. Build failures are reported as warnings and do not abort the overall installation.

If `DUnitX_VCL.dproj` is not found the step is skipped with a warning.

### 5d. Build FMX Runtime Packages

```text
source\packages\DUnitX_FMX.dproj   →   all enabled platforms  ×  Release + Debug
```

Platforms attempted: `Win32`, `Win64`, `Win64x`, `Android`, `Android64`, `OSX64`, `OSXARM64`, `iOSDevice64`, `iOSSimARM64`, `Linux64`.

Each platform/config combination is built independently. Failures (e.g. missing mobile SDK) are reported as warnings and skipped without aborting the remaining builds.

If `DUnitX_FMX.dproj` is not found the step is skipped with a warning.

### 6. Update Library Paths

The script enumerates every platform subkey under

```registry
HKCU\Software\Embarcadero\BDS\{ver}\Library\{platform}\
```

and updates three values per platform:

| Registry value   | Entry added                              | Notes                                                |
| ---------------- | ---------------------------------------- | ---------------------------------------------------- |
| `Search Path`    | `$(DUNITX)\packages\$(Platform)\Release` | Release DCPs for compilation                         |
| `Browsing Path`  | `$(DUNITX)`                              | Legacy `$(BDS)\source\DUnitX` entry is removed first |
| `Debug DCU Path` | `$(DUNITX)\packages\$(Platform)\Debug`   | Debug DCUs for stepping into DUnitX code             |

The `$(DUNITX)` and `$(Platform)` tokens are IDE macros written literally to the registry and expanded by the IDE at runtime. Each entry is only appended if it is not already present.

### 7. TestInsight Check

After all selected versions are processed, the script checks whether [TestInsight](https://bitbucket.org/sglienke/testinsight/wiki/Home) is registered in the `Experts` key for each successfully installed version:

```registry
HKCU\Software\Embarcadero\BDS\{ver}\Experts
```

If any value whose name contains `TestInsight` is absent, the script:

1. Lists the affected Delphi versions
2. Prompts `[Y/n]` to download and run the installer (defaults to **Y**)
3. Downloads `TestInsightSetup.zip` from `https://files.spring4d.com/TestInsight/latest/`
4. Extracts it to `%TEMP%\TestInsightSetup\`
5. Runs `TestInsightSetup.exe` and waits for it to finish

TestInsight integrates with DUnitX to show real-time test results inside the IDE.

---

## Version Table

List of Delphi versions checked

| Delphi name          | Reg ver | Compiler ver | BPL suffix | Expert `.dproj`                         |
| -------------------- | ------- | ------------ | ---------  | --------------------------------------- |
| Delphi 2010          | 7.0     | 21.0         | 210        | `DUnitX_IDE_Expert_2010.dproj`          |
| Delphi XE            | 8.0     | 22.0         | 220        | `DUnitX_IDE_Expert_XE.dproj`            |
| Delphi XE2           | 9.0     | 23.0         | 230        | `DUnitX_IDE_Expert_XE2.dproj`           |
| Delphi XE3           | 10.0    | 24.0         | 240        | `DUnitX_IDE_Expert_XE3.dproj`           |
| Delphi XE4           | 11.0    | 25.0         | 250        | `DUnitX_IDE_Expert_XE4.dproj`           |
| Delphi XE5           | 12.0    | 26.0         | 260        | `DUnitX_IDE_Expert_XE5.dproj`           |
| Delphi XE6           | 13.0    | 27.0         | 270        | `DUnitX_IDE_Expert_XE6.dproj`           |
| Delphi XE7           | 14.0    | 28.0         | 280        | `DUnitX_IDE_Expert_XE7.dproj`           |
| Delphi XE8           | 15.0    | 29.0         | 290        | `DUnitX_IDE_Expert_XE8.dproj`           |
| Delphi 10 Seattle    | 17.0    | 30.0         | 300        | `DUnitX_IDE_Expert_D10Seattle.dproj`    |
| Delphi 10.1 Berlin   | 18.0    | 31.0         | 310        | `DUnitX_IDE_Expert_D10Berlin.dproj`     |
| Delphi 10.2 Tokyo    | 19.0    | 32.0         | 320        | `DUnitX_IDE_Expert_D10Tokyo.dproj`      |
| Delphi 10.3 Rio      | 20.0    | 33.0         | 330        | `DUnitX_IDE_Expert_D10Rio.dproj`        |
| Delphi 10.4 Sydney   | 21.0    | 34.0         | 340        | `DUnitX_IDE_Expert_Sydney.dproj`        |
| Delphi 11 Alexandria | 22.0    | 35.0         | 350        | `DUnitX_IDE_Expert_D11Alexandria.dproj` |
| Delphi 12 Athens     | 23.0    | 36.0         | 360        | `DUnitX_IDE_Expert_D12Athens.dproj`     |
| Delphi 13 Florence   | 37.0    | 37.0         | 370        | `DUnitX_IDE_Expert_D13Florence.dproj`   |

Any detected registry version not in this table is skipped with a warning.

---

## Verification

After running the installer:

1. Open **Registry Editor** and check:
   - `HKCU\Software\Embarcadero\BDS\{ver}\Known Packages` — new value whose name is the full BPL path and whose data is `DUnitX - IDE Expert`
   - `HKCU\Software\Embarcadero\BDS\{ver}\Environment Variables` — `DUNITX` = `{repo}\source`
   - `HKCU\Software\Embarcadero\BDS\{ver}\Library\Win32\Search Path` — contains `$(DUNITX)\packages\$(Platform)\Release`
   - `HKCU\Software\Embarcadero\BDS\{ver}\Library\Win32\Browsing Path` — contains `$(DUNITX)` and does **not** contain `$(BDS)\source\DUnitX`
   - `HKCU\Software\Embarcadero\BDS\{ver}\Library\Win32\Debug DCU Path` — contains `$(DUNITX)\packages\$(Platform)\Debug`
2. Launch RAD Studio and confirm the DUnitX wizard appears under **File > New**.

## Folder Layout

```text
DUnitX\
├── Expert\
│   ├── DUnitX_IDE_Expert_2010.dproj
│   ├── DUnitX_IDE_Expert_XE.dproj
│   │   ... (one per supported version)
│   └── DUnitX_IDE_Expert_D13Florence.dproj
├── source\                   ← $(DUnitX) for Browsing Path and Environment Variables
│   └── packages\
│       ├── DUnitX_VCL.dproj
│       ├── DUnitX_FMX.dproj
│       ├── Win32\
│       │   ├── Release\     ← VCL/FMX DCUs for Search Path
│       │   └── Debug\       ← VCL/FMX DCUs for Debug DCU Path
│       └── [Other platforms]
│           ├── Release\
│           └── Debug\
└── Installer\
    ├── Install-DUnitX.ps1   ← the installer script
    └── Installer.md         ← this file
```

## License

```text
{***************************************************************************}
{                                                                           }
{           DUnitX Installer                                                }
{                                                                           }
{           Copyright © 2026 Jim McKeeth                                    }
{                                                                           }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}
```
