#***************************************************************************#
#                                                                           #
#           DUnitX Installer                                                #
#                                                                           #
#           Copyright © 2026 Jim McKeeth                                    #
#                                                                           #
#           Included with DUnitX - Delphi Unit Test Framework               #
#           https://github.com/VSoftTechnologies/DUnitX                     #
#                                                                           #
#***************************************************************************#
#                                                                           #
#  Licensed under the Apache License, Version 2.0 (the "License");          #
#  you may not use this file except in compliance with the License.         #
#  You may obtain a copy of the License at                                  #
#                                                                           #
#      http://www.apache.org/licenses/LICENSE-2.0                           #
#                                                                           #
#  Unless required by applicable law or agreed to in writing, software      #
#  distributed under the License is distributed on an "AS IS" BASIS,        #
#  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. #
#  See the License for the specific language governing permissions and      #
#  limitations under the License.                                           #
#                                                                           #
#***************************************************************************#

#Requires -Version 5.1
<#
.SYNOPSIS
    Installs the DUnitX IDE Expert wizard into one or more Delphi versions.

.DESCRIPTION
    Detects installed Delphi versions, lets the user choose which to install
    into, then builds and registers the Expert BPL for each selected version.

.PARAMETER Versions
    Non-interactive version selection. Use 'All' to install into every detected
    Delphi version, or provide one or more Delphi generation numbers separated
    by commas (e.g. '13' for Delphi 13 Florence, '12,13' for both).
    When omitted the script prompts interactively.

.PARAMETER InstallTestInsight
    When specified, automatically downloads and runs the TestInsight installer
    for any selected Delphi version that does not already have TestInsight
    registered, without prompting.

.NOTES
    Run from PowerShell (not the ISE prompt) in the repository root or from
    the Install sub-folder.  No elevated privileges required  - all registry
    writes go to HKCU.
#>
param(
    [string]$Versions,
    [switch]$InstallTestInsight
)

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

# ---------------------------------------------------------------------------
# Version table
# ---------------------------------------------------------------------------
$VersionTable = @(
    [PSCustomObject]@{ Version = '7.0';  Name = 'Delphi 2010';          CompilerVer = '21.0'; OldBplSuffix = '210'; DprojFile = 'DUnitX_IDE_Expert_2010.dproj' }
    [PSCustomObject]@{ Version = '8.0';  Name = 'Delphi XE';            CompilerVer = '22.0'; OldBplSuffix = '220'; DprojFile = 'DUnitX_IDE_Expert_XE.dproj' }
    [PSCustomObject]@{ Version = '9.0';  Name = 'Delphi XE2';           CompilerVer = '23.0'; OldBplSuffix = '230'; DprojFile = 'DUnitX_IDE_Expert_XE2.dproj' }
    [PSCustomObject]@{ Version = '10.0'; Name = 'Delphi XE3';           CompilerVer = '24.0'; OldBplSuffix = '240'; DprojFile = 'DUnitX_IDE_Expert_XE3.dproj' }
    [PSCustomObject]@{ Version = '11.0'; Name = 'Delphi XE4';           CompilerVer = '25.0'; OldBplSuffix = '250'; DprojFile = 'DUnitX_IDE_Expert_XE4.dproj' }
    [PSCustomObject]@{ Version = '12.0'; Name = 'Delphi XE5';           CompilerVer = '26.0'; OldBplSuffix = '260'; DprojFile = 'DUnitX_IDE_Expert_XE5.dproj' }
    [PSCustomObject]@{ Version = '13.0'; Name = 'Delphi XE6';           CompilerVer = '27.0'; OldBplSuffix = '270'; DprojFile = 'DUnitX_IDE_Expert_XE6.dproj' }
    [PSCustomObject]@{ Version = '14.0'; Name = 'Delphi XE7';           CompilerVer = '28.0'; OldBplSuffix = '280'; DprojFile = 'DUnitX_IDE_Expert_XE7.dproj' }
    [PSCustomObject]@{ Version = '15.0'; Name = 'Delphi XE8';           CompilerVer = '29.0'; OldBplSuffix = '290'; DprojFile = 'DUnitX_IDE_Expert_XE8.dproj' }
    [PSCustomObject]@{ Version = '17.0'; Name = 'Delphi 10 Seattle';    CompilerVer = '30.0'; OldBplSuffix = '300'; DprojFile = 'DUnitX_IDE_Expert_D10Seattle.dproj' }
    [PSCustomObject]@{ Version = '18.0'; Name = 'Delphi 10.1 Berlin';   CompilerVer = '31.0'; OldBplSuffix = '310'; DprojFile = 'DUnitX_IDE_Expert_D10Berlin.dproj' }
    [PSCustomObject]@{ Version = '19.0'; Name = 'Delphi 10.2 Tokyo';    CompilerVer = '32.0'; OldBplSuffix = '320'; DprojFile = 'DUnitX_IDE_Expert_D10Tokyo.dproj' }
    [PSCustomObject]@{ Version = '20.0'; Name = 'Delphi 10.3 Rio';      CompilerVer = '33.0'; OldBplSuffix = '330'; DprojFile = 'DUnitX_IDE_Expert_D10Rio.dproj' }
    [PSCustomObject]@{ Version = '21.0'; Name = 'Delphi 10.4 Sydney';   CompilerVer = '34.0'; OldBplSuffix = '340'; DprojFile = 'DUnitX_IDE_Expert_Sydney.dproj' }
    [PSCustomObject]@{ Version = '22.0'; Name = 'Delphi 11 Alexandria'; CompilerVer = '35.0'; OldBplSuffix = '350'; DprojFile = 'DUnitX_IDE_Expert_D11Alexandria.dproj' }
    [PSCustomObject]@{ Version = '23.0'; Name = 'Delphi 12 Athens';     CompilerVer = '36.0'; OldBplSuffix = '360'; DprojFile = 'DUnitX_IDE_Expert_D12Athens.dproj' }
    [PSCustomObject]@{ Version = '37.0'; Name = 'Delphi 13 Florence';   CompilerVer = '37.0'; OldBplSuffix = '370'; DprojFile = 'DUnitX_IDE_Expert_D13Florence.dproj' }
)

# Build a lookup by Version string for fast access
$VersionLookup = @{}
foreach ($entry in $VersionTable) {
    $VersionLookup[$entry.Version] = $entry
}

# ---------------------------------------------------------------------------
# Helper: write coloured status lines
# ---------------------------------------------------------------------------
function Write-Step   { param([string]$Msg) Write-Host "  --> $Msg" -ForegroundColor Cyan }
function Write-OK     { param([string]$Msg) Write-Host "  [OK] $Msg" -ForegroundColor Green }
function Write-Warn   { param([string]$Msg) Write-Host "  [WARN] $Msg" -ForegroundColor Yellow }
function Write-Fail   { param([string]$Msg) Write-Host "  [FAIL] $Msg" -ForegroundColor Red }

# ---------------------------------------------------------------------------
# Helper: append a value to a semicolon-separated registry string if absent
# ---------------------------------------------------------------------------
function Add-LibraryValue {
    param([string]$RegPath, [string]$ValueName, [string]$ToAdd, [string]$Label)
    $props   = Get-ItemProperty $RegPath -ErrorAction SilentlyContinue
    $hasProp = $props -and (Get-Member -InputObject $props -Name $ValueName -MemberType NoteProperty)
    $current = if ($hasProp) { $props.$ValueName } else { '' }
    if ($current -like "*$ToAdd*") {
        Write-OK "  ${Label}: '$ValueName' already up to date."
    } else {
        $newVal = if ([string]::IsNullOrEmpty($current)) { $ToAdd } else { "$current;$ToAdd" }
        Set-ItemProperty $RegPath $ValueName $newVal
        Write-OK "  ${Label}: '$ValueName' updated."
    }
}

# ---------------------------------------------------------------------------
# Helper: remove one entry from a semicolon-separated string
# ---------------------------------------------------------------------------
function Remove-LibraryEntry {
    param([string]$Current, [string]$Entry)
    ($Current -split ';' | Where-Object { $_ -ne $Entry }) -join ';'
}

# ---------------------------------------------------------------------------
# Helper: run a command via a temp batch file (avoids && parsing issues in PS5)
# Returns a hashtable with Output (string[]) and ExitCode (int)
# ---------------------------------------------------------------------------
function Invoke-BatchCommand {
    param(
        [string]$RsvarsBat,
        [string]$Command
    )
    $tempBat = [System.IO.Path]::ChangeExtension([System.IO.Path]::GetTempFileName(), '.bat')
    try {
        $lines = "@echo off", "call `"$RsvarsBat`"", $Command
        Set-Content $tempBat -Value ($lines -join "`r`n") -Encoding ASCII
        $output = & cmd /c $tempBat 2>&1
        return @{ Output = $output; ExitCode = $LASTEXITCODE }
    } finally {
        Remove-Item $tempBat -ErrorAction SilentlyContinue
    }
}

# ---------------------------------------------------------------------------
# Step 1: Detect installed Delphi versions
# ---------------------------------------------------------------------------
Write-Host ""
Write-Host "DUnitX IDE Expert Installer" -ForegroundColor White
Write-Host "===========================" -ForegroundColor White
Write-Host ""
# ---------------------------------------------------------------------------
# IDE open check
# ---------------------------------------------------------------------------
$ideProcs = Get-Process -Name 'bds' -ErrorAction SilentlyContinue
if ($ideProcs) {
    Write-Host ""
    Write-Warn "RAD Studio / Delphi is currently running (bds.exe detected)."
    Write-Warn "Installing while the IDE is open can cause registry changes to be overwritten on exit."
    Write-Host ""
    Write-Host "  Close the IDE and re-run, or press Enter to continue anyway (Ctrl+C to abort):" -ForegroundColor Yellow -NoNewline
    Write-Host " " -NoNewline
    Read-Host | Out-Null
}

Write-Step "Scanning registry for installed Delphi versions..."

$BdsRoot = 'HKCU:\Software\Embarcadero\BDS'
$DetectedVersions = [System.Collections.Generic.List[PSCustomObject]]::new()

if (-not (Test-Path $BdsRoot)) {
    Write-Fail "Registry key not found: $BdsRoot"
    Write-Host "No Delphi installation detected. Exiting." -ForegroundColor Red
    exit 1
}

foreach ($subkey in Get-ChildItem $BdsRoot) {
    $verStr = $subkey.PSChildName   # e.g. "23.0"

    # Check against known version table
    if (-not $VersionLookup.ContainsKey($verStr)) {
        Write-Warn "Unknown BDS version '$verStr'  - skipping."
        continue
    }

    # Read RootDir and verify it exists
    $rootDir = $null
    try {
        $rootDir = (Get-ItemProperty $subkey.PSPath -Name 'RootDir' -ErrorAction Stop).RootDir
    } catch {
        Write-Warn "Version ${verStr}: no RootDir value  - skipping."
        continue
    }

    if ([string]::IsNullOrWhiteSpace($rootDir) -or -not (Test-Path $rootDir)) {
        Write-Warn "Version ${verStr}: RootDir '$rootDir' not found  - skipping."
        continue
    }

    $entry = $VersionLookup[$verStr].PSObject.Copy()
    $entry | Add-Member -NotePropertyName 'RootDir' -NotePropertyValue $rootDir.TrimEnd('\')
    $DetectedVersions.Add($entry)
}

if ($DetectedVersions.Count -eq 0) {
    Write-Fail "No recognised Delphi installations found."
    exit 1
}

Write-OK "Found $($DetectedVersions.Count) Delphi installation(s)."

# ---------------------------------------------------------------------------
# Step 2: Interactive selection
# ---------------------------------------------------------------------------
Write-Host ""
Write-Host "Detected Delphi installations:" -ForegroundColor White
for ($i = 0; $i -lt $DetectedVersions.Count; $i++) {
    $v = $DetectedVersions[$i]
    Write-Host ("  [{0}] {1}  ({2})" -f ($i + 1), $v.Name, $v.RootDir)
}
Write-Host ""
$SelectedVersions = [System.Collections.Generic.List[PSCustomObject]]::new()

if ($PSBoundParameters.ContainsKey('Versions')) {
    # Non-interactive: resolve -Versions parameter
    if ($Versions -ieq 'all') {
        $SelectedVersions.AddRange($DetectedVersions)
    } else {
        foreach ($token in ($Versions -split ',')) {
            $token = $token.Trim()
            $matched = @($DetectedVersions | Where-Object {
                $_.Version -eq $token -or $_.Name -match "(?i)\b$([regex]::Escape($token))(\b|\.)"
            })
            if ($matched.Count -eq 0) {
                Write-Warn "No detected Delphi version matched '$token'  - ignored."
            } else {
                foreach ($m in $matched) {
                    if (-not @($SelectedVersions | Where-Object { $_.Version -eq $m.Version })) {
                        $SelectedVersions.Add($m)
                    }
                }
            }
        }
    }
} else {
    # Interactive
    Write-Host "Enter the number(s) to install into (comma-separated), or 'all':" -ForegroundColor White -NoNewline
    Write-Host " " -NoNewline
    $selection = (Read-Host).Trim()

    if ([string]::IsNullOrWhiteSpace($selection)) {
        $selection = 'all'
    }

    if ($selection -ieq 'all') {
        $SelectedVersions.AddRange($DetectedVersions)
    } else {
        foreach ($part in ($selection -split ',')) {
            $num = $part.Trim()
            if ($num -match '^\d+$') {
                $idx = [int]$num - 1
                if ($idx -ge 0 -and $idx -lt $DetectedVersions.Count) {
                    $SelectedVersions.Add($DetectedVersions[$idx])
                } else {
                    Write-Warn "Number $num is out of range  - ignored."
                }
            } else {
                Write-Warn "Invalid entry '$num'  - ignored."
            }
        }
    }
}

if ($SelectedVersions.Count -eq 0) {
    Write-Host "No valid versions selected. Exiting." -ForegroundColor Yellow
    exit 0
}

# ---------------------------------------------------------------------------
# Step 3: Determine script root paths
# ---------------------------------------------------------------------------
$ScriptDir  = $PSScriptRoot
$ExpertDir  = Join-Path $ScriptDir '..\Expert'
$ExpertDir  = (Resolve-Path $ExpertDir).Path
$RepoRoot   = (Resolve-Path (Join-Path $ScriptDir '..')).Path
$SourceDir  = Join-Path $RepoRoot 'source'
$PkgDir     = Join-Path $SourceDir 'packages'

# ---------------------------------------------------------------------------
# Process each selected version
# ---------------------------------------------------------------------------
$Results = [ordered]@{}

foreach ($ver in $SelectedVersions) {
    Write-Host ""
    Write-Host ("--- {0} ---" -f $ver.Name) -ForegroundColor White

    $dprojPath  = Join-Path $ExpertDir $ver.DprojFile
    $rsvarsBat  = Join-Path $ver.RootDir 'bin\rsvars.bat'
    $bplName    = [System.IO.Path]::GetFileNameWithoutExtension($ver.DprojFile) + '.bpl'

    # -----------------------------------------------------------------------
    # Registry backup
    # -----------------------------------------------------------------------
    $backupFile = Join-Path $env:TEMP ("DUnitX_BDS_{0}_{1:yyyyMMdd_HHmmss}.reg" -f ($ver.Version -replace '\.','_'), (Get-Date))
    $regKey     = "HKCU\Software\Embarcadero\BDS\$($ver.Version)"
    reg export $regKey $backupFile /y | Out-Null
    if ($LASTEXITCODE -eq 0) {
        Write-OK "Registry backup: $backupFile"
    } else {
        Write-Warn "Registry backup failed for $regKey - continuing anyway."
    }

    # -----------------------------------------------------------------------
    # Validate prerequisites
    # -----------------------------------------------------------------------
    if (-not (Test-Path $dprojPath)) {
        Write-Fail ".dproj not found: $dprojPath"
        $Results[$ver.Name] = 'SKIPPED (dproj missing)'
        continue
    }

    if (-not (Test-Path $rsvarsBat)) {
        Write-Fail "rsvars.bat not found: $rsvarsBat"
        $Results[$ver.Name] = 'SKIPPED (rsvars.bat missing)'
        continue
    }

    # -----------------------------------------------------------------------
    # Step 4: Build
    # -----------------------------------------------------------------------
    Write-Step "Building $($ver.DprojFile) (Win32 Release)..."

    # Capture env vars exported by rsvars.bat so we can find BDSCOMMONDIR
    $envResult = Invoke-BatchCommand -RsvarsBat $rsvarsBat -Command 'set'
    $envHash   = @{}
    foreach ($line in $envResult.Output) {
        if ($line -match '^([^=]+)=(.*)$') {
            $envHash[$Matches[1]] = $Matches[2]
        }
    }

    $bdsCommonDir = $envHash['BDSCOMMONDIR']
    if ([string]::IsNullOrWhiteSpace($bdsCommonDir)) {
        Write-Fail "Could not read BDSCOMMONDIR from rsvars.bat output."
        $Results[$ver.Name] = 'FAILED (BDSCOMMONDIR not found)'
        continue
    }

    $bplPath = Join-Path $bdsCommonDir "dcp\Win32\Release\$bplName"

    # Run msbuild
    $buildCmd    = "msbuild `"$dprojPath`" /p:Config=Release /p:Platform=Win32 /nologo /v:minimal"
    $buildResult = Invoke-BatchCommand -RsvarsBat $rsvarsBat -Command $buildCmd
    $buildOutput = $buildResult.Output
    $buildExitCode = $buildResult.ExitCode

    if ($buildExitCode -ne 0) {
        Write-Fail "Build failed (exit code $buildExitCode)."
        Write-Host ($buildOutput | Out-String) -ForegroundColor DarkRed
        $Results[$ver.Name] = 'FAILED (build error)'
        continue
    }

    if (-not (Test-Path $bplPath)) {
        Write-Fail "Build succeeded but BPL not found at expected path:"
        Write-Fail "  $bplPath"
        $Results[$ver.Name] = 'FAILED (BPL not found after build)'
        continue
    }

    Write-OK "BPL: $bplPath"

    # -----------------------------------------------------------------------
    # Step 5: Register Known Packages
    # -----------------------------------------------------------------------
    Write-Step "Registering in Known Packages..."

    $knownPkgsKey = "HKCU:\Software\Embarcadero\BDS\$($ver.Version)\Known Packages"

    if (-not (Test-Path $knownPkgsKey)) {
        New-Item $knownPkgsKey -Force | Out-Null
    }

    # Remove stale old-style entry (uses literal $(BDS) macro)
    $oldBplName = "`$(BDS)\bin\DUnitXIDEExpert$($ver.OldBplSuffix).bpl"
    if (Get-ItemProperty $knownPkgsKey -Name $oldBplName -ErrorAction SilentlyContinue) {
        Remove-ItemProperty $knownPkgsKey -Name $oldBplName
        Write-Step "Removed old entry: $oldBplName"
    }

    # Set new entry: value name = full BPL path, value data = description
    Set-ItemProperty $knownPkgsKey -Name $bplPath -Value 'DUnitX - IDE Expert'
    Write-OK "Registered: $bplPath"

    # -----------------------------------------------------------------------
    # Step 5b: Set DUNITX environment variable in IDE
    # -----------------------------------------------------------------------
    Write-Step "Setting DUNITX environment variable..."
    $envVarsKey = "HKCU:\Software\Embarcadero\BDS\$($ver.Version)\Environment Variables"
    if (-not (Test-Path $envVarsKey)) { New-Item $envVarsKey -Force | Out-Null }
    Set-ItemProperty $envVarsKey -Name 'DUNITX' -Value $SourceDir
    Write-OK "DUNITX = $SourceDir"

    # -----------------------------------------------------------------------
    # Step 5c: Build DUnitX_VCL (Win32 + Win64, Debug + Release)
    # -----------------------------------------------------------------------
    $vclDproj = Join-Path $PkgDir 'DUnitX_VCL.dproj'
    if (-not (Test-Path $vclDproj)) {
        Write-Warn "DUnitX_VCL.dproj not found - skipping VCL package build."
    } else {
        Write-Step "Building DUnitX_VCL packages..."
        foreach ($platform in @('Win32', 'Win64')) {
            foreach ($config in @('Release', 'Debug')) {
                $cmd = "msbuild `"$vclDproj`" /p:Config=$config /p:Platform=$platform /nologo /v:minimal"
                $r   = Invoke-BatchCommand -RsvarsBat $rsvarsBat -Command $cmd
                if ($r.ExitCode -eq 0) { Write-OK "  VCL $platform/$config" }
                else                   { Write-Warn "  VCL $platform/$config failed (exit $($r.ExitCode))" }
            }
        }
    }

    # -----------------------------------------------------------------------
    # Step 5d: Build DUnitX_FMX (all enabled platforms, Debug + Release)
    # -----------------------------------------------------------------------
    $fmxDproj     = Join-Path $PkgDir 'DUnitX_FMX.dproj'
    $fmxPlatforms = @('Win32','Win64','Win64x','Android','Android64','OSX64','OSXARM64','iOSDevice64','iOSSimARM64','Linux64')
    if (-not (Test-Path $fmxDproj)) {
        Write-Warn "DUnitX_FMX.dproj not found - skipping FMX package build."
    } else {
        Write-Step "Building DUnitX_FMX packages..."
        foreach ($platform in $fmxPlatforms) {
            foreach ($config in @('Release', 'Debug')) {
                $cmd = "msbuild `"$fmxDproj`" /p:Config=$config /p:Platform=$platform /nologo /v:minimal"
                $r   = Invoke-BatchCommand -RsvarsBat $rsvarsBat -Command $cmd
                if ($r.ExitCode -eq 0) { Write-OK "  FMX $platform/$config" }
                else                   { Write-Warn "  FMX $platform/$config failed (exit $($r.ExitCode)) - skipped" }
            }
        }
    }

    # -----------------------------------------------------------------------
    # Step 6: Update library paths for every platform subkey
    # -----------------------------------------------------------------------
    Write-Step "Updating Library paths..."

    $libraryKey = "HKCU:\Software\Embarcadero\BDS\$($ver.Version)\Library"

    if (-not (Test-Path $libraryKey)) {
        Write-Warn "Library key not found  - skipping library path update."
    } else {
        $platformSubkeys = Get-ChildItem $libraryKey -ErrorAction SilentlyContinue
        if ($null -eq $platformSubkeys -or @($platformSubkeys).Count -eq 0) {
            Write-Warn "No platform subkeys found under Library  - skipping library path update."
        } else {
            foreach ($platform in $platformSubkeys) {
                $pname = $platform.PSChildName
                $ppath = $platform.PSPath

                # Clean legacy $(BDS)\source\DUnitX from Browsing Path then add $(DUNITX)
                $props   = Get-ItemProperty $ppath -ErrorAction SilentlyContinue
                $hasProp = $props -and (Get-Member -InputObject $props -Name 'Browsing Path' -MemberType NoteProperty)
                $current = if ($hasProp) { $props.'Browsing Path' } else { '' }
                $cleaned = Remove-LibraryEntry $current '$(BDS)\source\DUnitX'
                if ($cleaned -ne $current) {
                    Set-ItemProperty $ppath 'Browsing Path' $cleaned
                    Write-Step "  ${pname}: removed legacy '`$(BDS)\source\DUnitX' from Browsing Path."
                }

                Add-LibraryValue $ppath 'Search Path'    '$(DUNITX)\packages\$(Platform)\Release' $pname
                Add-LibraryValue $ppath 'Browsing Path'  '$(DUNITX)'                              $pname
                Add-LibraryValue $ppath 'Debug DCU Path' '$(DUNITX)\packages\$(Platform)\Debug'   $pname
            }
        }
    }

    $Results[$ver.Name] = 'OK'
}

# ---------------------------------------------------------------------------
# TestInsight check
# ---------------------------------------------------------------------------
$MissingTestInsight = [System.Collections.Generic.List[PSCustomObject]]::new()
foreach ($ver in $SelectedVersions) {
    if ($Results[$ver.Name] -ne 'OK') { continue }

    $expertsKey    = "HKCU:\Software\Embarcadero\BDS\$($ver.Version)\Experts"
    $hasTestInsight = $false
    if (Test-Path $expertsKey) {
        $props = Get-ItemProperty $expertsKey -ErrorAction SilentlyContinue
        if ($props) {
            $hasTestInsight = @($props.PSObject.Properties | Where-Object { $_.Name -like '*TestInsight*' }).Count -gt 0
        }
    }

    if (-not $hasTestInsight) {
        $MissingTestInsight.Add($ver)
    }
}

if ($MissingTestInsight.Count -gt 0) {
    Write-Host ""
    Write-Warn "TestInsight is not installed for:"
    foreach ($ver in $MissingTestInsight) {
        Write-Host "    $($ver.Name)" -ForegroundColor Yellow
    }
    Write-Host ""
    Write-Host "  TestInsight adds real-time test results inside the IDE." -ForegroundColor White

    if ($InstallTestInsight) {
        $answer = 'y'
    } else {
        Write-Host "  Download and run the installer now? [Y/n]: " -ForegroundColor White -NoNewline
        $answer = (Read-Host).Trim()
    }

    if ($answer -eq '' -or $answer -ieq 'y' -or $answer -ieq 'yes') {
        $zipUrl     = 'https://files.spring4d.com/TestInsight/latest/TestInsightSetup.zip'
        $zipPath    = Join-Path $env:TEMP 'TestInsightSetup.zip'
        $extractDir = Join-Path $env:TEMP 'TestInsightSetup'

        try {
            Write-Step "Downloading TestInsight installer..."
            Invoke-WebRequest -Uri $zipUrl -OutFile $zipPath -UseBasicParsing
            Write-OK "Downloaded: $zipPath"

            Write-Step "Extracting..."
            if (Test-Path $extractDir) { Remove-Item $extractDir -Recurse -Force }
            Expand-Archive -Path $zipPath -DestinationPath $extractDir -Force

            $setupExe = Get-ChildItem $extractDir -Filter 'TestInsightSetup.exe' -Recurse |
                        Select-Object -First 1 -ExpandProperty FullName

            if ($setupExe) {
                Write-Step "Running TestInsightSetup.exe..."
                Start-Process -FilePath $setupExe -Wait
                Write-OK "TestInsight installer finished."
            } else {
                Write-Fail "TestInsightSetup.exe not found in archive."
            }
        } catch {
            Write-Fail "TestInsight install failed: $_"
        } finally {
            Remove-Item $zipPath -ErrorAction SilentlyContinue
        }
    } else {
        Write-Host "  Skipping TestInsight installation." -ForegroundColor Yellow
    }
}

# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------
Write-Host ""
Write-Host "Summary" -ForegroundColor White
Write-Host "-------" -ForegroundColor White
foreach ($key in $Results.Keys) {
    $status = $Results[$key]
    if ($status -eq 'OK') {
        Write-Host ("  {0,-35} {1}" -f $key, $status) -ForegroundColor Green
    } elseif ($status -like 'SKIPPED*') {
        Write-Host ("  {0,-35} {1}" -f $key, $status) -ForegroundColor Yellow
    } else {
        Write-Host ("  {0,-35} {1}" -f $key, $status) -ForegroundColor Red
    }
}
Write-Host ""
