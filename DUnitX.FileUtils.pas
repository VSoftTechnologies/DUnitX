{***************************************************************************}
{                                                                           }
{           DUnitX                                                          }
{                                                                           }
{           Copyright (C) 2017 Vincent Parrett                              }
{                                                                           }
{           vincent@finalbuilder.com                                        }
{           http://www.finalbuilder.com                                     }
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

unit DUnitX.FileUtils;

interface

{$I DUnitX.inc}

// Contains File and Path Utils - in a separate unit to make IFDEFing for
// platform independence and different Delphi version easier and more clearly
// defined as unit evolves.

uses
  {$IFDEF USE_NS}
  System.SysUtils,
  System.IOUtils,
  Winapi.Windows;
  {$ELSE}
  SysUtils,
  IOUtils,
  Windows;
  {$ENDIF}

type
  //Simply for readability of param types
  TDirectoryName = type string;

  FileUtils = class
  private
    class function InternalCreateTempFile(const ParentDir: TDirectoryName): TFileName;
  public
    class function FileExists(const FileName: TFileName): boolean;
    class function DirectoryExists(const DirectoryName: TDirectoryName): boolean;
    class function TempDirectory: string;
    class procedure DeleteFile(const FileName: TFileName);
    class procedure WriteAllText(const FileName, Contents: string; const Encoding: TEncoding); overload;
    class procedure WriteAllText(const FileName, Contents: string); overload;
    class function ReadAllText(const FileName: string): string;
    class procedure CreateDirectory(const Directory: TDirectoryName);
    class function CreateTempFile(const ParentDir: TDirectoryName = ''): TFileName;
    class procedure DeleteDirectory(const Directory: TDirectoryName; const Recursive: boolean = False);
    class function GetCreationDateTime(const FileName: TFileName): TDateTime;
    class procedure SetCreationDateTime(const FileName: TFileName; const Value: TDateTime);
    class function FileSize(const FileName: TFileName): Int64;
    class procedure CreateEmptyFile(const FileName: TFileName);
  end;

implementation

uses
  {$IFDEF USE_NS}
  System.StrUtils;
  {$ELSE}
  StrUtils;
  {$ENDIF}

class procedure FileUtils.CreateDirectory(const Directory: TDirectoryName);
begin
  TDirectory.CreateDirectory(Directory);
end;

class procedure FileUtils.CreateEmptyFile(const FileName: TFileName);
begin
  if FileExists(FileName) then
    raise Exception.Create('Cannot create file - it already exists:' + FileName);

  WriteAllText(FileName, '');
end;

class function FileUtils.CreateTempFile(const ParentDir: TDirectoryName): TFileName;
var
  WorkParentDir: TDirectoryName;
begin
  WorkParentDir := IfThen(ParentDir <> '', ParentDir, TempDirectory);

  if not DirectoryExists(WorkParentDir) then
    raise Exception.Create('ParentDir must exist: ' + WorkParentDir);

  Result := InternalCreateTempFile(WorkParentDir);
end;

class procedure FileUtils.DeleteDirectory(const Directory: TDirectoryName; const Recursive: boolean);
begin
  TDirectory.Delete(Directory, True);
end;

class procedure FileUtils.DeleteFile(const FileName: TFileName);
begin
  if FileName <> '' then
    TFile.Delete(FileName);
end;

class function FileUtils.DirectoryExists(const DirectoryName: TDirectoryName): boolean;
begin
  Result := TDirectory.Exists(DirectoryName);
end;

class function FileUtils.GetCreationDateTime(const FileName: TFileName): TDateTime;
begin
  Result := TFile.GetCreationTime(FileName);
end;

class function FileUtils.InternalCreateTempFile(const ParentDir: TDirectoryName): TFileName;
var
  Buffer: array[0..MAX_PATH] of char;
begin
  if GetTempFileName(PChar(ParentDir), PChar('tmp'), 0, Buffer) <> 0 then
    Result := string(Buffer)
  else
    RaiseLastOSError();
end;

class function FileUtils.FileExists(const FileName: TFileName): boolean;
begin
  Result := TFile.Exists(FileName);
end;

class function FileUtils.FileSize(const FileName: TFileName): Int64;
var
  Res: TSearchRec;
  Ret: integer;
begin
  Result := 0;

  Ret := FindFirst(FileName, faAnyFile, Res);
  try
    if Ret = 0 then
      Result := Res.Size
    else
      raise EFileNotFoundException.Create('File not found: ' + FileName);
  finally
    {$IFDEF USE_NS}System.{$ENDIF}SysUtils.FindClose(Res);
  end;
end;

class function FileUtils.ReadAllText(const FileName: string): string;
begin
  Result := TFile.ReadAllText(FileName);
end;

class procedure FileUtils.SetCreationDateTime(const FileName: TFileName; const Value: TDateTime);
begin
  TFile.SetCreationTime(FileName, Value);
end;

class function FileUtils.TempDirectory: string;
begin
  Result := IncludeTrailingPathDelimiter(TPath.GetTempPath);
end;

class procedure FileUtils.WriteAllText(const FileName, Contents: string);
begin
  TFile.WriteAllText(FileName, Contents);
end;

class procedure FileUtils.WriteAllText(const FileName, Contents: string; const Encoding: TEncoding);
begin
  TFile.WriteAllText(FileName, Contents, Encoding);
end;

end.
