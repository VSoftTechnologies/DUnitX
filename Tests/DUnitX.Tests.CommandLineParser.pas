unit DUnitX.Tests.CommandLineParser;
{***************************************************************************}
{                                                                           }
{           DUnitX                                                          }
{                                                                           }
{           Copyright (C) 2012 Vincent Parrett                              }
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

interface

{$I DUnitX.inc}

uses
  DUnitX.TestFramework,
  DUnitX.CommandLine.Options,
  DUnitX.CommandLine.Parser;

type
  TExampleEnum = (enOne,enTwo,enThree);

  TExampleSet = set of TExampleEnum;

  [TestFixture]
  TCommandLineParserTests = class
  public

  [Setup]
  procedure Setup;

  [TearDown]
  procedure TearDown;

  [Test]
  procedure Will_Raise_On_Registering_Duplicate_Options;

  [Test]
  procedure Will_Raise_On_Registering_UnNamed_Option;

  [Test]
  procedure Test_Single_Option;

  [Test]
  procedure Will_Generate_Error_For_Unknown_Option;

  [Test]
  procedure Will_Generate_Error_For_Missing_Value;

  [Test]
  procedure Can_Register_Unnamed_Parameter;

  [Test]
  procedure Can_Parse_Unnamed_Parameter;

  [Test]
  procedure Can_Parse_Multiple_Unnamed_Parameters;

  [Test]
  procedure Will_Generate_Error_For_Extra_Unamed_Parameter;

  [Test]
  procedure Can_Parse_Quoted_Value;

  [Test]
  procedure Will_Raise_For_Missing_Param_File;

  [Test]
  procedure Can_Parse_Enum_Parameter;

  [Test]
  procedure Will_Generate_Error_For_Invalid_Enum;

  [Test]
  procedure Can_Parse_Set_Parameter;

  [Test]
  procedure Will_Generate_Error_For_Invalid_Set;

  end;

implementation

uses
  Classes,
  DUnitX.CommandLine.OptionDef;

{ TCommandLineParserTests }

procedure TCommandLineParserTests.Can_Parse_Enum_Parameter;
var
  def : IOptionDefintion;
  test : TExampleEnum;
  sList : TStringList;
  parseResult : ICommandLineParseResult;

begin
  def := TOptionsRegistry.RegisterOption<TExampleEnum>('test','t',
                  procedure(value : TExampleEnum)
                  begin
                    test := value;
                  end);

  sList := TStringList.Create;
  sList.Add('--test:enTwo');
  try
    parseResult := TOptionsRegistry.Parse(sList);
  finally
    sList.Free;
  end;
  Assert.IsFalse(parseResult.HasErrors);
{$IFNDEF DELPHI_XE_DOWN}
  Assert.AreEqual<TExampleEnum>(enTwo,test);
{$ENDIF}
end;

procedure TCommandLineParserTests.Can_Parse_Multiple_Unnamed_Parameters;
var
  def : IOptionDefintion;
  file1 : string;
  file2 : string;
  sList : TStringList;
  parseResult : ICommandLineParseResult;
  test : boolean;
begin
  TOptionsRegistry.RegisterUnNamedOption<string>('the file we want to process',
                  procedure(value : string)
                  begin
                    file1 := value;
                  end);

  TOptionsRegistry.RegisterUnNamedOption<string>('the second file we want to process',
                  procedure(value : string)
                  begin
                    file2 := value;
                  end);

  def := TOptionsRegistry.RegisterOption<boolean>('test','t',
                  procedure(value : boolean)
                  begin
                    test := value;
                  end);
  def.HasValue := False;

  sList := TStringList.Create;
  sList.Add('c:\file1.txt');
  sList.Add('--test');
  sList.Add('c:\file2.txt');
  try
    parseResult := TOptionsRegistry.Parse(sList);
  finally
    sList.Free;
  end;
  Assert.IsFalse(parseResult.HasErrors);
  Assert.AreEqual('c:\file1.txt',file1);
  Assert.AreEqual('c:\file2.txt',file2);
end;

procedure TCommandLineParserTests.Can_Parse_Quoted_Value;
var
  test : string;
  test2 : string;
  parseResult : ICommandLineParseResult;
  sList : TStringList;
begin
  TOptionsRegistry.RegisterOption<string>('test','t',
                  procedure(value : string)
                  begin
                    test := value;
                  end);

  TOptionsRegistry.RegisterOption<string>('test2','t2',
                  procedure(value : string)
                  begin
                    test2 := value;
                  end);

  sList := TStringList.Create;
  sList.Add('--test:"hello world"');
  sList.Add('--test2:''hello world''');
  try
    parseResult := TOptionsRegistry.Parse(sList);
  finally
    sList.Free;
  end;
  Assert.AreEqual('hello world',test);
  Assert.AreEqual('hello world',test2);
end;

procedure TCommandLineParserTests.Can_Parse_Set_Parameter;
var
  def : IOptionDefintion;
  test : TExampleSet;
  sList : TStringList;
  parseResult : ICommandLineParseResult;

begin
  def := TOptionsRegistry.RegisterOption<TExampleSet>('test','t',
                  procedure(value : TExampleSet)
                  begin
                    test := value;
                  end);

  sList := TStringList.Create;
  sList.Add('--test:[enOne,enThree]');
  try
    parseResult := TOptionsRegistry.Parse(sList);
  finally
    sList.Free;
  end;
  Assert.IsFalse(parseResult.HasErrors);
{$IFNDEF DELPHI_XE_DOWN}
  Assert.AreEqual<TExampleSet>(test,[enOne,enThree]);
{$ENDIF}
end;

procedure TCommandLineParserTests.Can_Parse_Unnamed_Parameter;
var
  def : IOptionDefintion;
  res : string;
  sList : TStringList;
  parseResult : ICommandLineParseResult;
begin
  def := TOptionsRegistry.RegisterUnNamedOption<string>('the file we want to process',
                  procedure(value : string)
                  begin
                    res := value;
                  end);

  sList := TStringList.Create;
  sList.Add('c:\test.txt');
  try
    parseResult := TOptionsRegistry.Parse(sList);
  finally
    sList.Free;
  end;

  Assert.AreEqual('c:\test.txt',res);
end;

procedure TCommandLineParserTests.Can_Register_Unnamed_Parameter;
var
  def : IOptionDefintion;
begin
  def := TOptionsRegistry.RegisterUnNamedOption<string>('the file we want to process',
                  procedure(value : string)
                  begin
                  end);

  Assert.IsTrue(def.IsUnnamed);

end;

procedure TCommandLineParserTests.Setup;
begin
  TOptionsRegistry.RegisteredOptions.Clear;
  TOptionsRegistry.RegisteredUnamedOptions.Clear;
end;

procedure TCommandLineParserTests.TearDown;
begin
  TOptionsRegistry.RegisteredOptions.Clear;
  TOptionsRegistry.RegisteredUnamedOptions.Clear;
end;

procedure TCommandLineParserTests.Test_Single_Option;
var
  def : IOptionDefintion;
  result : boolean;
  parseResult : ICommandLineParseResult;
  sList : TStringList;
begin
  def := TOptionsRegistry.RegisterOption<boolean>('test','t',
                  procedure(value : boolean)
                  begin
                    result := value;
                  end);
  def.HasValue := False;

  sList := TStringList.Create;
  sList.Add('--test');
  try
    parseResult := TOptionsRegistry.Parse(sList);
  finally
    sList.Free;
  end;
  Assert.IsTrue(result);


end;

procedure TCommandLineParserTests.Will_Generate_Error_For_Extra_Unamed_Parameter;
var
  file1 : string;
  sList : TStringList;
  parseResult : ICommandLineParseResult;
  test : string;
begin
  TOptionsRegistry.RegisterUnNamedOption<string>('the file we want to process',
                  procedure(value : string)
                  begin
                    file1 := value;
                  end);

  TOptionsRegistry.RegisterOption<string>('test','t',
                  procedure(value : string)
                  begin
                    test := value;
                  end);
//  def.HasValue := False;

  sList := TStringList.Create;
  sList.Add('c:\file1.txt');
  sList.Add('--test:hello');
  sList.Add('c:\file2.txt');
  try
    parseResult := TOptionsRegistry.Parse(sList);
    TDUnitX.CurrentRunner.Log(parseResult.ErrorText);
  finally
    sList.Free;
  end;
  Assert.IsTrue(parseResult.HasErrors);
  Assert.AreEqual('c:\file1.txt',file1);
  Assert.AreEqual('hello',test);
end;

procedure TCommandLineParserTests.Will_Generate_Error_For_Missing_Value;
var
  def : IOptionDefintion;
  result : boolean;
  parseResult : ICommandLineParseResult;
  sList : TStringList;
begin
  def := TOptionsRegistry.RegisterOption<boolean>('test','t',
                  procedure(value : boolean)
                  begin
                    result := value;
                  end);
  def.HasValue := True;

  sList := TStringList.Create;
  sList.Add('--test');
  try
    parseResult := TOptionsRegistry.Parse(sList);
    TDUnitX.CurrentRunner.Log(parseResult.ErrorText);
  finally
    sList.Free;
  end;
  Assert.IsTrue(parseResult.HasErrors);
end;

procedure TCommandLineParserTests.Will_Generate_Error_For_Unknown_Option;
var
  parseResult : ICommandLineParseResult;
  sList : TStringList;
begin
  sList := TStringList.Create;
  sList.Add('--blah');
  try
    parseResult := TOptionsRegistry.Parse(sList);
    TDUnitX.CurrentRunner.Log(parseResult.ErrorText);
  finally
    sList.Free;
  end;
  Assert.IsTrue(parseResult.HasErrors);

end;

procedure TCommandLineParserTests.Will_Raise_For_Missing_Param_File;
var
  def : IOptionDefintion;
  parseResult : ICommandLineParseResult;
  sList : TStringList;
begin
  def := TOptionsRegistry.RegisterOption<boolean>('options','o',nil);
  def.IsOptionFile := true;
  sList := TStringList.Create;
  sList.Add('--options:"x:\blah blah.txt"');
  try
    parseResult := TOptionsRegistry.Parse(sList);
    TDUnitX.CurrentRunner.Log(parseResult.ErrorText);
  finally
    sList.Free;
  end;
  Assert.IsTrue(parseResult.HasErrors);
end;

procedure TCommandLineParserTests.Will_Raise_On_Registering_Duplicate_Options;
var
  result : boolean;
begin
  //same long names
  Assert.WillRaise(
    procedure
    begin
          TOptionsRegistry.RegisterOption<boolean>('test','t',
                        procedure(value : boolean)
                        begin
                          result := value;
                        end);
          TOptionsRegistry.RegisterOption<boolean>('test','t',
                          procedure(value : boolean)
                          begin
                            result := value;
                          end);

    end);

  //same short names
  Assert.WillRaise(
    procedure
    begin
          TOptionsRegistry.RegisterOption<boolean>('test','t',
                        procedure(value : boolean)
                        begin
                          result := value;
                        end);
          TOptionsRegistry.RegisterOption<boolean>('t','blah',
                          procedure(value : boolean)
                          begin
                            result := value;
                          end);

    end);



end;

procedure TCommandLineParserTests.Will_Raise_On_Registering_UnNamed_Option;
begin
  //same long names
  Assert.WillRaise(
    procedure
    begin
          TOptionsRegistry.RegisterOption<boolean>('','t',
                        procedure(value : boolean)
                        begin
                        end);

    end);
end;

procedure TCommandLineParserTests.Will_Generate_Error_For_Invalid_Enum;
var
  def : IOptionDefintion;
  test : TExampleEnum;
  sList : TStringList;
  parseResult : ICommandLineParseResult;

begin
  def := TOptionsRegistry.RegisterOption<TExampleEnum>('test','t',
                  procedure(value : TExampleEnum)
                  begin
                    test := value;
                  end);

  sList := TStringList.Create;
  sList.Add('--test:enbBlah');
  try
    parseResult := TOptionsRegistry.Parse(sList);
    TDUnitX.CurrentRunner.Log(parseResult.ErrorText);
  finally
    sList.Free;
  end;
  Assert.IsTrue(parseResult.HasErrors);
end;

procedure TCommandLineParserTests.Will_Generate_Error_For_Invalid_Set;
var
  def : IOptionDefintion;
  test : TExampleSet;
  sList : TStringList;
  parseResult : ICommandLineParseResult;

begin
  def := TOptionsRegistry.RegisterOption<TExampleSet>('test','t',
                  procedure(value : TExampleSet)
                  begin
                    test := value;
                  end);

  sList := TStringList.Create;
  sList.Add('--test:[enOne,enFoo]');
  try
    parseResult := TOptionsRegistry.Parse(sList);
    TDUnitX.CurrentRunner.Log(parseResult.ErrorText);
  finally
    sList.Free;
  end;
  Assert.IsTrue(parseResult.HasErrors);

end;

initialization
  TDUnitX.RegisterTestFixture(TCommandLineParserTests);
end.
