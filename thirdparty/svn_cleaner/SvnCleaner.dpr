{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL)                                                                  }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is SvnCleaner.dpr.                                                             }
{                                                                                                  }
{ The Initial Developer of the Original Code is Florent Ouchet.                                    }
{ Portions created by Florent Ouchet are Copyright (C) of Florent Ouchet. All rights reserved.     }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Subversion repository cleaner.                                                                   }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

program SvnCleaner;

{$APPTYPE CONSOLE}

{$I jcl.inc}
{$I crossplatform.inc}

uses
  SysUtils,
  Classes,
  JclFileUtils,
  JclSimpleXml,
  JclSysUtils,
  JclAnsiStrings;

type
  TSvnProperty = record
    Name: string;
    Value: string;
  end;

  TSvnProperties = array of TSvnProperty;

  TSvnSetting = record
    Path: string;
    Masks: array of string;
    Recurse: Boolean;
    DirOnly: Boolean;
    Properties: TSvnProperties;
  end;

//=== { TSvnSettings } =======================================================

type
  TSvnSettings = class
  private
    FSettings: array of TSvnSetting;
    FRoot: string;
  public
    procedure LoadFromXml(XmlNode: TJclSimpleXMLElem);
    function GetProperties(const Path: string): TSvnProperties;
    property Root: string read FRoot;
  end;

function TSvnSettings.GetProperties(const Path: string): TSvnProperties;
var
  IndexSetting, IndexMask, IndexProperty, IndexCheck: Integer;
  RelPath: string;
  AddProperties: Boolean;
begin
  SetLength(Result, 0);
  for IndexSetting := Low(FSettings) to High(FSettings) do
  begin
    AddProperties := False;
    RelPath := PathGetRelativePath(FSettings[IndexSetting].Path, Path);
    if Pos('..', RelPath) = 0 then
    begin
      if Length(FSettings[IndexSetting].Masks) = 0 then
        AddProperties := (RelPath = '.') or FSettings[IndexSetting].Recurse
      else
      if RelPath <> '.' then
        if (Pos(DirDelimiter, RelPath) = 0) or FSettings[IndexSetting].Recurse then
          for IndexMask := Low(FSettings[IndexSetting].Masks) to High(FSettings[IndexSetting].Masks) do
            if StrMatches(FSettings[IndexSetting].Masks[IndexMask], RelPath) then
            begin
              AddProperties := True;
              Break;
            end;

      if (FileGetAttr(Path) and faDirectory) = 0 then
      begin
        AddProperties := AddProperties and not FSettings[IndexSetting].DirOnly;
      end;
        
      if AddProperties then
      begin
        for IndexProperty := Low(FSettings[IndexSetting].Properties) to High(FSettings[IndexSetting].Properties) do
        begin
          for IndexCheck := Low(Result) to High(Result) do
            if FSettings[IndexSetting].Properties[IndexProperty].Name = Result[IndexCheck].Name then
              raise Exception.CreateFmt('duplicate property "%s" for item "%s"', [Result[IndexCheck].Name, Path]);
          SetLength(Result, Length(Result) + 1);
          Result[High(Result)] := FSettings[IndexSetting].Properties[IndexProperty];
        end;
      end;
    end;
  end;
end;

procedure TSvnSettings.LoadFromXml(XmlNode: TJclSimpleXMLElem);
  function LoadProperty(Elem: TJclSimpleXmlElem): TSvnProperty;
  var
    NameProp: TJclSimpleXMLProp;
    ValueElem: TJclSimpleXMLElem;
    Index: Integer;
  begin
    NameProp := Elem.Properties.ItemNamed['name'];
    if not Assigned(NameProp) then
      raise Exception.Create('no name property');
    Result.Name := NameProp.Value;
    Result.Value := '';
    for Index := 0 to Elem.Items.Count - 1 do
    begin
      ValueElem := Elem.Items.Item[Index];
      if ValueElem.Name = 'value' then
      begin
        if Result.Value = '' then
          Result.Value := ValueElem.Value
        else
          Result.Value := Result.Value + AnsiLineBreak + ValueElem.Value;
      end
      else
      if ValueElem.Name <> '' then
        raise Exception.CreateFmt('unknown item "%s"', [ValueElem.Name]);
    end;
  end;

  procedure LoadSetting(Elem: TJclSimpleXMLElem; const Path: string);
  var
    PathProp, MaskProp, RecurseProp, DirOnlyProp: TJclSimpleXMLProp;
    SubElem: TJclSimpleXMLElem;
    Index: Integer;
    MyPath: string;
    MySetting: TSvnSetting;
    Masks: TStrings;
  begin
    PathProp := Elem.Properties.ItemNamed['path'];
    if not Assigned(PathProp) then
      raise Exception.Create('no path property');
    MaskProp := Elem.Properties.ItemNamed['mask'];
    if not Assigned(MaskProp) then
      raise Exception.Create('no mask prop');
    RecurseProp := Elem.Properties.ItemNamed['recurse'];
    if not Assigned(RecurseProp) then
      raise Exception.Create('no recurse prop');
    DirOnlyProp := Elem.Properties.ItemNamed['dironly'];
    if not Assigned(DirOnlyProp) then
      raise Exception.Create('no dironly prop');
    {$IFDEF MSWINDOWS}
    MyPath := StringReplace(PathProp.Value, '/', DirDelimiter, [rfReplaceAll]);
    {$ELSE ~MSWINDOWS}
    MyPath := PathProp.Value;
    {$ENDIF ~MSWINDOWS}
    MyPath := PathAddSeparator(Path) + MyPath;
    MySetting.Path := PathCanonicalize(PathGetRelativePath(GetCurrentDir, MyPath));
    Masks := TStringList.Create;
    try
      StrToStrings(MaskProp.Value, AnsiSpace, Masks);
      SetLength(MySetting.Masks, Masks.Count);
      for Index := 0 to Masks.Count - 1 do
        {$IFDEF MSWINDOWS}
        MySetting.Masks[Index] := StringReplace(Masks.Strings[Index], '/', DirDelimiter, [rfReplaceAll]);
        {$ELSE ~MSWINDOWS}
        MySetting.Masks[Index] := Masks.Strings[Index];
        {$ENDIF ~MSWINDOWS}
    finally
      Masks.Free;
    end;
    MySetting.Recurse := RecurseProp.Value = 'yes';
    MySetting.DirOnly := DirOnlyProp.Value = 'yes';
    for Index := 0 to Elem.Items.Count - 1 do
    begin
      SubElem := Elem.Items.Item[Index];
      if SubElem.Name = 'setting' then
        LoadSetting(SubElem, MyPath)
      else
      if SubElem.Name = 'property' then
      begin
        SetLength(MySetting.Properties, Length(MySetting.Properties) + 1);
        MySetting.Properties[High(MySetting.Properties)] := LoadProperty(SubElem);
      end
      else
      if SubElem.Name <> '' then
        raise Exception.CreateFmt('unknown item "%s"', [SubElem.Name]);
    end;
    if Length(MySetting.Properties) > 0 then
    begin
      SetLength(FSettings, Length(FSettings) + 1);
      FSettings[High(FSettings)] := MySetting;
    end;
  end;
var
  Elem: TJclSimpleXMLElem;
  Index: Integer;
  RootProp: TJclSimpleXMLProp;
begin
  RootProp := XmlNode.Properties.ItemNamed['root'];
  if not Assigned(RootProp) then
    raise Exception.Create('no root property');
  {$IFDEF MSWINDOWS}
  FRoot := StringReplace(RootProp.Value, '/', DirDelimiter, [rfReplaceAll]);
  {$ELSE ~MSWINDOWS}
  FRoot := RootProp.Value;
  {$ENDIF ~MSWINDOWS}
  FRoot := PathCanonicalize(PathGetRelativePath(GetCurrentDir, FRoot));
  for Index := 0 to XmlNode.Items.Count - 1 do
  begin
    Elem := XmlNode.Items.Item[Index];
    if Elem.Name = 'setting' then
      LoadSetting(Elem, '.')
    else
    if Elem.Name <> '' then
      raise Exception.CreateFmt('Unknown elem name "%s"', [Elem.Name]);
  end;
end;

//=== { TSvnCleaner } ========================================================

type
  TSvnCleaner = class
  private
    FSettings: TSvnSettings;
    FSvnExe: string;
    function ExecuteSvn(const Argument: string): string;
    procedure CleanItem(const ItemName: string);
  public
    constructor Create(const XmlFileName: string);
    destructor Destroy; override;
    procedure Execute;
  end;

procedure TSvnCleaner.CleanItem(const ItemName: string);
  procedure ProcessProperties(const SvnResult: string);
  var
    Lines: TStrings;
    Line, Choice, PropFileName: string;
    Index, IndexCheck, SepPos: Integer;
    Properties, NewProperties: TSvnProperties;
    Found: Boolean;
  begin
    Lines := TStringList.Create;
    try
      StrToStrings(SvnResult, AnsiLineBreak, Lines);
      for Index := 1 to Lines.Count - 1 do
      begin
        Line := Lines.Strings[Index];
        if Pos('  ', Line) = 1 then
        begin
          SetLength(Properties, Length(Properties) + 1);
          Line := Copy(Line, 3, Length(Line) - 2);
          SepPos := Pos(' : ', Line);
          if SepPos = 0 then
            raise Exception.Create('could not determine property name');
          Properties[High(Properties)].Name := Copy(Line, 1, SepPos - 1);
          Properties[High(Properties)].Value := Copy(Line, SepPos + 3, Length(Line) - SepPos - 2);
        end
        else
        begin
          if Length(Properties) = 0 then
            raise Exception.Create('invalid sequence');
          Properties[High(Properties)].Value := Properties[High(Properties)].Value + AnsiLineBreak + Line;
        end;
      end;
    finally
      Lines.Free;
    end;

    NewProperties := FSettings.GetProperties(ItemName);

    for Index := Low(Properties) to High(Properties) do
    begin
      Found := False;
      for IndexCheck := Low(NewProperties) to High(NewProperties) do
        if Properties[Index].Name = NewProperties[IndexCheck].Name then
        begin
          if (Properties[Index].Value <> NewProperties[IndexCheck].Value)
            and ((Properties[Index].Value + AnsiLineBreak) <> NewProperties[IndexCheck].Value)
            and (Properties[Index].Value <> (NewProperties[IndexCheck].Value + AnsiLineBreak)) then
          begin
            WriteLn('property "', Properties[Index].Name, '" for item "', ItemName, '" will be changed');
            WriteLn('old value: ', Properties[Index].Value);
            WriteLn('new value: ', NewProperties[IndexCheck].Value);
            repeat
              Write('process? (y)es, (n)o, (a)bord: ');
              ReadLn(Choice);
            until (Choice = 'y') or (Choice = 'Y') or (Choice = 'n') or (Choice = 'N') or (Choice = 'a') or (Choice = 'A');
            case Choice[1] of
              'y', 'Y':
                begin
                  PropFileName := ChangeFileExt(ParamStr(0), '.prop');
                  StringToFile(PropFileName, NewProperties[IndexCheck].Value);
                  ExecuteSvn(Format('propset "%s" -F "%s" "%s"', [NewProperties[IndexCheck].Name, PropFileName, ItemName]));
                end;
              'n', 'N': ;
              'a', 'A':
                Abort;
            end;
          end;
          Found := True;
          Break;
        end;
      if not Found then
      begin
        WriteLn('property "', Properties[Index].Name, '" for item "', ItemName, '" will be deleted');
        WriteLn('old value: ', Properties[Index].Value);
        repeat
          Write('process? (y)es, (n)o, (a)bord: ');
          ReadLn(Choice);
        until (Choice = 'y') or (Choice = 'Y') or (Choice = 'n') or (Choice = 'N') or (Choice = 'a') or (Choice = 'A');
        case Choice[1] of
          'y', 'Y':
            ExecuteSvn(Format('propdel "%s" "%s"', [Properties[Index].Name, ItemName]));
          'n', 'N': ;
          'a', 'A':
            Abort;
        end;
      end;
    end;

    for Index := Low(NewProperties) to High(NewProperties) do
    begin
      Found := False;
      for IndexCheck := Low(Properties) to High(Properties) do
        if NewProperties[Index].Name = Properties[IndexCheck].Name then
        begin
          Found := True;
          Break;
        end;
      if not Found then
      begin
        WriteLn('property "', NewProperties[Index].Name, '" for item "', ItemName, '" will be added');
        WriteLn('new value: ', NewProperties[Index].Value);
        repeat
          Write('process? (y)es, (n)o, (a)bord: ');
          ReadLn(Choice);
        until (Choice = 'y') or (Choice = 'Y') or (Choice = 'n') or (Choice = 'N') or (Choice = 'a') or (Choice = 'A');
        case Choice[1] of
          'y', 'Y':
            begin
              PropFileName := ChangeFileExt(ParamStr(0), '.prop');
              StringToFile(PropFileName, NewProperties[Index].Value);
              ExecuteSvn(Format('propset "%s" -F "%s" "%s"', [NewProperties[Index].Name, PropFileName, ItemName]));
            end;
          'n', 'N': ;
          'a', 'A':
            Abort;
        end;
      end;
    end;
  end;

  procedure ProcessStatus(const SvnResult: string);
  var
    DirectoryXml: TJclSimpleXML;
    RootNode, TargetNode, EntryNode, WcStatusNode: TJclSimpleXMLElem;
    TargetIndex, EntryIndex, WcStatusIndex: Integer;
    PathProp, ItemProp: TJclSimpleXMLProp;
  begin
    DirectoryXml := TJclSimpleXML.Create;
    try
      DirectoryXml.LoadFromString(SvnResult);
      RootNode := DirectoryXml.Root;
      if RootNode.Name <> 'status' then
        raise Exception.Create('expecting status node');
      for TargetIndex := 0 to RootNode.Items.Count - 1 do
      begin
        TargetNode := RootNode.Items.Item[TargetIndex];
        if TargetNode.Name = 'target' then
        begin
          for EntryIndex := 0 to TargetNode.Items.Count - 1 do
          begin
            EntryNode := TargetNode.Items.Item[EntryIndex];
            if EntryNode.Name <> 'entry' then
              raise Exception.Create('expecting entry node');
            PathProp := EntryNode.Properties.ItemNamed['path'];
            if not Assigned(PathProp) then
              raise Exception.Create('no path node');
            for WcStatusIndex := 0 to EntryNode.Items.Count - 1 do
            begin
              WcStatusNode := EntryNode.Items.Item[WcStatusIndex];
              if not Assigned(WcStatusNode) then
                raise Exception.Create('expecting wc-status node');
              ItemProp := WcStatusNode.Properties.ItemNamed['item'];
              if not Assigned(ItemProp) then
                raise Exception.Create('expecting item prop');
              if (ItemProp.Value <> 'unversioned') and (PathProp.Value <> ItemName) then
                CleanItem(PathProp.Value);
            end;
          end;
        end
        else
        if TargetNode.Name <> '' then
          raise Exception.Create('expecting target node');
      end;
    finally
      DirectoryXml.Free;
    end;
  end;
begin
  WriteLn('processing item "', ItemName, '"');
  ProcessProperties(ExecuteSvn(Format('proplist -v "%s"', [ItemName])));

  if (FileGetAttr(ItemName) and faDirectory) <> 0 then
    ProcessStatus(ExecuteSvn(Format('status -v --xml -N "%s"', [ItemName])));
end;

constructor TSvnCleaner.Create(const XmlFileName: string);
var
  AXmlSettings: TJclSimpleXML;
begin
  inherited Create;
  FSvnExe := GetEnvironmentVariable('SVN');
  if FSvnExe = '' then
    repeat
      WriteLn('Enter path to svn.exe');
      ReadLn(FSvnExe);
    until FileExists(FSvnExe);
  FSettings := TSvnSettings.Create;
  AXmlSettings := TJclSimpleXML.Create;
  try
    AXmlSettings.LoadFromFile(XmlFileName);
    AXmlSettings.Options := AXmlSettings.Options - [sxoAutoCreate];

    FSettings.LoadFromXml(AXmlSettings.Root);
  finally
    AXmlSettings.Free;
  end;
end;

destructor TSvnCleaner.Destroy;
begin
  FSettings.Free;
  inherited Destroy;
end;

procedure TSvnCleaner.Execute;
begin
  CleanItem(FSettings.Root);
end;

function TSvnCleaner.ExecuteSvn(const Argument: string): string;
begin
  JclSysUtils.Execute(Format('"%s" %s', [FSvnExe, Argument]), Result);
end;

var
  ACleaner: TSvnCleaner;
begin
  try
    ACleaner := TSvnCleaner.Create(ChangeFileExt(ParamStr(0), '.xml'));
    try
      ACleaner.Execute;
    finally
      ACleaner.Free;
    end;
  except
    on E:Exception do
      WriteLn(E.Classname, ': ', E.Message);
  end;
end.
