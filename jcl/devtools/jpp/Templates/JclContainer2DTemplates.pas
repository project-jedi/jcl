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
{ The Original Code is JclContainer2DTemplates.pas.                                                }
{                                                                                                  }
{ The Initial Developer of the Original Code is Florent Ouchet                                     }
{         <outchy att users dott sourceforge dott net>                                             }
{ Portions created by Florent Ouchet are Copyright (C) of Florent Ouchet. All rights reserved.     }
{                                                                                                  }
{ Contributors:                                                                                    }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclContainer2DTemplates;

interface

{$I jcl.inc}

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JclBase,
  JclContainerTypes,
  JclContainer1DTemplates;

{$TYPEINFO ON}

type
  TJclContainerMapInfo = class
  private
    FCustomMapAttributes: TMapAttributes;
    FKnownMapAttributes: PKnownMapAttributes;
    FValueTypeInfo: TJclContainerTypeInfo;
    FKeyTypeInfo: TJclContainerTypeInfo;
    function GetCustomMapAttribute(Index: TMapAttributeID): string;
    function GetKeyAttribute(Index: TKeyAttributeID): string;
    function GetValueAttribute(Index: TValueAttributeID): string;
    procedure SetKeyAttribute(Index: TKeyAttributeID; const Value: string);
    procedure SetValueAttribute(Index: TValueAttributeID;
      const Value: string);
  protected
    function GetKnownMap: Boolean;
    function GetMapAttribute(Index: TMapAttributeID): string;
    function IsMapAttributeStored(Index: TMapAttributeID): Boolean;
    procedure SetKnownMap(Value: Boolean);
    procedure SetMapAttribute(Index: TMapAttributeID; const Value: string);
    procedure TypeKnownTypeChange(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    property KnownMap: Boolean read GetKnownMap write SetKnownMap;
    property KnownMapAttributes: PKnownMapAttributes read FKnownMapAttributes;
    property CustomMapAttributes[Index: TMapAttributeID]: string read GetCustomMapAttribute;
    property MapAttributes[Index: TMapAttributeID]: string read GetMapAttribute write SetMapAttribute;
    property KeyAttributes[Index: TKeyAttributeID]: string read GetKeyAttribute write SetKeyAttribute;
    property KeyTypeInfo: TJclContainerTypeInfo read FKeyTypeInfo;
    property ValueAttributes[Index: TValueAttributeID]: string read GetValueAttribute write SetValueAttribute;
    property ValueTypeInfo: TJclContainerTypeInfo read FValueTypeInfo;
  end;

  TJclMapInterfaceParams = class(TJclInterfaceParams)
  private
    FMapInfo: TJclContainerMapInfo;
  protected
    function GetKeyAttribute(Index: TKeyAttributeID): string;
    function GetMapAttribute(Index: TMapAttributeID): string;
    function GetValueAttribute(Index: TValueAttributeID): string;
    function IsMapAttributeStored(Index: TMapAttributeID): Boolean;
    procedure SetKeyAttribute(Index: TKeyAttributeID; const Value: string);
    procedure SetMapAttribute(Index: TMapAttributeID; const Value: string);
    procedure SetValueAttribute(Index: TValueAttributeID; const Value: string);
  public
    property MapInfo: TJclContainerMapInfo read FMapInfo write FMapInfo;
  end;

  TJclMapClassInterfaceParams = class(TJclMapInterfaceParams)
  protected
  public
  end;

  TJclMapImplementationParams = class(TJclImplementationParams)
  private
  protected
  public
  end;

  TJclMapClassImplementationParams = class(TJclMapImplementationParams)
  protected
    FMacroFooter: string;
    function GetSelfClassName: string; virtual; abstract;
  public
    function GetMacroFooter: string; override;
    procedure ResetDefault(Value: Boolean); override;
    property MacroFooter: string read GetMacroFooter write FMacroFooter;
  end;

{$IFNDEF TYPEINFO_ON}
  {$TYPEINFO OFF}
{$ENDIF ~TYPEINFO_ON}

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\devtools\jpp\Templates';
    Extra: '';
    Data: nil
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  TypInfo,
  SysUtils,
  ActiveX,
  ComObj,
  JclRTTI,
  JclSysUtils,
  JclContainerIntf,
  JclContainerKnownMaps;

//=== { TJclContainerMapInfo } ===============================================

constructor TJclContainerMapInfo.Create;
begin
  inherited Create;
  FKeyTypeInfo := TJclContainerTypeInfo.Create;
  FKeyTypeInfo.OnKnownTypeChange := TypeKnownTypeChange;
  FValueTypeInfo := TJclContainerTypeInfo.Create;
  FValueTypeInfo.OnKnownTypeChange := TypeKnownTypeChange;
end;

destructor TJclContainerMapInfo.Destroy;
begin
  FKeyTypeInfo.Free;
  FValueTypeInfo.Free;
  inherited Destroy;
end;

function TJclContainerMapInfo.GetCustomMapAttribute(Index: TMapAttributeID): string;
begin
  if (Index >= Low(TMapAttributeID)) and (Index <= High(TMapAttributeID)) then
    Result := FCustomMapAttributes[Index]
  else
  begin
    Error(reRangeError);
    Result := '';
  end;
end;

function TJclContainerMapInfo.GetKeyAttribute(Index: TKeyAttributeID): string;
begin
  if Index = kaKeyTypeName then
    Result := KeyTypeInfo.TypeName
  else
  if (Index >= Low(TKeyAttributeID)) and (Index <= High(TKeyAttributeID)) then
    Result := KeyTypeInfo.TypeAttributes[KeyAttributeInfos[Index]]
  else
  begin
    Error(reRangeError);
    Result := '';
  end;
end;

function TJclContainerMapInfo.GetKnownMap: Boolean;
var
  Index: TMapAttributeID;
begin
  Result := Assigned(FKnownMapAttributes) and KeyTypeInfo.KnownType and ValueTypeInfo.KnownType;
  for Index := Low(TMapAttributeID) to High(TMapAttributeID) do
  begin
    Result := Result and (FCustomMapAttributes[Index] = '');
    if not Result then
      Break;
  end;
end;

function TJclContainerMapInfo.GetMapAttribute(
  Index: TMapAttributeID): string;
begin
  if (Index >= Low(TMapAttributeID)) and (Index <= High(TMapAttributeID)) then
  begin
    if FCustomMapAttributes[Index] = '' then
    begin
      if Assigned(FKnownMapAttributes) then
        Result := FKnownMapAttributes^.MapAttributes[Index]
      else
        Result := Format(MapAttributeInfos[Index].DefaultValue, [KeyTypeInfo.TypeName, ValueTypeInfo.TypeName]);
    end
    else
      Result := FCustomMapAttributes[Index];
  end
  else
  begin
    Error(reRangeError);
    Result := '';
  end;
end;

function TJclContainerMapInfo.GetValueAttribute(Index: TValueAttributeID): string;
begin
  if Index = vaValueTypeName then
    Result := ValueTypeInfo.TypeName
  else
  if (Index >= Low(TValueAttributeID)) and (Index <= High(TValueAttributeID)) then
    Result := ValueTypeInfo.TypeAttributes[ValueAttributeInfos[Index]]
  else
  begin
    Error(reRangeError);
    Result := '';
  end;
end;

function TJclContainerMapInfo.IsMapAttributeStored(Index: TMapAttributeID): Boolean;
begin
  if (Index >= Low(TMapAttributeID)) and (Index <= High(TMapAttributeID)) then
    Result := FCustomMapAttributes[Index] <> ''
  else
  begin
    Error(reRangeError);
    Result := False;
  end;
end;

procedure TJclContainerMapInfo.SetKeyAttribute(Index: TKeyAttributeID;
  const Value: string);
begin
  if Index = kaKeyTypeName then
    KeyTypeInfo.TypeName := Value
  else
  if (Index >= Low(TKeyAttributeID)) and (Index <= High(TKeyAttributeID)) then
    KeyTypeInfo.TypeAttributes[KeyAttributeInfos[Index]] := Value
  else
    Error(reRangeError);
end;

procedure TJclContainerMapInfo.SetKnownMap(Value: Boolean);
var
  Index: TMapAttributeID;
  NewGUID: TGUID;
begin
  if Value then
  begin
    // reset to default values
    for Index := Low(TMapAttributeID) to High(TMapAttributeID) do
      FCustomMapAttributes[Index] := '';
  end
  else
  if {not Value and} Assigned(FKnownMapAttributes) then
  begin
    // copy with new GUIDs
    for Index := Low(TMapAttributeID) to High(TMapAttributeID) do
    begin
      if MapAttributeInfos[Index].IsGUID then
      begin
        OleCheck(CoCreateGuid(NewGUID));
        FCustomMapAttributes[Index] := GUIDToString(NewGUID);
      end
      else
        FCustomMapAttributes[Index] := FKnownMapAttributes^.MapAttributes[Index];
    end;
  end
  else
  begin
    {not Value and not Assigned(FKnownTypeAttributes)}
    // default names with new GUIDs
    for Index := Low(TMapAttributeID) to High(TMapAttributeID) do
    begin
      if MapAttributeInfos[Index].IsGUID then
      begin
        OleCheck(CoCreateGuid(NewGUID));
        FCustomMapAttributes[Index] := GUIDToString(NewGUID);
      end
      else
        FCustomMapAttributes[Index] := Format(MapAttributeInfos[Index].DefaultValue,
          [KeyTypeInfo.TypeName, ValueTypeInfo.TypeName]);
    end;
  end;
end;

procedure TJclContainerMapInfo.SetMapAttribute(Index: TMapAttributeID;
  const Value: string);
begin
  if (Index >= Low(TMapAttributeID)) and (Index <= High(TMapAttributeID)) then
    FCustomMapAttributes[Index] := Value
  else
    Error(reRangeError);
end;

procedure TJclContainerMapInfo.SetValueAttribute(Index: TValueAttributeID;
  const Value: string);
begin
  if Index = vaValueTypeName then
    ValueTypeInfo.TypeName := Value
  else
  if (Index >= Low(TValueAttributeID)) and (Index <= High(TValueAttributeID)) then
    ValueTypeInfo.TypeAttributes[ValueAttributeInfos[Index]] := Value
  else
    Error(reRangeError);
end;

procedure TJclContainerMapInfo.TypeKnownTypeChange(Sender: TObject);
begin
  if KeyTypeInfo.KnownType and ValueTypeInfo.KnownType then
  begin
    FKnownMapAttributes := IsKnownMap(KeyTypeInfo.TypeName, ValueTypeInfo.TypeName);
    SetKnownMap(True);
  end;
end;

//=== { TJclMapInterfaceParams } =============================================

function TJclMapInterfaceParams.GetKeyAttribute(Index: TKeyAttributeID): string;
begin
  Result := MapInfo.KeyAttributes[Index];
end;

function TJclMapInterfaceParams.GetMapAttribute(
  Index: TMapAttributeID): string;
begin
  Result := MapInfo.MapAttributes[Index];
end;

function TJclMapInterfaceParams.GetValueAttribute(
  Index: TValueAttributeID): string;
begin
  Result := MapInfo.ValueAttributes[Index];
end;

function TJclMapInterfaceParams.IsMapAttributeStored(
  Index: TMapAttributeID): Boolean;
begin
  Result := MapInfo.CustomMapAttributes[Index] <> '';
end;

procedure TJclMapInterfaceParams.SetKeyAttribute(Index: TKeyAttributeID;
  const Value: string);
begin
  MapInfo.KeyAttributes[Index] := Value;
end;

procedure TJclMapInterfaceParams.SetMapAttribute(Index: TMapAttributeID;
  const Value: string);
begin
  MapInfo.MapAttributes[Index] := Value;
end;

procedure TJclMapInterfaceParams.SetValueAttribute(Index: TValueAttributeID;
  const Value: string);
begin
  MapInfo.ValueAttributes[Index] := Value;
end;

//=== { TJclMapClassImplementationParams } ===================================

function TJclMapClassImplementationParams.GetMacroFooter: string;
//var
//  Ownership, SelfClassName, ConstructorParameters: string;
begin
  {if GetTypeAttribute(taOwnershipParameter) <> '' then
    Ownership := 'False'
  else
    Ownership := '';

  SelfClassName := GetSelfClassName;
  ConstructorParameters := GetConstructorParameters;

  Result := FMacroFooter;

  if (Result = '') and TypeInfo.KnownType then
  begin
    if (ConstructorParameters <> '') and (Ownership <> '') then
      ConstructorParameters := ConstructorParameters + ', ' + Ownership
    else
    if ConstructorParameters = '' then
      ConstructorParameters := Ownership;
    if ConstructorParameters <> '' then
      ConstructorParameters := '(' + ConstructorParameters + ')';
    Result := Format(NativeLineBreak + NativeLineBreak +
                     'function %s.CreateEmptyContainer: TJclAbstractContainerBase;' + NativeLineBreak +
                     'begin' + NativeLineBreak +
                     '  Result := %s.Create%s;' + NativeLineBreak +
                     '  AssignPropertiesTo(Result);' + NativeLineBreak +
                     'end;' + NativeLineBreak,
                     [SelfClassName, SelfClassName, ConstructorParameters]);
  end;}
  Result := '';
end;

procedure TJclMapClassImplementationParams.ResetDefault(Value: Boolean);
begin
  inherited ResetDefault(Value);
  FMacroFooter := '';
  if not Value then
    FMacroFooter := GetMacroFooter;
end;

initialization
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}

finalization
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}

end.
