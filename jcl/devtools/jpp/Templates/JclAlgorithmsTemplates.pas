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
{ The Original Code is JclAlgorithmsTemplates.pas.                                                 }
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

unit JclAlgorithmsTemplates;

interface

{$I jcl.inc}

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JclContainerTypes,
  JclContainerTemplates,
  JclContainer1DTemplates;

type
  TJclAlgorithmsIntParams = class(TJclContainerInterfaceParams)
  protected
    // function CodeUnit: string; override;
  end;

  TJclAlgorithmsIntProcParams = class(TJclAlgorithmsIntParams)
  protected
    FOverload: string;
    FProcName: string;
    function GetProcName: string; virtual;
    function IsProcNameStored: Boolean;
  public
    property Overload: string read FOverload write FOverload;
    property ProcName: string read GetProcName write FProcName stored IsProcNameStored;
  end;

  TJclAlgorithmsImpProcParams = class(TJclContainerImplementationParams)
  protected
    function GetProcName: string;
    procedure SetProcName(const Value: string);
    // function CodeUnit: string; override;
  public
    property ProcName: string read GetProcName write SetProcName stored False;
  end;

  (* APPLYINT(PROCNAME, ITRINTERFACENAME, CALLBACKTYPE, OVERLOAD) *)
  TJclApplyIntParams = class(TJclAlgorithmsIntProcParams)
  protected
    function GetProcName: string; override;
  published
    property Overload;
    property ProcName;
    property ItrInterfaceName: string index taIteratorInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property CallbackType: string index taApplyFunctionName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* APPLYIMP(PROCNAME, ITRINTERFACENAME, CALLBACKTYPE, SETTERNAME) *)
  TJclApplyImpParams = class(TJclAlgorithmsImpProcParams)
  published
    property ProcName;
    property ItrInterfaceName: string index taIteratorInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property CallbackType: string index taApplyFunctionName read GetTypeAttribute write SetTypeAttribute stored False;
    property SetterName: string index taSetterName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* SIMPLECOMPAREINT(PROCNAME, CONSTKEYWORD, TYPENAME) *)
  TJclSimpleCompareIntParams = class(TJclAlgorithmsIntParams)
  published
    property ProcName: string index taSimpleCompareFunctionName read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property ConstKeyword: string index taConstKeyword read GetTypeAttribute write SetTypeAttribute stored False;
    property TypeName: string index taTypeName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* SIMPLEEQUALITYCOMPAREINT(PROCNAME, CONSTKEYWORD, TYPENAME) *)
  TJclSimpleEqualityCompareIntParams = class(TJclAlgorithmsIntParams)
  published
    property ProcName: string index taSimpleEqualityCompareFunctionName read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property ConstKeyword: string index taConstKeyword read GetTypeAttribute write SetTypeAttribute stored False;
    property TypeName: string index taTypeName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* SIMPLEHASHCONVERTINT(PROCNAME, CONSTKEYWORD, PARAMETERNAME, TYPENAME) *)
  TJclSimpleHashConvertIntParams = class(TJclAlgorithmsIntParams)
  published
    property ProcName: string index taSimpleHashConvertFunctionName read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property ConstKeyword: string index taConstKeyword read GetTypeAttribute write SetTypeAttribute stored False;
    property ParameterName: string index taParameterName read GetTypeAttribute write SetTypeAttribute stored False;
    property TypeName: string index taTypeName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* FINDINT(PROCNAME, ITRINTERFACENAME, CONSTKEYWORD, PARAMETERNAME, TYPENAME, CALLBACKTYPE, OVERLOAD) *)
  TJclFindIntParams = class(TJclAlgorithmsIntProcParams)
  protected
    function GetProcName: string; override;
  published
    property ProcName;
    property ItrInterfaceName: string index taIteratorInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property ConstKeyword: string index taConstKeyword read GetTypeAttribute write SetTypeAttribute stored False;
    property ParameterName: string index taParameterName read GetTypeAttribute write SetTypeAttribute stored False;
    property TypeName: string index taTypeName read GetTypeAttribute write SetTypeAttribute stored False;
    property CallbackType: string index taCompareFunctionName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* FINDIMP(PROCNAME, ITRINTERFACENAME, CONSTKEYWORD, PARAMETERNAME, TYPENAME, CALLBACKTYPE) *)
  TJclFindImpParams = class(TJclAlgorithmsImpProcParams)
  published
    property ProcName;
    property ItrInterfaceName: string index taIteratorInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property ConstKeyword: string index taConstKeyword read GetTypeAttribute write SetTypeAttribute stored False;
    property ParameterName: string index taParameterName read GetTypeAttribute write SetTypeAttribute stored False;
    property TypeName: string index taTypeName read GetTypeAttribute write SetTypeAttribute stored False;
    property CallbackType: string index taCompareFunctionName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* FINDEQINT(PROCNAME,ITRINTERFACENAME, CONSTKEYWORD, PARAMETERNAME, TYPENAME, CALLBACKTYPE, OVERLOAD) *)
  TJclFindEqIntParams = class(TJclAlgorithmsIntProcParams)
  protected
    function GetProcName: string; override;
  published
    property ProcName;
    property ItrInterfaceName: string index taIteratorInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property ConstKeyword: string index taConstKeyword read GetTypeAttribute write SetTypeAttribute stored False;
    property ParameterName: string index taParameterName read GetTypeAttribute write SetTypeAttribute stored False;
    property TypeName: string index taTypeName read GetTypeAttribute write SetTypeAttribute stored False;
    property CallbackType: string index taEqualityCompareFunctionName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* FINDEQIMP(PROCNAME, ITRINTERFACENAME, CONSTKEYWORD, PARAMETERNAME, TYPENAME, CALLBACKTYPE) *)
  TJclFindEqImpParams = class(TJclAlgorithmsImpProcParams)
  published
    property ProcName;
    property ItrInterfaceName: string index taIteratorInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property ConstKeyword: string index taConstKeyword read GetTypeAttribute write SetTypeAttribute stored False;
    property ParameterName: string index taParameterName read GetTypeAttribute write SetTypeAttribute stored False;
    property TypeName: string index taTypeName read GetTypeAttribute write SetTypeAttribute stored False;
    property CallbackType: string index taEqualityCompareFunctionName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* COUNTOBJECTINT(PROCNAME, ITRINTERFACENAME, CONSTKEYWORD, PARAMETERNAME, TYPENAME, CALLBACKTYPE, OVERLOAD) *)
  TJclCountObjectIntParams = class(TJclAlgorithmsIntProcParams)
  protected
    function GetProcName: string; override;
  published
    property ProcName;
    property ItrInterfaceName: string index taIteratorInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property ConstKeyword: string index taConstKeyword read GetTypeAttribute write SetTypeAttribute stored False;
    property ParameterName: string index taParameterName read GetTypeAttribute write SetTypeAttribute stored False;
    property TypeName: string index taTypeName read GetTypeAttribute write SetTypeAttribute stored False;
    property CallbackType: string index taCompareFunctionName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* COUNTOBJECTIMP(PROCNAME, ITRINTERFACENAME, CONSTKEYWORD, PARAMETERNAME, TYPENAME, CALLBACKTYPE) *)
  TJclCountObjectImpParams = class(TJclAlgorithmsImpProcParams)
  published
    property ProcName;
    property ItrInterfaceName: string index taIteratorInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property ConstKeyword: string index taConstKeyword read GetTypeAttribute write SetTypeAttribute stored False;
    property ParameterName: string index taParameterName read GetTypeAttribute write SetTypeAttribute stored False;
    property TypeName: string index taTypeName read GetTypeAttribute write SetTypeAttribute stored False;
    property CallbackType: string index taCompareFunctionName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* COUNTOBJECTEQINT(PROCNAME, ITRINTERFACENAME, CONSTKEYWORD, PARAMETERNAME, TYPENAME, CALLBACKTYPE, OVERLOAD) *)
  TJclCountObjectEqIntParams = class(TJclAlgorithmsIntProcParams)
  protected
    function GetProcName: string; override;
  published
    property ProcName;
    property ItrInterfaceName: string index taIteratorInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property ConstKeyword: string index taConstKeyword read GetTypeAttribute write SetTypeAttribute stored False;
    property ParameterName: string index taParameterName read GetTypeAttribute write SetTypeAttribute stored False;
    property TypeName: string index taTypeName read GetTypeAttribute write SetTypeAttribute stored False;
    property CallbackType: string index taEqualityCompareFunctionName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* COUNTOBJECTEQIMP(PROCNAME, ITRINTERFACENAME, CONSTKEYWORD, PARAMETERNAME, TYPENAME, CALLBACKTYPE) *)
  TJclCountObjectEqImpParams = class(TJclAlgorithmsImpProcParams)
  published
    property ProcName;
    property ItrInterfaceName: string index taIteratorInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property ConstKeyword: string index taConstKeyword read GetTypeAttribute write SetTypeAttribute stored False;
    property ParameterName: string index taParameterName read GetTypeAttribute write SetTypeAttribute stored False;
    property TypeName: string index taTypeName read GetTypeAttribute write SetTypeAttribute stored False;
    property CallbackType: string index taEqualityCompareFunctionName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* COPYINT(PROCNAME, ITRINTERFACENAME, OVERLOAD) *)
  TJclCopyIntParams = class(TJclAlgorithmsIntProcParams)
  protected
    function GetProcName: string; override;
  published
    property ProcName;
    property ItrInterfaceName: string index taIteratorInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* COPYIMP(PROCNAME, ITRINTERFACENAME, SETTERNAME) *)
  TJclCopyImpParams = class(TJclAlgorithmsImpProcParams)
  published
    property ProcName;
    property ItrInterfaceName: string index taIteratorInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property SetterName: string index taSetterName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* GENERATEINT(PROCNAME, LISTINTERFACENAME, CONSTKEYWORD, PARAMETERNAME, TYPENAME, OVERLOAD) *)
  TJclGenerateIntParams = class(TJclAlgorithmsIntProcParams)
  protected
    function GetProcName: string; override;
  published
    property ProcName;
    property ListInterfaceName: string index taListInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property ConstKeyword: string index taConstKeyword read GetTypeAttribute write SetTypeAttribute stored False;
    property ParameterName: string index taParameterName read GetTypeAttribute write SetTypeAttribute stored False;
    property TypeName: string index taTypeName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* GENERATEIMP(PROCNAME, LISTINTERFACENAME, CONSTKEYWORD, PARAMETERNAME, TYPENAME) *)
  TJclGenerateImpParams = class(TJclAlgorithmsImpProcParams)
  published
    property ProcName;
    property ListInterfaceName: string index taListInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property ConstKeyword: string index taConstKeyword read GetTypeAttribute write SetTypeAttribute stored False;
    property ParameterName: string index taParameterName read GetTypeAttribute write SetTypeAttribute stored False;
    property TypeName: string index taTypeName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* FILLINT(PROCNAME, ITRINTERFACENAME, CONSTKEYWORD, PARAMETERNAME, TYPENAME, OVERLOAD) *)
  TJclFillIntParams = class(TJclAlgorithmsIntProcParams)
  protected
    function GetProcName: string; override;
  published
    property ProcName;
    property ItrInterfaceName: string index taIteratorInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property ConstKeyword: string index taConstKeyword read GetTypeAttribute write SetTypeAttribute stored False;
    property ParameterName: string index taParameterName read GetTypeAttribute write SetTypeAttribute stored False;
    property TypeName: string index taTypeName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* FILLIMP(PROCNAME, ITRINTERFACENAME, CONSTKEYWORD, PARAMETERNAME, TYPENAME, SETTERNAME) *)
  TJclFillImpParams = class(TJclAlgorithmsImpProcParams)
  published
    property ProcName;
    property ItrInterfaceName: string index taIteratorInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property ConstKeyword: string index taConstKeyword read GetTypeAttribute write SetTypeAttribute stored False;
    property ParameterName: string index taParameterName read GetTypeAttribute write SetTypeAttribute stored False;
    property TypeName: string index taTypeName read GetTypeAttribute write SetTypeAttribute stored False;
    property SetterName: string index taSetterName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* REVERSEINT(PROCNAME, ITRINTERFACENAME, OVERLOAD) *)
  TJclReverseIntParams = class(TJclAlgorithmsIntProcParams)
  protected
    function GetProcName: string; override;
  published
    property ProcName;
    property ItrInterfaceName: string index taIteratorInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* REVERSEIMP(PROCNAME, ITRINTERFACENAME, TYPENAME, GETTERNAME, SETTERNAME) *)
  TJclReverseImpParams = class(TJclAlgorithmsImpProcParams)
  published
    property ProcName;
    property ItrInterfaceName: string index taIteratorInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property TypeName: string index taTypeName read GetTypeAttribute write SetTypeAttribute stored False;
    property GetterName: string index taGetterName read GetTypeAttribute write SetTypeAttribute stored False;
    property SetterName: string index taSetterName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* SORTINT(PROCNAME, LISTINTERFACENAME, LEFT, RIGHT, CALLBACKTYPE, OVERLOAD) *)
  TJclSortIntParams = class(TJclAlgorithmsIntProcParams)
  private
    FLeft: string;
    FRight: string;
    function GetLeft: string;
    function GetRight: string;
    function IsLeftStored: Boolean;
    function IsRightStored: Boolean;
  protected
    function GetProcName: string; override;
  published
    property ProcName;
    property ListInterfaceName: string index taListInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property Left: string read GetLeft write FLeft stored IsLeftStored;
    property Right: string read GetRight write FRight stored IsRightStored;
    property CallbackType: string index taCompareFunctionName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* QUICKSORTIMP(PROCNAME, LISTINTERFACENAME, LEFT, RIGHT, CALLBACKTYPE, TYPENAME, GETTERNAME, SETTERNAME) *)
  TJclQuickSortImpParams = class(TJclAlgorithmsImpProcParams)
  private
    function GetLeft: string;
    function GetRight: string;
    procedure SetLeft(const Value: string);
    procedure SetRight(const Value: string);
  published
    property ProcName;
    property ListInterfaceName: string index taListInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property Left: string read GetLeft write SetLeft stored False;
    property Right: string read GetRight write SetRight stored False;
    property CallbackType: string index taCompareFunctionName read GetTypeAttribute write SetTypeAttribute stored False;
    property TypeName: string index taTypeName read GetTypeAttribute write SetTypeAttribute stored False;
    property GetterName: string index taGetterName read GetTypeAttribute write SetTypeAttribute stored False;
    property SetterName: string index taSetterName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

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

procedure RegisterJclContainers;
begin
  RegisterContainerParams('APPLYINT', TJclApplyIntParams);
  RegisterContainerParams('SIMPLECOMPAREINT', TJclSimpleCompareIntParams);
  RegisterContainerParams('SIMPLEEQUALITYCOMPAREINT', TJclSimpleEqualityCompareIntParams);
  RegisterContainerParams('SIMPLEHASHCONVERTINT', TJclSimpleHashConvertIntParams);
  RegisterContainerParams('FINDINT', TJclFindIntParams);
  RegisterContainerParams('FINDEQINT', TJclFindEqIntParams);
  RegisterContainerParams('COUNTOBJECTINT', TJclCountObjectIntParams);
  RegisterContainerParams('COUNTOBJECTEQINT', TJclCountObjectEqIntParams);
  RegisterContainerParams('COPYINT', TJclCopyIntParams);
  RegisterContainerParams('GENERATEINT', TJclGenerateIntParams);
  RegisterContainerParams('FILLINT', TJclFillIntParams);
  RegisterContainerParams('REVERSEINT', TJclReverseIntParams);
  RegisterContainerParams('SORTINT', TJclSortIntParams);

  RegisterContainerParams('APPLYIMP', TJclApplyImpParams, TJclApplyIntParams);
  RegisterContainerParams('FINDIMP', TJclFindImpParams, TJclFindIntParams);
  RegisterContainerParams('FINDEQIMP', TJclFindEqImpParams, TJclFindEqIntParams);
  RegisterContainerParams('COUNTOBJECTIMP', TJclCountObjectImpParams, TJclCountObjectIntParams);
  RegisterContainerParams('COUNTOBJECTEQIMP', TJclCountObjectEqImpParams, TJclCountObjectEqIntParams);
  RegisterContainerParams('COPYIMP', TJclCopyImpParams, TJclCopyIntParams);
  RegisterContainerParams('GENERATEIMP', TJclGenerateImpParams, TJclGenerateIntParams);
  RegisterContainerParams('FILLIMP', TJclFillImpParams, TJclFillIntParams);
  RegisterContainerParams('REVERSEIMP', TJclReverseImpParams, TJclReverseIntParams);
  RegisterContainerParams('QUICKSORTIMP', TJclQuickSortImpParams, TJclSortIntParams);
end;

//=== { TJclAlgorithmsIntFunctionParams } ====================================

function TJclAlgorithmsIntProcParams.GetProcName: string;
begin
  Result := FProcName;
  // override to customize
end;

function TJclAlgorithmsIntProcParams.IsProcNameStored: Boolean;
begin
  Result := FProcName <> '';
end;

//=== { TJclAlgorithmsImpProcParams } ========================================

function TJclAlgorithmsImpProcParams.GetProcName: string;
begin
  Result := (InterfaceParams as TJclAlgorithmsIntProcParams).ProcName;
end;

procedure TJclAlgorithmsImpProcParams.SetProcName(const Value: string);
begin
  (InterfaceParams as TJclAlgorithmsIntProcParams).ProcName := Value;
end;

//=== { TJclApplyIntParams } =================================================

function TJclApplyIntParams.GetProcName: string;
begin
  Result := inherited GetProcName;
  if Result = '' then
    Result := 'Apply';
end;

//=== { TJclFindIntParams } ==================================================

function TJclFindIntParams.GetProcName: string;
begin
  Result := inherited GetProcName;
  if Result = '' then
    Result := 'Find';
end;

//=== { TJclFindEqIntParams } ================================================

function TJclFindEqIntParams.GetProcName: string;
begin
  Result := inherited GetProcName;
  if Result = '' then
    Result := 'Find';
end;

//=== { TJclCountObjectIntParams } ===========================================

function TJclCountObjectIntParams.GetProcName: string;
begin
  Result := inherited GetProcName;
  if Result = '' then
    Result := 'CountObject';
end;

//=== { TJclCountObjectEqIntParams } =========================================

function TJclCountObjectEqIntParams.GetProcName: string;
begin
  Result := inherited GetProcName;
  if Result = '' then
    Result := 'CountObject';
end;

//=== { TJclCopyIntParams } ==================================================

function TJclCopyIntParams.GetProcName: string;
begin
  Result := inherited GetProcName;
  if Result = '' then
    Result := 'Copy';
end;

//=== { TJclGenerateIntParams } ==============================================

function TJclGenerateIntParams.GetProcName: string;
begin
  Result := inherited GetProcName;
  if Result = '' then
    Result := 'Generate';
end;

//=== { TJclFillIntParams } ==================================================

function TJclFillIntParams.GetProcName: string;
begin
  Result := inherited GetProcName;
  if Result = '' then
    Result := 'Fill';
end;

//=== { TJclReverseIntParams } ===============================================

function TJclReverseIntParams.GetProcName: string;
begin
  Result := inherited GetProcName;
  if Result = '' then
    Result := 'Reverse';
end;

//=== { TJclSortIntParams } ==================================================

function TJclSortIntParams.GetLeft: string;
begin
  Result := FLeft;
  if Result = '' then
    Result := 'L';
end;

function TJclSortIntParams.GetProcName: string;
begin
  Result := inherited GetProcName;
  if Result = '' then
    Result := 'QuickSort';
end;

function TJclSortIntParams.GetRight: string;
begin
  Result := FRight;
  if Result = '' then
    Result := 'R';
end;

function TJclSortIntParams.IsLeftStored: Boolean;
begin
  Result := FLeft <> '';
end;

function TJclSortIntParams.IsRightStored: Boolean;
begin
  Result := FRight <> '';
end;

//=== { TJclQuickSortImpParams } =============================================

function TJclQuickSortImpParams.GetLeft: string;
begin
  Result := (InterfaceParams as TJclSortIntParams).Left;
end;

function TJclQuickSortImpParams.GetRight: string;
begin
  Result := (InterfaceParams as TJclSortIntParams).Right;
end;

procedure TJclQuickSortImpParams.SetLeft(const Value: string);
begin
  (InterfaceParams as TJclSortIntParams).Left := Value;
end;

procedure TJclQuickSortImpParams.SetRight(const Value: string);
begin
  (InterfaceParams as TJclSortIntParams).Right := Value;
end;

initialization
  RegisterJclContainers;
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}

finalization
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}

end.

