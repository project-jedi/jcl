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
{ The Original Code is JclArrayListsTemplates.pas.                                                 }
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

unit JclPreProcessorArrayListsTemplates;

interface

{$I jcl.inc}

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JclPreProcessorContainerTypes,
  JclPreProcessorContainerTemplates,
  JclPreProcessorContainer1DTemplates;

type
  (* JCLARRAYLISTINT(SELFCLASSNAME, ANCESTORCLASSNAME, COLLECTIONINTERFACENAME, LISTINTERFACENAME,
                     ARRAYINTERFACENAME, ITRINTERFACENAME, DYNARRAYTYPE, INTERFACEADDITIONAL,
                     SECTIONADDITIONAL, COLLECTIONFLAGS, OWNERSHIPDECLARATION, CONSTKEYWORD,
                     PARAMETERNAME, TYPENAME, GETTERNAME, SETTERNAME) *)
  TJclArrayListIntParams = class(TJclCollectionInterfaceParams)
  protected
    // function CodeUnit: string; override;
    function GetInterfaceAdditional: string; override;
  public
    function AliasAttributeIDs: TAllTypeAttributeIDs; override;
  published
    property SelfClassName: string index taArrayListClassName read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property AncestorClassName;
    property CollectionInterfaceName: string index taCollectionInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property EqualityComparerInterfaceName: string index taEqualityComparerInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property ListInterfaceName: string index taListInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property ArrayInterfaceName: string index taArrayInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property ItrInterfaceName: string index taIteratorInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property DynArrayType: string index taDynArrayTypeName read GetTypeAttribute write SetTypeAttribute stored False;
    property InterfaceAdditional;
    property SectionAdditional;
    property CollectionFlags;
    property OwnershipDeclaration;
    property ConstKeyword: string index taConstKeyword read GetTypeAttribute write SetTypeAttribute stored False;
    property ParameterName: string index taParameterName read GetTypeAttribute write SetTypeAttribute stored False;
    property TypeName: string index taTypeName read GetTypeAttribute write SetTypeAttribute stored False;
    property GetterName: string index taGetterName read GetTypeAttribute write SetTypeAttribute stored False;
    property SetterName: string index taSetterName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* JCLARRAYLISTITRINT(SELFCLASSNAME, ITRINTERFACENAME, LISTINTERFACENAME, CONSTKEYWORD, PARAMETERNAME,
                        TYPENAME, GETTERNAME, SETTERNAME) *)
  TJclArrayListItrIntParams = class(TJclContainerInterfaceParams)
  protected
    // function CodeUnit: string; override;
  public
    function AliasAttributeIDs: TAllTypeAttributeIDs; override;
  published
    property SelfClassName: string index taArrayIteratorClassName read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property ItrInterfaceName: string index taIteratorInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property ListInterfaceName: string index taListInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property ConstKeyword: string index taConstKeyword read GetTypeAttribute write SetTypeAttribute stored False;
    property ParameterName: string index taParameterName read GetTypeAttribute write SetTypeAttribute stored False;
    property TypeName: string index taTypeName read GetTypeAttribute write SetTypeAttribute stored False;
    property GetterName: string index taGetterName read GetTypeAttribute write SetTypeAttribute stored False;
    property SetterName: string index taSetterName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

 (* JCLARRAYLISTIMP(SELFCLASSNAME, OWNERSHIPDECLARATION, OWNERSHIPPARAMETER, COLLECTIONINTERFACENAME,
                    ITRINTERFACENAME, ITRCLASSNAME, LISTINTERFACENAME, CONSTKEYWORD, PARAMETERNAME, GETTERNAME,
                    SETTERNAME, RELEASERNAME, TYPENAME, DEFAULTVALUE) *)
  TJclArrayListImpParams = class(TJclCollectionImplementationParams)
  protected
    // function CodeUnit: string; override;
  public
    function GetConstructorParameters: string; override;
    function GetSelfClassName: string; override;
  published
    property SelfClassName: string index taArrayListClassName read GetTypeAttribute write SetTypeAttribute stored False;
    property OwnershipDeclaration;
    property OwnershipParameter: string index taOwnershipParameter read GetTypeAttribute write SetTypeAttribute stored False;
    property CollectionInterfaceName: string index taCollectionInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property ItrInterfaceName: string index taIteratorInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property ItrClassName: string index taArrayIteratorClassName read GetTypeAttribute write SetTypeAttribute stored False;
    property ListInterfaceName: string index taListInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property ConstKeyword: string index taConstKeyword read GetTypeAttribute write SetTypeAttribute stored False;
    property ParameterName: string index taParameterName read GetTypeAttribute write SetTypeAttribute stored False;
    property GetterName: string index taGetterName read GetTypeAttribute write SetTypeAttribute stored False;
    property SetterName: string index taSetterName read GetTypeAttribute write SetTypeAttribute stored False;
    property ReleaserName: string index taReleaserName read GetTypeAttribute write SetTypeAttribute stored False;
    property TypeName: string index taTypeName read GetTypeAttribute write SetTypeAttribute stored False;
    property DefaultValue: string index taDefaultValue read GetTypeAttribute write SetTypeAttribute stored False;
    property MacroFooter;
  end;

  (* JCLARRAYLISTITRIMP(SELFCLASSNAME, ITRINTERFACENAME, LISTINTERFACENAME,
                        CONSTKEYWORD, PARAMETERNAME, TYPENAME, GETTERNAME, SETTERNAME) *)
  TJclArrayListItrImpParams = class(TJclContainerImplementationParams)
  protected
    // function CodeUnit: string; override;
  published
    property SelfClassName: string index taArrayIteratorClassName read GetTypeAttribute write SetTypeAttribute stored False;
    property ItrInterfaceName: string index taIteratorInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property ListInterfaceName: string index taListInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property ConstKeyword: string index taConstKeyword read GetTypeAttribute write SetTypeAttribute stored False;
    property ParameterName: string index taParameterName read GetTypeAttribute write SetTypeAttribute stored False;
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
    LogPath: 'JCL\source\common';
    Extra: '';
    Data: nil
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  SysUtils,
  JclStrings;

procedure RegisterJclContainers;
begin
  RegisterContainerParams('JCLARRAYLISTINT', TJclArrayListIntParams);
  RegisterContainerParams('JCLARRAYLISTITRINT', TJclArrayListItrIntParams);
  RegisterContainerParams('JCLARRAYLISTIMP', TJclArrayListImpParams, TJclArrayListIntParams);
  RegisterContainerParams('JCLARRAYLISTITRIMP', TJclArrayListItrImpParams, TJclArrayListItrIntParams);
end;

//=== { TJclArrayListIntParams } =============================================

function TJclArrayListIntParams.AliasAttributeIDs: TAllTypeAttributeIDs;
begin
  Result := [taArrayListClassName];
end;

function TJclArrayListIntParams.GetInterfaceAdditional: string;
begin
  Result := FInterfaceAdditional;
  if Result = '' then
    Result := Format('%s %s,', [inherited GetInterfaceAdditional, EqualityComparerInterfaceName]);
end;

//=== { TJclArrayListItrIntParams } ==========================================

function TJclArrayListItrIntParams.AliasAttributeIDs: TAllTypeAttributeIDs;
begin
  Result := [taArrayIteratorClassName];
end;

//=== { TJclArrayListImpParams } =============================================

function TJclArrayListImpParams.GetConstructorParameters: string;
begin
  Result := 'FSize';
end;

function TJclArrayListImpParams.GetSelfClassName: string;
begin
  Result := SelfClassName;
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

