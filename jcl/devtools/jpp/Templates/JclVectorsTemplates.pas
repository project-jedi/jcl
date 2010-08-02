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
{ The Original Code is JclVectorsTemplates.pas.                                                    }
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

unit JclVectorsTemplates;

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
  (* JCLVECTORINT(SELFCLASSNAME, ANCESTORCLASSNAME, COLLECTIONINTERFACENAME, LISTINTERFACENAME,
                  ARRAYINTERFACENAME, ITRINTERFACENAME, INTERFACEADDITIONAL, SECTIONADDITIONAL,
                  COLLECTIONFLAGS, OWNERSHIPDECLARATION, CONSTKEYWORD, PARAMETERNAME, TYPENAME,
                  DYNARRAYTYPE, GETTERNAME, SETTERNAME) *)
  TJclVectorIntParams = class(TJclCollectionInterfaceParams)
  protected
    // function CodeUnit: string; override;
    function GetInterfaceAdditional: string; override;
  public
    function AliasAttributeIDs: TAllTypeAttributeIDs; override;
  published
    property SelfClassName: string index taVectorClassName read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property AncestorClassName;
    property CollectionInterfaceName: string index taCollectionInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property EqualityComparerInterfaceName: string index taEqualityComparerInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property ListInterfaceName: string index taListInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property ArrayInterfaceName: string index taArrayInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property ItrInterfaceName: string index taIteratorInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property InterfaceAdditional;
    property SectionAdditional;
    property CollectionFlags;
    property OwnershipDeclaration;
    property ConstKeyword: string index taConstKeyword read GetTypeAttribute write SetTypeAttribute stored False;
    property ParameterName: string index taParameterName read GetTypeAttribute write SetTypeAttribute stored False;
    property TypeName: string index taTypeName read GetTypeAttribute write SetTypeAttribute stored False;
    property DynArrayType: string index taDynArrayTypeName read GetTypeAttribute write SetTypeAttribute stored False;
    property GetterName: string index taGetterName read GetTypeAttribute write SetTypeAttribute stored False;
    property SetterName: string index taSetterName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* JCLVECTORITRINT(SELFCLASSNAME, ITRINTERFACENAME, LISTINTERFACENAME,
                     CONSTKEYWORD, PARAMETERNAME, TYPENAME, GETTERNAME, SETTERNAME) *)
  TJclVectorItrIntParams = class(TJclContainerInterfaceParams)
  protected
    // function CodeUnit: string; override;
  public
    function AliasAttributeIDs: TAllTypeAttributeIDs; override;
  published
    property SelfClassName: string index taVectorIteratorClassName read GetTypeAttribute write SetTypeAttribute stored IsTypeAttributeStored;
    property ItrInterfaceName: string index taIteratorInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property ListInterfaceName: string index taListInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property ConstKeyword: string index taConstKeyword read GetTypeAttribute write SetTypeAttribute stored False;
    property ParameterName: string index taParameterName read GetTypeAttribute write SetTypeAttribute stored False;
    property TypeName: string index taTypeName read GetTypeAttribute write SetTypeAttribute stored False;
    property GetterName: string index taGetterName read GetTypeAttribute write SetTypeAttribute stored False;
    property SetterName: string index taSetterName read GetTypeAttribute write SetTypeAttribute stored False;
  end;

  (* JCLVECTORIMP(SELFCLASSNAME, COLLECTIONINTERFACENAME, LISTINTERFACENAME, ITRINTERFACENAME,
                  ITRCLASSNAME, OWNERSHIPDECLARATION, OWNERSHIPPARAMETER, CONSTKEYWORD,
                  PARAMETERNAME, TYPENAME, DEFAULTVALUE, GETTERNAME, SETTERNAME, RELEASERNAME) *)
  TJclVectorImpParams = class(TJclCollectionImplementationParams)
  protected
    // function CodeUnit: string; override;
  public
    function GetConstructorParameters: string; override;
    function GetSelfClassName: string; override;
  published
    property SelfClassName: string index taVectorClassName read GetTypeAttribute write SetTypeAttribute stored False;
    property CollectionInterfaceName: string index taCollectionInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property ListInterfaceName: string index taListInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property ItrInterfaceName: string index taIteratorInterfaceName read GetTypeAttribute write SetTypeAttribute stored False;
    property ItrClassName: string index taVectorIteratorClassName read GetTypeAttribute write SetTypeAttribute stored False;
    property OwnershipDeclaration;
    property OwnershipParameter: string index taOwnershipParameter read GetTypeAttribute write SetTypeAttribute stored False;
    property ConstKeyword: string index taConstKeyword read GetTypeAttribute write SetTypeAttribute stored False;
    property ParameterName: string index taParameterName read GetTypeAttribute write SetTypeAttribute stored False;
    property TypeName: string index taTypeName read GetTypeAttribute write SetTypeAttribute stored False;
    property DefaultValue: string index taDefaultValue read GetTypeAttribute write SetTypeAttribute stored False;
    property GetterName: string index taGetterName read GetTypeAttribute write SetTypeAttribute stored False;
    property SetterName: string index taSetterName read GetTypeAttribute write SetTypeAttribute stored False;
    property ReleaserName: string index taReleaserName read GetTypeAttribute write SetTypeAttribute stored False;
    property MacroFooter;
  end;

  (* JCLVECTORITRIMP(SELFCLASSNAME, ITRINTERFACENAME, LISTINTERFACENAME,
                     CONSTKEYWORD, PARAMETERNAME, TYPENAME, GETTERNAME, SETTERNAME) *)
  TJclVectorItrImpParams = class(TJclContainerImplementationParams)
  protected
    // function CodeUnit: string; override;
  published
    property SelfClassName: string index taVectorIteratorClassName read GetTypeAttribute write SetTypeAttribute stored False;
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
    LogPath: 'JCL\devtools\jpp\Templates';
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
  RegisterContainerParams('JCLVECTORINT', TJclVectorIntParams);
  RegisterContainerParams('JCLVECTORITRINT', TJclVectorItrIntParams);
  RegisterContainerParams('JCLVECTORIMP', TJclVectorImpParams, TJclVectorIntParams);
  RegisterContainerParams('JCLVECTORITRIMP', TJclVectorItrImpParams, TJclVectorItrIntParams);
end;

//=== { TJclVectorIntParams } ================================================

function TJclVectorIntParams.AliasAttributeIDs: TAllTypeAttributeIDs;
begin
  Result := [taVectorClassName];
end;

function TJclVectorIntParams.GetInterfaceAdditional: string;
begin
  Result := FInterfaceAdditional;
  if Result = '' then
    Result := Format('%s %s,', [inherited GetInterfaceAdditional, EqualityComparerInterfaceName]);
end;

//=== { TJclVectorItrIntParams } =============================================

function TJclVectorItrIntParams.AliasAttributeIDs: TAllTypeAttributeIDs;
begin
  Result := [taVectorIteratorClassName];
end;

//=== { TJclVectorImpParams } ================================================

function TJclVectorImpParams.GetConstructorParameters: string;
begin
  Result := 'FSize';
end;

function TJclVectorImpParams.GetSelfClassName: string;
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

