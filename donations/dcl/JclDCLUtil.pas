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
{ The Original Code is DCLUtil.pas.                                                                }
{                                                                                                  }
{ The Initial Developer of the Original Code is Jean-Philippe BEMPEL aka RDM. Portions created by  }
{ Jean-Philippe BEMPEL are Copyright (C) Jean-Philippe BEMPEL (rdm_30 att yahoo dott com)          }
{ All rights reserved.                                                                             }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ The Delphi Container Library                                                                     }
{                                                                                                  }
{**************************************************************************************************}

// Last modified: $Date$
// For history see end of file

unit JclDCLUtil;

interface

uses
  SysUtils,
  JclDCL_Intf;

const
  DCLDefaultCapacity = 16;

type
  TDynIInterfaceArray = array of IInterface;
  TDynObjectArray = array of TObject;
  TDynStringArray = array of string;

  // Exceptions
  EDCLException = class(Exception);
  EDCLOutOfBounds = class(EDCLException);
  EDCLNoSuchElement = class(EDCLException);
  EDCLIllegalState = class(EDCLException);
  EDCLConcurrentModification = class(EDCLException);
  EDCLIllegalArgument = class(EDCLException);
  EDCLOperationNotSupported = class(EDCLException);

resourcestring
  RsEOutOfBounds = 'Out of bounds';
  //RsENoSuchElement = 'No such element';
  //RsEIllegalState = 'Illegal state';
  //RsEConcurrentModification = 'Concurrent modification';
  //RsEIllegalArgument = 'Illegal argument';
  RsEOperationNotSupported = 'Operation not supported';
  RsEValueNotFound = 'Value %s not found';

implementation

end.
