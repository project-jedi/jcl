//------------------------------------------------------------------------------
// The Delphi Container Library
// Jean-Philippe BEMPEL aka RDM
// rdm_30@yahoo.com
//------------------------------------------------------------------------------
unit DCLUtil;

interface

uses
  DCL_Intf, SysUtils;

const
  DCLDefaultCapacity = 16;

type
  TIInterfaceArray = array of IInterface;
  TObjectArray = array of TObject;
  TStringArray = array of string;

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

implementation

end.
