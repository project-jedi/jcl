//------------------------------------------------------------------------------
// The Delphi Container Library
// Jean-Philippe BEMPEL aka RDM
// rdm_30@yahoo.com
//------------------------------------------------------------------------------
unit DCLUtil;

interface

uses DCL_Intf, SysUtils;

resourcestring
  SOutOfBounds = 'Out of bounds';
  SNoSuchElement = 'No such element';
  SIllegalState = 'Illegal state';
  SConcurrentModification = 'Concurrent modification';
  SIllegalArgument = 'Illegal argument';
  SOperationNotSupported = 'Operation not supported';

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

implementation


end.
