unit EDISDK_TLB;

// ************************************************************************ //
// WARNING                                                                    
// -------                                                                    
// The types declared in this file were generated from data read from a       
// Type Library. If this type library is explicitly or indirectly (via        
// another type library referring to this type library) re-imported, or the   
// 'Refresh' command of the Type Library Editor activated while editing the   
// Type Library, the contents of this file will be regenerated and all        
// manual modifications will be lost.                                         
// ************************************************************************ //

// PASTLWTR : $Revision$
// File generated on 17.7.2004 03:10:43 from Type Library described below.

// *************************************************************************//
// NOTE:                                                                      
// Items guarded by $IFDEF_LIVE_SERVER_AT_DESIGN_TIME are used by properties  
// which return objects that may need to be explicitly created via a function 
// call prior to any access via the property. These items have been disabled  
// in order to prevent accidental use from within the object inspector. You   
// may enable them by defining LIVE_SERVER_AT_DESIGN_TIME or by selectively   
// removing them from the $IFDEF blocks. However, such items must still be    
// programmatically created via a method of the appropriate CoClass before    
// they can be used.                                                          
// ************************************************************************ //
// Type Lib: I:\Quellen\jedi\jcl\examples\vcl\edisdk\comserver\EDISDK.dll (1)
// IID\LCID: {AF3BB992-62DF-41B7-92C7-FA41BDBB427E}\0
// Helpfile: 
// DepndLst: 
//   (1) v2.0 stdole, (F:\WINNT\system32\STDOLE2.TLB)
//   (2) v4.0 StdVCL, (F:\WINNT\system32\STDVCL40.DLL)
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
interface

uses Windows, ActiveX, Classes, Graphics, OleServer, OleCtrls, StdVCL;

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  EDISDKMajorVersion = 1;
  EDISDKMinorVersion = 0;

  LIBID_EDISDK: TGUID = '{AF3BB992-62DF-41B7-92C7-FA41BDBB427E}';

  IID_IEDICOMDelimiters: TGUID = '{A0181BBD-2F88-4FDC-9752-8303519D2D62}';
  CLASS_EDICOMDelimiters: TGUID = '{30B8A020-5D35-4ED8-B889-C13F309AE308}';
  IID_IEDICOMDataObject: TGUID = '{C7037767-05C8-4C6F-8201-655A6B5A4CF4}';
  IID_IEDICOMDataObjectGroup: TGUID = '{AEADBE04-6D1C-493E-BE6B-51E96BAD3680}';
  IID_IEDICOMElement: TGUID = '{E4ED3376-38AA-423C-9160-AAD190ACCB35}';
  CLASS_EDICOMElement: TGUID = '{4EFCADAA-60D0-4D61-875C-A27D6BCE932B}';
  IID_IEDICOMSegment: TGUID = '{467C692E-C22F-44B5-ACDB-C7A337B68675}';
  CLASS_EDICOMSegment: TGUID = '{63946EB6-DBDF-44FB-AAA4-123E7C2275B6}';
  IID_IEDICOMTransactionSet: TGUID = '{B2300104-4FF0-40A3-ABED-29E2A36C1844}';
  CLASS_EDICOMTransactionSet: TGUID = '{B540FDFC-B0D0-4E74-A7F4-B09DC260E656}';
  IID_IEDICOMFunctionalGroup: TGUID = '{C2FDB4EF-6252-4E67-BAD4-E7200B9CEA31}';
  CLASS_EDICOMFunctionalGroup: TGUID = '{C69EA833-88BF-4D55-AFC0-264F1B7ED54C}';
  IID_IEDICOMInterchangeControl: TGUID = '{B7FF3E84-8D1E-44F5-BC6A-578881CF7B5A}';
  CLASS_EDICOMInterchangeControl: TGUID = '{EF07065C-6E35-41B6-9564-D2D5714600A8}';
  IID_IEDICOMFile: TGUID = '{DEA6D2C3-98EE-4276-AA08-0AB4F1AEAC0F}';
  CLASS_EDICOMFile: TGUID = '{E8400822-5701-4226-8F78-A784B3777CB9}';

// *********************************************************************//
// Declaration of Enumerations defined in Type Library                    
// *********************************************************************//
// Constants for enum EDICOMDataObjectDataState
type
  EDICOMDataObjectDataState = TOleEnum;
const
  ediCreated = $00000000;
  ediAssembled = $00000001;
  ediDisassembled = $00000002;

// Constants for enum EDIFileOptions
type
  EDIFileOptions = TOleEnum;
const
  foNone = $00000000;
  foVariableDelimiterDetection = $00000001;
  foUseAltDelimiterDetection = $00000002;
  foRemoveCrLf = $00000004;
  foRemoveCr = $00000008;
  foRemoveLf = $00000010;
  foIgnoreGarbageAtEndOfFile = $00000020;

type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  IEDICOMDelimiters = interface;
  IEDICOMDelimitersDisp = dispinterface;
  IEDICOMDataObject = interface;
  IEDICOMDataObjectDisp = dispinterface;
  IEDICOMDataObjectGroup = interface;
  IEDICOMDataObjectGroupDisp = dispinterface;
  IEDICOMElement = interface;
  IEDICOMElementDisp = dispinterface;
  IEDICOMSegment = interface;
  IEDICOMSegmentDisp = dispinterface;
  IEDICOMTransactionSet = interface;
  IEDICOMTransactionSetDisp = dispinterface;
  IEDICOMFunctionalGroup = interface;
  IEDICOMFunctionalGroupDisp = dispinterface;
  IEDICOMInterchangeControl = interface;
  IEDICOMInterchangeControlDisp = dispinterface;
  IEDICOMFile = interface;
  IEDICOMFileDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  EDICOMDelimiters = IEDICOMDelimiters;
  EDICOMElement = IEDICOMElement;
  EDICOMSegment = IEDICOMSegment;
  EDICOMTransactionSet = IEDICOMTransactionSet;
  EDICOMFunctionalGroup = IEDICOMFunctionalGroup;
  EDICOMInterchangeControl = IEDICOMInterchangeControl;
  EDICOMFile = IEDICOMFile;


// *********************************************************************//
// Interface: IEDICOMDelimiters
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {A0181BBD-2F88-4FDC-9752-8303519D2D62}
// *********************************************************************//
  IEDICOMDelimiters = interface(IDispatch)
    ['{A0181BBD-2F88-4FDC-9752-8303519D2D62}']
    function Get_SD: WideString; safecall;
    procedure Set_SD(const Value: WideString); safecall;
    function Get_ED: WideString; safecall;
    procedure Set_ED(const Value: WideString); safecall;
    function Get_SS: WideString; safecall;
    procedure Set_SS(const Value: WideString); safecall;
    function Get_SDLen: Integer; safecall;
    function Get_EDLen: Integer; safecall;
    function Get_SSLen: Integer; safecall;
    property SD: WideString read Get_SD write Set_SD;
    property ED: WideString read Get_ED write Set_ED;
    property SS: WideString read Get_SS write Set_SS;
    property SDLen: Integer read Get_SDLen;
    property EDLen: Integer read Get_EDLen;
    property SSLen: Integer read Get_SSLen;
  end;

// *********************************************************************//
// DispIntf:  IEDICOMDelimitersDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {A0181BBD-2F88-4FDC-9752-8303519D2D62}
// *********************************************************************//
  IEDICOMDelimitersDisp = dispinterface
    ['{A0181BBD-2F88-4FDC-9752-8303519D2D62}']
    property SD: WideString dispid 201;
    property ED: WideString dispid 202;
    property SS: WideString dispid 203;
    property SDLen: Integer readonly dispid 204;
    property EDLen: Integer readonly dispid 205;
    property SSLen: Integer readonly dispid 206;
  end;

// *********************************************************************//
// Interface: IEDICOMDataObject
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {C7037767-05C8-4C6F-8201-655A6B5A4CF4}
// *********************************************************************//
  IEDICOMDataObject = interface(IDispatch)
    ['{C7037767-05C8-4C6F-8201-655A6B5A4CF4}']
    function Assemble: WideString; safecall;
    procedure Disassemble; safecall;
    function Get_State: Integer; safecall;
    function Get_Data: WideString; safecall;
    procedure Set_Data(const Value: WideString); safecall;
    function Get_DataLength: Integer; safecall;
    function Get_Delimiters: IEDICOMDelimiters; safecall;
    property State: Integer read Get_State;
    property Data: WideString read Get_Data write Set_Data;
    property DataLength: Integer read Get_DataLength;
    property Delimiters: IEDICOMDelimiters read Get_Delimiters;
  end;

// *********************************************************************//
// DispIntf:  IEDICOMDataObjectDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {C7037767-05C8-4C6F-8201-655A6B5A4CF4}
// *********************************************************************//
  IEDICOMDataObjectDisp = dispinterface
    ['{C7037767-05C8-4C6F-8201-655A6B5A4CF4}']
    function Assemble: WideString; dispid 201;
    procedure Disassemble; dispid 202;
    property State: Integer readonly dispid 203;
    property Data: WideString dispid 205;
    property DataLength: Integer readonly dispid 204;
    property Delimiters: IEDICOMDelimiters readonly dispid 206;
  end;

// *********************************************************************//
// Interface: IEDICOMDataObjectGroup
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {AEADBE04-6D1C-493E-BE6B-51E96BAD3680}
// *********************************************************************//
  IEDICOMDataObjectGroup = interface(IEDICOMDataObject)
    ['{AEADBE04-6D1C-493E-BE6B-51E96BAD3680}']
  end;

// *********************************************************************//
// DispIntf:  IEDICOMDataObjectGroupDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {AEADBE04-6D1C-493E-BE6B-51E96BAD3680}
// *********************************************************************//
  IEDICOMDataObjectGroupDisp = dispinterface
    ['{AEADBE04-6D1C-493E-BE6B-51E96BAD3680}']
    function Assemble: WideString; dispid 201;
    procedure Disassemble; dispid 202;
    property State: Integer readonly dispid 203;
    property Data: WideString dispid 205;
    property DataLength: Integer readonly dispid 204;
    property Delimiters: IEDICOMDelimiters readonly dispid 206;
  end;

// *********************************************************************//
// Interface: IEDICOMElement
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {E4ED3376-38AA-423C-9160-AAD190ACCB35}
// *********************************************************************//
  IEDICOMElement = interface(IEDICOMDataObject)
    ['{E4ED3376-38AA-423C-9160-AAD190ACCB35}']
  end;

// *********************************************************************//
// DispIntf:  IEDICOMElementDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {E4ED3376-38AA-423C-9160-AAD190ACCB35}
// *********************************************************************//
  IEDICOMElementDisp = dispinterface
    ['{E4ED3376-38AA-423C-9160-AAD190ACCB35}']
    function Assemble: WideString; dispid 201;
    procedure Disassemble; dispid 202;
    property State: Integer readonly dispid 203;
    property Data: WideString dispid 205;
    property DataLength: Integer readonly dispid 204;
    property Delimiters: IEDICOMDelimiters readonly dispid 206;
  end;

// *********************************************************************//
// Interface: IEDICOMSegment
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {467C692E-C22F-44B5-ACDB-C7A337B68675}
// *********************************************************************//
  IEDICOMSegment = interface(IEDICOMDataObjectGroup)
    ['{467C692E-C22F-44B5-ACDB-C7A337B68675}']
    function Get_Element(Index: Integer): IEDICOMElement; safecall;
    function Get_SegmentId: WideString; safecall;
    procedure Set_SegmentId(const Value: WideString); safecall;
    function AddElement: Integer; safecall;
    function InsertElement(InsertIndex: Integer): Integer; safecall;
    procedure DeleteElement(Index: Integer); safecall;
    function AddElements(Count: Integer): Integer; safecall;
    function InsertElements(InsertIndex: Integer; Count: Integer): Integer; safecall;
    procedure DeleteElements; safecall;
    function Get_ElementCount: Integer; safecall;
    property Element[Index: Integer]: IEDICOMElement read Get_Element;
    property SegmentId: WideString read Get_SegmentId write Set_SegmentId;
    property ElementCount: Integer read Get_ElementCount;
  end;

// *********************************************************************//
// DispIntf:  IEDICOMSegmentDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {467C692E-C22F-44B5-ACDB-C7A337B68675}
// *********************************************************************//
  IEDICOMSegmentDisp = dispinterface
    ['{467C692E-C22F-44B5-ACDB-C7A337B68675}']
    property Element[Index: Integer]: IEDICOMElement readonly dispid 401;
    property SegmentId: WideString dispid 402;
    function AddElement: Integer; dispid 403;
    function InsertElement(InsertIndex: Integer): Integer; dispid 404;
    procedure DeleteElement(Index: Integer); dispid 405;
    function AddElements(Count: Integer): Integer; dispid 406;
    function InsertElements(InsertIndex: Integer; Count: Integer): Integer; dispid 407;
    procedure DeleteElements; dispid 408;
    property ElementCount: Integer readonly dispid 409;
    function Assemble: WideString; dispid 201;
    procedure Disassemble; dispid 202;
    property State: Integer readonly dispid 203;
    property Data: WideString dispid 205;
    property DataLength: Integer readonly dispid 204;
    property Delimiters: IEDICOMDelimiters readonly dispid 206;
  end;

// *********************************************************************//
// Interface: IEDICOMTransactionSet
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {B2300104-4FF0-40A3-ABED-29E2A36C1844}
// *********************************************************************//
  IEDICOMTransactionSet = interface(IEDICOMDataObjectGroup)
    ['{B2300104-4FF0-40A3-ABED-29E2A36C1844}']
    function Get_SegmentST: IEDICOMSegment; safecall;
    function Get_SegmentSE: IEDICOMSegment; safecall;
    function Get_Segment(Index: Integer): IEDICOMSegment; safecall;
    function AddSegment: Integer; safecall;
    function InsertSegment(InsertIndex: Integer): Integer; safecall;
    procedure DeleteSegment(Index: Integer); safecall;
    function AddSegments(Count: Integer): Integer; safecall;
    function InsertSegments(InsertIndex: Integer; Count: Integer): Integer; safecall;
    procedure DeleteSegments; safecall;
    function Get_SegmentCount: Integer; safecall;
    property SegmentST: IEDICOMSegment read Get_SegmentST;
    property SegmentSE: IEDICOMSegment read Get_SegmentSE;
    property Segment[Index: Integer]: IEDICOMSegment read Get_Segment;
    property SegmentCount: Integer read Get_SegmentCount;
  end;

// *********************************************************************//
// DispIntf:  IEDICOMTransactionSetDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {B2300104-4FF0-40A3-ABED-29E2A36C1844}
// *********************************************************************//
  IEDICOMTransactionSetDisp = dispinterface
    ['{B2300104-4FF0-40A3-ABED-29E2A36C1844}']
    property SegmentST: IEDICOMSegment readonly dispid 401;
    property SegmentSE: IEDICOMSegment readonly dispid 402;
    property Segment[Index: Integer]: IEDICOMSegment readonly dispid 403;
    function AddSegment: Integer; dispid 404;
    function InsertSegment(InsertIndex: Integer): Integer; dispid 405;
    procedure DeleteSegment(Index: Integer); dispid 406;
    function AddSegments(Count: Integer): Integer; dispid 407;
    function InsertSegments(InsertIndex: Integer; Count: Integer): Integer; dispid 408;
    procedure DeleteSegments; dispid 409;
    property SegmentCount: Integer readonly dispid 410;
    function Assemble: WideString; dispid 201;
    procedure Disassemble; dispid 202;
    property State: Integer readonly dispid 203;
    property Data: WideString dispid 205;
    property DataLength: Integer readonly dispid 204;
    property Delimiters: IEDICOMDelimiters readonly dispid 206;
  end;

// *********************************************************************//
// Interface: IEDICOMFunctionalGroup
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {C2FDB4EF-6252-4E67-BAD4-E7200B9CEA31}
// *********************************************************************//
  IEDICOMFunctionalGroup = interface(IEDICOMDataObjectGroup)
    ['{C2FDB4EF-6252-4E67-BAD4-E7200B9CEA31}']
    function Get_SegmentGS: IEDICOMSegment; safecall;
    function Get_SegmentGE: IEDICOMSegment; safecall;
    function Get_TransactionSet(Index: Integer): IEDICOMTransactionSet; safecall;
    function AddTransactionSet: Integer; safecall;
    function InsertTransactionSet(InsertIndex: Integer): Integer; safecall;
    procedure DeleteTransactionSet(Index: Integer); safecall;
    function AddTransactionSets(Count: Integer): Integer; safecall;
    function InsertTransactionSets(InsertIndex: Integer; Count: Integer): Integer; safecall;
    procedure DeleteTransactionSets; safecall;
    function Get_TransactionSetCount: Integer; safecall;
    property SegmentGS: IEDICOMSegment read Get_SegmentGS;
    property SegmentGE: IEDICOMSegment read Get_SegmentGE;
    property TransactionSet[Index: Integer]: IEDICOMTransactionSet read Get_TransactionSet;
    property TransactionSetCount: Integer read Get_TransactionSetCount;
  end;

// *********************************************************************//
// DispIntf:  IEDICOMFunctionalGroupDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {C2FDB4EF-6252-4E67-BAD4-E7200B9CEA31}
// *********************************************************************//
  IEDICOMFunctionalGroupDisp = dispinterface
    ['{C2FDB4EF-6252-4E67-BAD4-E7200B9CEA31}']
    property SegmentGS: IEDICOMSegment readonly dispid 401;
    property SegmentGE: IEDICOMSegment readonly dispid 402;
    property TransactionSet[Index: Integer]: IEDICOMTransactionSet readonly dispid 403;
    function AddTransactionSet: Integer; dispid 404;
    function InsertTransactionSet(InsertIndex: Integer): Integer; dispid 405;
    procedure DeleteTransactionSet(Index: Integer); dispid 406;
    function AddTransactionSets(Count: Integer): Integer; dispid 407;
    function InsertTransactionSets(InsertIndex: Integer; Count: Integer): Integer; dispid 408;
    procedure DeleteTransactionSets; dispid 409;
    property TransactionSetCount: Integer readonly dispid 410;
    function Assemble: WideString; dispid 201;
    procedure Disassemble; dispid 202;
    property State: Integer readonly dispid 203;
    property Data: WideString dispid 205;
    property DataLength: Integer readonly dispid 204;
    property Delimiters: IEDICOMDelimiters readonly dispid 206;
  end;

// *********************************************************************//
// Interface: IEDICOMInterchangeControl
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {B7FF3E84-8D1E-44F5-BC6A-578881CF7B5A}
// *********************************************************************//
  IEDICOMInterchangeControl = interface(IEDICOMDataObjectGroup)
    ['{B7FF3E84-8D1E-44F5-BC6A-578881CF7B5A}']
    function Get_SegmentISA: IEDICOMSegment; safecall;
    function Get_SegmentIEA: IEDICOMSegment; safecall;
    function Get_FunctionalGroup(Index: Integer): IEDICOMFunctionalGroup; safecall;
    function AddFunctionalGroup: Integer; safecall;
    function InsertFunctionalGroup(InsertIndex: Integer): Integer; safecall;
    procedure DeleteFunctionalGroup(Index: Integer); safecall;
    function AddFunctionalGroups(InsertIndex: Integer): Integer; safecall;
    function InsertFunctionalGroups(InsertIndex: Integer; Count: Integer): Integer; safecall;
    procedure DeleteFunctionalGroups; safecall;
    procedure SetDelimiters(const SD: WideString; const ED: WideString; const SS: WideString); safecall;
    function Get_FunctionalGroupCount: Integer; safecall;
    property SegmentISA: IEDICOMSegment read Get_SegmentISA;
    property SegmentIEA: IEDICOMSegment read Get_SegmentIEA;
    property FunctionalGroup[Index: Integer]: IEDICOMFunctionalGroup read Get_FunctionalGroup;
    property FunctionalGroupCount: Integer read Get_FunctionalGroupCount;
  end;

// *********************************************************************//
// DispIntf:  IEDICOMInterchangeControlDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {B7FF3E84-8D1E-44F5-BC6A-578881CF7B5A}
// *********************************************************************//
  IEDICOMInterchangeControlDisp = dispinterface
    ['{B7FF3E84-8D1E-44F5-BC6A-578881CF7B5A}']
    property SegmentISA: IEDICOMSegment readonly dispid 401;
    property SegmentIEA: IEDICOMSegment readonly dispid 402;
    property FunctionalGroup[Index: Integer]: IEDICOMFunctionalGroup readonly dispid 403;
    function AddFunctionalGroup: Integer; dispid 404;
    function InsertFunctionalGroup(InsertIndex: Integer): Integer; dispid 405;
    procedure DeleteFunctionalGroup(Index: Integer); dispid 406;
    function AddFunctionalGroups(InsertIndex: Integer): Integer; dispid 407;
    function InsertFunctionalGroups(InsertIndex: Integer; Count: Integer): Integer; dispid 408;
    procedure DeleteFunctionalGroups; dispid 409;
    procedure SetDelimiters(const SD: WideString; const ED: WideString; const SS: WideString); dispid 410;
    property FunctionalGroupCount: Integer readonly dispid 411;
    function Assemble: WideString; dispid 201;
    procedure Disassemble; dispid 202;
    property State: Integer readonly dispid 203;
    property Data: WideString dispid 205;
    property DataLength: Integer readonly dispid 204;
    property Delimiters: IEDICOMDelimiters readonly dispid 206;
  end;

// *********************************************************************//
// Interface: IEDICOMFile
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {DEA6D2C3-98EE-4276-AA08-0AB4F1AEAC0F}
// *********************************************************************//
  IEDICOMFile = interface(IEDICOMDataObjectGroup)
    ['{DEA6D2C3-98EE-4276-AA08-0AB4F1AEAC0F}']
    procedure LoadFromFile(const FileName: WideString); safecall;
    procedure ReLoadFromFile; safecall;
    procedure SaveToFile; safecall;
    procedure SaveAsToFile(const FileName: WideString); safecall;
    function Get_FileName: WideString; safecall;
    procedure Set_FileName(const Value: WideString); safecall;
    function Get_Interchange(Index: Integer): IEDICOMInterchangeControl; safecall;
    function Get_Options: Byte; safecall;
    procedure Set_Options(Value: Byte); safecall;
    function AddInterchange: Integer; safecall;
    function InsertInterchange(InsertIndex: Integer): Integer; safecall;
    procedure DeleteInterchange(Index: Integer); safecall;
    function AddInterchanges(Count: Integer): Integer; safecall;
    function InsertInterchanges(InsertIndex: Integer; Count: Integer): Integer; safecall;
    procedure DeleteInterchanges; safecall;
    function Get_InterchangeCount: Integer; safecall;
    property FileName: WideString read Get_FileName write Set_FileName;
    property Interchange[Index: Integer]: IEDICOMInterchangeControl read Get_Interchange;
    property Options: Byte read Get_Options write Set_Options;
    property InterchangeCount: Integer read Get_InterchangeCount;
  end;

// *********************************************************************//
// DispIntf:  IEDICOMFileDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {DEA6D2C3-98EE-4276-AA08-0AB4F1AEAC0F}
// *********************************************************************//
  IEDICOMFileDisp = dispinterface
    ['{DEA6D2C3-98EE-4276-AA08-0AB4F1AEAC0F}']
    procedure LoadFromFile(const FileName: WideString); dispid 401;
    procedure ReLoadFromFile; dispid 402;
    procedure SaveToFile; dispid 403;
    procedure SaveAsToFile(const FileName: WideString); dispid 404;
    property FileName: WideString dispid 405;
    property Interchange[Index: Integer]: IEDICOMInterchangeControl readonly dispid 406;
    property Options: Byte dispid 407;
    function AddInterchange: Integer; dispid 408;
    function InsertInterchange(InsertIndex: Integer): Integer; dispid 409;
    procedure DeleteInterchange(Index: Integer); dispid 410;
    function AddInterchanges(Count: Integer): Integer; dispid 411;
    function InsertInterchanges(InsertIndex: Integer; Count: Integer): Integer; dispid 412;
    procedure DeleteInterchanges; dispid 413;
    property InterchangeCount: Integer readonly dispid 414;
    function Assemble: WideString; dispid 201;
    procedure Disassemble; dispid 202;
    property State: Integer readonly dispid 203;
    property Data: WideString dispid 205;
    property DataLength: Integer readonly dispid 204;
    property Delimiters: IEDICOMDelimiters readonly dispid 206;
  end;

// *********************************************************************//
// The Class CoEDICOMDelimiters provides a Create and CreateRemote method to          
// create instances of the default interface IEDICOMDelimiters exposed by              
// the CoClass EDICOMDelimiters. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoEDICOMDelimiters = class
    class function Create: IEDICOMDelimiters;
    class function CreateRemote(const MachineName: string): IEDICOMDelimiters;
  end;

// *********************************************************************//
// The Class CoEDICOMElement provides a Create and CreateRemote method to          
// create instances of the default interface IEDICOMElement exposed by              
// the CoClass EDICOMElement. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoEDICOMElement = class
    class function Create: IEDICOMElement;
    class function CreateRemote(const MachineName: string): IEDICOMElement;
  end;

// *********************************************************************//
// The Class CoEDICOMSegment provides a Create and CreateRemote method to          
// create instances of the default interface IEDICOMSegment exposed by              
// the CoClass EDICOMSegment. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoEDICOMSegment = class
    class function Create: IEDICOMSegment;
    class function CreateRemote(const MachineName: string): IEDICOMSegment;
  end;

// *********************************************************************//
// The Class CoEDICOMTransactionSet provides a Create and CreateRemote method to          
// create instances of the default interface IEDICOMTransactionSet exposed by              
// the CoClass EDICOMTransactionSet. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoEDICOMTransactionSet = class
    class function Create: IEDICOMTransactionSet;
    class function CreateRemote(const MachineName: string): IEDICOMTransactionSet;
  end;

// *********************************************************************//
// The Class CoEDICOMFunctionalGroup provides a Create and CreateRemote method to          
// create instances of the default interface IEDICOMFunctionalGroup exposed by              
// the CoClass EDICOMFunctionalGroup. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoEDICOMFunctionalGroup = class
    class function Create: IEDICOMFunctionalGroup;
    class function CreateRemote(const MachineName: string): IEDICOMFunctionalGroup;
  end;

// *********************************************************************//
// The Class CoEDICOMInterchangeControl provides a Create and CreateRemote method to          
// create instances of the default interface IEDICOMInterchangeControl exposed by              
// the CoClass EDICOMInterchangeControl. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoEDICOMInterchangeControl = class
    class function Create: IEDICOMInterchangeControl;
    class function CreateRemote(const MachineName: string): IEDICOMInterchangeControl;
  end;

// *********************************************************************//
// The Class CoEDICOMFile provides a Create and CreateRemote method to          
// create instances of the default interface IEDICOMFile exposed by              
// the CoClass EDICOMFile. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoEDICOMFile = class
    class function Create: IEDICOMFile;
    class function CreateRemote(const MachineName: string): IEDICOMFile;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TEDICOMFile
// Help String      : 
// Default Interface: IEDICOMFile
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TEDICOMFileProperties= class;
{$ENDIF}
  TEDICOMFile = class(TOleServer)
  private
    FIntf: IEDICOMFile;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps:       TEDICOMFileProperties;
    function      GetServerProperties: TEDICOMFileProperties;
{$ENDIF}
    function GetDefaultInterface: IEDICOMFile;
  protected
    procedure InitServerData; override;
    function Get_State: Integer;
    function Get_Data: WideString;
    procedure Set_Data(const Value: WideString);
    function Get_DataLength: Integer;
    function Get_Delimiters: IEDICOMDelimiters;
    function Get_FileName: WideString;
    procedure Set_FileName(const Value: WideString);
    function Get_Interchange(Index: Integer): IEDICOMInterchangeControl;
    function Get_Options: Byte;
    procedure Set_Options(Value: Byte);
    function Get_InterchangeCount: Integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IEDICOMFile);
    procedure Disconnect; override;
    function Assemble: WideString;
    procedure Disassemble;
    procedure LoadFromFile(const FileName: WideString);
    procedure ReLoadFromFile;
    procedure SaveToFile;
    procedure SaveAsToFile(const FileName: WideString);
    function AddInterchange: Integer;
    function InsertInterchange(InsertIndex: Integer): Integer;
    procedure DeleteInterchange(Index: Integer);
    function AddInterchanges(Count: Integer): Integer;
    function InsertInterchanges(InsertIndex: Integer; Count: Integer): Integer;
    procedure DeleteInterchanges;
    property DefaultInterface: IEDICOMFile read GetDefaultInterface;
    property State: Integer read Get_State;
    property DataLength: Integer read Get_DataLength;
    property Delimiters: IEDICOMDelimiters read Get_Delimiters;
    property Interchange[Index: Integer]: IEDICOMInterchangeControl read Get_Interchange;
    property InterchangeCount: Integer read Get_InterchangeCount;
    property Data: WideString read Get_Data write Set_Data;
    property FileName: WideString read Get_FileName write Set_FileName;
    property Options: Byte read Get_Options write Set_Options;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TEDICOMFileProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TEDICOMFile
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TEDICOMFileProperties = class(TPersistent)
  private
    FServer:    TEDICOMFile;
    function    GetDefaultInterface: IEDICOMFile;
    constructor Create(AServer: TEDICOMFile);
  protected
    function  Get_State: Integer;
    function  Get_Data: WideString;
    procedure Set_Data(const Value: WideString);
    function  Get_DataLength: Integer;
    function  Get_Delimiters: IEDICOMDelimiters;
    function  Get_FileName: WideString;
    procedure Set_FileName(const Value: WideString);
    function  Get_Interchange(Index: Integer): IEDICOMInterchangeControl;
    function  Get_Options: Byte;
    procedure Set_Options(Value: Byte);
    function  Get_InterchangeCount: Integer;
  public
    property DefaultInterface: IEDICOMFile read GetDefaultInterface;
  published
    property Data: WideString read Get_Data write Set_Data;
    property FileName: WideString read Get_FileName write Set_FileName;
    property Options: Byte read Get_Options write Set_Options;
  end;
{$ENDIF}


procedure Register;

implementation

uses ComObj;

class function CoEDICOMDelimiters.Create: IEDICOMDelimiters;
begin
  Result := CreateComObject(CLASS_EDICOMDelimiters) as IEDICOMDelimiters;
end;

class function CoEDICOMDelimiters.CreateRemote(const MachineName: string): IEDICOMDelimiters;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_EDICOMDelimiters) as IEDICOMDelimiters;
end;

class function CoEDICOMElement.Create: IEDICOMElement;
begin
  Result := CreateComObject(CLASS_EDICOMElement) as IEDICOMElement;
end;

class function CoEDICOMElement.CreateRemote(const MachineName: string): IEDICOMElement;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_EDICOMElement) as IEDICOMElement;
end;

class function CoEDICOMSegment.Create: IEDICOMSegment;
begin
  Result := CreateComObject(CLASS_EDICOMSegment) as IEDICOMSegment;
end;

class function CoEDICOMSegment.CreateRemote(const MachineName: string): IEDICOMSegment;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_EDICOMSegment) as IEDICOMSegment;
end;

class function CoEDICOMTransactionSet.Create: IEDICOMTransactionSet;
begin
  Result := CreateComObject(CLASS_EDICOMTransactionSet) as IEDICOMTransactionSet;
end;

class function CoEDICOMTransactionSet.CreateRemote(const MachineName: string): IEDICOMTransactionSet;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_EDICOMTransactionSet) as IEDICOMTransactionSet;
end;

class function CoEDICOMFunctionalGroup.Create: IEDICOMFunctionalGroup;
begin
  Result := CreateComObject(CLASS_EDICOMFunctionalGroup) as IEDICOMFunctionalGroup;
end;

class function CoEDICOMFunctionalGroup.CreateRemote(const MachineName: string): IEDICOMFunctionalGroup;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_EDICOMFunctionalGroup) as IEDICOMFunctionalGroup;
end;

class function CoEDICOMInterchangeControl.Create: IEDICOMInterchangeControl;
begin
  Result := CreateComObject(CLASS_EDICOMInterchangeControl) as IEDICOMInterchangeControl;
end;

class function CoEDICOMInterchangeControl.CreateRemote(const MachineName: string): IEDICOMInterchangeControl;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_EDICOMInterchangeControl) as IEDICOMInterchangeControl;
end;

class function CoEDICOMFile.Create: IEDICOMFile;
begin
  Result := CreateComObject(CLASS_EDICOMFile) as IEDICOMFile;
end;

class function CoEDICOMFile.CreateRemote(const MachineName: string): IEDICOMFile;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_EDICOMFile) as IEDICOMFile;
end;

procedure TEDICOMFile.InitServerData;
const
  CServerData: TServerData = (
    ClassID: '{E8400822-5701-4226-8F78-A784B3777CB9}';
    IntfIID: '{DEA6D2C3-98EE-4276-AA08-0AB4F1AEAC0F}';
    EventIID: '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TEDICOMFile.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf := punk as IEDICOMFile;
  end;
end;

procedure TEDICOMFile.ConnectTo(svrIntf: IEDICOMFile);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TEDICOMFile.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TEDICOMFile.GetDefaultInterface: IEDICOMFile;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call ''Connect'' or ''ConnectTo'' before this operation');
  Result := FIntf;
end;

constructor TEDICOMFile.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TEDICOMFileProperties.Create(Self);
{$ENDIF}
end;

destructor TEDICOMFile.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TEDICOMFile.GetServerProperties: TEDICOMFileProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function TEDICOMFile.Get_State: Integer;
begin
  Result := DefaultInterface.Get_State;
end;

function TEDICOMFile.Get_Data: WideString;
begin
  Result := DefaultInterface.Get_Data;
end;

procedure TEDICOMFile.Set_Data(const Value: WideString);
begin
  DefaultInterface.Set_Data(Value);
end;

function TEDICOMFile.Get_DataLength: Integer;
begin
  Result := DefaultInterface.Get_DataLength;
end;

function TEDICOMFile.Get_Delimiters: IEDICOMDelimiters;
begin
  Result := DefaultInterface.Get_Delimiters;
end;

function TEDICOMFile.Get_FileName: WideString;
begin
  Result := DefaultInterface.Get_FileName;
end;

procedure TEDICOMFile.Set_FileName(const Value: WideString);
begin
  DefaultInterface.Set_FileName(Value);
end;

function TEDICOMFile.Get_Interchange(Index: Integer): IEDICOMInterchangeControl;
begin
  Result := DefaultInterface.Get_Interchange(Index);
end;

function TEDICOMFile.Get_Options: Byte;
begin
  Result := DefaultInterface.Get_Options;
end;

procedure TEDICOMFile.Set_Options(Value: Byte);
begin
  DefaultInterface.Set_Options(Value);
end;

function TEDICOMFile.Get_InterchangeCount: Integer;
begin
  Result := DefaultInterface.Get_InterchangeCount;
end;

function TEDICOMFile.Assemble: WideString;
begin
  Result := DefaultInterface.Assemble;
end;

procedure TEDICOMFile.Disassemble;
begin
  DefaultInterface.Disassemble;
end;

procedure TEDICOMFile.LoadFromFile(const FileName: WideString);
begin
  DefaultInterface.LoadFromFile(FileName);
end;

procedure TEDICOMFile.ReLoadFromFile;
begin
  DefaultInterface.ReLoadFromFile;
end;

procedure TEDICOMFile.SaveToFile;
begin
  DefaultInterface.SaveToFile;
end;

procedure TEDICOMFile.SaveAsToFile(const FileName: WideString);
begin
  DefaultInterface.SaveAsToFile(FileName);
end;

function TEDICOMFile.AddInterchange: Integer;
begin
  Result := DefaultInterface.AddInterchange;
end;

function TEDICOMFile.InsertInterchange(InsertIndex: Integer): Integer;
begin
  Result := DefaultInterface.InsertInterchange(InsertIndex);
end;

procedure TEDICOMFile.DeleteInterchange(Index: Integer);
begin
  DefaultInterface.DeleteInterchange(Index);
end;

function TEDICOMFile.AddInterchanges(Count: Integer): Integer;
begin
  Result := DefaultInterface.AddInterchanges(Count);
end;

function TEDICOMFile.InsertInterchanges(InsertIndex: Integer; Count: Integer): Integer;
begin
  Result := DefaultInterface.InsertInterchanges(InsertIndex, Count);
end;

procedure TEDICOMFile.DeleteInterchanges;
begin
  DefaultInterface.DeleteInterchanges;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TEDICOMFileProperties.Create(AServer: TEDICOMFile);
begin
  inherited Create;
  FServer := AServer;
end;

function TEDICOMFileProperties.GetDefaultInterface: IEDICOMFile;
begin
  Result := FServer.DefaultInterface;
end;

function  TEDICOMFileProperties.Get_State: Integer;
begin
  Result := DefaultInterface.Get_State;
end;

function  TEDICOMFileProperties.Get_Data: WideString;
begin
  Result := DefaultInterface.Get_Data;
end;

procedure TEDICOMFileProperties.Set_Data(const Value: WideString);
begin
  DefaultInterface.Set_Data(Value);
end;

function  TEDICOMFileProperties.Get_DataLength: Integer;
begin
  Result := DefaultInterface.Get_DataLength;
end;

function  TEDICOMFileProperties.Get_Delimiters: IEDICOMDelimiters;
begin
  Result := DefaultInterface.Get_Delimiters;
end;

function  TEDICOMFileProperties.Get_FileName: WideString;
begin
  Result := DefaultInterface.Get_FileName;
end;

procedure TEDICOMFileProperties.Set_FileName(const Value: WideString);
begin
  DefaultInterface.Set_FileName(Value);
end;

function  TEDICOMFileProperties.Get_Interchange(Index: Integer): IEDICOMInterchangeControl;
begin
  Result := DefaultInterface.Get_Interchange(Index);
end;

function  TEDICOMFileProperties.Get_Options: Byte;
begin
  Result := DefaultInterface.Get_Options;
end;

procedure TEDICOMFileProperties.Set_Options(Value: Byte);
begin
  DefaultInterface.Set_Options(Value);
end;

function  TEDICOMFileProperties.Get_InterchangeCount: Integer;
begin
  Result := DefaultInterface.Get_InterchangeCount;
end;

{$ENDIF}

procedure Register;
begin
  RegisterComponents('ActiveX', [TEDICOMFile]);
end;

end.
