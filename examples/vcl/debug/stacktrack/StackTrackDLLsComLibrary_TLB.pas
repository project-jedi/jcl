unit StackTrackDLLsComLibrary_TLB;

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

// PASTLWTR : 1.2
// File generated on 21.12.2004 17:45:42 from Type Library described below.

// ************************************************************************  //
// Type Lib: I:\Quellen\jedi\jcl\examples\vcl\debug\stacktrack\StackTrackDLLsComLibrary.tlb (1)
// LIBID: {D4935E5D-790E-48CA-B360-0165C1305153}
// LCID: 0
// Helpfile: 
// HelpString: StackTrackDLLsComLibrary Library
// DepndLst: 
//   (1) v2.0 stdole, (F:\WINNT\system32\stdole2.tlb)
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
interface

uses Windows, ActiveX, Classes, Graphics, StdVCL, Variants;
  

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  StackTrackDLLsComLibraryMajorVersion = 1;
  StackTrackDLLsComLibraryMinorVersion = 0;

  LIBID_StackTrackDLLsComLibrary: TGUID = '{D4935E5D-790E-48CA-B360-0165C1305153}';

  IID_IStackTrackDllsTest: TGUID = '{26473046-CCEB-4671-9AB1-2216EF4D2164}';
  CLASS_StackTrackDllsTest: TGUID = '{DA3AEC52-1481-4119-B140-2157C7ADEC5B}';
type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  IStackTrackDllsTest = interface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  StackTrackDllsTest = IStackTrackDllsTest;


// *********************************************************************//
// Interface: IStackTrackDllsTest
// Flags:     (256) OleAutomation
// GUID:      {26473046-CCEB-4671-9AB1-2216EF4D2164}
// *********************************************************************//
  IStackTrackDllsTest = interface(IUnknown)
    ['{26473046-CCEB-4671-9AB1-2216EF4D2164}']
    function Error1: HResult; stdcall;
    function Error2: HResult; stdcall;
  end;

// *********************************************************************//
// The Class CoStackTrackDllsTest provides a Create and CreateRemote method to          
// create instances of the default interface IStackTrackDllsTest exposed by              
// the CoClass StackTrackDllsTest. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoStackTrackDllsTest = class
    class function Create: IStackTrackDllsTest;
    class function CreateRemote(const MachineName: string): IStackTrackDllsTest;
  end;

implementation

uses ComObj;

class function CoStackTrackDllsTest.Create: IStackTrackDllsTest;
begin
  Result := CreateComObject(CLASS_StackTrackDllsTest) as IStackTrackDllsTest;
end;

class function CoStackTrackDllsTest.CreateRemote(const MachineName: string): IStackTrackDllsTest;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_StackTrackDllsTest) as IStackTrackDllsTest;
end;

end.
