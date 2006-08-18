{******************************************************************************}
{                                                                              }
{ Project JEDI Code Library (JCL) extension                                    }
{                                                                              }
{ The contents of this file are subject to the Mozilla Public License Version  }
{ 1.0 (the "License"); you may not use this file except in compliance with the }
{ License. You may obtain a copy of the License at http://www.mozilla.org/MPL/ }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ The Original Code is JclDsgnD4.pas.                                          }
{                                                                              }
{ The Initial Developer of the Original Code is documented in the accompanying }
{ help file JCL.chm. Portions created by these individuals are Copyright (C)   }
{ of these individuals.                                                        }
{                                                                              }
{ Last modified: July 15, 2001                                                 }
{                                                                              }
{******************************************************************************}

unit JclDsgnD4;

{$I JCL.INC}

interface

uses
  Classes;

type
  TNotifierObject = class(TInterfacedObject)
  public
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;
  end;

implementation

{ TNotifierObject }

procedure TNotifierObject.AfterSave;
begin
end;

procedure TNotifierObject.BeforeSave;
begin
end;

procedure TNotifierObject.Destroyed;
begin
end;

procedure TNotifierObject.Modified;
begin
end;

end.
