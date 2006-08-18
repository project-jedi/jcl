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
{ The Original Code is JclDebugResult.pas.                                     }
{                                                                              }
{ The Initial Developer of the Original Code is documented in the accompanying }
{ help file JCL.chm. Portions created by these individuals are Copyright (C)   }
{ of these individuals.                                                        }
{                                                                              }
{ Last modified: July 2, 2001                                                  }
{                                                                              }
{******************************************************************************}

unit JclDebugIdeResult;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ImgList;

type
  TJclDebugResultForm = class(TForm)
    OkBtn: TButton;
    ResultListView: TListView;
    ImageList1: TImageList;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  JclDebugResultForm: TJclDebugResultForm;

implementation

{$R *.dfm}

end.
