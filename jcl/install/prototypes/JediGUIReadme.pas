{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL) extension                                                        }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is JediGUIReadme.pas.                                                          }
{                                                                                                  }
{ The Initial Developer of the Original Code is Florent Ouchet. Portions created by Florent Ouchet }
{ are Copyright (C) of Florent Ouchet. All Rights Reserved.                                        }
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

{$IFNDEF PROTOTYPE}
{$IFDEF VCL}
unit JediGUIReadme;
{$ELSE VisualCLX}
unit QJediGUIReadme;
{$ENDIF VisualCLX}
{$ENDIF ~PROTOTYPE}

{$I jcl.inc}
{$I crossplatform.inc}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows, Messages,
  {$ENDIF MSWINDOWS}
  SysUtils, Classes,
  {$IFDEF VisualCLX}
  Qt, QGraphics, QControls, QForms, QDialogs, QStdCtrls, QComCtrls,
  {$ELSE ~VisualCLX}
  Graphics, Controls, Forms, Dialogs, StdCtrls, ComCtrls,
  {$ENDIF ~VisualCLX}
  JediInstall;

type
  TReadmeFrame = class(TFrame, IJediReadmePage, IJediPage)
    {$IFDEF VCL}
    ReadmePane: TRichEdit;
    {$ELSE ~VCL}
    ReadmePane: TTextViewer;
    {$ENDIF ~VCL}
    procedure ReadmePaneDblClick(Sender: TObject);
  private
    FReadmeFileName: string;
  public
    // IJediPage
    function GetCaption: string;
    procedure SetCaption(const Value: string);
    function GetHintAtPos(ScreenX, ScreenY: Integer): string;
    procedure Show;
    // IJediReadmePage
    procedure SetReadmeFileName(const Value: string);
    function GetReadmeFileName: string;

    property ReadmeFileName: string read GetReadmeFileName write SetReadmeFileName;
  end;

implementation

{$IFDEF VCL}
{$R *.dfm}
{$ELSE ~VCL}
{$R *.xfm}
{$ENDIF ~VCL}

{$IFDEF MSWINDOWS}
uses
  JclShell;
{$ENDIF MSWINDOWS}

function TReadmeFrame.GetCaption: string;
begin
  Result := (Parent as TTabSheet).Caption;
end;

function TReadmeFrame.GetReadmeFileName: string;
begin
  Result := FReadmeFileName;
end;

procedure TReadmeFrame.ReadmePaneDblClick(Sender: TObject);
begin
  { TODO: implement for Unix }
  {$IFDEF MSWINDOWS}
  ShellExecEx(ReadmeFileName);
  {$ENDIF MSWINDOWS}
end;

procedure TReadmeFrame.SetCaption(const Value: string);
begin
  (Parent as TTabSheet).Caption := Value;
end;

function TReadmeFrame.GetHintAtPos(ScreenX, ScreenY: Integer): string;
begin
  Result := '';
end;

procedure TReadmeFrame.SetReadmeFileName(const Value: string);
begin
  FReadmeFileName := Value;
  if FileExists(Value) then
    ReadmePane.{$IFDEF VCL}Lines.{$ENDIF VCL}LoadFromFile(Value);
end;

procedure TReadmeFrame.Show;
var
  ATabSheet: TTabSheet;
begin
  ATabSheet := Parent as TTabSheet;
  (ATabSheet.Parent as TPageControl).ActivePage := ATabSheet;
end;

end.
