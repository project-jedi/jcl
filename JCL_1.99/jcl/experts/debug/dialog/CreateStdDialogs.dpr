program CreateStdDialogs;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Classes,
  JclBorlandTools,
  JclOtaTemplates in 'JclOtaTemplates.pas',
  JclOtaExcDlgRepository in 'JclOtaExcDlgRepository.pas';

function LoadTemplate(const FileName: string): string;
var
  AFileStream: TFileStream;
begin
  AFileStream := TFileStream.Create(FileName, fmOpenRead, fmShareDenyWrite);
  try
    SetLength(Result, AFileStream.Size);
    AFileStream.ReadBuffer(Result[1], AFileStream.Size);
  finally
    AFileStream.Free;
  end;
end;

procedure SaveFile(const FileName, FileContent: string);
var
  AFileStream: TFileStream;
begin
  AFileStream := TFileStream.Create(FileName, fmOpenWrite, fmShareExclusive);
  try
    AFileStream.Size := 0;
    AFileStream.Write(FileContent[1], Length(FileContent));
  finally
    AFileStream.Free;
  end;
end;

var
  Params: TJclOtaExcDlgParams;
begin
  try
    Params := TJclOtaExcDlgParams.Create;
    try
      Params.ActivePersonality := bpDelphi32;
      Params.FormName := 'ExceptionDialog';
      Params.FormAncestor := 'TForm';
      Params.ModalDialog := True;
      Params.SendEMail := False;
      Params.SizeableDialog := True;
      Params.AutoScrollBars := True;
      Params.DelayedTrace := True;
      Params.HookDll := True;
      Params.LogFile := True;
      Params.LogFileName := '''filename.log''';
      Params.OSInfo := True;
      Params.ModuleList := True;
      Params.ActiveControls := True;
      Params.MainThreadOnly := False;
      Params.TraceAllExceptions := False;
      Params.StackList := True;
      Params.RawData := True;
      Params.ModuleName := True;
      Params.ModuleOffset := True;
      Params.CodeDetails := True;
      Params.VirtualAddress := True;

      SaveFile('ExceptDlg.pas', GetFinalSourceContent(ApplyTemplate(LoadTemplate('ExceptDlg.Delphi32.pas'), Params), 'ExceptDlg', 'ExceptionDialog', 'TForm'));
      SaveFile('ExceptDlg.dfm', GetFinalSourceContent(ApplyTemplate(LoadTemplate('ExceptDlg.Delphi32.dfm'), Params), 'ExceptDlg', 'ExceptionDialog', 'TForm'));

      Params.FormName := 'ExceptionDialogMail';
      Params.SendEMail := True;
      Params.EMailAddress := '''name@domain.ext''';
      Params.EMailSubject := '''email subject''';

      SaveFile('ExceptDlgMail.pas', GetFinalSourceContent(ApplyTemplate(LoadTemplate('ExceptDlg.Delphi32.pas'), Params), 'ExceptDlgMail', 'ExceptionDialogMail', 'TForm'));
      SaveFile('ExceptDlgMail.dfm', GetFinalSourceContent(ApplyTemplate(LoadTemplate('ExceptDlg.Delphi32.dfm'), Params), 'ExceptDlgMail', 'ExceptionDialogMail', 'TForm'));
    finally
      Params.Free;
    end;
  except
    on E:Exception do
      Writeln(E.Classname, ': ', E.Message);
  end;
end.
