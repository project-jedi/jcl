program Contributors;

{$APPTYPE CONSOLE}

uses
  Classes, SysUtils;

var
  HTML: TStringList;    // HTML output
  List: TStringList;    // List of email=author pairs loaded from Contributers.txt
  I, P: Integer;        // List index and search position
  S: string;            // Temporary holding of List[I]
  Name, Email: string;  // Name and Email address as exacted from List

begin

  try
    HTML := TStringList.Create;
    try
      HTML.Add('<html>');
      HTML.Add('<head>');
      HTML.Add('  <link rel="stylesheet" href="..\DclStyle.css">');
      HTML.Add('  <title>JCL Contributors (code donators)</title>');
      HTML.Add('</head>');
      HTML.Add('<body>');
      HTML.Add('  <h2>Contributors</h2>');
      HTML.Add('  <p>');
      HTML.Add('  Following is a list of all people that donated, or gave permission to use their,');
      HTML.Add('  code in the JEDI Code Library. Be sure that you read the Contacting Authors page');
      HTML.Add('  in the JCL helpfile before contacting these people. Note that JCL is continously');
      HTML.Add('  in development and by far not all donations have been processed yet. We''re not');
      HTML.Add('  even half way! Therefore it is very well possible that you donated code but it''s');
      HTML.Add('  not in the JCL yet. However, if you''re name is not in the list below then it''s');
      HTML.Add('  likely that something has gone wrong. In that event, please');
      HTML.Add('  <a href="mailto:jcl@delphi-jedi.org?subject=JCL Donations">contact us</a>.');
      HTML.Add('  </p>');
      HTML.Add('  <p>');
      HTML.Add('  <table>');

      List := TStringList.Create;
      try
        List.LoadFromFile('contributors.txt');
        I := 0;
        while I < List.Count do
        begin
          if (I mod 4) = 0 then
          begin
            if I <> 0 then HTML.Add('  </tr>');
            HTML.Add('  <tr valign="top">');
          end;
          S := List[I];
          if Trim(S) <> '' then
          begin
            P := Pos('=', S);
            Email := Trim(Copy(S, 1, P - 1));
            System.Delete(S, 1, P);
            Name := Trim(S);
            HTML.Add(Format('    <td width=25%%><a href="mailto:%s">%s</a></td>', [Email, Name]));
          end;
          Inc(I);
        end;
        while (I mod 4) <> 0 do
        begin
          HTML.Add('    <td width=25%><a href="mailto:">You''re name here?</a></td>');
          Inc(I);
        end;
        List.Add('  </tr>');
      finally
        List.Free;
      end;

      HTML.Add('  </table>');
      HTML.Add('  </p>');
      HTML.Add('  <p>');
      HTML.Add('  <div class="footer">Built on Saterday, April 28, 2001</div>'); // todo use Now!
      HTML.Add('  </p>');
      HTML.Add('</body>');
      HTML.Add('</html>');

      HTML.SaveToFile('Contributors.htm');
      WriteLn('Succefully created Contibutors.htm');
    finally
      HTML.Free;
    end;
  except
    WriteLn('An error occured');
    raise;
  end;

end.
