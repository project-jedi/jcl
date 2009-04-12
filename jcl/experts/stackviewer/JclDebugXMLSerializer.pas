unit JclDebugXMLSerializer;

interface

uses
  SysUtils, Classes, JclDebugSerialization;

type
  TJclXMLSerializer = class(TJclCustomSimpleSerializer)
  public
    function SaveToString: string;
  end;

implementation

//=== { TJclXMLSerializer } ==================================================

function TJclXMLSerializer.SaveToString: string;

  procedure AddToStrings(ASerializer: TJclCustomSimpleSerializer; AXMLStrings: TStringList; AIdent: Integer);
  var
    I, P: Integer;
    S, S1, S2, V: string;
  begin
    if AIdent = 0 then
      S := ''
    else
      S := StringOfChar(' ', AIdent);
    V := '';
    for I := 0 to ASerializer.Values.Count - 1 do
    begin
      S1 := ASerializer.Values[I];
      P := Pos('=', S1);
      if P > 0 then
      begin
        S2 := S1;
        Delete(S1, P, Length(S1));
        Delete(S2, 1, P);
        V := V + ' ';
        V := V + Format('%s="%s"', [S1, S2]);
      end;
    end;
    if ASerializer.Count > 0 then
    begin
      AXMLStrings.Add(S + '<' + ASerializer.Name + V + '>');
      for I := 0 to ASerializer.Count - 1 do
        AddToStrings(ASerializer[I], AXMLStrings, AIdent + 2);
      AXMLStrings.Add(S + '</' + ASerializer.Name + '>');
    end
    else
      AXMLStrings.Add(S + '<' + ASerializer.Name + V + '/>');
  end;


var
  XMLStrings: TStringList;
begin
  XMLStrings := TStringList.Create;
  try
    AddToStrings(Self, XMLStrings, 0);
    Result := XMLStrings.Text;
  finally
    XMLStrings.Free;
  end;
end;

end.
