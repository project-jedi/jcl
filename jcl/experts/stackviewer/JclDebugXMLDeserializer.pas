unit JclDebugXMLDeserializer;

interface

uses
  SysUtils, JclDebugSerialization, JclSimpleXml;

type
  TJclXMLDeserializer = class(TJclCustomSimpleSerializer)
  public
    procedure LoadFromString(const AValue: string);
  end;

implementation

//=== { TJclXMLDeserializer } ================================================

procedure TJclXMLDeserializer.LoadFromString(const AValue: string);

  procedure AddItems(ASerializer: TJclCustomSimpleSerializer; AElem: TJclSimpleXMLElem);
  var
    I: Integer;
  begin
    for I := 0 to AElem.Properties.Count - 1 do
      ASerializer.Values.Add(Format('%s=%s', [AElem.Properties[I].Name, AElem.Properties[I].Value]));
    for I := 0 to AElem.Items.Count - 1 do
      AddItems(ASerializer.AddChild(nil, AElem.Items[I].Name), AElem.Items[I])
  end;

var
  XML: TJclSimpleXML;
begin
  XML := TJclSimpleXML.Create;
  try
    XML.LoadFromString(AValue);
    Clear;
    AddItems(Self, XML.Root);
  finally
    XML.Free;
  end;
end;

end.
