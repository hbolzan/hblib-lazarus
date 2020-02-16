unit HbXMLUtils;

interface

uses
  HbTypes,
  DOM, XMLRead, sysutils;

function FormatXML(XMLDoc: TXMLDocument): String;

implementation

function FormatXML(XMLDoc: TXMLDocument): String;
  function RenderAttributes(pNode: TDOMNode): String;
  var
    Indice: integer;
  begin
    Result := '';
    if pNode.HasAttributes then begin
      for Indice := 0 to pNode.Attributes.Length -1 do begin
        with pNode.Attributes[Indice] do begin
          Result := Result + Format(' %s="%s"', [NodeName, NodeValue]);
        end;
      end;
    end;
    Result := Trim(Result);
  end;

  function RenderOpenTagWithAttributes(pNode: TDOMNode): String;
  begin
    Result := '<' + Trim(pNode.NodeName) + RenderAttributes(pNode) + '>';
  end;

  function RenderCloseTag(pNode: TDOMNode): String;
  begin
    Result := '<' + Trim(pNode.NodeName) + '/>';
  end;

  function RenderIndent(IndentLevel: Integer): String;
  begin
    Result := StringOfChar(' ', IndentLevel * 2);
  end;

  function ParseXML(XML: String; Node: TDOMNode; Nivel: Integer): String;

    function ParseChildNodes(ChildNode: TDOMNode; pNivel: Integer): String;
    begin
      Result := '';
      while ChildNode <> Nil do begin
        Result := ParseXML(Result, ChildNode, pNivel);
        ChildNode := ChildNode.NextSibling;
      end;
    end;

    function RenderWithChildren(pNode: TDOMNode; pNivel: Integer): String;

      function GetNodeValue(NodeValue: String): String;
      begin
        if Trim(NodeValue) = '' then begin
          Result := '';
          Exit;
        end;
        Result := RenderIndent(pNivel + 1) + NodeValue + NEW_LINE;
      end;

    begin
      Result := RenderIndent(pNivel) + RenderOpenTagWithAttributes(pNode) + NEW_LINE +
        GetNodeValue(pNode.NodeValue) +
        ParseChildNodes(pNode.FirstChild, pNivel + 1) +
        RenderIndent(pNivel) + RenderCloseTag(pNode) + NEW_LINE;
    end;

    function RenderWithoutChildren(pNode: TDOMNode; pNivel: Integer): String;
      function GetNodeValue: String;
      begin
        if pNode.FirstChild <> Nil then begin
          Result := pNode.FirstChild.NodeValue;
        end else begin
          Result := pNode.NodeValue;
        end;
      end;

    begin
      Result :=  RenderIndent(pNivel) + RenderOpenTagWithAttributes(pNode) +
        GetNodeValue + RenderCloseTag(pNode) + NEW_LINE;
    end;

    function NodeHasChildren(pNode: TDOMNode): Boolean;
    begin
      Result := False;
      if pNode.FirstChild <> Nil then begin
        if pNode.FirstChild.NodeName[1] <> '#' then begin
          Result := True;
        end;
      end;
    end;

    function RenderNode(pNode: TDOMNode; pNivel: Integer): String;
    begin
      if pNode.NodeName[1] = '#' then begin
        Result := pNode.NodeValue;
        Exit;
      end;

      if NodeHasChildren(pNode) then begin
        Result := RenderWithChildren(pNode, pNivel);
      end else begin
        Result := RenderWithoutChildren(pNode, pNivel);
      end;
    end;

  begin
    Result := XML;
    if Node = nil then begin
      Exit;
    end;

    Result := XML + RenderNode(Node, Nivel);
  end;

begin
  Result :=  ParseXML('', XMLDoc.DocumentElement, 0);
end;


end.

