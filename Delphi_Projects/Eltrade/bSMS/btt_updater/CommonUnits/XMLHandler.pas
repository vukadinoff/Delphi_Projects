unit XMLHandler;

interface

uses
  Windows, ComObj, ActiveX, Classes, XMLHandlerMS;

type
  IXMLDocument = IXMLDOMDocument2;
  IXMLText = IXMLDOMText;
  IXMLElement = IXMLDOMElement;
  IXMLProcessingInstruction = IXMLDOMProcessingInstruction;
  IXMLCDATASection = IXMLDOMCDATASection;
  IXMLComment = IXMLDOMComment;
  IXMLAttr = IXMLDOMAttribute;
  IXMLNodeList = IXMLDOMNodeList;
  IXMLNamedNodeMap = IXMLDOMNamedNodeMap;
  IXMLNode = IXMLDOMNode;
  IXMLParseError = IXMLDOMParseError;

const
  DEFAULT_TRUE = '1';
  DEFAULT_FALSE = '0';

const
  ELEMENT_NODE = NODE_ELEMENT;
  ATTRIBUTE_NODE = NODE_ATTRIBUTE;
  TEXT_NODE = NODE_TEXT;
  CDATA_SECTION_NODE = NODE_CDATA_SECTION;
  ENTITY_REFERENCE_NODE = NODE_ENTITY_REFERENCE;
  ENTITY_NODE = NODE_ENTITY;
  PROCESSING_INSTRUCTION_NODE = NODE_PROCESSING_INSTRUCTION;
  COMMENT_NODE = NODE_COMMENT;
  DOCUMENT_NODE = NODE_DOCUMENT;
  DOCUMENT_TYPE_NODE = NODE_DOCUMENT_TYPE;
  DOCUMENT_FRAGMENT_NODE = NODE_DOCUMENT_FRAGMENT;
  NOTATION_NODE = NODE_NOTATION;

function CreateXMLDoc: IXMLDocument;
procedure FreeXmlDoc(Doc_: IXMLDocument);

implementation

function CreateXMLDoc: IXMLDocument;
begin
  Result := CreateComObject(CLASS_DOMDocument) as IXMLDocument;
end;

procedure FreeXmlDoc(Doc_: IXMLDocument);
begin
 Doc_ := nil;
end;



end.
