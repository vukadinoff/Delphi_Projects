unit XMLHandler;

interface

uses
  SysUtils, Windows, ComObj, ActiveX, Classes, XMLHandlerMS;

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


 THandlerXML = class(TObject)
 private
  FLastError : String;
  FXmlDoc    : IXMLDocument;

  function FGetXmlString: String;
 public
  constructor Create;
  destructor Destroy; override;

  function XSDateTimeToDateTime(XSDateTime: String): TDateTime;
  function DateTimeToXSDateTime(ADateTime: TDateTime): String;

  function LoadFromString(SourceXML: String): Boolean;
  function LoadFromFile(Fname: String): Boolean;
  function Validate(XsdFname: String): Boolean;
  function SaveToFile(Fname: String): Boolean;

  function XML_GetRootNode(RaiseExceptionOnFail: Boolean = true): IXMLElement;
  function XML_GetNodeByName(OnRoot: IXMLNode; NodeName: String; RaiseExceptionOnFail: Boolean=true): IXMLNode;
  function XML_GetNodeText(OnRoot: IXMLNode; NodeName: String; RaiseExceptionOnFail: Boolean=true; CheckForEmptyValue: Boolean=true): String;
  function XML_GetNodeInt(OnRoot: IXMLNode; NodeName: String; RaiseExceptionOnFail: Boolean=true): Integer;
  function XML_GetNodeFloat(OnRoot: IXMLNode; NodeName: String; RaiseExceptionOnFail: Boolean=true): Double;
  function XML_GetNodeDateTime(OnRoot: IXMLNode; NodeName: String; RaiseExceptionOnFail: Boolean=true): TDateTime;

  function XML_FindFirstNode(NodeName: String; RaiseExceptionOnFail: Boolean = true): IXMLNode;
  function XML_FindNodeByValue(OnRoot: IXMLNode; NodeValue: String; RaiseExceptionOnFail: Boolean=false): IXMLNode;
  procedure XML_ReplaceNodeValue(NodeName, NewValue: String; RaiseExceptionOnFail: Boolean=false);

  property XmlDocument: IXMLDocument read FXmlDoc write FXmlDoc;
  property XmlString: String read FGetXmlString;
  property LastError: String read FLastError write FLastError;
 end;


//function CreateXMLDoc: IXMLDocument;
//procedure FreeXmlDoc(Doc_: IXMLDocument);

implementation
uses XSBuiltIns;

function CreateXMLDoc: IXMLDocument;
begin
  Result := CreateComObject(CLASS_DOMDocument) as IXMLDocument;
end;

procedure FreeXmlDoc(Doc_: IXMLDocument);
begin
 Doc_ := nil;
end;


//**************************************************************************************************
//    THandlerXML
//**************************************************************************************************
constructor THandlerXML.Create;
begin
 inherited Create;
 FXmlDoc := CreateXMLDoc;
end;

destructor THandlerXML.Destroy;
begin
 FreeXmlDoc(FXmlDoc);
 inherited Destroy;
end;

function THandlerXML.XSDateTimeToDateTime(XSDateTime: String): TDateTime;
begin
  with TXSDateTime.Create do
  try
   XSToNative(XSDateTime);
   Result := AsUTCDateTime;
  finally
   Free;
  end;
end;

function THandlerXML.DateTimeToXSDateTime(ADateTime: TDateTime): String;
begin
  Result:= FormatDateTime('YYYY-MM-DD"T"HH:NN:SS', ADateTime);

{ tova vru6ta i timezone, ne namerih kak da formatiram datata
  with TXSDateTime.Create do
    try
      AsDateTime:= ADateTime;
      Result:= NativeToXS;
    finally
      Free;
    end; }
end;

function THandlerXML.LoadFromString(SourceXML: String): Boolean;
var iNode: IXMLElement;
begin
 try
  Result := FXmlDoc.loadXML(SourceXML);
  if not Result then
   begin
    raise EAbort.Create(FXmlDoc.parseError.reason+
                        ' [Line:'+IntToStr(FXmlDoc.parseError.line)+
                        ' Linepos:'+IntToStr(FXmlDoc.parseError.linepos)+
                        ' Filepos:'+IntToStr(FXmlDoc.parseError.filepos)+']');
   end;

  iNode := FXmlDoc.documentElement;
  if iNode = nil then raise EAbort.Create('XML document is empty');

//  FXmlDoc.save('test.xml');
 except
  on E: Exception do
   begin
    Result := false;
    LastError := '[THandlerXML][LoadXML Str] '+E.Message;
   end;
 end;
end;

function THandlerXML.LoadFromFile(Fname: String): Boolean;
var iNode: IXMLElement;
begin
 try
  if not FileExists(Fname) then raise EAbort.Create('File does not exist: '+Fname);

  Result := FXmlDoc.load(Fname);
  if not Result then
   begin
    raise EAbort.Create(FXmlDoc.parseError.reason+
                        ' [Line:'+IntToStr(FXmlDoc.parseError.line)+
                        ' Linepos:'+IntToStr(FXmlDoc.parseError.linepos)+
                        ' Filepos:'+IntToStr(FXmlDoc.parseError.filepos)+']');
   end;

  iNode := FXmlDoc.documentElement;
  if iNode = nil then raise EAbort.Create('XML document is empty');

 except
  on E: Exception do
   begin
    Result := false;
    LastError := '[THandlerXML][LoadXML File] '+E.Message;
   end;
 end;
end;

function THandlerXML.Validate(XsdFname: String): Boolean;
var Cache : IXMLDOMSchemaCollection2;
    XmlD  : IXMLDOMDocument2;
begin
 try
  if not FileExists(XsdFname) then raise EAbort.Create('Schema File does not exist: '+XsdFname);
  Cache := CoXMLSchemaCache40.Create;
  XmlD  := CoDOMDocument40.Create;
  try
    Cache.add('', XsdFname);
    XmlD.schemas := Cache;
    XmlD.async   := False;
    XmlD.validateOnParse  := True;
    XmlD.resolveExternals := True;

    Result := XmlD.loadXML(XmlString);
    if not Result then raise EAbort.Create('XML Validation fail'+sLineBreak+
                                           'URL:'+XmlD.parseError.url+sLineBreak+
                                           'Reason:'+XmlD.parseError.reason+sLineBreak+
                                           'Text:'+XmlD.parseError.srcText+sLineBreak+
                                           'Schema:'+XsdFname);
  finally
   XmlD  := nil;
   Cache := nil;
  end;
 except
  on E: Exception do
   begin
    Result := false;
    LastError := '[THandlerXML][XML_Validate] '+E.Message;
   end;
 end;
end;

function THandlerXML.SaveToFile(Fname: String): Boolean;
begin
 Result := false;
 try
  FXmlDoc.save(Fname);
 except
  on E: Exception do
   begin
    Result := false;
    LastError := '[THandlerXML][SaveToFile] '+E.Message;
   end;
 end;
end;

function THandlerXML.XML_FindFirstNode(NodeName: String; RaiseExceptionOnFail: Boolean = true): IXMLNode;
begin
  Result := nil;
  try
   if XmlDocument = nil then raise EAbort.Create('XML document is not assigned');

   with XmlDocument.getElementsByTagName(NodeName) do
    begin
     if Length > 0 then Result := item[0];
    end;

   if (Result = nil)and(RaiseExceptionOnFail) then raise EAbort.Create('XML node "'+NodeName+'" can not be found');
  except
   on E: Exception do if RaiseExceptionOnFail then raise EAbort.Create('[XML_FindFirstNode]'+E.Message);
  end;
end;

function THandlerXML.XML_FindNodeByValue(OnRoot: IXMLNode; NodeValue: String; RaiseExceptionOnFail: Boolean=false): IXMLNode;
var ND : IXMLNode;
begin
 Result := nil;
 try
  if OnRoot = nil then OnRoot := XML_GetRootNode;

  ND := OnRoot.firstChild;
  while ND <> nil do
   begin
    if SameText(NodeValue, ND.text) then
     Result := ND
    else
    if ND.hasChildNodes then
     Result := XML_FindNodeByValue(ND, NodeValue, RaiseExceptionOnFail);

    if Result <> nil then Exit;
    ND := ND.nextSibling;
   end;
 except
  on E: Exception do if RaiseExceptionOnFail then raise EAbort.Create('[FindNodeByValue]'+E.Message);
 end;
end;

procedure THandlerXML.XML_ReplaceNodeValue(NodeName, NewValue: String; RaiseExceptionOnFail: Boolean=false);
var Node: IXMLNode;
begin
 try
  Node := XML_FindFirstNode(NodeName, RaiseExceptionOnFail);
  if Node <> nil then Node.text := NewValue;
 except
  on E: Exception do if RaiseExceptionOnFail then raise EAbort.Create('[ReplaceNodeValue]'+E.Message);
 end;
end;

function THandlerXML.XML_GetRootNode(RaiseExceptionOnFail: Boolean = true): IXMLElement;
begin
  Result := nil;
  try
   if XmlDocument = nil then raise EAbort.Create('XML document is not assigned');

   Result := XmlDocument.documentElement;

   if (Result = nil)and(RaiseExceptionOnFail) then raise EAbort.Create('XML document is empty');
  except
   on E: Exception do if RaiseExceptionOnFail then raise EAbort.Create('[XML_GetRootNode]'+E.Message);
  end;
end;

function THandlerXML.XML_GetNodeByName(OnRoot: IXMLNode; NodeName: String; RaiseExceptionOnFail: Boolean = true): IXMLNode;
begin
  Result := nil;
  try
   if XmlDocument = nil then raise EAbort.Create('XML document is not assigned');

   Result := OnRoot.selectSingleNode(NodeName);

   if (Result = nil)and(RaiseExceptionOnFail) then raise EAbort.Create('"'+NodeName+'" does not exist in XML document');
  except
   on E: Exception do if RaiseExceptionOnFail then raise EAbort.Create('[XML_GetNodeByName]'+E.Message);
  end;
end;

function THandlerXML.XML_GetNodeText(OnRoot: IXMLNode; NodeName: String; RaiseExceptionOnFail: Boolean = true;
                                     CheckForEmptyValue: Boolean = true): String;
var iNode : IXMLNode;
begin
  Result := '';
  try
   if OnRoot = nil then raise EAbort.Create('XML search root is not assigned');

   iNode := OnRoot.selectSingleNode(NodeName);
   if iNode <> nil then
    Result := Trim(iNode.text)
   else
    raise EAbort.Create('XML node "'+NodeName+'" not found');

   if (Result = '')and(CheckForEmptyValue) then  raise EAbort.Create('XML node "'+NodeName+'" is empty');
  except
   on E: Exception do if RaiseExceptionOnFail then raise EAbort.Create('[XML_GetNodeText]'+E.Message);
  end;
end;

function THandlerXML.XML_GetNodeInt(OnRoot: IXMLNode; NodeName: String; RaiseExceptionOnFail: Boolean=true): Integer;
var S : String;
begin
 Result := 0;
 S := XML_GetNodeText(OnRoot, NodeName, RaiseExceptionOnFail);
 if (not TryStrToInt(S, Result))and(RaiseExceptionOnFail) then
  raise EAbort.Create('XML node "'+NodeName+'" is not integer: '+S);
end;

function THandlerXML.XML_GetNodeFloat(OnRoot: IXMLNode; NodeName: String; RaiseExceptionOnFail: Boolean=true): Double;
var S : String;
begin
 DecimalSeparator := '.';
 Result := 0;
 S := XML_GetNodeText(OnRoot, NodeName, RaiseExceptionOnFail);
 if (not TryStrToFloat(S, Result))and(RaiseExceptionOnFail) then
  raise EAbort.Create('XML node "'+NodeName+'" is not float: '+S);
end;

function THandlerXML.XML_GetNodeDateTime(OnRoot: IXMLNode; NodeName: String; RaiseExceptionOnFail: Boolean=true): TDateTime;
var S : String;
begin
 Result := 0;
 S := Trim(XML_GetNodeText(OnRoot, NodeName, RaiseExceptionOnFail, RaiseExceptionOnFail));
 if S <> '' then Result := XSDateTimeToDateTime(S)
end;

function THandlerXML.FGetXmlString: String;
begin
 Result := FXmlDoc.xml;
end;


end.
