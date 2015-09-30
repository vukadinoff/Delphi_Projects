unit TestHandler;

interface

uses SysUtils, Classes, BaseHandler, DeviceUnit, XMLHandler;

type
//******************************************************************************
//           PC Test Handler
//******************************************************************************
 TCmd_TestClient = class(TCommandClient)
 private
  FCollectClientData: Boolean;
 public
  constructor Create(XmlDoc: IXMLDocument); override;
  function AddRequestToDocument: Boolean; override;
  function GetAnswerFromDocument: Boolean; override;
  function GetCommandName: String; override;

  property CollectClientData: Boolean read FCollectClientData write FCollectClientData;
 end;

 THndr_TestServer = class(THandlerServer)
 private
  FClientParams: TStrings;
 public
  constructor Create(XmlDoc: IXMLDocument; RemoteDevice: TRemoteDevice);
  destructor Destroy; override;

  function GetRequestFromDocument: Boolean; override;
  function Execute(var ErrCode: Integer; var UserError: String): Boolean; override;
  function AddAnswerToDocument: Boolean; override;
 end;


implementation
uses BillingConstUnit, WinUtilsUnit;

//******************************************************************************
//   THndr_TestClient
//******************************************************************************
constructor TCmd_TestClient.Create(XmlDoc: IXMLDocument);
begin
 inherited Create(XmlDoc);
 FCollectClientData := true;
end;

function TCmd_TestClient.GetCommandName: String;
begin
 Result := cmd_PCTest;
end;

function TCmd_TestClient.AddRequestToDocument: Boolean;
var iNode : IXMLNode;
begin
 try
  iNode := XML_GetRootNode;
  iNode := iNode.appendChild(XmlDocument.createElement(xml_Node_TestData));

  if FCollectClientData then
   begin
    iNode.appendChild(XmlDocument.createElement('HostName')).Text    := Win_GetHostName;
    iNode.appendChild(XmlDocument.createElement('IP')).Text          := Win_GetIPAdress;
    iNode.appendChild(XmlDocument.createElement('CurrentUser')).Text := Win_GetCurrentUserName;
    iNode.appendChild(XmlDocument.createElement('OSName')).Text      := Win_GetOSName;
    iNode.appendChild(XmlDocument.createElement('OSVer')).Text       := Win_GetOSVersion;
    iNode.appendChild(XmlDocument.createElement('ProductId')).Text   := Win_GetProductID;
    iNode.appendChild(XmlDocument.createElement('Org')).Text         := Win_GetOrganisation;
    iNode.appendChild(XmlDocument.createElement('Owner')).Text       := Win_GetOwner;
   end;

  Result := true;
 except
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'] AddRequestToDocument Fail: '+E.Message;
    Result := false;
   end;
 end;
end;

function TCmd_TestClient.GetAnswerFromDocument: Boolean;
begin
 Result := true;
end;

//******************************************************************************
//   THndr_TestServer
//******************************************************************************
constructor THndr_TestServer.Create(XmlDoc: IXMLDocument; RemoteDevice: TRemoteDevice);
begin
 inherited Create(XmlDoc, RemoteDevice);
 FClientParams := TStringList.Create;
end;

destructor THndr_TestServer.Destroy;
begin
 FClientParams.Free;
 inherited Destroy;
end;

function THndr_TestServer.GetRequestFromDocument: Boolean;
var iNode: IXMLNode;
begin
 try
  iNode := XML_GetRootNode;
  iNode := XML_FindNodeByName(iNode, xml_Node_TestData);

  iNode := iNode.firstChild;
  while iNode <> nil do
   begin
    FClientParams.Add(iNode.nodeName+'='+iNode.text);
    iNode := iNode.nextSibling;
   end;

  Result := true;
 except
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'] GetRequestFromDocument Fail: '+E.Message;
    Result := false;
   end;
 end;
end;

function THndr_TestServer.Execute(var ErrCode: Integer; var UserError: String): Boolean;
begin
 ErrCode   := errcode_ExecuteSucceed;
 UserError := '';
 Result    := true;
end;

function THndr_TestServer.AddAnswerToDocument: Boolean;
begin
 Result := true;
end;

end.
