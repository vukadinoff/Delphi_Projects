unit LoginHandler;

interface

uses SysUtils, Classes, BaseHandler, CryptoHandler, DeviceUnit, XMLHandler;

type
 THndr_LoginClient = class(THandlerClient)
 private
  FCryptoHandler : TCrypto_TEA;
  FCrcHandler    : TCRC32;
  FUserName      : String;
  FPassword      : String;
  FFullName      : String;
  FPermList      : TStrings;
 public
  constructor Create(XmlDoc: IXMLDocument); 
  destructor Destroy; override;

  function AddRequestToDocument: Boolean; override;
  function GetAnswerFromDocument: Boolean; override;

  function CalculateCRC(Source: String): String;
  function Encrypt(Source, Key: String): String;
  function Decrypt(Source, Key: String): String;

  property UserName: String read FUserName write FUserName;
  property Password: String read FPassword write FPassword;
  property FullName: String read FFullName write FFullName;
  property PermissionList: TStrings read FPermList write FPermList;
 end;

 THndr_LoginServer = class(THandlerServer)
 private
  FCryptoHandler : TCrypto_TEA;
  FCrcHandler    : TCRC32;
  FUserName      : String;
  FPassword      : String;
  FFullName      : String;
  FPermList      : TStrings;
  function IsInternalIp(IP: String): Boolean;
 public
  constructor Create(XmlDoc: IXMLDocument; RemoteDevice: TRemoteDevice);
  destructor Destroy; override;

  function GetRequestFromDocument: Boolean; override;
  function Execute(var ErrCode: Integer; var UserError: String): Boolean; override;
  function AddAnswerToDocument: Boolean; override;

  function CalculateCRC(Source: String): String;
  function Encrypt(Source, Key: String): String;
  function Decrypt(Source, Key: String): String;

  property CryptoHandler: TCrypto_TEA read FCryptoHandler write FCryptoHandler;
  property CrcHandler: TCRC32 read FCrcHandler write FCrcHandler;
  property UserName: String read FUserName write FUserName;
  property Password: String read FPassword write FPassword;
  property FullName: String read FFullName write FFullName;
  property PermissionList: TStrings read FPermList write FPermList;
 end;

implementation
uses DB, BillingConstUnit, XMLHandlerMS, DBInterfaceUnit, md5;

//******************************************************************************
//   THndr_LoginClient
//******************************************************************************
constructor THndr_LoginClient.Create(XmlDoc: IXMLDocument);
begin
 inherited Create(XmlDoc);
 FCryptoHandler := TCrypto_TEA.Create;
 FCrcHandler    := TCRC32.Create;
 FPermList      := TStringList.Create;
end;

destructor THndr_LoginClient.Destroy;
begin
 FCryptoHandler.Free;
 FCrcHandler.Free;
 FPermList.Free;
 inherited Destroy;
end;

function THndr_LoginClient.AddRequestToDocument: Boolean;
var iNode : IXMLNode;
    SDate : String;
    SCRC  : String;
    SPass : String;
    I     : Integer;
begin
 try
  SDate := FormatDateTime('DD.MM.YY HH:NN:SS', Now);
  SPass := Encrypt(FUserName+FPassword, FUserName+SDate);
  SCRC  := CalculateCRC(FUserName + SPass + SDate);

  iNode := XML_GetRootNode;
  iNode := iNode.AppendChild(XmlDocument.CreateElement(xml_Node_LoginData));
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_LoginUser)).Text := FUserName;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_LoginPass)).Text := SPass;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_Time)).Text      := SDate;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_CRC)).Text       := SCRC;

  iNode := iNode.AppendChild(XmlDocument.CreateElement(xml_Node_LoginPermit));
  for I := 0 to FPermList.Count - 1 do
   iNode.AppendChild(XmlDocument.CreateElement(FPermList.Names[I])).Text  := '?';

  Result := true;
 except
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'] AddDataToDocument Fail: '+E.Message;
    Result := false;
   end;
 end;
end;

function THndr_LoginClient.GetAnswerFromDocument: Boolean;
var iNode : IXMLNode;
    SUser : String;
    SDate : String;
    SCRC  : String;
begin
 try
  iNode  := XML_GetRootNode;
  iNode  := XML_FindNodeByName(iNode, xml_Node_LoginData);
  SUser  := XML_GetNodeText(iNode, xml_Node_LoginUser);
  SDate  := XML_GetNodeText(iNode, xml_Node_Time);
  SCRC   := XML_GetNodeText(iNode, xml_Node_CRC);

  if SUser <> FUserName then raise EAbort.Create('User name differs!!!');
  if SCRC <> CalculateCRC(UserName + SDate) then raise EAbort.Create('Invalid signature');

  FPermList.Clear;
  iNode := XML_FindNodeByName(iNode, xml_Node_LoginPermit);
  iNode := iNode.firstChild;
  while iNode <> nil do
   begin
    FPermList.Add(iNode.nodeName + '=' + iNode.text);
    iNode := iNode.nextSibling;
   end;

  Result := true;
 except
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'] GetAnswerFromDocument Fail: '+E.Message;
    Result := false;
   end;
 end;
end;

function THndr_LoginClient.CalculateCRC(Source: String): String;
begin
 FCrcHandler.Reset;
 FCrcHandler.Process(Source[1], Length(Source));
 Result := FCrcHandler.AsString;
end;

function THndr_LoginClient.Encrypt(Source, Key: String): String;
begin
 Result := FCryptoHandler.EncryptStringEx(Source, Key);
 if (Source <> '')and(Result = '') then raise EAbort.Create('Fail encrypt: '+FCryptoHandler.LastError);
end;

function THndr_LoginClient.Decrypt(Source, Key: String): String;
begin
 Result := FCryptoHandler.DecryptStringEx(Source, Key);
 if (Source <> '')and(Result = '') then raise EAbort.Create('Fail decrypt: '+FCryptoHandler.LastError);
end;

//******************************************************************************
//   THndr_LoginServer
//******************************************************************************
constructor THndr_LoginServer.Create(XmlDoc: IXMLDocument; RemoteDevice: TRemoteDevice);
begin
 inherited Create(XmlDoc, RemoteDevice);
 FCryptoHandler := TCrypto_TEA.Create;
 FCrcHandler    := TCRC32.Create;
 FPermList      := TStringList.Create;
end;

destructor THndr_LoginServer.Destroy;
begin
 FCryptoHandler.Free;
 FCrcHandler.Free;
 FPermList.Free;
 inherited Destroy;
end;

function THndr_LoginServer.GetRequestFromDocument: Boolean;
var iNode : IXMLNode;
    SDate : String;
    SCRC  : String;
begin
 try
  iNode     := XML_GetRootNode;
  iNode     := XML_FindNodeByName(iNode, xml_Node_LoginData);
  FUserName := XML_GetNodeText(iNode, xml_Node_LoginUser, false, false);
  FPassword := XML_GetNodeText(iNode, xml_Node_LoginPass, false, false);
  SDate     := XML_GetNodeText(iNode, xml_Node_Time);
  SCRC      := XML_GetNodeText(iNode, xml_Node_CRC);

  if SCRC <> CalculateCRC(FUserName + FPassword + SDate) then raise EAbort.Create('Invalid signature');

  FPassword := Decrypt(FPassword, FUserName+SDate);
  if Copy(FPassword, 1, Length(FUserName)) <> FUserName then raise EAbort.Create('Invalid password -> decrypt fail');
  Delete(FPassword, 1, Length(FUserName));

  FPermList.Clear;
  iNode := XML_FindNodeByName(iNode, xml_Node_LoginPermit);
  iNode := iNode.firstChild;
  while iNode <> nil do
   begin
    FPermList.Add(iNode.nodeName+'='+iNode.text);
    iNode := iNode.nextSibling;
   end;

  Result := true;
 except
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'] GetDataFromDocument Fail: '+E.Message;
    Result := false;
   end;
 end;
end;

function THndr_LoginServer.Execute(var ErrCode: Integer; var UserError: String): Boolean;
var I    : Integer;
    SPass: String;

    procedure SetErrorCode(Code_: Integer; Message_: String);
    begin
     ErrCode   := Code_;
     UserError := Message_;
    end;
begin
 Result := true;
 try
  with Device.DbInterface do
   begin
    SPass := MD5Print(MD5String(Password));

    if not FillDataSet('select U_NAME, U_EXTACCESS, U_RIGHT from LoginAdmin('+StrToSQL(FUserName)+', '+StrToSQL(SPass)+')') then
     raise EAbort.Create('Exec LoginAdmin fail: '+LastError);

    if DataSet.RecordCount > 0 then
     begin
      DataSet.First;
      FullName := DataSet.FieldByName('U_NAME').AsString;

      if (DataSet.FieldByName('U_EXTACCESS').AsInteger = 1)or
         (IsInternalIp(Device.ConnectionInfo.RemoteIP)) then
       begin
        if FPermList.Count = 0 then
         begin
          while not DataSet.Eof do
           begin
            if DataSet.FieldByName('U_RIGHT').AsString <> '' then FPermList.Add(DataSet.FieldByName('U_RIGHT').AsString+'=YES');
            DataSet.Next;
           end;
         end
        else
         begin
          for I := 0 to FPermList.Count - 1 do
           begin
            if DataSet.Locate('U_RIGHT', FPermList.Names[I], [loCaseInsensitive]) then
             FPermList.Values[FPermList.Names[I]] := 'YES'
            else
             FPermList.Values[FPermList.Names[I]] := 'NO'
           end;
         end;
        SetErrorCode(errcode_ExecuteSucceed, '');
       end
      else
       SetErrorCode(errcode_LoginFail_AccessDenied, 'Неуспешен логин!'+sLineBreak+'Външния достъп е забранен за този потребител!');
     end
    else
     SetErrorCode(errcode_LoginFail_AccessDenied, 'Неуспешен логин!'+sLineBreak+'Невалидно потребителско име или парола!');
   end;
 except
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'] Execute Fail: '+E.Message;
    Result    := false;
   end;
 end;
 Device.DbInterface.CloseDataSet;
end;

function THndr_LoginServer.AddAnswerToDocument: Boolean;
var iNode: IXMLNode;
    SDate : String;
    SCRC  : String;
    I     : Integer;
begin
 try
  SDate := FormatDateTime('DD.MM.YY HH:NN:SS', Now);
  SCRC  := CalculateCRC(FUserName + SDate);

  iNode := XML_GetRootNode;
  iNode := iNode.AppendChild(XmlDocument.CreateElement(xml_Node_LoginData));
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_LoginUser)).Text := FUserName;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_Time)).Text      := SDate;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_CRC)).Text       := SCRC;

  iNode := iNode.AppendChild(XmlDocument.CreateElement(xml_Node_LoginPermit));
  for I := 0 to FPermList.Count - 1 do
   begin
    if FPermList.Names[I] <> '' then
     iNode.AppendChild(XmlDocument.CreateElement(FPermList.Names[I])).Text := FPermList.Values[FPermList.Names[I]];
   end;

  Result := true;
 except
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'] SetAnswerToDocument Fail: '+E.Message;
    Result := false;
   end;
 end;
end;

function THndr_LoginServer.IsInternalIp(IP: String): Boolean;
begin
 Result := (copy(Ip, 1, 11) = '192.168.146');
end;

function THndr_LoginServer.CalculateCRC(Source: String): String;
begin
 FCrcHandler.Reset;
 FCrcHandler.Process(Source[1], Length(Source));
 Result := FCrcHandler.AsString;
end;

function THndr_LoginServer.Encrypt(Source, Key: String): String;
begin
 Result := FCryptoHandler.EncryptStringEx(Source, Key);
 if (Source <> '')and(Result = '') then raise EAbort.Create('Fail encrypt: '+FCryptoHandler.LastError);
end;

function THndr_LoginServer.Decrypt(Source, Key: String): String;
begin
 Result := FCryptoHandler.DecryptStringEx(Source, Key);
 if (Source <> '')and(Result = '') then raise EAbort.Create('Fail decrypt: '+FCryptoHandler.LastError);
end;

end.
