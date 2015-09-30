unit ChangePassHandler;

interface

uses SysUtils, Classes, BaseHandler, CryptoHandler, DeviceUnit, XMLHandler,
     LoginHandler;

type
 TCmd_ChangePassClient = class(TCommandClient)
 private
  FLoginHandler  : THndr_LoginClient;
  FNewPassword   : String;
  function FGetUserName: String;
  function FGetPassword: String;
  procedure FSetUserName(Value: String);
  procedure FSetPassword(Value: String);
 public
  constructor Create(XmlDoc: IXMLDocument); override;
  destructor Destroy; override;

  function GetCommandName: String; override;
  function AddRequestToDocument: Boolean; override;
  function GetAnswerFromDocument: Boolean; override;

  property UserName: String read FGetUserName write FSetUserName;
  property Password: String read FGetPassword write FSetPassword;
  property NewPassword: String read FNewPassword write FNewPassword;
 end;

 TCmd_ChangePassServer = class(THandlerServerUserEvent)
 private
  FLoginHandler  : THndr_LoginServer;
  FNewPass       : String;
  function FGetUserName: String;
  function FGetPassword: String;
  procedure FSetUserName(Value: String);
  procedure FSetPassword(Value: String);
 public
  constructor Create(XmlDoc: IXMLDocument; RemoteDevice: TRemoteDevice);
  destructor Destroy; override;

  function GetRequestFromDocument: Boolean; override;
  function Execute(var ErrCode: Integer; var UserError: String): Boolean; override;
  function AddAnswerToDocument: Boolean; override;

  property UserName: String read FGetUserName write FSetUserName;
  property Password: String read FGetPassword write FSetPassword;
  property NewPassword: String read FNewPass write FNewPass;
 end;

implementation
uses BillingConstUnit, XMLHandlerMS, DBInterfaceUnit, MD5;

//******************************************************************************
//   TChangePassClient
//******************************************************************************
constructor TCmd_ChangePassClient.Create(XmlDoc: IXMLDocument);
begin
 inherited Create(XmlDoc);
 FLoginHandler := THndr_LoginClient.Create(XmlDoc);
end;

destructor TCmd_ChangePassClient.Destroy;
begin
 FLoginHandler.Free;
 inherited Destroy;
end;

function TCmd_ChangePassClient.GetCommandName: String;
begin
 Result := cmd_SysChangePass;
end;

function TCmd_ChangePassClient.FGetUserName: String;
begin
 Result := FLoginHandler.UserName;
end;

function TCmd_ChangePassClient.FGetPassword: String;
begin
 Result := FLoginHandler.Password;
end;

procedure TCmd_ChangePassClient.FSetUserName(Value: String);
begin

 FLoginHandler.UserName := Value;
end;

procedure TCmd_ChangePassClient.FSetPassword(Value: String);
begin
 FLoginHandler.Password := Value;
end;

function TCmd_ChangePassClient.AddRequestToDocument: Boolean;
var iNode : IXMLNode;
    SDate : String;
    SCRC  : String;
    SPass : String;
begin
 try                                        
  if not FLoginHandler.AddRequestToDocument then raise EAbort.Create(FLoginHandler.LastError);

  SDate := FormatDateTime('DD.MM.YY HH:NN:SS', Now);
  SPass := FLoginHandler.Encrypt(UserName + NewPassword, UserName + SDate);
  SCRC  := FLoginHandler.CalculateCRC(UserName + SPass + SDate);

  iNode := XML_GetRootNode;
  iNode := iNode.AppendChild(XmlDocument.CreateElement(xml_Node_ChangePass));
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_LoginUser)).Text := UserName;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_LoginPass)).Text := SPass;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_Time)).Text      := SDate;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_CRC)).Text       := SCRC;

  Result := true;
 except
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'] AddRequestToDocument Fail: '+E.Message;
    Result := false;
   end;
 end;
end;

function TCmd_ChangePassClient.GetAnswerFromDocument: Boolean;
var iNode  : IXMLNode;
    SDate  : String;
    SCRC   : String;
    SUser  : String;
begin
 try
  if not FLoginHandler.GetAnswerFromDocument then raise EAbort.Create(FLoginHandler.LastError);

  iNode    := XML_GetRootNode;
  iNode    := XML_FindNodeByName(iNode, xml_Node_ChangePass);
  SUser    := XML_GetNodeText(iNode, xml_Node_LoginUser);
  SDate    := XML_GetNodeText(iNode, xml_Node_Time);
  SCRC     := XML_GetNodeText(iNode, xml_Node_CRC);

  if SCRC <> FLoginHandler.CalculateCRC(UserName + SDate) then raise EAbort.Create('Invalid signature');
  if SUser <> UserName then raise EAbort.Create('Invalid UserName');

  Result := true;
 except
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'] GetDataFromDocument Fail: '+E.Message;
    Result := false;
   end;
 end;
end;

//******************************************************************************
//   TChangePassServer
//******************************************************************************
constructor TCmd_ChangePassServer.Create(XmlDoc: IXMLDocument; RemoteDevice: TRemoteDevice);
begin
 inherited Create(XmlDoc, RemoteDevice);
 FLoginHandler := THndr_LoginServer.Create(XmlDoc, RemoteDevice);
end;

destructor TCmd_ChangePassServer.Destroy;
begin
 FLoginHandler.Free;
 inherited Destroy;
end;

function TCmd_ChangePassServer.GetRequestFromDocument: Boolean;
var iNode  : IXMLNode;
    SDate  : String;
    SCRC   : String;
    SUser  : String;
begin
 try
  if not FLoginHandler.GetRequestFromDocument then raise EAbort.Create(FLoginHandler.LastError);

  iNode     := XML_GetRootNode;
  iNode     := XML_FindNodeByName(iNode, xml_Node_ChangePass);
  SUser     := XML_GetNodeText(iNode, xml_Node_LoginUser);
  FNewPass  := XML_GetNodeText(iNode, xml_Node_LoginPass);
  SDate     := XML_GetNodeText(iNode, xml_Node_Time);
  SCRC      := XML_GetNodeText(iNode, xml_Node_CRC);

  if SCRC <> FLoginHandler.CalculateCRC(SUser + FNewPass + SDate) then raise EAbort.Create('Invalid signature');
  if SUser <> UserName then raise EAbort.Create('Invalid username');

  FNewPass := FLoginHandler.Decrypt(FNewPass, UserName+SDate);
  if Copy(FNewPass, 1, Length(UserName)) <> UserName then raise EAbort.Create('Invalid password -> decrypt fail');
  Delete(FNewPass, 1, Length(UserName));

  Result := true;
 except
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'] GetRequestFromDocument Fail: '+E.Message;
    Result := false;
   end;
 end;
end;

function TCmd_ChangePassServer.Execute(var ErrCode: Integer; var UserError: String): Boolean;
var Ra   : Integer;
    SQL  : String;
    SPass: String;

    procedure SetErrorCode(Code_: Integer; Message_: String);
    begin
     ErrCode   := Code_;
     UserError := Message_;
    end;
begin
 Result := true;
 try
  if not FLoginHandler.Execute(ErrCode, UserError) then  raise EAbort.Create(FLoginHandler.LastError);

  if ErrCode = errcode_ExecuteSucceed then
   begin
    if FLoginHandler.PermissionList.Values['sys_changeownpass'] <> 'YES' then
     raise EHandledException.Create(errcode_ChangePassFail_Denied,
                                    'Неуспешна промяна парола!'+sLineBreak+
                                    'Нямате необходимите права'+sLineBreak+
                                    'за да смените паролата си.');
    if Length(NewPassword) < 6 then
     raise EHandledException.Create(errcode_ChangePassFail_Invalid,
                                    'Неуспешна промяна парола!'+sLineBreak+
                                    'Въведената парола е невалидна!'+sLineBreak+
                                    'Въведете парола с дължина поне 6 символа.');

    with Device.DbInterface do
     begin
      SPass := MD5Print(MD5String(NewPassword));
      SQL := 'UPDATE ADMIN_USERS SET AU_PASSWORD = '+StrToSQL(SPass)+' '+
             'WHERE AU_USERNAME = '+StrToSQL(UserName);

      if not ExecuteSQLStatement(SQL, Ra) then raise EAbort.Create('Update password fail: '+LastError);
      if Ra = 0 then raise EAbort.Create('No records affected after change pass SQL');

      SetErrorCode(errcode_ExecuteSucceed, '');
     end;
   end;
 except
  on E: EHandledException do
   begin
    SetErrorCode(E.ErrorCode, E.Message);
    Result := true;
   end;
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'] Execute Fail: '+E.Message;
    Result    := false;
   end;
 end;
end;

function TCmd_ChangePassServer.AddAnswerToDocument: Boolean;
var iNode : IXMLNode;
    SDate : String;
    SCRC  : String;
begin
 try
  FLoginHandler.XmlDocument := Self.XmlDocument; // много е важно да превключим документа
  if not FLoginHandler.AddAnswerToDocument then raise EAbort.Create(FLoginHandler.LastError);

  SDate := FormatDateTime('DD.MM.YY HH:NN:SS', Now);
  SCRC  := FLoginHandler.CalculateCRC(UserName + SDate);

  iNode := XML_GetRootNode;
  iNode := iNode.AppendChild(XmlDocument.CreateElement(xml_Node_ChangePass));
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_LoginUser)).Text := UserName;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_Time)).Text := SDate;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_CRC)).Text  := SCRC;

  Result := true;
 except
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'] AddAnswerToDocument Fail: '+E.Message;
    Result := false;
   end;
 end;
end;

function TCmd_ChangePassServer.FGetUserName: String;
begin
 Result := FLoginHandler.UserName;
end;

function TCmd_ChangePassServer.FGetPassword: String;
begin
 Result := FLoginHandler.Password;
end;

procedure TCmd_ChangePassServer.FSetUserName(Value: String);
begin
 FLoginHandler.UserName := Value;
end;

procedure TCmd_ChangePassServer.FSetPassword(Value: String);
begin
 FLoginHandler.Password := Value;
end;

end.
