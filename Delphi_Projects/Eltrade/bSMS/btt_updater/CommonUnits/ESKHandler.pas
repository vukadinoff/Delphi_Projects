unit ESKHandler;

interface

uses SysUtils, Classes, BaseHandler, CryptoHandler, DeviceUnit, Forms, XMLHandler,
     LoginHandler;

type
//*****************************************************
//  ESK Login
//*****************************************************
 TEskData = record
  Serial  : String;
  Version : String;
  Code    : String;
  Key     : String;
  Modules : String;
 end;

 TDealerData = class(THandlerXML)
 private
  FUserFullName : String;
  FUserLevel    : String;
  FCompanyEIK   : String;
  FCompanyName  : String;
  FCompanyTown  : String;
  FCompanyAddres: String;
  FCompanyPhone : String;
  FCompanyContrN: String;
  FCompanyContrD: String;
  FBranchName   : String;
  FBranchTown   : String;
  FBranchAddres : String;
  FBranchPhone  : String;
  FBranchID     : Integer;
  FUserID       : Integer;
  FBranchFiscCtr: String;
 protected
  procedure ReadFromXML;
  procedure WriteToXML;
 public
  property UserFullName: String read FUserFullName write FUserFullName;
  property UserLevel: String read FUserLevel write FUserLevel;
  property CompanyEIK: String read FCompanyEIK write FCompanyEIK;
  property CompanyName: String read FCompanyName write FCompanyName;
  property CompanyTown: String read FCompanyTown write FCompanyTown;
  property CompanyAddres: String read FCompanyAddres write FCompanyAddres;
  property CompanyPhone: String read FCompanyPhone write FCompanyPhone;
  property CompanyContrNumb: String read FCompanyContrN write FCompanyContrN;
  property CompanyContrDate: String read FCompanyContrD write FCompanyContrD;
  property BranchName: String read FBranchName write FBranchName;
  property BranchTown: String read FBranchTown write FBranchTown;
  property BranchAddres: String read FBranchAddres write FBranchAddres;
  property BranchPhone: String read FBranchPhone write FBranchPhone;
  property BranchID: Integer read FBranchID write FBranchID;
  property UserID: Integer read FUserID write FUserID;
  property BranchFiscCtr: String read FBranchFiscCtr write FBranchFiscCtr;
 end;

 THndr_ESKLoginClient = class(THandlerClient)
 private
  FCryptoHandler : TCrypto_TEA;
  FCrcHandler    : TCRC32;
  FEskSerial     : String;
  FEskVersion    : String;
  FEskCode       : String;
  FEskKey        : String;
  FPin           : String;
  FProjectList   : TStrings;
  FPermList      : TStrings;
  FExtParams     : TStrings;
  FDealerData    : TDealerData;
 public
  constructor Create(XmlDoc: IXMLDocument);
  destructor Destroy; override;

  function AddRequestToDocument: Boolean; override;
  function GetAnswerFromDocument: Boolean; override;
  function Encrypt(Source, Key: String): String;
  function Decrypt(Source, Key: String): String;
  function CalculateCRC(Source: String): String;
  function EncryptCode(ESKCode_, ESKKey_: String): String;
  procedure AssignEskData(EskData: TEskData);

  property EskSerial: String read FEskSerial write FEskSerial;
  property EskVersion: String read FEskVersion write FEskVersion;
  property EskCode: String read FEskCode write FEskCode;
  property EskKey: String read FEskKey write FEskKey;
  property Pin: String read FPin write FPin;
  property ProjectList: TStrings read FProjectList write FProjectList;
  property PermissionList: TStrings read FPermList write FPermList;
  property ExtendedParams: TStrings read FExtParams write FExtParams;
  property DealerData: TDealerData read FDealerData write FDealerData;
 end;

 THndr_ESKLoginServer = class(THandlerServer)
 private
  FCryptoHandler : TCrypto_TEA;
  FCrcHandler    : TCRC32;
  FEskSerial     : String;
  FEskVersion    : String;
  FEskCode       : String;
  FPin           : String;
  FProjectList   : TStrings;
  FPermList      : TStrings;
  FExtParams     : TStrings;
  FDealerData    : TDealerData;
 public
  constructor Create(XmlDoc: IXMLDocument; RemoteDevice: TRemoteDevice);
  destructor Destroy; override;

  function GetRequestFromDocument: Boolean; override;
  function Execute(var ErrCode: Integer; var UserError: String): Boolean; override;
  function AddAnswerToDocument: Boolean; override;
  function CalculateCRC(Source: String): String;
  function Encrypt(Source, Key: String): String;
  function Decrypt(Source, Key: String): String;
  function DecryptCode(ESKCode_, ESKKey_: String): String;

  property EskSerial: String read FEskSerial write FEskSerial;
  property EskVersion: String read FEskVersion write FEskVersion;
  property EskCode: String read FEskCode write FEskCode;
  property Pin: String read FPin write FPin;
  property ProjectList: TStrings read FProjectList write FProjectList;
  property PermissionList: TStrings read FPermList write FPermList;
  property ExtendedParams: TStrings read FExtParams write FExtParams;
  property DealerData: TDealerData read FDealerData write FDealerData;
 end;

//*****************************************************
//  ESK Create
//*****************************************************

 TCmd_ESKCreateClient = class(TCommandClient)
 private
  FLoginHandler  : THndr_LoginClient;
  FEskSerial     : String;
  FEskVersion    : String;
  FEskCode       : String;
  FEskKey        : String;

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

  property LoginUserName: String read FGetUserName write FSetUserName;
  property LoginPassword: String read FGetPassword write FSetPassword;
  property EskSerial: String read FEskSerial write FEskSerial;
  property EskVersion: String read FEskVersion write FEskVersion;
  property EskCode: String read FEskCode write FEskCode;
  property EskKey: String read FEskKey write FEskKey;
 end;

 TCmd_ESKCreateServer = class(THandlerServerUserEvent)
 private
  FLoginHandler  : THndr_LoginServer;
  FEskSerial     : String;
  FEskVersion    : String;
  FEskCode       : String;
  FEskKey        : String;

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
  property EskSerial: String read FEskSerial write FEskSerial;
  property EskVersion: String read FEskSerial write FEskSerial;
  property EskCode: String read FEskSerial write FEskSerial;
  property EskKey: String read FEskKey write FEskKey;
 end;

//*****************************************************
//  ESK Read
//*****************************************************

 TCmd_ESKReadClient = class(TCommandClient)
 private
  FESKLoginHndr  : THndr_ESKLoginClient;
 public
  constructor Create(XmlDoc: IXMLDocument); override;
  destructor Destroy; override;

  function GetCommandName: String; override;
  function AddRequestToDocument: Boolean; override;
  function GetAnswerFromDocument: Boolean; override;

  property ESKLoginData: THndr_ESKLoginClient read FESKLoginHndr write FESKLoginHndr;
 end;

 TCmd_ESKReadServer = class(THandlerServerUserEvent)
 private
  FESKLoginHndr  : THndr_ESKLoginServer;
  function FGetESKSerial: String;
  function FGetEskVersion: String;
 public
  constructor Create(XmlDoc: IXMLDocument; RemoteDevice: TRemoteDevice);
  destructor Destroy; override;

  function GetRequestFromDocument: Boolean; override;
  function Execute(var ErrCode: Integer; var UserError: String): Boolean; override;
  function AddAnswerToDocument: Boolean; override;

  property ESKLoginData: THndr_ESKLoginServer read FESKLoginHndr write FESKLoginHndr;
  property ESKSerial: String read FGetESKSerial;
  property EskVersion: String read FGetEskVersion;
 end;

//*****************************************************
//  ESK Change PIN
//*****************************************************

 TCmd_ESKChangePinClient = class(TCommandClient)
 private
  FNewPin        : String;
  FUserFullName  : String;
  FESKLoginHndr  : THndr_ESKLoginClient;
  function FGetESKSerial: String;
 public
  constructor Create(XmlDoc: IXMLDocument); override;
  destructor Destroy; override;

  function GetCommandName: String; override;
  function AddRequestToDocument: Boolean; override;
  function GetAnswerFromDocument: Boolean; override;

  property ESKSerial: String read FGetESKSerial;
  property NewPin: String read FNewPin write FNewPin;
  property UserFullName: String read FUserFullName write FUserFullName;
  property ESKLoginData: THndr_ESKLoginClient read FESKLoginHndr write FESKLoginHndr;
 end;

 TCmd_ESKChangePinServer = class(THandlerServerUserEvent)
 private
  FNewPin        : String;
  FUserFullName  : String;
  FESKLoginHndr  : THndr_ESKLoginServer;
  function FGetESKSerial: String;
  function FGetEskVersion: String;
 public
  constructor Create(XmlDoc: IXMLDocument; RemoteDevice: TRemoteDevice);
  destructor Destroy; override;

  function GetRequestFromDocument: Boolean; override;
  function Execute(var ErrCode: Integer; var UserError: String): Boolean; override;
  function AddAnswerToDocument: Boolean; override;

  property ESKLoginData: THndr_ESKLoginServer read FESKLoginHndr write FESKLoginHndr;
  property ESKSerial: String read FGetESKSerial;
  property EskVersion: String read FGetEskVersion;
  property NewPin: String read FNewPin write FNewPin;
  property UserFullName: String read FUserFullName write FUserFullName;  
 end;


implementation
uses BillingConstUnit, DBInterfaceUnit, Math, DateUtils, MD5, WinUtilsUnit;

//******************************************************************************
//   TDealerData
//******************************************************************************
procedure TDealerData.ReadFromXML;
var iNode : IXMLNode;
begin
  iNode  := XML_GetRootNode;
  iNode  := XML_FindNodeByName(iNode, xml_Node_DealerData);
  FUserFullName  := XML_GetNodeText(iNode, xml_Node_UserFullName, false);
  FUserLevel     := XML_GetNodeText(iNode, xml_Node_UserLevel, false);
  FCompanyEIK    := XML_GetNodeText(iNode, xml_Node_CompanyEIK);
  FCompanyName   := XML_GetNodeText(iNode, xml_Node_CompanyName);
  FCompanyTown   := XML_GetNodeText(iNode, xml_Node_CompanyTown, true, false);
  FCompanyAddres := XML_GetNodeText(iNode, xml_Node_CompanyAddres, true, false);
  FCompanyPhone  := XML_GetNodeText(iNode, xml_Node_CompanyPhone, true, false);
  FCompanyContrN := XML_GetNodeText(iNode, xml_Node_CompanyContrN, true, false);
  FCompanyContrD := XML_GetNodeText(iNode, xml_Node_CompanyContrD, true, false);
  FBranchName    := XML_GetNodeText(iNode, xml_Node_BranchName);
  FBranchTown    := XML_GetNodeText(iNode, xml_Node_BranchTown, true, false);
  FBranchAddres  := XML_GetNodeText(iNode, xml_Node_BranchAddres, true, false);
  FBranchPhone   := XML_GetNodeText(iNode, xml_Node_BranchPhone, true, false);
  FBranchID      := StrToInt(XML_GetNodeText(iNode, xml_Node_BranchID));
  FUserID        := StrToInt(XML_GetNodeText(iNode, xml_Node_UserID));
  FBranchFiscCtr := XML_GetNodeText(iNode, xml_Node_BranchFiscCtr, false);
end;

procedure TDealerData.WriteToXML;
var iNode : IXMLNode;
begin
  iNode := XML_GetRootNode;
  iNode := iNode.AppendChild(XmlDocument.CreateElement(xml_Node_DealerData));
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_UserFullName)).Text  := FUserFullName;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_UserLevel)).Text     := FUserLevel;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_CompanyEIK)).Text    := FCompanyEIK;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_CompanyName)).Text   := FCompanyName;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_CompanyTown)).Text   := FCompanyTown;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_CompanyAddres)).Text := FCompanyAddres;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_CompanyPhone)).Text  := FCompanyPhone;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_CompanyContrN)).Text := FCompanyContrN;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_CompanyContrD)).Text := FCompanyContrD;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_BranchName)).Text    := FBranchName;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_BranchTown)).Text    := FBranchTown;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_BranchAddres)).Text  :=FBranchAddres;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_BranchPhone)).Text   :=FBranchPhone;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_BranchID)).Text      := IntToStr(FBranchID);
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_UserID)).Text        := IntToStr(FUserID);
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_BranchFiscCtr)).Text := FBranchFiscCtr;
end;

//******************************************************************************
//   THndr_ESKLoginClient
//******************************************************************************
constructor THndr_ESKLoginClient.Create(XmlDoc: IXMLDocument);
begin
 inherited Create(XmlDoc);
 FCryptoHandler := TCrypto_TEA.Create;
 FCrcHandler    := TCRC32.Create;
 FPermList      := TStringList.Create;
 FExtParams     := TStringList.Create;
 FProjectList   := TStringList.Create;
 FDealerData    := nil;

 FExtParams.Values[C_Project_SwName] := ChangeFileExt(ExtractFileName(Application.ExeName), '');
 FExtParams.Values[C_Project_SwVer]  := Win_GetAppVersion(Application.ExeName);
 FExtParams.Values[C_Project_Host]   := Win_GetHostName;
 FExtParams.Values[C_Project_Path]   := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName));
end;

destructor THndr_ESKLoginClient.Destroy;
begin
 FCryptoHandler.Free;
 FCrcHandler.Free;
 FPermList.Free;
 FExtParams.Free;
 FProjectList.Free;
 if FDealerData <> nil then FDealerData.Free;
 inherited Destroy;
end;

function THndr_ESKLoginClient.AddRequestToDocument: Boolean;
var iNode : IXMLNode;
    SDate : String;
    SCRC  : String;
    SCode : String;
    SPin  : String;
    I     : Integer;
begin
 try
  if FEskSerial = '' then raise EAbort.Create('Internal error!. Empty EskSerial');
  if FEskCode   = '' then raise EAbort.Create('Internal error!. Empty EskCode');
  if FEskKey    = '' then raise EAbort.Create('Internal error!. Empty EskKey');

  SDate := FormatDateTime('DD.MM.YY HH:NN:SS', Now);
  SCode := EncryptCode(FEskCode, FEskKey);
  SPin  := Encrypt(FEskSerial+FPin, FEskSerial+SDate);
  SCRC  := CalculateCRC(FEskSerial + FEskVersion + SCode + SPin + SDate);

  iNode := XML_GetRootNode;
  iNode := iNode.AppendChild(XmlDocument.CreateElement(xml_Node_ESKLogin));
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_ESKSerial)).Text  := FEskSerial;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_ESKVersion)).Text := FEskVersion;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_ESKCode)).Text    := SCode;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_ESKPin)).Text     := SPin;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_Time)).Text       := SDate;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_CRC)).Text        := SCRC;

{  with iNode.AppendChild(XmlDocument.CreateElement(xml_Node_ESKPermit)) do
   begin
    for I := 0 to FPermList.Count - 1 do
     AppendChild(XmlDocument.CreateElement('Perm'+FPermList.Names[I])).Text  := '?';
   end;}

  with iNode.AppendChild(XmlDocument.CreateElement(xml_Node_ESKProjects)) do
   begin
    for I := 0 to FProjectList.Count - 1 do
     AppendChild(XmlDocument.CreateElement(xml_Node_ESKProject)).Text  := FProjectList.Strings[I];
   end;

  with iNode.AppendChild(XmlDocument.CreateElement(xml_Node_ESKExtParams)) do
   begin
    for I := 0 to FExtParams.Count - 1 do
     begin
      if FExtParams.Names[I] <> '' then
        AppendChild(XmlDocument.CreateElement(FExtParams.Names[I])).Text := FExtParams.ValueFromIndex[I];
     end;
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

function THndr_ESKLoginClient.GetAnswerFromDocument: Boolean;
var iNode : IXMLNode;
    iRoot : IXMLNode;
    SSer  : String;
    SVer  : String;
    SDate : String;
    SCRC  : String;
begin
 try
  iRoot  := XML_GetRootNode;
  if XML_FindNodeByName(iRoot, xml_Node_DealerData, false) <> nil then FDealerData := TDealerData.Create(XmlDocument);
  iRoot  := XML_FindNodeByName(iRoot, xml_Node_ESKLogin);
  SSer   := XML_GetNodeText(iRoot, xml_Node_ESKSerial);
  SVer   := XML_GetNodeText(iRoot, xml_Node_ESKVersion);
  SDate  := XML_GetNodeText(iRoot, xml_Node_Time);
  SCRC   := XML_GetNodeText(iRoot, xml_Node_CRC);

  if SSer <> FEskSerial then raise EAbort.Create('ESK serial differs!!!');
  if SVer <> FEskVersion then raise EAbort.Create('ESK version differs!!!');
  if SCRC <> CalculateCRC(FEskSerial + FEskVersion + SDate) then raise EAbort.Create('Invalid signature');

  FPermList.Clear;
  iNode := XML_FindNodeByName(iRoot, xml_Node_ESKPermit);
  iNode := iNode.firstChild;
  while iNode <> nil do
   begin
    FPermList.Add(iNode.nodeName + '=' + iNode.text);
    iNode := iNode.nextSibling;
   end;

  FExtParams.Clear;
  iNode := XML_FindNodeByName(iRoot, xml_Node_ESKExtParams, false);
  if iNode <> nil then
   begin
    iNode := iNode.firstChild;
    while iNode <> nil do
     begin
      FExtParams.Values[iNode.nodeName] := iNode.text;
      iNode := iNode.nextSibling;
     end;
   end;

  if FDealerData <> nil then FDealerData.ReadFromXML;

  Result := true;
 except
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'] GetAnswerFromDocument Fail: '+E.Message;
    Result := false;
   end;
 end;
end;

function THndr_ESKLoginClient.EncryptCode(ESKCode_, ESKKey_: String): String;
var Cnt : Integer;
    D2k : TDateTime;
begin
 Randomize;
 Cnt := RandomRange(6, $F);
 D2k := EncodeDate(2000, 1, 1);
 if (Cnt < 6)or(Cnt > $F) then Cnt := 6;
 Result := IntToHex(Cnt, 1) + IntToHex(DaysBetween(D2k, Date) , 4);
 while Length(Result) < Cnt do Result := Result + IntToHex(RandomRange(1, $F), 1);
 Result := Encrypt(Result + ESKCode_, ESKKey_);
end;

function THndr_ESKLoginClient.CalculateCRC(Source: String): String;
begin
 FCrcHandler.Reset;
 FCrcHandler.Process(Source[1], Length(Source));
 Result := FCrcHandler.AsString;
end;

function THndr_ESKLoginClient.Encrypt(Source, Key: String): String;
begin
 Result := FCryptoHandler.EncryptStringEx(Source, Key);
 if (Source <> '')and(Result = '') then raise EAbort.Create('Fail encrypt: '+FCryptoHandler.LastError);
end;

function THndr_ESKLoginClient.Decrypt(Source, Key: String): String;
begin
 Result := FCryptoHandler.DecryptStringEx(Source, Key);
 if (Source <> '')and(Result = '') then raise EAbort.Create('Fail decrypt: '+FCryptoHandler.LastError);
end;

procedure THndr_ESKLoginClient.AssignEskData(EskData: TEskData);
begin
  FEskSerial     := EskData.Serial;
  FEskVersion    := EskData.Version;
  FEskCode       := EskData.Code;
  FEskKey        := EskData.Key;
  FProjectList.Text := EskData.Modules;
end;

//******************************************************************************
//   THndr_ESKLoginServer
//******************************************************************************
constructor THndr_ESKLoginServer.Create(XmlDoc: IXMLDocument; RemoteDevice: TRemoteDevice);
begin
 inherited Create(XmlDoc, RemoteDevice);
 FCryptoHandler := TCrypto_TEA.Create;
 FCrcHandler    := TCRC32.Create;
 FProjectList   := TStringList.Create;
 FPermList      := TStringList.Create;
 FExtParams     := TStringList.Create;
 FDealerData    := nil;
end;

destructor THndr_ESKLoginServer.Destroy;
begin
 FCryptoHandler.Free;
 FCrcHandler.Free;
 FProjectList.Free;
 FPermList.Free;
 FExtParams.Free;
 if FDealerData <> nil then FDealerData.Free;
 inherited Destroy;
end;

function THndr_ESKLoginServer.GetRequestFromDocument: Boolean;
var iNode : IXMLNode;
    iRoot : IXMLNode;
    SDate : String;
    SCRC  : String;
begin
 try
  iRoot       := XML_GetRootNode;
  iRoot       := XML_FindNodeByName(iRoot, xml_Node_ESKLogin);
  FEskSerial  := XML_GetNodeText(iRoot, xml_Node_ESKSerial);
  FEskVersion := XML_GetNodeText(iRoot, xml_Node_ESKVersion);
  FEskCode    := XML_GetNodeText(iRoot, xml_Node_ESKCode);
  FPin        := XML_GetNodeText(iRoot, xml_Node_ESKPin);
  SDate       := XML_GetNodeText(iRoot, xml_Node_Time);
  SCRC        := XML_GetNodeText(iRoot, xml_Node_CRC);

  if SCRC <> CalculateCRC(FEskSerial + EskVersion + FEskCode + FPin + SDate) then raise EAbort.Create('Invalid signature');

  FPin := Decrypt(FPin, FEskSerial+SDate);
  if Copy(FPin, 1, Length(FEskSerial)) <> FEskSerial then raise EAbort.Create('Invalid password -> decrypt fail');
  Delete(FPin, 1, Length(FEskSerial));

{  FPermList.Clear;
  iNode := XML_FindNodeByName(iRoot, xml_Node_ESKPermit);
  iNode := iNode.firstChild;
  while iNode <> nil do
   begin
    FPermList.Add(iNode.nodeName+'='+iNode.text);
    iNode := iNode.nextSibling;
   end;}

  FProjectList.Clear;
  iNode := XML_FindNodeByName(iRoot, xml_Node_ESKProjects);
  iNode := iNode.firstChild;
  while iNode <> nil do
   begin
    FProjectList.Add(iNode.text);
    iNode := iNode.nextSibling;
   end;

  FExtParams.Clear;
  iNode := XML_FindNodeByName(iRoot, xml_Node_ESKExtParams, false);
  if iNode <> nil then
   begin
    iNode := iNode.firstChild;
    while iNode <> nil do
     begin
      FExtParams.Values[iNode.nodeName] := iNode.text;
      iNode := iNode.nextSibling;
     end;
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

function THndr_ESKLoginServer.Execute(var ErrCode: Integer; var UserError: String): Boolean;
var SQL : String;
    Ra  : Integer;

    procedure SetErrorCode(Code_: Integer; Message_: String);
    begin
     ErrCode   := Code_;
     UserError := Message_;
    end;
begin
 Result := true;
 FDealerData := TDealerData.Create(nil);
 try
  with Device.DbInterface do
   begin
    // Log software to DB
     if (FExtParams.Values[C_Project_SwName] <> '')and(FExtParams.Values[C_Project_SwVer] <> '')and
        (FExtParams.Values[C_Project_Host] <> '')and(FExtParams.Values[C_Project_Path] <> '')and
        (not SameText(FExtParams.Values[C_Project_Host], Win_GetHostName)) then // да не се записва web съръра на COLORADO
      begin
       //SwName=w3wp SwVersion=7.5.7600.16385 (win7_rtm.090713-1255) Host=COLORADO Path=C:\Windows\SysWOW64\inetsrv
       SQL := 'SELECT PV_ID, PV_VERSION FROM PROJECTS_VERSIONS WHERE '+
              '(PV_PROJECTNAME = '+StrToSQL(FExtParams.Values[C_Project_SwName])+')AND'+
              '(PV_HOSTNAME = '   +StrToSQL(FExtParams.Values[C_Project_Host])+')AND'+
              '(PV_LOCALPATH = '  +StrToSQL(FExtParams.Values[C_Project_Path])+')AND'+
              '(PV_ESKSERIAL = '  +StrToSQL(FEskSerial)+')';
       if not FillDataSet(SQL) then raise EAbort.Create('Read PROJECTS_VERSIONS data fail: '+LastError);
       if DataSet.RecordCount > 0 then
        begin
         SQL := 'UPDATE PROJECTS_VERSIONS SET '+
                'PV_LASTESKLOGIN = '+DateTimeToSQL(Now)+' ';
         if DataSet.FieldByName('PV_VERSION').AsString <> FExtParams.Values[C_Project_SwVer] then
          SQL := SQL + ', PV_VERSION = '+StrToSQL(FExtParams.Values[C_Project_SwVer], 20)+
                       ', PV_DATA = '+ StrToSQL(FExtParams.Text)+
                       ', PV_MODIFYDATE = getdate() ';

         SQL := SQL + 'WHERE PV_ID = '+IntToSql(DataSet.FieldByName('PV_ID').AsInteger);
         if not ExecuteSQLStatement(SQL, RA) then EAbort.Create('Update PROJECTS_VERSIONS fail: '+LastError);
        end
       else
        begin
         SQL := 'INSERT INTO PROJECTS_VERSIONS (PV_PROJECTNAME, PV_HOSTNAME, PV_LOCALPATH, '+
                'PV_ESKSERIAL, PV_VERSION, PV_DATA, PV_LASTESKLOGIN)VALUES ('+
                StrToSQL(FExtParams.Values[C_Project_SwName], 100) +','+ //[PV_PROJECTNAME] varchar(100) NOT NULL,
                StrToSQL(FExtParams.Values[C_Project_Host], 100)   +','+ //[PV_HOSTNAME] varchar(100) NULL,
                StrToSQL(FExtParams.Values[C_Project_Path], 100)   +','+ //[PV_LOCALPATH] varchar(100) NULL,
                StrToSQL(EskSerial, 10)                            +','+ //[PV_ESKSERIAL] varchar(10) NULL,
                StrToSQL(FExtParams.Values[C_Project_SwVer], 20)   +','+ //[PV_VERSION] varchar(20) NULL,
                StrToSQL(FExtParams.Text)                          +','+ //[PV_DATA] varchar(max) NULL,
                DateTimeToSQL(Now)                                 +')'; //[PV_LASTESKLOGIN] datetime NULL,
         if not ExecuteSQLStatement(SQL, RA) then EAbort.Create('Insert PROJECTS_VERSIONS fail: '+LastError);
        end;
       CloseDataSet;
      end;

    // Find Device by serial
    SQL := 'SELECT ESK_CODE, ESK_KEY, ESK_EXPIREDATE, ESK_ACTIVE '+
           'FROM DEVICES_ESK '+
           'WHERE (ESK_SERIAL = '+StrToSQL(FEskSerial)+')';
    if not FillDataSet(SQL) then raise EAbort.Create('Select ESK Device fail: '+LastError);

    if DataSet.RecordCount = 0 then
     raise EHandledException.Create(errcode_LoginFail_ESKNotFound, 'Неуспешен логин!'+sLineBreak+
                                                                   'Непознат "Електроннен ключ"!',
                                                                   'ESK not found in table DEVICES_ESK -> '+FEskSerial);

    FEskCode := DecryptCode(FEskCode, DataSet.FieldByName('ESK_KEY').AsString);

    if FEskCode = '$$DateError$$' then
     raise EHandledException.Create(errcode_LoginFail_ESKInvalidCode, 'Неуспешен логин!'+sLineBreak+
                                                                      'Моля проверете датата на компютъра си.',
                                                                      'ESK Code error: Local PC date differs from server');

    if FEskCode <> DataSet.FieldByName('ESK_CODE').AsString then
     raise EHandledException.Create(errcode_LoginFail_ESKInvalidCode, 'Неуспешен логин!'+sLineBreak+
                                                                      'Невалиден "Електроннен ключ"!',
                                                                      'ESK Code donot match: '+FEskCode+'<->'+DataSet.FieldByName('ESK_CODE').AsString);

    if DataSet.FieldByName('ESK_ACTIVE').AsInteger = 0 then
     raise EHandledException.Create(errcode_LoginFail_ESKDisabled, 'Неуспешен логин!'+sLineBreak+
                                                                   'Деактивиран "Електроннен ключ"!',
                                                                   'ESK not active -> '+FEskSerial);

    if (not DataSet.FieldByName('ESK_EXPIREDATE').IsNull)and(DataSet.FieldByName('ESK_EXPIREDATE').AsDateTime < Date) then
     raise EHandledException.Create(errcode_LoginFail_ESKExpired, 'Неуспешен логин!'+sLineBreak+
                                                                  'Изтекъл "Електроннен ключ"!',
                                                                  'EKS expired -> '+FEskSerial+' / '+DataSet.FieldByName('ESK_EXPIREDATE').AsString);

    SQL := 'SELECT DU.DU_ID, DU.DU_DEALEREIK, DU.DU_NAME, DU.DU_ACTIVE, DU.DU_EXPIREDATE, '+
           'DU.DU_ACCESSLEVEL, DU.DU_FISCCODE, '+
           'DB.DB_ID, DB.DB_NAME, DB.DB_TOWNNAME, DB.DB_ADDRESS, DB.DB_FISCALCENTER, '+
           'DB.DB_PHONE_G1, DB.DB_PHONE_G2, DB.DB_PHONE_M1, DB.DB_PHONE_M2, '+
           'D.D_EIK, D.D_NAME, D.D_TOWNNAME, D.D_ADDRESS, D.D_PHONE_G1, D.D_PHONE_G2, D.D_PHONE_M1, '+
           'D.D_PHONE_M2, D.D_CONTRACTDATE, D.D_CONTRACTNUMB, D.D_ACTIVE, D.D_EXPIREDATE '+
           'FROM DEALERS_USERS DU '+
           'LEFT JOIN DEALERS_BRANCHES DB ON DB.DB_ID = DU.DU_DEALERBRANCH '+
           'LEFT JOIN DEALERS D ON D.D_EIK = DU.DU_DEALEREIK '+
           'WHERE (DU_USERNAME = '+StrToSQL(FEskSerial)+')AND(DU_PASSWORD = '+StrToSQL(MD5Print(MD5String(Pin)))+')';
    if not FillDataSet(SQL) then raise EAbort.Create('Select DEALER_USER fail: '+LastError);

    if DataSet.RecordCount = 0 then
     raise EHandledException.Create(errcode_LoginFail_UserNotFound, 'Неуспешен логин!'+sLineBreak+
                                                                    'Непознат потребител и/или ПИН!',
                                                                    '');

    if DataSet.FieldByName('DU_ACTIVE').AsInteger = 0 then
     raise EHandledException.Create(errcode_LoginFail_UserDisabled, 'Неуспешен логин!'+sLineBreak+
                                                                    'Деактивиран потребител!',
                                                                    'ESK user not active -> '+FEskSerial);

    if (not DataSet.FieldByName('DU_EXPIREDATE').IsNull)and(DataSet.FieldByName('DU_EXPIREDATE').AsDateTime < Date) then
     raise EHandledException.Create(errcode_LoginFail_UserExpired, 'Неуспешен логин!'+sLineBreak+
                                                                   'Изтекъл потребител!',
                                                                   'ESK user expired -> '+FEskSerial+' / '+DataSet.FieldByName('DU_EXPIREDATE').AsString);

    if DataSet.FieldByName('D_ACTIVE').AsInteger = 0 then
     raise EHandledException.Create(errcode_LoginFail_DealerDisabled, 'Неуспешен логин!'+sLineBreak+
                                                                      'Деактивиран потребител/фирма!',
                                                                      'Dealer not active -> '+DataSet.FieldByName('DU_NAME').AsString);

    if (not DataSet.FieldByName('D_EXPIREDATE').IsNull)and(DataSet.FieldByName('D_EXPIREDATE').AsDateTime < Date) then
     raise EHandledException.Create(errcode_LoginFail_DealerExpired, 'Неуспешен логин!'+sLineBreak+
                                                                     'Изтекъл потребител/фирма!',
                                                                     'Dealer expired -> '+DataSet.FieldByName('DU_NAME').AsString+' / '+DataSet.FieldByName('D_EXPIREDATE').AsString);

    FDealerData.FUserFullName := DataSet.FieldByName('DU_NAME').AsString;
    FDealerData.FUserLevel    := DataSet.FieldByName('DU_ACCESSLEVEL').AsString;
    FDealerData.FCompanyEIK   := DataSet.FieldByName('D_EIK').AsString;
    FDealerData.FCompanyName  := DataSet.FieldByName('D_NAME').AsString;
    FDealerData.FCompanyTown  := DataSet.FieldByName('D_TOWNNAME').AsString;
    FDealerData.FCompanyAddres:= DataSet.FieldByName('D_ADDRESS').AsString;
    FDealerData.FCompanyPhone := DataSet.FieldByName('D_PHONE_G1').AsString;
    if FDealerData.FCompanyPhone = '' then FDealerData.FCompanyPhone := DataSet.FieldByName('D_PHONE_G2').AsString;
    if FDealerData.FCompanyPhone = '' then FDealerData.FCompanyPhone := DataSet.FieldByName('D_PHONE_M1').AsString;
    if FDealerData.FCompanyPhone = '' then FDealerData.FCompanyPhone := DataSet.FieldByName('D_PHONE_M2').AsString;
    FDealerData.FCompanyContrN:= DataSet.FieldByName('D_CONTRACTDATE').AsString;
    FDealerData.FCompanyContrD:= DataSet.FieldByName('D_CONTRACTNUMB').AsString;
    FDealerData.FBranchName   := DataSet.FieldByName('DB_NAME').AsString;
    FDealerData.FBranchTown   := DataSet.FieldByName('DB_TOWNNAME').AsString;
    FDealerData.FBranchAddres := DataSet.FieldByName('DB_ADDRESS').AsString;
    FDealerData.FBranchPhone  := DataSet.FieldByName('DB_PHONE_G1').AsString;
    if FDealerData.FBranchPhone = '' then FDealerData.FBranchPhone := DataSet.FieldByName('DB_PHONE_G2').AsString;
    if FDealerData.FBranchPhone = '' then FDealerData.FBranchPhone := DataSet.FieldByName('DB_PHONE_M1').AsString;
    if FDealerData.FBranchPhone = '' then FDealerData.FBranchPhone := DataSet.FieldByName('DB_PHONE_M2').AsString;

    FDealerData.FBranchID     := DataSet.FieldByName('DB_ID').AsInteger;
    FDealerData.FUserID       := DataSet.FieldByName('DU_ID').AsInteger;
    FDealerData.FBranchFiscCtr:= DataSet.FieldByName('DB_FISCALCENTER').AsString;

    SQL := 'SELECT DUR_RIGHTID FROM DEALERS_USERSRIGHTS WHERE DUR_USERID = ' + DataSet.FieldByName('DU_ID').AsString;
    if not FillDataSet(SQL) then raise EAbort.Create('Select DEALERS_USERSRIGHTS fail: '+LastError);
    FPermList.Clear;
    while not DataSet.Eof do
     begin
      FPermList.Values[DataSet.FieldByName('DUR_RIGHTID').AsString] := 'YES';
      DataSet.Next;
     end;

    CloseDataSet;
    SQL := 'UPDATE DEALERS_USERS SET DU_LASTLOGIN = getdate() '+
           'WHERE DU_USERNAME = '+StrToSQL(FEskSerial);
    if not ExecuteSQLStatement(SQL, Ra) then raise EAbort.Create('Update DEALER_USER fail: '+LastError);

    SQL := 'UPDATE DEVICES_ESK SET '+
           'ESK_VERSION = '+StrToSQL(FEskVersion)        +', '+
           'ESK_LASTACTION = getdate()'                  +', '+
           'ESK_PROJECTS = '+StrToSQL(FProjectList.Text) +' '+
           'WHERE ESK_SERIAL = '+StrToSQL(FEskSerial);
    if not ExecuteSQLStatement(SQL, Ra) then raise EAbort.Create('Update DEVICES_ESK fail: '+LastError);

    SetErrorCode(errcode_ExecuteSucceed, '');
  end;
 except
  on E: EHandledException do
   begin
    if E.SystemMessage <> '' then
     begin
      Device.PostEventSystem(C_EvType_Error, 'Error code:'+IntToStr(E.ErrorCode)+sLineBreak+
                                             E.Message+sLineBreak+
                                             E.SystemMessage, Self.ClassName+'/Execute');
     end;
    FreeAndNil(FDealerData);
    SetErrorCode(E.ErrorCode, E.Message);
    Result := true;
   end;
  on E: Exception do
   begin
    FreeAndNil(FDealerData);
    LastError := '['+Self.ClassName+'][Execute]'+E.Message;
    Result    := false;
   end;
 end;
 Device.DbInterface.CloseDataSet;
end;

function THndr_ESKLoginServer.AddAnswerToDocument: Boolean;
var iNode : IXMLNode;
    SDate : String;
    SCRC  : String;
    I     : Integer;
begin
 try
  SDate := FormatDateTime('DD.MM.YY HH:NN:SS', Now);
  SCRC  := CalculateCRC(FEskSerial+FEskVersion+SDate);

  iNode := XML_GetRootNode;
  iNode := iNode.AppendChild(XmlDocument.CreateElement(xml_Node_ESKLogin));
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_ESKSerial)).Text  := FEskSerial;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_ESKVersion)).Text := FEskVersion;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_Time)).Text       := SDate;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_CRC)).Text        := SCRC;

  with iNode.AppendChild(XmlDocument.CreateElement(xml_Node_ESKPermit)) do
   begin
    for I := 0 to FPermList.Count - 1 do
     begin
      AppendChild(XmlDocument.CreateElement(FPermList.Names[I])).Text := FPermList.Values[FPermList.Names[I]];
     end;
   end;

  with iNode.AppendChild(XmlDocument.CreateElement(xml_Node_ESKExtParams)) do
   begin
    for I := 0 to FExtParams.Count - 1 do
     begin
      if FExtParams.Names[I] <> '' then
        AppendChild(XmlDocument.CreateElement(FExtParams.Names[I])).Text := FExtParams.ValueFromIndex[I];
     end;
   end;

  if FDealerData <> nil then
   begin
    FDealerData.XmlDocument := XmlDocument;
    FDealerData.WriteToXML;
   end;

  Result := true;
 except
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'] AddAnswerToDocument Fail: '+E.Message;
    Result := false;
   end;
 end;
end;

function THndr_ESKLoginServer.CalculateCRC(Source: String): String;
begin
 FCrcHandler.Reset;
 FCrcHandler.Process(Source[1], Length(Source));
 Result := FCrcHandler.AsString;
end;

function THndr_ESKLoginServer.Encrypt(Source, Key: String): String;
begin
 Result := FCryptoHandler.EncryptStringEx(Source, Key);
 if (Source <> '')and(Result = '') then raise EAbort.Create('Fail encrypt: '+FCryptoHandler.LastError);
end;

function THndr_ESKLoginServer.Decrypt(Source, Key: String): String;
begin
 Result := FCryptoHandler.DecryptStringEx(Source, Key);
 if (Source <> '')and(Result = '') then raise EAbort.Create('Fail decrypt: '+FCryptoHandler.LastError);
end;

function THndr_ESKLoginServer.DecryptCode(ESKCode_, ESKKey_: String): String;
var Cnt : Integer;
    Days: Integer;
    D2k : TDateTime;
begin
 Result := Decrypt(ESKCode_, ESKKey_);
 if Result <> '' then
  begin
   Cnt := StrToIntDef('$'+Copy(Result, 1, 1), 0);
   Days:= StrToIntDef('$'+Copy(Result, 2, 4), 0);
   D2k := EncodeDate(2000, 1, 1);
   Delete(Result, 1, Cnt);
   if Days <> DaysBetween(D2k, Date) then Result := '$$DateError$$';
  end;
end;

//******************************************************************************
//   TCmd_ESKCreateClient
//******************************************************************************
constructor TCmd_ESKCreateClient.Create(XmlDoc: IXMLDocument);
begin
 inherited Create(XmlDoc);
 FLoginHandler := THndr_LoginClient.Create(XmlDoc);
 FEskSerial    := '';
 FEskVersion   := '';
 FEskCode      := '';
 FEskKey       := '';
end;

destructor TCmd_ESKCreateClient.Destroy;
begin
 FLoginHandler.Free;
 inherited Destroy;
end;

function TCmd_ESKCreateClient.GetCommandName: String;
begin
 Result := cmd_ESKCreate;
end;

function TCmd_ESKCreateClient.FGetUserName: String;
begin
 Result := FLoginHandler.UserName;
end;

function TCmd_ESKCreateClient.FGetPassword: String;
begin
 Result := FLoginHandler.Password;
end;

procedure TCmd_ESKCreateClient.FSetUserName(Value: String);
begin
 FLoginHandler.UserName := Value;
end;

procedure TCmd_ESKCreateClient.FSetPassword(Value: String);
begin
 FLoginHandler.Password := Value;
end;

function TCmd_ESKCreateClient.AddRequestToDocument: Boolean;
var iNode : IXMLNode;
    SDate : String;
    SCRC  : String;
begin
 try
  if not FLoginHandler.AddRequestToDocument then raise EAbort.Create(FLoginHandler.LastError);

  SDate := FormatDateTime('DD.MM.YY HH:NN:SS', Now);
  SCRC  := FLoginHandler.CalculateCRC(FEskSerial + FEskVersion + SDate);

  iNode := XML_GetRootNode;
  iNode := iNode.AppendChild(XmlDocument.CreateElement(xml_Node_ESKData));
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_ESKSerial)).Text  := FEskSerial;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_ESKVersion)).Text := FEskVersion;
//  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_ESKCode)).Text    := '?';
//  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_ESKKey)).Text     := '?';
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_Time)).Text       := SDate;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_CRC)).Text        := SCRC;

  Result := true;
 except
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'] AddRequestToDocument Fail: '+E.Message;
    Result := false;
   end;
 end;
end;

function TCmd_ESKCreateClient.GetAnswerFromDocument: Boolean;
var iNode  : IXMLNode;
    SCode  : String;
    SKey   : String;
    SDate  : String;
    SCRC   : String;
begin
 try
  if not FLoginHandler.GetAnswerFromDocument then raise EAbort.Create(FLoginHandler.LastError);

  iNode    := XML_GetRootNode;
  iNode    := XML_FindNodeByName(iNode, xml_Node_ESKData);

  FEskSerial := XML_GetNodeText(iNode, xml_Node_ESKSerial);
  FEskVersion:= XML_GetNodeText(iNode, xml_Node_ESKVersion);
  SCode      := XML_GetNodeText(iNode, xml_Node_ESKCode);
  SKey       := XML_GetNodeText(iNode, xml_Node_ESKKey);
  SDate      := XML_GetNodeText(iNode, xml_Node_Time);
  SCRC       := XML_GetNodeText(iNode, xml_Node_CRC);

  if SCRC <> FLoginHandler.CalculateCRC(FEskSerial+FEskVersion+SCode+SKey+SDate) then raise EAbort.Create('Invalid signature');

  FEskCode := FLoginHandler.Decrypt(SCode, SDate+FEskSerial);
  if Copy(FEskCode, 1, Length(FEskSerial)) <> FEskSerial then raise EAbort.Create('Invalid ESKCode -> decrypt fail');
  Delete(FEskCode, 1, Length(FEskSerial));

  FEskKey := FLoginHandler.Decrypt(SKey, SDate+FEskSerial);
  if Copy(FEskKey, 1, Length(FEskSerial)) <> FEskSerial then raise EAbort.Create('Invalid ESKKey -> decrypt fail');
  Delete(FEskKey, 1, Length(FEskSerial));

  Result := true;
 except
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'] GetAnswerFromDocument Fail: '+E.Message;
    Result := false;
   end;
 end;
end;

//******************************************************************************
//   TCmd_ESKCreateServer
//******************************************************************************
constructor TCmd_ESKCreateServer.Create(XmlDoc: IXMLDocument; RemoteDevice: TRemoteDevice);
begin
 inherited Create(XmlDoc, RemoteDevice);
 FLoginHandler := THndr_LoginServer.Create(XmlDoc, RemoteDevice);
end;

destructor TCmd_ESKCreateServer.Destroy;
begin
 FLoginHandler.Free;
 inherited Destroy;
end;

function TCmd_ESKCreateServer.GetRequestFromDocument: Boolean;
var iNode  : IXMLNode;
    SDate  : String;
    SCRC   : String;
begin
 try
  if not FLoginHandler.GetRequestFromDocument then raise EAbort.Create(FLoginHandler.LastError);

  iNode    := XML_GetRootNode;
  iNode    := XML_FindNodeByName(iNode, xml_Node_ESKData);

  FEskSerial := XML_GetNodeText(iNode, xml_Node_ESKSerial);
  FEskVersion:= XML_GetNodeText(iNode, xml_Node_ESKVersion);
//  FEskCode   := XML_GetNodeText(iNode, xml_Node_ESKCode);
//  FEskKey    := XML_GetNodeText(iNode, xml_Node_ESKKey);
  SDate      := XML_GetNodeText(iNode, xml_Node_Time);
  SCRC       := XML_GetNodeText(iNode, xml_Node_CRC);

  if SCRC <> FLoginHandler.CalculateCRC(FEskSerial + FEskVersion + SDate) then raise EAbort.Create('Invalid signature');

  Result := true;
 except
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'] GetRequestFromDocument Fail: '+E.Message;
    Result := false;
   end;
 end;
end;

function TCmd_ESKCreateServer.Execute(var ErrCode: Integer; var UserError: String): Boolean;
var Ra : Integer;
    Cnt: Integer;
    SQL: String;

    procedure SetErrorCode(Code_: Integer; Message_: String);
    begin
     ErrCode   := Code_;
     UserError := Message_;
    end;
    function GenerateKey: String;
    begin
     Result := '';
     Randomize;
     while Length(Result) < 10 do Result := Result + Chr(Random(25)+65);
    end;
begin
 Result := true;
 try
  if not FLoginHandler.Execute(ErrCode, UserError) then raise EAbort.Create(FLoginHandler.LastError);

  if ErrCode = errcode_ExecuteSucceed then
   begin
    if FLoginHandler.PermissionList.Values['esk_createdevice'] <> 'YES' then
     raise EHandledException.Create(errcode_ESKCreate_Denied,
                                    'Неуспешно генериране на електронен ключ!'+sLineBreak+
                                    'Нямате необходимите права за генерирането'+sLineBreak+
                                    'на електронни ключове.');

    with Device.DbInterface do
     begin
      Cnt := 1;
      repeat
       SQL := 'SELECT ESK_SERIAL, ESK_CODE, ESK_KEY FROM DEVICES_ESK '+
              'WHERE ESK_SERIAL = '+StrToSQL(FEskSerial);
       if not FillDataSet(SQL) then raise EAbort.Create('Read ESK data fail: '+LastError);
       if DataSet.RecordCount < 1 then
        begin
         SQL := 'INSERT INTO DEVICES_ESK(ESK_SERIAL, ESK_VERSION, ESK_CODE, ESK_KEY)VALUES ('+
                StrToSQL(FEskSerial, 10)  +','+ // ESK_SERIAL,
                StrToSQL(FEskVersion,10)  +','+ // ESK_VERSION,
                'NEWID()'                 +','+ // ESK_CODE,
                StrToSQL(GenerateKey, 20) +')'; // ESK_KEY,
         if not ExecuteSQLStatement(SQL, Ra) then raise EAbort.Create('Insert ESK data fail: '+LastError);
        end;
       Inc(Cnt);
      until (DataSet.RecordCount > 0)or(Cnt > 5);

      FEskCode := DataSet.FieldByName('ESK_CODE').AsString;
      FEskKey  := DataSet.FieldByName('ESK_KEY').AsString;
      CloseDataSet;

      SetErrorCode(errcode_ExecuteSucceed, '');
     end;
   end;
 except
  on E: EHandledException do
   begin
    if E.SystemMessage <> '' then
     begin
      Device.PostEventSystem(C_EvType_Error, 'Error code:'+IntToStr(E.ErrorCode)+sLineBreak+
                                             E.Message+sLineBreak+
                                             E.SystemMessage, Self.ClassName+'/Execute');
     end;
    SetErrorCode(E.ErrorCode, E.Message);
    Result := true;
   end;
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'][Execute]'+E.Message;
    Result    := false;
   end;
 end;
end;

function TCmd_ESKCreateServer.AddAnswerToDocument: Boolean;
var iNode : IXMLNode;
    SDate : String;
    SCRC  : String;
    SCode : String;
    SKey  : String;
begin
 try
  FLoginHandler.XmlDocument := Self.XmlDocument; // много е важно да превключим документа
  if not FLoginHandler.AddAnswerToDocument then raise EAbort.Create(FLoginHandler.LastError);

  SDate := FormatDateTime('DD.MM.YY HH:NN:SS', Now);
  SCode := FLoginHandler.Encrypt(FEskSerial+FEskCode, SDate+FEskSerial);
  SKey  := FLoginHandler.Encrypt(FEskSerial+FEskKey,  SDate+FEskVersion);
  SCRC  := FLoginHandler.CalculateCRC(FEskSerial+FEskVersion+SCode+SKey+SDate);

  iNode := XML_GetRootNode;
  iNode := iNode.AppendChild(XmlDocument.CreateElement(xml_Node_ESKData));
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_ESKSerial)).Text  := FEskSerial;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_ESKVersion)).Text := FEskVersion;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_ESKCode)).Text    := SCode;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_ESKKey)).Text     := SKey;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_Time)).Text       := SDate;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_CRC)).Text        := SCRC;
                     
  Result := true;
 except
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'] AddAnswerToDocument Fail: '+E.Message;
    Result := false;
   end;
 end;
end;

function TCmd_ESKCreateServer.FGetUserName: String;
begin
 Result := FLoginHandler.UserName;
end;

function TCmd_ESKCreateServer.FGetPassword: String;
begin
 Result := FLoginHandler.Password;
end;

procedure TCmd_ESKCreateServer.FSetUserName(Value: String);
begin
 FLoginHandler.UserName := Value;
end;

procedure TCmd_ESKCreateServer.FSetPassword(Value: String);
begin
 FLoginHandler.Password := Value;
end;

//*****************************************************
//  TCmd_ESKTestClient
//*****************************************************
constructor TCmd_ESKReadClient.Create(XmlDoc: IXMLDocument);
begin
 inherited Create(XmlDoc);
 FESKLoginHndr := THndr_ESKLoginClient.Create(XmlDoc);
end;

destructor TCmd_ESKReadClient.Destroy;
begin
 FESKLoginHndr.Free;
 inherited Destroy;
end;

function TCmd_ESKReadClient.GetCommandName: String;
begin
 Result := cmd_ESKRead;
end;

function TCmd_ESKReadClient.AddRequestToDocument: Boolean;
begin
 try
  if not FESKLoginHndr.AddRequestToDocument then raise EAbort.Create(FESKLoginHndr.LastError);

  Result := true;
 except
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'] AddRequestToDocument Fail: '+E.Message;
    Result := false;
   end;
 end;
end;

function TCmd_ESKReadClient.GetAnswerFromDocument: Boolean;
begin
 try
  if not FESKLoginHndr.GetAnswerFromDocument then raise EAbort.Create(FESKLoginHndr.LastError);

  Result := true;
 except
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'] GetAnswerFromDocument Fail: '+E.Message;
    Result := false;
   end;
 end;
end;

//*****************************************************
//  TCmd_ESKTestServer
//*****************************************************
constructor TCmd_ESKReadServer.Create(XmlDoc: IXMLDocument; RemoteDevice: TRemoteDevice);
begin
 inherited Create(XmlDoc, RemoteDevice);
 FESKLoginHndr := THndr_ESKLoginServer.Create(XmlDoc, RemoteDevice);
end;

destructor TCmd_ESKReadServer.Destroy;
begin
 FESKLoginHndr.Free;
 inherited Destroy;
end;

function TCmd_ESKReadServer.GetRequestFromDocument: Boolean;
begin
 try
  if not FESKLoginHndr.GetRequestFromDocument then raise EAbort.Create(FESKLoginHndr.LastError);

  Result := true;
 except
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'] GetRequestFromDocument Fail: '+E.Message;
    Result := false;
   end;
 end;
end;

function TCmd_ESKReadServer.Execute(var ErrCode: Integer; var UserError: String): Boolean;
    procedure SetErrorCode(Code_: Integer; Message_: String);
    begin
     ErrCode   := Code_;
     UserError := Message_;
    end;
begin
 try
  if not FESKLoginHndr.Execute(ErrCode, UserError) then  raise EAbort.Create(FESKLoginHndr.LastError);

  Result := true;
 except
  on E: EHandledException do
   begin
    SetErrorCode(E.ErrorCode, E.Message);
    Result := true;
   end;
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'][Execute]'+E.Message;
    Result    := false;
   end;
 end;
end;

function TCmd_ESKReadServer.AddAnswerToDocument: Boolean;
begin
 try
  FESKLoginHndr.XmlDocument := Self.XmlDocument; // много е важно да превключим документа
  if not FESKLoginHndr.AddAnswerToDocument then raise EAbort.Create(FESKLoginHndr.LastError);

  Result := true;
 except
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'] AddAnswerToDocument Fail: '+E.Message;
    Result := false;
   end;
 end;
end;

function TCmd_ESKReadServer.FGetESKSerial: String;
begin
 Result := FESKLoginHndr.EskSerial;
end;

function TCmd_ESKReadServer.FGetEskVersion: String;
begin
 Result := FESKLoginHndr.EskVersion;
end;

//*****************************************************
//  TCmd_ESKChangePinClient
//*****************************************************
constructor TCmd_ESKChangePinClient.Create(XmlDoc: IXMLDocument);
begin
 inherited Create(XmlDoc);
 FESKLoginHndr := THndr_ESKLoginClient.Create(XmlDoc);
end;

destructor TCmd_ESKChangePinClient.Destroy;
begin
 FESKLoginHndr.Free;
 inherited Destroy;
end;

function TCmd_ESKChangePinClient.GetCommandName: String;
begin
 Result := cmd_ESKChgePin;
end;

function TCmd_ESKChangePinClient.AddRequestToDocument: Boolean;
var iNode : IXMLNode;
    SDate : String;
    SCRC  : String;
    SPin  : String;
begin
 try
  if not FESKLoginHndr.AddRequestToDocument then raise EAbort.Create(FESKLoginHndr.LastError);

  SDate := FormatDateTime('DD.MM.YY HH:NN:SS', Now);
  SPin  := FESKLoginHndr.Encrypt(ESKSerial + NewPin, ESKSerial + SDate);
  SCRC  := FESKLoginHndr.CalculateCRC(ESKSerial + SPin + FUserFullName + SDate);

  iNode := XML_GetRootNode;
  iNode := iNode.AppendChild(XmlDocument.CreateElement(xml_Node_ChangePin));
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_LoginUser)).Text    := ESKSerial;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_LoginPass)).Text    := SPin;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_UserFullName)).Text := FUserFullName;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_Time)).Text         := SDate;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_CRC)).Text          := SCRC;

  Result := true;
 except
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'] AddRequestToDocument Fail: '+E.Message;
    Result := false;
   end;
 end;
end;

function TCmd_ESKChangePinClient.GetAnswerFromDocument: Boolean;
var iNode  : IXMLNode;
    SDate  : String;
    SCRC   : String;
    SUser  : String;
begin
 try
  if not FESKLoginHndr.GetAnswerFromDocument then raise EAbort.Create(FESKLoginHndr.LastError);

  iNode    := XML_GetRootNode;
  iNode    := XML_FindNodeByName(iNode, xml_Node_ChangePin);
  SUser    := XML_GetNodeText(iNode, xml_Node_LoginUser);
  SDate    := XML_GetNodeText(iNode, xml_Node_Time);
  SCRC     := XML_GetNodeText(iNode, xml_Node_CRC);
  FUserFullName := XML_GetNodeText(iNode, xml_Node_UserFullName);

  if SCRC <> FESKLoginHndr.CalculateCRC(ESKSerial + FUserFullName + SDate) then raise EAbort.Create('Invalid signature');
  if SUser <> ESKSerial then raise EAbort.Create('Invalid UserName');

  Result := true;
 except
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'] GetAnswerFromDocument Fail: '+E.Message;
    Result := false;
   end;
 end;
end;

function TCmd_ESKChangePinClient.FGetESKSerial: String;
begin
 Result := FESKLoginHndr.EskSerial;
end;

//*****************************************************
//  TCmd_ESKChangePinServer
//*****************************************************
constructor TCmd_ESKChangePinServer.Create(XmlDoc: IXMLDocument; RemoteDevice: TRemoteDevice);
begin
 inherited Create(XmlDoc, RemoteDevice);
 FESKLoginHndr := THndr_ESKLoginServer.Create(XmlDoc, RemoteDevice);
end;

destructor TCmd_ESKChangePinServer.Destroy;
begin
 FESKLoginHndr.Free;
 inherited Destroy;
end;

function TCmd_ESKChangePinServer.GetRequestFromDocument: Boolean;
var iNode  : IXMLNode;
    SDate  : String;
    SCRC   : String;
    SUser  : String;
begin
 try
  if not FESKLoginHndr.GetRequestFromDocument then raise EAbort.Create(FESKLoginHndr.LastError);

  iNode     := XML_GetRootNode;
  iNode     := XML_FindNodeByName(iNode, xml_Node_ChangePin);
  SUser     := XML_GetNodeText(iNode, xml_Node_LoginUser);
  FNewPin   := XML_GetNodeText(iNode, xml_Node_LoginPass);
  FUserFullName := XML_GetNodeText(iNode, xml_Node_UserFullName);
  SDate     := XML_GetNodeText(iNode, xml_Node_Time);
  SCRC      := XML_GetNodeText(iNode, xml_Node_CRC);

  if SCRC <> FESKLoginHndr.CalculateCRC(SUser + FNewPin + FUserFullName + SDate) then raise EAbort.Create('Invalid signature');
  if SUser <> ESKSerial then raise EAbort.Create('Invalid username');

  FNewPin := FESKLoginHndr.Decrypt(FNewPin, SUser+SDate);
  if Copy(FNewPin, 1, Length(SUser)) <> SUser then raise EAbort.Create('Invalid password -> decrypt fail');
  Delete(FNewPin, 1, Length(SUser));

  Result := true;
 except
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'] GetRequestFromDocument Fail: '+E.Message;
    Result := false;
   end;
 end;
end;

function TCmd_ESKChangePinServer.Execute(var ErrCode: Integer; var UserError: String): Boolean;
var Ra  : Integer;
    SQL : String;
    SPin: String;
    procedure SetErrorCode(Code_: Integer; Message_: String);
    begin
     ErrCode   := Code_;
     UserError := Message_;
    end;
begin
 try
  if not FESKLoginHndr.Execute(ErrCode, UserError) then  raise EAbort.Create(FESKLoginHndr.LastError);

  if ErrCode = errcode_ExecuteSucceed then
   begin
    if FESKLoginHndr.PermissionList.Values[C_DlrRight_ChangePinOwn] <> 'YES' then
     raise EHandledException.Create(errcode_ChangePassFail_Denied,
                                    'Неуспешна промяна на ПИН!'+sLineBreak+
                                    'Нямате необходимите права'+sLineBreak+
                                    'за да смените ПИНа си.');
    if Length(NewPin) < 4 then
     raise EHandledException.Create(errcode_ChangePassFail_Invalid,
                                    'Неуспешна промяна на ПИН!'+sLineBreak+
                                    'Въведеният ПИН е невалиден!'+sLineBreak+
                                    'Въведете ПИН с дължина поне 4 символа.');

    with Device.DbInterface do
     begin
      SPin:= MD5Print(MD5String(NewPin));
      SQL := 'UPDATE DEALERS_USERS SET '+
             'DU_NAME = '+StrToSQL(FUserFullName)+', '+
             'DU_PASSWORD = '+StrToSQL(SPin)+' '+
             'WHERE DU_USERNAME = '+StrToSQL(ESKSerial);

      if not ExecuteSQLStatement(SQL, Ra) then raise EAbort.Create('Update pin fail: '+LastError);
      if Ra = 0 then raise EAbort.Create('No records affected after change pin SQL');

      SetErrorCode(errcode_ExecuteSucceed, '');
     end;
   end;

  Result := true;
 except
  on E: EHandledException do
   begin
    if E.SystemMessage <> '' then
     begin
      Device.PostEventSystem(C_EvType_Error, 'Error code:'+IntToStr(E.ErrorCode)+sLineBreak+
                                             E.Message+sLineBreak+
                                             E.SystemMessage, Self.ClassName+'/Execute');
     end;
    SetErrorCode(E.ErrorCode, E.Message);
    Result := true;
   end;
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'][Execute]'+E.Message;
    Result    := false;
   end;
 end;
end;

function TCmd_ESKChangePinServer.AddAnswerToDocument: Boolean;
var iNode : IXMLNode;
    SDate : String;
    SCRC  : String;
begin
 try
  FESKLoginHndr.XmlDocument := Self.XmlDocument; // много е важно да превключим документа
  if not FESKLoginHndr.AddAnswerToDocument then raise EAbort.Create(FESKLoginHndr.LastError);

  SDate := FormatDateTime('DD.MM.YY HH:NN:SS', Now);
  SCRC  := FESKLoginHndr.CalculateCRC(ESKSerial + FUserFullName + SDate);

  iNode := XML_GetRootNode;
  iNode := iNode.AppendChild(XmlDocument.CreateElement(xml_Node_ChangePin));
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_LoginUser)).Text    := ESKSerial;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_UserFullName)).Text := FUserFullName;
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

function TCmd_ESKChangePinServer.FGetESKSerial: String;
begin
 Result := FESKLoginHndr.EskSerial;
end;

function TCmd_ESKChangePinServer.FGetEskVersion: String;
begin
 Result := FESKLoginHndr.EskVersion;
end;

end.
