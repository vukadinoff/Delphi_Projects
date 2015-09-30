unit FilesHandler;

interface

uses SysUtils, Classes, BaseHandler, DeviceUnit, XMLHandler, ESKHandler, Controls, StdCtrls, ComCtrls, ExtCtrls, Contnrs, StrUtils,
     Forms, SyncObjs, Windows, CryptoHandler, ExtDataHandler, VersionUtilsUnit, GetFilesUnit;

type

//******************************************************************************
//   CheckUpdate
//******************************************************************************

 TFileInfo = class(TObject)
 private                                                           
  FFileName   : String;
  FFileMD5    : String;
  FFileVersion: String;
  FFileDate   : TDateTime;
  FFileSize   : Integer;
  FOffset     : Integer;
  FPartSize   : Integer;  
  FNeedUpdate : Boolean;
 public
  function GetFileMD5(FileName_: String): String;
  function GetFileVersion(Fname_: String): String;

  property FileName: String read FFileName write FFileName;
  property FileMD5: String read FFileMD5 write FFileMD5;
  property FileVersion: String read FFileVersion write FFileVersion;
  property FileDate: TDateTime read FFileDate write FFileDate;
  property FileSize: Integer read FFileSize write FFileSize;
  property Offset: Integer read FOffset write FOffset;
  property PartSize: Integer read FPartSize write FPartSize;  
  property NeedUpdate: Boolean read FNeedUpdate write FNeedUpdate;
 end;

 TCmd_CheckUpdateServer = class(THandlerServerUserEvent)
 private
  FEskSerial     : String;
  FProjectName   : String;
  FProjectData   : TStrings;
  FDataFromClient: THndr_ExtDataServer;
  FFilesList     : TObjectList;
  FCrcHandler    : TCRC32;
  FDeviceType    : String;
  FDeviceSerial  : String;
  function FGetFilesCount: Integer;
  function FGetFilesInfo(Index: Integer): TFileInfo;
  function CalculateCRC(Source: String): String;
 public
  constructor Create(XmlDoc: IXMLDocument; RemoteDevice: TRemoteDevice);
  destructor Destroy; override;

  function GetRequestFromDocument: Boolean; override;
  function Execute(var ErrCode: Integer; var UserError: String): Boolean; override;
  function AddAnswerToDocument: Boolean; override;

  property EskSerial: String read FEskSerial write FEskSerial;
  property ProjectName: String read FProjectName write FProjectName;
  property ProjectData: TStrings read FProjectData write FProjectData;
  property DataFromClient: THndr_ExtDataServer read FDataFromClient write FDataFromClient;
  property FilesCount: Integer read FGetFilesCount;
  property FilesInfo[Index: Integer]: TFileInfo read FGetFilesInfo;
  property DeviceType: String read FDeviceType;
  property DeviceSerial: String read FDeviceSerial;
 end;

 
 TCmd_CheckUpdateClient = class(TCommandClient)
 private
  FEskSerial     : String;
  FProjectName   : String;
  FProjectData   : TStrings;
  FDataToServer  : TStrings;
  FFilesList     : TObjectList;
  FCrcHandler    : TCRC32;
  function FGetFilesCount: Integer;
  function FGetFilesInfo(Index: Integer): TFileInfo;
  function CalculateCRC(Source: String): String;
 public
  constructor Create(XmlDoc: IXMLDocument); override;
  destructor Destroy; override;

  function GetCommandName: String; override;
  function AddRequestToDocument: Boolean; override;
  function GetAnswerFromDocument: Boolean; override;

  property EskSerial: String read FEskSerial write FEskSerial;
  property ProjectName: String read FProjectName write FProjectName;
  property ProjectData: TStrings read FProjectData write FProjectData;
  property DataToServer: TStrings read FDataToServer write FDataToServer;
  property FilesCount: Integer read FGetFilesCount;
  property FilesInfo[Index: Integer]: TFileInfo read FGetFilesInfo;
 end;

 //******************************************************************************
//    DownloadFile
//******************************************************************************
 TFileData = class(TFileInfo)
 private
  FFileStream : TMemoryStream;
 public
  constructor Create;
  destructor Destroy; override;

  property FileStream: TMemoryStream read FFileStream write FFileStream;
 end;

 TCmd_DownloadFileServer = class(THandlerServerUserEvent)
 private
  FESKLoginHndr  : THndr_ESKLoginServer;
  FFilesList     : TObjectList;
  FProjectName   : String;
  function FGetFilesCount: Integer;
  function FGetFilesData(Index: Integer): TFileData;
  function FGetFilesListAsText: String;
 public
  constructor Create(XmlDoc: IXMLDocument; RemoteDevice: TRemoteDevice);
  destructor Destroy; override;

  function GetRequestFromDocument: Boolean; override;
  function Execute(var ErrCode: Integer; var UserError: String): Boolean; override;
  function AddAnswerToDocument: Boolean; override;

  property ESKLoginData: THndr_ESKLoginServer read FESKLoginHndr write FESKLoginHndr;
  property ProjectName: String read FProjectName write FProjectName;
  property FilesCount: Integer read FGetFilesCount;
  property FilesData[Index: Integer]: TFileData read FGetFilesData;
  property FilesListAsText: String read FGetFilesListAsText;
 end;
 
 TCmd_DownloadFileClient = class(TCommandClient)
 private
  FESKLoginHndr  : THndr_ESKLoginClient;
  FFilesList     : TObjectList;
  FProjectName   : String;
  function FGetFilesCount: Integer;
  function FGetFilesData(Index: Integer): TFileData;
 public
  constructor Create(XmlDoc: IXMLDocument); override;
  destructor Destroy; override;

  function GetCommandName: String; override;
  function AddRequestToDocument: Boolean; override;
  function GetAnswerFromDocument: Boolean; override;

  //function AddFileForDownload(Fname: String): TFileData;
  procedure AddFileForDownload(Fname: String);

  property ESKLoginData: THndr_ESKLoginClient read FESKLoginHndr write FESKLoginHndr;
  property ProjectName: String read FProjectName write FProjectName;
  property FilesCount: Integer read FGetFilesCount;
  property FilesData[Index: Integer]: TFileData read FGetFilesData;
 end;


implementation
uses BillingConstUnit, DBInterfaceUnit, MD5, XMLHandlerMS, SZCodeBaseX, ZLib;


//*****************************************************
//  TFileInfo
//*****************************************************
function TFileInfo.GetFileMD5(FileName_: String): String;
begin
 Result := MD5Print(MD5File(FileName_));
end;

function TFileInfo.GetFileVersion(Fname_: String): String;
const
  InfoNum = 10;
  InfoStr: array[1..InfoNum] of string = ('CompanyName', 'FileDescription',
                   'FileVersion', 'InternalName', 'LegalCopyright', 'LegalTradeMarks',
                   'OriginalFileName', 'ProductName', 'ProductVersion', 'Comments');

var N, Len : Cardinal;
    Buf    : PChar;
    Value  : PChar;
    LangCharset : string;
    PCharset    : PLongInt;
    InfoLength  : UINT;
begin
 try
  Result := '';
  N := GetFileVersionInfoSize(PChar(Fname_), N);
  if N > 0 then
   begin
    Buf := AllocMem(N);
    try
     GetFileVersionInfo(PChar(Fname_), 0, N, Buf);
     if VerQueryValue(Buf, '\VarFileInfo\Translation', Pointer(PCharset), InfoLength) then
       LangCharset := Format('%.4x%.4x',[LoWord (PCharset^), HiWord (PCharset^)])
     else
       LangCharset := '040904E4';
     if VerQueryValue(Buf, PChar('StringFileInfo\'+LangCharset+'\' + InfoStr[3]), Pointer(Value), Len) then
       Result := StrPas(Value);
    finally
     FreeMem(Buf, N);
    end;
   end;
 except
 end;
end;


//*****************************************************
//  TFileData
//*****************************************************
constructor TFileData.Create;
begin
 inherited Create;
 FFileStream := TMemoryStream.Create;
end;

destructor TFileData.Destroy;
begin
 FFileStream.Clear;
 FFileStream.Free;
 inherited Destroy;
end;


//*****************************************************
//  TCmd_CheckUpdateServer
//*****************************************************
constructor TCmd_CheckUpdateServer.Create(XmlDoc: IXMLDocument; RemoteDevice: TRemoteDevice);
begin
 inherited Create(XmlDoc, RemoteDevice);
 FEskSerial      := '';
 FProjectName    := '';
 FFilesList      := TObjectList.Create(true);
 FProjectData    := TStringList.Create;
 FCrcHandler     := TCRC32.Create;
 FDeviceType     := '';
 FDeviceSerial   := '';
 FDataFromClient := nil;
end;

destructor TCmd_CheckUpdateServer.Destroy;
begin
 FFilesList.Free;
 FCrcHandler.Free;
 FProjectData.Free;
 if FDataFromClient <> nil then FDataFromClient.Free;
 inherited Destroy;
end;

function TCmd_CheckUpdateServer.GetRequestFromDocument: Boolean;
var iNode  : IXMLNode;
    iChild : IXMLNode;
    SDate  : String;
    SCRC   : String;
    Strm   : TStringStream;
    B      : Char;
    S      : String;
begin
 try
  if FDataFromClient <> nil then FreeAndNil(FDataFromClient);

  iNode        := XML_GetRootNode;
  iNode        := XML_FindNodeByName(iNode, xml_Node_CheckVersion);
  FProjectName := XML_GetNodeText(iNode, xml_Node_ProjectName);
  FEskSerial   := XML_GetNodeText(iNode, xml_Node_ESKSerial, false, false);    // iiyayaa na a ii-eunia aa?ney
  SDate        := XML_GetNodeText(iNode, xml_Node_Time);
  SCRC         := XML_GetNodeText(iNode, xml_Node_CRC);

  FProjectData.Clear;
  iChild := XML_FindNodeByName(iNode, xml_Node_ProjectData, false);
  if iChild <> nil then
   begin
    iChild := iChild.firstChild;
    while iChild <> nil do
     begin
      ProjectData.Values[iChild.nodeName] := iChild.text;
      iChild := iChild.nextSibling;
     end;
   end;

  if SCRC <> CalculateCRC(ProjectName + SDate) then raise EAbort.Create('Invalid signature');

  iChild := XML_FindNodeByName(iNode, xml_Node_ExtendedData, false);
  if iChild <> nil then
   begin
    Strm := TStringStream.Create('');
    try
     Strm.WriteString(SZDecodeBase64(iChild.text));
     Strm.Position := 0;

     S := '';
     with TDecompressionStream.Create(Strm) do
     try
      while Read(B, 1) = 1 do S := S + B;
     finally
      Free;
     end;
    finally
     Strm.Free;
    end;

    if S <> '' then FDataFromClient := THndr_ExtDataServer.Create(Device, FEskSerial, S);
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

function TCmd_CheckUpdateServer.Execute(var ErrCode: Integer; var UserError: String): Boolean;
var PPath     : String;
    AppendPath: String;
    RA        : Integer;
    SR        : TSearchRec;
    FData     : TFileInfo;
    CSec      : TCriticalSection;
    SQL       : String;
    FilesList : TStringList;
    i         : Integer;
    myFile    : File;

    procedure SetErrorCode(Code_: Integer; Message_: String);
    begin
     ErrCode   := Code_;
     UserError := Message_;
    end;
begin
 Result    := true;
 UserError := '';
 ErrCode   := errcode_ExecuteSucceed;
 try
//  if not FESKLoginHndr.Execute(ErrCode, UserError) then  raise EAbort.Create(FESKLoginHndr.LastError);

//  if ErrCode = errcode_ExecuteSucceed then
//   begin

   FFilesList.Clear;

   if ProjectName = '' then
    begin
     raise EHandledException.Create(errcode_CheckUpd_NoProject, 'Project name is not specified')
    end
   else
    // UPDATE PC
    begin
     // Save statistics to database
     if (ProjectData.Values[C_Project_SwVer] <> '')and(ProjectData.Values[C_Project_Host] <> '')and
        (ProjectData.Values[C_Project_Path] <> '') then
      begin
       FDeviceType   := C_DeviceType_PC;
       FDeviceSerial := ProjectData.Values[C_Project_Host];
      end;

     // update data sent from client
     {
     if FDataFromClient <> nil then
      begin
       if not FDataFromClient.Execute(SQL) then
        Device.PostEventSystem(C_EvType_Error, 'Error processing client data:'+sLineBreak+SQL, Self.ClassName);
      end;
      }
     // Search for update files
     CSec := TCriticalSection.Create;
     try
      PPath := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName));
      PPath := IncludeTrailingPathDelimiter(PPath + 'Updates');
      PPath := IncludeTrailingPathDelimiter(PPath + ProjectName);
      if not DirectoryExists(PPath) then
       begin
        Device.PostEventSystem(C_EvType_Error, 'Check update fail.'+sLineBreak+
                                               'Unknown project: '+ProjectName+sLineBreak+
                                               'Dir dont exist: '+PPath+sLineBreak+
                                               ''+sLineBreak+
                                               ProjectData.Text, Self.ClassName);
        raise EHandledException.Create(errcode_CheckUpd_ProjectNotExist, 'Unknown project: '+ProjectName);
       end;
              
       FilesList := TStringList.Create;
       FindFiles(FilesList, PPath, '*.*');

       for i := 0 to FilesList.Count-1 do
       begin
        CSec.Acquire;
        try
          if (FileExists(FilesList[i])) then
          begin
            FData := TFileInfo.Create;
            FData.FileName    := RightStr(FilesList[i], Length(FilesList[i]) - Length(PPath));
            FData.FileMD5     := FData.GetFileMD5(FilesList[i]);
            FData.FileVersion := FData.GetFileVersion(FilesList[i]);
            FData.FileDate    := FileDateToDateTime(FileAge(FilesList[i]));
            FData.FileSize    := ObtainFileSize(FilesList[i]);
            FFilesList.Add(FData);
          end;
        finally
         CSec.Leave;
        end;
       end;

     finally
      CSec.Free;
     end;
    end;

 except
  on E: EHandledException do
   begin
    if E.SystemMessage <> '' then
     begin
      Device.PostEventSystem(C_EvType_Error, 'Error code:'+IntToStr(E.ErrorCode)+sLineBreak+
                                             E.Message+sLineBreak+
                                             E.SystemMessage, Self.ClassName);
     end;
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

function TCmd_CheckUpdateServer.AddAnswerToDocument: Boolean;
var iNode : IXMLNode;
    SDate : String;
    SCRC  : String;
    I     : Integer;
begin
 try
//  FESKLoginHndr.XmlDocument := Self.XmlDocument; // iiiai a aa?ii aa i?aaee??ei aieoiaioa
//  if not FESKLoginHndr.AddAnswerToDocument then raise EAbort.Create(FESKLoginHndr.LastError);

  SDate := FormatDateTime('DD.MM.YY HH:NN:SS', Now);
  SCRC  := CalculateCRC(ProjectName + SDate);

  iNode := XML_GetRootNode;
  iNode := iNode.AppendChild(XmlDocument.CreateElement(xml_Node_CheckVersion));
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_ProjectName)).Text := FProjectName;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_ESKSerial)).Text   := FEskSerial;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_Time)).Text := SDate;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_CRC)).Text  := SCRC;

  iNode := iNode.AppendChild(XmlDocument.CreateElement(xml_Node_FilesData));
  for I := 0 to FFilesList.Count - 1 do
   begin
    with iNode.AppendChild(XmlDocument.CreateElement(xml_Node_FileData)) do
     begin
      appendChild(XmlDocument.CreateElement(xml_Node_FileName)).text    := FilesInfo[I].FileName;
      appendChild(XmlDocument.CreateElement(xml_Node_FileMD5)).text     := FilesInfo[I].FileMD5;
      appendChild(XmlDocument.CreateElement(xml_Node_FileVersion)).text := FilesInfo[I].FileVersion;
      appendChild(XmlDocument.CreateElement(xml_Node_FileDate)).text    := FloatToStr(FilesInfo[I].FileDate);
      appendChild(XmlDocument.CreateElement(xml_Node_FileSize)).text    := IntToStr(FilesInfo[I].FileSize);
      appendChild(XmlDocument.CreateElement(xml_Node_Offset)).text      := IntToStr(FilesInfo[I].Offset);
      appendChild(XmlDocument.CreateElement(xml_Node_PartSize)).text    := IntToStr(FilesInfo[I].PartSize);
     end;
   end;

  if FProjectData.Count > 0 then
   begin
    with iNode.AppendChild(XmlDocument.CreateElement(xml_Node_ProjectData)) do
     begin
      for I := 0 to FProjectData.Count - 1 do
       begin
        if FProjectData.Names[I] <> '' then
          AppendChild(XmlDocument.CreateElement(FProjectData.Names[I])).Text := FProjectData.ValueFromIndex[I];
       end;
     end;
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

function TCmd_CheckUpdateServer.FGetFilesCount: Integer;
begin
 Result := FFilesList.Count;
end;

function TCmd_CheckUpdateServer.FGetFilesInfo(Index: Integer): TFileInfo;
begin
 Result := TFileInfo(FFilesList.Items[Index]);
end;

function TCmd_CheckUpdateServer.CalculateCRC(Source: String): String;
begin
 FCrcHandler.Reset;
 FCrcHandler.Process(Source[1], Length(Source));
 Result := FCrcHandler.AsString;
end;


//*****************************************************
//  TCmd_CheckUpdateClient
//*****************************************************
constructor TCmd_CheckUpdateClient.Create(XmlDoc: IXMLDocument);
begin
 inherited Create(XmlDoc);
// FESKLoginHndr := THndr_ESKLoginClient.Create(XmlDoc);
 FEskSerial    := '';
 FProjectName  := '';
 FFilesList    := TObjectList.Create(true);
 FProjectData  := TStringList.Create;
 FDataToServer := nil;
 FCrcHandler   := TCRC32.Create;
end;

destructor TCmd_CheckUpdateClient.Destroy;
begin
// FESKLoginHndr.Free;
 FCrcHandler.Free;
 FFilesList.Free;
 FProjectData.Free;
 inherited Destroy;
end;

function TCmd_CheckUpdateClient.GetCommandName: String;
begin
 Result := cmd_SysCheckUpdate;
end;

function TCmd_CheckUpdateClient.AddRequestToDocument: Boolean;
var iNode : IXMLNode;
    iElem : IXMLElement;
    SDate : String;
    SCRC  : String;
    I     : Integer;
    Strm  : TStringStream;
    CStrm : TCompressionStream;
begin
 try
//  if not FESKLoginHndr.AddRequestToDocument then raise EAbort.Create(FESKLoginHndr.LastError);

  SDate := FormatDateTime('DD.MM.YY HH:NN:SS', Now);
  SCRC  := CalculateCRC(ProjectName + SDate);

  iNode := XML_GetRootNode;
  iNode := iNode.AppendChild(XmlDocument.CreateElement(xml_Node_CheckVersion));

  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_ESKSerial)).Text   := FEskSerial;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_ProjectName)).Text := FProjectName;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_Time)).Text        := SDate;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_CRC)).Text         := SCRC;

  if FProjectData.Count > 0 then
   begin
    with iNode.AppendChild(XmlDocument.CreateElement(xml_Node_ProjectData)) do
     begin
      for I := 0 to FProjectData.Count - 1 do
       begin
        if FProjectData.Names[I] <> '' then
          AppendChild(XmlDocument.CreateElement(FProjectData.Names[I])).Text := FProjectData.ValueFromIndex[I];
       end;
     end;
   end;

  if (FDataToServer <> nil)and(FDataToServer.Count > 0) then
   begin
    Strm := TStringStream.Create('');
    try
     CStrm := TCompressionStream.Create(clFastest, Strm);
     try
      FDataToServer.SaveToStream(CStrm);
     finally
      CStrm.Free;
     end;

     iElem := XmlDocument.CreateElement(xml_Node_ExtendedData);
     iElem.Set_dataType('bin.base64');
     iElem.nodeTypedValue := SZFullEncodeBase64(Strm.DataString);
     iNode.appendChild(iElem);
    finally
     Strm.Free;
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

function TCmd_CheckUpdateClient.GetAnswerFromDocument: Boolean;
var iNode  : IXMLNode;
    FileInf: TFileInfo;
    SDate  : String;
    SCRC   : String;
begin
 try
//  if not FESKLoginHndr.GetAnswerFromDocument then raise EAbort.Create(FESKLoginHndr.LastError);

  iNode        := XML_GetRootNode;
  iNode        := XML_FindNodeByName(iNode, xml_Node_CheckVersion);
  FProjectName := XML_GetNodeText(iNode, xml_Node_ProjectName);
  FEskSerial   := XML_GetNodeText(iNode, xml_Node_ESKSerial, false, false);    // iiyayaa na a ii-unia aa?ney
  SDate        := XML_GetNodeText(iNode, xml_Node_Time);
  SCRC         := XML_GetNodeText(iNode, xml_Node_CRC);

  if SCRC <> CalculateCRC(ProjectName + SDate) then raise EAbort.Create('Invalid signature');

  FFilesList.Clear;
  iNode := XML_FindNodeByName(iNode, xml_Node_FilesData);
  iNode := iNode.firstChild;
  while iNode <> nil do
   begin
    if iNode.nodeName = xml_Node_FileData then
     begin
      FileInf := TFileInfo.Create;
      FFilesList.Add(FileInf);

      FileInf.FileName    := XML_GetNodeText(iNode, xml_Node_FileName);
      FileInf.FileMD5     := XML_GetNodeText(iNode, xml_Node_FileMD5);
      FileInf.FileVersion := XML_GetNodeText(iNode, xml_Node_FileVersion, true, false);
      FileInf.FileDate    := StrToFloatDef(XML_GetNodeText(iNode, xml_Node_FileDate), 0);
      FileInf.FileSize    := StrToIntDef(XML_GetNodeText(iNode, xml_Node_FileSize), 0);
      FileInf.Offset      := StrToIntDef(XML_GetNodeText(iNode, xml_Node_Offset), 0);
      FileInf.PartSize    := StrToIntDef(XML_GetNodeText(iNode, xml_Node_PartSize), 0);

     end;
    iNode := iNode.nextSibling;
   end;

  FProjectData.Clear;
  iNode := XML_FindNodeByName(iNode, xml_Node_ProjectData, false);
  if iNode <> nil then
   begin
    iNode := iNode.firstChild;
    while iNode <> nil do
     begin
      ProjectData.Values[iNode.nodeName] := iNode.text;
      iNode := iNode.nextSibling;
     end;
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

function TCmd_CheckUpdateClient.FGetFilesCount: Integer;
begin
 Result := FFilesList.Count;
end;

function TCmd_CheckUpdateClient.FGetFilesInfo(Index: Integer): TFileInfo;
begin
 Result := TFileInfo(FFilesList.Items[Index]);
end;

function TCmd_CheckUpdateClient.CalculateCRC(Source: String): String;
begin
 FCrcHandler.Reset;
 FCrcHandler.Process(Source[1], Length(Source));
 Result := FCrcHandler.AsString;
end;


//*****************************************************
//  TCmd_DownloadFileServer
//*****************************************************
constructor TCmd_DownloadFileServer.Create(XmlDoc: IXMLDocument; RemoteDevice: TRemoteDevice);
begin
 inherited Create(XmlDoc, RemoteDevice);
 FESKLoginHndr := THndr_ESKLoginServer.Create(XmlDoc, RemoteDevice);
 FFilesList    := TObjectList.Create(true);
end;

destructor TCmd_DownloadFileServer.Destroy;
begin
 FESKLoginHndr.Free;
 FFilesList.Free;
 inherited Destroy;
end;

function TCmd_DownloadFileServer.GetRequestFromDocument: Boolean;
var iNode  : IXMLNode;
    SDate  : String;
    SCRC   : String;
    FileDat: TFileData;
begin
 try
  if not FESKLoginHndr.GetRequestFromDocument then raise EAbort.Create(FESKLoginHndr.LastError);

  iNode       := XML_GetRootNode;
  iNode       := XML_FindNodeByName(iNode, xml_Node_DownloadFile);
  ProjectName := XML_GetNodeText(iNode, xml_Node_ProjectName);
  SDate       := XML_GetNodeText(iNode, xml_Node_Time);
  SCRC        := XML_GetNodeText(iNode, xml_Node_CRC);

  if SCRC <> FESKLoginHndr.CalculateCRC(ProjectName + SDate) then raise EAbort.Create('Invalid signature');

  FFilesList.Clear;
  iNode := XML_FindNodeByName(iNode, xml_Node_FilesData);
  iNode := iNode.firstChild;
  while iNode <> nil do
   begin
    if iNode.nodeName = xml_Node_FileData then
     begin
      FileDat := TFileData.Create;
      FFilesList.Add(FileDat);

      FileDat.FileName := iNode.text;
     end;
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

function TCmd_DownloadFileServer.Execute(var ErrCode: Integer; var UserError: String): Boolean;
var PPath : String;
    I     : Integer;
    CSec  : TCriticalSection;
    InFile: TFileStream;
    Fname: String;
    FOffset: Integer;
    FSize: Integer;
    LastMD5: String;
    LastVer: String;
    LastFname: String;
    LastDate: TDateTime;

    procedure SetErrorCode(Code_: Integer; Message_: String);
    begin
     ErrCode   := Code_;
     UserError := Message_;
    end;
begin
 CSec := TCriticalSection.Create;
 try
  if not FESKLoginHndr.Execute(ErrCode, UserError) then  raise EAbort.Create(FESKLoginHndr.LastError);

  if ErrCode = errcode_ExecuteSucceed then
   begin
    if ProjectName = '' then
     raise EHandledException.Create(errcode_CheckUpd_NoProject, 'Project name is not specified')
    else
    if ProjectName = C_Project_ModemFW then // uiaaeo nioooa?a ia iiaaia
     begin
      PPath := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName));
      PPath := IncludeTrailingPathDelimiter(PPath + 'Downloads');
     end
    else
     begin
      PPath := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName));
      PPath := IncludeTrailingPathDelimiter(PPath + 'Updates');
      PPath := IncludeTrailingPathDelimiter(PPath + ProjectName);
     end;

    if not DirectoryExists(PPath) then
     begin
      Device.PostEventSystem(C_EvType_Error, 'Download update fail.'+sLineBreak+
                                             'Project: '+ProjectName+sLineBreak+
                                             'Path not exist: '+PPath, Self.ClassName);
      raise EHandledException.Create(errcode_DownlUpd_ProjectNotExist, 'Unknown project: '+ProjectName);
     end;

    for I := 0 to FFilesList.Count - 1 do
     begin
      Fname := FilesData[I].FileName;
      FOffset := FilesData[I].Offset;
      FSize := FilesData[I].PartSize;

      if not FileExists(PPath + Fname) then
       begin
        Device.PostEventSystem(C_EvType_Error, 'Download update fail.'+sLineBreak+
                                               'Project: '+ProjectName+sLineBreak+
                                               'File not exist: '+Fname, Self.ClassName);
        raise EHandledException.Create(errcode_DownlUpd_FileNotFound, 'File not found. Project:'+ProjectName+' File:'+Fname);
       end;

      InFile := nil;
      CSec.Acquire;
      try
        try
         InFile := TFileStream.Create(PPath + Fname, fmOpenRead+fmShareDenyWrite);
         InFile.Position := FOffset;
        except
         on E: Exception do
          begin
           Device.PostEventSystem(C_EvType_Error, 'Download update fail.'+sLineBreak+
                                                  'Unable to open file: '+Fname+sLineBreak+
                                                  E.Message, Self.ClassName);
           raise EHandledException.Create(errcode_DownlUpd_ErrorOpenFile, 'Unable to open file: '+Fname);
          end;
        end;

        with TCompressionStream.Create(clFastest, FilesData[I].FileStream) do
        try
         CopyFrom(InFile, FSize);
        finally
         Free;
        end;

        if Fname <> LastFname then
        begin
          LastFname := Fname;
          LastMD5 := FilesData[I].GetFileMD5(PPath + Fname);
          LastVer := FilesData[I].GetFileVersion(PPath + Fname);
          LastDate := FileDateToDateTime(FileAge(PPath + Fname));
        end;

        FilesData[I].FileMD5     := LastMD5;
        FilesData[I].FileVersion := LastVer;
        FilesData[I].FileDate    := LastDate;
        FilesData[I].FileSize    := InFile.Size;
        FilesData[I].Offset      := FOffset;
        FilesData[I].PartSize    := FSize;

      finally
       if InFile <> nil then InFile.Free;
       CSec.Leave;
      end;
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
                                             E.SystemMessage, Self.ClassName);
     end;   
    SetErrorCode(E.ErrorCode, E.Message);
    Result := true;
   end;
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'] Execute Fail: '+E.Message;
    Result    := false;
   end;
 end;
 CSec.Free;
end;

function TCmd_DownloadFileServer.AddAnswerToDocument: Boolean;
var iNode  : IXMLNode;
    iElem  : IXMLElement;
    SDate  : String;
    SCRC   : String;
    I      : Integer;
    TmpStrm: TStringStream;
begin
 try
  FESKLoginHndr.XmlDocument := Self.XmlDocument; // iiiai a aa?ii aa i?aaee??ei aieoiaioa
  if not FESKLoginHndr.AddAnswerToDocument then raise EAbort.Create(FESKLoginHndr.LastError);


  SDate := FormatDateTime('DD.MM.YY HH:NN:SS', Now);
  SCRC  := FESKLoginHndr.CalculateCRC(ProjectName + SDate);

  iNode := XML_GetRootNode;
  iNode := iNode.AppendChild(XmlDocument.CreateElement(xml_Node_DownloadFile));
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_ProjectName)).Text := ProjectName;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_Time)).Text        := SDate;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_CRC)).Text         := SCRC;

  iNode := iNode.AppendChild(XmlDocument.CreateElement(xml_Node_FilesData));
  for I := 0 to FFilesList.Count - 1 do
   begin
    TmpStrm := TStringStream.Create('');
    try
      FilesData[I].FFileStream.Position := 0;
      SZFullEncodeBase64(FilesData[I].FFileStream, TmpStrm, FilesData[I].FFileStream.Size);
      with iNode.appendChild(XmlDocument.CreateElement(xml_Node_FileData)) as IXMLElement do
       begin
        appendChild(XmlDocument.CreateElement(xml_Node_FileName)).text    := FilesData[I].FileName;
        appendChild(XmlDocument.CreateElement(xml_Node_FileMD5)).text     := FilesData[I].FileMD5;
        appendChild(XmlDocument.CreateElement(xml_Node_FileVersion)).text := FilesData[I].FileVersion;
        appendChild(XmlDocument.CreateElement(xml_Node_FileDate)).text    := FloatToStr(FilesData[I].FileDate);
        appendChild(XmlDocument.CreateElement(xml_Node_FileSize)).text    := IntToStr(FilesData[I].FileSize);
        appendChild(XmlDocument.CreateElement(xml_Node_Offset)).text      := IntToStr(FilesData[I].Offset);
        appendChild(XmlDocument.CreateElement(xml_Node_PartSize)).text    := IntToStr(FilesData[I].PartSize);

        iElem := XmlDocument.CreateElement(xml_Node_FileContent);
        iElem.Set_dataType('bin.base64');
        iElem.nodeTypedValue := TmpStrm.DataString;
        appendChild(iElem);
       end;
    finally
     TmpStrm.Free;
    end;
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

function TCmd_DownloadFileServer.FGetFilesCount: Integer;
begin
 Result := FFilesList.Count;
end;

function TCmd_DownloadFileServer.FGetFilesData(Index: Integer): TFileData;
begin
 Result := TFileData(FFilesList.Items[Index]);
end;

function TCmd_DownloadFileServer.FGetFilesListAsText: String;
var I : Integer;
begin
 Result := '';
 for I := 0 to FilesCount - 1 do Result := Result + FilesData[I].FileName+';'
end;


//*****************************************************
//  TCmd_DownloadFileClient
//*****************************************************
constructor TCmd_DownloadFileClient.Create(XmlDoc: IXMLDocument);
begin
 inherited Create(XmlDoc);
 FESKLoginHndr := THndr_ESKLoginClient.Create(XmlDoc);
 FFilesList    := TObjectList.Create(true);
end;

destructor TCmd_DownloadFileClient.Destroy;
begin
 FESKLoginHndr.Free;
 FFilesList.Free;
 inherited Destroy;
end;

function TCmd_DownloadFileClient.GetCommandName: String;
begin
 Result := cmd_SysDownloadFile;
end;

function TCmd_DownloadFileClient.AddRequestToDocument: Boolean;
var iNode : IXMLNode;
    SDate : String;
    SCRC  : String;
    I     : Integer;
begin
 try
  if not FESKLoginHndr.AddRequestToDocument then raise EAbort.Create(FESKLoginHndr.LastError);

  SDate := FormatDateTime('DD.MM.YY HH:NN:SS', Now);
  SCRC  := FESKLoginHndr.CalculateCRC(ProjectName + SDate);

  iNode := XML_GetRootNode;
  iNode := iNode.AppendChild(XmlDocument.CreateElement(xml_Node_DownloadFile));
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_ProjectName)).Text := ProjectName;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_Time)).Text        := SDate;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_CRC)).Text         := SCRC;

  iNode := iNode.AppendChild(XmlDocument.CreateElement(xml_Node_FilesData));
  for I := 0 to FFilesList.Count - 1 do
   begin
    iNode.appendChild(XmlDocument.CreateElement(xml_Node_FileData)).text := FilesData[I].FileName;
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

function TCmd_DownloadFileClient.GetAnswerFromDocument: Boolean;
var iNode  : IXMLNode;
    SDate  : String;
    SCRC   : String;
    FileDat: TFileData;
    StrStrm: TStringStream;
    Buffer : Pointer;
    TmpRead: Integer;
begin
 try
  if not FESKLoginHndr.GetAnswerFromDocument then raise EAbort.Create(FESKLoginHndr.LastError);

  iNode       := XML_GetRootNode;
  iNode       := XML_FindNodeByName(iNode, xml_Node_DownloadFile);
  ProjectName := XML_GetNodeText(iNode, xml_Node_ProjectName);
  SDate       := XML_GetNodeText(iNode, xml_Node_Time);
  SCRC        := XML_GetNodeText(iNode, xml_Node_CRC);

  if SCRC <> FESKLoginHndr.CalculateCRC(ProjectName + SDate) then raise EAbort.Create('Invalid signature');

  FFilesList.Clear;
  iNode := XML_FindNodeByName(iNode, xml_Node_FilesData);
  iNode := iNode.firstChild;
  while iNode <> nil do
   begin
    if iNode.nodeName = xml_Node_FileData then
     begin
      FileDat := TFileData.Create;
      FFilesList.Add(FileDat);

      FileDat.FileName    := XML_GetNodeText(iNode, xml_Node_FileName);
      FileDat.FileMD5     := XML_GetNodeText(iNode, xml_Node_FileMD5);
      FileDat.FileVersion := XML_GetNodeText(iNode, xml_Node_FileVersion, true, false);
      FileDat.FileDate    := StrToFloatDef(XML_GetNodeText(iNode, xml_Node_FileDate), 0);
      FileDat.FileSize    := StrToIntDef(XML_GetNodeText(iNode, xml_Node_FileSize), 0);
      FileDat.Offset      := StrToIntDef(XML_GetNodeText(iNode, xml_Node_Offset), 0);
      FileDat.PartSize    := StrToIntDef(XML_GetNodeText(iNode, xml_Node_PartSize), 0);

      StrStrm := TStringStream.Create('');
      try
       StrStrm.WriteString(XML_GetNodeText(iNode, xml_Node_FileContent));
       StrStrm.Position := 0;
       SZDecodeBase64(StrStrm, FileDat.FileStream);
       FileDat.FileStream.Position := 0;
      finally
       StrStrm.Free;
      end;

      GetMem(Buffer, FileDat.PartSize + 1);
      try
       with TDecompressionStream.Create(FileDat.FileStream) do
       try
        TmpRead := Read(Buffer^, FileDat.PartSize + 1);

        if ((TmpRead <> FileDat.PartSize) and (TmpRead <> FileDat.PartSize + 1)) then raise EAbort.Create('Decompression error. Invalid file partsize');

       finally
        Free;
       end;
       FileDat.FileStream.Clear;
       FileDat.FileStream.WriteBuffer(Buffer^, FileDat.PartSize);
      finally
       FreeMem(Buffer);
      end;
     end;
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

function TCmd_DownloadFileClient.FGetFilesCount: Integer;
begin
 Result := FFilesList.Count;
end;

function TCmd_DownloadFileClient.FGetFilesData(Index: Integer): TFileData;
begin
 Result := TFileData(FFilesList.Items[Index]);
end;

procedure TCmd_DownloadFileClient.AddFileForDownload(Fname: String);
var FD : TFileData;
begin
 FD := TFileData.Create;
 FD.FileName := Fname;
 FD.FFileStream.Clear;
 
 FFilesList.Clear();
 FFilesList.Add(FD);
end;

end.
