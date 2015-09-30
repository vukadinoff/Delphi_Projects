unit ExportFilesHandler;

interface

uses SysUtils, Classes, BaseHandler, CryptoHandler, DeviceUnit, XMLHandler,
     LoginHandler, Forms;

type
 TCmd_ExportTblFilesClient = class(TCommandClient)
 private
  FLoginHandler  : THndr_LoginClient;
  FTaskNameList  : TStrings;
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
  property TaskNameList: TStrings read FTaskNameList write FTaskNameList;
 end;

 TCmd_ExportTblFilesServer = class(THandlerServerUserEvent)
 private
  FLoginHandler  : THndr_LoginServer;
  FTaskNameList  : TStrings;
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
  property TaskNameList: TStrings read FTaskNameList write FTaskNameList;
 end;

implementation
uses BillingConstUnit, XMLHandlerMS, DBInterfaceUnit, IniFiles, ADODB, DB, md5,
     DbClient, Provider, ZLib, ZLIBArchive;

//******************************************************************************
//   TCmd_ExportTblFilesClient
//******************************************************************************
constructor TCmd_ExportTblFilesClient.Create(XmlDoc: IXMLDocument);
begin
 inherited Create(XmlDoc);
 FLoginHandler := THndr_LoginClient.Create(XmlDoc);
 FTaskNameList := TStringList.Create;
end;

destructor TCmd_ExportTblFilesClient.Destroy;
begin
 FLoginHandler.Free;
 FTaskNameList.Free;
 inherited Destroy;
end;

function TCmd_ExportTblFilesClient.GetCommandName: String;
begin
 Result := cmd_SysExpTblFiles;
end;

function TCmd_ExportTblFilesClient.FGetUserName: String;
begin
 Result := FLoginHandler.UserName;
end;

function TCmd_ExportTblFilesClient.FGetPassword: String;
begin
 Result := FLoginHandler.Password;
end;

procedure TCmd_ExportTblFilesClient.FSetUserName(Value: String);
begin
 FLoginHandler.UserName := Value;
end;

procedure TCmd_ExportTblFilesClient.FSetPassword(Value: String);
begin
 FLoginHandler.Password := Value;
end;

function TCmd_ExportTblFilesClient.AddRequestToDocument: Boolean;
var iNode : IXMLNode;
    SDate : String;
    SCRC  : String;
    I     : Integer;
begin
 try
  if not FLoginHandler.AddRequestToDocument then raise EAbort.Create(FLoginHandler.LastError);

  SDate := FormatDateTime('DD.MM.YY HH:NN:SS', Now);
  SCRC  := FLoginHandler.CalculateCRC(FTaskNameList.Text + SDate);

  iNode := XML_GetRootNode;
  iNode := iNode.AppendChild(XmlDocument.CreateElement(xml_Node_ExportData));
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_Time)).Text := SDate;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_CRC)).Text  := SCRC;

  with iNode.AppendChild(XmlDocument.CreateElement(xml_Node_ExportTasks)) do
   begin
    for I := 0 to FTaskNameList.Count - 1 do
     AppendChild(XmlDocument.CreateElement('Task'+Inttostr(I))).Text  := FTaskNameList.Strings[I];
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

function TCmd_ExportTblFilesClient.GetAnswerFromDocument: Boolean;
var iNode  : IXMLNode;
    SDate  : String;
    SCRC   : String;
begin
 try
  if not FLoginHandler.GetAnswerFromDocument then raise EAbort.Create(FLoginHandler.LastError);

  iNode    := XML_GetRootNode;
  iNode    := XML_FindNodeByName(iNode, xml_Node_ExportData);
  SDate    := XML_GetNodeText(iNode, xml_Node_Time);
  SCRC     := XML_GetNodeText(iNode, xml_Node_CRC);

  FTaskNameList.Clear;
  iNode := XML_FindNodeByName(iNode, xml_Node_ExportTasks);
  iNode := iNode.firstChild;
  while iNode <> nil do
   begin
    FTaskNameList.Add(iNode.text);
    iNode := iNode.nextSibling;
   end;

  if SCRC <> FLoginHandler.CalculateCRC(FTaskNameList.Text + SDate) then raise EAbort.Create('Invalid signature');

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
//   TCmd_ExportTblFilesServer
//******************************************************************************
constructor TCmd_ExportTblFilesServer.Create(XmlDoc: IXMLDocument; RemoteDevice: TRemoteDevice);
begin
 inherited Create(XmlDoc, RemoteDevice);
 FLoginHandler := THndr_LoginServer.Create(XmlDoc, RemoteDevice);
 FTaskNameList := TStringList.Create;
end;

destructor TCmd_ExportTblFilesServer.Destroy;
begin
 FLoginHandler.Free;
 FTaskNameList.Free;
 inherited Destroy;
end;

function TCmd_ExportTblFilesServer.GetRequestFromDocument: Boolean;
var iNode  : IXMLNode;
    SDate  : String;
    SCRC   : String;
begin
 try
  if not FLoginHandler.GetRequestFromDocument then raise EAbort.Create(FLoginHandler.LastError);

  iNode     := XML_GetRootNode;
  iNode     := XML_FindNodeByName(iNode, xml_Node_ExportData);
  SDate     := XML_GetNodeText(iNode, xml_Node_Time);
  SCRC      := XML_GetNodeText(iNode, xml_Node_CRC);

  FTaskNameList.Clear;
  iNode := XML_FindNodeByName(iNode, xml_Node_ExportTasks);
  iNode := iNode.firstChild;
  while iNode <> nil do
   begin
    FTaskNameList.Add(iNode.text);
    iNode := iNode.nextSibling;
   end;

  if SCRC <> FLoginHandler.CalculateCRC(FTaskNameList.Text + SDate) then raise EAbort.Create('Invalid signature');

  Result := true;
 except
  on E: Exception do
   begin
    LastError := '['+Self.ClassName+'] GetRequestFromDocument Fail: '+E.Message;
    Result := false;
   end;
 end;
end;

function TCmd_ExportTblFilesServer.Execute(var ErrCode: Integer; var UserError: String): Boolean;
var I, J     : Integer;
    IniFile  : TIniFile;
    LocPath  : String;
    TaskSect : String;
    TaskFile : String;
    TaskTFile: String;
    TableSect: String;
    TableFile: String;

    SQL     : String;
    RecCnt  : Integer;
    Cnt     : Integer;
    DsClient: TClientDataSet;
    DsProvdr: TDataSetProvider;
    TmpStrm : TMemoryStream;
//    TmpStrm2: TMemoryStream;

    procedure SetErrorCode(Code_: Integer; Message_: String);
    begin
     ErrCode   := Code_;
     UserError := Message_;
    end;
    procedure CreateTextFile(Fname_, FData_: String);
    var F_ : TextFile;
    begin
     AssignFile(F_, Fname_);
     try
      Rewrite(F_);
      Writeln(F_, FData_);
     finally
      CloseFile(F_);
     end;
    end;
begin
 Result := true;
 IniFile:= nil;
 try
  if not FLoginHandler.Execute(ErrCode, UserError) then  raise EAbort.Create(FLoginHandler.LastError);

  if ErrCode = errcode_ExecuteSucceed then
   begin
    LocPath  := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName));
    if not FileExists(LocPath + 'ElBngHttpExportTasks.ini') then raise EAbort.Create('Configuration file does not exist: ElBngHttpExportTasks.ini');
    IniFile := TIniFile.Create(LocPath + 'ElBngHttpExportTasks.ini');

    for I := 0 to FTaskNameList.Count - 1 do
     begin
      RecCnt   := 0;
      TaskSect := IniFile.ReadString('EXPORT TASK NAMES', FTaskNameList.Strings[I], '');
      if TaskSect <> '' then
       begin
        TaskFile := LocPath + IniFile.ReadString(TaskSect, 'FileName', '');
        if not DirectoryExists(ExtractFilePath(TaskFile)) then raise EHandledException.Create(errcode_ExportFail_ConfigError, 'Invalid path for task file "'+TaskSect+'": '+TaskFile);
        TaskTFile := ChangeFileExt(TaskFile, '.tmp');
        if FileExists(TaskTFile) then DeleteFile(TaskTFile);

        for J := 1 to 1000 do
         begin
          TableSect := IniFile.ReadString(TaskSect, 'Table'+IntToStr(J), '');
          if TableSect <> '' then
           begin
            SQL       := IniFile.ReadString(TableSect, 'SQL',      '');
            TableFile := IniFile.ReadString(TableSect, 'Filename', '');
            if SQL       = '' then raise EHandledException.Create(errcode_ExportFail_ConfigError, 'Empty "SQL" for file: '+TableSect);
            if TableFile = '' then raise EHandledException.Create(errcode_ExportFail_ConfigError, 'Empty "FileName" for file: '+TableSect);

            if not Device.DbInterface.FillDataSet(SQL) then raise EHandledException.Create(errcode_ExportFail_ConfigError, 'SQL error: '+Device.DbInterface.LastError);
            Device.DbInterface.DataSet.DisableControls;

            TmpStrm := TMemoryStream.Create;
            try
              // export data to stream
              DsClient := TClientDataSet.Create(nil);
              DsProvdr := TDataSetProvider.Create(nil);
              try
               DsProvdr.DataSet := Device.DbInterface.DataSet;
               DsClient.Data    := DsProvdr.GetRecords(-1, Cnt, MetaDataOption);
               DsClient.SaveToStream(TmpStrm, dfBinary);
               RecCnt := RecCnt + Cnt;
              finally
               DsClient.Free;
               DsProvdr.Free;
               Device.DbInterface.CloseDataSet;
              end;

              // add table to archive
              with TZLBArchive.create(nil) do
              try
               CompressionLevel := fcFastest;
               if FileExists(TaskTFile) then OpenArchive(TaskTFile)
                else CreateArchive(TaskTFile);
               AddStream(TableFile, TmpStrm);
              finally
               Free;
              end;
            finally
             TmpStrm.Free;
            end;

           end
          else
           begin
            if J > 1 then Break
             else raise EHandledException.Create(errcode_ExportFail_ConfigError, 'Table with name "'+TableSect+'" not found in configuration file!');
           end;
         end;

        // rename final file
        if FileExists(TaskTFile) then
        try
         if FileExists(TaskFile) then DeleteFile(TaskFile);
         if not RenameFile(TaskTFile, TaskFile) then
          raise EHandledException.Create(errcode_ExportFail_ConfigError, 'System error! Fail rename file: '+TaskTFile);
        finally
         DeleteFile(TaskTFile);
        end;

        if not FileExists(TaskFile) then raise EHandledException.Create(errcode_ExportFail_ConfigError, 'At the end file does not exist: '+TaskTFile);
       end
      else
       raise EHandledException.Create(errcode_ExportFail_ConfigError, 'Task with name "'+FTaskNameList.Strings[I]+'" not found in configuration file!');

      FTaskNameList.Strings[I] := FTaskNameList.Strings[I] + '='+IntToStr(RecCnt);
     end;

    SetErrorCode(errcode_ExecuteSucceed, '');
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
 if IniFile <> nil then IniFile.Free;
end;

function TCmd_ExportTblFilesServer.AddAnswerToDocument: Boolean;
var iNode : IXMLNode;
    SDate : String;
    SCRC  : String;
    I     : Integer;
begin
 try
  FLoginHandler.XmlDocument := Self.XmlDocument; // много е важно да превключим документа
  if not FLoginHandler.AddAnswerToDocument then raise EAbort.Create(FLoginHandler.LastError);

  SDate := FormatDateTime('DD.MM.YY HH:NN:SS', Now);
  SCRC  := FLoginHandler.CalculateCRC(FTaskNameList.Text + SDate);

  iNode := XML_GetRootNode;
  iNode := iNode.AppendChild(XmlDocument.CreateElement(xml_Node_ExportData));
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_Time)).Text := SDate;
  iNode.AppendChild(XmlDocument.CreateElement(xml_Node_CRC)).Text  := SCRC;

  with iNode.AppendChild(XmlDocument.CreateElement(xml_Node_ExportTasks)) do
   begin
    for I := 0 to FTaskNameList.Count - 1 do
     AppendChild(XmlDocument.CreateElement('Task'+Inttostr(I))).Text  := FTaskNameList.Strings[I];
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

function TCmd_ExportTblFilesServer.FGetUserName: String;
begin
 Result := FLoginHandler.UserName;
end;

function TCmd_ExportTblFilesServer.FGetPassword: String;
begin
 Result := FLoginHandler.Password;
end;

procedure TCmd_ExportTblFilesServer.FSetUserName(Value: String);
begin
 FLoginHandler.UserName := Value;
end;

procedure TCmd_ExportTblFilesServer.FSetPassword(Value: String);
begin
 FLoginHandler.Password := Value;
end;

end.
