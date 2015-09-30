unit DP_SystemUnit;

interface

uses SysUtils, Classes, Forms, DeviceUnit;

type
 TDP_PCTest = class(TDataProcessor)
 public
  function ProcessData(var InfoMsg: String): Boolean; override;
 end;

 TDP_PCMessage = class(TDataProcessor)
 public
  function ProcessData(var InfoMsg: String): Boolean; override;
 end;

 TDP_PCExtData = class(TDataProcessor)
 public
  function ProcessData(var InfoMsg: String): Boolean; override;
 end;

 TDP_PCErrorLog = class(TDataProcessor)
 public
  function ProcessData(var InfoMsg: String): Boolean; override;
 end;

 TDP_SysCheckUpdate = class(TDataProcessor)
 public
  function ProcessData(var InfoMsg: String): Boolean; override;
 end;

 TDP_SysDownloadFile = class(TDataProcessor)
 public
  function ProcessData(var InfoMsg: String): Boolean; override;
 end;

{
 TDP_SysExportTableFiles = class(TDataProcessor)
 public
  function ProcessData(var InfoMsg: String): Boolean; override;
 end;
 }
 TDP_SysUpdateOK = class(TDataProcessor)
 public
  function ProcessData(var InfoMsg: String): Boolean; override;
 end;

 TDP_SysUpdateFailed = class(TDataProcessor)
 public
  function ProcessData(var InfoMsg: String): Boolean; override;
 end;

implementation
uses BillingConstUnit, TestHandler,
     FilesHandler, SendMessageHandler;

//******************************************************************************
//        TDP_Test
//******************************************************************************
function TDP_PCTest.ProcessData(var InfoMsg: String): Boolean;
var TestHandler : THndr_TestServer;
    UserError   : String;
    Res         : Integer;
begin
 TestHandler := THndr_TestServer.Create(XmlInDocument, Device);
 try
  if not TestHandler.GetRequestFromDocument then raise EAbort.Create(TestHandler.LastError);
  if not TestHandler.Execute(Res, UserError) then raise EAbort.Create(TestHandler.LastError);

  if Res = errcode_ExecuteSucceed then
   begin
    TestHandler.XmlDocument := XmlOutDocument;
    if not TestHandler.AddAnswerToDocument then raise EAbort.Create(TestHandler.LastError);
   end;

  OutDoc_SetResponceCode(Res, UserError);
  Result  := true;
 except
  on E: Exception do
   begin
    LastError := E.Message;
    Result    := false;
   end;
 end;
 FreeAndNil(TestHandler);
end;

//******************************************************************************
//        TDP_PCMessage
//******************************************************************************
function TDP_PCMessage.ProcessData(var InfoMsg: String): Boolean;
var MsgHandler : TCmd_PCMessageServer;
    UserError   : String;
    Res         : Integer;
begin
 MsgHandler := TCmd_PCMessageServer.Create(XmlInDocument, Device);
 try
  if not MsgHandler.GetRequestFromDocument then raise EAbort.Create(MsgHandler.LastError);
  if not MsgHandler.Execute(Res, UserError) then raise EAbort.Create(MsgHandler.LastError);

  if Res = errcode_ExecuteSucceed then
   begin
    MsgHandler.XmlDocument := XmlOutDocument;
    if not MsgHandler.AddAnswerToDocument then raise EAbort.Create(MsgHandler.LastError);

    // ESKLoginData.DealerData -> ако не е минал логина = nil
    if not MsgHandler.PostEventUser(0, C_EvType_Message, 'D', MsgHandler.ESKLoginData.EskSerial, '', '',
                         'Text message arrived ['+MsgHandler.MessageType+']'+sLineBreak+
                         MsgHandler.ESKLoginData.EskSerial + '/ ['+InCommandAppname+'/'+InCommandVersion+']') then
     raise EAbort.Create(Self.LastError);

    InfoMsg := 'Message arrived ['+MsgHandler.MessageType+']. Dealer:'+MsgHandler.ESKLoginData.EskSerial;
   end
  else
   begin
    if not MsgHandler.PostEventUser(Res, C_EvType_Message, 'D', MsgHandler.ESKLoginData.EskSerial, '', '',
                         'FAIL receiving message ['+MsgHandler.MessageType+']'+sLineBreak+
                         'ErrorCode:'+IntToStr(Res)+' ErrorMessage: '+UserError+sLineBreak+
                         '['+InCommandAppname+'/'+InCommandVersion+']['+Device.DeviceInfoAsText+']') then
     raise EAbort.Create(Self.LastError);

    InfoMsg := 'Receive message fail. ErrorCode:'+IntToStr(Res)+' ErrorMessage: '+UserError;
   end;

  OutDoc_SetResponceCode(Res, UserError);
  Result  := true;
 except
  on E: Exception do
   begin
    LastError := E.Message;
    Result    := false;
   end;
 end;
 FreeAndNil(MsgHandler);
end;

//******************************************************************************
//        TDP_PCExtData
//******************************************************************************
function TDP_PCExtData.ProcessData(var InfoMsg: String): Boolean;
var DataHandler : TCmd_PCExtDataServer;
    UserError   : String;
    Res         : Integer;
begin
 DataHandler := TCmd_PCExtDataServer.Create(XmlInDocument, Device);
 try
  if not DataHandler.GetRequestFromDocument then raise EAbort.Create(DataHandler.LastError);
  if not DataHandler.Execute(Res, UserError) then raise EAbort.Create(DataHandler.LastError);

  if Res = errcode_ExecuteSucceed then
   begin
    DataHandler.XmlDocument := XmlOutDocument;
    if not DataHandler.AddAnswerToDocument then raise EAbort.Create(DataHandler.LastError);

    // ESKLoginData.DealerData -> ако не е минал логина = nil
    if not DataHandler.PostEventUser(0, C_EvType_DataIn, 'D', DataHandler.EskSerial, '', '',
                         'Ext.client data arrived.'+sLineBreak+
                         '['+InCommandAppname+'/'+InCommandVersion+']['+Device.DeviceInfoAsText+']') then
     raise EAbort.Create(Self.LastError);

    InfoMsg := 'Ext.client data arrived.';
   end
  else
   begin
    if not DataHandler.PostEventUser(Res, C_EvType_DataIn, 'D', DataHandler.EskSerial, '', '',
                         'FAIL receiving ext.data'+sLineBreak+
                         'ErrorCode:'+IntToStr(Res)+' ErrorMessage: '+UserError+sLineBreak+
                         '['+InCommandAppname+'/'+InCommandVersion+']['+Device.DeviceInfoAsText+']') then
     raise EAbort.Create(Self.LastError);

    InfoMsg := 'FAIL receiving ext.data. ErrorCode:'+IntToStr(Res)+' ErrorMessage: '+UserError;
   end;

  OutDoc_SetResponceCode(Res, UserError);
  Result  := true;
 except
  on E: Exception do
   begin
    LastError := E.Message;
    Result    := false;
   end;
 end;
 FreeAndNil(DataHandler);
end;

//******************************************************************************
//     TDP_PCErrorLog
//******************************************************************************
function TDP_PCErrorLog.ProcessData(var InfoMsg: String): Boolean;
var LogHandler : TCmd_PCErrorLogServer;
    UserError  : String;
    Res        : Integer;
begin
 LogHandler := TCmd_PCErrorLogServer.Create(XmlInDocument, Device);
 try
  if not LogHandler.GetRequestFromDocument then raise EAbort.Create(LogHandler.LastError);

  // запис на грешката в базата данни
  LogHandler.PostEventUser(LogHandler.ErrorCode, C_EvType_Error, 'D', LogHandler.EskSerial, LogHandler.DevType, LogHandler.DevSerial,
                           LogHandler.ClientLog.Text+sLineBreak+
                           '['+InCommandAppname+'/'+InCommandVersion+']['+Device.DeviceInfoAsText+']');


  if not LogHandler.Execute(Res, UserError) then raise EAbort.Create(LogHandler.LastError);

  if Res = errcode_ExecuteSucceed then
   begin
    LogHandler.XmlDocument := XmlOutDocument;
    if not LogHandler.AddAnswerToDocument then raise EAbort.Create(LogHandler.LastError);

    InfoMsg := 'Ext.client error arrived.';
   end
  else
   begin
    if not LogHandler.PostEventUser(Res, C_EvType_Error, 'D', LogHandler.EskSerial, '', '',
                         'FAIL receiving client error data'+sLineBreak+
                         'ErrorCode:'+IntToStr(Res)+' ErrorMessage: '+UserError+sLineBreak+
                         '['+InCommandAppname+'/'+InCommandVersion+']['+Device.DeviceInfoAsText+']') then
     raise EAbort.Create(Self.LastError);

    InfoMsg := 'FAIL receiving client error. ErrorCode:'+IntToStr(Res)+' ErrorMessage: '+UserError;
   end;

  OutDoc_SetResponceCode(Res, UserError);
  Result  := true;
 except
  on E: Exception do
   begin
    LastError := E.Message;
    Result    := false;
   end;
 end;
 FreeAndNil(LogHandler);
end;


//******************************************************************************
//        TDP_SysCheckUpdate
//******************************************************************************
function TDP_SysCheckUpdate.ProcessData(var InfoMsg: String): Boolean;
var CheckUpdate: TCmd_CheckUpdateServer;
    UserError  : String;
    Res        : Integer;
begin
 Result     := true;
 CheckUpdate := TCmd_CheckUpdateServer.Create(XmlInDocument, Device);
 try
  if not CheckUpdate.GetRequestFromDocument then raise EAbort.Create(CheckUpdate.LastError);

  if (CheckUpdate.EskSerial = '')and(Device.DeviceInfo.Dev_Type = 'ESK') then CheckUpdate.EskSerial := Device.DeviceInfo.Dev_Serial;

  if not CheckUpdate.Execute(Res, UserError) then raise EAbort.Create(CheckUpdate.LastError);

  if Res = errcode_ExecuteSucceed then
   begin
    CheckUpdate.XmlDocument := XmlOutDocument;
    if not CheckUpdate.AddAnswerToDocument then raise EAbort.Create(CheckUpdate.LastError);

    if not CheckUpdate.PostEventUser(0, C_EvType_FileUpd, 'D', CheckUpdate.EskSerial,
                         CheckUpdate.DeviceType, CheckUpdate.DeviceSerial,
                         'Check version. Proj:'+CheckUpdate.ProjectName+sLineBreak+
                         '['+InCommandAppname+'/'+InCommandVersion+']['+Device.DeviceInfoAsText+']') then
     raise EAbort.Create(Self.LastError);

     if CheckUpdate.DataFromClient <> nil then
      begin
       CheckUpdate.PostEventUser(0, C_EvType_DataIn, 'D', CheckUpdate.EskSerial,
                     CheckUpdate.DeviceType, CheckUpdate.DeviceSerial,
                     'Client data received.');
      end;

    InfoMsg := 'Checked update for project: '+CheckUpdate.ProjectName;
   end
  else
   begin
    if not CheckUpdate.PostEventUser(Res, C_EvType_FileUpd, 'D', CheckUpdate.EskSerial, '', '',
                         'FAIL check update. Proj:'+CheckUpdate.ProjectName+sLineBreak+
                         'ErrorCode:'+IntToStr(Res)+' ErrorMessage: '+UserError+sLineBreak+
                         '['+InCommandAppname+'/'+InCommandVersion+']['+Device.DeviceInfoAsText+']') then
     raise EAbort.Create(Self.LastError);

   InfoMsg := 'Check update for project "'+CheckUpdate.ProjectName+'" FAIL. ErrorCode:'+IntToStr(Res)+' ErrorMessage: '+UserError;
   end;

  OutDoc_SetResponceCode(Res, UserError);
 except
  on E: Exception do
   begin
    Result    := false;
    LastError := E.Message;
   end;
 end;
 CheckUpdate.Free;
end;

//******************************************************************************
//        TDP_SysDownloadFile
//******************************************************************************
function TDP_SysDownloadFile.ProcessData(var InfoMsg: String): Boolean;
var DownlFile : TCmd_DownloadFileServer;
    UserError : String;
    Res       : Integer;
begin
 Result    := true;
 DownlFile := TCmd_DownloadFileServer.Create(XmlInDocument, Device);
 try
  if not DownlFile.GetRequestFromDocument then raise EAbort.Create(DownlFile.LastError);
  if not DownlFile.Execute(Res, UserError) then raise EAbort.Create(DownlFile.LastError);

  if Res = errcode_ExecuteSucceed then
   begin
    DownlFile.XmlDocument := XmlOutDocument;
    if not DownlFile.AddAnswerToDocument then raise EAbort.Create(DownlFile.LastError);

    // ESKLoginData.DealerData -> ако не е минал логина = nil
    if not DownlFile.PostEventUser(0, C_EvType_FileDwn, 'D', DownlFile.ESKLoginData.EskSerial, '', '',
                         'Serve files. Proj:'+DownlFile.ProjectName+sLineBreak+
                         'Files: '+IntToStr(DownlFile.FilesCount)+sLineBreak+
                         DownlFile.FilesListAsText+sLineBreak+
                         InCommandAppname+'/'+InCommandVersion) then
     raise EAbort.Create(Self.LastError);

    InfoMsg := 'Serve files to: '+DownlFile.ESKLoginData.EskSerial;
   end
  else
   begin
    if not DownlFile.PostEventUser(Res, C_EvType_FileDwn, 'D', DownlFile.ESKLoginData.EskSerial, '', '',
                         'FAIL Serving files!'+sLineBreak+
                         'ErrorCode:'+IntToStr(Res)+' ErrorMessage: '+UserError+sLineBreak+
                         '['+InCommandAppname+'/'+InCommandVersion+']['+Device.DeviceInfoAsText+']') then
     raise EAbort.Create(Self.LastError);

    InfoMsg := 'Fail serve files. ErrorCode:'+IntToStr(Res)+' ErrorMessage: '+UserError;
   end;

  OutDoc_SetResponceCode(Res, UserError);
 except
  on E: Exception do
   begin
    Result    := false;
    LastError := E.Message;
   end;
 end;
 DownlFile.Free;
end;

//******************************************************************************
//        TDP_SysExportTableFiles
//******************************************************************************
{
function TDP_SysExportTableFiles.ProcessData(var InfoMsg: String): Boolean;
var FilesExport : TCmd_ExportTblFilesServer;
    UserError   : String;
    Res         : Integer;
begin
 Result     := true;
 FilesExport := TCmd_ExportTblFilesServer.Create(XmlInDocument, Device);
 try
  if not FilesExport.GetRequestFromDocument then raise EAbort.Create(FilesExport.LastError);
  if not FilesExport.Execute(Res, UserError) then raise EAbort.Create(FilesExport.LastError);

  if Res = errcode_ExecuteSucceed then
   begin
    FilesExport.XmlDocument := XmlOutDocument;
    if not FilesExport.AddAnswerToDocument then raise EAbort.Create(FilesExport.LastError);

    if not FilesExport.PostEventUser(0, C_EvType_FileExp, 'A', FilesExport.UserName, '', '',
                         'Table files exported:'+FilesExport.TaskNameList.Text+sLineBreak+
                         '['+InCommandAppname+'/'+InCommandVersion+']['+Device.DeviceInfoAsText+']') then
     raise EAbort.Create(Self.LastError);

    InfoMsg := 'Export table files succeed.';
   end
  else
   begin
    if not FilesExport.PostEventUser(Res, C_EvType_FileExp, 'A',  FilesExport.UserName, '', '',
                         'FAIL export table files.'+sLineBreak+
                         'ErrorCode:'+IntToStr(Res)+' ErrorMessage: '+UserError+sLineBreak+
                         '['+InCommandAppname+'/'+InCommandVersion+']['+Device.DeviceInfoAsText+']') then
     raise EAbort.Create(Self.LastError);

    InfoMsg := 'Export table files fail. ErrorCode:'+IntToStr(Res)+' ErrorMessage: '+UserError;
   end;

  OutDoc_SetResponceCode(Res, UserError);
 except
  on E: Exception do
   begin
    Result    := false;
    LastError := E.Message;
   end;
 end;
 FilesExport.Free;
end;
 }

//******************************************************************************
//        TDP_SysUpdateOK
//******************************************************************************
function TDP_SysUpdateOK.ProcessData(var InfoMsg: String): Boolean;
var CheckUpdate: TCmd_CheckUpdateServer;
    UserError  : String;
    Res        : Integer;
begin
 Result     := true;
end;

//******************************************************************************
//        TDP_SysUpdateFailed
//******************************************************************************
function TDP_SysUpdateFailed.ProcessData(var InfoMsg: String): Boolean;
var CheckUpdate: TCmd_CheckUpdateServer;
    UserError  : String;
    Res        : Integer;
begin
 Result     := true;
end;


end.
