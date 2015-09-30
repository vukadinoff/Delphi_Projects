unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, BillingClientUnit, StrResUnit, ConstUnit, Contnrs,
  TestHandler, BillingConstUnit, FilesHandler, SendMessageHandler, IniFiles, ExtCtrls,
  SyncObjs, ShellApi, GetFilesUnit, DateUtils, dxCore, dxSpeedButton;

const

 C_IniFileName    : String = 'BTT_Updater.ini';
 C_RunFileName    : String = 'Run.ini';
 C_POSIniFileName : String = 'POS\ELPos_TouchConfig.ini';

type
  TMainForm = class(TForm)
    PanelMain: TPanel;
    PanelCpanel: TPanel;
    PanelTitle: TPanel;
    lblMsg: TLabel;
    lblTitle: TLabel;
    EltradeImage: TImage;
    PanelLeft: TPanel;
    PanelTop: TPanel;
    PanelBtn: TPanel;
    btnCheckAgain: TdxSpeedButton;
    btnContinueAdmin: TdxSpeedButton;
    btnContinue: TdxSpeedButton;
    PanelBottom: TPanel;
    prgBar: TProgressBar;
    lstMsg: TMemo;
    bvlLine: TBevel;
    PanelProgress: TPanel;
    prgParts: TProgressBar;
    PanelProgressSpace: TPanel;

    procedure FormCreate(Sender: TObject);
    procedure btnTestClick(Sender: TObject);
    procedure btnCheckAgainClick(Sender: TObject);
    procedure btnContinueClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);

  private
    { Private declarations }
    LocalPath     : String;
    LogPath       : String;
    ProjectPath   : String;
    ProjectName   : String;
    Version       : String;
    CmdTimeDelta  : TDateTime;
    StartTime     : TDateTime;
    Set_ServerHostWork: String;
    ObjectNum     : Integer;

    TitleMsg          : String;
    UpdateMsg         : String;
    DownloadingMsg    : String;
    FailedMsg         : String;
    RebootMsg         : String;
    BtnCheckAgainMsg  : String;
    BtnNoCheckMsg     : String;
    BtnContinueMsg    : String;

    ReadTimeout   : Integer;
    ChunkSize     : Integer;
    LogDays       : Integer;
    ViewDebug     : Integer;
    NeedRestart   : Boolean;
    UpdateSuccess : Boolean;

    LogFileName   : String;
    ErrorFileName : String;

    procedure AddMsg(msg: String);
    procedure HandleGlobalException(Sender: TObject; E: Exception);
    procedure PostLogToFile(Path, Fname, Line: String);
    function  GetVersion: String;
    function  GetApplicationName: String;
    function  FormatFileSize(Size_: Integer): String;
    procedure HttpReadProgress(Percent: Integer);

    procedure LoadIniSettings(IniFileName: String; PosIniFileName: String);

    procedure TestConnection();
    procedure ConfirmUpdate();
    procedure FailedUpdate();
    procedure RunTheExternal();
    procedure ClearLogs();
    procedure DoUpdate();
    procedure CheckUpdates();

  public
    { Public declarations }
    property C_LogFileName: String read LogFileName;
    property C_ErrorFileName: String read ErrorFileName;
  end;

var
  MainForm: TMainForm;

implementation

uses DataUnit;

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
 DecimalSeparator := '.';
 ShortDateFormat  := 'DD.MM.YYYY';
 LocalPath        := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName));
 LogPath          := IncludeTrailingPathDelimiter(LocalPath + 'Log\');
 CmdTimeDelta     := Now;
 StartTime        := Now;
 Version          := GetVersion;
 prgBar.Position := 0;

 Application.OnException  := HandleGlobalException;
 
 ForceDirectories(LogPath);

 LogFileName             := 'BTT_UpdaterLog_' + FormatDateTime('DD_MM_YY', Now) + '.txt';
 ErrorFileName           := 'BTT_UpdaterErrors_' + FormatDateTime('DD_MM_YY', Now) + '.txt';

 LoadIniSettings(LocalPath+C_IniFileName, C_POSIniFileName);

 ClearLogs();

 Self.Width := Screen.Width;
 Self.Height := Screen.Height;

 PanelCpanel.Top := PanelTop.Height + Round((Self.Height - PanelTop.Height - PanelBottom.Height - PanelCpanel.Height) / 2);
 PanelCpanel.Left := Round((Self.Width - PanelCpanel.Width) / 2);

end;

procedure TMainForm.AddMsg(msg: String);
begin
  lstMsg.Lines.Add(msg);
end;

function TMainForm.GetApplicationName: String;
begin
 Result := ChangeFileExt(ExtractFileName(Application.ExeName), '');
end;

procedure TMainForm.PostLogToFile(Path, Fname, Line: String);
var F : TextFile;
    I : Integer;
    CSec : TCriticalSection;
begin
  CSec := TCriticalSection.Create;
  CSec.Enter;
  try
  {$I-}
    AssignFile(f, Path + Fname);
    Append(f);
    if IOResult <> 0 then Rewrite(f);
    Writeln(f, FormatDateTime('DD.MM.YY HH:NN:SS.zzz', Now)+' '+line);
    I := FileSize(F);
    CloseFile(f);
  finally
	  CSec.Release;
  end;
end;

procedure TMainForm.HandleGlobalException(Sender: TObject; E: Exception);
var Nme  : String;
begin
try
  if Sender <> nil then Nme := '['+Sender.ClassName+']'
   else Nme := '';

  AddMsg('Global exception: ' + Nme + ':' + E.Message);

  PostLogToFile(LogPath, C_ErrorFileName, Nme+':Global exception:' + E.Message);
except
 on E:Exception
 do raise Exception.CreateFmt('Critical Error@HandleGlobalException with message "%s"',[E.Message]);
end
end;

function TMainForm.GetVersion: String;
const
  InfoNum = 10;
  InfoStr: array[1..InfoNum] of string = ('CompanyName', 'FileDescription',
                   'FileVersion', 'InternalName', 'LegalCopyright', 'LegalTradeMarks',
                   'OriginalFileName', 'ProductName', 'ProductVersion', 'Comments');

var S      : string;
    N, Len : DWORD;
    Buf    : PChar;
    Value  : PChar;
    LangCharset : string;
    PCharset    : PLongInt;
    InfoLength  : UINT;
begin
 try
  Result := '';
  S := Application.ExeName;
  N := GetFileVersionInfoSize(PChar(S), N);
  if N > 0 then
   begin
    Buf := AllocMem(N);
    GetFileVersionInfo(PChar(S), 0, N, Buf);
    // Get translation info for Language / CharSet IDs
    if VerQueryValue(Buf, '\VarFileInfo\Translation',
      Pointer(PCharset), InfoLength) then
     begin
      LangCharset := Format('%.4x%.4x',[LoWord (PCharset^), HiWord (PCharset^)]);
     end
    else LangCharset := '040904E4';

    if VerQueryValue(Buf, PChar('StringFileInfo\'+LangCharset+'\' + InfoStr[3]), Pointer(Value), Len) then
      Result := StrPas(Value);
    FreeMem(Buf, N);
   end;
 except
 end;
end;

procedure TMainForm.btnTestClick(Sender: TObject);
begin
  TestConnection();
  //ConfirmUpdate();
end;

procedure TMainForm.DoUpdate();
begin
  Self.btnCheckAgain.Visible := false;
  Self.btnContinue.Visible := false;
  Self.btnContinueAdmin.Visible := false;
  Self.lblMsg.Caption := Self.UpdateMsg;

  if Self.ViewDebug > 0 then Self.lstMsg.Visible := true;


  MainForm.Repaint();
  MainForm.Refresh();
  MainForm.Invalidate();

  screen.Cursor := crHourglass;

  try
    CheckUpdates();
  finally
    screen.Cursor := crArrow;
  end;

  if Self.UpdateSuccess then
  begin
    if Self.ViewDebug > 0 then
      begin
        Self.btnContinueAdmin.Visible := true;
      end
    else
      begin
        RunTheExternal();
        Application.Terminate();
      end;
  end
  else
  begin
    Self.lblMsg.Caption := Self.FailedMsg;
    btnCheckAgain.Visible := True;
    btnContinue.Visible := True;
  end;
end;


function TMainForm.FormatFileSize(Size_: Integer): String;
begin
 Result := IntToStr(Size_) + ' bytes';
 if Size_  > 1024 then
  begin
   Size_ := Size_ div 1024;
   Result := IntToStr(Size_) + ' Kbytes';
  end;
 if Size_  > 1024 then
  begin
   Result := FormatFloat('0.00', Size_/1024) + ' Mbytes';
  end;
end;

procedure TMainForm.HttpReadProgress(Percent: Integer);
begin
 prgBar.Position := Percent;
 //prgBar.Visible  := (Percent in [0..100]);
end;

procedure TMainForm.LoadIniSettings(IniFileName: String; PosIniFileName: String);
var IniSection : String;
begin
 IniSection := 'SETTINGS';

 with TIniFile.Create(IniFileName) do
 try
  if not ValueExists(IniSection, 'UpdateServer')   then WriteString (IniSection, 'UpdateServer', 'partners.eltrade.com:7012');
  if not ValueExists(IniSection, 'ProjectPath')    then WriteString (IniSection, 'ProjectPath', 'D:\ELTRADE');
  if not ValueExists(IniSection, 'ProjectName')    then WriteString (IniSection, 'ProjectName', 'Bulgartabac');
  if not ValueExists(IniSection, 'ChunkSize')      then WriteInteger (IniSection,'ChunkSize', 65536);
  if not ValueExists(IniSection, 'LogDays')        then WriteInteger (IniSection,'LogDays', 30);
  if not ValueExists(IniSection, 'ViewDebug')      then WriteInteger (IniSection,'ViewDebug', 0);

  Set_ServerHostWork := ReadString (IniSection, 'UpdateServer', '');
  ProjectPath := IncludeTrailingPathDelimiter(ReadString (IniSection, 'ProjectPath', ''));
  ProjectName := ReadString (IniSection, 'ProjectName', '');
  ChunkSize :=  ReadInteger (IniSection, 'ChunkSize', 65536);
  LogDays :=  ReadInteger (IniSection, 'LogDays', 30);
  ViewDebug :=  ReadInteger (IniSection, 'ViewDebug', 0);

  if ViewDebug > 0 then lstMsg.Visible := true;

 finally
  Free;
 end;

 IniSection := 'COMMON';

 with TIniFile.Create(ProjectPath + PosIniFileName) do
 try
  ObjectNum := ReadInteger (IniSection, 'POSName', 0);
 finally
  Free;
 end;


 IniSection := 'BG';

 with TIniFile.Create(ChangeFileExt(IniFileName, '.lng')) do
 try
  if not ValueExists(IniSection, 'TitleMsg')   then WriteString (IniSection, 'TitleMsg', '�������� �� ������� ���� ������ �� ��������');
  if not ValueExists(IniSection, 'UpdateMsg')    then WriteString (IniSection, 'UpdateMsg', '����, ��������� ������ ���� ���������� �� ���� ������!');
  if not ValueExists(IniSection, 'DownloadingMsg')    then WriteString (IniSection, 'DownloadingMsg', '������� � ���� ������ �� �������. ����, ��������� ������������!');
  if not ValueExists(IniSection, 'FailedMsg')    then WriteString (IniSection, 'FailedMsg', '���� ���������� � ����������!|����, ��������� ���� ������ � ������� (������ �����).|��� �������� ��������� ��������, ����, �������� �� � ������ �� ������� ���.|�� ����������: ���. 0877 736 677.');
  if not ValueExists(IniSection, 'RebootMsg')     then WriteString (IniSection, 'RebootMsg', '��������� ��� ����� �� �������.|������ �� �� ������������ ����?');
  if not ValueExists(IniSection, 'BtnCheckAgainMsg')   then WriteString (IniSection, 'BtnCheckAgainMsg', '��� ���� �� �������� �� ���� ������');
  if not ValueExists(IniSection, 'BtnNoCheckMsg')      then WriteString (IniSection, 'BtnNoCheckMsg', '������������ ��� ��������');
  if not ValueExists(IniSection, 'BtnContinueMsg')      then WriteString (IniSection, 'BtnContinueMsg', '��������');

  TitleMsg := StringReplace(ReadString (IniSection, 'TitleMsg', ''), '|', sLineBreak, [rfReplaceAll, rfIgnoreCase]);
  UpdateMsg := StringReplace(ReadString (IniSection, 'UpdateMsg', ''), '|', sLineBreak, [rfReplaceAll, rfIgnoreCase]);
  DownloadingMsg := StringReplace(ReadString (IniSection, 'DownloadingMsg', ''), '|', sLineBreak, [rfReplaceAll, rfIgnoreCase]);
  FailedMsg := StringReplace(ReadString (IniSection, 'FailedMsg', ''), '|', sLineBreak, [rfReplaceAll, rfIgnoreCase]);
  RebootMsg := StringReplace(ReadString (IniSection, 'RebootMsg', ''), '|', sLineBreak, [rfReplaceAll, rfIgnoreCase]);
  BtnCheckAgainMsg := StringReplace(ReadString (IniSection, 'BtnCheckAgainMsg', ''), '|', sLineBreak, [rfReplaceAll, rfIgnoreCase]);
  BtnNoCheckMsg := StringReplace(ReadString (IniSection, 'BtnNoCheckMsg', ''), '|', sLineBreak, [rfReplaceAll, rfIgnoreCase]);
  BtnContinueMsg := StringReplace(ReadString (IniSection, 'BtnContinueMsg', ''), '|', sLineBreak, [rfReplaceAll, rfIgnoreCase]);

 finally
  Free;
 end;

 Self.lblTitle.Caption := TitleMsg;
 Self.lblMsg.Caption := UpdateMsg;
 Self.btnCheckAgain.Caption := BtnCheckAgainMsg;
 Self.btnContinue.Caption := BtnNoCheckMsg;
 Self.btnContinueAdmin.Caption := BtnContinueMsg;
end;


procedure TMainForm.TestConnection();
var HttpClient : TBillingXMLClient;
begin
 HttpClient := nil;

 try
  AddMsg('���� �� �������� ��� ������� ...');

  if Set_ServerHostWork = '' then raise EAbort.Create(S_ErrCommServerNotSpecified);

  HttpClient := TBillingXMLClient.Create(Set_ServerHostWork, dev_ESK, IntToStr(ObjectNum), CurrentAppVers);
  HttpClient.AddCommand(TCmd_TestClient);

  case HttpClient.SendCommand(0, hcmPost, true) of

  errcode_ExecuteFail_Internal: raise EAbort.Create(S_ErrCommunicationError+sLineBreak+HttpClient.LastError);

  errcode_ExecuteSucceed:
     begin
      AddMsg('���������� ��������� �������!');
     end;
  else
     begin
      PostLogToFile(LogPath, C_ErrorFileName, HttpClient.LastCommandErrorMessage);

      AddMsg('���������� �������� �� �������.');
      AddMsg('��� �� ��������: ' + IntToStr(HttpClient.LastCommandResultCode));
      AddMsg(HttpClient.LastCommandErrorMessage);

      raise EAbort.Create('���������� �������� �� �������:'+sLineBreak+
                          '��� ������: '+IntToStr(HttpClient.LastCommandResultCode)+sLineBreak+
                          HttpClient.LastCommandErrorMessage);
     end;
  end;
 except
  on E: Exception do
   begin
    PostLogToFile(LogPath, C_ErrorFileName, E.Message);
    AddMsg('��������� ���� �� �������� ��� �������:');
    AddMsg(E.Message);

    {
    MessageDlg('��������� ���� �� �������� ��� �������:'+#13+#10+
               ''+#13+#10+
               E.Message, mtError, [mbOK], 0);
    }
   end;
 end;
 if HttpClient <> nil then HttpClient.Free;
end;


procedure TMainForm.CheckUpdates();
var tmp_dnl: Integer;
var HttpClient : TBillingXMLClient;
    CmdCheckUpd: TCmd_CheckUpdateClient;
    CmdDownload: TCmd_DownloadFileClient;
    APin       : String;
    ASavePin   : Boolean;
    UpdPath    : String;
    DownlPath  : String;
    BackupPath : String;
    I          : Integer;
    DownlSize  : Integer;
    lRemains: Integer;
    lOffset: Integer;
    PartNum: Integer;

    Fname: String;
    FNList: TStringList;
    FOffset: Integer;
    FSize: Integer;
    LastMD5: String;
    LastVer: String;
    LastFname: String;
    LastDate: TDateTime;
    LastFileSize: Integer;
    OutFile: TFileStream;
    Buffer: Pointer;
    CSec      : TCriticalSection;

    procedure MoveFile_(Fname_, FromPath_, ToPath_: String; RaiseOnFail_: Boolean = true);
    begin
      ForceDirectories(ExtractFileDir(FromPath_ + Fname_));
      ForceDirectories(ExtractFileDir(ToPath_ + Fname_));

     if FileExists(ToPath_ + Fname_) then DeleteFile(ToPath_+Fname_);
     if MoveFile(PChar(FromPath_+Fname_), PChar(ToPath_+Fname_)) then
      PostLogToFile(LogPath, C_LogFileName, 'Moved file "'+Fname_+'" ('+FromPath_+' -> '+ToPath_+')')
     else
     if RaiseOnFail_ then raise EAbort.Create('Failed to move file "'+Fname_+'" ('+FromPath_+' -> '+ToPath_+')');
    end;

    procedure CopyFile_(Fname_, FromPath_, ToPath_: String; RaiseOnFail_: Boolean = true);
    begin
      ForceDirectories(ExtractFileDir(FromPath_ + Fname_));
      ForceDirectories(ExtractFileDir(ToPath_ + Fname_));

      if FileExists(ToPath_+Fname_) then DeleteFile(ToPath_+Fname_);

      if CopyFile(PChar(FromPath_+Fname_), PChar(ToPath_+Fname_), true) then
      begin
        PostLogToFile(LogPath, C_LogFileName, 'Copy file "'+Fname_+'" ('+FromPath_+' -> '+ToPath_+')')
      end
      else
      begin
        if RaiseOnFail_ then raise EAbort.Create('Failed to copy file "'+Fname_+'" ('+FromPath_+' -> '+ToPath_+')');
      end;
      
    end;

    procedure MergeIniFile_(Fname_, FromPath_, ToPath_: String; RaiseOnFail_: Boolean = true);
    var i, j: integer;
    var TmpValue, TmpOrigValue: String;
    var TmpSections, TmpValues: TStringList;
    begin
      ForceDirectories(ExtractFileDir(FromPath_ + Fname_));
      ForceDirectories(ExtractFileDir(ToPath_ + Fname_)); 

     try

      TmpSections := TStringList.Create;
      TmpValues := TStringList.Create;

      with TIniFile.Create(FromPath_+Fname_) do
      try
       ReadSections(TmpSections);

       for i := 0 to TmpSections.Count -1 do
       begin
         TmpValues.Clear;
         ReadSection(TmpSections[i], TmpValues);

         for j := 0 to TmpValues.Count - 1 do
         begin
           TmpValue := ReadString(TmpSections[i], TmpValues[j], '');

           with TIniFile.Create(ToPath_+Fname_) do
           try
             TmpOrigValue := ReadString(TmpSections[i], TmpValues[j], '');

             if (TmpOrigValue <> TmpValue) then
             begin
              WriteString (TmpSections[i], TmpValues[j], TmpValue);
              AddMsg('Merged IniFile ' + Fname_ + ' [' + TmpSections[i] + '] ' + TmpValues[j] + '=' + TmpValue);
              PostLogToFile(LogPath, C_LogFileName, 'Merged IniFile ' + Fname_ + ' [' + TmpSections[i] + '] ' + TmpValues[j] + '=' + TmpValue);
             end;

           finally
             Free;
           end;

         end;

       end;
      finally
        Free;
      end;

      TmpSections.Free;
      TmpValues.Free;

     except
       on E: Exception do
       begin
        AddMsg('Error: ' + E.Message);

        if RaiseOnFail_ then raise EAbort.Create('Failed to merge ini file "'+Fname_+'" ('+FromPath_+' -> '+ToPath_+')');
       end;

     end;

    end;

begin
 Self.UpdateSuccess := false;
 Self.NeedRestart := false;

 prgBar.Position := 0;
 prgParts.Position := 0;
 tmp_dnl := 0;
    
 HttpClient := nil;
 APin       := '';

 UpdPath    := IncludeTrailingPathDelimiter(LocalPath + 'AutoUpdates');

 if not DirectoryExists(UpdPath) then CreateDir(UpdPath);

 UpdPath    := IncludeTrailingPathDelimiter(UpdPath + FormatDateTime('YYYYMMDD', Date));

 try
  AddMsg('Checking for updates. Server: ' + Set_ServerHostWork + ' ...');
  PostLogToFile(LogPath, C_LogFileName, 'Checking for updates. Server: ' + Set_ServerHostWork);

  if Set_ServerHostWork = '' then raise EAbort.Create(S_ErrCommServerNotSpecified);

  HttpClient := TBillingXMLClient.Create(Set_ServerHostWork, dev_ESK, IntToStr(ObjectNum), CurrentAppVers);
  CmdCheckUpd := TCmd_CheckUpdateClient(HttpClient.AddCommand(TCmd_CheckUpdateClient));
  CmdDownload := TCmd_DownloadFileClient(HttpClient.AddCommand(TCmd_DownloadFileClient));

  CmdCheckUpd.EskSerial    := IntToStr(ObjectNum);
  CmdCheckUpd.ProjectName  := ProjectName;
  CmdCheckUpd.ProjectData.Values[C_Project_SwName] := CmdCheckUpd.ProjectName;
  CmdCheckUpd.ProjectData.Values[C_Project_SwVer]  := CurrentAppVers;
  CmdCheckUpd.ProjectData.Values[C_Project_Host]   := CurrentHostName;
  CmdCheckUpd.ProjectData.Values[C_Project_Path]   := ProjectPath;
  //CmdCheckUpd.ProjectData.Values[C_Project_ESKVer] := CurrentESK.ESKVersion;

  //CmdDownload.ESKLoginData.AssignEskData(CurrentESK.ESKData);

  CmdDownload.ESKLoginData.EskSerial := IntToStr(ObjectNum);
  CmdDownload.ESKLoginData.EskCode := IntToStr(ObjectNum);
  CmdDownload.ESKLoginData.EskKey := IntToStr(ObjectNum);
  CmdDownload.ESKLoginData.EskVersion := IntToStr(ObjectNum);

  CmdDownload.ProjectName  := CmdCheckUpd.ProjectName;

  case HttpClient.SendCommand(0, hcmPost, true) of

  errcode_ExecuteFail_Internal: raise EAbort.Create(S_ErrCommunicationError+sLineBreak+HttpClient.LastError);

  errcode_ExecuteSucceed:
  begin
      AddMsg('Success, ' + IntToStr(CmdCheckUpd.FilesCount)+' files found');

      // ���������� ������� �� ��������� �� ������� � ���������� �� � ������� �� ���������
      DownlSize := 0;

      for I := 0 to CmdCheckUpd.FilesCount - 1 do
      with CmdCheckUpd.FilesInfo[I] do
      begin

        ForceDirectories(ExtractFileDir(ProjectPath + FileName));

        if FileExists(ProjectPath + FileName) then
        begin
          // �������� �� MD5 �� ��������� �� ������� � ���������
          NeedUpdate := not SameText(FileMD5,  GetFileMD5(ProjectPath + FileName));

          if (NeedUpdate) then
            if ((ExtractFileExt(FileName) = '.gdb') or (ExtractFileExt(FileName) = '.GDB')) then
            begin
              NeedUpdate := false; // ���� �� �� ���������!!! ������� �� ���� ��� �� ����
            end
            else
            begin
               DownlSize := DownlSize + FileSize;
            end;

        end
        else
        begin
          // ���������� ������� ������� �������� �� ������
          NeedUpdate := true;
          DownlSize := DownlSize + FileSize;
        end;
      end;
      
      if DownlSize >0 then
      begin
        lblMsg.Caption := DownloadingMsg;
      end;

      CSec := TCriticalSection.Create;

      for I := 0 to CmdCheckUpd.FilesCount - 1 do
      with CmdCheckUpd.FilesInfo[I] do
      begin
        if not NeedUpdate then continue;

        BackupPath := IncludeTrailingPathDelimiter(UpdPath + 'Backup');
        DownlPath  := IncludeTrailingPathDelimiter(UpdPath + 'Update');

        ForceDirectories(ExtractFileDir(UpdPath + FileName));
        ForceDirectories(ExtractFileDir(DownlPath + FileName));
        ForceDirectories(ExtractFileDir(BackupPath + FileName));

        if FileExists(DownlPath + FileName) then DeleteFile(DownlPath + FileName);

        AddMsg('Get file ' + FileName + ', uncompressed size: ' +IntToStr(FileSize)+' ('+FormatFileSize(FileSize) + ')');
        PostLogToFile(LogPath, C_LogFileName, 'Get file ' + FileName + ', uncompressed size: ' +IntToStr(FileSize)+' ('+FormatFileSize(FileSize) + ')');

        PartNum := 0;

        CSec.Enter;
        try
          if FileExists(DownlPath + FileName) then OutFile := TFileStream.Create(DownlPath + FileName, fmOpenReadWrite)
          else OutFile := TFileStream.Create(DownlPath + FileName, fmCreate);

          repeat
            Inc(PartNum);
            lOffset := ChunkSize*(PartNum-1);
            lRemains := FileSize - lOffset;

            if lRemains > ChunkSize then
            begin
              CmdDownload.AddFileForDownload(FileName + '@' + IntToStr(lOffset) + '@' + IntToStr(ChunkSize));
            end
            else
            begin
              CmdDownload.AddFileForDownload(FileName + '@' + IntToStr(lOffset) + '@' + IntToStr(lRemains));
            end;

            prgBar.Position := 0;

            HttpClient.OnReadProgress := HttpReadProgress;

            case HttpClient.SendCommand(1, hcmPost) of

              errcode_ExecuteFail_Internal:
              begin
                prgBar.Position := 0;
                
                AddMsg('Error! ' + HttpClient.LastError);

                OutFile.Free;
                CSec.Release;

                FailedUpdate();
                
                raise EAbort.Create(S_ErrCommunicationError+sLineBreak+HttpClient.LastError);
              end;

              errcode_ExecuteSucceed:
              begin
                with CmdDownload.FilesData[0] do
                begin
                  prgBar.Position := 0;
                  tmp_dnl := tmp_dnl + ChunkSize;
                  AddMsg('Downloaded file ' + FileName + ' part ' + IntToStr(PartNum) + ' (offset ' + IntToStr(Offset) + ', partsize: ' + IntToStr(PartSize) + ')');
                  //OutFile.Position := Offset;
                  FileStream.SaveToStream(OutFile);
                  prgParts.Position := Round((tmp_dnl / DownlSize) * 100);
                  Application.ProcessMessages();
                  MainForm.Repaint();
                  MainForm.Refresh();
                  MainForm.Invalidate();
                end;
              end;
             end;

          until FileSize < (lOffset + ChunkSize);

          OutFile.Free;

        finally
          CSec.Release;
        end;

        if GetFileMD5(DownlPath + FileName) <> FileMD5 then
        begin
          AddMsg('Downloaded file is corupt:' + DownlPath + FileName);
          FailedUpdate();
          raise EAbort.Create('Corrupted file found: ' + DownlPath + FileName);
        end;

        AddMsg('File ' + FileName + ' OK! ' + FormatFileSize(FileSize));
        PostLogToFile(LogPath, C_LogFileName, 'File ' + FileName + ' OK! ' + FormatFileSize(FileSize));

      end;
       
      if DownlSize > 0 then
      begin

        // backup old files
        AddMsg('Backing up old files:');
        try
          for I := 0 to CmdCheckUpd.FilesCount - 1 do
          with CmdCheckUpd.FilesInfo[I] do
          begin
            if not NeedUpdate then continue;

            ForceDirectories(ExtractFileDir(ProjectPath + FileName));
            ForceDirectories(ExtractFileDir(BackupPath + FileName));

            if FileExists(ProjectPath + FileName) then
            begin

            if (ExtractFileExt(ProjectPath + FileName) = '.ini') then
              begin
                CopyFile_(FileName, ProjectPath, BackupPath);
                AddMsg(ProjectPath + FileName + ' -> ' + BackupPath + FileName + ' (copy)');
                PostLogToFile(LogPath, C_LogFileName, ProjectPath + FileName + ' -> ' + BackupPath + FileName + ' (copy)');
              end
            else
              begin
                MoveFile_(FileName, ProjectPath, BackupPath);
                AddMsg(ProjectPath + FileName + ' -> ' + BackupPath + FileName);
                PostLogToFile(LogPath, C_LogFileName, ProjectPath + FileName + ' -> ' + BackupPath + FileName);
              end;
            end;
          end;

        except
          on E: Exception do
          begin
            AddMsg('Error: ' + E.Message);
            AddMsg('Restoring old files:');

            PostLogToFile(LogPath, C_ErrorFileName, 'Exception during update: ' + E.Message);
            PostLogToFile(LogPath, C_ErrorFileName, 'Restoring old files...');

            // restore files if somethig fails
            for I := 0 to CmdCheckUpd.FilesCount - 1 do
            with CmdCheckUpd.FilesInfo[I] do
            begin
              if not NeedUpdate then continue;

              if (
                  (FileExists(BackupPath + FileName)) and
                  (
                    (not FileExists(ProjectPath + FileName)) or
                    (ExtractFileExt(ProjectPath + FileName) = '.ini')
                  )
                ) then
              begin
                AddMsg(BackupPath + FileName + ' -> ' + ProjectPath + FileName);
                MoveFile_(FileName, BackupPath, ProjectPath, false);
              end;
            end;

            FailedUpdate();
            raise;
          end;
        end;

        prgParts.Position := 101;
        MainForm.Repaint();
        MainForm.Refresh();
        MainForm.Invalidate();
        
        // Update files
        try
          for I := 0 to CmdCheckUpd.FilesCount - 1 do
          with CmdCheckUpd.FilesInfo[I] do
          begin
            if not NeedUpdate then continue;

            if (FileExists(ProjectPath + FileName) and
                (ExtractFileExt(ProjectPath + FileName) = '.ini')) then // �� ini ������� �� ����� ���� ������ �� �������� � �����������
              begin
                MergeIniFile_(FileName, DownlPath, ProjectPath);
              end
            else
              begin
                CopyFile_(FileName, DownlPath, ProjectPath);
                AddMsg('Updated file: ' + FileName);
                PostLogToFile(LogPath, C_LogFileName, 'Updated file: ' + FileName);
              end;
          end;

        except
          on E: Exception do
          begin
            AddMsg('Error: ' + E.Message);
            AddMsg('Restoring old files:');
            
            PostLogToFile(LogPath, C_ErrorFileName, 'Exception during update: ' + E.Message);
            PostLogToFile(LogPath, C_ErrorFileName, 'Restoring old files...');

            // restore files if somethig fails
            for I := 0 to CmdCheckUpd.FilesCount - 1 do
            with CmdCheckUpd.FilesInfo[I] do
            begin
              if not NeedUpdate then continue;

              if (
                  (FileExists(BackupPath + FileName)) and
                  (
                    (not FileExists(ProjectPath + FileName)) or
                    (ExtractFileExt(ProjectPath + FileName) = '.ini')
                  )
                 ) then
              begin
                AddMsg(BackupPath + FileName + ' -> ' + ProjectPath + FileName);
                MoveFile_(FileName, BackupPath, ProjectPath, false);
              end;
            end;

            FailedUpdate();
            raise;
          end;

        end;

        AddMsg('Operation success.');
        PostLogToFile(LogPath, C_LogFileName, 'Update completed.');
        Self.UpdateSuccess := true;
        ConfirmUpdate();

        prgParts.Position := 102;
        MainForm.Repaint();
        MainForm.Refresh();
        MainForm.Invalidate();
       end
      else
       begin
        AddMsg('No updates found.');
        Self.UpdateSuccess := true;
       end;
     end;
  else
     begin
      AddMsg('Operation denied by server:');
      AddMsg(HttpClient.LastCommandErrorMessage);

      raise EAbort.Create('Operation denied by server:'+sLineBreak+
                          'Error code: '+IntToStr(HttpClient.LastCommandResultCode)+sLineBreak+
                          HttpClient.LastCommandErrorMessage);
     end;
  end;
 except
  on E: Exception do
   begin
    AddMsg('Check update failed:');
    AddMsg(E.Message);
    PostLogToFile(LogPath, C_ErrorFileName, 'Check update failed: ' + E.Message);

    FailedUpdate();
   end;
 end;

 if HttpClient <> nil then HttpClient.Free;

end;

procedure TMainForm.ConfirmUpdate();
var HttpClient : TBillingXMLClient;
    CmdSendMsg : TCmd_PCMessageClient;
begin
 HttpClient := nil;

 try
  AddMsg('Send update confirmation ...');

  if Set_ServerHostWork = '' then raise EAbort.Create(S_ErrCommServerNotSpecified);

  HttpClient := TBillingXMLClient.Create(Set_ServerHostWork, dev_ESK, IntToStr(ObjectNum), CurrentAppVers);
  CmdSendMsg := TCmd_PCMessageClient(HttpClient.AddCommand(TCmd_PCMessageClient));
  //CmdSendMsg.ESKLoginData.AssignEskData(CurrentESK.ESKData);
  //CmdSendMsg.ESKLoginData.Pin := APin;
  CmdSendMsg.MessageType      := 'MSG';
  CmdSendMsg.MessageText.Add('updated');

  CmdSendMsg.ESKLoginData.EskSerial := IntToStr(ObjectNum);
  CmdSendMsg.ESKLoginData.EskCode := IntToStr(ObjectNum);
  CmdSendMsg.ESKLoginData.EskKey := IntToStr(ObjectNum);
  CmdSendMsg.ESKLoginData.EskVersion := IntToStr(ObjectNum);

  case HttpClient.SendCommand(0, hcmPost, true) of
  errcode_ExecuteFail_Internal:
    begin
      raise EAbort.Create(S_ErrCommunicationError+sLineBreak+HttpClient.LastError);
    end;

  errcode_ExecuteSucceed:
     begin
      AddMsg('Success.');
      PostLogToFile(LogPath, C_LogFileName, 'Update confirmation has been sent to server.');
     end;
  else
     begin
      AddMsg('Operation denied by server.');
      AddMsg('Error code: ' + IntToStr(HttpClient.LastCommandResultCode));
      AddMsg(HttpClient.LastCommandErrorMessage);

      raise EAbort.Create('Operation denied by server:'+sLineBreak+
                          'Error code: '+IntToStr(HttpClient.LastCommandResultCode)+sLineBreak+
                          HttpClient.LastCommandErrorMessage);
     end;
  end;
 except
  on E: Exception do
   begin
    PostLogToFile(LogPath, C_ErrorFileName, E.Message);
    AddMsg('Update confirmation failed:');
    AddMsg(E.Message);
   end;
 end;

 if HttpClient <> nil then HttpClient.Free;

end;

procedure TMainForm.FailedUpdate();
var HttpClient : TBillingXMLClient;
    CmdSendMsg : TCmd_PCMessageClient;
begin
 HttpClient := nil;

 try
  AddMsg('Send failed update message to server...');

  if Set_ServerHostWork = '' then raise EAbort.Create(S_ErrCommServerNotSpecified);

  HttpClient := TBillingXMLClient.Create(Set_ServerHostWork, dev_ESK, IntToStr(ObjectNum), CurrentAppVers);
  CmdSendMsg := TCmd_PCMessageClient(HttpClient.AddCommand(TCmd_PCMessageClient));
  //CmdSendMsg.ESKLoginData.AssignEskData(CurrentESK.ESKData);
  //CmdSendMsg.ESKLoginData.Pin := APin;
  CmdSendMsg.MessageType      := 'MSG';
  CmdSendMsg.MessageText.Add('failed');

  CmdSendMsg.ESKLoginData.EskSerial := IntToStr(ObjectNum);
  CmdSendMsg.ESKLoginData.EskCode := IntToStr(ObjectNum);
  CmdSendMsg.ESKLoginData.EskKey := IntToStr(ObjectNum);
  CmdSendMsg.ESKLoginData.EskVersion := IntToStr(ObjectNum);
  
  case HttpClient.SendCommand(0, hcmPost, true) of
  errcode_ExecuteFail_Internal:
    begin
      raise EAbort.Create(S_ErrCommunicationError+sLineBreak+HttpClient.LastError);
    end;

  errcode_ExecuteSucceed:
     begin
      AddMsg('Success!');
      PostLogToFile(LogPath, C_LogFileName, 'Failed update message has been sent to server');
     end;
  else
     begin
      AddMsg('Operation denied by server.');
      AddMsg('Error code: ' + IntToStr(HttpClient.LastCommandResultCode));
      AddMsg(HttpClient.LastCommandErrorMessage);

      raise EAbort.Create('Operation denied by server:'+sLineBreak+
                          'Error code: '+IntToStr(HttpClient.LastCommandResultCode)+sLineBreak+
                          HttpClient.LastCommandErrorMessage);
     end;
  end;
 except
  on E: Exception do
   begin
    PostLogToFile(LogPath, C_ErrorFileName, E.Message);
    AddMsg('Connection to server failed:');
    AddMsg(E.Message);
   end;
 end;
 if HttpClient <> nil then HttpClient.Free;
end;


procedure TMainForm.RunTheExternal();
var i, j, r: Integer;
var RunProgram, Path: String;
var TmpSections, TmpValues: TStringList;
begin

  try
    TmpSections := TStringList.Create;
    TmpValues := TStringList.Create;

    with TIniFile.Create(LocalPath+C_RunFileName) do
    try
      ReadSections(TmpSections);

      for i := 0 to TmpSections.Count -1 do
      begin
        TmpValues.Clear;
        ReadSection(TmpSections[i], TmpValues);

        for j := 0 to TmpValues.Count - 1 do
        begin
          try
            RunProgram := ReadString(TmpSections[i], TmpValues[j], '');
            Path := ExtractFilePath(RunProgram);
            r := ShellExecute(Handle, 'open', PCHar(RunProgram), nil, PCHar(Path), SW_SHOWNORMAL);

            if r < 33 then
            begin
              AddMsg('Failed to run external program ' + RunProgram + ' (code: ' + IntToStr(r) + ')');
              PostLogToFile(LogPath, C_ErrorFileName, 'Failed to run external program ' + RunProgram + ' (code: ' + IntToStr(r) + ')');
            end;

          except
            on E: Exception do
            begin
              AddMsg('Failed to run external program ' + RunProgram + ':');
              AddMsg(E.Message);
              PostLogToFile(LogPath, C_ErrorFileName, 'Failed to run external program ' + RunProgram + ': ' + E.Message);
            end;

          end; //try

        end; // for j

      end; // for i

    finally // (try ini file)
      Free;
    end;

  except
  on E: Exception do
    begin
      AddMsg('Error: ' + E.Message);
      PostLogToFile(LogPath, C_ErrorFileName, 'Failed to run external program ' + RunProgram + ': ' + E.Message);
    end;

  end; // firsat try

end;

procedure TMainForm.ClearLogs();
var i: Integer;
    Age : Integer;
    FilesList : TStringList;
begin
  FilesList := TStringList.Create;
  FindFiles(FilesList, LogPath, '*.*');

  for i := 0 to FilesList.Count-1 do
  begin
    Age := DaysBetween(Now, FileDateToDateTime(FileAge(FilesList[i])));

    if Age > LogDays then
    begin
      try
        DeleteFile(FilesList[i]);
      except
        on E: Exception do
        begin
          AddMsg('Unable to delete old log file ' + LogPath + FilesList[i] + ':');
          AddMsg(E.Message);
        end;
      end;

    end;

  end;

end;

procedure TMainForm.btnCheckAgainClick(Sender: TObject);
begin
  DoUpdate()
end;

procedure TMainForm.btnContinueClick(Sender: TObject);
begin
  RunTheExternal();
  Application.Terminate();
end;


procedure TMainForm.FormActivate(Sender: TObject);
begin
  DoUpdate();
end;

end.
