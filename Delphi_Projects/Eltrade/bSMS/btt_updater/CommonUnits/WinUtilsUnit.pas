unit WinUtilsUnit;

interface

uses SysUtils, Classes, Windows, IdIPWatch, Registry, Winsvc, PsAPI, TlHelp32, SHFolder;


function Win_GetHostName: String;
function Win_GetSystemDir: String;   
function Win_GetWindowsDir: String;
function Win_GetAppDataDir: String;
function Win_GetIPAdress: String;
function Win_GetCurrentUserName: String;
function Win_GetOSName: String;
function Win_GetOSVersion: String;
function Win_GetProductID: String;
function Win_GetOrganisation: String;
function Win_GetOwner: String;
function Win_GerRegistrySubKeys(KeyName: String): String;
function Win_ServiceStarted(ServiceName_: String): Boolean;
function Win_ServiceInstalled(ServiceName_: String): Boolean;
function Win_GetParentWindowClassName(Hnd: THandle): String;
function Win_FindServiceByFileName(FileName: String; var ServiceName: String): Boolean;
function Win_GetProcessNameFromWnd(Wnd: HWND): string;
function Win_IsParentWindowVisible(Hnd: THandle): Boolean;
function Win_LoadServiceList(SrvList: TStrings): Boolean;
function Win_GetAppVersion(FileName: String): String;

implementation

uses
  Dialogs;


const
  RsSystemIdleProcess = 'System Idle Process';
  RsSystemProcess     = 'System Process';

function Win_GetHostName: String;
var ChData  : PChar;
    DataLen : Cardinal;
begin
  Result  := '';
  DataLen := MAX_COMPUTERNAME_LENGTH + 1;
  GetMem(ChData, DataLen);
  try
   GetComputerName(ChData, DataLen);
   Result := StrPas(ChData);
  finally
   FreeMem(ChData);
  end;
end;

function Win_GetSystemDir: String;
var ChData  : PChar;
    DataLen : Cardinal;
begin
  Result  := '';
  DataLen := MAX_PATH + 1;
  GetMem(ChData, DataLen);
  try
    GetSystemDirectory(ChData, DataLen);
    Result := StrPas(ChData);
  except
  end;
  FreeMem(ChData);
end;

function Win_GetWindowsDir: String;
var ChData  : PChar;
    DataLen : Cardinal;
begin
  Result  := '';
  DataLen := MAX_PATH + 1;
  GetMem(ChData, DataLen);
  try
    GetWindowsDirectory(ChData, DataLen);
    Result := StrPas(ChData);
  except
  end;
  FreeMem(ChData);
end;

function GetSpecialFolderPath(folder : integer) : string;
const  SHGFP_TYPE_CURRENT = 0;
var    path: array [0..MAX_PATH] of char;
begin
 if SUCCEEDED(SHGetFolderPath(0,folder,0,SHGFP_TYPE_CURRENT,@path[0])) then
   Result := path
 else
   Result := '';
end;

function Win_GetAppDataDir: String;
begin
 Result := GetSpecialFolderPath(CSIDL_COMMON_APPDATA);
end;

function Win_GetCurrentUserName: String;
var ChData  : PChar;
    DataLen : Cardinal;
begin
  Result  := '';
  DataLen := 255;
  GetMem(ChData, DataLen);
  try
    GetUserName(ChData, DataLen);
    Result := StrPas(ChData);
  except
  end;
  FreeMem(ChData);
end;

function Win_GetIPAdress: String;
begin
with TIdIPWatch.Create(nil) do
 begin
  HistoryEnabled := false;
  try
   Result := LocalIP;
  finally
   Free;
  end;
 end;
end;

function ReadRegistryString(KeyName, Value: String): String;
begin
 with TRegistry.Create do
  begin
   try
    Access  := KEY_READ;
    RootKey := HKEY_LOCAL_MACHINE;
    if KeyExists(KeyName) then
     begin
      OpenKey(KeyName, False);
      if ValueExists(Value) then Result := ReadString(Value);
     end; 
   except
   end;
   Free;
  end;
end;

function ReadRegistryWinValue(Value: String): String;
begin
 Result := '';
 if Result = '' then Result := ReadRegistryString('\Software\Microsoft\Windows\CurrentVersion', Value);
 if Result = '' then Result := ReadRegistryString('\SOFTWARE\Microsoft\Windows NT\CurrentVersion', Value);
 if Result = '' then Result := ReadRegistryString('\SOFTWARE\Microsoft\WindowsNT\CurrentVersion', Value);
end;

function Win_GetOSName: String;
begin
 Result := ReadRegistryWinValue('ProductName');
end;

function Win_GetOSVersion: String;
begin
 Result := ReadRegistryWinValue('CurrentVersion')+'.'+
           ReadRegistryWinValue('CurrentBuildNumber')+' '+
           ReadRegistryWinValue('CSDVersion');
end;

function Win_GetProductID: String;
begin
 Result := ReadRegistryWinValue('ProductId');
end;

function Win_GetOrganisation: String;
begin
 Result := ReadRegistryWinValue('RegisteredOrganization');
end;

function Win_GetOwner: String;
begin
 Result := ReadRegistryWinValue('RegisteredOwner');
end;

function Win_GerRegistrySubKeys(KeyName: String): String;
var StrList : TStrings;
begin
 with TRegistry.Create do
  begin
   try
    Access  := KEY_READ;
    RootKey := HKEY_LOCAL_MACHINE;
    if KeyExists(KeyName) then
     begin
      OpenKey(KeyName, False);
      StrList := TStringList.Create;
      GetKeyNames(StrList);
      Result := StrList.Text;
      StrList.Free;
     end;
   except
   end;
   Free;
  end;
end;

function Win_ServiceStarted(ServiceName_: String): Boolean;
var SCOpen    : SC_HANDLE;
    SrvStat   : TServiceStatus;
    SCManager : SC_HANDLE;
begin
  SCManager := OpenSCManager(nil, nil, SC_MANAGER_ALL_ACCESS);
  if SCManager <> 0 then
   begin
    try
      SCOpen := OpenService(SCManager, PChar(ServiceName_), SERVICE_QUERY_STATUS);
      if SCOpen = 0 then
       begin
        Result := false;
       end
      else
       begin
        if QueryServiceStatus(SCOpen, SrvStat) then
         begin
          Result := SrvStat.dwCurrentState = SERVICE_RUNNING;
         end
        else
         result := false;
        CloseServiceHandle(SCOpen);
       end;
    finally
      CloseServiceHandle(SCManager);
    end;
   end
  else
   Result := false;
end;

function Win_ServiceInstalled(ServiceName_: String): Boolean;
var SCOpen    : SC_HANDLE;
    SCManager : SC_HANDLE;
begin
  SCManager := OpenSCManager(nil, nil, SC_MANAGER_ALL_ACCESS);
  if SCManager <> 0 then
   begin
    try
      SCOpen := OpenService(SCManager, PChar(ServiceName_), SERVICE_QUERY_STATUS);
      if SCOpen = 0 then
       begin
        Result := false;
       end
      else
       begin
        Result := true;
        CloseServiceHandle(SCOpen);
       end;
    finally
      CloseServiceHandle(SCManager);
    end;
   end
  else
   Result := false;
end;

function Win_GetParentWindowClassName(Hnd: THandle): String;
var ClassName : String;
begin
 while GetParent(Hnd) > 0 do Hnd := GetParent(Hnd);
 SetLength(ClassName, 255);
 SetLength(ClassName, GetClassName(Hnd, PChar(className), Length(className)));
 Result := ClassName;
end;

function Win_IsParentWindowVisible(Hnd: THandle): Boolean;
begin
 while GetParent(Hnd) > 0 do Hnd := GetParent(Hnd);
 Result := IsWindowVisible(Hnd);
end;

function Win_FindServiceByFileName(FileName: String; var ServiceName: String): Boolean;
var SCService    : SC_HANDLE;
    SCManager    : SC_HANDLE;
    EnumSrvStat  : array of TEnumServiceStatus;
    ServiceConfig: PQueryServiceConfig;
    sSize        : DWORD;
    sCount       : DWORD;
    sResumeHnd   : DWORD;
    I            : Integer;
begin
 Result      := false;
 ServiceName := '';
 SCManager := OpenSCManager(nil, nil, SC_MANAGER_ENUMERATE_SERVICE);
 if SCManager <> 0 then
  begin
   try
    sResumeHnd := 0; sSize := 0; sCount := 0;
    try
      EnumServicesStatus(SCManager, SERVICE_WIN32, SERVICE_STATE_ALL,
                         EnumSrvStat[0], 0, sSize, sCount, sResumeHnd);
      SetLength(EnumSrvStat, sSize div SizeOf(TEnumServiceStatus)+1);
      sResumeHnd := 0; sSize := 0; sCount := 0;
      EnumServicesStatus(SCManager, SERVICE_WIN32, SERVICE_STATE_ALL,
                         EnumSrvStat[0], Length(EnumSrvStat) * SizeOf(TEnumServiceStatus),
                         sSize, sCount, sResumeHnd);
      for I := 0 to sCount - 1 do
       begin
        SCService := OpenService(SCManager, EnumSrvStat[i].lpServiceName, SERVICE_QUERY_CONFIG);
        try
         if SCService > 0 then
          begin
           QueryServiceConfig(SCService, nil, 0, sSize); // Get Buffer Length
           GetMem(ServiceConfig, sSize + 1);
           try
            if QueryServiceConfig(SCService, ServiceConfig, sSize + 1, sSize) then // Get Buffer Length
             begin
              if Pos(UpperCase(FileName), UpperCase(StrPas(ServiceConfig.lpBinaryPathName))) > 0 then
               begin
                Result := true;
                ServiceName := StrPas(EnumSrvStat[i].lpServiceName);
                Exit;
               end;
             end;
           finally
            FreeMem(ServiceConfig);
           end;
          end;
        finally
         CloseServiceHandle(SCService)
        end;
       end;
    finally
     SetLength(EnumSrvStat, 0);
    end;
   finally
    CloseServiceHandle(SCManager);
   end;
  end;
end;

function Win_LoadServiceList(SrvList: TStrings): Boolean;
var SCService    : SC_HANDLE;
    SCManager    : SC_HANDLE;
    EnumSrvStat  : array of TEnumServiceStatus;
    ServiceConfig: PQueryServiceConfig;
    sSize        : DWORD;
    sCount       : DWORD;
    sResumeHnd   : DWORD;
    I            : Integer;
begin
 Result      := false;
 if SrvList = nil then exit;
 SCManager := OpenSCManager(nil, nil, SC_MANAGER_ENUMERATE_SERVICE);
 if SCManager <> 0 then
  begin
   try
    sResumeHnd := 0; sSize := 0; sCount := 0;
    try
      EnumServicesStatus(SCManager, SERVICE_WIN32, SERVICE_STATE_ALL,
                         EnumSrvStat[0], 0, sSize, sCount, sResumeHnd);
      SetLength(EnumSrvStat, sSize div SizeOf(TEnumServiceStatus)+1);
      sResumeHnd := 0; sSize := 0; sCount := 0;
      EnumServicesStatus(SCManager, SERVICE_WIN32, SERVICE_STATE_ALL,
                         EnumSrvStat[0], Length(EnumSrvStat) * SizeOf(TEnumServiceStatus),
                         sSize, sCount, sResumeHnd);
      for I := 0 to sCount - 1  do
       begin
        SrvList.Values[StrPas(EnumSrvStat[I].lpServiceName)] := '';
        SCService := OpenService(SCManager, EnumSrvStat[i].lpServiceName, SERVICE_QUERY_CONFIG);
        try
         if SCService > 0 then
          begin
           QueryServiceConfig(SCService, nil, 0, sSize); // Get Buffer Length
           GetMem(ServiceConfig, sSize + 1);
           try
            if QueryServiceConfig(SCService, ServiceConfig, sSize + 1, sSize) then // Get Buffer Length
             begin
              SrvList.Values[StrPas(EnumSrvStat[i].lpServiceName)] := StrPas(ServiceConfig.lpBinaryPathName);
              Result := true;
             end;
           finally
            FreeMem(ServiceConfig);
           end;
          end;
        finally
         CloseServiceHandle(SCService)
        end;
       end;
    finally
     SetLength(EnumSrvStat, 0);
    end;
   finally
    CloseServiceHandle(SCManager);
   end;
  end;
end;

//********************************************************************************
// Process functions
//********************************************************************************

function Win_GetProcessFileName(PID: DWORD; FullPath: Boolean): String;
var Handle: THandle;
begin
 Result := '';
 Handle := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, PID);
 if Handle <> 0 then
  try
    SetLength(Result, MAX_PATH);
    if FullPath then
    begin
      if GetModuleFileNameEx(Handle, 0, PChar(Result), MAX_PATH) > 0 then
        SetLength(Result, StrLen(PChar(Result)))
      else
        Result := '';
    end
    else
    begin
      if GetModuleBaseNameA(Handle, 0, PChar(Result), MAX_PATH) > 0 then
        SetLength(Result, StrLen(PChar(Result)))
      else
        Result := '';
    end;
  finally
    CloseHandle(Handle);
  end;
end;


function IsWinXP: Boolean; 
begin 
  Result := (Win32Platform = VER_PLATFORM_WIN32_NT) and 
    (Win32MajorVersion = 5) and (Win32MinorVersion = 1); 
end; 

function IsWin2k: Boolean; 
begin 
  Result := (Win32MajorVersion >= 5) and 
    (Win32Platform = VER_PLATFORM_WIN32_NT); 
end; 

function IsWinNT4: Boolean; 
begin 
  Result := Win32Platform = VER_PLATFORM_WIN32_NT; 
  Result := Result and (Win32MajorVersion = 4);
end;

function IsWin3X: Boolean;
begin
  Result := Win32Platform = VER_PLATFORM_WIN32_NT;
  Result := Result and (Win32MajorVersion = 3) and
    ((Win32MinorVersion = 1) or (Win32MinorVersion = 5) or
    (Win32MinorVersion = 51));
end;


function RunningProcessesList(const List: TStrings; FullPath: Boolean): Boolean;

function BuildListTH: Boolean;
  var
    SnapProcHandle: THandle;
    ProcEntry: TProcessEntry32;
    NextProc: Boolean;
    FileName: string;
  begin
    SnapProcHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
    Result := (SnapProcHandle <> INVALID_HANDLE_VALUE); 
    if Result then 
      try 
        ProcEntry.dwSize := SizeOf(ProcEntry); 
        NextProc := Process32First(SnapProcHandle, ProcEntry); 
        while NextProc do 
        begin 
          if ProcEntry.th32ProcessID = 0 then 
          begin 
            FileName := RsSystemIdleProcess; 
          end 
          else 
          begin 
            if IsWin2k or IsWinXP then 
            begin 
              FileName := Win_GetProcessFileName(ProcEntry.th32ProcessID, FullPath);
              if FileName = '' then
                FileName := ProcEntry.szExeFile;
            end 
            else 
            begin 
              FileName := ProcEntry.szExeFile; 
              if not FullPath then 
                FileName := ExtractFileName(FileName); 
            end; 
          end;
          List.AddObject(FileName, Pointer(ProcEntry.th32ProcessID));
          NextProc := Process32Next(SnapProcHandle, ProcEntry);
        end; 
      finally 
        CloseHandle(SnapProcHandle); 
      end; 
  end; 
  
function BuildListPS: Boolean; 
  var 
    PIDs: array [0..1024] of DWORD; 
    Needed: DWORD; 
    I: Integer; 
    FileName: string; 
  begin 
    Result := EnumProcesses(@PIDs, SizeOf(PIDs), Needed); 
    if Result then 
    begin 
      for I := 0 to (Needed div SizeOf(DWORD)) - 1 do 
      begin 
        case PIDs[I] of 
          0: 
            FileName := RsSystemIdleProcess; 
          2: 
            if IsWinNT4 then 
              FileName := RsSystemProcess 
            else 
              FileName := Win_GetProcessFileName(PIDs[I], FullPath);
          8:
            if IsWin2k or IsWinXP then
              FileName := RsSystemProcess
            else
              FileName := Win_GetProcessFileName(PIDs[I], FullPath);
          else
            FileName := Win_GetProcessFileName(PIDs[I], FullPath);
        end;
        if FileName <> '' then 
          List.AddObject(FileName, Pointer(PIDs[I]));
      end; 
    end; 
  end; 
begin 
  if IsWin3X or IsWinNT4 then
    Result := BuildListPS
  else 
    Result := BuildListTH; 
end; 

function Win_GetProcessNameFromWnd(Wnd: HWND): String;
var
  List: TStringList; 
  PID: DWORD; 
  I: Integer; 
begin 
  Result := ''; 
  if IsWindow(Wnd) then 
  begin 
    PID := INVALID_HANDLE_VALUE;
    GetWindowThreadProcessId(Wnd, @PID);
    List := TStringList.Create;
    try 
      if RunningProcessesList(List, True) then 
      begin
        I := List.IndexOfObject(Pointer(PID)); 
        if I > -1 then 
          Result := List[I]; 
      end; 
    finally 
      List.Free; 
    end; 
  end; 
end;

function Win_GetAppVersion(FileName: String): String;
const
  InfoNum = 10;
  InfoStr: array[1..InfoNum] of string = ('CompanyName', 'FileDescription',
                   'FileVersion', 'InternalName', 'LegalCopyright', 'LegalTradeMarks',
                   'OriginalFileName', 'ProductName', 'ProductVersion', 'Comments');

var N, Len : DWORD;
    Buf    : PChar;
    Value  : PChar;
    LangCharset : string;
    PCharset    : PLongInt;
    InfoLength  : UINT;
begin
 try
  Result := '';
  N := GetFileVersionInfoSize(PChar(FileName), N);
  if N > 0 then
   begin
    Buf := AllocMem(N);
    try
     GetFileVersionInfo(PChar(FileName), 0, N, Buf);
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

end.
