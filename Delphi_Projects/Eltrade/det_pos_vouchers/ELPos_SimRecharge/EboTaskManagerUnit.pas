unit EboTaskManagerUnit;

interface

uses Classes, SysUtils, Controls, SyncObjs, Windows, PsApi, ShellApi, Contnrs, Forms, Graphics, Messages;

type
  TEBO_Process = class(TObject)
  private
   FCheckTime   : TDateTime;
   FIsCurrentApp: Boolean;
   FFileName    : String;
   FIcon        : TIcon;
   FProcessHnd  : THandle;
   FProcessId   : DWORD;
   FModuleId    : Cardinal;
   FMainWndHnd  : THandle;

  public
   constructor Create;
   destructor Destroy; override;
   function Clone: TEBO_Process;

   procedure ActivateApp;
   procedure CloseApp;

   property IsCurrentApp: Boolean read FIsCurrentApp write FIsCurrentApp;
   property FileName: String read FFileName write FFileName;
   property Icon: TIcon read FIcon write FIcon;
  end;

  TEBO_TaskManager = class(TThread)
  private
   FWaitEvent : TEvent;
   FProcList  : TObjectList;
   FCrSection : TCriticalSection;
   FUpdateTime: Integer;
   FLastError : String;
   FCurrExeNme: String;
   FLoadCurr  : Boolean;
   FSmallIcons: Boolean;

   function FGetProcessCount: Integer;
   function FGetCurrentProcessIndex: Integer;
   function FGetProcess(Index: Integer): TEBO_Process;
   function FGetModIcon(ModuleName: String): TIcon;
//   function FFindProcessByExeName(ExeName: String; InList: TObjectList=nil): TEBO_Process;
   function FFindProcessByPID(ProcessId: Cardinal; InList: TObjectList=nil): TEBO_Process;
//   function FCalcProcessListPaths(InList: TObjectList): String;
//   function FGetModIconIndex(ModuleName: String): Integer;
  protected
    procedure Execute; override;
  public
    constructor Create(LoadCurrentProc: Boolean=false; Activate: Boolean=false; SmallIcons: Boolean=true);
    destructor Destroy; override;



    procedure ActivateNextApp;
    procedure ActivatePrevApp;

    procedure Start;
    procedure Stop;

    property UpdateTime: Integer read FUpdateTime write FUpdateTime;
    property LoadCurrentProcess: Boolean read FLoadCurr write FLoadCurr;
    property ProcessCount: Integer read FGetProcessCount;
    property Processes[Index: Integer]: TEBO_Process read FGetProcess;
    property CurrentProcessIndex: Integer read FGetCurrentProcessIndex;
  end;



implementation
uses TlHelp32;

const
 C_DetModExeNames : array[1..30] of String = (
              {1}   'ELPos_BOSOperations.exe',
              {2}   'ELPos_KbdIntf.exe',
              {3}   'ELPos_TouchIntf.exe',
              {4}   'CanteenOrders.exe',
              {5}   'ELPos_SimRecharge.exe',
              {6}   'ELPos_Lottery.exe',
              {7}   'ELPos_CashFlow.exe',
              {8}   'ELPos_Messaging.exe',
              {9}   '',
              {10}  '',
              {11}  '',
              {12}  'AdminClient.exe',
              {13}   'CashReport.exe',
              {14}  'Consignation.exe',
              {15}  'DebitCredit.exe',
              {16}  'Delivery.exe',
              {17}  'DeliveryQty.exe',
              {18}  'Disintegration.exe',
              {19}  'Inspection.exe',
              {20}  'Manufacture.exe',
              {21}  'Orders.exe',
              {22}  'RequestDelivery.exe',
              {23}  'SaleClient.exe',
              {24}  'Scrap.exe',
              {25}  'Transfer.exe',
              {26}  '',
              {27}  '',
              {28}  '',
              {29}  'EBOTaskSchedulerAdm.exe',
              {30}  'EBOConfigManager.exe');
{type
 TWindowInfo = record
//  FClassName : ShortString;
//  FTitle     : ShortString;
  FPid       : DWORD;
  FHandle    : THandle;
 end;
 TWidowsInfo = array of TWindowInfo;
}

var //WidowsInfo : TWidowsInfo;
    MainWndHnd : THandle;
//    MHnd       : THandle;

{
function EnumProcess(hHwnd: HWND; lParam : Integer): BOOL; stdcall;
var Pid_ : DWORD;
    Cls_ : String;
//    Ttl_ : String;
begin
 Result := false;
 if (hHwnd <> 0) then
  begin
    GetWindowThreadProcessId(hHwnd, Pid_);
    SetLength(Cls_, 255);
    SetLength(Cls_, GetClassName(hHwnd, PChar(Cls_), Length(Cls_)));
//    SetLength(Ttl_, 255);
//    SetLength(Ttl_, GetWindowText(hHwnd, PChar(Ttl_), Length(Ttl_)));
    if SameText('TApplication', Cls_) then
     begin
      SetLength(WidowsInfo, Length(WidowsInfo) + 1);
      with WidowsInfo[Length(WidowsInfo)-1] do
       begin
//        ClassName := Cls_;
//        Title     := Ttl_;
        FPid       := Pid_;
        FHandle    := hHwnd;
       end;
     end;
    Result := true;
    SetLength(Cls_, 0);
  end;
end;

function GetMainWindowHandle(ProcessId: Dword): THandle;
var I : Integer;
begin
 Result := 0;
 for I := 0 to Length(WidowsInfo) - 1 do
  begin
   if WidowsInfo[I].FPid = ProcessId then
    begin
     Result := WidowsInfo[I].FHandle;
     while GetWindow(Result, GW_OWNER) <> 0 do Result := GetWindow(Result, GW_OWNER);
     Break;
    end;
  end;
end;

}
//**************************************************************************************************
//    TEBO_Process
//**************************************************************************************************
constructor TEBO_Process.Create;
begin
  inherited Create;
  Icon := nil;
end;

destructor TEBO_Process.Destroy;
begin
  if Icon <> nil then Icon.Free;
  inherited Destroy;
end;

procedure TEBO_Process.ActivateApp;
begin
 if FMainWndHnd = 0 then Exit;
 if FIsCurrentApp then Exit;

 if (IsIconic(FMainWndHnd)) then
   ShowWindow(FMainWndHnd, SW_RESTORE)
 else
   BringWindowToTop(FMainWndHnd);
 SetForegroundWindow(FMainWndHnd);
// SendMessage(FMainWndHnd, WM_STYLECHANGED, 0, 0);
end;

procedure TEBO_Process.CloseApp;
begin

end;

function TEBO_Process.Clone: TEBO_Process;
begin
 Result := TEBO_Process.Create;
 Result.FCheckTime   := FCheckTime;
 Result.FIsCurrentApp:= FIsCurrentApp;
 Result.FFileName    := FFileName;
 Result.FProcessHnd  := FProcessHnd;
 Result.FProcessId   := FProcessId;
 Result.FMainWndHnd  := FMainWndHnd;

 if FIcon <> nil then
  begin
   Result.FIcon := TIcon.Create;
   Result.FIcon.Assign(FIcon);
  end; 
end;

//**************************************************************************************************
//    TEBO_TaskManager
//**************************************************************************************************
constructor TEBO_TaskManager.Create(LoadCurrentProc: Boolean=false; Activate: Boolean=false; SmallIcons: Boolean=true);
begin
 inherited Create(true);
 FUpdateTime := 5000;
 FLoadCurr   := LoadCurrentProc;
 FSmallIcons := SmallIcons;
 FCurrExeNme := ExtractFileName(Application.ExeName);

 FWaitEvent  := TEvent.Create(nil, true, false, '');
 FProcList   := TObjectList.Create(true);
 FCrSection  := TCriticalSection.Create;
 if Activate then Resume;
end;

destructor TEBO_TaskManager.Destroy;
begin
 if Suspended then Resume;
 if not Terminated then Terminate;
 FWaitEvent.SetEvent;
 WaitFor;

// SetLength(WidowsInfo, 0);
 FWaitEvent.Free;

 FProcList.Clear;
 FProcList.Free;

 FCrSection.Free;

 inherited Destroy;
end;

procedure TEBO_TaskManager.Start;
begin
 if Suspended then Resume;
end;

procedure TEBO_TaskManager.Stop;
begin
 Suspend;
end;

function TEBO_TaskManager.FGetModIcon(ModuleName: String): TIcon;
var H1, H2: HICON;
begin
 Result := nil;
 // Get the number of Icons
 if ExtractIcon(Application.Handle, PChar(ModuleName), UINT(-1)) > 0 then
  begin
   if ExtractIconEx(PChar(ModuleName), 0, H1, H2, 1) > 1 then
    begin
     Result := TIcon.Create;
     if FSmallIcons then Result.Handle := H2
      else Result.Handle := H1;
    end;
  end;
end;

{function TEBO_TaskManager.FGetModIconIndex(ModuleName: String): Integer;
var H1, H2: HICON;
    Ico : TIcon;
begin
 Result := -1;
 // Get the number of Icons
 if ExtractIcon(Handle, PChar(ModuleName), UINT(-1)) > 0 then
  begin
   ExtractIconEx(PChar(ModuleName), 0, H1, H2, 1);
   Ico := TIcon.Create;
   try
    Ico.Handle := H2;
    Result := FImgList.AddIcon(Ico);
   finally
    Ico.Free;
   end;
  end;
end;}

procedure TEBO_TaskManager.Execute;
var //PIDArray : array [0..1023] of DWORD;
    cb       : DWORD;
    hProcess : THandle;
    hMod     : HMODULE;
    CharArr  : array [0..300] of Char;
//    ModName  : String;
    ExeFname : String;
    I, J     : Integer;
    EboP     : TEBO_Process;
    ChkT     : TDateTime;
    TmpList  : TObjectList;
    ChangeF  : Boolean;


    ShHandle : THandle;
    ProcEntry: TProcessEntry32;


    function EnumCallback(hHwnd: HWND; lParam : Integer): BOOL; stdcall;
    var Pid_ : DWORD;
        Cls_ : String;
    begin
     Result := (hHwnd <> 0);
     if Result then
      begin
       GetWindowThreadProcessId(hHwnd, Pid_);
       if Pid_ = Round(lParam) then
        begin
         SetLength(Cls_, 255);
         SetLength(Cls_, GetClassName(hHwnd, PChar(Cls_), Length(Cls_)));
         if SameText('TApplication', Cls_) then
          begin
           MainWndHnd := hHwnd;
           Result     := false;
          end; 
        end;
      end;
    end;

begin
 TmpList := TObjectList.Create;
 ChangeF := false;
 try
  repeat
    ChkT:= Now;

{    SetLength(WidowsInfo, 0);
    EnumWindows(@EnumProcess, 0);

    EnumProcesses(@PIDArray, SizeOf(PIDArray), cb);
    for I := 0 to (cb div SizeOf(DWORD)) - 1 do
     begin
      hProcess := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, PIDArray[I]);
      if (hProcess <> 0) then
      try
        EnumProcessModules(hProcess, @hMod, SizeOf(hMod), cb);
        GetModuleFilenameEx(hProcess, hMod, CharArr, SizeOf(CharArr));
        ModName  := StrPas(CharArr);
        ExeFname := ExtractFileName(ModName);

        if not FileExists(ModName) then Continue;
        if (SameText(ExeFname, FCurrExeNme))and(not LoadCurrentProcess) then Continue;
        for J := 1 to Length(C_DetModExeNames) do
         begin
          if SameText(C_DetModExeNames[J], ExeFname) then
           begin
            // имаме активен Детелински процес
             EboP := FFindProcessByExeName(ModName, TmpList);
             if EboP = nil then
              begin
               EboP := TEBO_Process.Create;
               EboP.IsCurrentApp := SameText(ExeFname, FCurrExeNme);
               EboP.FileName     := ModName;
               EboP.Icon         := FGetModIcon(ModName);

               TmpList.Add(EboP);
              end;

             EboP.FProcessHnd  := hProcess;
             EboP.FProcessId   := PIDArray[I];
             EboP.FMainWndHnd  := GetMainWindowHandle(PIDArray[I]);
             EboP.FCheckTime   := ChkT;
           end;
         end;
      finally
       CloseHandle(hProcess);
      end;
     end;}

    ShHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
    try
      ProcEntry.dwSize := SizeOf(ProcEntry);
      if Process32First(ShHandle, ProcEntry) then
       repeat
        ExeFname := ExtractFileName(ProcEntry.szExeFile);

        for J := 1 to Length(C_DetModExeNames) do
         begin
          if SameText(C_DetModExeNames[J], ExeFname) then
           begin
            // имаме активен Детелински процес
             EboP := FFindProcessByPID(ProcEntry.th32ProcessID, TmpList);
             if EboP = nil then
              begin
               EboP := TEBO_Process.Create;
               TmpList.Add(EboP);
               ChangeF := true;

               EboP.IsCurrentApp := SameText(ExeFname, FCurrExeNme);
               EboP.FProcessId   := ProcEntry.th32ProcessID;

               GetModuleFileName(ProcEntry.th32ModuleID, CharArr, SizeOf(CharArr));
               EboP.FileName     := StrPas(CharArr);

               // трябва да изчетем пълния път до EXE файла за да вземем иконата
               hProcess := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, ProcEntry.th32ProcessID);
               if (hProcess <> 0) then
               try
                 EnumProcessModules(hProcess, @hMod, SizeOf(hMod), cb);
                 GetModuleFilenameEx(hProcess, hMod, CharArr, SizeOf(CharArr));
                 EboP.FModuleId    := hMod;
                 EboP.FProcessHnd  := hProcess;
                 EboP.FileName     := StrPas(CharArr);
               finally
                CloseHandle(hProcess);
               end;

               // извличане на иконата
               if FileExists(EboP.FileName) then EboP.Icon := FGetModIcon(EboP.FileName);

               // извличане на главния преозорец
               MainWndHnd := 0;
               EnumWindows(@EnumCallback, EboP.FProcessId);
               if MainWndHnd <> 0 then  while GetWindow(MainWndHnd, GW_OWNER) <> 0 do MainWndHnd := GetWindow(MainWndHnd, GW_OWNER);
               EboP.FMainWndHnd := MainWndHnd;
              end;

             EboP.FCheckTime := ChkT;
           end;
         end;
       until (not Process32Next(ShHandle, ProcEntry));
    finally
     CloseHandle(ShHandle);
    end;

    // премахване на спрените процеси от списъка
    I := 0;
    while I < TmpList.Count do
     begin
      if TEBO_Process(TmpList.Items[I]).FCheckTime <> ChkT then
       begin
        TmpList.Delete(I);
        ChangeF := true;
       end
      else
       Inc(I);
     end;

    // Имаме промяна във временния списък
    // прехвърляме я в официалния...
    // Ползваме официален и временен списък защото при репаинт на статус бара има конфликт на нишките и програмата замръзва...
    if ChangeF then
     begin
      ChangeF := false;
      FCrSection.Acquire;
      try
       FProcList.Clear;
       for I := 0 to TmpList.Count - 1 do
        begin
         if (TEBO_Process(TmpList.Items[I]).IsCurrentApp)and
            ((not LoadCurrentProcess)or(TmpList.Count = 1)) then Continue;
         FProcList.Add(TEBO_Process(TmpList.Items[I]).Clone);
        end;
      finally
       FCrSection.Release;
      end;
     end;

    FWaitEvent.WaitFor(FUpdateTime);
  until Terminated;
 except
  on E: Exception do
   begin
    FLastError := E.Message;
   end;
 end;
 TmpList.Free;
 Terminate;
end;

function TEBO_TaskManager.FGetProcessCount: Integer;
begin
 Result := FProcList.Count;
end;

function TEBO_TaskManager.FGetCurrentProcessIndex: Integer;
var I : Integer;
begin
 Result := -1;
 for I := 0 to FProcList.Count - 1 do
  begin
   if TEBO_Process(FProcList.Items[I]).IsCurrentApp then
    begin
     Result := I;
     Break;
    end;
  end;
end;

function TEBO_TaskManager.FGetProcess(Index: Integer): TEBO_Process;
begin
 Result := nil;
 if (Index >= 0)and(Index < FProcList.Count) then Result := TEBO_Process(FProcList.Items[Index]);
end;

{function TEBO_TaskManager.FFindProcessByExeName(ExeName: String; InList: TObjectList=nil): TEBO_Process;
var I : Integer;
begin
 Result := nil;
 if InList = nil then InList := FProcList;
 for I := 0 to InList.Count - 1 do
  begin
   if SameText(TEBO_Process(InList.Items[I]).FileName, ExeName) then
    begin
     Result := TEBO_Process(InList.Items[I]);
     Break;
    end;
  end;
end;}

function TEBO_TaskManager.FFindProcessByPID(ProcessId: Cardinal; InList: TObjectList=nil): TEBO_Process;
var I : Integer;
begin
 Result := nil;
 if InList = nil then InList := FProcList;
 for I := 0 to InList.Count - 1 do
  begin
   if TEBO_Process(InList.Items[I]).FProcessId = ProcessId then
    begin
     Result := TEBO_Process(InList.Items[I]);
     Break;
    end;
  end;
end;

{function TEBO_TaskManager.FCalcProcessListPaths(InList: TObjectList): String;
var I : Integer;
begin
 Result := '';
 for I := 0 to InList.Count - 1 do
  Result := Result + TEBO_Process(InList.Items[I]).FileName + ';';
end;}

procedure TEBO_TaskManager.ActivateNextApp;
var I, J : Integer;
begin
 for I := 0 to FProcList.Count - 1 do
  begin
   if TEBO_Process(FProcList.Items[I]).IsCurrentApp then
    begin
     J := I + 1;
     if J >= FProcList.Count then J := 0;
     if J <> I then TEBO_Process(FProcList.Items[J]).ActivateApp;
     Break;
    end;
  end;
end;

procedure TEBO_TaskManager.ActivatePrevApp;
var I, J : Integer;
begin
 for I := 0 to FProcList.Count - 1 do
  begin
   if TEBO_Process(FProcList.Items[I]).IsCurrentApp then
    begin
     J := I - 1;
     if J < 0 then J := FProcList.Count - 1;
     if J <> I then TEBO_Process(FProcList.Items[J]).ActivateApp;
     Break;
    end;
  end;
end;

initialization
// SetLength(WidowsInfo, 0);

finalization
// SetLength(WidowsInfo, 0);

end.
