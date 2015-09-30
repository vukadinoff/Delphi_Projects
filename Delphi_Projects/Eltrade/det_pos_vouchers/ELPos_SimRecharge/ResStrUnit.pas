unit ResStrUnit;

interface

uses SysUtils, IniFiles, Classes, Forms, StdCtrls, ExtCtrls, RXCtrls, Buttons;

const C_ExcludeComponentList = 'VersionLabel;';

var
 S_CurrentModulName  : String = '����� ��������� �� ��� ����� (tscrn)';
 S_Version           : String = '������:';
 S_SettingsNotLoaded : String = '������� ����������������� �������!'+
                                ''+sLineBreak+
                                '�������� � ������ �� ���� ����������.';
 S_InvalidRegistr    : String = '������ ������� ����������� �� ������.'+sLineBreak+
                                '�������� � ���� �� ���� ����������.';
 S_DBErrorConnect    : String = '���������� ����������� � ������ �����.'+#13+
                                '�� �� ��������� �������� ���������.'+#13+
                                '����, ����������� ��.';
 S_SeeErrForDetails  : String = '�� ����������� ���������� ����� � ��������.';
 S_UserNameNotFound  : String = '�� � ������� ��������';
 // registration
 S_InvalidRegKey     : String = '��������� �������������� ����.';
 S_InvalidStorageNumb: String = '������������� � ��������� !'+sLineBreak+
                                '��������� ������� �����.';
 S_StorageStr1       : String = '������� ����� ������ ����� No:';
 S_StorageStr2       : String = '������� �� �� ��������� ������� ������� �����';
 S_StorageStr3       : String = '�� ����� No:';
 S_StorageStr4       : String = '�� ����� No:';
 S_StorageStr5       : String = '������� ����� �:';
 S_StorageStr6       : String = '����� No:';


 S_ShutDown          : String = '������ ���������';
 S_ExitOs            : String = '�����';
 S_InvalidPassword   : String = '�������� �������!'+sLineBreak+
                                '��������� ������.';
 S_SessionStarted    : String = '��������� �����';
 S_LoginFailed       : String = '������ ��� login �� ��������';
 S_LogoutFailed      : String = '������ ��� logout �� ��������';
 S_ModuleStart       : String = '����� �� ������.';
 S_ModuleStop        : String = '���� �� ������.';
 S_ModuleLoginFail   : String = '������ ��� login/logout �� ������:';
 S_CustomerNotSelect : String = '�� ��� ������� ������.';
 S_Required          : String = '������� ��:';
 S_Invalid           : String = '���������';
 S_InvalidPrc        : String = '���������';
 S_ConfirmReq        : String = '������� �� �� ���������� ��������?';
 S_ContragentInserted: String = '�������� �� ����������:';
 S_DuplicateBulstat  : String = '���������� ������ � ����� �������.';
 S_ErrInsContragent  : String = '������ ��� �������� �� ������.';
 S_ContrNotFoundCnf  : String = '�� �� ������� �����, ����������'+sLineBreak+
                                '�� ���������� �������.'+sLineBreak+
                                '������� �� �� �������� ������� ���� ���?';
 S_ContrManyFound    : String = '������� �� ��� 50 �������'+sLineBreak+
                                '���������� �� ���������� �������.'+sLineBreak+
                                '����, �������� �� ����� �����.'+sLineBreak+
                                '���������� �� ������.';
 S_NegativeForbidden : String = '�� ���� �� ���� � ����������� ��������.';
 S_FailPrintDocument : String = '������ ��� ����� �� ��������.';
 // Database
 S_Server            : String = '������';
 S_Local             : String = '�������';
 S_NoConnection      : String = '���� ������';
 S_PleaseWait        : String = '����, ���������';
 S_Bulstat           : String = '��� No';
 S_VATNo             : String = '���� No';
 // CyrrToStr
 S_Cm_100               : String= '���';
 S_Cm_200               : String= '������';
 S_Cm_300               : String= '������';
 S_Cm_400               : String= '������������';
 S_Cm_500               : String= '���������';
 S_Cm_600               : String= '����������';
 S_Cm_700               : String= '�����������';
 S_Cm_800               : String= '����������';
 S_Cm_900               : String= '�����������';
 S_Cm_10                : String= '�����';
 S_Cm_20                : String= '��������';
 S_Cm_30                : String= '��������';
 S_Cm_40                : String= '�����������';
 S_Cm_50                : String= '��������';
 S_Cm_60                : String= '���������';
 S_Cm_70                : String= '����������';
 S_Cm_80                : String= '���������';
 S_Cm_90                : String= '���������';
 S_Cm_11                : String= '����������';
 S_Cm_12                : String= '����������';
 S_Cm_13                : String= '����������';
 S_Cm_14                : String= '�������������';
 S_Cm_15                : String= '����������';
 S_Cm_16                : String= '�����������';
 S_Cm_17                : String= '������������';
 S_Cm_18                : String= '�����������';
 S_Cm_19                : String= '������������';
 S_Cm_3                 : String= '���';
 S_Cm_4                 : String= '������';
 S_Cm_5                 : String= '���';
 S_Cm_6                 : String= '����';
 S_Cm_7                 : String= '�����';
 S_Cm_8                 : String= '����';
 S_Cm_9                 : String= '�����';
 S_Cm_1_1               : String= '����';
 S_Cm_1_2               : String= '����';
 S_Cm_1_3               : String= '����';
 S_Cm_1_4               : String= '����';
 S_Cm_2_1               : String= '��e';
 S_Cm_2_2               : String= '��a';
 S_Cm_2_3               : String= '��e';
 S_Cm_2_4               : String= '��a';
 S_And                  : String= '�';
 S_Stotinka             : String= '��������';
 S_Stotinki             : String= '��������';
 S_AThousand            : String= '������';
 S_ThousandS            : String= '������';
 S_AMillion             : String= '������';
 S_Millions             : String= '�������';
 S_Levs                 : String= '����';
 S_ALev                 : String= '���';
 S_StotinkiShort        : String= '��.';
 S_BtnYes               : String = '&��';
 S_BtnNo                : String = '&��';
 S_BtnOK                : String = '&��������';
 S_BtnCancel            : String = '&�����';
 S_BtnAbort             : String = '&��������';
 S_BtnRetry             : String = '&������';
 S_BtnIgnore            : String = '&���������';

 S_AccessDenied         : String = '�������� �� ���������� �������!';
 S_NotSelMobOper        : string = '�� ��� ������� ������� ��������.';
 S_NotSelPayType        : string = '�� ��� ������� ��� �� �������.';
 S_NotSelDocType        : string = '�� ��� ������� �������� �� �����.';
 S_ConnectingToMobOpr   : String = '��������� � �������� ��������.';
 S_RechargeSim          : String = '��������� �� ���������� �����.';
 S_MTel                 : string = '��������';
 S_Globul               : string = '������';
 S_Vivacom              : string = '�������';
 S_SuccSaveSale         : string = '������� �������� SIM �����.';
 
 S_UnsuccSaveSale       : string = '��������� ����� �� ����������.';
 S_ErrGetOperData       : string = '������ ��� ��������� ����� �� ���������';
 S_ErrGetSysNomecl      : string = '������ ��� ��������� �������� ������������.';
 S_MissingMobOperPlu    : string = '���� �������� ������� �� ������� ��������';
 S_PluNotFound          : string = '� �� �� � ������� ������� No';
 S_Ammount              : String = '����:';
 S_Phone                : String = '���e��� N:';
 S_ErrSearchPlu : string = '������ ��� ������� �� ������� � ������';
 S_UnsuccSearchPlu : string = '�������� ������� �� ������� � ������.';
 S_PhoneMissing : string = '�� ��� ������ ��������� �����.';
 S_InvalidPhone : string = '��������� ��������� �����.';
 S_SumMissing : string = '�� ��� ������ ���� �� ���������.';
 S_InvalidSumEntNumber : string = '��������� ���� �� ���������.'+sLineBreak+'�������� ����������� �����.';
 S_SumIsBiggerThanLimit : string = '������ �� ��������� ���������'+sLineBreak+'������ �� ���������� �� ��������.';
 S_SumIsSmallerThanLimit : string = '������ � ��� �������� �� ���������'+sLineBreak+'�� �������� ��������.';
 S_SumIsBiggerThanMOLimit : string = '������ ��������� ��������� �� ���������'+sLineBreak+'�� �������� ��������.';
 S_SumIsBiggerThanDayLimit : string = '������ �� ��������� ���������'+sLineBreak+'������� ����� �� ��������.';
 S_PassedDayLimit : string = '�������� ������ �����.';
 S_LimitIsSmallerThanMinMO : string = '���������� ������ ����� � ��� �������� �� ��������� �� �������� ��������';
 S_SumFrom : string = '���� �� ��������� ��';
 S_To : string = '��';
 S_SumAbove : string = '���� �� ��������� ���';
 S_NoLimit : string = '��� ����������� � ������ �� ���������';
 S_SumTo : string = '���� �� ��������� ��';
 S_ErrCalcTrunover : string = '������ ��� ����������� �� ������� ������ �� ��������';
 S_ErrCreatePrnFile: string = '������ ��� ���������� �� ����������� �������.';
 S_ErrCreateFiscPrnFile: string = '������ ��� ����� �� �������.';
 S_ErrGetCustData: string = '������ ��� ��������� ����� �� ������.';
 S_ErrCreateReport: string = '������ ��� ����� � ��������.';

 S_MobOprTestMode       : String = '������ ����� �� ������'+sLineBreak+
                                   '��� ����������� � ������� ��������!';
 S_ErrPayPrePaid        : String = '���������� �������� �� �������� ��������!';
 S_ErrConnectSystem     : String = '���������� ������ ��� ���� �� ���������'+sLineBreak+
                                   '� �������� ��������.'+sLineBreak+
                                   '���� ��������� �������������� �� ���������.'+sLineBreak+
                                   ''+sLineBreak+
                                   '�� ����������� ���������� ���� �� ����������.';
 S_ErrConnectServer     : String = '��������� ��������� � �������� ��������!';
 S_ErrCheckPrePaid      : String = '���������� �������� �� �������� ��������!';
 S_ErrOnSavingCurrInvN  : String = '������ ��� ����� �� ������� ����� �������!';

function LoadResStrings(Fname_: String): Boolean;
function LoadControlsStrings(Fname_: String; Form_: TForm): Boolean;

implementation

function ReplaceCmdString(Str_: String): String;
var I : Integer;
begin
 Result := '';
 for I := 1 to Length(Str_) do
  begin
   case Str_[I] of
   #13 : Result := Result + '|';
   #10 : Result := Result + '';
   ' ' : Result := Result + '_';
   else Result := Result + Str_[I];
   end;
  end;
end;

function ReplaceStringCmd(Str_: String): String;
var I : Integer;
begin
 Result := '';
 for I := 1 to Length(Str_) do
  begin
   case Str_[I] of
   '|' : Result := Result + #13;
   '_' : Result := Result + ' ';
   else Result := Result + Str_[I];
   end;
  end;
end;

procedure GetValueFromIni(IniFile_: TIniFile; Section_, Key_: String; var Value: String);
var S : String;
begin
 S := ReplaceStringCmd(IniFile_.ReadString(Section_, Key_, ''));
 if S <> '' then Value := S
  else IniFile_.WriteString(Section_, Key_, ReplaceCmdString(Value));
end;

function LoadResStrings(Fname_: String): Boolean;
var IniFile : TIniFile;
begin
 Result := false;
 if FileExists(Fname_) then
  begin
   IniFile := TIniFile.Create(Fname_);
   try
     GetValueFromIni(IniFile, 'MessagesCmn', 'Version',            S_Version);
     GetValueFromIni(IniFile, 'MessagesCmn', 'SettingsNotLoaded',  S_SettingsNotLoaded);
     GetValueFromIni(IniFile, 'MessagesCyrrToStr', 'S_Cm_100',                  S_Cm_100);
     GetValueFromIni(IniFile, 'MessagesCyrrToStr', 'S_Cm_200',                  S_Cm_200);
     GetValueFromIni(IniFile, 'MessagesCyrrToStr', 'S_Cm_300',                  S_Cm_300);
     GetValueFromIni(IniFile, 'MessagesCyrrToStr', 'S_Cm_400',                  S_Cm_400);
     GetValueFromIni(IniFile, 'MessagesCyrrToStr', 'S_Cm_500',                  S_Cm_500);
     GetValueFromIni(IniFile, 'MessagesCyrrToStr', 'S_Cm_600',                  S_Cm_600);
     GetValueFromIni(IniFile, 'MessagesCyrrToStr', 'S_Cm_700',                  S_Cm_700);
     GetValueFromIni(IniFile, 'MessagesCyrrToStr', 'S_Cm_800',                  S_Cm_800);
     GetValueFromIni(IniFile, 'MessagesCyrrToStr', 'S_Cm_900',                  S_Cm_900);
     GetValueFromIni(IniFile, 'MessagesCyrrToStr', 'S_Cm_10',                   S_Cm_10);
     GetValueFromIni(IniFile, 'MessagesCyrrToStr', 'S_Cm_20',                   S_Cm_20);
     GetValueFromIni(IniFile, 'MessagesCyrrToStr', 'S_Cm_30',                   S_Cm_30);
     GetValueFromIni(IniFile, 'MessagesCyrrToStr', 'S_Cm_40',                   S_Cm_40);
     GetValueFromIni(IniFile, 'MessagesCyrrToStr', 'S_Cm_50',                   S_Cm_50);
     GetValueFromIni(IniFile, 'MessagesCyrrToStr', 'S_Cm_60',                   S_Cm_60);
     GetValueFromIni(IniFile, 'MessagesCyrrToStr', 'S_Cm_70',                   S_Cm_70);
     GetValueFromIni(IniFile, 'MessagesCyrrToStr', 'S_Cm_80',                   S_Cm_80);
     GetValueFromIni(IniFile, 'MessagesCyrrToStr', 'S_Cm_90',                   S_Cm_90);
     GetValueFromIni(IniFile, 'MessagesCyrrToStr', 'S_Cm_11',                   S_Cm_11);
     GetValueFromIni(IniFile, 'MessagesCyrrToStr', 'S_Cm_12',                   S_Cm_12);
     GetValueFromIni(IniFile, 'MessagesCyrrToStr', 'S_Cm_13',                   S_Cm_13);
     GetValueFromIni(IniFile, 'MessagesCyrrToStr', 'S_Cm_14',                   S_Cm_14);
     GetValueFromIni(IniFile, 'MessagesCyrrToStr', 'S_Cm_15',                   S_Cm_15);
     GetValueFromIni(IniFile, 'MessagesCyrrToStr', 'S_Cm_16',                   S_Cm_16);
     GetValueFromIni(IniFile, 'MessagesCyrrToStr', 'S_Cm_17',                   S_Cm_17);
     GetValueFromIni(IniFile, 'MessagesCyrrToStr', 'S_Cm_18',                   S_Cm_18);
     GetValueFromIni(IniFile, 'MessagesCyrrToStr', 'S_Cm_19',                   S_Cm_19);
     GetValueFromIni(IniFile, 'MessagesCyrrToStr', 'S_Cm_3',                    S_Cm_3);
     GetValueFromIni(IniFile, 'MessagesCyrrToStr', 'S_Cm_4',                    S_Cm_4);
     GetValueFromIni(IniFile, 'MessagesCyrrToStr', 'S_Cm_5',                    S_Cm_5);
     GetValueFromIni(IniFile, 'MessagesCyrrToStr', 'S_Cm_6',                    S_Cm_6);
     GetValueFromIni(IniFile, 'MessagesCyrrToStr', 'S_Cm_7',                    S_Cm_7);
     GetValueFromIni(IniFile, 'MessagesCyrrToStr', 'S_Cm_8',                    S_Cm_8);
     GetValueFromIni(IniFile, 'MessagesCyrrToStr', 'S_Cm_9',                    S_Cm_9);
     GetValueFromIni(IniFile, 'MessagesCyrrToStr', 'S_Cm_1_1',                  S_Cm_1_1);
     GetValueFromIni(IniFile, 'MessagesCyrrToStr', 'S_Cm_1_2',                  S_Cm_1_2);
     GetValueFromIni(IniFile, 'MessagesCyrrToStr', 'S_Cm_1_3',                  S_Cm_1_3);
     GetValueFromIni(IniFile, 'MessagesCyrrToStr', 'S_Cm_1_4',                  S_Cm_1_4);
     GetValueFromIni(IniFile, 'MessagesCyrrToStr', 'S_Cm_2_1',                  S_Cm_2_1);
     GetValueFromIni(IniFile, 'MessagesCyrrToStr', 'S_Cm_2_2',                  S_Cm_2_2);
     GetValueFromIni(IniFile, 'MessagesCyrrToStr', 'S_Cm_2_3',                  S_Cm_2_3);
     GetValueFromIni(IniFile, 'MessagesCyrrToStr', 'S_Cm_2_4',                  S_Cm_2_4);
     GetValueFromIni(IniFile, 'MessagesCyrrToStr', 'S_And',                     S_And);
     GetValueFromIni(IniFile, 'MessagesCyrrToStr', 'S_Stotinka',                S_Stotinka);
     GetValueFromIni(IniFile, 'MessagesCyrrToStr', 'S_Stotinki',                S_Stotinki);
     GetValueFromIni(IniFile, 'MessagesCyrrToStr', 'S_AThousand',               S_AThousand);
     GetValueFromIni(IniFile, 'MessagesCyrrToStr', 'S_ThousandS',               S_ThousandS);
     GetValueFromIni(IniFile, 'MessagesCyrrToStr', 'S_AMillion',                S_AMillion);
     GetValueFromIni(IniFile, 'MessagesCyrrToStr', 'S_Millions',                S_Millions);
     GetValueFromIni(IniFile, 'MessagesCyrrToStr', 'S_Levs',                    S_Levs);
     GetValueFromIni(IniFile, 'MessagesCyrrToStr', 'S_ALev',                    S_ALev);
     GetValueFromIni(IniFile, 'MessagesCyrrToStr', 'S_StotinkiShort',           S_StotinkiShort);
   finally
    IniFile.Free;
   end;
   Result := true;
  end;
end;

procedure UpdateComponentFromIni(IniFile_: TIniFile; Section_: String; Comp_: TComponent);
var S : String;
    I : Integer;
begin
 if Pos(Comp_.Name+';', C_ExcludeComponentList) > 0 then Exit;
 if Comp_ is TForm then
  begin
   S := ReplaceStringCmd(IniFile_.ReadString(Section_, Comp_.Name, ''));
   if S <> '' then TForm(Comp_).Caption := S
    else IniFile_.WriteString(Section_, Comp_.Name, ReplaceCmdString(TForm(Comp_).Caption));
  end
 else
 if Comp_ is TLabel then
  begin
   S := ReplaceStringCmd(IniFile_.ReadString(Section_, Comp_.Name, ''));
   if S <> '' then TLabel(Comp_).Caption := S
    else IniFile_.WriteString(Section_, Comp_.Name, ReplaceCmdString(TLabel(Comp_).Caption));
  end
 else
 if Comp_ is TRxLabel then
  begin
   S := ReplaceStringCmd(IniFile_.ReadString(Section_, Comp_.Name, ''));
   if S <> '' then TLabel(Comp_).Caption := S
    else IniFile_.WriteString(Section_, Comp_.Name, ReplaceCmdString(TLabel(Comp_).Caption));
  end
 else
 if Comp_ is TGroupBox then
  begin
   S := ReplaceStringCmd(IniFile_.ReadString(Section_, Comp_.Name, ''));
   if S <> '' then TGroupBox(Comp_).Caption := S
    else IniFile_.WriteString(Section_, Comp_.Name, ReplaceCmdString(TGroupBox(Comp_).Caption));
  end
 else
 if Comp_ is TButton then
  begin
   S := ReplaceStringCmd(IniFile_.ReadString(Section_, Comp_.Name, ''));
   if S <> '' then TButton(Comp_).Caption := S
    else IniFile_.WriteString(Section_, Comp_.Name, ReplaceCmdString(TButton(Comp_).Caption));
  end
 else
 if Comp_ is TBitBtn then
  begin
   S := ReplaceStringCmd(IniFile_.ReadString(Section_, Comp_.Name, ''));
   if S <> '' then TBitBtn(Comp_).Caption := S
    else IniFile_.WriteString(Section_, Comp_.Name, ReplaceCmdString(TBitBtn(Comp_).Caption));
  end
 else
 if Comp_ is TradioGroup then
  begin
   for I := 0 to TRadioGroup(Comp_).Items.Count - 1 do
    begin
     S := ReplaceStringCmd(IniFile_.ReadString(Section_, Comp_.Name + '.Itm' + IntToStr(I), ''));
     if S <> '' then TRadioGroup(Comp_).Items.Strings[I] := S
      else IniFile_.WriteString(Section_, Comp_.Name + '.Itm' + IntToStr(I), ReplaceCmdString(TRadioGroup(Comp_).Items.Strings[I]));
    end;
  end
 else
 if Comp_ is TCheckBox then
  begin
   S := ReplaceStringCmd(IniFile_.ReadString(Section_, Comp_.Name, ''));
   if S <> '' then TCheckBox(Comp_).Caption := S
    else IniFile_.WriteString(Section_, Comp_.Name, ReplaceCmdString(TCheckBox(Comp_).Caption));
  end
 else
 if Comp_ is TPanel then
  begin
   if TPanel(Comp_).Caption <> '' then
    begin
     S := ReplaceStringCmd(IniFile_.ReadString(Section_, Comp_.Name, ''));
     if S <> '' then TPanel(Comp_).Caption := S
      else IniFile_.WriteString(Section_, Comp_.Name, ReplaceCmdString(TPanel(Comp_).Caption));
    end;
  end;
end;

function LoadControlsStrings(Fname_: String; Form_: TForm): Boolean;
var I       : Integer;
    IniFile : TIniFile;
begin
 Result := false;
 if FileExists(Fname_) then
  begin
   IniFile := TIniFile.Create(Fname_);
   try
     UpdateComponentFromIni(IniFile, Form_.Name, Form_);
     for I := 0 to Form_.ComponentCount - 1 do
      begin
       UpdateComponentFromIni(IniFile, Form_.Name, Form_.Components[I]);
      end;
   finally
    IniFile.Free;
   end;
   Result := true;
  end;
end;

end.
