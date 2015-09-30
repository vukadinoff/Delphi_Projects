unit ResStrUnit;

interface

uses SysUtils, IniFiles, Classes, Forms, StdCtrls, ExtCtrls, RXCtrls, Buttons;

const C_ExcludeComponentList = 'VersionLabel;';

var
 S_CurrentModulName  : String = 'Модул Зареждане на СИМ карти (tscrn)';
 S_Version           : String = 'ВЕРСИЯ:';
 S_SettingsNotLoaded : String = 'Липсват конфигурационните файлове!'+
                                ''+sLineBreak+
                                'Работата с модула ще бъде прекратена.';
 S_InvalidRegistr    : String = 'Нямате валидна регистрация за модула.'+sLineBreak+
                                'Работата с него ще бъде прекратена.';
 S_DBErrorConnect    : String = 'Невъзможно свързването с базата данни.'+#13+
                                'Не са направени коректни настройки.'+#13+
                                'Моля, коригирайте ги.';
 S_SeeErrForDetails  : String = 'За подробности погледнете файла с грешките.';
 S_UserNameNotFound  : String = 'Не е намерен оператор';
 // registration
 S_InvalidRegKey     : String = 'Невалиден регистрационен ключ.';
 S_InvalidStorageNumb: String = 'Регистрацията е неуспешна !'+sLineBreak+
                                'Невалиден работен склад.';
 S_StorageStr1       : String = 'Текущия склад остава склад No:';
 S_StorageStr2       : String = 'Желаете ли да промените текущия работен склад';
 S_StorageStr3       : String = 'от склад No:';
 S_StorageStr4       : String = 'на склад No:';
 S_StorageStr5       : String = 'Текущия склад е:';
 S_StorageStr6       : String = 'Склад No:';


 S_ShutDown          : String = 'Изгаси терминала';
 S_ExitOs            : String = 'Изход';
 S_InvalidPassword   : String = 'Достъпът отказан!'+sLineBreak+
                                'Невалидна парола.';
 S_SessionStarted    : String = 'Започната сесия';
 S_LoginFailed       : String = 'Грешка при login на оператор';
 S_LogoutFailed      : String = 'Грешка при logout на оператор';
 S_ModuleStart       : String = 'Старт на модула.';
 S_ModuleStop        : String = 'Стоп на модула.';
 S_ModuleLoginFail   : String = 'Грешка при login/logout на модула:';
 S_CustomerNotSelect : String = 'Не сте избрали клиент.';
 S_Required          : String = 'Изисква се:';
 S_Invalid           : String = 'Невалиден';
 S_InvalidPrc        : String = 'Невалидна';
 S_ConfirmReq        : String = 'Желаете ли да продължите нататъка?';
 S_ContragentInserted: String = 'Добавяне на контрагент:';
 S_DuplicateBulstat  : String = 'Съществува клиент с такъв булстат.';
 S_ErrInsContragent  : String = 'Грешка при добавяне на клиент.';
 S_ContrNotFoundCnf  : String = 'Не са открити данни, отговарящи'+sLineBreak+
                                'на посоченото условие.'+sLineBreak+
                                'Желаете ли да въведете клиента като нов?';
 S_ContrManyFound    : String = 'Открити са над 50 клиента'+sLineBreak+
                                'отговарящи на посоченото условие.'+sLineBreak+
                                'Моля, въведете по точни данни.'+sLineBreak+
                                'Операцията се отменя.';
 S_NegativeForbidden : String = 'не може да бъде с отрицателна стойност.';
 S_FailPrintDocument : String = 'Грешка при печат на документ.';
 // Database
 S_Server            : String = 'Сървър';
 S_Local             : String = 'ЛОКАЛЕН';
 S_NoConnection      : String = 'НЯМА ВРЪЗКА';
 S_PleaseWait        : String = 'Моля, изчакайте';
 S_Bulstat           : String = 'ЕИК No';
 S_VATNo             : String = 'ЗДДС No';
 // CyrrToStr
 S_Cm_100               : String= 'сто';
 S_Cm_200               : String= 'двеста';
 S_Cm_300               : String= 'триста';
 S_Cm_400               : String= 'четиристотин';
 S_Cm_500               : String= 'петстотин';
 S_Cm_600               : String= 'шестстотин';
 S_Cm_700               : String= 'седемстотин';
 S_Cm_800               : String= 'осемстотин';
 S_Cm_900               : String= 'деветстотин';
 S_Cm_10                : String= 'десет';
 S_Cm_20                : String= 'двадесет';
 S_Cm_30                : String= 'тридесет';
 S_Cm_40                : String= 'четиридесет';
 S_Cm_50                : String= 'петдесет';
 S_Cm_60                : String= 'шестдесет';
 S_Cm_70                : String= 'седемдесет';
 S_Cm_80                : String= 'осемдесет';
 S_Cm_90                : String= 'деведесет';
 S_Cm_11                : String= 'единадесет';
 S_Cm_12                : String= 'дванадесет';
 S_Cm_13                : String= 'тринадесет';
 S_Cm_14                : String= 'четиринадесет';
 S_Cm_15                : String= 'петнадесет';
 S_Cm_16                : String= 'шестнадесет';
 S_Cm_17                : String= 'седемнадесет';
 S_Cm_18                : String= 'осемнадесет';
 S_Cm_19                : String= 'деветнадесет';
 S_Cm_3                 : String= 'три';
 S_Cm_4                 : String= 'четири';
 S_Cm_5                 : String= 'пет';
 S_Cm_6                 : String= 'шест';
 S_Cm_7                 : String= 'седем';
 S_Cm_8                 : String= 'осем';
 S_Cm_9                 : String= 'девет';
 S_Cm_1_1               : String= 'една';
 S_Cm_1_2               : String= 'един';
 S_Cm_1_3               : String= 'една';
 S_Cm_1_4               : String= 'един';
 S_Cm_2_1               : String= 'двe';
 S_Cm_2_2               : String= 'двa';
 S_Cm_2_3               : String= 'двe';
 S_Cm_2_4               : String= 'двa';
 S_And                  : String= 'и';
 S_Stotinka             : String= 'стотинка';
 S_Stotinki             : String= 'стотинки';
 S_AThousand            : String= 'хиляда';
 S_ThousandS            : String= 'хиляди';
 S_AMillion             : String= 'милион';
 S_Millions             : String= 'милиона';
 S_Levs                 : String= 'лева';
 S_ALev                 : String= 'лев';
 S_StotinkiShort        : String= 'ст.';
 S_BtnYes               : String = '&ДА';
 S_BtnNo                : String = '&НЕ';
 S_BtnOK                : String = '&Потвърди';
 S_BtnCancel            : String = '&Отказ';
 S_BtnAbort             : String = '&Прекъсни';
 S_BtnRetry             : String = '&Отново';
 S_BtnIgnore            : String = '&Игнорирай';

 S_AccessDenied         : String = 'Достъпът до операцията отказан!';
 S_NotSelMobOper        : string = 'Не сте избрали мобилен оператор.';
 S_NotSelPayType        : string = 'Не сте избрали тип на плащане.';
 S_NotSelDocType        : string = 'Не сте избрали документ за печат.';
 S_ConnectingToMobOpr   : String = 'Свързване с мобилния оператор.';
 S_RechargeSim          : String = 'Зареждане на предплатен номер.';
 S_MTel                 : string = 'Мобилтел';
 S_Globul               : string = 'Глобул';
 S_Vivacom              : string = 'Виваком';
 S_SuccSaveSale         : string = 'Успешно заредена SIM карта.';
 
 S_UnsuccSaveSale       : string = 'Неуспешен запис на продажбата.';
 S_ErrGetOperData       : string = 'Грешка при извличане името на оператора';
 S_ErrGetSysNomecl      : string = 'Грешка при извличане служебни номенклатури.';
 S_MissingMobOperPlu    : string = 'Няма настроен артикул за мобилен оператор';
 S_PluNotFound          : string = 'В БД не е намерен артикул No';
 S_Ammount              : String = 'Сума:';
 S_Phone                : String = 'Телeфон N:';
 S_ErrSearchPlu : string = 'Грешка при търсене на артикул в базата';
 S_UnsuccSearchPlu : string = 'Неупешно търсене на артикул в базата.';
 S_PhoneMissing : string = 'Не сте задали телефонен номер.';
 S_InvalidPhone : string = 'Невалиден телефонен номер.';
 S_SumMissing : string = 'Не сте задали сума за зареждане.';
 S_InvalidSumEntNumber : string = 'Невалидна сума за зареждане.'+sLineBreak+'Въведете положително число.';
 S_SumIsBiggerThanLimit : string = 'Сумата за зареждане надвишава'+sLineBreak+'лимита за транзакция за оператор.';
 S_SumIsSmallerThanLimit : string = 'Сумата е под минимума на зареждане'+sLineBreak+'на мобилния оператор.';
 S_SumIsBiggerThanMOLimit : string = 'Сумата надвишава максимума на зареждане'+sLineBreak+'на мобилния оператор.';
 S_SumIsBiggerThanDayLimit : string = 'Сумата за зареждане надвишава'+sLineBreak+'дневния лимит за оператор.';
 S_PassedDayLimit : string = 'Изчерпан дневен лимит.';
 S_LimitIsSmallerThanMinMO : string = 'Оставащият дневен лимит е под минимума на зареждане на мобилния оператор';
 S_SumFrom : string = 'Сума за зареждане от';
 S_To : string = 'до';
 S_SumAbove : string = 'Сума за зареждане над';
 S_NoLimit : string = 'Без ограничение в сумата за зареждане';
 S_SumTo : string = 'Сума за зареждане до';
 S_ErrCalcTrunover : string = 'Грешка при изчисляване на дневния оборот за оператор';
 S_ErrCreatePrnFile: string = 'Грешка при генериране на клиентската бележка.';
 S_ErrCreateFiscPrnFile: string = 'Грешка при печат на бележка.';
 S_ErrGetCustData: string = 'Грешка при извличане данни за клиент.';
 S_ErrCreateReport: string = 'Грешка при запис в отчетите.';

 S_MobOprTestMode       : String = 'Тестов режим на работа'+sLineBreak+
                                   'Без комуникация с мобилен оператор!';
 S_ErrPayPrePaid        : String = 'Операцията отказана от мобилния оператор!';
 S_ErrConnectSystem     : String = 'Неочаквана грешка при опит за свързване'+sLineBreak+
                                   'с мобилния оператор.'+sLineBreak+
                                   'Моля проверете конфигурацията на системата.'+sLineBreak+
                                   ''+sLineBreak+
                                   'За подробности погледнете лога на програмата.';
 S_ErrConnectServer     : String = 'Неуспешно свързване с мобилния оператор!';
 S_ErrCheckPrePaid      : String = 'Операцията отказана от мобилния оператор!';
 S_ErrOnSavingCurrInvN  : String = 'Грешка при запис на текущия номер фактура!';

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
