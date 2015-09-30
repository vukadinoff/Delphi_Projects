unit ResStrUnit;

interface

uses SysUtils, IniFiles, Classes, Forms, StdCtrls, ExtCtrls, RXCtrls, Buttons;

const C_ExcludeComponentList = 'VersionLabel;';

var
 S_ApplicationTitle  : String = 'POS терминал "ТИС ДЕТЕЛИНА"';
 S_CurrentModulName  : String = 'POS станция за ресторант';
 S_Version           : String = 'ВЕРСИЯ:';
 S_ModuleName        : String = 'ELTRADE POS Touch';
 S_RegDecimalError   : String = 'Невалиден десетичен разделител. Трябва да бъде "."';
 S_SettingsNotLoaded : String = 'Неуспешно зареждане на конфигурационния файл!'+sLineBreak+
                                'Моля задайте конфигурацията на системата'+sLineBreak+
                                'като използвате "Конфигурационния Мениджър"'+sLineBreak+
                                ''+sLineBreak+
                                'POS станцията ще бъде деактивирана!';
 // DK2
 S_DK2NotRegistered1 : String = 'Нямате валидна регистрация за модул';
 S_DK2NotRegistered5 : String = 'Остават ви още (';
 S_DK2NotRegistered6 : String = ') дни да използвате модула.'+sLineBreak+
                                'След изтичането на това време лиценза трябва да се '+sLineBreak+
                                'поднови за да функционира модула.';
 S_DemoVGersion      : String = 'НЕЛИЦЕНЗИРАНА ВЕРСИЯ';
 S_DBErrorConnect    : String = 'Невъзможно свързването с базата данни.'+#13+
                                'Не са направени коректни настройки.'+#13+
                                'Моля коригирайте ги.';

// Database
 S_DBNotConnected    : String = 'НЕ Е осъществена е връзка с базата данни.';
 S_DBNoConection     : String = 'Няма връзка с базата данни';
 S_Server            : String = 'Сървър';
 S_Local             : String = 'ЛОКАЛЕН';
 S_NoConnection      : String = 'НЯМА ВРЪЗКА';
 S_ServerOnLine      : String = 'СЪРВЪРА Е ОНЛАЙН';
 S_ServerOffLine     : String = 'НЯМА ВРЪЗКА СЪС СЪРВЪРА';
 S_NetworkError      : String = 'Статус: МРЕЖОВА ГРЕШКА';
 S_StopPrinters      : String = 'Стоп на печатаща система.';

// LOGIN  UNIT
 S_LogOut            : String = 'Изход оператор';
 S_LoginFail         : String = 'Невалидно име или парола.'+#13+'Достъпът отказан!';
 S_NotPermitions     : String = 'Нямате разрешен достъп до модула.';
 S_HavePermitions    : String = 'Разрешен достъп за ';
 S_Cashier           : String = 'касиер';
 S_Administrator     : String = 'администратор';
 S_User              : String = 'потребител';

// Dialogs
 S_WantToExit        : String =  'Желаете ли да прекратите'+#13+'работата със системата ?';
 S_MustEndBon        : String =  'Има започнат бон. Трябва да го приключите'+sLineBreak+
                                 'преди да излезете от системата.';
 S_ChangePluPrice    : String =  'Смяна на цена за артикул:';
 S_ChgPrcOldPrice    : String =  'Стара цена :';
 S_ChgPrcNewPrice    : String =  'Нова цена :';
 S_ChangePluQuant    : String =  'Смяна количество за артикул:';
 S_ChgQtyOldQuant    : String =  'Старо колич.:';
 S_ChgQtyNewQuant    : String =  'Ново колич.:';
 S_Confirm           : String =  'Моля потвърдете!';

// UDP
 S_UDP_StopModule    : String = 'Стоп на работната станция';

// Cashier Unit
S_NoActiveOperator   : String = 'Няма активен оператор или активния оператор няма необходимите права.';
S_MustOpenTable      : String = 'Първо трябва да отворите клиентска сметка';


//DBPluUnit
S_CustomerCardN      : String = 'Клиентска карта No:';
S_Customer           : String = 'Клиент: ';
S_CustomerDiscount   : String = 'Клиентска отстъпка';
S_Card               : String = 'Карта:';
S_LeftSum            : String = 'Остатък:';
S_TotalPayedSum      : String = 'Плат:';
S_CustCardNotFound   : String = 'Не е открита кл. карта No:';
S_NotFound           : String = 'Не е намерен';
S_NotForThisStation  : String = 'Не е предназначен за този терминал.';
S_SellForbidden      : String = 'Забранен за продажба.';
S_FracQuantForbidden : String = 'Забранени дробни количества.';
S_PluRfndForbidden   : String = 'За да върнете стока е необходимо сумата на'+#13+
                                'върнатата стока да е по - малка от общата сума'+#13+
                                'на касовия бон.'+#13+
                                'ВРЪЩАНЕТО НА СТОКИ СЕ ОТКАЗВА.';
S_PluRfndForbidden1  : String = 'Връщането на стоки не е позволено'+#13+'за този оператор.';
S_ZeroPriceForbidden : String = 'Забрана нулева цена.';
S_MinusPrice         : String = 'Отрицателна цена ???.';
S_NoQuantity         : String = 'Не достига колич.';

S_ConfirmClearBon    : String = 'Изчистване на започнатия касов бон.'+#13+
                                'Моля потвърдете.';
S_SelPluIsNotRecipe  : String = 'Избраният артикул не е рецепта.';
S_FreeOfCharge       : String = 'Безплатен подарък.';
S_FreeQuantity       : String = 'Безплатно количество.';
S_PromotionMsg       : String = 'Маркиран е артикул в промоция.'+#13+
                                'Клиента получава безплатно:';
S_PromotionMsg1      : String = 'Допълнително количество от:';
S_PromotionMsg2      : String = 'Допълнително подарък:';

// Bill Unit
S_TOTAL              : String = 'ОБЩА СУМА:';
S_Resto              : String = 'РЕСТО';
S_SystemNumb         : String = 'Сист.No:';
S_Discount1          : String = 'Надбавка посл. артикул';
S_Discount2          : String = 'Отстъпка посл. артикул';
S_Discount3          : String = 'Надбавка межд. сума';
S_Discount4          : String = 'Отстъпка межд. сума';
S_Discount5          : String = 'Надбавка';
S_Discount6          : String = 'Отстъпка';
S_DiscountError      : String = 'Непозволена отстъпка';
S_AddOnError         : String = 'Непозволена надбавка';
S_BonFinal           : String = 'ПРИКЛЮЧЕНА СМЕТКА';
S_StaroSaldo         : String = 'Старо салдо:';
S_NovoSaldo          : String = 'Ново салдо:';
S_OperStr            : String = 'Опр:';
S_Service            : String = 'Сервиз:';
S_ServicePLU         : String = 'Сервиз';
S_ServiceComment     : String = 'Начислен сервиз:';
S_CustomDiscPlu      : String = 'Отстъпка до сума';
S_CustomDiscComment  : String = 'Сума:';
S_BillStr            : String = 'Маса:';
S_LastBon            : String = 'Продажба ';
S_SumStr             : String = 'Сума:';
S_CustCount          : String = 'Поръчки:';
S_BillDiscountStr    : String = 'Отстъпка:';
S_PrefPriceStr       : String = 'ПРЕФ. цени';
S_BillAddOnStr       : String = 'Надбавка:';
S_ValueDiascountStr  : String = 'Стойностна ';
S_STLSumStr          : String = 'Сума до момента:';
S_CustomerStr        : String = 'Клиент';
S_InvoiceStr         : String = 'Фактура';
S_StockRecept        : String = 'Сток.разп.';
S_Terminal           : String = 'Терминал No:';
S_CloseTable         : String = 'ЗАКРИТА СМЕТКА';
S_SaveTable          : String = 'Затворена сметка';
S_NotClosedTable     : String = 'СМЕТКАТА НЕ Е ЗАКРИТА';
S_PrintTable         : String = 'ПРЕГЛЕД СМЕТКА';
S_PrintedTable       : String = 'Отпечатана сметка: ';
S_MiddleRecept       : String = 'МЕЖДИННА ПОРЪЧКА';
S_RevokedPlu         : String = 'СТОРНИРАН АРТИКУЛ';
S_Value              : String = 'СТОЙНОСТ';
S_RevokedDiscount    : String = 'СТОРНИРАНА ОТСТЪПКА';
S_RevokedAddOn       : String = 'СТОРНИРАНА НАДБАВКА';
S_SaldoBeforeRev     : String = 'Салдо преди сторно';
S_Start              : String = 'Начало:';
S_End                : String = 'Край:';
S_Revoked            : String = 'СТОРН.';
S_KitPrnRevokedPlu   : String = 'СТОРНИРАНИ АРТИКУЛИ';
S_BonStarted         : String = 'Има започнат бон';
S_Subtotal           : String = 'Сума до момента:';
S_OpenTableRequired  : String = 'Избора на маса е задължителен!';
S_OpenTable          : String = 'Отворена маса:';
S_ChgPriceForbidden  : String = 'Нямате разрешен достъп до функция:'+#13+
                                '"СМЯНА ЦЕНА В МОМЕНТ НА ПРОДАЖБА"'+#13+
                                ''+#13+
                                'Операцията е отказана.';
S_ChgPriceForbidden1 : String = 'Не можете да промените цената на артикул: '+#13;
S_ChgPriceForbidden2 : String = 'Нулева цена не е позволена за артикул: '+#13;
S_EditPluForbidden   : String = 'Не може да редактирате артикул: '+#13;
S_VOIDForbidden      : String = 'Нямате разрешен достъп до функция:'+#13+
                                '" СТОРНИРАНЕ "'+#13+
                                ''+#13+
                                'Операцията е отказана.';
S_VOIDError          : String = 'Неуспешно изпълнение на операция ВОИД';
S_VOID               : String = 'ВОИД:';
S_VOIDPluForbidden   : String = 'Не може да сторнирате артикул: '+#13;
S_VOIDConfirm        : String = 'Сторниране в междинно приключена'+#13+
                                'сметка. Моля потвърдете.';
S_DiscountForbidden  : String = 'Нямате разрешен достъп до функция:'+#13+
                                '"СВОБОДНИ ОТСТЪПКИ/НАДБАВКИ"'+#13+
                                ''+#13+
                                'Операцията е отказана.';
S_SearchPluForbidden : String = 'Нямате разрешен достъп до функция:'+#13+
                                '"ТЪРСЕНЕ НА АРТИКУЛ"'+#13+
                                ''+#13+
                                'Операцията е отказана.';
S_InvoiceForbidden   : String = 'Нямате разрешен достъп до функция:'+#13+
                                '"ИЗДАВАНЕ НА ФАКТУРИ"'+#13+
                                ''+#13+
                                'Операцията е отказана.';
S_InvToBon_EnterBonID :String = 'Моля въведете системния номер на бона.';
S_InvToBon_BonNotFound:String = 'Няма намерен касов бон с '+sLineBreak+
                                'такъв системен номер.'+sLineBreak+
                                'Операцията се отменя.';
S_InvToBon_BonIsOld   :String = 'Избрания касов бон е отпечатан'+sLineBreak+
                                'преди повече от 5 дни.'+sLineBreak+
                                'Нямате право да издадете фактура'+sLineBreak+
                                'към този бон.';
S_InvToBon_ConfirmCopy: String= 'Към избрания бон има генерирана фактура.'+sLineBreak+
                                'Желаете ли да отпечатате копие на фактурата?';
S_InvToBon_ConfirmNew : String= 'Желаете ли да отпечатате фактура '+sLineBreak+
                                'към бон със Системен номер:';
S_InvToBon_PrintCopy : String = 'Отпечатано копие на фактура към бон със Сист.No:';
S_InvToBon_PrintInv  : String = 'Отпечатана фактура към бон със Сист.No:';
S_FunctionForbidden  : String = 'Нямате право на достъп до тази функция.';
S_TableIsBussy       : String = 'Тази маса е собственост на друг оператор.'+#13+
                                'Нямате право на достъп до масата.'+#13+
                                ''+#13+
                                '';
S_TableIsLocked      : String = 'В момента масатa е отворена от друг оператор:';
S_TableIsLocked1     : String = 'Операцията е отказана. Моля, опитайте по-късно.';
S_TableNotExist      : String = 'Няма отворена сметка с такъв номер.';
S_PrnEndTableQuestion: String = 'Желаете ли печат на бележка?'+sLineBreak+
                                'Моля потвърдете!';
S_PrnEndBonQuestion  : String = 'Желаете ли печат на бележка?'+sLineBreak+
                                'Моля потвърдете!';
S_CustomerCount      : String = 'Брой клиенти: ';

S_ChangeServiceQuest : String = 'Промяна процент сервиз за'+sLineBreak+
                                'отворената сметка.';
S_RemoveServiceQuest : String = 'Премахване на сервиз'+sLineBreak+
                                'от отворената сметка.';
S_AddServiceQuest    : String = 'Добавяне на сервиз'+sLineBreak+
                                'към отворената сметка.';
S_BillChecksumError  : String = 'Грешка при отваряне на сметка.'+sLineBreak+
                                'Грешна контролна сума на файла.';

// Main Unit
S_CanNotOpenTable    : String = 'Не може да отворите сметка.'+#13+
                                'Има започнат Бон.';
S_PrinterError       : String = 'Грешка принтер:';
S_NotOpenTable       : String = 'Няма отворена сметка.';
S_NothingMarked      : String = 'Няма нищо маркирано';
S_WrongPayType       : String = 'Неразрешен тип плащане';
S_ClearBillDialog    : String = 'Желаете ли да откажете започнатия бон?'+#13+
                                'Моля потвърдете.';
S_ClearBeforeClose   : String = 'Има започнат касов бон.'+sLineBreak+
                                'Този бон ще бъде сторниран.'+sLineBreak+
                                'Да продължа ли ?';
S_CanNotClearTable   : String = 'Има отворена клиентска сметка.'+#13+
                                'Не можете да сторнирате целия бон!';
S_PluRecipe          : String = 'Рецепта за артикул: ';
S_PluInfo            : String = 'Информ. артикул: ';
S_BonInfo            : String = 'Информация за текущ бон';
S_CardInfo           : String = 'Информация за разплащателна карта';
S_ReportError        : String = 'Грешка при генериране на справката';

S_Plu                : String = 'Артикул';
S_YES                : String = 'ДА';
S_NO                 : String = 'НЕ';
S_SysErrors          : String = 'Системни събития';
S_PrnErrors          : String = 'Събития печатащо устройство';
S_Printer            : String = 'Принтер';
S_ScannerOK          : String = 'Баркод четец - инсталиран';
S_ScaleOK            : String = 'Електронна везна - инсталирана';
S_DisplcayOK         : String = 'Клиентски дисплей - инсталиран';
S_PaymentServerOK    : String = 'Разплащане с карти - инсталирано';
S_LoyaltySystemOK    : String = 'Система за лоялност - инсталирана';

S_NotInstalled       : String = 'НЕ Е ИНСТАЛИРАН';
S_PrintProfile       : String = 'Профил на печат: ';
S_NoPrinterInstalled : String = 'Няма инсталиран нито един принтер';
S_WrongPrinterNumb   : String = 'Непозволен номер на принтер';
S_PrinterIsBussy     : String = 'Принтера е зает.';
S_ErrorInPrinterTime : String = 'Часовника на принтера се отличава '+#13+
                                'от тозина компютъра. '+#13+
                                'Желаете ли да сверите часовника на принтера.';
S_PrinterTimeIsOk    : String = 'Часовника на принтера е наред.';
S_PrinterNotResponding: String= 'Принтера на отговаря,';
S_CardPayNotInstalled: String = 'Не са инсталирани разплащания с карти.'+#13+
                                'Операцията се отменя.';
S_DisplayNotInstalled: String = 'Не е инсталиран клиентски дисплей.'+#13+
                                'Операцията се отменя.';
S_FailToGetCardData  : String = 'Данните за клиентската карта не са прочетени.'+#13+
                                'Операцията се отменя.';
S_PrefPriceActive    : String = 'Вече има активна преференциална цена.'+#13+
                                'Операцията се отменя.';
S_PrefPriceNotAllowed: String = 'Тази карта няма право на преференциална цена.';
S_PrefPriceStarted   : String = 'Активирана преференциална цена';
S_PrefPriceStartedShrt: String= 'Преференциални цени';
S_CardHaveNoDiscount : String = 'Тази карта няма право на отстъпка.';
S_CardDiscountStarted: String = 'картова отстъпка.';
S_CardDiscountCurrent: String = 'Отстъпка текущата сметка: ';
S_CardDiscountNew    : String = 'Картата позволява: ';
S_CardDiscountChange : String = 'Желаете ли да приемете новата картова отстъпка?';
S_PaymentRefused     : String = 'Плащането отказано от сървъра.';
S_Payment            : String = 'Плащане';
S_PayType            : String = 'тип плащане';
S_CardOwner          : String = '  Притежател : ';
S_CardCategory       : String = '   Категория : ';
S_CardLogNumb        : String = '   Лог.номер : ';
S_CardPhisNumb       : String = '   Физ.номер : ';
S_CardTotalPayed1    : String = 'Общо платeни : ';
S_CardTotalPayed2    : String = 'Общо плат.(2): ';
S_CardLeftSum1       : String = '    Остатък  : ';
S_CardLeftSum2       : String = '  Остатък (2): ';
S_CardDiscount       : String = '    Отстъпка : ';
S_CardPrefPrice      : String = '   Преф.цени : ';
S_CardPrefOverprice  : String = ' Надценка ПЦ : ';
S_PaymentStarted     : String = 'Има започнато плащане.'+#13+
                                'Операцията се отменя';
S_EnterOperatorNumb  : String = 'Моля въведете номер на оператор'+#13+
                                'преди да натиснете бутона.';

// Split/merge Bills
S_SplitTblConfirm1   : String = 'Желаете ли всички артикули от'+#13+
                                'сметка: ';
S_SplitTblConfirm2   : String = 'Да бъдат добавени към отворената'+#13+
                                'сметка: ';
S_SplitTblError      : String = 'Грешка при разделяне на сметки';
S_MergeTblError      : String = 'Грешка при обединяване на сметки';
S_SplitErrHaveNewPlu : String = 'Сметката съдържа нови артикули.'+sLineBreak+
                                'Не може да разделяте сметка съдържаща нови артикули.';
S_SplitErrHavePayment: String = 'Сметката съдържа платена сума.'+sLineBreak+
                                'Не може да разделяте сметка съдържаща палтена сума.';
S_ConfirmSplit       : String = 'Да запиша ли промените в двете сметки?';
S_SplitBillEvent     : String = 'Разделяне на сметки.';
S_MergeBillEvent     : String = 'Обединяване на сметки.';
S_SplitBillMessage   : String = 'РЕДАКТИРАНА СМЕТКА';
S_MergeBillMessage   : String = 'ОБЕДИНЕНА СМЕТКА';

// Old Bills Unit
S_AscToReprintFiscal1: String = 'Към избраната сметка има'+sLineBreak+
                                'отпечатан фискален бон.'+sLineBreak+
                                'Желаете ли да отпечатате'+sLineBreak+
                                'още един фискален бон.'+sLineBreak+
                                'Моля потвърдете!';
S_AscToReprintFiscal2: String = 'Желаете ли да отпечатате'+sLineBreak+
                                'фискален бон, към избраната'+sLineBreak+
                                'сметка?'+sLineBreak+
                                'Моля потвърдете!';
S_AscToReprintComment: String = 'Желаете ли да отпечатате служебно'+sLineBreak+
                                'копие на избраната сметка?'+sLineBreak+
                                'Моля потвърдете!';
S_AscToReprintInvoice: String = 'Желаете ли да отпечатате фактура'+sLineBreak+
                                'към избраната сметка ?'+sLineBreak+
                                'Моля потвърдете!';
S_ReprintInvoiceFail : String = 'Към избраната сметка вече има'+sLineBreak+
                                'отпечатан фискален бон.'+sLineBreak+
                                'Не може да отпечатате фактура.';
S_ReprintInvoiceFail1: String = 'Към избраната сметка вече има'+sLineBreak+
                                'отпечатана фактура.'+sLineBreak+
                                'Не може да отпечатате нова фактура.';

// TXT REPORTS UNIT
S_SetReportsToZero   : String = 'Дневен отчет с нулиране';
S_ReportWithZero     : String = 'Отчет с нулиране';
S_EndOfDayConfirm    : String = 'Избраната операция ще нулира '+sLineBreak+
                                'дневния отчет.'+sLineBreak+
                                'Желаете ли да продължите?';
S_EndOfDayTblConfirm : String = 'Терминала има отворени сметки.'+sLineBreak+
                                'Желаете ли да нулирате'+sLineBreak+
                                'дневния отчет?';
S_EndOfDayOperConfirm: String = 'Избраната операция ще нулира '+sLineBreak+
                                'оборота на оператора.'+sLineBreak+
                                'Желаете ли да продължите?';
S_EndOfDayOperTblConfirm: String = 'Избрания оператор има'+sLineBreak+
                                'отворени сметки.'+sLineBreak+
                                'Желаете ли да нулирате'+sLineBreak+
                                'оборота на оператора?';
S_PrintEodFiscalConf : String = 'Проверете дали фискалния принтер е включен'+sLineBreak+
                                'и има достатъчно хартия. '+sLineBreak+
                                'Ако това е така продължете нататък.';
S_GetTotalReport     : String = 'Отчет общ оборот';
S_GetOperSumsReport  : String = 'Отчет оборот по оператори';
S_GetFullReport      : String = 'Отчет пълен';
S_GetPrewTotalReport : String = 'Последен отчет общ оборот';
S_GetSingleOperReport: String = 'Отчет оборот на оператор';
S_LastOperReport     : String = 'Последен отчет на оператор';
S_Operators          : String = 'ОПЕРАТОРИ';

// Search PLU Unit
S_AllPlues           : String = 'ВСИЧКИ АРТИКУЛИ';
S_Close              : String = 'Затвори';
S_Back               : String = 'Назад';

// ИНЖОИЦЕ
S_ErrorLocateCustData: String = 'Грешка при търсене на клиентски данни.'+sLineBreak+
                                'Операцията се отменя.'+sLineBreak+
                                'Моля опитайте отново.';
S_CustomerNotFound   : String = 'Не са открити данни отговарящи на '+sLineBreak+
                                'посоченото условие.'+sLineBreak+
                                'Желаете ли да въведете клиента като нов?';
S_TooManyCustomers   : String = 'Открити са над 50 клиента,'+sLineBreak+
                                'отговарящи на посоченото условие.'+sLineBreak+
                                'Моля въведете по точни данни.'+sLineBreak+
                                'Операцията се отменя.';
S_RequiredField1     : String = 'Полето "';
S_RequiredField2     : String = '" изисква стойност.'+sLineBreak+
                                'Моля въведете коректни данни.';
S_InvalidValue       : String = '" има некоректна стойност.'+sLineBreak+
                                'Да продължа ли с въведените данни.';
S_BulstatStr         : String = 'Идент.№ ';
S_TaxNumbStr         : String = 'ЗДДС № ';
S_BulstatAlreadyExist: String = 'Клиент с такъв Идент.№ вече съществува.'+sLineBreak+
                                'Не може да въведете клиент с дублиращ се'+sLineBreak+
                                'Идент.№.'+sLineBreak+
                                'Моля коригирайте данните.';
S_AddCustomerError   : String = 'Грешка при въвеждане на нов клиент.';
S_EditCustomerError  : String = 'Грешка при редактиране на клиент.';
S_PrintInvoiceError  : String = 'Грешка при печат на фактура.'+sLineBreak+
                                'Операцията се отменя.';
S_EnterSumBefore     : String = 'Моля въведете сума'+sLineBreak+
                                'преди да натиснете бутона.';
S_ServiceSumIn       : String = 'Служебно въвеждане на сума:';
S_ServiceSumOut      : String = 'Служебно извеждане на сума:';
S_ServiceSumInOK     : String = 'Служебно въвеждане на сума - потвърдено:';
S_ServiceSumOutOK    : String = 'Служебно извеждане на сума - потвърдено:';
S_SetDatabeseConn    : String = 'Настройка база данни.';
S_ChangeWorkStorage  : String = 'Промяна работен склад.';
S_SystemSettings     : String = 'Меню системни настройки.';
S_StoragesList       : String = 'Списък активни складове';
S_Number             : String = 'Номер';
S_Name               : String = 'Наименование';
S_Comment            : String = 'Описание';
S_MOL                : String = 'МОЛ';
S_CurrentStorageNumb : String = 'Текущ склад No:';
S_StorageStr1        : String = 'Текущия склад остава склад No:';
S_StorageStr2        : String = 'Желаете ли да промените текущия работен склад';
S_StorageStr3        : String = 'от склад No:';
S_StorageStr4        : String = 'на склад No:';
S_StorageStr5        : String = 'Текущия склад е:';
S_StorageStr6        : String = 'Склад No:';

// Buttons
S_BtnYes             : String = '&ДА';
S_BtnNo              : String = '&НЕ';
S_BtnOK              : String = '&Потвърди';
S_BtnCancel          : String = '&Отказ';
S_BtnAbort           : String = '&Прекъсни';
S_BtnRetry           : String = '&Отново';
S_BtnIgnore          : String = '&Игнорирай';

// Select Date FORM
S_SelectDate_SelDate : String = 'Моля изберете дата.';
S_SelectDate_SelTime : String = 'Моля изберете час.';
S_SelectDate_SelDateTime   : String = 'Моля изберете дата и час.';
S_SelectDate_SelPeriodDate : String = 'Моля изберете период.';
S_SelectDate_SelPeriodTime : String = 'Моля изберете период.';
S_SelectDate_SelPeriodDtTm : String = 'Моля изберете период.';
S_SelectDate_Date    : String = 'Дата:';
S_SelectDate_Time    : String = 'Час:';
S_SelectDate_DateTime: String = 'Време:';
S_SelectDate_FromDate: String = 'От Дата:';
S_SelectDate_ToDate  : String = 'До Дата:';
S_SelectDate_FromHour: String = 'От Час:';
S_SelectDate_ToHour  : String = 'До Час:';
S_SelectDate_FromDtTm: String = 'От време:';
S_SelectDate_ToDtTm  : String = 'До време:';

// Txt Reports
S_TxtRep_SepBillInfo  : String = '-----------------------------------';
S_TxtRep_Table        : String = '     Маса :';
S_TxtRep_SysNumber    : String = ' Сист. No :';
S_TxtRep_OrderCount   : String = '  Поръчки :';
S_TxtRep_PLUCount     : String = ' Артикули :';
S_TxtRep_TotalSum     : String = 'Обща сума :';
S_TxtRep_DiscountSum  : String = 'Сума отст.:';
S_TxtRep_DiscountCount: String = 'Брой отст.:';
S_TxtRep_Terminal     : String = ' Терминал :';
S_TxtRep_OperNumb     : String = ' Оператор :';
S_TxtRep_OperName     : String = ' Оператор :';
S_TxtRep_CustDiscount : String = 'Кл. отст. :';
S_TxtRep_TBLOpen      : String = '  Открита :';
S_TxtRep_TBLClosed    : String = '  Закрита :';
S_TxtRep_CustCount    : String = '  Клиенти :';
S_TxtRep_Service      : String = '   Сервиз :';
S_TxtRep_Comment      : String = ' Коментар :';
S_TxtRep_SepCard      : String = '--------- КЛИЕНТСКА КАРТА ---------';
S_TxtRep_CardPhis     : String = '   Физ.No :';
S_TxtRep_CardLog      : String = '   Лог.No :';
S_TxtRep_CardOwner    : String = ' Притежат :';
S_TxtRep_SepCompany   : String = '----------- ДАННИ ФИРМА -----------';
S_TxtRep_Bulstat      : String = '  Идент.№ :';
S_TxtRep_TaxNumb      : String = '   ЗДДС № :';
S_TxtRep_FirmName     : String = '    Фирма :';
S_TxtRep_Town         : String = '     Град :';
S_TxtRep_Address      : String = '    Адрес :';
S_TxtRep_MOL          : String = '      МОЛ :';
S_TxtRep_Discount     : String = ' Отстъпка :';
S_TxtRep_SepPlues     : String = '------------ АРТИКУЛИ -------------';
S_TxtRep_PluAddOn     : String = 'Надб. в/у';
S_TxtRep_PluDiscount  : String = 'Отст. в/у';

S_TxtRep_SepReport    : String = '------------------------------';
S_TxtRep_TermNumb     : String = 'Терм. No:';
S_TxtRep_TermName     : String = '   Терм.:';
S_TxtRep_StartRep     : String = ' Начало :';
S_TxtRep_EndRep       : String = '   Край :';
S_TxtRep_SellsCount   : String = 'Прости продажби :';
S_TxtRep_TablesCount  : String = '  Отворени маси :';
S_TxtRep_TotalRepSum  : String = 'Общ оборот:';
S_TxtRep_SepCashiers  : String = '   ОПЕРАТОРИ (КАСИЕРИ)';
S_TxtRep_SepCashier   : String = '   ОПЕРАТОР (КАСИЕР)';
S_TxtRep_ZreportDone  : String = '  ОТЧЕТА Е НУЛИРАН';
S_TxtRep_Operator     : String = 'ОПЕРАТОР';

S_TerminalZReport      : String= 'Нулиране на терминал:';
S_TerminalZReportTbl   : String= 'Съществуват отворени маси при нулиране на терминал: ';
S_OperatorZReportTbl   : String= 'Съществуват отворени маси при нулиране на оператор: ';
S_StockTemplateNotFound: String= 'Темплейта за касова бележка'+sLineBreak+
                                 'през Windows не е намерен.';
S_StockCopy_ConfirmNew : String= 'Желаете ли да отпечатате копие '+sLineBreak+
                                 'към бон със Системен номер:';


// Registration form
S_InvalidSerialNumber  : String= 'Регистрацията е неуспешна !'+sLineBreak+
                                 'Невалиден сериен номер.';
S_InvalidStorageNumber : String= 'Регистрацията е неуспешна !'+sLineBreak+
                                 'Невалиден работен склад.';

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

S_UnsuccessfulScaleInit    : String = 'Неуспешна инициализация на електрона везна.';
S_UnsuccessfulDisplayInit  : String = 'Неуспешна инициализация на клиентски дисплей.';
S_UnsuccessfulScannerInit  : String = 'Неуспешна инициализация на баркод скенер.';
S_UnsuccessfulPrinterInit  : String = 'Неуспешна инициализация на печатащо устройство.';
S_PleaseCheckHWDevice      : String = 'Моля проверете дали устройството е включено'+sLineBreak+
                                      'и дали има валидна конфигурация.'+sLineBreak+
                                      'За подробности погледнете лога на грешките.';


S_ModuleNotRegistered_StopWork            : String = 'Модула не е регистриран в базата данни.'+sLineBreak+
                                                     'Работата с модула ще бъде прекратена.';
S_CommandUnknown                          : String = 'Непозната команда';
S_ErrorOnWritingEvent                     : String = 'Грешка при запис на събитие';
S_MessageAdded                            : String = 'Съобщението добавено';
S_WatchSetRight                           : String = 'Сверен часовник';
S_ErrOnSetWatch                           : String = 'Грешка при сверяване';
S_InCorrectTimeFormat                     : String = 'Некоректен формат на времето';
S_Refreshed                               : String = 'ОБНОВЕНИ';
S_POSRestDoesNotSupportCommand            : String = 'ELTRADE POS Touch не поддържа тази команда';
S_Reloaded                                : String = 'ПРЕЗАРЕДЕН';
S_WorkingStorageChanged                   : String = 'Променен номер работен склад';
S_ErrOnChangingStoragaNumb                : String = 'ГРЕШКА ПРИ ПРОМЯНА НОМЕР НА СКЛАД';
S_WrongStorageNumb                        : String = 'НЕКОРЕКТЕН НОМЕР НА СКЛАД';
S_ErrOnLoadingSystemNomenclature          : String = 'Грешка при зареждане на системни номенклатури.';
S_SeeErrorFileForDetails                  : String = 'Погледни файла с грешките за повече информация.';
S_CurrencySign                            : String = 'лв.';
S_ErrOnSavingCurrentInvoiceNumb           : String = 'Грешка при запис на текущия номер фактура';
S_SeriousError                            : String = 'СЕРИОЗНА ГРЕШКА';
S_WrongModuleInit                         : String = 'Грешно инициализиран номер на модул';
S_Code                                    : String = 'Код';
S_NameShortly                             : String = 'Име';
S_Quant                                   : String = 'Колич.';
S_Portion                                 : String = 'Порции';
S_ECRName                                 : String = 'Касово име';
S_ForeignName                             : String = 'Допълн.име';
S_NomenclCode                             : String = 'Номенкл.код';
S_ScaleCode                               : String = 'Код везна';
S_MaxAddOn                                : String = 'Макс.надб.';
S_MaxDiscount                             : String = 'Макс.отст.';
S_MinReserve                              : String = 'Мин. запас';
S_MaxReserve                              : String = 'Макс. запас';
S_ForPOS                                  : String = 'За терминал';
S_Measure                                 : String = 'Мярка';
S_PrintingGroup                           : String = 'Група печат';
S_TaxGroup                                : String = 'Дан. група';
S_Tax                                     : String = 'ДДС';
S_PluGroup                                : String = 'Арт. група';
S_BuyPrice                                : String = 'Покупна цена';
S_TradePrice                              : String = 'Прод. цена едро';
S_SellPrice                               : String = 'Прод. цена дребно';
S_ForbidenSaleInMinus                     : String = 'Забрана отрицателни колич.';
S_Promotional                             : String = 'Промоционален';
S_Recipe                                  : String = 'Съставен артикул (рецепта)';
S_OnlyIntQuant                            : String = 'Само цели количества';
S_SaleOnZeroPrice                         : String = 'Разрешена нулева цена';
S_ChangePriceOnSale                       : String = 'Разр. смяна цена при продажба';
S_Barcode                                 : String = 'Баркод';
S_Used                                    : String = 'Използ.';
S_Barcodes                                : String = 'БАРКОДОВЕ';
S_StorageName                             : String = 'Склад Име';
S_Quantity                                : String = 'Количество';
S_Availabilities                          : String = 'НАЛИЧНОСТИ';
S_Priority                                : String = 'Приор.';
S_Days                                    : String = 'Дни';
S_Promotions                              : String = 'ПРОМОЦИИ';
S_NoConnectionToPrinterForInvoices        : String = 'Няма връзка с принтера за фактури';
S_ErrOnLoadingPluesToMerge                : String = 'Грешка при зареждане на артикули за събиране на сметки.';
S_Count                                   : String = 'Бр.';

S_RevokeLastBonQuest                      : String= 'Желаете ли да сторнирате последно'+sLineBreak+
                                                    'въведената касова бележка?'+sLineBreak+
                                                    'Моля потвърдете.';
S_RevokeLastBonSucceed                    : String= 'Успешно сторниране на касов бон.';
S_RevokeLastBonFail                       : String= 'Неуспешно сторниране на касов бон:';
S_RevokeLastNotFound                      : String= 'Няма издадени касови бележки за последните 5 дни.';
S_RevokeLastAlreadyDone                   : String= 'Последно издадения касов бон е вече сторниран.';
S_BonOldLoaded                            : String = 'ПРЕГЛЕД СТАРА СМЕТКА';
S_ItemReturnVeryOld                       : String= 'Избраната касова бележка по-стара'+sLineBreak+
                                                    'от 30 дни. Връщането на стоки от'+sLineBreak+
                                                    'такава бележка е невъзможно.';
S_ItemReturnConfirm                       : String= 'Връщане на стока'+sLineBreak+
                                                    'Моля потвърдете.';
S_CannotReturnService                     : String= 'Не можете да върнете начисления сервиз.';
S_ItemReturnOK                            : String= 'Връщане на стока приключено.';
S_ItemReturnFail                          : String= 'Неуспешно връщане на стока.';
S_ItemReturnAlreadyDone                   : String= 'Избраната стока е вече върната!'+sLineBreak+
                                                    'Дата на връщане: ';
S_ItemReturn                              : String= 'ВРЪЩАНЕ НА СТОКА';
S_ItemBuyDate                             : String= 'Закупена на:';
S_ChangeQuantForbiden                     : String= 'Забранена промяната на количество'+sLineBreak+
                                                    'за артикул';
S_PluIsPromotional                        : String= 'Артикулът е безплатен от промоция.';
S_PluQuantNegative                        : String= 'Избраният артикул вече е върнат.';
S_UnCorrectConfigOfReport                 : String= 'Некоректна конфигурация на справката.';
S_SelectedTermNumberIsCurrentNumb         : String= 'Посочения номер за терминал, на който'+sLineBreak+
                                                    'да бъде преместена сметката съвпада с текущия.';
S_TermShares_NoExport                     : String= 'Терминала споделя сметки с останалите терминали.'+
                                                    ' Сметката няма да бъде прехвърлена.';
S_NoSalesForTerminal                      : String= 'Няма продажби за терминал с номер';
S_NoExportAccount                         : String= 'Сметката няма да бъде прехвърлена.';
S_SuccessfulAccountExport                 : String= 'Успешен експорт на сметка към терминал';
S_ExportForbidden                         : String= 'Нямате разрешен достъп до функция:'+#13+
                                                    '" ЕКСПОРТ НА СМЕТКА "'+#13+
                                                    ''+#13+
                                                    'Операцията е отказана.';
S_TxtRep_ReturnedTitle                    : String= '     ВЪРНАТИ СТОКИ ';
S_TxtRep_ReturnedCount                    : String= 'Брой върнати';
S_TxtRep_ReturnedTotal                    : String= 'Общо върнати';
S_TxtRep_ReturnedPayType                  : String= 'Върнати   ';
S_TxtRep_RevokedTitle                     : String= '     СТОРНИРАНИ БОНОВЕ ';
S_TxtRep_RevokedCount                     : String= 'Брой сторнирани';
S_TxtRep_RevokedTotal                     : String= 'Общо сторнирани';
S_TxtRep_RevokedPayType                   : String= 'Сторнирани';
S_NoRegularBonWithNumber                  : String= 'Несъществува редовен бон'+sLineBreak+' със системен номер';
S_SelectedBonIsRevoked                    : String= 'Избраният бон е сторниран.'+sLineBreak+
                                                    'Не може да бъде издадена фактура.'+sLineBreak+
                                                    'Операцията се отменя.';
S_ReviewAccountsForTerminal               : String= 'Преглед на бележки за терминал';
S_AutoDeletedEmptyAccounts                : String= 'Автоматично изтрити празни сметки';
S_ChangePriceForbiden                     : String= 'Забранена промяната на цена'+sLineBreak+
                                                    'за артикул';
S_PluesWithoutPromotion                   : String= 'за арт. без промоция';
S_ErrOnCreateReport                       : String= 'Грешка при създаване запис за отчет';
S_ErrOnUpdateReportRec                    : String= 'Грешка при update на запис за отчет';
S_ErrOnExtractingRecForReport             : String= 'Грешка при извличане на запис за отчет';
S_ErrOnExtractingDataForReport            : String= 'Грешка при извличане на данни за отчет';
S_PluShort                                : String= 'Арт.';
S_Price                                   : String= 'Цена';
S_OverLastPlu                             : String= 'Над посл. артикул';
S_OverSTL                                 : String= 'Над междинна сума';
S_ValueShort                              : String= 'С-ст';
S_RevokeLastBon                           : String= 'Сторниране на последен бон';
S_OperationAborted                        : String= 'Операцията се отменя.';
S_TryAgainToPutSumInFU                    : String= 'Опитай отново отразяване на операцията?';
S_ServiceSumInNotFU                       : String= 'Служебното въвеждане на сума не е' +sLineBreak+
                                                    'отразено във Фискалното устройство.';
S_ServiceSumOutNotFU                      : String= 'Служебното извеждане на сума не е' +sLineBreak+
                                                    'отразено във Фискалното устройство.';
S_SumOutCannotBeMoreThanAvailable         : String= 'Служебно изведената сума не може' +sLineBreak+
                                                    'да бъде по-голяма от наличната.';
S_ErrorOnExtractingMaxSumOut              : String= 'Грешка при извличане на максимална сума за служебно извеждане.';
S_DatabaseNeedUpdate                      : String= 'Необходим е ъпдейт на базата данни.'+sLineBreak+
                                                    'Модул "ELTRADE POS Touch" ще бъде затворен.';
S_NoObligations                           : String= '   Няма задължения';
S_OpenedOperatorsAccounts                 : String= 'Операторите имат отворени сметки.'+#13#10+
                                                    'Желаете ли да занулите всички оператори?';
S_WantToZeroAllOperators                  : String= 'Избраната операция ще занули'+#13#10+
                                                    'отчетите на всички оператори.'+#13#10+
                                                    'Желаете ли да продължите?';
S_ZeroOperators                           : String= 'Нулиране на всички оператори';
S_OpenedBillsOnZeroOperators              : String= 'Съществуват отворени сметки при нулиране на всички оператори';
S_Available                               : String= 'НАЛИЧНО';
S_SinglePrice                             : String= 'Ед.цена';
S_PluWithNumb                             : String= 'Артикул с номер';
S_PluWithBarcode                          : String= 'Артикул с баркод';
S_QuantNotEnought                         : String= 'Недостатъчна наличност за промяна количество на артикул.';
S_ErrorOnSerchPlu                         : String= 'Грешка при търсене на артикул';
S_NoGroupSet                              : String= 'Не е зададена група';
S_DeepVOID                                : String= 'Дълбок ВОЙД';
S_Voids                                   : String= '   ВОЙДОВЕ';
S_CountVoid                               : String= 'Брой ВОЙД';
S_SumVoid                                 : String= 'Сумa ВОЙД';
S_CountDeepVoid                           : String= 'Брой Дълбок ВОЙД';
S_SumDeepVoid                             : String= 'Сума Дълбок ВОЙД';
S_ForcedClearBon                          : String= 'Направено е принудително изчистване на сметка.';
S_BonToLongOnScreen                       : String= 'Сметката е оставена отворена пoвече от ';
S_Account                                 : String= 'Сметка';
S_InternalSystemError_KeyCheck            : String= 'Системна грешка при проверка на защитен ключ.';
S_WorkWillBeStopped                       : String= 'Работата с "ELTRADE POS Touch" ще бъде прекратена!';
S_NoDeviceFound_KeyCheck                  : String= 'Защитният ключ не е намерен!';
S_PleaseInsertValidKey                    : String= 'Моля, поставете валиден защитен ключ';
S_TheAttachedKeyIsNotValid                : String= 'Поставeният ключ не е валиден.';
S_TheDeviceDoesNotSupportRestPOS          : String= 'Поставеният ключ не е подходящ за'+sLineBreak+'"POS Сензорен Интерфейс".';
S_TheDeviceDoesNotSupportTIS              : String= 'Поставеният ключ не е подходящ за'+sLineBreak+'"ТИС Детелина".';
S_ValidityExpiredCloseApp                 : String= 'Времето за демострационно ползване на модула е изтекло.';
S_UnexpectedErrorOnCheckKey               : String= 'Неочаквана грешка при проверка на защитен ключ.';
S_ClosedAfterKeyEvent                     : String= 'Терминиране след проверка на защитен ключ.';
S_KeyRestPOS                              : String= 'Защитен ключ ELTRADE POS Touch';
S_PleaseWaitESKCheck                      : String= 'Моля, изчакайте.'+sLineBreak+
                                                    'Проверка на защитен ключ.';
S_LastOperationIsOlder                    : String= 'Текущата дата е по-малка от датата на'+#10#13+
                                                    'последната операция.';
S_SystemTime_Changed                      : String= 'Направена е промяна на системното време.';
S_RestPOSWillBeClosed                     : String= 'Модул "ELTRADE POS Touch" ще бъде затворен.';
S_ControlReceipt                          : String= 'КОНТРОЛНА БЕЛЕЖКА';
S_FromOldBon                              : String= 'от стар бон';
S_ReturnPlu                               : String= 'ВРЪЩАНЕ НА АРТИКУЛ';
S_PrintFiscalCopy                         : String= 'ПЕЧАТ ФИСКАЛНО КОПИЕ';
S_PrintNonFiscalCopy                      : String= 'ПЕЧАТ СЛУЖЕБНО КОПИЕ';
S_FiscalReceipt                           : String= 'ФИСКАЛЕН';
S_SystemReceipt                           : String= 'СЛУЖЕБЕН';
S_InvoicePrinted                          : String= 'ПЕЧАТ НА ФАКТУРА';
S_ToBonNo                                 : String= 'Към бон';
S_SimpleVoid                              : String= 'ВОЙД';
S_SellOnFreePrice                         : String= 'Продажба на свободна цена';
S_ErrorOnExtractRepsData                  : String= 'Грешка при извличане на данни за отчети.';
S_Client_Not_Responding                   : String= 'Клиентът не отговаря на изпратената команда';
S_ReportFileNotFound                      : String= 'Не е намерена бланката за отчети:';
S_StopGenerateReport                      : String= 'Генерирането на отчети се прекратява.';
S_RepNOTZeroed                            : String= 'Отчетът не е приключен';
S_SellOnFreePriceForbidden                : String= 'Нямате разрешен достъп до функция:'+#13+
                                                    '"СМЯНА ЦЕНА В МОМЕНТ НА ПРОДАЖБА"'+#13+
                                                    'Продажбата ще бъде на стандартна цена.';
S_ClearStartedBon                         : String= 'Изчистване на започнат бон';
S_AccountShort                            : String= 'См.';

S_ErrDuplicatePOSNumber                   : String= 'В мрежата съществува друг POS терминал'+#13+#10+
                                                    'притежаващ същия номер и работещ в същата'+#13+#10+
                                                    'база данни. Работа на два терминала с еднакви'+#13+#10+
                                                    'номера в една база не е препоръчително.';
S_CannotVoidTotalBecomeNegative           : String= 'Не можете да войдирате избрания артикул,'+#13+#10+
                                                    'защото сумата на бона ще стане отрицателна.';
S_CannotChangeQuantTotalBecomeNegative    : String= 'Промяната на количеството на избрания артикул се отменя,'+
                                                    'защото общата сума на бона ще стане отрицателна.';
S_CannotChangePriceTotalBecomeNegative    : String= 'Промяната на цена на избрания артикул се отменя,'+
                                                    'защото общата сума на бона ще стане отрицателна.';
S_CreatingReport                          : String= 'Генериране на отчет';
S_Menu                                    : String= 'Меню';
S_PrintFiscalReceiptConfirm               : String= 'Желаете ли печат на фискална бележка?'+ sLineBreak+
                                                    'Моля, потвърдете!';
S_SelectReceiptType                       : String= 'Моля, изберете тип на бележка!';
S_DiscWillBeRejected                      : String= 'Отстъпката ще бъде отказана!';
S_AddOnWillBeRejected                     : String= 'Надбавката ще бъде отказана!';
S_Left                                    : String= 'Остатък';
S_CardFoundButNotAllowedToChange          : String= 'Забранена промяна на клиент за бон.';
S_NaveNewPluesNoOperChange                : String= 'Сметката съдържа нови артикули.'+sLineBreak+
                                                    'Не може да сменяте оператор на сметка с нови артикули.';
S_IllegalOperNumber                       : String= 'Некоректен номер на оператор.';
S_ErrorOnOperCheck                        : String= 'Грешка при извличане данни за оператор.';
S_SelectedOperator                        : String= 'Посоченият оператор';
S_NotCashier                              : String= 'не е касиер';
S_ChangeBillOper                          : String= 'Промяна касиер на сметка';
S_OperNotExist                            : String= 'Не съществува оператор с No';
S_SelectedOperSameAsCurrent               : String= 'Посоченият оператор съвпада с текущият на сметката.';
S_OnlyForOperWithAccessToOthersTables     : String= 'Функцията е разрешена само за оператори с достъп до чужди маси.';
S_ChangePrice                             : String= 'Промяна цена';
S_ChangeQuant                             : String= 'Промяна кол.';
S_ChangeService                           : String= 'Промяна сервиз';
S_RetItemOnlyForOldSale                   : String= 'Избраната функция се използва само за'+#13+#10+
                                                    'връщане на артикул от приключена сметка.'+#13+#10+
                                                    'Операцията се отменя.';
S_RetPaymentOnlyForOldSale                : String= 'Избраната функция се използва само за'+#13+#10+
                                                    'връщане на плащане за приключена сметка.'+#13+#10+
                                                    'Операцията се отменя.';
S_RecInPOSILLSDeleted                     : String= 'Временният запис за избраната сметка е изтрит.';
S_ReturnPaymentsRejected                  : String= 'Плащанията не могат да бъдат върнати.'+#13+#10+
                                                    'Операцията се отменя.';
S_PrintCopyInvoice                        : String= 'Отпечатано копие на ф-ра, издадена на фискално устройство.';
S_PrintCopyInvShort                       : String= 'Печат копие ф-ра, издадена на ФУ';
S_ErrorOnPrnCopyInv                       : String= 'Грешка при печат копие ф-ра, издадена на ФУ.';
S_ThereIsFiscalBonConfirmInvoice          : String= 'Към избраната сметка вече има издаден' + #13+#10+
                                                    'фискален бон. Фактурата може да бъде' + #13+#10+
                                                    'издадена като служебен бон.'+#13+#10+
                                                    'Желаете ли да продължите?';
S_ORIGINAL                                : String= 'ОРИГИНАЛ';
S_DUBLIKAT                                : String= 'ДУБЛИКАТ';
S_InvSinglePrice                          : String= 'единична цена';
S_InvQuantity                             : String= 'количество';
S_InvSum                                  : String= 'сума';
S_TaxGrp1Letter                           : String= 'А';
S_TaxGrp2Letter                           : String= 'Б';
S_TaxGrp3Letter                           : String= 'В';
S_TaxGrp4Letter                           : String= 'Г';
S_TaxGrp5Letter                           : String= 'Д';
S_TaxGrp6Letter                           : String= 'Е';
S_TaxGrp7Letter                           : String= 'Ж';
S_TaxGrp8Letter                           : String= 'З';
S_SalePerson                              : String= 'Продавач';
S_Receiver                                : String= 'Получател';
S_ReceivedBy                              : String= 'ПОЛУЧИЛ';
S_TownName                                : String= 'ГРАД';
S_Address                                 : String= 'АДРЕС';
S_Ident                                   : String= 'ИДЕНТ N';
S_ZDDS                                    : String= 'ЗДДС  N';
S_DiscOverSubSumAlreadyAdded              : String= 'Сметката вече има отстъпка/надбавка над междинна сума.'+#13+#10+
                                                    'Не можете да добавите друга.'+#13+#10+
                                                    'Операцията се отменя.';
S_TotalAmount                             : String= 'ОБЩА СУМА';
S_All                                     : String= 'всичко';
S_NetValue                                : String= 'нето стойност';
S_ItemsWithLimitDiscount                  : String= 'СТОКИ С ОГР.ОТСТЪПКА';
S_ItemsInPromo                            : String= 'СТОКИ В ПРОМОЦИЯ';
S_ItemNoDiscount                          : String= 'СТОКИ БЕЗ ОТСТЪПКА';
S_OtherItems                              : String= 'ДРУГИ СТОКИ';
S_ReturnItems                             : String= 'ВЪРНАТИ';
S_DiscountForTime                         : String= 'Часова отстъпка';
S_DiscountForTurnOver                     : String= 'Отстъпка за оборот';
S_NotSupportedFunctionPmntSrv             : String= 'Сървърът за разплащане не'+#13+#10+
                                                    'поддържа избраната функция.';
S_CardPaymentSystem                       : String = 'КАРТОВО РАЗПЛАЩАНЕ';
S_CardSystem                              : String = 'КАРТОВА СИСТЕМА';
S_ErrOnCallPaySrvSettings                 : String = 'Грешка при извикване на настройки'+#13+#10+
                                                     'за картово разплащане.';
S_ConnectionToPaymentSrvOK                : String = 'Успешна връзка със сървъра за разплащане с карти.';
S_ConnectionToPaymentSrvErr               : String = 'Неуспешно свързване със сървъра за разплащане с карти';
S_SuccessfulRefund                        : String = 'Успешно връщане на сума за направено плащане.';
S_UnSuccessfulRefund                      : String = 'Неуспешно връщане на сума за направено плащане.';
S_RefundAmount                            : String = 'Връщане на сума';
S_AddPmntCardSum                          : String = 'Добавяне на сума към сметка';
S_UnsuccessfulAddSum                      : String = 'Неуспешно добавяне на сума към сметка.';
S_AddedAmount                             : String = 'Добавена сума';
S_NewAvSum                                : String = 'Нова наличност';
S_NoSumEntered                            : String = 'Няма зададена сума за добавяне!';
S_SumNotAvailableNoReturn                 : String = 'Отразяването на отстъпката с връщане на стоки'+#13+#10+
                                                     'е невъзможно и ще бъде отказано.';
S_MaxSumOfTaxGrp                          : String = 'Максимална сума от данъчна група';
S_SumToReturn                             : String = 'Сума за ВЪРНАТИ';
S_TblDoesNotExists                        : String = 'Несъществува сметка (маса) с номер';
S_TblNumbNotSelected                      : String = 'Не е посочен номер на сметка (маса).';
S_Monday                                  : String = 'Понеделник';
S_Tuesday                                 : String = 'Вторник';
S_Wednesday                               : String = 'Сряда';
S_Thursday                                : String = 'Четвъртък';
S_Friday                                  : String = 'Петък';
S_Saturday                                : String = 'Събота';
S_Sunday                                  : String = 'Неделя';
S_MenuFor                                 : String = 'Меню за';
S_NoActiveMenuToLoad                      : String = 'Няма активно меню за зареждане.';
S_OrderFor                                : String = 'Поръчка за';
S_NoPeriod                                : String = 'Не е зададен период.';
S_ListOfEndedBills                        : String = 'Списък приключени сметки';
S_EndedBills                              : String = 'Приключени сметки';
S_LoyaltyNotInstalled                     : String = 'Не е инсталирана система за лоялност.'+#13+
                                                     'Операцията се отменя.';
S_ConnectionToLoyaltySrvOK                : String = 'Успешна връзка със системата за лоялност.';
S_ConnectionToLoyaltySrvErr               : String = 'Неуспешно свързване със системата за лоялност';
S_NotSupportedFunctionLoyaltySrv          : String = 'Системата за лоялност не поддържа избраната функция.';
S_LoyaltyUnsuccesfulShowStatus            : String = 'Система за лоялност - неуспешно извеждане на статус';
S_LoyaltyUnsuccesfulRefundPayment         : String = 'Система за лоялност - неуспешно връщане на плащане';
S_LoyaltyUnsuccesfulShowReports           : String = 'Система за лоялност - неуспешно извеждане на отчети';
S_LoyaltyUnsuccesfulShowAdminInterface    : String = 'Система за лоялност - неуспешно извеждане на администраторски интерфейс';
S_LoyaltyUnsuccesfulShowAccount           : String = 'Система за лоялност - неуспешно извеждане информация за сметка';
S_LoyaltyUnsuccesfulLoadAccount           : String = 'Система за лоялност - грешка при зареждане на сметка';
S_LoyaltyErrorOnClearCustData             : String = 'Система за лоялност - неуспешно изчистване на клиентски данни';
S_LoyaltyErrorOnAddSum                    : String = 'СИСТЕМА ЗА ЛОЯЛНОСТ'+#13+#10+
                                                     'Неуспешно добавяне на сума';
S_LoyaltyErrorOnShowSettings              : String = 'Система за лоялност - неуспешно извеждане на настройки.';
S_BonAlreadyHaveCustChosenChangeIt        : String = 'Сметката вече има избран клиент.'+#13+#10+
                                                     'Желаете ли да го промените?';
S_AlreadyHaveCustFromLoyaltyChangeIt      : String = 'Вече има избрана сметка от системата за лоялност.'+#13+#10+
                                                     'Желаете ли да го смените?';
S_NoPrintOpenTblIfNoTbl                   : String = 'Не може да се направи разпечатка на сметка за обикновена касова бележка';
S_OperNotCashierCantSelectCustForBON      : String = 'Текущият оператор не е касиер и не може да зададе клиент за сметка.';
S_PlsSelectTbl                            : String = 'Моля, посочете сметка на маса.';
S_CommentTooLong_PrintFirst255            : String = 'Коментара за артикула надвишава'+#13+#10+
                                                     'максимално допустимата дължина.'+#13+#10+
                                                     'Ще бъдат разпечатани първите'+#13+#10+
                                                     '255 символа.';
S_WantToContinue                          : String = 'Желаете ли да продължите?';

// new
S_PluWorksWithIntegerQty                  : String = 'Артикулът работи само с цели количества.';
S_QtyWillNotBeChanged                     : String = 'Количеството няма да бъде променено.';
S_HaveOpenTblReportWillOnlyShown          : String = 'Отчетът няма да бъде нулиран, защото'+#13+#10+
                                                     'има отворени клиентски сметки!';
S_PaymentAlreadyRevoked                   : String = 'Плащането вече е войдирано.';
S_PaymentIsFromPaymentSystem              : String = 'Плащането е направено от '+#13+#10+
                                                     '"КАРТОВА СИСТЕМА ЗА РАЗПЛАЩАНЕ"!' +#13+#10+
                                                     'Необходимо е войдираната сума да' +#13+#10+
                                                     'бъде отразена и в нея.';
S_VOIDPayment                             : String = 'Войдиране на плащане';
S_PlsConfirm                              : String = 'Моля, потвърдете';
S_New                                     : String = 'ново';
S_ReportNOTZeroed                         : String = 'ОТЧЕТЪТ НЕ Е НУЛИРАН';
S_ErrOnExtractXLCustDiscount              : String = 'Система за лоялност - неуспешно извличане на клиентска отстъпка.';

S_SetQty                                  : String = 'Зададено количество';
S_ConfirmIt                               : String = 'Ще го потвърдите ли?';
S_KCashier                                : String = 'Касиер';
S_PlueDoesNotExists                       : String = 'Несъществува артикул с No';
S_ErrOnSearchPlu                          : String = 'Грешка при търсене на артикул';
S_ModuleCustMonitorDoesNotExist           : String = 'Модул "Клиентски монитор" (Presentation.exe)'+#13+#10+
                                                     'не може да бъде намерен в папката на ПОС-а.';
S_ErrorOnActivateModCustMonitor           : String = 'Грешка при активиране на модул "Клиентски монитор".';
S_CustMonitorModuleNoSettings             : String = 'Няма записани настройки за модул "Клиентски монитор".';
S_CustMonitorOK                           : String = 'Модул "Клиентски монитор" - инсталиран';
S_PluIsForbiddenForReturn                 : String = 'Забранен за връщане';
S_VoidEndBonDiscountOnVoidPayment         : String = 'Войдиране на отстъпка при край на бон след войд на плащане.';
S_SumAfterDiscount                        : String = 'Сума след отстъпка';
S_AftDisc                                 : String = 'След отст.';
S_CannotEndAsSystem                       : String = 'Нямате необходимите права да приключите бона служебно.'+#13+#10+
                                                     'Моля, променете профила.';
S_HDeliveryAddress                        : String = 'Адрес на доставка';
S_HDeliveryReceiver                       : String = 'Получател';
S_HDeliveryPhone                          : String = 'Телефон';
S_HDeliveryInfo                           : String = 'ИНФОРМАЦИЯ ЗА ДОСТАВКА';
// DK2


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
     GetValueFromIni(IniFile, 'MessagesCmn', 'ApplicationTitle',   S_ApplicationTitle);
     GetValueFromIni(IniFile, 'MessagesCmn', 'Version',            S_Version);
//     GetValueFromIni(IniFile, 'MessagesCmn', 'ModuleName',         S_ModuleName);   ne se prevejda weche
     GetValueFromIni(IniFile, 'MessagesCmn', 'RegDecimalError',    S_RegDecimalError);
     GetValueFromIni(IniFile, 'MessagesCmn', 'SettingsNotLoaded',  S_SettingsNotLoaded);

     // DK2
     GetValueFromIni(IniFile, 'MessagesLock', 'DK2NotRegistered1', S_DK2NotRegistered1);
     GetValueFromIni(IniFile, 'MessagesLock', 'DK2NotRegistered5', S_DK2NotRegistered5);
     GetValueFromIni(IniFile, 'MessagesLock', 'DK2NotRegistered6', S_DK2NotRegistered6);
     GetValueFromIni(IniFile, 'MessagesLock', 'DemoVGersion',      S_DemoVGersion);
     GetValueFromIni(IniFile, 'MessagesLock', 'DBErrorConnect',    S_DBErrorConnect);

     // Database
     GetValueFromIni(IniFile, 'MessagesDB', 'DBNotConnected',   S_DBNotConnected);
     GetValueFromIni(IniFile, 'MessagesDB', 'DBNoConection',    S_DBNoConection);
     GetValueFromIni(IniFile, 'MessagesDB', 'Server',           S_Server);
     GetValueFromIni(IniFile, 'MessagesDB', 'Local',            S_Local);
     GetValueFromIni(IniFile, 'MessagesDB', 'NoConnection',     S_NoConnection);
     GetValueFromIni(IniFile, 'MessagesDB', 'ServerOnLine',     S_ServerOnLine);
     GetValueFromIni(IniFile, 'MessagesDB', 'ServerOffLine',    S_ServerOffLine);
     GetValueFromIni(IniFile, 'MessagesDB', 'NetworkError',     S_NetworkError);
     GetValueFromIni(IniFile, 'MessagesDB', 'StopPrinters',     S_StopPrinters);

     // LOGIN  UNIT
     GetValueFromIni(IniFile, 'MessagesLogin', 'LogOut',         S_LogOut);
     GetValueFromIni(IniFile, 'MessagesLogin', 'LoginFail',      S_LoginFail);
     GetValueFromIni(IniFile, 'MessagesLogin', 'NotPermitions',  S_NotPermitions);
     GetValueFromIni(IniFile, 'MessagesLogin', 'HavePermitions', S_HavePermitions);
     GetValueFromIni(IniFile, 'MessagesLogin', 'Cashier',        S_Cashier);
     GetValueFromIni(IniFile, 'MessagesLogin', 'Administrator',  S_Administrator);
     GetValueFromIni(IniFile, 'MessagesLogin', 'User',           S_User);

     // Dialogs
     GetValueFromIni(IniFile, 'MessagesDlg', 'WantToExit',     S_WantToExit);
     GetValueFromIni(IniFile, 'MessagesDlg', 'MustEndBon',     S_MustEndBon);
     GetValueFromIni(IniFile, 'MessagesDlg', 'ChangePluPrice', S_ChangePluPrice);
     GetValueFromIni(IniFile, 'MessagesDlg', 'ChgPrcOldPrice', S_ChgPrcOldPrice);
     GetValueFromIni(IniFile, 'MessagesDlg', 'ChgPrcNewPrice', S_ChgPrcNewPrice);
     GetValueFromIni(IniFile, 'MessagesDlg', 'ChangePluQuant', S_ChangePluQuant);
     GetValueFromIni(IniFile, 'MessagesDlg', 'ChgQtyOldQuant', S_ChgQtyOldQuant);
     GetValueFromIni(IniFile, 'MessagesDlg', 'ChgQtyNewQuant', S_ChgQtyNewQuant);
     GetValueFromIni(IniFile, 'MessagesDlg', 'Confirm',        S_Confirm);

     // UDP
     GetValueFromIni(IniFile, 'MessagesUdp', 'UDP_StopModule',   S_UDP_StopModule);

     // Cashier Unit
     GetValueFromIni(IniFile, 'MessagesChr', 'NoActiveOperator', S_NoActiveOperator);
     GetValueFromIni(IniFile, 'MessagesChr', 'MustOpenTable',    S_MustOpenTable);


//DBPluUnit
     GetValueFromIni(IniFile, 'MessagesDbP', 'CustomerCardN',      S_CustomerCardN);
     GetValueFromIni(IniFile, 'MessagesDbP', 'Customer',           S_Customer);
     GetValueFromIni(IniFile, 'MessagesDbP', 'CustomerDiscount',   S_CustomerDiscount);
     GetValueFromIni(IniFile, 'MessagesDbP', 'Card',               S_Card);
     GetValueFromIni(IniFile, 'MessagesDbP', 'LeftSum',            S_LeftSum);
     GetValueFromIni(IniFile, 'MessagesDbP', 'TotalPayedSum',      S_TotalPayedSum);
     GetValueFromIni(IniFile, 'MessagesDbP', 'CustCardNotFound',   S_CustCardNotFound);
     GetValueFromIni(IniFile, 'MessagesDbP', 'NotFound',           S_NotFound);
     GetValueFromIni(IniFile, 'MessagesDbP', 'NotForThisStation',  S_NotForThisStation);
     GetValueFromIni(IniFile, 'MessagesDbP', 'SellForbidden',      S_SellForbidden);
     GetValueFromIni(IniFile, 'MessagesDbP', 'FracQuantForbidden', S_FracQuantForbidden);
     GetValueFromIni(IniFile, 'MessagesDbP', 'PluRfndForbidden',   S_PluRfndForbidden);
     GetValueFromIni(IniFile, 'MessagesDbP', 'PluRfndForbidden1',  S_PluRfndForbidden1);
     GetValueFromIni(IniFile, 'MessagesDbP', 'ZeroPriceForbidden', S_ZeroPriceForbidden);
     GetValueFromIni(IniFile, 'MessagesDbP', 'MinusPrice',         S_MinusPrice);
     GetValueFromIni(IniFile, 'MessagesDbP', 'NoQuantity',         S_NoQuantity);
     GetValueFromIni(IniFile, 'MessagesDbP', 'ConfirmClearBon',    S_ConfirmClearBon);
     GetValueFromIni(IniFile, 'MessagesDbP', 'SelPluIsNotRecipe',  S_SelPluIsNotRecipe);
     GetValueFromIni(IniFile, 'MessagesDbP', 'FreeOfCharge',       S_FreeOfCharge);
     GetValueFromIni(IniFile, 'MessagesDbP', 'FreeQuantity',       S_FreeQuantity);
     GetValueFromIni(IniFile, 'MessagesDbP', 'PromotionMsg',       S_PromotionMsg);
     GetValueFromIni(IniFile, 'MessagesDbP', 'PromotionMsg1',      S_PromotionMsg1);
     GetValueFromIni(IniFile, 'MessagesDbP', 'PromotionMsg2',      S_PromotionMsg2);

     // Bill Unit
     GetValueFromIni(IniFile, 'MessagesBill', 'TOTAL',                  S_TOTAL);
     GetValueFromIni(IniFile, 'MessagesBill', 'Resto',                  S_Resto);
     GetValueFromIni(IniFile, 'MessagesBill', 'SystemNumb',             S_SystemNumb);
     GetValueFromIni(IniFile, 'MessagesBill', 'Discount1',              S_Discount1);
     GetValueFromIni(IniFile, 'MessagesBill', 'Discount2',              S_Discount2);
     GetValueFromIni(IniFile, 'MessagesBill', 'Discount3',              S_Discount3);
     GetValueFromIni(IniFile, 'MessagesBill', 'Discount4',              S_Discount4);
     GetValueFromIni(IniFile, 'MessagesBill', 'Discount5',	        S_Discount5	);
     GetValueFromIni(IniFile, 'MessagesBill', 'Discount6',	        S_Discount6	);
     GetValueFromIni(IniFile, 'MessagesBill', 'DiscountError',	        S_DiscountError	);
     GetValueFromIni(IniFile, 'MessagesBill', 'AddOnError',	        S_AddOnError	);
     GetValueFromIni(IniFile, 'MessagesBill', 'BonFinal',	        S_BonFinal	);
     GetValueFromIni(IniFile, 'MessagesBill', 'StaroSaldo',	        S_StaroSaldo	);
     GetValueFromIni(IniFile, 'MessagesBill', 'NovoSaldo',	        S_NovoSaldo	);
     GetValueFromIni(IniFile, 'MessagesBill', 'OperStr',	        S_OperStr	);
     GetValueFromIni(IniFile, 'MessagesBill', 'Service',	        S_Service	);
     GetValueFromIni(IniFile, 'MessagesBill', 'ServicePLU',	        S_ServicePLU	);
     GetValueFromIni(IniFile, 'MessagesBill', 'ServiceComment',	        S_ServiceComment	);
     GetValueFromIni(IniFile, 'MessagesBill', 'CustomDiscPlu',	        S_CustomDiscPlu	);
     GetValueFromIni(IniFile, 'MessagesBill', 'CustomDiscComment',	S_CustomDiscComment	);
     GetValueFromIni(IniFile, 'MessagesBill', 'BillStr',	        S_BillStr	);
     GetValueFromIni(IniFile, 'MessagesBill', 'LastBon',	        S_LastBon	);
     GetValueFromIni(IniFile, 'MessagesBill', 'SumStr',	                S_SumStr	);
     GetValueFromIni(IniFile, 'MessagesBill', 'CustCount',	        S_CustCount	);
     GetValueFromIni(IniFile, 'MessagesBill', 'BillDiscountStr',	S_BillDiscountStr	);
     GetValueFromIni(IniFile, 'MessagesBill', 'PrefPriceStr',	        S_PrefPriceStr	);
     GetValueFromIni(IniFile, 'MessagesBill', 'BillAddOnStr',	        S_BillAddOnStr	);
     GetValueFromIni(IniFile, 'MessagesBill', 'ValueDiascountStr',	S_ValueDiascountStr	);
     GetValueFromIni(IniFile, 'MessagesBill', 'STLSumStr',	        S_STLSumStr	);
     GetValueFromIni(IniFile, 'MessagesBill', 'CustomerStr',	        S_CustomerStr	);
     GetValueFromIni(IniFile, 'MessagesBill', 'InvoiceStr',	        S_InvoiceStr	);
     GetValueFromIni(IniFile, 'MessagesBill', 'StockRecept',	        S_StockRecept	);
     GetValueFromIni(IniFile, 'MessagesBill', 'Terminal',	        S_Terminal	);
     GetValueFromIni(IniFile, 'MessagesBill', 'CloseTable',	        S_CloseTable	);
     GetValueFromIni(IniFile, 'MessagesBill', 'SaveTable',	        S_SaveTable	);
     GetValueFromIni(IniFile, 'MessagesBill', 'NotClosedTable',	        S_NotClosedTable	);
     GetValueFromIni(IniFile, 'MessagesBill', 'PrintTable',	        S_PrintTable	);
     GetValueFromIni(IniFile, 'MessagesBill', 'PrintedTable',	        S_PrintedTable	);
     GetValueFromIni(IniFile, 'MessagesBill', 'MiddleRecept',	        S_MiddleRecept	);
     GetValueFromIni(IniFile, 'MessagesBill', 'RevokedPlu',	        S_RevokedPlu	);
     GetValueFromIni(IniFile, 'MessagesBill', 'Value',                  S_Value         );
     GetValueFromIni(IniFile, 'MessagesBill', 'RevokedDiscount',	S_RevokedDiscount 	);
     GetValueFromIni(IniFile, 'MessagesBill', 'S_RevokedAddOn',	        S_RevokedAddOn	);
     GetValueFromIni(IniFile, 'MessagesBill', 'S_SaldoBeforeRev',	S_SaldoBeforeRev);
     GetValueFromIni(IniFile, 'MessagesBill', 'Start',	                S_Start	);
     GetValueFromIni(IniFile, 'MessagesBill', 'End',	                S_End	);
     GetValueFromIni(IniFile, 'MessagesBill', 'Revoked',	        S_Revoked	);
     GetValueFromIni(IniFile, 'MessagesBill', 'KitPrnRevokedPlu',	S_KitPrnRevokedPlu	);
     GetValueFromIni(IniFile, 'MessagesBill', 'BonStarted',	        S_BonStarted	        );
     GetValueFromIni(IniFile, 'MessagesBill', 'Subtotal',	        S_Subtotal              );
     GetValueFromIni(IniFile, 'MessagesBill', 'OpenTableRequired',	S_OpenTableRequired	);
     GetValueFromIni(IniFile, 'MessagesBill', 'OpenTable',	        S_OpenTable	);
     GetValueFromIni(IniFile, 'MessagesBill', 'ChgPriceForbidden',	S_ChgPriceForbidden	);
     GetValueFromIni(IniFile, 'MessagesBill', 'ChgPriceForbidden1',	S_ChgPriceForbidden1	);
     GetValueFromIni(IniFile, 'MessagesBill', 'ChgPriceForbidden2',	S_ChgPriceForbidden2	);
     GetValueFromIni(IniFile, 'MessagesBill', 'EditPluForbidden',	S_EditPluForbidden	);
     GetValueFromIni(IniFile, 'MessagesBill', 'VOIDForbidden',	        S_VOIDForbidden	        );
     GetValueFromIni(IniFile, 'MessagesBill', 'VOIDError',	        S_VOIDError	        );
     GetValueFromIni(IniFile, 'MessagesBill', 'VOID',	                S_VOID	                );
     GetValueFromIni(IniFile, 'MessagesBill', 'VOIDPluForbidden',	S_VOIDPluForbidden	);
     GetValueFromIni(IniFile, 'MessagesBill', 'VOIDConfirm',	        S_VOIDConfirm	        );
     GetValueFromIni(IniFile, 'MessagesBill', 'DiscountForbidden',	S_DiscountForbidden	);
     GetValueFromIni(IniFile, 'MessagesBill', 'SearchPluForbidden',	S_SearchPluForbidden	);
     GetValueFromIni(IniFile, 'MessagesBill', 'InvoiceForbidden',	S_InvoiceForbidden	);
     GetValueFromIni(IniFile, 'MessagesBill', 'InvToBon_EnterBonID',	S_InvToBon_EnterBonID	);
     GetValueFromIni(IniFile, 'MessagesBill', 'InvToBon_BonNotFound',	S_InvToBon_BonNotFound	);
     GetValueFromIni(IniFile, 'MessagesBill', 'InvToBon_BonIsOld',	S_InvToBon_BonIsOld	);
     GetValueFromIni(IniFile, 'MessagesBill', 'InvToBon_ConfirmCopy',	S_InvToBon_ConfirmCopy	);
     GetValueFromIni(IniFile, 'MessagesBill', 'InvToBon_ConfirmNew',	S_InvToBon_ConfirmNew	);
     GetValueFromIni(IniFile, 'MessagesBill', 'InvToBon_PrintCopy',	S_InvToBon_PrintCopy	);
     GetValueFromIni(IniFile, 'MessagesBill', 'InvToBon_PrintInv',	S_InvToBon_PrintInv	);
     GetValueFromIni(IniFile, 'MessagesBill', 'FunctionForbidden',	S_FunctionForbidden	);
     GetValueFromIni(IniFile, 'MessagesBill', 'TableIsBussy',	        S_TableIsBussy	        );
     GetValueFromIni(IniFile, 'MessagesBill', 'TableIsLocked',          S_TableIsLocked         );
     GetValueFromIni(IniFile, 'MessagesBill', 'TableIsLocked1',         S_TableIsLocked1        );
     GetValueFromIni(IniFile, 'MessagesBill', 'TableNotExist',	        S_TableNotExist	        );
     GetValueFromIni(IniFile, 'MessagesBill', 'PrnEndTableQuestion',	S_PrnEndTableQuestion	);
     GetValueFromIni(IniFile, 'MessagesBill', 'PrnEndBonQuestion',	S_PrnEndBonQuestion	);
     GetValueFromIni(IniFile, 'MessagesBill', 'CustomerCount',	        S_CustomerCount	        );
     GetValueFromIni(IniFile, 'MessagesBill', 'ChangeServiceQuest',	S_ChangeServiceQuest	);
     GetValueFromIni(IniFile, 'MessagesBill', 'RemoveServiceQuest',	S_RemoveServiceQuest	);
     GetValueFromIni(IniFile, 'MessagesBill', 'AddServiceQuest',	S_AddServiceQuest	);
     GetValueFromIni(IniFile, 'MessagesBill', 'BillChecksumError',	S_BillChecksumError	);

     // Main Unit
     GetValueFromIni(IniFile, 'MessagesMain', 'CanNotOpenTable',	S_CanNotOpenTable	);
     GetValueFromIni(IniFile, 'MessagesMain', 'PrinterError',	        S_PrinterError	);
     GetValueFromIni(IniFile, 'MessagesMain', 'NotOpenTable',	        S_NotOpenTable	);
     GetValueFromIni(IniFile, 'MessagesMain', 'NothingMarked',	        S_NothingMarked	);
     GetValueFromIni(IniFile, 'MessagesMain', 'WrongPayType',	        S_WrongPayType	);
     GetValueFromIni(IniFile, 'MessagesMain', 'ClearBillDialog',	S_ClearBillDialog	);
     GetValueFromIni(IniFile, 'MessagesMain', 'ClearBeforeClose',	S_ClearBeforeClose	);
     GetValueFromIni(IniFile, 'MessagesMain', 'CanNotClearTable',	S_CanNotClearTable	);
     GetValueFromIni(IniFile, 'MessagesMain', 'PluRecipe',	        S_PluRecipe	);
     GetValueFromIni(IniFile, 'MessagesMain', 'PluInfo',	        S_PluInfo	);
     GetValueFromIni(IniFile, 'MessagesMain', 'BonInfo',	        S_BonInfo	);
     GetValueFromIni(IniFile, 'MessagesMain', 'CardInfo',	        S_CardInfo	);
     GetValueFromIni(IniFile, 'MessagesMain', 'ReportError',	        S_ReportError	);
     GetValueFromIni(IniFile, 'MessagesMain', 'Plu',	                S_Plu	);
     GetValueFromIni(IniFile, 'MessagesMain', 'YES',	                S_YES	);
     GetValueFromIni(IniFile, 'MessagesMain', 'NO',	                S_NO	);
     GetValueFromIni(IniFile, 'MessagesMain', 'SysErrors',	        S_SysErrors	);
     GetValueFromIni(IniFile, 'MessagesMain', 'PrnErrors',	        S_PrnErrors	);
     GetValueFromIni(IniFile, 'MessagesMain', 'Printer',	        S_Printer	);
     GetValueFromIni(IniFile, 'MessagesMain', 'ScannerOK',	        S_ScannerOK	);
     GetValueFromIni(IniFile, 'MessagesMain', 'ScaleOK',	        S_ScaleOK	);
     GetValueFromIni(IniFile, 'MessagesMain', 'DisplcayOK',	        S_DisplcayOK	);
     GetValueFromIni(IniFile, 'MessagesMain', 'PaymentServerOK',	S_PaymentServerOK	);
     GetValueFromIni(IniFile, 'MessagesMain', 'LoyaltySystemOK',	S_LoyaltySystemOK	);
     GetValueFromIni(IniFile, 'MessagesMain', 'NotInstalled',	        S_NotInstalled	);
     GetValueFromIni(IniFile, 'MessagesMain', 'PrintProfile',	        S_PrintProfile	);
     GetValueFromIni(IniFile, 'MessagesMain', 'NoPrinterInstalled',	S_NoPrinterInstalled	);
     GetValueFromIni(IniFile, 'MessagesMain', 'WrongPrinterNumb',	S_WrongPrinterNumb	);
     GetValueFromIni(IniFile, 'MessagesMain', 'PrinterIsBussy',	        S_PrinterIsBussy	);
     GetValueFromIni(IniFile, 'MessagesMain', 'ErrorInPrinterTime',	S_ErrorInPrinterTime	);
     GetValueFromIni(IniFile, 'MessagesMain', 'PrinterTimeIsOk',	S_PrinterTimeIsOk	);
     GetValueFromIni(IniFile, 'MessagesMain', 'PrinterNotResponding',	S_PrinterNotResponding	);
     GetValueFromIni(IniFile, 'MessagesMain', 'CardPayNotInstalled',	S_CardPayNotInstalled	);
     GetValueFromIni(IniFile, 'MessagesMain', 'DisplayNotInstalled',	S_DisplayNotInstalled	);

     // Card payment
     GetValueFromIni(IniFile, 'MessagesCrd', 'FailToGetCardData',	S_FailToGetCardData	);
     GetValueFromIni(IniFile, 'MessagesCrd', 'PrefPriceActive',	        S_PrefPriceActive	);
     GetValueFromIni(IniFile, 'MessagesCrd', 'PrefPriceNotAllowed',	S_PrefPriceNotAllowed	);
     GetValueFromIni(IniFile, 'MessagesCrd', 'PrefPriceStarted',	S_PrefPriceStarted	);
     GetValueFromIni(IniFile, 'MessagesCrd', 'PrefPriceStartedShrt',	S_PrefPriceStartedShrt	);
     GetValueFromIni(IniFile, 'MessagesCrd', 'CardHaveNoDiscount',	S_CardHaveNoDiscount	);
     GetValueFromIni(IniFile, 'MessagesCrd', 'CardDiscountStarted',	S_CardDiscountStarted	);
     GetValueFromIni(IniFile, 'MessagesCrd', 'CardDiscountCurrent',	S_CardDiscountCurrent	);
     GetValueFromIni(IniFile, 'MessagesCrd', 'CardDiscountNew',	        S_CardDiscountNew	);
     GetValueFromIni(IniFile, 'MessagesCrd', 'CardDiscountChange',	S_CardDiscountChange	);
     GetValueFromIni(IniFile, 'MessagesCrd', 'PaymentRefused',	        S_PaymentRefused	);
     GetValueFromIni(IniFile, 'MessagesCrd', 'Payment',	                S_Payment	);
     GetValueFromIni(IniFile, 'MessagesCrd', 'PayType',	                S_PayType	);
     GetValueFromIni(IniFile, 'MessagesCrd', 'CardOwner',	        S_CardOwner	);
     GetValueFromIni(IniFile, 'MessagesCrd', 'CardCategory',	        S_CardCategory	);
     GetValueFromIni(IniFile, 'MessagesCrd', 'CardLogNumb',	        S_CardLogNumb	);
     GetValueFromIni(IniFile, 'MessagesCrd', 'CardPhisNumb',	        S_CardPhisNumb	);
     GetValueFromIni(IniFile, 'MessagesCrd', 'CardTotalPayed1',	        S_CardTotalPayed1	);
     GetValueFromIni(IniFile, 'MessagesCrd', 'CardTotalPayed2',	        S_CardTotalPayed2	);
     GetValueFromIni(IniFile, 'MessagesCrd', 'CardLeftSum1',	        S_CardLeftSum1	);
     GetValueFromIni(IniFile, 'MessagesCrd', 'CardLeftSum2',	        S_CardLeftSum2	);
     GetValueFromIni(IniFile, 'MessagesCrd', 'CardDiscount',	        S_CardDiscount	);
     GetValueFromIni(IniFile, 'MessagesCrd', 'CardPrefPrice',	        S_CardPrefPrice	);
     GetValueFromIni(IniFile, 'MessagesCrd', 'CardPrefOverprice',	S_CardPrefOverprice	);
     GetValueFromIni(IniFile, 'MessagesCrd', 'PaymentStarted',	        S_PaymentStarted	);
     GetValueFromIni(IniFile, 'MessagesCrd', 'EnterOperatorNumb',	S_EnterOperatorNumb	);

// Split/merge Bills
     GetValueFromIni(IniFile, 'MessagesSMBills', 'SplitTblConfirm1',	S_SplitTblConfirm1	);
     GetValueFromIni(IniFile, 'MessagesSMBills', 'SplitTblConfirm2',	S_SplitTblConfirm2	);
     GetValueFromIni(IniFile, 'MessagesSMBills', 'SplitTblError',	S_SplitTblError	);
     GetValueFromIni(IniFile, 'MessagesSMBills', 'MergeTblError',	S_MergeTblError	);
     GetValueFromIni(IniFile, 'MessagesSMBills', 'SplitErrHaveNewPlu',	S_SplitErrHaveNewPlu	);
     GetValueFromIni(IniFile, 'MessagesSMBills', 'SplitErrHavePayment',	S_SplitErrHavePayment	);
     GetValueFromIni(IniFile, 'MessagesSMBills', 'ConfirmSplit',	S_ConfirmSplit	);
     GetValueFromIni(IniFile, 'MessagesSMBills', 'SplitBillEvent',	S_SplitBillEvent	);
     GetValueFromIni(IniFile, 'MessagesSMBills', 'MergeBillEvent',	S_MergeBillEvent	);
     GetValueFromIni(IniFile, 'MessagesSMBills', 'SplitBillMessage',	S_SplitBillMessage	);
     GetValueFromIni(IniFile, 'MessagesSMBills', 'MergeBillMessage',	S_MergeBillMessage	);

// Old Bills Unit
     GetValueFromIni(IniFile, 'MessagesOldBills', 'AscToReprintFiscal1',	S_AscToReprintFiscal1	);
     GetValueFromIni(IniFile, 'MessagesOldBills', 'AscToReprintFiscal2',	S_AscToReprintFiscal2	);
     GetValueFromIni(IniFile, 'MessagesOldBills', 'AscToReprintComment',	S_AscToReprintComment	);
     GetValueFromIni(IniFile, 'MessagesOldBills', 'AscToReprintInvoice',	S_AscToReprintInvoice	);
     GetValueFromIni(IniFile, 'MessagesOldBills', 'ReprintInvoiceFail',	        S_ReprintInvoiceFail	);
     GetValueFromIni(IniFile, 'MessagesOldBills', 'ReprintInvoiceFail1',	S_ReprintInvoiceFail1	);

// TXT REPORTS UNIT
     GetValueFromIni(IniFile, 'MessagesTxtRep', 'SetReportsToZero',	S_SetReportsToZero	);
     GetValueFromIni(IniFile, 'MessagesTxtRep', 'EndOfDayConfirm',	S_EndOfDayConfirm	);
     GetValueFromIni(IniFile, 'MessagesTxtRep', 'EndOfDayTblConfirm',	S_EndOfDayTblConfirm    );
     GetValueFromIni(IniFile, 'MessagesTxtRep', 'EndOfDayOperConfirm',	S_EndOfDayOperConfirm	);
     GetValueFromIni(IniFile, 'MessagesTxtRep', 'EndOfDayOperTblConfirm',S_EndOfDayOperTblConfirm);
     GetValueFromIni(IniFile, 'MessagesTxtRep', 'PrintEodFiscalConf',	S_PrintEodFiscalConf	);
     GetValueFromIni(IniFile, 'MessagesTxtRep', 'GetTotalReport',	S_GetTotalReport	);
     GetValueFromIni(IniFile, 'MessagesTxtRep', 'GetOperSumsReport',	S_GetOperSumsReport	);
     GetValueFromIni(IniFile, 'MessagesTxtRep', 'GetFullReport',	S_GetFullReport	);
     GetValueFromIni(IniFile, 'MessagesTxtRep', 'GetPrewTotalReport',	S_GetPrewTotalReport	);

// Search PLU Unit
     GetValueFromIni(IniFile, 'MessagesSrPlu', 'AllPlues',	S_AllPlues	);
     GetValueFromIni(IniFile, 'MessagesSrPlu', 'Close',	        S_Close	);
     GetValueFromIni(IniFile, 'MessagesSrPlu', 'Back',	        S_Back	);

// ИНЖОИЦЕ
     GetValueFromIni(IniFile, 'MessagesInv', 'ErrorLocateCustData',	S_ErrorLocateCustData	);
     GetValueFromIni(IniFile, 'MessagesInv', 'CustomerNotFound',	S_CustomerNotFound	);
     GetValueFromIni(IniFile, 'MessagesInv', 'TooManyCustomers',	S_TooManyCustomers	);
     GetValueFromIni(IniFile, 'MessagesInv', 'RequiredField1',	        S_RequiredField1	);
     GetValueFromIni(IniFile, 'MessagesInv', 'RequiredField2',	        S_RequiredField2	);
     GetValueFromIni(IniFile, 'MessagesInv', 'InvalidValue',	        S_InvalidValue	);
     GetValueFromIni(IniFile, 'MessagesInv', 'BulstatStr',	        S_BulstatStr	);
     GetValueFromIni(IniFile, 'MessagesInv', 'TaxNumbStr',	        S_TaxNumbStr	);
     GetValueFromIni(IniFile, 'MessagesInv', 'BulstatAlreadyExist',	S_BulstatAlreadyExist	);
     GetValueFromIni(IniFile, 'MessagesInv', 'AddCustomerError',	S_AddCustomerError	);
     GetValueFromIni(IniFile, 'MessagesInv', 'EditCustomerError',	S_EditCustomerError	);
     GetValueFromIni(IniFile, 'MessagesInv', 'PrintInvoiceError',	S_PrintInvoiceError	);
     GetValueFromIni(IniFile, 'MessagesInv', 'MOL',	S_MOL	);

     GetValueFromIni(IniFile, 'Messages', 'EnterSumBefore',	S_EnterSumBefore	);
     GetValueFromIni(IniFile, 'Messages', 'ServiceSumIn',	S_ServiceSumIn	);
     GetValueFromIni(IniFile, 'Messages', 'ServiceSumOut',	S_ServiceSumOut	);
     GetValueFromIni(IniFile, 'Messages', 'ServiceSumInOK',	S_ServiceSumInOK	);
     GetValueFromIni(IniFile, 'Messages', 'ServiceSumOutOK',	S_ServiceSumOutOK	);
     GetValueFromIni(IniFile, 'Messages', 'SetDatabeseConn',	S_SetDatabeseConn	);
     GetValueFromIni(IniFile, 'Messages', 'ChangeWorkStorage',	S_ChangeWorkStorage	);
     GetValueFromIni(IniFile, 'Messages', 'SystemSettings',	S_SystemSettings	);
     GetValueFromIni(IniFile, 'Messages', 'StoragesList',	S_StoragesList	);
     GetValueFromIni(IniFile, 'Messages', 'Number',	        S_Number	);
     GetValueFromIni(IniFile, 'Messages', 'Name',	        S_Name	);
     GetValueFromIni(IniFile, 'Messages', 'Comment',	        S_Comment	);
     GetValueFromIni(IniFile, 'Messages', 'CurrentStorageNumb',	S_CurrentStorageNumb	);
     GetValueFromIni(IniFile, 'Messages', 'StorageStr1',	S_StorageStr1	);
     GetValueFromIni(IniFile, 'Messages', 'StorageStr2',	S_StorageStr2	);
     GetValueFromIni(IniFile, 'Messages', 'StorageStr3',	S_StorageStr3	);
     GetValueFromIni(IniFile, 'Messages', 'StorageStr4',	S_StorageStr4	);
     GetValueFromIni(IniFile, 'Messages', 'StorageStr5',	S_StorageStr5	);
     GetValueFromIni(IniFile, 'Messages', 'StorageStr6',	S_StorageStr6	);

// Buttons
     GetValueFromIni(IniFile, 'MessagesBtn', 'BtnYes',	        S_BtnYes	);
     GetValueFromIni(IniFile, 'MessagesBtn', 'BtnNo',	        S_BtnNo	);
     GetValueFromIni(IniFile, 'MessagesBtn', 'BtnOK',	        S_BtnOK	);
     GetValueFromIni(IniFile, 'MessagesBtn', 'BtnCancel',	S_BtnCancel	);
     GetValueFromIni(IniFile, 'MessagesBtn', 'BtnAbort',	S_BtnAbort	);
     GetValueFromIni(IniFile, 'MessagesBtn', 'BtnRetry',	S_BtnRetry	);
     GetValueFromIni(IniFile, 'MessagesBtn', 'BtnIgnore',	S_BtnIgnore	);

// Txt Reports
     GetValueFromIni(IniFile, 'MessagesTxtRep', 'TxtRep_SepBillInfo',	S_TxtRep_SepBillInfo	);
     GetValueFromIni(IniFile, 'MessagesTxtRep', 'TxtRep_Table',	        S_TxtRep_Table	);
     GetValueFromIni(IniFile, 'MessagesTxtRep', 'TxtRep_SysNumber',	S_TxtRep_SysNumber	);
     GetValueFromIni(IniFile, 'MessagesTxtRep', 'TxtRep_OrderCount',	S_TxtRep_OrderCount	);
     GetValueFromIni(IniFile, 'MessagesTxtRep', 'TxtRep_PLUCount',	S_TxtRep_PLUCount	);
     GetValueFromIni(IniFile, 'MessagesTxtRep', 'TxtRep_TotalSum',	S_TxtRep_TotalSum	);
     GetValueFromIni(IniFile, 'MessagesTxtRep', 'TxtRep_DiscountSum',	S_TxtRep_DiscountSum	);
     GetValueFromIni(IniFile, 'MessagesTxtRep', 'TxtRep_DiscountCount',	S_TxtRep_DiscountCount	);
     GetValueFromIni(IniFile, 'MessagesTxtRep', 'TxtRep_Terminal',	S_TxtRep_Terminal	);
     GetValueFromIni(IniFile, 'MessagesTxtRep', 'TxtRep_OperNumb',	S_TxtRep_OperNumb	);
     GetValueFromIni(IniFile, 'MessagesTxtRep', 'TxtRep_OperName',	S_TxtRep_OperName	);
     GetValueFromIni(IniFile, 'MessagesTxtRep', 'TxtRep_CustDiscount',	S_TxtRep_CustDiscount	);
     GetValueFromIni(IniFile, 'MessagesTxtRep', 'TxtRep_TBLOpen',	S_TxtRep_TBLOpen	);
     GetValueFromIni(IniFile, 'MessagesTxtRep', 'TxtRep_TBLClosed',	S_TxtRep_TBLClosed	);
     GetValueFromIni(IniFile, 'MessagesTxtRep', 'TxtRep_CustCount',	S_TxtRep_CustCount	);
     GetValueFromIni(IniFile, 'MessagesTxtRep', 'TxtRep_Service',	S_TxtRep_Service	);
     GetValueFromIni(IniFile, 'MessagesTxtRep', 'TxtRep_Comment',	S_TxtRep_Comment	);
     GetValueFromIni(IniFile, 'MessagesTxtRep', 'TxtRep_SepCard',	S_TxtRep_SepCard	);
     GetValueFromIni(IniFile, 'MessagesTxtRep', 'TxtRep_CardPhis',	S_TxtRep_CardPhis	);
     GetValueFromIni(IniFile, 'MessagesTxtRep', 'TxtRep_CardLog',	S_TxtRep_CardLog	);
     GetValueFromIni(IniFile, 'MessagesTxtRep', 'TxtRep_CardOwner',	S_TxtRep_CardOwner	);
     GetValueFromIni(IniFile, 'MessagesTxtRep', 'TxtRep_SepCompany',	S_TxtRep_SepCompany	);
     GetValueFromIni(IniFile, 'MessagesTxtRep', 'TxtRep_Bulstat',	S_TxtRep_Bulstat	);
     GetValueFromIni(IniFile, 'MessagesTxtRep', 'TxtRep_TaxNumb',	S_TxtRep_TaxNumb	);
     GetValueFromIni(IniFile, 'MessagesTxtRep', 'TxtRep_FirmName',	S_TxtRep_FirmName	);
     GetValueFromIni(IniFile, 'MessagesTxtRep', 'TxtRep_Town',	        S_TxtRep_Town	);
     GetValueFromIni(IniFile, 'MessagesTxtRep', 'TxtRep_Address',	S_TxtRep_Address	);
     GetValueFromIni(IniFile, 'MessagesTxtRep', 'TxtRep_MOL',	        S_TxtRep_MOL	);
     GetValueFromIni(IniFile, 'MessagesTxtRep', 'TxtRep_Discount',	S_TxtRep_Discount	);
     GetValueFromIni(IniFile, 'MessagesTxtRep', 'TxtRep_SepPlues',	S_TxtRep_SepPlues	);
     GetValueFromIni(IniFile, 'MessagesTxtRep', 'TxtRep_PluAddOn',	S_TxtRep_PluAddOn	);
     GetValueFromIni(IniFile, 'MessagesTxtRep', 'TxtRep_PluDiscount',	S_TxtRep_PluDiscount	);

     GetValueFromIni(IniFile, 'MessagesTxtRep', 'TxtRep_SepReport',	S_TxtRep_SepReport	);
     GetValueFromIni(IniFile, 'MessagesTxtRep', 'TxtRep_TermNumb',	S_TxtRep_TermNumb	);
     GetValueFromIni(IniFile, 'MessagesTxtRep', 'TxtRep_TermName',	S_TxtRep_TermName	);
     GetValueFromIni(IniFile, 'MessagesTxtRep', 'TxtRep_StartRep',	S_TxtRep_StartRep	);
     GetValueFromIni(IniFile, 'MessagesTxtRep', 'TxtRep_EndRep',	S_TxtRep_EndRep	);
     GetValueFromIni(IniFile, 'MessagesTxtRep', 'TxtRep_SellsCount',	S_TxtRep_SellsCount	);
     GetValueFromIni(IniFile, 'MessagesTxtRep', 'TxtRep_TablesCount',	S_TxtRep_TablesCount	);
     GetValueFromIni(IniFile, 'MessagesTxtRep', 'TxtRep_TotalRepSum',	S_TxtRep_TotalRepSum	);

     GetValueFromIni(IniFile, 'MessagesTxtRep', 'TxtRep_SepCashiers',	S_TxtRep_SepCashiers	);
     GetValueFromIni(IniFile, 'MessagesTxtRep', 'TxtRep_SepCashier', S_TxtRep_SepCashier);

     GetValueFromIni(IniFile, 'MessagesTxtRep', 'TxtRep_ZreportDone',	S_TxtRep_ZreportDone    );
     GetValueFromIni(IniFile, 'MessagesTxtRep', 'TxtRep_Operator',	S_TxtRep_Operator       );

     GetValueFromIni(IniFile, 'Messages', 'TerminalZReport',	        S_TerminalZReport	);
     GetValueFromIni(IniFile, 'Messages', 'TerminalZReportTbl',	        S_TerminalZReportTbl    );
     GetValueFromIni(IniFile, 'Messages', 'OperatorZReportTbl',	        S_OperatorZReportTbl    );
     GetValueFromIni(IniFile, 'Messages', 'StockTemplateNotFound',	S_StockTemplateNotFound	);
     GetValueFromIni(IniFile, 'Messages', 'StockCopy_ConfirmNew',	S_StockCopy_ConfirmNew	);
     GetValueFromIni(IniFile, 'Messages', 'InvalidSerialNumber',        S_InvalidSerialNumber   );
     GetValueFromIni(IniFile, 'Messages', 'InvalidStorageNumber',       S_InvalidStorageNumber  );

     GetValueFromIni(IniFile, 'MessagesDateTime', 'S_SelectDate_SelDate',       S_SelectDate_SelDate       );
     GetValueFromIni(IniFile, 'MessagesDateTime', 'S_SelectDate_SelTime',       S_SelectDate_SelTime       );
     GetValueFromIni(IniFile, 'MessagesDateTime', 'S_SelectDate_SelDateTime',   S_SelectDate_SelDateTime   );
     GetValueFromIni(IniFile, 'MessagesDateTime', 'S_SelectDate_SelPeriodDate', S_SelectDate_SelPeriodDate );
     GetValueFromIni(IniFile, 'MessagesDateTime', 'S_SelectDate_SelPeriodTime', S_SelectDate_SelPeriodTime );
     GetValueFromIni(IniFile, 'MessagesDateTime', 'S_SelectDate_SelPeriodDtTm', S_SelectDate_SelPeriodDtTm );
     GetValueFromIni(IniFile, 'MessagesDateTime', 'S_SelectDate_Date',          S_SelectDate_Date          );
     GetValueFromIni(IniFile, 'MessagesDateTime', 'S_SelectDate_Time',          S_SelectDate_Time          );
     GetValueFromIni(IniFile, 'MessagesDateTime', 'S_SelectDate_DateTime',      S_SelectDate_DateTime      );
     GetValueFromIni(IniFile, 'MessagesDateTime', 'S_SelectDate_FromDate',      S_SelectDate_FromDate      );
     GetValueFromIni(IniFile, 'MessagesDateTime', 'S_SelectDate_ToDate',        S_SelectDate_ToDate        );
     GetValueFromIni(IniFile, 'MessagesDateTime', 'S_SelectDate_FromHour',      S_SelectDate_FromHour      );
     GetValueFromIni(IniFile, 'MessagesDateTime', 'S_SelectDate_ToHour',        S_SelectDate_ToHour        );
     GetValueFromIni(IniFile, 'MessagesDateTime', 'S_SelectDate_FromDtTm',      S_SelectDate_FromDtTm      );
     GetValueFromIni(IniFile, 'MessagesDateTime', 'S_SelectDate_ToDtTm',        S_SelectDate_ToDtTm        );

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

     GetValueFromIni(IniFile, 'Messages', 'UnsuccessfulScaleInit',              S_UnsuccessfulScaleInit);
     GetValueFromIni(IniFile, 'Messages', 'UnsuccessfulDisplayInit',            S_UnsuccessfulDisplayInit);
     GetValueFromIni(IniFile, 'Messages', 'UnsuccessfulScannerInit',            S_UnsuccessfulScannerInit);
     GetValueFromIni(IniFile, 'Messages', 'UnsuccessfulPrinterInit',            S_UnsuccessfulPrinterInit);
     GetValueFromIni(IniFile, 'Messages', 'PleaseCheckHWDevice',                S_PleaseCheckHWDevice);


     GetValueFromIni(IniFile, 'Messages', 'ModuleNotRegistered_StopWork',       S_ModuleNotRegistered_StopWork);
     GetValueFromIni(IniFile, 'Messages', 'CommandUnknown',                     S_CommandUnknown);
     GetValueFromIni(IniFile, 'Messages', 'ErrorOnWritingEvent',                S_ErrorOnWritingEvent);
     GetValueFromIni(IniFile, 'Messages', 'MessageAdded',                       S_MessageAdded);
     GetValueFromIni(IniFile, 'Messages', 'WatchSetRight',                      S_WatchSetRight);
     GetValueFromIni(IniFile, 'Messages', 'ErrOnSetWatch',                      S_ErrOnSetWatch);
     GetValueFromIni(IniFile, 'Messages', 'InCorrectTimeFormat',                S_InCorrectTimeFormat);
     GetValueFromIni(IniFile, 'Messages', 'Refreshed',                          S_Refreshed);
     GetValueFromIni(IniFile, 'Messages', 'POSRestDoesNotSupportCommand',       S_POSRestDoesNotSupportCommand);
     GetValueFromIni(IniFile, 'Messages', 'Reloaded',                           S_Reloaded);
     GetValueFromIni(IniFile, 'Messages', 'WorkingStorageChanged',              S_WorkingStorageChanged);
     GetValueFromIni(IniFile, 'Messages', 'ErrOnChangingStoragaNumb',           S_ErrOnChangingStoragaNumb);
     GetValueFromIni(IniFile, 'Messages', 'WrongStorageNumb',                   S_WrongStorageNumb);
     GetValueFromIni(IniFile, 'Messages', 'ErrOnLoadingSystemNomenclature',     S_ErrOnLoadingSystemNomenclature);
     GetValueFromIni(IniFile, 'Messages', 'SeeErrorFileForDetails',             S_SeeErrorFileForDetails);
     GetValueFromIni(IniFile, 'Messages', 'CurrencySign',                       S_CurrencySign);
     GetValueFromIni(IniFile, 'Messages', 'ErrOnSavingCurrentInvoiceNumb',      S_ErrOnSavingCurrentInvoiceNumb);
     GetValueFromIni(IniFile, 'Messages', 'SeriousError',                       S_SeriousError);
     GetValueFromIni(IniFile, 'Messages', 'WrongModuleInit',                    S_WrongModuleInit);
     GetValueFromIni(IniFile, 'Messages', 'Code',                               S_Code);
     GetValueFromIni(IniFile, 'Messages', 'NameShortly',                        S_NameShortly);
     GetValueFromIni(IniFile, 'Messages', 'Quant',                              S_Quant);
     GetValueFromIni(IniFile, 'Messages', 'Portion',                            S_Portion);
     GetValueFromIni(IniFile, 'Messages', 'ECRName',                            S_ECRName);
     GetValueFromIni(IniFile, 'Messages', 'ForeignName',                        S_ForeignName);
     GetValueFromIni(IniFile, 'Messages', 'NomenclCode',                        S_NomenclCode);
     GetValueFromIni(IniFile, 'Messages', 'ScaleCode',                          S_ScaleCode);
     GetValueFromIni(IniFile, 'Messages', 'MaxAddOn',                           S_MaxAddOn);
     GetValueFromIni(IniFile, 'Messages', 'MaxDiscount',                        S_MaxDiscount);
     GetValueFromIni(IniFile, 'Messages', 'MinReserve',                         S_MinReserve);
     GetValueFromIni(IniFile, 'Messages', 'MaxReserve',                         S_MaxReserve);
     GetValueFromIni(IniFile, 'Messages', 'ForPOS',                             S_ForPOS);
     GetValueFromIni(IniFile, 'Messages', 'Measure',                            S_Measure);
     GetValueFromIni(IniFile, 'Messages', 'PrintingGroup',                      S_PrintingGroup);
     GetValueFromIni(IniFile, 'Messages', 'TaxGroup',                           S_TaxGroup);
     GetValueFromIni(IniFile, 'Messages', 'Tax',                                S_Tax);
     GetValueFromIni(IniFile, 'Messages', 'PluGroup',                           S_PluGroup);
     GetValueFromIni(IniFile, 'Messages', 'BuyPrice',                           S_BuyPrice);
     GetValueFromIni(IniFile, 'Messages', 'TradePrice',                         S_TradePrice);
     GetValueFromIni(IniFile, 'Messages', 'SellPrice',                          S_SellPrice);
     GetValueFromIni(IniFile, 'Messages', 'ForbidenSaleInMinus',                S_ForbidenSaleInMinus);
     GetValueFromIni(IniFile, 'Messages', 'Promotional',                        S_Promotional);
     GetValueFromIni(IniFile, 'Messages', 'Recipe',                             S_Recipe);
     GetValueFromIni(IniFile, 'Messages', 'OnlyIntQuant',                       S_OnlyIntQuant);
     GetValueFromIni(IniFile, 'Messages', 'SaleOnZeroPrice',                    S_SaleOnZeroPrice);
     GetValueFromIni(IniFile, 'Messages', 'ChangePriceOnSale',                  S_ChangePriceOnSale);
     GetValueFromIni(IniFile, 'Messages', 'Barcode',                            S_Barcode);
     GetValueFromIni(IniFile, 'Messages', 'Used',                               S_Used);
     GetValueFromIni(IniFile, 'Messages', 'Barcodes',                           S_Barcodes);
     GetValueFromIni(IniFile, 'Messages', 'StorageName',                        S_StorageName);
     GetValueFromIni(IniFile, 'Messages', 'Quantity',                           S_Quantity);
     GetValueFromIni(IniFile, 'Messages', 'Availabilities',                     S_Availabilities);
     GetValueFromIni(IniFile, 'Messages', 'Priority',                           S_Priority);
     GetValueFromIni(IniFile, 'Messages', 'Days',                               S_Days);
     GetValueFromIni(IniFile, 'Messages', 'Promotions',                         S_Promotions);
     GetValueFromIni(IniFile, 'Messages', 'NoConnectionToPrinterForInvoices',   S_NoConnectionToPrinterForInvoices);
     GetValueFromIni(IniFile, 'Messages', 'ErrOnLoadingPluesToMerge',           S_ErrOnLoadingPluesToMerge);
     GetValueFromIni(IniFile, 'Messages', 'Count',                              S_Count);
     GetValueFromIni(IniFile, 'Messages', 'RevokeLastBonQuest ',                S_RevokeLastBonQuest );
     GetValueFromIni(IniFile, 'Messages', 'RevokeLastBonSucceed',               S_RevokeLastBonSucceed);
     GetValueFromIni(IniFile, 'Messages', 'RevokeLastBonFail ',                 S_RevokeLastBonFail);
     GetValueFromIni(IniFile, 'Messages', 'RevokeLastNotFound',                 S_RevokeLastNotFound);
     GetValueFromIni(IniFile, 'Messages', 'RevokeLastAlreadyDone',              S_RevokeLastAlreadyDone);
     GetValueFromIni(IniFile, 'Messages', 'BonOldLoaded',                       S_BonOldLoaded);

     GetValueFromIni(IniFile, 'Messages', 'ItemReturnVeryOld',                S_ItemReturnVeryOld);
     GetValueFromIni(IniFile, 'Messages', 'ItemReturnConfirm',                S_ItemReturnConfirm);
     GetValueFromIni(IniFile, 'Messages', 'ItemReturnOK',                     S_ItemReturnOK);
     GetValueFromIni(IniFile, 'Messages', 'ItemReturnFail',                   S_ItemReturnFail);
     GetValueFromIni(IniFile, 'Messages', 'CannotReturnService',              S_CannotReturnService);
     GetValueFromIni(IniFile, 'Messages', 'ItemReturnAlreadyDone',            S_ItemReturnAlreadyDone);
     GetValueFromIni(IniFile, 'Messages', 'ItemReturn',                       S_ItemReturn);
     GetValueFromIni(IniFile, 'Messages', 'ItemBuyDate',                      S_ItemBuyDate);
     GetValueFromIni(IniFile, 'Messages', 'ChangeQuantForbiden',              S_ChangeQuantForbiden);
     GetValueFromIni(IniFile, 'Messages', 'PluIsPromotional',                 S_PluIsPromotional);
     GetValueFromIni(IniFile, 'Messages', 'PluQuantNegative',                 S_PluQuantNegative);
     GetValueFromIni(IniFile, 'Messages', 'UnCorrectConfigOfReport',          S_UnCorrectConfigOfReport);
     GetValueFromIni(IniFile, 'Messages', 'SelectedTermNumberIsCurrentNumb',  S_SelectedTermNumberIsCurrentNumb);
     GetValueFromIni(IniFile, 'Messages', 'TermShares_NoExport',              S_TermShares_NoExport);
     GetValueFromIni(IniFile, 'Messages', 'NoSalesForTerminal',               S_NoSalesForTerminal);
     GetValueFromIni(IniFile, 'Messages', 'NoExportAccount',                  S_NoExportAccount);
     GetValueFromIni(IniFile, 'Messages', 'SuccessfulAccountExport',          S_SuccessfulAccountExport);
     GetValueFromIni(IniFile, 'Messages', 'ExportForbidden',                  S_ExportForbidden);

     GetValueFromIni(IniFile, 'Messages', 'TxtRep_ReturnedTitle',             S_TxtRep_ReturnedTitle);
     GetValueFromIni(IniFile, 'Messages', 'TxtRep_ReturnedCount',             S_TxtRep_ReturnedCount);
     GetValueFromIni(IniFile, 'Messages', 'TxtRep_ReturnedTotal',             S_TxtRep_ReturnedTotal);
     GetValueFromIni(IniFile, 'Messages', 'TxtRep_ReturnedPayType',           S_TxtRep_ReturnedPayType);
     GetValueFromIni(IniFile, 'Messages', 'TxtRep_RevokedTitle',              S_TxtRep_RevokedTitle);
     GetValueFromIni(IniFile, 'Messages', 'TxtRep_RevokedCount',              S_TxtRep_RevokedCount);
     GetValueFromIni(IniFile, 'Messages', 'TxtRep_RevokedTotal',              S_TxtRep_RevokedTotal);
     GetValueFromIni(IniFile, 'Messages', 'TxtRep_RevokedPayType',            S_TxtRep_RevokedPayType);

     GetValueFromIni(IniFile, 'Messages', 'NoRegularBonWithNumber',           S_NoRegularBonWithNumber);
     GetValueFromIni(IniFile, 'Messages', 'SelectedBonIsRevoked',             S_SelectedBonIsRevoked);
     GetValueFromIni(IniFile, 'Messages', 'ReviewAccountsForTerminal',        S_ReviewAccountsForTerminal);
     GetValueFromIni(IniFile, 'Messages', 'AutoDeletedEmptyAccounts',         S_AutoDeletedEmptyAccounts);
     GetValueFromIni(IniFile, 'Messages', 'ChangePriceForbiden',              S_ChangePriceForbiden);
     GetValueFromIni(IniFile, 'Messages', 'PluesWithoutPromotion',            S_PluesWithoutPromotion);
     GetValueFromIni(IniFile, 'Messages', 'ChangePriceForbiden',              S_ChangePriceForbiden);
     GetValueFromIni(IniFile, 'Messages', 'PluesWithoutPromotion',            S_PluesWithoutPromotion);
     GetValueFromIni(IniFile, 'Messages', 'ErrOnCreateReport',                S_ErrOnCreateReport);
     GetValueFromIni(IniFile, 'Messages', 'ErrOnUpdateReportRec',             S_ErrOnUpdateReportRec);
     GetValueFromIni(IniFile, 'Messages', 'ErrOnExtractingRecForReport',      S_ErrOnExtractingRecForReport);
     GetValueFromIni(IniFile, 'Messages', 'ErrOnExtractingDataForReport',     S_ErrOnExtractingDataForReport);

     GetValueFromIni(IniFile, 'MessagesBill', 'PluShort',                     S_PluShort);
     GetValueFromIni(IniFile, 'MessagesBill', 'Price',                        S_Price);
     GetValueFromIni(IniFile, 'MessagesBill', 'OverLastPlu',                  S_OverLastPlu);
     GetValueFromIni(IniFile, 'MessagesBill', 'OverSTL',                      S_OverSTL);
     GetValueFromIni(IniFile, 'MessagesBill', 'ValueShort',                   S_ValueShort);
     GetValueFromIni(IniFile, 'MessagesBill', 'RevokeLastBon',                S_RevokeLastBon);

     GetValueFromIni(IniFile, 'Messages', 'OperationAborted',                 S_OperationAborted);
     GetValueFromIni(IniFile, 'Messages', 'TryAgainToPutSumInFU',             S_TryAgainToPutSumInFU);
     GetValueFromIni(IniFile, 'Messages', 'ServiceSumInNotFU',                S_ServiceSumInNotFU);
     GetValueFromIni(IniFile, 'Messages', 'ServiceSumOutNotFU',               S_ServiceSumOutNotFU);
     GetValueFromIni(IniFile, 'Messages', 'SumOutCannotBeMoreThanAvailable',  S_SumOutCannotBeMoreThanAvailable);
     GetValueFromIni(IniFile, 'Messages', 'ErrorOnExtractingMaxSumOut',       S_ErrorOnExtractingMaxSumOut);
     GetValueFromIni(IniFile, 'Messages', 'DatabaseNeedUpdate',               S_DatabaseNeedUpdate);
     GetValueFromIni(IniFile, 'Messages', 'NoObligations',                    S_NoObligations);
     GetValueFromIni(IniFile, 'Messages', 'OpenedOperatorsAccounts',          S_OpenedOperatorsAccounts);
     GetValueFromIni(IniFile, 'Messages', 'WantToZeroAllOperators',           S_WantToZeroAllOperators);
     GetValueFromIni(IniFile, 'Messages', 'ZeroOperators',                    S_ZeroOperators);
     GetValueFromIni(IniFile, 'Messages', 'OpenedBillsOnZeroOperators',       S_OpenedBillsOnZeroOperators);
     GetValueFromIni(IniFile, 'Messages', 'Available',                        S_Available);
     GetValueFromIni(IniFile, 'Messages', 'SinglePrice',                      S_SinglePrice);
     GetValueFromIni(IniFile, 'Messages', 'PluWithNumb',                      S_PluWithNumb);
     GetValueFromIni(IniFile, 'Messages', 'PluWithBarcode',                   S_PluWithBarcode);
     GetValueFromIni(IniFile, 'Messages', 'QuantNotEnought',                  S_QuantNotEnought);
     GetValueFromIni(IniFile, 'Messages', 'ErrorOnSerchPlu',                  S_ErrorOnSerchPlu);
     GetValueFromIni(IniFile, 'Messages', 'NoGroupSet',                       S_NoGroupSet);
     GetValueFromIni(IniFile, 'Messages', 'DeepVOID',                         S_DeepVOID);
     GetValueFromIni(IniFile, 'Messages', 'Voids',                            S_Voids);
     GetValueFromIni(IniFile, 'Messages', 'CountVoid',                        S_CountVoid);
     GetValueFromIni(IniFile, 'Messages', 'SumVoid',                          S_SumVoid);
     GetValueFromIni(IniFile, 'Messages', 'CountDeepVoid',                    S_CountDeepVoid);
     GetValueFromIni(IniFile, 'Messages', 'SumDeepVoid',                      S_SumDeepVoid);
     GetValueFromIni(IniFile, 'Messages', 'ForcedClearBon',                   S_ForcedClearBon);
     GetValueFromIni(IniFile, 'Messages', 'BonToLongOnScreen',                S_BonToLongOnScreen);
     GetValueFromIni(IniFile, 'Messages', 'Account',                          S_Account );
     GetValueFromIni(IniFile, 'Messages', 'InternalSystemError_KeyCheck',     S_InternalSystemError_KeyCheck);
     GetValueFromIni(IniFile, 'Messages', 'WorkWillBeStopped',                S_WorkWillBeStopped);
     GetValueFromIni(IniFile, 'Messages', 'NoDeviceFound_KeyCheck',           S_NoDeviceFound_KeyCheck);
     GetValueFromIni(IniFile, 'Messages', 'PleaseInsertValidKey',             S_PleaseInsertValidKey);
     GetValueFromIni(IniFile, 'Messages', 'TheAttachedKeyIsNotValid',         S_TheAttachedKeyIsNotValid);
     GetValueFromIni(IniFile, 'Messages', 'TheDeviceDoesNotSupportRestPOS',   S_TheDeviceDoesNotSupportRestPOS);
     GetValueFromIni(IniFile, 'Messages', 'TheDeviceDoesNotSupportTIS',       S_TheDeviceDoesNotSupportTIS);
     GetValueFromIni(IniFile, 'Messages', 'ValidityExpiredCloseApp',          S_ValidityExpiredCloseApp);
     GetValueFromIni(IniFile, 'Messages', 'UnexpectedErrorOnCheckKey',        S_UnexpectedErrorOnCheckKey);
     GetValueFromIni(IniFile, 'Messages', 'ClosedAfterKeyEvent',              S_ClosedAfterKeyEvent);
     GetValueFromIni(IniFile, 'Messages', 'KeyRestPOS',                       S_KeyRestPOS);
     GetValueFromIni(IniFile, 'Messages', 'PleaseWaitESKCheck',               S_PleaseWaitESKCheck);
     GetValueFromIni(IniFile, 'Messages', 'LastOperationIsOlder',             S_LastOperationIsOlder);
     GetValueFromIni(IniFile, 'Messages', 'SystemTime_Changed',               S_SystemTime_Changed);
     GetValueFromIni(IniFile, 'Messages', 'RestPOSWillBeClosed',              S_RestPOSWillBeClosed);
     GetValueFromIni(IniFile, 'Messages', 'ControlReceipt',                   S_ControlReceipt);
     GetValueFromIni(IniFile, 'Messages', 'FromOldBon',                       S_FromOldBon);
     GetValueFromIni(IniFile, 'Messages', 'ReturnPlu',                        S_ReturnPlu);
     GetValueFromIni(IniFile, 'Messages', 'PrintFiscalCopy',                  S_PrintFiscalCopy);
     GetValueFromIni(IniFile, 'Messages', 'PrintNonFiscalCopy',               S_PrintNonFiscalCopy);
     GetValueFromIni(IniFile, 'Messages', 'FiscalReceipt',                    S_FiscalReceipt);
     GetValueFromIni(IniFile, 'Messages', 'SystemReceipt',                    S_SystemReceipt);
     GetValueFromIni(IniFile, 'Messages', 'InvoicePrinted',                   S_InvoicePrinted);
     GetValueFromIni(IniFile, 'Messages', 'ToBonNo',                          S_ToBonNo);
     GetValueFromIni(IniFile, 'Messages', 'SimpleVoid',                       S_SimpleVoid);
     GetValueFromIni(IniFile, 'Messages', 'SellOnFreePrice',                  S_SellOnFreePrice);
     GetValueFromIni(IniFile, 'Messages', 'ErrorOnExtractRepsData',           S_ErrorOnExtractRepsData);
     GetValueFromIni(IniFile, 'Messages', 'Client_Not_Responding',            S_Client_Not_Responding);
     GetValueFromIni(IniFile, 'Messages', 'ReportFileNotFound',               S_ReportFileNotFound);
     GetValueFromIni(IniFile, 'Messages', 'StopGenerateReport',               S_StopGenerateReport);
     GetValueFromIni(IniFile, 'Messages', 'RepNOTZeroed',                     S_RepNOTZeroed);
     GetValueFromIni(IniFile, 'Messages', 'SellOnFreePriceForbidden',         S_SellOnFreePriceForbidden);
     GetValueFromIni(IniFile, 'Messages', 'ClearStartedBon',                  S_ClearStartedBon);
     GetValueFromIni(IniFile, 'Messages', 'AccountShort',                     S_AccountShort);
     GetValueFromIni(IniFile, 'Messages', 'ErrDuplicatePOSNumber',            S_ErrDuplicatePOSNumber);
     GetValueFromIni(IniFile, 'Messages', 'CannotVoidTotalBecomeNegative',    S_CannotVoidTotalBecomeNegative);
     GetValueFromIni(IniFile, 'Messages', 'CannotChangeQuantTotalBecomeNegative',S_CannotChangeQuantTotalBecomeNegative);
     GetValueFromIni(IniFile, 'Messages', 'CannotChangePriceTotalBecomeNegative',S_CannotChangePriceTotalBecomeNegative);
     GetValueFromIni(IniFile, 'Messages', 'CreatingReport',                   S_CreatingReport);
     GetValueFromIni(IniFile, 'Messages', 'SelectReceiptType',                S_SelectReceiptType);
     GetValueFromIni(IniFile, 'Messages', 'Menu',                             S_Menu);
     GetValueFromIni(IniFile, 'Messages', 'PrintFiscalReceiptConfirm',        S_PrintFiscalReceiptConfirm);
     GetValueFromIni(IniFile, 'Messages', 'SelectReceiptType',                S_SelectReceiptType);
     GetValueFromIni(IniFile, 'Messages', 'DiscWillBeRejected',               S_DiscWillBeRejected);
     GetValueFromIni(IniFile, 'Messages', 'AddOnWillBeRejected',              S_AddOnWillBeRejected);
     GetValueFromIni(IniFile, 'Messages', 'Left',                             S_Left);
     GetValueFromIni(IniFile, 'Messages', 'CardFoundButNotAllowedToChange',   S_CardFoundButNotAllowedToChange);
     GetValueFromIni(IniFile, 'Messages', 'NaveNewPluesNoOperChange',         S_NaveNewPluesNoOperChange);
     GetValueFromIni(IniFile, 'Messages', 'IllegalOperNumber',                S_IllegalOperNumber);
     GetValueFromIni(IniFile, 'Messages', 'ErrorOnOperCheck',                 S_ErrorOnOperCheck);
     GetValueFromIni(IniFile, 'Messages', 'SelectedOperator',                 S_SelectedOperator);
     GetValueFromIni(IniFile, 'Messages', 'NotCashier',                       S_NotCashier);
     GetValueFromIni(IniFile, 'Messages', 'ChangeBillOper',                   S_ChangeBillOper);
     GetValueFromIni(IniFile, 'Messages', 'OperNotExist',                     S_OperNotExist);
     GetValueFromIni(IniFile, 'Messages', 'SelectedOperSameAsCurrent',        S_SelectedOperSameAsCurrent);
     GetValueFromIni(IniFile, 'Messages', 'OnlyForOperWithAccessToOthersTables', S_OnlyForOperWithAccessToOthersTables);
     GetValueFromIni(IniFile, 'Messages', 'ChangePrice',                      S_ChangePrice);
     GetValueFromIni(IniFile, 'Messages', 'ChangeQuant',                      S_ChangeQuant);
     GetValueFromIni(IniFile, 'Messages', 'ChangeService',                    S_ChangeService);
     GetValueFromIni(IniFile, 'Messages', 'RetItemOnlyForOldSale',            S_RetItemOnlyForOldSale);
     GetValueFromIni(IniFile, 'Messages', 'RetPaymentOnlyForOldSale',         S_RetPaymentOnlyForOldSale);
     GetValueFromIni(IniFile, 'Messages', 'GetSingleOperReport',              S_GetSingleOperReport);
     GetValueFromIni(IniFile, 'Messages', 'ReportWithZero',                   S_ReportWithZero);
     GetValueFromIni(IniFile, 'Messages', 'LastOperReport',                   S_LastOperReport);
     GetValueFromIni(IniFile, 'Messages', 'Operators',                        S_Operators);
     GetValueFromIni(IniFile, 'Messages', 'RecInPOSILLSDeleted',              S_RecInPOSILLSDeleted);
     GetValueFromIni(IniFile, 'Messages', 'ReturnPaymentsRejected',           S_ReturnPaymentsRejected);
     GetValueFromIni(IniFile, 'Messages', 'PrintCopyInvoiceOnFiscDevice',     S_PrintCopyInvoice);
     GetValueFromIni(IniFile, 'Messages', 'PrintCopyInvFDShort',              S_PrintCopyInvShort);
     GetValueFromIni(IniFile, 'Messages', 'ErrorOnPrnCopyInvFD',              S_ErrorOnPrnCopyInv);
     GetValueFromIni(IniFile, 'Messages', 'ThereIsFiscalBonConfirmInvoice',   S_ThereIsFiscalBonConfirmInvoice);
     GetValueFromIni(IniFile, 'Messages', 'ORIGINAL',                         S_ORIGINAL);
     GetValueFromIni(IniFile, 'Messages', 'DUBLIKAT',                         S_DUBLIKAT);
     GetValueFromIni(IniFile, 'Messages', 'InvSinglePrice',                   S_InvSinglePrice);
     GetValueFromIni(IniFile, 'Messages', 'InvQuantity',                      S_InvQuantity);
     GetValueFromIni(IniFile, 'Messages', 'InvSum',                           S_InvSum);
     GetValueFromIni(IniFile, 'Messages', 'TaxGrp1Letter',                    S_TaxGrp1Letter);
     GetValueFromIni(IniFile, 'Messages', 'TaxGrp2Letter',                    S_TaxGrp2Letter);
     GetValueFromIni(IniFile, 'Messages', 'TaxGrp3Letter',                    S_TaxGrp3Letter);
     GetValueFromIni(IniFile, 'Messages', 'TaxGrp4Letter',                    S_TaxGrp4Letter);
     GetValueFromIni(IniFile, 'Messages', 'TaxGrp5Letter',                    S_TaxGrp5Letter);
     GetValueFromIni(IniFile, 'Messages', 'TaxGrp6Letter',                    S_TaxGrp6Letter);
     GetValueFromIni(IniFile, 'Messages', 'TaxGrp7Letter',                    S_TaxGrp7Letter);
     GetValueFromIni(IniFile, 'Messages', 'TaxGrp8Letter',                    S_TaxGrp8Letter);
     GetValueFromIni(IniFile, 'Messages', 'SalePerson',                       S_SalePerson);
     GetValueFromIni(IniFile, 'Messages', 'Receiver',                         S_Receiver);
     GetValueFromIni(IniFile, 'Messages', 'ReceivedBy',                       S_ReceivedBy);
     GetValueFromIni(IniFile, 'Messages', 'TownName',                         S_TownName);
     GetValueFromIni(IniFile, 'Messages', 'Address',                          S_Address);
     GetValueFromIni(IniFile, 'Messages', 'Ident',                            S_Ident);
     GetValueFromIni(IniFile, 'Messages', 'ZDDS',                             S_ZDDS);
     GetValueFromIni(IniFile, 'Messages', 'DiscOverSubSumAlreadyAdded',       S_DiscOverSubSumAlreadyAdded);
     GetValueFromIni(IniFile, 'Messages', 'TotalAmount',                      S_TotalAmount);
     GetValueFromIni(IniFile, 'Messages', 'All',                              S_All);
     GetValueFromIni(IniFile, 'Messages', 'NetValue',                         S_NetValue);
     GetValueFromIni(IniFile, 'Messages', 'ItemsWithLimitDiscount',           S_ItemsWithLimitDiscount);
     GetValueFromIni(IniFile, 'Messages', 'ItemsInPromo',                     S_ItemsInPromo);
     GetValueFromIni(IniFile, 'Messages', 'OtherItems',                       S_OtherItems);
     GetValueFromIni(IniFile, 'Messages', 'ReturnItems',                      S_ReturnItems);
     GetValueFromIni(IniFile, 'Messages', 'DiscountForTime',                  S_DiscountForTime);
     GetValueFromIni(IniFile, 'Messages', 'DiscountForTurnOver',              S_DiscountForTurnOver);
     GetValueFromIni(IniFile, 'Messages', 'NotSupportedFunctionPmntSrv',      S_NotSupportedFunctionPmntSrv);
     GetValueFromIni(IniFile, 'Messages', 'CardPaymentSystem',                S_CardPaymentSystem);
     GetValueFromIni(IniFile, 'Messages', 'CardSystem',                       S_CardSystem);
     GetValueFromIni(IniFile, 'Messages', 'ErrOnCallPaySrvSettings',          S_ErrOnCallPaySrvSettings);
     GetValueFromIni(IniFile, 'Messages', 'ConnectionToPaymentSrvOK',         S_ConnectionToPaymentSrvOK);
     GetValueFromIni(IniFile, 'Messages', 'ConnectionToPaymentSrvErr',        S_ConnectionToPaymentSrvErr);
     GetValueFromIni(IniFile, 'Messages', 'SuccessfulRefund',                 S_SuccessfulRefund);
     GetValueFromIni(IniFile, 'Messages', 'UnSuccessfulRefund',               S_UnSuccessfulRefund);
     GetValueFromIni(IniFile, 'Messages', 'RefundAmount',                     S_RefundAmount);
     GetValueFromIni(IniFile, 'Messages', 'AddPmntCardSum',                   S_AddPmntCardSum);
     GetValueFromIni(IniFile, 'Messages', 'UnsuccessfulAddSum',               S_UnsuccessfulAddSum);
     GetValueFromIni(IniFile, 'Messages', 'AddedAmount',                      S_AddedAmount);
     GetValueFromIni(IniFile, 'Messages', 'NewAvSum',                         S_NewAvSum);
     GetValueFromIni(IniFile, 'Messages', 'NoSumEntered',                     S_NoSumEntered);
     GetValueFromIni(IniFile, 'Messages', 'SumNotAvailableNoReturn',          S_SumNotAvailableNoReturn);
     GetValueFromIni(IniFile, 'Messages', 'MaxSumOfTaxGrp',                   S_MaxSumOfTaxGrp);
     GetValueFromIni(IniFile, 'Messages', 'SumToReturn',                      S_SumToReturn);
     GetValueFromIni(IniFile, 'Messages', 'TblDoesNotExists',                 S_TblDoesNotExists);
     GetValueFromIni(IniFile, 'Messages', 'TblNumbNotSelected',               S_TblNumbNotSelected);
     GetValueFromIni(IniFile, 'Messages', 'Monday',                           S_Monday);
     GetValueFromIni(IniFile, 'Messages', 'Tuesday',                          S_Tuesday);
     GetValueFromIni(IniFile, 'Messages', 'Wednesday',                        S_Wednesday);
     GetValueFromIni(IniFile, 'Messages', 'Thursday',                         S_Thursday);
     GetValueFromIni(IniFile, 'Messages', 'Friday',                           S_Friday);
     GetValueFromIni(IniFile, 'Messages', 'Saturday',                         S_Saturday);
     GetValueFromIni(IniFile, 'Messages', 'Sunday',                           S_Sunday);
     GetValueFromIni(IniFile, 'Messages', 'MenuFor',                          S_MenuFor);
     GetValueFromIni(IniFile, 'Messages', 'NoActiveMenuToLoad',               S_NoActiveMenuToLoad);
     GetValueFromIni(IniFile, 'Messages', 'OrderFor',                         S_OrderFor);
     GetValueFromIni(IniFile, 'Messages', 'NoPeriod',                         S_NoPeriod);
     GetValueFromIni(IniFile, 'Messages', 'ListOfEndedBills',                 S_ListOfEndedBills);
     GetValueFromIni(IniFile, 'Messages', 'EndedBills',                       S_EndedBills);


     GetValueFromIni(IniFile, 'Messages', 'LoyaltyNotInstalled',              S_LoyaltyNotInstalled                 );
     GetValueFromIni(IniFile, 'Messages', 'ConnectionToLoyaltySrvOK',         S_ConnectionToLoyaltySrvOK            );
     GetValueFromIni(IniFile, 'Messages', 'ConnectionToLoyaltySrvErr',        S_ConnectionToLoyaltySrvErr           );
     GetValueFromIni(IniFile, 'Messages', 'NotSupportedFunctionLoyaltySrv',   S_NotSupportedFunctionLoyaltySrv      );
     GetValueFromIni(IniFile, 'Messages', 'LoyaltyUnsuccesfulShowStatus',     S_LoyaltyUnsuccesfulShowStatus        );
     GetValueFromIni(IniFile, 'Messages', 'LoyaltyUnsuccesfulRefundPayment',  S_LoyaltyUnsuccesfulRefundPayment     );
     GetValueFromIni(IniFile, 'Messages', 'LoyaltyUnsuccesfulShowReports',    S_LoyaltyUnsuccesfulShowReports       );
     GetValueFromIni(IniFile, 'Messages', 'LoyaltyUnsuccesfulShowAdminInterface',S_LoyaltyUnsuccesfulShowAdminInterface);
     GetValueFromIni(IniFile, 'Messages', 'LoyaltyUnsuccesfulShowAccount',    S_LoyaltyUnsuccesfulShowAccount       );
     GetValueFromIni(IniFile, 'Messages', 'LoyaltyUnsuccesfulLoadAccount',    S_LoyaltyUnsuccesfulLoadAccount       );
     GetValueFromIni(IniFile, 'Messages', 'LoyaltyErrorOnClearCustData',      S_LoyaltyErrorOnClearCustData         );
     GetValueFromIni(IniFile, 'Messages', 'LoyaltyErrorOnAddSum',             S_LoyaltyErrorOnAddSum                );
     GetValueFromIni(IniFile, 'Messages', 'LoyaltyErrorOnShowSettings',       S_LoyaltyErrorOnShowSettings          );
     GetValueFromIni(IniFile, 'Messages', 'BonAlreadyHaveCustChosenChangeIt', S_BonAlreadyHaveCustChosenChangeIt    );
     GetValueFromIni(IniFile, 'Messages', 'AlreadyHaveCustFromLoyaltyChangeIt',S_AlreadyHaveCustFromLoyaltyChangeIt  );
     GetValueFromIni(IniFile, 'Messages', 'NoPrintOpenTblIfNoTbl',            S_NoPrintOpenTblIfNoTbl               );
     GetValueFromIni(IniFile, 'Messages', 'OperNotCashierCantSelectCustForBON',S_OperNotCashierCantSelectCustForBON  );
     GetValueFromIni(IniFile, 'Messages', 'PlsSelectTbl',                     S_PlsSelectTbl                        );
     GetValueFromIni(IniFile, 'Messages', 'CommentTooLong_PrintFirst255',     S_CommentTooLong_PrintFirst255        );
     GetValueFromIni(IniFile, 'Messages', 'WantToContinue',                   S_WantToContinue                      );
     GetValueFromIni(IniFile, 'Messages', 'PluWorksWithIntegerQty',           S_PluWorksWithIntegerQty         );
     GetValueFromIni(IniFile, 'Messages', 'QtyWillNotBeChanged',              S_QtyWillNotBeChanged            );
     GetValueFromIni(IniFile, 'Messages', 'HaveOpenTblReportWillOnlyShown',   S_HaveOpenTblReportWillOnlyShown );
     GetValueFromIni(IniFile, 'Messages', 'PaymentAlreadyRevoked',            S_PaymentAlreadyRevoked      );
     GetValueFromIni(IniFile, 'Messages', 'PaymentIsFromPaymentSystem',       S_PaymentIsFromPaymentSystem );
     GetValueFromIni(IniFile, 'Messages', 'VOIDPayment',                      S_VOIDPayment                );
     GetValueFromIni(IniFile, 'Messages', 'PlsConfirm',                       S_PlsConfirm                 );
     GetValueFromIni(IniFile, 'Messages', 'New',                              S_New                        );
     GetValueFromIni(IniFile, 'Messages', 'ReportNOTZeroed',                  S_ReportNOTZeroed            );
     GetValueFromIni(IniFile, 'Messages', 'ErrOnExtractXLCustDiscount',       S_ErrOnExtractXLCustDiscount );
     GetValueFromIni(IniFile, 'Messages', 'SetQty',                           S_SetQty    );
     GetValueFromIni(IniFile, 'Messages', 'ConfirmIt',                        S_ConfirmIt );
     GetValueFromIni(IniFile, 'Messages', 'KCashier',                         S_KCashier);
     GetValueFromIni(IniFile, 'Messages', 'PlueDoesNotExists',                S_PlueDoesNotExists);
     GetValueFromIni(IniFile, 'Messages', 'ErrOnSearchPlu',                   S_ErrOnSearchPlu);
     GetValueFromIni(IniFile, 'Messages', 'ModuleCustMonitorDoesNotExist',    S_ModuleCustMonitorDoesNotExist);
     GetValueFromIni(IniFile, 'Messages', 'ErrorOnActivateModCustMonitor',    S_ErrorOnActivateModCustMonitor);
     GetValueFromIni(IniFile, 'Messages', 'CustMonitorModuleNoSettings',      S_CustMonitorModuleNoSettings);
     GetValueFromIni(IniFile, 'Messages', 'CustMonitorOK',                    S_CustMonitorOK);
     GetValueFromIni(IniFile, 'Messages', 'PluIsForbiddenForReturn',          S_PluIsForbiddenForReturn);
     GetValueFromIni(IniFile, 'Messages', 'VoidEndBonDiscountOnVoidPayment',  S_VoidEndBonDiscountOnVoidPayment);
     GetValueFromIni(IniFile, 'Messages', 'SumAfterDiscount',                 S_SumAfterDiscount);
     GetValueFromIni(IniFile, 'Messages', 'AftDisc',                          S_AftDisc);


{
     GetValueFromIni(IniFile, 'Messages', '',);
     GetValueFromIni(IniFile, 'Messages', '',);
     GetValueFromIni(IniFile, 'Messages', '',);
     GetValueFromIni(IniFile, 'Messages', '',);
     GetValueFromIni(IniFile, 'Messages', '',);
     GetValueFromIni(IniFile, 'Messages', '',);
}

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
