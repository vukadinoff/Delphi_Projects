unit ResStrUnit;

interface

uses SysUtils, IniFiles, Classes, Forms, StdCtrls, ExtCtrls, RXCtrls, Buttons;

const C_ExcludeComponentList = 'VersionLabel;';

var
 S_ApplicationTitle  : String = 'POS �������� "��� ��������"';
 S_CurrentModulName  : String = 'POS ������� �� ���������';
 S_Version           : String = '������:';
 S_ModuleName        : String = 'ELTRADE POS Touch';
 S_RegDecimalError   : String = '��������� ��������� ����������. ������ �� ���� "."';
 S_SettingsNotLoaded : String = '��������� ��������� �� ���������������� ����!'+sLineBreak+
                                '���� ������� �������������� �� ���������'+sLineBreak+
                                '���� ���������� "���������������� ��������"'+sLineBreak+
                                ''+sLineBreak+
                                'POS ��������� �� ���� ������������!';
 // DK2
 S_DK2NotRegistered1 : String = '������ ������� ����������� �� �����';
 S_DK2NotRegistered5 : String = '������� �� ��� (';
 S_DK2NotRegistered6 : String = ') ��� �� ���������� ������.'+sLineBreak+
                                '���� ���������� �� ���� ����� ������� ������ �� �� '+sLineBreak+
                                '������� �� �� ����������� ������.';
 S_DemoVGersion      : String = '������������� ������';
 S_DBErrorConnect    : String = '���������� ����������� � ������ �����.'+#13+
                                '�� �� ��������� �������� ���������.'+#13+
                                '���� ����������� ��.';

// Database
 S_DBNotConnected    : String = '�� � ����������� � ������ � ������ �����.';
 S_DBNoConection     : String = '���� ������ � ������ �����';
 S_Server            : String = '������';
 S_Local             : String = '�������';
 S_NoConnection      : String = '���� ������';
 S_ServerOnLine      : String = '������� � ������';
 S_ServerOffLine     : String = '���� ������ ��� �������';
 S_NetworkError      : String = '������: ������� ������';
 S_StopPrinters      : String = '���� �� �������� �������.';

// LOGIN  UNIT
 S_LogOut            : String = '����� ��������';
 S_LoginFail         : String = '��������� ��� ��� ������.'+#13+'�������� �������!';
 S_NotPermitions     : String = '������ �������� ������ �� ������.';
 S_HavePermitions    : String = '�������� ������ �� ';
 S_Cashier           : String = '������';
 S_Administrator     : String = '�������������';
 S_User              : String = '����������';

// Dialogs
 S_WantToExit        : String =  '������� �� �� ����������'+#13+'�������� ��� ��������� ?';
 S_MustEndBon        : String =  '��� �������� ���. ������ �� �� ����������'+sLineBreak+
                                 '����� �� �������� �� ���������.';
 S_ChangePluPrice    : String =  '����� �� ���� �� �������:';
 S_ChgPrcOldPrice    : String =  '����� ���� :';
 S_ChgPrcNewPrice    : String =  '���� ���� :';
 S_ChangePluQuant    : String =  '����� ���������� �� �������:';
 S_ChgQtyOldQuant    : String =  '����� �����.:';
 S_ChgQtyNewQuant    : String =  '���� �����.:';
 S_Confirm           : String =  '���� ����������!';

// UDP
 S_UDP_StopModule    : String = '���� �� ��������� �������';

// Cashier Unit
S_NoActiveOperator   : String = '���� ������� �������� ��� �������� �������� ���� ������������ �����.';
S_MustOpenTable      : String = '����� ������ �� �������� ��������� ������';


//DBPluUnit
S_CustomerCardN      : String = '��������� ����� No:';
S_Customer           : String = '������: ';
S_CustomerDiscount   : String = '��������� ��������';
S_Card               : String = '�����:';
S_LeftSum            : String = '�������:';
S_TotalPayedSum      : String = '����:';
S_CustCardNotFound   : String = '�� � ������� ��. ����� No:';
S_NotFound           : String = '�� � �������';
S_NotForThisStation  : String = '�� � ������������ �� ���� ��������.';
S_SellForbidden      : String = '�������� �� ��������.';
S_FracQuantForbidden : String = '��������� ������ ����������.';
S_PluRfndForbidden   : String = '�� �� ������� ����� � ���������� ������ ��'+#13+
                                '��������� ����� �� � �� - ����� �� ������ ����'+#13+
                                '�� ������� ���.'+#13+
                                '��������� �� ����� �� �������.';
S_PluRfndForbidden1  : String = '��������� �� ����� �� � ���������'+#13+'�� ���� ��������.';
S_ZeroPriceForbidden : String = '������� ������ ����.';
S_MinusPrice         : String = '����������� ���� ???.';
S_NoQuantity         : String = '�� ������� �����.';

S_ConfirmClearBon    : String = '���������� �� ���������� ����� ���.'+#13+
                                '���� ����������.';
S_SelPluIsNotRecipe  : String = '��������� ������� �� � �������.';
S_FreeOfCharge       : String = '��������� �������.';
S_FreeQuantity       : String = '��������� ����������.';
S_PromotionMsg       : String = '�������� � ������� � ��������.'+#13+
                                '������� �������� ���������:';
S_PromotionMsg1      : String = '������������ ���������� ��:';
S_PromotionMsg2      : String = '������������ �������:';

// Bill Unit
S_TOTAL              : String = '���� ����:';
S_Resto              : String = '�����';
S_SystemNumb         : String = '����.No:';
S_Discount1          : String = '�������� ����. �������';
S_Discount2          : String = '�������� ����. �������';
S_Discount3          : String = '�������� ����. ����';
S_Discount4          : String = '�������� ����. ����';
S_Discount5          : String = '��������';
S_Discount6          : String = '��������';
S_DiscountError      : String = '����������� ��������';
S_AddOnError         : String = '����������� ��������';
S_BonFinal           : String = '���������� ������';
S_StaroSaldo         : String = '����� �����:';
S_NovoSaldo          : String = '���� �����:';
S_OperStr            : String = '���:';
S_Service            : String = '������:';
S_ServicePLU         : String = '������';
S_ServiceComment     : String = '�������� ������:';
S_CustomDiscPlu      : String = '�������� �� ����';
S_CustomDiscComment  : String = '����:';
S_BillStr            : String = '����:';
S_LastBon            : String = '�������� ';
S_SumStr             : String = '����:';
S_CustCount          : String = '�������:';
S_BillDiscountStr    : String = '��������:';
S_PrefPriceStr       : String = '����. ����';
S_BillAddOnStr       : String = '��������:';
S_ValueDiascountStr  : String = '���������� ';
S_STLSumStr          : String = '���� �� �������:';
S_CustomerStr        : String = '������';
S_InvoiceStr         : String = '�������';
S_StockRecept        : String = '����.����.';
S_Terminal           : String = '�������� No:';
S_CloseTable         : String = '������� ������';
S_SaveTable          : String = '��������� ������';
S_NotClosedTable     : String = '�������� �� � �������';
S_PrintTable         : String = '������� ������';
S_PrintedTable       : String = '���������� ������: ';
S_MiddleRecept       : String = '�������� �������';
S_RevokedPlu         : String = '��������� �������';
S_Value              : String = '��������';
S_RevokedDiscount    : String = '���������� ��������';
S_RevokedAddOn       : String = '���������� ��������';
S_SaldoBeforeRev     : String = '����� ����� ������';
S_Start              : String = '������:';
S_End                : String = '����:';
S_Revoked            : String = '�����.';
S_KitPrnRevokedPlu   : String = '���������� ��������';
S_BonStarted         : String = '��� �������� ���';
S_Subtotal           : String = '���� �� �������:';
S_OpenTableRequired  : String = '������ �� ���� � ������������!';
S_OpenTable          : String = '�������� ����:';
S_ChgPriceForbidden  : String = '������ �������� ������ �� �������:'+#13+
                                '"����� ���� � ������ �� ��������"'+#13+
                                ''+#13+
                                '���������� � ��������.';
S_ChgPriceForbidden1 : String = '�� ������ �� ��������� ������ �� �������: '+#13;
S_ChgPriceForbidden2 : String = '������ ���� �� � ��������� �� �������: '+#13;
S_EditPluForbidden   : String = '�� ���� �� ����������� �������: '+#13;
S_VOIDForbidden      : String = '������ �������� ������ �� �������:'+#13+
                                '" ���������� "'+#13+
                                ''+#13+
                                '���������� � ��������.';
S_VOIDError          : String = '��������� ���������� �� �������� ����';
S_VOID               : String = '����:';
S_VOIDPluForbidden   : String = '�� ���� �� ���������� �������: '+#13;
S_VOIDConfirm        : String = '���������� � �������� ����������'+#13+
                                '������. ���� ����������.';
S_DiscountForbidden  : String = '������ �������� ������ �� �������:'+#13+
                                '"�������� ��������/��������"'+#13+
                                ''+#13+
                                '���������� � ��������.';
S_SearchPluForbidden : String = '������ �������� ������ �� �������:'+#13+
                                '"������� �� �������"'+#13+
                                ''+#13+
                                '���������� � ��������.';
S_InvoiceForbidden   : String = '������ �������� ������ �� �������:'+#13+
                                '"�������� �� �������"'+#13+
                                ''+#13+
                                '���������� � ��������.';
S_InvToBon_EnterBonID :String = '���� �������� ��������� ����� �� ����.';
S_InvToBon_BonNotFound:String = '���� ������� ����� ��� � '+sLineBreak+
                                '����� �������� �����.'+sLineBreak+
                                '���������� �� ������.';
S_InvToBon_BonIsOld   :String = '�������� ����� ��� � ���������'+sLineBreak+
                                '����� ������ �� 5 ���.'+sLineBreak+
                                '������ ����� �� �������� �������'+sLineBreak+
                                '��� ���� ���.';
S_InvToBon_ConfirmCopy: String= '��� �������� ��� ��� ���������� �������.'+sLineBreak+
                                '������� �� �� ���������� ����� �� ���������?';
S_InvToBon_ConfirmNew : String= '������� �� �� ���������� ������� '+sLineBreak+
                                '��� ��� ��� �������� �����:';
S_InvToBon_PrintCopy : String = '���������� ����� �� ������� ��� ��� ��� ����.No:';
S_InvToBon_PrintInv  : String = '���������� ������� ��� ��� ��� ����.No:';
S_FunctionForbidden  : String = '������ ����� �� ������ �� ���� �������.';
S_TableIsBussy       : String = '���� ���� � ����������� �� ���� ��������.'+#13+
                                '������ ����� �� ������ �� ������.'+#13+
                                ''+#13+
                                '';
S_TableIsLocked      : String = '� ������� �����a � �������� �� ���� ��������:';
S_TableIsLocked1     : String = '���������� � ��������. ����, �������� ��-�����.';
S_TableNotExist      : String = '���� �������� ������ � ����� �����.';
S_PrnEndTableQuestion: String = '������� �� ����� �� �������?'+sLineBreak+
                                '���� ����������!';
S_PrnEndBonQuestion  : String = '������� �� ����� �� �������?'+sLineBreak+
                                '���� ����������!';
S_CustomerCount      : String = '���� �������: ';

S_ChangeServiceQuest : String = '������� ������� ������ ��'+sLineBreak+
                                '���������� ������.';
S_RemoveServiceQuest : String = '���������� �� ������'+sLineBreak+
                                '�� ���������� ������.';
S_AddServiceQuest    : String = '�������� �� ������'+sLineBreak+
                                '��� ���������� ������.';
S_BillChecksumError  : String = '������ ��� �������� �� ������.'+sLineBreak+
                                '������ ��������� ���� �� �����.';

// Main Unit
S_CanNotOpenTable    : String = '�� ���� �� �������� ������.'+#13+
                                '��� �������� ���.';
S_PrinterError       : String = '������ �������:';
S_NotOpenTable       : String = '���� �������� ������.';
S_NothingMarked      : String = '���� ���� ���������';
S_WrongPayType       : String = '���������� ��� �������';
S_ClearBillDialog    : String = '������� �� �� �������� ���������� ���?'+#13+
                                '���� ����������.';
S_ClearBeforeClose   : String = '��� �������� ����� ���.'+sLineBreak+
                                '���� ��� �� ���� ���������.'+sLineBreak+
                                '�� �������� �� ?';
S_CanNotClearTable   : String = '��� �������� ��������� ������.'+#13+
                                '�� ������ �� ���������� ����� ���!';
S_PluRecipe          : String = '������� �� �������: ';
S_PluInfo            : String = '������. �������: ';
S_BonInfo            : String = '���������� �� ����� ���';
S_CardInfo           : String = '���������� �� ������������� �����';
S_ReportError        : String = '������ ��� ���������� �� ���������';

S_Plu                : String = '�������';
S_YES                : String = '��';
S_NO                 : String = '��';
S_SysErrors          : String = '�������� �������';
S_PrnErrors          : String = '������� �������� ����������';
S_Printer            : String = '�������';
S_ScannerOK          : String = '������ ����� - ����������';
S_ScaleOK            : String = '���������� ����� - �����������';
S_DisplcayOK         : String = '��������� ������� - ����������';
S_PaymentServerOK    : String = '���������� � ����� - �����������';
S_LoyaltySystemOK    : String = '������� �� �������� - �����������';

S_NotInstalled       : String = '�� � ����������';
S_PrintProfile       : String = '������ �� �����: ';
S_NoPrinterInstalled : String = '���� ���������� ���� ���� �������';
S_WrongPrinterNumb   : String = '���������� ����� �� �������';
S_PrinterIsBussy     : String = '�������� � ����.';
S_ErrorInPrinterTime : String = '��������� �� �������� �� �������� '+#13+
                                '�� ������ ���������. '+#13+
                                '������� �� �� ������� ��������� �� ��������.';
S_PrinterTimeIsOk    : String = '��������� �� �������� � �����.';
S_PrinterNotResponding: String= '�������� �� ��������,';
S_CardPayNotInstalled: String = '�� �� ����������� ����������� � �����.'+#13+
                                '���������� �� ������.';
S_DisplayNotInstalled: String = '�� � ���������� ��������� �������.'+#13+
                                '���������� �� ������.';
S_FailToGetCardData  : String = '������� �� ����������� ����� �� �� ���������.'+#13+
                                '���������� �� ������.';
S_PrefPriceActive    : String = '���� ��� ������� �������������� ����.'+#13+
                                '���������� �� ������.';
S_PrefPriceNotAllowed: String = '���� ����� ���� ����� �� �������������� ����.';
S_PrefPriceStarted   : String = '���������� �������������� ����';
S_PrefPriceStartedShrt: String= '�������������� ����';
S_CardHaveNoDiscount : String = '���� ����� ���� ����� �� ��������.';
S_CardDiscountStarted: String = '������� ��������.';
S_CardDiscountCurrent: String = '�������� �������� ������: ';
S_CardDiscountNew    : String = '������� ���������: ';
S_CardDiscountChange : String = '������� �� �� �������� ������ ������� ��������?';
S_PaymentRefused     : String = '��������� �������� �� �������.';
S_Payment            : String = '�������';
S_PayType            : String = '��� �������';
S_CardOwner          : String = '  ���������� : ';
S_CardCategory       : String = '   ��������� : ';
S_CardLogNumb        : String = '   ���.����� : ';
S_CardPhisNumb       : String = '   ���.����� : ';
S_CardTotalPayed1    : String = '���� ����e�� : ';
S_CardTotalPayed2    : String = '���� ����.(2): ';
S_CardLeftSum1       : String = '    �������  : ';
S_CardLeftSum2       : String = '  ������� (2): ';
S_CardDiscount       : String = '    �������� : ';
S_CardPrefPrice      : String = '   ����.���� : ';
S_CardPrefOverprice  : String = ' �������� �� : ';
S_PaymentStarted     : String = '��� ��������� �������.'+#13+
                                '���������� �� ������';
S_EnterOperatorNumb  : String = '���� �������� ����� �� ��������'+#13+
                                '����� �� ��������� ������.';

// Split/merge Bills
S_SplitTblConfirm1   : String = '������� �� ������ �������� ��'+#13+
                                '������: ';
S_SplitTblConfirm2   : String = '�� ����� �������� ��� ����������'+#13+
                                '������: ';
S_SplitTblError      : String = '������ ��� ��������� �� ������';
S_MergeTblError      : String = '������ ��� ����������� �� ������';
S_SplitErrHaveNewPlu : String = '�������� ������� ���� ��������.'+sLineBreak+
                                '�� ���� �� ��������� ������ ��������� ���� ��������.';
S_SplitErrHavePayment: String = '�������� ������� ������� ����.'+sLineBreak+
                                '�� ���� �� ��������� ������ ��������� ������� ����.';
S_ConfirmSplit       : String = '�� ������ �� ��������� � ����� ������?';
S_SplitBillEvent     : String = '��������� �� ������.';
S_MergeBillEvent     : String = '����������� �� ������.';
S_SplitBillMessage   : String = '����������� ������';
S_MergeBillMessage   : String = '��������� ������';

// Old Bills Unit
S_AscToReprintFiscal1: String = '��� ��������� ������ ���'+sLineBreak+
                                '��������� �������� ���.'+sLineBreak+
                                '������� �� �� ����������'+sLineBreak+
                                '��� ���� �������� ���.'+sLineBreak+
                                '���� ����������!';
S_AscToReprintFiscal2: String = '������� �� �� ����������'+sLineBreak+
                                '�������� ���, ��� ���������'+sLineBreak+
                                '������?'+sLineBreak+
                                '���� ����������!';
S_AscToReprintComment: String = '������� �� �� ���������� ��������'+sLineBreak+
                                '����� �� ��������� ������?'+sLineBreak+
                                '���� ����������!';
S_AscToReprintInvoice: String = '������� �� �� ���������� �������'+sLineBreak+
                                '��� ��������� ������ ?'+sLineBreak+
                                '���� ����������!';
S_ReprintInvoiceFail : String = '��� ��������� ������ ���� ���'+sLineBreak+
                                '��������� �������� ���.'+sLineBreak+
                                '�� ���� �� ���������� �������.';
S_ReprintInvoiceFail1: String = '��� ��������� ������ ���� ���'+sLineBreak+
                                '���������� �������.'+sLineBreak+
                                '�� ���� �� ���������� ���� �������.';

// TXT REPORTS UNIT
S_SetReportsToZero   : String = '������ ����� � ��������';
S_ReportWithZero     : String = '����� � ��������';
S_EndOfDayConfirm    : String = '��������� �������� �� ������ '+sLineBreak+
                                '������� �����.'+sLineBreak+
                                '������� �� �� ����������?';
S_EndOfDayTblConfirm : String = '��������� ��� �������� ������.'+sLineBreak+
                                '������� �� �� ��������'+sLineBreak+
                                '������� �����?';
S_EndOfDayOperConfirm: String = '��������� �������� �� ������ '+sLineBreak+
                                '������� �� ���������.'+sLineBreak+
                                '������� �� �� ����������?';
S_EndOfDayOperTblConfirm: String = '�������� �������� ���'+sLineBreak+
                                '�������� ������.'+sLineBreak+
                                '������� �� �� ��������'+sLineBreak+
                                '������� �� ���������?';
S_PrintEodFiscalConf : String = '��������� ���� ��������� ������� � �������'+sLineBreak+
                                '� ��� ���������� ������. '+sLineBreak+
                                '��� ���� � ���� ���������� �������.';
S_GetTotalReport     : String = '����� ��� ������';
S_GetOperSumsReport  : String = '����� ������ �� ���������';
S_GetFullReport      : String = '����� �����';
S_GetPrewTotalReport : String = '�������� ����� ��� ������';
S_GetSingleOperReport: String = '����� ������ �� ��������';
S_LastOperReport     : String = '�������� ����� �� ��������';
S_Operators          : String = '���������';

// Search PLU Unit
S_AllPlues           : String = '������ ��������';
S_Close              : String = '�������';
S_Back               : String = '�����';

// �������
S_ErrorLocateCustData: String = '������ ��� ������� �� ��������� �����.'+sLineBreak+
                                '���������� �� ������.'+sLineBreak+
                                '���� �������� ������.';
S_CustomerNotFound   : String = '�� �� ������� ����� ���������� �� '+sLineBreak+
                                '���������� �������.'+sLineBreak+
                                '������� �� �� �������� ������� ���� ���?';
S_TooManyCustomers   : String = '������� �� ��� 50 �������,'+sLineBreak+
                                '���������� �� ���������� �������.'+sLineBreak+
                                '���� �������� �� ����� �����.'+sLineBreak+
                                '���������� �� ������.';
S_RequiredField1     : String = '������ "';
S_RequiredField2     : String = '" ������� ��������.'+sLineBreak+
                                '���� �������� �������� �����.';
S_InvalidValue       : String = '" ��� ���������� ��������.'+sLineBreak+
                                '�� �������� �� � ���������� �����.';
S_BulstatStr         : String = '�����.� ';
S_TaxNumbStr         : String = '���� � ';
S_BulstatAlreadyExist: String = '������ � ����� �����.� ���� ����������.'+sLineBreak+
                                '�� ���� �� �������� ������ � �������� ��'+sLineBreak+
                                '�����.�.'+sLineBreak+
                                '���� ����������� �������.';
S_AddCustomerError   : String = '������ ��� ��������� �� ��� ������.';
S_EditCustomerError  : String = '������ ��� ����������� �� ������.';
S_PrintInvoiceError  : String = '������ ��� ����� �� �������.'+sLineBreak+
                                '���������� �� ������.';
S_EnterSumBefore     : String = '���� �������� ����'+sLineBreak+
                                '����� �� ��������� ������.';
S_ServiceSumIn       : String = '�������� ��������� �� ����:';
S_ServiceSumOut      : String = '�������� ��������� �� ����:';
S_ServiceSumInOK     : String = '�������� ��������� �� ���� - ����������:';
S_ServiceSumOutOK    : String = '�������� ��������� �� ���� - ����������:';
S_SetDatabeseConn    : String = '��������� ���� �����.';
S_ChangeWorkStorage  : String = '������� ������� �����.';
S_SystemSettings     : String = '���� �������� ���������.';
S_StoragesList       : String = '������ ������� ��������';
S_Number             : String = '�����';
S_Name               : String = '������������';
S_Comment            : String = '��������';
S_MOL                : String = '���';
S_CurrentStorageNumb : String = '����� ����� No:';
S_StorageStr1        : String = '������� ����� ������ ����� No:';
S_StorageStr2        : String = '������� �� �� ��������� ������� ������� �����';
S_StorageStr3        : String = '�� ����� No:';
S_StorageStr4        : String = '�� ����� No:';
S_StorageStr5        : String = '������� ����� �:';
S_StorageStr6        : String = '����� No:';

// Buttons
S_BtnYes             : String = '&��';
S_BtnNo              : String = '&��';
S_BtnOK              : String = '&��������';
S_BtnCancel          : String = '&�����';
S_BtnAbort           : String = '&��������';
S_BtnRetry           : String = '&������';
S_BtnIgnore          : String = '&���������';

// Select Date FORM
S_SelectDate_SelDate : String = '���� �������� ����.';
S_SelectDate_SelTime : String = '���� �������� ���.';
S_SelectDate_SelDateTime   : String = '���� �������� ���� � ���.';
S_SelectDate_SelPeriodDate : String = '���� �������� ������.';
S_SelectDate_SelPeriodTime : String = '���� �������� ������.';
S_SelectDate_SelPeriodDtTm : String = '���� �������� ������.';
S_SelectDate_Date    : String = '����:';
S_SelectDate_Time    : String = '���:';
S_SelectDate_DateTime: String = '�����:';
S_SelectDate_FromDate: String = '�� ����:';
S_SelectDate_ToDate  : String = '�� ����:';
S_SelectDate_FromHour: String = '�� ���:';
S_SelectDate_ToHour  : String = '�� ���:';
S_SelectDate_FromDtTm: String = '�� �����:';
S_SelectDate_ToDtTm  : String = '�� �����:';

// Txt Reports
S_TxtRep_SepBillInfo  : String = '-----------------------------------';
S_TxtRep_Table        : String = '     ���� :';
S_TxtRep_SysNumber    : String = ' ����. No :';
S_TxtRep_OrderCount   : String = '  ������� :';
S_TxtRep_PLUCount     : String = ' �������� :';
S_TxtRep_TotalSum     : String = '���� ���� :';
S_TxtRep_DiscountSum  : String = '���� ����.:';
S_TxtRep_DiscountCount: String = '���� ����.:';
S_TxtRep_Terminal     : String = ' �������� :';
S_TxtRep_OperNumb     : String = ' �������� :';
S_TxtRep_OperName     : String = ' �������� :';
S_TxtRep_CustDiscount : String = '��. ����. :';
S_TxtRep_TBLOpen      : String = '  ������� :';
S_TxtRep_TBLClosed    : String = '  ������� :';
S_TxtRep_CustCount    : String = '  ������� :';
S_TxtRep_Service      : String = '   ������ :';
S_TxtRep_Comment      : String = ' �������� :';
S_TxtRep_SepCard      : String = '--------- ��������� ����� ---------';
S_TxtRep_CardPhis     : String = '   ���.No :';
S_TxtRep_CardLog      : String = '   ���.No :';
S_TxtRep_CardOwner    : String = ' �������� :';
S_TxtRep_SepCompany   : String = '----------- ����� ����� -----------';
S_TxtRep_Bulstat      : String = '  �����.� :';
S_TxtRep_TaxNumb      : String = '   ���� � :';
S_TxtRep_FirmName     : String = '    ����� :';
S_TxtRep_Town         : String = '     ���� :';
S_TxtRep_Address      : String = '    ����� :';
S_TxtRep_MOL          : String = '      ��� :';
S_TxtRep_Discount     : String = ' �������� :';
S_TxtRep_SepPlues     : String = '------------ �������� -------------';
S_TxtRep_PluAddOn     : String = '����. �/�';
S_TxtRep_PluDiscount  : String = '����. �/�';

S_TxtRep_SepReport    : String = '------------------------------';
S_TxtRep_TermNumb     : String = '����. No:';
S_TxtRep_TermName     : String = '   ����.:';
S_TxtRep_StartRep     : String = ' ������ :';
S_TxtRep_EndRep       : String = '   ���� :';
S_TxtRep_SellsCount   : String = '������ �������� :';
S_TxtRep_TablesCount  : String = '  �������� ���� :';
S_TxtRep_TotalRepSum  : String = '��� ������:';
S_TxtRep_SepCashiers  : String = '   ��������� (�������)';
S_TxtRep_SepCashier   : String = '   �������� (������)';
S_TxtRep_ZreportDone  : String = '  ������ � �������';
S_TxtRep_Operator     : String = '��������';

S_TerminalZReport      : String= '�������� �� ��������:';
S_TerminalZReportTbl   : String= '����������� �������� ���� ��� �������� �� ��������: ';
S_OperatorZReportTbl   : String= '����������� �������� ���� ��� �������� �� ��������: ';
S_StockTemplateNotFound: String= '��������� �� ������ �������'+sLineBreak+
                                 '���� Windows �� � �������.';
S_StockCopy_ConfirmNew : String= '������� �� �� ���������� ����� '+sLineBreak+
                                 '��� ��� ��� �������� �����:';


// Registration form
S_InvalidSerialNumber  : String= '������������� � ��������� !'+sLineBreak+
                                 '��������� ������ �����.';
S_InvalidStorageNumber : String= '������������� � ��������� !'+sLineBreak+
                                 '��������� ������� �����.';

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

S_UnsuccessfulScaleInit    : String = '��������� ������������� �� ��������� �����.';
S_UnsuccessfulDisplayInit  : String = '��������� ������������� �� ��������� �������.';
S_UnsuccessfulScannerInit  : String = '��������� ������������� �� ������ ������.';
S_UnsuccessfulPrinterInit  : String = '��������� ������������� �� �������� ����������.';
S_PleaseCheckHWDevice      : String = '���� ��������� ���� ������������ � ��������'+sLineBreak+
                                      '� ���� ��� ������� ������������.'+sLineBreak+
                                      '�� ����������� ���������� ���� �� ��������.';


S_ModuleNotRegistered_StopWork            : String = '������ �� � ����������� � ������ �����.'+sLineBreak+
                                                     '�������� � ������ �� ���� ����������.';
S_CommandUnknown                          : String = '��������� �������';
S_ErrorOnWritingEvent                     : String = '������ ��� ����� �� �������';
S_MessageAdded                            : String = '����������� ��������';
S_WatchSetRight                           : String = '������ ��������';
S_ErrOnSetWatch                           : String = '������ ��� ���������';
S_InCorrectTimeFormat                     : String = '���������� ������ �� �������';
S_Refreshed                               : String = '��������';
S_POSRestDoesNotSupportCommand            : String = 'ELTRADE POS Touch �� �������� ���� �������';
S_Reloaded                                : String = '����������';
S_WorkingStorageChanged                   : String = '�������� ����� ������� �����';
S_ErrOnChangingStoragaNumb                : String = '������ ��� ������� ����� �� �����';
S_WrongStorageNumb                        : String = '���������� ����� �� �����';
S_ErrOnLoadingSystemNomenclature          : String = '������ ��� ��������� �� �������� ������������.';
S_SeeErrorFileForDetails                  : String = '�������� ����� � �������� �� ������ ����������.';
S_CurrencySign                            : String = '��.';
S_ErrOnSavingCurrentInvoiceNumb           : String = '������ ��� ����� �� ������� ����� �������';
S_SeriousError                            : String = '�������� ������';
S_WrongModuleInit                         : String = '������ ������������� ����� �� �����';
S_Code                                    : String = '���';
S_NameShortly                             : String = '���';
S_Quant                                   : String = '�����.';
S_Portion                                 : String = '������';
S_ECRName                                 : String = '������ ���';
S_ForeignName                             : String = '������.���';
S_NomenclCode                             : String = '�������.���';
S_ScaleCode                               : String = '��� �����';
S_MaxAddOn                                : String = '����.����.';
S_MaxDiscount                             : String = '����.����.';
S_MinReserve                              : String = '���. �����';
S_MaxReserve                              : String = '����. �����';
S_ForPOS                                  : String = '�� ��������';
S_Measure                                 : String = '�����';
S_PrintingGroup                           : String = '����� �����';
S_TaxGroup                                : String = '���. �����';
S_Tax                                     : String = '���';
S_PluGroup                                : String = '���. �����';
S_BuyPrice                                : String = '������� ����';
S_TradePrice                              : String = '����. ���� ����';
S_SellPrice                               : String = '����. ���� ������';
S_ForbidenSaleInMinus                     : String = '������� ����������� �����.';
S_Promotional                             : String = '�������������';
S_Recipe                                  : String = '�������� ������� (�������)';
S_OnlyIntQuant                            : String = '���� ���� ����������';
S_SaleOnZeroPrice                         : String = '��������� ������ ����';
S_ChangePriceOnSale                       : String = '����. ����� ���� ��� ��������';
S_Barcode                                 : String = '������';
S_Used                                    : String = '������.';
S_Barcodes                                : String = '���������';
S_StorageName                             : String = '����� ���';
S_Quantity                                : String = '����������';
S_Availabilities                          : String = '����������';
S_Priority                                : String = '�����.';
S_Days                                    : String = '���';
S_Promotions                              : String = '��������';
S_NoConnectionToPrinterForInvoices        : String = '���� ������ � �������� �� �������';
S_ErrOnLoadingPluesToMerge                : String = '������ ��� ��������� �� �������� �� �������� �� ������.';
S_Count                                   : String = '��.';

S_RevokeLastBonQuest                      : String= '������� �� �� ���������� ��������'+sLineBreak+
                                                    '���������� ������ �������?'+sLineBreak+
                                                    '���� ����������.';
S_RevokeLastBonSucceed                    : String= '������� ���������� �� ����� ���.';
S_RevokeLastBonFail                       : String= '��������� ���������� �� ����� ���:';
S_RevokeLastNotFound                      : String= '���� �������� ������ ������� �� ���������� 5 ���.';
S_RevokeLastAlreadyDone                   : String= '�������� ��������� ����� ��� � ���� ���������.';
S_BonOldLoaded                            : String = '������� ����� ������';
S_ItemReturnVeryOld                       : String= '��������� ������ ������� ��-�����'+sLineBreak+
                                                    '�� 30 ���. ��������� �� ����� ��'+sLineBreak+
                                                    '������ ������� � ����������.';
S_ItemReturnConfirm                       : String= '������� �� �����'+sLineBreak+
                                                    '���� ����������.';
S_CannotReturnService                     : String= '�� ������ �� ������� ���������� ������.';
S_ItemReturnOK                            : String= '������� �� ����� ����������.';
S_ItemReturnFail                          : String= '��������� ������� �� �����.';
S_ItemReturnAlreadyDone                   : String= '��������� ����� � ���� �������!'+sLineBreak+
                                                    '���� �� �������: ';
S_ItemReturn                              : String= '������� �� �����';
S_ItemBuyDate                             : String= '�������� ��:';
S_ChangeQuantForbiden                     : String= '��������� ��������� �� ����������'+sLineBreak+
                                                    '�� �������';
S_PluIsPromotional                        : String= '��������� � ��������� �� ��������.';
S_PluQuantNegative                        : String= '��������� ������� ���� � ������.';
S_UnCorrectConfigOfReport                 : String= '���������� ������������ �� ���������.';
S_SelectedTermNumberIsCurrentNumb         : String= '��������� ����� �� ��������, �� �����'+sLineBreak+
                                                    '�� ���� ���������� �������� ������� � �������.';
S_TermShares_NoExport                     : String= '��������� ������� ������ � ���������� ���������.'+
                                                    ' �������� ���� �� ���� �����������.';
S_NoSalesForTerminal                      : String= '���� �������� �� �������� � �����';
S_NoExportAccount                         : String= '�������� ���� �� ���� �����������.';
S_SuccessfulAccountExport                 : String= '������� ������� �� ������ ��� ��������';
S_ExportForbidden                         : String= '������ �������� ������ �� �������:'+#13+
                                                    '" ������� �� ������ "'+#13+
                                                    ''+#13+
                                                    '���������� � ��������.';
S_TxtRep_ReturnedTitle                    : String= '     ������� ����� ';
S_TxtRep_ReturnedCount                    : String= '���� �������';
S_TxtRep_ReturnedTotal                    : String= '���� �������';
S_TxtRep_ReturnedPayType                  : String= '�������   ';
S_TxtRep_RevokedTitle                     : String= '     ���������� ������ ';
S_TxtRep_RevokedCount                     : String= '���� ����������';
S_TxtRep_RevokedTotal                     : String= '���� ����������';
S_TxtRep_RevokedPayType                   : String= '����������';
S_NoRegularBonWithNumber                  : String= '������������ ������� ���'+sLineBreak+' ��� �������� �����';
S_SelectedBonIsRevoked                    : String= '��������� ��� � ���������.'+sLineBreak+
                                                    '�� ���� �� ���� �������� �������.'+sLineBreak+
                                                    '���������� �� ������.';
S_ReviewAccountsForTerminal               : String= '������� �� ������� �� ��������';
S_AutoDeletedEmptyAccounts                : String= '����������� ������� ������ ������';
S_ChangePriceForbiden                     : String= '��������� ��������� �� ����'+sLineBreak+
                                                    '�� �������';
S_PluesWithoutPromotion                   : String= '�� ���. ��� ��������';
S_ErrOnCreateReport                       : String= '������ ��� ��������� ����� �� �����';
S_ErrOnUpdateReportRec                    : String= '������ ��� update �� ����� �� �����';
S_ErrOnExtractingRecForReport             : String= '������ ��� ��������� �� ����� �� �����';
S_ErrOnExtractingDataForReport            : String= '������ ��� ��������� �� ����� �� �����';
S_PluShort                                : String= '���.';
S_Price                                   : String= '����';
S_OverLastPlu                             : String= '��� ����. �������';
S_OverSTL                                 : String= '��� �������� ����';
S_ValueShort                              : String= '�-��';
S_RevokeLastBon                           : String= '���������� �� �������� ���';
S_OperationAborted                        : String= '���������� �� ������.';
S_TryAgainToPutSumInFU                    : String= '������ ������ ���������� �� ����������?';
S_ServiceSumInNotFU                       : String= '���������� ��������� �� ���� �� �' +sLineBreak+
                                                    '�������� ��� ���������� ����������.';
S_ServiceSumOutNotFU                      : String= '���������� ��������� �� ���� �� �' +sLineBreak+
                                                    '�������� ��� ���������� ����������.';
S_SumOutCannotBeMoreThanAvailable         : String= '�������� ���������� ���� �� ����' +sLineBreak+
                                                    '�� ���� ��-������ �� ���������.';
S_ErrorOnExtractingMaxSumOut              : String= '������ ��� ��������� �� ���������� ���� �� �������� ���������.';
S_DatabaseNeedUpdate                      : String= '��������� � ������ �� ������ �����.'+sLineBreak+
                                                    '����� "ELTRADE POS Touch" �� ���� ��������.';
S_NoObligations                           : String= '   ���� ����������';
S_OpenedOperatorsAccounts                 : String= '����������� ���� �������� ������.'+#13#10+
                                                    '������� �� �� �������� ������ ���������?';
S_WantToZeroAllOperators                  : String= '��������� �������� �� ������'+#13#10+
                                                    '�������� �� ������ ���������.'+#13#10+
                                                    '������� �� �� ����������?';
S_ZeroOperators                           : String= '�������� �� ������ ���������';
S_OpenedBillsOnZeroOperators              : String= '����������� �������� ������ ��� �������� �� ������ ���������';
S_Available                               : String= '�������';
S_SinglePrice                             : String= '��.����';
S_PluWithNumb                             : String= '������� � �����';
S_PluWithBarcode                          : String= '������� � ������';
S_QuantNotEnought                         : String= '������������ ��������� �� ������� ���������� �� �������.';
S_ErrorOnSerchPlu                         : String= '������ ��� ������� �� �������';
S_NoGroupSet                              : String= '�� � �������� �����';
S_DeepVOID                                : String= '������ ����';
S_Voids                                   : String= '   �������';
S_CountVoid                               : String= '���� ����';
S_SumVoid                                 : String= '���a ����';
S_CountDeepVoid                           : String= '���� ������ ����';
S_SumDeepVoid                             : String= '���� ������ ����';
S_ForcedClearBon                          : String= '��������� � ������������ ���������� �� ������.';
S_BonToLongOnScreen                       : String= '�������� � �������� �������� �o���� �� ';
S_Account                                 : String= '������';
S_InternalSystemError_KeyCheck            : String= '�������� ������ ��� �������� �� ������� ����.';
S_WorkWillBeStopped                       : String= '�������� � "ELTRADE POS Touch" �� ���� ����������!';
S_NoDeviceFound_KeyCheck                  : String= '��������� ���� �� � �������!';
S_PleaseInsertValidKey                    : String= '����, ��������� ������� ������� ����';
S_TheAttachedKeyIsNotValid                : String= '������e���� ���� �� � �������.';
S_TheDeviceDoesNotSupportRestPOS          : String= '����������� ���� �� � �������� ��'+sLineBreak+'"POS �������� ���������".';
S_TheDeviceDoesNotSupportTIS              : String= '����������� ���� �� � �������� ��'+sLineBreak+'"��� ��������".';
S_ValidityExpiredCloseApp                 : String= '������� �� �������������� �������� �� ������ � �������.';
S_UnexpectedErrorOnCheckKey               : String= '���������� ������ ��� �������� �� ������� ����.';
S_ClosedAfterKeyEvent                     : String= '����������� ���� �������� �� ������� ����.';
S_KeyRestPOS                              : String= '������� ���� ELTRADE POS Touch';
S_PleaseWaitESKCheck                      : String= '����, ���������.'+sLineBreak+
                                                    '�������� �� ������� ����.';
S_LastOperationIsOlder                    : String= '�������� ���� � ��-����� �� ������ ��'+#10#13+
                                                    '���������� ��������.';
S_SystemTime_Changed                      : String= '��������� � ������� �� ���������� �����.';
S_RestPOSWillBeClosed                     : String= '����� "ELTRADE POS Touch" �� ���� ��������.';
S_ControlReceipt                          : String= '��������� �������';
S_FromOldBon                              : String= '�� ���� ���';
S_ReturnPlu                               : String= '������� �� �������';
S_PrintFiscalCopy                         : String= '����� �������� �����';
S_PrintNonFiscalCopy                      : String= '����� �������� �����';
S_FiscalReceipt                           : String= '��������';
S_SystemReceipt                           : String= '��������';
S_InvoicePrinted                          : String= '����� �� �������';
S_ToBonNo                                 : String= '��� ���';
S_SimpleVoid                              : String= '����';
S_SellOnFreePrice                         : String= '�������� �� �������� ����';
S_ErrorOnExtractRepsData                  : String= '������ ��� ��������� �� ����� �� ������.';
S_Client_Not_Responding                   : String= '�������� �� �������� �� ����������� �������';
S_ReportFileNotFound                      : String= '�� � �������� �������� �� ������:';
S_StopGenerateReport                      : String= '������������ �� ������ �� ����������.';
S_RepNOTZeroed                            : String= '������� �� � ���������';
S_SellOnFreePriceForbidden                : String= '������ �������� ������ �� �������:'+#13+
                                                    '"����� ���� � ������ �� ��������"'+#13+
                                                    '���������� �� ���� �� ���������� ����.';
S_ClearStartedBon                         : String= '���������� �� �������� ���';
S_AccountShort                            : String= '��.';

S_ErrDuplicatePOSNumber                   : String= '� ������� ���������� ���� POS ��������'+#13+#10+
                                                    '���������� ����� ����� � ������� � ������'+#13+#10+
                                                    '���� �����. ������ �� ��� ��������� � �������'+#13+#10+
                                                    '������ � ���� ���� �� � ��������������.';
S_CannotVoidTotalBecomeNegative           : String= '�� ������ �� ��������� �������� �������,'+#13+#10+
                                                    '������ ������ �� ���� �� ����� �����������.';
S_CannotChangeQuantTotalBecomeNegative    : String= '��������� �� ������������ �� �������� ������� �� ������,'+
                                                    '������ ������ ���� �� ���� �� ����� �����������.';
S_CannotChangePriceTotalBecomeNegative    : String= '��������� �� ���� �� �������� ������� �� ������,'+
                                                    '������ ������ ���� �� ���� �� ����� �����������.';
S_CreatingReport                          : String= '���������� �� �����';
S_Menu                                    : String= '����';
S_PrintFiscalReceiptConfirm               : String= '������� �� ����� �� �������� �������?'+ sLineBreak+
                                                    '����, ����������!';
S_SelectReceiptType                       : String= '����, �������� ��� �� �������!';
S_DiscWillBeRejected                      : String= '���������� �� ���� ��������!';
S_AddOnWillBeRejected                     : String= '���������� �� ���� ��������!';
S_Left                                    : String= '�������';
S_CardFoundButNotAllowedToChange          : String= '��������� ������� �� ������ �� ���.';
S_NaveNewPluesNoOperChange                : String= '�������� ������� ���� ��������.'+sLineBreak+
                                                    '�� ���� �� ������� �������� �� ������ � ���� ��������.';
S_IllegalOperNumber                       : String= '���������� ����� �� ��������.';
S_ErrorOnOperCheck                        : String= '������ ��� ��������� ����� �� ��������.';
S_SelectedOperator                        : String= '���������� ��������';
S_NotCashier                              : String= '�� � ������';
S_ChangeBillOper                          : String= '������� ������ �� ������';
S_OperNotExist                            : String= '�� ���������� �������� � No';
S_SelectedOperSameAsCurrent               : String= '���������� �������� ������� � �������� �� ��������.';
S_OnlyForOperWithAccessToOthersTables     : String= '��������� � ��������� ���� �� ��������� � ������ �� ����� ����.';
S_ChangePrice                             : String= '������� ����';
S_ChangeQuant                             : String= '������� ���.';
S_ChangeService                           : String= '������� ������';
S_RetItemOnlyForOldSale                   : String= '��������� ������� �� �������� ���� ��'+#13+#10+
                                                    '������� �� ������� �� ���������� ������.'+#13+#10+
                                                    '���������� �� ������.';
S_RetPaymentOnlyForOldSale                : String= '��������� ������� �� �������� ���� ��'+#13+#10+
                                                    '������� �� ������� �� ���������� ������.'+#13+#10+
                                                    '���������� �� ������.';
S_RecInPOSILLSDeleted                     : String= '���������� ����� �� ��������� ������ � ������.';
S_ReturnPaymentsRejected                  : String= '���������� �� ����� �� ����� �������.'+#13+#10+
                                                    '���������� �� ������.';
S_PrintCopyInvoice                        : String= '���������� ����� �� �-��, �������� �� �������� ����������.';
S_PrintCopyInvShort                       : String= '����� ����� �-��, �������� �� ��';
S_ErrorOnPrnCopyInv                       : String= '������ ��� ����� ����� �-��, �������� �� ��.';
S_ThereIsFiscalBonConfirmInvoice          : String= '��� ��������� ������ ���� ��� �������' + #13+#10+
                                                    '�������� ���. ��������� ���� �� ����' + #13+#10+
                                                    '�������� ���� �������� ���.'+#13+#10+
                                                    '������� �� �� ����������?';
S_ORIGINAL                                : String= '��������';
S_DUBLIKAT                                : String= '��������';
S_InvSinglePrice                          : String= '�������� ����';
S_InvQuantity                             : String= '����������';
S_InvSum                                  : String= '����';
S_TaxGrp1Letter                           : String= '�';
S_TaxGrp2Letter                           : String= '�';
S_TaxGrp3Letter                           : String= '�';
S_TaxGrp4Letter                           : String= '�';
S_TaxGrp5Letter                           : String= '�';
S_TaxGrp6Letter                           : String= '�';
S_TaxGrp7Letter                           : String= '�';
S_TaxGrp8Letter                           : String= '�';
S_SalePerson                              : String= '��������';
S_Receiver                                : String= '���������';
S_ReceivedBy                              : String= '�������';
S_TownName                                : String= '����';
S_Address                                 : String= '�����';
S_Ident                                   : String= '����� N';
S_ZDDS                                    : String= '����  N';
S_DiscOverSubSumAlreadyAdded              : String= '�������� ���� ��� ��������/�������� ��� �������� ����.'+#13+#10+
                                                    '�� ������ �� �������� �����.'+#13+#10+
                                                    '���������� �� ������.';
S_TotalAmount                             : String= '���� ����';
S_All                                     : String= '������';
S_NetValue                                : String= '���� ��������';
S_ItemsWithLimitDiscount                  : String= '����� � ���.��������';
S_ItemsInPromo                            : String= '����� � ��������';
S_ItemNoDiscount                          : String= '����� ��� ��������';
S_OtherItems                              : String= '����� �����';
S_ReturnItems                             : String= '�������';
S_DiscountForTime                         : String= '������ ��������';
S_DiscountForTurnOver                     : String= '�������� �� ������';
S_NotSupportedFunctionPmntSrv             : String= '�������� �� ���������� ��'+#13+#10+
                                                    '�������� ��������� �������.';
S_CardPaymentSystem                       : String = '������� ����������';
S_CardSystem                              : String = '������� �������';
S_ErrOnCallPaySrvSettings                 : String = '������ ��� ��������� �� ���������'+#13+#10+
                                                     '�� ������� ����������.';
S_ConnectionToPaymentSrvOK                : String = '������� ������ ��� ������� �� ���������� � �����.';
S_ConnectionToPaymentSrvErr               : String = '��������� ��������� ��� ������� �� ���������� � �����';
S_SuccessfulRefund                        : String = '������� ������� �� ���� �� ��������� �������.';
S_UnSuccessfulRefund                      : String = '��������� ������� �� ���� �� ��������� �������.';
S_RefundAmount                            : String = '������� �� ����';
S_AddPmntCardSum                          : String = '�������� �� ���� ��� ������';
S_UnsuccessfulAddSum                      : String = '��������� �������� �� ���� ��� ������.';
S_AddedAmount                             : String = '�������� ����';
S_NewAvSum                                : String = '���� ���������';
S_NoSumEntered                            : String = '���� �������� ���� �� ��������!';
S_SumNotAvailableNoReturn                 : String = '������������ �� ���������� � ������� �� �����'+#13+#10+
                                                     '� ���������� � �� ���� ��������.';
S_MaxSumOfTaxGrp                          : String = '���������� ���� �� ������� �����';
S_SumToReturn                             : String = '���� �� �������';
S_TblDoesNotExists                        : String = '������������ ������ (����) � �����';
S_TblNumbNotSelected                      : String = '�� � ������� ����� �� ������ (����).';
S_Monday                                  : String = '����������';
S_Tuesday                                 : String = '�������';
S_Wednesday                               : String = '�����';
S_Thursday                                : String = '���������';
S_Friday                                  : String = '�����';
S_Saturday                                : String = '������';
S_Sunday                                  : String = '������';
S_MenuFor                                 : String = '���� ��';
S_NoActiveMenuToLoad                      : String = '���� ������� ���� �� ���������.';
S_OrderFor                                : String = '������� ��';
S_NoPeriod                                : String = '�� � ������� ������.';
S_ListOfEndedBills                        : String = '������ ���������� ������';
S_EndedBills                              : String = '���������� ������';
S_LoyaltyNotInstalled                     : String = '�� � ����������� ������� �� ��������.'+#13+
                                                     '���������� �� ������.';
S_ConnectionToLoyaltySrvOK                : String = '������� ������ ��� ��������� �� ��������.';
S_ConnectionToLoyaltySrvErr               : String = '��������� ��������� ��� ��������� �� ��������';
S_NotSupportedFunctionLoyaltySrv          : String = '��������� �� �������� �� �������� ��������� �������.';
S_LoyaltyUnsuccesfulShowStatus            : String = '������� �� �������� - ��������� ��������� �� ������';
S_LoyaltyUnsuccesfulRefundPayment         : String = '������� �� �������� - ��������� ������� �� �������';
S_LoyaltyUnsuccesfulShowReports           : String = '������� �� �������� - ��������� ��������� �� ������';
S_LoyaltyUnsuccesfulShowAdminInterface    : String = '������� �� �������� - ��������� ��������� �� ���������������� ���������';
S_LoyaltyUnsuccesfulShowAccount           : String = '������� �� �������� - ��������� ��������� ���������� �� ������';
S_LoyaltyUnsuccesfulLoadAccount           : String = '������� �� �������� - ������ ��� ��������� �� ������';
S_LoyaltyErrorOnClearCustData             : String = '������� �� �������� - ��������� ���������� �� ��������� �����';
S_LoyaltyErrorOnAddSum                    : String = '������� �� ��������'+#13+#10+
                                                     '��������� �������� �� ����';
S_LoyaltyErrorOnShowSettings              : String = '������� �� �������� - ��������� ��������� �� ���������.';
S_BonAlreadyHaveCustChosenChangeIt        : String = '�������� ���� ��� ������ ������.'+#13+#10+
                                                     '������� �� �� �� ���������?';
S_AlreadyHaveCustFromLoyaltyChangeIt      : String = '���� ��� ������� ������ �� ��������� �� ��������.'+#13+#10+
                                                     '������� �� �� �� �������?';
S_NoPrintOpenTblIfNoTbl                   : String = '�� ���� �� �� ������� ���������� �� ������ �� ���������� ������ �������';
S_OperNotCashierCantSelectCustForBON      : String = '�������� �������� �� � ������ � �� ���� �� ������ ������ �� ������.';
S_PlsSelectTbl                            : String = '����, �������� ������ �� ����.';
S_CommentTooLong_PrintFirst255            : String = '��������� �� �������� ���������'+#13+#10+
                                                     '���������� ����������� �������.'+#13+#10+
                                                     '�� ����� ����������� �������'+#13+#10+
                                                     '255 �������.';
S_WantToContinue                          : String = '������� �� �� ����������?';

// new
S_PluWorksWithIntegerQty                  : String = '��������� ������ ���� � ���� ����������.';
S_QtyWillNotBeChanged                     : String = '������������ ���� �� ���� ���������.';
S_HaveOpenTblReportWillOnlyShown          : String = '������� ���� �� ���� �������, ������'+#13+#10+
                                                     '��� �������� ��������� ������!';
S_PaymentAlreadyRevoked                   : String = '��������� ���� � ���������.';
S_PaymentIsFromPaymentSystem              : String = '��������� � ��������� �� '+#13+#10+
                                                     '"������� ������� �� ����������"!' +#13+#10+
                                                     '���������� � ����������� ���� ��' +#13+#10+
                                                     '���� �������� � � ���.';
S_VOIDPayment                             : String = '��������� �� �������';
S_PlsConfirm                              : String = '����, ����������';
S_New                                     : String = '����';
S_ReportNOTZeroed                         : String = '������� �� � �������';
S_ErrOnExtractXLCustDiscount              : String = '������� �� �������� - ��������� ��������� �� ��������� ��������.';

S_SetQty                                  : String = '�������� ����������';
S_ConfirmIt                               : String = '�� �� ���������� ��?';
S_KCashier                                : String = '������';
S_PlueDoesNotExists                       : String = '������������ ������� � No';
S_ErrOnSearchPlu                          : String = '������ ��� ������� �� �������';
S_ModuleCustMonitorDoesNotExist           : String = '����� "��������� �������" (Presentation.exe)'+#13+#10+
                                                     '�� ���� �� ���� ������� � ������� �� ���-�.';
S_ErrorOnActivateModCustMonitor           : String = '������ ��� ���������� �� ����� "��������� �������".';
S_CustMonitorModuleNoSettings             : String = '���� �������� ��������� �� ����� "��������� �������".';
S_CustMonitorOK                           : String = '����� "��������� �������" - ����������';
S_PluIsForbiddenForReturn                 : String = '�������� �� �������';
S_VoidEndBonDiscountOnVoidPayment         : String = '��������� �� �������� ��� ���� �� ��� ���� ���� �� �������.';
S_SumAfterDiscount                        : String = '���� ���� ��������';
S_AftDisc                                 : String = '���� ����.';
S_CannotEndAsSystem                       : String = '������ ������������ ����� �� ���������� ���� ��������.'+#13+#10+
                                                     '����, ��������� �������.';
S_HDeliveryAddress                        : String = '����� �� ��������';
S_HDeliveryReceiver                       : String = '���������';
S_HDeliveryPhone                          : String = '�������';
S_HDeliveryInfo                           : String = '���������� �� ��������';
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

// �������
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
