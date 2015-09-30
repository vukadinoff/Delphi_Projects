unit BillingConstUnit;

interface

const
 C_IniSection_DBConfig        = 'DATABASE CONFIG';
 C_IniSection_EventHandler    = 'EVENT HANDLER CONFIG';
 C_DBConfig_DBConnString      = 'ConnectionString';

 C_FileName_TraceLog          = 'TraceLog_Server.txt';
 C_FileName_ErrorSQL          = 'DbInterfaceSQLErrorLog.txt';
 C_FileName_EvHndSQLQue       = 'SQL_NotHandledQue.sql';
 C_FileName_EvHndSQLErrorLog  = 'EventHandlerSQLErrorsLog.txt';
 C_FileName_EvHndSQLFailScript= 'EventHandlerSQLFailScript.sql';

 C_DropDeviceWoSerialTimeSec = 40; // колко секунди да се търпи устройство без сериен номер
 C_DropDeviceWoActivity      = 60; // колко секунди да се търпи устройсттво без активност

 C_NRA_Url_Test              = 'http://10.199.1.1/fdmon/FDMonitorPort';
 C_NRA_Url_Work              = 'http://10.199.1.1:7790/rfdmon/FDMonitorPort';


 C_ModuleNameSvr : String = 'ELBNGSERVER';
 C_ModuleNameDev : String = 'ELBNGDEVICE';
 C_EvType_Comm   : String = 'COMM';
 C_EvType_Comm0  : String = 'COMM_0';
 C_EvType_Exec   : String = 'EXEC';
 C_EvType_Error  : String = 'ERR';
 C_EvType_System : String = 'SYS';
 C_EvType_Login  : String = 'LOGIN';
 C_EvType_ChgPass: String = 'CHGPASS';
 C_EvType_Message: String = 'MESSAGE';
 C_EvType_ESK    : String = 'ESK';
 C_EvType_FileUpd: String = 'FUPD';
 C_EvType_FileDwn: String = 'FDWN';
 C_EvType_FileExp: String = 'FEXP';
 C_EvType_ChgPin : String = 'CHGPIN';
 C_EvType_NRA    : String = 'NRA';
 C_EvType_FiscV  : String = 'FISC_V';
 C_EvType_FiscR  : String = 'FISC_R';
 C_EvType_FiscC  : String = 'FISC_C';
 C_EvType_DataIn : String = 'DDIN';
 C_EvType_SIM    : String = 'SIM';
 C_EvType_SIMPay : String = 'SIMPAY';
 C_EvType_ReadReq: String = 'REQRead';

 C_Project_ModemFW  : String = '&&ModemFirmware##';
 C_Project_Serial   : String = 'Serial';
 C_Project_HwVer    : String = 'HwVersion';
 C_Project_SwName   : String = 'SwName';
 C_Project_SwVer    : String = 'SwVersion';
 C_Project_ESKVer   : String = 'ESKVersion';
 C_Project_Host     : String = 'Host';
 C_Project_Path     : String = 'Path';

 C_DeviceType_GPRS  : String = 'M';
 C_DeviceType_FD    : String = 'F';
 C_DeviceType_PC    : String = 'P';
 C_DeviceType_ESK   : String = 'K';
 C_DeviceType_SIM   : String = 'S';

 C_InCommandAppname_CC : String = 'CCxxx';

// XML formatting
 xml_ProcessInstruction: String = 'version="1.0" encoding="UTF-8"';
 xml_Node_DocumentRoot : String = 'EltradeCommandRoot';
 xml_Node_FiscalizeRoot: String = 'FiscalizationData';
 xml_Node_ResultCode   : String = 'ResultCode';
 xml_Node_ErrorMessage : String = 'ErrorMessage';
 xml_Node_ErrorCode    : String = 'ErrorCode';
 xml_Node_Time         : String = 'Time';
 xml_Node_CRC          : String = 'Signature';
 xml_Node_Line         : String = 'Line';
 // login
 xml_Node_LoginData    : String = 'Login';
 xml_Node_LoginUser    : String = 'User';
 xml_Node_LoginPass    : String = 'Pass';
 xml_Node_LoginPermit  : String = 'Permissions';
 // change pass
 xml_Node_ChangePass   : String = 'ChangePass';
 xml_Node_ChangePin    : String = 'ChangePin';
 // test
 xml_Node_TestData     : String = 'TestData';
 // PC message
 xml_Node_SendMessage  : String = 'Message';
 xml_Node_MessageType  : String = 'MsgType';
 xml_Node_MessageBody  : String = 'MsgBody';
 xml_Node_CommitData   : String = 'CommitExtData';
 xml_Node_SendError    : String = 'ErrorLog';
 xml_Node_DevType      : String = 'DeviceType';

 // ESK
 xml_Node_ESKLogin     : String = 'ESKLogin';
 xml_Node_ESKData      : String = 'ESKData';
 xml_Node_ESKSerial    : String = 'ESKSerial';
 xml_Node_ESKVersion   : String = 'ESKVersion';
 xml_Node_ESKCode      : String = 'ESKCode';
 xml_Node_ESKKey       : String = 'ESKKey';
 xml_Node_ESKPin       : String = 'ESKPin';
 xml_Node_ESKProjects  : String = 'ESKProjects';
 xml_Node_ESKProject   : String = 'ESKProj';
 xml_Node_ESKPermit    : String = 'ESKPermissions';
 xml_Node_ESKExtParams : String = 'ESKExtParams'; 
 // dealer data
 xml_Node_DealerData   : String = 'DealerData';
 xml_Node_UserFullName : String = 'UFName';
 xml_Node_UserLevel    : String = 'ULevel';
 xml_Node_CompanyEIK   : String = 'CEik';
 xml_Node_CompanyName  : String = 'CName';
 xml_Node_CompanyTown  : String = 'CTown';
 xml_Node_CompanyAddres: String = 'CAddres';
 xml_Node_CompanyPhone : String = 'CPhone';
 xml_Node_CompanyContrN: String = 'CContrN';
 xml_Node_CompanyContrD: String = 'CContrD';
 xml_Node_BranchName   : String = 'BName';
 xml_Node_BranchTown   : String = 'BTown';
 xml_Node_BranchAddres : String = 'BAddres';
 xml_Node_BranchPhone  : String = 'BPhone';
 xml_Node_BranchID     : String = 'BId';
 xml_Node_UserID       : String = 'UId';
 xml_Node_BranchFiscCtr: String = 'BFiscCntr';
 // Export table to file
 xml_Node_ExportData   : String = 'ExpData';
 xml_Node_ExportTasks  : String = 'ExpTaskNames';
 // Check version for update
 xml_Node_CheckVersion : String = 'CheckVersion';
 xml_Node_ProjectName  : String = 'ProjectName';
 xml_Node_ProjectData  : String = 'ProjectData';
 xml_Node_ExtendedData : String = 'ExtendedData';
 xml_Node_FilesData    : String = 'FilesData';
 xml_Node_FileData     : String = 'File';
 xml_Node_FileName     : String = 'FName';
 xml_Node_FileMD5      : String = 'FMD5';
 xml_Node_FileVersion  : String = 'FVer';
 xml_Node_FileDate     : String = 'FDate';
 xml_Node_FileSize     : String = 'FSize';
 xml_Node_Offset       : String = 'FOffset';
 xml_Node_PartSize     : String = 'FPartSize';
 xml_Node_FileContent  : String = 'FContent';
 xml_Node_DownloadFile : String = 'DownloadFile';
 // NRA sim reg
 xml_Node_NraSimReg    : String = 'SimRegRequest';
 xml_Node_NRASimRegData: String = 'NraSimRegData';
 xml_Node_NRASimStatus : String = 'NraSimStatus';
 xml_Node_NRAReqType   : String = 'NraReqType';
 xml_Node_NRAReqList   : String = 'NraRequestList';
 xml_Node_NRAReqResult : String = 'NraRequestResult';
 xml_Node_NRAReq       : String = 'NraReq';
 // Fiscalization
 xml_Node_FiscData     : String = 'FiscalizationData';
 xml_Node_FiscReqType  : String = 'RequestType';
 xml_Node_FiscReqID    : String = 'RequestID';
 xml_Node_FiscReqCmnt  : String = 'RequestComment';
 xml_Node_FiscReqDate  : String = 'RequestDate';
 xml_Node_FiscCommit   : String = 'FiscalizationCommit';
 xml_Node_DevSerial    : String = 'FiscDevSerial';
 xml_Node_FiscReggId   : String = 'FiscRegistrID';
 xml_Node_TimeElapsed  : String = 'TimeElapsed';
 xml_Node_FiscResult   : String = 'FiscResult';
 xml_Node_FiscError    : String = 'FiscErrorMsg';
 xml_Node_TestMode     : String = 'TestMode';
 xml_Node_FiscRequest  : String = 'FiscRequest';
 // SIM payment
 xml_Node_SimStatus    : String = 'SimStatus';
 xml_Node_SimImsi      : String = 'IMSI';
 xml_Node_SimImsiNew   : String = 'IMSInew';
 xml_Node_SimMsisdn    : String = 'MSISDN';
 xml_Node_SimMsisdnNew : String = 'MSISDNnew';
 xml_Node_SimIcc       : String = 'ICC';
 xml_Node_SimStatNap   : String = 'StNra';
 xml_Node_SimStatMob   : String = 'StMob';
 xml_Node_SimStatMobNew: String = 'StMobNew';
 xml_Node_SimCustEik   : String = 'CustEik';
 xml_Node_SimDevFisc   : String = 'DevF';
 xml_Node_SimDevGprs   : String = 'DevG';
 xml_Node_SimVerGprs   : String = 'VerG';
 xml_Node_SimOprCode   : String = 'OprC';
 xml_Node_SimOprCodeNew: String = 'OprCnew';
 xml_Node_SimOprName   : String = 'OprN';
 xml_Node_SimActivat   : String = 'Activ';
 xml_Node_SimPayedTo   : String = 'Payed';
 xml_Node_SimPayedToNew: String = 'PayedNew';
 xml_Node_SimPayPeriod : String = 'PayPeriod';
 xml_Node_SimMinPer    : String = 'MinPer';
 xml_Node_SimPayment   : String = 'SimPayment';
 xml_Node_SimAmmount   : String = 'SimAmmount';
 xml_Node_SimChargeTo  : String = 'ChargeTo';
 xml_Node_SimReplace   : String = 'SimReplace';
 xml_Node_SimStopOld   : String = 'StopOldSim';
 xml_Node_SimSource    : String = 'Source';
 xml_Node_SimExternal  : String = 'External';
 xml_Node_SimUnlockCode: String = 'SimUnlockC';
 xml_Node_SimApn2Url   : String = 'SimApn2Url';

 xml_Attr_Command      : String = 'Cmd';
 xml_Attr_Version      : String = 'Ver';
 xml_Attr_Application  : String = 'App';

// XML formatting CC modem
 xml_NodeCC_PingReq    : String = 'SRES';
 xml_NodeCC_PingRes    : String = 'SREQ';
 xml_NodeCC_ReportReq  : String = 'report';
 xml_NodeCC_DevSerMod  : String = 'ssn';
 xml_NodeCC_DevModel   : String = 'mod';
 xml_NodeCC_DevImsi    : String = 'imsi';
 xml_NodeCC_DevSerFu   : String = 'FDIN';
 xml_NodeCC_DevMFM     : String = 'FMIN';
 xml_NodeCC_DevVer     : String = 'SV';
 xml_NodeCC_DevNraType : String = 'RType';
 xml_NodeCC_DevRegId   : String = 'FDRID';
 xml_NodeCC_DevZSend   : String = 'zsent';
 xml_NodeCC_DevZSaved  : String = 'zsaved';
 xml_NodeCC_DevApn1ctr : String = 'apn1c';
 xml_NodeCC_DevApn2ctr : String = 'apn2c';
 xml_NodeCC_DevPayedTo : String = 'svcend';
 xml_NodeCC_FileRows   : String = '/fd0/1/n_2snd/ROWS';
 xml_NodeCC_FileRChge  : String = '/fd0/1/n_2snd/RCHANGE';
 xml_NodeCC_FileStpTsk : String = '/fd0/1/req/TSK';
 // xml_NodeCC_FileRows   : String = '/fd0/1/req/ROWS';
// xml_NodeCC_FileRChge  : String = '/fd0/1/req/RCHANGE';

 xml_NodeCC_Task       : String = 'tsk';
 xml_NodeCC_TaskType   : String = 'tt';
 xml_NodeCC_TaskId     : String = 'tid';
 xml_NodeCC_TaskStart  : String = 'SD';
 xml_NodeCC_TaskPeriod : String = 'prd';
 xml_NodeCC_Parameter  : String = 'par';
 xml_NodeCC_File       : String = 'file';
 xml_NodeCC_Report     : String = 'rep';
 xml_NodeCC_Name       : String = 'name';
 xml_NodeCC_Value      : String = 'val';
 xml_NodeCC_From       : String = 'from';
 xml_NodeCC_To         : String = 'to';
 xml_NodeCC_Receipt    : String = 'rcpt';

 xml_NodeCC_Event      : String = 'event';
 xml_NodeCC_ErrCode    : String = 'err';
 xml_NodeCC_ErrMsg     : String = 'msg';
// Commands
 cmd_DevTest     : String = 'Test';
 cmd_DevPing     : String = 'SRES';
 cmd_DevReport   : String = 'REPORT';
 cmd_DevUpd      : String = 'Update';

 cmd_ESKCreate   : String = 'ESK_CreateSignature';
 cmd_ESKRead     : String = 'ESK_ReadSignature';
 cmd_ESKChgePin  : String = 'ESK_ChangePin';

 cmd_PCTest         : String = 'System_Test';
 cmd_SysChangePass  : String = 'System_ChangePass';
 cmd_SysExpTblFiles : String = 'System_ExportTableFiles';
 cmd_SysCheckUpdate : String = 'System_CheckUpdate';
 cmd_SysDownloadFile: String = 'System_DownloadFile';
 cmd_SysSendMessage : String = 'System_SendMessage';
 cmd_SysCommitData  : String = 'System_Commit';
 cmd_SysSendError   : String = 'System_SendError';
 cmd_NRARegisterSim : String = 'Nra_RegisterSim';

 cmd_FiscValidate   : String = 'Fiscalize-Validate';
 cmd_FiscRequest    : String = 'Fiscalize-Request';
 cmd_FiscCommit     : String = 'Fiscalize-Commit';
 cmd_FiscGetReq     : String = 'Fiscalize-ReadReq';
 cmd_SimStatus      : String = 'SIM_Status';
 cmd_SimPayment     : String = 'SIM_Payment';
 cmd_SimChange      : String = 'SIM_Change';



 // Client Error Codes
 errcode_ExecuteFail_Internal     = -1;
 errcode_ExecuteSucceed           = 0;

 // Admin user errors
 errcode_LoginFail_AccessDenied   : Integer = 102;
 errcode_ChangePassFail_Denied    : Integer = 104;
 errcode_ChangePassFail_Invalid   : Integer = 105;

 // ESK login errors
 errcode_LoginFail_ESKNotFound    : Integer = 110;
 errcode_LoginFail_ESKDisabled    : Integer = 111;
 errcode_LoginFail_ESKExpired     : Integer = 112;
 errcode_LoginFail_ESKInvalidCode : Integer = 113;
 errcode_LoginFail_UserNotFound   : Integer = 114;
 errcode_LoginFail_UserDisabled   : Integer = 115;
 errcode_LoginFail_UserExpired    : Integer = 116;
 errcode_LoginFail_DealerDisabled : Integer = 117;
 errcode_LoginFail_DealerExpired  : Integer = 118;

 errcode_FileUpdateFail_Internal  : Integer = 121;
 errcode_ESKCreate_Denied         : Integer = 122;
 errcode_ExportFail_ConfigError   : Integer = 123;

 errcode_CheckUpd_NoProject       : Integer = 131;
 errcode_CheckUpd_ProjectNotExist : Integer = 132;
 errcode_DownlUpd_NoProject       : Integer = 133;
 errcode_DownlUpd_ProjectNotExist : Integer = 134;
 errcode_DownlUpd_FileNotFound    : Integer = 135;
 errcode_DownlUpd_ErrorOpenFile   : Integer = 136;
 errcode_CheckUpd_WrongParameter  : Integer = 137;

 // NRA communication errors
 errcode_NraSimReg_InvalidSimData : Integer = 150;
 errcode_NraSimReg_CreateXMLFail  : Integer = 151;
 errcode_NraSimReg_PostFail       : Integer = 152;
 errcode_NraSimReg_InvalidResp    : Integer = 153;
 errcode_NraSimReg_InvldXMLContnt : Integer = 154;
 errcode_NraSimReg_ReturnError    : Integer = 155;
 errcode_NraSimReg_PostFail3times : Integer = 156;
 errcode_NraSimReg_InvalidInput   : Integer = 157;

 // fiscalization - validation
 errcode_Fisc_MissingData         : Integer = 200;
 errcode_Fisc_InvalidData         : Integer = 201;
 errcode_Fisc_InvalidRequest      : Integer = 202;
 errcode_Fisc_DealerAccessDenied  : Integer = 211;
 errcode_Fisc_DealerUserDisabled  : Integer = 212;
 errcode_Fisc_DealerUserExpired   : Integer = 213;
 errcode_Fisc_DealerFirmDisabled  : Integer = 214;
 errcode_Fisc_DealerFirmExpired   : Integer = 215;
 errcode_Fisc_DealerLimitExceeded : Integer = 216;
 errcode_Fisc_DeviceNotExist      : Integer = 230;
 errcode_Fisc_DeviceNotRegistered : Integer = 231;
 errcode_Fisc_DeviceAlrRegistered : Integer = 232;
 errcode_Fisc_DeviceAlrDisabled   : Integer = 233;

 errcode_Sim_NotExist             : Integer = 250;
 errcode_Sim_Disabled             : Integer = 251;
 errcode_Sim_WaitApproval         : Integer = 252;
 errcode_Sim_PriceError           : Integer = 253;
 errcode_Sim_PeriodError          : Integer = 254;
 errcode_Sim_NotAssigned          : Integer = 255;

 errcode_Fisc_InternalError       : Integer = 260;
 errcode_Fisc_RequestNotFound     : Integer = 270;
 errcode_Fisc_RequestNotMatch     : Integer = 271;
 errcode_Fisc_RequestInvalidData  : Integer = 272;
 errcode_Fisc_ModemNeedUpdate     : Integer = 280;
 errcode_Fisc_TestModeMismatch    : Integer = 299;


 errcode_Modem_FailConnectModem   : Integer = 300;
 errcode_Modem_FailPutFile        : Integer = 301;
 errcode_Modem_FailWaitIdle       : Integer = 302;
 errcode_Modem_FailListFolder     : Integer = 303;
 errcode_Modem_FailReadReggID     : Integer = 304;
 errcode_Modem_InvalidReggID      : Integer = 305;

 errcode_FailProcessClientError   : Integer = 500;

 // грешки изпращани към сървъра
 errcode_ModemHardwareTestFail    : Integer = 600;
 errcode_ModemFwUpdateFail        : Integer = 601;
 errcode_OpenDatabaseFail         : Integer = 602;
 errcode_DatabaseError            : Integer = 603;
 errcode_PrintingError            : Integer = 604;
 errcode_CommitOperationFail      : Integer = 605;
 errcode_FiscOperationFail        : Integer = 606;
 errcode_PerformModemFormat       : Integer = 607;
 errcode_ReSaveFiscParamsFail     : Integer = 608;
 errcode_ReSaveFiscParams         : Integer = 609;
 errcode_VNCAppNotFound           : Integer = 610;
 errcode_TryToRegFiscalizedDevice : Integer = 611;
 errcode_CheckSIMStatusFail       : Integer = 612;
 errcode_ExchangeSimFail          : Integer = 613;
 errcode_ModemNRATestFail         : Integer = 614;

 errcode_CC_TaskDeviceMismatch    : Integer = 700;
 errcode_CC_UnknownFDModel        : Integer = 701;
 errcode_CC_XMLParseFail          : Integer = 702;

 //[DEVICES_FU_ACTIONTYPES]
 fdevact_InitialInsert             = 1;  //	Първоначално въвеждане на данните в системата
 fdevact_FiscalizeRequest          = 2;  //	Заявка за фискализация
 fdevact_FiscalizeCommit           = 3;  //	Потвърждение на заявката за фискализация
 fdevact_SimActivation             = 4;  //	Активация на СИМ
 fdevact_InsertPayment             = 5;  // Плащане на абонамент
 fdevact_FiscChangeRequest         = 6;  //	Заявка промяна на обстоятелства
 fdevact_FiscChangeCommit          = 7;  //	Успешно потвърдена промана обстоятелства
 fdevact_FiscDropRequest           = 8;  //	Заявка за дерегистрация
 fdevact_FiscDropCommit            = 9;  //	Успешно потвърдена дерегистрация
 fdevact_MoveSimPayDate            = 10; // Удължаване на абонамент
 fdevact_MoveSimFromDevice         = 11; // Местене на СИМ от друго устройство
 fdevact_StopSimDuePayExpire       = 12; // Заявка за деактивиране на СИМ, поради изтекъл абонамент
 fdevact_ReplaceSIM                = 13; // Подмяна на СИМ карта
 fdevact_SysDeregistartionRequest  = 14; // Заявка за дерегистрация на каса през модем
 fdevact_SysDeregistartionCommit   = 15; // Потвърждаване дерегистрация на каса през модем
 fdevact_SysDeregistartionErroe    = 16; // Грешка при дерегистрация на каса през модем
 fdevact_PaymentRequestFromWeb     = 17; // Заявка за плащане на абонамент през сайта
 fdevact_PaymentRevoke             = 18; // Сторниране на плащане
 fdevact_ReturnDeviceToDealer      = 19; //	Връщане на устройството на първоначлен дилър
 fdevact_ChangeSimPaymentDate      = 20; //	Промяна датата на абонамента на СИМ
 fdevact_SimPaymentCheck           = 21; // Проверка абонамент СИМ за валидност
 fdevact_TerminateSimDuePayExpire  = 22; // Заявка за деактивиране на СИМ, поради изтекъл абонамент
 fdevact_Apn2FiscChangeCommit      = 23; // Отдалечена промяна на обстоятелства
 fdevact_RejectOperation           = 100;//	Отказ на операцията

 //[SIM_OPERATORACTIONS_TYPES]
 simoperact_InitialInsert          = 1; //	Импорт на нов СИМ
 simoperact_ActivateNewSim         = 2; //	Активиране на СИМ
 simoperact_DeactivateSim          = 3; //	Временно спиране на СИМ
 simoperact_ActivateStoppedSim     = 4; //  Активиране на спрян СИМ
 simoperact_SetChargeFree          = 5; //	Освобождаване от такса към МО
 simoperact_RequestDeactivateSim   = 6; //	Заявка за спиране на СИМ
 simoperact_RequeastActivateSim    = 7; //	Заявка за активиране на спрян СИМ
 simoperact_PaymentRequest         = 8; //	Заявка за плащане на абонамент на СИМ
 simoperact_PaymentDelete          = 9; //	Сторниране на плащане
 simoperact_CheckStatus            = 10;//	Проверка състояние SIM
 simoperact_Unspecified            = 100; // --


 //[SIM_EXTERNALACTIONS_TYPES]
 simextact_InitialInsert           = 1; //	Въвеждане нов СИМ
 simextact_SimRegistered           = 2; //	Регистрация СИМ
 simextact_RegisterRequest         = 3; //	Заявка за регистрация СИМ
 simextact_FDevRegistered          = 4; //	Регистрация ФУ


 C_DlrRight_AccFiscApp      = 'sys_access_fiscapp';
 C_DlrRight_AccSitre        = 'sys_access_partnerssite';
 C_DlrRight_ChangePermOther = 'sys_change_others_permissions';
 C_DlrRight_ChangePinOther  = 'sys_change_others_pin';
 C_DlrRight_ChangePermOwn   = 'sys_change_own_permissions';
 C_DlrRight_ChangePinOwn    = 'sys_change_own_pin';


implementation



end.
