unit RptOrderArticlesFUnit;

interface

uses
  ActnList, Classes, Controls, ExtCtrls, Forms, ImgList,
  dxSkinsdxBarPainter, FrameRptOrderArticlesUnit, dxSkinsCore,
  dxSkinsDefaultPainters, dxBar, cxClasses;

type
  TRptOrderArticlesF = class(TForm)
    AL1          : TActionList;
    actClose     : TAction;
    actRefresh   : TAction;
    actPrint     : TAction;
    actExport    : TAction;

    BM1          : TdxBarManager;
    BM1Bar1      : TdxBar;
    btnClose     : TdxBarLargeButton;
    btnRefresh   : TdxBarLargeButton;
    btnPrint     : TdxBarLargeButton;
    btnExport    : TdxBarLargeButton;
    ilImages     : TImageList;

    pnlG1G2      : TPanel;

    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDestroy;

    procedure actCloseExecute(Sender: TObject);
    procedure actRefreshExecute(Sender: TObject);
    procedure actPrintExecute(Sender: TObject);
    procedure actExportExecute(Sender: TObject);
  private
    FrameRptOrderArticles: TFrameRptOrderArticlesF; //Frame instance variable
  end;

var
  RptOrderArticlesF: TRptOrderArticlesF;

implementation

uses
  MLDMS_CommonConstants;

{$R *.dfm}

procedure TRptOrderArticlesF.FormCreate(Sender: TObject);
begin
  FrameRptOrderArticles := TFrameRptOrderArticlesF.Create(RptOrderArticlesF);
  FrameRptOrderArticles.Parent := pnlG1G2;
end;

procedure TRptOrderArticlesF.FormActivate(Sender: TObject);
begin
  FrameRptOrderArticles.G1.SetFocus;
  FrameRptOrderArticles.G1V1.DataController.FocusedRowIndex := 0;
end;

procedure TRptOrderArticlesF.FormDestroy;
begin
  if (Assigned(FrameRptOrderArticles)) then
    FrameRptOrderArticles.Free;
end;

procedure TRptOrderArticlesF.actCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TRptOrderArticlesF.actRefreshExecute(Sender: TObject);
begin
  FrameRptOrderArticles.Notifier_RefreshAll;
end;

procedure TRptOrderArticlesF.actPrintExecute(Sender: TObject);
begin
  FrameRptOrderArticles.Notifier_PrintReport;
end;

procedure TRptOrderArticlesF.actExportExecute(Sender: TObject);
begin
  FrameRptOrderArticles.Notifier_ExportReport(cUnknownID);
end;

end.

