unit Main;

interface

uses
  Windows, SysUtils, Classes, Forms, Graphics, cxGraphics, cxControls,
  cxLookAndFeels, cxLookAndFeelPainters, cxStyles, dxSkinsCore,
  dxSkinsDefaultPainters, dxSkinscxPCPainter, cxCustomData, cxFilter,
  cxData, cxDataStorage, cxEdit, cxNavigator, DB, cxDBData, cxContainer,
  cxTextEdit, cxMaskEdit, cxDropDownEdit, cxGroupBox, cxGridLevel,
  cxClasses, cxGridCustomView, cxGridCustomTableView, cxGridTableView,
  cxGridDBTableView, Controls, cxGrid, cxGridStrs, DBClient, mySQLDbTables;

type
  TfrmMainForm = class(TForm)
    dbCafeDevDB     : TmySQLDatabase;
    mySQLQuery1     : TmySQLQuery;
    dsArticles      : TDataSource;

    cxGrid          : TcxGrid;

    cxgrpbx1        : TcxGroupBox;

    cxgrdlvlGrid1Level1: TcxGridLevel;
    TableViewDBColumn: TcxGridDBColumn;
    TableViewDBColumn1: TcxGridDBColumn;
    TableViewDBColumn2: TcxGridDBColumn;
    TableViewDBColumn3: TcxGridDBColumn;
    procedure TableViewCustomDrawCell(Sender: TcxCustomGridTableView;
              ACanvas: TcxCanvas; AViewInfo: TcxGridTableDataCellViewInfo;
              var ADone: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMainForm: TfrmMainForm;

implementation

{$R *.dfm}

uses
  Variants;

procedure TfrmMainForm.TableViewCustomDrawCell(
  Sender: TcxCustomGridTableView; ACanvas: TcxCanvas;
  AViewInfo: TcxGridTableDataCellViewInfo; var ADone: Boolean);
begin
  ACanvas.Brush.Color := clCream;
end;

begin
  cxSetResourceString(@scxGridGroupByBoxCaption, '<Лента за групиране>');
end.





