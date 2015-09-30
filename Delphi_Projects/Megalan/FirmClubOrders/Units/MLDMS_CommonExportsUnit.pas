unit MLDMS_CommonExportsUnit;

interface

uses
  Menus, Classes;

type
  TCommonExportsSupport = (cesExcel, cesHTML, cesXML, cesTXT);

  TCommonExports = class
  public
    class function ExportGridToCSV(aGrid:TObject; const aSep:string=','):boolean;
    class function ExportGridTo(aGrid:TObject; const aExportFormat:TCommonExportsSupport = cesExcel; const aExpand:boolean=True):boolean;
    class function ExportTreeListTo(aTreeList:TObject; const aExportFormat:TCommonExportsSupport = cesExcel; const aExpand:boolean=True;
      const aSaveAll:Boolean = True; const aUseNativeFormat:Boolean = True):boolean;
  end;{TCommonExports}

  TCommonExportsClass = class of TCommonExports;

type
  TcxBaseExportMenuGroup = class(TMenuItem)
  private
    procedure ItemClick(Sender: TObject);
  protected
    procedure DoItemClick(APopupComponent:TComponent; const aExportTag:Integer); virtual; abstract;
  public
    class function CreateMenuGroup(aMenu:TPopupMenu; aInsertBefore:TMenuItem):TcxBaseExportMenuGroup; virtual;
    destructor Destroy; override;
  end;{TcxBaseExportMenuGroup}

  TcxGridExportMenuGroup = class(TcxBaseExportMenuGroup)
  protected
    procedure DoItemClick(APopupComponent:TComponent; const aExportTag:Integer); override;
  end;{TcxGridExportMenuGroup}

  TcxTreeListExportMenuGroup = class(TcxBaseExportMenuGroup)
  protected
    procedure DoItemClick(APopupComponent:TComponent; const aExportTag:Integer); override;
  end;{TcxTreeListExportMenuGroup}

type
  TMenuGroupParamInitValues = (mgpUnchecked, mgpChecked, mgpNonExisting);
  TcxGridViewControlsMenuGroupParams = class;

  TcxGridViewControlsMenuGroup = class(TMenuItem)
    procedure ItemClick(Sender: TObject);
  protected
    class function InternalCreate(aMenu:TPopupMenu; aInsertBefore:TMenuItem):TcxGridViewControlsMenuGroup;
    procedure CreateChildItems; virtual;
  public
    FParams:TcxGridViewControlsMenuGroupParams;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function CreateMenuGroup(aMenu:TPopupMenu; aInsertBefore:TMenuItem; aInitCallbackMethod:TNotifyEvent):TcxGridViewControlsMenuGroup; overload;
    class function CreateMenuGroup(aMenu:TPopupMenu; aInsertBefore:TMenuItem; aGridView:TObject):TcxGridViewControlsMenuGroup; overload;
  end;{TcxGridViewControlsMenuGroup}

  TcxGridViewControlsMenuGroupParams = class
  private
    FGroupByBox      : TMenuGroupParamInitValues;
    FFooter          : TMenuGroupParamInitValues;
    FColumnAutoWidth : TMenuGroupParamInitValues;
    FCellAutoHeight  : TMenuGroupParamInitValues;
  public
    constructor Create; overload;
    constructor Create(aGroupByBox, aFooter, aColumnAutoWidth, aCellAutoHeight:TMenuGroupParamInitValues); overload;
    property GroupByBox      : TMenuGroupParamInitValues read FGroupByBox      write FGroupByBox;       //OptionsView.GroupByBox
    property Footer          : TMenuGroupParamInitValues read FFooter          write FFooter;           //OptionsView.Footer
    property ColumnAutoWidth : TMenuGroupParamInitValues read FColumnAutoWidth write FColumnAutoWidth;  //OptionsView.ColumnAutoWidth
    property CellAutoHeight  : TMenuGroupParamInitValues read FCellAutoHeight  write FCellAutoHeight;   //OptionsView.CellAutoHeight
  end;{TcxGridViewControlsMenuGroupParams}

var
  CommonExports:TCommonExportsClass;

implementation

uses
  {$IFDEF VER140}cxExportGrid4Link, cxExportTL4Link,{$ELSE}cxGridExportLink, cxTLExportLink,{$ENDIF}
  Controls, Dialogs, Forms, cxGrid, cxGridCustomView, cxGridCustomTableView, cxGridTableView, cxTL;

const
  gcSupportedFileTypes:array[TCommonExportsSupport] of string = ('MS Excel', 'HTML', 'XML', 'Text');
  gcSupportedFileEXT:array[TCommonExportsSupport] of string = ('xls', 'html', 'xml', 'txt');
  gcSupportedFileDlgFltr:array[TCommonExportsSupport] of string = (
    'MS Excel (*.xls)|*.xls|Âñè÷êè (*.*)|*.*',
    'HTML (*.html)|*.html|Âñè÷êè (*.*)|*.*',
    'XML (*.xml)|*.xml|Âñè÷êè (*.*)|*.*',
    'Òåêñòîâ (*.txt)|*.txt|Âñè÷êè (*.*)|*.*');

const
  EXPORTTAG_Excel = 1024;
  EXPORTTAG_HTML  = 1025;
  EXPORTTAG_XML   = 1026;
  EXPORTTAG_TXT   = 1027;

{—— TCommonExports ————————————————————————————————————————————————————————————————————————————————}

class function TCommonExports.ExportGridToCSV(aGrid:TObject; const aSep:string): boolean;
var Dlg:TSaveDialog;
begin
  Dlg:=TSaveDialog.Create(Application);
  try
    Dlg.DefaultExt:='csv';
    Dlg.Filter:='Òåêñòîâ (*.csv)|*.csv|Âñè÷êè (*.*)|*.*';
    Result:=Dlg.Execute;
    If(Result)then try
      {$IFDEF VER140}
      ExportGrid4ToText(Dlg.FileName, TcxGrid(aGrid), True, True, aSep, '', '', Dlg.DefaultExt);
      {$ELSE}
      ExportGridToText(Dlg.FileName, TcxGrid(aGrid), True, True, aSep, '', '', Dlg.DefaultExt);
      {$ENDIF}
    finally Screen.Cursor:=crDefault; end;//try-finally
  finally Dlg.Free; end;//try-finally
end;{TCommonExports.ExportGridToCSV}

class function TCommonExports.ExportGridTo(aGrid:TObject; const aExportFormat:TCommonExportsSupport; const aExpand:boolean):boolean;
var Dlg:TSaveDialog;
begin
  Dlg:=TSaveDialog.Create(Application);
  try
    Dlg.DefaultExt:=gcSupportedFileEXT[aExportFormat];
    Dlg.Filter:=gcSupportedFileDlgFltr[aExportFormat];
    Result:=Dlg.Execute;
    If(Result)then try
      Screen.Cursor:=crHourGlass;
      case aExportFormat of
        {$IFDEF VER140}
        cesExcel: ExportGrid4ToExcel(Dlg.FileName, TcxGrid(aGrid), aExpand);
        cesHTML : ExportGrid4ToHTML(Dlg.FileName, TcxGrid(aGrid), aExpand);
        cesXML  : ExportGrid4ToXML(Dlg.FileName, TcxGrid(aGrid), aExpand);
        cesTXT  : ExportGrid4ToText(Dlg.FileName, TcxGrid(aGrid), aExpand);
        {$ELSE}
        cesExcel: ExportGridToExcel(Dlg.FileName, TcxGrid(aGrid), aExpand, True, False);
        cesHTML : ExportGridToHTML(Dlg.FileName, TcxGrid(aGrid), aExpand);
        cesXML  : ExportGridToXML(Dlg.FileName, TcxGrid(aGrid), aExpand);
        cesTXT  : ExportGridToText(Dlg.FileName, TcxGrid(aGrid), aExpand);
        {$ENDIF}
      end;//case
    finally Screen.Cursor:=crDefault; end;//try-finally
  finally Dlg.Free; end;//try-finally
end;{TCommonExports.ExportGridTo}

class function TCommonExports.ExportTreeListTo(aTreeList:TObject; const aExportFormat:TCommonExportsSupport; const aExpand, aSaveAll, aUseNativeFormat:Boolean):boolean;
var Dlg:TSaveDialog;
begin
  Dlg:=TSaveDialog.Create(Application);
  try
    Dlg.DefaultExt:=gcSupportedFileEXT[aExportFormat];
    Dlg.Filter:=gcSupportedFileDlgFltr[aExportFormat];
    Result:=Dlg.Execute;
    If(Result)then try
      Screen.Cursor:=crHourGlass;
      case aExportFormat of
        cesExcel: cxExportTLToExcel(Dlg.FileName, TcxCustomTreeList(aTreeList), aExpand, aSaveAll, aUseNativeFormat);
        cesHTML : cxExportTLToHTML(Dlg.FileName, TcxCustomTreeList(aTreeList), aExpand, aSaveAll);
        cesXML  : cxExportTLToXML(Dlg.FileName, TcxCustomTreeList(aTreeList), aExpand, aSaveAll);
        cesTXT  : cxExportTLToText(Dlg.FileName, TcxCustomTreeList(aTreeList), aExpand, aSaveAll);
      end;//case
    finally Screen.Cursor:=crDefault; end;//try-finally
  finally Dlg.Free; end;//try-finally
end;{TCommonExports.ExportTreeListTo}

{—— TcxBaseExportMenuGroup ————————————————————————————————————————————————————————————————————————}

class function TcxBaseExportMenuGroup.CreateMenuGroup(aMenu:TPopupMenu; aInsertBefore:TMenuItem):TcxBaseExportMenuGroup;
var lvItem:TMenuItem;
begin
  Result:=self.Create(aMenu);
  If(aInsertBefore<>nil)then
    aMenu.Items.Insert(aMenu.Items.IndexOf(aInsertBefore), Result);
  Result.Caption:='Åêñïîðò';
  //---------- TXT
  lvItem:=TMenuItem.Create(aMenu);
  Result.Insert(0, lvItem);
  lvItem.Caption:='Text file(txt)'; lvItem.Hint:='Åêñïîðò â òåêñòîâ ôàéë'; lvItem.Tag:=EXPORTTAG_TXT;
  lvItem.OnClick:=Result.ItemClick;
  //---------- XML
  lvItem:=TMenuItem.Create(aMenu);
  Result.Insert(0, lvItem);
  lvItem.Caption:='XML file(xml)'; lvItem.Hint:='Åêñïîðò â XML ôîðìàò'; lvItem.Tag:=EXPORTTAG_XML;
  lvItem.OnClick:=Result.ItemClick;
  //---------- HTML
  lvItem:=TMenuItem.Create(aMenu);
  Result.Insert(0, lvItem);
  lvItem.Caption:='HTML file(html)'; lvItem.Hint:='Åêñïîðò âúâ web ôîðìàò'; lvItem.Tag:=EXPORTTAG_HTML;
  lvItem.OnClick:=Result.ItemClick;
  //---------- Excel
  lvItem:=TMenuItem.Create(aMenu);
  Result.Insert(0, lvItem);
  lvItem.Caption:='MS Excel(xls)'; lvItem.Hint:='Åêñïîðò êúì excel'; lvItem.Tag:=EXPORTTAG_Excel;
  lvItem.OnClick:=Result.ItemClick;
end;{TcxBaseExportMenuGroup.CreateMenuGroup}

destructor TcxBaseExportMenuGroup.Destroy;
begin
  inherited Destroy;
end;{TcxBaseExportMenuGroup.Destroy}

procedure TcxBaseExportMenuGroup.ItemClick(Sender: TObject);
begin
  If(csDestroying in ComponentState)then EXIT;
  If(Sender is TMenuItem)and(TMenuItem(Sender).Owner is TPopupMenu)and(Assigned(TPopupMenu(TMenuItem(Sender).Owner).PopupComponent))then
    DoItemClick(TPopupMenu(TMenuItem(Sender).Owner).PopupComponent, TMenuItem(Sender).Tag);
end;{TcxBaseExportMenuGroup.ItemClick}

{—— TcxTreeListExportMenuGroup ————————————————————————————————————————————————————————————————————}

procedure TcxTreeListExportMenuGroup.DoItemClick(APopupComponent:TComponent; const aExportTag:Integer);
begin
  inherited;
  If(Assigned(APopupComponent))and(APopupComponent is TcxCustomTreeList)then begin
    //S:=APopupComponent.ClassName; Application.MessageBox(PChar(S), '', 0);
    case aExportTag of
      EXPORTTAG_Excel : CommonExports.ExportTreeListTo(APopupComponent);
      EXPORTTAG_HTML  : CommonExports.ExportTreeListTo(APopupComponent, cesHTML);
      EXPORTTAG_XML   : CommonExports.ExportTreeListTo(APopupComponent, cesXML);
      EXPORTTAG_TXT   : CommonExports.ExportTreeListTo(APopupComponent, cesTXT);
    end;//case
  end;//If
end;{TcxTreeListExportMenuGroup.DoItemClick}

{—— TcxGridExportMenuGroup ———————————————————————————————————————————————————————————————————————————————}

procedure TcxGridExportMenuGroup.DoItemClick(APopupComponent:TComponent; const aExportTag:Integer);
var G:TcxGrid;
begin
  inherited;
  If(Assigned(APopupComponent))then begin
    //S:=APopupComponent.ClassName; Application.MessageBox(PChar(S), '', 0);
    If(APopupComponent is TcxGridSite)and(TcxGridSite(APopupComponent).Container is TcxGrid)then G:=TcxGridSite(APopupComponent).Container as TcxGrid
    else If(APopupComponent is TcxGrid)then G:=APopupComponent as TcxGrid
    else If(APopupComponent is TcxCustomGridTableView)and(TcxCustomGridTableView(APopupComponent).Control is TcxGrid)then G:=TcxCustomGridTableView(APopupComponent).Control as TcxGrid
    else EXIT;
    case aExportTag of
      EXPORTTAG_Excel : CommonExports.ExportGridTo(G);
      EXPORTTAG_HTML  : CommonExports.ExportGridTo(G, cesHTML);
      EXPORTTAG_XML   : CommonExports.ExportGridTo(G, cesXML);
      EXPORTTAG_TXT   : CommonExports.ExportGridTo(G, cesTXT);
    end;//case
  end;//If
end;{TcxGridExportMenuGroup.DoItemClick}

{—— TcxGridViewControlsMenuGroupParams ————————————————————————————————————————————————————————————}

constructor TcxGridViewControlsMenuGroupParams.Create;
begin
  FGroupByBox:=mgpChecked;      FFooter:=mgpChecked;
  FColumnAutoWidth:=mgpChecked; FCellAutoHeight:=mgpChecked;
end;{TcxGridViewControlsMenuGroupParams.Create}

constructor TcxGridViewControlsMenuGroupParams.Create(aGroupByBox, aFooter, aColumnAutoWidth, aCellAutoHeight:TMenuGroupParamInitValues);
begin
  FGroupByBox      := aGroupByBox;
  FFooter          := aFooter;
  FColumnAutoWidth := aColumnAutoWidth;
  FCellAutoHeight  := aCellAutoHeight;
end;{TcxGridViewControlsMenuGroupParams.Create}

{—— TcxGridViewControlsMenuGroup ——————————————————————————————————————————————————————————————————}

class function TcxGridViewControlsMenuGroup.InternalCreate(aMenu:TPopupMenu; aInsertBefore:TMenuItem):TcxGridViewControlsMenuGroup;
begin
  Result:=TcxGridViewControlsMenuGroup.Create(aMenu);
  If(aInsertBefore<>nil)then
    aMenu.Items.Insert(aMenu.Items.IndexOf(aInsertBefore), Result);
  Result.Caption:='Òàáëè÷íè êîíòðîëè';
end;{TcxGridViewControlsMenuGroup.InternalCreate}

class function TcxGridViewControlsMenuGroup.CreateMenuGroup(aMenu:TPopupMenu; aInsertBefore:TMenuItem; aInitCallbackMethod:TNotifyEvent): TcxGridViewControlsMenuGroup;
begin
  Result:=InternalCreate(aMenu, aInsertBefore);
  If(Assigned(aInitCallbackMethod))then
    aInitCallbackMethod(Result);
  Result.CreateChildItems;
end;{TcxGridViewControlsMenuGroup.CreateMenuGroup}

class function TcxGridViewControlsMenuGroup.CreateMenuGroup(aMenu:TPopupMenu; aInsertBefore:TMenuItem; aGridView:TObject):TcxGridViewControlsMenuGroup;
begin
  Result:=InternalCreate(aMenu, aInsertBefore);
  If(aGridView is TcxGridTableView)then begin
    Result.FParams.GroupByBox:=TMenuGroupParamInitValues(TcxGridTableView(aGridView).OptionsView.GroupByBox);
    Result.FParams.Footer:=TMenuGroupParamInitValues(TcxGridTableView(aGridView).OptionsView.Footer);
    Result.FParams.ColumnAutoWidth:=TMenuGroupParamInitValues(TcxGridTableView(aGridView).OptionsView.ColumnAutoWidth);
    Result.FParams.CellAutoHeight:=TMenuGroupParamInitValues(TcxGridTableView(aGridView).OptionsView.CellAutoHeight);
  end;//If
  Result.CreateChildItems;
end;{TcxGridViewControlsMenuGroup.CreateMenuGroup}

constructor TcxGridViewControlsMenuGroup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FParams:=TcxGridViewControlsMenuGroupParams.Create;
end;{TcxGridViewControlsMenuGroup.Create}

destructor TcxGridViewControlsMenuGroup.Destroy;
begin
  FParams.Free;
  inherited Destroy;
end;{TcxGridViewControlsMenuGroup.Destroy}

procedure TcxGridViewControlsMenuGroup.CreateChildItems;
var lvItem:TMenuItem; lvMenu:TPopupMenu;
begin
  lvMenu:=self.Owner as TPopupMenu;
  //---------- OptionsView.CellAutoHeight
  If(self.FParams.CellAutoHeight in[mgpUnchecked, mgpChecked])then begin
    lvItem:=TMenuItem.Create(lvMenu);
    self.Insert(0, lvItem);
    lvItem.Caption:='Àâòîìàòè÷åí ðàçìåð íà ðåäîâåòå'; lvItem.Hint:=''; lvItem.AutoCheck:=True; lvItem.Tag:=5004;
    lvItem.OnClick:=self.ItemClick; lvItem.Checked:=Boolean(self.FParams.CellAutoHeight);
  end;//If
  //---------- OptionsView.ColumnAutoWidth
  If(self.FParams.ColumnAutoWidth in[mgpUnchecked, mgpChecked])then begin
    lvItem:=TMenuItem.Create(lvMenu);
    self.Insert(0, lvItem);
    lvItem.Caption:='Àâòîìàòè÷åí ðàçìåð íà êîëîíèòå'; lvItem.Hint:=''; lvItem.AutoCheck:=True; lvItem.Tag:=5003;
    lvItem.OnClick:=self.ItemClick; lvItem.Checked:=Boolean(self.FParams.ColumnAutoWidth);
  end;//If
  //---------- OptionsView.Footer
  If(self.FParams.Footer in[mgpUnchecked, mgpChecked])then begin
    lvItem:=TMenuItem.Create(lvMenu);
    self.Insert(0, lvItem);
    lvItem.Caption:='Ëåíòà çà îáùè ñóìè'; lvItem.Hint:=''; lvItem.AutoCheck:=True; lvItem.Tag:=5002;
    lvItem.OnClick:=self.ItemClick; lvItem.Checked:=Boolean(self.FParams.Footer);
  end;//If
  //---------- OptionsView.GroupByBox
  If(FParams.GroupByBox in[mgpUnchecked, mgpChecked])then begin
    lvItem:=TMenuItem.Create(lvMenu);
    self.Insert(0, lvItem);
    lvItem.Caption:='Ëåíòà çà ãðóïèðàíå'; lvItem.Hint:=''; lvItem.AutoCheck:=True; lvItem.Tag:=5001;
    lvItem.OnClick:=self.ItemClick; lvItem.Checked:=Boolean(self.FParams.GroupByBox);
  end;//If
end;{TcxGridViewControlsMenuGroup.CreateChildItems}

procedure TcxGridViewControlsMenuGroup.ItemClick(Sender: TObject);
var GV:TcxGridTableView;
begin
  If(csDestroying in ComponentState)then EXIT;
  If(Sender is TMenuItem)and(TMenuItem(Sender).Owner is TPopupMenu)and
    (Assigned(TPopupMenu(TMenuItem(Sender).Owner).PopupComponent))then
  begin
    If(TPopupMenu(TMenuItem(Sender).Owner).PopupComponent is TcxGridSite)and
      (TcxGridSite(TPopupMenu(TMenuItem(Sender).Owner).PopupComponent).GridView is TcxGridTableView)then
    begin
      GV:=TcxGridTableView(TcxGridSite(TPopupMenu(TMenuItem(Sender).Owner).PopupComponent).GridView);
    end else If(TPopupMenu(TMenuItem(Sender).Owner).PopupComponent is TcxGrid)and
               (TcxGrid(TPopupMenu(TMenuItem(Sender).Owner).PopupComponent).ActiveView is TcxGridTableView)then
    begin
      GV:=TcxGridTableView(TcxGrid(TPopupMenu(TMenuItem(Sender).Owner).PopupComponent).ActiveView);
    end else If(TPopupMenu(TMenuItem(Sender).Owner).PopupComponent is TcxGridTableView)then
    begin
      GV:=TPopupMenu(TMenuItem(Sender).Owner).PopupComponent as TcxGridTableView;
    end else EXIT;

    If(GV<>nil)then
      case TMenuItem(Sender).Tag of
        5001: GV.OptionsView.GroupByBox      := TMenuItem(Sender).Checked;
        5002: GV.OptionsView.Footer          := TMenuItem(Sender).Checked;
        5003: GV.OptionsView.ColumnAutoWidth := TMenuItem(Sender).Checked;
        5004: GV.OptionsView.CellAutoHeight  := TMenuItem(Sender).Checked;
      end;//case
  end;//If

  {If(Sender is TMenuItem)and(TMenuItem(Sender).Owner is TPopupMenu)and
    (TPopupMenu(TMenuItem(Sender).Owner).PopupComponent is TcxGridSite)and
    (TcxGridSite(TPopupMenu(TMenuItem(Sender).Owner).PopupComponent).GridView is TcxGridTableView)then
  begin
    GV:=TcxGridTableView(TcxGridSite(TPopupMenu(TMenuItem(Sender).Owner).PopupComponent).GridView);
    case TMenuItem(Sender).Tag of
      5001: GV.OptionsView.GroupByBox      := TMenuItem(Sender).Checked;
      5002: GV.OptionsView.Footer          := TMenuItem(Sender).Checked;
      5003: GV.OptionsView.ColumnAutoWidth := TMenuItem(Sender).Checked;
      5004: GV.OptionsView.CellAutoHeight  := TMenuItem(Sender).Checked;
    end;//case
  end;//If}
end;{TcxGridViewControlsMenuGroup.ItemClick}

initialization
  CommonExports:=TCommonExports;
finalization

end.
