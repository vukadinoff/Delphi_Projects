program RptOrderArticles;

uses
  Forms,
  RptOrderArticlesFUnit in 'RptOrderArticlesFUnit.pas' {RptOrderArticlesF},
  FrameRptOrderArticlesUnit in 'FrameRptOrderArticlesUnit.pas' {FrameRptOrderArticlesF: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TRptOrderArticlesF, RptOrderArticlesF);
  Application.Run;
end.
