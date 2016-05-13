unit uConfigurarAtalhos;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls, Buttons, IniFiles;

type
  TfConfigurarAtalhos = class(TForm)
    gbAtalhos: TGroupBox;
    hkServidor: THotKey;
    hkAplicacao: THotKey;
    hkDiretorioBin: THotKey;
    hkSpCfg: THotKey;
    hkItemRTC: THotKey;
    hkVisualizaDTS: THotKey;
    lbAbrirServidor: TLabel;
    lbAbrirAplicacao: TLabel;
    lbAbrirDiretorioBin: TLabel;
    lbAbrirSpCfg: TLabel;
    lbAbrirItemRTC: TLabel;
    lbAbrirVisualizaDTS: TLabel;
    lbSpMonitor: TLabel;
    lbSpMonitor3: TLabel;
    lbSelectSQL: TLabel;
    lbSqlDbx: TLabel;
    lbWinSpy: TLabel;
    hkSpMonitor: THotKey;
    hkSpMonitor3: THotKey;
    hkSelectSQL: THotKey;
    hkSqlDbx: THotKey;
    hkWinSpy: THotKey;
    bvl1: TBevel;
    Bevel1: TBevel;
    lbVisualizarDataSet: TLabel;
    lblVisualizarDataSetManual: TLabel;
    lbLerTStringList: TLabel;
    hkVisualizarDataSet: THotKey;
    hkVisualizarDataSetManual: THotKey;
    hkLerTStringList: THotKey;
    btnOK: TBitBtn;
    lbConsultarNoRansack: TLabel;
    hkConsultarNoRansack: THotKey;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private
    FoArquivoINI: TIniFile;

    function PegarAtalho(const psNomeChave: string): TShortCut;
    procedure CarregarAtalhos;
    procedure SalvarAtalho(const psNomeChave: string; const poAtalho: TShortCut);
  end;

var
  fConfigurarAtalhos: TfConfigurarAtalhos;

implementation

uses
  Menus, uConstantes;

{$R *.DFM}

{ TfPersonalizarAtalhos }

procedure TfConfigurarAtalhos.FormCreate(Sender: TObject);
begin
  FoArquivoINI := TIniFile.Create(sPATH_ARQUIVO_INI); //PC_OK
  CarregarAtalhos;
end;

procedure TfConfigurarAtalhos.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FoArquivoINI); //PC_OK
end;

procedure TfConfigurarAtalhos.CarregarAtalhos;
begin
  hkServidor.HotKey := PegarAtalho('AbrirServidor');
  hkAplicacao.HotKey := PegarAtalho('AbrirAplicacao');
  hkDiretorioBin.HotKey := PegarAtalho('AbrirDiretorioBin');
  hkSpCfg.HotKey := PegarAtalho('AbrirSpCfg');
  hkItemRTC.HotKey := PegarAtalho('AbrirItemRTC');
  hkVisualizaDTS.HotKey := PegarAtalho('AbrirVisualizaDTS');
  hkSpMonitor.HotKey := PegarAtalho('AbrirSpMonitor');
  hkSpMonitor3.HotKey := PegarAtalho('AbrirSpMonitor3');
  hkSelectSQL.HotKey := PegarAtalho('AbrirSelectSQL');
  hkSqlDbx.HotKey := PegarAtalho('AbrirSqlDbx');
  hkWinSpy.HotKey := PegarAtalho('AbrirWinSpy');
  hkConsultarNoRansack.HotKey := PegarAtalho('ConsultarRansack');
  hkVisualizarDataSet.HotKey := PegarAtalho('VisualizarDataSet');
  hkVisualizarDataSetManual.HotKey := PegarAtalho('VisualizarDataSetManual');
  hkLerTStringList.HotKey := PegarAtalho('LerTStringList');
end;

procedure TfConfigurarAtalhos.btnOKClick(Sender: TObject);
begin
  SalvarAtalho('AbrirServidor', hkServidor.HotKey);
  SalvarAtalho('AbrirAplicacao', hkAplicacao.HotKey);
  SalvarAtalho('AbrirDiretorioBin', hkDiretorioBin.HotKey);
  SalvarAtalho('AbrirSpCfg', hkSpCfg.HotKey);
  SalvarAtalho('AbrirItemRTC', hkItemRTC.HotKey);
  SalvarAtalho('AbrirVisualizaDTS', hkVisualizaDTS.HotKey);
  SalvarAtalho('AbrirSpMonitor', hkSpMonitor.HotKey);
  SalvarAtalho('AbrirSpMonitor3', hkSpMonitor3.HotKey);
  SalvarAtalho('AbrirSelectSQL', hkSelectSQL.HotKey);
  SalvarAtalho('AbrirSqlDbx', hkSqlDbx.HotKey);
  SalvarAtalho('AbrirWinSpy', hkWinSpy.HotKey);
  SalvarAtalho('ConsultarRansack', hkConsultarNoRansack.HotKey);
  SalvarAtalho('VisualizarDataSet', hkVisualizarDataSet.HotKey);
  SalvarAtalho('VisualizarDataSetManual', hkVisualizarDataSetManual.HotKey);
  SalvarAtalho('LerTStringList', hkLerTStringList.HotKey);

  Close;
end;

procedure TfConfigurarAtalhos.SalvarAtalho(const psNomeChave: string; const poAtalho: TShortCut);
begin
  FoArquivoINI.WriteString(sSECAO_ATALHOS, psNomeChave, ShortCutToText(poAtalho));
end;

function TfConfigurarAtalhos.PegarAtalho(const psNomeChave: string): TShortCut;
begin
  result := TextToShortCut(FoArquivoINI.ReadString(sSECAO_ATALHOS, psNomeChave, EmptyStr));
end;

end.

