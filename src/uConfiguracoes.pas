unit uConfiguracoes;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls, Buttons, IniFiles;

type
  TfConfiguracoes = class(TForm)
    GroupBoxAtalhos: TGroupBox;
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
    hkSpMonitor: THotKey;
    hkSpMonitor3: THotKey;
    Bevel2: TBevel;
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
    GroupBoxNomeMenu: TGroupBox;
    LabelNomeMenu: TLabel;
    EditNomeMenu: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private
    FoArquivoINI: TIniFile;

    function PegarAtalho(const psNomeChave: string): TShortCut;
    function PegarNomeMenu: string;
    procedure CarregarAtalhos;
    procedure SalvarAtalho(const psNomeChave: string; const poAtalho: TShortCut);
    procedure SalvarNomeMenu;
  end;

var
  fConfiguracoes: TfConfiguracoes;

implementation

uses
  Menus, uConstantes;

{$R *.DFM}

{ TfPersonalizarAtalhos }

procedure TfConfiguracoes.FormCreate(Sender: TObject);
begin
  FoArquivoINI := TIniFile.Create(sPATH_ARQUIVO_INI); //PC_OK
  CarregarAtalhos;
  EditNomeMenu.Text := PegarNomeMenu;
end;

procedure TfConfiguracoes.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FoArquivoINI); //PC_OK
end;

procedure TfConfiguracoes.CarregarAtalhos;
begin
  hkServidor.HotKey := PegarAtalho('AbrirServidor');
  hkAplicacao.HotKey := PegarAtalho('AbrirAplicacao');
  hkDiretorioBin.HotKey := PegarAtalho('AbrirDiretorioBin');
  hkSpCfg.HotKey := PegarAtalho('AbrirSpCfg');
  hkItemRTC.HotKey := PegarAtalho('AbrirItemRTC');
  hkVisualizaDTS.HotKey := PegarAtalho('AbrirVisualizaDTS');
  hkSpMonitor.HotKey := PegarAtalho('AbrirSpMonitor');
  hkSpMonitor3.HotKey := PegarAtalho('AbrirSpMonitor3');
  hkConsultarNoRansack.HotKey := PegarAtalho('ConsultarRansack');
  hkVisualizarDataSet.HotKey := PegarAtalho('VisualizarDataSet');
  hkVisualizarDataSetManual.HotKey := PegarAtalho('VisualizarDataSetManual');
  hkLerTStringList.HotKey := PegarAtalho('LerTStringList');
end;

procedure TfConfiguracoes.btnOKClick(Sender: TObject);
begin
  SalvarAtalho('AbrirServidor', hkServidor.HotKey);
  SalvarAtalho('AbrirAplicacao', hkAplicacao.HotKey);
  SalvarAtalho('AbrirDiretorioBin', hkDiretorioBin.HotKey);
  SalvarAtalho('AbrirSpCfg', hkSpCfg.HotKey);
  SalvarAtalho('AbrirItemRTC', hkItemRTC.HotKey);
  SalvarAtalho('AbrirVisualizaDTS', hkVisualizaDTS.HotKey);
  SalvarAtalho('AbrirSpMonitor', hkSpMonitor.HotKey);
  SalvarAtalho('AbrirSpMonitor3', hkSpMonitor3.HotKey);
  SalvarAtalho('ConsultarRansack', hkConsultarNoRansack.HotKey);
  SalvarAtalho('VisualizarDataSet', hkVisualizarDataSet.HotKey);
  SalvarAtalho('VisualizarDataSetManual', hkVisualizarDataSetManual.HotKey);
  SalvarAtalho('LerTStringList', hkLerTStringList.HotKey);

  SalvarNomeMenu;

  Close;
end;

procedure TfConfiguracoes.SalvarAtalho(const psNomeChave: string; const poAtalho: TShortCut);
begin
  FoArquivoINI.WriteString(sSECAO_ATALHOS, psNomeChave, ShortCutToText(poAtalho));
end;

function TfConfiguracoes.PegarAtalho(const psNomeChave: string): TShortCut;
begin
  result := TextToShortCut(FoArquivoINI.ReadString(sSECAO_ATALHOS, psNomeChave, EmptyStr));
end;

function TfConfiguracoes.PegarNomeMenu: string;
begin
  result := FoArquivoINI.ReadString(sSECAO_PARAMETROS, 'NomeMenu', sMENU_DB1);
end;

procedure TfConfiguracoes.SalvarNomeMenu;
begin
  FoArquivoINI.WriteString(sSECAO_PARAMETROS, 'NomeMenu', Trim(EditNomeMenu.Text));
end;

end.

