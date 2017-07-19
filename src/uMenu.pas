unit uMenu;

//jcf:format=off

interface

uses
  ToolsAPI, Classes, Menus, ActnList, Dialogs, Contnrs, ExtCtrls;

type
  TWizard = class(TInterfacedObject, IOTAWizard, IOTANotifier)
  private
    FoTimerAtalhos: TTimer;
    FoMenuPrincipal: TMenuItem;
    FoMenuMVP: TMenuItem;

    procedure AbrirArquivoMVP(Sender: TObject);
    procedure CriarItemMenuPrincipal(const psCaption, psIdentificador: string;
      poEvento: TNotifyEvent; const psMenuPai: string = '');
    procedure CriarItemMenuMVP(const psCaption, psIdentificador: string; const pnTag: integer = 0);
    procedure CriarMenuPrincipal;
    procedure CriarMenuMVP;
    procedure CriarTemporizadorAtalhos;
    procedure CriarPastaOutput;
    procedure AdicionarAcoesMenuPrincipal;
    procedure AdicionarAcoesMVP;
    procedure CarregarAtalhos;
    procedure AtribuirAtalhos(Sender: TObject);
    procedure MarcarMenu;
    procedure SelecionarSistemaPG(Sender: TObject);
    procedure SelecionarSistemaSG(Sender: TObject);
    procedure SelecionarSistemaMP(Sender: TObject);
    procedure AbrirConfiguracoes(Sender: TObject);
    procedure ProcessarArquivosMVP(Sender: TObject);
    function PegarAtalho(const psIdentificador: string): TShortCut;
    function PegarNomeMenuPrincipal: string;
    function ProcurarMenu(const psNomeMenu: string): TMenuItem;
  public
    constructor Create;

    function GetState: TWizardState;
    function GetIDString: string;
    function GetName: string;
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Execute;
    procedure Modified;
  end;

function InitWizard(const BorlandIDEServices: IBorlandIDEServices;
  RegisterProc: TWizardRegisterProc; var Terminate: TWizardTerminateProc) : boolean; stdcall;

exports
  InitWizard Name WizardEntryPoint;

procedure Register;

implementation

uses
  SysUtils, Forms, Windows, ComCtrls, Graphics, IniFiles, FileCtrl, uFuncoes, uConstantes;

var
  FoFuncoes: TFuncoes;
  FActions: TObjectList;
  FslAtalhos: TStringList;
  nIDWizard: integer = 0;

function InitialiseWizard(BIDES: IBorlandIDEServices): TWizard;
begin
  result := TWizard.Create;
  Application.Handle := (BIDES as IOTAServices).GetParentHandle;
end;

function InitWizard(Const BorlandIDEServices: IBorlandIDEServices;
  RegisterProc: TWizardRegisterProc; var Terminate: TWizardTerminateProc): boolean; stdcall;
begin
  result := BorlandIDEServices <> nil;
  RegisterProc(InitialiseWizard(BorlandIDEServices));
End;

procedure Register;
begin
  nIDWizard :=
    (BorlandIDEServices as IOTAWizardServices).AddWizard(InitialiseWizard(BorlandIDEServices));
    
  RegisterPackageWizard(TWizard.Create); //PC_OK
end;

{ TWizard }

procedure TWizard.CriarItemMenuPrincipal(const psCaption, psIdentificador: string;
  poEvento: TNotifyEvent; const psMenuPai: string = '');
var
  oNTAS: INTAServices;
  oAction: TAction;
  oBitmap: TBitmap;
  oMenuItem: TMenuItem;
  oMenuPai: TMenuItem;
begin
  oAction := nil;
  oNTAS := (BorlandIDEServices as INTAServices);
  oMenuItem := TMenuItem.Create(oNTAS.MainMenu); //PC_OK

  if Assigned(poEvento) then
  begin
    oAction := TAction.Create(oNTAS.ActionList); //PC_OK
    oAction.ActionList := oNTAS.ActionList;
    oAction.Name := psIdentificador;
    oAction.Caption := psCaption;
    oAction.OnExecute := poEvento;
    oAction.Category := 'PluginDB1';
    oAction.ShortCut := PegarAtalho(psIdentificador);

    if FileExists(Format('%s%s.bmp', [sPATH_IMAGENS, psIdentificador])) then
    begin
      oBitmap := TBitmap.Create;
      oBitmap.LoadFromFile(Format('%s%s.bmp', [sPATH_IMAGENS, psIdentificador]));
      oAction.ImageIndex := oNTAS.AddMasked(oBitmap, clFuchsia);
      oBitmap.Free;
    end;

    FActions.Add(oAction);
  end;

  oMenuItem.Caption := psCaption;
  oMenuItem.Action := oAction;
  oMenuItem.Name := 'im' + psIdentificador;

  if psMenuPai = EmptyStr then
    oMenuPai := FoMenuPrincipal
  else
    oMenuPai := ProcurarMenu(psMenuPai);

  oMenuPai.Add(oMenuItem);
end;

constructor TWizard.Create;
begin
  CriarMenuMVP;
  AdicionarAcoesMVP;

  CriarMenuPrincipal;
  AdicionarAcoesMenuPrincipal;

  CarregarAtalhos;
  MarcarMenu;
  CriarTemporizadorAtalhos;
  CriarPastaOutput;
end;

function TWizard.GetIDString: string;
begin
  result := 'DB1 Global Software';
end;

function TWizard.GetName: string;
begin
  result := 'Menu de Apoio para Desenvolvedores da DB1';
end;

procedure TWizard.AtribuirAtalhos(Sender: TObject);
var
  nCont: integer;
  oAction: TAction;
begin
  for nCont := 0 to Pred(FActions.Count) do
  begin
    oAction := FActions[nCont] as TAction;
    oAction.ShortCut := PegarAtalho(oAction.Name);
  end;

  if Assigned(FoTimerAtalhos) then
  begin
    FoTimerAtalhos.Enabled := False;
    FreeAndNil(FoTimerAtalhos); //PC_OK
  end;
end;

procedure TWizard.CriarMenuPrincipal;
var
  oMainMenu: TMainMenu;
begin
  oMainMenu := (BorlandIDEServices as INTAServices).MainMenu;
  FoMenuPrincipal := TMenuItem.Create(oMainMenu); //PC_OK
  FoMenuPrincipal.Name := 'miDB1';
  FoMenuPrincipal.Caption := PegarNomeMenuPrincipal;
  FoMenuPrincipal.OnClick := AtribuirAtalhos;
  oMainMenu.Items.Add(FoMenuPrincipal);
end;

procedure TWizard.SelecionarSistemaPG(Sender: TObject);
begin
  FoFuncoes.TipoSistema := tsPG;
  MarcarMenu;
end;

procedure TWizard.SelecionarSistemaSG(Sender: TObject);
begin
  FoFuncoes.TipoSistema := tsSG;
  MarcarMenu;
end;

procedure TWizard.MarcarMenu;
var
  nCont: integer;
  oAction: TAction;
  sNomeMenu: string;
begin
  case FoFuncoes.TipoSistema of
    tsPG: sNomeMenu := sNOME_PG;
    tsSG: sNomeMenu := sNOME_SG;
    tsMP: sNomeMenu := sNOME_MP;
  end;

  for nCont := 0 to Pred(FActions.Count) do
  begin
    oAction := FActions[nCont] as TAction;
    oAction.Checked := oAction.Caption = sNomeMenu;
  end;
end;

procedure TWizard.CriarTemporizadorAtalhos;
begin
  FoTimerAtalhos := TTimer.Create(nil); //PC_OK
  FoTimerAtalhos.Interval := 20000;
  FoTimerAtalhos.OnTimer := AtribuirAtalhos;
end;

procedure TWizard.AdicionarAcoesMenuPrincipal;
begin
  CriarItemMenuPrincipal('Abrir Servidor', 'AbrirServidor', FoFuncoes.AbrirServidor);
  CriarItemMenuPrincipal('Abrir Aplicação', 'AbrirAplicacao', FoFuncoes.AbrirAplicacao);
  CriarItemMenuPrincipal('Abrir ADM', 'AbrirADM', FoFuncoes.AbrirADM);
  CriarItemMenuPrincipal('Abrir Diretório Bin', 'AbrirDiretorioBin', FoFuncoes.AbrirDiretorioBin);
  CriarItemMenuPrincipal('Abrir spCfg.ini', 'AbrirSpCfg', FoFuncoes.AbrirSPCfg);
  CriarItemMenuPrincipal('Abrir Item no RTC', 'AbrirItemRTC', FoFuncoes.AbrirItemRTC);
  CriarItemMenuPrincipal('Excluir Cache', 'ExcluirCache', FoFuncoes.ExcluirCache);
  CriarItemMenuPrincipal('Finalizar Processos', 'FinalizarProcessos', FoFuncoes.FinalizarProcessos);

  CriarItemMenuPrincipal(sSEPARADOR, 'Separador1', nil);

  CriarItemMenuPrincipal('Compilar Projetos Cliente', 'CompilarCliente', FoFuncoes.CompilarProjetosClientes);
  CriarItemMenuPrincipal('Compilar Projetos Servidor', 'CompilarServidor', FoFuncoes.CompilarProjetosServidores);
  CriarItemMenuPrincipal('Compilar Todos', 'CompilarTodos', FoFuncoes.CompilarTodosProjetos);
  CriarItemMenuPrincipal('Compilação Personalizada', 'CompilacaoPersonalizada', FoFuncoes.CompilacaoPersonalizada);

  CriarItemMenuPrincipal(sSEPARADOR, 'Separador2', nil);

  CriarItemMenuPrincipal('Abrir VisualizaDTS', 'AbrirVisualizaDTS', FoFuncoes.AbrirVisualizaDTS);
  CriarItemMenuPrincipal('Abrir spMonitor', 'AbrirSpMonitor', FoFuncoes.AbrirSPMonitor);
  CriarItemMenuPrincipal('Abrir spMonitor3', 'AbrirSpMonitor3', FoFuncoes.AbrirSPMonitor3);
  CriarItemMenuPrincipal('Abrir SelectSQL', 'AbrirSelectSQL', FoFuncoes.AbrirSelectSQL);
  CriarItemMenuPrincipal('Abrir WinSpy', 'AbrirWinSpy', FoFuncoes.AbrirWinSpy);

  CriarItemMenuPrincipal(sSEPARADOR, 'Separador3', nil);

  CriarItemMenuPrincipal('Abrir Visualizador de DataSets', 'AbrirVisualizadorDataSets', FoFuncoes.AbrirVisualizadorDataSets);
  CriarItemMenuPrincipal('Exportar Dados do DataSet', 'ExportarDataSet', FoFuncoes.ExportarDadosDataSet);

  CriarItemMenuPrincipal(sSEPARADOR, 'Separador4', nil);

  CriarItemMenuPrincipal('Visualizar DataSet', 'VisualizarDataSet', FoFuncoes.VisualizarDataSet);
  CriarItemMenuPrincipal('Visualizar DataSet Manual', 'VisualizarDataSetManual', FoFuncoes.VisualizarDataSetManual);
  CriarItemMenuPrincipal('Ler TStringList', 'LerTStringList', FoFuncoes.LerStringList);
  CriarItemMenuPrincipal('Consultar no Ransack', 'ConsultarRansack', FoFuncoes.ConsultarRansack);
  CriarItemMenuPrincipal('Testar SpSelect', 'TestarSpSelect', FoFuncoes.TestarSpSelect);
  CriarItemMenuPrincipal('Não Formatar Código', 'NaoFormatarCodigo', FoFuncoes.NaoFormatarCodigo);

  CriarItemMenuPrincipal(sSEPARADOR, 'Separador5', nil);

  CriarItemMenuPrincipal('Selecionar Base de Dados', 'SelecionarBaseDados', nil);
  CriarItemMenuPrincipal('175 - Desenvolvimento', 'Base175', FoFuncoes.SelecionarBase175, 'SelecionarBaseDados');
  CriarItemMenuPrincipal('152 - Testes', 'Base152', FoFuncoes.SelecionarBase152, 'SelecionarBaseDados');
  CriarItemMenuPrincipal('202 - System Team', 'Base202', FoFuncoes.SelecionarBase202, 'SelecionarBaseDados');

  //CriarItemMenuPrincipal(sSEPARADOR, 'Separador6', nil);

  CriarItemMenuPrincipal('Documentação', 'Documentacao', nil);
  CriarItemMenuPrincipal('Consultar Documentação Delphi', 'ConsultarDocDelphi', FoFuncoes.ConsultarDocDelphi, 'Documentacao');
  CriarItemMenuPrincipal('Consultar Documentação SP4', 'ConsultarDocSP4', FoFuncoes.ConsultarDocSP4, 'Documentacao');
  CriarItemMenuPrincipal('Consultar Colabore', 'ConsultarColabore', FoFuncoes.ConsultarColabore, 'Documentacao');
  CriarItemMenuPrincipal('Configurações...', 'Configuracoes', AbrirConfiguracoes);

  CriarItemMenuPrincipal(sSEPARADOR, 'Separador7', nil);

  CriarItemMenuPrincipal(sNOME_PG, 'SelecionarProjetoPG', SelecionarSistemaPG);
  CriarItemMenuPrincipal(sNOME_SG, 'SelecionarProjetoSG', SelecionarSistemaSG);
  CriarItemMenuPrincipal(sNOME_MP, 'SelecionarProjetoMP', SelecionarSistemaMP);
end;

procedure TWizard.AbrirConfiguracoes(Sender: TObject);
begin
  FoFuncoes.AbrirConfiguracoes;
  CarregarAtalhos;
  AtribuirAtalhos(Sender);
end;

procedure TWizard.CarregarAtalhos;
var
  oArquivoINI: TIniFile;
begin
  oArquivoINI := TIniFile.Create(sPATH_ARQUIVO_INI);
  try
    oArquivoINI.ReadSectionValues(sSECAO_ATALHOS, FslAtalhos);
  finally
    FreeAndNil(oArquivoINI);
  end;
end;

procedure TWizard.CriarPastaOutput;
begin
  if not DirectoryExists('C:\PluginDB1\Output') then
    ForceDirectories('C:\PluginDB1\Output');
end;

function TWizard.GetState: TWizardState;
begin
  result := [];
end;

procedure TWizard.AfterSave;
begin

end;

procedure TWizard.BeforeSave;
begin

end;

procedure TWizard.Destroyed;
begin

end;

procedure TWizard.Execute;
begin

end;

procedure TWizard.Modified;
begin

end;

procedure TWizard.CriarMenuMVP;
var
  oMainMenu: TMainMenu;
begin
  oMainMenu := (BorlandIDEServices as INTAServices).MainMenu;
  FoMenuMVP := TMenuItem.Create(oMainMenu); //PC_OK
  FoMenuMVP.Name := 'miMVP';
  FoMenuMVP.Caption := sMENU_MVP;
  FoMenuMVP.OnClick := ProcessarArquivosMVP;
  oMainMenu.Items.Add(FoMenuMVP);
end;

procedure TWizard.AdicionarAcoesMVP;
begin
  CriarItemMenuMVP('Abrir API', 'MVPAbrirAPI');
  CriarItemMenuMVP('Abrir Impl', 'MVPAbrirImpl');
  CriarItemMenuMVP(sSEPARADOR, 'Separador8');

  CriarItemMenuMVP('Model API', 'MVPModelAPI', Ord(taModelAPI));
  CriarItemMenuMVP('Presenter API', 'MVPPresenterAPI', Ord(taPresenterAPI));
  CriarItemMenuMVP('PresenteView API', 'MVPViewAPI', Ord(taViewAPI));
  CriarItemMenuMVP(sSEPARADOR, 'Separador9');

  CriarItemMenuMVP('Model', 'MVPModel', Ord(taModel));
  CriarItemMenuMVP('Presenter', 'MVPPresenter', Ord(taPresenter));
  CriarItemMenuMVP('ViewFrame', 'MVPViewFrame', Ord(taViewFrame));
  CriarItemMenuMVP('ViewPanel', 'MVPViewPanel', Ord(taViewPanel));
  CriarItemMenuMVP('ViewForm', 'MVPViewForm', Ord(taViewForm));
  CriarItemMenuMVP('View', 'MVPView', Ord(taView));
  CriarItemMenuMVP(sSEPARADOR, 'Separador10');

  CriarItemMenuMVP('Builder', 'MVPBuilder', Ord(taBuilder));
  CriarItemMenuMVP('ParamsBuild', 'MVPParamsBuild', Ord(taParamsBuild));
  CriarItemMenuMVP('ParamsBuild API', 'MVPParamsBuildAPI', Ord(taParamsBuildAPI));
end;

procedure TWizard.CriarItemMenuMVP(const psCaption, psIdentificador: string; const pnTag: integer = 0);
var
  oNTAS: INTAServices;
  oAction: TAction;
  oMenuItem: TMenuItem;
begin
  oAction := nil;
  oNTAS := (BorlandIDEServices as INTAServices);
  oMenuItem := TMenuItem.Create(oNTAS.MainMenu); //PC_OK

  if psCaption <> sSEPARADOR then
  begin
    oAction := TAction.Create(oNTAS.ActionList); //PC_OK
    oAction.ActionList := oNTAS.ActionList;
    oAction.Name := psIdentificador;
    oAction.Caption := psCaption;
    oAction.OnExecute := AbrirArquivoMVP;
    oAction.Category := 'PluginDB1';
    oAction.Tag := pnTag;

    FActions.Add(oAction);
  end;

  oMenuItem.Caption := psCaption;
  oMenuItem.Action := oAction;
  oMenuItem.Name := 'im' + psIdentificador;
  FoMenuMVP.Add(oMenuItem);
end;

procedure TWizard.AbrirArquivoMVP(Sender: TObject);
begin
  FoFuncoes.AbrirArquivoMVP(TMenuItem(Sender).Tag);
end;

procedure TWizard.ProcessarArquivosMVP(Sender: TObject);
begin
  FoFuncoes.CriarExpansorArquivoMVP;
end;

function TWizard.PegarAtalho(const psIdentificador: string): TShortCut;
begin
  result := TextToShortCut(FslAtalhos.Values[psIdentificador]);
end;

function TWizard.PegarNomeMenuPrincipal: string;
var
  oArquivoINI: TIniFile;
begin
  oArquivoINI := TIniFile.Create(sPATH_ARQUIVO_INI);
  try
    result := oArquivoINI.ReadString('Parametros', 'NomeMenu', sMENU_DB1);
  finally
    FreeAndNil(oArquivoINI);
  end;
end;

function TWizard.ProcurarMenu(const psNomeMenu: string): TMenuItem;
var
  nContador: byte;
begin
  result := nil;
  for nContador := 0 to Pred(FoMenuPrincipal.Count) do
  begin
    if FoMenuPrincipal.Items[nContador].Name = ('im' + psNomeMenu) then
    begin
      result := FoMenuPrincipal[nContador];
      Break;
    end;
  end;
end;

procedure TWizard.SelecionarSistemaMP(Sender: TObject);
begin
  FoFuncoes.TipoSistema := tsMP;
  MarcarMenu;
end;

initialization
  FoFuncoes := TFuncoes.Create; //PC_OK
  FActions := TObjectList.Create(True); //PC_OK
  FslAtalhos := TStringList.Create; //PC_OK

finalization
  FreeAndNil(FoFuncoes);
  FreeAndNil(FActions);
  FreeAndNil(FslAtalhos);

  if nIDWizard > 0 then
    (BorlandIDEServices as IOTAWizardServices).RemoveWizard(nIDWizard);

  //jcf:format=on
end.

