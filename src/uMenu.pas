unit uMenu;

//jcf:format=off

interface

uses
  ToolsAPI, Classes, Menus, ActnList, Dialogs, Contnrs, ExtCtrls;

type
  TWizard = class(TInterfacedObject, IOTAWizard, IOTANotifier)
  private
    FoTimerAtalhos: TTimer;
    FoMenuDB1: TMenuItem;
    FoMenuMVP: TMenuItem;

    procedure AbrirArquivoMVP(Sender: TObject);
    procedure CriarItemMenuDB1(const psCaption, psIdentificador: string; poEvento: TNotifyEvent);
    procedure CriarItemMenuMVP(const psCaption, psIdentificador: string; const pnTag: integer = 0);
    procedure CriarMenuDB1;
    procedure CriarMenuMVP;
    procedure CriarTemporizadorAtalhos;
    procedure CriarPastaOutput;
    procedure AdicionarAcoesDB1;
    procedure AdicionarAcoesMVP;
    procedure CarregarAtalhos;
    procedure AtribuirAtalhos(Sender: TObject);
    procedure MarcarMenu;
    procedure SelecionarSistemaPG(Sender: TObject);
    procedure SelecionarSistemaSG(Sender: TObject);
    procedure SelecionarSistemaPJ(Sender: TObject);
    procedure ConfigurarAtalhos(Sender: TObject);
    procedure ProcessarArquivosMVP(Sender: TObject);
    function PegarAtalho(const psIdentificador: string): TShortCut;
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
  SysUtils, Forms, Windows, Graphics, IniFiles, FileCtrl, uFuncoes, uConstantes, ComCtrls;

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

procedure TWizard.CriarItemMenuDB1(const psCaption, psIdentificador: string; poEvento: TNotifyEvent);
var
  oNTAS: INTAServices;
  oAction: TAction;
  oBitmap: TBitmap;
  oMenuItem: TMenuItem;
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
  FoMenuDB1.Add(oMenuItem);
end;

constructor TWizard.Create;
begin
  CriarMenuMVP;
  AdicionarAcoesMVP;

  CriarMenuDB1;
  AdicionarAcoesDB1;

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

procedure TWizard.CriarMenuDB1;
var
  oMainMenu: TMainMenu;
begin
  oMainMenu := (BorlandIDEServices as INTAServices).MainMenu;
  FoMenuDB1 := TMenuItem.Create(oMainMenu); //PC_OK
  FoMenuDB1.Name := 'miDB1';
  FoMenuDB1.Caption := sMENU_DB1;
  FoMenuDB1.OnClick := AtribuirAtalhos;
  oMainMenu.Items.Add(FoMenuDB1);
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

procedure TWizard.SelecionarSistemaPJ(Sender: TObject);
begin
  FoFuncoes.TipoSistema := tsPJ;
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
    tsPJ: sNomeMenu := sNOME_PJ;
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

procedure TWizard.AdicionarAcoesDB1;
begin
  CriarItemMenuDB1('Abrir Servidor', 'AbrirServidor', FoFuncoes.AbrirServidor);
  CriarItemMenuDB1('Abrir Aplicação', 'AbrirAplicacao', FoFuncoes.AbrirAplicacao);
  CriarItemMenuDB1('Abrir Diretório Bin', 'AbrirDiretorioBin', FoFuncoes.AbrirDiretorioBin);
  CriarItemMenuDB1('Abrir spCfg.ini', 'AbrirSpCfg', FoFuncoes.AbrirSPCfg);
  CriarItemMenuDB1('Abrir Item no RTC', 'AbrirItemRTC', FoFuncoes.AbrirItemRTC);
  CriarItemMenuDB1('Excluir Cache', 'ExcluirCache', FoFuncoes.ExcluirCache);
  CriarItemMenuDB1('Finalizar Processos', 'FinalizarProcessos', FoFuncoes.FinalizarProcessos);

  CriarItemMenuDB1(sSEPARADOR, 'Separador1', nil);

  CriarItemMenuDB1('Compilar Projetos Cliente', 'CompilarCliente', FoFuncoes.CompilarProjetosClientes);
  CriarItemMenuDB1('Compilar Projetos Servidor', 'CompilarServidor', FoFuncoes.CompilarProjetosServidores);
  CriarItemMenuDB1('Compilar Todos', 'CompilarTodos', FoFuncoes.CompilarTodosProjetos);

  CriarItemMenuDB1(sSEPARADOR, 'Separador2', nil);

  CriarItemMenuDB1('Abrir VisualizaDTS', 'AbrirVisualizaDTS', FoFuncoes.AbrirVisualizaDTS);
  CriarItemMenuDB1('Abrir spMonitor', 'AbrirSpMonitor', FoFuncoes.AbrirSPMonitor);
  CriarItemMenuDB1('Abrir spMonitor3', 'AbrirSpMonitor3', FoFuncoes.AbrirSPMonitor3);
  CriarItemMenuDB1('Abrir SelectSQL', 'AbrirSelectSQL', FoFuncoes.AbrirSelectSQL);
  CriarItemMenuDB1('Abrir WinSpy', 'AbrirWinSpy', FoFuncoes.AbrirWinSpy);

  CriarItemMenuDB1(sSEPARADOR, 'Separador3', nil);

  CriarItemMenuDB1('Consultar no Ransack', 'ConsultarRansack', FoFuncoes.ConsultarRansack);
  CriarItemMenuDB1('Consultar Documentação Delphi', 'ConsultarDocDelphi', FoFuncoes.ConsultarDocDelphi);
  CriarItemMenuDB1('Consultar Documentação SP4', 'ConsultarDocSP4', FoFuncoes.ConsultarDocSP4);
  CriarItemMenuDB1('Consultar Colabore', 'ConsultarColabore', FoFuncoes.ConsultarColabore);

  CriarItemMenuDB1(sSEPARADOR, 'Separador4', nil);

  CriarItemMenuDB1('Visualizar DataSet', 'VisualizarDataSet', FoFuncoes.VisualizarDataSet);
  CriarItemMenuDB1('Visualizar DataSet Manual', 'VisualizarDataSetManual', FoFuncoes.VisualizarDataSetManual);
  CriarItemMenuDB1('Ler TStringList', 'LerTStringList', FoFuncoes.LerStringList);
  CriarItemMenuDB1('Não Formatar Código', 'NaoFormatarCodigo', FoFuncoes.NaoFormatarCodigo);

  CriarItemMenuDB1(sSEPARADOR, 'Separador5', nil);

  CriarItemMenuDB1('Usar base 175', 'UsarBase175', FoFuncoes.UsarBase175);
  CriarItemMenuDB1('Usar base 152', 'UsarBase152', FoFuncoes.UsarBase152);
  CriarItemMenuDB1('Usar base 202', 'UsarBase202', FoFuncoes.UsarBase202);

  CriarItemMenuDB1(sSEPARADOR, 'Separador6', nil);

  CriarItemMenuDB1('Configurar Atalhos', 'ConfigurarAtalhos', ConfigurarAtalhos);
  
  CriarItemMenuDB1(sSEPARADOR, 'Separador7', nil);

  CriarItemMenuDB1(sNOME_PG, 'SelecionarSistemaPG', SelecionarSistemaPG);
  CriarItemMenuDB1(sNOME_SG, 'SelecionarSistemaSG', SelecionarSistemaSG);
  CriarItemMenuDB1(sNOME_PJ, 'SelecionarSistemaPJ', SelecionarSistemaPJ);
end;

procedure TWizard.ConfigurarAtalhos(Sender: TObject);
begin
  FoFuncoes.ConfigurarAtalhos;
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

