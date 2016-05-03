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

    function CriarMenu(const psCaption, psIdentificador: string; poEvento: TNotifyEvent): TMenuItem;
    procedure CriarMenuDB1;
    procedure CriarTemporizadorAtalhos;
    procedure CriarPastaOutput;
    procedure AdicionarAcoes;
    procedure CarregarAtalhos;
    procedure AtribuirAtalhos(Sender: TObject);
    procedure MarcarMenu;
    procedure SelecionarSistemaPG(Sender: TObject);
    procedure SelecionarSistemaSG(Sender: TObject);
    procedure SelecionarSistemaPJ(Sender: TObject);
    procedure ConfigurarAtalhos(Sender: TObject);
    procedure ExcluirArquivosAntigos;
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
  SysUtils, Forms, Windows, IniFiles, FileCtrl, uFuncoes, uConstantes;

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
  nIDWizard := (BorlandIDEServices as IOTAWizardServices).AddWizard(
    InitialiseWizard(BorlandIDEServices));
    
  RegisterPackageWizard(TWizard.Create); //PC_OK
end;

{ TWizard }

function TWizard.CriarMenu(const psCaption, psIdentificador: string; poEvento: TNotifyEvent): TMenuItem;
var
  oNTAS: INTAServices;
  oAction: TAction;
begin
  oAction := nil;
  oNTAS := (BorlandIDEServices as INTAServices);
  result := TMenuItem.Create(oNTAS.MainMenu); //PC_OK

  if Assigned(poEvento) then
  begin
    oAction := TAction.Create(oNTAS.ActionList); //PC_OK
    oAction.ActionList := oNTAS.ActionList;
    oAction.Name := psIdentificador;
    oAction.Caption := psCaption;
    oAction.OnExecute := poEvento;
    oAction.Category := 'PluginDB1';
    oAction.ShortCut := TextToShortCut(FslAtalhos.Values[psIdentificador]);

    FActions.Add(oAction);
  end;

  if psCaption = sSEPARADOR then
    result.Caption := sSEPARADOR
  else
    result.Caption := psCaption;

  result.Action := oAction;
  result.Name := 'im' + psIdentificador;
  FoMenuDB1.Add(result);
end;

constructor TWizard.Create;
begin
  CriarMenuDB1;
  CarregarAtalhos;
  AdicionarAcoes;
  MarcarMenu;
  CriarTemporizadorAtalhos;
  CriarPastaOutput;
  ExcluirArquivosAntigos;
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
  for nCont := 0 to FActions.Count - 1 do
  begin
    oAction := FActions[nCont] as TAction;
    oAction.ShortCut := TextToShortCut(FslAtalhos.Values[oAction.Name]);
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

  for nCont := 0 to FActions.Count - 1 do
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

procedure TWizard.AdicionarAcoes;
begin
  CriarMenu('Abrir Servidor', 'AbrirServidor', FoFuncoes.AbrirServidor);
  CriarMenu('Abrir Aplicação', 'AbrirAplicacao', FoFuncoes.AbrirAplicacao);
  CriarMenu('Abrir Diretório Bin', 'AbrirDiretorioBin', FoFuncoes.AbrirDiretorioBin);
  CriarMenu('Abrir spCfg.ini', 'AbrirSpCfg', FoFuncoes.AbrirSPCfg);
  CriarMenu('Abrir Item no RTC', 'AbrirItemRTC', FoFuncoes.AbrirItemRTC);
  CriarMenu(sSEPARADOR, 'Separador1', nil);
  CriarMenu('Abrir VisualizaDTS', 'AbrirVisualizaDTS', FoFuncoes.AbrirVisualizaDTS);
  CriarMenu('Abrir spMonitor', 'AbrirSpMonitor', FoFuncoes.AbrirSPMonitor);
  CriarMenu('Abrir spMonitor3', 'AbrirSpMonitor3', FoFuncoes.AbrirSPMonitor3);
  CriarMenu('Abrir SelectSQL', 'AbrirSelectSQL', FoFuncoes.AbrirSelectSQL);
  CriarMenu('Abrir SqlDbx', 'AbrirSqlDbx', FoFuncoes.AbrirSqlDbx);
  CriarMenu('Abrir WinSpy', 'AbrirWinSpy', FoFuncoes.AbrirWinSpy);
  CriarMenu(sSEPARADOR, 'Separador2', nil);
  CriarMenu('Visualizar DataSet', 'VisualizarDataSet', FoFuncoes.VisualizarDataSet);
  CriarMenu('Visualizar DataSet Manual', 'VisualizarDataSetManual', FoFuncoes.VisualizarDataSetManual);
  CriarMenu('Ler TStringList', 'LerTStringList', FoFuncoes.LerStringList);
  CriarMenu(sSEPARADOR, 'Separador3', nil);
  CriarMenu('Configurar Atalhos', 'ConfigurarAtalhos', ConfigurarAtalhos);
  CriarMenu(sSEPARADOR, 'Separador4', nil);
  CriarMenu(sNOME_PG, 'SelecionarSistemaPG', SelecionarSistemaPG);
  CriarMenu(sNOME_SG, 'SelecionarSistemaSG', SelecionarSistemaSG);
  CriarMenu(sNOME_PJ, 'SelecionarSistemaPJ', SelecionarSistemaPJ);
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
  begin
    ForceDirectories('C:\PluginDB1\Output');
  end;
end;

procedure TWizard.ExcluirArquivosAntigos;
begin
  DeleteFile('C:\PluginDB1\Dados.xml');
  DeleteFile('C:\PluginDB1\Filtro.txt');
  DeleteFile('C:\PluginDB1\StringList.txt');
  DeleteFile('C:\PluginDB1\unins000.exe');
  DeleteFile('C:\PluginDB1\unins000.dat');
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
  begin
    (BorlandIDEServices as IOTAWizardServices).RemoveWizard(nIDWizard);
  end;

  //jcf:format=on
end.

