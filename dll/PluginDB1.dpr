library PluginDB1;

uses
  ShareMem,
  ToolsAPI,
  SysUtils,
  Classes,
  uMenu in '..\src\uMenu.pas',
  uAguarde in '..\src\uAguarde.pas' {fAguarde},
  uStringList in '..\src\uStringList.pas' {fStringList},
  uConfiguracoes in '..\src\uConfiguracoes.pas' {fConfiguracoes},
  uFuncoes in '..\src\uFuncoes.pas',
  uConstantes in '..\src\uConstantes.pas',
  uToolsAPIUtils in '..\src\uToolsAPIUtils.pas',
  uExpansorArquivoMVP in '..\src\uExpansorArquivoMVP.pas',
  uCompilacao in '..\src\uCompilacao.pas' {fCompilacao};

{$R *.RES}

exports
  InitWizard name WizardEntryPoint;

begin
end.
