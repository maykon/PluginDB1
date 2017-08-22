unit uConstantes;

//jcf:format=off

interface

type
  TTipoSistema = (tsTodos, tsPG, tsSG, tsMP);
  TTipoSistemaDesc = (Nenhum, PG5, SG5, SIG);

  TenTipoArquivoMVP = (taNaoMVP, taModelAPI, taPresenterAPI, taViewAPI, taModel,
    taPresenter, taViewFrame, taViewPanel, taViewForm, taView, taBuilder,
    taParamsBuild, taParamsBuildAPI);

const
  // Servidor
  sNOME_SERVIDOR_PG       = 'pg5Servidor.exe';
  sNOME_SERVIDOR_SG       = 'sg5Servidor.exe';
  sNOME_SERVIDOR_MP       = 'sigServidor.exe';

  // Aplicação
  sNOME_APLICACAO_PG      = 'sajpg5app.exe';
  sNOME_APLICACAO_SG      = 'sajsg5app.exe';
  sNOME_APLICACAO_MP      = 'sigapp.exe';

  // ADM
  sNOME_ADM_PG            = 'pg5ADMApp.exe';
  sNOME_ADM_SG            = 'sg5ADMApp.exe';
  sNOME_ADM_MP            = 'sigADMApp.exe';

  sNOME_ARQUIVO_CONFIG    = 'spCfg.ini';
  sPATH_VISUALIZA_DTS     = 'C:\PluginDB1\Tools\VisualizaDTS\VisualizaDTS.exe';
  sPATH_SP_MONITOR        = 'C:\PluginDB1\Tools\spMonitor\spMonitor.exe';
  sPATH_SP_MONITOR3       = 'C:\PluginDB1\Tools\spMonitor3\spMonitor3.exe';
  sPATH_SELECT_SQL        = 'C:\PluginDB1\Tools\SelectSQL\SelectSQL.exe';
  sPATH_WINSPY            = 'C:\PluginDB1\Tools\WinSpy\WinSpy.exe';
  sPATH_ARQUIVO_DADOS     = 'C:\PluginDB1\Visualizador\Dados.xml';
  sPATH_PROP_DATASET      = 'C:\PluginDB1\Visualizador\Propriedades.txt';
  sPATH_VISUALIZADOR      = 'C:\PluginDB1\Visualizador\Visualizador.exe';
  sPATH_VISUALIZADOR_AUTO = 'C:\PluginDB1\Visualizador\Visualizador.exe /auto';
  sPATH_TESTE_SPSELECT    = 'C:\PluginDB1\TesteSpSelect\TesteSpSelect.exe';
  sPATH_PARAMS_SELECT     = 'C:\PluginDB1\TesteSpSelect\Parametros.xml';
  sPATH_ARQUIVO_INI       = 'C:\PluginDB1\Params.ini';
  sPATH_IMAGENS           = 'C:\PluginDB1\Imagens\';
  sNOME_BASE_MENU         = 'Base';
  sTIPO_BANCO_DB2         = 'DB2';
  sTIPO_BANCO_ORACLE      = 'ORACLE';
  sTIPO_BANCO_SQLSERVER   = 'SQLSERVER';
  
  nNUMERO_TENTATIVAS_LEITURA = 20;
  nTENTATIVAS_PROCESSAMENTO = 10;
  nBORDA_DBGRID = 12;
  nTAMANHO_MAXIMO_ITEM_RTC = 8;
  sMENU_DB1 = 'DB&1';
  sMENU_MVP = '&MVP';
  sSEPARADOR = '-';
  sINDEX_ASC = '_ASC';
  sINDEX_DESC = '_DESC';
  sNOME_PG = 'PG';
  sNOME_SG = 'SG';
  sNOME_MP = 'MP';
  sSECAO_ATALHOS = 'Atalhos';
  sSECAO_PARAMETROS = 'Parametros';
  sMENSAGEM_ARQUIVO_NAO_ENCONTRADO = 'O arquivo %s não foi encontrado.';
  sCOMANDO_RMDIR = 'cmd.exe /c rmdir /s /q ';
  sCOMANDO_FINALIZAR_PROCESSOS = 'tskill %s';

  sURL_ITEM_RTC =
    'https://clm.unj.softplan.com.br/ccm/web/projects/Tribunais#action=com.ibm.team.workitem.viewWorkItem&id=%s';

  sURL_SALT_RTC =
    'https://clm.unj.softplan.com.br/ccm/web/projects/Tribunais#action=com.ibm.team.workitem.search&q=%s';

  sURL_DOCUMENTACAO_DELPHI =
    'http://docwiki.embarcadero.com/RADStudio/Berlin/e/index.php?search=%s';

  sURL_DOCUMENTACAO_SP4 =
    'http://jenkins/view/Suporte%%20Interno/view/3%%20-%%20Noturno/job/SIU_DEVELOPMENT_NOTURNO_METRICAS/lastSuccessfulBuild/artifact/Artifacts/NOTURNO/Metrics/pasdoc/_tipue_results.html?q=%s';

  sURL_COLABORE =
    'https://colabore.softplan.com.br/dosearchsite.action?queryString=%s';

  sCOMANDO_RANSACK =
    '"C:\Program Files\Mythicsoft\Agent Ransack\AgentRansack.exe" -c "\"%s"\" -d "%s" -r -s';

  sCOMANDO_TFS_CHECKOUT =
    '"C:\Program Files (x86)\Microsoft Visual Studio 10.0\Common7\IDE\tf.exe" checkout "%s"';

implementation

//jcf:format=on

end.
                                                                              
