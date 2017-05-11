object fConfiguracoes: TfConfiguracoes
  Left = 195
  Top = 125
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Configurações'
  ClientHeight = 383
  ClientWidth = 286
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBoxAtalhos: TGroupBox
    Left = 3
    Top = 2
    Width = 280
    Height = 297
    Caption = ' Atalhos '
    TabOrder = 0
    object lbAbrirServidor: TLabel
      Left = 64
      Top = 20
      Width = 70
      Height = 13
      Caption = 'Abrir Servidor:'
    end
    object lbAbrirAplicacao: TLabel
      Left = 59
      Top = 40
      Width = 75
      Height = 13
      Caption = 'Abrir Aplicação:'
    end
    object lbAbrirDiretorioBin: TLabel
      Left = 46
      Top = 60
      Width = 88
      Height = 13
      Caption = 'Abrir Diretório Bin:'
    end
    object lbAbrirSpCfg: TLabel
      Left = 62
      Top = 80
      Width = 72
      Height = 13
      Caption = 'Abrir spCfg.ini:'
    end
    object lbAbrirItemRTC: TLabel
      Left = 44
      Top = 100
      Width = 90
      Height = 13
      Caption = 'Abrir Item no RTC:'
    end
    object lbAbrirVisualizaDTS: TLabel
      Left = 44
      Top = 133
      Width = 92
      Height = 13
      Caption = 'Abrir Visualiza DTS:'
    end
    object lbSpMonitor: TLabel
      Left = 59
      Top = 153
      Width = 77
      Height = 13
      Caption = 'Abrir spMonitor:'
    end
    object lbSpMonitor3: TLabel
      Left = 53
      Top = 173
      Width = 83
      Height = 13
      Caption = 'Abrir spMonitor3:'
    end
    object Bevel2: TBevel
      Left = 10
      Top = 123
      Width = 250
      Height = 11
      Shape = bsTopLine
    end
    object Bevel1: TBevel
      Left = 13
      Top = 217
      Width = 250
      Height = 11
      Shape = bsTopLine
    end
    object lbVisualizarDataSet: TLabel
      Left = 46
      Top = 226
      Width = 90
      Height = 13
      Caption = 'Visualizar DataSet:'
    end
    object lblVisualizarDataSetManual: TLabel
      Left = 9
      Top = 246
      Width = 127
      Height = 13
      Caption = 'Visualizar DataSet Manual:'
    end
    object lbLerTStringList: TLabel
      Left = 64
      Top = 266
      Width = 72
      Height = 13
      Caption = 'Ler TStringList:'
    end
    object lbConsultarNoRansack: TLabel
      Left = 28
      Top = 193
      Width = 108
      Height = 13
      Caption = 'Consultar no Ransack:'
    end
    object hkServidor: THotKey
      Left = 138
      Top = 18
      Width = 110
      Height = 19
      HotKey = 0
      InvalidKeys = [hcNone, hcShift]
      Modifiers = []
      TabOrder = 0
    end
    object hkAplicacao: THotKey
      Left = 138
      Top = 38
      Width = 110
      Height = 19
      HotKey = 0
      InvalidKeys = [hcNone, hcShift]
      Modifiers = []
      TabOrder = 1
    end
    object hkDiretorioBin: THotKey
      Left = 138
      Top = 58
      Width = 110
      Height = 19
      HotKey = 0
      InvalidKeys = [hcNone, hcShift]
      Modifiers = []
      TabOrder = 2
    end
    object hkSpCfg: THotKey
      Left = 138
      Top = 78
      Width = 110
      Height = 19
      HotKey = 0
      InvalidKeys = [hcNone, hcShift]
      Modifiers = []
      TabOrder = 3
    end
    object hkItemRTC: THotKey
      Left = 138
      Top = 98
      Width = 110
      Height = 19
      HotKey = 0
      InvalidKeys = [hcNone, hcShift]
      Modifiers = []
      TabOrder = 4
    end
    object hkVisualizaDTS: THotKey
      Left = 140
      Top = 131
      Width = 110
      Height = 19
      HotKey = 0
      InvalidKeys = [hcNone, hcShift]
      Modifiers = []
      TabOrder = 5
    end
    object hkSpMonitor: THotKey
      Left = 140
      Top = 151
      Width = 110
      Height = 19
      HotKey = 0
      InvalidKeys = [hcNone, hcShift]
      Modifiers = []
      TabOrder = 6
    end
    object hkSpMonitor3: THotKey
      Left = 140
      Top = 171
      Width = 110
      Height = 19
      HotKey = 0
      InvalidKeys = [hcNone, hcShift]
      Modifiers = []
      TabOrder = 7
    end
    object hkVisualizarDataSet: THotKey
      Left = 140
      Top = 226
      Width = 110
      Height = 19
      HotKey = 0
      InvalidKeys = [hcNone, hcShift]
      Modifiers = []
      TabOrder = 8
    end
    object hkVisualizarDataSetManual: THotKey
      Left = 140
      Top = 244
      Width = 110
      Height = 19
      HotKey = 0
      InvalidKeys = [hcNone, hcShift]
      Modifiers = []
      TabOrder = 9
    end
    object hkLerTStringList: THotKey
      Left = 140
      Top = 264
      Width = 110
      Height = 19
      HotKey = 0
      InvalidKeys = [hcNone, hcShift]
      Modifiers = []
      TabOrder = 10
    end
    object hkConsultarNoRansack: THotKey
      Left = 140
      Top = 191
      Width = 110
      Height = 19
      HotKey = 0
      InvalidKeys = [hcNone, hcShift]
      Modifiers = []
      TabOrder = 11
    end
  end
  object btnOK: TBitBtn
    Left = 93
    Top = 354
    Width = 100
    Height = 25
    Caption = 'OK'
    TabOrder = 2
    OnClick = btnOKClick
  end
  object GroupBoxNomeMenu: TGroupBox
    Left = 3
    Top = 304
    Width = 280
    Height = 46
    Caption = 'Configuração '
    TabOrder = 1
    object LabelNomeMenu: TLabel
      Left = 61
      Top = 22
      Width = 75
      Height = 13
      Caption = 'Nome do Menu:'
    end
    object EditNomeMenu: TEdit
      Left = 140
      Top = 18
      Width = 110
      Height = 21
      TabOrder = 0
    end
  end
end
