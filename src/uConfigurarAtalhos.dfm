object fConfigurarAtalhos: TfConfigurarAtalhos
  Left = 195
  Top = 116
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Personalizar Atalhos'
  ClientHeight = 381
  ClientWidth = 280
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
  object gbAtalhos: TGroupBox
    Left = 3
    Top = 2
    Width = 275
    Height = 345
    Caption = ' Atalhos '
    TabOrder = 0
    object lbAbrirServidor: TLabel
      Left = 30
      Top = 20
      Width = 70
      Height = 13
      Caption = 'Abrir Servidor:'
    end
    object lbAbrirAplicacao: TLabel
      Left = 25
      Top = 40
      Width = 75
      Height = 13
      Caption = 'Abrir Aplicação:'
    end
    object lbAbrirDiretorioBin: TLabel
      Left = 12
      Top = 60
      Width = 88
      Height = 13
      Caption = 'Abrir Diretório Bin:'
    end
    object lbAbrirSpCfg: TLabel
      Left = 28
      Top = 80
      Width = 72
      Height = 13
      Caption = 'Abrir spCfg.ini:'
    end
    object lbAbrirItemRTC: TLabel
      Left = 10
      Top = 100
      Width = 90
      Height = 13
      Caption = 'Abrir Item no RTC:'
    end
    object lbAbrirVisualizaDTS: TLabel
      Left = 8
      Top = 140
      Width = 92
      Height = 13
      Caption = 'Abrir Visualiza DTS:'
    end
    object lbSpMonitor: TLabel
      Left = 23
      Top = 160
      Width = 77
      Height = 13
      Caption = 'Abrir spMonitor:'
    end
    object lbSpMonitor3: TLabel
      Left = 17
      Top = 180
      Width = 83
      Height = 13
      Caption = 'Abrir spMonitor3:'
    end
    object lbSelectSQL: TLabel
      Left = 22
      Top = 200
      Width = 78
      Height = 13
      Caption = 'Abrir SelectSQL:'
    end
    object lbSqlDbx: TLabel
      Left = 37
      Top = 220
      Width = 63
      Height = 13
      Caption = 'Abrir SqlDbx:'
    end
    object lbWinSpy: TLabel
      Left = 34
      Top = 240
      Width = 66
      Height = 13
      Caption = 'Abrir WinSpy:'
    end
    object bvl1: TBevel
      Left = 10
      Top = 124
      Width = 250
      Height = 11
      Shape = bsTopLine
    end
    object Bevel1: TBevel
      Left = 10
      Top = 264
      Width = 250
      Height = 11
      Shape = bsTopLine
    end
    object lbVisualizarDataSet: TLabel
      Left = 10
      Top = 280
      Width = 90
      Height = 13
      Caption = 'Visualizar DataSet:'
    end
    object lbAvaliarDataSet: TLabel
      Left = 21
      Top = 300
      Width = 79
      Height = 13
      Caption = 'Avaliar DataSet:'
    end
    object lbLerTStringList: TLabel
      Left = 28
      Top = 320
      Width = 72
      Height = 13
      Caption = 'Ler TStringList:'
    end
    object hkServidor: THotKey
      Left = 105
      Top = 18
      Width = 110
      Height = 19
      HotKey = 0
      InvalidKeys = [hcNone, hcShift]
      Modifiers = []
      TabOrder = 0
    end
    object hkAplicacao: THotKey
      Left = 105
      Top = 38
      Width = 110
      Height = 19
      HotKey = 0
      InvalidKeys = [hcNone, hcShift]
      Modifiers = []
      TabOrder = 1
    end
    object hkDiretorioBin: THotKey
      Left = 105
      Top = 58
      Width = 110
      Height = 19
      HotKey = 0
      InvalidKeys = [hcNone, hcShift]
      Modifiers = []
      TabOrder = 2
    end
    object hkSpCfg: THotKey
      Left = 105
      Top = 78
      Width = 110
      Height = 19
      HotKey = 0
      InvalidKeys = [hcNone, hcShift]
      Modifiers = []
      TabOrder = 3
    end
    object hkItemRTC: THotKey
      Left = 105
      Top = 98
      Width = 110
      Height = 19
      HotKey = 0
      InvalidKeys = [hcNone, hcShift]
      Modifiers = []
      TabOrder = 4
    end
    object hkVisualizaDTS: THotKey
      Left = 105
      Top = 138
      Width = 110
      Height = 19
      HotKey = 0
      InvalidKeys = [hcNone, hcShift]
      Modifiers = []
      TabOrder = 5
    end
    object hkSpMonitor: THotKey
      Left = 105
      Top = 158
      Width = 110
      Height = 19
      HotKey = 0
      InvalidKeys = [hcNone, hcShift]
      Modifiers = []
      TabOrder = 6
    end
    object hkSpMonitor3: THotKey
      Left = 105
      Top = 178
      Width = 110
      Height = 19
      HotKey = 0
      InvalidKeys = [hcNone, hcShift]
      Modifiers = []
      TabOrder = 7
    end
    object hkSelectSQL: THotKey
      Left = 105
      Top = 198
      Width = 110
      Height = 19
      HotKey = 0
      InvalidKeys = [hcNone, hcShift]
      Modifiers = []
      TabOrder = 8
    end
    object hkSqlDbx: THotKey
      Left = 105
      Top = 218
      Width = 110
      Height = 19
      HotKey = 0
      InvalidKeys = [hcNone, hcShift]
      Modifiers = []
      TabOrder = 9
    end
    object hkWinSpy: THotKey
      Left = 105
      Top = 238
      Width = 110
      Height = 19
      HotKey = 0
      InvalidKeys = [hcNone, hcShift]
      Modifiers = []
      TabOrder = 10
    end
    object hkVisualizarDataSet: THotKey
      Left = 105
      Top = 278
      Width = 110
      Height = 19
      HotKey = 0
      InvalidKeys = [hcNone, hcShift]
      Modifiers = []
      TabOrder = 11
    end
    object hkAvaliarDataSet: THotKey
      Left = 105
      Top = 298
      Width = 110
      Height = 19
      HotKey = 0
      InvalidKeys = [hcNone, hcShift]
      Modifiers = []
      TabOrder = 12
    end
    object hkLerTStringList: THotKey
      Left = 105
      Top = 318
      Width = 110
      Height = 19
      HotKey = 0
      InvalidKeys = [hcNone, hcShift]
      Modifiers = []
      TabOrder = 13
    end
  end
  object btnOK: TBitBtn
    Left = 90
    Top = 351
    Width = 100
    Height = 25
    Caption = 'OK'
    TabOrder = 1
    OnClick = btnOKClick
  end
end
