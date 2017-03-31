object fVisualizadorDataSet: TfVisualizadorDataSet
  Left = 189
  Top = 119
  Width = 1035
  Height = 608
  Caption = 'Visualizador de DataSet'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lbQuantidade: TLabel
    Left = 13
    Top = 547
    Width = 511
    Height = 19
    Anchors = [akLeft, akBottom]
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lbFiltro: TLabel
    Left = 24
    Top = 9
    Width = 28
    Height = 13
    Caption = 'Filtro:'
  end
  object lbIndices: TLabel
    Left = 14
    Top = 32
    Width = 38
    Height = 13
    Caption = 'Índices:'
  end
  object grdDados: TDBGrid
    Left = 12
    Top = 53
    Width = 820
    Height = 491
    Anchors = [akLeft, akTop, akRight, akBottom]
    DataSource = DataSource
    PopupMenu = PopupMenu
    TabOrder = 6
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
    OnTitleClick = grdDadosTitleClick
  end
  object edtFiltro: TEdit
    Left = 56
    Top = 5
    Width = 703
    Height = 21
    MaxLength = 200
    TabOrder = 0
    OnChange = edtFiltroChange
    OnKeyPress = edtFiltroKeyPress
  end
  object chkFiltroAtivado: TCheckBox
    Left = 766
    Top = 7
    Width = 66
    Height = 17
    Caption = 'Ativado'
    TabOrder = 1
    OnClick = chkFiltroAtivadoClick
  end
  object clCampos: TCheckListBox
    Left = 845
    Top = 51
    Width = 168
    Height = 472
    OnClickCheck = clCamposClickCheck
    Anchors = [akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 7
  end
  object btnTodos: TBitBtn
    Left = 845
    Top = 22
    Width = 80
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Todos'
    TabOrder = 4
    OnClick = btnTodosClick
  end
  object btnNenhum: TBitBtn
    Left = 933
    Top = 22
    Width = 80
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Nenhum'
    TabOrder = 5
    OnClick = btnNenhumClick
  end
  object chkAjustarTamanhoColunas: TCheckBox
    Left = 845
    Top = 527
    Width = 168
    Height = 17
    Anchors = [akRight, akBottom]
    Caption = 'Ajustar Tamanho das Colunas'
    TabOrder = 8
    OnClick = chkAjustarTamanhoColunasClick
  end
  object edtIndices: TEdit
    Left = 56
    Top = 28
    Width = 703
    Height = 21
    MaxLength = 200
    TabOrder = 2
    OnChange = edtIndicesChange
    OnKeyPress = edtIndicesKeyPress
  end
  object chkIndicesAtivado: TCheckBox
    Left = 766
    Top = 31
    Width = 66
    Height = 17
    Caption = 'Ativado'
    TabOrder = 3
    OnClick = chkIndicesAtivadoClick
  end
  object DataSource: TDataSource
    AutoEdit = False
    DataSet = ClientDataSet
    Left = 49
    Top = 494
  end
  object ClientDataSet: TClientDataSet
    Aggregates = <>
    Params = <>
    BeforeInsert = ClientDataSetBeforeInsert
    AfterScroll = ClientDataSetAfterScroll
    Left = 19
    Top = 494
  end
  object PopupMenu: TPopupMenu
    Left = 79
    Top = 494
    object PopupMenuCopiar: TMenuItem
      Caption = 'Copiar'
      ShortCut = 16451
      OnClick = PopupMenuCopiarClick
    end
    object PopupMenuExcluir: TMenuItem
      Caption = 'Excluir'
      ShortCut = 46
      OnClick = PopupMenuExcluirClick
    end
  end
end
