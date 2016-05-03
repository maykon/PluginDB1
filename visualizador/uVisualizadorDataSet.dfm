object fVisualizadorDataSet: TfVisualizadorDataSet
  Left = 190
  Top = 76
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
    Top = 550
    Width = 511
    Height = 16
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
    Left = 13
    Top = 9
    Width = 28
    Height = 13
    Caption = 'Filtro:'
  end
  object grdDados: TDBGrid
    Left = 12
    Top = 31
    Width = 820
    Height = 516
    Anchors = [akLeft, akTop, akRight, akBottom]
    DataSource = DataSource
    PopupMenu = PopupMenu
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
    OnTitleClick = grdDadosTitleClick
  end
  object edtFiltro: TEdit
    Left = 46
    Top = 5
    Width = 361
    Height = 21
    MaxLength = 200
    TabOrder = 1
    OnChange = edtFiltroChange
    OnKeyPress = edtFiltroKeyPress
  end
  object chkFiltroAtivado: TCheckBox
    Left = 416
    Top = 8
    Width = 66
    Height = 17
    Caption = 'Ativado'
    TabOrder = 2
    OnClick = chkFiltroAtivadoClick
  end
  object clCampos: TCheckListBox
    Left = 845
    Top = 31
    Width = 168
    Height = 516
    OnClickCheck = clCamposClickCheck
    Anchors = [akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 3
  end
  object btnTodos: TBitBtn
    Left = 845
    Top = 3
    Width = 80
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Todos'
    TabOrder = 4
    OnClick = btnTodosClick
  end
  object btnNenhum: TBitBtn
    Left = 933
    Top = 3
    Width = 80
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Nenhum'
    TabOrder = 5
    OnClick = btnNenhumClick
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
    AfterOpen = ClientDataSetAfterOpen
    BeforeInsert = ClientDataSetBeforeInsert
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
