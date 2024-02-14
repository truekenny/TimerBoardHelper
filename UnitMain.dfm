object FormMain: TFormMain
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'TimerBoardHelper'
  ClientHeight = 440
  ClientWidth = 323
  Color = clBtnFace
  Constraints.MinHeight = 300
  Constraints.MinWidth = 300
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnClose = FormClose
  OnCreate = FormCreate
  OnDblClick = FormDblClick
  OnResize = FormResize
  DesignSize = (
    323
    440)
  TextHeight = 15
  object LabelGetCode: TLabel
    Left = 246
    Top = 101
    Width = 49
    Height = 15
    Cursor = crHandPoint
    Alignment = taRightJustify
    Anchors = [akTop, akRight]
    Caption = 'Get Code'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = [fsUnderline]
    ParentFont = False
    OnClick = LabelGetCodeClick
    ExplicitLeft = 240
  end
  object LabelLog: TLabel
    Left = 16
    Top = 402
    Width = 25
    Height = 15
    Cursor = crHandPoint
    Anchors = [akLeft, akBottom]
    Caption = 'Logs'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = [fsUnderline]
    ParentFont = False
    OnClick = LabelLogClick
    ExplicitTop = 403
  end
  object LabeledEditSite: TLabeledEdit
    Left = 16
    Top = 24
    Width = 279
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    EditLabel.Width = 19
    EditLabel.Height = 15
    EditLabel.Caption = 'Site'
    TabOrder = 0
    Text = ''
  end
  object LabeledEditCode: TLabeledEdit
    Left = 16
    Top = 72
    Width = 279
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    EditLabel.Width = 28
    EditLabel.Height = 15
    EditLabel.Caption = 'Code'
    TabOrder = 1
    Text = ''
  end
  object CheckListBoxOptions: TCheckListBox
    Left = 16
    Top = 128
    Width = 279
    Height = 255
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    ItemHeight = 17
    Items.Strings = (
      'At Timer'
      '5 minutes before a Timer'
      '10 minutes before a Timer'
      'Show Welcome message'
      'Show Disconnect message')
    ParentFont = False
    TabOrder = 2
  end
  object ButtonStart: TButton
    Left = 146
    Top = 398
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Start'
    TabOrder = 3
    OnClick = ButtonStartClick
  end
  object ButtonStop: TButton
    Left = 220
    Top = 398
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Stop'
    Enabled = False
    TabOrder = 4
    OnClick = ButtonStopClick
  end
  object ButtonClose: TButton
    Left = 64
    Top = 398
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Close'
    TabOrder = 5
    Visible = False
    OnClick = ButtonCloseClick
  end
  object TrayIcon: TTrayIcon
    Hint = 'TimerBoardHelper'
    PopupMenu = PopupMenuTray
    Visible = True
    Left = 80
    Top = 320
  end
  object PopupMenuTray: TPopupMenu
    Left = 80
    Top = 208
    object MenuExit: TMenuItem
      Caption = 'Exit'
      OnClick = MenuExitClick
    end
    object MenuSeparator: TMenuItem
      Caption = '-'
    end
    object MenuRestore: TMenuItem
      Caption = 'Restore'
      OnClick = MenuRestoreClick
    end
    object MenuTimerBoardHelper: TMenuItem
      Caption = 'TimerBoardHelper'
      Enabled = False
    end
  end
  object TimerReconnect: TTimer
    Enabled = False
    Interval = 10000
    OnTimer = TimerReconnectTimer
    Left = 200
    Top = 264
  end
  object TimerReconnectForSleep: TTimer
    Enabled = False
    Interval = 5000
    OnTimer = TimerReconnectForSleepTimer
    Left = 80
    Top = 264
  end
  object TimerNotificationHide: TTimer
    OnTimer = TimerNotificationHideTimer
    Left = 200
    Top = 320
  end
end
