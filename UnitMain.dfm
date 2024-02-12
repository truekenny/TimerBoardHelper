object FormMain: TFormMain
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'TimerBoardHelper'
  ClientHeight = 441
  ClientWidth = 327
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
  OnResize = FormResize
  DesignSize = (
    327
    441)
  TextHeight = 15
  object LabelGetCode: TLabel
    Left = 254
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
    Top = 403
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
  end
  object LabeledEditSite: TLabeledEdit
    Left = 16
    Top = 24
    Width = 287
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    EditLabel.Width = 19
    EditLabel.Height = 15
    EditLabel.Caption = 'Site'
    TabOrder = 0
    Text = ''
    ExplicitWidth = 283
  end
  object LabeledEditCode: TLabeledEdit
    Left = 16
    Top = 72
    Width = 287
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    EditLabel.Width = 28
    EditLabel.Height = 15
    EditLabel.Caption = 'Code'
    TabOrder = 1
    Text = ''
    ExplicitWidth = 283
  end
  object CheckListBoxOptions: TCheckListBox
    Left = 16
    Top = 128
    Width = 287
    Height = 256
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    ItemHeight = 15
    Items.Strings = (
      'At Timer'
      '5 minutes before a Timer'
      '10 minutes before a Timer'
      'Show Welcome message'
      'Show Disconnect message')
    ParentFont = False
    TabOrder = 2
    ExplicitWidth = 283
    ExplicitHeight = 255
  end
  object ButtonStart: TButton
    Left = 154
    Top = 399
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Start'
    TabOrder = 3
    OnClick = ButtonStartClick
    ExplicitLeft = 150
    ExplicitTop = 398
  end
  object ButtonStop: TButton
    Left = 228
    Top = 399
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Stop'
    Enabled = False
    TabOrder = 4
    OnClick = ButtonStopClick
    ExplicitLeft = 224
    ExplicitTop = 398
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
