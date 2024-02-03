object FormMain: TFormMain
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'TimerBoardHelper'
  ClientHeight = 441
  ClientWidth = 301
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
  DesignSize = (
    301
    441)
  TextHeight = 15
  object LabelGetCode: TLabel
    Left = 232
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
  object LabeledEditSite: TLabeledEdit
    Left = 16
    Top = 24
    Width = 265
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    EditLabel.Width = 19
    EditLabel.Height = 15
    EditLabel.Caption = 'Site'
    TabOrder = 0
    Text = ''
    ExplicitWidth = 261
  end
  object LabeledEditCode: TLabeledEdit
    Left = 16
    Top = 72
    Width = 265
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    EditLabel.Width = 28
    EditLabel.Height = 15
    EditLabel.Caption = 'Code'
    TabOrder = 1
    Text = ''
    ExplicitWidth = 261
  end
  object CheckListBoxOptions: TCheckListBox
    Left = 16
    Top = 128
    Width = 265
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
      '10 minutes before a Timer')
    ParentFont = False
    TabOrder = 2
    ExplicitWidth = 261
    ExplicitHeight = 255
  end
  object ButtonStart: TButton
    Left = 132
    Top = 399
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Start'
    TabOrder = 3
    OnClick = ButtonStartClick
    ExplicitLeft = 128
    ExplicitTop = 398
  end
  object ButtonStop: TButton
    Left = 206
    Top = 399
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Stop'
    Enabled = False
    TabOrder = 4
    OnClick = ButtonStopClick
    ExplicitLeft = 202
    ExplicitTop = 398
  end
  object TrayIcon: TTrayIcon
    PopupMenu = PopupMenuTray
    Visible = True
    OnBalloonClick = TrayIconBalloonClick
    Left = 136
    Top = 8
  end
  object WebSocket: TSslWebSocketCli
    LocalAddr = '0.0.0.0'
    LocalAddr6 = '::'
    ProxyPort = '80'
    Agent = 'Mozilla/4.0'
    Accept = 'image/gif, image/x-xbitmap, image/jpeg, image/pjpeg, */*'
    NoCache = False
    ResponseNoException = False
    ContentTypePost = 'application/x-www-form-urlencoded'
    LmCompatLevel = 0
    RequestVer = '1.1'
    FollowRelocation = True
    LocationChangeMaxCount = 5
    ServerAuth = httpAuthNone
    ProxyAuth = httpAuthNone
    BandwidthLimit = 10000
    BandwidthSampling = 1000
    Options = [httpoEnableContentCoding, httpoGetContent]
    Timeout = 30
    SocksLevel = '5'
    SocksAuthentication = socksNoAuthentication
    SocketFamily = sfAny
    SocketErrs = wsErrTech
    RestParams.PContent = PContUrlencoded
    RestParams.RfcStrict = False
    RestParams = <>
    DebugLevel = DebugSsl
    MaxBodySize = 104857600
    SslCliSecurity = sslCliSecTls12
    SslSessCache = True
    CertVerMethod = CertVerNone
    SslRootFile = 'RootCaCertsBundle.pem'
    SslRevocation = False
    SslReportChain = False
    SslAllowSelfSign = False
    HttpMemStrategy = HttpStratMem
    HttpDownReplace = False
    ResumeMinSize = 65535
    ProgIntSecs = 2
    ShowProgress = False
    HttpUploadStrat = HttpUploadNone
    WSPingSecs = 10
    OnWSConnected = WebSocketWSConnected
    OnWSDisconnected = WebSocketWSDisconnected
    OnWSFrameRcvd = WebSocketWSFrameRcvd
    Left = 200
    Top = 8
  end
  object PopupMenuTray: TPopupMenu
    Left = 56
    Top = 8
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
    Interval = 5000
    OnTimer = TimerReconnectTimer
    Left = 56
    Top = 96
  end
end
