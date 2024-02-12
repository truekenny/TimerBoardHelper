unit UnitMain;

interface

uses
  UnitNotification,
  DateUtils,
  MMSystem,
  System.JSON,
  ShellApi,
  IniFiles,
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  OverbyteIcsWndControl,
  OverbyteIcsHttpProt,
  OverbyteIcsSslHttpRest,
  OverbyteIcsWebSocketCli,
  Vcl.StdCtrls,
  Vcl.Mask,
  Vcl.CheckLst,
  Vcl.Menus;

const
  CHECK_TIMER_0 = 0;
  CHECK_TIMER_5 = 1;
  CHECK_TIMER_10 = 2;
  CHECK_SHOW_WELCOME = 3;
  CHECK_SHOW_DISCONNECT = 4;

type
  TFormMain = class(TForm)
    TrayIcon: TTrayIcon;
    WebSocket: TSslWebSocketCli;
    LabeledEditSite: TLabeledEdit;
    LabeledEditCode: TLabeledEdit;
    LabelGetCode: TLabel;
    CheckListBoxOptions: TCheckListBox;
    ButtonStart: TButton;
    ButtonStop: TButton;
    PopupMenuTray: TPopupMenu;
    MenuRestore: TMenuItem;
    MenuTimerBoardHelper: TMenuItem;
    TimerReconnect: TTimer;
    MenuExit: TMenuItem;
    TimerReconnectForSleep: TTimer;
    TimerNotificationHide: TTimer;
    LabelLog: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure LabelGetCodeClick(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
    procedure WebSocketWSFrameRcvd(Sender: TSslWebSocketCli;
      const APacket: string; var AFrame: TWebSocketReceivedFrame);
    procedure WebSocketWSDisconnected(Sender: TObject);
    procedure WebSocketWSConnected(Sender: TObject);
    procedure MenuRestoreClick(Sender: TObject);
    procedure TimerReconnectTimer(Sender: TObject);
    procedure MenuExitClick(Sender: TObject);
    procedure TimerReconnectForSleepTimer(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure TimerNotificationHideTimer(Sender: TObject);
    procedure LabelLogClick(Sender: TObject);
  private
    { Private declarations }
    autoReconnect: Boolean;
    Notification: TNotification;

    function GetFile(): TIniFile;
    procedure log(s: String; firstMessage: Boolean = False);
    procedure Send(s: String);
    procedure ShowNotification(const Title, Text: String;
      const ExpiredTime: TDateTime; const Url: String = '');
    procedure AlignItems();
    procedure AlignWidth(Item: TControl);
    procedure AlignRight(Item: TControl; addSpace: Integer = 0);
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.AlignWidth(Item: TControl);
begin
  Item.Width := ClientWidth - 2 * Item.Left;
end;

procedure TFormMain.AlignRight(Item: TControl; addSpace: Integer = 0);
begin
  Item.Left := ClientWidth - Item.Width - LabeledEditSite.Left - addSpace;
end;

procedure TFormMain.AlignItems();
begin
  AlignWidth(LabeledEditSite);
  AlignWidth(LabeledEditCode);
  AlignWidth(CheckListBoxOptions);

  AlignRight(ButtonStop);
  AlignRight(LabelGetCode);
  AlignRight(ButtonStart, ButtonStop.Width);
end;

procedure TFormMain.ShowNotification(const Title, Text: String;
  const ExpiredTime: TDateTime; const Url: String = '');
const
  // <action content="Open Google" activationType="protocol" arguments="http://www.google.com" />
  // <action content="Open path" activationType="protocol" arguments="file:///c:\" />
  // <action activationType="system" arguments="dismiss" content=""/>

  TEMPLATE = '<toast activationType="protocol" launch="__URL__">' +
    '<visual><binding template="ToastGeneric">' +
    '<text>__TITLE__</text><text>__TEXT__</text>' +
    '<image placement="appLogoOverride" src="file:///__ICON__"/>' +
    '</binding></visual>' + '<actions></actions>' + '</toast>';

  APP_ID = 'TimerBoardHelper';
var
  xml: String;
begin
  xml := TEMPLATE;
  xml := StringReplace(xml, '__URL__', LabeledEditSite.Text + Url,
    [rfIgnoreCase]);
  xml := StringReplace(xml, '__TITLE__', Title, [rfIgnoreCase]);
  xml := StringReplace(xml, '__TEXT__', Text, [rfIgnoreCase]);
  xml := StringReplace(xml, '__ICON__', ExtractFilePath(ParamStr(0)) +
    'appLogoOverride.png', [rfIgnoreCase]);

  Notification.Show(APP_ID, xml, ExpiredTime);
end;

procedure TFormMain.Send(s: String);
begin
  log('send: ' + s);
  WebSocket.WSSendText(nil, s);
end;

procedure TFormMain.log(s: String; firstMessage: Boolean = False);
var
  fileName: String;
  logFile: TextFile;
begin
  fileName := ExtractFilePath(ParamStr(0)) + 'log.txt';

  AssignFile(logFile, fileName);

  if FileExists(fileName) then
  begin
    Append(logFile);

    if firstMessage then
    begin
      Writeln(logFile, #13);
    end;
  end
  else
  begin
    Rewrite(logFile);
  end;

  Writeln(logFile, DateTimeToStr(Now) + ' ' + s);
  CloseFile(logFile);
end;

function TFormMain.GetFile(): TIniFile;
begin
  Result := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'config.ini')
end;

procedure TFormMain.LabelGetCodeClick(Sender: TObject);
begin
  ShellExecute(0, 'open', PChar(LabeledEditSite.Text + '/notification-code'),
    nil, nil, SW_SHOWNORMAL);
end;

procedure TFormMain.LabelLogClick(Sender: TObject);
begin
  ShellExecute(0, 'open', PChar(ExtractFilePath(ParamStr(0)) + 'log.txt'), nil,
    nil, SW_SHOWNORMAL);
end;

procedure TFormMain.MenuExitClick(Sender: TObject);
begin
  log('MenuExitClick');

  Close();
end;

procedure TFormMain.MenuRestoreClick(Sender: TObject);
begin
  ShowWindow(Handle, SW_NORMAL);
end;

procedure TFormMain.TimerNotificationHideTimer(Sender: TObject);
begin
  if Notification <> nil then
    Notification.HideAll(True);
end;

procedure TFormMain.TimerReconnectForSleepTimer(Sender: TObject);
begin
  log('TimerReconnectForSleepTimer: autoReconnect: ' + BoolToStr(autoReconnect,
    True), True);

  TimerReconnectForSleep.Enabled := False;
  TimerReconnect.Enabled := True;
end;

procedure TFormMain.TimerReconnectTimer(Sender: TObject);
begin
  log('TimerReconnectTimer: autoReconnect: ' + BoolToStr(autoReconnect, True));

  TimerReconnect.Enabled := False;

  WebSocket.Abort;
  WebSocket.WSConnect;
end;

procedure TFormMain.WebSocketWSConnected(Sender: TObject);
begin
  log('WebSocketWSConnected: ' + WebSocket.ReasonPhrase);
end;

procedure TFormMain.WebSocketWSDisconnected(Sender: TObject);
var
  Text: String;
begin
  log('WebSocketWSDisconnected: Connected: ' + BoolToStr(WebSocket.Connected,
    True) + ', ReasonPhrase: ' + WebSocket.ReasonPhrase + ', autoReconnect: ' +
    BoolToStr(autoReconnect, True));

  if not WebSocket.Connected then
  begin
    if autoReconnect then
    begin
      Text := '(Auto reconnecting...)';
      autoReconnect := False;
      TimerReconnectForSleep.Enabled := True;
    end
    else
    begin
      Text := '(Shutdown)';
      ButtonStopClick(Sender);
    end;

    if CheckListBoxOptions.Checked[CHECK_SHOW_DISCONNECT] then
    begin
      ShowNotification('Disconnected', Text, IncSecond(Now, 15));
    end;
  end;
end;

procedure TFormMain.WebSocketWSFrameRcvd(Sender: TSslWebSocketCli;
  const APacket: string; var AFrame: TWebSocketReceivedFrame);
var
  JSON: TJSONObject;
  sound: String;
begin
  log('WebSocketWSFrameRcvd: ReasonPhrase: "' + WebSocket.ReasonPhrase +
    '" APacket: "' + APacket + '"');

  // ping
  if APacket = 'ping' then
  begin
    Send('pong')
  end

  // welcome
  else if APacket = 'welcome' then
  begin
    autoReconnect := True;

    Send('code/' + LabeledEditCode.Text);

    if CheckListBoxOptions.Checked[CHECK_SHOW_WELCOME] then
    begin
      ShowNotification(APacket, '(Message received)', IncSecond(Now, 5));
    end;
  end

  // ok
  else if APacket = 'ok' then
  begin

  end

  // options
  else if APacket = 'options' then
  begin
    if CheckListBoxOptions.Checked[CHECK_TIMER_0] then
    begin
      Send('timer/0');
    end;
    if CheckListBoxOptions.Checked[CHECK_TIMER_5] then
    begin
      Send('timer/5');
    end;
    if CheckListBoxOptions.Checked[CHECK_TIMER_10] then
    begin
      Send('timer/10');
    end;
  end

  // json
  else if Pos('{', APacket) = 1 then
  begin
    JSON := TJSONObject.ParseJSONValue(APacket, False, True) as TJSONObject;

    ShowNotification(JSON.FindValue('title').Value, JSON.FindValue('text')
      .Value, IncSecond(Now, 60), JSON.FindValue('url').Value);

    sound := JSON.FindValue('sound').Value;
    if sound <> '' then
    begin
      sndPlaySound(PChar(sound), SND_NODEFAULT Or SND_ASYNC);
    end;

    FreeAndNil(JSON);
  end

  // else
  else if APacket <> '' then
  begin
    ShowNotification(APacket, '(Message received)', IncSecond(Now, 15));
  end;
end;

procedure TFormMain.ButtonStartClick(Sender: TObject);
begin
  ShowWindow(Handle, SW_HIDE);

  ButtonStart.Enabled := False;
  ButtonStop.Enabled := True;
  CheckListBoxOptions.Enabled := False;
  LabeledEditSite.Enabled := False;
  LabeledEditCode.Enabled := False;

  WebSocket.Url := LabeledEditSite.Text + '/socket/';
  WebSocket.Url := StringReplace(WebSocket.Url, 'http', 'ws', [rfIgnoreCase]);

  // WebSocket.Abort;
  WebSocket.WSConnect;
end;

procedure TFormMain.ButtonStopClick(Sender: TObject);
begin
  autoReconnect := False;
  TimerReconnect.Enabled := False;
  TimerReconnectForSleep.Enabled := False;

  ShowWindow(Handle, SW_NORMAL);

  ButtonStart.Enabled := True;
  ButtonStop.Enabled := False;
  CheckListBoxOptions.Enabled := True;
  LabeledEditSite.Enabled := True;
  LabeledEditCode.Enabled := True;

  WebSocket.Abort;
end;

procedure TFormMain.FormClose(Sender: TObject; var Action: TCloseAction);
var
  ini: TIniFile;
  index: Integer;
begin
  ini := GetFile();
  try
    ini.WriteInteger('main', 'left', Left);
    ini.WriteInteger('main', 'top', Top);
    ini.WriteInteger('main', 'width', Width);
    ini.WriteInteger('main', 'height', Height);

    ini.WriteString('main', 'url', LabeledEditSite.Text);
    ini.WriteString('main', 'code', LabeledEditCode.Text);

    for index := 0 to CheckListBoxOptions.Count - 1 do
    begin
      ini.WriteBool('notification', 'checked.' + inttostr(index),
        CheckListBoxOptions.Checked[index]);
    end;
  finally
    ini.Free;
  end;

  autoReconnect := False;
  WebSocket.Abort;

  log('FormClose');

  Notification.HideAll();
end;

procedure TFormMain.FormCreate(Sender: TObject);
const
  DEFAULT_WIDTH = 300;
  DEFAULT_HEIGHT = 400;
var
  ini: TIniFile;
  index: Integer;
begin
  log('FormCreate', True);

  Notification := TNotification.Create;
  Notification.Init('TimerBoardHelper');

  AlignItems();

  WebSocket.Timeout := 30;
  WebSocket.WSPingSecs := 10;
  autoReconnect := False;

  ini := GetFile();
  try
    Left := ini.ReadInteger('main', 'left',
      Round((Screen.Width - DEFAULT_WIDTH) / 2));
    Top := ini.ReadInteger('main', 'top',
      Round((Screen.Height - DEFAULT_HEIGHT) / 2));
    Width := ini.ReadInteger('main', 'width', DEFAULT_WIDTH);
    Height := ini.ReadInteger('main', 'height', DEFAULT_HEIGHT);

    LabeledEditSite.Text := ini.ReadString('main', 'url',
      'https://pirat.zcharboard.ru');
    LabeledEditCode.Text := ini.ReadString('main', 'code',
      '<code require, press Get Code>');

    for index := 0 to CheckListBoxOptions.Count - 1 do
    begin
      CheckListBoxOptions.Checked[index] := ini.ReadBool('notification',
        'checked.' + inttostr(index), True);
    end;

  finally
    ini.Free;
  end;
end;

procedure TFormMain.FormResize(Sender: TObject);
begin
  if WindowState = TWindowState.wsMinimized then
  begin
    ShowWindow(Handle, SW_HIDE);
  end;
end;

end.
