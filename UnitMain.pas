unit UnitMain;

interface

uses
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
  Vcl.CheckLst, Vcl.Menus;

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
    procedure TrayIconBalloonClick(Sender: TObject);
    procedure TimerReconnectTimer(Sender: TObject);
    procedure MenuExitClick(Sender: TObject);
    procedure TimerReconnectForSleepTimer(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
    lastUrl: String;
    autoReconnect: Boolean;

    function GetFile(): TIniFile;
    procedure log(s: String; firstMessage: Boolean = False);
    procedure Send(s: String);
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

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
  fileName := ExtractFilePath(ParamStr(0)) + '\log.txt';

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
  Result := TIniFile.Create(ExtractFilePath(ParamStr(0)) + '\' + 'config.ini')
end;

procedure TFormMain.LabelGetCodeClick(Sender: TObject);
begin
  ShellExecute(0, 'open', PChar(LabeledEditSite.Text + '/notification-code'),
    nil, nil, SW_SHOWNORMAL);
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

procedure TFormMain.TimerReconnectForSleepTimer(Sender: TObject);
begin
  log('TimerReconnectForSleepTimer: autoReconnect: ' + BoolToStr(autoReconnect));

  TimerReconnectForSleep.Enabled := False;
  TimerReconnect.Enabled := True;
end;

procedure TFormMain.TimerReconnectTimer(Sender: TObject);
begin
  log('TimerReconnectTimer: autoReconnect: ' + BoolToStr(autoReconnect));

  TimerReconnect.Enabled := False;

  WebSocket.Abort;
  WebSocket.WSConnect;
end;

procedure TFormMain.TrayIconBalloonClick(Sender: TObject);
begin
  if lastUrl <> '' then
  begin
    ShellExecute(0, 'open', PChar(lastUrl), nil, nil, SW_SHOWNORMAL);
  end;
end;

procedure TFormMain.WebSocketWSConnected(Sender: TObject);
begin
  log('WebSocketWSConnected: ' + WebSocket.ReasonPhrase);

  autoReconnect := True;

  Send('code/' + LabeledEditCode.Text);
end;

procedure TFormMain.WebSocketWSDisconnected(Sender: TObject);
begin
  log('WebSocketWSDisconnected: ' + WebSocket.ReasonPhrase + ' autoReconnect: '
    + BoolToStr(autoReconnect));

  if autoReconnect then
  begin
    autoReconnect := False;
    TimerReconnectForSleep.Enabled := True;
  end
  else
  begin
    ButtonStopClick(Sender);
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

  if APacket = 'ping' then
  begin
    Send('pong')
  end

  else if APacket = 'ok' then
  begin

  end

  else if APacket = 'options' then
  begin
    if CheckListBoxOptions.Checked[0] then
    begin
      Send('timer/0');
    end;
    if CheckListBoxOptions.Checked[1] then
    begin
      Send('timer/5');
    end;
    if CheckListBoxOptions.Checked[2] then
    begin
      Send('timer/10');
    end;
  end

  else if Pos('{', APacket) = 1 then
  begin
    JSON := TJSONObject.ParseJSONValue(APacket, False, True) as TJSONObject;

    TrayIcon.BalloonTitle := JSON.FindValue('title').Value;
    TrayIcon.BalloonHint := JSON.FindValue('text').Value;
    TrayIcon.ShowBalloonHint;

    lastUrl := LabeledEditSite.Text + JSON.FindValue('url').Value;

    sound := JSON.FindValue('sound').Value;
    if sound <> '' then
    begin
      sndPlaySound(PChar(sound), SND_NODEFAULT Or SND_ASYNC);
    end;

    FreeAndNil(JSON);
  end

  else
  begin
    lastUrl := LabeledEditSite.Text + '/timer';

    TrayIcon.BalloonTitle := '';
    TrayIcon.BalloonHint := APacket;
    TrayIcon.ShowBalloonHint;
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

  WebSocket.URL := LabeledEditSite.Text + '/socket/';
  WebSocket.URL := StringReplace(WebSocket.URL, 'http', 'ws', [rfIgnoreCase]);

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
      ini.WriteBool('notification', 'checked.' + IntToStr(index),
        CheckListBoxOptions.Checked[index]);
    end;
  finally
    ini.Free;
  end;

  autoReconnect := False;
  WebSocket.Abort;

  log('FormClose');
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

  WebSocket.Timeout := 30;
  WebSocket.WSPingSecs := 30;
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
        'checked.' + IntToStr(index), True);
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
