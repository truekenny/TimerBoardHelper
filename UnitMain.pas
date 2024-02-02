unit UnitMain;

interface

uses
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
  private
    { Private declarations }
    lastUrl: String;

    function GetFile(): TIniFile;
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

function TFormMain.GetFile(): TIniFile;
begin
  Result := TIniFile.Create(ExtractFilePath(ParamStr(0)) + '\' + 'config.ini')
end;

procedure TFormMain.LabelGetCodeClick(Sender: TObject);
begin
  ShellExecute(0, 'open', PChar(LabeledEditSite.Text + '/notification-code'),
    nil, nil, SW_SHOWNORMAL);
end;

procedure TFormMain.MenuRestoreClick(Sender: TObject);
begin
  ShowWindow(Handle, SW_NORMAL);
end;

procedure TFormMain.TrayIconBalloonClick(Sender: TObject);
begin
  ShellExecute(0, 'open', PChar(lastUrl), nil, nil, SW_SHOWNORMAL);
end;

procedure TFormMain.WebSocketWSConnected(Sender: TObject);
begin
  WebSocket.WSSendText(nil, 'code,' + LabeledEditCode.Text);
end;

procedure TFormMain.WebSocketWSDisconnected(Sender: TObject);
begin
  ButtonStopClick(Sender);
end;

procedure TFormMain.WebSocketWSFrameRcvd(Sender: TSslWebSocketCli;
  const APacket: string; var AFrame: TWebSocketReceivedFrame);
begin
  if APacket = 'ping' then
  begin
    WebSocket.WSSendText(nil, 'pong')
  end

  else if APacket = 'options' then
  begin
    if CheckListBoxOptions.Checked[0] then
    begin
      WebSocket.WSSendText(nil, 'timer,0');
    end;
    if CheckListBoxOptions.Checked[1] then
    begin
      WebSocket.WSSendText(nil, 'timer,5');
    end;
    if CheckListBoxOptions.Checked[2] then
    begin
      WebSocket.WSSendText(nil, 'timer,10');
    end;
  end

  else
  begin
    lastUrl := LabeledEditSite.Text + '/timer';

    TrayIcon.BalloonTitle := 'Title';
    TrayIcon.BalloonHint := APacket;
    TrayIcon.ShowBalloonHint;
  end;
end;

procedure TFormMain.ButtonStartClick(Sender: TObject);
begin
  ShowWindow(Handle, SW_HIDE);

  ButtonStart.Enabled := False;
  ButtonStop.Enabled := True;

  WebSocket.URL := LabeledEditSite.Text + '/socket/';
  WebSocket.URL := StringReplace(WebSocket.URL, 'http', 'ws', [rfIgnoreCase]);

  WebSocket.WSConnect;
end;

procedure TFormMain.ButtonStopClick(Sender: TObject);
begin
  ShowWindow(Handle, SW_NORMAL);

  ButtonStart.Enabled := True;
  ButtonStop.Enabled := False;

  WebSocket.Close;
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

  WebSocket.Close;
end;

procedure TFormMain.FormCreate(Sender: TObject);
const
  DEFAULT_WIDTH = 300;
  DEFAULT_HEIGHT = 400;
var
  ini: TIniFile;
  index: Integer;
begin
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

end.
