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
  Vcl.CheckLst;

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
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure LabelGetCodeClick(Sender: TObject);
  private
    { Private declarations }
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
  ShellExecute(0, 'open', PChar(LabeledEditSite.Text + '/notification-code'), nil, nil, SW_SHOWNORMAL);
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

    LabeledEditSite.Text := ini.ReadString('main', 'url', 'https://pirat.zcharboard.ru');
    LabeledEditCode.Text := ini.ReadString('main', 'code', '<code require, press Get Code>');

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
