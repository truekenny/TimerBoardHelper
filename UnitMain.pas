unit UnitMain;

interface

uses
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
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

end.
