program TimerBoardHelper;

uses
  Vcl.Forms,
  UnitMain in 'UnitMain.pas' {FormMain},
  UnitNotification in 'UnitNotification.pas',
  Bird.Socket.Client.Consts in 'Bird.Socket.Client.Consts.pas',
  Bird.Socket.Client in 'Bird.Socket.Client.pas',
  Bird.Socket.Client.Types in 'Bird.Socket.Client.Types.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
