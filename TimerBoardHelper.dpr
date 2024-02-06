program TimerBoardHelper;

uses
  Vcl.Forms,
  UnitMain in 'UnitMain.pas' {FormMain},
  UnitNotification in 'UnitNotification.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
