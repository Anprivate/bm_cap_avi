program MCRegister;

uses
  Vcl.Forms,
  RegisterMainUnit in 'RegisterMainUnit.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
