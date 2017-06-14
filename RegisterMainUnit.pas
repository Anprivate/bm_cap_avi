unit RegisterMainUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, System.Win.Registry,
  Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    ButtonSetInvert: TButton;
    ButtonClearInvert: TButton;
    ButtonForce169: TButton;
    ButtonForce43: TButton;
    ButtonARAuto: TButton;
    ButtonDVSD: TButton;
    ButtonDVCPRO: TButton;
    ButtonClampYUV: TButton;
    ButtonNoClampYUV: TButton;
    procedure ShowCurrentState;
    procedure FormCreate(Sender: TObject);
    procedure ButtonSetInvertClick(Sender: TObject);
    procedure SetParameter(Name: string; value: integer);
    procedure ButtonClearInvertClick(Sender: TObject);
    procedure ButtonForce169Click(Sender: TObject);
    procedure ButtonForce43Click(Sender: TObject);
    procedure ButtonARAutoClick(Sender: TObject);
    procedure ButtonDVSDClick(Sender: TObject);
    procedure ButtonDVCPROClick(Sender: TObject);
    procedure ButtonClampYUVClick(Sender: TObject);
    procedure ButtonNoClampYUVClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.ButtonARAutoClick(Sender: TObject);
begin
  SetParameter('Force 16:9 flag in output', 0);
  SetParameter('Clear 16:9 flag in output', 0);
  ShowCurrentState;
end;

procedure TForm1.ButtonClampYUVClick(Sender: TObject);
begin
  SetParameter('Clamp YUV colorspaces', 1);
  ShowCurrentState;
end;

procedure TForm1.ButtonClearInvertClick(Sender: TObject);
begin
  SetParameter('Invert fields', 0);
  ShowCurrentState;
end;

procedure TForm1.ButtonDVCPROClick(Sender: TObject);
begin
  SetParameter('Encoding mode', 1);
  ShowCurrentState;
end;

procedure TForm1.ButtonDVSDClick(Sender: TObject);
begin
  SetParameter('Encoding mode', 0);
  ShowCurrentState;
end;

procedure TForm1.ButtonForce169Click(Sender: TObject);
begin
  SetParameter('Force 16:9 flag in output', 1);
  SetParameter('Clear 16:9 flag in output', 0);
  ShowCurrentState;
end;

procedure TForm1.ButtonForce43Click(Sender: TObject);
begin
  SetParameter('Force 16:9 flag in output', 0);
  SetParameter('Clear 16:9 flag in output', 1);
  ShowCurrentState;
end;

procedure TForm1.ButtonNoClampYUVClick(Sender: TObject);
begin
  SetParameter('Clamp YUV colorspaces', 0);
  ShowCurrentState;
end;

procedure TForm1.ButtonSetInvertClick(Sender: TObject);
begin
  SetParameter('Invert fields', 1);
  ShowCurrentState;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ShowCurrentState;
end;

procedure TForm1.SetParameter(Name: string; value: integer);
var
  Registry: TRegistry;
  ProgramNames: TStringList;
  ValuesNames: TStringList;
  i: integer;
  basic_path, current_path: string;
begin
  Registry := TRegistry.Create();
  try
    Registry.Access := KEY_READ;

    Registry.RootKey := HKEY_CURRENT_USER;
    basic_path := '\Software\MainConcept\MainConcept DV Video Encoder';

    if (not Registry.KeyExists(basic_path)) then
    begin
      Memo1.Lines.Append('Mainconcept не обнаружен в регистре!');
      Exit;
    end;

    if not Registry.OpenKey(basic_path, false) then
    begin
      Memo1.Lines.Append('Не могу открыть ' + basic_path + '!');
      Exit;
    end;

    ProgramNames := TStringList.Create;
    Registry.GetKeyNames(ProgramNames);

    Registry.Access := KEY_WRITE;
    for i := 0 to ProgramNames.Count - 1 do
    begin
      current_path := basic_path + '\' + ProgramNames.Strings[i];
      if not Registry.OpenKey(current_path, false) then
      begin
        Memo1.Lines.Append('Не могу открыть ' + current_path + '!');
        Exit;
      end;

      Registry.WriteInteger(Name, value);
    end;

    ProgramNames.Free;
  finally
    Registry.Free;
  end;

end;

procedure TForm1.ShowCurrentState;
var
  Registry: TRegistry;
  ProgramNames: TStringList;
  ValuesNames: TStringList;
  i: integer;
  basic_path, current_path: string;
begin
  Memo1.Clear;
  Registry := TRegistry.Create();
  try
    Registry.Access := KEY_READ;

    Registry.RootKey := HKEY_CURRENT_USER;
    basic_path := '\Software\MainConcept\MainConcept DV Video Encoder';

    if (not Registry.KeyExists(basic_path)) then
    begin
      Memo1.Lines.Append('Mainconcept не обнаружен в регистре!');
    end;

    if not Registry.OpenKey(basic_path, false) then
      Memo1.Lines.Append('Не могу открыть ' + basic_path + '!');

    ProgramNames := TStringList.Create;
    // Registry.GetValueNames();
    Registry.GetKeyNames(ProgramNames);

    for i := 0 to ProgramNames.Count - 1 do
    begin
      Memo1.Lines.Append(ProgramNames.Strings[i]);
      current_path := basic_path + '\' + ProgramNames.Strings[i];
      if not Registry.OpenKey(current_path, false) then
        Memo1.Lines.Append('Не могу открыть ' + current_path + '!');
      if Registry.ReadInteger('Invert fields') = 0 then
        Memo1.Lines.Append(' Invert fields = NO')
      else
        Memo1.Lines.Append(' Invert fields = YES');

      if Registry.ReadInteger('Force 16:9 flag in output') = 0 then
        Memo1.Lines.Append(' Force 16:9 flag in output = NO')
      else
        Memo1.Lines.Append(' Force 16:9 flag in output = YES');

      if Registry.ReadInteger('Clear 16:9 flag in output') = 0 then
        Memo1.Lines.Append(' Clear 16:9 flag in output = NO')
      else
        Memo1.Lines.Append(' Clear 16:9 flag in output = YES');

      if Registry.ReadInteger('Encoding mode') = 0 then
        Memo1.Lines.Append(' Encoding mode = DVSD')
      else
        Memo1.Lines.Append(' Encoding mode = DVCPRO');

      if Registry.ReadInteger('Clamp YUV colorspaces') = 0 then
        Memo1.Lines.Append(' Clamp YUV colorspaces = NO')
      else
        Memo1.Lines.Append(' Clamp YUV colorspaces = YES');

      Memo1.Lines.Append('');
    end;

    ProgramNames.Free;
  finally
    Registry.Free;
  end;
end;

end.
