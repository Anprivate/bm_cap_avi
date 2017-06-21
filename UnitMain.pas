unit UnitMain;

interface

uses
  System.SysUtils, System.Variants, System.Classes, System.IniFiles,
  System.StrUtils, System.DateUtils, System.Math, System.AnsiStrings,
  Winapi.Windows, Winapi.Messages, Winapi.MMsystem, Winapi.ActiveX,
  Winapi.DirectShow9,
  Vcl.Menus, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.ComCtrls, System.Win.ComObj,
  Data.DSUtil,
  Unit_AVIwriting, Unit_DS_Capturing, Copier_thread, emailer_thread;

const
  MyLogMessage = WM_USER + 1;

type
  TFormDecklink = class(TForm)
    TimerMain: TTimer;
    Panel1: TPanel;
    MemoMain: TMemo;
    GroupBoxBuffer: TGroupBox;
    LabelVideoFrInBuf: TLabel;
    ProgressBarVideoBuf: TProgressBar;
    GroupBoxFile: TGroupBox;
    LabelFileName: TLabel;
    LabelFileFrames: TLabel;
    ProgressBarFile: TProgressBar;
    GroupBoxTotal: TGroupBox;
    LabelTotal: TLabel;
    GroupBoxTC: TGroupBox;
    LabelTC: TLabel;
    ProgressBarAudioBuf: TProgressBar;
    LabelAudioFrInBuf: TLabel;
    GroupBoxAudio: TGroupBox;
    CheckBox0102: TCheckBox;
    CheckBox0304: TCheckBox;
    CheckBox0506: TCheckBox;
    CheckBox0708: TCheckBox;
    CheckBox0910: TCheckBox;
    CheckBox1112: TCheckBox;
    CheckBox1314: TCheckBox;
    CheckBox1516: TCheckBox;
    StatusBar1: TStatusBar;
    PanelControl: TPanel;
    ButtonStartStop: TButton;
    EditPrefix: TEdit;
    Label1: TLabel;
    ButtonEmail: TButton;
    ButtonRestart: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TimerMainTimer(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ButtonStartStopClick(Sender: TObject);
    procedure ButtonEmailClick(Sender: TObject);
    procedure ButtonRestartClick(Sender: TObject);
    procedure OnLogMessage(var Msg: TMessage); message MyLogMessage;
  private
    procedure WriteToLog(chto: string; email: Boolean = false);
    procedure FlushLog;
    function TCtoString(inTC: longint): AnsiString;
    function StringToBCD(instring: AnsiString): byte;
    procedure SetTCSpecial(TCin: AnsiString; IsWS: Boolean; OneFrame: PByte);
    procedure CloseAviFile;
  public
    { Public declarations }
  end;

type
  EMyOwnException = class(Exception);

var
  FormDecklink: TFormDecklink;
  CopierThread: TCopierThread;
  Emailer: Temailer;

  AviWriter: TAviWriter;
  DSCapturer: TDSCapturer;

  FileTC: Integer;
  Period: Integer;
  Preload: Integer;

  strcntr: Integer = 0;

  WasCloseRequest: Boolean = false;

  FilePath: string;
  FileTimeSource: Integer;
  FileUseDate: Boolean;
  VideoIsWS: Boolean;

  AudioHeaderType: TWAVEFORMATEX;

  ForLog: TStringList;
  RecordTO: Integer;

implementation

{$R *.dfm}

var
  Buff_Count: Integer;

  LastStamp: Int64;

  FilesToCopy: TStringList;
  DeleteSource: Boolean;
  WillUseCopier: Boolean;
  CopierThreadActive: Boolean;
  TimerIsActive: Boolean;

  ControlPanelVisible: Boolean;

  EmailerThreadActive: Boolean;
  EmailerForSend: TStringList;
  EmailerLastSend: TDateTime;
  IniFileName: string;

procedure TFormDecklink.WriteToLog(chto: string; email: Boolean = false);
var
  tmpStr: string;
begin
  tmpStr := FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now()) + ' ' + chto;
  ForLog.Append(tmpStr);

  MemoMain.Lines.Append(tmpStr);
  while MemoMain.Lines.Count > 100 do
    MemoMain.Lines.Delete(0);

  MemoMain.Perform(WM_VScroll, SB_BOTTOM, 0);

  StatusBar1.Panels[0].Text := tmpStr;
  if email then
    EmailerForSend.Add(tmpStr);
end;

procedure TFormDecklink.OnLogMessage(var Msg: TMessage);
begin
  if DSCapturer.ForLog.Count > 0 then
  begin
    StatusBar1.Panels[0].Text := DSCapturer.ForLog.Strings
      [DSCapturer.ForLog.Count - 1];

    ForLog.AddStrings(DSCapturer.ForLog);
    MemoMain.Lines.AddStrings(DSCapturer.ForLog);
    DSCapturer.ForLog.Clear;

    while MemoMain.Lines.Count > 100 do
      MemoMain.Lines.Delete(0);

    MemoMain.Perform(WM_VScroll, SB_BOTTOM, 0);
  end;
end;

procedure TFormDecklink.FlushLog;
var
  F1: TextFile;
  LFN: string;
begin
  if ForLog.Count > 0 then
  begin
    LFN := extractfilepath(paramstr(0)) + 'control.log';
    if FileExists(LFN) then
    begin
      AssignFile(F1, LFN);
      Append(F1);
    end
    else
    begin
      AssignFile(F1, LFN);
      Rewrite(F1);
    end;

    while ForLog.Count > 0 do
    begin
      Writeln(F1, ForLog.Strings[0]);
      ForLog.Delete(0);
    end;

    Flush(F1);
    CloseFile(F1);
  end;
end;

procedure TFormDecklink.FormCreate(Sender: TObject);
var
  i: Integer;
  ColorSystem: string;

  ini: TIniFile;
  tmpStr: string;
  tmpbool: Boolean;
  AudioSampleSize: LongWord;
  VideoSampleSize: Integer;

  VideoMediaType: TAMMediaType;
  AudioMediaType: TAMMediaType;
begin
  ForLog := TStringList.Create();

  WriteToLog('Начали');
  MemoMain.Clear;

  DSCapturer := TDSCapturer.Create(Self);
  AviWriter := TAviWriter.Create(Self);

  // ini read

  IniFileName := extractfilepath(paramstr(0)) + paramstr(1);
  if not FileExists(IniFileName) then
    IniFileName := extractfilepath(paramstr(0)) + 'setup.ini';

  ini := TIniFile.Create(IniFileName);
  try
    i := GetFileVersion(Application.ExeName);
    tmpStr := ini.Readstring('common', 'id', '');
    Self.Caption := 'Захват видео с Decklink. V' +
      format('%d.%d', [i div $10000, i mod $10000]) + ' ' + tmpStr;

    // capture graph adjusting
    DSCapturer.CapWidth := ini.ReadInteger('common', 'capwidth', 720);
    DSCapturer.CapHeight := ini.ReadInteger('common', 'capheight', 576);

    DSCapturer.PreviewOn := ini.ReadBool('common', 'preview', false);

    AviWriter.BufferSize := ini.ReadInteger('common', 'file_buffer', 1048576);

    ColorSystem := ini.Readstring('common', 'color', 'YUY2');

    if ColorSystem = 'RGB24' then
      DSCapturer.NeededSubType := MEDIASUBTYPE_RGB24
    else
    begin
      if ColorSystem = 'UYVY' then
        DSCapturer.NeededSubType := MEDIASUBTYPE_UYVY
      else if ColorSystem = 'HDYC' then
        DSCapturer.NeededSubType :=
          StringToGuid('{43594448-0000-0010-8000-00AA00389B71}')
      else
        DSCapturer.NeededSubType := MEDIASUBTYPE_YUY2;
    end;

    DSCapturer.VideoIsWS := ini.ReadBool('common', 'ws', false);
    DSCapturer.FieldCorrectionActive :=
      ini.ReadBool('common', 'field_correction', true);
    DSCapturer.FrameInterval := ini.ReadInteger('common',
      'frameinterval', 40000);

    tmpStr := ini.Readstring('common', 'coder_clsid', '');
    if tmpStr = '' then
      tmpStr := ini.Readstring('common', 'ffdshow_clsid', '');

    DSCapturer.ffdshow_CLSID_string := tmpStr;

    DSCapturer.card_no := ini.ReadInteger('common', 'card_no', -1);
    if (DSCapturer.card_no < 1) or (DSCapturer.card_no > 8) then
      DSCapturer.card_no := 1;

    DSCapturer.NodataTO := ini.ReadInteger('timeouts', 'nodata', 5);
    DSCapturer.FrameTO := ini.ReadInteger('timeouts', 'frame', 2);

    VideoSampleSize := ini.ReadInteger('common', 'frame_buffer_size', 0);

    Buff_Count := ini.ReadInteger('common', 'buf_size', 10);

    DSCapturer.AudioIsMultichannel := false;
    for i := 0 to 7 do
    begin
      if ini.ReadBool('wav', format('ch%2.2d%2.2d', [i * 2 + 1, i * 2 + 2]),
        false) then
      begin
        DSCapturer.AudioIsMultichannel := true;
        break;
      end;
    end;

    GroupBoxAudio.Visible := DSCapturer.AudioIsMultichannel;

    if DSCapturer.AudioIsMultichannel then
      AudioSampleSize := 48 * 2 * 16 * DSCapturer.FrameInterval div 1000
    else
      AudioSampleSize := 48 * 2 * 2 * DSCapturer.FrameInterval div 1000;

    DSCapturer.PrepareBuffers(Buff_Count, VideoSampleSize, AudioSampleSize);

    DSCapturer.VideoPanel := Panel1;

    FilePath := IncludeTrailingPathDelimiter(ini.Readstring('filename',
      'path', ''));
    if not DirectoryExists(FilePath) then
    begin
      ShowMessage('Каталог ' + FilePath + ' не существует');
      Application.Terminate;
    end;

    EditPrefix.Text := ini.Readstring('filename', 'prefix', 'filedv');
    FileTimeSource := ini.ReadInteger('filename', 'source', 0);
    FileUseDate := ini.ReadBool('filename', 'use_date', true);

    // common parameters
    Period := ini.ReadInteger('common', 'period', 7500);
    ProgressBarFile.Max := Period;

    Preload := ini.ReadInteger('common', 'preload', 5);

    RecordTO := ini.ReadInteger('timeouts', 'record', 40);

    for i := 0 to 7 do
    begin
      tmpStr := format('%2.2d%2.2d', [i * 2 + 1, i * 2 + 2]);
      tmpbool := ini.ReadBool('wav', 'ch' + tmpStr, false);
      AviWriter.SetWavData(i, tmpbool);
      if tmpbool then
        case i of
          0:
            CheckBox0102.Checked := true;
          1:
            CheckBox0304.Checked := true;
          2:
            CheckBox0506.Checked := true;
          3:
            CheckBox0708.Checked := true;
          4:
            CheckBox0910.Checked := true;
          5:
            CheckBox1112.Checked := true;
          6:
            CheckBox1314.Checked := true;
          7:
            CheckBox1516.Checked := true;
        end;
    end;

    ProgressBarVideoBuf.Max := DSCapturer.BuffCount;
    ProgressBarAudioBuf.Max := DSCapturer.BuffCount;

    //
    if ini.ReadBool('common', 'hide_close', false) then
    begin
      FormDecklink.BorderIcons := [];
    end;

    if ini.ReadBool('common', 'compact_mode', false) then
    begin
      Self.height := Self.height - MemoMain.height - 8;
      MemoMain.Visible := false;
      MemoMain.Enabled := false;
    end;

    ControlPanelVisible := ini.ReadBool('common', 'control', false);
    if ControlPanelVisible then
    begin
      if ini.ReadBool('common', 'autostart', true) then
        ButtonStartStop.Tag := 1
      else
        ButtonStartStop.Tag := 0;
    end
    else
    begin
      PanelControl.Visible := false;
      PanelControl.Enabled := false;
      ButtonEmail.Visible := false;
      ButtonEmail.Enabled := false;
      ButtonRestart.Visible := false;
      ButtonRestart.Enabled := false;
      Self.height := Self.height - PanelControl.height - 8;
      MemoMain.Top := PanelControl.Top;
      ButtonStartStop.Tag := 1
    end;

    if ButtonStartStop.Tag = 1 then
      ButtonStartStop.Caption := 'STOP'
    else
      ButtonStartStop.Caption := 'RECORD';

    Self.Left := ini.ReadInteger('common', 'left', 300);
    Self.Top := ini.ReadInteger('common', 'top', 100);

    i := 1;
    while ini.ValueExists('copier', 'path' + inttostr(i)) do
    begin
      tmpStr := IncludeTrailingPathDelimiter(ini.Readstring('copier',
        'path' + inttostr(i), ''));
      if not DirectoryExists(tmpStr) then
      begin
        ShowMessage('Каталог ' + tmpStr + ' не существует');
        Application.Terminate;
      end;
      inc(i);
    end;
    WillUseCopier := i > 1;

    DeleteSource := ini.ReadBool('copier', 'delete_source', false);
    CopierThreadActive := false;

    EmailerThreadActive := false;
    EmailerForSend := TStringList.Create;
    EmailerForSend.Clear;
    EmailerLastSend := 0;

  finally
    ini.Free;
  end;

  FilesToCopy := TStringList.Create;
  tmpStr := extractfilepath(paramstr(0)) + 'fileslist.txt';
  if FileExists(tmpStr) then
    FilesToCopy.LoadFromFile(tmpStr);

  DSCapturer.CreateCaptureGraph;

  if DSCapturer.GetVideoMediaType(VideoMediaType) then
  begin
    // MemoMain.Lines.Append(GUIDtostring(VideoMediaType.formatType));
    AviWriter.FillVideoHeaders(VideoMediaType);
  end;

  if DSCapturer.GetAudioMediaType(AudioMediaType) then
  begin
    // MemoMain.Lines.Append(GUIDtostring(AudioMediaType.formatType));
    AviWriter.FillAudioHeaders(AudioMediaType);
  end;

  DSCapturer.RunCaptureGraph;

  TimerIsActive := false;
  TimerMain.Interval := 200;
  TimerMain.Enabled := true;
end;

procedure TFormDecklink.FormDestroy(Sender: TObject);
var
  ini: TIniFile;
  tmpStr: string;

begin
  DSCapturer.DestroyCaptureGraph;

  if CopierThreadActive then
  begin
    while not CopierThread.WasFinished do
      Application.ProcessMessages;
  end;

  ini := TIniFile.Create(IniFileName);
  try
    ini.WriteInteger('common', 'left', FormDecklink.Left);
    ini.WriteInteger('common', 'top', FormDecklink.Top);
    ini.WriteInteger('common', 'frame_buffer_size',
      DSCapturer.GetLargestBufferSize);
    ini.Writestring('filename', 'prefix', EditPrefix.Text);
  finally
    ini.Free;
  end;

  tmpStr := extractfilepath(paramstr(0)) + 'fileslist.txt';

  if FilesToCopy.Count > 0 then
    FilesToCopy.SaveToFile(tmpStr)
  else if FileExists(tmpStr) then
    System.SysUtils.DeleteFile(tmpStr);

  WriteToLog('Закончили');
  FlushLog();

  FilesToCopy.Free;
  EmailerForSend.Free;

  AviWriter.Free;

  DSCapturer.Free;

  ForLog.Free;
end;

procedure TFormDecklink.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  WasCloseRequest := true;

  DSCapturer.CloseRequest;

  if CopierThreadActive then
    CopierThread.CancelRequest := true;

  CanClose := not AviWriter.FileIsOpen;
end;

procedure TFormDecklink.TimerMainTimer(Sender: TObject);
const
  Frames24h = 24 * 60 * 60 * 25;
var
  FrInBuffV, FrInBuffA: Integer;
  i: Integer;
  tmpStr: string;
  FileName: string;
  iHours, iMinutes, iSeconds, itmp: Integer;

  RecStart: TDateTime;
  RecDur: Integer;

  tc_str, date_str: AnsiString;
  setted_tc: AnsiString;
  audio_ts, video_ts: Int64;

  CurrentVideoFrame: POneVideoFrame;
  CurrentAudioFrame: POneAudioFrame;
begin
  FlushLog();

  if WasCloseRequest then
  begin
    CloseAviFile();
    Application.Terminate;
  end;

  if TimerIsActive then
    Exit;
  TimerIsActive := true;

  // frame processing
  repeat
    // calculation fillings of buffers
    FrInBuffV := DSCapturer.GetVideoFramesInBuffer;
    CurrentVideoFrame := DSCapturer.GetFirstVideoFrame;

    if Assigned(CurrentVideoFrame) then
      LastStamp := CurrentVideoFrame^.Stamp;

    FrInBuffA := DSCapturer.GetAudioFramesInBuffer;
    CurrentAudioFrame := DSCapturer.GetFirstAudioFrame;

    // indication
    LabelVideoFrInBuf.Caption :=
      format('%4d/%4d', [FrInBuffV, DSCapturer.BuffCount]);
    ProgressBarVideoBuf.Position := FrInBuffV;

    LabelAudioFrInBuf.Caption :=
      format('%4d/%4d', [FrInBuffA, DSCapturer.BuffCount]);
    ProgressBarAudioBuf.Position := FrInBuffA;

    Application.ProcessMessages;

    if WasCloseRequest then
      break;

    // conditions checks
    if (FrInBuffV < Preload) or (FrInBuffA < Preload) then
      break;

    // main processing
    if Assigned(CurrentVideoFrame) and Assigned(CurrentAudioFrame) then
    begin
      if ButtonStartStop.Tag = 1 then
      begin
        // writing frame data
        video_ts := CurrentVideoFrame^.Stamp;
        audio_ts := CurrentAudioFrame^.Stamp;

        if audio_ts = video_ts then
        begin
          // file is not open - we must create new file
          if not AviWriter.FileIsOpen then
          begin
            // new file name processing
            tc_str := TCtoString(trunc(TimeOf(CurrentVideoFrame^.Time) *
              Frames24h));
            tc_str := System.AnsiStrings.StringReplace(tc_str, ':', '.',
              [rfReplaceAll]);
            date_str := AnsiString(FormatDateTime('yyyy-mm-dd ',
              CurrentVideoFrame^.Time));

            if FileUseDate then
              FileName := EditPrefix.Text + String(date_str + tc_str)
            else
              FileName := EditPrefix.Text + String(tc_str);

            tmpStr := FilePath + FileName + '.tmp';
            if FileExists(tmpStr) then
            begin
              i := 1;
              while true do
              begin
                tmpStr := FilePath + FileName + '_' + format('%4.4d', [i]
                  ) + '.tmp';
                if FileExists(tmpStr) then
                  inc(i)
                else
                  break;
              end; // while
            end; // if file exists
            AviWriter.CreateAviFile(tmpStr);
            LabelFileName.Caption := tmpStr;
          end;

          if not AviWriter.FileIsOpen then
            break;

          if AviWriter.FramesInFile = 0 then
          begin //
            FileTC := trunc(TimeOf(CurrentVideoFrame^.Time) * Frames24h);
          end;

          // tc processing
          setted_tc := TCtoString(FileTC);
          if DSCapturer.DVCapturing then
          begin
            if CurrentVideoFrame^.SampleSize = 144000 then
              SetTCSpecial(setted_tc, VideoIsWS, CurrentVideoFrame^.Buffer)
            else
              WriteToLog('Формат захвата DV, но кадр размером ' +
                inttostr(CurrentVideoFrame^.SampleSize) + ' байт');
          end;
          LabelTC.Caption := String(setted_tc);

          RecStart := Now();
          AviWriter.WriteAVframe(CurrentVideoFrame^.Buffer,
            CurrentVideoFrame^.SampleSize, CurrentAudioFrame^.Buffer,
            CurrentAudioFrame^.SampleSize);
          RecDur := Millisecondsbetween(Now(), RecStart);
          if RecDur > RecordTO then
            WriteToLog('На запись одного кадра ушло ' +
              inttostr(RecDur) + ' мс');

          CurrentVideoFrame^.Filled := false;
          CurrentVideoFrame^.Used := false;
          CurrentAudioFrame^.Filled := false;
          CurrentAudioFrame^.Used := false;

          // total data change
          inc(FileTC);
        end
        else
        begin
          // if timestamps is not equal
          if (Abs(audio_ts - video_ts) mod (DSCapturer.FrameInterval * 10)) = 0
          then
          begin
            if audio_ts > video_ts then
            begin
              // skip one video frame
              WriteToLog('Skip one video frame: audio ts=' +
                inttostr(audio_ts div 10000) + 'ms, video ts=' +
                inttostr(video_ts div 10000) + 'ms');

              CurrentVideoFrame^.Filled := false;
              CurrentVideoFrame^.Used := false;
            end
            else
            begin
              // skip one audio frame
              WriteToLog('Skip one audio frame: audio ts=' +
                inttostr(audio_ts div 10000) + 'ms, video ts=' +
                inttostr(video_ts div 10000) + 'ms');

              CurrentAudioFrame^.Filled := false;
              CurrentAudioFrame^.Used := false;
            end;
          end
          else
          begin
            WriteToLog('Record restart: audio ts=' +
              inttostr(audio_ts div 10000) + 'ms, video ts=' +
              inttostr(video_ts div 10000) + 'ms');

            DSCapturer.StopCaptureGraph;

            CloseAviFile;

            DSCapturer.RunCaptureGraph;

            break;
          end;

        end; // audio_ts = video_ts

        if AviWriter.FramesInFile >= Period then
          CloseAviFile;
      end
      else
      begin
        if AviWriter.FileIsOpen then
          CloseAviFile;

        // discard current frame
        CurrentVideoFrame^.Filled := false;
        CurrentVideoFrame^.Used := false;
        CurrentAudioFrame^.Filled := false;
        CurrentAudioFrame^.Used := false;
      end;
    end;
    Application.ProcessMessages;
  until (FrInBuffV <= 0) or (FrInBuffA <= 0) or WasCloseRequest;

  if WasCloseRequest then
    Exit;

  if WillUseCopier then
  begin
    if CopierThreadActive then
    begin
      if CopierThread.WasFinished then
      begin
        if CopierThread.WasError then
        begin
          for i := 0 to CopierThread.ErrorStrings.Count - 1 do
            WriteToLog(CopierThread.ErrorStrings.Strings[i]);
          StatusBar1.Panels[0].Text := 'Файл ' +
            ExtractFileName(CopierThread.InFileName) + ' скопирован с ошибками';
        end
        else
        begin
          StatusBar1.Panels[0].Text := 'Файл ' +
            ExtractFileName(CopierThread.InFileName) + ' скопирован успешно';
        end;

        StatusBar1.Panels[1].Text := '';

        if CopierThread.AtLeastOneCopied and DeleteSource then
          System.SysUtils.DeleteFile(CopierThread.InFileName);
        i := FilesToCopy.IndexOf(CopierThread.InFileName);
        if i >= 0 then
          FilesToCopy.Delete(i);

        CopierThread.Free;
        CopierThreadActive := false;
      end
      else
      begin
        StatusBar1.Panels[1].Text := inttostr(CopierThread.Percent) + '%';
      end;
    end
    else
    begin
      if FilesToCopy.Count > 0 then
      begin
        if FileExists(FilesToCopy.Strings[0]) then
        begin

          CopierThread := TCopierThread.Create(true);
          CopierThread.Priority := tpLower;
          CopierThread.FreeOnTerminate := false;

          CopierThread.InFileName := FilesToCopy.Strings[0];
          CopierThread.IniFileName := IniFileName;
          CopierThread.ErrorStrings := TStringList.Create;

          CopierThread.Start;

          CopierThreadActive := true;
          StatusBar1.Panels[0].Text := 'Копирую файл ' +
            ExtractFileName(CopierThread.InFileName);
        end
        else
        begin
          FilesToCopy.Delete(0);
        end;
      end;
    end;
  end;

  if EmailerThreadActive then
  begin
    if Emailer.Finished then
    begin
      if Emailer.WasError then
        WriteToLog(Emailer.ErrorMessage);
      Emailer.Free;
      EmailerThreadActive := false;
    end;
  end
  else
  begin
    if (EmailerForSend.Count > 0) and
      (MinutesBetween(Now(), EmailerLastSend) > 30) then
    begin
      Emailer := Temailer.Create(true);
      Emailer.Priority := tpLower;
      Emailer.FreeOnTerminate := false;

      Emailer.body := TStringList.Create;
      Emailer.body.AddStrings(EmailerForSend);
      EmailerForSend.Clear;

      Emailer.Start;
      EmailerThreadActive := true;
      EmailerLastSend := Now();
    end;
  end;

  if DSCapturer.CheckForTimeouts then
  begin
    WriteToLog('Полная пересборка графа по таймауту');
    CloseAviFile;
    DSCapturer.ColdRestart;
  end;

  // indication
  itmp := LastStamp div 10000000; // in seconds
  iSeconds := itmp mod 60;
  itmp := itmp div 60;
  iMinutes := itmp mod 60;
  iHours := itmp div 60;
  LabelTotal.Caption := format('%7d:%2.2d:%2.2d', [iHours, iMinutes, iSeconds]);

  if ButtonStartStop.Tag = 1 then
  begin
    LabelFileFrames.Caption := format('%5d/%5d',
      [AviWriter.FramesInFile, Period]);
    ProgressBarFile.Position := AviWriter.FramesInFile;
    ProgressBarFile.Visible := true;
  end
  else
  begin
    LabelFileFrames.Caption := '';
    ProgressBarFile.Position := 0;
    ProgressBarFile.Visible := false;
  end;

  TimerIsActive := false;
end;

procedure TFormDecklink.ButtonEmailClick(Sender: TObject);
begin
  EmailerForSend.Add('Test message');
end;

procedure TFormDecklink.ButtonRestartClick(Sender: TObject);
begin
  CloseAviFile;

  DSCapturer.ColdRestart;
end;

procedure TFormDecklink.ButtonStartStopClick(Sender: TObject);
begin
  if ButtonStartStop.Tag = 1 then
  begin
    ButtonStartStop.Caption := 'RECORD';
    ButtonStartStop.Tag := 0;
  end
  else
  begin
    ButtonStartStop.Caption := 'STOP';
    ButtonStartStop.Tag := 1;
  end;
end;

procedure TFormDecklink.CloseAviFile;
begin
  AviWriter.CloseAviFile;
  LabelFileName.Caption := '';

  FilesToCopy.AddStrings(AviWriter.FilesToCopy);
  AviWriter.FilesToCopy.Clear;
end;

function TFormDecklink.TCtoString(inTC: longint): AnsiString;
var
  iHours, iMinutes, iSeconds, iFrames, tmp: longint;
begin
  tmp := inTC;
  iHours := tmp div 90000;
  tmp := tmp mod 90000;
  iMinutes := tmp div 1500;
  tmp := tmp mod 1500;
  iSeconds := tmp div 25;
  iFrames := tmp mod 25;
  result := System.AnsiStrings.format('%.2u', [iHours]) + ':' +
    System.AnsiStrings.format('%.2u', [iMinutes]) + ':' +
    System.AnsiStrings.format('%.2u', [iSeconds]) + ':' +
    System.AnsiStrings.format('%.2u', [iFrames]);
end;

function TFormDecklink.StringToBCD(instring: AnsiString): byte;
var
  upper, lower: AnsiChar;
begin
  upper := instring[1];
  lower := instring[2];
  StringToBCD := ((byte(lower) - byte('0')) mod 10) + 16 *
    ((byte(upper) - byte('0')) mod 10);
end;

procedure TFormDecklink.SetTCSpecial(TCin: AnsiString; IsWS: Boolean;
  OneFrame: PByte);
var
  offset: Int64;
  blocknum, ssybblock, vauxpack: Integer;
  i1: Integer;
  bframe, bsec, bmin, bhour: byte;
begin
  bframe := StringToBCD(System.AnsiStrings.MidStr(TCin, 10, 2));
  bsec := StringToBCD(System.AnsiStrings.MidStr(TCin, 7, 2));
  bmin := StringToBCD(System.AnsiStrings.MidStr(TCin, 4, 2));
  bhour := StringToBCD(System.AnsiStrings.LeftStr(TCin, 2));

  for blocknum := 0 to 1799 do
  begin
    if (OneFrame[blocknum * 80] div 32) = 1 then
    begin
      for ssybblock := 0 to 5 do
      begin
        offset := (blocknum * 80) + 3 + (ssybblock * 8);

        OneFrame[offset] := $80;
        OneFrame[offset + 1] := ssybblock + OneFrame[blocknum * 80 + 2] * 6;
        OneFrame[offset + 2] := $FF;
        if (ssybblock = 3) or (ssybblock = 5) then
        begin
          OneFrame[offset + 3] := $13;
          OneFrame[offset + 4] := bframe;
          OneFrame[offset + 5] := bsec;
          OneFrame[offset + 6] := bmin;
          OneFrame[offset + 7] := bhour;
        end
        else
        begin
          for i1 := 3 to 7 do
            OneFrame[offset + i1] := $FF;
        end;

      end;
    end;

    if (OneFrame[blocknum * 80] div 32) = 2 then
    begin
      for vauxpack := 0 to 14 do
      begin
        offset := (blocknum * 80) + 3 + (vauxpack * 5);
        if OneFrame[offset] = $61 then
        begin
          if IsWS then
            OneFrame[offset + 2] := (OneFrame[offset + 2] and $F8) or $02
          else
            OneFrame[offset + 2] := OneFrame[offset + 2] and $F8;
        end;
      end;
    end;
  end; // for
end;

end.
