unit Copier_Thread;

interface

uses
  System.Classes, System.SysUtils, System.DateUtils, System.IniFiles,
  Winapi.Windows;

const
  buffer_size = 131072;

type
  TCopierThread = class(TThread)
  private
    { Private declarations }
  public
    InFileName: string;
    IniFileName: string;
    ErrorStrings: TStringList;
    WasFinished: boolean;
    WasError: boolean;
    AtLeastOneCopied: boolean;
    CancelRequest: boolean;
    Percent: integer;
  protected
    procedure Execute; override;
  end;

implementation

{ CopierThread }

procedure TCopierThread.Execute;
var
  i: integer;
  InFileHandle: THandle;
  OutFileHandles: array of THandle;
  DataBuffer: PByte;
  ReadedBytes: integer;
  OutSize: Int64;
  CopyStartTime: TDateTime;
  MustTimeMs, ElapsedTimeMs: integer;
  tmpstr: string;

  InSize: Int64;
  total: Int64;

  Ini: TIniFile;
  OutPaths: TStringList;
  speedlimit: integer;

  InFileExt: string;
  ffmpegstring: string;
begin
  InFileHandle := INVALID_HANDLE_VALUE;

  DataBuffer := nil;
  WasError := true;
  AtLeastOneCopied := false;
  CancelRequest := false;

  InFileExt := UpperCase(Stringreplace(ExtractFileExt(InFileName), '.', '',
    [rfReplaceAll]));

  // ini read
  Ini := TIniFile.Create(IniFileName);
  try
    OutPaths := TStringList.Create;
    i := 1;
    while Ini.ValueExists('copier', 'path' + inttostr(i)) do
    begin
      tmpstr := IncludeTrailingPathDelimiter(Ini.Readstring('copier',
        'path' + inttostr(i), ''));
      OutPaths.Append(tmpstr);
      inc(i);
    end;

    speedlimit := Ini.ReadInteger('copier', 'speed', 0);
    ffmpegstring := Ini.Readstring('copier', InFileExt, '');
  finally
    Ini.Free;
  end;

  SetLength(OutFileHandles, OutPaths.Count);
  for i := 0 to Length(OutFileHandles) - 1 do
    OutFileHandles[i] := INVALID_HANDLE_VALUE;

  try
    if not FileExists(InFileName) then
      raise Exception.Create('Input file does not exists ' + InFileName);

    InFileHandle := Winapi.Windows.CreateFile(PChar(InFileName), GENERIC_READ,
      FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL or
      FILE_FLAG_SEQUENTIAL_SCAN, 0);
    if InFileHandle = INVALID_HANDLE_VALUE then
      raise Exception.Create('Can''t open input file ' + InFileName);

    InSize := FileSeek(InFileHandle, Int64(0), 2);

    FileSeek(InFileHandle, Int64(0), 0);

    for i := 0 to OutPaths.Count - 1 do
    begin
      tmpstr := OutPaths.Strings[i] +
        ChangeFileExt(ExtractFileName(InFileName), '.tmp');
      if FileExists(tmpstr) then
      begin
        if not System.SysUtils.DeleteFile(tmpstr) then
          ErrorStrings.Append('Output file exists and can''t be deleted '
            + tmpstr);
      end;

      if not FileExists(tmpstr) then
      begin
        OutFileHandles[i] := Winapi.Windows.CreateFile(PChar(tmpstr),
          GENERIC_WRITE, FILE_SHARE_READ, nil, CREATE_ALWAYS,
          FILE_ATTRIBUTE_NORMAL or FILE_FLAG_WRITE_THROUGH or
          FILE_FLAG_SEQUENTIAL_SCAN, 0);

        if OutFileHandles[i] = INVALID_HANDLE_VALUE then
          ErrorStrings.Append('Can''t create output file ' + tmpstr);
      end;
    end;

    DataBuffer := GetMemory(buffer_size);

    CopyStartTime := Now();
    total := 0;
    repeat
      ReadedBytes := FileRead(InFileHandle, DataBuffer^, buffer_size);
      for i := 0 to Length(OutFileHandles) - 1 do
        if OutFileHandles[i] <> INVALID_HANDLE_VALUE then
          FileWrite(OutFileHandles[i], DataBuffer^, ReadedBytes);

      inc(total, ReadedBytes);

      if InSize > 0 then
        Percent := total * 100 div InSize
      else
        Percent := 0;

      ElapsedTimeMs := Millisecondsbetween(Now(), CopyStartTime);
      if (speedlimit > 0) and (ElapsedTimeMs > 1) then
      begin
        MustTimeMs := (total * 1000) div (speedlimit * 1048576);
        if MustTimeMs > ElapsedTimeMs then
          Sleep(MustTimeMs - ElapsedTimeMs);
      end;
    until (ReadedBytes < buffer_size) or CancelRequest;

    if not CancelRequest then
    begin
      WasError := false;
      for i := 0 to Length(OutFileHandles) - 1 do
      begin
        OutSize := FileSeek(OutFileHandles[i], Int64(0), 2);
        if OutSize <> InSize then
        begin
          ErrorStrings.Append('Не удалось скопировать весь файл в ' +
            OutPaths.Strings[i]);
          WasError := true;
        end
        else
          AtLeastOneCopied := true;
      end;
    end;
  except
    on E: Exception do
    begin
      ErrorStrings.Append(E.ClassName + ': ' + E.Message);
    end;
  end;

  if InFileHandle <> INVALID_HANDLE_VALUE then
    FileClose(InFileHandle);

  for i := 0 to Length(OutFileHandles) - 1 do
  begin
    if OutFileHandles[i] <> INVALID_HANDLE_VALUE then
      FileClose(OutFileHandles[i]);

    if CancelRequest then
    begin
      tmpstr := OutPaths.Strings[i] +
        ChangeFileExt(ExtractFileName(InFileName), '.tmp');
      if FileExists(tmpstr) then
        System.SysUtils.DeleteFile(tmpstr);
    end
    else
    begin
      tmpstr := OutPaths.Strings[i] + ExtractFileName(InFileName);
      if FileExists(tmpstr) then
      begin
        if not System.SysUtils.DeleteFile(tmpstr) then
          ErrorStrings.Append('Output file exists and can''t be deleted '
            + tmpstr);
      end;

      if not RenameFile(ChangeFileExt(tmpstr, '.tmp'), tmpstr) then
        ErrorStrings.Append('Can''t rename file ' + tmpstr);
    end;
  end;

  if Assigned(DataBuffer) then
    FreeMemory(DataBuffer);

  WasFinished := true;
end;

end.
