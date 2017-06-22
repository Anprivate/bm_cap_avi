unit Unit_AVIwriting;

interface

uses
  System.SysUtils, System.Classes, WinApi.MMsystem, WinApi.Windows,
  WinApi.DirectShow9, Unit_IO_ops;

type
  T4bytes = array [0 .. 3] of byte;

  TAVIAudioFormat = record
    cbSize: LongWord; { the count in bytes of the size of }
    wFormatTag: Word; { format type }
    nChannels: Word; { number of channels (i.e. mono, stereo, etc.) }
    nSamplesPerSec: DWORD; { sample rate }
    nAvgBytesPerSec: DWORD; { for buffer estimation }
    nBlockAlign: Word; { block size of data }
    wBitsPerSample: Word; { number of bits per sample of mono data }
  end;

  TWavHeader = record
    chunkId: FOURCC; // RIFF
    chunkSize: LongWord; // filesize - 8;
    format: FOURCC; // WAVE
    subchunk1Id: FOURCC; // "fmt "
    subchunk1Size: LongWord; // =16
    audioFormat: Word; // PCM=1
    numChannels: Word; // 2
    sampleRate: LongWord; // 48000
    byteRate: LongWord; // bytes per second
    blockAlign: Word; // 4
    bitsPerSample: Word; // 16
    subchunk2Id: FOURCC; // data
    subchunk2Size: LongWord; // audio data size in bytes
  end;

  PLongWordArray = ^TLongWordArray;
  TLongWordArray = array [0 .. 8191] of LongWord;

  TWavData = record
    InUse: Boolean;
    id: string;
    FileHandle: THandle;
    Buffer: PLongWordArray;
    BufferSize: LongWord;
  end;

  TAVindex = record
    Vposition: Int64;
    Vsize: Uint32;
    Aposition: Int64;
    Asize: Uint32;
  end;

  TSuspendedRecord = record
    position: Int64;
    data: LongWord;
  end;

  TAviWriter = class(TComponent)
  private
    CurrentChunkSizePosition: Int64;
    SuspendedRecord: array of TSuspendedRecord;
    //
    LocalFramesInFile: integer;
    FramesInFirstChunk: LongWord;
    current_chunk_number: integer;
    start_of_current_chunk: Int64;
    audio_real_buffer_size: Int64;
    AVIndex: array of TAVindex;
    WavData: array [0 .. 7] of TWavData;
    Audio_is_multichannel: Boolean;
    // headers
    avih: TAVIMainHeader;
    wavh: TWavHeader;
    strh_video, strh_audio: TAVIStreamHeader;
    strf_video: TBitmapInfoHeader;
    strf_audio: TAVIAudioFormat;
    MaxVideoSampleSize, MaxAudioSampleSize: Cardinal;
    //
    IO_ops: TIO_ops;
    function str_to_4b(input: Ansistring): T4bytes;
    procedure AddSuspended(position: Int64; data: LongWord);
    //
    procedure FinishChunk0();
    procedure FinishChunkNot0();
    procedure StartNextChunk();
    procedure SetBufferSize(insize: Int64);
  public
    OutFileName: string;
    FilesToCopy: TStringList;
    property BufferSize: Int64 write SetBufferSize;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property FramesInFile: integer read LocalFramesInFile;

    procedure SetWavData(i: integer; InUse: Boolean);
    //
    function FillVideoHeaders(VideoMediaType: TAMMediaType): Boolean;
    function FillAudioHeaders(AudioMediaType: TAMMediaType): Boolean;
    //
    function AudioIsMultichannel: Boolean;
    function FileIsOpen: Boolean;
    //
    procedure CreateAviFile(FileName: string);
    procedure WriteAVframe(VBuffer: pByte; VBufferSize: Cardinal;
      ABuffer: pByte; ABufferSize: Cardinal);
    procedure CloseAviFile();
  end;
  //

implementation

{ TAviWriter }

constructor TAviWriter.Create(AOwner: TComponent);
var
  i: integer;
begin
  inherited;

  // initializing here
  IO_ops := TIO_ops.Create(Self);
  IO_ops.OutFileHandle := INVALID_HANDLE_VALUE;

  FilesToCopy := TStringList.Create;

  // avih header pre filling
  avih.fcc := ckidMAINAVIHEADER; // avih
  avih.cb := sizeof(TAVIMainHeader) - 8;
  avih.dwMicroSecPerFrame := 40000;
  avih.dwMaxBytesPerSec := 3792969; // ???
  avih.dwPaddingGranularity := 0;
  avih.dwFlags := AVIF_ISINTERLEAVED or AVIF_HASINDEX;
  avih.dwInitialFrames := 0;
  avih.dwStreams := 2;
  avih.dwSuggestedBufferSize := 0;
  avih.dwWidth := 720;
  avih.dwHeight := 576;
  for i := 0 to 3 do
    avih.dwReserved[i] := 0;

  // video stream header filling
  strh_video.fcc := ckidSTREAMHEADER; // strh
  strh_video.cb := sizeof(TAVIStreamHeader) - 8;
  strh_video.fccType := streamtypeVIDEO; // vids
  strh_video.fccHandler := 0;
  strh_video.dwFlags := 0;
  strh_video.wPriority := 0;
  strh_video.wLanguage := 0;
  strh_video.dwInitialFrames := 0;
  strh_video.dwScale := 1;
  strh_video.dwRate := 25;
  strh_video.dwStart := 0;
  strh_video.dwSuggestedBufferSize := 144000;
  strh_video.dwQuality := 0;
  strh_video.dwSampleSize := 0;
  strh_video.rcFrame.Left := 0;
  strh_video.rcFrame.Top := 0;
  strh_video.rcFrame.Right := 720;
  strh_video.rcFrame.Bottom := 576;

  // video stream format filling
  strf_video.biSize := sizeof(TBitmapInfoHeader);
  strf_video.biWidth := 720;
  strf_video.biHeight := 576;
  strf_video.biPlanes := 1;
  strf_video.biBitCount := 24;
  strf_video.biCompression := mmioStringToFOURCC('dvsd', 0);
  strf_video.biSizeImage := 144000;
  strf_video.biXPelsPerMeter := 0;
  strf_video.biYPelsPerMeter := 0;
  strf_video.biClrUsed := 0;
  strf_video.biClrImportant := 0;

  // audio stream header filling
  strh_audio.fcc := ckidSTREAMHEADER; // strh
  strh_audio.cb := sizeof(TAVIStreamHeader) - 8;
  strh_audio.fccType := streamtypeAUDIO; // auds
  strh_audio.fccHandler := 0;
  strh_audio.dwFlags := 0;
  strh_audio.wPriority := 0;
  strh_audio.wLanguage := 0;
  strh_audio.dwInitialFrames := 0;
  strh_audio.dwScale := 4;
  strh_audio.dwRate := 192000;
  strh_audio.dwStart := 0;
  strh_audio.dwSuggestedBufferSize := 9600;
  strh_audio.dwQuality := 10000;
  strh_audio.dwSampleSize := 4;
  strh_audio.rcFrame.Left := 0;
  strh_audio.rcFrame.Top := 0;
  strh_audio.rcFrame.Right := 0;
  strh_audio.rcFrame.Bottom := 0;

  // audio stream format filling
  strf_audio.cbSize := sizeof(TAVIAudioFormat) - 4;
  strf_audio.wFormatTag := WAVE_FORMAT_PCM; // PCM
  strf_audio.nChannels := 2;
  strf_audio.nSamplesPerSec := 48000;
  strf_audio.nAvgBytesPerSec := 192000;
  strf_audio.nBlockAlign := 4;
  strf_audio.wBitsPerSample := 16;
end;

destructor TAviWriter.Destroy;
var
  i: integer;
begin
  // desctruction here
  FilesToCopy.Free;
  IO_ops.Free;

  for i := 0 to 7 do
    if Assigned(WavData[i].Buffer) then
      FreeMem(WavData[i].Buffer);
  inherited;
end;

procedure TAviWriter.CreateAviFile(FileName: string);
var
  pTmp: pByte;
  tmp_lw: LongWord;
  i: integer;

  frame_id: T4bytes;
begin
  OutFileName := FileName;

  // IO_ops.OutFileHandle := FileCreate(OutFileName);

  IO_ops.OutFileHandle := WinApi.Windows.CreateFile(PChar(OutFileName),
    GENERIC_WRITE, FILE_SHARE_READ, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL or
    FILE_FLAG_SEQUENTIAL_SCAN, 0);

  // FILE_FLAG_WRITE_THROUGH or
  // IO_ops.BufferSize :=;
  // IO_ops.BufferedMode := true;

  // writing predata and positions
  pTmp := AllocMem($2800);
  frame_id := str_to_4b('RIFF');
  move(frame_id, pTmp^, 4);
  IO_ops.WriteData(pTmp^, $2800);
  FreeMemory(pTmp);

  // start chunk#0
  frame_id := str_to_4b('LIST');
  IO_ops.WriteData(frame_id, 4);

  // save position to write ChunkSize
  CurrentChunkSizePosition := IO_ops.GetCurrentPosition;
  tmp_lw := 0; // TMP SIZE!!!
  IO_ops.WriteData(tmp_lw, 4);

  frame_id := str_to_4b('movi');
  IO_ops.WriteData(frame_id, 4);

  // wav
  for i := 0 to 7 do
  begin
    if WavData[i].InUse then
    begin
      WavData[i].FileHandle :=
        FileCreate(ChangeFileExt(OutFileName, '_ch' + WavData[i].id + '.tmp'));
      FileWrite(WavData[i].FileHandle, wavh, sizeof(wavh));
    end;
  end;

  // special data
  SetLength(SuspendedRecord, 0);
  LocalFramesInFile := 0;
  current_chunk_number := 0;
  start_of_current_chunk := 0;
  MaxVideoSampleSize := 0;
  MaxAudioSampleSize := 0;
end;

function TAviWriter.FileIsOpen: Boolean;
begin
  Result := IO_ops.OutFileHandle <> INVALID_HANDLE_VALUE;
end;

procedure TAviWriter.FinishChunk0;
var
  ListMoviSize: Uint32;
  idx1size: Uint32;
  tmp_lw: Uint32;
  tmp_flags: T4bytes;
  i: Uint32;
  frame_id: T4bytes;
begin
  // write main chunk size to chunksizeposition
  ListMoviSize := IO_ops.GetCurrentPosition - CurrentChunkSizePosition - 4;
  AddSuspended(CurrentChunkSizePosition, ListMoviSize);

  // write main chunk index idx1
  tmp_lw := ckidAVIOLDINDEX; // idx1
  IO_ops.WriteData(tmp_lw, 4);

  idx1size := Length(AVIndex) * 32;
  IO_ops.WriteData(idx1size, 4);

  for i := 0 to 3 do
    tmp_flags[i] := 0;
  tmp_flags[0] := $10;

  for i := 0 to Length(AVIndex) - 1 do
  begin
    frame_id := str_to_4b('00dc');
    IO_ops.WriteData(frame_id, 4);

    IO_ops.WriteData(tmp_flags, 4);

    tmp_lw := AVIndex[i].Vposition - CurrentChunkSizePosition - 12;
    IO_ops.WriteData(tmp_lw, 4);

    IO_ops.WriteData(AVIndex[i].Vsize, 4);

    frame_id := str_to_4b('01wb');
    IO_ops.WriteData(frame_id, 4);

    IO_ops.WriteData(tmp_flags, 4);

    tmp_lw := AVIndex[i].Aposition - CurrentChunkSizePosition - 12;
    IO_ops.WriteData(tmp_lw, 4);

    IO_ops.WriteData(strh_audio.dwSuggestedBufferSize, 4);
  end;

  // size of full first chunk!
  tmp_lw := IO_ops.GetCurrentPosition - 8;
  AddSuspended(4, tmp_lw);

  FramesInFirstChunk := Length(AVIndex);
end;

procedure TAviWriter.FinishChunkNot0;
var
  tmp_64: Int64;
  tmp_lw: LongWord;
begin
  // finish current chunk (not #0)
  // current end of file
  tmp_64 := IO_ops.GetCurrentPosition;

  // to RIFF size
  tmp_lw := tmp_64 - CurrentChunkSizePosition + 8;
  AddSuspended(CurrentChunkSizePosition - 12, tmp_lw);

  // to LIST:movi size
  tmp_lw := tmp_64 - CurrentChunkSizePosition - 4;
  AddSuspended(CurrentChunkSizePosition, tmp_lw);
end;

procedure TAviWriter.SetBufferSize(insize: Int64);
begin
  IO_ops.BufferSize := insize;
  if insize > 0 then
    IO_ops.BufferedMode := true;
end;

procedure TAviWriter.SetWavData(i: integer; InUse: Boolean);
begin
  WavData[i].id := format('%2.2d%2.2d', [i * 2 + 1, i * 2 + 2]);
  WavData[i].InUse := InUse;
  WavData[i].FileHandle := INVALID_HANDLE_VALUE;
  WavData[i].Buffer := nil;
  WavData[i].BufferSize := 0;
  if InUse then
    Audio_is_multichannel := true;
end;

procedure TAviWriter.StartNextChunk;
var
  tmp_lw: Uint32;
  frame_id: T4bytes;
begin
  if current_chunk_number = 0 then
    FinishChunk0()
  else
    FinishChunkNot0();

  start_of_current_chunk := IO_ops.GetCurrentPosition;

  // start next chunk
  frame_id := str_to_4b('RIFF');
  IO_ops.WriteData(frame_id, 4);

  tmp_lw := 0; // TMP SIZE!!!
  IO_ops.WriteData(tmp_lw, 4);

  frame_id := str_to_4b('AVIX');
  IO_ops.WriteData(frame_id, 4);

  frame_id := str_to_4b('LIST');
  IO_ops.WriteData(frame_id, 4);

  // save position to write ChunkSize
  CurrentChunkSizePosition := IO_ops.GetCurrentPosition;
  tmp_lw := 0; // TMP SIZE!!!
  IO_ops.WriteData(tmp_lw, 4);

  frame_id := str_to_4b('movi');
  IO_ops.WriteData(frame_id, 4);
  //
  inc(current_chunk_number);
end;

procedure TAviWriter.AddSuspended(position: Int64; data: LongWord);
var
  i: integer;
begin
  i := Length(SuspendedRecord);
  SetLength(SuspendedRecord, i + 1);
  SuspendedRecord[i].position := position;
  SuspendedRecord[i].data := data;
end;

function TAviWriter.AudioIsMultichannel: Boolean;
begin
  Result := Audio_is_multichannel;
end;

procedure TAviWriter.CloseAviFile;
const
  indexes_per_chunk = 8192;
type
  TtmpIDX = record
    offset: Uint32;
    size: Uint32;
  end;

  Tsuperindex = record
    offset: Int64;
    chunkSize: LongWord;
    duration: LongWord;
  end;
var
  tmp_lw, cur_pos: LongWord;
  tmp_w: Word;
  tmp_b: byte;
  hdrl_s_pos, strl_v_s_pos, strl_a_s_pos: LongWord;
  long_offset: Int64;
  i, si_indx: LongWord;
  si_Video, si_Audio: array of Tsuperindex;
  samples_per_frame: LongWord;
  frame_id: T4bytes;
  tmpstr: string;
  tmpIndex: array [0 .. indexes_per_chunk - 1] of TtmpIDX;
  CurrIDX, InTmpIDX: integer;
  tmpInt64: Int64;
begin
  // file is not open?
  if IO_ops.OutFileHandle = INVALID_HANDLE_VALUE then
    exit;

  // file is too short (< 5 sec) - ignore and delete
  if Length(AVIndex) < 125 then
  begin
    FileClose(IO_ops.OutFileHandle);
    IO_ops.OutFileHandle := INVALID_HANDLE_VALUE;
    System.SysUtils.DeleteFile(OutFileName);

    for i := 0 to 7 do
    begin
      if WavData[i].InUse then
      begin
        if WavData[i].FileHandle <> INVALID_HANDLE_VALUE then
        begin
          // wav close
          FileClose(WavData[i].FileHandle);
          WavData[i].FileHandle := INVALID_HANDLE_VALUE;
          tmpstr := ChangeFileExt(OutFileName, '_ch' + WavData[i].id + '.tmp');
          System.SysUtils.DeleteFile(tmpstr);
        end;
      end;
    end; // for i

    exit;
  end;

  samples_per_frame := Int64(strf_audio.nSamplesPerSec) *
    Int64(strh_video.dwScale) div Int64(strh_video.dwRate);

  // write ix00 index (ODML standard index for video)
  CurrIDX := 0;
  si_indx := 0;

  repeat
    InTmpIDX := 0;

    long_offset := AVIndex[CurrIDX].Vposition;

    repeat
      tmpInt64 := AVIndex[CurrIDX].Vposition - long_offset;
      if tmpInt64 > High(Uint32) then
        break;

      tmpIndex[InTmpIDX].offset := tmpInt64;
      tmpIndex[InTmpIDX].size := AVIndex[CurrIDX].Vsize;

      inc(CurrIDX);
      inc(InTmpIDX);
    until (InTmpIDX >= indexes_per_chunk) or (CurrIDX >= Length(AVIndex));

    if InTmpIDX > 0 then
    begin
      // for superindex
      SetLength(si_Video, si_indx + 1);
      si_Video[si_indx].offset := IO_ops.GetCurrentPosition;
      si_Video[si_indx].chunkSize := (InTmpIDX * 8) + 32;
      si_Video[si_indx].duration := InTmpIDX;
      inc(si_indx);

      // main index
      frame_id := str_to_4b('ix00');
      IO_ops.WriteData(frame_id, 4);

      tmp_lw := (InTmpIDX * 8) + 24;
      IO_ops.WriteData(tmp_lw, 4); // chunk size

      tmp_w := 2;
      IO_ops.WriteData(tmp_w, 2); // longs per entry

      // index sub type
      tmp_b := AVI_INDEX_SUB_DEFAULT;
      IO_ops.WriteData(tmp_b, 1);

      // index type
      tmp_b := AVI_INDEX_OF_CHUNKS;
      IO_ops.WriteData(tmp_b, 1);

      // entries in use
      tmp_lw := InTmpIDX;
      IO_ops.WriteData(tmp_lw, 4);

      // chunk ID
      frame_id := str_to_4b('00dc');
      IO_ops.WriteData(frame_id, 4);

      // offset
      IO_ops.WriteData(long_offset, 8);

      tmp_lw := 0;
      IO_ops.WriteData(tmp_lw, 4); // reserved

      for i := 0 to InTmpIDX - 1 do
      begin
        IO_ops.WriteData(tmpIndex[i].offset, 4); // offset
        IO_ops.WriteData(tmpIndex[i].size, 4); // size
      end;
    end;
  until CurrIDX >= Length(AVIndex);

  // write ix01 index (ODML standard index for audio)
  CurrIDX := 0;
  si_indx := 0;

  repeat
    InTmpIDX := 0;

    long_offset := AVIndex[CurrIDX].Aposition;

    repeat
      tmpInt64 := AVIndex[CurrIDX].Aposition - long_offset;
      if tmpInt64 > High(Uint32) then
        break;

      tmpIndex[InTmpIDX].offset := tmpInt64;
      tmpIndex[InTmpIDX].size := strh_audio.dwSuggestedBufferSize;
      // AVIndex[CurrIDX].Asize;

      inc(CurrIDX);
      inc(InTmpIDX);
    until (InTmpIDX >= indexes_per_chunk) or (CurrIDX >= Length(AVIndex));

    if InTmpIDX > 0 then
    begin
      // for superindex
      SetLength(si_Audio, si_indx + 1);
      si_Audio[si_indx].offset := IO_ops.GetCurrentPosition;
      si_Audio[si_indx].chunkSize := (InTmpIDX * 8) + 32;
      si_Audio[si_indx].duration := InTmpIDX * samples_per_frame;
      inc(si_indx);

      // main index
      frame_id := str_to_4b('ix01');
      IO_ops.WriteData(frame_id, 4);

      tmp_lw := (InTmpIDX * 8) + 24;
      IO_ops.WriteData(tmp_lw, 4); // chunk size

      tmp_w := 2;
      IO_ops.WriteData(tmp_w, 2); // longs per entry

      // index sub type
      tmp_b := AVI_INDEX_SUB_DEFAULT;
      IO_ops.WriteData(tmp_b, 1);

      // index type
      tmp_b := AVI_INDEX_OF_CHUNKS;
      IO_ops.WriteData(tmp_b, 1);

      // entries in use
      tmp_lw := InTmpIDX;
      IO_ops.WriteData(tmp_lw, 4);

      // chunk ID
      frame_id := str_to_4b('01wb');
      IO_ops.WriteData(frame_id, 4);

      // offset
      IO_ops.WriteData(long_offset, 8);

      tmp_lw := 0;
      IO_ops.WriteData(tmp_lw, 4); // reserved

      for i := 0 to InTmpIDX - 1 do
      begin
        IO_ops.WriteData(tmpIndex[i].offset, 4); // offset
        IO_ops.WriteData(tmpIndex[i].size, 4); // size
      end;
    end;
  until CurrIDX >= Length(AVIndex);

  if current_chunk_number = 0 then
    FinishChunk0()
  else
    FinishChunkNot0();

  IO_ops.BufferedMode := false;

  for i := 0 to Length(SuspendedRecord) - 1 do
  begin
    IO_ops.SetPosition(SuspendedRecord[i].position);
    IO_ops.WriteData(SuspendedRecord[i].data, 4);
  end;

  IO_ops.SetPosition(8);

  frame_id := str_to_4b('AVI ');
  IO_ops.WriteData(frame_id, 4);

  frame_id := str_to_4b('LIST');
  IO_ops.WriteData(frame_id, 4);

  tmp_lw := 0;
  hdrl_s_pos := IO_ops.GetCurrentPosition;
  IO_ops.WriteData(tmp_lw, 4); // place for LIST header size

  frame_id := str_to_4b('hdrl');
  IO_ops.WriteData(frame_id, 4);

  strh_video.dwSuggestedBufferSize := MaxVideoSampleSize;
  strh_audio.dwSuggestedBufferSize := MaxAudioSampleSize;

  avih.dwMaxBytesPerSec := Int64(MaxVideoSampleSize + MaxAudioSampleSize + 50) *
    (1000000 div avih.dwMicroSecPerFrame);

  // avih header writing
  avih.dwTotalFrames := FramesInFirstChunk; // ONLY FOR FIRST CHUNK!!!
  IO_ops.WriteData(avih, sizeof(TAVIMainHeader));

  // video LIST:strh
  frame_id := str_to_4b('LIST');
  IO_ops.WriteData(frame_id, 4);

  // save current position
  strl_v_s_pos := IO_ops.GetCurrentPosition;
  IO_ops.WriteData(strl_v_s_pos, 4); // place for LIST header size

  tmp_lw := ckidSTREAMLIST; // strl
  IO_ops.WriteData(tmp_lw, 4);

  // video stream header writing
  strh_video.dwLength := Length(AVIndex); // FULL MOVIE VIDEO LENGTH!!!
  IO_ops.WriteData(strh_video, sizeof(TAVIStreamHeader));

  // video stream format writing
  tmp_lw := ckidSTREAMFORMAT; // strf
  IO_ops.WriteData(tmp_lw, 4);

  tmp_lw := sizeof(TBitmapInfoHeader);
  IO_ops.WriteData(tmp_lw, 4);
  IO_ops.WriteData(strf_video, sizeof(TBitmapInfoHeader));

  // writing video superindex
  frame_id := str_to_4b('indx');
  IO_ops.WriteData(frame_id, 4);

  tmp_lw := Length(si_Video) * 16 + 24; // indx size
  IO_ops.WriteData(tmp_lw, 4);

  tmp_w := 4;
  IO_ops.WriteData(tmp_w, 2);

  tmp_b := AVI_INDEX_SUB_DEFAULT;
  IO_ops.WriteData(tmp_b, 1); // index sub type

  tmp_b := AVI_INDEX_OF_INDEXES;
  IO_ops.WriteData(tmp_b, 1);

  // index type
  tmp_lw := Length(si_Video);
  IO_ops.WriteData(tmp_lw, 4);

  // entries in use
  frame_id := str_to_4b('00dc');
  IO_ops.WriteData(frame_id, 4);

  // chunk ID
  tmp_lw := 0;
  for i := 0 to 2 do
    IO_ops.WriteData(tmp_lw, 4);

  // reserved
  for i := 0 to Length(si_Video) - 1 do
  begin
    IO_ops.WriteData(si_Video[i], 16);
  end;

  // fill first LIST:strl size
  cur_pos := IO_ops.GetCurrentPosition; // save current position
  tmp_lw := cur_pos - strl_v_s_pos - 4;
  IO_ops.SetPosition(strl_v_s_pos);
  IO_ops.WriteData(tmp_lw, 4);

  // restore position
  IO_ops.SetPosition(cur_pos);

  // audio LIST:strh
  frame_id := str_to_4b('LIST');
  IO_ops.WriteData(frame_id, 4);

  // save current position
  strl_a_s_pos := IO_ops.GetCurrentPosition;
  IO_ops.WriteData(strl_a_s_pos, 4); // place for LIST header size

  frame_id := str_to_4b('strl');
  IO_ops.WriteData(frame_id, 4);

  // audio stream header writing
  strh_audio.dwLength := samples_per_frame * Length(AVIndex);
  // FULL MOVIE VIDEO LENGTH!!!
  IO_ops.WriteData(strh_audio, sizeof(TAVIStreamHeader));

  // audio stream format writing
  tmp_lw := ckidSTREAMFORMAT; // strf
  IO_ops.WriteData(tmp_lw, 4);
  IO_ops.WriteData(strf_audio, sizeof(TAVIAudioFormat));

  // writing audio superindex
  frame_id := str_to_4b('indx');
  IO_ops.WriteData(frame_id, 4);

  tmp_lw := Length(si_Audio) * 16 + 24; // indx size
  IO_ops.WriteData(tmp_lw, 4);

  tmp_w := 4;
  IO_ops.WriteData(tmp_w, 2);

  tmp_b := AVI_INDEX_SUB_DEFAULT;
  IO_ops.WriteData(tmp_b, 1); // index sub type

  tmp_b := AVI_INDEX_OF_INDEXES;
  IO_ops.WriteData(tmp_b, 1);

  // index type
  tmp_lw := Length(si_Audio);
  IO_ops.WriteData(tmp_lw, 4);

  // entries in use
  frame_id := str_to_4b('01wb');
  IO_ops.WriteData(frame_id, 4);

  // chunk ID
  tmp_lw := 0;
  for i := 0 to 2 do
    IO_ops.WriteData(tmp_lw, 4);

  // reserved
  for i := 0 to Length(si_Audio) - 1 do
    IO_ops.WriteData(si_Audio[i], 16);

  // fill second LIST:strl size
  cur_pos := IO_ops.GetCurrentPosition; // save current position
  tmp_lw := cur_pos - strl_a_s_pos - 4;
  IO_ops.SetPosition(strl_a_s_pos);
  IO_ops.WriteData(tmp_lw, 4);
  IO_ops.SetPosition(cur_pos);

  // LIST:odml
  frame_id := str_to_4b('LIST');
  IO_ops.WriteData(frame_id, 4);

  tmp_lw := 16;
  IO_ops.WriteData(tmp_lw, 4);

  frame_id := str_to_4b('odml');
  IO_ops.WriteData(frame_id, 4);

  frame_id := str_to_4b('dmlh');
  IO_ops.WriteData(frame_id, 4);

  tmp_lw := 4;
  IO_ops.WriteData(tmp_lw, 4);
  IO_ops.WriteData(strh_video.dwLength, 4); // full movie length

  // fill LIST:hdrl size
  cur_pos := IO_ops.GetCurrentPosition; // save current position
  tmp_lw := cur_pos - hdrl_s_pos - 4;
  IO_ops.SetPosition(hdrl_s_pos);
  IO_ops.WriteData(tmp_lw, 4);
  IO_ops.SetPosition(cur_pos);

  // junk filling
  frame_id := str_to_4b('JUNK');
  IO_ops.WriteData(frame_id, 4);
  cur_pos := IO_ops.GetCurrentPosition; // save current position
  tmp_lw := $2800 - cur_pos - 4;
  IO_ops.WriteData(tmp_lw, 4);

  //
  FileClose(IO_ops.OutFileHandle);
  IO_ops.OutFileHandle := INVALID_HANDLE_VALUE;
  RenameFile(OutFileName, ChangeFileExt(OutFileName, '.avi'));

  FilesToCopy.Append(ChangeFileExt(OutFileName, '.avi'));

  for i := 0 to 7 do
  begin
    if WavData[i].InUse then
    begin
      if WavData[i].FileHandle <> INVALID_HANDLE_VALUE then
      begin
        // wav header
        cur_pos := FileSeek(WavData[i].FileHandle, Int64(0), 2);
        // get size of file
        wavh.chunkSize := cur_pos - 8;
        wavh.subchunk2Size := cur_pos;
        FileSeek(WavData[i].FileHandle, Int64(0), 0); // to start
        FileWrite(WavData[i].FileHandle, wavh, sizeof(wavh));

        // wav close
        FileClose(WavData[i].FileHandle);
        WavData[i].FileHandle := INVALID_HANDLE_VALUE;

        tmpstr := ChangeFileExt(OutFileName, '_ch' + WavData[i].id + '.tmp');
        RenameFile(tmpstr, ChangeFileExt(tmpstr, '.wav'));
        FilesToCopy.Append(ChangeFileExt(tmpstr, '.wav'));
      end;
    end;
  end; // for i
end;

procedure TAviWriter.WriteAVframe(VBuffer: pByte; VBufferSize: Cardinal;
  ABuffer: pByte; ABufferSize: Cardinal);
var
  frame_id: T4bytes;
  tmp_lw: Cardinal;
  tmp_ptr: PLongWord;
  i, i1: integer;
begin
  if VBufferSize > MaxVideoSampleSize then
    MaxVideoSampleSize := VBufferSize;

  // writing video frame
  SetLength(AVIndex, FramesInFile + 1);

  frame_id := str_to_4b('00dc');
  IO_ops.WriteData(frame_id, 4);

  AVIndex[LocalFramesInFile].Vsize := VBufferSize;
  IO_ops.WriteData(VBufferSize, 4);

  AVIndex[FramesInFile].Vposition := IO_ops.GetCurrentPosition;

  IO_ops.WriteData(VBuffer^, VBufferSize);

  if Odd(VBufferSize) then
    IO_ops.WriteData(frame_id, 1);

  // writing audio frame
  frame_id := str_to_4b('01wb');
  IO_ops.WriteData(frame_id, 4);

  if Audio_is_multichannel then
  begin
    AVIndex[FramesInFile].Asize := ABufferSize div 8;
    tmp_lw := ABufferSize div 8;
    IO_ops.WriteData(tmp_lw, 4);

    AVIndex[FramesInFile].Aposition := IO_ops.GetCurrentPosition;

    for i := 0 to 7 do
    begin
      tmp_lw := ABufferSize div 8;
      if WavData[i].BufferSize < tmp_lw then
      begin
        if Assigned(WavData[i].Buffer) then
          FreeMemory(WavData[i].Buffer);
        WavData[i].Buffer := GetMemory(tmp_lw);
        WavData[i].BufferSize := tmp_lw;
      end;
    end;

    tmp_ptr := PLongWord(ABuffer);
    for i := 0 to (ABufferSize div 32) - 1 do
    begin
      for i1 := 0 to 7 do
      begin
        tmp_lw := tmp_ptr^;
        inc(tmp_ptr);
        WavData[i1].Buffer[i] := tmp_lw;
      end;
    end;

    tmp_lw := ABufferSize div 8;
    IO_ops.WriteData(WavData[0].Buffer^, tmp_lw);

    if tmp_lw > MaxAudioSampleSize then
      MaxAudioSampleSize := tmp_lw;

    for i := 0 to 7 do
    begin
      if WavData[i].InUse then
        FileWrite(WavData[i].FileHandle, WavData[i].Buffer^, ABufferSize div 8);
    end;
  end
  else
  begin
    // audio is not multichannel
    if ABufferSize > MaxAudioSampleSize then
      MaxAudioSampleSize := ABufferSize;

    AVIndex[FramesInFile].Asize := ABufferSize;
    IO_ops.WriteData(ABufferSize, 4);

    AVIndex[FramesInFile].Aposition := IO_ops.GetCurrentPosition;

    IO_ops.WriteData(ABuffer^, ABufferSize);
    if Odd(ABufferSize) then
      IO_ops.WriteData(frame_id, 1);
  end;

  if (IO_ops.GetCurrentPosition - start_of_current_chunk) > $3F000000 then
    StartNextChunk();

  inc(LocalFramesInFile);
end;

function TAviWriter.FillAudioHeaders(AudioMediaType: TAMMediaType): Boolean;
var
  AudioGuidFound: Boolean;
begin
  AudioGuidFound := false;
  if IsEqualGUID(AudioMediaType.formatType, FORMAT_WaveFormatEx) then
  begin
    with TWaveFormatEx(AudioMediaType.pbFormat^) do
    begin
      strh_audio.dwScale := 1; // nBlockAlign;
      strh_audio.dwRate := nSamplesPerSec; // nAvgBytesPerSec;
      strh_audio.dwSampleSize := 4; // nBlockAlign;

      strh_audio.dwSuggestedBufferSize := Int64(strh_audio.dwRate) *
        Int64(avih.dwMicroSecPerFrame) * Int64(strh_audio.dwSampleSize)
        div Int64(1000000);

      audio_real_buffer_size := Int64(nAvgBytesPerSec) *
        Int64(avih.dwMicroSecPerFrame) div Int64(1000000);

      // strf filling
      strf_audio.wFormatTag := wFormatTag; // PCM
      strf_audio.nChannels := 2; // nChannels;
      strf_audio.nSamplesPerSec := nSamplesPerSec;
      strf_audio.nAvgBytesPerSec := nSamplesPerSec * 2 * 2;
      // nAvgBytesPerSec;
      strf_audio.nBlockAlign := 4; // nBlockAlign;
      strf_audio.wBitsPerSample := wBitsPerSample;

      // wavh filling
      wavh.chunkId := mmioStringToFOURCC('RIFF', MMIO_TOUPPER);
      // RIFF
      wavh.chunkSize := 0; // filesize - 8;
      wavh.format := mmioStringToFOURCC('WAVE', MMIO_TOUPPER);
      // WAVE
      wavh.subchunk1Id := mmioStringToFOURCC('fmt ', 0); // "fmt "
      wavh.subchunk1Size := 16;
      wavh.audioFormat := 1; // PCM=1
      wavh.numChannels := 2;
      wavh.sampleRate := nSamplesPerSec; // 48000
      wavh.byteRate := nSamplesPerSec * 2 * 2; // bytes per second
      wavh.blockAlign := 4; // 4 ?
      wavh.bitsPerSample := wBitsPerSample; // 16
      wavh.subchunk2Id := mmioStringToFOURCC('data', 0); // data
      wavh.subchunk2Size := 0; // audio data size in bytes
    end;
    AudioGuidFound := true;
  end;

  avih.dwMaxBytesPerSec := Int64(strh_video.dwSuggestedBufferSize +
    strh_audio.dwSuggestedBufferSize + 50) *
    (1000000 div avih.dwMicroSecPerFrame);

  Result := AudioGuidFound;
end;

function TAviWriter.FillVideoHeaders(VideoMediaType: TAMMediaType): Boolean;
var
  VideoHeaderType: TVIDEOINFOHEADER;
  VideoHeaderTypeDV: TDVINFO;
  VideoGuidFound: Boolean;
begin
  VideoGuidFound := false;
  if IsEqualGUID(VideoMediaType.formatType, FORMAT_DvInfo) then
  begin
    move(VideoMediaType.pbFormat^, VideoHeaderTypeDV, sizeof(TDVINFO));
    if (VideoHeaderTypeDV.dwDVVAuxSrc and $00010000) = 0 then
    begin // dvsd
      strf_video.biCompression := mmioStringToFOURCC('dvsd', 0);
    end
    else
    begin
      strf_video.biCompression := mmioStringToFOURCC('dvsl', 0);
    end;
    strh_video.fccHandler := strf_video.biCompression;
    if (VideoHeaderTypeDV.dwDVVAuxSrc and $00200000) = 0 then
    begin // 60 fps
      avih.dwMicroSecPerFrame := 33367;
      avih.dwWidth := 720;
      avih.dwHeight := 480;
      strh_video.rcFrame.Right := 720;
      strh_video.rcFrame.Bottom := 480;
      strh_video.dwScale := 100;
      strh_video.dwRate := 2997;
      strh_video.dwSuggestedBufferSize := 120000;
      strf_video.biWidth := 720;
      strf_video.biHeight := 480;
      strf_video.biSizeImage := 120000;
    end
    else
    begin // 50 fps
      avih.dwMicroSecPerFrame := 40000;
      avih.dwWidth := 720;
      avih.dwHeight := 576;
      strh_video.rcFrame.Right := 720;
      strh_video.rcFrame.Bottom := 576;
      strh_video.dwScale := 1;
      strh_video.dwRate := 25;
      strh_video.dwSuggestedBufferSize := 144000;
      strf_video.biWidth := 720;
      strf_video.biHeight := 576;
      strf_video.biSizeImage := 144000;
    end;
    VideoGuidFound := true;
  end;

  if IsEqualGUID(VideoMediaType.formatType, FORMAT_VideoInfo) then
  begin
    move(VideoMediaType.pbFormat^, VideoHeaderType, sizeof(VideoHeaderType));

    avih.dwMicroSecPerFrame := VideoHeaderType.AvgTimePerFrame div 10;
    avih.dwWidth := VideoHeaderType.bmiHeader.biWidth;
    avih.dwHeight := VideoHeaderType.bmiHeader.biHeight;
    strh_video.rcFrame.Right := VideoHeaderType.bmiHeader.biWidth;
    strh_video.rcFrame.Bottom := VideoHeaderType.bmiHeader.biHeight;
    if VideoHeaderType.AvgTimePerFrame = 400000 then
    begin
      strh_video.dwScale := 1;
      strh_video.dwRate := 25;
    end
    else
    begin
      strh_video.dwScale := VideoHeaderType.AvgTimePerFrame;
      strh_video.dwRate := 10000000;
    end;
    strh_video.dwSuggestedBufferSize := VideoMediaType.lSampleSize;
    strf_video := VideoHeaderType.bmiHeader;
    strh_video.fccHandler := strf_video.biCompression;
    VideoGuidFound := true;
  end;

  Result := VideoGuidFound;
end;

function TAviWriter.str_to_4b(input: Ansistring): T4bytes;
var
  i: integer;
begin
  for i := 0 to 3 do
    str_to_4b[i] := byte(input[i + 1]);
end;

end.
