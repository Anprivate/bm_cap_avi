unit Unit_IO_ops;

interface

uses
  System.SysUtils, System.Classes, Winapi.Windows;

type
  TIO_ops = class(TComponent)
  private
    local_buffer: PByte;
    globalcurpos: int64;
    bufcurpos: int64;
    LBufferedMode: boolean;
    buffer_size: int64;
    //
    procedure SeekToStartOfFile;
    function SeekToEndOfFile: int64;
    procedure SetBufferedMode(inmode: boolean);
    procedure SetBufferSize(insize: int64);
  public
    OutFileHandle: THandle;
    property BufferedMode: boolean write SetBufferedMode;
    property BufferSize: int64 write SetBufferSize;
    //
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //
    function WriteData(const Buffer; Count: LongWord): Integer;
    function SetPosition(position: int64): int64;
    function GetCurrentPosition: int64;

  end;

implementation

{ TIO_ops }

constructor TIO_ops.Create(AOwner: TComponent);
begin
  inherited;
  LBufferedMode := false;
  local_buffer := nil;
  buffer_size := 1048576;
end;

destructor TIO_ops.Destroy;
begin
  if Assigned(local_buffer) then
    FreeMemory(local_buffer);
  inherited;
end;

function TIO_ops.GetCurrentPosition: int64;
begin
  if LBufferedMode then
    Result := globalcurpos
  else
    Result := System.SysUtils.FileSeek(OutFileHandle, int64(0), FILE_CURRENT);
end;

function TIO_ops.SeekToEndOfFile: int64;
begin
  Result := System.SysUtils.FileSeek(OutFileHandle, int64(0), FILE_END);
end;

procedure TIO_ops.SeekToStartOfFile;
begin
  System.SysUtils.FileSeek(OutFileHandle, int64(0), FILE_BEGIN);
end;

procedure TIO_ops.SetBufferedMode(inmode: boolean);
begin
  if inmode and not LBufferedMode then
  begin
    if not Assigned(local_buffer) then
      local_buffer := GetMemory(buffer_size);
    globalcurpos := 0;
    bufcurpos := 0;
  end;

  if not inmode and LBufferedMode then
    if Assigned(local_buffer) then
    begin
      FileWrite(OutFileHandle, local_buffer^, bufcurpos);
      FlushFileBuffers(OutFileHandle);
      FreeMemory(local_buffer);
      local_buffer := nil;
    end;

  LBufferedMode := inmode;
end;

procedure TIO_ops.SetBufferSize(insize: int64);
begin
  buffer_size := insize;
end;

function TIO_ops.SetPosition(position: int64): int64;
begin
  Result := FileSeek(OutFileHandle, position, FILE_BEGIN);
end;

function TIO_ops.WriteData(const Buffer; Count: LongWord): Integer;
var
  tmp_ptr: PByte;
  tmp_count: LongWord;
  tmp_size: LongWord;
begin
  if LBufferedMode then
  begin
    tmp_ptr := Addr(Buffer);
    tmp_count := Count;
    while (bufcurpos + tmp_count) > buffer_size do
    begin
      tmp_size := buffer_size - bufcurpos;
      Move(tmp_ptr^, (local_buffer + bufcurpos)^, tmp_size);
      tmp_ptr := tmp_ptr + tmp_size;
      tmp_count := tmp_count - tmp_size;
      bufcurpos := 0;

      FileWrite(OutFileHandle, local_buffer^, buffer_size);
      FlushFileBuffers(OutFileHandle);
    end;

    tmp_size := tmp_count;
    Move(tmp_ptr^, (local_buffer + bufcurpos)^, tmp_size);
    inc(bufcurpos, tmp_size);

    inc(globalcurpos, Count);

    Result := Count;
  end
  else
  begin
    Result := FileWrite(OutFileHandle, Buffer, Count);
  end;
end;

end.
