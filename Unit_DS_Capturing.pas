unit Unit_DS_Capturing;

interface

uses
  System.SysUtils, System.Variants, System.Classes, System.IniFiles,
  System.StrUtils, System.DateUtils, System.Math, System.AnsiStrings,
  Winapi.Windows, Winapi.Messages, Winapi.MMsystem, Winapi.ActiveX,
  Winapi.DirectShow9,
  Vcl.Menus, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.ComCtrls, System.Win.ComObj,
  Data.DSUtil, Decklink;

type
  TOneVideoFrame = record
    Used: boolean;
    Filled: boolean;
    Buffer: PByte;
    SampleSize: Cardinal;
    BufferSize: Cardinal;
    Time: TDateTime;
    Stamp: int64;
    MediaStamp: int64;
    TickCount: DWORD;
    TC: integer;
  end;

  POneVideoFrame = ^TOneVideoFrame;

  TOneAudioFrame = record
    Used: boolean;
    Filled: boolean;
    Buffer: PByte;
    BufferSize: Cardinal;
    SampleSize: Cardinal;
    Stamp: int64;
    MediaStamp: int64;
    TickCount: DWORD;
  end;

  POneAudioFrame = ^TOneAudioFrame;

  TFieldCorrectionCB = class(TComponent, ISampleGrabberCB)
  private
    // Capturer: TObject;
    function SampleCB(SampleTime: Double; pSample: IMediaSample)
      : HResult; stdcall;
    function BufferCB(SampleTime: Double; pBuffer: PByte; BufferLen: longint)
      : HResult; stdcall;
    function QueryInterface(const IID: TGUID; out Obj): HResult;
      reintroduce; stdcall;
    function _AddRef: integer; stdcall;
    function _Release: integer; stdcall;
  end;

  TVideoGrabCB = class(TComponent, ISampleGrabberCB)
  private
    // Capturer: TObject;
    function SampleCB(SampleTime: Double; pSample: IMediaSample)
      : HResult; stdcall;
    function BufferCB(SampleTime: Double; pBuffer: PByte; BufferLen: longint)
      : HResult; stdcall;
    function QueryInterface(const IID: TGUID; out Obj): HResult;
      reintroduce; stdcall;
    function _AddRef: integer; stdcall;
    function _Release: integer; stdcall;
  end;

  TAudioGrabCB = class(TComponent, ISampleGrabberCB)
  private
    // Capturer: TObject;
    function SampleCB(SampleTime: Double; pSample: IMediaSample)
      : HResult; stdcall;
    function BufferCB(SampleTime: Double; pBuffer: PByte; BufferLen: longint)
      : HResult; stdcall;
    function QueryInterface(const IID: TGUID; out Obj): HResult;
      reintroduce; stdcall;
    function _AddRef: integer; stdcall;
    function _Release: integer; stdcall;
  end;

  TDSCapturer = class(TComponent)
  private
    Buff_count: integer;
    GraphIsReady: boolean;
    use_ffdshow: boolean;
    VideoFrames: array of TOneVideoFrame;
    AudioFrames: array of TOneAudioFrame;
    WasCloseRequest: boolean;
    NeededFrameInterval: int64;
    GraphIsOnlyCreated: boolean;

    LastFrameRXED, LastAudioRXED, LastVideoRXED: TDateTime;
    LastFrameRXEDtick, LastAudioRXEDtick, LastVideoRXEDtick: DWORD;
    GraphWasCreatedAt: TDateTime;
    LastFrameTS, LastAudioTS, LastVideoTS: int64;

    // service
    ErrMsg: string;

    // graph related
    pGraph: IGraphBuilder;
    pBuild: ICaptureGraphBuilder2;
    pControl: IMediaControl;
    GraphID: integer;

    // video capture filter related
    DLV_CLSID_use: boolean;
    DLV_CLSID: TGUID;
    pVCap: IBaseFilter;
    DLVoutpin: IPin;

    // audio capture filter related
    DLA_CLSID_use: boolean;
    DLA_CLSID: TGUID;
    pACap: IBaseFilter;
    DLAoutpin: IPin;

    // smart tee filter related
    pSmartTee: IBaseFilter;
    STIn, STOutCapture, SToutPreview: IPin;

    // video encoder filter related
    ffdshow_CLSID: TGUID;
    pVideoEnc: IBaseFilter;
    VideoEncInPin, VideoEncOutPin: IPin;
    DVEncoder: IDVEnc;
    DVenc_info: DVINFO;

    // field correction sample grabber filter related
    pSGfc: IBaseFilter;
    SGfcinpin, SGfcoutpin: IPin;
    SGFCused: boolean;
    SampleGrabberFC: ISampleGrabber;
    pFieldCorrectionCB: TFieldCorrectionCB;

    // encoded video sample grabber filter related
    pEncodedSG: IBaseFilter;
    EncodedSGinpin, EncodedSGoutpin: IPin;
    EncodedSampleGrabber: ISampleGrabber;
    pVideoGrabCB: TVideoGrabCB;

    // audio sample grabber filter related
    pSGAudio: IBaseFilter;
    SGAudioinpin, SGAudiooutpin: IPin;
    SampleGrabberAudio: ISampleGrabber;
    pAudioGrabCB: TAudioGrabCB;

    // dvmuxer filter related
    pDVmux: IBaseFilter;
    DVMuxVinpin, DVMuxAinpin, DVMuxoutpin: IPin;

    // video mixing renderer filter related
    pVMR: IBaseFilter;
    RenderPin: IPin;

    // null renderer filter related
    pNullRenderer: IBaseFilter;
    NullRendererInPin: IPin;

    // internal procedures
    procedure ClearBuffers;
    procedure MarkAllBuffersAsUnused;
    procedure WriteToLog(chto: string);
    // graph building procedures
    procedure CreateVideoCaptureDevice;
    procedure CreateAudioCaptureDevice;
    procedure CreateSmartTeeFilter;
    procedure CreateVideoEncoder;
    procedure CreateVideoSampleGrabber;
    procedure CreateFieldCorrectionSampleGrabber;
    procedure CreateAudioSampleGrabber;
    procedure CreateDVmuxer;
    procedure SearchDVmuxerAudioInputPin;
    procedure CreateVideoMixingRenderer;
    procedure CreateNullRenderer;
    procedure ConnectPins(PinFrom, PinTo: IPin;
      SuccessString, ErrorString: string);
    procedure SetFrameInterval(invalue: int64);
    function GetFrameInterval: int64;
  public
    // must be filled!!!
    ffdshow_CLSID_string: string;
    card_no: integer;
    FieldCorrectionActive: boolean;
    CapWidth, CapHeight: integer;
    NeededSubType: TGUID;
    AudioIsMultichannel: boolean;
    PreviewOn: boolean;
    VideoPanel: TPanel;
    VideoIsWS: boolean;

    FrameTO: integer;
    NodataTO: integer;
    // output
    DVCapturing: boolean;
    was_any_error: boolean;

    ForLog: TStringList;

    property BuffCount: integer read Buff_count;
    property FrameInterval: int64 read GetFrameInterval write SetFrameInterval;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function CreateCaptureGraph: boolean;
    procedure DestroyCaptureGraph;
    procedure RunCaptureGraph;
    procedure StopCaptureGraph;
    procedure ColdRestart;
    function CheckForTimeouts: boolean;
    procedure CloseRequest;

    // get av parameters
    function GetVideoMediaType(var VideoMediaType: TAMMediaType): boolean;
    function GetAudioMediaType(var AudioMediaType: TAMMediaType): boolean;

    // working with buffers
    procedure PrepareBuffers(Count, VBufferSize, ABufferSize: integer);
    function GetLargestBufferSize: integer;
    function GetVideoFramesInBuffer: integer;
    function GetAudioFramesInBuffer: integer;
    function GetFirstVideoFrame: POneVideoFrame;
    function GetFirstAudioFrame: POneAudioFrame;
    function GetUnusedVideoFrame: POneVideoFrame;
    function GetUnusedAudioFrame: POneAudioFrame;
  end;

  EMyOwnException = class(Exception);

implementation

const
  Frames24h = 24 * 60 * 60 * 25;

function TCtoString(inTC: longint): AnsiString;
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
  TCtoString := System.AnsiStrings.format('%.2u', [iHours]) + ':' +
    System.AnsiStrings.format('%.2u', [iMinutes]) + ':' +
    System.AnsiStrings.format('%.2u', [iSeconds]) + ':' +
    System.AnsiStrings.format('%.2u', [iFrames]);
end;

function AddGraphToRot(Graph: IFilterGraph; out ID: integer): HResult;
var
  Moniker: IMoniker;
  ROT: IRunningObjectTable;
  wsz: WideString;
begin
  Result := GetRunningObjectTable(0, ROT);
  if (Result <> S_OK) then
    exit;
  wsz := format('FilterGraph %p pid %x',
    [pointer(Graph), GetCurrentProcessId()]);
  Result := CreateItemMoniker('!', PWideChar(wsz), Moniker);
  if (Result <> S_OK) then
    exit;
  Result := ROT.Register(0, Graph, Moniker, ID);
  Moniker := nil;
end;

function RemoveGraphFromRot(ID: integer): HResult;
var
  ROT: IRunningObjectTable;
begin
  Result := GetRunningObjectTable(0, ROT);
  if (Result <> S_OK) then
    exit;
  Result := ROT.Revoke(ID);
  ROT := nil;
end;

function BMRenderGuidsFromNum(card_no: integer; var VGUID: TGUID;
  var AGUID: TGUID): boolean;
begin
  Result := true;
  case card_no of
    1:
      begin
        VGUID := CLSID_DecklinkVideoRenderFilter;
        AGUID := CLSID_DecklinkAudioRenderFilter;
      end;
    2:
      begin
        VGUID := CLSID_DecklinkVideoRenderFilter2;
        AGUID := CLSID_DecklinkAudioRenderFilter2;
      end;
    3:
      begin
        VGUID := CLSID_DecklinkVideoRenderFilter3;
        AGUID := CLSID_DecklinkAudioRenderFilter3;
      end;
    4:
      begin
        VGUID := CLSID_DecklinkVideoRenderFilter4;
        AGUID := CLSID_DecklinkAudioRenderFilter4;
      end;
    5:
      begin
        VGUID := CLSID_DecklinkVideoRenderFilter5;
        AGUID := CLSID_DecklinkAudioRenderFilter5;
      end;
    6:
      begin
        VGUID := CLSID_DecklinkVideoRenderFilter6;
        AGUID := CLSID_DecklinkAudioRenderFilter6;
      end;
    7:
      begin
        VGUID := CLSID_DecklinkVideoRenderFilter7;
        AGUID := CLSID_DecklinkAudioRenderFilter7;
      end;
    8:
      begin
        VGUID := CLSID_DecklinkVideoRenderFilter8;
        AGUID := CLSID_DecklinkAudioRenderFilter8;
      end;
  else
    Result := false;
  end;
end;

function BMCaptureGuidsFromNum(card_no: integer; var VGUID: TGUID;
  var AGUID: TGUID): boolean;
begin
  Result := true;
  case card_no of
    1:
      begin
        VGUID := CLSID_DecklinkVideoCaptureFilter;
        AGUID := CLSID_DecklinkAudioCaptureFilter;
      end;
    2:
      begin
        VGUID := CLSID_DecklinkVideoCaptureFilter2;
        AGUID := CLSID_DecklinkAudioCaptureFilter2;
      end;
    3:
      begin
        VGUID := CLSID_DecklinkVideoCaptureFilter3;
        AGUID := CLSID_DecklinkAudioCaptureFilter3;
      end;
    4:
      begin
        VGUID := CLSID_DecklinkVideoCaptureFilter4;
        AGUID := CLSID_DecklinkAudioCaptureFilter4;
      end;
    5:
      begin
        VGUID := CLSID_DecklinkVideoCaptureFilter5;
        AGUID := CLSID_DecklinkAudioCaptureFilter5;
      end;
    6:
      begin
        VGUID := CLSID_DecklinkVideoCaptureFilter6;
        AGUID := CLSID_DecklinkAudioCaptureFilter6;
      end;
    7:
      begin
        VGUID := CLSID_DecklinkVideoCaptureFilter7;
        AGUID := CLSID_DecklinkAudioCaptureFilter7;
      end;
    8:
      begin
        VGUID := CLSID_DecklinkVideoCaptureFilter8;
        AGUID := CLSID_DecklinkAudioCaptureFilter8;
      end;
  else
    Result := false;
  end;
end;

{ TFieldCorrectionCB }

function TFieldCorrectionCB.QueryInterface(const IID: TGUID; out Obj)
  : HResult; stdcall;
begin
  Result := inherited QueryInterface(IID, Obj);
end;

function TFieldCorrectionCB._AddRef: integer; stdcall;
begin
  Result := 2;
end;

function TFieldCorrectionCB._Release: integer; stdcall;
begin
  Result := 1;
end;

function TFieldCorrectionCB.SampleCB(SampleTime: Double;
  pSample: IMediaSample): HResult;
const
  lines = 1;
var
  pBuffer: PByte;
  sample_size: LongWord;
  pFrom, pTo: PByte;
  move_size: LongWord;
  iTmp1, iTmp2: int64;
begin
  Result := S_OK;

  if not Assigned(Owner) then
    exit;

  with Owner as TDSCapturer do
  begin
    LastVideoRXED := Now();
    LastVideoRXEDtick := GetTickCount();

    if WasCloseRequest then
      exit;

    sample_size := pSample.GetActualDataLength;

    pSample.GetPointer(pBuffer);
    pSample.GetTime(iTmp1, iTmp2);

    if LastVideoTS > 0 then
    begin
      if (iTmp1 - LastVideoTS) <> (NeededFrameInterval * 10) then
      begin
        WriteToLog(' Nonmonotonous timestamps in video ' +
          inttostr(LastVideoTS div 10000) + '->' + inttostr(iTmp1 div 10000));
      end;
    end;
    LastVideoTS := iTmp1;

    if sample_size = (720 * 576 * 2) then
    begin
      pTo := pBuffer;
      pFrom := pBuffer + (1440 * lines);
      move_size := sample_size - (1440 * lines);
      Move(pFrom^, pTo^, move_size);
    end;
  end;
end;

function TFieldCorrectionCB.BufferCB(SampleTime: Double; pBuffer: PByte;
  BufferLen: longint): HResult; stdcall;
begin
  Result := S_OK;
end;

{ TVideoGrabCB }

function TVideoGrabCB.QueryInterface(const IID: TGUID; out Obj)
  : HResult; stdcall;
begin
  Result := inherited QueryInterface(IID, Obj);
end;

function TVideoGrabCB._AddRef: integer; stdcall;
begin
  Result := 2;
end;

function TVideoGrabCB._Release: integer; stdcall;
begin
  Result := 1;
end;

function TVideoGrabCB.SampleCB(SampleTime: Double;
  pSample: IMediaSample): HResult;
var
  pBuffer: PByte;
  sample_size: LongWord;
  iTmp1, iTmp2: int64;
  CurrentBuffer: POneVideoFrame;
begin
  Result := S_OK;

  if not Assigned(Owner) then
    exit;

  with Owner as TDSCapturer do
  begin
    LastFrameRXED := Now();
    LastFrameRXEDtick := GetTickCount();

    if WasCloseRequest then
      exit;

    sample_size := pSample.GetActualDataLength;

    pSample.GetTime(iTmp1, iTmp2);

    GraphIsOnlyCreated := false;

    if LastFrameTS > 0 then
    begin
      if (iTmp1 - LastFrameTS) <> (NeededFrameInterval * 10) then
      begin
        WriteToLog(' Nonmonotonous timestamps in encoded video ' +
          inttostr(LastFrameTS div 10000) + '->' + inttostr(iTmp1 div 10000));
      end;
    end;
    LastFrameTS := iTmp1;

    CurrentBuffer := GetUnusedVideoFrame;

    if not Assigned(CurrentBuffer) then
    begin // overrun
      WriteToLog('Видеобуфер переполнен. Кадр с TS=' + inttostr(iTmp1 div 10000)
        + 'ms потерян');
      exit;
    end;

    CurrentBuffer^.Time := LastFrameRXED;
    CurrentBuffer^.Stamp := iTmp1;
    CurrentBuffer^.TickCount := GetTickCount();

    pSample.GetMediaTime(iTmp1, iTmp2);
    CurrentBuffer^.MediaStamp := iTmp1;

    pSample.GetPointer(pBuffer);

    if sample_size > CurrentBuffer^.BufferSize then
    begin
      FreeMem(CurrentBuffer^.Buffer);
      GetMem(CurrentBuffer^.Buffer, sample_size);
      CurrentBuffer^.BufferSize := sample_size;
      WriteToLog(String(TCtoString(CurrentBuffer^.Stamp div 400000)) +
        ' увеличен размер буфера для видео до ' + inttostr(sample_size));
    end
    else
    begin
      CurrentBuffer^.SampleSize := sample_size;
    end;
    Move(pBuffer^, CurrentBuffer^.Buffer^, sample_size);
    CurrentBuffer^.Filled := true;
  end;
end;

function TVideoGrabCB.BufferCB(SampleTime: Double; pBuffer: PByte;
  BufferLen: longint): HResult; stdcall;
begin
  Result := S_OK;
end;

{ TAudioGrabCB }

function TAudioGrabCB.QueryInterface(const IID: TGUID; out Obj)
  : HResult; stdcall;
begin
  Result := inherited QueryInterface(IID, Obj);
end;

function TAudioGrabCB._AddRef: integer; stdcall;
begin
  Result := 2;
end;

function TAudioGrabCB._Release: integer; stdcall;
begin
  Result := 1;
end;

function TAudioGrabCB.SampleCB(SampleTime: Double;
  pSample: IMediaSample): HResult;
var
  pBuffer: PByte;
  sample_size: LongWord;
  iTmp1, iTmp2: int64;
  CurrentBuffer: POneAudioFrame;
begin
  Result := S_OK;

  if not Assigned(Owner) then
    exit;

  with Owner as TDSCapturer do
  begin
    LastAudioRXED := Now();
    LastAudioRXEDtick := GetTickCount();

    pSample.GetTime(iTmp1, iTmp2);

    if LastAudioTS > 0 then
    begin
      if (iTmp1 - LastAudioTS) <> (NeededFrameInterval * 10) then
      begin
        WriteToLog(' Nonmonotonous timestamps in audio ' +
          inttostr(LastAudioTS div 10000) + '->' + inttostr(iTmp1 div 10000));
      end;
    end;
    LastAudioTS := iTmp1;

    CurrentBuffer := GetUnusedAudioFrame;

    if not Assigned(CurrentBuffer) then
    begin // overrun
      WriteToLog('Аудиобуфер переполнен. Кадр с TS=' + inttostr(iTmp1 div 10000)
        + 'ms потерян');
      exit;
    end;

    CurrentBuffer^.Stamp := iTmp1;
    pSample.GetMediaTime(iTmp1, iTmp2);
    CurrentBuffer^.MediaStamp := iTmp1;

    pSample.GetPointer(pBuffer);
    sample_size := pSample.GetActualDataLength;
    if sample_size > CurrentBuffer^.BufferSize then
    begin
      FreeMem(CurrentBuffer^.Buffer);
      GetMem(CurrentBuffer^.Buffer, sample_size);
      CurrentBuffer^.BufferSize := sample_size;
      WriteToLog(String(TCtoString(CurrentBuffer^.Stamp div 400000)) +
        ' увеличен размер буфера для аудио до ' + inttostr(sample_size));
    end;
    Move(pBuffer^, CurrentBuffer^.Buffer^, sample_size);
    CurrentBuffer^.SampleSize := sample_size;
    CurrentBuffer^.Filled := true;
  end;
end;

function TAudioGrabCB.BufferCB(SampleTime: Double; pBuffer: PByte;
  BufferLen: longint): HResult; stdcall;
begin
  Result := S_OK;
end;

{ TDSCapturer }

constructor TDSCapturer.Create(AOwner: TComponent);
begin
  inherited;
  ForLog := TStringList.Create;
  SetLength(ErrMsg, 512);
  WasCloseRequest := false;
  GraphIsReady := false;

  if not Assigned(pVideoGrabCB) then
    pVideoGrabCB := TVideoGrabCB.Create(Self);

  if not Assigned(pAudioGrabCB) then
    pAudioGrabCB := TAudioGrabCB.Create(Self);

  if not Assigned(pFieldCorrectionCB) then
    pFieldCorrectionCB := TFieldCorrectionCB.Create(Self);
end;

destructor TDSCapturer.Destroy;
begin
  pVideoGrabCB.Free;
  pAudioGrabCB.Free;
  pFieldCorrectionCB.Free;

  SetLength(ErrMsg, 0);
  ForLog.Free;
  inherited;
end;

{ graph creation }

procedure TDSCapturer.CreateVideoCaptureDevice;
var
  i: integer;
  hr: HResult;
  iCount, iSize: integer;
  ModeOk: boolean;
  //
  pDevEnum: ICreateDevEnum;
  pEnum: IEnumMoniker;
  pMoniker: IMoniker;
  pPropBag: IPropertyBag;
  varName: OleVariant;
  CaptureName: String;
  VendorInfo: PChar;
  //
  pAMStreamConfig: IAMStreamConfig;
  currentMediaType: PAMMEDIATYPE;
  vscc: VIDEO_STREAM_CONFIG_CAPS;
  //
  enumPins: IEnumPins;
  pin: IPin;
  pinDir: _PinDirection;
begin
  // create video capture device
  if DLV_CLSID_use then
  begin // direct working with CLSID
    hr := CoCreateInstance(DLV_CLSID, nil, CLSCTX_INPROC_SERVER,
      IID_IBaseFilter, pVCap);
    if FAILED(hr) then
    begin
      AMGetErrorText(hr, PChar(ErrMsg), 512);
      raise EMyOwnException.Create('Не могу создать Decklink Video Capture - ' +
        Trim(ErrMsg));
    end;
  end
  else
  begin // if DLV_CLSID_use
    // video capture device enumerate
    pDevEnum := nil;
    hr := CoCreateInstance(CLSID_SystemDeviceEnum, nil, CLSCTX_INPROC_SERVER,
      IID_ICreateDevEnum, pDevEnum);
    if FAILED(hr) then
    begin
      AMGetErrorText(hr, PChar(ErrMsg), 512);
      raise EMyOwnException.Create('Не могу создать SystemDeviceEnum - ' +
        Trim(ErrMsg));
    end;

    // Create an enumerator for the video capture category.
    pEnum := nil;
    hr := pDevEnum.CreateClassEnumerator(CLSID_VideoInputDeviceCategory,
      pEnum, 0);
    if FAILED(hr) then
    begin
      AMGetErrorText(hr, PWideChar(ErrMsg), 512);
      raise EMyOwnException.Create('Не могу создать Enum Video Input Device - '
        + Trim(ErrMsg));
    end;

    // enumerate video capture devices
    pVCap := nil;
    pMoniker := nil;
    while pEnum.Next(1, pMoniker, nil) = S_OK do
    begin
      pPropBag := nil;
      // IID_IPropertyBag
      hr := pMoniker.BindToStorage(nil, nil,
        StringToGuid('{ 55272A00-42CB-11CE-8135-00AA004BB851}'), pPropBag);
      if FAILED(hr) then
      begin
        Continue; // Skip this one, maybe the next one will work.
      end
      else
      begin
        // get name
        VariantInit(varName);
        hr := pPropBag.Read('Description', varName, nil);
        if FAILED(hr) then
          hr := pPropBag.Read('FriendlyName', varName, nil);
        if SUCCEEDED(hr) then
        begin
          CaptureName := string(varName);
          // binding to pCap
          if Pos('Decklink', CaptureName) > 0 then
          begin
            hr := pMoniker.BindToObject(nil, nil, IID_IBaseFilter, pVCap);
            if SUCCEEDED(hr) then
              break;
          end;
        end;
        VariantClear(varName);
      end;
    end; // while
  end; // if DLV_CLSID_use

  if pVCap = nil then
  begin
    raise EMyOwnException.Create('Не могу найти video capture device');
  end;

  hr := pVCap.QueryVendorInfo(VendorInfo);
  if FAILED(hr) then
  begin
    AMGetErrorText(hr, PWideChar(ErrMsg), 512);
    raise EMyOwnException.Create('Не могу получить video VendorInfo - ' +
      Trim(ErrMsg));
  end;

  CaptureName := VendorInfo;
  WriteToLog('Video capture device - ' + CaptureName);

  hr := pGraph.AddFilter(pVCap, PChar(CaptureName));
  if FAILED(hr) then
  begin
    AMGetErrorText(hr, PWideChar(ErrMsg), 512);
    raise EMyOwnException.Create
      ('Не могу добавить в граф video capture device - ' + Trim(ErrMsg));
  end;

  hr := pBuild.FindInterface(@PIN_CATEGORY_CAPTURE, @MEDIATYPE_Video, pVCap,
    IID_IAMStreamConfig, pAMStreamConfig);
  if FAILED(hr) then
  begin
    AMGetErrorText(hr, PWideChar(ErrMsg), 512);
    raise EMyOwnException.Create('Не могу получить video StreamConfig - ' +
      Trim(ErrMsg));
  end;

  hr := pAMStreamConfig.GetNumberOfCapabilities(iCount, iSize);
  if FAILED(hr) then
  begin
    AMGetErrorText(hr, PWideChar(ErrMsg), 512);
    raise EMyOwnException.Create
      ('Не могу получить video NumberOfCapabilities - ' + Trim(ErrMsg));
  end;

  ModeOk := false;
  for i := 0 to iCount - 1 do
  begin
    hr := pAMStreamConfig.GetStreamCaps(i, currentMediaType, vscc);
    if SUCCEEDED(hr) then
    begin
      if IsEqualGUID(currentMediaType.subtype, NeededSubType) and
        (vscc.InputSize.cx = CapWidth) and (vscc.InputSize.cy = CapHeight) and
        ((vscc.MaxFrameInterval div 10) = NeededFrameInterval) then
      begin
        hr := pAMStreamConfig.SetFormat(currentMediaType);
        if SUCCEEDED(hr) then
        begin
          ModeOk := true;
          break;
        end;
      end;
    end;
  end;

  if not ModeOk then
  begin
    raise EMyOwnException.Create
      ('Не могу установить требуемый режим видеозахвата');
  end;

  // search Decklink video output pin
  DLVoutpin := nil;
  hr := pVCap.enumPins(enumPins);
  if SUCCEEDED(hr) then
  begin
    while (enumPins.Next(1, pin, nil) = S_OK) do
    begin
      pin.QueryDirection(pinDir);
      if pinDir = pindir_output then
      begin
        DLVoutpin := pin;
        break;
      end;
    end;
  end
  else
  begin
    AMGetErrorText(hr, PWideChar(ErrMsg), 512);
    raise EMyOwnException.Create('Не могу enum пины decklink video - ' +
      Trim(ErrMsg));
  end;

  if DLVoutpin = nil then
  begin
    raise EMyOwnException.Create('Не могу найти video capture output pin');
  end;
end;

procedure TDSCapturer.CreateAudioCaptureDevice;
var
  i: integer;
  hr: HResult;
  iCount, iSize: integer;
  ModeOk: boolean;
  ReqAudioChannels: Cardinal;
  //
  pDevEnum: ICreateDevEnum;
  pEnum: IEnumMoniker;
  pMoniker: IMoniker;
  pPropBag: IPropertyBag;
  varName: OleVariant;
  CaptureName: String;
  VendorInfo: PChar;
  //
  pAMStreamConfig: IAMStreamConfig;
  currentMediaType: PAMMEDIATYPE;
  ascc: AUDIO_STREAM_CONFIG_CAPS;
  //
  enumPins: IEnumPins;
  pin: IPin;
  pinDir: _PinDirection;
begin
  // create audio capture device
  if DLA_CLSID_use then
  begin // direct working with CLSID
    hr := CoCreateInstance(DLA_CLSID, nil, CLSCTX_INPROC_SERVER,
      IID_IBaseFilter, pACap);
    if FAILED(hr) then
    begin
      AMGetErrorText(hr, PWideChar(ErrMsg), 512);
      raise EMyOwnException.Create('Не могу создать audio capture  - ' +
        Trim(ErrMsg));
    end;
  end
  else
  begin // if DLA_CLSID_use
    // audio capture device enumerate
    pDevEnum := nil;
    pEnum := nil;
    hr := CoCreateInstance(CLSID_SystemDeviceEnum, nil, CLSCTX_INPROC_SERVER,
      IID_ICreateDevEnum, pDevEnum);
    if FAILED(hr) then
    begin
      AMGetErrorText(hr, PWideChar(ErrMsg), 512);
      raise EMyOwnException.Create('Не могу создать audio SystemDeviceEnum - ' +
        Trim(ErrMsg));
    end;

    // Create an enumerator for the audio capture category
    hr := pDevEnum.CreateClassEnumerator(CLSID_AudioInputDeviceCategory,
      pEnum, 0);
    if FAILED(hr) then
    begin
      AMGetErrorText(hr, PWideChar(ErrMsg), 512);
      raise EMyOwnException.Create
        ('Не могу создать enumerator AudioInputDeviceCategory - ' +
        Trim(ErrMsg));
    end;

    // enumerate audio capture devices
    pACap := nil;
    pMoniker := nil;
    while pEnum.Next(1, pMoniker, nil) = S_OK do
    begin
      pPropBag := nil;
      hr := pMoniker.BindToStorage(nil, nil,
        StringToGuid('{ 55272A00-42CB-11CE-8135-00AA004BB851}'), pPropBag);
      if FAILED(hr) then
      begin
        Continue; // Skip this one, maybe the next one will work.
      end
      else
      begin
        // get name
        VariantInit(varName);
        hr := pPropBag.Read('Description', varName, nil);
        if FAILED(hr) then
          hr := pPropBag.Read('FriendlyName', varName, nil);
        if SUCCEEDED(hr) then
        begin
          CaptureName := string(varName);
          // binding to pCap
          if Pos('Decklink', CaptureName) > 0 then
          begin
            hr := pMoniker.BindToObject(nil, nil, IID_IBaseFilter, pACap);
            if SUCCEEDED(hr) then
              break;
          end; // if decklink
        end;
        VariantClear(varName);
      end;
    end; // while
  end; // if DLA_CLSID_use

  if pACap = nil then
  begin
    raise EMyOwnException.Create('Не могу найти audio capture device');
  end;

  hr := pACap.QueryVendorInfo(VendorInfo);
  if FAILED(hr) then
  begin
    AMGetErrorText(hr, PWideChar(ErrMsg), 512);
    raise EMyOwnException.Create('Не могу получить audio VendorInfo - ' +
      Trim(ErrMsg));
  end;

  CaptureName := VendorInfo;
  WriteToLog('Audio capture device - ' + CaptureName);

  hr := pGraph.AddFilter(pACap, PChar(CaptureName));
  if FAILED(hr) then
  begin
    AMGetErrorText(hr, PWideChar(ErrMsg), 512);
    raise EMyOwnException.Create
      ('Не могу добавить в граф audio capture device - ' + Trim(ErrMsg));
  end;

  hr := pBuild.FindInterface(@PIN_CATEGORY_CAPTURE, @MEDIATYPE_Audio, pACap,
    IID_IAMStreamConfig, pAMStreamConfig);
  if FAILED(hr) then
  begin
    AMGetErrorText(hr, PWideChar(ErrMsg), 512);
    raise EMyOwnException.Create('Не могу получить audio StreamConfig - ' +
      Trim(ErrMsg));
  end;

  hr := pAMStreamConfig.GetNumberOfCapabilities(iCount, iSize);
  if FAILED(hr) then
  begin
    AMGetErrorText(hr, PWideChar(ErrMsg), 512);
    raise EMyOwnException.Create
      ('Не могу получить audio NumberOfCapabilities - ' + Trim(ErrMsg));
  end;

  // search audio mode
  if AudioIsMultichannel then
    ReqAudioChannels := 16
  else
    ReqAudioChannels := 2;

  ModeOk := false;
  for i := 0 to iCount - 1 do
  begin
    hr := pAMStreamConfig.GetStreamCaps(i, currentMediaType, ascc);
    if SUCCEEDED(hr) then
    begin
      if (ascc.MinimumBitsPerSample = 16) and
        (ascc.MinimumSampleFrequency = 48000) and
        (ascc.MinimumChannels = ReqAudioChannels) then
      begin
        hr := pAMStreamConfig.SetFormat(currentMediaType);
        if SUCCEEDED(hr) then
        begin
          ModeOk := true;
          break;
        end;
      end;
    end;
  end;

  if not ModeOk then
  begin
    raise EMyOwnException.Create
      ('Не могу установить требуемый режим аудиозахвата');
  end;

  // search Decklink audio output pin
  DLAoutpin := nil;
  hr := pACap.enumPins(enumPins);
  if SUCCEEDED(hr) then
  begin
    while (enumPins.Next(1, pin, nil) = S_OK) do
    begin
      pin.QueryDirection(pinDir);
      if pinDir = pindir_output then
      begin
        DLAoutpin := pin;
        break;
      end;
    end;
  end
  else
  begin
    AMGetErrorText(hr, PWideChar(ErrMsg), 512);
    raise EMyOwnException.Create('Не могу enum пины decklink audio - ' +
      Trim(ErrMsg));
  end;
  if DLAoutpin = nil then
  begin
    raise EMyOwnException.Create('Не могу найти audio capture output pin');
  end;
end;

procedure TDSCapturer.CreateSmartTeeFilter;
var
  hr: HResult;
  i: integer;
  //
  enumPins: IEnumPins;
  pin: IPin;
  pinDir: _PinDirection;
begin
  // create Smart Tee
  hr := CoCreateInstance(CLSID_SmartTee, nil, CLSCTX_INPROC_SERVER,
    IID_IBaseFilter, pSmartTee);
  if FAILED(hr) then
  begin
    AMGetErrorText(hr, PChar(ErrMsg), 512);
    raise EMyOwnException.Create('Не могу создать SmartTee - ' + Trim(ErrMsg));
  end; // CLSID_SmartTee

  hr := pGraph.AddFilter(pSmartTee, 'Smart tee');
  if SUCCEEDED(hr) then
  begin
    WriteToLog('Smart tee added');
  end
  else
  begin
    AMGetErrorText(hr, PChar(ErrMsg), 512);
    raise EMyOwnException.Create('Не могу добавить в граф SmartTee - ' +
      Trim(ErrMsg));
  end;

  // search Smart tee input/output pins
  STIn := nil;
  STOutCapture := nil;
  SToutPreview := nil;
  i := 0;

  hr := pSmartTee.enumPins(enumPins);
  if SUCCEEDED(hr) then
  begin
    while (enumPins.Next(1, pin, nil) = S_OK) do
    begin
      pin.QueryDirection(pinDir);
      if pinDir = pindir_input then
      begin
        STIn := pin;
      end
      else
      begin
        if i = 0 then
          STOutCapture := pin
        else
          SToutPreview := pin;
        inc(i);
      end;
    end;
  end
  else
  begin
    AMGetErrorText(hr, PChar(ErrMsg), 512);
    raise EMyOwnException.Create('Не могу enum пины SmartTee - ' +
      Trim(ErrMsg));
  end;

  if not Assigned(STIn) then
    raise EMyOwnException.Create('Не могу найти smart tee input pin');

  if not Assigned(STOutCapture) then
    raise EMyOwnException.Create('Не могу найти smart tee capture output pin');

  if not Assigned(SToutPreview) then
    raise EMyOwnException.Create('Не могу найти smart tee preview output pin');
end;

procedure TDSCapturer.CreateVideoEncoder;
var
  hr: HResult;
  //
  enumPins: IEnumPins;
  pin: IPin;
  pinDir: _PinDirection;
begin
  if use_ffdshow then
  begin
    // create custom video Encoder
    hr := CoCreateInstance(ffdshow_CLSID, nil, CLSCTX_INPROC_SERVER,
      IID_IBaseFilter, pVideoEnc);
    if FAILED(hr) then
    begin
      AMGetErrorText(hr, PChar(ErrMsg), 512);
      raise EMyOwnException.Create('Не могу создать ffdshow video encoder - ' +
        Trim(ErrMsg));
    end; // ffdshow_clsid

    hr := pGraph.AddFilter(pVideoEnc, 'Custom video encoder');
    if SUCCEEDED(hr) then
    begin
      WriteToLog('Custom video encoder added');
    end
    else
    begin
      AMGetErrorText(hr, PChar(ErrMsg), 512);
      raise EMyOwnException.Create
        ('Не могу добавить custom video encoder в граф - ' + Trim(ErrMsg));
    end;
  end
  else
  begin
    // create default DV Encoder
    hr := CoCreateInstance(CLSID_DVVideoEnc, nil, CLSCTX_INPROC_SERVER,
      IID_IBaseFilter, pVideoEnc);
    if FAILED(hr) then
    begin
      AMGetErrorText(hr, PChar(ErrMsg), 512);
      raise EMyOwnException.Create('Не могу создать default DV video encoder - '
        + Trim(ErrMsg));
    end; // CLSID_DVVideoEnc

    hr := pVideoEnc.QueryInterface(IID_IDVEnc, DVEncoder);
    if FAILED(hr) then
    begin
      AMGetErrorText(hr, PChar(ErrMsg), 512);
      raise EMyOwnException.Create('Не могу получить IID_IDVEnc интерфейс - ' +
        Trim(ErrMsg));
    end;

    hr := DVEncoder.put_IFormatResolution(DVENCODERVIDEOFORMAT_PAL,
      DVENCODERFORMAT_DVSD, DVENCODERRESOLUTION_720x480, ByteBool(false),
      DVenc_info);
    if FAILED(hr) then
    begin
      AMGetErrorText(hr, PChar(ErrMsg), 512);
      raise EMyOwnException.Create('Не могу установить параметры DV Encoder - '
        + Trim(ErrMsg));
    end;

    hr := pGraph.AddFilter(pVideoEnc, 'DV encoder');
    if SUCCEEDED(hr) then
    begin
      WriteToLog('DV Encoder added');
    end
    else
    begin
      AMGetErrorText(hr, PChar(ErrMsg), 512);
      raise EMyOwnException.Create('Не могу добавить video encoder в граф - ' +
        Trim(ErrMsg));
    end;
  end;

  // search video encoder input and output pins
  VideoEncInPin := nil;
  VideoEncOutPin := nil;
  hr := pVideoEnc.enumPins(enumPins);
  if SUCCEEDED(hr) then
  begin
    while (enumPins.Next(1, pin, nil) = S_OK) do
    begin
      pin.QueryDirection(pinDir);
      if pinDir = pindir_input then
        VideoEncInPin := pin
      else
        VideoEncOutPin := pin;
    end;
  end
  else
  begin
    AMGetErrorText(hr, PChar(ErrMsg), 512);
    raise EMyOwnException.Create('Не могу enum пины encoder - ' + Trim(ErrMsg));
  end;

  if not Assigned(VideoEncInPin) then
    raise EMyOwnException.Create('Не могу найти encoder input pin');

  if not Assigned(VideoEncOutPin) then
    raise EMyOwnException.Create('Не могу найти encoder output pin');
end;

procedure TDSCapturer.CreateFieldCorrectionSampleGrabber;
var
  hr: HResult;
  //
  enumPins: IEnumPins;
  pin: IPin;
  pinDir: _PinDirection;
begin
  // create field correction sample grabber
  hr := CoCreateInstance(CLSID_SampleGrabber, nil, CLSCTX_INPROC_SERVER,
    IID_IBaseFilter, pSGfc);
  if FAILED(hr) then
  begin
    AMGetErrorText(hr, PChar(ErrMsg), 512);
    raise EMyOwnException.Create
      ('Не могу создать field correction SampleGrabber - ' + Trim(ErrMsg));
  end; // CLSID_SampleGrabber

  // adjusting sample grabber
  hr := pSGfc.QueryInterface(IID_ISampleGrabber, SampleGrabberFC);
  if FAILED(hr) then
  begin
    AMGetErrorText(hr, PChar(ErrMsg), 512);
    raise EMyOwnException.Create
      ('Не могу получить интерфейс field correction SampleGrabber - ' +
      Trim(ErrMsg));
  end;

  hr := SampleGrabberFC.SetBufferSamples(true);
  if FAILED(hr) then
  begin
    AMGetErrorText(hr, PChar(ErrMsg), 512);
    raise EMyOwnException.Create(Trim(ErrMsg));
  end;

  hr := SampleGrabberFC.SetOneShot(false);
  if FAILED(hr) then
  begin
    AMGetErrorText(hr, PChar(ErrMsg), 512);
    raise EMyOwnException.Create(Trim(ErrMsg));
  end;

  hr := SampleGrabberFC.SetCallback(pFieldCorrectionCB, 0);
  if FAILED(hr) then
  begin
    AMGetErrorText(hr, PChar(ErrMsg), 512);
    raise EMyOwnException.Create(Trim(ErrMsg));
  end;

  hr := pGraph.AddFilter(pSGfc, 'Field correction sample grabber');
  if SUCCEEDED(hr) then
  begin
    WriteToLog('Field correction sample grabber added');
  end
  else
  begin
    AMGetErrorText(hr, PChar(ErrMsg), 512);
    raise EMyOwnException.Create
      ('Не могу добавить Field correction sample grabber в граф - ' +
      Trim(ErrMsg));
  end;

  // search Sample Grabber input and output pins
  SGfcinpin := nil;
  SGfcoutpin := nil;

  hr := pSGfc.enumPins(enumPins);
  if SUCCEEDED(hr) then
  begin
    while (enumPins.Next(1, pin, nil) = S_OK) do
    begin
      pin.QueryDirection(pinDir);
      if pinDir = pindir_input then
        SGfcinpin := pin
      else
        SGfcoutpin := pin;
    end;
  end
  else
  begin
    AMGetErrorText(hr, PChar(ErrMsg), 512);
    raise EMyOwnException.Create
      ('Не могу enum пины field correction sample grabber - ' + Trim(ErrMsg));
  end;

  if not Assigned(SGfcinpin) then
    raise EMyOwnException.Create
      ('Не могу найти field correction sample grabber input pin');

  if not Assigned(SGfcoutpin) then
    raise EMyOwnException.Create
      ('Не могу найти field correction sample grabber output pin');
end;

procedure TDSCapturer.CreateVideoSampleGrabber;
var
  hr: HResult;
  //
  enumPins: IEnumPins;
  pin: IPin;
  pinDir: _PinDirection;
begin
  hr := CoCreateInstance(CLSID_SampleGrabber, nil, CLSCTX_INPROC_SERVER,
    IID_IBaseFilter, pEncodedSG);
  if FAILED(hr) then
  begin
    AMGetErrorText(hr, PChar(ErrMsg), 512);
    raise EMyOwnException.Create('Не могу создать video SampleGrabber - ' +
      Trim(ErrMsg));
  end; // CLSID_SampleGrabber

  // adjusting sample grabber
  hr := pEncodedSG.QueryInterface(IID_ISampleGrabber, EncodedSampleGrabber);
  if FAILED(hr) then
  begin
    AMGetErrorText(hr, PChar(ErrMsg), 512);
    raise EMyOwnException.Create
      ('Не могу получить интерфейс video SampleGrabber - ' + Trim(ErrMsg));
  end;

  hr := EncodedSampleGrabber.SetBufferSamples(true);
  if FAILED(hr) then
  begin
    AMGetErrorText(hr, PChar(ErrMsg), 512);
    raise EMyOwnException.Create(Trim(ErrMsg));
  end;

  hr := EncodedSampleGrabber.SetOneShot(false);
  if FAILED(hr) then
  begin
    AMGetErrorText(hr, PChar(ErrMsg), 512);
    raise EMyOwnException.Create(Trim(ErrMsg));
  end;

  hr := EncodedSampleGrabber.SetCallback(pVideoGrabCB, 0);
  if FAILED(hr) then
  begin
    AMGetErrorText(hr, PChar(ErrMsg), 512);
    raise EMyOwnException.Create(Trim(ErrMsg));
  end;

  hr := pGraph.AddFilter(pEncodedSG, 'Encoded video sample grabber');
  if SUCCEEDED(hr) then
    WriteToLog('Encoded video sample grabber added')
  else
  begin
    AMGetErrorText(hr, PChar(ErrMsg), 512);
    raise EMyOwnException.Create
      ('Не могу добавить encoded video sample grabber в граф - ' +
      Trim(ErrMsg));
  end;

  // search Encoded Sample Grabber input and output pins
  EncodedSGinpin := nil;
  EncodedSGoutpin := nil;
  hr := pEncodedSG.enumPins(enumPins);
  if SUCCEEDED(hr) then
  begin
    while (enumPins.Next(1, pin, nil) = S_OK) do
    begin
      pin.QueryDirection(pinDir);
      if pinDir = pindir_input then
        EncodedSGinpin := pin
      else
        EncodedSGoutpin := pin;
    end;
  end
  else
  begin
    AMGetErrorText(hr, PChar(ErrMsg), 512);
    raise EMyOwnException.Create
      ('Не могу enum пины encoded video sample grabber - ' + Trim(ErrMsg));
  end;

  if not Assigned(EncodedSGinpin) then
    raise EMyOwnException.Create
      ('Не могу найти encoded video sample grabber input pin');

  if not Assigned(EncodedSGoutpin) then
    raise EMyOwnException.Create
      ('Не могу найти encoded video sample grabber output pin');
end;

procedure TDSCapturer.CreateAudioSampleGrabber;
var
  hr: HResult;
  //
  enumPins: IEnumPins;
  pin: IPin;
  pinDir: _PinDirection;
begin
  hr := CoCreateInstance(CLSID_SampleGrabber, nil, CLSCTX_INPROC_SERVER,
    IID_IBaseFilter, pSGAudio);
  if FAILED(hr) then
  begin
    AMGetErrorText(hr, PChar(ErrMsg), 512);
    raise EMyOwnException.Create('Не могу создать Audio SampleGrabber - ' +
      Trim(ErrMsg));
  end; // CLSID_SampleGrabber

  // adjusting audio sample grabber
  hr := pSGAudio.QueryInterface(IID_ISampleGrabber, SampleGrabberAudio);
  if FAILED(hr) then
  begin
    AMGetErrorText(hr, PChar(ErrMsg), 512);
    raise EMyOwnException.Create
      ('Не могу получить интерфейс Audio SampleGrabber - ' + Trim(ErrMsg));
  end;

  // set buffer on
  hr := SampleGrabberAudio.SetBufferSamples(true);
  if FAILED(hr) then
  begin
    AMGetErrorText(hr, PChar(ErrMsg), 512);
    raise EMyOwnException.Create(Trim(ErrMsg));
  end;

  // set oneshot false
  hr := SampleGrabberAudio.SetOneShot(false);
  if FAILED(hr) then
  begin
    AMGetErrorText(hr, PChar(ErrMsg), 512);
    raise EMyOwnException.Create(Trim(ErrMsg));
  end;

  // set call back
  hr := SampleGrabberAudio.SetCallback(pAudioGrabCB, 0);
  if FAILED(hr) then
  begin
    AMGetErrorText(hr, PChar(ErrMsg), 512);
    raise EMyOwnException.Create(Trim(ErrMsg));
  end;

  hr := pGraph.AddFilter(pSGAudio, 'Audio sample grabber');
  if SUCCEEDED(hr) then
  begin
    WriteToLog('Audio sample grabber added');
  end
  else
  begin
    AMGetErrorText(hr, PChar(ErrMsg), 512);
    raise EMyOwnException.Create
      ('Не могу добавить Audio sample grabber в граф - ' + Trim(ErrMsg));
  end;

  // search Audio sample Grabber input and output pins
  SGAudioinpin := nil;
  SGAudiooutpin := nil;

  hr := pSGAudio.enumPins(enumPins);
  if SUCCEEDED(hr) then
  begin
    while (enumPins.Next(1, pin, nil) = S_OK) do
    begin
      pin.QueryDirection(pinDir);
      if pinDir = pindir_input then
        SGAudioinpin := pin
      else
        SGAudiooutpin := pin;
    end;
  end
  else
  begin
    AMGetErrorText(hr, PChar(ErrMsg), 512);
    raise EMyOwnException.Create('Не могу enum пины audio sample grabber - ' +
      Trim(ErrMsg));
  end;

  if not Assigned(SGAudioinpin) then
    raise EMyOwnException.Create
      ('Не могу найти Audio sample grabber input pin');

  if not Assigned(SGAudiooutpin) then
    raise EMyOwnException.Create
      ('Не могу найти Audio sample grabber output pin');
end;

procedure TDSCapturer.CreateDVmuxer;
var
  hr: HResult;
  //
  enumPins: IEnumPins;
  pin: IPin;
  pinDir: _PinDirection;
begin
  hr := CoCreateInstance(CLSID_DVMux, nil, CLSCTX_INPROC_SERVER,
    IID_IBaseFilter, pDVmux);
  if FAILED(hr) then
  begin
    AMGetErrorText(hr, PChar(ErrMsg), 512);
    raise EMyOwnException.Create('Не могу создать DVmux - ' + Trim(ErrMsg));
  end; // CLSID_DVmux

  // add DV Mux to graph
  hr := pGraph.AddFilter(pDVmux, 'DV muxer');
  if SUCCEEDED(hr) then
  begin
    WriteToLog('DV muxer added');
  end
  else
  begin
    AMGetErrorText(hr, PChar(ErrMsg), 512);
    raise EMyOwnException.Create('Не могу добавить DV muxer в граф - ' +
      Trim(ErrMsg));
  end; // CLSID_DVmux

  // search DV muxer video input and output pins
  DVMuxVinpin := nil;
  DVMuxoutpin := nil;
  hr := pDVmux.enumPins(enumPins);
  if SUCCEEDED(hr) then
  begin
    while (enumPins.Next(1, pin, nil) = S_OK) do
    begin
      pin.QueryDirection(pinDir);
      if pinDir = pindir_input then
        DVMuxVinpin := pin
      else
        DVMuxoutpin := pin;
    end;
  end
  else
  begin
    AMGetErrorText(hr, PChar(ErrMsg), 512);
    raise EMyOwnException.Create('Не могу enum пины DV muxer - ' +
      Trim(ErrMsg));
  end;

  if not Assigned(DVMuxVinpin) then
    raise EMyOwnException.Create('Не могу найти DVMuxer video input pin');

  if not Assigned(DVMuxoutpin) then
    raise EMyOwnException.Create('Не могу найти DVMuxer output pin');
end;

procedure TDSCapturer.SearchDVmuxerAudioInputPin;
var
  i: integer;
  hr: HResult;
  //
  enumPins: IEnumPins;
  pin: IPin;
  pinDir: _PinDirection;
begin
  i := 0;
  DVMuxAinpin := nil;

  hr := pDVmux.enumPins(enumPins);
  if SUCCEEDED(hr) then
  begin
    while (enumPins.Next(1, pin, nil) = S_OK) do
    begin
      pin.QueryDirection(pinDir);
      if pinDir = pindir_input then
      begin
        if i = 1 then
        begin
          DVMuxAinpin := pin;
          break;
        end;
        inc(i);
      end;
    end;
  end
  else
  begin
    AMGetErrorText(hr, PChar(ErrMsg), 512);
    raise EMyOwnException.Create('Не могу enum пины DV muxer - ' +
      Trim(ErrMsg));
  end;

  if not Assigned(DVMuxAinpin) then
    raise EMyOwnException.Create('Не могу найти DVMuxer audio input pin');
end;

procedure TDSCapturer.CreateVideoMixingRenderer;
var
  hr: HResult;
  //
  enumPins: IEnumPins;
  pin: IPin;
  pinDir: _PinDirection;
begin
  hr := CoCreateInstance(CLSID_VideoMixingRenderer, nil, CLSCTX_INPROC_SERVER,
    IID_IBaseFilter, pVMR);
  if FAILED(hr) then
  begin
    AMGetErrorText(hr, PChar(ErrMsg), 512);
    raise EMyOwnException.Create('Не могу создать VideoMixingRenderer - ' +
      Trim(ErrMsg));
  end; // CLSID_VideoMixingRenderer

  hr := pGraph.AddFilter(pVMR, 'VMR');
  if SUCCEEDED(hr) then
  begin
    WriteToLog('VMR added');
  end
  else
  begin
    AMGetErrorText(hr, PChar(ErrMsg), 512);
    raise EMyOwnException.Create('Не могу добавить VMR в граф - ' +
      Trim(ErrMsg));
  end;

  // search render input pin
  RenderPin := nil;
  hr := pVMR.enumPins(enumPins);
  if SUCCEEDED(hr) then
  begin
    while (enumPins.Next(1, pin, nil) = S_OK) do
    begin
      pin.QueryDirection(pinDir);
      if pinDir = pindir_input then
      begin
        RenderPin := pin;
        break;
      end;
    end;
  end
  else
  begin
    AMGetErrorText(hr, PChar(ErrMsg), 512);
    raise EMyOwnException.Create('Не могу enum пины VMR - ' + Trim(ErrMsg));
  end;

  if not Assigned(RenderPin) then
    raise EMyOwnException.Create('Не могу найти VMR input pin');
end;

procedure TDSCapturer.CreateNullRenderer;
var
  hr: HResult;
  //
  enumPins: IEnumPins;
  pin: IPin;
  pinDir: _PinDirection;
begin
  hr := CoCreateInstance(CLSID_NullRenderer, nil, CLSCTX_INPROC_SERVER,
    IID_IBaseFilter, pNullRenderer);
  if FAILED(hr) then
  begin
    AMGetErrorText(hr, PChar(ErrMsg), 512);
    raise EMyOwnException.Create('Не могу создать NullRenderer - ' +
      Trim(ErrMsg));
  end;

  hr := pGraph.AddFilter(pNullRenderer, 'NullRenderer');
  if SUCCEEDED(hr) then
  begin
    WriteToLog('Null Renderer added');
  end
  else
  begin
    AMGetErrorText(hr, PChar(ErrMsg), 512);
    raise EMyOwnException.Create('Не могу добавить Null Renderer в граф - ' +
      Trim(ErrMsg));
  end;

  // search renderer input pin
  NullRendererInPin := nil;
  hr := pNullRenderer.enumPins(enumPins);
  if SUCCEEDED(hr) then
  begin
    while (enumPins.Next(1, pin, nil) = S_OK) do
    begin
      pin.QueryDirection(pinDir);
      if pinDir = pindir_input then
      begin
        NullRendererInPin := pin;
        break;
      end;
    end;
  end
  else
  begin
    AMGetErrorText(hr, PChar(ErrMsg), 512);
    raise EMyOwnException.Create('Не могу enum пины render - ' + Trim(ErrMsg));
  end;

  if not Assigned(NullRendererInPin) then
    raise EMyOwnException.Create('Не могу найти null renderer input pin');
end;

procedure TDSCapturer.CloseRequest;
begin
  WasCloseRequest := true;

  StopCaptureGraph;
end;

procedure TDSCapturer.ConnectPins(PinFrom, PinTo: IPin;
  SuccessString, ErrorString: string);
var
  hr: HResult;
begin
  hr := pGraph.Connect(PinFrom, PinTo);
  if SUCCEEDED(hr) then
    WriteToLog(SuccessString)
  else
  begin
    AMGetErrorText(hr, PWideChar(ErrMsg), 512);
    raise EMyOwnException.Create(ErrorString + Trim(ErrMsg));
  end;
end;

function TDSCapturer.CreateCaptureGraph: boolean;
var
  hr: HResult;

  VideoMediaType: TAMMediaType;

  pVMRFilterConfig: IVMRFilterConfig;
  pVideoWindow: IVideoWindow;
  iWindowStyle: integer;
begin
  use_ffdshow := false;
  try
    if Pos('}', ffdshow_CLSID_string) > 0 then
      ffdshow_CLSID := StringToGuid(ffdshow_CLSID_string)
    else
      ffdshow_CLSID := StringToGuid('{' + ffdshow_CLSID_string + '}');
    use_ffdshow := true;
  except
  end;

  if BMCaptureGuidsFromNum(card_no, DLV_CLSID, DLA_CLSID) then
  begin
    DLV_CLSID_use := true;
    DLA_CLSID_use := true;
  end
  else
  begin
    DLV_CLSID_use := false;
    DLV_CLSID_use := false;
  end;

  SGFCused := FieldCorrectionActive and (CapWidth = 720) and (CapHeight = 576)
    and IsEqualGUID(NeededSubType, MEDIASUBTYPE_UYVY);

  was_any_error := true;

  try
    CoInitialize(nil);

    pGraph := nil;
    pBuild := nil;

    // create Capture Graph Builder
    hr := CoCreateInstance(CLSID_CaptureGraphBuilder2, nil,
      CLSCTX_INPROC_SERVER, IID_ICaptureGraphBuilder2, pBuild);
    if FAILED(hr) then
    begin
      AMGetErrorText(hr, PChar(ErrMsg), 512);
      raise EMyOwnException.Create('Не могу создать Capture Graph Builder 2 - '
        + ErrMsg);
    end;

    // create graph
    hr := CoCreateInstance(CLSID_FilterGraph, nil, CLSCTX_INPROC_SERVER,
      IID_IGraphBuilder, pGraph);
    if FAILED(hr) then
    begin
      AMGetErrorText(hr, PChar(ErrMsg), 512);
      raise EMyOwnException.Create('Не могу создать Filter Graph - ' +
        Trim(ErrMsg));
    end;

    // link graph to builder
    hr := pBuild.SetFiltergraph(pGraph);
    if FAILED(hr) then
    begin
      AMGetErrorText(hr, PChar(ErrMsg), 512);
      raise EMyOwnException.Create('Не могу привязать Graph to Builder - ' +
        Trim(ErrMsg));
    end;

    CreateVideoCaptureDevice;

    CreateAudioCaptureDevice;

    if SGFCused then
    begin
      // create Field correction sample grabber
      CreateFieldCorrectionSampleGrabber;

      // connect Decklink to Field correction sample grabber
      ConnectPins(DLVoutpin, SGfcinpin,
        'Decklink video out->Field correction sample grabber in connected',
        'Decklink video out->Field correction sample grabber in failed - ');

      if PreviewOn then
      begin
        // smarttee is only necessary when preview on
        CreateSmartTeeFilter;

        // connect Field correction sample grabber to SmartTee
        ConnectPins(SGfcoutpin, STIn,
          'Field correction sample grabber -> Smart tee in connected',
          'Field correction sample grabber -> Smart tee in failed - ');

        // create video encoder
        CreateVideoEncoder;

        // connect SmartTee to video encoder
        ConnectPins(STOutCapture, VideoEncInPin,
          'SmartTee out -> video encoder in connected',
          'SmartTee out -> video encoder in failed - ');
      end
      else
      begin
        // create video encoder
        CreateVideoEncoder;

        // connect Field correction sample grabber to video encoder
        ConnectPins(SGfcoutpin, VideoEncInPin,
          'Field correction sample grabber -> video encoder in connected',
          'Field correction sample grabber -> video encoder in failed - ');
      end;
    end
    else
    begin
      if PreviewOn then
      begin
        // smarttee is only necessary when preview on
        CreateSmartTeeFilter;

        // connect Decklink to SmartTee
        ConnectPins(DLVoutpin, STIn,
          'Decklink video out-> smart tee in connected',
          'Decklink video out-> smart tee in failed - ');

        // create video encoder
        CreateVideoEncoder;

        // connect SmartTee to video encoder
        ConnectPins(STOutCapture, VideoEncInPin,
          'SmartTee out -> video encoder in connected',
          'SmartTee out -> video encoder in failed - ');
      end
      else
      begin
        // create video encoder
        CreateVideoEncoder;

        // connect Decklink to video encoder
        ConnectPins(DLVoutpin, VideoEncInPin,
          'Decklink video out-> video encoder in connected',
          'Decklink video out-> video encoder in failed - ');
      end;
    end;

    // create video sample grabber
    CreateVideoSampleGrabber;

    ConnectPins(VideoEncOutPin, EncodedSGinpin,
      'Video encoder out -> Encoded sample grabber in connected',
      'Video encoder out -> Encoded sample grabber in failed - ');

    // we are capturing DV?
    hr := VideoEncOutPin.ConnectionMediatype(VideoMediaType);
    if SUCCEEDED(hr) then
    begin
      DVCapturing := IsEqualGUID(VideoMediaType.subtype, MEDIASUBTYPE_dvsd_);
      if DVCapturing then
        WriteToLog('Формат захвата DV');
    end
    else
    begin
      AMGetErrorText(hr, PChar(ErrMsg), 512);
      raise EMyOwnException.Create('Can not get video type - ' + Trim(ErrMsg));
    end;

    if DVCapturing and not AudioIsMultichannel then
    begin
      // disconnect temporary connection
      VideoEncOutPin.Disconnect;
      EncodedSGinpin.Disconnect;

      // create DV muxer
      CreateDVmuxer;

      ConnectPins(VideoEncOutPin, DVMuxVinpin,
        'DV Encoder out -> DV muxer in connected',
        'DV Encoder out -> DV muxer in failed - ');

      // search DV muxer audio input pin
      SearchDVmuxerAudioInputPin;

      ConnectPins(DVMuxoutpin, EncodedSGinpin,
        'DV muxer out -> Encoded Sample grabber in connected',
        'DV muxer out -> Encoded Sample grabber in failed - ');
    end; // if DVENCODING!!!

    // create audio sample grabber
    CreateAudioSampleGrabber;

    ConnectPins(DLAoutpin, SGAudioinpin,
      'Decklink audio out -> Audio sample grabber in connected',
      'Decklink audio out -> Audio sample grabber in failed - ');

    if DVCapturing and not AudioIsMultichannel then
    begin
      ConnectPins(SGAudiooutpin, DVMuxAinpin,
        'Audio sample grabber out -> DV muxer in connected',
        'Audio sample grabber out -> DV muxer in failed - ');
    end;

    if not Assigned(VideoPanel) then
      raise EMyOwnException.Create('Не задана панель отображения видео');

    VideoPanel.Width := 360;
    VideoPanel.height := 288;

    // create Null Renderer
    CreateNullRenderer;

    ConnectPins(EncodedSGoutpin, NullRendererInPin,
      'Encoded sample grabber out -> Null Renderer in connected',
      'Encoded sample grabber out -> Null Renderer in failed - ');

    if PreviewOn then
    begin
      // create Video Mixing Renderer
      CreateVideoMixingRenderer;

      ConnectPins(SToutPreview, RenderPin,
        'SmartTee out -> VMR Renderer in connected',
        'SmartTee out -> VMR Renderer in failed - ');

      // set Video Mixing Renderer parameters
      hr := pVMR.QueryInterface(IID_IVMRFilterConfig, pVMRFilterConfig);
      if SUCCEEDED(hr) then
      begin
        pVMRFilterConfig.SetRenderingMode(VMRMode_Windowed);
        pVMRFilterConfig.SetNumberOfStreams(1);
      end
      else
      begin
        AMGetErrorText(hr, PWideChar(ErrMsg), 512);
        raise EMyOwnException.Create('Не могу получить IID_IVMRFilterConfig - '
          + Trim(ErrMsg));
      end;

      // Get video window
      hr := pVMR.QueryInterface(IID_IVideoWindow, pVideoWindow);
      if FAILED(hr) then
      begin
        AMGetErrorText(hr, PWideChar(ErrMsg), 512);
        raise EMyOwnException.Create('Не могу получить IID_IVideoWindow - ' +
          Trim(ErrMsg));
      end;

      with pVideoWindow do
      begin
        put_Caption('test window');
        put_Left(0);
        put_Width(VideoPanel.Width);
        if ((CapWidth / CapHeight) > 1.4) or VideoIsWS then
        begin
          put_Top(VideoPanel.height div 8);
          put_Height((VideoPanel.height * 3) div 4)
        end
        else
        begin
          put_Top(0);
          put_Height(VideoPanel.height);
        end;
      end;

      pVideoWindow.get_WindowStyle(iWindowStyle);
      iWindowStyle := iWindowStyle and not WS_CAPTION;
      iWindowStyle := iWindowStyle and not WS_BORDER;
      iWindowStyle := iWindowStyle and not WS_SIZEBOX;
      pVideoWindow.put_WindowStyle(iWindowStyle);
      pVideoWindow.HideCursor(false);

      pVideoWindow.put_Owner(VideoPanel.Handle);
    end
    else
    begin
      VideoPanel.Caption := 'Preview off in settings';
    end;

    hr := pGraph.QueryInterface(IID_IMediaControl, pControl);
    if FAILED(hr) then
    begin
      AMGetErrorText(hr, PWideChar(ErrMsg), 512);
      raise EMyOwnException.Create('Не могу получить IID_IMediaControl - ' +
        Trim(ErrMsg));
    end;

    AddGraphToRot(pGraph, GraphID);

    was_any_error := false;
  except
    on E: EMyOwnException do
    begin
      WriteToLog(E.Message);
      ShowMessage(E.Message);
    end;
    else
      ShowMessage('Неизвестная ошибка');
  end;

  WriteToLog('Граф собран');
  CoFreeUnusedLibraries();

  GraphWasCreatedAt := Now();
  LastFrameRXED := Now();
  LastVideoRXED := Now();
  LastAudioRXED := Now();
  LastFrameRXEDtick := GetTickCount();
  LastVideoRXEDtick := GetTickCount();
  LastAudioRXEDtick := GetTickCount();

  GraphIsReady := not was_any_error;
  Result := GraphIsReady;
end;

procedure TDSCapturer.DestroyCaptureGraph;
var
  hr: HResult;
  pEnum: IEnumFilters;
  pFilter: IBaseFilter;
  ErrMsg: string;
begin
  StopCaptureGraph;

  if not Assigned(pGraph) then
    exit;

  hr := pGraph.EnumFilters(pEnum);
  if FAILED(hr) then
  begin
    SetLength(ErrMsg, 512);
    AMGetErrorText(hr, PChar(ErrMsg), 512);
    WriteToLog('GraphDestroy: не могу создать Filter Enum - ' + Trim(ErrMsg));
  end;

  while SUCCEEDED(pEnum.Next(1, pFilter, nil)) do
  begin
    if pFilter = nil then
      break;
    pGraph.RemoveFilter(pFilter);
    pEnum.Reset;
    pFilter := nil;
    Application.ProcessMessages;
  end;

  pEnum := nil;

  RemoveGraphFromRot(GraphID);
  CoUninitialize();

  GraphIsReady := false;

  WriteToLog('Граф разобран');
end;

function TDSCapturer.GetFrameInterval: int64;
begin
  Result := NeededFrameInterval;
end;

function TDSCapturer.GetLargestBufferSize: integer;
var
  i: integer;
  CurMax: Cardinal;
begin
  CurMax := 0;
  for i := 0 to Length(VideoFrames) - 1 do
    if VideoFrames[i].BufferSize > CurMax then
      CurMax := VideoFrames[i].BufferSize;

  Result := CurMax;
end;

procedure TDSCapturer.SetFrameInterval(invalue: int64);
begin
  NeededFrameInterval := invalue;
end;

procedure TDSCapturer.ClearBuffers;
var
  i, j: integer;
begin
  j := Length(VideoFrames);
  if j <> 0 then
  begin
    for i := 0 to j - 1 do
    begin
      if Assigned(VideoFrames[i].Buffer) then
        FreeMem(VideoFrames[i].Buffer);
      VideoFrames[i].Buffer := nil;
    end;

  end;

  j := Length(AudioFrames);
  if j <> 0 then
  begin
    for i := 0 to j - 1 do
    begin
      if Assigned(AudioFrames[i].Buffer) then
        FreeMem(AudioFrames[i].Buffer);
      AudioFrames[i].Buffer := nil;
    end;
  end;

  Buff_count := 0;
  SetLength(VideoFrames, 0);
  SetLength(AudioFrames, 0);
end;

procedure TDSCapturer.PrepareBuffers(Count, VBufferSize, ABufferSize: integer);
var
  i: integer;
begin
  ClearBuffers;

  SetLength(VideoFrames, Count);
  SetLength(AudioFrames, Count);

  for i := 0 to Count - 1 do
  begin
    GetMem(VideoFrames[i].Buffer, VBufferSize);
    VideoFrames[i].BufferSize := VBufferSize;
    VideoFrames[i].Filled := false;
    VideoFrames[i].Used := false;

    GetMem(AudioFrames[i].Buffer, ABufferSize);
    AudioFrames[i].BufferSize := ABufferSize;
    AudioFrames[i].Filled := false;
    AudioFrames[i].Used := false;
  end;

  Buff_count := Count;
end;

procedure TDSCapturer.RunCaptureGraph;
begin
  if GraphIsReady and Assigned(pControl) and (Length(VideoFrames) > 0) then
  begin
    GraphIsOnlyCreated := true;
    GraphWasCreatedAt := Now();

    LastAudioTS := -1;
    LastVideoTS := -1;
    LastFrameTS := -1;

    MarkAllBuffersAsUnused;

    pControl.Run;
  end;
end;

procedure TDSCapturer.StopCaptureGraph;
begin
  if GraphIsReady and Assigned(pControl) then
    pControl.Stop;
end;

procedure TDSCapturer.ColdRestart;
begin
  DestroyCaptureGraph;

  CreateCaptureGraph;

  RunCaptureGraph;
end;

function TDSCapturer.GetVideoFramesInBuffer: integer;
var
  i: integer;
  FrInBuffV: integer;
begin
  FrInBuffV := 0;
  for i := 0 to Length(VideoFrames) - 1 do
    if VideoFrames[i].Filled then
      inc(FrInBuffV);

  Result := FrInBuffV;
end;

function TDSCapturer.GetVideoMediaType(var VideoMediaType
  : TAMMediaType): boolean;
var
  hr: HResult;
begin
  hr := EncodedSampleGrabber.GetConnectedMediatype(VideoMediaType);
  Result := SUCCEEDED(hr);
end;

function TDSCapturer.GetAudioMediaType(var AudioMediaType
  : TAMMediaType): boolean;
var
  hr: HResult;
begin
  hr := SampleGrabberAudio.GetConnectedMediatype(AudioMediaType);
  Result := SUCCEEDED(hr);
end;

procedure TDSCapturer.MarkAllBuffersAsUnused;
var
  i: integer;
begin
  for i := 0 to Length(VideoFrames) - 1 do
  begin
    VideoFrames[i].Filled := false;
    VideoFrames[i].Used := false;
  end;

  for i := 0 to Length(AudioFrames) - 1 do
  begin
    AudioFrames[i].Filled := false;
    AudioFrames[i].Used := false;
  end;
end;

function TDSCapturer.GetAudioFramesInBuffer: integer;
var
  i: integer;
  FrInBuffA: integer;
begin
  FrInBuffA := 0;
  for i := 0 to Length(AudioFrames) - 1 do
    if AudioFrames[i].Filled then
      inc(FrInBuffA);

  Result := FrInBuffA;
end;

function TDSCapturer.GetFirstVideoFrame: POneVideoFrame;
var
  i: integer;
  video_ts: int64;
begin
  video_ts := High(int64);

  Result := nil;
  for i := 0 to Length(VideoFrames) - 1 do
    if VideoFrames[i].Used and VideoFrames[i].Filled then
      if VideoFrames[i].Stamp < video_ts then
      begin
        video_ts := VideoFrames[i].Stamp;
        Result := Addr(VideoFrames[i]);
      end;
end;

function TDSCapturer.GetFirstAudioFrame: POneAudioFrame;
var
  i: integer;
  audio_ts: int64;
begin
  audio_ts := High(int64);

  Result := nil;
  for i := 0 to Length(AudioFrames) - 1 do
    if AudioFrames[i].Used and AudioFrames[i].Filled then
      if AudioFrames[i].Stamp < audio_ts then
      begin
        audio_ts := AudioFrames[i].Stamp;
        Result := Addr(AudioFrames[i]);
      end;
end;

function TDSCapturer.GetUnusedVideoFrame: POneVideoFrame;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to Length(VideoFrames) - 1 do
    if not VideoFrames[i].Used then
    begin
      VideoFrames[i].Used := true;
      Result := Addr(VideoFrames[i]);
      break;
    end;
end;

function TDSCapturer.GetUnusedAudioFrame: POneAudioFrame;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to Length(AudioFrames) - 1 do
    if not AudioFrames[i].Used then
    begin
      AudioFrames[i].Used := true;
      Result := Addr(AudioFrames[i]);
      break;
    end;
end;

function TDSCapturer.CheckForTimeouts: boolean;
var
  tmpstr: string;
begin
  Result := false;
{$Q-}
  if not GraphIsOnlyCreated and (Cardinal(GetTickCount() - LastFrameRXEDtick) >=
    Cardinal(FrameTO * 1000)) then
  begin
    tmpstr := 'Frame timeout. Last frames captured at:' + ' Video: ' +
      FormatDateTime('hh:nn:ss:zzz', LastVideoRXED) + ' Encoded video: ' +
      FormatDateTime('hh:nn:ss:zzz', LastFrameRXED) + ' Audio: ' +
      FormatDateTime('hh:nn:ss:zzz', LastAudioRXED);
    WriteToLog(tmpstr);
    Result := true;
  end;

  if GraphIsReady and GraphIsOnlyCreated and
    (SecondsBetween(GraphWasCreatedAt, Now()) > NodataTO) then
  begin
    tmpstr := 'No DATA timeout. Graph was started at: ' +
      FormatDateTime('hh:nn:ss:zzz', GraphWasCreatedAt);
    WriteToLog(tmpstr);
    Result := true;
  end;
end;

procedure TDSCapturer.WriteToLog(chto: string);
const
  MyLogMessage = WM_USER + 1;
var
  tmpstr: string;
begin
  tmpstr := FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now()) + ' ' + chto;
  ForLog.Append(tmpstr);
  Winapi.Windows.SendMessage((Owner as TForm).Handle, MyLogMessage, 0, 0);
end;

end.
