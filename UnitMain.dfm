object FormDecklink: TFormDecklink
  Left = 302
  Top = 267
  BorderStyle = bsSingle
  Caption = #1047#1072#1093#1074#1072#1090' '#1074#1080#1076#1077#1086' '#1089' Decklink'
  ClientHeight = 531
  ClientWidth = 688
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 8
    Top = 8
    Width = 360
    Height = 288
    Caption = 'Panel1'
    TabOrder = 0
  end
  object MemoMain: TMemo
    Left = 8
    Top = 352
    Width = 673
    Height = 154
    Lines.Strings = (
      'Memo1')
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object GroupBoxBuffer: TGroupBox
    Left = 384
    Top = 72
    Width = 297
    Height = 73
    Caption = #1041#1091#1092#1077#1088
    TabOrder = 2
    object LabelVideoFrInBuf: TLabel
      Left = 8
      Top = 24
      Width = 87
      Height = 13
      Caption = 'LabelVideoFrInBuf'
    end
    object LabelAudioFrInBuf: TLabel
      Left = 8
      Top = 48
      Width = 87
      Height = 13
      Caption = 'LabelVideoFrInBuf'
    end
    object ProgressBarVideoBuf: TProgressBar
      Left = 96
      Top = 22
      Width = 193
      Height = 16
      TabOrder = 0
    end
    object ProgressBarAudioBuf: TProgressBar
      Left = 96
      Top = 46
      Width = 193
      Height = 16
      TabOrder = 1
    end
  end
  object GroupBoxFile: TGroupBox
    Left = 384
    Top = 152
    Width = 297
    Height = 65
    Caption = #1058#1077#1082#1091#1097#1080#1081' '#1092#1072#1081#1083
    TabOrder = 3
    object LabelFileName: TLabel
      Left = 8
      Top = 24
      Width = 70
      Height = 13
      Caption = 'LabelFileName'
    end
    object LabelFileFrames: TLabel
      Left = 8
      Top = 40
      Width = 76
      Height = 13
      Caption = 'LabelFileFrames'
    end
    object ProgressBarFile: TProgressBar
      Left = 96
      Top = 40
      Width = 193
      Height = 16
      TabOrder = 0
    end
  end
  object GroupBoxTotal: TGroupBox
    Left = 384
    Top = 16
    Width = 81
    Height = 49
    Caption = #1047#1072#1093#1074#1072#1095#1077#1085#1086
    TabOrder = 4
    object LabelTotal: TLabel
      Left = 8
      Top = 24
      Width = 50
      Height = 13
      Caption = 'LabelTotal'
    end
  end
  object GroupBoxTC: TGroupBox
    Left = 472
    Top = 16
    Width = 209
    Height = 49
    Caption = 'Timecode'
    TabOrder = 5
    object LabelTC: TLabel
      Left = 8
      Top = 24
      Width = 40
      Height = 13
      Caption = 'LabelTC'
    end
  end
  object GroupBoxAudio: TGroupBox
    Left = 383
    Top = 224
    Width = 297
    Height = 73
    Caption = #1047#1074#1091#1082#1086#1074#1099#1077' '#1082#1072#1085#1072#1083#1099
    TabOrder = 6
    object CheckBox0102: TCheckBox
      Left = 8
      Top = 24
      Width = 49
      Height = 17
      Alignment = taLeftJustify
      Caption = '01-02'
      Enabled = False
      TabOrder = 0
    end
    object CheckBox0304: TCheckBox
      Left = 80
      Top = 24
      Width = 49
      Height = 17
      Alignment = taLeftJustify
      Caption = '03-04'
      Enabled = False
      TabOrder = 1
    end
    object CheckBox0506: TCheckBox
      Left = 152
      Top = 24
      Width = 49
      Height = 17
      Alignment = taLeftJustify
      Caption = '05-06'
      Enabled = False
      TabOrder = 2
    end
    object CheckBox0708: TCheckBox
      Left = 224
      Top = 24
      Width = 49
      Height = 17
      Alignment = taLeftJustify
      Caption = '07-08'
      Enabled = False
      TabOrder = 3
    end
    object CheckBox0910: TCheckBox
      Left = 8
      Top = 48
      Width = 49
      Height = 17
      Alignment = taLeftJustify
      Caption = '09-10'
      Enabled = False
      TabOrder = 4
    end
    object CheckBox1112: TCheckBox
      Left = 80
      Top = 48
      Width = 49
      Height = 17
      Alignment = taLeftJustify
      Caption = '11-12'
      Enabled = False
      TabOrder = 5
    end
    object CheckBox1314: TCheckBox
      Left = 152
      Top = 48
      Width = 49
      Height = 17
      Alignment = taLeftJustify
      Caption = '13-14'
      Enabled = False
      TabOrder = 6
    end
    object CheckBox1516: TCheckBox
      Left = 224
      Top = 48
      Width = 49
      Height = 17
      Alignment = taLeftJustify
      Caption = '15-16'
      Enabled = False
      TabOrder = 7
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 512
    Width = 688
    Height = 19
    Panels = <
      item
        Width = 600
      end
      item
        Alignment = taRightJustify
        Width = 50
      end>
  end
  object PanelControl: TPanel
    Left = 8
    Top = 302
    Width = 360
    Height = 41
    TabOrder = 8
    object Label1: TLabel
      Left = 136
      Top = 10
      Width = 36
      Height = 16
      Caption = 'Prefix:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object ButtonStartStop: TButton
      Left = 8
      Top = 8
      Width = 89
      Height = 25
      Caption = 'Record'
      TabOrder = 0
      OnClick = ButtonStartStopClick
    end
    object EditPrefix: TEdit
      Left = 178
      Top = 8
      Width = 175
      Height = 24
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      Text = 'EditPrefix'
    end
  end
  object ButtonEmail: TButton
    Left = 552
    Top = 309
    Width = 104
    Height = 25
    Caption = 'Send test email'
    TabOrder = 9
    OnClick = ButtonEmailClick
  end
  object ButtonRestart: TButton
    Left = 445
    Top = 309
    Width = 75
    Height = 25
    Caption = 'Restart graph'
    TabOrder = 10
    OnClick = ButtonRestartClick
  end
  object TimerMain: TTimer
    Enabled = False
    OnTimer = TimerMainTimer
    Left = 112
    Top = 48
  end
end
