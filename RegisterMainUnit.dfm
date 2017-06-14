object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Maincocept DV codec settings'
  ClientHeight = 576
  ClientWidth = 490
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 8
    Top = 8
    Width = 465
    Height = 457
    Lines.Strings = (
      'Memo1')
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object ButtonSetInvert: TButton
    Left = 8
    Top = 480
    Width = 121
    Height = 25
    Caption = 'Invert fields = ON'
    TabOrder = 1
    OnClick = ButtonSetInvertClick
  end
  object ButtonClearInvert: TButton
    Left = 8
    Top = 511
    Width = 121
    Height = 25
    Caption = 'Invert fields = OFF'
    TabOrder = 2
    OnClick = ButtonClearInvertClick
  end
  object ButtonForce169: TButton
    Left = 152
    Top = 480
    Width = 105
    Height = 25
    Caption = 'Force 16:9 flag'
    TabOrder = 3
    OnClick = ButtonForce169Click
  end
  object ButtonForce43: TButton
    Left = 152
    Top = 511
    Width = 105
    Height = 25
    Caption = 'Force 4:3 flag'
    TabOrder = 4
    OnClick = ButtonForce43Click
  end
  object ButtonARAuto: TButton
    Left = 152
    Top = 542
    Width = 105
    Height = 25
    Caption = '16:9 4:3 Auto'
    TabOrder = 5
    OnClick = ButtonARAutoClick
  end
  object ButtonDVSD: TButton
    Left = 280
    Top = 480
    Width = 75
    Height = 25
    Caption = 'DVSD'
    TabOrder = 6
    OnClick = ButtonDVSDClick
  end
  object ButtonDVCPRO: TButton
    Left = 280
    Top = 511
    Width = 75
    Height = 25
    Caption = 'DVCPRO'
    TabOrder = 7
    OnClick = ButtonDVCPROClick
  end
  object ButtonClampYUV: TButton
    Left = 376
    Top = 480
    Width = 97
    Height = 25
    Caption = 'Clamp YUV'
    TabOrder = 8
    OnClick = ButtonClampYUVClick
  end
  object ButtonNoClampYUV: TButton
    Left = 376
    Top = 511
    Width = 97
    Height = 25
    Caption = 'Not Clamp YUV'
    TabOrder = 9
    OnClick = ButtonNoClampYUVClick
  end
end
