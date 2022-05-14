          {********************************************************}
          {                                                        }
          {    Anzeige von Waveformdateien vom Oszi Owon HDS242    }
          {                                                        }
          {       Copyright (c) 2022    Helmut Elsner              }
          {                                                        }
          {       Compiler: FPC 3.2.2   /  Lazarus 2.2.0           }
          {                                                        }
          { Pascal programmers tend to plan ahead, they think      }
          { before they type. We type a lot because of Pascal      }
          { verboseness, but usually our code is right from the    }
          { start. We end up typing less because we fix less bugs. }
          {           [Jorge Aldo G. de F. Junior]                 }
          {********************************************************}

(* Show CSV files with saved waveforms from handheld oscilloscope HDS242.
   You now can see the waveforms on a larger screen and zoom into it.
   Zoom with mouse wheel, pan with Left mouse button,
   cursors with Left mouse button + Shift.
   With cursors you can measure intervals:
   1. Place Red cursor somewhere with Left mouse button + Shift. Let it go.
   2. Move mouse to another place and switch cursor on with
      Left mouse button + Shift: X-Delta shows the interval between the two cursors.


Format CSV files from HDS242 (example):

Channel			  :,CH1
Frequency			  :,F=1.498kHz
Period 			  :,T=667.4us
PK-PK				  :,Vpp=1.280V
Average			  :,V=-9.333mV
Vertical pos		  :,0.00mV
Probe attenuation	  :,1X
Voltage per ADC value:,20.00mV
Time interval		  :,0.20uS

index,CH1_Voltage(mV)
0,-600.00
1,-620.00
...

2022-05-07 first version
2022-05-08 chart tools added
2022-05-09 upload version 1.0 to GitHub
2022-05-10 drag & drop added
2022-05-12 Menu save a copy added
*)

unit owon_oszi_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, lclintf,
  XMLPropStorage, ActnList, Menus, ExtCtrls, Buttons, StdCtrls, EditBtn,
  TAGraph, TASeries, TATools, TADataTools, Types, lazUTF8;

type

  { Tmain }

  Tmain = class(TForm)
    actAbout: TAction;
    actDownload: TAction;
    actHomepage: TAction;
    actSaveScreen: TAction;
    actManual: TAction;
    actClose: TAction;
    btnOK: TBitBtn;
    Chart: TChart;
    cbCopy: TCheckBox;
    CopyDir: TDirectoryEdit;
    gbCopy: TGroupBox;
    lblDelta: TLabel;
    lblDeltaValue: TLabel;
    lnHorPos: TConstantLine;
    CrossHair: TDataPointCrosshairTool;
    gbKursor: TGroupBox;
    lblAverage: TLabel;
    lblFreq: TLabel;
    lblInterval: TLabel;
    lblPeak: TLabel;
    lblPeriod: TLabel;
    lblVertical: TLabel;
    lblVolt: TLabel;
    lblX: TLabel;
    lblXValue: TLabel;
    lblY: TLabel;
    lblYValue: TLabel;
    LeftGUI: TPanel;
    lnVertPos: TConstantLine;
    ChartToolset1: TChartToolset;
    ChartToolset1PanDragTool1: TPanDragTool;
    ChartToolset1ZoomMouseWheelTool1: TZoomMouseWheelTool;
    Image: TImage;
    lblChannel: TLabel;
    actLoadBMP: TAction;
    actLoadCSV: TAction;
    ActionList: TActionList;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    mnCopy: TMenuItem;
    mnHomepage: TMenuItem;
    mnDownload: TMenuItem;
    mnSaveChart1: TMenuItem;
    rgTeiler: TRadioGroup;
    Separator3: TMenuItem;
    mnSaveScreen: TMenuItem;
    PopupMenu1: TPopupMenu;
    SaveDialog: TSaveDialog;
    Separator2: TMenuItem;
    mnManual: TMenuItem;
    mnAbout: TMenuItem;
    mnInfo: TMenuItem;
    mnClose: TMenuItem;
    Separator1: TMenuItem;
    mnImg: TMenuItem;
    mnCSV: TMenuItem;
    mnFile: TMenuItem;
    Pic: TPanel;
    ImageList: TImageList;
    MainMenu: TMainMenu;
    OpenDialog: TOpenDialog;
    TopGUI: TPanel;
    Settings: TXMLPropStorage;
    Wave: TLineSeries;
    procedure actAboutExecute(Sender: TObject);
    procedure actCloseExecute(Sender: TObject);
    procedure actDownloadExecute(Sender: TObject);
    procedure actHomepageExecute(Sender: TObject);
    procedure actManualExecute(Sender: TObject);
    procedure actSaveScreenExecute(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure ChartMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CrossHairAfterMouseUp(ATool: TChartTool; APoint: TPoint);
    procedure CrossHairDraw(ASender: TDataPointDrawTool);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure actLoadBMPExecute(Sender: TObject);
    procedure actLoadCSVExecute(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure mnCopyClick(Sender: TObject);
    procedure rgTeilerClick(Sender: TObject);
  private
    procedure StartFile(fn: string);                   {Start action depending on file name}
    procedure ClearMetaData;
    procedure SetCSVenv;
    procedure SetBMPenv;
    procedure ExchangeIcons;                           {Show status of copy action}
  public
    procedure ZeichneWave(const fn: string; fkt: double);
    procedure SaveCopyWave(list: TStringList);         {Save a copy of the waveform with new file name}
  end;

var
  main: Tmain;
  interv: double;                                      {Time interval per sample}

const
  osziname='Owon HDS242';
  oszifarbe=$00674322;                                 {Gehäusefarbe so gut ich kann}
  ch1color=$00009FFF;
  ch2color=clNavy;
  meinname='Helmut Elsner';
  email='helmut.elsner@live.com';
  homepage='http://h-elsner.mooo.com';
  tab1=' ';
  kop='[';
  kcl=']';
  sep=',';
  dez='.';                                             {Decimal separator in use}
  gleich='=';
  ziff=['0'..'9', '-', '+', dez];                      {Valid digits and signs for float}
  floatform='0.00';
  copyfn='HDS242_wave_';
  csvext='.CSV';
  bmpext='.BMP';

  idChannel='Channel';
  idCH1='CH1';
  idCH2='CH2';
  idFreq='Frequency';
  idPeriod='Period';
  idPeak='PK-PK';
  idAverage='Average';
  idVertical='Vertical pos';
  idTeiler='Probe attenuation';
  id1X='1X';
  id10X='10X';
  idVolt='Voltage per ADC value';
  idInterval='Time interval';
  idIndex='index';


implementation

{$R *.lfm}

{$I owon_dt.inc}                                       {German GUI}
{.$I owon_en.inc}                                      {English GUI}

{ Tmain }

procedure Tmain.FormCreate(Sender: TObject);           {Init GUI with language settings}
var
  fn: string;

begin
  Caption:=osziname;
  OpenDialog.Title:=openCSVfile;
  Image.Visible:=true;
  Chart.Visible:=false;
  TopGui.Hint:=hntChannel;
  Pic.Color:=clWhite;
  actLoadCSV.Caption:=capCSVfile;
  actLoadBMP.Caption:=capImage;
  actClose.Caption:=capClose;
  actLoadCSV.Hint:=hntCSVfile;
  actLoadBMP.Hint:=hntImage;
  actClose.Hint:=hntClose;
  actAbout.Caption:=capAbout;
  actManual.Caption:=capManual;
  actSaveScreen.Caption:=capSaveScreen;
  actSaveScreen.Hint:=hntSaveScreen;
  actDownload.Caption:=capDownload;
  actHomepage.Caption:=capHomepage;
  mnFile.Caption:=capFile;
  mnInfo.Caption:=capInfo;
  rgTeiler.Caption:=capTeiler;
  rgteiler.Hint:=hntTeiler;
  Chart.AxisList[0].Title.Caption:=rsVolt;
  Chart.AxisList[1].Title.Caption:=rsTime;
  Wave.SeriesColor:=ch1color;
  lblX.Caption:=rsTime;
  lblY.Caption:=rsVolt;
  lblDelta.Caption:=rsDelta;
  gbKursor.Caption:=capKursor;
  gbKursor.Hint:=hntKursor;
  mnCopy.Caption:=capCopy;
  mnCopy.Hint:=hntCopy;
  gbCopy.Caption:=capCopy;
  gbCopy.Hint:=hntCopy;
  cbCopy.Caption:=capCopy;
  CopyDir.Hint:=hntCopyDir;
  CopyDir.TextHint:=hntCopyDir;
  btnOK.Hint:=hntOK;
  ClearMetaData;
  fn:=trim(ParamstrUTF8(1));                           {File name added as first (and only) parameter}
  if fn<>'' then
    StartFile(fn);
end;

procedure Tmain.actCloseExecute(Sender: TObject);      {Close application}
begin
  Close;
end;

procedure Tmain.actDownloadExecute(Sender: TObject);   {Download update}
begin
  OpenURL(homepage+DownURL);
end;

procedure Tmain.actHomepageExecute(Sender: TObject);   {Show homepage}
begin
  OpenURL(homepage);
end;

procedure Tmain.actManualExecute(Sender: TObject);     {Open manual}
begin
  OpenDocument(Application.Location+manual);
end;

procedure Tmain.actSaveScreenExecute(Sender: TObject); {Save chart as PNG pic}
begin
  SaveDialog.Title:=capSaveDlg;
  SaveDialog.FileName:=ChangeFileExt(OpenDialog.FileName, '.png');
  if SaveDialog.Execute then
    Chart.SaveToFile(TPortableNetworkGraphic, SaveDialog.FileName);
end;

procedure Tmain.ExchangeIcons;                         {Show status of copy action in menu icon}
begin
  if cbCopy.Checked then
    mnCopy.ImageIndex:=10
  else
    mnCopy.ImageIndex:=8;
  gbCopy.Visible:=false;                               {Close settings window}
end;

procedure Tmain.btnOKClick(Sender: TObject);           {Selection for copy done}
begin
  ExchangeIcons;
end;

procedure Tmain.actAboutExecute(Sender: TObject);      {About box}
begin
  MessageDlg(rsAction+tab1+Osziname+LineEnding+LineEnding+
             meinname+LineEnding+homepage+LineEnding+email,
             mtInformation,[mbOK],0);
end;

procedure Tmain.ChartMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);                  {Reset Zoom}
begin
  if (ssCtrl in Shift) or                              {Klicken mit gedrückter Ctrl}
     (ssMiddle in Shift) then                          {Klicken mit mittlerer Taste}
    Chart.ZoomFull;
end;

procedure Tmain.CrossHairAfterMouseUp(ATool: TChartTool; APoint: TPoint);
var                                                    {Set cursor for Delta}
  cpos: int64;

begin
  cpos:=CrossHair.PointIndex;
  if cpos>=0 then
    lnHorPos.Position:=Wave.XValue[cpos];
end;

procedure Tmain.CrossHairDraw(ASender: TDataPointDrawTool);
var                                                    {Show cursor at mouse pos}
  idx: int64;

begin
  idx:=ASender.PointIndex;
    {If zoomed-in to deep the idx reports -1 between the datapoints,
     this needs to be checked before reading and show the X, Y and delta values}
  if idx>=0 then begin
    lblXValue.Caption:=FormatFloat(floatform, Wave.XValue[idx]);
    lblYValue.Caption:=FormatFloat(floatform, Wave.YValue[idx]);
    lblDeltaValue.Caption:=FormatFloat(floatform, Wave.XValue[idx]-lnHorPos.Position);
  end;
end;

procedure Tmain.FormActivate(Sender: TObject);         {Take over status of copy selection}
begin
  ExchangeIcons;
end;

procedure Tmain.actLoadBMPExecute(Sender: TObject);
begin                                                  {Load screenshots from oszi}
  OpenDialog.Title:=openBMPfile;
  OpenDialog.FilterIndex:=2;
  if OpenDialog.Execute then begin
    SetBMPenv;
    Caption:=osziname+tab1+kop+ExtractFileName(OpenDialog.FileName)+kcl;
    Image.Picture.LoadFromFile(OpenDialog.FileName);
  end;
end;

procedure Tmain.SetCSVenv;                             {Setup for CSV mode}
begin
  rgTeiler.Enabled:=false;
  Image.Visible:=false;
  Chart.Visible:=true;
  actSaveScreen.Enabled:=true;
end;

procedure Tmain.SetBMPenv;                             {Setup for BMP mode}
begin
  rgTeiler.Enabled:=false;
  Image.Visible:=true;
  Chart.Visible:=false;
  actSaveScreen.Enabled:=false;
  ClearMetaData;
  Pic.Color:=oszifarbe;
end;

procedure Tmain.actLoadCSVExecute(Sender: TObject);    {Load CSV file from oszi, main functionality}
begin
  OpenDialog.Title:=openCSVfile;
  OpenDialog.FilterIndex:=1;
  if OpenDialog.Execute then begin
    SetCSVenv;
    Caption:=osziname+tab1+kop+ExtractFileName(OpenDialog.FileName)+kcl;
    ZeichneWave(OpenDialog.FileName, 1);
  end;
end;

procedure Tmain.StartFile(fn: string);                 {Start action depending on file name}
begin
  if ExtractFileExt(fn)=csvext then begin              {CSV mode}
    SetCSVenv;
    Caption:=osziname+tab1+kop+ExtractFileName(fn)+kcl;
    ZeichneWave(fn, 1);
  end else begin
    if ExtractFileExt(fn)=bmpext then begin            {BMP mode}
      SetBMPenv;
      Caption:=osziname+tab1+kop+ExtractFileName(fn)+kcl;
      Image.Picture.LoadFromFile(fn);
    end;
  end;
  Application.BringToFront;
end;

procedure Tmain.FormDropFiles(Sender: TObject; const FileNames: array of string);
begin                                                  {Drop file from file manager}
  StartFile(FileNames[0]);
end;

procedure Tmain.ClearMetaData;                         {Reset display of meta data}
begin
  gbCopy.Visible:=false;
  interv:=0;                                           {Reset global variable for time interval}
  lblChannel.Caption:='<---';
  rgTeiler.ItemIndex:=0;
  lblFreq.Caption:=idFreq;
  lblPeriod.Caption:=idPeriod;
  lblPeak.Caption:=idPeak;
  lblAverage.Caption:=idAverage;
  lblVertical.Caption:=idVertical;
  lblVolt.Caption:=idVolt;
  lblInterval.Caption:=idInterval;
  lblX.Caption:=rsTime;
  lblY.Caption:=rsVolt;
  lblXValue.Caption:='';
  lblYValue.Caption:='';
  lblDeltaValue.Caption:='';
end;

function clean(const v: string): string;               {Filter float from text}
var
  i: integer;

begin
  result:='';
  for i:=1 to length(v) do
    if v[i] in ziff then
      result:=result+v[i];
  if result='' then                                    {Nothing found, set to zero}
    result:='0';
end;

function Einheit(const mu: string): integer;           {Identify measurement units}
var
  s: string;

const
  p=13;                                                {Min position of measurement unit}

begin
  result:=4;                                           {Invalid ID}
  s:=Lowercase(mu);
  if pos('s', s)>p then
    result:=0;
  if pos('ms', s)>p then begin
    result:=1;
    exit;
  end;
  if pos('us', s)>p then begin
    result:=2;
    exit;
  end;
  if pos('ns', s)>p then begin
    result:=3;
    exit;
  end;

  if pos('v', s)>p then
    result:=8;
  if pos('mv', s)>p then
    result:=9;
end;

function EinheitStr(const e: integer): string;         {Unit ID to string}
begin
  result:='';
  case e of
    0: result:='s';
    1: result:='ms';
    2: result:='µs';
    3: result:='ns';
    4: result:='?';                                    {Invalid}
    5: result:='samples';
    8: result:='V';
    9: result:='mV';
  end;
end;

procedure Tmain.SaveCopyWave(list: TStringList);       {Save a copy of the waveform with new file name}
begin
  if (CopyDir.Text<>'') and DirectoryExists(CopyDir.Directory) then
    list.SaveToFile(IncludeTrailingPathDelimiter(CopyDir.Directory)+copyfn+
                    FormatDateTime('yyyymmdd_hhnnss_zzz', now)+csvext);
end;

procedure Tmain.ZeichneWave(const fn: string; fkt: double);
var                                                    {Write/rewrite chart with correction factor fkt}
  inlist: TStringList;
  i, sampleno, eh: integer;
  dsep: char;                                          {Decimal separator}
  data: boolean;
  wavevalue, vertpos: double;

begin
  gbCopy.Visible:=false;
  inlist:=TStringList.Create;
  Screen.Cursor:=crHourGlass;
  dsep:=DefaultFormatSettings.DecimalSeparator;        {Remember decimal separator}
  DefaultFormatSettings.DecimalSeparator:=dez;
  try
    inlist.LoadFromFile(fn);
    if inlist.Count>10 then begin                      {Smaller files do not contain valid data}
      vertpos:=0;
      lnVertPos.Position:=0;
      lnHorPos.Position:=0;
      interv:=1;
      data:=false;
      ClearMetaData;
      Wave.Clear;
      Wave.BeginUpdate;
      for i:=0 to inlist.Count-1 do begin
        if length(inlist[1])>2 then begin
          if data then begin                           {Index column header found, now all following lines are waveform data}
            sampleno:=StrToInt(inlist[i].Split([sep])[0]);
            wavevalue:=StrToFloat(inlist[i].Split([sep])[1]);
            Wave.AddXY(sampleno*interv, (wavevalue+vertpos)*fkt);
          end else begin
            if pos(idIndex, inlist[i])>0 then begin    {Header of data found}
              lblY.Caption:=rsVolt+tab1+kop+EinheitStr(Einheit(inlist[i]))+kcl;
              Chart.AxisList[0].Title.Caption:=lblY.Caption;
              data:=true;
              continue;
            end;
            if pos(idChannel, inlist[i])>0 then begin  {Channel in use}
              if pos(idCH1, inlist[i])>10 then begin   {Channel 1}
                lblChannel.Caption:=capChannel+'1';
                Wave.SeriesColor:=ch1color;
              end else begin                           {Channel 2}
                if pos(idCH2, inlist[i])>10 then begin
                  lblChannel.Caption:=capChannel+'2';
                  Wave.SeriesColor:=ch2color;
                end;
              end;
              continue;
            end;
            if pos(idFreq, inlist[i])>0 then begin     {Frequency if it is available}
              lblFreq.Caption:=inlist[i].Split([sep])[1];
              if pos('?', lblFreq.Caption)<1 then
                lblChannel.Caption:=lblChannel.Caption+'         '+
                StringReplace(lblFreq.Caption, 'F=', '', []);
              continue;
            end;
            if pos(idPeriod, inlist[i])>0 then begin   {Show period as it is, T=...}
              lblPeriod.Caption:=StringReplace(inlist[i].Split([sep])[1], 'us', 'µs', []);
              continue;
            end;
            if pos(idPeak, inlist[i])>0 then begin
              wavevalue:=StrToFloatDef(clean(inlist[i].Split([sep])[1]), 0)*fkt;
              eh:=Einheit(inlist[i]);
              lblPeak.Caption:='Vpp'+gleich+FormatFloat(floatform, wavevalue)+EinheitStr(eh);
              continue;
            end;
            if pos(idAverage, inlist[i])>0 then begin
              lblAverage.Caption:='Avg'+inlist[i].Split([sep])[1];
              continue;
            end;
            if pos(idVertical, inlist[i])>0 then begin
              lblVertical.Caption:='Vpos'+gleich+inlist[i].Split([sep])[1];
              vertpos:=StrToFloatDef(clean(inlist[i].Split([sep])[1]), 0);
              continue;
            end;
            if pos(idVolt, inlist[i])>0 then begin
              lblVolt.Caption:='ADC value'+gleich+inlist[i].Split([sep])[1];
              continue;
            end;
            if pos(idInterval, inlist[i])>0 then begin {Time axis interval}
              interv:=StrToFloatDef(clean(inlist[i].Split([sep])[1]), 1);
              eh:=Einheit(inlist[i]);
              if interv=0 then begin                   {FW error HDS242 for Horizontal time base in nano sec}
                eh:=5;                                 {samples}
                interv:=1;
                lblInterval.Caption:=idInterval+gleich+'1 sample';
              end else begin
                if (interv>99) and (eh>0) then begin   {Optimize interval x axis}
                  eh:=eh-1;
                  interv:=interv/1000;
                end;
                lblInterval.Caption:=idInterval+gleich+FormatFloat(floatform, interv)+EinheitStr(eh);
              end;
              lblX.Caption:=rsTime+tab1+kop+EinheitStr(eh)+kcl;
              Chart.AxisList[1].Title.Caption:=lblX.Caption;
              lblDelta.Caption:=rsDelta+tab1+kop+EinheitStr(eh)+kcl;
              continue;
            end;
            if pos(idTeiler, inlist[i])>0 then begin   {Probe attenuation}
              if pos(id1X, inlist[i])>1 then begin
                rgTeiler.Tag:=0;
                rgTeiler.ItemIndex:=0;
              end else begin
                if pos(id10X, inlist[i])>10 then begin
                  rgTeiler.Tag:=1;
                  rgTeiler.ItemIndex:=1;
                end;
              end;
              rgTeiler.Enabled:=true;
            end;
          end;
        end;
      end;
      if data then begin
        lnVertPos.Position:=vertpos;
        actSaveScreen.Enabled:=true;
        lblXValue.Caption:=FormatFloat(floatform, Wave.XValue[0]);
        lblYValue.Caption:=FormatFloat(floatform, Wave.YValue[0]);
        if cbCopy.Checked and (pos(copyfn, ExtractFileName(fn))=0) then
          SaveCopyWave(inlist);
      end else
        lblChannel.Caption:=errNoData;
    end else
      lblChannel.Caption:=errNoData;
  finally
    Wave.EndUpdate;
    inlist.Free;
    Screen.Cursor:=crDefault;
    DefaultFormatSettings.DecimalSeparator:=dsep;      {Restore decimal separator}
    Application.ProcessMessages;
    Chart.ZoomFull;
  end;
end;

procedure Tmain.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin                                                  {Move red cursor lnHorPos}
  if Chart.Visible then begin
    if ssShift in Shift then begin                     {Shift + ...}
      if key=39 then begin                             {Right}
        lnHorPos.Position:=lnHorPos.Position+interv;
        exit;
      end;
      if key=37 then begin                             {Left}
        lnHorPos.Position:=lnHorPos.Position-interv;
        exit;
      end;
      if key=33 then begin                             {Page up}
        lnHorPos.Position:=lnHorPos.Position+interv*10;
        exit;
      end;
      if key=34 then begin                             {Page down}
        lnHorPos.Position:=lnHorPos.Position-interv*10;
        exit;
      end;
      if key=36 then begin                             {Pos1}
        lnHorPos.Position:=0;
      end;
    end;
  end;
end;

procedure Tmain.FormResize(Sender: TObject);           {Positioning channel label}
begin
  lblChannel.Left:=TopGui.Width div 2;
end;

procedure Tmain.mnCopyClick(Sender: TObject);          {Enable the settings for copy}
begin
  gbCopy.Visible:=true;
end;

procedure Tmain.rgTeilerClick(Sender: TObject);        {Correct probe attenuation rate}
begin
  if rgTeiler.Enabled then begin
    if rgTeiler.ItemIndex<>rgTeiler.Tag then begin
      case rgTeiler.ItemIndex of
        0: ZeichneWave(OpenDialog.FileName, 0.1);
        1: ZeichneWave(OpenDialog.FileName, 10);
      end;
    end;
  end;
end;

end.

