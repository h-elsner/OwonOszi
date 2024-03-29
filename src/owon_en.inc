resourcestring                                     {english}

  manual='HDS200_series_user_manual.pdf';
{$IFDEF LINUX}
  DownURL='/downloads/owon_oszi.tar.gz';
{$ELSE}
    DownURL='/downloads/owon_oszi.zip';
{$ENDIF}

rsAction='Show saved waveforms from';
rsTime='Time';
rsVolt='Voltage';
rsDelta='X-Delta';
openCSVfile='Open CSV file...';
openBMPfile='Open screenshot...';
capClose='&Close';
hntClose='Close application.';
capImage='&Screenshot';
hntImage='Show screenshots from HDS242.';
capCSVfile='&CSV file';
hntCSVfile='Show a CSV file with saved waveform from HDS242.';
capFile='&File';
capInfo='&Info';
capAbout='&About...';
capManual='&HDS242 manual';
capTeiler='Attenuator';
hntTeiler='Used probe attenuation rate.';
capChannel='Channel ';
hntChannel='Open file from HDS242 or drag&drop it to the application window.';
capSaveScreen='&Save chart...';
capSaveDlg='Save chart as PNG file...';
hntSaveScreen='Save chart as picture to PNG file.';
capKursor='Cursor (+ Shift)';
hntKursor='Set cursor with Shift + left mouse key.'+lineending+
          'Move second cursor with Shift + cursor keys to measure time.';
capDownload='Download updates';
capHomepage='Homepage';
errNoData='No valid data';
capCopy='Save a copy';
hntCopy='Always save a copy of the waveform in the selected local directory.';
hntCopyDir='Select directory for copies.';
hntOK='Close this dialog.';


