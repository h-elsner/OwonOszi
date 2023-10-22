resourcestring                                     {deutsch}

  manual='HDS242_Manual_de.PDF';
{$IFDEF LINUX}
  DownURL='/downloads/owon_oszi.tar.gz';
{$ELSE}
    DownURL='/downloads/owon_oszi.zip';
{$ENDIF}

rsAction='Auswerten gespeicherter Anzeigen vom';
rsTime='Zeit';
rsVolt='Spannung';
rsDelta='X-Delta';
openCSVfile='CSV-Datei öffnen...';
openBMPfile='Screenshot öffnen...';
capClose='&Beenden';
hntClose='Programm beenden.';
capImage='&Screenshot';
hntImage='Vom HDS242 aufgenommene Screenshot anzeigen.';
capCSVfile='&CSV-Datei';
hntCSVfile='Vom HDS242 als CSV-Datei gespeicherte Waveform anzeigen.';
capFile='&Datei';
capInfo='&Info';
capAbout='&Über...';
capManual='&HDS242 Handbuch';
capTeiler='Teiler';
hntTeiler='Teilerfaktor des Tastkopfes.';
capChannel='Kanal ';
hntChannel='Öffne Datei vom HDS242 oder schiebe sie per Drag&Drop auf das Programmfenster.';
capSaveScreen='&Anzeige speichern...';
capSaveDlg='Anzeige in PNG-Datei speichern...';
hntSaveScreen='Anzeige als Bild in PNG-Datei speichern.';
capKursor='Kursor (+ Shift)';
hntKursor='Kursor mit Shift + linker Maustaste anzeigen.'+lineending+
          'Bewege den 2. Kursor mit Shift + Kursortasten zur Messung von Zeiten.';
capDownload='Download Update';
capHomepage='Homepage';
errNoData='Keine gültigen Daten';
capCopy='Kopie speichern';
hntCopy='Immer eine Kopie der Waveform in dem ausgewählten Verzeichnis speichern.';
hntCopyDir='Speicherort für die Kopien auswählen';
hntOK='Diesen Dialog schliessen.';


