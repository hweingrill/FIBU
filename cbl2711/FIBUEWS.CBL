      $SET LINKCOUNT"240" ANS85 BOUND AUTOLOCK NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     FIBUEWS.
      ******************************************************************
      *         öberweisungstrÑger oder DatentrÑger                    *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.   PC.
       SPECIAL-NAMES.     DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY FIBUSEC.CPY.
           SELECT ZAHLPOS   ASSIGN TO "FIBUZAHL.DAT"
                            ORGANIZATION INDEXED ACCESS DYNAMIC
                            RECORD KEY ZP-KEY
                            ALTERNATE RECORD ZP-AKEY
                            FILE STATUS IS WF-STATUS.
           SELECT ZUSTX     ASSIGN TO "ZUSHILF.DAT"      *> Hilfsdatei
                            ORGANIZATION LINE SEQUENTIAL
                            FILE STATUS WF-STATUS.
           SELECT DATTR     ASSIGN TO WN-DATTR           *> DatentrÑger
                            ORGANIZATION LINE SEQUENTIAL
                            ACCESS SEQUENTIAL
                            FILE STATUS WF-STATUS.
           SELECT DRUCKER   ASSIGN TO PRINTER WH-DRUNAM  *> Protokoll
                            FILE STATUS IS WF-STATUS.
           SELECT DRUCKB    ASSIGN TO PRINTER "DTT-BGL.LST"
                            FILE STATUS IS WF-STATUS.   *> Begleitzettel
           SELECT DRUCKF    ASSIGN TO PRINTER "DTT-FEH.LST"
                            FILE STATUS IS WF-STATUS.   *> Fehlerprotok.
       DATA DIVISION.
       FILE SECTION.
       COPY "FIBUFD.CPY".
      ********************************************* Zahlungspositionen *
       FD  ZAHLPOS      external       LABEL RECORD STANDARD.
       01  ZP-SATZ.
           03  ZP-AKEY.
               05 ZP-BAKEY             PIC 9999       COMP.
               05 ZP-KEY.
                  07 ZP-DK             PIC 99         COMP.
                  07 ZP-KTONR          PIC 9(6)       COMP.
                  07 ZP-VALDAT         PIC 9(8)       COMP.
                  07 ZP-LFD            PIC 9999       COMP.
         02 ZP-REST.
           03  ZP-BETRAG               PIC S9(7)V99   COMP.
           03  ZP-SKTO                 PIC S9(7)V99   COMP.
           03  ZP-EMPFG                PIC X(52).
           03  ZP-FSYM                 PIC XXX.
      *    03  ZP-BLZ                  PIC 9(5)       COMP.
           03  ZP-VERWEND              PIC X(60).
           03  ZP-DATUM                PIC 9(8)       COMP.
           03  ZP-REDAT                PIC 9(6)       COMP.
           03  ZP-BAKTO                PIC X(15).
           03  ZP-BABEZ                PIC X(30).
      ******************************************************************
       FD  DATTR                       LABEL RECORD STANDARD.
       01  DTR-SATZ                    PIC X(120).
      ******************************************************************
       FD  ZUSTX                       LABEL RECORD STANDARD.
       01  ZU-SATZ                     PIC X(120).
      ******************************************************************
       FD  DRUCKER                     LABEL RECORD OMITTED.
       01  DR-SATZ                     PIC X(132).
       01  DRA-SATZ.
           03  FILLER                  PIC X(5).
           03  DRA-LFDNR               PIC Z(4)9-.
           03  DRA-LEITWEG             PIC Z(11)9-.
           03  DRA-KONTONR             PIC Z(10)9-.
           03  DRA-BETRAG              PIC Z.ZZZ.ZZ9,99-.
           03  DRA-NAME                PIC X(35).
           03  FILLER                  PIC X.
           03  DRA-VERWENDUNG          PIC X(38).
           03  DRA-KUNDENDATEN         PIC X(2).
           03  DRA-FUNKTION            PIC 9.
           03  DRA-FORMULAR            PIC 9-.
           03  DRA-TXTZEILEN           PIC Z9.
           03  FILLER                  PIC X(6).
       01  DRF-SATZ.
           03  FILLER                  PIC X(5).
           03  FILLER                  PIC X(5).
           03  DRF-KEY2                PIC 9(14).
           03  FILLER                  PIC X(6).
           03  DRF-KUNDE3              PIC Z(8).9.
           03  DRF-FIRMA3              PIC /999.
           03  FILLER                  PIC XX.
           03  DRF-BANKNR              PIC /9.
           03  FILLER                  PIC X(4).
           03  DRF-KUNDE1              PIC Z(8).9.
           03  DRF-FIRMA1              PIC /999.
           03  FILLER                  PIC X(71).
       01  DRK-SATZ.
           03  FILLER                  PIC X(5).
           03  DRK-LISTBEZ             PIC X(39).
           03  DRK-TXVON               PIC X(5).
           03  DRK-VONDAT              PIC X(8).
           03  FILLER                  PIC X.
           03  DRK-TXBIS               PIC X(5).
           03  DRK-BISDAT              PIC 99B99B99.
           03  FILLER                  PIC X(5).
           03  DRK-TEXT1               PIC X(11).
           03  DRK-BLZ                 PIC Z(6)9.
           03  FILLER                  PIC X(25).
           03  DRK-TEXT2               PIC X(6).
           03  DRK-SEITNR              PIC Z(3)9.
       01  DRH-SATZ.
           03  FILLER                  PIC X(5).
           03  DRH-TEXT1               PIC X(6).
           03  DRH-KONTO               PIC Z(10)9.
           03  FILLER                  PIC X(4).
           03  DRH-ABSENDER            PIC X(38).
           03  FILLER                  PIC X(4).
           03  DRH-TEXT2               PIC X(7).
           03  DRH-DATUM               PIC X(8).
           03  DRH-ZEIT                PIC /99B99.
           03  FILLER                  PIC X(4).
           03  DRH-TEXT3               PIC X(13).
           03  DRH-KTRNR               PIC X(8).
           03  FILLER                  PIC X(3).
           03  DRH-TEXT4               PIC X(20).
       01  DRS-SATZ.
           03  FILLER                  PIC X(5).
           03  FILLER                  PIC X(10).
           03  DRS-TEXT                PIC X(21).
           03  DRS-BETRAEGE            PIC ZZZZ.ZZZ.ZZ9,99.
           03  FILLER                  PIC X(3).
           03  DRS-LEITWEGE            PIC Z(11)9.
           03  FILLER                  PIC X(3).
           03  DRS-KTONR               PIC Z(10)9.
           03  FILLER                  PIC X(3).
           03  DRS-STK                 PIC ZZZ.ZZ9.
           03  FILLER                  PIC X(3).
           03  DRS-TXTZEILEN           PIC ZZ.ZZZ.ZZ9.
           03  FILLER                  PIC X(34).
      *----------------------------------> neues öberweisungsformular <-
       01  DRN-SATZ.
           03  FILLER                  PIC X(34).
           03  DRN-VERWEND.
               05 DRN-BET              PIC *B*B*B*B*B*B*B*B*B*B*.
       01  DRN-ASATZ.
           03  DRN-ATX.
               05 DRN-KTO              PIC X(22).
               05 DRN-BLZL             PIC X(12).
      *----------------------------------> neues öberweisungsformular <-
       01  DRU-SATZ.
           03  FILLER                  PIC XXX.
           03  DRU-VERWEND.
               05 FILLER               PIC X(34).
               05 DRU-BET              PIC *B*B*B*B*B*B*B*B*B*B*.
       01  DRU-ASATZ.
           03  FILLER                  PIC XXX.
           03  DRU-EKTO                PIC X(22).
           03  DRU-EMPF                PIC X(33).
       01  DRU-BSATZ.
           03  FILLER                  PIC XXX.
           03  DRU-BLZL                PIC X(9).
      ******************************************************************
       FD  DRUCKF                      LABEL RECORD OMITTED.
       01  DF-SATZ                     PIC X(132).
       01  DFA-SATZ.
           03  FILLER                  PIC X(5).
           03  DFA-LFDNR               PIC Z(4)9.
           03  FILLER                  PIC X(6).
           03  DFA-LEITWEG             PIC Z(7)9-.
           03  DFA-KONTONR             PIC Z(10)9-.
           03  DFA-BETRAG              PIC ZZZ.ZZZ.ZZ9,99-.
           03  DFA-NAME                PIC X(38).
           03  FILLER                  PIC X.
           03  DFA-VERWENDUNG          PIC X(28).
           03  DFA-KUNDENDATEN         PIC Z(11)9-.
           03  DFA-FUNKTION            PIC 9.
           03  DFA-FORMULAR            PIC 9-.
           03  DFA-TXTZEILEN           PIC Z9.
       01  DFF-SATZ.
           03  FILLER                  PIC X(5).
           03  FILLER                  PIC X(5).
           03  DFF-KEY2                PIC 9(14).
           03  FILLER                  PIC X(6).
           03  DFF-KUNDE3              PIC Z(8).9.
           03  DFF-FIRMA3              PIC /999.
           03  FILLER                  PIC XX.
           03  DFF-BANKNR              PIC /9.
           03  FILLER                  PIC X(4).
           03  DFF-KUNDE1              PIC Z(8).9.
           03  DFF-FIRMA1              PIC /999.
           03  FILLER                  PIC X(71).
       01  DFK-SATZ.
           03  FILLER                  PIC X(5).
           03  DFK-LISTBEZ             PIC X(39).
           03  DFK-TXVON               PIC X(5).
           03  DFK-VONDAT              PIC 99B99B99.
           03  FILLER                  PIC X.
           03  DFK-TXBIS               PIC X(5).
           03  DFK-BISDAT              PIC 99B99B99.
           03  FILLER                  PIC X(5).
           03  DFK-TEXT1               PIC X(11).
           03  DFK-BLZ                 PIC Z(6)9.
           03  FILLER                  PIC X(25).
           03  DFK-TEXT2               PIC X(6).
           03  DFK-SEITNR              PIC Z(3)9.
       01  DFH-SATZ.
           03  FILLER                  PIC X(5).
           03  DFH-TEXT1               PIC X(6).
           03  DFH-KONTO               PIC Z(10)9.
           03  FILLER                  PIC X(4).
           03  DFH-ABSENDER            PIC X(38).
           03  FILLER                  PIC X(4).
           03  DFH-TEXT2               PIC X(7).
           03  DFH-DATUM               PIC X(8).
           03  DFH-ZEIT                PIC /99B99.
           03  FILLER                  PIC X(4).
           03  DFH-TEXT3               PIC X(13).
           03  DFH-KTRNR               PIC 9(8).
           03  FILLER                  PIC X(3).
           03  DFH-TEXT4               PIC X(20).
      ******************************************************************
       FD  DRUCKB                      LABEL RECORD OMITTED.
       01  DB-SATZ                     PIC X(132).
       01  DBK-SATZ.
           03  FILLER                  PIC X(5).
           03  DBK-STRICH.
               05 DBK-LISTBEZ          PIC X(68).
               05 DBK-DATUMTX          PIC X(7).
               05 DBK-DATUM            PIC X(8).
               05 FILLER               PIC X(5).
               05 DBK-STX              PIC X(7).
               05 DBK-SEITE            PIC ZZ9.
           03  FILLER                  PIC X(34).
       01  DBA-SATZ.
           03  FILLER                  PIC X(5).
           03  DBA-TXT1                PIC X(19).
           03  DBA-TXT2.
               05 DBA-DATUM            PIC 99B99B99.
               05 DBA-R1 REDEFINES DBA-DATUM.
                  07 DBA-ANZAHL        PIC Z.ZZ9.
                  07 FILLER            PIC X(3).
               05 FILLER               PIC X(31).
           03  DBA-TXT3                PIC X(20).
           03  DBA-TXT4.
               05 DBA-DDAT             PIC 99B99B99.
               05 DBA-R2 REDEFINES DBA-DDAT.
                  07 DBA-ANZ           PIC Z.ZZ9.
                  07 FILLER            PIC XXX.
               05 FILLER               PIC X(12).
           03  FILLER                  PIC X(34).
       01  DBB-SATZ.
           03  FILLER                  PIC X(5).
           03  DBB-STRICH.
               05 DBB-LFDNR            PIC ZZ9-.
               05 DBB-BKN              PIC X(9).
               05 DBB-VALUTA           PIC 9(8)-.
               05 DBB-FORM             PIC 99-.
               05 DBB-BETRAG           PIC ZZZ.ZZZ.ZZZ.ZZ9,99-.
               05 DBB-UMS              PIC Z.ZZ9-.
               05 DBB-INFO             PIC Z.ZZ9-.
           03  FILLER                  PIC X(76).
       01  DBC-SATZ.
           03  FILLER                  PIC X(5).
           03  DBC-TXT1                PIC X(20).
           03  DBC-BEST                PIC Z.ZZ9-.
           03  FILLER                  PIC X(10).
           03  DBC-TXT2                PIC X(20).
           03  DBC-TXT2B.
               05 DBC-BETR             PIC ZZZ.ZZZ.ZZ9,99-.
               05 FILLER               PIC X.
           03  R2 REDEFINES DBC-TXT2B.
               05 DBC-UMS              PIC Z.ZZ9-.
               05 FILLER               PIC X(10).
           03  DBC-TXT3                PIC X(20).
           03  DBC-TXT3B.
               05 DBC-INFO-BETR        PIC ZZZ.ZZZ.ZZ9,99-.
               05 FILLER               PIC X.
           03  R3 REDEFINES DBC-TXT3B.
               05 DBC-INFO             PIC Z.ZZ9-.
               05 FILLER               PIC X(10).
           03  FILLER                  PIC X(24).
      ******************************************************************
       WORKING-STORAGE SECTION.
       01  WH-CALL.
           03  WL-CA                   PIC 99.
           03  WL-REST                 PIC 9(13).
       COPY WHCREG.CPY.
       01  WF-REG.
           03  WT-FXT.
               05 WH-FXT               PIC X(57)     OCCURS 15.
           03  WV-BAKEY                PIC 9999     COMP.
           03  WB-Z                    PIC X.
           03  WA-TAGE                 PIC 9(8)     COMP.
           03  WB-TAGE                 PIC 9(8)     COMP.
      *---------------------------------- EDIFACT - Byte-Stream-Files <-
           03  WE-ANZ                  PIC Z(6)9.
           03  WE-BET                  PIC Z(6)9,99.
           03  WE-OPEN                 PIC 9.
           03  WB-SATZ                 PIC X(160).
           03  WB-AC                   PIC X     COMP-X.
           03  WB-DE                   PIC X     COMP-X.
           03  WB-DV                   PIC X     COMP-X.
           03  WB-FH                   PIC X(4).
           03  WB-OFS                  PIC X(8)  COMP-x.
           03  WB-CNT                  PIC X(4)  COMP-x.
           03  WB-FLG                  PIC X     COMP-x.
           03  WN-EDIFACT              PIC X(15) VALUE "PAYMULV3.DAT".
           03  WN-NACH                 PIC X(20).
           03  WK-UNH-T                PIC X(20).
           03  WK-UNB-Z                PIC X(20).
           03  WH-CNT                  PIC 9999.
           03  WH-SEQ                  PIC 9999.
           03  WM-CD                   PIC X(35).
           03  WK-DSTG.
               05 WK-STG               PIC X(10)    OCCURS 10.
           03  WH-P1                   PIC 99       COMP.
           03  WH-P2                   PIC 99       COMP.
           03  WM-OPEN                 PIC 9        COMP.
           03  WM-TX                   PIC 99       COMP.
           03  WH-DRUNAM               PIC X(15).
           03  WX-PRNO                 PIC 99       COMP-X.
           03  WX-PRSTAT               PIC 99       COMP-X.
           03  WZ-SEITE                PIC 99       COMP-X VALUE 0.
           03  WZ-SCHALT               PIC 99       COMP-X VALUE 0.
           03  WZ-ZEILEN               PIC 99       COMP-X VALUE 0.
           03  WD-BET                  PIC ZZ.ZZZ.ZZ9,99-.
           03  WT-BET                  PIC ZZZZZZZ9,99-.
           03  WD-KTONR                PIC ZZ.ZZ9,9-.
           03  WH-PX                   PIC XX       OCCURS 2.
           03  WX-VWD.
               05 WM-VERWEND           PIC X(42)    OCCURS 6.
           03  WH-MOD                  PIC 99        COMP.
           03  WH-LOE                  PIC 99        COMP.
           03  WI                      PIC 99        COMP.
           03  WE                      PIC 99        COMP.
           03  WL                      PIC 99        COMP.
           03  WS                      PIC 99        COMP.
           03  IX                      PIC 99        COMP.
           03  IY                      PIC 99        COMP.
           03  IQ                      PIC 9999      COMP.
           03  IC                      PIC 9999      COMP.
           03  WM-FSYM                 PIC XXX.
           03  WV-VERDAT               PIC 9(8)      COMP.
           03  WV-KTONR                PIC 9(6)      COMP.
           03  WM-KONTO                PIC 9(6)      COMP.
           03  WB-KTONR                PIC X(22).
           03  WB-FBEZ                 PIC X(110).
           03  WB-FSACHB               PIC X(25).
           03  WB-FTEL                 PIC X(25).
           03  WD-TGN                  PIC ZZ9.
           03  WS-BET                  PIC S9(9)V99  COMP.
           03  WZ-BET                  PIC S9(9)V99  COMP.
           03  WD-DRN                  PIC X(22).
           03  WM-BANK                 PIC 9.
           03  WD-KZ                   PIC 9.
           03  WD-SKTO                 PIC ZZ.ZZ9,99-.
           03  WD-BELNR                PIC ZZ.ZZ9-.
           03  WZ-SKTO                 PIC S9(9)V99  COMP.
           03  WD-SKT                  PIC 9,9-.
           03  WT-EINZUG.
               05 WT-EKEY              PIC X(10)        OCCURS 50.
               05 WT-ESUMME            PIC S9(9)V99  COMP.
           03  WU-KTO                  PIC 999999.
           03  WV-KTO                  PIC 999999.
           03  WD-ANZ                  PIC ZZZ9.
      *-----------------------------------------> Konditionentabelle <-
           03  WH-KK                   PIC 9(13).
           03  WR-KK REDEFINES WH-KK.
               05  WH-SK1              PIC 9V9.
               05  WH-TG1              PIC 999.
               05  WH-SK2              PIC 9V9.
               05  WH-TG2              PIC 999.
               05  WH-TGN              PIC 999.
           03  WF-TX.
               05 WF-NR                PIC 9(5).
               05 WF-RE                PIC 9(15).
       01  WB-REG.
           03  WB-BANK-NR              PIC 9.
           03  WB-ABSBKTO              PIC 9(11).
           03  WB-ABSBLZL              PIC 9(5).
      *************** Belegung DatentrÑger lt DatentrÑgerabkommen 1.88 *
      *-----------------------------------------------------> Vorsatz <-
       01  WV-SATZ.
           03  WV-BELEGART             PIC 99.
                                 88 EINZUG  VALUE 89.
           03  WV-KTRSUMME             PIC 9(14).
           03  WV-BESTANDKTRNR.
               05  WV-BESTANDKENNUNG   PIC XX.
               05  WV-BESTANDVALUTA    PIC 9(6).
           03  WV-BLZEMPF              PIC 9(7).
           03  WV-KTOEMPF              PIC 9(11).
           03  WV-ERSTELLDAT           PIC 9(6).
           03  WV-VALUTA               PIC 9(6).
           03  WV-BLZABS               PIC 9(7).
           03  WV-KTOABS               PIC 9(11).
           03  WV-NAME-ABSENDER        PIC X(38).
           03  WV-VALUTA-BEG           PIC 9(8).
           03  WV-SA                   PIC 9(2).
      *-----------------------------------------------> Umsatz-Satz-1 <-
       01  WU-SATZ.
           03  WU-BELEGART             PIC 99.
           03  WU-RESERVE              PIC 999.
           03  WU-BETRAG               PIC S9(9)V99.
           03  WU-LEITWEG              PIC 9(12).
           03  WU-LEITWEG-R REDEFINES WU-LEITWEG.
               05  WU-LEITWEG-A        PIC 9(7).
               05  WU-LEITWEG-B        PIC 9(5).
           03  WU-KONTONR-17           PIC 9(11).
           03  WU-BLZ                  PIC 9(5).
           03  WU-KONTONR-19           PIC 9(11).
           03  WU-LEER                 PIC X(7).
           03  WU-VERWENDUNG           PIC X(28).
           03  WU-KUNDENDATEN          PIC X(12).
           03  WU-KLAUSELDAT           PIC 9(6).
           03  WU-FUNKTION             PIC 9.
           03  WU-FORMULAR             PIC 9.
           03  WU-BUCHUNGSDAT          PIC 9(6).
           03  WU-TEXTZEILEN           PIC 9(2).
           03  WU-SA                   PIC 9(2).
      *---------------------> Umsatz-Satz-2 (Umsatz-Fortsetzungssatz) <-
       01  WF-SATZ.
           03  WF-BELEGART             PIC 99.
           03  WF-NAME-30              PIC X(35).
           03  WF-BLANK-30             PIC X(3).
           03  WF-NAME-31              PIC X(35).
           03  WF-BLANK-31             PIC X(3).
           03  WF-RES                  PIC 9(2).
           03  WF-NAME-33              PIC X(35).
           03  WF-BLANK-33             PIC X(3).
           03  WF-SAX                  PIC 9(2).
      *---------------------------------------------------> Text-SAtz <-
       01  WT-SATZ.
           03  WT-ZNRA                 PIC 99.
           03  WT-ZINF-A               PIC X(57).
           03  WT-ZNRB                 PIC 99.
           03  WT-ZINF-B               PIC X(57).
           03  WT-SA                   PIC 9(2).
      *-------------------------------------------------> Summen-Satz <-
       01  WS-SATZ.
           03  WS-BELEGART             PIC 99.
           03  WS-KTRSUMME-BTR         PIC 9(12)V99.
           03  WS-BESTANDKTRNR.
               05  WS-BESTANDKENNUNG   PIC XX.
               05  WS-BESTANDVALUTA    PIC 9(6).
           03  WS-BLZEMPF              PIC 9(7).
           03  WS-KTOEMPF              PIC 9(11).
           03  WS-ERSTELLDAT           PIC 9(6).
           03  WS-ANZ-UMS              PIC 9(6).
           03  WS-BLZABS               PIC 9(7).
           03  WS-KTOABS               PIC 9(11).
           03  WS-KTRSUMME-LW          PIC 9(14).
           03  WS-KTRSUMME-KTO         PIC 9(14).
           03  WS-NULL                 PIC 9(10).
           03  WS-SUMME-ZUSINF         PIC 9(8).
           03  WS-SA                   PIC 9(2).
      *-----------------------------------------------------------------
       01  WH-FELDER.
           03  WM-X.
               05 WM-A                 PIC 99        COMP-X.
           03  WM-BESTAND              PIC 9  COMP-3 VALUE 0.
           03  WH-TIME.
               05  WH-HHMM             PIC 9(4)      VALUE 0.
               05  WH-SSHH             PIC 9(4)      VALUE 0.
           03  WM-DURCHDAT             PIC 9(8).
       01  WP-FELDER.
           03  WP-ABSENDER             PIC X(38).
           03  WP-BISDAT               PIC 9(6).
           03  WP-EMPFBANK             PIC X(38).
           03  WP-TELEFON              PIC X(38).
           03  WP-BLZKTO               PIC X(38).
           03  WP-DURCHDAT             PIC 9(6).
       01  WM-FELDER.
           03  WM-FEHLER               PIC 9      COMP.
           03  WM-LNG                  PIC 9(3)   COMP-3    VALUE 0.
           03  WH-VOR-BELEGART         PIC X      VALUE SPACE.
           03  WM-DRSATZ               PIC X(132) VALUE SPACE.
           03  WM-D2SATZ               PIC X(132) VALUE SPACE.
           03  WM-VOR-BANK-KTO         PIC 9(11)  VALUE 0   COMP-3.
           03  WM-VOR-BANK-BLZ         PIC 9(5)   VALUE 0   COMP-3.
           03  WM-VOR-BANK-NR          PIC 9      VALUE 0   COMP-3.
       01  WZ-FELDER.
           03  WZ-ZEILE                PIC 999      COMP-3  VALUE 0.
           03  WZ-BESTAND              PIC 9(5)     COMP-3  VALUE 0.
           03  WZ-UMSATZ               PIC 9(5)     COMP-3  VALUE 0.
           03  WS-SUANZ-GS             PIC 9(5)     COMP-3  VALUE 0.
           03  WS-SUANZ-LAST           PIC 9(5)     COMP-3  VALUE 0.
           03  WS-ANZGS                PIC 9(5)     COMP-3  VALUE 0.
           03  WS-ANZLAST              PIC 9(5)     COMP-3  VALUE 0.
           03  WS-SUMGS                PIC S9(9)V99 COMP-3  VALUE 0.
           03  WS-SUMLAST              PIC S9(9)V99 COMP-3  VALUE 0.
           03  WS-GESUM-GUT            PIC S9(9)V99 COMP-3  VALUE 0.
           03  WS-GESUM-LAST           PIC S9(9)V99 COMP-3  VALUE 0.
           03  WZ-TXT                  PIC 9(5)     COMP-3  VALUE 0.
       01  W2-FELDER.
           03  W2-SCHALT               PIC 999      COMP    VALUE 0.
           03  W2-ZEILE                PIC 999      COMP    VALUE 0.
           03  W2-SEITE                PIC 99       COMP    VALUE 0.
           03  WH-VOR-VALUTA           PIC 9(6)             VALUE 0.
           03  WH-SA-LAST              PIC 99               VALUE 83.
           03  WZ-RECORD               PIC 9(5)             VALUE 0.
           03  WZ-RECORD-OK            PIC 9(5)             VALUE 0.
       01  WN-DATTR.
           03  WH-FTX                  PIC X(6)  VALUE "FIBANK".
           03  FILLER                  PIC X(5)  VALUE ".DAT ".
       COPY FIBUEXT.CPY.
       COPY FIBUDECL.CPY.
       DECL-P SECTION.         USE AFTER ERROR PROCEDURE ON ZAHLPOS.
       A.  CALL "CADECL" USING "FIBUZAHL.DAT" WH-CREG.
       DECL-DATTR SECTION. USE AFTER ERROR PROCEDURE ON DATTR.
       A.   CALL "CADECL" USING WN-DATTR      WH-CREG.
       DECL-ZSUTX SECTION. USE AFTER ERROR PROCEDURE ON ZUSTX.
       A.   CALL "CADECL" USING "ZUSHILF.DAT " WH-CREG.
       DECL-Y SECTION.         USE AFTER ERROR PROCEDURE ON DRUCKER.
       A.  CALL "CADECL" USING "1DRUCKER    " WH-CREG.
       Z.  EXIT.
       END DECLARATIVES.
      *****************************************************************
       STEUER SECTION.
       A.  MOVE WL-CALL TO WH-CALL.
           MOVE WL-CREG TO WH-CREG.
           IF WL-CA = 60 PERFORM BK-EINZUG
              MOVE 9 TO WH-PG GO X.
           IF WL-CA = 70 PERFORM DTT-COPY GO X.
           OPEN I-O ZAHLPOS.
           PERFORM UEBERWSG.
           CLOSE ZAHLPOS.
       X.  MOVE WH-CREG TO WL-CREG.
       Z.  EXIT PROGRAM.
      *****************************************************************
       DATDREH SECTION.
       A.  CALL "CAUP" USING "04DATDREH" WH-CREG.
       Z.  EXIT.
      ******************************************************************
       BESETZT SECTION.
       A.  DISPLAY "Record - besetzt" AT 2401.
       Z.  EXIT.
      *****************************************************************
       NO-REC SECTION.
       A.  DISPLAY "keine Daten vorhanden," AT 2401.
           PERFORM WEITER.
           MOVE 03 TO WX-TASTE.
       Z.  EXIT.
      ******************************************************************
       WEITER SECTION.
       A.  DISPLAY " weiter mit <ret>: " with highlight AT 0000.
           MOVE SPACE TO WH-X.
           ACCEPT WH-X AT 0000.
           CALL "CAUP" USING "1324012480000" WH-CREG.
       Z.  EXIT.
      ******************************************************* Drucker *
       DRUCK SECTION.
       A.  WRITE DRA-SATZ AFTER WZ-SCHALT.
           MOVE SPACE TO DRA-SATZ.
           ADD WZ-SCHALT TO WZ-ZEILEN.
           MOVE 1 TO WZ-SCHALT.
       Z.  EXIT.
      ******************************* DruckerrÅckstellung auf 10/Zoll *
       END-DRU SECTION.
       A.  IF WM-OPEN = 0 GO Z.
           IF WM-DRU = 0 MOVE x"1B210000" TO DRA-SATZ(1:).
       B.  MOVE 0 TO WZ-SCHALT.
           PERFORM DRUCK.
           IF WF-STATUS = 27 GO B.
           MOVE SPACE TO DRA-SATZ.
           MOVE 0 TO WM-OPEN.
           CLOSE DRUCKER.
       Z.  EXIT.
      ******************************************************************
       BEG-DRU SECTION.
       A.  IF WM-OPEN > 0 GO X.
           MOVE 1 TO WM-OPEN.
           OPEN EXTEND DRUCKER.
           MOVE 47 TO WX-PRNO.
           CALL X"91" USING WX-PRSTAT WX-PRNO DRUCKER.
       C.  MOVE 0 TO WZ-SCHALT.
           IF WM-DRU = 1 MOVE WE-STG(WH-P1 - 10) TO DRA-SATZ
                    else MOVE WK-STG(WH-P1) TO DRA-SATZ.
       D.  WRITE DRA-SATZ AFTER 0.
           IF WF-STATUS = 27 GO D.
           IF WM-DRU = 1 GO X.
           MOVE WK-STG(WH-P2) TO DRA-SATZ.
       E.  WRITE DRA-SATZ AFTER 0.
           IF WF-STATUS = 27 GO E.
       X.  MOVE SPACE TO DRA-SATZ.
       Z.  EXIT.
      ******************************************************************
       LADE-DRU SECTION.
       A.  MOVE 12 TO WH-KEY.
           READ KONSTANT IGNORE LOCK not INVALID GO X.
       X.  MOVE KOP-TX TO WK-DSTG.
       Z.  EXIT.
      ******************************************** Auftragggeber laden *
       LAD-FIRMA SECTION.
       A.  MOVE 70 TO WH-KEY.
           READ KONSTANT IGNORE LOCK INVALID MOVE SPACE TO KO-FBEZ.
           MOVE KO-FBEZ TO WB-FBEZ WP-ABSENDER.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 38 OR
               WB-FBEZ(WX:1) = "#" CONTINUE.
           IF WX < 38 MOVE WB-FBEZ((WX + 1):) TO WP-ABSENDER(WX:).
           INSPECT WP-ABSENDER REPLACING ALL "#" BY ",".
           MOVE KO-FSACHB TO WB-FSACHB.
           MOVE KO-FTEL TO WB-FTEL.
       Z.  EXIT.
      **************************************** öberweisungen auswÑhlen *
       UEBERWSG SECTION.
       A.  MOVE "   öberweisungen" TO WK-GEB.
           CALL "CAUP" USING "06KOPF" WH-CREG.
           MOVE 0 TO WY WI WM-OPEN WV-BELEGART.
           CALL "CAUP" USING "0707100759000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Druck öberweisungen " AT VDU-LP.
           MOVE "UEBERWSG.LST" TO WH-DRUNAM.
           MOVE X"1B430004" TO WK-STG(5)(1:).
           MOVE WM-DATUM TO WS-DATUM.
       C.  ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "Zahlung per:" AT VDU-LP.
           IF WS-DATUM = 0 MOVE WM-DATUM TO WS-DATUM.
           MOVE WS-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           ADD 13 TO VDU-LP.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           MOVE WS-DATUM TO WZ-DATUM WH-WERT.
           DISPLAY "<esc>= Abbruch, <ret>= DurchfÅhrungsdatum" AT 2301.
           CALL "CAUP" USING "1103166006" WH-CREG.
           IF ESC GO X.
           IF not RET GO C.
           IF WZ-DATUM = 0 GO C.
           MOVE WX-DATUM TO WS-DATUM.
           IF WZ-DATUM < WH-DATUM GO C.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
      *-----------------------------------------> Firmenadresse lesen <-
           PERFORM LAD-FIRMA.
       E.  ADD 403 VDU-ECK GIVING VDU-LP.
           DISPLAY "Bank:" AT VDU-LP.
           DISPLAY "<esc>= Abbruch, <ret>= Nr. lt. Stammdatenanl."
               AT 2301.
           DISPLAY "<ret-leer>= Banken anzeigen" AT 2401.
           CALL "CAUP" USING "1004093003" WH-CREG.
           IF ESC GO X.
           IF RET AND WH-NUM = 0 CALL "FIBSTAM" USING "10BANK" WH-CREG
                                 CANCEL "FIBSTAM".
           IF not RET GO E.
           IF WH-NUM = 0 GO E.
           MOVE WH-NUM TO WD-TGN.
           DISPLAY WD-TGN with highlight AT VDU-LP.
           ADD WH-NUM 70 GIVING WH-KEY.
       F.  READ KONSTANT IGNORE LOCK INVALID PERFORM NO-REC GO E.
           MOVE KB-KTONR TO WB-KTONR.
           ADD 414 VDU-ECK GIVING VDU-LP.
           DISPLAY KB-BEZ AT VDU-LP.
           IF KB-BEZ = SPACE OR KB-KTONR = SPACE OR KB-BLZ = SPACE
               DISPLAY "Eigen Bank oder -Ktonr. oder -Blzl. fehlt!"
                   AT 2401 PERFORM WEITER GO X.
           MOVE KB-NUM TO WV-BAKEY ZP-BAKEY.
           PERFORM ZAHLSUM.
           MOVE SPACE TO WX-VWD.
           IF KB-ART = 2
               PERFORM BANKDT GO X.
       G.  ADD 503 VDU-ECK GIVING VDU-LP.
           DISPLAY "<esc>= Abbruch, <ret>= Start < >" AT VDU-LP.
           CALL "CAUP" USING "1005330000" WH-CREG.
           IF WS-BET = 0 GO X.
           IF ESC GO X.
           IF not RET GO G.
           MOVE LOW-VALUE TO ZP-SATZ.
           MOVE 0 TO IX WS-BET WZ-BET WM-KONTO KR-KTONR.
           MOVE SPACE TO WT-ADR.
           START ZAHLPOS KEY not < ZP-AKEY INVALID GO X.
       H.  READ ZAHLPOS NEXT AT END PERFORM DRU-FORM
               MOVE 9 TO WI GO Q.
           IF ZUGRIF PERFORM BESETZT GO H.
           IF WM-DATUM not = ZP-DATUM GO H.
           IF ZP-BAKEY not = KB-NUM PERFORM DRU-FORM
               MOVE 9 TO WI GO Q.
           IF WM-KONTO = 0 MOVE ZP-KTONR TO WM-KONTO.
           IF WM-KONTO not = ZP-KTONR PERFORM DRU-FORM.
       I.  IF WM-OPEN = 0;
               IF WM-DRU = 1 MOVE 15 TO WH-P1
               else MOVE 1 TO WH-P1
                    MOVE 5 TO WH-P2
               end-if PERFORM BEG-DRU.
           ADD 1 TO IX.
           IF ZP-KTONR = KR-KTONR GO O.
           MOVE ZP-KTONR TO KR-KTONR.
       M.  READ KREDIT IGNORE LOCK INVALID INITIALIZE KR-SATZ.
           ADD 503 VDU-ECK GIVING VDU-LP.
           DISPLAY KR-BEZ with SIZE 40 AT VDU-LP.
       O.  MOVE ZP-VERWEND TO WM-VERWEND(IX).
           ADD ZP-BETRAG TO WZ-BET WS-BET.
           ADD 900 KB-NUM GIVING ZP-BAKEY.    *> Merker durchgef. öbwsg.
           REWRITE ZP-SATZ.
           IF KB-ART = 0; IF IX = 4 PERFORM DRU-FORM.
           IF KB-ART = 1; IF IX = 6 PERFORM DRU-FORM.
           GO H.
       Q.  MOVE 9 TO IX.
           IF WY > 1 MOVE "SammelÅberweisung" TO KR-BEZ
              MOVE WS-BET TO WZ-BET
              PERFORM DRU-FORM.
           MOVE 0 TO IX IY WS-BET WZ-BET WM-KONTO KR-KTONR.
           PERFORM END-DRU.
           PERFORM LADE-DRU.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           MOVE "FIBZAHL 30ZAHL" TO WT-TX.
       Z.  EXIT.
      ******************************************************************
       DRU-FORM SECTION.
       A.  IF IX = 0 GO Z.
           IF KB-ART = 1 PERFORM NEUFORM GO Z.
           MOVE WM-VERWEND(1) TO DRU-VERWEND.
           MULTIPLY WZ-BET BY 100 GIVING DRU-BET.
           MOVE 3 TO WZ-SCHALT.
           PERFORM DRUCK.
           IF IX > 1 MOVE WM-VERWEND(2) TO DRU-VERWEND.
           PERFORM DRUCK.
           IF IX > 2 MOVE WM-VERWEND(3) TO DRU-VERWEND.
           IF IX > 3 PERFORM VARYING WX FROM 55 BY -1 UNTIL WX = 0 OR
                        DRU-VERWEND(WX:1) not = SPACE CONTINUE
                     end-perform
                     ADD 1 TO WX
                     IF WX > 1 MOVE "," TO DRU-VERWEND(WX:1)
                         ADD 2 TO WX
                     end-if
                     MOVE WM-VERWEND(4) TO DRU-VERWEND(WX:40).
           PERFORM DRUCK.
           PERFORM DRUCK.
           IF IX not = 9 MOVE KR-BANKKTO TO DRU-EKTO WT-TX
               PERFORM FILTER.
           MOVE KR-BEZ TO WT-BEZ.
           PERFORM HOLADR.
           IF WR-ADR(3) = SPACE MOVE WR-ADR(4) TO WR-ADR(3)
                                MOVE WR-ADR(5) TO WR-ADR(4).
           MOVE WR-ADR(1) TO DRU-EMPF.
           PERFORM DRUCK.
           MOVE WR-ADR(3) TO DRU-EMPF.
           PERFORM DRUCK.
           IF IX = 9 MOVE KB-BEZ TO DRU-EKTO
               else MOVE KR-BANK TO DRU-EKTO.
           MOVE WR-ADR(4) TO DRU-EMPF.
           PERFORM DRUCK.
           PERFORM DRUCK.
           IF IX = 9 MOVE KB-BLZ TO DRU-BLZL WT-TX
                PERFORM FILTER
           else MOVE KR-BANKLTZ TO DRU-BLZL WT-TX
                PERFORM FILTER.
           PERFORM DRUCK.
           MOVE 4 TO WZ-SCHALT.
           MOVE WB-FBEZ TO WT-BEZ.
           PERFORM HOLADR.
           MOVE WR-ADR(1) TO DRU-EMPF.
           PERFORM DRUCK.
           MOVE WR-ADR(3) TO DRU-EMPF.
           PERFORM DRUCK.
           MOVE WB-KTONR TO DRU-EKTO WT-TX.
           PERFORM FILTER.
           MOVE WR-ADR(4) TO DRU-EMPF.
           PERFORM DRUCK.
           WRITE DRA-SATZ AFTER PAGE.
           MOVE 0 TO IX WZ-BET WZ-SCHALT WZ-ZEILEN.
           ADD 1 TO WY.
           MOVE ZP-KTONR TO WM-KONTO.
           MOVE SPACE TO WT-ADR WX-VWD.
       Z.  EXIT.
      ******************************************** neues Bankformlular *
       NEUFORM SECTION.
       A.  IF KB-ART = 1 MOVE KB-BEZ TO DRN-SATZ(3:).
           MULTIPLY WZ-BET BY 100 GIVING DRN-BET.          *> ohne Komma
           MOVE "," TO DRN-BET(18:1).
           MOVE 0 TO WZ-SCHALT.
           PERFORM DRUCK.
           MOVE 2 TO WZ-SCHALT.
      *----------------------------------------------> EmpfÑngerdaten <-
           IF IX not = 9 MOVE KR-BANKKTO TO WT-TX
               PERFORM NFILTER
               MOVE WD-DRN TO DRN-KTO
               MOVE KR-BANKLTZ TO WT-TX
               PERFORM NFILTER
               MOVE WD-DRN TO DRN-BLZL(3:)
               MOVE KR-BANK TO DRN-VERWEND.
           PERFORM DRUCK.
           MOVE 2 TO WZ-SCHALT.
           MOVE KR-BEZ TO WT-BEZ.
           INSPECT WT-BEZ REPLACING ALL "#" BY SPACE.
           MOVE WT-BEZ(1:55) TO DRN-SATZ.
           PERFORM DRUCK.
      *----------------------------------------> Adresse Auftraggeber <-
           MOVE WB-FBEZ TO WT-BEZ.
           PERFORM HOLADR.
      *-------------------------------------------> Verwendungszeilen <-
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 6
               PERFORM VERWENDZEIL.
           WRITE DRA-SATZ AFTER PAGE.
           MOVE 0 TO IX WZ-BET WZ-SCHALT WZ-ZEILEN.
           MOVE SPACE TO WT-ADR WX-VWD.
           ADD 1 TO WY.
           MOVE ZP-KTONR TO WM-KONTO.
       Z.  EXIT.
      *********************************************** Verwendungszeile *
       VERWENDZEIL SECTION.
       A.  MOVE WM-VERWEND(WX) TO WT-TX.
           MOVE 2 TO WZ-SCHALT.
           MOVE WT-TX(1:21) TO DRN-VERWEND.
           IF WT-TX(22:21) not = SPACE MOVE 1 TO WZ-SCHALT
               PERFORM DRUCK
               MOVE WT-TX(22:21) TO DRN-VERWEND.
           EVALUATE WX
               WHEN 3 MOVE WB-KTONR TO WT-TX
                   PERFORM NFILTER
                   MOVE WD-DRN TO DRN-KTO
                   MOVE KB-BLZ TO WT-TX
                   PERFORM NFILTER
                   MOVE WD-DRN TO DRN-BLZL(3:)
               WHEN 4 MOVE WR-ADR(1) TO DRN-SATZ
               WHEN 5 MOVE WR-ADR(3) TO DRN-SATZ
               WHEN 6 MOVE WR-ADR(4) TO DRN-SATZ.
           PERFORM DRUCK.
       Z.  EXIT.
      *************************************** ausfiltern Sonderzeichen *
       NFILTER SECTION.
       A.  MOVE SPACE TO WD-DRN WF-TX.
           MOVE 1 TO IY WL.
           PERFORM VARYING WS FROM 1 BY 1 UNTIL WS > 17
               IF WT-TX(WS:1) IS NUMERIC
                   MOVE WT-TX(WS:1) TO WD-DRN(IY:1)
                   MOVE WT-TX(WS:1) TO WF-TX(WL:1)
                   ADD 2 TO IY
                   ADD 1 TO WL.
      *----------------------------------> und rechtsbÅndig abstellen <-
           SET RX TO 13.
           MOVE 0 TO WH-NUM.
           PERFORM VARYING WL FROM 13 BY -1 UNTIL WL = 0
               IF WF-TX(WL:1) not = SPACE
                   MOVE WF-TX(WL:1) TO WR-NUM(RX)
                   SET RX DOWN BY 1.
       Z.  EXIT.
      *************************************** ausfiltern Sonderzeichen *
       FILTER SECTION.
       A.  MOVE SPACE TO DRU-EKTO WF-TX.
           MOVE 1 TO IY WL.
           PERFORM VARYING WS FROM 1 BY 1 UNTIL WS > 17
               IF WT-TX(WS:1) IS NUMERIC
                   MOVE WT-TX(WS:1) TO DRU-EKTO(IY:1)
                   MOVE WT-TX(WS:1) TO WF-TX(WL:1)
                   ADD 2 TO IY
                   ADD 1 TO WL.
      *----------------------------------> und rechtsbÅndig abstellen <-
           SET RX TO 13.
           MOVE 0 TO WH-NUM.
           PERFORM VARYING WL FROM 13 BY -1 UNTIL WL = 0
               IF WF-TX(WL:1) not = SPACE
                   MOVE WF-TX(WL:1) TO WR-NUM(RX)
                   SET RX DOWN BY 1.
       Z.  EXIT.
      ************************************************** lies Hausbank *
       READ-BANK SECTION.
       A.  ADD ZP-BAKEY 70 GIVING WH-KEY.
       C.  READ KONSTANT IGNORE LOCK INVALID
               MOVE SPACE TO KB-KTONR KB-BEZ
               MOVE 0 TO KB-BLZ.
           MOVE KB-KTONR TO WB-KTONR.
       Z.  EXIT.
      *************************************** Adresszerlegung f. Druck *
       HOLADR SECTION.
       A.  MOVE SPACE TO WT-ADR.
           MOVE "#" TO WR-BEZ(131).
           UNSTRING WT-BEZ DELIMITED BY "#" INTO WR-ADR(1) WR-ADR(2)
               WR-ADR(3) WR-ADR(4) WR-ADR(5) WR-ADR(6).
       Z.  EXIT.
      ************************************************ Summe Zahlungen *
       ZAHLSUM SECTION.
       A.  ADD 328 VDU-ECK GIVING VDU-LP.
           DISPLAY "Summe: " AT VDU-LP.
           IF KB-ART = 2 MOVE 11 TO WY
                    else MOVE 4 TO WY.
           MOVE 0 TO WS-BET WZ-BET ZP-DK ZP-KTONR
                     WV-KTONR ZP-VALDAT ZP-LFD WX.
           MOVE WV-BAKEY TO ZP-BAKEY.
           START ZAHLPOS KEY not < ZP-AKEY INVALID GO Q.
       C.  READ ZAHLPOS NEXT IGNORE LOCK AT END GO Q.
           IF ZP-DATUM = 0 DELETE ZAHLPOS GO C.
           IF ZP-BAKEY not = WV-BAKEY GO Q.
           IF ZP-DATUM not = WS-DATUM GO C.
           IF WH-PG not = 2 ADD 503 VDU-ECK GIVING VDU-LP
               DISPLAY ZP-EMPFG with SIZE 36 AT VDU-LP.
           IF WV-KTONR = 0 MOVE ZP-KTONR TO WV-KTONR.
           IF WV-KTONR not = ZP-KTONR OR WX = WY;
              IF WZ-BET < 0 DISPLAY "Betrag negativ - öberweisung nicht
      -           "mîglich" AT 2401 PERFORM WEITER
              else MOVE 0 TO WZ-BET WX
                   MOVE ZP-KTONR TO WV-KTONR.
           ADD 1 TO WX.
           ADD ZP-BETRAG TO WS-BET WZ-BET.
           MOVE WS-BET TO WD-BET.
           ADD 335 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-BET with highlight AT VDU-LP.
           MOVE ZP-FSYM TO WM-FSYM.
           GO C.
       Q.  ADD 503 VDU-ECK GIVING VDU-LP.
           DISPLAY ALL " " with SIZE 50 AT VDU-LP.
       Z.  EXIT.
      ******************************************************************
       BANKDT SECTION.
       A.  PERFORM VORLAUF.
           IF ESC GO Y.
           PERFORM OPEN-BGL.
           PERFORM OPEN-PRT.
      *------------------------------> îffnen und prÅfen ob vorhanden <-
      *    OPEN OUTPUT EDIFACT.
           MOVE 0 TO WM-BESTAND WM-TX WT-SA WU-TEXTZEILEN
                     WU-FUNKTION WH-CNT WH-SEQ.
           ADD 1003 VDU-ECK GIVING VDU-LP.
           DISPLAY "Eingabedatei:" AT VDU-LP.
           ADD 1 TO VDU-L.
           DISPLAY "Ausgabedatei:" AT VDU-LP.
           INITIALIZE ZP-SATZ.
           START ZAHLPOS KEY not < ZP-AKEY INVALID GO X.
       B.  READ ZAHLPOS NEXT AT END GO X.
           IF ZUGRIF PERFORM BESETZT GO B.
           IF ZP-BAKEY not = WV-BAKEY GO B.
           IF ZP-DATUM not = WS-DATUM GO B.
           ADD 1 TO WZ-RECORD.
           ADD 1017 VDU-ECK GIVING VDU-LP.
           DISPLAY WZ-RECORD with highlight AT VDU-LP.
           MOVE 0 TO WM-FEHLER.
           MOVE ZP-KTONR TO WD-KZ.
           IF WD-KZ = 3 PERFORM READ-KREDIT
                   else PERFORM READ-DEBIT.
           IF ESC GO B.
      *-----------------------------------------------> neuer Bestand <-
       R.  IF WB-BANK-NR = WM-VOR-BANK-NR
               and WB-ABSBLZL = WM-VOR-BANK-BLZ
               and WB-ABSBKTO = WM-VOR-BANK-KTO
               and ZP-DATUM   = WH-VOR-VALUTA GO U.
      *---------------------------------------------> Bestandswechsel <-
       S.  IF WM-BESTAND = 1 PERFORM DT-SUMMENSATZ
                             MOVE 0 TO WM-BESTAND.
      *-------------------------------------------------> Umsatzzeile <-
       U.  IF WM-BESTAND = 0 PERFORM DT-VORLAUFSATZ
                             MOVE 1 TO WM-BESTAND.
           PERFORM DT-UMSATZ.
           GO B.
       X.  IF WM-BESTAND = 1 PERFORM DT-SUMMENSATZ
                             MOVE 0 TO WM-BESTAND.
           PERFORM BEGL-SUMMEN.
           CLOSE DATTR.
           MOVE 0 TO WM-OPEN.
           DISPLAY "DatentrÑger-Erstellung beendet! Weiter mit <ret> < >
      -        "" AT 2301.
           PERFORM CLOSE-BGL.
           PERFORM CLOSE-PRT.
           CALL "CAUP" USING "0123510000" WH-CREG.
           MOVE WH-CREG TO WL-CREG.
           DELETE FILE ZUSTX.
       Y.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ****************************************************** Kopfdaten *
       EDIKOPF SECTION.
       A.  MOVE 2 TO WB-AC.
           MOVE 0 TO WB-DE WB-DV WB-FLG.
           MOVE SPACE TO WB-FH.
           IF EINZUG MOVE "BKEINZUG.DAT" TO WN-EDIFACT
           else MOVE "BKUEWSG.DAT" TO WN-EDIFACT.
           CALL "CBL_CREATE_FILE" USING
                                  WN-EDIFACT WB-AC WB-DE WB-DV WB-FH.
           MOVE 0 TO WB-OFS WH-SEQ WH-CNT.
           MOVE 1 TO WE-OPEN.                         *> Datei geîffnet
           MOVE "UNA:+,? '" TO WB-SATZ.
           MOVE 9 TO WB-CNT.
           CALL "CBL_WRITE_FILE" USING
                                  WB-FH WB-OFS WB-CNT WB-FLG WB-SATZ.
           ADD WB-CNT TO WB-OFS.
           MOVE "UNB+UNOC:3+" TO WB-SATZ.
           MOVE WK-FIRMA TO WB-SATZ(12:15).
           MOVE "+" TO WM-CD.
           PERFORM ANHANG.
           MOVE KB-ATBEZ TO WM-CD.
           PERFORM ANHANG.
           MOVE "+" TO WM-CD.
           PERFORM ANHANG.
           MOVE WH-DATUM TO WC-DATUM.
           MOVE WC-DATUM TO WM-CD.
           PERFORM ANHANG.
           MOVE ":" TO WM-CD.
           PERFORM ANHANG.
           ACCEPT WH-TIME FROM TIME.
           MOVE WH-HHMM TO WM-CD(1:4).
           PERFORM ANHANG.
           MOVE "+" TO WM-CD.
           PERFORM ANHANG.
           MOVE KB-KENNUNG TO WK-UNB-Z WK-UNH-T.
           MOVE WC-DATUM TO WK-UNB-Z(3:)
           MOVE KB-KONTROL TO WK-UNB-Z(9:) WK-UNH-T(3:).
           MOVE WK-UNB-Z TO WM-CD.                         *> Kennung
           PERFORM ANHANG.
           PERFORM ZEILEND.
           MOVE "UNH+" TO WB-SATZ.
           MOVE WK-UNH-T TO WM-CD.                         *> Kennung
           PERFORM ANHANG.
           MOVE "+" TO WM-CD.
           PERFORM ANHANG.
           MOVE "PAYMUL" TO WM-CD.
           IF EINZUG MOVE "DIRDEB" TO WM-CD.
           PERFORM ANHANG.
           MOVE ":D:" TO WM-CD.
           PERFORM ANHANG.
           MOVE "96A:" TO WM-CD.
           PERFORM ANHANG.
           MOVE "UN" TO WM-CD.
           PERFORM ANHANG.
           MOVE ":FAT01G" TO WM-CD.
           PERFORM ANHANG.
           PERFORM ZEILEND.
           MOVE "BGM+452+" TO WB-SATZ.
           IF EINZUG MOVE "BGM+214+" TO WB-SATZ.
           MOVE WK-UNB-Z TO WM-CD.
           PERFORM ANHANG.
           MOVE "+9" TO WM-CD.
           PERFORM ANHANG.
           PERFORM ZEILEND.
           MOVE "DTM+137:" TO WB-SATZ.
           MOVE WM-DATUM TO WM-CD.
           PERFORM ANHANG.
           MOVE WH-TIME(1:4) TO WM-CD.
           PERFORM ANHANG.
           MOVE ":" TO WM-CD.
           PERFORM ANHANG.
           MOVE "203" TO WM-CD.
           PERFORM ANHANG.
           PERFORM ZEILEND.
           MOVE "FII+MR+" TO WB-SATZ.
           MOVE WB-KTONR TO WM-CD.
           PERFORM 0-ANHANG.
           MOVE "+:::" TO WM-CD.
           PERFORM ANHANG.
           MOVE KB-BLZ TO WM-CD.
           PERFORM ANHANG.
           MOVE ":25:137+AT" TO WM-CD.
           PERFORM ANHANG.
           PERFORM ZEILEND.
           MOVE "NAD+MS+++" TO WB-SATZ.
           MOVE KB-KTOBEZ(1) TO WM-CD.
           PERFORM ANHANG.
           MOVE "+++++AT" TO WM-CD.
           PERFORM ANHANG.
           PERFORM ZEILEND.
           MOVE "CTA+MS+Buchhaltung:" TO WB-SATZ.
           MOVE WB-FSACHB TO WM-CD.                         *> variabel
           PERFORM ANHANG.
           PERFORM ZEILEND.
           MOVE "COM+" TO WB-SATZ.
           MOVE WB-FTEL TO WM-CD.
           PERFORM ANHANG.                                  *> variabel
           MOVE ":TE" TO WM-CD.
           PERFORM ANHANG.
           PERFORM ZEILEND.
      *--------------------------------------> Beginn des B-Segmentes <-
           MOVE "LIN+1" TO WB-SATZ.
           PERFORM ZEILEND.
           MOVE "DTM+203:" TO WB-SATZ.
           MOVE WM-DURCHDAT TO WM-CD.             *> DurchfÅhrungsdatum
           PERFORM ANHANG.
           MOVE ":102" TO WM-CD.
           PERFORM ANHANG.
           PERFORM ZEILEND.
           MOVE "RFF+AEK:" TO WB-SATZ.
           IF EINZUG MOVE "RFF+AKJ:" TO WB-SATZ.
           MOVE WK-UNB-Z TO WM-CD.
           PERFORM ANHANG.
           PERFORM ZEILEND.
           MOVE "BUS++DO++TRF" TO WB-SATZ.
           IF EINZUG MOVE "BUS++DO+13+DDT" TO WB-SATZ.
           PERFORM ZEILEND.
           MOVE "MOA+9:" TO WB-SATZ.
           MOVE WS-BET TO WT-BET.
           PERFORM B-ANHANG.
           MOVE ":" TO WM-CD.
           PERFORM ANHANG.
           MOVE "EUR" TO WM-CD.
           PERFORM ANHANG.
           PERFORM ZEILEND.
           MOVE "FII+OR+" TO WB-SATZ.
           IF KB-IBAN not = SPACE MOVE KB-IBAN TO WM-CD.
           MOVE KB-KTONR TO WM-CD.
           PERFORM 0-ANHANG.
           MOVE ":" TO WM-CD.
           PERFORM ANHANG.
      *-----------------------------------------------> Kontowortlaut <-
           MOVE KB-KTOBEZ(1) TO WM-CD.
           PERFORM ANHANG.
           MOVE ":" TO WM-CD.
           PERFORM ANHANG.
           IF KB-KTOBEZ(2) not = space
               MOVE KB-KTOBEZ(2) TO WM-CD
               PERFORM ANHANG.
           IF KB-IBAN not = SPACE MOVE ":EUR" TO WM-CD
                                  GO W.
           MOVE ":EUR+:::" TO WM-CD.
           PERFORM ANHANG.
           MOVE KB-BLZ TO WM-CD.
           PERFORM ANHANG.
           MOVE "+AT" TO WM-CD.
       W.  PERFORM ANHANG.
           PERFORM ZEILEND.
       Z.  EXIT.
      ************************************* Lieferenten-/EmpfÑngerende *
       EDIZEIL SECTION.
       A.  MOVE "SEQ++"TO WB-SATZ.
           ADD 1 TO WH-SEQ.
           MOVE WH-SEQ TO WD-ANZ.
           PERFORM N-ANHANG.
           PERFORM ZEILEND.
           MOVE "MOA+9:" TO WB-SATZ.
           MOVE WU-BETRAG TO WT-BET
           PERFORM B-ANHANG.
           MOVE ":" TO WM-CD.
           PERFORM ANHANG.
           MOVE "EUR" TO WM-CD.
           PERFORM ANHANG.
           PERFORM ZEILEND.
           IF EINZUG MOVE "RFF+PQ:" TO WB-SATZ
               MOVE BK-KTONR TO WM-CD
               PERFORM 0-ANHANG
           else MOVE "RFF+PQ:" TO WB-SATZ
                MOVE WB-FBEZ TO WT-BEZ             *> Hinweis fÅr Empf.
                PERFORM HOLADR
                MOVE WR-ADR(1) TO WM-CD
                PERFORM ANHANG
                MOVE "/" TO WM-CD
                PERFORM ANHANG
                MOVE WH-SEQ TO WM-CD            *> fÅr RÅckleitung
                PERFORM 0-ANHANG
                MOVE "-" TO WM-CD
                PERFORM ANHANG
                MOVE WV-KTONR TO WM-CD
                PERFORM 0-ANHANG.
           PERFORM ZEILEND.
           IF EINZUG MOVE "FII+PH+" TO WB-SATZ
               MOVE BK-BANKKTO TO WM-CD
               PERFORM 0-ANHANG
               MOVE ":" TO WM-CD
               PERFORM ANHANG
               MOVE DE-BEZ TO WT-BEZ
               PERFORM HOLADR
               IF WR-ADR(2) = SPACE MOVE WR-ADR(3) TO WR-ADR(2)
               end-if
               MOVE SPACE TO WR-ADR(3) WR-ADR(4) WR-ADR(5)
               MOVE WR-ADR(1) TO WT-TX
               PERFORM YKLGR
               PERFORM ANHANG
               MOVE "+:::"TO WM-CD
               PERFORM ANHANG
               MOVE BK-BANKLTZ TO WM-CD
               PERFORM 0-ANHANG
               MOVE "+AT" TO WM-CD
               PERFORM ANHANG
               GO E.
           MOVE "FII+BF+" TO WB-SATZ.
           IF BK-IBAN not = SPACE MOVE BK-IBAN TO WM-CD
           else MOVE KR-BANKKTO TO WT-TX
               PERFORM NFILTER
               MOVE WF-TX TO WM-CD.
           PERFORM ANHANG.
           MOVE ":" TO WM-CD.
           PERFORM ANHANG.
           MOVE 0 TO WR.
           MOVE KR-BEZ TO WT-BEZ.
           PERFORM HOLADR.
           PERFORM VARYING WZ FROM 1 BY 1 UNTIL WR = 2 or WZ > 4
               IF WR-ADR(WZ) not = SPACE MOVE WR-ADR(WZ) TO WT-TX
                   PERFORM YKLGR
                   PERFORM ANHANG
                   ADD 1 TO WR
                   IF WR = 1 MOVE ":" TO WM-CD
                       PERFORM ANHANG
                   end-if.
           IF BK-IBAN not = SPACE MOVE KB-IBAN TO WM-CD
                                  GO E.
           MOVE "+:::" TO WM-CD.
           PERFORM ANHANG.
           MOVE KR-BANKLTZ TO WT-TX.
           PERFORM NFILTER.
           MOVE WF-TX TO WM-CD.
           PERFORM ANHANG.
           MOVE ":25:137+AT" TO WM-CD.
           PERFORM ANHANG.
       E.  PERFORM ZEILEND.
           MOVE "PRC+11" TO WB-SATZ.
           PERFORM ZEILEND.
      *------------------------------> Verwendungszweck-Aufgliederung <-
           MOVE 1 TO IQ.
           PERFORM VARYING WR FROM 1 BY 1 UNTIL WR > 3
               PERFORM VARYING WE FROM 1 BY 1 UNTIL WE > 5
                   IF WH-FXT(IQ) not = SPACE;
                       IF WE = 1 MOVE "FTX+PMD+++" TO WB-SATZ
                       end-if
                   IF WE not = 1 MOVE ":" TO WM-CD
                                 PERFORM ANHANG
                   end-if
                   MOVE WH-FXT(IQ) TO WT-TX
                   PERFORM YKLGR
                   PERFORM ANHANG
                   ADD 1 TO IQ
               end-perform
               IF WB-SATZ not = SPACE PERFORM ZEILEND
                           else MOVE 6 TO WR.
       Z.  EXIT.
      ******************************************************************
       DT-VORLAUFSATZ SECTION.
       A.  IF WM-OPEN not = 1 OPEN OUTPUT DATTR
                              MOVE 1 TO WM-OPEN.
           MOVE 0 TO WS-KTRSUMME-BTR WS-ANZ-UMS
                        WS-KTRSUMME-LW WS-KTRSUMME-KTO
                        WS-SUMME-ZUSINF.
           MOVE 0 TO WS-ANZGS WS-ANZLAST WS-SUMGS WS-SUMLAST.
           MOVE WB-BANK-NR TO WM-VOR-BANK-NR.
           IF WV-BAKEY > 10 MOVE "L" TO WH-VOR-BELEGART
                            MOVE 89 TO WV-BELEGART
                       else MOVE "G" TO WH-VOR-BELEGART
                            MOVE 49 TO WV-BELEGART.
           MOVE ZP-DATUM TO WH-VOR-VALUTA.
           MOVE WB-ABSBKTO TO WM-VOR-BANK-KTO.
           MOVE WB-ABSBLZL TO WM-VOR-BANK-BLZ.
           MOVE 0 TO WV-KTRSUMME.
           MOVE KB-KENNUNG TO WV-BESTANDKENNUNG.
       F.  READ KONSTANT INVALID STOP RUN.
           IF ZUGRIF PERFORM BESETZT GO F.
           READ KONSTANT
           ADD 1 TO KB-KONTROL.
           REWRITE KO-SATZ.
           MOVE KB-KONTROL TO WV-BESTANDVALUTA.
           MOVE 0 TO WV-KTOEMPF.
           MOVE WM-DATUM TO WV-ERSTELLDAT.
           MOVE 0 TO WV-VALUTA.
           MOVE 0 TO WV-BLZABS.
           MOVE WM-VOR-BANK-KTO TO WV-KTOABS.
           MOVE WM-VOR-BANK-BLZ TO WV-BLZEMPF.
           MOVE WP-ABSENDER TO WV-NAME-ABSENDER.
           MOVE 0 TO WV-VALUTA-BEG.
           MOVE 0 TO WV-SA.
           MOVE WV-SATZ TO DTR-SATZ.
           PERFORM WRITE-DT.
           PERFORM EDIKOPF.
       Z.  EXIT.
      ***************************************************** Zeilenende *
       ZEILEND SECTION.
       A.  MOVE "'" TO WM-CD(1:).
      *    move x"0d0a" to wm-cd(2:).                          *> raus
           PERFORM ANHANG.
      *    add 2 to wx.                                        *> raus
           MOVE WX TO WB-CNT.
           CALL "CBL_WRITE_FILE" USING
                                  WB-FH WB-OFS WB-CNT WB-FLG WB-SATZ.
           ADD WB-CNT TO WB-OFS.
           MOVE SPACE TO WB-SATZ.
           ADD 1 TO WH-CNT.
       Z.  EXIT.
      ******************************************************************
       0-ANHANG SECTION.
       A.  PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 15
               IF WM-CD(1:1) = "0" MOVE WM-CD(2:) TO WM-CD(1:)
               else MOVE 16 TO WZ.
           PERFORM ANHANG.
       Z.  EXIT.
      ******************************************************************
       B-ANHANG SECTION.
       A.  PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 8
               IF WT-BET(1:1) = SPACE MOVE WT-BET(2:) TO WT-BET(1:).
           MOVE WT-BET TO WM-CD.
           PERFORM ANHANG.
       Z.  EXIT.
      ******************************************************************
       N-ANHANG SECTION.
       A.  PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 7
               IF WD-ANZ(1:1) = " " MOVE WD-ANZ(2:) TO WD-ANZ(1:).
           MOVE WD-ANZ TO WM-CD.
           PERFORM ANHANG.
       Z.  EXIT.
      ******************************************************* anhÑngen *
       ANHANG SECTION.
       A.  PERFORM VARYING WX FROM 160 BY -1
               UNTIL WB-SATZ(WX:1) not = SPACE continue.
           ADD 1 TO WX.
           MOVE WM-CD TO WB-SATZ(WX:).
       Z.  EXIT.
      ****************************************** öberweisungsgsadresse *
       READ-UEWGADR SECTION.
       A.  MOVE 5 TO WI.
           MOVE 3 TO WU-KTO.
           PERFORM VARYING WX FROM  10 BY -1 UNTIL WX = 1
               IF KR-BANKKTO(WX:1) not = " "
                   MOVE KR-BANKKTO(WX:1) TO WU-KTO(WI:1)
                   ADD -1 TO WI.
           MOVE KR-KTONR TO WV-KTO.
           MOVE WU-KTO TO KR-KTONR.
           READ KREDIT IGNORE LOCK INVALID
               MOVE SPACE TO KR-BANKKTO KR-BANKLTZ KR-BANK GO Z.
       Z.  EXIT.
      *********************************** EmpfÑnger / Kreditoren lesen *
       READ-KREDIT SECTION.
       A.  MOVE ZP-KTONR TO KR-KTONR WV-KTONR.
           READ KREDIT IGNORE LOCK INVALID STOP RUN.
           IF KR-BANKKTO(1:1) = "*" PERFORM READ-UEWGADR.
           MOVE KR-BANKLTZ TO WT-TX.                  *> Empf.-Blzl.
           PERFORM FILTER.
           MOVE WH-NUM TO WU-LEITWEG.
           MOVE KR-BANKKTO TO WT-TX                   *> Empfg.-Kto.Nr.
           PERFORM FILTER.
           MOVE WH-NUM TO WU-KONTONR-17.
           MOVE KR-BEZ TO WT-BEZ.
           PERFORM HOLADR.
           IF WR-ADR(2) = SPACE MOVE WR-ADR(3) TO WR-ADR(2).
           MOVE SPACE TO WR-ADR(3) WR-ADR(4) WR-ADR(5).
           IF KR-BANKLTZ = 0 OR KR-BANKKTO = 0 SET ESC TO TRUE.
       Z.  EXIT.
      **************************************** EmpfÑnger / Kunde lesen *
       READ-DEBIT SECTION.
       A.  MOVE ZP-KTONR TO DE-KTONR BK-KTONR WV-KTONR.
           READ DEBITOR IGNORE LOCK INVALID STOP RUN.
           MOVE 2 TO BK-DK.
      *    OPEN INPUT BANKDAT.
       C.  READ BANKDAT IGNORE LOCK INVALID STOP RUN.
           MOVE BK-BANKLTZ TO WT-TX.                  *> Empf.-Blzl.
           PERFORM FILTER.
           MOVE WH-NUM TO WU-LEITWEG.
           MOVE BK-BANKKTO TO WT-TX                   *> Empfg.-Kto.Nr.
           PERFORM FILTER.
           MOVE WH-NUM TO WU-KONTONR-17.
           MOVE DE-BEZ TO WT-BEZ.
           PERFORM HOLADR.
           IF WR-ADR(2) = SPACE MOVE WR-ADR(3) TO WR-ADR(2).
           MOVE SPACE TO WR-ADR(3) WR-ADR(4) WR-ADR(5).
           IF BK-BANKLTZ = 0 OR BK-BANKKTO = 0 SET ESC TO TRUE.
      *    CLOSE BANKDAT.
       Z.  EXIT.
      ******************************************************************
       DT-UMSATZ SECTION.
       A.  ADD 1 TO WS-ANZ-UMS.
           MOVE SPACE TO WT-FXT.
           MOVE WV-BELEGART TO WU-BELEGART.
           IF WV-BELEGART = 49 MOVE 43 TO WU-BELEGART.
           IF WV-BELEGART = 89 MOVE WH-SA-LAST TO WU-BELEGART.
           MOVE 0 TO        WU-RESERVE.
           MOVE ZP-BETRAG TO  WU-BETRAG.
           MOVE WM-VOR-BANK-BLZ TO WU-BLZ.
           MOVE WM-VOR-BANK-KTO TO WU-KONTONR-19.
           MOVE SPACE TO WU-LEER.
           MOVE 28 TO WM-LNG.
           MOVE SPACE TO WU-KUNDENDATEN.
           MOVE 0 TO WU-KLAUSELDAT.
           IF WU-BELEGART = 43 MOVE 1 TO WU-FORMULAR
                          else MOVE 1 TO WU-FORMULAR.
           MOVE 0 TO WU-BUCHUNGSDAT.                         *> lt. RLB
           MOVE 1 TO WU-SA.
      *-----------------------------------------------> Umsatz-Satz-2 <-
           MOVE WV-BELEGART TO WF-BELEGART.
           IF WV-BELEGART = 49 MOVE 43 TO WF-BELEGART.
           IF WV-BELEGART = 89 MOVE WH-SA-LAST TO WF-BELEGART.
           IF WR-ADR(1) not = SPACE MOVE WR-ADR(1) TO WT-TX
               PERFORM YKLGR
               MOVE WT-TX TO WR-ADR(1) WF-NAME-30.
           MOVE SPACE TO WF-BLANK-30.
           IF WR-ADR(2) not = SPACE MOVE WR-ADR(2) TO WT-TX
               PERFORM YKLGR
               MOVE WT-TX TO WR-ADR(2).
       D.  MOVE WR-ADR(2) TO WF-NAME-31.
           MOVE SPACE TO WF-BLANK-31.
           MOVE 0 TO WF-RES.
           MOVE WP-ABSENDER TO WF-NAME-33.
           MOVE SPACE TO WF-BLANK-33.
           MOVE 2 TO WF-SAX.
           IF KB-OHNEVW = 1 PERFORM ZUSINFO
           else IF ZP-VERWEND(29:) not = SPACE PERFORM ZUSINFO
                else MOVE ZP-VERWEND TO WT-TX
                     PERFORM YKLGR
                     MOVE WT-TX TO ZP-VERWEND WU-VERWENDUNG.
           PERFORM FXT-EDI.
           PERFORM PROTOKOLL.
       F.  ADD 900 TO ZP-BAKEY.              *> Merker durchgef. öbwsg.
           REWRITE ZP-SATZ.
           ADD -900 TO ZP-BAKEY.
           START ZAHLPOS KEY not < ZP-AKEY INVALID GO Q.
       G.  READ ZAHLPOS NEXT AT END GO Q.
           IF ZUGRIF PERFORM BESETZT GO G.
           ADD 1 TO WZ-RECORD.
           ADD 1017 VDU-ECK GIVING VDU-LP.
           DISPLAY WZ-RECORD with highlight AT VDU-LP.
           MOVE 0 TO WM-FEHLER.
           IF ZP-BAKEY not = WV-BAKEY GO Q.
           IF ZP-DATUM not = WS-DATUM GO G.
           IF ZP-KTONR = WV-KTONR;
               IF WM-TX = 11 PERFORM LIEFEND GO A
               else ADD ZP-BETRAG TO WU-BETRAG
                    PERFORM FXT-EDI
                    PERFORM ZUSINFO
                    PERFORM TEILPROT
                    GO F.
      *--------------------------------------> EmpfÑngerende erreicht <-
           PERFORM LIEFEND.
           IF WD-KZ = 3 PERFORM READ-KREDIT
                   else PERFORM READ-DEBIT.
           IF ESC GO G.
           GO A.
      *------------------------------------------> Dateiende erreicht <-
       Q.  PERFORM LIEFEND.
       Z.  EXIT.
      ********************************** Tabelle abstellen fÅr EDIFACT *
       FXT-EDI SECTION.
       A.  PERFORM VARYING WI FROM 1 BY 1 UNTIL WI > 14
              IF WH-FXT(WI) = SPACE MOVE ZP-VERWEND TO WH-FXT(WI)
                  MOVE 15 TO WI.
       Z.  EXIT.
      ************************************* Lieferenten-/EmpfÑngerende *
       LIEFEND SECTION.
       A.  IF WM-TX > 0 AND WT-SA not = 0
               WRITE ZU-SATZ FROM WT-SATZ.
           ADD WU-BETRAG TO WS-KTRSUMME-BTR.
           IF WV-BELEGART = 49 ADD 1 TO WS-ANZGS
                               ADD WU-BETRAG TO WS-SUMGS.
           IF WV-BELEGART = 89 ADD 1 TO WS-ANZLAST
                               ADD WU-BETRAG TO WS-SUMLAST.
           ADD WU-LEITWEG TO WS-KTRSUMME-LW.          *> Kontrollsummen
           ADD WU-KONTONR-17 TO WS-KTRSUMME-KTO.      *> Kontrollsummen
           MOVE WU-SATZ TO DTR-SATZ.
      *-------------------------------------> Umsatz-Satz-1 schreiben <-
           PERFORM WRITE-DT.
           MOVE WF-SATZ TO DTR-SATZ.
           PERFORM EDIZEIL.
           MOVE SPACE TO WT-FXT.
      *-------------------------------------> Umsatz-Satz-2 schreiben <-
           PERFORM WRITE-DT.
           IF WM-TX = 0 GO Z.
      *----------------------------------------> Text-SÑtze schreiben <-
           CLOSE ZUSTX.                  *> noch offen siehe alte Datei
           OPEN I-O ZUSTX.
       C.  READ ZUSTX AT END GO X.
           MOVE ZU-SATZ TO DTR-SATZ.
           PERFORM WRITE-DT.
           GO C.
       X.  CLOSE ZUSTX.
           MOVE 0 TO WM-TX WT-SA WU-TEXTZEILEN WU-FUNKTION.
       Z.  EXIT.
      *************************************************** Zusatzzeilen *
       ZUSINFO SECTION.
       A.  IF WM-TX = 0 OPEN OUTPUT ZUSTX
                        MOVE SPACE TO WT-SATZ
                        MOVE 0 TO WT-ZNRA WT-ZNRB WT-SA.
           ADD 1 TO WM-TX.
           COMPUTE WT-SA = (WM-TX + 5) / 2.
           IF WT-ZNRA = 00 ADD 5 WM-TX GIVING WT-ZNRA
               ADD 1 TO WS-SUMME-ZUSINF
               ADD 1 TO WU-TEXTZEILEN
               MOVE 1 TO WU-FUNKTION
               MOVE ZP-VERWEND TO WT-TX
               PERFORM YKLGR
               MOVE WT-TX TO WT-ZINF-A
               MOVE 0 TO WT-ZNRB
               MOVE SPACE TO WT-ZINF-B
               GO Z.
           ADD 5 WM-TX GIVING WT-ZNRB.
           ADD 1 TO WS-SUMME-ZUSINF.
           ADD 1 TO WU-TEXTZEILEN.
           MOVE 1 TO WU-FUNKTION.
           MOVE ZP-VERWEND TO WT-TX.
           PERFORM YKLGR.
           MOVE WT-TX TO WT-ZINF-B.
           WRITE ZU-SATZ FROM WT-SATZ.
           MOVE SPACE TO WT-SATZ.
           MOVE 0 TO WT-ZNRA WT-ZNRB WT-SA.
       Z.  EXIT.
      ******************************************************************
       DT-SUMMENSATZ SECTION.
       A.  MOVE WV-BELEGART TO WS-BELEGART.
           MOVE WV-BESTANDKTRNR  TO WS-BESTANDKTRNR.
           MOVE WV-BLZEMPF TO WS-BLZEMPF.
           MOVE WV-KTOEMPF TO WS-KTOEMPF.
           MOVE WV-ERSTELLDAT TO WS-ERSTELLDAT.
           MOVE WV-BLZABS TO WS-BLZABS.
           MOVE WV-KTOABS TO WS-KTOABS.
           MOVE 0 TO WS-NULL.
           MOVE 99 TO WS-SA.
           IF W2-ZEILE = 0 PERFORM BEGL-KOPF.
           MOVE WS-SATZ TO DTR-SATZ.
           PERFORM WRITE-DT.
           PERFORM EDIFUS.
           PERFORM SUMMENDRUCK.
           PERFORM BEGL-BESTAND.
           MOVE 0 TO WS-SUMME-ZUSINF.
       Z.  EXIT.
      ******************************************************************
       EDIFUS SECTION.
       A.  MOVE "CNT+1:" TO WB-SATZ.
           MOVE WS-BET TO WT-BET.
           PERFORM B-ANHANG.
           PERFORM ZEILEND.
           MOVE "CNT+2:1" TO WB-SATZ.
           PERFORM ZEILEND.
           MOVE "CNT+39:" TO WB-SATZ.
           MOVE WH-SEQ TO WD-ANZ.
           PERFORM N-ANHANG.
           PERFORM ZEILEND.
           MOVE "UNT+" TO WB-SATZ.
           MOVE WH-CNT TO WD-ANZ.
           PERFORM N-ANHANG.
           MOVE "+" TO WM-CD.
           PERFORM ANHANG.
           MOVE WK-UNH-T TO WM-CD.
           PERFORM ANHANG.
           PERFORM ZEILEND.
           MOVE "UNZ+1+" TO WB-SATZ.
           MOVE WK-UNB-Z TO WM-CD.
           PERFORM ANHANG.
           PERFORM ZEILEND.
           CALL "CBL_CLOSE_FILE" USING WB-FH.
           PERFORM EDIPROT.
       Z.  EXIT.
      ******************************************** Protokoll EDI-Daten *
       EDIPROT SECTION.
       A.  MOVE 1 TO WB-AC.                               *> read only
           MOVE 0 TO WB-DE WB-DV WB-FLG WB-OFS IC.
           MOVE SPACE TO WB-FH.
           IF EINZUG MOVE "BKEINZUG.DAT" TO WN-EDIFACT
           else MOVE "BKUEWSG.DAT" TO WN-EDIFACT.
           CALL "CBL_OPEN_FILE" USING
                                WN-EDIFACT WB-AC WB-DE WB-DV WB-FH.
           MOVE 0 TO WB-OFS WH-SEQ WH-CNT.
           MOVE 1 TO WB-CNT IQ.
           MOVE 10 TO WX.
       E.  MOVE SPACE TO WB-Z.
           CALL "CBL_READ_FILE" USING WB-FH WB-OFS WB-CNT WB-FLG WB-Z.
           IF RETURN-CODE not = 0 GO K.
           ADD WB-CNT TO WB-OFS.
           EVALUATE WB-Z
               WHEN x"0D"
               WHEN x"0A" NEXT SENTENCE
               WHEN  "'"  GO G
               WHEN OTHER MOVE WB-Z TO WB-SATZ(IQ:1)
                          ADD 1 TO IQ.
           GO E.
      *-----------------------------------------> Segmente abarbeiten <-
       G.  MOVE 1 TO IQ IC.
           IF WB-OFS < 100
               EVALUATE WB-SATZ(1:5)
                   WHEN "UNB+U"
                   WHEN "UNA:+" GO E
                   WHEN "UNA:+" GO E
                   WHEN OTHER GO X.                     *> kein Edifact
           SET RET TO TRUE.
           EVALUATE WB-SATZ(1:4)
               WHEN "UNB+"
               WHEN "UNH+" NEXT SENTENCE
               WHEN "BGM+" PERFORM BGM-NR                *> Lfs.-Nr.
               WHEN "DTM+" PERFORM DTM-DAT               *> Datum
               WHEN "RFF+" PERFORM RFF-NR                *>
               WHEN "MOA+" PERFORM MOA-BET
               WHEN "UNT+" NEXT SENTENCE
               WHEN "UNZ+" SET ESC TO TRUE.
           MOVE SPACE TO WB-SATZ.
           IF not ESC GO E.
       K.  CALL "CBL_CLOSE_FILE" USING WB-FH.
           IF ESC GO X.


       X.  CALL "CBL_CLOSE_FILE" USING WB-FH.
       Z.  EXIT.
      ******************************************************************
       BGM-NR SECTION.
       Z.  EXIT.
      ******************************************************************
       DTM-DAT SECTION.
       Z.  EXIT.
      ******************************************************************
       RFF-NR SECTION.
       Z.  EXIT.
      ******************************************************************
       MOA-BET SECTION.
       Z.  EXIT.
      ******************************************************************
       WRITE-DT SECTION.
       A.  WRITE DTR-SATZ.
           ADD 1 TO WZ-RECORD-OK.
           ADD 1117 VDU-ECK GIVING VDU-LP.
           DISPLAY WZ-RECORD-OK with highlight AT VDU-LP.
       Z.  EXIT.
      ******************************************************************
       PROTOKOLL SECTION.
       A.  IF WZ-ZEILE = 0 MOVE 99 TO WZ-ZEILE.
           MOVE SPACE TO DRA-SATZ.
           MOVE WS-ANZ-UMS     TO DRA-LFDNR.
           MOVE WU-LEITWEG     TO DRA-LEITWEG.
           MOVE WU-KONTONR-17  TO DRA-KONTONR.
           MOVE ZP-BETRAG      TO DRA-BETRAG.
           MOVE WF-NAME-30     TO DRA-NAME.
           MOVE ZP-VERWEND     TO DRA-VERWENDUNG.
           MOVE WU-KUNDENDATEN TO DRA-KUNDENDATEN.
           MOVE WU-FUNKTION    TO DRA-FUNKTION.
           MOVE WU-FORMULAR    TO DRA-FORMULAR.
           MOVE WU-TEXTZEILEN  TO DRA-TXTZEILEN.
           ADD WZ-SCHALT TO WZ-ZEILE.
           IF WZ-ZEILE > 60 PERFORM KOPFDRUCK.
           PERFORM ZEILE.
       Z.  EXIT.
      ******************************************************************
       TEILPROT SECTION.
       A.  IF WZ-ZEILE = 0 MOVE 99 TO WZ-ZEILE.
           MOVE SPACE TO DRA-SATZ.
           MOVE ZP-BETRAG      TO DRA-BETRAG.
           MOVE WF-NAME-30     TO DRA-NAME.
           MOVE ZP-VERWEND     TO DRA-VERWENDUNG.
           MOVE WU-KUNDENDATEN TO DRA-KUNDENDATEN.
           MOVE WU-TEXTZEILEN  TO DRA-TXTZEILEN.
           ADD WZ-SCHALT TO WZ-ZEILE.
           IF WZ-ZEILE > 60 PERFORM KOPFDRUCK.
           PERFORM ZEILE.
       Z.  EXIT.
      ******************************************************************
       ZEILE SECTION.
       A.  WRITE DR-SATZ AFTER WZ-SCHALT.
           MOVE SPACE TO DR-SATZ.
           MOVE 1 TO WZ-SCHALT.
       Z.  EXIT.
      ******************************************* Erstellungsprotokoll *
       OPEN-PRT SECTION.
       A.  OPEN OUTPUT DRUCKER.
           IF WM-DRU = 0 MOVE x"1B210400" TO DR-SATZ
                         WRITE DR-SATZ AFTER 0
                         MOVE x"1B43000C" TO DR-SATZ
           else MOVE WE-STG(7) TO DR-SATZ.
           WRITE DR-SATZ AFTER 0.
           MOVE SPACE TO DR-SATZ.
       Z.  EXIT.
      ******************************************************************
       CLOSE-PRT SECTION.
       A.  IF WM-DRU = 0 MOVE x"1B210000" TO DR-SATZ.
           WRITE DR-SATZ AFTER 0.
           MOVE SPACE TO DR-SATZ.
           WRITE DR-SATZ AFTER PAGE.
           CLOSE DRUCKER.
       Z.  EXIT.
      ******************************************************************
       KOPFDRUCK SECTION.
       A.  MOVE DR-SATZ TO WM-DRSATZ.
           MOVE SPACE TO DR-SATZ.
           ADD 1 TO WZ-SEITE.
       B.  MOVE "DATENTRAEGER-ERSTELLUNGSPROTOKOLL " TO DRK-LISTBEZ.
           MOVE "vom:" TO DRK-TXVON.
           MOVE WM-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRK-VONDAT.
           MOVE " fÅr BLZ:" TO DRK-TEXT1.
           MOVE WV-BLZEMPF  TO DRK-BLZ.
           MOVE "Blatt "    TO DRK-TEXT2.
           MOVE WZ-SEITE    TO DRK-SEITNR.
           PERFORM ZEILE.
           MOVE SPACE TO DR-SATZ.
           MOVE "Konto"     TO DRH-TEXT1.
           MOVE WV-KTOABS   TO DRH-KONTO.
           MOVE WV-NAME-ABSENDER TO DRH-ABSENDER.
           MOVE "DT per "   TO DRH-TEXT2.
           MOVE VDU-DATUM   TO DRH-DATUM.
           MOVE WH-HHMM TO DRH-ZEIT.
           INSPECT DRH-ZEIT REPLACING ALL SPACE BY ":".
           MOVE "Kontroll-Nr." TO DRH-TEXT3.
           MOVE WV-BESTANDKTRNR TO DRH-KTRNR.
           IF WV-BELEGART = 49 MOVE "GUTSCHRIFTEN" TO DRH-TEXT4.
           IF WV-BELEGART = 89 MOVE "LASTSCHRIFTEN" TO DRH-TEXT4.
           MOVE 2 TO WZ-SCHALT.
           PERFORM ZEILE.
       D.  MOVE "Lfd.Nr     Leitweg Kontonummer       Betrag Name d. Emp
      -         "fÑngers/Zahlungspflichtigen Verwendungszweck         Ku
      -         "ndendaten SF TZ." TO DR-SATZ(6:).
           MOVE 2 TO WZ-SCHALT.
           PERFORM ZEILE.
           MOVE 2 TO WZ-SCHALT.
       E.  MOVE WM-DRSATZ TO DR-SATZ.
           MOVE 6 TO WZ-ZEILE.
       Z.  EXIT.
      ******************************************************************
       SUMMENDRUCK SECTION.
       A.  MOVE "BetrÑge       Leitwege  Kontonummern     StÅck   Textze
      -       "ilen" TO DR-SATZ(45:).
           MOVE 2 TO WZ-SCHALT.
           PERFORM ZEILE.
       B.  MOVE "DT-Kontrollsummen:" TO DRS-TEXT.
           MOVE WS-KTRSUMME-BTR TO DRS-BETRAEGE.
           MOVE WS-KTRSUMME-LW  TO DRS-LEITWEGE.
           MOVE WS-KTRSUMME-KTO TO DRS-KTONR.
           MOVE WS-ANZ-UMS      TO DRS-STK.
           MOVE WS-SUMME-ZUSINF TO DRS-TXTZEILEN.
           PERFORM ZEILE.
           MOVE 4 TO WZ-SCHALT.
       Z.  EXIT.
      ******************************************************************
       BEGL-KOPF SECTION.
       A.  MOVE DB-SATZ TO WM-D2SATZ.
           IF W2-ZEILE > 60 WRITE DB-SATZ AFTER PAGE
               MOVE 0 TO W2-ZEILE.
           ADD 1 TO W2-SEITE.
           MOVE "D A T E N T R é G E R - B E G L E I T Z E T T E L"
                TO DB-SATZ(6:).
           MOVE "Datum:"    TO DBK-DATUMTX.
           MOVE VDU-DATUM   TO DBK-DATUM.
           MOVE "Seite:"    TO DBK-STX.
           MOVE W2-SEITE    TO DBK-SEITE.
           PERFORM BEGL-ZEILE.
           MOVE ALL "-" TO DBK-STRICH.
           MOVE 1 TO W2-SCHALT.
           PERFORM BEGL-ZEILE.
           MOVE 2 TO W2-SCHALT.
           MOVE 5 TO W2-ZEILE.
           IF W2-SEITE > 1 GO TO X.
           MOVE "EmpfÑnger:" TO DBA-TXT1.
           MOVE WP-EMPFBANK TO DBA-TXT2.
           PERFORM BEGL-ZEILE.
           MOVE "Absender:" TO DBA-TXT1.
           MOVE WP-ABSENDER TO DBA-TXT2.
           MOVE "Telefon/FS:" TO DBA-TXT3.
           MOVE WP-TELEFON  TO DBA-TXT4.
           MOVE 2 TO W2-SCHALT.
           PERFORM BEGL-ZEILE.
           MOVE "Bankleitzahl/Kto:" TO DBA-TXT1.
           MOVE WP-BLZKTO   TO DBA-TXT2.
           PERFORM BEGL-ZEILE.
           MOVE "Erstellungsdatum:" TO DBA-TXT1.
           MOVE VDU-DATUM TO DBA-TXT2.
           MOVE "DurchfÅhrungsdatum:" TO DBA-TXT3.
           IF WP-DURCHDAT = 0 MOVE ALL "." TO DBA-TXT4
           ELSE MOVE WP-DURCHDAT TO DBA-DDAT
                INSPECT DBA-DDAT REPLACING ALL SPACE BY "-".
           MOVE 2 TO W2-SCHALT.
           PERFORM BEGL-ZEILE.
           MOVE "BESTéNDE:" TO DBA-TXT1.
           MOVE 2 TO W2-SCHALT.
           PERFORM BEGL-ZEILE.
           MOVE 14 TO W2-ZEILE.
           MOVE 2 TO W2-SCHALT.
       X.  MOVE "LNr Best.KNr. Wertst. FA       Betragssumme  Ums.  Infz
      -        "." TO DB-SATZ(6:).
           PERFORM BEGL-ZEILE.
           MOVE ALL "-" TO DBB-STRICH.
           PERFORM BEGL-ZEILE.
           MOVE 2 TO W2-SCHALT.
           MOVE WM-D2SATZ TO DB-SATZ.
       Z.  EXIT.
      ******************************************************************
       BEGL-BESTAND SECTION.
       A.  IF W2-ZEILE = 0 MOVE 99 TO W2-ZEILE.
           ADD 1 TO WZ-BESTAND.
           MOVE WZ-BESTAND  TO DBB-LFDNR.
           MOVE WS-BELEGART TO DBB-FORM.
           MOVE WS-BESTANDKTRNR TO DBB-BKN.
           MOVE WS-SUMME-ZUSINF TO DBB-INFO.
           ADD  WS-SUMME-ZUSINF TO WZ-TXT.
           MOVE WS-KTRSUMME-BTR TO DBB-BETRAG.
           MOVE WS-ANZ-UMS      TO DBB-UMS.
           ADD WS-ANZ-UMS TO WZ-UMSATZ.
           ADD WS-ANZLAST TO WS-SUANZ-LAST.
           ADD WS-ANZGS TO WS-SUANZ-GS
           ADD WS-SUMLAST TO WS-GESUM-LAST.
           ADD WS-SUMGS  TO WS-GESUM-GUT.
           PERFORM BEGL-DRUCK.
       Z.  EXIT.
      ******************************************************************
       BEGL-SUMMEN SECTION.
       A.  MOVE "Anz. BestÑnde:" TO DBC-TXT1.
           MOVE "Anz. UmsÑtze:"  TO DBC-TXT2.
           MOVE "Anz. Zus.inf.:" TO DBC-TXT3.
           MOVE WZ-BESTAND       TO DBC-BEST.
           MOVE WZ-UMSATZ        TO DBC-UMS.
           MOVE WZ-TXT           TO DBC-INFO.
           MOVE 2 TO W2-SCHALT.
           PERFORM BEGL-ZEILE.
           MOVE "Anz. Lastschriften:" TO DBC-TXT2.
           MOVE "Anz. Gutschriften:"  TO DBC-TXT3.
           MOVE WS-SUANZ-LAST TO DBC-UMS.
           MOVE WS-SUANZ-GS  TO DBC-INFO.
           PERFORM BEGL-ZEILE.
           MOVE "Betr.Lastschriften:" TO DBC-TXT2.
           MOVE "Betr.Gutschriften:"  TO DBC-TXT3.
           MOVE WS-GESUM-LAST TO DBC-BETR.
           MOVE WS-GESUM-GUT  TO DBC-INFO-BETR.
           MOVE DB-SATZ(42:) TO DB-SATZ(6:).
           MOVE 2 TO W2-SCHALT.
           PERFORM BEGL-ZEILE.
           MOVE SPACE TO DB-SATZ.
           PERFORM BEGL-ZEILE.
           MOVE 0 TO WZ-TXT WZ-UMSATZ WZ-BESTAND.
           MOVE 0 TO WS-SUANZ-LAST WS-GESUM-LAST.
           MOVE 0 TO WS-SUANZ-GS WS-GESUM-GUT.
           WRITE DB-SATZ BEFORE PAGE.
       Z.  EXIT.
      ******************************************************************
       BEGL-DRUCK SECTION.
       A.  IF W2-ZEILE > 60 PERFORM BEGL-KOPF.
           PERFORM BEGL-ZEILE.
       Z.  EXIT.
      ******************************************************************
       BEGL-ZEILE SECTION.
       A.  WRITE DB-SATZ AFTER W2-SCHALT.
           MOVE SPACE TO DB-SATZ.
           ADD W2-SCHALT TO W2-ZEILE.
           MOVE 1 TO W2-SCHALT.
       Z.  EXIT.
      ************************************************** Begleitzettel *
       OPEN-BGL SECTION.
       A.  OPEN OUTPUT DRUCKB.
           IF WM-DRU = 0 MOVE x"1B210400" TO DB-SATZ
                         WRITE DB-SATZ AFTER 0
                         MOVE x"1B43000C" TO DB-SATZ
           else MOVE WE-STG(7) TO DB-SATZ.
           WRITE DB-SATZ AFTER 0.
           MOVE SPACE TO DB-SATZ.
       Z.  EXIT.
      ******************************************************************
       CLOSE-BGL SECTION.
       A.  IF WM-DRU = 0 MOVE x"1B210000" TO DB-SATZ.
           WRITE DB-SATZ AFTER 0.
           MOVE SPACE TO DB-SATZ.
           CLOSE DRUCKB.
       Z.  EXIT.
      ******************************************************************
       DTT-COPY SECTION.
       A.  CALL "CAUP" USING "0707100560000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " DatentrÑgerdiskette erstellen " AT VDU-LP.
           ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY " Bitte eine geeignete Diskette ins Laufwerk A: einle
      -        "gen!" with highlight AT VDU-LP.
           DISPLAY LOW-VALUE AT 1980.
           ADD 303 VDU-ECK GIVING VDU-LP.
       C.  DISPLAY "<esc>= Abbruch, <ret>= Start < >" AT VDU-LP.
           CALL "CAUP" USING "1003330000" WH-CREG.
           IF ESC GO X.
           IF not RET GO C.
       E.  CALL "CAUP" USING "1320012580" WH-CREG.
           MOVE "A:\FIBANK.DAT" TO WN-NACH.
           CALL "CBL_COPY_FILE" USING WN-DATTR WN-NACH.
       G.  DISPLAY "Weiter mit belibieger Taste < >" with highlight
               AT 2301.
           DISPLAY "<>= wiederholen" AT 2401.
           CALL "CAUP" USING "002330000" WH-CREG.
           IF WOLI GO E.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           MOVE 9 TO WH-PG.
       Z.  EXIT.
      ******************************************************************
       VORLAUF SECTION.
       A.  MOVE "DTT-PRT.LST" TO WH-DRUNAM.
           MOVE "83" TO WH-SA-LAST.
           MOVE SPACE TO WH-VOR-BELEGART.
           MOVE 0 TO WM-VOR-BANK-BLZ WM-VOR-BANK-KTO WH-VOR-VALUTA
                     WZ-BESTAND WZ-UMSATZ WZ-TXT WP-BISDAT
                     WP-DURCHDAT.
           MOVE 1 TO WB-BANK-NR.
           MOVE 0 TO WB-ABSBKTO WB-ABSBLZL.
      *-------------------------------> eigene Kto-Nr. u. Blzl. laden <-
           MOVE KB-BLZ TO WT-TX.
           PERFORM FILTER.
           MOVE WH-NUM TO WB-ABSBLZL.
           MOVE WT-TX TO WP-BLZKTO(1:).
           MOVE KB-KTONR TO WT-TX.
           PERFORM FILTER.
           MOVE WH-NUM TO WB-ABSBKTO.
           MOVE WF-TX TO WP-BLZKTO(9:11).
           MOVE "/" TO WP-BLZKTO(7:1).
           CALL "CAUP" USING "0707101266000" WH-CREG.
           CALL "CAUP" USING "16CLOFEN" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " DatentrÑgererstellung " with highlight AT VDU-LP.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "Erstellungsdatum:" AT VDU-LP.
           ADD 1 TO VDU-L.
           DISPLAY "Absender........:" AT VDU-LP.
           ADD 1 TO VDU-L.
           DISPLAY "Bank............:" AT VDU-LP.
           ADD 1 TO VDU-L.
           DISPLAY "Blz/Konto.......:" AT VDU-LP.
           ADD 1 TO VDU-L.
           DISPLAY "Betrag..........:" AT VDU-LP.
           ADD 1 TO VDU-L.
           DISPLAY "durchzufÅhren am:" AT VDU-LP.
      *--------------------------> Absender, Bank, Telefon etc. laden <-
           ADD 321 VDU-ECK GIVING VDU-LP.
           MOVE WS-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           ADD 421 VDU-ECK GIVING VDU-LP.
           DISPLAY WP-ABSENDER with highlight AT VDU-LP.
           ADD 521 VDU-ECK GIVING VDU-LP.
           DISPLAY KB-BEZ AT VDU-LP.
           MOVE KB-BEZ TO WP-EMPFBANK.
           DISPLAY WP-EMPFBANK with highlight AT VDU-LP.
           ADD 621 VDU-ECK GIVING VDU-LP.
           DISPLAY WP-BLZKTO with highlight AT VDU-LP.
           ADD 721 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-BET with highlight AT VDU-LP " ".
           IF WS-BET = 0 DISPLAY "Keine Anweisungen vorhanden!" with
                             highlight blink AT VDU-LP
                         DISPLAY LOW-VALUE AT 2380
                         PERFORM WEITER
                         PERFORM EDIPROT
                         SET ESC TO TRUE GO Z.
           DISPLAY WM-FSYM with highlight foreground-color 6 AT 0000.
      *------------------------------------------> DurchfÅhrungsdatum <-
       P.  ADD 821 VDU-ECK GIVING VDU-LP.
           MOVE WS-DATUM TO WC-DATUM WH-WERT.
           PERFORM DATDREH.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           MOVE WS-DATUM TO WZ-DATUM WH-WERT.
           DISPLAY "<esc>= Abbruch, <ret>= DurchfÅhrungsdatum" AT 2301.
           CALL "CAUP" USING "1108216006" WH-CREG.
           IF ESC GO Z.
           IF not RET GO P.
           MOVE WX-DATUM TO WP-DURCHDAT WM-DURCHDAT.
           IF WP-DURCHDAT = 0 MOVE WS-DATUM TO WP-DURCHDAT.
           IF WZ-DATUM = 0 DISPLAY "Datum falsch! " AT 2401 VDU-DATUM.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
       Q.  DISPLAY "<esc>= Abbruch, <ret>= Start, <>= zurÅck < >"
                AT 2301.
           CALL "CAUP" USING "0023440000" WH-CREG.
           IF ESC GO Z.
           IF WOLI GO P.
           IF not RET GO Q.
           ADD 0935 VDU-ECK GIVING VDU-LP.
           DISPLAY "Datendatei:      '" AT VDU-LP WN-DATTR "'".
           ADD 1 TO VDU-L.
           DISPLAY "Protokoll:       'DTT-PRT.LST'" AT VDU-LP.
           ADD 1 TO VDU-L.
           DISPLAY "Begleitzettel:   'DTT-BGL.LST'" AT VDU-LP.
           MOVE 0 TO WZ-RECORD.
       Z.  EXIT.
      ******************** Erlaubt sind nur Gro·buchstaben und Ziffern *
       YKLGR SECTION.
       A.  PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 38
               MOVE WT-TX(WX:1) TO WM-X
               EVALUATE WM-X
                   WHEN "Ñ" MOVE "é" TO WM-X
                   WHEN "é" MOVE "é" TO WM-X
                   WHEN "î" MOVE "ô" TO WM-X
                   WHEN "ô" MOVE "ô" TO WM-X
                   WHEN "Å" MOVE "ö" TO WM-X
                   WHEN "ö" MOVE "ö" TO WM-X
                   WHEN "·" MOVE "S" TO WM-X
                   WHEN LOW-VALUE MOVE SPACE TO WM-X
                   WHEN OTHER
                       IF WM-X > x"60" SUBTRACT 32 FROM WM-A
               END-EVALUATE MOVE WM-X TO WT-TX(WX:1).
           MOVE WT-TX TO WM-CD.
           INSPECT WM-CD REPLACING ALL "Ñ" by "‰"
                    ALL "î" by  "ˆ" ALL "Å" by "¸"
                    ALL "·" by "ﬂ"  ALL "é" by "ƒ"
                    ALL "ô" by "÷"  ALL "ö" by "‹".
       Z.  EXIT.
      ******************************************* Bankeinzugerstellung *
       BK-EINZUG SECTION.
       A.  MOVE "     BankeinzÅge" TO WK-GEB.
           CALL "CAUP" USING "06KOPF" WH-CREG.
           PERFORM LAD-FIRMA.
           PERFORM BANKWAHL.
           IF ESC GO Z.
           PERFORM BANK-ANZ.
           DISPLAY "FÅr alle bis: 12.12.88 fÑlligen Rechnungen"
               AT 1010.
           DISPLAY "wird ein Bankeinzug durchgefÅhrt!" AT 1210.
           MOVE WM-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           DISPLAY VDU-DATUM with highlight AT 1024.
           MOVE WM-DATUM TO WZ-DATUM WH-WERT.
       C.  DISPLAY "<esc>= Abbruch, <ret/Entf>= Datum" AT 2301.
           DISPLAY "<Entf> statt <ret>= Lîschung EinzÅge" AT 2401.
           CALL "CAUP" USING "0110246006" WH-CREG.
           IF ESC GO Z.
           SET EINZUG TO TRUE.
           IF not RET and not ENTF GO C.
           IF WZ-DATUM = 0 GO C.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           MOVE WX-DATUM TO WS-DATUM WV-VERDAT WM-DATUM.
           IF ENTF PERFORM LOE-EINZUG
                   GO Z.
       E.  DISPLAY "<ret/ >= Start, <esc>= Abbruch, <>= zurÅck < >"
               AT 2301.
           DISPLAY "< > statt <ret>= Posten einzeln anzeigen" AT 2401.
           MOVE 0 TO WH-MOD.
           CALL "CAUP" USING "0023460000" WH-CREG.
           IF ESC GO Z.
           IF AB MOVE 1 TO WH-MOD
                 SET RET TO TRUE.
           IF not RET GO E.
           MOVE 0 TO WS-BET WZ-BET.
      *----------------------------------> Start Einzugsbanken suchen <-
           OPEN INPUT BANKDAT.
           OPEN I-O ZAHLPOS.
           MOVE LOW-VALUE TO BK-SATZ.
           MOVE 2 TO BK-DK.
           START BANKDAT KEY not < BK-KEY INVALID PERFORM NO-REC GO Z.
       F.  READ BANKDAT NEXT AT END GO K.
           IF ZUGRIF PERFORM BESETZT GO F.
           MOVE LOW-VALUE TO DE-SATZ.
      *------------------------------------> Start OP-Suche u. Einzug <-
           MOVE BK-KTONR TO DE-KTONR.
       G.  READ DEBITOR IGNORE LOCK INVALID DELETE BANKDAT GO F.
           MOVE DE-BEZ TO WT-BEZ.
           INSPECT WT-BEZ REPLACING ALL "#" BY SPACE.
           DISPLAY DE-KTONR AT 2010 " "
               WT-BEZ(1:60) with foreground-color 3.
           IF DE-OPKZ not = 20 GO F.
           PERFORM OB-EINZUG.
           IF WT-ESUMME not > 0 GO F.
           MOVE 0 TO IX.
       H.  ADD 1 TO IX.
           IF WT-EKEY(IX) = 0 GO F.                       *> Kundenende
           MOVE LOW-VALUE TO OP-SATZ.
           MOVE WT-EKEY(IX) TO OP-KEY.
       I.  READ OPDATEI INVALID GO F.
           IF ZUGRIF PERFORM BESETZT GO I.
           MOVE OP-KOND TO WH-KK.
           COMPUTE WA-TAGE = FUNCTION INTEGER-OF-DATE(OP-VALDAT).
           IF WH-TG1 not = 0 ADD WH-TG1 TO WA-TAGE
           else IF WH-TG2 NOT = 0 ADD WH-TG2 TO WA-TAGE
                else ADD WH-TGN TO WA-TAGE.
           COMPUTE WB-TAGE = FUNCTION DATE-OF-INTEGER(WA-TAGE).
           IF WB-TAGE > WV-VERDAT GO H.
           COMPUTE WH-WERT = OP-ZABETRAG + OP-ZPBET + OP-ZPSKTO.
           SUBTRACT WH-WERT FROM OP-REBETRAG GIVING ZP-BETRAG.
           MOVE 2 TO ZP-DK.
           MOVE OP-KTONR TO ZP-KTONR.
           MOVE OP-VALDAT TO ZP-VALDAT.
           MOVE WV-VERDAT TO ZP-DATUM.
           MOVE 0 TO ZP-LFD.
           ADD 50 WM-BANK GIVING ZP-BAKEY.
           MOVE OP-REDAT TO ZP-REDAT.
           MOVE BK-BANKKTO TO ZP-BAKTO.
           MOVE BK-BANK TO ZP-BABEZ.
           MOVE WT-BEZ TO ZP-EMPFG.
      *--------------------------------------------> Skonto berechnen <-
      *    MOVE OP-KOND TO WH-KK.
           IF WH-MOD = 1 PERFORM SKTOEIN
           else IF ZP-BETRAG < 0 and WH-SK1 = 0 PERFORM SKTOEIN.
           IF ENTF SET RET TO TRUE GO H.
           MOVE WH-SK1 TO WZ-SKTO.
           IF OP-SKTOBAS = 0 MOVE OP-REBETRAG TO OP-SKTOBAS.
           COMPUTE WZ-SKTO = OP-SKTOBAS * WZ-SKTO / 100.
           SUBTRACT WZ-SKTO FROM ZP-BETRAG GIVING ZP-BETRAG.
           PERFORM VERWEND.
      *---------------------------------------> Einzugssatz schreiben <-
           MOVE OP-LFD TO ZP-LFD.
           MOVE WZ-SKTO TO ZP-SKTO OP-ZPSKTO.
           WRITE ZP-SATZ INVALID REWRITE ZP-SATZ.
           MOVE ZP-BETRAG TO OP-ZPBET.
           ADD ZP-BETRAG TO WS-BET WZ-BET.
           REWRITE OP-SATZ.
           GO H.
       K.  CALL "CAUP" USING "1317012580" WH-CREG.
           MOVE WV-DATUM TO WM-DATUM.
           ADD 50 TO KB-NUM GIVING WV-BAKEY.
           MOVE 131 TO VDU-ECK.
           PERFORM ZAHLSUM.
           PERFORM BANKDT.
           PERFORM EDIPROT.
           CLOSE BANKDAT.
           CLOSE ZAHLPOS.
       Z.  EXIT.
      ***************************************** ob Saldo > 0 vorhanden *
       OB-EINZUG SECTION.
       A.  MOVE 1 TO IX.
           MOVE LOW-VALUE TO OP-SATZ WT-EINZUG.
           MOVE 2 TO OP-DK.
           MOVE DE-KTONR TO OP-KTONR.
           START OPDATEI KEY not < OP-KEY INVALID GO Z.
       I.  READ OPDATEI NEXT AT END GO Z.
           IF ZUGRIF PERFORM BESETZT GO I.
           IF OP-KTONR not = DE-KTONR GO Z.
           IF OP-VALDAT > WV-VERDAT GO I.
           COMPUTE WH-WERT = OP-ZABETRAG + OP-ZPBET + OP-ZPSKTO.
           SUBTRACT WH-WERT FROM OP-REBETRAG GIVING ZP-BETRAG.
           IF ZP-BETRAG = 0 GO I.
           MOVE OP-KEY TO WT-EKEY(IX).
           ADD 1 TO IX.
           ADD ZP-BETRAG TO WT-ESUMME.
           GO I.
       Z.  EXIT.
      ********************************* Skontoeingabe bei Gutschriften *
       SKTOEIN SECTION.
       A.  CALL "CAUP" USING "0717070466000" WH-CREG.
           ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "Kde.:" AT VDU-LP.
           ADD 6 TO VDU-LP.
           DISPLAY WT-BEZ(1:57) with foreground-color 3 AT VDU-LP.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "Beleg-Nr.:          vom:           S
      -        "%:     " AT VDU-LP.
           MOVE OP-RENUM TO WD-BELNR.
           ADD 314 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-BELNR with highlight AT VDU-LP.
           ADD 328 VDU-ECK GIVING VDU-LP.
           MOVE OP-REDAT TO WC-DATUM.
           PERFORM DATDREH.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           MOVE WB-TAGE TO WC-DATUM.
           PERFORM DATDREH.
           ADD 428 VDU-ECK GIVING VDU-LP.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           MOVE OP-REBETRAG TO WD-BET.
           ADD 339 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-BET with highlight AT VDU-LP.
           MOVE WH-SK1 TO WD-SKT.
           ADD 358 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-SKT with highlight AT VDU-LP.
       E.  DISPLAY "<ret>= Skontoeingabe, <Entf>= Ablehnen" AT 2301.
           MOVE WH-SK1 TO WH-WERT WD-SKT.
           ADD 358 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-SKT with highlight AT VDU-LP.
           CALL "CAUP" USING "1003582103" WH-CREG.
           IF ENTF GO X.
           IF not RET GO E.
           IF WH-WERT not = 0 and (OP-B1 = 0 and OP-B2 = 0)
               DISPLAY "Kein Skonto mîglich!" AT 2401
               MOVE 0 TO WH-SK1
               PERFORM WEITER GO E.
           MOVE WH-WERT TO WH-SK1 WD-SKT.
           DISPLAY WD-SKT with highlight AT VDU-LP.
       G.  DISPLAY "<ret>= ok, <>= zurÅck < >" AT 2301.
           CALL "CAUP" USING "0023250000" WH-CREG.
           IF WOLI GO E.
           IF not RET GO G.
           MOVE WH-WERT TO WH-SK1.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      **************************************** Auswahl der Einzugsbank *
       BANKWAHL SECTION.
       A.  CALL "CAUP" USING "1303012480" WH-CREG.
           DISPLAY " Einzug per: " with reverse-video AT 0358.
           MOVE WM-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           DISPLAY VDU-DATUM with highlight AT 0372.
           MOVE WM-DATUM TO WZ-DATUM WH-WERT.
       C.  DISPLAY "<esc>= Abbruch, <ret>= DurchfÅhrungsdatum" AT 2301.
           CALL "CAUP" USING "0103726006" WH-CREG.
           IF ESC GO Z.
           IF not RET GO A.
           IF WZ-DATUM = 0 GO A.
           IF WZ-DATUM < WH-DATUM GO A.
           MOVE WX-DATUM TO WV-DATUM.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
       E.  CALL "CAUP" USING "1303012457" WH-CREG.
           DISPLAY "Bank:" AT 0301.
           DISPLAY "<esc>= Abbruch, <ret>= Nr. lt. Stammdatenanl."
               AT 2301.
           DISPLAY "<ret-leer>= Banken anzeigen" AT 2401.
           CALL "CAUP" USING "0003073003" WH-CREG.
           IF ESC GO Z.
           IF RET AND WH-NUM = 0 CALL "FIBSTAM" USING "10BANK" WH-CREG
                                 CANCEL "FIBSTAM".
           IF not RET GO E.
           IF WH-NUM = 0 GO E.
           MOVE WH-NUM TO WD-TGN WM-BANK.
           DISPLAY WD-TGN with highlight AT VDU-LP.
           ADD WH-NUM 70 GIVING WH-KEY.
       D.  READ KONSTANT IGNORE LOCK INVALID PERFORM NO-REC GO E.
           IF ZUGRIF PERFORM BESETZT GO D.
       Z.  EXIT.
      ************************************************ Bank einblenden *
       BANK-ANZ SECTION.
       A.  CALL "CAUP" USING "1303012456" WH-CREG.
           DISPLAY "Bank:" AT 0301.
           DISPLAY KB-KTONR AT 0307.
           DISPLAY KB-BEZ AT 0323.
       Z.  EXIT.
      ******************************************************************
       VERWEND SECTION.
       A.  MOVE OP-REDAT TO WC-DATUM.
           PERFORM DATDREH.
           MOVE OP-RENUM TO WD-BELNR.
           MOVE "Re.Nr." TO ZP-VERWEND.
           MOVE WD-BELNR TO ZP-VERWEND(8:).
           MOVE "/" TO ZP-VERWEND(15:1).
           MOVE VDU-DATUM TO ZP-VERWEND(15:9).
           IF WZ-SKTO = 0 GO Z.
           IF WZ-SKTO not = 0 MOVE WZ-SKTO TO WD-SKTO
               MOVE "-" TO ZP-VERWEND(25:1)
               MOVE 27 TO WZ
               PERFORM VARYING WY FROM 1 BY 1 UNTIL WY > 10
                   IF WD-SKTO(WY:1) not = SPACE
                       MOVE WD-SKTO(WY:1) TO ZP-VERWEND(WZ:1)
                       ADD 1 TO WZ
               end-perform
                   MOVE " Skto" TO ZP-VERWEND(WZ:6).
       Z.  EXIT.
      ************************************************** Einzuglîschen *
       LOE-EINZUG SECTION.
       A.  CALL "CAUP" USING "1305012480" WH-CREG.
           DISPLAY "Lîschung BankeinzÅge vom: " AT 1010.
           DISPLAY VDU-DATUM with highlight AT 0000.
           OPEN I-O ZAHLPOS.
       C.  DISPLAY "<esc>= Abbruch, <Entf>= Start Lîschung <       >"
               AT 2301.
           CALL "CAUP" USING "0023416108" WH-CREG.
           IF ESC GO X.
           IF not ENTF GO C.
           MOVE 0 TO ZP-KTONR ZP-VALDAT ZP-LFD WH-LOE.
           IF WH-NUM not = 0 MOVE WH-NUM TO WV-KTONR ZP-KTONR WH-LOE.
           MOVE 2 TO ZP-DK.
           START ZAHLPOS KEY > ZP-KEY INVALID PERFORM NO-REC GO X.
       E.  READ ZAHLPOS NEXT AT END GO X.
           IF ZUGRIF PERFORM BESETZT GO E.
           IF WH-LOE not = 0 and WV-KTONR = ZP-KTONR PERFORM LOEANZEIG
               EVALUATE TRUE
                   WHEN RET GO E
                   WHEN SENT GO G
                   WHEN OTHER GO C.
           IF ZP-DATUM not = WS-DATUM or ZP-DK not = 2 GO E.
       G.  DELETE ZAHLPOS INVALID GO E.
           IF ZUGRIF PERFORM BESETZT GO G.
           MOVE ZP-KEY TO OP-KEY.
       I.  READ OPDATEI INVALID GO E.
           IF ZUGRIF PERFORM BESETZT GO I.
           SUBTRACT ZP-BETRAG FROM OP-ZPBET.
           SUBTRACT ZP-SKTO FROM OP-ZPSKTO.
           REWRITE OP-SATZ.
           GO E.
       X.  CLOSE ZAHLPOS.
       Z.  EXIT.
      ****************************************** fÅr Lîschung anzeigen *
       LOEANZEIG SECTION.
       A.  CALL "CAUP" USING "271510036000010" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Bankeinzug " with highlight AT VDU-LP.
           ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "Kto.Nr: 12.345,2 Datum: 12.12.08 Betrag: 12.356,12"
               AT VDU-LP.
           COMPUTE WD-KTONR = ZP-KTONR / 10.
           ADD 211 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-KTONR with highlight AT VDU-LP.
           MOVE ZP-VALDAT TO WC-DATUM.
           PERFORM DATDREH.
           ADD 227 VDU-ECK GIVING VDU-LP.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           MOVE ZP-BETRAG TO WD-BET.
           ADD 242 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-BET(4:) with highlight AT VDU-LP.
       C.  DISPLAY "<esc>= Abbruch, <ret>= weiter anzeigen, <Strg+Entf>=
      -        " lîschen < >" AT 2301.
           CALL "CAUP" USING "0023630000" WH-CREG.
           IF RET GO X.
           IF ESC GO X.
           IF not SENT GO C.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
