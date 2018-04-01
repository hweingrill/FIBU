      $SET LINKCOUNT "240" ANS85 BOUND AUTOLOCK NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     FIBMAHN.
      ******************************************************************
      *        Mahnung und Zinsenberechnung                            *
      ******************************************************************
       ENVIRONMENT    DIVISION.
       CONFIGURATION   SECTION.
       SOURCE-COMPUTER.  PC.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY FIBUSEC.CPY.
           SELECT ZINSEN   ASSIGN TO "FIBUZINS.DAT"
                           ORGANIZATION INDEXED, ACCESS DYNAMIC
                           RECORD KEY ZI-BZEIL
                           ALTERNATE KEY ZI-KEY DUPLICATES
                           FILE STATUS WF-STATUS.
           SELECT DRUCKER  ASSIGN TO PRINTER WH-DRUNAM
                           FILE STATUS WF-STATUS.
           SELECT MAHNTXT  ASSIGN TO WN-MAHN
                           ORGANIZATION LINE SEQUENTIAL
                           FILE STATUS WF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       COPY FIBUFD.CPY.
      **************************************************** Zinsendatei *
       FD  ZINSEN                      LABEL RECORD STANDARD.
       01  ZI-SATZ.
           03  ZI-KEY.
               05 ZI-NUM               PIC 9(6)       COMP.
               05 ZI-SA                PIC 99         COMP.
               05 ZI-VALDAT            PIC 9(8)       COMP.
         02 ZI-REST.
           03  ZI-BELDAT               PIC 9(6)       COMP.
           03  ZI-BELNR                PIC 9(7)       COMP.
           03  ZI-SYM                  PIC 99         COMP.
           03  ZI-BET                  PIC S9(7)V99   COMP.
           03  ZI-BZEIL.
               05 ZI-USE               PIC 99         COMP.
               05 ZI-SEITE             PIC 9(7)       COMP-X.
      ****************************************************** Textdatei *
       FD  MAHNTXT                     LABEL RECORD STANDARD.
       01  MX-SATZ                     PIC X(99).
      ******************************************************************
       FD  DRUCKER                     LABEL RECORD OMITTED.
       01  DRA-SATZ.
           03  FILLER                  PIC XXXX.
           03  DRA-STR                 PIC X(132).
       01  DRK-SATZ.
           03  FILLER                  PIC XXXX.
           03  DRK-STR.
               05 FILLER               PIC X.
               05 DRK-BUDAT            PIC X(9).
               05 DRK-BELNR            PIC Z(7)-.
               05 DRK-BELDAT           PIC X(9).
               05 DRK-SYM              PIC XXX.
               05 DRK-GGKTO            PIC ZZZZZ,Z-.
               05 DRK-GGART            PIC XX.
               05 DRK-JS               PIC ZZZ9-.
               05 DRK-BEZ              PIC X(41).
               05 DRK-MWSKT            PIC ZZ.ZZZ.ZZZ,ZZ-.
               05 FILLER               PIC X.
               05 DRK-UST              PIC ZZ-.
               05 DRK-SOLL             PIC ZZ.ZZZ.ZZZ,ZZ-.
               05 DRK-HABEN            PIC ZZ.ZZZ.ZZZ,ZZ-.
       01  DRP-SATZ.
           03  FILLER                  PIC X(9).
           03  DRP-STR.
               05 DRP-KTONR            PIC ZZ.999,9-.
               05 DRP-BEZ              PIC X(51).
               05 DRP-KZ               PIC XX.
               05 DRP-SOLL             PIC ZZZ.ZZZ.ZZ9,99-.
               05 DRP-HABEN            PIC ZZZ.ZZZ.ZZ9,99-.
               05 DRP-SALDO            PIC ZZZ.ZZZ.ZZ9,99-.
       01  DRO-SATZ.
           03  FILLER                  PIC X(7).
           03  DRO-SYM                 PIC XXX.
           03  DRO-REFNR               PIC ZZZZZZ9-.
           03  FILLER                  PIC XX.
           03  DRO-DAT                 PIC X(8).
           03  FILLER                  PIC XX.
           03  DRO-FAELLIG             PIC X(9).
           03  DRO-REBET               PIC ZZZZZ.ZZ9,99-.
           03  DRO-ZABET               PIC ZZZZZ.ZZ9,99-.
           03  FILLER                  PIC XXXX.
           03  DRO-OFFEN               PIC ZZZZZ.ZZ9,99-.
           03  FILLER                  PIC XX.
           03  DRO-MKZ                 PIC Z9.
       01  DRS-SATZ.
           03  FILLER                  PIC X(41).
           03  DRS-BEZ                 PIC X(16).
           03  DRS-SOLL                PIC ZZZZ.ZZZ.ZZ9,99-.
           03  FILLER                  PIC XXXX.
           03  DRS-HATX                PIC X(8).
           03  DRS-HABEN               PIC ZZZZ.ZZZ.ZZ9,99-.
           03  FILLER                  PIC XXXX.
           03  DRS-SALTX               PIC X(8).
           03  DRS-SALDO               PIC ZZZ.ZZZ.ZZ9,99-.
           03  FILLER                  PIC X(8).
       01  DRZ-SATZ.
           03  FILLER                  PIC X(8).
           03  DRZ-STR.
               05 DRZ-BELDAT           PIC X(9).
               05 DRZ-BELNR            PIC ZZZZ.ZZ9-.
               05 DRZ-SYM              PIC XXX.
               05 DRZ-VALDAT           PIC X(9).
               05 DRZ-S.
                  07 DRZ-SOLL          PIC ZZ.ZZZ.ZZ9,99-.
                  07 FILLER            PIC X(8).
               05 DRZ-H REDEFINES DRZ-S.
                  07 FILLER            PIC X(8).
                  07 DRZ-HABEN         PIC ZZ.ZZZ.ZZ9,99-.
               05 DRZ-SALDO            PIC ZZ.ZZZ.ZZ9,99-.
               05 DRZ-TAGE             PIC ZZ9-.
               05 DRZ-ZINSEN           PIC ZZZ.ZZ9,99-.
      ******************************************************************
       WORKING-STORAGE SECTION.
       01  WH-CALL.
           03  WL-CA                   PIC 99.
           03  WL-REST                 PIC 9(13).
       COPY WHCREG.CPY.
       01  WH-REG.
           03  WT-ERFTAB.
               05 WT-KEY               PIC X(8)      OCCURS 10.
           03  WK-DSTG.
               05 WK-STG               PIC X(10)     OCCURS 10.
           03  WH-P1                   PIC 99        COMP.
           03  WH-P2                   PIC 99        COMP.
           03  WX-PRNO                 PIC 99        COMP-X.
           03  WX-PRSTAT               PIC 99        COMP-X.
           03  WZ-SEITE                PIC 99        COMP   VALUE ZERO.
           03  WZ-SCHALT               PIC 99        COMP   VALUE ZERO.
           03  WZ-ZEILEN               PIC 99        COMP   VALUE ZERO.
           03  WH-DRUNAM               PIC X(12)     VALUE   "LPT1:".
           03  WM-OPEN                 PIC 9         COMP   VALUE ZERO.
           03  WM-WJAHR                PIC 9(6)      COMP   OCCURS 2.
           03  WM-ZEIL                 PIC 99        COMP.
           03  WV-KTONR                PIC 9(6)      COMP.
           03  WJ-DATUM                PIC 9(8)      COMP.
           03  WH-OPKEY                PIC X(9).
           03  WH-SIZE                 PIC 99        COMP   VALUE 63.
           03  WM-END                  PIC 99        COMP.
           03  WB                      PIC 99        COMP.
           03  WE                      PIC 99        COMP.
           03  WI                      PIC 99        COMP.
           03  WL                      PIC 99        COMP.
           03  WS                      PIC 99        COMP.
           03  IX                      PIC 99        COMP.
           03  IY                      PIC 99        COMP.
           03  IZ                      PIC 99        COMP.
           03  WK-MCODE                PIC X(10).
           03  WM-SOLL                 PIC S9(9)V99  COMP.
           03  WM-HABEN                PIC S9(9)V99  COMP.
           03  WS-SALDO                PIC S9(9)V99  COMP.
           03  WS-OPS                  PIC S9(9)V99  COMP.
           03  WS-SUM                  PIC S9(9)V99  COMP.
           03  WS-ZINS                 PIC S9(9)V99  COMP.
           03  WS-VORAUS               PIC S9(9)V99  COMP.
           03  WG                      PIC 99        COMP.
           03  WH-MOD                  PIC 99        COMP.
           03  WM-SMAHN                PIC 99        COMP.
           03  WM-Z                    PIC 99        COMP.
           03  WH-BKEY.
               05 WH-KTONR             PIC 9(6)      COMP.
               05 WH-BUDAT             PIC 9(6)      COMP.
               05 WH-LFD               PIC 9999      COMP.
           03  WH-KAT                  PIC 9         COMP.
           03  WH-ZINS                 PIC S9(9)V99  COMP.
           03  WH-ZTAG                 PIC 99        COMP.
           03  WH-ZPZ                  PIC S99V99    COMP.
           03  WV-ZPZ                  PIC S99V99    COMP.
           03  WH-SPES                 PIC S99V99    COMP.
      *-----------------------------------------------> Anzeigefelder <-
           03  WD-BET                  PIC ZZ.ZZZ.ZZ9,99-.
           03  WD-TGN                  PIC ZZ9-.
           03  WD-ZTAG                 PIC Z9-.
           03  WD-ZPZ                  PIC Z9,99.
           03  WD-SPES                 PIC Z9,99.
           03  WD-NUM                  PIC ZZZZZ9.
           03  WD-KTO                  PIC ZZZZ9,9.
           03  WD-KZ                   PIC 9.
           03  WD-POS                  PIC 9.
           03  WH-DRUSATZ              PIC X(128).
           03  WH-VALUTA               PIC 9(6)      COMP.
           03  WA-HEUTE                PIC 9(8)      COMP.
           03  WA-TAGE                 PIC 9(7)      COMP.
           03  WB-TAGE                 PIC 9(8)      COMP.
           03  WH-TAG                  PIC S999      COMP.
           03  WN-MAHN                 PIC X(15).
           03  WH-VON                  PIC 9(6)      COMP.
           03  WH-BIS                  PIC 9(6)      COMP.
      *------------------------------------------> Konditionentabelle <-
           03  WH-KK                   PIC 9(13).
           03  WR-KK REDEFINES WH-KK.
               05  WH-SK1              PIC 9V9.
               05  WH-TG1              PIC 999.
               05  WH-SK2              PIC 9V9.
               05  WH-TG2              PIC 999.
               05  WH-TGN              PIC 999.
       COPY FIBUEXT.CPY.
       COPY FIBUDECL.CPY.
       DECL-A SECTION.         USE AFTER ERROR PROCEDURE ON MAHNTXT.
       A.  CALL "CADECL" USING WN-MAHN WH-CREG.
       DECL-Z SECTION.         USE AFTER ERROR PROCEDURE ON ZINSEN.
       A.  CALL "CADECL" USING "FIBUZINS.DAT" WH-CREG.
       END DECLARATIVES.
      *****************************************************************
       STEUER SECTION.
       A.  MOVE WL-CALL TO WH-CALL.
           MOVE WL-CREG TO WH-CREG.
           CALL "CAUP" USING "06KOPF" WH-CREG.
           EVALUATE WL-CA
               WHEN 10 PERFORM LI-WAHL.
           MOVE WH-CREG TO WL-CREG.
       Z.  EXIT PROGRAM.
      *****************************************************************
       DATDREH SECTION.
       A.  MOVE WC-TAG  TO WZ-TAG VDU-JAHR.
           MOVE WC-MONAT TO WZ-MONAT VDU-MONAT.
           MOVE WC-JAHR TO WZ-JAHR VDU-TAG.
       Z.  EXIT.
      ******************************************************************
       BESETZT SECTION.
       A.  DISPLAY "Record - besetzt" AT 2401.
       Z.  EXIT.
      ******************************************************************
       WEITER SECTION.
       A.  DISPLAY " weiter mit <ret>: " with highlight AT 0000.
           MOVE SPACE TO WH-X.
           ACCEPT WH-X AT 0000.
           CALL "CAUP" USING "1324012480000" WH-CREG.
       Z.  EXIT.
      *****************************************************************
       NO-REC SECTION.
       A.  DISPLAY "keine Daten vorhanden," AT 2401.
           PERFORM WEITER.
           MOVE 07 TO WX-TASTE.
       Z.  EXIT.
      ******************************************************************
       NODATA SECTION.
       A.  DISPLAY "** keine Daten mehr **" with highlight blink
               AT 2458.
           SET FINE TO TRUE.
       Z.  EXIT.
      ************************************************* ob Drucker ok *
       DRU-OK SECTION.
       A.  IF WH-DRUNAM(1:3) NOT = "LPT" GO Z.
           MOVE 0 TO WX-PRNO.
           CALL "PC_TEST_PRINTER" USING WX-PRNO WX-PRSTAT.
           IF WX-PRSTAT =
               208 OR 192 OR 144 OR 128 OR 80 OR 64 OR 16 GO Z.
           DISPLAY "Drucker nicht bereit: Fehler beheben und" AT 2401
               PERFORM WEITER GO A.
       Z.  EXIT.
      ******************************************************* Drucker *
       DRUCK SECTION.
       A.  PERFORM DRU-OK.
       C.  WRITE DRA-SATZ AFTER WZ-SCHALT.
           IF WF-STATUS = 27 OR WF-SB = 27 GO C.
           MOVE SPACE TO DRA-SATZ.
           ADD WZ-SCHALT TO WZ-ZEILEN.
           MOVE 1 TO WZ-SCHALT.
       Z.  EXIT.
      ****************************** Druckerrueckstellung auf 10/Zoll *
       END-DRU SECTION.
       A.  IF WM-OPEN = 0 GO Z.
           MOVE 46 TO WX-PRNO.
           CALL X"91" USING WX-PRSTAT WX-PRNO DRUCKER.
           MOVE SPACE TO DRA-SATZ.
           CLOSE DRUCKER.
           MOVE 0 TO WM-OPEN.
       Z.  EXIT.
      ******************************************************************
       BEG-DRU SECTION.
       A.  PERFORM DRU-OK.
           IF WM-OPEN > 0 GO X.
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
           READ KONSTANT WITH NO LOCK NOT INVALID GO X.
           IF ZUGRIF PERFORM BESETZT GO A.
       X.  MOVE KOP-TX TO WK-DSTG.
       Z.  EXIT.
      ******************************************************************
       DRUAUS SECTION.
       A.  IF WH-DRUNAM NOT = "LPT1:" GO C.
           MOVE SPACE TO WH-DRUNAM.
           GO Z.
      *-----------------------------------------------> ob Drucker ok <-
       C.  MOVE 0 TO WX-PRNO.
           CALL "PC_TEST_PRINTER" USING WX-PRNO WX-PRSTAT.
           EVALUATE WX-PRSTAT
               WHEN 48 WHEN 176 WHEN 8 WHEN 192 WHEN 24 WHEN 56 WHEN 136
                   CALL "CAUP" USING "0712120444000" WH-CREG
                   ADD 203 VDU-ECK GIVING VDU-LP
                   DISPLAY " lokaler Drucker:" with highlight AT VDU-LP
               WHEN OTHER MOVE "LPT1:" TO WH-DRUNAM GO Z.
           ADD 221 VDU-ECK GIVING VDU-LP.
           EVALUATE WX-PRSTAT
               WHEN  48 DISPLAY " nicht vorhanden " with reverse-video
                           AT VDU-LP
               WHEN 176 WHEN 8 WHEN 136
               WHEN 192 DISPLAY " nicht eingeschaltet " with
                           reverse-video AT VDU-LP
               WHEN  24 DISPLAY " nicht ON LINE " with reverse-video
                           AT VDU-LP
               WHEN  56 DISPLAY " kein Papier " with reverse-video
                           AT VDU-LP.
       E.  ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "<esc>= Abbruch, <ret>= Wiederholung < >" AT VDU-LP.
           CALL "CAUP" USING "1003400000" WH-CREG.
           CALL "CAUP" USING "08CLOFEN" WH-CREG.
           IF RET GO C.
           IF NOT ESC GO E.
       Z.  EXIT.
      ******************************************************************
       LI-WAHL SECTION.
       A.  CALL "CAUP" USING "0706101350000" WH-CREG.
           MOVE SPACE TO WH-DRUNAM.
           MOVE 0 TO WL.
       B.  PERFORM LADE-DRU.
           MOVE 9 TO WH-PG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Mahnung / Zinsen " with highlight AT VDU-LP.
           ADD 30 TO VDU-P.
           IF WH-DRUNAM = "LPT1:" DISPLAY " Direktdruck " AT VDU-LP
                             ELSE DISPLAY " Druckdatei  " AT VDU-LP.
           ADD 308 VDU-ECK GIVING VDU-LP.
           DISPLAY "1 - Mahnung"               AT VDU-LP.
           ADD 1 TO VDU-L.
           DISPLAY "2 - Zinsenberechnung"      AT VDU-LP.
           ADD 1 TO VDU-L.
           DISPLAY "3 - Zinsenverwaltung"      AT VDU-LP.
           ADD 1 TO VDU-L.
           DISPLAY "4 - Buchungsupdate"        AT VDU-LP.
           ADD 1 TO VDU-L.
           DISPLAY "5 - Reorganisation Zinsen" AT VDU-LP.
           ADD 1 TO VDU-L.
           DISPLAY "6 - Wechsel Druckausgabe"  AT VDU-LP.
           ADD 1 TO VDU-L.
           DISPLAY "<ret-leer>= Programmende"  AT VDU-LP.
           ADD 215 TO VDU-LP.
           DISPLAY "bitte wÑhlen Sie:" with highlight AT VDU-LP.
       C.  CALL "CAUP" USING "1011411001" WH-CREG.
           MOVE WH-NUM TO WH-PG.
           IF ESC OR WH-PG = 0 GO X.
           IF NOT RET GO C.
           EVALUATE WH-PG
               WHEN 1 PERFORM MAHNUNG
               WHEN 2 PERFORM ZINSRECH
               WHEN 3 PERFORM ZINSVERWALT
               WHEN 4 PERFORM BU-UPDATE
               WHEN 5 PERFORM ZIREORG
               WHEN 6 PERFORM DRUAUS
               WHEN 7 IF WL = 0 MOVE 1 TO WL
                           else MOVE 0 TO WL.
           GO B.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           UNLOCK KONSTANT.
           UNLOCK BUCHZEIL.
       Z.  EXIT.
      ******************************************************************
       LIESKTO SECTION.
       A.  IF WH-NUM = 0 GO X.
           MOVE WH-NUM TO WD-KZ.
           MOVE WD-KZ TO WH-KAT.
           DIVIDE 10 INTO WH-NUM GIVING WD-KTO.
           EVALUATE WD-KZ
               WHEN 3 MOVE WH-NUM TO KR-KTONR GO G
               WHEN 2 MOVE WH-NUM TO DE-KTONR GO E
               WHEN 1 SET ESC TO TRUE GO X
               WHEN 0 MOVE WH-NUM TO SA-KTONR
               WHEN OTHER GO X.
      *---------------------------------------------> lesen Sachkonto <-
       C.  READ SACHBUCH NOT INVALID MOVE SA-BEZ TO WT-TX
               MOVE SA-MCODE TO WK-MCODE
               MOVE SA-SOLL TO WE-SOLL
               MOVE SA-HABEN TO WE-HABEN
               ADD WE-SOLL WE-HABEN GIVING WE-SALDO GO Z.
           IF ZUGRIF PERFORM BESETZT GO X.
           GO X.
      *---------------------------------------------> lesen Debitoren <-
       E.  READ DEBITOR NOT INVALID MOVE DE-BEZ TO WT-TX
               INSPECT WT-TX REPLACING ALL "#" BY ","
               MOVE DE-MCODE TO WK-MCODE
               MOVE DE-SOLL TO WE-SOLL
               MOVE DE-HABEN TO WE-HABEN
               ADD WE-SOLL WE-HABEN GIVING WE-SALDO GO Z.
           IF ZUGRIF PERFORM BESETZT GO X.
           GO X.
      *--------------------------------------------> lesen Kreditoren <-
       G.  READ KREDIT NOT INVALID MOVE KR-BEZ TO WT-TX
               MOVE KR-MCODE TO WK-MCODE
               MOVE KR-SOLL TO WE-SOLL
               MOVE KR-HABEN TO WE-HABEN
               ADD WE-SOLL WE-HABEN GIVING WE-SALDO GO Z.
           IF ZUGRIF PERFORM BESETZT GO X.
       X.  SET FEHLER TO TRUE.
           MOVE "Konto fehlt" TO WT-TX.
       Z.  EXIT.
      ******************************************* von - bis Kunden-Nr. *
       VOBI SECTION.
       A.  ADD 303 VDU-ECK GIVING VDU-LP.
           MOVE 0 TO WH-VON WH-BIS.
           DISPLAY "von Kd.Nr.:          bis Kd.Nr.:        " AT VDU-LP.
           DISPLAY "<esc>= Abbruch, <ret>= von Kunden-Nr." AT 2301.
           CALL "CAUP" USING "1003156007" WH-CREG.
           IF ESC GO Z.
           IF not RET GO A.
           MOVE WH-WERT TO WH-VON WH-BIS.
           COMPUTE WD-KTO = WH-WERT / 10.
           DISPLAY WD-KTO with highlight AT VDU-LP.
       C.  DISPLAY "<esc>= Abbruch, <ret>= bis Kunden-Nr." AT 2301.
           CALL "CAUP" USING "1003366007" WH-CREG.
           IF ESC GO Z.
           IF WOLI or AUF GO A.
           IF not RET GO C.
           MOVE WH-WERT TO WH-BIS.
           COMPUTE WD-KTO = WH-WERT / 10.
           DISPLAY WD-KTO with highlight AT VDU-LP.
       Z.  EXIT.
      ********************************************** Listung Debitoren *
       MAHNUNG SECTION.
       A.  CALL "CAUP" USING "0708080771010" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           IF WH-DRUNAM NOT = "LPT1:" MOVE "MAHNUNG.LST" TO WH-DRUNAM.
           DISPLAY " Mahnungen " with highlight AT VDU-LP.
           ADD 136 VDU-ECK GIVING VDU-LP.
           MOVE "FIBMAHN.TXT" TO WN-MAHN.
           OPEN INPUT MAHNTXT.
           IF WF-STATUS = 00
               DISPLAY " FIBMAHN.TXT " with highlight AT VDU-LP
               CLOSE MAHNTXT.
           MOVE "FIBMAHN1.TXT" TO WN-MAHN.
           ADD 151 VDU-ECK GIVING VDU-LP.
           MOVE 0 TO WM-SMAHN WH-VON WM-END.
           MOVE 999999 TO WH-BIS.
           OPEN INPUT MAHNTXT.
           IF WF-STATUS = 00 MOVE 1 TO WM-SMAHN
               DISPLAY " FIBMAHN1.TXT " with highlight AT VDU-LP
               CLOSE MAHNTXT.
           ADD 255 VDU-ECK GIVING VDU-LP.
           IF WM-MA = 1 DISPLAY "mit Mahnstufen" with highlight
                            foreground-color 6 AT VDU-LP.
       B.  ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "Zahlungen erfa·t bis: " AT VDU-LP.
           MOVE WM-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           ADD 225 VDU-ECK GIVING VDU-LP.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           MOVE WM-DATUM TO WH-WERT WZ-DATUM.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "<esc>= Abbruch, <ret>= Zahlungsbuchungsdatum"
               AT 2301.
           CALL "CAUP" USING "1102256006" WH-CREG.
           IF ESC GO X.
           IF not RET GO B.
           IF WZ-DATUM  = 0 GO B.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           MOVE WX-DATUM TO WS-DATUM.
       C.  ADD 403 VDU-ECK GIVING VDU-LP.
           DISPLAY "<ret-leer>= alle, <Kto.Nr + ret>= einzelnes Konto <
      -        "         > " AT VDU-LP.
           ADD 1 TO VDU-L.
           DISPLAY "<esc>= Abbruch, <#>= alle mit Saldo, < >= mit Einzel
      -        "mahntext" AT VDU-LP.
           ADD 100 TO VDU-LP.
           DISPLAY "<>= von-bis" AT VDU-LP.
           MOVE 0 TO WE WH-WERT.
           DISPLAY "Ein eigener Mahntext kann in einer Textdatei 'FIBMAH
      -        "N.TXT' mittels eines" AT 2301
               "Texeditor (Edit) erstellt werden." AT 2401.
           DISPLAY "fÅr Einzelmahnung kann eine Datei 'FIBMAHN1.TXT' ers
      -        "tellt werden!" AT 2501.
           CALL "CAUP" USING "1004546110" WH-CREG.
           IF ESC GO X.
           IF WOLI PERFORM VOBI
                   IF ESC MOVE 0 TO WH-VON WH-BIS
                   else MOVE 1 TO WH-MOD
                        MOVE 9 TO WG
                        GO C.
           IF KIST MOVE 1 TO WE
               SET RET TO TRUE.
           IF AB MOVE 1 TO WH-MOD
                 SET RET TO TRUE
           else MOVE 0 TO WH-MOD.
           IF NOT RET GO C.
           IF WH-NUM = 0 AND WH-MCODE = SPACE
               DISPLAY " alle " with highlight AT VDU-LP GO D.
           IF WH-WERT < 0,4 OR WH-NUM < 4 MOVE 2 TO WH-NUM
               CALL "FIBANZ" USING "99SUCH" WH-CREG
               CANCEL "FIBANZ"
               IF WH-NUM = 0 GO C.
       D.  ADD 503 VDU-ECK GIVING VDU-LP.
           MOVE 0 TO WM-SOLL WM-HABEN WZ-ZEILEN WZ-SEITE WB.
           IF WH-NUM not = 0 and WH-BIS not = 999999;
                IF WH-BIS not = 0 DISPLAY "Nicht mîglich!"
                    with highlight foreground-color 4 AT 2401
                    PERFORM WEITER
                    GO C.
           IF WH-NUM not = 0 MOVE WH-NUM TO WV-KTONR
                PERFORM LIESKTO
                UNLOCK DEBITOR
                IF FEHLER GO C ELSE GO H.
           MOVE 0 TO DE-KTONR WV-KTONR.
           MOVE SPACE TO DE-MCODE.
           IF WH-VON not = 0 or WH-BIS not = 0 MOVE WH-VON TO DE-KTONR
               START DEBITOR KEY not < DE-KEY INVALID GO Z
               end-start
               GO G.
           START DEBITOR KEY NOT < DE-MCODE INVALID GO Z.
       G.  READ DEBITOR NEXT NO LOCK AT END MOVE 99 TO WB
               GO Q.
           IF ZUGRIF PERFORM BESETZT GO G.
           IF DE-KTONR > WH-BIS MOVE 99 TO WB GO Q.
       H.  DIVIDE 10 INTO DE-KTONR GIVING WD-KTO.
           MOVE DE-BEZ TO WT-TX.
           INSPECT WT-TX REPLACING ALL "#" BY ",".
           DISPLAY WD-KTO with highlight AT VDU-LP " " WT-TX(1:55).
           IF DE-SOLL = DE-HABEN OR DE-HABEN > DE-SOLL GO G.
      *---------------------------------------------> OP's abarbeiten <-
           MOVE 0 TO WS-SUM WS-OPS OP-REF OP-LFD OP-VALDAT WH-ZPZ
                  WH-SPES WZ-SEITE WZ-ZEILEN WZ-SCHALT WM-ZEIL WS-ZINS.
           MOVE DE-KTONR TO OP-KTONR.
           MOVE 2 TO OP-DK.
           MOVE OP-KEY TO WH-OPKEY.
           IF WE = 1 PERFORM OB-DRUCK
                     IF ESC GO G.
      *-----------------------------------------> Mahntext einstellen <-
           MOVE "FIBMAHN.TXT" TO WN-MAHN.
           IF WH-MOD = 1 and WM-SMAHN = 1
               MOVE "FIBMAHN1.TXT" TO WN-MAHN.
           MOVE WH-OPKEY TO OP-KEY.
           START OPDATEI KEY NOT < OP-KEY INVALID GO G.
       J.  READ OPDATEI NEXT NO LOCK AT END GO Q.
           IF ZUGRIF PERFORM BESETZT GO J.
           IF OP-KTONR NOT = DE-KTONR GO Q.
           SUBTRACT OP-ZABETRAG FROM OP-REBETRAG GIVING WH-WERT.
           IF WH-WERT = 0 GO J.
           PERFORM MAHNKOPF.
           PERFORM OB-FAELLIG.
           MOVE OP-VALDAT TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRO-DAT.
           MOVE WE-SYM(OP-SYM + 1) TO DRO-SYM.
           MOVE OP-REF TO DRO-REFNR.
           MOVE OP-REBETRAG TO DRO-REBET.
           SUBTRACT OP-ZABETRAG FROM OP-REBETRAG GIVING WH-WERT.
           MOVE WH-WERT TO DRO-ZABET.
           ADD WH-WERT TO WS-OPS.
           MOVE WV-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRO-FAELLIG.
           IF WV-DATUM > WM-DATUM GO K.
           IF WH-ZPZ not = 0 ADD WH-TAG TO WA-TAGE
               COMPUTE WB-TAGE = WA-HEUTE - WA-TAGE
               COMPUTE WH-ZINS ROUNDED =
                       (WH-WERT * WB-TAGE * WH-ZPZ) / 36000
               ADD WH-ZINS TO WS-ZINS.
           ADD WH-WERT TO WS-SUM.
           MOVE WH-WERT TO DRO-OFFEN.
           IF WM-MA = 1 and OP-MSTUFE not = 4 ADD 1 TO OP-MSTUFE
               MOVE OP-MSTUFE TO DRO-MKZ
               REWRITE OP-SATZ.
       K.  PERFORM DRUCK.
           ADD 1 TO WM-ZEIL.
           GO J.
      *------------------------------------------------> Mahnungsende <-
       Q.  PERFORM KTO-ENDE.
           IF WV-KTONR NOT = 0 GO C.
           IF WB NOT = 99 GO G.
       W.  PERFORM END-DRU.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ********************************************* ob fÑlligen Posten *
       OB-DRUCK SECTION.
       A.  MOVE WH-OPKEY TO OP-KEY.
           SET ESC TO TRUE.
           START OPDATEI KEY NOT < OP-KEY INVALID GO Z.
       J.  READ OPDATEI NEXT NO LOCK AT END GO Z.
           IF ZUGRIF PERFORM BESETZT GO J.
           IF OP-KTONR NOT = DE-KTONR GO Z.
           SUBTRACT OP-ZABETRAG FROM OP-REBETRAG GIVING WH-WERT.
           IF WH-WERT = 0 GO J.
           PERFORM OB-FAELLIG.
           IF WV-DATUM > WM-DATUM GO J.
           SET RET TO TRUE.
       Z.  EXIT.
      ******************************************* FÑlligkeit ermitteln *
       OB-FAELLIG SECTION.
       A.  IF OP-VALDAT(1:3)= "0  " DELETE OPDATEI
               MOVE OP-REDAT TO WX-DATUM
               CALL "CAUP" USING "05DATPRF" WH-CREG
               MOVE WX-DATUM TO OP-VALDAT                   *> fÅr 2000
               WRITE OP-SATZ.
           IF OP-VALDAT = 0 MOVE OP-REDAT TO OP-VALDAT.
           MOVE OP-VALDAT TO WZ-DATUM WH-VALUTA.
           MOVE OP-KOND TO WH-KK.
           IF OP-KOND = 0 MOVE DE-KOND TO WH-KK.
           IF WH-TGN = 0 MOVE 30 TO WH-TGN.
           MOVE WH-TGN TO WH-TAG.
           COMPUTE WA-TAGE = FUNCTION INTEGER-OF-DATE(OP-VALDAT).
           ADD WH-TAG TO WA-TAGE GIVING WB-TAGE.     *> + Tagedifferenz
           COMPUTE WB-TAGE = FUNCTION DATE-OF-INTEGER(WB-TAGE).
           MOVE WB-TAGE TO WC-DATUM WZ-DATUM WV-DATUM.
       Z.  EXIT.
      ***************************************************** Kontenkopf *
       MAHNKOPF SECTION.
       A.  IF WZ-SEITE = 0 GO C.
           IF WZ-ZEILEN > 61 ADD 1 TO WZ-SEITE
               MOVE WZ-SEITE TO WD-NUM
               MOVE "öbertrag auf Seite" TO DRA-STR(20:)
               MOVE WD-NUM(4:) TO DRA-STR(39:)
               IF WM-END not = 1 MOVE WS-OPS TO DRO-ZABET
                                 MOVE WS-SUM TO DRO-OFFEN
               end-if
               PERFORM DRUCK
               WRITE DRA-SATZ AFTER PAGE
               MOVE 0 TO WZ-ZEILEN
           else GO Z.
       C.  MOVE 2 TO WH-P1.
           MOVE 4 TO WH-P2.
           IF WM-DRU = 1 MOVE 15 TO WH-P1.
           PERFORM BEG-DRU.
           MOVE 10 TO WZ-SCHALT.
      *-----------------------------------------------> Kundenadresse <-
           MOVE DE-BEZ TO WT-BEZ.
           MOVE SPACE TO WT-ADR.
           MOVE "#" TO WR-BEZ(131).
           UNSTRING WT-BEZ DELIMITED BY "#" INTO WR-ADR(1) WR-ADR(2)
               WR-ADR(3) WR-ADR(4) WR-ADR(5) WR-ADR(6).
           MOVE WT-TXT(DE-ANREDE + 1) TO DRA-STR(8:).
           PERFORM DRUCK.
           PERFORM VARYING TX FROM 1 BY 1 UNTIL TX > 5
               MOVE WR-ADR(TX) TO DRA-STR(8:)
               PERFORM DRUCK.
           MOVE "Kontoauszug / Zahlungserinnerung" TO DRA-STR(50:).
           PERFORM DRUCK.
           MOVE 2 TO WZ-SCHALT.
           MOVE "Kto.:            Datum:" TO DRA-STR(50:).
           MOVE WD-KTO TO DRA-STR(56:8).
           MOVE WM-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRA-STR(74:).
           PERFORM DRUCK.
           MOVE 3 TO WZ-SCHALT.
           IF WM-END = 1 GO Z.
           PERFORM MA-TEXT.
           IF WZ-SEITE > 1 ADD -1 WZ-SEITE GIVING WD-NUM
               MOVE "öbertrag von Seite" TO DRA-STR(20:)
               MOVE WD-NUM(4:) TO DRA-STR(39:)
               MOVE WS-OPS TO DRO-ZABET
               MOVE WS-SUM TO DRO-OFFEN
               PERFORM DRUCK
           else ADD 1 TO WZ-SEITE.
       Z.  EXIT.
      ************************************************** Mahnungstexte *
       MA-TEXT SECTION.
       A.  IF WZ-SEITE > 0 GO I.
           MOVE SPACE TO MX-SATZ.
           OPEN INPUT MAHNTXT.
           IF WF-STATUS NOT = 00 GO G.
       C.  READ MAHNTXT NEXT AT END CLOSE MAHNTXT
               MOVE SPACE TO MX-SATZ GO I.
           IF ZUGRIF PERFORM BESETZT GO C.
           IF MX-SATZ(1:1) = "#";
               IF MX-SATZ(2:1) = "Z" MOVE MX-SATZ(3:5) TO WD-ZPZ(1:)
                   MOVE WD-ZPZ TO WH-ZPZ WV-ZPZ
                   COMPUTE WA-HEUTE = FUNCTION INTEGER-OF-DATE(WM-DATUM)
                   ADD 5 TO WA-HEUTE
                   MOVE MX-SATZ(9:5) TO WD-SPES(1:)
                   MOVE WD-SPES TO WH-SPES
               end-if
               GO I.
           MOVE MX-SATZ TO DRA-STR(3:) PERFORM DRUCK.
           GO C.
       G.  MOVE "Sehr geehrter GeschÑftsfreund!" TO DRA-STR(3:).
           PERFORM DRUCK.
           MOVE "Wir erlauben uns, Ihnen eine Aufstellung Ihrer offenen
      -        "Posten zu Åberreichen." TO DRA-STR(3:) PERFORM DRUCK.
           MOVE "Bitte prÅfen Sie diese und veranlassen Sie die öberweis
      -        "ung der fÑlligen" TO DRA-STR(3:) PERFORM DRUCK.
           MOVE "BetrÑge innerhalb von 8 Tagen." TO DRA-STR(3:)
           PERFORM DRUCK.
       I.  MOVE 1 TO WZ-SCHALT.
           IF WM-DRU = 1 MOVE "(s0p17.0h0b0s6T" TO DRA-SATZ
               PERFORM DRUCK
                  MOVE "Sym.   Beleg-Nr.         Datum        FÑllig
      -               "Rechnungsbetrag    Offene Posten   Offene fÑllige
      -               " Posten    Ms." TO DRA-STR(6:)          *> Laser
           else MOVE x"1B2104" TO DRA-SATZ
               MOVE "Sym.   Beleg-Nr.          Datum        FÑllig    Re
      -        "chnungsbetrag     Offene Posten   Offene fÑllige Posten"
                   TO DRA-STR(9:).                            *> Nadler
           IF WM-MA = 0 INSPECT DRA-SATZ REPLACING ALL "Ms." BY SPACE.
           PERFORM DRUCK.
           MOVE 0 TO WZ-SCHALT.
           IF WM-DRU = 1 MOVE "(s0p12h0b0s3T" TO DRA-SATZ
                    else MOVE x"1B2101" TO DRA-SATZ.
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRA-STR(3:80) PERFORM DRUCK.
       Z.  EXIT.
      ****************************************************** Kontoende *
       KTO-ENDE SECTION.
       A.  IF WM-ZEIL = 0 GO Z.
           MOVE 1 TO WM-END.
           IF WZ-ZEILEN > 57 MOVE 70 TO WZ-ZEILEN
                             PERFORM MAHNKOPF.
           MOVE ALL "ƒ" TO DRA-STR(3:80) PERFORM DRUCK.
           MOVE 0 TO WZ-SCHALT.
           IF WM-DRU = 1 MOVE "(s0p17.0h0b0s6T" TO DRA-SATZ
               PERFORM DRUCK
               MOVE "Ihr Kontostand         FÑlliger Betrag"
                   TO DRA-STR(72:)
           else MOVE x"1B2104" TO DRA-SATZ
               PERFORM DRUCK
               MOVE "Ihr Kontostand         FÑlliger Betrag"
                   TO DRA-STR(74:).
           PERFORM DRUCK.
           MOVE 0 TO WZ-SCHALT.
           IF WM-DRU = 1 MOVE "(s0p12h0b0s3T" TO DRA-SATZ
                    else MOVE x"1B2101" TO DRA-SATZ.
           PERFORM DRUCK.
           MOVE "Zahlungen bis zum          erfa·t!         Eur"
               TO DRO-SATZ(7:).
           MOVE WS-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRO-SATZ(25:8).
           MOVE WS-OPS TO DRO-ZABET.
           IF WS-SUM < 0 MOVE 0 TO DRO-OFFEN
                    else MOVE WS-SUM TO DRO-OFFEN.
           PERFORM DRUCK.
           IF WH-ZPZ not = 0 and WS-ZINS > 0
              MOVE "zuzÅgl. Verzugszinsen" TO DRO-SATZ(7:)
              MOVE "Eur" TO DRO-SATZ(50:3)
              MOVE WS-ZINS TO DRO-OFFEN
              PERFORM DRUCK
              ADD WS-ZINS TO WS-SUM
              IF WH-SPES not = 0
                  MOVE "zuzÅgl. Mahnspesen" TO DRO-SATZ(7:)
                  MOVE "Eur" TO DRO-SATZ(50:3)
                  MOVE WH-SPES TO DRO-OFFEN
                  ADD WH-SPES TO WS-SUM
                  PERFORM DRUCK
              end-if
              MOVE "Eur" TO DRO-SATZ(50:3)
              MOVE WS-SUM TO DRO-OFFEN
              PERFORM DRUCK.
           MOVE ALL "=" TO DRO-SATZ(50:33) PERFORM DRUCK.
           IF WZ-ZEILEN > 57 MOVE 70 TO WZ-ZEILEN
                             PERFORM MAHNKOPF.
           MOVE 2 TO WZ-SCHALT.
           IF MX-SATZ(1:1) NOT = "#" GO G.
       C.  READ MAHNTXT NEXT AT END CLOSE MAHNTXT GO W.
           IF ZUGRIF PERFORM BESETZT GO C.
           MOVE MX-SATZ TO DRA-STR(3:) PERFORM DRUCK.
           GO C.
       G.  MOVE "Sollte sich Ihre Zahlung mit dieser automatisch erstell
      -        "ten Mahnung gekreuzt" TO DRA-STR(3:80).
           PERFORM DRUCK.
           MOVE "haben, bitten wir Sie, diese als gegenstandslos zu betr
      -        "achten. Bei öber-" TO DRA-STR(3:80).
           PERFORM DRUCK.
           MOVE "schreitung des Zahlungszieles verrechnen wir Verzugszin
      -        "sen. Wir danken im" TO DRA-STR(3:80).
           PERFORM DRUCK.
           MOVE "voraus fÅr Ihre öberweisung." TO DRA-STR(3:80).
           PERFORM DRUCK.
       W.  MOVE 0 TO WS-OPS WS-SUM WZ-SEITE WM-ZEIL WM-END.
           WRITE DRA-SATZ AFTER PAGE.
       Z.  EXIT.
      **************************************************** Zinsenzeile *
       ZINSZEIL SECTION.
       A.  ADD ZI-BET TO WS-SALDO.
           MOVE ZI-BELDAT TO WC-DATUM
           PERFORM DATDREH
           MOVE VDU-DATUM TO DRZ-BELDAT
           MOVE ZI-VALDAT TO WC-DATUM
           PERFORM DATDREH
           MOVE VDU-DATUM TO DRZ-VALDAT
           MOVE WE-SYM(ZI-SYM + 1) TO DRZ-SYM
           MOVE ZI-BELNR TO DRZ-BELNR
           MOVE WS-SALDO TO DRZ-SALDO
           IF ZI-BET > 0 MOVE ZI-BET TO DRZ-SOLL
                    else MOVE ZI-BET TO DRZ-HABEN.
       Z.  EXIT.
      *********************************************** Zinsenberechnung *
       ZINSRECH SECTION.
       A.  CALL "CAUP" USING "0706071060000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           MOVE 0 TO WG.
           DISPLAY " Zinsenberechnung " with highlight AT VDU-LP.
           MOVE 1 TO WH-KEY.
       B.  READ KONSTANT NO LOCK INVALID STOP RUN.
           IF ZUGRIF PERFORM BESETZT GO B.
           MOVE KO-PER TO WS-DATUM.
           MOVE KO-ZTAG TO WH-ZTAG.
           MOVE KO-ZPZ TO WH-ZPZ.
       C.  CALL "CAUP" USING "16CLRFEN" WH-CREG.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "Zinstermin: " AT VDU-LP.
           MOVE WH-DATUM TO WZ-DATUM.
           ADD -1 TO WZ-MONAT.
           IF WZ-MONAT = 0 MOVE 12 TO WZ-MONAT
               ADD -1 TO WZ-JAHR.
           MOVE 32 TO WZ-TAG.
       E.  MOVE WZ-DATUM TO WV-DATUM.
           CALL "CAUP" USING "05DATPRF" WH-CREG.
           IF WZ-DATUM = 0 MOVE WV-DATUM TO WZ-DATUM
               ADD -1 TO WZ-TAG GO E.
           ADD 315 VDU-ECK GIVING VDU-LP.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           DISPLAY "<esc>= Abbruch, <ret>= Datum" AT 2301.
           CALL "CAUP" USING "1103156006" WH-CREG.
           IF ESC GO X.
           IF NOT RET GO C.
           IF WZ-DATUM = 0 GO C.
           MOVE WX-DATUM TO WV-DATUM WS-DATUM WE-PER.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
       I.  DISPLAY "<esc>= Abbruch, <ret>= vorlÑufig, <tab>= Start < >"
               AT 2301.
           CALL "CAUP" USING "0023490000" WH-CREG.
           IF ESC GO X.
           IF RET MOVE 1 TO WG.
           IF TABL SET RET TO TRUE.
           IF NOT RET GO I.
           MOVE 1 TO WH-KEY.
       J.  READ KONSTANT INVALID STOP RUN.
           IF ZUGRIF PERFORM BESETZT GO J.
           MOVE WV-DATUM TO KO-PER.
           MOVE 0 TO KO-ZTAG KO-ZPZ.
           IF WG = 0 REWRITE KO-SATZ.
           OPEN I-O ZINSEN.
           IF WF-STATUS = "05" PERFORM ERSTVALUTA GO W.
           MOVE 0 TO DE-KTONR.
      *--------------------------------------------> Zinsenermittlung <-
       K.  PERFORM NEXTDEB.
           IF ESC GO W.
           MOVE 0 TO WM-Z.
           MOVE 1 TO ZI-SA.
           MOVE DE-KTONR TO ZI-NUM.
           IF WH-ZPZ = 0 MOVE WV-ZPZ TO WH-ZPZ.
           START ZINSEN KEY > ZI-KEY INVALID GO W.
       M.  READ ZINSEN NEXT AT END GO Q.
           IF ZUGRIF PERFORM BESETZT GO M.
           IF ZI-VALDAT = 0 AND ZI-SA = 0 GO M.
           IF ZI-NUM NOT = WV-KTONR OR ZI-SA = 50 GO Q.
           IF ZI-VALDAT > WV-DATUM;
               IF WM-Z = 1 MOVE WV-DATUM TO WB-TAGE
                   PERFORM ZI-RE
                   MOVE 0 TO WM-Z
               end-if
                   ADD ZI-BET TO WS-VORAUS
                   PERFORM ZINSKOPF
                   PERFORM ZINSZEIL
                   PERFORM DRUCK GO M.
           PERFORM ZINSKOPF.
           IF ZI-VALDAT = WE-PER PERFORM DRUCK
               PERFORM ZINSZEIL
               GO M.
           MOVE ZI-VALDAT TO WB-TAGE.
           PERFORM ZI-RE.
           PERFORM ZINSZEIL.
           MOVE ZI-VALDAT TO WE-PER.
           MOVE 1 TO WM-Z.
           GO M.
      *--------------------------------------------------> Kundenende <-
       Q.  IF DRA-SATZ NOT = SPACE PERFORM DRUCK.
           IF WZ-ZEILEN = 0 GO K.
           IF WZ-ZEILEN > 59 MOVE 72 TO WZ-ZEILEN
               PERFORM ZINSKOPF.
           MOVE ALL "ƒ" TO DRZ-STR PERFORM DRUCK.
           MOVE "Kontostand per: " TO DRZ-STR(25:).
           MOVE WV-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRZ-STR(41:8).
           SUBTRACT WS-VORAUS FROM WS-SALDO GIVING DRZ-SALDO ZI-BET.
           MOVE WS-ZINS TO DRZ-ZINSEN.
           PERFORM DRUCK.
           IF WS-VORAUS NOT = 0 MOVE WS-VORAUS TO DRZ-SALDO
               MOVE "Vorausvalutierung" TO DRZ-STR(25:17)
               PERFORM DRUCK
               MOVE "Gesamtkontostand" TO DRZ-STR(25:17)
               MOVE WS-SALDO TO DRZ-SALDO
               PERFORM DRUCK.
           IF WG = 1 GO T.
           MOVE DE-KTONR TO ZI-NUM.
           MOVE 50 TO ZI-SA.
           MOVE WV-DATUM TO ZI-VALDAT.
           MOVE 99 TO ZI-USE.
           MOVE 0 TO ZI-SEITE.
       S.  ADD 1 TO ZI-SEITE.
           WRITE ZI-SATZ INVALID GO S.
       T.  WRITE DRA-SATZ AFTER PAGE.
           GO K.
      *-------------------------------> valutarischen Saldo abstellen <-
       W.  CLOSE ZINSEN.
           PERFORM END-DRU.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      *************************************** Zinsen fÅr Tagedifferenz *
       ZI-RE SECTION.
       A.  IF WE-PER > 500000 ADD 19000000 WE-PER GIVING WH-NUM
                         else ADD 20000000 WE-PER GIVING WH-NUM.
           COMPUTE WA-TAGE = FUNCTION INTEGER-OF-DATE(WH-NUM).
           IF WB-TAGE > 500000 ADD 19000000 WB-TAGE GIVING WH-NUM
                          else ADD 20000000 WB-TAGE GIVING WH-NUM.
           COMPUTE WB-TAGE = FUNCTION INTEGER-OF-DATE(WH-NUM).
           SUBTRACT WA-TAGE FROM WB-TAGE.
           COMPUTE WH-ZINS ROUNDED =
                    (WS-SALDO * WB-TAGE * WH-ZPZ) / 36000.
           IF WS-SALDO < 0 MOVE 0 TO WH-ZINS.
           ADD WH-ZINS TO WS-ZINS.
           IF WH-DRUSATZ NOT = SPACE MOVE WH-DRUSATZ TO DRZ-SATZ
                MOVE SPACE TO WH-DRUSATZ.
           MOVE WS-SALDO TO DRZ-SALDO.
           IF WS-SALDO > 0 MOVE WB-TAGE TO DRZ-TAGE
               MOVE WH-ZINS TO DRZ-ZINSEN.
           PERFORM DRUCK.
       Z.  EXIT.
      ************************** nÑchsten Deb. u. letzten valut. Saldo *
       NEXTDEB SECTION.
       A.  MOVE SPACE TO WH-DRUSATZ.
           MOVE 0 TO WH-ZINS WZ-ZEILEN WZ-SEITE WS-SUM WS-SALDO
               WS-ZINS WS-VORAUS WH-ZPZ.
           ADD 10 DE-KTONR GIVING ZI-NUM.
           MOVE 0 TO ZI-SA ZI-VALDAT.
           START ZINSEN KEY NOT < ZI-KEY INVALID GO Q.
       C.  READ ZINSEN NEXT NO LOCK AT END GO Q.
           IF ZUGRIF PERFORM BESETZT GO C.
           MOVE ZI-NUM TO DE-KTONR WV-KTONR.
           IF ZI-VALDAT = 0 AND ZI-SA = 0 MOVE ZI-BET TO WH-ZPZ WD-ZPZ.
       E.  READ DEBITOR NO LOCK INVALID GO A.
           IF ZUGRIF PERFORM BESETZT GO E.
           MOVE 50 TO ZI-SA
           MOVE 99999999 TO ZI-VALDAT.
           MOVE WV-DATUM TO WS-DATUM.
           START ZINSEN KEY < ZI-KEY INVALID GO Q.
       G.  READ ZINSEN PREVIOUS NO LOCK AT END GO Q.
           IF ZUGRIF PERFORM BESETZT GO G.
           IF ZI-NUM NOT = DE-KTONR OR ZI-SA NOT = 50
               MOVE 0 TO WS-DATUM
           else MOVE ZI-BET TO WS-SALDO
                MOVE ZI-VALDAT TO WE-PER WC-DATUM
                MOVE SPACE TO DRA-SATZ
                PERFORM DATDREH
                MOVE VDU-DATUM TO DRZ-VALDAT
                MOVE WS-SALDO TO DRZ-SALDO
                MOVE DRZ-SATZ TO WH-DRUSATZ.
           ADD 703 VDU-ECK GIVING VDU-LP.
           DIVIDE 10 INTO DE-KTONR GIVING WD-KTO.
           MOVE DE-BEZ TO WT-TX.
           INSPECT WT-TX REPLACING ALL "#" BY ",".
           DISPLAY WD-KTO AT VDU-LP " " WT-TX(1:47).
           ADD 540 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-ZPZ with highlight AT VDU-LP " %".
           GO Z.
       Q.  SET ESC TO TRUE.
       Z.  EXIT.
      **************************************** Kopf fÅr Zinsennachweis *
       ZINSKOPF SECTION.
       A.  IF WZ-SEITE = 0 GO C.
           IF WZ-ZEILEN > 60 MOVE DRZ-SATZ TO WH-DRUSATZ
               MOVE SPACE TO DRA-SATZ
               MOVE ALL "ƒ" TO DRZ-STR
               PERFORM DRUCK
               MOVE "öbertrag auf Seite:" TO DRZ-STR(20:)
               ADD 1 TO WZ-SEITE
               MOVE WZ-SEITE TO WD-NUM
               MOVE WD-NUM(4:) TO DRZ-STR(39:)
               MOVE WS-ZINS TO DRZ-ZINSEN
               PERFORM DRUCK
               WRITE DRA-SATZ AFTER PAGE
               MOVE 0 TO WZ-ZEILEN
           else GO Z.
       C.  MOVE "ZINSBEL.LST" TO WH-DRUNAM.
           MOVE 2 TO WH-P1.
           MOVE 4 TO WH-P2.
           IF WM-DRU = 1 MOVE 15 TO WH-P1.
           PERFORM BEG-DRU.
           MOVE 10 TO WZ-SCHALT.
      *-----------------------------------------------> Kundenadresse <-
           MOVE DE-BEZ TO WT-BEZ.
           MOVE SPACE TO WT-ADR.
           MOVE "#" TO WR-BEZ(131).
           UNSTRING WT-BEZ DELIMITED BY "#" INTO WR-ADR(1) WR-ADR(2)
               WR-ADR(3) WR-ADR(4) WR-ADR(5) WR-ADR(6).
           MOVE WT-TXT(DE-ANREDE + 1) TO DRZ-STR(8:).
           PERFORM DRUCK.
           PERFORM VARYING TX FROM 1 BY 1 UNTIL TX > 5
               MOVE WR-ADR(TX) TO DRZ-STR(8:)
               PERFORM DRUCK.
           MOVE "Zinsenabschlu· per:" TO DRZ-STR(50:).
           MOVE WS-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRZ-STR(70:8).
           PERFORM DRUCK.
           MOVE 2 TO WZ-SCHALT.
           MOVE "Kto.:            Datum:" TO DRZ-STR(50:).
           MOVE WD-KTO TO DRZ-STR(56:8).
           MOVE WM-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRZ-STR(74:).
           PERFORM DRUCK.
           MOVE "Zinssatz:" TO DRZ-STR(50:).
           MOVE WH-ZPZ TO WD-ZPZ.
           MOVE WD-ZPZ TO DRZ-STR(61:5).
           MOVE "%" TO DRZ-STR(66:1).
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRZ-STR PERFORM DRUCK.
           MOVE " Bel.dat Bel-Num. Sym  Valuta          Soll    Haben
      -       "     Saldo Tage    Zinsen" TO DRZ-STR PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRZ-STR PERFORM DRUCK.
           IF WZ-SEITE > 1 ADD -1 WZ-SEITE GIVING WD-NUM
               MOVE "öbertrag von Seite:" TO DRZ-STR(20:)
               MOVE WD-NUM(4:) TO DRZ-STR(39:)
               MOVE WS-ZINS TO DRZ-ZINSEN
               PERFORM DRUCK
               MOVE WH-DRUSATZ TO DRZ-SATZ
               MOVE SPACE TO WH-DRUSATZ
           else ADD 1 TO WZ-SEITE.
       Z.  EXIT.
      ************************************************ Erstvalutierung *
       ERSTVALUTA SECTION.
       A.  CALL "CAUP" USING "0708070660000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Zinseninitialisierung " with highlight AT VDU-LP.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "<ret>= Start, <esc>= Abbruch < >" AT VDU-LP.
       C.  CALL "CAUP" USING "1003330000" WH-CREG.
           IF ESC GO X.
           IF NOT RET GO C.
           MOVE 0 TO DE-KTONR.
           START DEBITOR KEY > DE-KEY INVALID GO X.
       E.  READ DEBITOR NEXT NO LOCK AT END GO X.
           IF ZUGRIF PERFORM BESETZT GO E.
           DIVIDE 10 INTO DE-KTONR GIVING WD-KTO.
           MOVE DE-BEZ TO WT-TX.
           INSPECT WT-TX REPLACING ALL "#" BY SPACE.
           ADD 403 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-KTO AT VDU-LP " " WT-TX(1:47).
           IF DE-OPKZ < 10 GO E.
           EVALUATE DE-OPKZ
               WHEN 10
               WHEN 11 PERFORM VALPOS.
           GO E.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ******************************************************************
       BU-UPDATE SECTION.
       A.  MOVE 1 TO WH-KEY.
           READ KONSTANT.
           IF ZUGRIF PERFORM BESETZT GO A.
           MOVE KO-PER TO WE-PER WC-DATUM.
           PERFORM DATDREH.
           MOVE KO-ZPZ TO WH-ZPZ.
           OPEN I-O ZINSEN.
           CALL "CAUP" USING "0708070660000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Buchungszeilenupdate " AT VDU-LP.
           ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "Buchungszeilen ab: " AT VDU-LP
               VDU-DATUM with highlight.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "<ret>= Start, <esc>= Abbruch < >" AT VDU-LP.
       C.  CALL "CAUP" USING "1003330000" WH-CREG.
           IF ESC GO X.
           IF NOT RET GO C.
           MOVE 0 TO DE-KTONR.
           START DEBITOR KEY > DE-KEY INVALID GO X.
       E.  READ DEBITOR NEXT NO LOCK AT END GO X.
           IF ZUGRIF PERFORM BESETZT GO E.
           DIVIDE 10 INTO DE-KTONR GIVING WD-KTO.
           MOVE DE-BEZ TO WT-TX.
           INSPECT WT-TX REPLACING ALL "#" BY SPACE.
           ADD 403 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-KTO AT VDU-LP " " WT-TX(1:47).
           IF DE-OPKZ < 10 GO E.
      *-----------------------------------------> update neuer Zeilen <-
           MOVE LOW-VALUE TO ZI-SATZ.
           MOVE WH-ZPZ TO ZI-BET.
           MOVE 99 TO ZI-USE.
           MOVE DE-KTONR TO ZI-NUM ZI-SEITE.
           WRITE ZI-SATZ INVALID NEXT SENTENCE.
           MOVE DE-KTONR TO BZ-KTONR.
           MOVE 0 TO BZ-LFD.
           MOVE WE-PER TO BZ-BUDAT.
           START BUCHZEIL KEY > BZ-KEY INVALID GO X.
       G.  READ BUCHZEIL NEXT NO LOCK AT END GO X.
           IF ZUGRIF PERFORM BESETZT GO G.
           IF BZ-KTONR NOT = DE-KTONR GO E.
           IF BZ-SYM > 97 GO G.
           MOVE WE-SKZ(BZ-SYM + 1) TO WE-SYMT.
941218     IF BZ-SH = 1 GO K.
           MOVE BZ-BUBET TO ZI-BET.
           IF WE-DIV = 1 OR WE-DIV = 2 OR WE-DIV = 4
               MOVE BZ-KOND TO WH-KK
               IF WH-TG1 > WH-TGN
                   MOVE WH-TG1 TO WH-TGN
               end-if
               ADD WH-ZTAG TO WH-TGN
               MOVE WH-KK TO OP-KOND
               MOVE BZ-BELDAT TO OP-VALDAT
               PERFORM OB-FAELLIG
           else MOVE BZ-BELDAT TO WV-DATUM.
       K.  IF BZ-VM = 4 ADD BZ-MW BZ-B1 BZ-B2 TO BZ-BUBET.
           IF BZ-SH = 1 MULTIPLY BZ-BUBET BY -1 GIVING ZI-BET
                        MOVE BZ-BELDAT TO WV-DATUM.
           MOVE WV-DATUM TO ZI-VALDAT.
       M.  MOVE DE-KTONR TO ZI-NUM.
           MOVE BZ-SYM TO ZI-SYM.
           MOVE BZ-BELDAT TO ZI-BELDAT.
           MOVE BZ-BELNR TO ZI-BELNR.
           MOVE 1 TO ZI-SA.
           MOVE BZ-JKEY TO ZI-BZEIL.
           WRITE ZI-SATZ INVALID rewrite zi-satz.
           GO G.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           CLOSE ZINSEN.
       Z.  EXIT.
      ******************************************************************
       VALPOS SECTION.
       A.  MOVE 0 TO WS-OPS.
           MOVE LOW-VALUE TO ZI-SATZ.
           MOVE WH-ZPZ TO ZI-BET.
           MOVE WH-ZTAG TO ZI-BELDAT.
           MOVE 99 TO ZI-USE.
           MOVE DE-KTONR TO ZI-NUM ZI-SEITE.
           WRITE ZI-SATZ INVALID REWRITE ZI-SATZ.
           PERFORM OPZINS.
           MOVE DE-KTONR TO BZ-KTONR.
           MOVE 0 TO BZ-LFD BZ-BUDAT.
           START BUCHZEIL KEY > BZ-KEY INVALID GO Z.
       G.  READ BUCHZEIL NEXT NO LOCK AT END GO Z.
           IF ZUGRIF PERFORM BESETZT GO G.
           IF BZ-KTONR NOT = DE-KTONR GO Q.
           IF BZ-SYM > 97 GO G.
           MOVE WE-SKZ(BZ-SYM + 1) TO WE-SYMT.
           IF BZ-SH = 1 GO K.
           MOVE BZ-BUBET TO ZI-BET.
           IF WE-DIV = 1 OR WE-DIV = 2 OR WE-DIV = 4
               MOVE BZ-KOND TO WH-KK
               IF WH-TG1 > WH-TGN
                   MOVE WH-TG1 TO WH-TGN
               end-if
               ADD WH-ZTAG TO WH-TGN
               MOVE WH-KK TO OP-KOND
               MOVE BZ-BELDAT TO OP-VALDAT
               PERFORM OB-FAELLIG
           else MOVE BZ-BELDAT TO WV-DATUM.
      *-----------------------------------> nicht fÑllig in den Saldo <-
           IF WV-DATUM NOT > WS-DATUM
                ADD BZ-BUBET TO WS-OPS GO G.
           MOVE WV-DATUM TO ZI-VALDAT.
           GO M.
       K.  IF BZ-VM = 4 ADD BZ-MW BZ-B1 BZ-B2 TO BZ-BUBET.
           MULTIPLY BZ-BUBET BY -1 GIVING ZI-BET.
           MOVE BZ-BELDAT TO ZI-VALDAT.
       M.  MOVE DE-KTONR TO ZI-NUM.
           MOVE BZ-SYM TO ZI-SYM.
           MOVE BZ-BELDAT TO ZI-BELDAT.
           MOVE BZ-BELNR TO ZI-BELNR.
           MOVE 1 TO ZI-SA.
           MOVE BZ-JKEY TO ZI-BZEIL.
           WRITE ZI-SATZ.
           GO G.
      *-------------------------------> valorisierten Saldo abstellen <-
       Q.  MOVE WS-OPS TO ZI-BET.
           MOVE DE-KTONR TO ZI-NUM.
           MOVE 50 TO ZI-SA.                *> 50 = Saldo / 1= Bewegung
           MOVE WS-DATUM TO ZI-VALDAT.
           MOVE 99 TO ZI-USE.
           MOVE 0 TO ZI-SEITE.
       S.  ADD 1 TO ZI-SEITE.
           WRITE ZI-SATZ INVALID GO S.
       Z.  EXIT.
      ********************************************* Op-öbernahmezinsen *
       OPZINS SECTION.
       A.  MOVE 9999000 TO ZI-SEITE.
           MOVE 2 TO OP-DK.
           MOVE DE-KTONR TO OP-KTONR.
           MOVE 0 TO OP-VALDAT OP-LFD.
           START OPDATEI KEY NOT < OP-KEY.
       C.  READ OPDATEI NEXT NO LOCK AT END GO Z.
           IF ZUGRIF PERFORM BESETZT GO Z.
           IF DE-KTONR NOT = OP-KTONR GO Z.
           IF OP-REDAT > WS-DATUM GO C.
           MOVE OP-REBETRAG TO ZI-BET.
           MOVE OP-KOND TO WH-KK.
           ADD WH-ZTAG TO WH-TGN.
           MOVE WH-KK TO OP-KOND.
           PERFORM OB-FAELLIG.
      *-----------------------------------> nicht fÑllig in den Saldo <-
           IF WV-DATUM NOT > WS-DATUM
                ADD OP-REBETRAG TO WS-OPS GO C.
           MOVE WV-DATUM TO ZI-VALDAT.
           MOVE OP-REBETRAG TO ZI-BET.
      *    IF OP-REBETRAG < 0 MULTIPLY OP-REBETRAG BY -1 GIVING ZI-BET.
           MOVE DE-KTONR TO ZI-NUM.
           MOVE 1 TO ZI-SA.
           MOVE OP-SYM TO ZI-SYM.
           MOVE OP-REDAT TO ZI-BELDAT.
           MOVE OP-RENUM TO ZI-BELNR.
       D.  ADD 1 TO ZI-SEITE.
           WRITE ZI-SATZ INVALID GO D.
           GO C.
       Z.  EXIT.
      *********************************************** Zinsenmitteilung *
       ZINSVERWALT SECTION.
       A.  CALL "CAUP" USING "0706071460031" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           MOVE 10 TO WH-ZPZ MOVE 3 TO WH-ZTAG.
           DISPLAY " Zinsenverwaltung " with highlight AT VDU-LP.
           ADD 402 VDU-ECK GIVING VDU-LP.
           DISPLAY " Zl Buch.Dat Bel.Num Sym   Valuta           Soll
      -        "Haben " with reverse-video AT VDU-LP.
           OPEN I-O ZINSEN.
       C.  CALL "CAUP" USING "16CLRFEN" WH-CREG.
           ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "Kto-Nr.:             Zinssatz:       %    +Tage:
      -        "  " AT VDU-LP.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY ALL " " AT VDU-LP WITH SIZE 55.
           DISPLAY "<esc>= Abbruch, <ret>= Kto.Nr., alpha+<ret>= suchen"
               AT 2301.
           DISPLAY "<Einf>= Zinsen neu ermitteln" AT 2401.
           CALL "CAUP" USING "1002126110" WH-CREG.
           IF ESC GO X.
           IF EINF SET RET TO TRUE
                   MOVE 1 TO WH-MOD
              else MOVE 0 TO WH-MOD.
           IF NOT RET GO C.
           IF WH-WERT < 0,4 OR WH-NUM < 4 MOVE 2 TO WH-NUM
               CALL "FIBANZ" USING "99SUCH" WH-CREG
               CANCEL "FIBANZ"
               IF WH-NUM = 0 GO C.
       D.  IF WH-NUM NOT = 0 MOVE WH-NUM TO WV-KTONR
                PERFORM LIESKTO
                UNLOCK DEBITOR
                IF FEHLER GO C.
           DIVIDE 10 INTO DE-KTONR GIVING WD-KTO.
           DISPLAY WD-KTO with highlight AT VDU-LP.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY WT-TX(1:55) AT VDU-LP.
           IF WH-MOD = 1 PERFORM NEUZINS
               MOVE 0 TO WH-MOD
               IF ESC GO C.
           MOVE LOW-VALUE TO ZI-SATZ.
           MOVE DE-KTONR TO ZI-NUM.
       E.  READ ZINSEN KEY IS ZI-KEY INVALID
               DISPLAY "Keine Zinsdatei vorhanden" AT 2301
               PERFORM WEITER
               GO C.
           IF ZUGRIF PERFORM BESETZT GO E.
           MOVE ZI-BET TO WD-ZPZ.
           ADD 234 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-ZPZ with highlight AT VDU-LP.
           ADD 252 VDU-ECK GIVING VDU-LP.
           MOVE ZI-BELDAT TO WD-TGN.
           DISPLAY WD-TGN with highlight AT VDU-LP.
           MOVE 0 TO WG.
       G.  UNLOCK ZINSEN.
           MOVE WH-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           MOVE WH-DATUM TO WH-WERT WZ-DATUM.
           DISPLAY "<esc>= Abbruch, < >= Zinssatz, <ret>= anzeigen ab Va
      -        "lutadat." AT 2301.
           CALL "CAUP" USING "0123626006" WH-CREG.
           IF ENTF PERFORM ZILOESCH GO C.
           IF AB GO Q.                              *> ZinssatzÑnderung
           IF ESC GO C.
           IF NOT RET GO G.
      *---------------------------------> anzeigen mit/ohne Val.Datum <-
       H.  PERFORM VAL-ANZ.
           MOVE WH-NUM TO IX.
           IF ESC GO G.
       I.  READ ZINSEN INVALID PERFORM NO-REC GO G.
           IF ZUGRIF PERFORM BESETZT GO I.
           MOVE ZI-VALDAT TO WC-DATUM.
           PERFORM DATDREH.
           MOVE ZI-VALDAT TO WH-WERT WZ-DATUM.
           COMPUTE VDU-LP = IX * 100 + 406.
       K.  DISPLAY "<esc>= Abbruch, <ret>= Valutadatum" AT 2301.
           CALL "CAUP" USING "1100276006" WH-CREG.
           IF ESC GO C.
           IF NOT RET GO K.
           IF WZ-DATUM = 0 GO I.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           MOVE WZ-DATUM TO ZI-VALDAT.
           SUBTRACT VDU-ECK FROM VDU-LP.
      *---------------------------------------------> BetragsÑnderung <-
       M.  DISPLAY "<esc>= Abbruch, <ret>= Betrag" AT 2301.
           MOVE ZI-BET TO WH-WERT.
           IF WH-WERT < 0 MOVE 46 TO VDU-P
                     else MOVE 37 TO VDU-P.
           CALL "CAUP" USING "1000007210" WH-CREG.
           IF ESC GO C.
           IF NOT RET GO M.
           MOVE WH-WERT TO ZI-BET WD-BET.
           COMPUTE VDU-LP = IX * 100 + 437 + VDU-ECK.
           DISPLAY " " with SIZE 23 AT VDU-LP.
           IF WH-WERT < 0 COMPUTE VDU-LP = IX * 100 + 446 + VDU-ECK
                     else COMPUTE VDU-LP = IX * 100 + 437 + VDU-ECK.
           DISPLAY WD-BET with highlight AT VDU-LP.
       O.  DISPLAY "<esc>= Abbruch, <ret>= speichern < >" AT 2301
           CALL "CAUP" USING "0023350000" WH-CREG.
           IF ESC GO C.
           IF NOT RET GO O.
           REWRITE ZI-SATZ.
           MOVE WS-DATUM TO WZ-DATUM.
           GO H.
      *--------------------------------------------> ZinssatzÑnderung <-
       Q.  MOVE LOW-VALUE TO ZI-SATZ.
           MOVE DE-KTONR TO ZI-NUM.
           READ ZINSEN KEY IS ZI-KEY INVALID PERFORM NO-REC GO G.
           IF ZUGRIF PERFORM BESETZT GO M.
           MOVE ZI-BET TO WH-WERT.
           DISPLAY "<esc>= Abbruch, <ret>= Zinssatz" AT 2301.
           CALL "CAUP" USING "1002342205" WH-CREG.
           IF ESC GO C.
           IF NOT RET GO G.
           MOVE WH-WERT TO ZI-BET WD-ZPZ.
           DISPLAY WD-ZPZ with highlight AT VDU-LP.
       R.  MOVE ZI-BELDAT TO WH-WERT.
           DISPLAY "<esc>= Abbruch, <ret>= +Tage" AT 2301.
           CALL "CAUP" USING "1002523003" WH-CREG.
           IF ESC GO C.
           IF NOT RET GO R.
           MOVE WH-WERT TO ZI-BELDAT WD-TGN.
           DISPLAY WD-TGN with highlight AT VDU-LP.
       W.  DISPLAY "<esc>= Abbruch, <ret>= speichern < >" AT 2301
           CALL "CAUP" USING "0023350000" WH-CREG.
           IF ESC GO C.
           IF NOT RET GO W.
           REWRITE ZI-SATZ.
           GO C.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           CLOSE ZINSEN.
       Z.  EXIT.
      ******************************************** Zinsendatei lîschen *
       ZILOESCH SECTION.
       A.  DISPLAY "Wirklich lîschen? Code: <      >" AT 2301.
           CALL "CAUP" USING "0023266006" WH-CREG.
           IF ESC GO Z.
           IF NOT RET GO A.
           IF WH-NUM NOT = DE-KTONR GO A.
           DISPLAY "-- bitte warten --" with highlight BLINK AT 2331.
       C.  MOVE LOW-VALUE TO ZI-SATZ.
           MOVE DE-KTONR TO ZI-NUM.
           START ZINSEN KEY NOT < ZI-KEY INVALID PERFORM NO-REC GO X.
       D.  READ ZINSEN NEXT AT END GO X.
           IF ZUGRIF PERFORM BESETZT GO D.
           IF ZI-NUM NOT = DE-KTONR GO X.
       F.  DELETE ZINSEN.
           IF ZUGRIF PERFORM BESETZT GO F.
           MOVE DE-OPKZ TO WD-KZ.
           MOVE WD-KZ TO DE-OPKZ.
           REWRITE DE-SATZ.
           GO D.
       X.  CALL "CAUP" USING "1323012580000" WH-CREG.
       Z.  EXIT.
      ******************************************************************
       NEUZINS SECTION.
       A.  MOVE LOW-VALUE TO ZI-SATZ.
           MOVE DE-KTONR TO ZI-NUM.
           READ ZINSEN KEY IS ZI-KEY INVALID GO B.
           IF ZUGRIF PERFORM BESETZT GO A.
           DISPLAY "Zinsdatei vorhanden" AT 2301.
           PERFORM WEITER.
           SET ESC TO TRUE.
           GO Z.
       B.  MOVE DE-OPKZ TO WD-KZ.
           ADD 10 WD-KZ GIVING DE-OPKZ.
       C.  ADD 807 VDU-ECK GIVING VDU-LP.
           DISPLAY "Beginn der Zinsenberechnung:           " AT VDU-LP.
           DISPLAY "<esc>= Abbruch, <ret>= Zinsabschlu·datum" AT 2301.
           CALL "CAUP" USING "1108366006" WH-CREG.
           IF ESC GO Z.
           IF NOT RET GO C.
           IF WZ-DATUM = 0 GO C.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           MOVE WZ-DATUM TO WS-DATUM.
       E.  ADD 234 VDU-ECK GIVING VDU-LP.
           IF WH-ZPZ = 0 MOVE 10 TO WH-ZPZ.
           MOVE WH-ZPZ TO WD-ZPZ WH-WERT.
           DISPLAY WD-ZPZ with highlight AT VDU-LP.
           DISPLAY "<esc>= Abbruch, <ret>= Zinssatz" AT 2301.
           CALL "CAUP" USING "1002342205" WH-CREG.
           IF ESC GO Z.
           IF NOT RET GO E.
           MOVE WH-WERT TO WD-ZPZ WH-ZPZ WV-ZPZ.
           DISPLAY WD-ZPZ with highlight AT VDU-LP.
       G.  ADD 252 VDU-ECK GIVING VDU-LP.
           MOVE WH-ZTAG TO WD-ZTAG WH-WERT.
           DISPLAY WD-ZTAG with highlight AT VDU-LP.
           DISPLAY "<esc>= Abbruch, <ret>= Tage" AT 2301.
           CALL "CAUP" USING "1002522002" WH-CREG.
           IF ESC GO Z.
           IF NOT RET GO G.
           MOVE WH-WERT TO WH-ZTAG WD-ZTAG.
           DISPLAY WD-ZTAG with highlight AT VDU-LP.
           PERFORM VALPOS.
           REWRITE DE-SATZ.
       Z.  EXIT.
      *****************************************************************
       VAL-ANZ SECTION.
       A.  CALL "CAUP" USING "16CLRFEN" WH-CREG.
           ADD 503 VDU-ECK GIVING VDU-LP.
           PERFORM VARYING WI FROM 1 BY 1 UNTIL WI > 9
               MOVE WI TO WD-POS
               DISPLAY WD-POS with highlight AT VDU-LP
               ADD 100 TO VDU-LP.
           MOVE LOW-VALUE TO WT-ERFTAB.
           MOVE 0 TO WX WR WF.
           MOVE 3 TO WI.
           MOVE 1 TO ZI-SA.
           MOVE WZ-DATUM TO ZI-VALDAT WS-DATUM.
           MOVE DE-KTONR TO ZI-NUM.
           START ZINSEN KEY NOT < ZI-KEY INVALID PERFORM NO-REC
                GO Z.
       E.  READ ZINSEN NEXT NO LOCK AT END MOVE 8 TO WI GO L.
           IF ZUGRIF PERFORM BESETZT GO E.
           IF (ZI-VALDAT = 0 AND ZI-SA = 0)
              OR ZI-SA = 50 OR ZI-NUM NOT = DE-KTONR
                  SET FINE TO TRUE
                  MOVE 8 TO WI GO L.
           IF WI < 3 PERFORM VARYING WF FROM 1 BY 1 UNTIL WF > 9
                        IF ZI-BZEIL = WT-KEY(WF) GO E.
           IF WI = 3 GO K.
           CALL "CAUP" USING "17SCROLLF" WH-CREG.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 8
               MOVE WT-KEY(WX + 1) TO WT-KEY(WX).
           MOVE 8 TO WX.
       K.  PERFORM VAL-DIS.
           IF WR NOT = 9 GO E.
           MOVE 9 TO WI.
       L.  PERFORM WASNUN.
           IF ESC OR KIST MOVE 0 TO ZI-NUM GO Z.
           IF RET; IF WH-NUM > 10  GO A
                   else MOVE WT-KEY(WH-NUM) TO ZI-BZEIL
                        MOVE DE-KTONR TO WH-WERT GO Z.
           IF AB GO E.
       N.  READ ZINSEN PREVIOUS NO LOCK AT END MOVE 2 TO WI GO L.
           IF ZUGRIF PERFORM BESETZT GO N.
           IF (ZI-VALDAT = 0 AND ZI-SA = 0)
              OR ZI-SA = 50 OR ZI-NUM NOT = DE-KTONR
                  SET FINE TO TRUE
                  MOVE 2 TO WI GO L.
           IF WI > 7 PERFORM VARYING WF FROM 9 BY -1 UNTIL WF < 1
                        IF ZI-BZEIL = WT-KEY(WF) GO N.
           CALL "CAUP" USING "22SCROLAB" WH-CREG.
           PERFORM VARYING WX FROM 8 BY -1 UNTIL WX < 1
              MOVE WT-KEY(WX) TO WT-KEY(WX + 1).
           MOVE 0 TO WX.
           PERFORM VAL-DIS.
           IF WR NOT = 9 GO N.
           MOVE 1 TO WI.
           GO L.
       Z.  EXIT.
      ******************************************************************
       VAL-DIS SECTION.
       A.  COMPUTE VDU-LP = WX * 100 + 506 + VDU-ECK.
           MOVE ZI-BELDAT TO WC-DATUM.
           PERFORM DATDREH.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           ADD 9 TO VDU-P.
           MOVE ZI-BELNR TO WD-NUM.
           DISPLAY WD-NUM AT VDU-LP.
           ADD 8 TO VDU-P.
           DISPLAY WE-SYM(ZI-SYM + 1) AT VDU-LP.
           ADD 4 TO VDU-P.
           MOVE ZI-VALDAT TO WC-DATUM.
           PERFORM DATDREH.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           MOVE ZI-BET TO WD-BET.
           IF ZI-BET > 0 ADD 10 TO VDU-P
                    else ADD 19 TO VDU-P.
           DISPLAY WD-BET with highlight AT VDU-LP.
           ADD 1 TO WR WX.
           MOVE ZI-BZEIL TO WT-KEY(WX).
       Z.  EXIT.
      ******************************************************************
       WASNUN SECTION.
       A.  IF WX = 0 PERFORM NO-REC
               SET ESC TO TRUE GO Z.
       B.  DISPLAY "Zeile+<ret>= Auswahl, < / >= weiter-/zurÅcklisten, <
      -        "esc/Ende>= Ende" AT 2301.
           DISPLAY "<Datum+ret>= anzeigen <      >" AT 2401.
           IF FINE DISPLAY "** keine Daten mehr **"
               with highlight AT 2455.
           CALL "CAUP" USING "0124246006" WH-CREG.
           MOVE 1 TO WR.
           IF ESC OR ENDE SET ESC TO TRUE MOVE 0 TO WH-NUM GO Z.
           IF WH-NUM > 10000; IF WZ-DATUM = 0 GO B else GO Z.
           IF WH-NUM > 0 AND WH-NUM < 10;
               IF WT-KEY(WH-NUM) = LOW-VALUE GO B ELSE GO Z.
           IF RET; SET AB TO TRUE.
           IF FINE; IF AB  AND WI = 8 GO B
               ELSE IF AUF AND WI = 2 GO B.
           IF NOT AB AND NOT AUF GO A.
       Z.  EXIT.
      ********************************* Reorg = lîschen alter BestÑnde *
       ZIREORG SECTION.
       A.  CALL "CAUP" USING "0715130660000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           OPEN I-O ZINSEN.
           DISPLAY " Reorg Zinsen " with highlight AT VDU-LP.
       C.  CALL "CAUP" USING "16CLRFEN" WH-CREG.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "Lîschdatum: " AT VDU-LP.
           MOVE WH-DATUM TO WZ-DATUM.
           IF WZ-MONAT < 8 ADD 12 TO WZ-MONAT
               SUBTRACT 1 FROM WZ-JAHR.
           SUBTRACT 7 FROM WZ-MONAT.
           MOVE 32 TO WZ-TAG.
       E.  MOVE WZ-DATUM TO WV-DATUM.
           CALL "CAUP" USING "05DATPRF" WH-CREG.
           IF WZ-DATUM = 0 ADD -1 TO WV-DATUM GIVING WZ-DATUM GO E.
           DISPLAY VDU-DATUM with highlight AT 0000.
           MOVE WX-DATUM TO WV-DATUM.
       G.  DISPLAY "<esc>= Abbruch, <ret>= Start < >" AT 2301.
           CALL "CAUP" USING "0023310000" WH-CREG.
           IF ESC GO X.
           IF NOT RET GO G.
           MOVE LOW-VALUE TO ZI-SATZ.
           MOVE 0 TO DE-KTONR.
           START ZINSEN KEY NOT < ZI-KEY INVALID GO X.
           GO O.
      *--------------------------------------> lîschen Zinsbewegungen <-
       I.  MOVE 0 TO WH-MOD.
           MOVE ZI-NUM TO DE-KTONR.
       K.  READ DEBITOR NO LOCK INVALID MOVE 1 TO WH-MOD.
           IF ZUGRIF PERFORM BESETZT GO K.
           DIVIDE 10 INTO DE-KTONR GIVING WD-KTO.
           ADD 403 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-KTO AT VDU-LP " " WT-TX(1:47).
       M.  IF WH-MOD = 0 AND ZI-VALDAT > WV-DATUM GO O.
           IF WH-MOD = 0 AND ZI-VALDAT = 0 AND ZI-SA = 0 GO O.
      *--------------------> 0.Satz nur bei fehlendem Debitor lîschen <-
           DELETE ZINSEN.
       O.  READ ZINSEN NEXT AT END GO X.
           IF ZUGRIF PERFORM BESETZT GO O.
           IF ZI-NUM NOT = DE-KTONR GO I.
           GO M.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           CLOSE ZINSEN.
