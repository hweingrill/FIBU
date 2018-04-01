      $SET LINKCOUNT"240" ANS85 BOUND AUTOLOCK NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     FIBKRED.
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.   PC.
       SPECIAL-NAMES.     DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY FIBUSEC.CPY.
           SELECT EXCELIS   ASSIGN TO PRINTER WH-FIBNAM
                            FILE STATUS WF-STATUS.
           SELECT DRUCKER  ASSIGN TO PRINTER WH-DRUNAM
                           FILE STATUS WF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       COPY FIBUFD.CPY.
      ******************************************************************
       FD  DRUCKER                     LABEL RECORD STANDARD.
       01  DRA-SATZ.
           03  FILLER                  PIC XXXX.
           03  DRA-STR.
               05 DRA-KTONR            PIC ZZZZZZ,Z-.
               05 DRA-BELNR            PIC ZZZZZZ-.
               05 FILLER               PIC X.
               05 DRA-BELDAT           PIC X(8).
               05 DRA-REFNR            PIC ZZZZZZ-.
               05 DRA-SYM              PIC XX.
               05 DRA-GGKTO            PIC ZZZZZZ,Z-.
               05 DRA-TX               PIC X(40).
               05 DRA-UST              PIC ZZ-.
               05 DRA-MWSKT            PIC ZZZZZ.ZZZ,ZZ-.
               05 DRA-SOLL             PIC ZZ.ZZZ.ZZZ,ZZ-.
               05 DRA-HABEN            PIC ZZ.ZZZ.ZZZ,ZZ-.
           03  FILLER                  PIC X.
      ******************************************** Excel-öbergabedatei *
       FD  EXCELIS                     LABEL RECORD OMITTED.
       01  EXC-SATZ.
           03  EXC-KAT                 PIC 9.
           03  EXC-A                   PIC X.
           03  EXC-KTONR               PIC ZZZZ9.
           03  EXC-B                   PIC X.
           03  EXC-MCODE               PIC X(15).
           03  EXC-C                   PIC X.
           03  EXC-GRP                 PIC 9.
           03  EXC-D                   PIC X.
           03  EXC-OPKZ                PIC 9.
           03  EXC-E                   PIC X.
           03  EXC-DT                  PIC 99.
           03  EXC-G                   PIC X.
           03  EXC-PZ1                 PIC 99,9.
           03  EXC-J                   PIC X.
           03  EXC-TG1                 PIC 999.
           03  EXC-K                   PIC X.
           03  EXC-PZ2                 PIC 99,9.
           03  EXC-L                   PIC X.
           03  EXC-TG2                 PIC 999.
           03  EXC-M                   PIC X.
           03  EXC-TGN                 PIC 999.
           03  EXC-N                   PIC X.
           03  EXC-BKKTO               PIC X(15).
           03  EXC-NN                  PIC X.
           03  EXC-BKLZL               PIC X(6).
           03  EXC-O                   PIC X.
           03  EXC-BKNAME              PIC X(35).
           03  EXC-P                   PIC X.
           03  EXC-NAME                PIC X(95).
           03  EXC-Q                   PIC X.
           03  EXC-ZUSATZ              PIC X(35).
           03  EXC-R                   PIC X.
           03  EXC-STR                 PIC X(35).
           03  EXC-S                   PIC X.
           03  EXC-PLZL                PIC 99999.
           03  EXC-T                   PIC X.
           03  EXC-ORT                 PIC X(30).
      ******************************************************************
       WORKING-STORAGE SECTION.
       01  WH-CALL.
           03  WL-CA                   PIC 99.
           03  WL-REST                 PIC 9(13).
       COPY WHCREG.CPY.
       01  WF-REG.
           03  WK-DSTG.
               05 WK-STG               PIC X(10)     OCCURS 10.
           03  WH-P1                   PIC 99        COMP.
           03  WH-P2                   PIC 99        COMP.
           03  WX-PRNO                 PIC 99        COMP-X.
           03  WX-PRSTAT               PIC 99        COMP-X.
           03  WZ-SEITE                PIC 99        COMP   VALUE ZERO.
           03  WZ-SCHALT               PIC 99        COMP   VALUE ZERO.
           03  WZ-ZEILEN               PIC 99        COMP   VALUE ZERO.
           03  WH-FIBNAM               PIC X(12).
           03  WH-DRUNAM               PIC X(12)     VALUE   "LPT1:".
           03  WM-OPEN                 PIC 9         COMP   VALUE ZERO.
           03  WD-SEITE                PIC ZZ9.
           03  WM-MERKER               PIC 9        COMP-3 VALUE ZERO.
                   88 ANLAGE   VALUE 1 3.      88  AEND   VALUE 0 2 3.
           03  WT-ERFTAB.
               05  WT-KEY              PIC X(12)         OCCURS 15.
           03  WD-KTO                  PIC ZZ.ZZ9,9.
           03  WD-BANK                 PIC ZZ9.
           03  WD-BLZ                  PIC 99.999.
           03  WH-MOD                  PIC 99           COMP.
           03  VDU-REL                 PIC 9999.
           03  VDR-REL REDEFINES VDU-REL.
               05  VDU-RL              PIC 99.
               05  VDU-RP              PIC 99.
           03  WD-X                    PIC X.
           03  WU-KTO                  PIC 9(6).
      *----------------------------------------> Konditionentabelle <--
           03  WH-KK                   PIC 9(13).
           03  WR-KK REDEFINES WH-KK.
               05  WH-SK1              PIC 9V9.
               05  WH-TG1              PIC 999.
               05  WH-SK2              PIC 9V9.
               05  WH-TG2              PIC 999.
               05  WH-TGN              PIC 999.
           03  WD-TG                   PIC ZZ9.
           03  WD-PZ                   PIC 9,9.
           03  WD-KZ                   PIC 9.
           03  WD-KK.
               05 WD-SK1               PIC Z,Z/.
               05 WD-TG1               PIC ZZZ/.
               05 WD-TGN               PIC ZZ9-.
       COPY FIBUEXT.CPY.
       COPY FIBUDECL.CPY.
       END DECLARATIVES.
      ******************************************************************
       STEUER SECTION.
       A.  MOVE WL-CALL TO WH-CALL.
           MOVE WL-CREG TO WH-CREG.
           EVALUATE WL-CA
               WHEN 40
               WHEN 41
               WHEN 42 PERFORM KRED.
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
      *****************************************************************
       NO-REC SECTION.
       A.  DISPLAY "keine Daten vorhanden," AT 2401.
           PERFORM WEITER.
           MOVE 03 TO WX-TASTE.
       Z.  EXIT.
      ******************************************************************
       WEITER SECTION.
       A.  DISPLAY " weiter mit <ret>: " with highlight AT 0000.
           MOVE SPACE TO WD-X.
           ACCEPT WD-X AT 0000.
           CALL "CAUP" USING "1324012480000" WH-CREG.
       Z.  EXIT.
      ************************************************* ob Drucker ok *
       DRU-OK SECTION.
       A.  GO Z.
           MOVE 0 TO WX-PRNO.
           CALL "PC_TEST_PRINTER" USING WX-PRNO WX-PRSTAT.
           IF WX-PRSTAT =
               208 OR 192 OR 144 OR 128 OR 80 OR 64 OR 16 GO Z.
           DISPLAY "Drucker nicht bereit: Fehler beheben und" AT 2401
              PERFORM WEITER GO A.
       Z.  EXIT.
      ******************************************************* Drucker *
       DRUCK SECTION.
       A.  WRITE DRA-SATZ AFTER WZ-SCHALT.
           MOVE SPACE TO DRA-SATZ.
           ADD WZ-SCHALT TO WZ-ZEILEN.
           MOVE 1 TO WZ-SCHALT.
       Z.  EXIT.
      ****************************** Druckerrueckstellung auf 10/Zoll *
       END-DRU SECTION.
       A.  IF WM-OPEN = 0 GO Z.
           IF WM-DRU = 0 MOVE X"1B210000" TO DRA-SATZ(1:4).
       B.  WRITE DRA-SATZ BEFORE PAGE.
           IF WF-STATUS = 27 GO B.
           MOVE SPACE TO DRA-SATZ.
           CLOSE DRUCKER.
           MOVE 0 TO WM-OPEN.
       Z.  EXIT.
      ******************************************************************
       BEG-DRU SECTION.
       A.  IF WM-OPEN > 0 GO Z.
           MOVE 1 TO WM-OPEN.
           OPEN OUTPUT DRUCKER.
       C.  MOVE 0 TO WZ-ZEILEN WZ-SCHALT.
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
       KRED SECTION.
       A.  CALL "CAUP" USING "0711011080000" WH-CREG.
      *    OPEN I-O ZUSATZ.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Verwaltung Kreditoren " with highlight AT VDU-LP.
       C.  ADD 202 VDU-ECK GIVING VDU-LP.
           CALL "CAUP" USING "16CLRFEN" WH-CREG.
           DISPLAY "Nr.:" AT VDU-LP.
           IF WL-CA = 41 or WL-CA = 42 GO E.
           DISPLAY "<ret>= Anlage/éndern, <ret>+ alpha = suchen, <esc>=
      -        "Ende, < >= Stammdatendruck" AT 2301.
           CALL "CAUP" USING "1002086006" WH-CREG.
           IF ESC OR WOLI MOVE 0 TO KR-KTONR;
                IF WL-CA = 41 GO X
                ELSE MOVE "FIBSTAM 60STAM" TO WT-TX GO X.
           IF AB PERFORM STAMDRUCK GO C.
           IF NOT RET GO C.
           IF WH-NUM = 0 MOVE 3 TO WH-NUM
               CALL "FIBANZ" USING "49KRESUCH" WH-CREG
               CANCEL "FIBANZ"
               IF KR-KTONR = 0 GO C.
       D.  MOVE WH-NUM TO KR-KTONR WD-KZ.
           IF WD-KZ NOT = 3 GO C.
           MOVE 0 TO WM-MERKER.
       E.  READ KREDIT INVALID MOVE 1 TO WM-MERKER.
           IF ZUGRIF PERFORM BESETZT GO E.
       F.  PERFORM KR-ANL.
           UNLOCK KREDIT.
           IF WL-CA = 40 GO C.
           IF WL-CA = 41 or 42 GO X.
           IF ESC MOVE 0 TO KR-KTONR.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ******************************************************************
       KR-BILD SECTION.
       A.  COMPUTE WH-WERT = KR-KTONR / 10.
           MOVE WH-WERT TO WD-KTO.
           ADD 214 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-KTO with highlight AT VDU-LP.
           ADD 202 VDU-ECK GIVING VDU-LP.
           DISPLAY "Konto-Nr.: " AT VDU-LP.
           ADD 302 VDU-ECK GIVING VDU-LP.
           DISPLAY "Bezeichnung:" AT VDU-LP.
           ADD 223 VDU-ECK GIVING VDU-LP.
           DISPLAY "Kurzname:" AT VDU-LP.
           ADD 250 VDU-ECK GIVING VDU-LP.
           DISPLAY "Kontogruppe......:" AT VDU-LP.
           ADD 350 VDU-ECK GIVING VDU-LP.
           DISPLAY "WÑhrungs-Kz......:" AT VDU-LP.
           ADD 450 VDU-ECK GIVING VDU-LP.
           DISPLAY "OP-Verarb.Kz.....:" AT VDU-LP.
           ADD 550 VDU-ECK GIVING VDU-LP.
           DISPLAY "Zahlungsart......:" AT VDU-LP.
           ADD 650 VDU-ECK GIVING VDU-LP.
           DISPLAY "Zahlungskond.Kz..:" AT VDU-LP.
           ADD 902 VDU-ECK GIVING VDU-LP.
           DISPLAY "Bank-Kto.:                 Bank-Lzl.:        Bez.:"
               AT VDU-LP.
           IF AEND GO D.
           INITIALIZE KR-SATZ.
           MOVE WH-NUM TO KR-KTONR.
           MOVE 1 TO WX.
       D.  ADD 214 VDU-ECK GIVING VDU-LP.
           DIVIDE KR-KTONR BY 10 GIVING WD-KTO.
           DISPLAY WD-KTO with highlight AT VDU-LP.
           PERFORM HOLADR.
           ADD 233 VDU-ECK GIVING VDU-LP.
           MOVE KR-MCODE TO WT-TX.
           CALL "CAUP" USING "21CONV08" WH-CREG.
           DISPLAY WT-TX with highlight SIZE 8 AT VDU-LP.
           ADD 269 VDU-ECK GIVING VDU-LP.
           MOVE KR-KTOART TO WD-KZ.
           DISPLAY WD-KZ with highlight AT VDU-LP.
           ADD 369 VDU-ECK GIVING VDU-LP.
           MOVE KR-FWKZ TO WD-KZ.
           DISPLAY WD-KZ with highlight AT VDU-LP.
           ADD 469 VDU-ECK GIVING VDU-LP.
           MOVE KR-OPKZ TO WD-KZ.
           DISPLAY WD-KZ with highlight AT VDU-LP.
           ADD 569 VDU-ECK GIVING VDU-LP.
           MOVE KR-UEWTRKZ TO WD-KZ.
           DISPLAY WD-KZ with highlight AT VDU-LP.
           ADD 913 VDU-ECK GIVING VDU-LP.
           DISPLAY KR-BANKKTO with highlight AT VDU-LP.
           ADD 940 VDU-ECK GIVING VDU-LP.
           DISPLAY KR-BANKLTZ with highlight AT VDU-LP.
           ADD 953 VDU-ECK GIVING VDU-LP.
           DISPLAY KR-BANK with highlight AT VDU-LP.
           PERFORM KOND-ANZ.
       Z.  EXIT.
      ******************************************************************
       BEDMES SECTION.
       A.  DISPLAY "<esc>= Abbruch, <ret-leer>= keine énderung, <ende>=
      -        "Ende" AT 2301.
       Z.  EXIT.
      ******************************************************************
       KR-ANL SECTION.
       A.  PERFORM KR-BILD.
           DISPLAY "<esc>= Abbruch" AT 2301.
           IF ANLAGE DISPLAY "Lieferant fehlt!" AT 2401.
       C.  PERFORM HOLADR.
           CALL "CAUP" USING "1203150534" WH-CREG.
           IF WL-CA = 42 GO Z.
           IF WOLI OR ESC MOVE 0 TO KR-KTONR GO Z.
           IF WT-BEZ = SPACE DISPLAY "Bezeichnung mu· sein!" AT 2401
               PERFORM WEITER GO C.
           MOVE WT-BEZ TO KR-BEZ.
       E.  DISPLAY "< />= zurÅck, <esc>= Abbruch, <ret>= Eingabe, <ret-
      -        "leer>= Wiederholung" AT 2301.
           MOVE KR-MCODE TO WT-TX.
           CALL "CAUP" USING "21CONV08" WH-CREG.
           ADD 233 VDU-ECK GIVING VDU-LP.
           DISPLAY WT-TX WITH SIZE 8 HIGHLIGHT AT VDU-LP.
           CALL "CAUP" USING "1202330108" WH-CREG.
           IF WOLI or AUF GO C.
           IF ESC GO R.
           CALL "CAUP" USING "20CONV08" WH-CREG.
           MOVE WT-TX TO KR-MCODE.
           CALL "CAUP" USING "21CONV08" WH-CREG.
           DISPLAY WT-TX WITH SIZE 8 HIGHLIGHT AT VDU-LP.
       F.  MOVE KR-KTOART TO WH-WERT.
           DISPLAY "Kontogruppe (0,1,2) fÅr Sammelkontenaufteilung"
               AT 2401.
           CALL "CAUP" USING "1002691001" WH-CREG.
           IF ESC GO R.
           IF WOLI GO E.
           IF NOT RET GO F.
           MOVE WH-WERT TO KR-KTOART WD-KZ.
           DISPLAY WD-KZ with highlight AT VDU-LP.
       G.  MOVE KR-FWKZ TO WH-WERT.
           DISPLAY "in Planung daher 0 eingeben" AT 2401.
           CALL "CAUP" USING "1003691001" WH-CREG.
           IF ESC GO R.
           IF WOLI GO F.
           IF NOT RET GO G.
           MOVE WH-WERT TO KR-FWKZ WD-KZ.
           DISPLAY WD-KZ with highlight AT VDU-LP.
       H.  MOVE KR-OPKZ TO WH-WERT.
           DISPLAY "in Planung daher 0 eingeben" AT 2401.
           CALL "CAUP" USING "1004691001" WH-CREG.
           IF ESC GO R.
           IF WOLI GO G.
           IF NOT RET GO H.
           MOVE WH-WERT TO KR-OPKZ WD-KZ.
           DISPLAY WD-KZ with highlight AT VDU-LP.
       I.  MOVE KR-UEWTRKZ TO WH-WERT.
           DISPLAY "0= öberweisung, 1= Bankeinzug" AT 2301.
           CALL "CAUP" USING "1005691001" WH-CREG.
           IF ESC GO R.
           IF WOLI GO H.
           IF NOT RET GO I.
           MOVE WH-WERT TO KR-UEWTRKZ WD-KZ.
           DISPLAY WD-KZ with highlight AT VDU-LP.
       J.  MOVE KR-KOND TO WH-KK.
           IF KR-KOND NOT = 0 PERFORM KOND-ANZ.
           PERFORM BEDMES.
           MOVE 0 TO WH-NUM WH-WERT.
           DISPLAY "<#>= Konditionen anzeigen" AT 2301.
           CALL "CAUP" USING "1006691001" WH-CREG.
           IF KIST CALL "FIBSTAM" USING "12KOND" WH-CREG
               CANCEL "FIBSTAM" GO J.
           IF ENDE GO R.
           IF ESC GO Z.
           IF WOLI GO I.
           IF NOT RET GO J.
           MOVE WH-WERT TO WD-KZ WY.
           DISPLAY WD-KZ with highlight AT VDU-LP.
           IF WY NOT = 0 MOVE WT-KONDIT(WY) TO KR-KOND
               PERFORM KOND-ANZ
           ELSE PERFORM MAN-KOND.
           IF WOLI GO I.
           IF ESC GO Z.
       K.  MOVE KR-BANKKTO TO WT-TX.
           DISPLAY "Bankkonto des EmpfÑngers, (*Konto-Nr = öwsgs.Kto.)"
               AT 2401.
           DISPLAY "Bei öwsg.Kto immer ohne ,3 angeben" AT 2501.
           CALL "CAUP" USING "1209130115" WH-CREG.
           IF ESC GO R.
           IF WOLI GO J.
           IF NOT RET GO K.
           MOVE WT-TX TO KR-BANKKTO.
           PERFORM PRUEFEN.
           IF ESC GO K.
       L.  MOVE KR-BANKLTZ TO WT-TX.
           DISPLAY "Bankleitzahl der EmpfÑngerbank" AT 2301.
           CALL "CAUP" USING "1209400107" WH-CREG.
           IF WOLI GO K.
           IF ESC GO R.
           IF NOT RET GO L.
           MOVE WT-TX TO KR-BANKLTZ.
           IF KR-BANKKTO not = SPACE and KR-BANKLTZ = SPACE
               DISPLAY "Achtung! Fehlerhafte Anlage!" AT 2401
               PERFORM WEITER.
       N.  MOVE KR-BANK TO WT-TX.
           IF KR-BANKKTO(1:1) = "*"
               DISPLAY "öwsg.Adresse" with highlight AT 2301
           else DISPLAY "Bankverbindung des EmpfÑngers" AT 2301.
           CALL "CAUP" USING "1209530122" WH-CREG.
           IF WOLI OR ESC GO R.
           MOVE WT-TX TO KR-BANK.
           IF KR-BANKKTO not = SPACE and KR-BANK = SPACE
               DISPLAY "Achtung! Fehlerhafte Anlage!" AT 2401
               PERFORM WEITER.
       R.  DISPLAY "<tab>= lîschen, <ret>= speichern, <esc>= Abbruch, <
      -        ">= zurÅck" AT 2301.
           CALL "CAUP" USING "0023630000" WH-CREG.
           IF ESC UNLOCK KREDIT GO Z.
           IF WOLI GO N.
           IF TABL GO Y.
           IF NOT RET GO R.
       X.  WRITE KR-SATZ INVALID REWRITE KR-SATZ.
           IF KR-BANKKTO(1:1) = "*" PERFORM AUCH-ANL.
           GO Z.
       Y.  ADD 316 VDU-ECK GIVING VDU-LP.
           DISPLAY "    ** gelîscht **    " with highlight AT VDU-LP.
           DELETE KREDIT INVALID GO Z.
           IF ZUGRIF PERFORM BESETZT GO Y.
       Z.  EXIT.
      *****************************************************************
       AUCH-ANL SECTION.
       A.  MOVE WU-KTO TO KR-KTONR.
           READ KREDIT IGNORE LOCK not INVALID GO Z.
           DISPLAY "Bitte auch das Konto: " AT 2401.
           DISPLAY WU-KTO AT 0000 " anlegen!".
           PERFORM WEITER.
       Z.  EXIT.
      *****************************************************************
       PRUEFEN SECTION.
       A.  SET RET TO TRUE.
           IF WT-TX(1:1) not = "*" GO Z.
           MOVE 5 TO WR.
           MOVE 3 TO WU-KTO.
           IF WT-TX(7:) not = SPACE GO Q.
           PERFORM VARYING WX FROM 6 BY -1 UNTIL WX = 1
               IF WT-TX(WX:1) not = " "
                   MOVE WT-TX(WX:1) TO WU-KTO(WR:1)
                   ADD -1 TO WR.
           IF WU-KTO not = 3 GO Z.
       Q.  DISPLAY "keine gÅltige Konto-Nr." AT 2401.
           PERFORM WEITER.
           SET ESC TO TRUE.
       Z.  EXIT.
      *****************************************************************
       KOND-ANZ SECTION.
       A.  ADD 750 VDU-ECK GIVING VDU-LP.
           MOVE KR-KOND TO WH-KK.
           IF KR-KOND = 0
               DISPLAY "keine Skontierung    " AT VDU-LP
               ADD 100 TO VDU-LP
               DISPLAY "                     " AT VDU-LP GO Z.
           DISPLAY "                             " AT VDU-LP.
           ADD 100 TO VDU-LP.
           DISPLAY "                     " AT VDU-LP.
           SUBTRACT 100 FROM VDU-LP.
           IF KR-KOND = 9999999 MOVE 0 TO KR-KOND WH-KK.
           DISPLAY " %-1 Tg. %-2 Tg. TgN. " with reverse-video
               AT VDU-LP.
           ADD 101 TO VDU-LP.
           IF WH-SK1 NOT = 0 MOVE WH-SK1 TO WD-PZ
               DISPLAY WD-PZ with highlight AT VDU-LP
               MOVE WH-TG1 TO WD-TG
               ADD 4 TO VDU-LP
               DISPLAY WD-TG with highlight AT VDU-LP
               ADD 4 TO VDU-P
           else ADD 8 TO VDU-LP.
           IF WH-SK2 NOT = 0 MOVE WH-SK2 TO WD-PZ
               DISPLAY WD-PZ with highlight AT VDU-LP
               MOVE WH-TG2 TO WD-TG
               ADD 4 TO VDU-LP
               DISPLAY WD-TG with highlight AT VDU-LP
               ADD 4 TO VDU-P
           else ADD 8 TO VDU-LP.
           IF WH-TGN NOT = 0 MOVE WH-TGN TO WD-TG
               DISPLAY WD-TG with highlight AT VDU-LP.
       Z.  EXIT.
      ***************************************** man. Konditionseingabe *
       MAN-KOND SECTION.
       A.  IF KR-KOND = 0 MOVE 9999999 TO KR-KOND.
           PERFORM KOND-ANZ.
           DISPLAY "<ret>= Skonto-1 (x,x), <esc>= Abbruch" AT 2301.
           MOVE WH-SK1 TO WH-WERT.
           CALL "CAUP" USING "1008512103" WH-CREG.
           IF ESC OR WOLI GO Z.
           IF NOT RET GO A.
           MOVE WH-WERT TO WH-SK1 WD-PZ.
           DISPLAY WD-PZ with highlight AT VDU-LP.
       F.  DISPLAY "<ret>= Tage-1, <esc>= Abbruch" AT 2301.
           MOVE WH-TG1 TO WH-WERT.
           CALL "CAUP" USING "1008553003" WH-CREG.
           IF ESC GO Z.
           IF WOLI GO A.
           IF NOT RET GO F.
           MOVE WH-WERT TO WH-TG1 WD-TG.
           DISPLAY WD-TG with highlight AT VDU-LP.
       G.  DISPLAY "<ret>= Skonto-2 (x,x), <esc>= Abbruch" AT 2301.
           MOVE WH-SK2 TO WH-WERT.
           CALL "CAUP" USING "1008592103" WH-CREG.
           IF ESC GO Z.
           IF WOLI GO F.
           IF NOT RET GO G.
           MOVE WH-WERT TO WH-SK2 WD-PZ.
           DISPLAY WD-PZ with highlight AT VDU-LP.
       H.  DISPLAY "<ret>= Tage-2, <esc>= Abbruch" AT 2301.
           MOVE WH-TG2 TO WH-WERT.
           CALL "CAUP" USING "1008633003" WH-CREG.
           IF ESC GO Z.
           IF WOLI GO G.
           IF NOT RET GO H.
           MOVE WH-WERT TO WH-TG2 WD-TG.
           DISPLAY WD-TG with highlight AT VDU-LP.
       I.  DISPLAY "<ret>= Nettotage, <esc>= Abbruch" AT 2301.
           MOVE WH-TGN TO WH-WERT.
           CALL "CAUP" USING "1008673003" WH-CREG.
           IF ESC GO Z.
           IF WOLI GO H.
           IF NOT RET GO I.
           MOVE WH-WERT TO WH-TGN WD-TG.
           DISPLAY WD-TG with highlight AT VDU-LP.
           MOVE WH-KK TO KR-KOND.
       Z.  EXIT.
      *************************************** Adresszerlegung f. Druck *
       HOLADR SECTION.
       A.  ADD 315 VDU-ECK GIVING VDU-LP.
           MOVE KR-BEZ TO WT-BEZ.
           MOVE SPACE TO WT-ADR.
           MOVE "#" TO WR-BEZ(131).
           UNSTRING WT-BEZ DELIMITED BY "#" INTO WR-ADR(1) WR-ADR(2)
               WR-ADR(3) WR-ADR(4) WR-ADR(5) WR-ADR(6).
           PERFORM VARYING WR FROM 1 BY 1 UNTIL WR > 5
               DISPLAY WR-ADR(WR) with SIZE 34 highlight AT VDU-LP
               ADD 100 TO VDU-LP.
       Z.  EXIT.
      *********************************************** Druck Stammdaten *
       STAMDRUCK SECTION.
       A.  CALL "CAUP" USING "0710100466000" WH-CREG.
           PERFORM LADE-DRU.
           MOVE "KREDSTAM.LST" TO WH-DRUNAM.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Stammdatendruck " AT VDU-LP.
           ADD 30 TO VDU-LP.
           DISPLAY " KREDSTAM.LST " with highlight AT VDU-LP.
           MOVE 0 TO WH-MOD.
       C.  ADD 150 VDU-ECK GIVING VDU-LP.
           IF WH-MOD = 0 DISPLAY " schmal " AT VDU-LP
                    else DISPLAY " breit  " AT VDU-LP.
           ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "<esc>= Abbruch, <ret>= Nummerndruck, <tab>= Matchcod
      -        "edruck < >" AT VDU-LP.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "< >= Wechsel zu " AT VDU-LP.
           IF WH-MOD = 0 DISPLAY "breit " with highlight AT 0000
                    else DISPLAY "schmal" with highlight AT 0000.
           CALL "CAUP" USING "1102630000" WH-CREG.
           IF ESC GO X.
           IF AUF; IF WH-MOD = 0 MOVE 1 TO WH-MOD
                            else MOVE 0 TO WH-MOD.
           IF NOT RETA GO C.
           MOVE "KREDIT.LST" TO WH-FIBNAM.
           OPEN OUTPUT EXCELIS.
           IF WM-OPEN = 0 MOVE "Kat;Kto-Nr.;Matchcode;Grp.;OP-Kz;Dat-Tr;
      -        "%-Sk1;Tg-1;%-Sk2;TG-2;TG-N;Bank-Kto;Bklzl;Bankmname;Name
      -        ";Zusatz;Strasse;Plzl;Ort" TO EXC-SATZ
               INSPECT EXC-SATZ REPLACING ALL ";" BY X"09"
               WRITE EXC-SATZ
               MOVE SPACE TO EXC-SATZ.
           ADD 302 VDU-ECK GIVING VDU-LP.
           CALL "CAUP" USING "1300000060" WH-CREG.
           IF RET MOVE 0 TO KR-KTONR
               START KREDIT KEY NOT < KR-KEY INVALID GO C
           else MOVE LOW-VALUES TO KR-MCODE
               START KREDIT KEY NOT < KR-MCODE INVALID GO C.
       E.  READ KREDIT NEXT IGNORE LOCK AT END GO W.
           IF ZUGRIF PERFORM BESETZT GO E.
           ADD 302 VDU-ECK GIVING VDU-LP.
           MOVE KR-KTONR TO WD-KZ.
           PERFORM KREDKOPF.
           MOVE ALL x"09" TO EXC-SATZ.
           DIVIDE 10 INTO KR-KTONR GIVING EXC-KTONR.
           DIVIDE 10 INTO KR-KTONR GIVING WD-KTO.
           MOVE WD-KTO TO DRA-STR(1:10).
           INSPECT KR-BEZ REPLACING ALL "#" BY ",".
           MOVE KR-BEZ TO DRA-STR(10:50).
           DISPLAY WD-KTO with highlight AT VDU-LP DRA-STR(9:50).
           MOVE KR-MCODE TO WT-TX.
           CALL "CAUP" USING "21CONV08" WH-CREG.
           MOVE WT-TX TO DRA-STR(61:8) EXC-MCODE.
           MOVE KR-ZUS TO WD-KZ.
           MOVE WD-KZ TO DRA-STR(70:1).
           IF KR-UEWTRKZ = 1 MOVE "E" TO DRA-STR(72:1).
           MOVE KR-KOND TO WH-KK.
           MOVE WH-SK1 TO WD-SK1.
           MOVE WH-TG1 TO WD-TG1.
           MOVE WH-TGN TO WD-TGN.
           MOVE WD-KK TO DRA-STR(74:).
           IF WH-MOD = 1 MOVE KR-BANKKTO TO DRA-STR(87:13)
                         MOVE KR-BANKLTZ TO DRA-STR(99:6)
                         MOVE KR-BANK TO DRA-STR(105:24).
           PERFORM DRUCK.
           MOVE KR-KTONR TO EXC-KAT.
           MOVE KR-UEWTRKZ TO EXC-DT.
           MOVE WH-SK1 TO WD-PZ EXC-PZ1.
           MOVE WD-PZ TO DRA-STR(70:).
           MOVE WH-TG1 TO WD-TG EXC-TG1.
           MOVE WD-TG TO DRA-STR(73:).
           MOVE WH-SK2 TO WD-PZ EXC-PZ2.
           MOVE WD-PZ TO DRA-STR(78:).
           MOVE WH-TG2 TO WD-TG EXC-TG2.
           MOVE WD-TG TO DRA-STR(81:).
           MOVE WH-TGN TO WD-TG EXC-TGN.
           MOVE WD-TG TO DRA-STR(86:).
      *    MOVE KR-TEL TO DRA-STR(110:) EXC-TEL.
      *--------------------------------------> restl. Daten fÅr Excel <-
      *    03  KR-ZUS                  PIC 99        COMP.
           MOVE KR-BANKKTO TO EXC-BKKTO.
           MOVE KR-BANKLTZ TO EXC-BKLZL.
           MOVE KR-BANK TO EXC-BKNAME.
           MOVE KR-KTOART TO EXC-GRP.
           MOVE KR-OPKZ TO EXC-OPKZ.
           MOVE KR-BEZ TO WT-BEZ.
           MOVE SPACE TO WT-ADR.
           MOVE "," TO WR-BEZ(131).
           UNSTRING WT-BEZ DELIMITED BY "," INTO WR-ADR(1) WR-ADR(2)
               WR-ADR(3) WR-ADR(4) WR-ADR(5) WR-ADR(6).
           MOVE WR-ADR(1) TO EXC-NAME.
           MOVE WR-ADR(2) TO EXC-ZUSATZ.
           MOVE 0 TO EXC-PLZL.
           MOVE SPACE TO EXC-ORT EXC-STR.
           IF WR-ADR(6)(1:10) not = SPACE
               MOVE WR-ADR(6) TO WT-TX
               PERFORM PLZL-ORT
               MOVE WR-ADR(5) TO EXC-STR
               GO G.
           IF WR-ADR(5) not = SPACE MOVE WR-ADR(5) TO WT-TX
               IF WT-TX(1:4) not NUMERIC GO F.
           IF WR-ADR(5) not = SPACE
               MOVE WR-ADR(5) TO WT-TX
               PERFORM PLZL-ORT
               MOVE WR-ADR(4) TO EXC-STR
               GO G.
       F.  IF WR-ADR(4) not = SPACE
               MOVE WR-ADR(4) TO WT-TX
               PERFORM PLZL-ORT
               MOVE WR-ADR(3) TO EXC-STR.

       G.  WRITE EXC-SATZ.
           GO E.
       W.  PERFORM END-DRU.
           CLOSE EXCELIS.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ***************************************** PLZL und ORT abstellen *
       PLZL-ORT SECTION.
       A.  PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 5
               IF WT-TX(WX:1) = SPACE or LOW-VALUES MOVE WX TO WZ.
           ADD -1 WZ GIVING WX.
           IF WT-TX(1:4) is NUMERIC MOVE WT-TX(1:WX) TO EXC-PLZL.
           ADD 1 TO WZ.
           MOVE WT-TX(WZ:) TO EXC-ORT.
       Z.  EXIT.
      ******************************************************************
       KREDKOPF SECTION.
       A.  IF WH-MOD = 1 PERFORM BREITKOPF GO Z.
           IF WZ-ZEILEN > 62 WRITE DRA-SATZ AFTER PAGE
               MOVE 0 TO WZ-ZEILEN.
           IF WZ-ZEILEN > 0 GO Z.
           IF WM-OPEN = 0 MOVE 2 TO WH-P1
               MOVE 4 TO WH-P2
               IF WM-DRU = 1 MOVE 15 TO WH-P1
               end-if
               PERFORM BEG-DRU.
           IF WM-DRU = 0 MOVE 1 TO WZ-SCHALT.
           MOVE WK-FIRMA TO DRA-STR(1:30).
           MOVE "Kreditorenliste" TO DRA-STR(33:).
           MOVE WH-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRA-STR(65:10).
           MOVE "Seite:" TO DRA-STR(75:).
           ADD 1 TO WZ-SEITE.
           MOVE WZ-SEITE TO WD-SEITE.
           MOVE WD-SEITE TO DRA-STR(82:).
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRA-STR(1:85) PERFORM DRUCK.
           MOVE "Konto-Nr Bezeichnung
      -       "     M-code  KG ZA-Konditonen" TO DRA-STR(1:).
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRA-STR(1:85) PERFORM DRUCK.
       Z.  EXIT.
      ******************************************************************
       BREITKOPF SECTION.
       A.  IF WZ-ZEILEN > 38 WRITE DRA-SATZ AFTER PAGE
               MOVE 0 TO WZ-ZEILEN.
           IF WZ-ZEILEN > 0 GO Z.
           IF WM-OPEN = 0 MOVE 2 TO WH-P1
               MOVE 4 TO WH-P2
               IF WM-DRU = 1 MOVE 12 TO WH-P1
               end-if
               PERFORM BEG-DRU.
           IF WM-DRU = 0 MOVE 1 TO WZ-SCHALT.
           MOVE WK-FIRMA TO DRA-STR(1:30).
           MOVE "Kreditorenliste" TO DRA-STR(33:).
           MOVE WH-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRA-STR(102:10).
           MOVE "Seite:" TO DRA-STR(112:).
           ADD 1 TO WZ-SEITE.
           MOVE WZ-SEITE TO WD-SEITE.
           MOVE WD-SEITE TO DRA-STR(119:).
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRA-STR(1:125).
           PERFORM DRUCK.
           MOVE "Konto-Nr Bezeichnung
      -       "     M-code  KG ZA-Konditonen    Bank-Kto. Bklzl Bank "
      *             12345678 1 1 3,5/ 10/ 30 12345678901 12345
                 TO DRA-STR(1:).
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRA-STR(1:125).
           PERFORM DRUCK.
       Z.  EXIT.
