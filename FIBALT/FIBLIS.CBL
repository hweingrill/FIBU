      $SET LINKCOUNT"480" ANS85 BOUND AUTOLOCK NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     FIBLI.
      ******************************************************************
      *           Umsatzsteuer- und GetrÑnkesteuerhilfliste            *
      ******************************************************************
       ENVIRONMENT    DIVISION.
       CONFIGURATION   SECTION.
       SOURCE-COMPUTER.  PC.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY FIBUSEC.CPY.
           SELECT IOPDATEI  ASSIGN WN-IOPES
                           ORGANIZATION INDEXED, ACCESS DYNAMIC
                           RECORD KEY IOP-KEY
                           ALTERNATE KEY IOP-AKEY DUPLICATES
                           FILE STATUS WF-STATUS.
           SELECT IBUCHZEIL ASSIGN WN-IBZEI
                           ORGANIZATION INDEXED, ACCESS DYNAMIC
                           RECORD KEY IBZ-KEY
                           ALTERNATE KEY IS IBZ-JKEY
                           FILE STATUS WF-STATUS.
           SELECT IAUSZIFF  ASSIGN WN-IAUSZ
                           ORGANIZATION INDEXED, ACCESS DYNAMIC
                           RECORD KEY IAZ-KEY
                           FILE STATUS WF-STATUS.
           SELECT FQDAT    ASSIGN TO "FISORT.DAT"
                           ORGANIZATION INDEXED, ACCESS DYNAMIC
                           RECORD KEY FQ-KEY
                           FILE STATUS WF-STATUS.
           SELECT DRUCKER  ASSIGN TO PRINTER "FQLIST.LST"
                           ORGANIZATION RECORD SEQUENTIAL
                           FILE STATUS WF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       COPY FIBUFD.CPY.
      ************************************* Journal- u. Buchungszeilen *
       FD  IBUCHZEIL     external      RECORD VARYING FROM 68 TO 115
                                       DEPENDING ON WZ-BUCH.
       01  IBZ-SATZ.
           03  IBZ-KEY.
               05 IBZ-KTONR             PIC 9(6)      COMP.
               05 IBZ-BUDAT             PIC 9(8)      COMP.
               05 IBZ-LFD               PIC 9999      COMP.
           03  IBZ-JKEY.
               05 IBZ-USE               PIC 99        COMP.
               05 IBZ-SEITE             PIC 9(7)      COMP-X.
           03  IBZ-BELNR                PIC 9(6)      COMP.
           03  IBZ-BELDAT               PIC 9(8)      COMP.
           03  IBZ-REFNR                PIC 9(7)      COMP.
           03  IBZ-SYM                  PIC 99        COMP.
           03  IBZ-GGKTO                PIC 9(6)      COMP.
           03  IBZ-SH                   PIC 9         COMP.
           03  IBZ-BUBET                PIC S9(9)V99  COMP.
           03  IBZ-Z                    PIC 9         COMP.
           03  IBZ-VM                   PIC 9         COMP.
           03  IBZ-MW                   PIC S9(7)V99  COMP.
           03  IBZ-U1                   PIC 99        COMP.
           03  IBZ-B1                   PIC S9(9)V99  COMP.
           03  IBZ-U2                   PIC 99        COMP.
           03  IBZ-B2                   PIC S9(9)V99  COMP.
           03  IBZ-KSTNR                PIC 9(5)      COMP.
           03  IBZ-KOND                 PIC 9(13)     COMP.
           03  IBZ-OPLFD                PIC 9999      COMP.
           03  IBZ-FWBET                PIC S9(9)V99  COMP.
           03  IBZ-KURS                 PIC 9(4)V9(7) COMP.
           03  IBZ-FSYM                 PIC XXX.
           03  IBZ-TX                   PIC X(40).
      *----------------------------------------> Jahresabschlu·summen <-
       01  IBZ-ASATZ.
           03  FILLER                   PIC X(52).
           03  IBZ-SOLL                 PIC S9(9)V99  COMP.
           03  IBZ-HABEN                PIC S9(9)V99  COMP.
           03  IBZ-EBIL                 PIC S9(9)V99  COMP.
      ***************************************************** OP - Datei *
       FD  IOPDATEI      external       LABEL RECORD STANDARD.
       01  IOP-SATZ.
           03  IOP-AKEY.
               05 IOP-SA                PIC 99         COMP.
               05 IOP-KEY.
                  07 IOP-DK             PIC 99         COMP.
                  07 IOP-KTONR          PIC 9(6)       COMP.
                  07 IOP-VALDAT         PIC 9(8)       COMP.
                  07 IOP-LFD            PIC 9999       COMP.
           03  IOP-REF                  PIC 9(7)       COMP.
           03  IOP-RENUM                PIC 9(7)       COMP.
           03  IOP-REDAT                PIC 9(8)       COMP.
           03  IOP-SYM                  PIC 99         COMP.
           03  IOP-ZUSTAND              PIC 99         COMP.
           03  IOP-KOND                 PIC 9(13)      COMP.
           03  IOP-MW                   PIC S9(7)V99   COMP.
           03  IOP-U1                   PIC 99         COMP.
           03  IOP-B1                   PIC S9(9)V99   COMP.
           03  IOP-U2                   PIC 99         COMP.
           03  IOP-B2                   PIC S9(9)V99   COMP.
           03  IOP-REBETRAG             PIC S9(9)V99   COMP.
           03  IOP-ZABETRAG             PIC S9(9)V99   COMP.
           03  IOP-SKTOBAS              PIC S9(9)V99   COMP.
           03  IOP-ZADAT                PIC 9(6)       COMP.
           03  IOP-ZPBET                PIC S9(9)V99   COMP.
           03  IOP-ZPSKTO               PIC S9(7)V99   COMP.
           03  IOP-KURS                 PIC 9(4)V9(7)  COMP.
           03  IOP-FSYM                 PIC XXX.              *> WÑhrung
           03  IOP-MSTUFE               PIC 99         COMP.
           03  IOP-MAHNDAT              PIC 9(6)       COMP.
           03  IOP-ZESS                 PIC 99         COMP.
           03  IOP-FREMDNR              PIC X(15).
      ********************************************* Auszifferungsdatei *
       FD  IAUSZIFF      external      LABEL RECORD STANDARD
                                       RECORD VARYING FROM 20 TO 1060
                                       DEPENDING ON WZ-AUSZ.
       01  IAZ-SATZ.
           03  IAZ-KEY.
               05 IAZ-SA                PIC 99        COMP.
               05 IAZ-TKEY.
                  07 IAZ-KTONR          PIC 9(6)      COMP.
                  07 IAZ-BUDAT          PIC 9(8)      COMP.
                  07 IAZ-LFD            PIC 9999      COMP.
           03  IAZ-RETAB       OCCURS 50.
               05 IAZ-OPNR              PIC 9(7)      COMP.
               05 IAZ-OPSYM             PIC 99        COMP.
               05 IAZ-OPBET             PIC S9(8)V99  COMP.
               05 IAZ-OPSKTO            PIC S9(7)V99  COMP.
               05 IAZ-OPFSYM            PIC XXX.
               05 IAZ-KURS              PIC 9(5)V9(7) COMP.
      ******************************************************************
       FD  FQDAT                       LABEL RECORD STANDARD.
       01  FQ-SATZ.
           03  FQ-KEY                  PIC 999999.
           03  FQ-NIX                  PIC X.
           03  FQ-DATUM                PIC 9(8).
      ******************************************************************
       FD  DRUCKER                     LABEL RECORD STANDARD.
       01  DRA-SATZ.
           03  FILLER                  PIC XXXX.
           03  DRA-STR.
               05 DRA-KTONR            PIC ZZZZZ,Z-.
               05 DRA-BEZ              PIC X(26).
               05 DRA-SALDO            PIC ZZZ.ZZZ.ZZZ,ZZ-.
               05 DRA-UST              PIC ZZ9,99.
               05 DRA-PZ               PIC XX.
               05 DRA-SOLL             PIC ZZZ.ZZZ.ZZZ,ZZ-.
               05 DRA-HABEN            PIC ZZZ.ZZZ.ZZZ,ZZ-.
           03  FILLER                  PIC X(8).
       01  DRG-SATZ.
           03  FILLER                  PIC XXXX.
           03  DRG-STR.
               05 DRG-KTONR            PIC ZZZZZ,Z-.
               05 DRG-BEZ              PIC X(26).
               05 DRG-SALDO            PIC ZZZ.ZZZ.ZZZ,ZZ-.
               05 DRG-GST              PIC ZZ9,9999.
               05 DRG-PZ               PIC XX.
               05 DRG-SOLL             PIC ZZZ.ZZZ.ZZZ,ZZ-.
               05 DRG-RESERVE          PIC ZZZ.ZZZ.ZZZ,Z.
       01  DRU-SATZ.
           03  FILLER                  PIC XXXX.
           03  DRU-STR.
               05 DRU-KTONR            PIC ZZ.ZZZ,Z-.
               05 DRU-GGKTO            PIC ZZ.ZZZ,Z-.
               05 DRU-DATUM            PIC X(9).
               05 DRU-SYM              PIC X(4).
               05 DRU-BETRAG           PIC ZZZ.ZZZ.ZZZ,ZZ-.
               05 DRU-VST              PIC ZZ.ZZZ.ZZZ,ZZ-.
               05 DRU-UST              PIC ZZ9-.
               05 DRU-BMG              PIC ZZZ.ZZZ.ZZZ,ZZ-.
               05 DRU-MWST             PIC ZZ.ZZZ.ZZZ,ZZ-.
       01  DRW-SATZ.
           03  FILLER                  PIC XXXX.
           03  DRW-STR.
               05 DRW-KTONR            PIC ZZZZZ,Z-.
               05 DRW-BEZ              PIC X(62).
               05 DRW-SUMME            PIC ZZZ.ZZZ.ZZZ,ZZ-.
       01  DRZ-SATZ.
           03  FILLER                  PIC XXX.
           03  DRZ-BELNR               PIC ZZZZ9-.
           03  DRZ-BELDAT              PIC Z(8).
      ******************************************************************
       WORKING-STORAGE SECTION.
       01  WH-CALL.
           03  WL-CA                   PIC 99.
           03  WL-ECK                  PIC 9999.
           03  FILLER REDEFINES WL-ECK.
               05  WL-VL               PIC 99.
               05  WL-VP               PIC 99.
           03  WL-GROSS                PIC 9999.
           03  FILLER REDEFINES WL-GROSS.
               05  WL-AZ               PIC 99.
               05  WL-SZ               PIC 99.
           03  WL-KO                   PIC 99.
           03  WL-MA                   PIC 9.
           03  WL-ATTR                 PIC XX.
       COPY WHCREG.CPY.
       01  WH-SONS.
           03  WN-PFAD                 PIC X(30).
           03  WN-IOPES                PIC X(40).
           03  WN-IBZEI                PIC X(40).
           03  WN-IAUSZ                PIC X(40).
           03  WH-DRA                  PIC X(128).
           03  WT-TAB           OCCURS 90.
               05 WT-SUM               PIC S9(9)V99  COMP.
               05 WT-UKTO              PIC 9(6)      COMP.
               05 WT-UKZ               PIC 99        COMP.
               05 WT-GPZ               PIC 99V9999   COMP.
           03  WK-DSTG.
               05 WK-STG               PIC X(10)     OCCURS 10.
           03  WH-P1                   PIC 99        COMP.
           03  WH-P2                   PIC 99        COMP.
           03  WB                      PIC 99        COMP.
           03  WR-AR                   PIC 99        COMP.
           03  WR-GA                   PIC 99        COMP.
           03  WH-KAT                  PIC 9         COMP.
           03  WH-UST                  PIC 99V99     COMP.
           03  WH-GST                  PIC 99V9999   COMP.
           03  WH-BET                  PIC S9(12)V99  COMP.
           03  WS-MWST                 PIC S9(9)V99  COMP.
           03  WS-SUM                  PIC S9(9)V99  COMP.
           03  WZ-SUM                  PIC S9(9)V99  COMP.
           03  WM-SOLL                 PIC S9(9)V99  COMP.
           03  WM-HABEN                PIC S9(9)V99  COMP.
           03  WK-NETTO                PIC S9(9)V99  COMP.
           03  WK-BUBET                PIC S9(9)V99  COMP.
           03  WK-SKTO                 PIC S9(9)V99  COMP.
           03  WK-UDIF                 PIC S9(7)V99  COMP.
           03  WK-BET                  PIC S9(9)V99  COMP.
           03  WH-SIZE                 PIC 99        COMP   VALUE 62.
           03  WX-PRNO                 PIC 99        COMP-X.
           03  WX-PRSTAT               PIC 99        COMP-X.
           03  WZ-SEITE                PIC 99        COMP   VALUE ZERO.
           03  WZ-SCHALT               PIC 99        COMP   VALUE ZERO.
           03  WZ-ZEILEN               PIC 99        COMP   VALUE ZERO.
           03  WH-DRUNAM               PIC X(12)     VALUE   "LPT1:".
           03  WM-OPEN                 PIC 9         COMP   VALUE ZERO.
           03  WD-KTO                  PIC ZZ.ZZ9,9.
           03  WD-KZ                   PIC 9.
           03  WD-UST                  PIC ZZ,99.
           03  WD-GST                  PIC ZZ,9999.
           03  WD-PER                  PIC 9999/99.
           03  WV-PER                  PIC 9(6)      COMP.
           03  WV-ABDAT                PIC 9(8)      COMP.
           03  WV-BISDAT               PIC 9(8)      COMP.
           03  WV-KTONR                PIC 9(6)      COMP.
           03  WV-BELNR                PIC 9(8)      COMP.
       COPY FIBUEXT.CPY.
       COPY FIBUDECL.CPY.
       END DECLARATIVES.
      *****************************************************************
       STEUER SECTION.
       A.  MOVE WL-CALL TO WH-CALL.
           MOVE WL-CREG TO WH-CREG.
           EVALUATE WL-CA
               WHEN 10 PERFORM AR-SORT
               WHEN 20 PERFORM EINLAG.
           MOVE WH-CREG TO WL-CREG.
       Z.  EXIT PROGRAM.
      ******************************************************************
       BESETZT SECTION.
       A.  DISPLAY "Record - besetzt" AT 2401.
       Z.  EXIT.
      ******************************************************************
       WEITER SECTION.
       A.  DISPLAY " weiter mit <ret>: " with highlight AT 0000.
           MOVE SPACE TO WH-X.
           ACCEPT WH-X AT 0000.
           CALL "CAUP" using "1324012480000" WH-CREG.
       Z.  EXIT.
      *****************************************************************
       DATDREH SECTION.
       A.  MOVE WC-TAG  TO WZ-TAG VDU-JAHR.
           MOVE WC-MONAT TO WZ-MONAT VDU-MONAT.
           MOVE WC-JAHR TO WZ-JAHR VDU-TAG.
       Z.  EXIT.
      ************************************************* ob Drucker ok *
       DRU-OK SECTION.
       A.  IF WH-DRUNAM(1:3) not = "LPT" GO Z.
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
           IF WF-STATUS = 27 GO C.
           MOVE SPACE TO DRA-SATZ.
           ADD WZ-SCHALT TO WZ-ZEILEN.
           MOVE 1 TO WZ-SCHALT.
       Z.  EXIT.
      ****************************** Druckerrueckstellung auf 10/Zoll *
       END-DRU SECTION.
       A.  IF WM-OPEN = 0 GO Z.
           MOVE X"1B210000" TO DRA-SATZ(1:4).
       B.  WRITE DRA-SATZ BEFORE PAGE.
           IF WF-STATUS = 27 GO B.
           MOVE SPACE TO DRA-SATZ.
           CLOSE DRUCKER.
           MOVE 0 TO WM-OPEN.
       Z.  EXIT.
      ******************************************************************
       BEG-DRU SECTION.
       A.  PERFORM DRU-OK.
           IF WM-OPEN > 0 GO Z.
           MOVE 1 TO WM-OPEN.
           OPEN EXTEND DRUCKER.
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
      *-----------------------------------> Steuerzeichen LASER laden <-
      *                                  11  *> quer A4 /10" 6 Zeilen <-
           MOVE "&l26a6d1O(s0p10h0b0s3T" TO WE-STG(1).
      *                                  12  *> quer A4 /12" 6 Zeilen <-
           MOVE "&l26a6d1O(s0p12h0b0s3T" TO WE-STG(2).
      *                                  13  *> quer A4 /16" 6 Zeilen <-
           MOVE "&l26a6d1O(s0p14.6h0b0s3T" TO WE-STG(3).
      *                                  14  *> hoch A4 /10" 6 Zeilen <-
           MOVE "&l26a6d0O(s0p10h0b0s3T" TO WE-STG(4).
      *                                  15  *> hoch A4 /12" 6 Zeilen <-
           MOVE "&l26a6d0O(s0p12h0b0s3T" TO WE-STG(5).
      *                                  16  *> hoch A4 /16" 6 Zeilen <-
           MOVE "&l26a6d0O(s0p14.6h0b0s3T" TO WE-STG(6).
      *                                  17  *> hoch A4 /16" 6 Zeilen <-
           MOVE "&l26a6d0O(s0p16.6h0b0s6T" TO WE-STG(7).
      *                                  18  *> hoch A4 /12" 6 Zeilen <-
           MOVE "&l3a6d0O(s0p12h0b0s3T" TO WE-STG(8).
      *    &la: Format, c: 6 Zeilen, 1/0O: quer/hoch
      *    (sp: Abstand, h: Zeich./Zoll b: StÑrke s: Schrift T: Schrift
       Z.  EXIT.
      ******************************************************************
       AR-SORT SECTION.
       A.  MOVE 995002 TO BZ-KTONR DE-KTONR.
           MOVE 20010301 TO BZ-BUDAT.
           move 0 to WV-BELNR.
           MOVE 0 TO BZ-LFD.
           open output fqdat.
           START BUCHZEIL KEY NOT < BZ-KEY INVALID GO Z.
       E.  READ BUCHZEIL NEXT AT END GO Z.
           IF DE-KTONR not = BZ-KTONR GO Q.
           IF BZ-SYM > 97 GO E.
           if bz-sym not = 00 and bz-sym not = 10 go e.
           DISPLAY BZ-BELNR AT 2020.
           IF BZ-BELNR < 90000 GO E.
           IF BZ-BELNR < 94000 ADD 100000 TO BZ-BELNR.
           MOVE BZ-BELNR TO FQ-KEY.
           MOVE BZ-BELDAT TO FQ-DATUM.
           MOVE ";" TO FQ-NIX.
           WRITE FQ-SATZ.
           GO E.
       Q.  CLOSE FQDAT.
           OPEN I-O FQDAT.
           OPEN OUTPUT DRUCKER.
           MOVE SPACE TO DRA-SATZ.
       R.  READ FQDAT NEXT AT END GO X.
           IF WV-BELNR = 0 MOVE FQ-KEY TO WV-BELNR GO R.
       S.  ADD 1 TO WV-BELNR.
           IF WV-BELNR = 99000 MOVE 190001 TO WV-BELNR.
           IF WV-BELNR = FQ-KEY GO R.
           MOVE SPACE TO DRA-SATZ.
           MOVE WV-BELNR TO DRZ-BELNR.
           MOVE FQ-DATUM TO DRZ-BELDAT.
           WRITE DRA-SATZ AFTER 1.
           GO S.
       X.  CLOSE DRUCKER.
           STOP RUN.
       Z.  EXIT.
      ************************************************* Fibu einlagern *
       EINLAG SECTION.
       A.  CALL "CAUP" USING "270505077000011" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Fibu einlagern " with highlight AT VDU-LP.
           CALL "CADRUCK" USING "99OPEN" WH-CREG.
           MOVE "C:\FIOLD\" TO WN-PFAD.
           PERFORM VARYING WX FROM 30 BY -1 UNTIL WX = 1
               or WT-TX(WX:4) = "\FIB" continue.
           PERFORM VARYING WY FROM 30 BY -1 UNTIL WY = 1
               or WN-PFAD(WY:1) = "\" continue.
           MOVE WT-TX(WX:) TO WN-PFAD(WY:).
           PERFORM VARYING WY FROM 30 BY -1 UNTIL WY = 1
               or WN-PFAD(WY:1) not = " " continue.
           ADD 1 TO WY.
           MOVE "\" TO WN-PFAD(WY:1).
       C.  ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "Pfad-Einlagerung: " AT VDU-LP WN-PFAD
                with highlight foreground-color 6.
           MOVE WN-PFAD TO WT-TX.
           CALL "CAUP" USING "1202210125" WH-CREG.
           IF ESC GO X.
           IF not RET GO C.
           MOVE WT-TX TO WN-PFAD WN-IOPES WN-IBZEI WN-IAUSZ.
           DISPLAY WN-PFAD with highlight foreground-color 6 AT VDU-LP.
           PERFORM VARYING WX FROM 40 BY -1 UNTIL WX = 1
               or WN-IBZEI(WX:1) = "\" continue.
           ADD 1 TO WX.
           MOVE "FIBUZEIL.DAT" TO WN-IBZEI(WX:).
           MOVE "FIBUOPES.DAT" TO WN-IOPES(WX:).
           MOVE "FIBUAUSZ.DAT" TO WN-IAUSZ(WX:).
           OPEN INPUT IBUCHZEIL.
           IF WF-STATUS not = "00" GO X.
           OPEN INPUT IOPDATEI.
           IF WF-STATUS not = "00" GO X.
           OPEN INPUT IAUSZIFF.
           IF WF-STATUS not = "00" GO X.
       E.  DISPLAY "<esc>= Abbruch, <ret>= Start < >" AT 2301.
           CALL "CAUP" USING "0023310000" WH-CREG.
           IF ESC GO X.
           IF not RET GO E.
      *---------------------------------> eigentliche RÅckeinlagerung <-
           MOVE LOW-VALUE TO IAZ-SATZ.
           START IAUSZIFF KEY > IAZ-KEY INVALID STOP RUN.
       G.  READ IAUSZIFF NEXT AT END GO I.
           MOVE IAZ-SATZ TO AZ-SATZ.
           IF IAZ-BUDAT > 20000331 GO G.
           WRITE AZ-SATZ INVALID GO G.
           GO G.
      *----------------------------------------------------> Op-Datei <-
       I.  MOVE LOW-VALUE TO IOP-SATZ.
           START IOPDATEI KEY > IOP-KEY INVALID STOP RUN.
       J.  READ IOPDATEI NEXT AT END GO L.
           MOVE IOP-SATZ TO OP-SATZ.
           IF IOP-REDAT > 20000331 GO J.
           WRITE OP-SATZ INVALID GO J.
           GO J.
      *----------------------------------------------------> Buchzeil <-
       L.  MOVE LOW-VALUE TO IBZ-SATZ.
           START IBUCHZEIL KEY > IBZ-KEY INVALID STOP RUN.
       N.  READ IBUCHZEIL NEXT AT END GO X.
           MOVE IBZ-SATZ TO BZ-SATZ.
           IF IBZ-BUDAT > 20000331 GO N.
           WRITE BZ-SATZ INVALID GO N.
           GO N.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
