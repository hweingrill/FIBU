      $SET LINKCOUNT"240" ANS85 BOUND AUTOLOCK NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     FIBGUST.
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
           SELECT DRUCKER  ASSIGN TO PRINTER WH-DRUNAM
                           ORGANIZATION RECORD SEQUENTIAL
                           FILE STATUS WF-STATUS.
           SELECT VST-DRU  ASSIGN TO PRINTER "VST-GRP.LST"
                           ORGANIZATION RECORD SEQUENTIAL
                           FILE STATUS WF-STATUS.
           SELECT USTEXL   ASSIGN TO PRINTER "FIUSTEXL.TXT"
                           ORGANIZATION RECORD SEQUENTIAL
                           FILE STATUS WF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       COPY FIBUFD.CPY.
      ******************************************************************
       FD  USTEXL                      LABEL RECORD STANDARD.
       01  UE-SATZ.
           03  UE-KTONR            PIC ZZ.ZZZ,Z-.
           03  UE-GGKTO            PIC ZZ.ZZZ,Z-.
           03  UE-DATUM            PIC X(9).
           03  UE-SYM              PIC X(4).
           03  UE-BETRAG           PIC -ZZZ.ZZZ.ZZZ,ZZ.
           03  UE-VST              PIC -ZZ.ZZZ.ZZZ,ZZ.
           03  UE-UST              PIC ZZ9-.
           03  UE-BMG              PIC -ZZZ.ZZZ.ZZZ,ZZ.
           03  UE-MWST             PIC -ZZ.ZZZ.ZZZ,ZZ.
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
      ******************************************************************
       FD  VST-DRU                     LABEL RECORD STANDARD.
       01  VST-SATZ.
           03  FILLER                  PIC XXXX.
           03  VST-STR.
               05 VST-KTONR            PIC ZZZZZ,Z-.
               05 VST-BEZ              PIC X(26).
               05 VST-BUDAT            PIC X(9).
               05 VST-BK               PIC XX.
               05 VST-UMSATZ           PIC ZZ.ZZZ.ZZZ,ZZ-.
               05 VST-STEUER           PIC Z.ZZZ.ZZZ,ZZ-.
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
           03  WH-DRA                  PIC X(128).
           03  WH-UST-27               PIC S9(9)V99  COMP.
           03  WH-UST-28               PIC S9(9)V99  COMP.
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
           03  WV-OPEN                 PIC 9         COMP   VALUE ZERO.
           03  WV-SCHALT               PIC 99        COMP   VALUE ZERO.
           03  WV-ZEILEN               PIC 99        COMP   VALUE ZERO.
           03  WD-KTO                  PIC ZZ.ZZ9,9.
           03  WD-KZ                   PIC 9.
           03  WD-UST                  PIC ZZ,99.
           03  WD-GST                  PIC ZZ,9999.
           03  WD-PER                  PIC 9999/99.
           03  WV-PER                  PIC 9(6)      COMP.
           03  WV-ABDAT                PIC 9(8)      COMP.
           03  WV-BISDAT               PIC 9(8)      COMP.
           03  WV-KTONR                PIC 9(6)      COMP.
           03  WS-UMSATZ               PIC S9(8)V99  COMP   OCCURS 2.
           03  WS-STEUER               PIC S9(8)V99  COMP   OCCURS 2.
       COPY FIBUEXT.CPY.
       COPY FIBUDECL.CPY.
       END DECLARATIVES.
      *****************************************************************
       STEUER SECTION.
       A.  MOVE WL-CALL TO WH-CALL.
           MOVE WL-CREG TO WH-CREG.
           PERFORM LADE-DRU.
           EVALUATE WL-CA
               WHEN 10 PERFORM USTPROB
               WHEN 20 PERFORM USTLIST
               WHEN 30 PERFORM WARENUMSATZ
               WHEN 40 PERFORM IG-ERWERB.
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
      *********************************************** Ust-Konten laden *
       LADEUST SECTION.
       A.  PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 90
               MOVE 0 TO WT-SUM(WX) WT-UKTO(WX) WT-UKZ(WX).
           MOVE 69 TO WH-KEY.
       G.  READ KONSTANT NO LOCK INVALID MOVE LOW-VALUES TO KO-USATZ.
           IF ZUGRIF PERFORM BESETZT GO G.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 40
               MOVE KO-UKTO(WX) TO WT-UKTO(WX)
               MOVE KO-UKZ(WX) TO WT-UKZ(WX).
           MOVE 67 TO WH-KEY.
       H.  READ KONSTANT NO LOCK INVALID MOVE LOW-VALUES TO KO-USATZ.
           IF ZUGRIF PERFORM BESETZT GO H.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 40
               MOVE KO-UKTO(WX) TO WT-UKTO(WX + 40)
               MOVE KO-UKZ(WX) TO WT-UKZ(WX + 40).
       Z.  EXIT.
      ************************************************* Ust-Verprobung *
       USTPROB SECTION.
       A.  CALL "CAUP" USING "0707100560000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Ust - Ermittlung " with highlight AT VDU-LP.
           MOVE "USTSUMM.LST" TO WH-DRUNAM.
           PERFORM LADE-DRU.
       C.  CALL "CAUP" USING "16CLOFEN" WH-CREG.
           ADD 240 VDU-ECK GIVING VDU-LP.
           DISPLAY WH-DRUNAM with highlight AT VDU-LP.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "Periode:" AT VDU-LP.
           DISPLAY "<esc>= Abbruch, <ret>= Periode (JJMM oder JJJJMM)"
               AT 2301.
           CALL "CAUP" USING "1003126006" WH-CREG.
           IF ESC GO X.
           IF NOT RET GO C.
           IF WH-NUM = 0 GO C.
           MOVE WH-NUM TO WV-PER.
           IF WV-PER < 4913 ADD 200000 TO WV-PER.
           IF WV-PER < 9913 ADD 190000 TO WV-PER.
           MOVE WV-PER TO WD-PER.
           DISPLAY WD-PER with highlight AT VDU-LP.
           COMPUTE WV-ABDAT = WV-PER * 100 + 1.
           ADD 30 WV-ABDAT GIVING WV-BISDAT.
       G.  MOVE WV-BISDAT TO WZ-DATUM.
           CALL "CAUP" USING "05DATPRF" WH-CREG.
           IF WX-DATUM = 0 ADD -1 TO WV-BISDAT
                           GO G.
      *------------------------------> Ust-konten laden und summieren <-
           PERFORM LADEUST.
      *--------------------------------------> zwingende Konten laden <-
           MOVE 9 TO WH-KEY.
       K.  READ KONSTANT NO LOCK INVALID GO Z.
           IF ZUGRIF PERFORM BESETZT GO K.
           MOVE KO-KTONR(1) TO WT-UKTO(90).           *> Vorsteuer
           MOVE KO-KTONR(2) TO WT-UKTO(89).           *> Mehrwertsteuer
      *---------------------------> Skto.Aufwand Konten UST Kz. 1 - 6 <-
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 6
               PERFORM VARYING WY FROM 83 BY 1 UNTIL WY > 89
                   IF WT-UKTO(WY) = KO-KTONR(WX + 4) MOVE 90 TO WY
                   else IF WT-UKTO(WY) = 0
                            MOVE KO-KTONR(WX + 4) TO WT-UKTO(WY)
                            MOVE WX TO WT-UKZ(WY)
                            MOVE 90 TO WY.
      *-----------------------------------------> Konten durchrechnen <-
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 90
               IF WT-UKTO(WX) NOT = 0 PERFORM SUMRECH
                   SUBTRACT WM-SOLL FROM WM-HABEN GIVING WT-SUM(WX).
           PERFORM UST-DRUCK.
      *----------------------------------------------> GetrÑnkeabgabe <-
           MOVE 0 TO WZ-ZEILEN WZ-SCHALT.
           MOVE 68 TO WH-KEY.
       M.  READ KONSTANT NO LOCK INVALID GO X.
           IF ZUGRIF PERFORM BESETZT GO M.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 50
               MOVE 0 TO WT-SUM(WX) WT-UKTO(WX) WT-GPZ(WX)
               IF WX < 32 MOVE KO-GKTO(WX) TO WT-UKTO(WX)
                          MOVE KO-GPZ(WX) TO WT-GPZ(WX)
                          MOVE 0 TO WH-KEY.
           IF WH-KEY = 68 GO X.
      *-----------------------------------------> Konten durchrechnen <-
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 50
               IF WT-UKTO(WX) NOT = 0 PERFORM SUMRECH
                   SUBTRACT WM-SOLL FROM WM-HABEN GIVING WT-SUM(WX).
           PERFORM GST-DRUCK.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           MOVE 9 TO WH-PG.
       Z.  EXIT.
      **************************************** Ust-Kontosummen rechnen *
       SUMRECH SECTION.
       A.  MOVE WT-UKTO(WX) TO SA-KTONR.
       C.  READ SACHBUCH NO LOCK INVALID MOVE 0 TO BZ-BUDAT GO Z.
           IF ZUGRIF PERFORM BESETZT GO C.
           MOVE SA-KTONR TO BZ-KTONR.
           ADD 403 VDU-ECK GIVING VDU-LP.
           DISPLAY SA-KTONR with highlight AT VDU-LP " " SA-TX(1).
           MOVE 0 TO BZ-LFD WM-SOLL WM-HABEN WB.
           MOVE WV-ABDAT TO BZ-BUDAT.
           START BUCHZEIL KEY NOT < BZ-KEY INVALID GO Z.
       E.  READ BUCHZEIL NEXT AT END GO Z.
           IF ZUGRIF PERFORM BESETZT GO E.
           IF SA-KTONR NOT = BZ-KTONR GO Z.
           IF BZ-BUDAT > WV-BISDAT GO Z.
           IF BZ-SYM > 97 GO E.
           IF WX > 87 and BZ-SYM not = 19 and BZ-GGKTO not = 99999 GO E.
           IF SA-VSTKZ = 27 or 28;
               if bz-budat > 20080000
                   move sa-vstkz to bz-vm
                   if bz-mw = 0 compute bz-mw = bz-fwbet - bz-bubet
                   end-if
               rewrite bz-satz
               end-rewrite
               end-if
               IF BZ-SH = 0 ADD BZ-MW TO WM-SOLL
                       else ADD BZ-MW TO WM-HABEN
               end-if
               GO E.
           IF BZ-SH = 0 ADD BZ-BUBET TO WM-SOLL
                  else ADD BZ-BUBET TO WM-HABEN.
           GO E.
       Z.  EXIT.
      ************************************************ Sachkonto lesen *
       READ-SACH SECTION.
       A.  READ SACHBUCH NO LOCK INVALID MOVE "-- fehlt --" TO SA-BEZ.
           IF ZUGRIF PERFORM BESETZT GO A.
       Z.  EXIT.
      *********************************************** Druck Ust-Summen *
       UST-DRUCK SECTION.
       A.  PERFORM UST-KOPF.
           MOVE 0 TO WS-SUM WS-MWST WZ-SUM.
           PERFORM VARYING WY FROM 1 BY 1 UNTIL WY > 6
               MOVE 0 TO WR
               MOVE SPACE TO WH-DRA
               MOVE WT-UST(WY) TO WD-UST WH-UST
               PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 80
                   IF WT-UKZ(WX) = WY MOVE 1 TO WR
                       MOVE WT-UKTO(WX) TO SA-KTONR WD-KZ
                       PERFORM READ-SACH
                       DIVIDE 10 INTO WT-UKTO(WX) GIVING DRA-KTONR
                       MOVE SA-TX(1) TO DRA-BEZ
                       MOVE WT-SUM(WX) TO WH-WERT
                       MOVE WH-WERT TO DRA-SALDO
                       IF WD-KZ = 1 MOVE DRA-SATZ TO WH-DRA
                           MOVE SPACE TO DRA-SATZ
                       else PERFORM U-DRUCK
                            ADD WH-WERT TO WS-SUM
               end-perform
               IF WR = 1 MOVE ALL "ƒ" TO DRA-STR
                   PERFORM U-DRUCK
                   MOVE "Entgelte Ust-Kz.:  " TO DRA-STR
                   MOVE WY TO WD-KZ
                   MOVE WD-KZ TO DRA-STR(19:1)
                   MOVE " / " TO DRA-STR(20:3)
                   MOVE WD-UST TO DRA-STR(23:5)
                   MOVE "%" TO DRA-STR(29:2)
                   MOVE WS-SUM TO DRA-SALDO
                   MOVE WH-UST TO DRA-UST
                   MOVE "%" TO DRA-PZ
                   COMPUTE WH-BET = WS-SUM * WH-UST / 100
                   MOVE WH-BET TO DRA-SOLL
                   ADD WH-BET TO WS-MWST
                   ADD WS-SUM TO WZ-SUM
                   PERFORM U-DRUCK
                   MOVE 0 TO WS-SUM
                   MOVE WH-DRA TO DRA-SATZ
                   PERFORM U-DRUCK
                   MOVE 3 TO WZ-SCHALT.
           MOVE 1 TO WZ-SCHALT.
           MOVE "Summe Entgelte..:  " TO DRA-STR.
           MOVE WZ-SUM TO DRA-SALDO.
           PERFORM U-DRUCK.
           IF WZ-ZEILEN > 56 MOVE 70 TO WZ-SCHALT
               PERFORM U-DRUCK.
           MOVE 2 TO WZ-SCHALT.
           MOVE "errechnete Mehrwertsteuer" TO DRA-STR(10:).
           MOVE WS-MWST TO DRA-SOLL.
           PERFORM U-DRUCK.
           MOVE 2 TO WZ-SCHALT.
           MOVE "gebuchte Mehrwertsteuer" TO DRA-STR(10:).
           MOVE WT-SUM(89) TO DRA-SOLL.
           SUBTRACT WT-SUM(89) FROM WS-MWST GIVING DRA-HABEN WH-WERT.
           PERFORM U-DRUCK.
           MOVE 0 TO WH-UST-27.
           PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 89
               IF WT-UKZ(WZ) = 27 ADD WT-SUM(WZ) TO WH-UST-27.
           MULTIPLY -1 BY WH-UST-27.
           ADD WH-UST-27 TO WT-SUM(90).
           COMPUTE WS-MWST = WS-MWST - WH-UST-27.
           MOVE 0 TO WH-UST-28.
           PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 89
               IF WT-UKZ(WZ) = 28 ADD WT-SUM(WZ) TO WH-UST-28
           MULTIPLY -1 BY WH-UST-28.
           ADD WH-UST-28 TO WT-SUM(90).
           COMPUTE WS-MWST = WS-MWST - WH-UST-28.
           MOVE "gebuchte Vorsteuer Kz. 60" TO DRA-STR(10:).
           MULTIPLY -1 BY WT-SUM(90).
           MOVE WT-SUM(90) TO DRA-SOLL.
           PERFORM U-DRUCK.
           MOVE "gebuchte Vorsteuer Kz. 27" TO DRA-STR(10:).
           MOVE WH-UST-27 TO DRA-SOLL.
           PERFORM U-DRUCK.
           MOVE "gebuchte Vorsteuer Kz. 28" TO DRA-STR(10:).
           MOVE WH-UST-28 TO DRA-SOLL.
           PERFORM U-DRUCK.
           MOVE 2 TO WZ-SCHALT.
           MOVE WM-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRA-STR.
           MOVE "Z a h l l a s t" TO DRA-STR(10:).
           COMPUTE WS-MWST = WS-MWST - WT-SUM(90).
           MOVE WS-MWST TO DRA-SOLL.
           PERFORM U-DRUCK.
           PERFORM END-DRU.
       Z.  EXIT.
      ******************************************************************
       U-DRUCK SECTION.
       A.  MOVE DRA-SATZ TO WT-ADR.
           MOVE SPACE TO DRA-SATZ.
           PERFORM UST-KOPF.
           MOVE WT-ADR TO DRA-SATZ.
           PERFORM DRUCK.
       Z.  EXIT.
      ******************************************************************
       UST-KOPF SECTION.
       A.  IF WZ-ZEILEN > WH-SIZE WRITE DRA-SATZ AFTER PAGE
               MOVE 0 TO WZ-ZEILEN.
           IF WZ-ZEILEN > 0 GO Z.
           MOVE 2 TO WH-P1.
           IF WM-KO = 0 MOVE 5 TO WH-P2
                        MOVE 40 TO WH-SIZE
                        IF WM-DRU = 1 MOVE 12 TO WH-P1         *> Laser
                            MOVE 0 TO WZ-SCHALT
                        end-if
                   else MOVE 4 TO WH-P2
                        MOVE 62 TO WH-SIZE
                        IF WM-DRU = 1 MOVE 15 TO WH-P1         *> Laser
                            MOVE 0 TO WZ-SCHALT.
           PERFORM BEG-DRU.
           ADD 1 TO WZ-SEITE.
           IF WM-DRU = 0 MOVE 1 TO WZ-SCHALT.
           MOVE WK-FIRMA TO DRA-STR(1:30).
           MOVE "Umsatzsteuerhilfsliste " TO DRA-STR(43:).
           MOVE WV-ABDAT TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRA-STR(66:9).
           MOVE " - " TO DRA-STR(75:)
           MOVE WV-BISDAT TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRA-STR(79:10).
           PERFORM DRUCK.
           MOVE ALL "ƒƒƒ" TO DRA-STR PERFORM DRUCK.
           MOVE "Kto-Nr. B e z e i c h n u n g             Summen   Ust-
      -       "%     Ust-Betrag      Differenz" TO DRA-STR(1:).
           PERFORM DRUCK.
           MOVE ALL "ƒƒƒ" TO DRA-STR PERFORM DRUCK.
           MOVE 2 TO WZ-SCHALT.
       Z.  EXIT.
      *********************************************** Druck Gst-Summen *
       GST-DRUCK SECTION.
       A.  PERFORM GST-KOPF.
           MOVE 0 TO WS-SUM WS-MWST WZ-SUM.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 32
              IF WT-GPZ(WX) not = 0
                  MOVE WT-GPZ(WX) TO WH-GST DRG-GST
                  MOVE "%" TO DRG-PZ
                  MOVE WT-UKTO(WX) TO SA-KTONR WD-KZ
                  PERFORM READ-SACH
                  DIVIDE 10 INTO WT-UKTO(WX) GIVING DRG-KTONR
                  MOVE SA-TX(1) TO DRG-BEZ
                  MOVE WT-SUM(WX) TO WS-SUM DRG-SALDO
                  COMPUTE WH-BET = WS-SUM * WH-GST / 100
                  ADD WH-BET TO WS-MWST
                  MOVE WH-BET TO DRG-SOLL
                  PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRA-STR PERFORM DRUCK.
           MOVE "Summe GetrÑnkeabgabe:  " TO DRG-STR.
           IF WS-MWST > 0 ADD 0,49 TO WS-MWST
                    else ADD -0,49 TO WS-MWST.
           DIVIDE 100 INTO WS-MWST.
           MULTIPLY 100 BY WS-MWST GIVING DRG-SOLL.
           PERFORM DRUCK.
           MOVE 3 TO WZ-SCHALT.
           MOVE WM-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRG-STR.
           PERFORM DRUCK.
           PERFORM END-DRU.
       Z.  EXIT.
      ******************************************************************
       GST-KOPF SECTION.
       A.  IF WZ-ZEILEN > WH-SIZE WRITE DRA-SATZ AFTER PAGE
               MOVE 0 TO WZ-ZEILEN.
           IF WZ-ZEILEN > 0 GO Z.
           MOVE 2 TO WH-P1.
           IF WM-KO = 0 MOVE 5 TO WH-P2
                        MOVE 40 TO WH-SIZE
                        IF WM-DRU = 1 MOVE 12 TO WH-P1         *> Laser
                            MOVE 0 TO WZ-SCHALT
                        end-if
                   else MOVE 4 TO WH-P2
                        MOVE 62 TO WH-SIZE
                        IF WM-DRU = 1 MOVE 15 TO WH-P1         *> Laser
                            MOVE 0 TO WZ-SCHALT.
           PERFORM BEG-DRU.
           ADD 1 TO WZ-SEITE.
           IF WM-DRU = 0 MOVE 1 TO WZ-SCHALT.
           MOVE WK-FIRMA TO DRG-STR(1:30).
           MOVE "GetrÑnkeabgabehilfsliste " TO DRG-STR(35:).
           MOVE WV-ABDAT TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRG-STR(66:9).
           MOVE " - " TO DRG-STR(75:)
           MOVE WV-BISDAT TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRG-STR(79:10).
           PERFORM DRUCK.
           MOVE ALL "ƒƒƒ" TO DRG-STR PERFORM DRUCK.
           MOVE "Kto-Nr. B e z e i c h n u n g             Summen     Ga
      -       "g-%     Getr.Abgab." TO DRG-STR(1:).
           PERFORM DRUCK.
           MOVE ALL "ƒƒƒ" TO DRG-STR PERFORM DRUCK.
           MOVE 2 TO WZ-SCHALT.
       Z.  EXIT.
      ******************************************************************
       LIESKTO SECTION.
       A.  SET RET TO TRUE.
           IF WH-NUM = 0 GO X.
           MOVE WH-NUM TO WD-KZ WH-KAT.
           DIVIDE 10 INTO WH-NUM GIVING WD-KTO.
           EVALUATE WD-KZ
               WHEN 3 MOVE WH-NUM TO KR-KTONR GO G
               WHEN 2 MOVE WH-NUM TO DE-KTONR GO E
               WHEN 1
               WHEN 0 MOVE WH-NUM TO SA-KTONR GO C
               WHEN OTHER GO X.
      *---------------------------------------------> lesen Sachkonto <-
       C.  READ SACHBUCH NOT INVALID MOVE SA-BEZ TO WT-TX
               MOVE SA-SOLL TO WE-SOLL
               MOVE SA-HABEN TO WE-HABEN
               ADD WE-SOLL WE-HABEN GIVING WE-SALDO GO Z.
           IF ZUGRIF PERFORM BESETZT.
           GO X.
      *---------------------------------------------> lesen Debitoren <-
       E.  READ DEBITOR NOT INVALID MOVE DE-BEZ TO WT-TX
               INSPECT WT-TX REPLACING ALL "#" BY ","
               MOVE DE-SOLL TO WE-SOLL
               MOVE DE-HABEN TO WE-HABEN
               ADD WE-SOLL WE-HABEN GIVING WE-SALDO GO Z.
           IF ZUGRIF PERFORM BESETZT.
           GO X.
      *--------------------------------------------> lesen Kreditoren <-
       G.  READ KREDIT NOT INVALID MOVE KR-BEZ TO WT-TX
               INSPECT WT-TX REPLACING ALL "#" BY ","
               MOVE KR-SOLL TO WE-SOLL
               MOVE KR-HABEN TO WE-HABEN
               ADD WE-SOLL WE-HABEN GIVING WE-SALDO GO Z.
           IF ZUGRIF PERFORM BESETZT.
       X.  SET FEHLER TO TRUE.
           DISPLAY "Konto fehlt: " at 2401 WH-NUM.
           PERFORM WEITER.
       Z.  EXIT.
      **************************************** Ust-Liste aus Buchungen *
       USTLIST SECTION.
       A.  CALL "CAUP" USING "0707100560000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Ust aus Buchungen " with highlight AT VDU-LP.
           MOVE "USTBUCH.LST" TO WH-DRUNAM.
       C.  CALL "CAUP" USING "16CLOFEN" WH-CREG.
           ADD 240 VDU-ECK GIVING VDU-LP.
           DISPLAY WH-DRUNAM with highlight AT VDU-LP.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "Periode:" AT VDU-LP.
           DISPLAY "<esc>= Abbruch, <ret>= Periode (JJMM)" AT 2301.
           CALL "CAUP" USING "1003124004" WH-CREG.
           IF ESC GO X.
           IF NOT RET GO C.
           IF WH-NUM = 0 GO C.
           MOVE WH-NUM TO WV-PER.
           IF WV-PER < 4913 ADD 200000 TO WV-PER.
           IF WV-PER < 9913 ADD 190000 TO WV-PER.
           MOVE WV-PER TO WD-PER.
           DISPLAY WD-PER with highlight AT VDU-LP.
           COMPUTE WV-ABDAT = WV-PER * 100 + 1.
           ADD 30 WV-ABDAT GIVING WV-BISDAT.
           MOVE 0 TO WZ-ZEILEN WZ-SEITE.
       E.  DISPLAY "<esc>= Abbruch, <ret>= Start < >" AT 2301.
           CALL "CAUP" USING "0023310000" WH-CREG.
           IF ESC GO X.
           IF not RET GO E.
           PERFORM LADEUST.
           INITIALIZE KO-BSATZ.
           MOVE 0 TO WS-UMSATZ(1) WS-UMSATZ(2)
                     WS-STEUER(1) WS-STEUER(2).
      *-----------------------------------------------> Start Listung <-
           MOVE 0 TO BZ-KTONR BZ-LFD.
           MOVE WV-ABDAT TO BZ-BUDAT.
       F.  MOVE 0 TO BZ-LFD.
           START BUCHZEIL KEY not < BZ-KEY INVALID GO Q.
       G.  READ BUCHZEIL NEXT NO LOCK AT END GO Q.
           IF ZUGRIF PERFORM BESETZT GO G.
           IF BZ-SYM > 20 GO G.
           IF BZ-BUDAT < WV-ABDAT MOVE WV-ABDAT TO BZ-BUDAT GO F.
           IF BZ-BUDAT > WV-BISDAT MOVE 99999999 TO BZ-BUDAT GO F.
           MOVE BZ-KTONR TO WH-NUM WD-KTO WD-KZ.
           ADD 403 VDU-ECK GIVING VDU-LP.
           DISPLAY BZ-KTONR AT VDU-LP " " BZ-BUDAT.
           PERFORM LIESKTO.
           PERFORM USTBUCH-DRUCK.
           DIVIDE 10 INTO BZ-KTONR GIVING DRU-KTONR UE-KTONR.
           DIVIDE 10 INTO BZ-GGKTO GIVING DRU-GGKTO UE-GGKTO.
           IF BZ-GGKTO = 999999
               MOVE "sabu" TO DRU-GGKTO(1:) UE-GGKTO(1:).
           MOVE BZ-BUDAT TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRU-DATUM UE-DATUM.
           MOVE WE-SYM(BZ-SYM + 1) TO DRU-SYM UE-SYM.
           MOVE BZ-BUBET TO DRU-BETRAG UE-BETRAG.
           MOVE 0 TO WK-SKTO.
           MOVE BZ-BUBET TO WK-BUBET WK-NETTO.
           MOVE BZ-VM TO WE-UST.
           IF BZ-VM = 0 GO G.
           IF BZ-MW = 0 and BZ-B1 = 0 and BZ-B2 = 0 GO G.
           EVALUATE BZ-VM
               WHEN 1 MOVE BZ-MW TO DRU-MWST UE-MWST
                      IF BZ-U1 not = 0
                          MOVE WT-UST(BZ-U1) TO DRU-UST UE-UST
                          MOVE BZ-B1 TO DRU-BMG UE-BMG
                      end-if
                      IF BZ-U2 not = 0 PERFORM EX-DRUCK
                          MOVE BZ-B1 TO DRU-BMG UE-BMG
                          MOVE WT-UST(BZ-U1) TO DRU-UST UE-UST
                      end-if
               WHEN 2 MOVE BZ-MW TO DRU-VST UE-VST
               WHEN 3 IF BZ-MW NOT = 0 ADD BZ-MW BZ-B1 TO WK-SKTO
                      MOVE WK-SKTO TO DRU-BETRAG UE-BETRAG
                      COMPUTE WH-WERT = BZ-B1 * -1
                      MOVE WH-WERT TO DRU-VST UE-UST
                      SUBTRACT BZ-B1 FROM KO-SKTOVST
               WHEN 9 ADD BZ-MW BZ-B1 BZ-B2 GIVING WK-SKTO
                      MOVE WK-SKTO TO DRU-BETRAG UE-BETRAG
                      COMPUTE WK-UDIF = BZ-MW * -1
                      COMPUTE DRU-MWST UE-MWST = BZ-MW * -1
                      IF BZ-U1 NOT = 0 MOVE BZ-B1 TO WK-BET
                          COMPUTE DRU-BMG UE-BMG = WK-BET * -1
                          SUBTRACT WK-BET FROM KO-MWSTBMG(BZ-U1)
                          MOVE WT-UST(BZ-U1) TO WH-UST DRU-UST UE-UST
                          COMPUTE WK-BET = (WH-UST / 100) * BZ-B1 * -1
                          ADD WK-BET TO KO-SKTOAUF(BZ-U1)
                          MOVE WK-BET TO DRU-MWST UE-MWST
                          SUBTRACT WK-BET FROM WK-UDIF
                          end-if
                     IF BZ-U2 NOT = 0 PERFORM EX-DRUCK
                          MOVE BZ-B2 TO WK-BET
                          COMPUTE DRU-BMG UE-BMG = WK-BET * -1
                          SUBTRACT WK-BET FROM KO-MWSTBMG(BZ-U2)
                          MOVE WT-UST(BZ-U2) TO WH-UST DRU-UST
                          COMPUTE WK-BET = (WH-UST / 100) * BZ-B2 * -1
                          ADD WK-BET TO KO-SKTOAUF(BZ-U2)
                          MOVE WK-BET TO DRU-MWST UE-MWST
                          SUBTRACT WK-BET FROM WK-UDIF
                          end-if
                     IF BZ-U1 NOT = 0 ADD WK-UDIF TO KO-SKTOAUF(BZ-U1)
                     else IF BZ-U2 NOT = 0
                              ADD WK-UDIF TO KO-SKTOAUF(BZ-U2)
               WHEN OTHER GO G.
           IF DRU-STR(22:40) = SPACE GO G.
           IF WD-KZ = 0 and BZ-VM not = 2
               PERFORM VARYING WY FROM 1 BY 1 UNTIL WY > 90
                   IF BZ-KTONR = WT-UKTO(WY) MOVE 94 TO WY
               end-perform
               IF WY = 91 MOVE "*" TO DRU-STR(9:1) UE-KTONR(9:1).
           IF WD-KZ = 2 and BZ-VM not = 4 or
              (WD-KZ = 3 and BZ-VM not = 3 and not = 2);
      *    IF WD-KZ = 2 or 3 and BZ-VM not = 2
               PERFORM VARYING WY FROM 1 BY 1 UNTIL WY > 90
                   IF BZ-GGKTO = WT-UKTO(WY) MOVE 94 TO WY
               end-perform
               IF WY = 91 MOVE "*" TO DRU-STR(9:1) UE-KTONR(9:1).
           PERFORM ADD-UST.
           PERFORM EX-DRUCK.
           IF SA-VSTKZ > 0 PERFORM VST-GRP.
           GO G.
      *-------------------------------------------------> Summendruck <-
       Q.  MOVE ALL "ƒ" TO DRU-STR PERFORM EX-DRUCK.
           MOVE "Summen:" TO DRU-STR(15:) UE-SATZ(15:).
           ADD KO-VST KO-SKTOVST GIVING DRU-VST UE-VST.
           ADD KO-MWST KO-SKTOAUF(1) KO-SKTOAUF(2) KO-SKTOAUF(3)
                KO-SKTOAUF(4) KO-SKTOAUF(5) KO-SKTOAUF(6)
                    GIVING DRU-MWST UE-MWST.
           PERFORM EX-DRUCK.
           MOVE 0 TO WS-MWST.
           MOVE "Bemessungssummen:" TO DRU-STR(15:) UE-SATZ(15:).
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 6
               IF KO-MWSTBMG(WX) not = 0
                   MOVE WT-UST(WX) TO DRU-UST UE-UST
                  MOVE KO-MWSTBMG(WX) TO DRU-BMG UE-BMG
                  COMPUTE WH-WERT = KO-MWSTBMG(WX) / 100
                  COMPUTE WH-WERT =  WH-WERT * WT-UST(WX)
                  MOVE WH-WERT TO DRU-MWST UE-MWST
                  ADD WH-WERT TO WS-MWST
                  PERFORM EX-DRUCK.
           MOVE WS-MWST TO DRU-MWST UE-MWST.
           PERFORM EX-DRUCK.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           IF WM-OPEN = 1 PERFORM END-DRU
               CLOSE USTEXL.
           IF WV-OPEN = 1 PERFORM END-VST.
           MOVE 9 TO WH-PG.
       Z.  EXIT.
      *********************************************** Ende Vst-Listung *
       END-VST SECTION.
       A.  IF WV-ZEILEN > 59 WRITE VST-SATZ BEFORE PAGE
               PERFORM VST-KOPF.
           MOVE ALL "ƒƒƒ" TO VST-STR.
           PERFORM V-DRUCK.
           MOVE "Kennziffer 27 " TO VST-BEZ.
           MOVE WS-UMSATZ(1) TO VST-UMSATZ.
           MOVE WS-STEUER(1) TO VST-STEUER.
           PERFORM V-DRUCK.
           MOVE "Kennziffer 28 " TO VST-BEZ.
           MOVE WS-UMSATZ(2) TO VST-UMSATZ.
           MOVE WS-STEUER(2) TO VST-STEUER.
           PERFORM V-DRUCK.
           CLOSE VST-DRU.
       Z.  EXIT.
      ****************************************** erweiterte VST-Summen *
       VST-GRP SECTION.
       A.  IF WV-OPEN = 0 OPEN OUTPUT VST-DRU
               MOVE WE-STG(5) TO VST-SATZ
               WRITE VST-SATZ AFTER 0
               MOVE 0 TO WV-ZEILEN WV-SCHALT
               MOVE SPACE TO VST-SATZ
               MOVE 1 TO WV-OPEN.
           IF WV-ZEILEN > 59 WRITE VST-SATZ BEFORE PAGE
               MOVE 0 TO WV-ZEILEN.
           IF WV-ZEILEN = 0 PERFORM VST-KOPF.
       C.  COMPUTE VST-KTONR = SA-KTONR / 10.
           MOVE BZ-BUDAT TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO VST-BUDAT.
           MOVE BZ-TX TO VST-BEZ.
           MOVE SA-VSTKZ TO VST-BK.
           MOVE BZ-BUBET TO VST-UMSATZ.
           MOVE BZ-MW TO VST-STEUER.
           EVALUATE SA-VSTKZ
               WHEN 27 ADD BZ-BUBET TO WS-UMSATZ(1)
                       ADD BZ-MW TO WS-STEUER(1)
               WHEN 28 ADD BZ-BUBET TO WS-UMSATZ(2)
                       ADD BZ-MW TO WS-STEUER(1).
           PERFORM V-DRUCK.
       Z.  EXIT.
      ******************************************************************
       VST-KOPF SECTION.
       A.  MOVE " - " TO VST-STR(34:)
           MOVE "Vorsteuerliste BAU/KFZ " TO VST-STR.
           MOVE WV-ABDAT TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO VST-STR(25:9).
           MOVE " - " TO VST-STR(34:)
           MOVE WV-BISDAT TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO VST-STR(38:10).
           PERFORM V-DRUCK.
           MOVE ALL "ƒƒƒ" TO VST-STR PERFORM V-DRUCK.
           MOVE "Kto-Nr. B e z e i c h n u n g             Summen
      -       "Ust-Betrag" TO VST-STR(1:).
           PERFORM V-DRUCK.
           MOVE ALL "ƒƒƒ" TO VST-STR PERFORM V-DRUCK.
           MOVE 2 TO WV-SCHALT.
       Z.  EXIT.
      ******************************************************************
       V-DRUCK SECTION.
       A.  WRITE VST-SATZ AFTER WV-SCHALT.
           MOVE SPACE TO VST-SATZ.
           ADD WV-SCHALT TO WV-ZEILEN.
           MOVE 1 TO WV-SCHALT.
       Z.  EXIT.
      ******************************************************************
       EX-DRUCK SECTION.
       A.  WRITE UE-SATZ AFTER 1.
           MOVE SPACE TO UE-SATZ.
           PERFORM DRUCK.
       Z.  EXIT.
      ********************************************* Ust + Bmg addieren *
       ADDUST-BMG SECTION.
       A.  ADD BZ-MW TO KO-MWST.
           IF BZ-U1 NOT = 0 ADD BZ-B1 TO KO-MWSTBMG(BZ-U1).
           IF BZ-U2 NOT = 0 ADD BZ-B2 TO KO-MWSTBMG(BZ-U2).
       Z.  EXIT.
      ***************************************** Ust + Bmg subtrahieren *
       SUBUST-BMG SECTION.
       A.  SUBTRACT BZ-MW FROM KO-MWST.
           IF BZ-U1 NOT = 0 SUBTRACT BZ-B1 FROM KO-MWSTBMG(BZ-U1).
           IF BZ-U2 NOT = 0 SUBTRACT BZ-B2 FROM KO-MWSTBMG(BZ-U2).
       Z.  EXIT.
      *************************************************** Addition UST *
       ADD-UST SECTION.
       A.  EVALUATE WE-UST
               WHEN 1 if wh-kat = 0;
                          if bz-sh = 1 PERFORM ADDUST-BMG
                          else PERFORM SUBUST-BMG
                      else
                      if wh-kat > 1;
                          if bz-sh = 0 PERFORM ADDUST-BMG
                          else PERFORM SUBUST-BMG
               WHEN 2 if wh-kat = 0;
                          if bz-sh = 0 add bz-mw TO KO-VST
                          else subtract bz-mw from ko-vst
                      else
                      if wh-kat > 1;
                          if bz-sh = 1 add bz-mw TO KO-VST
                      else subtract bz-mw from ko-vst end-if.
       Z.  EXIT.
      ******************************************************************
       USTBUCH-DRUCK SECTION.
       A.  IF WZ-ZEILEN > WH-SIZE WRITE DRA-SATZ AFTER PAGE
               MOVE 0 TO WZ-ZEILEN.
           IF WZ-ZEILEN > 0 GO Z.
           MOVE 2 TO WH-P1.
           IF WM-KO = 0 MOVE 5 TO WH-P2
                        MOVE 40 TO WH-SIZE
                        IF WM-DRU = 1 MOVE 12 TO WH-P1         *> Laser
                            MOVE 0 TO WZ-SCHALT
                        end-if
                   else MOVE 4 TO WH-P2
                        MOVE 62 TO WH-SIZE
                        IF WM-DRU = 1 MOVE 15 TO WH-P1         *> Laser
                            MOVE 0 TO WZ-SCHALT.
           IF WM-OPEN = 0 PERFORM BEG-DRU
               OPEN OUTPUT USTEXL.
           ADD 1 TO WZ-SEITE.
           IF WM-DRU = 0 MOVE 1 TO WZ-SCHALT.
           MOVE WK-FIRMA TO DRU-STR(1:30).
           MOVE "Ust-Liste aus Buchungen " TO DRU-STR(33:).
           MOVE WV-ABDAT TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRU-STR(73:9).
           MOVE " - " TO DRU-STR(82:)
           MOVE WV-BISDAT TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRU-STR(86:10).
           PERFORM DRUCK.
           MOVE ALL "ƒƒƒ" TO DRU-STR PERFORM DRUCK.
           MOVE " Kto-Nr.  Gg.Kto.    Datum Sym         Betrag     Vorst
      -        "euer Ust      Bemessung        Steuer" TO DRU-STR.
      *    MOVE " Kto-Nr.    Datum Sym         Betrag     Vorsteuer Ust
      *        "     Bemessung        Steuer" TO DRU-STR.
           PERFORM DRUCK.
           MOVE ALL "ƒƒƒ" TO DRU-STR PERFORM DRUCK.
           MOVE 2 TO WZ-SCHALT.
       Z.  EXIT.
      *********************************** Auswahl von Konto bis Konto **
       VONBIS SECTION.
       A.  IF WOLI GO E.
       C.  ADD 403 VDU-ECK GIVING VDU-LP.
           DISPLAY "von Konto:           bis Konto:" AT VDU-LP.
           DISPLAY "<esc>= Abbruch, <ret-leer>= ab Anfang, <ret>= Konto-
      -        "Nr." AT 2301.
           ADD 1 TO VDU-L.
           MOVE 0 TO WH-WERT.
           CALL "CAUP" USING "1004146110" WH-CREG.
           IF ESC GO Z.
           IF NOT RET GO C.
           IF WH-WERT = 0 AND WH-MCODE NOT = SPACE MOVE 2 TO WH-NUM
               CALL "FIBANZ" USING "99SUCH" WH-CREG
               CANCEL "FIBANZ"
               IF WH-NUM = 0 GO C.
           MOVE WH-NUM TO DE-KTONR.
           IF WH-NUM = 0 AND WH-MCODE = SPACE
               DISPLAY "Anfang" with highlight AT VDU-LP
           else DIVIDE 10 INTO DE-KTONR GIVING WD-KTO
               DISPLAY WD-KTO with highlight AT VDU-LP.
       E.  DISPLAY "<esc>= Abbruch, <ret-leer>= bis Ende, <ret>= Konto-N
      -        "r. " AT 2301.
           CALL "CAUP" USING "1004356110" WH-CREG.
           IF WOLI GO C.
           IF ESC GO Z.
           MOVE WH-WERT TO WV-KTONR.
           IF WH-NUM = 0 AND WH-MCODE = SPACE
               DISPLAY "Ende" with highlight AT VDU-LP
               MOVE 999999 TO WV-KTONR
           else DIVIDE 10 INTO WV-KTONR GIVING WD-KTO
               DISPLAY WD-KTO with highlight AT VDU-LP.
           IF WV-KTONR < DE-KTONR GO E.
       Z.  EXIT.
      ***************************************** Warenumsatz aus Konten *
       WARENUMSATZ SECTION.
       A.  CALL "CAUP" USING "0707100660000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Warenumsatzliste " with highlight AT VDU-LP.
           MOVE "WARENUM.LST" TO WH-DRUNAM.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 20
               MOVE WE-SKZ(WX) TO WE-SYMT
               EVALUATE WE-DIV
                   WHEN 1 COMPUTE WR-AR = WX - 1
                   WHEN 2 COMPUTE WR-GA = WX - 1.
           MOVE 0 TO WZ-SUM WH-KAT.
       C.  CALL "CAUP" USING "16CLOFEN" WH-CREG.
           ADD 240 VDU-ECK GIVING VDU-LP.
           DISPLAY WH-DRUNAM with highlight AT VDU-LP.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "Zeitraum:" AT VDU-LP.
           MOVE 0 TO WZ-DATUM.
           DISPLAY "<esc>= Abbruch, <ret>= Datum (TTMMJJ)" AT 2301.
           CALL "CAUP" USING "1103136006" WH-CREG.
           IF ESC GO X.
           IF NOT RET GO C.
           IF WZ-DATUM = 0 GO C.
           DISPLAY VDU-DATUM with highlight AT VDU-LP "  bis: "
           MOVE WX-DATUM TO WV-ABDAT.
       D.  DISPLAY "<esc>= Abbruch, <ret>= Datum (TTMMJJ)" AT 2301.
           MOVE 0 TO WZ-DATUM.
           CALL "CAUP" USING "1103296006" WH-CREG.
           IF ESC GO X.
           IF WOLI or AUF GO C.
           IF NOT RET GO D.
           IF WZ-DATUM = 0 GO D.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           MOVE WX-DATUM TO WV-BISDAT.
           IF WV-BISDAT < WV-ABDAT GO D.
           MOVE 0 TO WZ-ZEILEN WZ-SEITE DE-KTONR.
       E.  PERFORM VONBIS.
           IF ESC GO X.
       F.  ADD 502 VDU-ECK GIVING VDU-LP.
           DISPLAY ALL " " with SIZE 58 AT VDU-LP.
           ADD 503 VDU-ECK GIVING VDU-LP.
           DISPLAY "<esc>= Abbruch, <>= zurÅck, <ret>= Start < >"
               AT VDU-LP.
           CALL "CAUP" USING "1005460000" WH-CREG.
           IF WOLI GO E.
           IF ESC GO X.
           IF NOT RET GO F.
           ADD 502 VDU-ECK GIVING VDU-LP.
           DISPLAY ALL " " with SIZE 58 AT VDU-LP.
      *------------------------------------------> Start WarenumsÑtze <-
           MOVE DE-KTONR TO WD-KZ.
           EVALUATE WD-KZ
               WHEN 2 START DEBITOR KEY not < DE-KEY INVALID GO X
               WHEN 3 START KREDIT  KEY not < KR-KEY INVALID GO X.
       G.  EVALUATE WD-KZ
               WHEN 2 READ DEBITOR NEXT IGNORE LOCK AT END
                         MOVE 1 TO WH-KAT GO K
                         IF DE-KTONR > WV-KTONR MOVE 1 TO WH-KAT GO K
               WHEN 3 READ KREDIT  NEXT IGNORE LOCK AT END
                         MOVE 1 TO WH-KAT GO K
                         IF KR-KTONR > WV-KTONR MOVE 1 TO WH-KAT GO K.
           ADD 503 VDU-ECK GIVING VDU-LP.
           DIVIDE 10 INTO DE-KTONR GIVING WD-KTO.
           MOVE DE-BEZ TO WT-TX.
           INSPECT WT-TX REPLACING ALL "#" BY ",".
           DISPLAY WD-KTO AT VDU-LP " " WT-TX(1:40).
           MOVE DE-KTONR TO BZ-KTONR.
           MOVE WV-ABDAT TO BZ-BUDAT.
           MOVE 0 TO BZ-LFD WS-SUM.
           START BUCHZEIL KEY not < BZ-KEY INVALID GO K.
       I.  READ BUCHZEIL NEXT NO LOCK AT END GO K.
           IF ZUGRIF PERFORM BESETZT GO I.
           IF BZ-KTONR not = DE-KTONR GO K.
           IF BZ-BUDAT < WV-ABDAT or > WV-BISDAT GO K.
           IF BZ-SYM = WR-AR or BZ-SYM = WR-GA ADD BZ-BUBET TO WS-SUM.
           GO I.
       K.  IF WS-SUM = 0; IF WH-KAT = 0 GO G
                          else GO Q.
           PERFORM WAREN-KOPF.
           DIVIDE 10 INTO DE-KTONR GIVING DRW-KTONR.
           MOVE WT-TX TO DRW-BEZ.
           MOVE WS-SUM TO DRW-SUMME.
           ADD WS-SUM TO WZ-SUM.
           PERFORM DRUCK.
           MOVE 0 TO WS-SUM.
           IF DE-KTONR > WV-KTONR GO Q.
           IF WH-KAT = 0 GO G.
       Q.  MOVE ALL "ƒ" TO DRW-STR.
           PERFORM DRUCK.
           MOVE "Summe:" TO DRW-STR.
           MOVE WZ-SUM TO DRW-SUMME.
           PERFORM DRUCK.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           PERFORM END-DRU.
           MOVE 9 TO WH-PG.
       Z.  EXIT.
      ******************************************************************
       WAREN-KOPF SECTION.
       A.  IF WZ-ZEILEN > WH-SIZE WRITE DRA-SATZ AFTER PAGE
               MOVE 0 TO WZ-ZEILEN.
           IF WZ-ZEILEN > 0 GO Z.
           MOVE 2 TO WH-P1.
           IF WM-KO = 0 MOVE 5 TO WH-P2
                        MOVE 40 TO WH-SIZE
                        IF WM-DRU = 1 MOVE 12 TO WH-P1         *> Laser
                            MOVE 0 TO WZ-SCHALT
                        end-if
                   else MOVE 4 TO WH-P2
                        MOVE 62 TO WH-SIZE
                        IF WM-DRU = 1 MOVE 15 TO WH-P1         *> Laser
                            MOVE 0 TO WZ-SCHALT.
           PERFORM BEG-DRU.
           ADD 1 TO WZ-SEITE.
           IF WM-DRU = 0 MOVE 1 TO WZ-SCHALT.
           MOVE WK-FIRMA TO DRW-STR(1:30).
           MOVE "WarenumsÑtze " TO DRW-STR(36:).
           MOVE WV-ABDAT TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRW-STR(64:9).
           MOVE " - " TO DRW-STR(73:)
           MOVE WV-BISDAT TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRW-STR(77:10).
           PERFORM DRUCK.
           MOVE ALL "ƒƒƒ" TO DRW-STR PERFORM DRUCK.
           MOVE " Kto-Nr. Bezeichnung
      -        "                       Umsatz" TO DRW-STR.
           PERFORM DRUCK.
           MOVE ALL "ƒƒƒ" TO DRW-STR PERFORM DRUCK.
           MOVE 2 TO WZ-SCHALT.
       Z.  EXIT.
      ************************************ Auslistung innergem. Erwerb *
       IG-ERWERB SECTION.
       A.  CALL "CAUP" USING "0707100660000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " innerg. Erwerb" with highlight AT VDU-LP.
           MOVE "IGERWER.LST" TO WH-DRUNAM.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 20
               MOVE WE-SKZ(WX) TO WE-SYMT
               EVALUATE WE-DIV
                   WHEN 1 COMPUTE WR-AR = WX - 1
                   WHEN 2 COMPUTE WR-GA = WX - 1.
           MOVE 0 TO WZ-SUM WH-KAT.
       C.  CALL "CAUP" USING "16CLOFEN" WH-CREG.
           ADD 240 VDU-ECK GIVING VDU-LP.
           DISPLAY WH-DRUNAM with highlight AT VDU-LP.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "Zeitraum:" AT VDU-LP.
           MOVE 0 TO WZ-DATUM.
           DISPLAY "<esc>= Abbruch, <ret>= Datum (TTMMJJ)" AT 2301.
           CALL "CAUP" USING "1103136006" WH-CREG.
           IF ESC GO X.
           IF NOT RET GO C.
           IF WZ-DATUM = 0 GO C.
           DISPLAY VDU-DATUM with highlight AT VDU-LP "  bis: "
           MOVE WX-DATUM TO WV-ABDAT.
       D.  DISPLAY "<esc>= Abbruch, <ret>= Datum (TTMMJJ)" AT 2301.
           MOVE 0 TO WZ-DATUM.
           CALL "CAUP" USING "1103296006" WH-CREG.
           IF ESC GO X.
           IF WOLI or AUF GO C.
           IF NOT RET GO D.
           IF WZ-DATUM = 0 GO D.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           MOVE WX-DATUM TO WV-BISDAT.
           IF WV-BISDAT < WV-ABDAT GO D.
           MOVE 0 TO WZ-ZEILEN WZ-SEITE DE-KTONR.
       E.  PERFORM VONBIS.
           IF ESC GO X.
       F.  ADD 502 VDU-ECK GIVING VDU-LP.
           DISPLAY ALL " " with SIZE 58 AT VDU-LP.
           ADD 503 VDU-ECK GIVING VDU-LP.
           DISPLAY "<esc>= Abbruch, <>= zurÅck, <ret>= Start < >"
               AT VDU-LP.
           CALL "CAUP" USING "1005460000" WH-CREG.
           IF WOLI GO E.
           IF ESC GO X.
           IF NOT RET GO F.
           ADD 502 VDU-ECK GIVING VDU-LP.
           DISPLAY ALL " " with SIZE 58 AT VDU-LP.
      *------------------------------------------> Start WarenumsÑtze <-
           MOVE DE-KTONR TO WD-KZ.
           MOVE 2 TO WD-KZ.
           EVALUATE WD-KZ
               WHEN 2 START DEBITOR KEY not < DE-KEY INVALID GO X
               WHEN 3 START KREDIT  KEY not < KR-KEY INVALID GO X.
       G.  EVALUATE WD-KZ
               WHEN 2 READ DEBITOR NEXT IGNORE LOCK AT END
                         MOVE 1 TO WH-KAT GO K
                         IF DE-KTONR > WV-KTONR MOVE 1 TO WH-KAT GO K
               WHEN 3 READ KREDIT  NEXT IGNORE LOCK AT END
                         MOVE 1 TO WH-KAT GO K
                         IF KR-KTONR > WV-KTONR MOVE 1 TO WH-KAT GO K.
           ADD 503 VDU-ECK GIVING VDU-LP.
           DIVIDE 10 INTO DE-KTONR GIVING WD-KTO.
           MOVE DE-BEZ TO WT-TX.
           INSPECT WT-TX REPLACING ALL "#" BY ",".
           DISPLAY WD-KTO AT VDU-LP " " WT-TX(1:40).
           IF DE-OPKZ < 30 GO G.
           MOVE DE-KTONR TO BZ-KTONR.
           MOVE WV-ABDAT TO BZ-BUDAT.
           MOVE 0 TO BZ-LFD WS-SUM.
           START BUCHZEIL KEY not < BZ-KEY INVALID GO K.
       I.  READ BUCHZEIL NEXT NO LOCK AT END GO K.
           IF ZUGRIF PERFORM BESETZT GO I.
           IF BZ-KTONR not = DE-KTONR GO K.
           IF BZ-BUDAT < WV-ABDAT or > WV-BISDAT GO K.
           IF BZ-SYM = WR-AR or BZ-SYM = WR-GA ADD BZ-BUBET TO WS-SUM.
           GO I.
       K.  IF WS-SUM = 0; IF WH-KAT = 0 GO G
                          else GO Q.
           PERFORM WAREN-KOPF.
           DIVIDE 10 INTO DE-KTONR GIVING DRW-KTONR.
           MOVE WT-TX TO DRW-BEZ.
           MOVE WS-SUM TO DRW-SUMME.
           ADD WS-SUM TO WZ-SUM.
           PERFORM DRUCK.
           MOVE 0 TO WS-SUM.
           IF DE-KTONR > WV-KTONR GO Q.
           IF WH-KAT = 0 GO G.
       Q.  MOVE ALL "ƒ" TO DRW-STR.
           PERFORM DRUCK.
           MOVE "Summe:" TO DRW-STR.
           MOVE WZ-SUM TO DRW-SUMME.
           PERFORM DRUCK.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           PERFORM END-DRU.
           MOVE 9 TO WH-PG.
       Z.  EXIT.
