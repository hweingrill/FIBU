      $SET LINKCOUNT "240" ANS85 BOUND AUTOLOCK NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    FIBKONTO.
      ******************************************************************
      *        Kontodruck Debitoren, Kreditoren u. Sachkonto           *
      ******************************************************************
       ENVIRONMENT    DIVISION.
       CONFIGURATION   SECTION.
       SOURCE-COMPUTER.  PC.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY FIBUSEC.CPY.
           SELECT DRUCKER  ASSIGN TO PRINTER WH-DRUNAM
                           FILE STATUS WF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       COPY FIBUFD.CPY.
      ******************************************************************
       FD  DRUCKER                     LABEL RECORD OMITTED.
       01  DRA-SATZ.
           03  FILLER                  PIC X(137).
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
       01  DRQ-SATZ.
           03  FILLER                  PIC X(10).
           03  DRQ-STR         OCCURS 2.
               05 DRQ-SYM              PIC XX.
               05 DRQ-RENR             PIC ZZZZ.ZZ9-.
               05 DRQ-BET              PIC ZZZZZ.ZZ9,99-.
               05 DRQ-SKTO             PIC ZZZZ.ZZ9,99-.
               05 DRQ-TX               PIC X(7).
           03  drq-sum                 pic zzzz.zz9,99-.
           03  drq-dif                 pic zzzz.zz9,99-.
       01  DRP-SATZ.
           03  FILLER                  PIC X(9).
           03  DRP-STR.
               05 DRP-KTONR            PIC ZZ.999,9-.
               05 DRP-BEZ              PIC X(51).
               05 DRP-KZ               PIC XX.
               05 DRP-SOLL             PIC ZZZ.ZZZ.ZZ9,99-.
               05 DRP-HABEN            PIC ZZZ.ZZZ.ZZ9,99-.
               05 DRP-SALDO            PIC ZZZ.ZZZ.ZZ9,99-.
       01  DRS-SATZ.
           03  FILLER                  PIC XXXX.
           03  DRS-WUTX                PIC X(13).
           03  DRS-WUBET               PIC ZZZZ.ZZZ.ZZ9,99-.
           03  FILLER                  PIC X(8).
           03  DRS-BEZ                 PIC X(16).
           03  DRS-SOLL                PIC ZZZZ.ZZZ.ZZ9,99-.
           03  FILLER                  PIC XXXX.
           03  DRS-HATX                PIC X(8).
           03  DRS-HABEN               PIC ZZZZ.ZZZ.ZZ9,99-.
           03  FILLER                  PIC XXXX.
           03  DRS-SALTX               PIC X(8).
           03  DRS-SALDO               PIC ZZZ.ZZZ.ZZ9,99-.
           03  FILLER                  PIC X(8).
      ******************************************************************
       WORKING-STORAGE SECTION.
       01  WH-CALL.
           03  WL-CA                   PIC 99.
           03  WL-REST                 PIC 9(13).
       COPY WHCREG.CPY.
       01  WH-REG.
           03  WK-DSTG.
               05 WK-STG               PIC X(10)     OCCURS 10.
           03  WH-P1                   PIC 99        COMP.
           03  WH-P2                   PIC 99        COMP.
           03  WX-PRNO                 PIC 99        COMP-X.
           03  WX-PRSTAT               PIC 99        COMP-X.
           03  WZ-SEITE                PIC 999       COMP   VALUE ZERO.
           03  WZ-SCHALT               PIC 99        COMP   VALUE ZERO.
           03  WZ-ZEILEN               PIC 99        COMP   VALUE ZERO.
           03  WH-DRUNAM               PIC X(12)     VALUE   "LPT1:".
           03  WM-OPEN                 PIC 9         COMP   VALUE ZERO.
           03  WM-WJAHR                PIC 9(8)      COMP   OCCURS 2.
           03  WM-ZEIL                 PIC 99        COMP.
           03  WA-KZ                   PIC 99        COMP.
           03  WA-AB                   PIC 9(8)      COMP.
           03  WA-BIS                  PIC 9(8)      COMP.
           03  WV-KTONR                PIC 9(6)      COMP.
           03  WM-KTONR                PIC 9(6)      COMP.
           03  WH-SIZE                 PIC 99        COMP   VALUE 63.
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
           03  WS-OPS                  PIC S9(9)V99  COMP.
           03  WG                      PIC 99        COMP.
           03  WH-MOD                  PIC 99        COMP.
           03  WV-ENDE                 PIC X(10).
           03  WH-BKEY.
               05 WH-KTONR             PIC 9(6)      COMP.
               05 WH-BUDAT             PIC 9(6)      COMP.
               05 WH-LFD               PIC 9999      COMP.
           03  WH-KAT                  PIC 9         COMP.
      *-----------------------------------------------> Anzeigefelder <-
           03  WD-NUM                  PIC ZZZZZ9.
           03  WD-KTO                  PIC ZZZZ9,9.
           03  WD-KZ                   PIC 9.
       COPY FIBUEXT.CPY.
       COPY FIBUDECL.CPY.
       END DECLARATIVES.
      *****************************************************************
       STEUER SECTION.
       A.  MOVE WL-CALL TO WH-CALL.
           MOVE WL-CREG TO WH-CREG.
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
           MOVE 46 TO WX-PRNO.
           CALL X"91" USING WX-PRSTAT WX-PRNO DRUCKER.
           MOVE SPACE TO DRA-SATZ.
           CLOSE DRUCKER.
           MOVE 0 TO WM-OPEN.
       Z.  EXIT.
      ******************************************************************
       BEG-DRU SECTION.
       A.  IF WM-OPEN > 0 GO Z.
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
       A.  CALL "CAUP" USING "07041019490000" WH-CREG.
           MOVE SPACE TO WH-DRUNAM.
           MOVE WE-WJAHR(1) TO WM-WJAHR(1).
           MOVE WE-WJAHR(2) TO WM-WJAHR(2).
           MOVE 0 TO WL.
       B.  PERFORM LADE-DRU.
           MOVE 0 TO WA-AB WA-BIS.
           MOVE 9 TO WH-PG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Kontendruck " with highlight AT VDU-LP.
           ADD 18 TO VDU-P.
           IF WH-DRUNAM = "LPT1:" DISPLAY " Direktdruck " AT VDU-LP
                             ELSE DISPLAY " Druckdatei  " AT VDU-LP.
           ADD 138 VDU-ECK GIVING VDU-LP.
           IF WL = 0 DISPLAY " Einzel " with highlight AT VDU-LP
                else DISPLAY " Endlos " with highlight AT VDU-LP.
           ADD 317 VDU-ECK GIVING VDU-LP.
           MOVE WM-WJAHR(2) TO WC-DATUM.
           PERFORM DATDREH.
           DISPLAY "Wirtschaftsjahrende: " AT VDU-LP VDU-DATUM with
               highlight.
           ADD 508 VDU-ECK GIVING VDU-LP.
           DISPLAY "1 - Sachbuch"   AT VDU-LP.
           ADD 1 TO VDU-L.
           DISPLAY "2 - Debitoren"  AT VDU-LP.
           ADD 1 TO VDU-L.
           DISPLAY "3 - Kreditoren" AT VDU-LP.
           ADD 2 TO VDU-L.
           DISPLAY "4 - Wechsel Druckausgabe" AT VDU-LP.
           ADD 1 TO VDU-L.
           DISPLAY "5 - Wechsel Formularart" AT VDU-LP.
           ADD 1 TO VDU-L.
           DISPLAY "6 - Lîschen altes Jahr (alles)" AT VDU-LP.
           ADD 2 TO VDU-L.
           DISPLAY "7 - Lîschen altes Jahr (Sachk.)" AT VDU-LP.
           ADD 1 TO VDU-L.
           DISPLAY "8 - Lîschen altes Jahr (Persk.)" AT VDU-LP.
           ADD 1 TO VDU-L.
           DISPLAY "9 - Konten auf DatentrÑger" AT VDU-LP.
           ADD 216 TO VDU-LP.
           DISPLAY "bitte wÑhlen Sie:" with highlight AT VDU-LP.
       C.  DISPLAY "< / > - Wirtschaftsjahrwechel, <ret-leer>= Programme
      -        "nde" AT 2301.
           CALL "CAUP" USING "1017421001" WH-CREG.
           MOVE WH-NUM TO WH-PG.
           IF AUF OR AB PERFORM WJ-WECHSEL GO B.
           IF ESC OR WH-PG = 0 GO X.
           IF NOT RET GO C.
           EVALUATE WH-PG
               WHEN 1 PERFORM SACHKONT
               WHEN 2 PERFORM DEBKONT
               WHEN 3 PERFORM KREDKONT
               WHEN 4 PERFORM DRUAUS   GO B
               WHEN 5 IF WL = 0 MOVE 1 TO WL
                           else MOVE 0 TO WL GO B
               WHEN 6
               WHEN 7 PERFORM ALTJAHR
               WHEN 8 PERFORM LOEPERS
               WHEN 9 PERFORM FIN-DRUCK.
           GO B.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           UNLOCK KONSTANT.
           UNLOCK BUCHZEIL.
           MOVE "FRWLIST 70LIST" TO WT-TX.
       Z.  EXIT.
      **************************************** Wechsel Wirtschaftsjahr *
       WJ-WECHSEL SECTION.
       A.  MOVE 9 TO WH-KEY.
           READ KONSTANT INVALID GO Z.
           IF ZUGRIF PERFORM BESETZT GO A.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 4
               or WM-WJAHR(1) = KO-WBEG(WX) end-perform.
           MOVE WX TO WY.
           IF WY > 3 AND AB OR (WY = 1 AND AUF) GO X.
           IF AB  ADD 1 TO WY.
           IF AUF ADD -1 TO WY.
           IF KO-WBEG(WY) = 0 GO X.
           MOVE KO-WBEG(WY) TO WM-WJAHR(1).
           MOVE KO-WEND(WY) TO WM-WJAHR(2).
           GO Z.
       X.  IF AB and WX = 5 MOVE 9 TO WY
               COMPUTE WM-WJAHR(1) = WM-WJAHR(1) + 10000
               COMPUTE WM-WJAHR(2) = WM-WJAHR(2) + 10000
               GO Z.
           IF not AB MOVE 9 TO WY
               COMPUTE WM-WJAHR(1) = WM-WJAHR(1) - 10000
               COMPUTE WM-WJAHR(2) = WM-WJAHR(2) - 10000
               GO Z.
           DISPLAY "keine Daten vorhanden" with highlight BLINK AT 2401.
       Z.  EXIT.
      ******************************************************************
       S-AUSZUG SECTION.
       A.  MOVE 0 TO WA-AB WA-BIS WA-KZ.
           ADD 6 VDU-ECK GIVING VDU-LP.
           DISPLAY "Auszug ab:            bis:" with highlight
               foreground-color 6 AT VDU-LP.
       C.  DISPLAY "<esc>= Abbruch, <ret>= Datum" AT 2301.
           MOVE 17 TO VDU-LP.
           CALL "CAUP" USING "1100006006" WH-CREG.
           IF ESC GO X.
           IF not RET GO C.
           IF WX-DATUM = 0 GO C.
           MOVE WX-DATUM TO WA-AB.
           DISPLAY VDU-DATUM with highlight foreground-color 3
               AT VDU-LP.
       E.  DISPLAY "<esc>= Abbruch, <ret>= Datum" AT 2301.
           MOVE 33 TO VDU-LP.
           CALL "CAUP" USING "1100006006" WH-CREG.
           IF ESC GO X.
           IF AUF or WOLI GO C.
           IF not RET GO E.
           IF WX-DATUM = 0 GO E.
           DISPLAY VDU-DATUM with highlight foreground-color 3
               AT VDU-LP.
           IF WX-DATUM < WA-AB GO E.
           MOVE WX-DATUM TO WA-BIS.
           GO Z.
       X.  MOVE 0 TO WA-AB WA-BIS.
           ADD 6 VDU-ECK GIVING VDU-LP.
           DISPLAY "                                " with highlight
               AT VDU-LP.
       Z.  EXIT.
      ************************************************* Sachbuchkonten *
       SACHKONT SECTION.
       A.  CALL "CAUP" USING "07070806710000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           IF WH-DRUNAM NOT = "LPT1:" MOVE "SACHKONT.LST" TO WH-DRUNAM.
           DISPLAY " Sachkontendruck " with highlight AT VDU-LP.
           MOVE 0 TO WA-AB WA-BIS.
       C.  CALL "CAUP" USING "16CLRFEN" WH-CREG.
           ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "bis:" AT VDU-LP.
           MOVE WM-WJAHR(2) TO WC-DATUM.
           PERFORM DATDREH.
           ADD 208 VDU-ECK GIVING VDU-LP.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           ADD 243 VDU-ECK GIVING VDU-LP.
           DISPLAY WH-DRUNAM with highlight AT VDU-LP.
           MOVE 0 TO WM-SOLL WM-HABEN WZ-ZEILEN WZ-SEITE WB.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "von Konto:           bis Konto:" AT VDU-LP.
           ADD 403 VDU-ECK GIVING VDU-LP.
           DISPLAY "<esc>= Abbruch, <ret-leer>= ab Anfang, <ret>= Konto-
      -        "Nr." AT VDU-LP.
           ADD 1 TO VDU-L.
           DISPLAY "<#> statt <ret>= nur mit Saldo" AT VDU-LP.
           MOVE 0 TO WE WH-WERT.
           DISPLAY "< >= Kontodruck bis, < >= Kontoauszug"
               with highlight AT 2401.
           CALL "CAUP" USING "1003146110" WH-CREG.
           IF ESC GO X.
           IF TABL PERFORM SACHRECH.
           IF KIST MOVE 1 TO WE
               SET RET TO TRUE.
           IF AUF PERFORM NURBIS GO C.
           IF AB PERFORM S-AUSZUG GO C.
           IF NOT RET GO C.
           IF WH-WERT = 0 AND WH-MCODE NOT = SPACE MOVE 0 TO WH-NUM
               CALL "FIBANZ" USING "99SUCH" WH-CREG
               CANCEL "FIBANZ"
               IF WH-NUM = 0 GO C.
           MOVE WH-NUM TO SA-KTONR.
           IF WH-NUM = 0 AND WH-MCODE = SPACE
               DISPLAY "Anfang" with highlight AT VDU-LP
           else DIVIDE 10 INTO SA-KTONR GIVING WD-KTO
               DISPLAY WD-KTO with highlight AT VDU-LP.
           ADD 257 VDU-ECK GIVING VDU-LP.
           IF WE = 1 DISPLAY "Saldo" AT VDU-LP
                else DISPLAY "alle " AT VDU-LP.
       E.  ADD 403 VDU-ECK GIVING VDU-LP.
           DISPLAY "<esc>= Abbruch, 0+<ret>= bis Ende, <ret>= Konto-Nr."
                AT VDU-LP.
           IF SA-KTONR > 0 COMPUTE WD-KTO = SA-KTONR / 10
               MOVE SA-KTONR TO WH-WERT
               ADD 335 VDU-ECK GIVING VDU-LP
               DISPLAY WD-KTO with highlight AT VDU-LP.
           CALL "CAUP" USING "1003356110" WH-CREG.
           IF WOLI GO C.
           IF ESC GO X.
           MOVE WH-WERT TO WV-KTONR.
           IF WH-NUM = 0 AND WH-MCODE(1:1) = "0"
               DISPLAY "Ende" with highlight AT VDU-LP
               MOVE 999999 TO WV-KTONR
           else DIVIDE 10 INTO WV-KTONR GIVING WD-KTO
               DISPLAY WD-KTO with highlight AT VDU-LP.
           IF WV-KTONR < SA-KTONR GO E.
       F.  ADD 503 VDU-ECK GIVING VDU-LP.
           DISPLAY "<esc>= Abbruch, <>= zurÅck, <ret>= Start < >"
               AT VDU-LP.
           ADD 402 VDU-ECK GIVING VDU-LP.
           DISPLAY ALL " " with SIZE 68 AT VDU-LP.
           CALL "CAUP" USING "1005460000" WH-CREG.
           IF WOLI GO E.
           IF ESC GO X.
           IF NOT RET GO F.
           PERFORM SAKODRU.
       W.  PERFORM END-DRU.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      *********************************************** Druck Sachkonten *
       SAKODRU SECTION.
       A.  ADD 502 VDU-ECK GIVING VDU-LP.
           DISPLAY ALL " " with SIZE 68 AT VDU-LP.
           START SACHBUCH KEY NOT < SA-KEY INVALID GO Z.
       G.  READ SACHBUCH NEXT IGNORE LOCK AT END MOVE 99 TO WB GO Q.
           IF SA-KTONR > WV-KTONR MOVE 99 TO WB GO Q.
       H.  DIVIDE 10 INTO SA-KTONR GIVING WD-KTO.
           ADD 403 VDU-ECK GIVING VDU-LP
           DISPLAY WD-KTO with highlight AT VDU-LP " " SA-BEZ(1:47).
           IF WE = 1; IF SA-SOLL = SA-HABEN GO G.
           MOVE SA-KTONR TO BZ-KTONR.
           MOVE SA-BEZ TO WT-TX.
           MOVE WM-WJAHR(1) TO BZ-BUDAT.
           MOVE 0 TO BZ-LFD WM-ZEIL.
           START BUCHZEIL KEY NOT < BZ-KEY INVALID GO G.
       I.  MOVE SPACE TO BZ-TX.
           READ BUCHZEIL NEXT AT END GO Q.
           IF ZUGRIF PERFORM BESETZT GO I.
           MOVE 0 TO WA-KZ.
           IF WA-AB not = 0;
               IF WA-AB > BZ-BUDAT MOVE 1 TO WA-KZ
               else IF DRA-SATZ not = SPACE PERFORM DRUCK.
           IF BZ-BUDAT > WM-WJAHR(2) GO Q.
           IF BZ-KTONR NOT = SA-KTONR GO Q.
           IF WA-BIS not = 0; IF BZ-BUDAT > WA-BIS GO Q.
           PERFORM KTO-ZEIL.
           GO I.
       Q.  PERFORM KTO-ENDE.
           IF WB NOT = 99 GO G.
       Z.  EXIT.
      ******************************************************************
       NURBIS SECTION.
       A.  ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "bis:" AT VDU-LP.
           MOVE WM-WJAHR(2) TO WC-DATUM.
           PERFORM DATDREH.
           ADD 208 VDU-ECK GIVING VDU-LP.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           MOVE WM-WJAHR(2) TO WZ-DATUM WH-WERT.
       C.  CALL "CAUP" USING "1102086006" WH-CREG.
           IF ESC ADD 203 VDU-ECK GIVING VDU-LP
               DISPLAY "               " AT VDU-LP
               GO Z.
           IF not RET GO C.
           IF WX-DATUM = 0 GO C.
           MOVE WX-DATUM TO WM-WJAHR(2).
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
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
      ******************************************************************
       FALSCH SECTION.
       A.  DISPLAY "Es wurde alphat. angewÑhlt" AT 2401.
           PERFORM WEITER.
       Z.  EXIT.
      **************************************** gemeinsame Anwahlpunkte *
       DEBKRED SECTION.
       A.  CALL "CAUP" USING "07090706710000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           IF WH-PG = 2 DISPLAY " Kundenkonten " with reverse-video
                           highlight foreground-color 3 AT VDU-LP
                   else DISPLAY " Lieferantenkonten " with reverse-video
                           highlight foreground-color 2 AT VDU-LP.
           MOVE 1 TO WH-MOD.
       C.  CALL "CAUP" USING "16CLRFEN" WH-CREG.
           ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "bis:" AT VDU-LP.
           MOVE WM-WJAHR(2) TO WC-DATUM.
           PERFORM DATDREH.
           ADD 208 VDU-ECK GIVING VDU-LP.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           ADD 243 VDU-ECK GIVING VDU-LP.
           DISPLAY WH-DRUNAM with highlight AT VDU-LP.
           ADD 257 VDU-ECK GIVING VDU-LP.
           IF WH-MOD = 0
                DISPLAY " numerisch " with reverse-video AT VDU-LP
                ADD 303 VDU-ECK GIVING VDU-LP
                DISPLAY "von Konto:           bis Konto:" AT VDU-LP
           else DISPLAY " alphabet. " with reverse-video AT VDU-LP
                ADD 303 VDU-ECK GIVING VDU-LP
                DISPLAY "von Mcode:           bis Mcode:" AT VDU-LP.
           MOVE 0 TO WM-SOLL WM-HABEN WZ-ZEILEN WZ-SEITE WB.
           ADD 403 VDU-ECK GIVING VDU-LP.
           DISPLAY "<esc>= Abbruch, <ret-leer>= ab Anfang, <ret>= Konto-
      -        "Nr." AT VDU-LP.
           ADD 1 TO VDU-L.
           DISPLAY "<#> statt <ret>= nur mit Saldo, " AT VDU-LP.
           IF WH-MOD = 0 DISPLAY "< >= alphabet." AT 0000
                    else DISPLAY "< >= numerisch" AT 0000.
           MOVE 0 TO WE WH-WERT.
           DISPLAY "< >= Kontodruck bis, < >= Kontoauszug"
               with highlight AT 2401.
           CALL "CAUP" USING "1003146110" WH-CREG.
           IF ESC GO Z.
           IF TABL; IF WH-PG = 2 PERFORM DEBRECH
                           else PERFORM KREDRECH.
           IF KIST MOVE 1 TO WE
               SET RET TO TRUE.
           IF AUF PERFORM NURBIS GO C.
           IF WORE ADD -1 TO WH-MOD GO C.
           IF AB PERFORM S-AUSZUG GO C.
           IF NOT RET GO C.
           IF WH-MOD = 1; IF WH-NUM not = 0 PERFORM FALSCH GO C
           else MOVE WH-MCODE TO WT-TX
               CALL "CAUP" USING "20CONV10" WH-CREG
               MOVE WT-TX TO WK-MCODE
               CALL "CAUP" USING "21CONV10" WH-CREG
               MOVE WT-TX TO WH-MCODE
               IF WH-MCODE = SPACE
                    DISPLAY "Anfang" with highlight AT VDU-LP
               else DISPLAY WH-MCODE(1:10) with highlight AT VDU-LP
               GO E.
           IF WH-WERT = 0 AND WH-MCODE NOT = SPACE MOVE 3 TO WH-NUM
               CALL "FIBANZ" USING "99SUCH" WH-CREG
               CANCEL "FIBANZ"
               IF WH-NUM = 0 GO C.
           MOVE WH-NUM TO WM-KTONR.
           IF WH-NUM = 0 AND WH-MCODE = SPACE
               DISPLAY "Anfang" with highlight AT VDU-LP
           else DIVIDE 10 INTO WM-KTONR GIVING WD-KTO
               DISPLAY WD-KTO with highlight AT VDU-LP.
       E.  ADD 503 VDU-ECK GIVING VDU-LP.
           DISPLAY " " with SIZE 50 AT VDU-LP.
           ADD 228 VDU-ECK GIVING VDU-LP.
           IF WE = 1 DISPLAY "Saldo" with highlight AT VDU-LP
                else DISPLAY "alle " with highlight AT VDU-LP.
           ADD 403 VDU-ECK GIVING VDU-LP.
           DISPLAY "<esc>= Abbruch, <ret-leer>= bis Ende, <ret>= Konto-N
      -        "r. " AT VDU-LP.
           CALL "CAUP" USING "1003356110" WH-CREG.
           IF WOLI GO C.
           IF ESC GO Z.
           IF WH-MOD = 1; IF WH-NUM not = 0 PERFORM FALSCH GO C
           else MOVE WH-MCODE TO WT-TX
               CALL "CAUP" USING "20CONV10" WH-CREG
               MOVE WT-TX TO WV-ENDE
               CALL "CAUP" USING "21CONV08" WH-CREG
               MOVE WT-TX TO WH-MCODE
               INSPECT WV-ENDE REPLACING ALL SPACE BY x"ff"
               IF WH-MCODE = SPACE
                    DISPLAY "Ende" with highlight AT VDU-LP
               else DISPLAY WH-MCODE(1:10) with highlight AT VDU-LP
               GO F.
           MOVE WH-WERT TO WV-KTONR.
           IF WH-NUM = 0 AND WH-MCODE = SPACE
               DISPLAY "Ende" with highlight AT VDU-LP
               MOVE 999999 TO WV-KTONR
           else DIVIDE 10 INTO WV-KTONR GIVING WD-KTO
               DISPLAY WD-KTO with highlight AT VDU-LP.
           IF WV-KTONR < WM-KTONR GO E.
       F.  ADD 503 VDU-ECK GIVING VDU-LP.
           DISPLAY "<esc>= Abbruch, <>= zurÅck, <ret>= Start < >    "
               AT VDU-LP.
           ADD 402 VDU-ECK GIVING VDU-LP.
           DISPLAY ALL " " with SIZE 68 AT VDU-LP.
           CALL "CAUP" USING "1005460000" WH-CREG.
           IF WOLI GO E.
           IF ESC GO Z.
           IF NOT RET GO F.
           ADD 502 VDU-ECK GIVING VDU-LP.
           DISPLAY ALL " " with SIZE 68 AT VDU-LP.
       Z.  EXIT.
      ********************************************** Listung Debitoren *
       DEBKONT SECTION.
       A.  IF WH-PG not = 9 PERFORM DEBKRED
               IF WH-DRUNAM NOT = "LPT1:"
                   MOVE "KUNDKONT.LST" TO WH-DRUNAM.
           IF ESC GO X.
           EVALUATE WH-MOD
               WHEN 0 MOVE WM-KTONR TO DE-KTONR
                      START DEBITOR KEY NOT < DE-KEY INVALID GO Z
                      end-start GO G
               WHEN 1 MOVE WK-MCODE TO DE-MCODE
                      START DEBITOR KEY NOT < DE-MCODE INVALID GO Z.
       G.  READ DEBITOR NEXT IGNORE LOCK AT END MOVE 99 TO WB GO Q.
           EVALUATE WH-MOD
               WHEN 1 IF DE-MCODE > WV-ENDE MOVE 99 TO WB GO Q
               WHEN 0 IF DE-KTONR > WV-KTONR MOVE 99 TO WB GO Q.
       H.  DIVIDE 10 INTO DE-KTONR GIVING WD-KTO.
           MOVE DE-BEZ TO WT-TX.
           INSPECT WT-TX REPLACING ALL "#" BY ",".
           DISPLAY WD-KTO with highlight AT VDU-LP " " WT-TX(1:47).
           IF WE = 1; IF DE-SOLL = DE-HABEN GO G.
           MOVE DE-KTONR TO BZ-KTONR.
           MOVE WM-WJAHR(1) TO BZ-BUDAT.
           MOVE 0 TO BZ-LFD WM-ZEIL.
           START BUCHZEIL KEY NOT < BZ-KEY INVALID GO G.
       I.  MOVE SPACE TO BZ-TX.
           READ BUCHZEIL NEXT AT END GO Q.
           IF ZUGRIF PERFORM BESETZT GO I.
           MOVE 0 TO WA-KZ.
           IF WA-AB not = 0;
               IF WA-AB > BZ-BUDAT MOVE 1 TO WA-KZ
               else IF DRA-SATZ not = SPACE PERFORM DRUCK.
           IF BZ-BUDAT > WM-WJAHR(2) GO Q.
           IF BZ-KTONR NOT = DE-KTONR GO Q.
           IF WA-BIS not = 0; IF BZ-BUDAT > WA-BIS GO Q.
           PERFORM KTO-ZEIL.
           GO I.
       Q.  PERFORM KTO-ENDE.
           IF WB NOT = 99 GO G.
       W.  IF WH-PG = 9 GO Z.
           PERFORM END-DRU.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ***************************************************** Kontozeile *
       KTO-ZEIL SECTION.
       A.  PERFORM KTO-KOPF.
           MOVE 1 TO WM-ZEIL.
           MOVE BZ-BUDAT TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRK-BUDAT.
           MOVE BZ-BELNR TO DRK-BELNR.
           MOVE BZ-BELDAT TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRK-BELDAT.
           EVALUATE BZ-SYM
               WHEN 99    MOVE SPACE TO DRA-SATZ
                          GO Z
               WHEN 98    MOVE "SV" TO DRK-SYM
                          MOVE "Saldovortrag" TO DRK-BEZ
               WHEN 97    MOVE "SV" TO DRK-SYM
                          MOVE "SaldoÅbernahme" TO DRK-BEZ
               WHEN OTHER MOVE WE-SYM(BZ-SYM + 1) TO DRK-SYM.
           DIVIDE 100 INTO BZ-SEITE GIVING DRK-JS.
           MOVE BZ-TX TO DRK-BEZ.
           IF BZ-Z = 0 GO J.
           IF BZ-Z = 4 AND BZ-GGKTO = 0
                MOVE " *auft*" TO DRK-GGKTO(2:) GO J.
           IF BZ-GGKTO > 999997 MOVE "sabu" TO DRK-GGKTO(2:)
           else DIVIDE 10 INTO BZ-GGKTO GIVING DRK-GGKTO.
       J.  EVALUATE BZ-VM
               WHEN 0 CONTINUE
               WHEN 1 IF BZ-U1 NOT = 0 MOVE WT-UST(BZ-U1) TO DRK-UST
                                       MOVE "m" TO DRK-UST(3:)
                                       MOVE BZ-MW TO DRK-MWSKT
               WHEN 2 IF BZ-U1 NOT = 0 MOVE WT-UST(BZ-U1) TO DRK-UST
                      end-if IF BZ-MW NOT = 0 MOVE "v" TO DRK-UST(3:)
                                       MOVE BZ-MW TO DRK-MWSKT
               WHEN 3 IF BZ-MW NOT = 0 or BZ-B1 NOT = 0 or BZ-B2 NOT = 0
                          MOVE "Ske" TO DRK-UST(1:)
                          ADD BZ-MW BZ-B1 BZ-B2 GIVING WH-WERT
                          ADD WH-WERT TO WM-SOLL
                          MOVE WH-WERT TO DRK-MWSKT
               WHEN 4 IF BZ-MW NOT = 0 or BZ-B1 NOT = 0 or BZ-B2 NOT = 0
                          MOVE "Ska" TO DRK-UST(1:)
                          ADD BZ-MW BZ-B1 BZ-B2 GIVING WH-WERT
                          ADD WH-WERT TO WM-HABEN
                          MOVE WH-WERT TO DRK-MWSKT
               WHEN 5 IF BZ-U1 NOT = 0 MOVE WT-UST(BZ-U1) TO DRK-UST
                      end-if IF BZ-MW NOT = 0 MOVE "i" TO DRK-UST(3:)
                                       MOVE BZ-MW TO DRK-MWSKT
               WHEN 6 IF BZ-U1 NOT = 0 MOVE WT-UST(BZ-U1) TO DRK-UST
                                       MOVE "n" TO DRK-UST(3:)
                                       MOVE BZ-MW TO DRK-MWSKT
               WHEN 9 IF BZ-U1 NOT = 0 MOVE WT-UST(BZ-U1) TO DRK-UST
                                       MOVE "f" TO DRK-UST(3:)
                                       MOVE BZ-MW TO DRK-MWSKT.
           IF BZ-SH = 0 MOVE BZ-BUBET TO DRK-SOLL
                        ADD BZ-BUBET TO WM-SOLL
                   else MOVE BZ-BUBET TO DRK-HABEN WH-WERT
                        ADD BZ-BUBET TO WM-HABEN.
      *---------------------------------------> Saldoermittlung (Auszug)
           IF WA-KZ = 1 and WA-AB > BZ-BUDAT;
               MOVE SPACE TO DRK-SATZ
               MOVE "S a l d o" TO DRK-BEZ
               MOVE WA-AB TO WC-DATUM
               PERFORM DATDREH
               MOVE VDU-DATUM TO DRK-BELDAT
               IF WM-SOLL > WM-HABEN
                    COMPUTE DRK-SOLL = WM-SOLL - WM-HABEN
               else COMPUTE DRK-HABEN = WM-SOLL - WM-HABEN
               end-if
               GO Z.
           PERFORM DRUCK.
           IF BZ-OPLFD NOT = 999 GO Z.
      *-------------------------------------> Druck Zahlungszuordnung <-
           MOVE BZ-KEY TO AZ-TKEY.
           MOVE 0 TO AZ-SA.
       K.  MOVE LOW-VALUE TO AZ-SATZ(11:).
           READ AUSZIFF INVALID GO Z.
           IF ZUGRIF PERFORM BESETZT GO K.
           IF WZ-AUSZ < 22 GO Z.
           MOVE 1 TO WZ.
280506     move 0 to WS-OPS.
           PERFORM VARYING WI FROM 1 BY 1 UNTIL WI > 50
               IF AZ-OPNR(WI) NOT = 0 or AZ-OPBET(WI) NOT = 0
                   PERFORM KTO-KOPF
                   ADD AZ-OPSYM(WI) 1 GIVING IZ
                   MOVE WE-SYM(IZ) TO DRQ-SYM(WZ)
                   MOVE AZ-OPNR(WI) TO DRQ-RENR(WZ)
                   MOVE AZ-OPBET(WI) TO DRQ-BET(WZ)
                   IF AZ-OPSKTO(WI) NOT = 0
                       MOVE AZ-OPSKTO(WI) TO DRQ-SKTO(WZ)
                       MOVE "Skto." TO DRQ-TX(WZ)
                   end-if
280506             add az-opbet(wi) az-opskto(wi) to ws-ops
                   IF WZ = 2 PERFORM DRUCK
                             MOVE 1 TO WZ
                        else MOVE 2 TO WZ.
           IF WZ = 2 PERFORM DRUCK.
280506     move ws-ops to drq-sum.
           if ws-ops not = wh-wert compute drq-dif = wh-wert - ws-ops.
280506     if wm-datum = 20060528 perform druck.       *> Abgleichsumme
           MOVE SPACE TO DRA-SATZ.
       Z.  EXIT.
      ****************************************************** Kontoende *
       KTO-ENDE SECTION.
       A.  IF WM-ZEIL = 0 GO Z.
           MOVE ALL "ƒ" TO DRK-STR(1:) PERFORM DRUCK.
           IF WD-KTO(6:2) = ",2" and DE-UMSATZ not = 0
               MOVE "Warenumsatz:" TO DRS-WUTX
               MOVE DE-UMSATZ TO DRS-WUBET.
           MOVE "SUMMEN   Soll:" TO DRS-BEZ.
           MOVE WM-SOLL TO DRS-SOLL.
           MOVE "Haben:" TO DRS-HATX.
           MOVE WM-HABEN TO DRS-HABEN.
           MOVE "Saldo:" TO DRS-SALTX.
           SUBTRACT WM-HABEN FROM WM-SOLL GIVING DRS-SALDO.
           PERFORM DRUCK.
           MOVE ALL "=" TO DRS-SATZ(42:) PERFORM DRUCK.
       W.  MOVE 0 TO WM-SOLL WM-HABEN WZ-SEITE WM-ZEIL.
           IF WL = 1 MOVE 4 TO WZ-SCHALT
                else MOVE 98 TO WZ-ZEILEN.
       Z.  EXIT.
      ***************************************************** Kontenkopf *
       KTO-KOPF SECTION.
       A.  IF WZ-ZEILEN > WH-SIZE;
               IF WM-SOLL NOT = 0 OR WM-HABEN NOT = 0
                   MOVE ALL "ƒƒƒ" TO DRK-STR(1:) PERFORM DRUCK
                   PERFORM UEBERTRAG
               end-if
               WRITE DRA-SATZ AFTER PAGE
               MOVE 0 TO WZ-ZEILEN.
           IF WZ-SEITE = 0 GO C.
           IF WZ-ZEILEN > 0 GO Z.
       C.  MOVE 2 TO WH-P1.
           IF WM-KO = 0 MOVE 5 TO WH-P2
                        MOVE 39 TO WH-SIZE
                   ELSE MOVE 4 TO WH-P2
                        MOVE 63 TO WH-SIZE.
           IF WM-DRU = 1 MOVE 39 TO WH-SIZE
               MOVE 12 TO WH-P1.
           PERFORM BEG-DRU.
           ADD 1 TO WZ-SEITE.
           IF WZ-ZEILEN > 0; IF WL = 1 MOVE 2 TO WZ-SCHALT.
           MOVE "Kto.:" TO DRK-STR(1:5).
           MOVE WD-KTO TO DRK-STR(7:8).
           MOVE WT-TX TO DRK-STR(15:).
           MOVE SPACE TO DRK-STR(89:1).
           MOVE WK-FIRMA TO DRK-STR(86:24).
           MOVE WH-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRK-STR(111:10).
           MOVE "Seite:" TO DRK-STR(121:).
           MOVE WZ-SEITE TO WD-NUM.
           MOVE WD-NUM(4:) TO DRK-STR(128:).
           PERFORM DRUCK.
           MOVE ALL "ƒƒƒ" TO DRK-STR(1:) PERFORM DRUCK.
           MOVE " Buch.Dat Bel-Num. Bel.Dat Sym Gegkto.    JS T  e  x  t
      -       "                                Steuer/Skto.  kz.
      -       " SOLL         HABEN" TO DRK-STR(1:).
           IF WA-AB not = 0 MOVE "(" TO DRK-STR(60:1)
                            MOVE WA-AB TO WC-DATUM
                            PERFORM DATDREH
                            MOVE VDU-DATUM TO DRK-STR(61:8)
                            MOVE "-" TO DRK-STR(70:1)
                            MOVE WA-BIS TO WC-DATUM
                            PERFORM DATDREH
                            MOVE VDU-DATUM TO DRK-STR(72:8)
                            MOVE ")" TO DRK-STR(80:1).
           PERFORM DRUCK.
           MOVE ALL "ƒƒƒ" TO DRK-STR(1:) PERFORM DRUCK.
           IF WM-SOLL NOT = 0 OR WM-HABEN NOT = 0 PERFORM UEBERTRAG.
       Z.  EXIT.
      ******************************************************* öbertrag *
       UEBERTRAG SECTION.
       A.  MOVE "öbertrag:" TO DRK-BEZ(27:).
           MOVE WM-SOLL TO DRK-SOLL.
           MOVE WM-HABEN TO DRK-HABEN.
           PERFORM DRUCK.
       Z.  EXIT.
      ***************************************** Kontendruck Kreditoren *
       KREDKONT SECTION.
       A.  IF WH-PG not = 9 PERFORM DEBKRED
               IF WH-DRUNAM NOT = "LPT1:"
                   MOVE "KUNDKONT.LST" TO WH-DRUNAM.
           IF WH-DRUNAM NOT = "LPT1:" MOVE "LIEFKONT.LST" TO WH-DRUNAM.
           IF ESC GO X.
           EVALUATE WH-MOD
               WHEN 0 MOVE WM-KTONR TO KR-KTONR
                      START KREDIT KEY NOT < KR-KEY INVALID GO Z
                      end-start GO G
               WHEN 1 MOVE WK-MCODE TO KR-MCODE
                      START KREDIT KEY NOT < KR-MCODE INVALID GO Z.
       G.  READ KREDIT NEXT IGNORE LOCK AT END MOVE 99 TO WB GO Q.
           EVALUATE WH-MOD
               WHEN 1 IF KR-MCODE > WV-ENDE MOVE 99 TO WB GO Q
               WHEN 0 IF KR-KTONR > WV-KTONR MOVE 99 TO WB GO Q.
       H.  DIVIDE 10 INTO KR-KTONR GIVING WD-KTO.
           MOVE KR-BEZ TO WT-TX.
           INSPECT WT-TX REPLACING ALL "#" BY ",".
           DISPLAY WD-KTO with highlight AT VDU-LP " " WT-TX(1:47).
           IF WE = 1; IF KR-SOLL = KR-HABEN GO G.
           MOVE KR-KTONR TO BZ-KTONR.
           MOVE WM-WJAHR(1) TO BZ-BUDAT.
           MOVE 0 TO BZ-LFD WM-ZEIL.
           START BUCHZEIL KEY NOT < BZ-KEY INVALID GO G.
       I.  MOVE SPACE TO BZ-TX.
           READ BUCHZEIL NEXT AT END GO Q.
           IF ZUGRIF PERFORM BESETZT GO I.
           MOVE 0 TO WA-KZ.
           IF WA-AB not = 0;
               IF WA-AB > BZ-BUDAT MOVE 1 TO WA-KZ
               else IF DRA-SATZ not = SPACE PERFORM DRUCK.
           IF BZ-BUDAT > WM-WJAHR(2) GO Q.
           IF BZ-KTONR NOT = KR-KTONR GO Q.
           IF WA-BIS not = 0; IF BZ-BUDAT > WA-BIS GO Q.
           PERFORM KTO-ZEIL.
           GO I.
       Q.  PERFORM KTO-ENDE.
           IF WB NOT = 99 GO G.
       W.  IF WH-PG = 9 GO Z.
           PERFORM END-DRU.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ******************************************** Saldendurchrechnung *
       SACHRECH SECTION.
       A.  MOVE 0 TO SA-KTONR.
           START SACHBUCH KEY NOT < SA-KEY INVALID GO Z.
       G.  READ SACHBUCH NEXT NO LOCK AT END MOVE 0 TO BZ-BUDAT GO Q.
           IF ZUGRIF PERFORM BESETZT GO G.
           MOVE SA-KTONR TO BZ-KTONR.
           DISPLAY SA-KTONR AT 1901.
           MOVE 0 TO BZ-BUDAT BZ-LFD WM-SOLL WM-HABEN.
           START BUCHZEIL KEY NOT < BZ-KEY INVALID GO Z.
       H.  READ BUCHZEIL NEXT AT END GO W.
           IF ZUGRIF PERFORM BESETZT GO H.
           IF SA-KTONR NOT = BZ-KTONR GO Q.
           IF BZ-SH = 0 ADD BZ-BUBET TO WM-SOLL
                   else ADD BZ-BUBET TO WM-HABEN.
           GO H.
       Q.  IF WM-SOLL NOT = SA-SOLL OR WM-HABEN NOT = SA-HABEN
               DISPLAY SA-SOLL AT 1920
               DISPLAY SA-HABEN AT 1940
               DISPLAY WM-SOLL AT 2020
               DISPLAY WM-HABEN AT 2040
               CALL "CAUP" USING "0023300000" WH-CREG.
           MOVE WM-SOLL TO SA-SOLL.
           MOVE WM-HABEN TO SA-HABEN.
           REWRITE SA-SATZ.
           IF BZ-BUDAT = 0 GO Z.
           GO G.
       W.  REWRITE SA-SATZ.
       Z.  EXIT.
      ******************************************** Saldendurchrechnung *
       DEBRECH SECTION.
       A.  MOVE 0 TO DE-KTONR.
           START DEBITOR KEY NOT < DE-KEY INVALID GO Z.
       G.  READ DEBITOR NEXT NO LOCK AT END MOVE 0 TO BZ-BUDAT GO Q.
           IF ZUGRIF PERFORM BESETZT GO G.
           MOVE DE-KTONR TO BZ-KTONR.
           DISPLAY DE-KTONR AT 1901.
           MOVE 0 TO BZ-BUDAT BZ-LFD WM-SOLL WM-HABEN.
           START BUCHZEIL KEY NOT < BZ-KEY INVALID GO Z.
       H.  READ BUCHZEIL NEXT AT END GO W.
           IF ZUGRIF PERFORM BESETZT GO H.
           IF DE-KTONR NOT = BZ-KTONR GO Q.
           IF BZ-VM = 4 ADD BZ-MW BZ-B1 BZ-B2 TO WM-HABEN.
           IF BZ-SH = 0 ADD BZ-BUBET TO WM-SOLL
                   else ADD BZ-BUBET TO WM-HABEN.
           GO H.
       Q.  IF WM-SOLL NOT = DE-SOLL OR WM-HABEN NOT = DE-HABEN
               DISPLAY WM-SOLL AT 1920
               DISPLAY WM-HABEN AT 1940
               DISPLAY DE-SOLL AT 2020
               DISPLAY DE-HABEN AT 2040
               CALL "CAUP" USING "0023300000" WH-CREG.
           MOVE WM-SOLL TO DE-SOLL.
           MOVE WM-HABEN TO DE-HABEN.
           REWRITE DE-SATZ.
           IF BZ-BUDAT = 0 GO Z.
           GO G.
       W.  REWRITE DE-SATZ.
       Z.  EXIT.
      ******************************************** Saldendurchrechnung *
       KREDRECH SECTION.
       A.  MOVE 0 TO KR-KTONR.
           START KREDIT KEY NOT < KR-KEY INVALID GO Z.
       G.  READ KREDIT NEXT NO LOCK AT END MOVE 0 TO BZ-BUDAT GO Q.
           IF ZUGRIF PERFORM BESETZT GO G.
           MOVE KR-KTONR TO BZ-KTONR.
           DISPLAY KR-KTONR AT 1901.
           MOVE 0 TO BZ-BUDAT BZ-LFD WM-SOLL WM-HABEN.
           START BUCHZEIL KEY NOT < BZ-KEY INVALID GO Z.
       H.  READ BUCHZEIL NEXT AT END GO W.
           IF ZUGRIF PERFORM BESETZT GO H.
           IF KR-KTONR NOT = BZ-KTONR GO Q.
           IF BZ-VM = 3 ADD BZ-MW BZ-B1 BZ-B2 TO WM-SOLL.
           IF BZ-SH = 0 ADD BZ-BUBET TO WM-SOLL
                   else ADD BZ-BUBET TO WM-HABEN.
           GO H.
       Q.  IF WM-SOLL NOT = KR-SOLL OR WM-HABEN NOT = KR-HABEN
               DISPLAY WM-SOLL AT 1920
               DISPLAY WM-HABEN AT 1940
               DISPLAY KR-SOLL AT 2020
               DISPLAY KR-HABEN AT 2040
               CALL "CAUP" USING "0023300000" WH-CREG.
           MOVE WM-SOLL TO KR-SOLL.
           MOVE WM-HABEN TO KR-HABEN.
           REWRITE KR-SATZ.
           IF BZ-BUDAT = 0 GO Z.
           GO G.
       W.  REWRITE KR-SATZ.
       Z.  EXIT.
      ******************************************* lîschen altes W-Jahr *
       ALTJAHR SECTION.
       A.  CALL "CAUP" USING "270710056000005" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " lîschen altes W-Jahr " with highlight AT VDU-LP.
           ADD 140 VDU-ECK GIVING VDU-LP.
           IF WH-PG = 7
                DISPLAY " nur Sachkonten " with highlight AT VDU-LP
           else DISPLAY " alle Konten " with highlight AT VDU-LP.
       C.  CALL "CAUP" USING "16CLRFEN" WH-CREG.
           ADD 225 VDU-ECK GIVING VDU-LP.
           DISPLAY "Gelîscht wird bis inkl.: " AT VDU-LP
                VDU-DATUM with highlight.
           ADD 403 VDU-ECK GIVING VDU-LP.
           DISPLAY "Vorgang kann nicht rÅckgÑngig gemacht werden!"
                with highlight blink AT VDU-LP.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "<Entf>= lîschen, <ret/esc>= Abbruch < >" AT VDU-LP.
           CALL "CAUP" USING "1003400000" WH-CREG.
           IF ESC OR RET GO X.
           IF NOT ENTF GO C.
           ADD 403 VDU-ECK GIVING VDU-LP.
           CALL "CAUP" USING "1300000057" WH-CREG.
      *-----------------------------------> eigentlicher Lîschvorgang <-
           MOVE LOW-VALUE TO BZ-KEY.
           ADD 410 VDU-ECK GIVING VDU-LP.
           START BUCHZEIL KEY NOT < BZ-KEY INVALID GO X.
       G.  READ BUCHZEIL NEXT AT END GO Q
           IF ZUGRIF PERFORM BESETZT GO G.
           MOVE BZ-BUDAT TO WC-DATUM.
           PERFORM DATDREH.
           DIVIDE 10 INTO BZ-KTONR GIVING WD-KTO.
           DISPLAY VDU-DATUM AT VDU-LP " / " WD-KTO.
           IF BZ-BUDAT > WM-WJAHR(2) GO G.
           MOVE BZ-KTONR TO WD-KZ.
           IF WH-PG = 7 AND WD-KZ > 1 GO G.
       I.  DELETE BUCHZEIL INVALID GO G.
           IF ZUGRIF PERFORM BESETZT GO I.
           GO G.
       Q.  IF WH-PG not = 6 GO X.
           MOVE 9 TO WH-KEY.
           READ KONSTANT INVALID GO X.
           IF ZUGRIF PERFORM BESETZT GO Q.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 4 or
               KO-WEND(WX) = WM-WJAHR(2) CONTINUE.
           IF WX = 4 GO X.
           PERFORM VARYING WZ FROM 1 BY 1 UNTIL WX > 4
               MOVE KO-WIJ(WX) TO KO-WIJ(WZ)
               ADD 1 TO WX.
           PERFORM VARYING WZ FROM WZ BY 1 UNTIL WZ > 4
               MOVE LOW-VALUE TO KO-WIJ(WZ).
           REWRITE KO-SATZ.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ******************************************************************
       LOETX SECTION.
       A.  CALL "CAUP" USING "16CLRFEN" WH-CREG.
           ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "Nur Konten die keine Bewegungen aufweisen werden gel
      -        "îscht!" AT VDU-LP.
       Z.  EXIT.
      ***************************************** lîschen Personenkonten *
       LOEPERS SECTION.
       A.  CALL "CAUP" USING "0707100563000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Lîschung Personenkonten " with highlight AT VDU-LP.
       C.  PERFORM LOETX.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "1- Kunden" AT VDU-LP.
           ADD 403 VDU-ECK GIVING VDU-LP.
           DISPLAY "2- Lieferanten" AT VDU-LP.
           DISPLAY "<esc>= Abbruch, <ret>= Auswahl < >" AT 2301.
           CALL "CAUP" USING "0023331001" WH-CREG.
           IF ESC GO X.
           IF NOT RET GO C.
           PERFORM LOETX.
           ADD 303 VDU-ECK GIVING VDU-LP.
           EVALUATE WH-NUM
               WHEN 1 DISPLAY "Debitoren" with highlight AT VDU-LP
               WHEN 2 DISPLAY "Kreditoren" with highlight AT VDU-LP
               WHEN OTHER GO C.
           MOVE WH-NUM TO WG.              *> Kunden / Lieferanten
           ADD 314 VDU-ECK GIVING VDU-LP.
           DISPLAY "Kateg.: " AT VDU-LP.
       E.  DISPLAY "<esc>= Abbruch, <ret>= Kategorie: 0/1/2" AT 2301.
           CALL "CAUP" USING "1003221001" WH-CREG.
           IF ESC GO X.
           IF NOT RET OR WH-NUM > 2 GO E.
           MOVE WH-NUM TO WH-KAT WD-KZ.
           DISPLAY WD-KZ with highlight AT VDU-LP.
       G.  DISPLAY "<esc>= Abbruch, <ret>= Start < >" AT 2301.
           CALL "CAUP" USING "0023310000" WH-CREG.
           IF ESC GO X.
           IF NOT RET GO G.
      *----------------------------------------------> Start Lîschung <-
           IF WG = 1 PERFORM LOEKUSTAM.
           IF WG = 2 PERFORM LOELFSTAM.
           UNLOCK DEBITOR.
           UNLOCK KREDIT.
           UNLOCK BUCHZEIL.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ******************************************** lîschen Kundenstamm *
       LOEKUSTAM SECTION.
       A.  MOVE 0 TO DE-KTONR.
           START DEBITOR KEY NOT < DE-KEY INVALID GO Z.
       C.  READ DEBITOR NEXT AT END GO Z.
           IF ZUGRIF PERFORM BESETZT GO C.
           ADD 403 VDU-ECK GIVING VDU-LP.
           INSPECT DE-BEZ REPLACING ALL "#" BY ",".
           DISPLAY DE-BEZ(1:58) AT VDU-LP.
           IF DE-SOLL NOT = 0 OR DE-HABEN NOT = 0 GO C.
           IF DE-KTOART NOT = WH-KAT GO C.
           MOVE DE-KTONR TO BZ-KTONR WH-KTONR.
           PERFORM OB-BEWEG.
           IF ESC GO C.
           DELETE DEBITOR INVALID GO C.
           GO C.
       Z.  EXIT.
      *************************************** lîschen Lieferantenstamm *
       LOELFSTAM SECTION.
       A.  MOVE 0 TO KR-KTONR.
           START KREDIT KEY NOT < KR-KEY INVALID GO Z.
       C.  READ KREDIT NEXT AT END GO Z.
           IF ZUGRIF PERFORM BESETZT GO C.
           ADD 403 VDU-ECK GIVING VDU-LP.
           INSPECT KR-BEZ REPLACING ALL "#" BY ",".
           DISPLAY KR-BEZ(1:55) AT VDU-LP.
           IF KR-SOLL NOT = 0 OR KR-HABEN NOT = 0 GO C.
           IF KR-KTOART NOT = WH-KAT GO C.
           MOVE KR-KTONR TO BZ-KTONR WH-KTONR.
           PERFORM OB-BEWEG.
           IF ESC GO C.
           DELETE KREDIT INVALID GO C.
           GO C.
       Z.  EXIT.
      ********************************************* Bewegungskontrolle *
       OB-BEWEG SECTION.
       A.  MOVE 0 TO BZ-BUDAT BZ-LFD.
           SET RET TO TRUE.
           START BUCHZEIL KEY NOT < BZ-KEY INVALID GO Z.
       C.  READ BUCHZEIL NEXT NO LOCK AT END GO Z.
           IF ZUGRIF PERFORM BESETZT GO C.
           IF BZ-KTONR = WH-KTONR SET ESC TO TRUE.
       Z.  EXIT.
      *************************************** Druck ohne Steuerzeichen *
       FIN-DRUCK SECTION.
       A.  CALL "CAUP" USING "07070806710000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           MOVE "KON-mmjj.FIN" TO WH-DRUNAM.
           DISPLAY " Kontendruck fÅr DatentrÑger " with highlight
               AT VDU-LP.
           MOVE 0 TO WA-AB WA-BIS.
           MOVE WM-WJAHR(1) TO WZ-DATUM.
           MOVE WZ-JAHR TO WH-DRUNAM(7:2).
           MOVE WZ-MONAT TO WH-DRUNAM(5:2).
           ADD 305 VDU-ECK GIVING VDU-LP.
           MOVE WM-WJAHR(1) TO WC-DATUM.
           PERFORM DATDREH.
           DISPLAY "von: " AT VDU-LP VDU-DATUM with highlight
               foreground-color 4.
           ADD 322 VDU-ECK GIVING VDU-LP.
           MOVE WM-WJAHR(2) TO WC-DATUM.
           PERFORM DATDREH.
           DISPLAY "bis: " AT VDU-LP VDU-DATUM with highlight
               foreground-color 4.
           ADD 250 VDU-ECK GIVING VDU-LP.
           DISPLAY WH-DRUNAM with highlight AT VDU-LP.
       C.  DISPLAY "<esc>= Abbruch, <ret/#>= Start < >" AT 2301.
           DISPLAY "<#> statt <ret>= mit Personenkonten" AT 2401.
           MOVE 0 TO WG.
           CALL "CAUP" USING "0023330000" WH-CREG.
           IF ESC GO X.
           IF KIST MOVE 1 TO WG
                   SET RET TO TRUE.
           IF not RET GO C.
           INITIALIZE SA-SATZ DE-SATZ KR-SATZ.
           PERFORM SAKODRU.
           MOVE 0 TO WH-MOD WM-KTONR WB.
           MOVE 999999 TO WV-KTONR.
           IF WG = 0 GO Q.
           PERFORM DEBKONT.
           MOVE 0 TO WH-MOD WM-KTONR WB.
           MOVE 999999 TO WV-KTONR.
           PERFORM KREDKONT.
       Q.  PERFORM END-DRU.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
