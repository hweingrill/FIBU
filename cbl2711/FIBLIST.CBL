      $SET LINKCOUNT "240" ANS85 AUTOLOCK BOUND NOALTER
      $SET SEGSIZE"52000"
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     FIBLIST.
      ******************************************************************
      *        Journalende mit autom. Sammelbuchungen
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
           SELECT EXCELIS  ASSIGN TO PRINTER WH-FIBNAM
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
       01  DRP-SATZ.
           03  FILLER                  PIC X(9).
           03  DRP-STR.
               05 DRP-KTONR            PIC ZZ.ZZ9,9-.
               05 DRP-BEZ              PIC X(51).
               05 DRP-KZ               PIC XX.
               05 DRP-SOLL             PIC ZZZ.ZZZ.ZZ9,99-.
               05 DRP-HABEN            PIC ZZZ.ZZZ.ZZ9,99-.
               05 DRP-SALDO            PIC ZZZ.ZZZ.ZZ9,99-.
       01  DRO-SATZ.
           03  FILLER                  PIC X(18).
           03  DRO-DAT                 PIC X(9).
           03  DRO-SYM                 PIC XXX.
           03  DRO-REFNR               PIC ZZZZZZ9-.
           03  DRO-REBET               PIC ZZZZZ.ZZ9,99-.
           03  DRO-ZABET               PIC ZZZZZ.ZZ9,99-.
           03  DRO-OFFEN               PIC ZZZZZ.ZZ9,99-.
           03  FILLER                  PIC XX.
           03  DRO-FAELLIG             PIC X(9).
           03  DRO-ZAHL                PIC XXX.
       01  DRS-SATZ.
           03  FILLER                  PIC X(4).
           03  DRS-STR.
               05 DRS-KTONR            PIC 99.999,9-.
               05 DRS-BEZ.
                  07 DRS-TX            PIC X(38).
                  07 DRS-EB            PIC ZZZ.ZZZ.ZZ9,99-.
               05 DRS-MSOLL            PIC ZZZ.ZZZ.ZZ9,99-.
               05 DRS-MHABEN           PIC ZZZ.ZZZ.ZZ9,99-.
               05 DRS-GSOLL            PIC ZZZ.ZZZ.ZZ9,99-.
               05 DRS-GHABEN           PIC ZZZ.ZZZ.ZZ9,99-.
      ******************************************************************
       FD  EXCELIS                     LABEL RECORD OMITTED.
       01  EXC-SATZ.
           03  EXC-KTONR               PIC ZZZZ.
           03  EXC-TA                  PIC X.
           03  EXC-BEZ                 PIC X(50).
           03  EXC-TB                  PIC X.
           03  EXC-MBET                PIC ZZZZZZZZ9,99-.
           03  EXC-TC                  PIC X.
           03  EXC-GBET                PIC ZZZZZZZZ9,99-.
           03  EXC-TD                  PIC X.
           03  EXC-KER                 PIC ZZ9.
       01  EXO-SATZ.
           03  EXO-BELEG               PIC 999999.
           03  EXO-TA                  PIC X.
           03  EXO-ZADAT               PIC 9(8).
           03  EXO-TB                  PIC X.
           03  EXO-ZABET               PIC ZZZZZZ9,99-.
           03  EXO-TC                  PIC X.
           03  EXO-OBET                PIC ZZZZZZ9,99-.
      ******************************************************************
       WORKING-STORAGE SECTION.
       01  WH-CALL.
           03  WL-CA                   PIC 99.
           03  WL-REST                 PIC 9(13).
       COPY WHCREG.CPY.
       01  WH-REG.
           03  WK-DSTG.
               05 WK-STG               PIC X(10)     OCCURS 10.
           03  WM-FIN                  PIC 99        COMP.
           03  WH-P1                   PIC 99        COMP.
           03  WH-P2                   PIC 99        COMP.
           03  WH-KB                   PIC 99        COMP-X.
           03  WX-PRNO                 PIC 99        COMP-X.
           03  WX-PRSTAT               PIC 99        COMP-X.
           03  WZ-SEITE                PIC 99        COMP   VALUE ZERO.
           03  WZ-SCHALT               PIC 99        COMP   VALUE ZERO.
           03  WZ-ZEILEN               PIC 99        COMP   VALUE ZERO.
           03  WH-DRUNAM               PIC X(12)     VALUE   "LPT1:".
           03  WH-FIBNAM               PIC X(12).
           03  WM-FIBEX                PIC 9         COMP   VALUE ZERO.
           03  WM-OPEN                 PIC 9         COMP   VALUE ZERO.
           03  WH-SIZE                 PIC 99        COMP   VALUE 62.
           03  WZ-BREIT                PIC 999       COMP.
           03  WZ-GROSS                PIC 9(4).
           03  WH-MOD                  PIC 99        COMP.
           03  WH-VON                  PIC 9(6)      COMP.
           03  WH-BIS                  PIC 9(6)      COMP.
           03  WH-VAR                  PIC 99        COMP.
           03  WM-ART                  PIC 99        COMP.
           03  WM-LOE                  PIC 99        COMP.
           03  WB                      PIC 99        COMP.
           03  WP                      PIC 99        COMP.
           03  WE                      PIC 99        COMP.
           03  WI                      PIC 99        COMP.
           03  IX                      PIC 99        COMP.
           03  WG                      PIC 99        COMP.
           03  WV-BISJ                 PIC 9(5)      COMP.
           03  WS-REG.
               05 WZ-SOLL              PIC S9(9)V99  COMP  OCCURS 4.
               05 WZ-HABEN             PIC S9(9)V99  COMP  OCCURS 4.
               05 WG-SOLL              PIC S9(9)V99  COMP  OCCURS 4.
               05 WG-HABEN             PIC S9(9)V99  COMP  OCCURS 4.
           03  WM-TEIL                 PIC S9(9)V99  COMP.
           03  WM-SOLL                 PIC S9(9)V99  COMP.
           03  WM-HABEN                PIC S9(9)V99  COMP.
           03  WM-UMSATZ               PIC S9(9)V99  COMP.
           03  WM-EBIL                 PIC S9(9)V99  COMP.
           03  WV-SOLL                 PIC S9(9)V99  COMP.
           03  WV-HABEN                PIC S9(9)V99  COMP.
           03  WV-EBIL                 PIC S9(9)V99  COMP.
           03  WS-OPS                  PIC S9(9)V99  COMP.
           03  WS-EUR                  PIC S9(9)V99  COMP.
           03  WM-EUR                  PIC S9(9)V99  COMP.
           03  WS-ART                  PIC 9(5)      COMP   OCCURS 3.
           03  wj-zeil                 pic 9(6)      COMP-x.
           03  WV-KTONR                PIC 9(6)      COMP.
           03  WV-VDAT                 PIC 9(6).
           03  WV-BDAT                 PIC 9(6).
           03  WH-BKEY.
               05 WH-KTONR             PIC 9(6)      COMP.
               05 WH-BUDAT             PIC 9(8)      COMP.
               05 WH-LFD               PIC 9999      COMP.
           03  WH-KAT                  PIC 9         COMP.
           03  WK-JS                   PIC 9(7).
           03  WR-JS REDEFINES WK-JS.
               05 WK-SEITE             PIC 99999.
               05 WK-ZEIL              PIC 99.
           03  WV-SEITE                PIC 99999.
           03  wx-seite                pic 9(6)      COMP.
           03  WV-ABDAT                PIC 9(8)      COMP.
           03  WV-BISDAT               PIC 9(8)      COMP.
           03  WV-SALDAT               PIC 9(8)      COMP.
           03  WV-WBEG                 PIC 9(8)      COMP.
           03  WV-WEND                 PIC 9(8)      COMP.
           03  WV-WKZ                  PIC 99        COMP.
           03  WH-BZSATZ               PIC X(120).
      *-----------------------------------------------> Anzeigefelder <-
           03  WD-JONUM                PIC Z.ZZ9-.
           03  WD-SALDO                PIC ZZ.ZZZ.ZZ9,99-.
           03  WD-UMSATZ               PIC ZZZ.ZZZ.ZZ9,99-.
           03  WD-NUM                  PIC ZZZZZ9.
           03  WD-KTO                  PIC ZZZZ9,9.
           03  WD-KZ                   PIC 9.
           03  WD-PZ                   PIC 99.
      *------------------------------------------> Konditionentabelle <-
           03  WH-KK                   PIC 9(13).
           03  WR-KK REDEFINES WH-KK.
               05  WH-SK1              PIC 9V9.
               05  WH-TG1              PIC 999.
               05  WH-SK2              PIC 9V9.
               05  WH-TG2              PIC 999.
               05  WH-TGN              PIC 999.
           03  WH-VALUTA               PIC 9(6)      COMP.
           03  WA-TAGE                 PIC 9(7)      COMP.
           03  WB-TAGE                 PIC 9(8)      COMP.
           03  WH-TAG                  PIC S999      COMP.
       COPY FIBUEXT.CPY.
       COPY FIBUDECL.CPY.
       END DECLARATIVES.
      *****************************************************************
       STEUER SECTION.
       A.  MOVE WL-CALL TO WH-CALL.
           MOVE WL-CREG TO WH-CREG.
           EVALUATE WL-CA
               WHEN 70 PERFORM LI-WAHL.
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
       A.  IF WM-DRU = 1 MOVE DRA-STR TO DRA-SATZ(4:).
       C.  WRITE DRA-SATZ AFTER WZ-SCHALT.
           IF WF-STATUS = 27 GO C.
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
           OPEN EXTEND DRUCKER.
       C.  MOVE 0 TO WZ-ZEILEN WZ-SCHALT.
           IF WM-FIN = 1 GO Z.                          *> Finanzexport
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
           READ KONSTANT WITH NO LOCK not INVALID GO X.
           IF ZUGRIF PERFORM BESETZT GO A.
       X.  MOVE KOP-TX TO WK-DSTG.
       Z.  EXIT.
      ******************************************************************
       LI-WAHL SECTION.
       A.  MOVE 68 TO WH-KEY WM-LOE.
           READ KONSTANT NO LOCK INVALID MOVE 0 TO WM-LOE.
           IF ZUGRIF PERFORM BESETZT GO A.
           CALL "CAUP" USING "0704111948000" WH-CREG.
           IF WM-DRU = 0 DISPLAY "Nadeldrucker (Epson)" AT 0358
                    else DISPLAY "Laserdrucker (LJ4)" AT 0358.
       B.  PERFORM LADE-DRU.
           MOVE 9 TO WH-PG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Fibu-Listungen " with highlight AT VDU-LP.
           ADD 308 VDU-ECK GIVING VDU-LP.
           DISPLAY "1 - Saldenliste Sachbuch"   AT 0617.
           DISPLAY "2 - Saldenliste Debitoren"  AT 0017.
           DISPLAY "3 - Saldenliste Kreditoren" AT 0017.
           DISPLAY "4 - Journalwiederholung"    AT 0017.
           DISPLAY "5 - Kontendruck"            AT 0017.
           DISPLAY "6 - Jahresabschlu·"         AT 0017.
           DISPLAY "7 - OP-Liste Debitoren"     AT 0017.
           DISPLAY "8 - OP-Liste Kreditoren"    AT 0017.
           DISPLAY "9 - OP-Lîschung"            AT 0017.
           DISPLAY "10 - Ust-Ermittlung"        AT 0016.
           DISPLAY "11 - Liste Ust-Buchungen"   AT 0016.
           DISPLAY "12 - WarenumsÑtze"          AT 0016.
           DISPLAY "13 - Innerg. Erwerb"        AT 0016.
           DISPLAY " " AT 0016.
           DISPLAY "bitte wÑhlen Sie:" with highlight AT 0032.
      *    DISPLAY "13 - lîfib" AT 0370.
           IF WM-LOE > 0 DISPLAY "/Getr.Abg.-Ermittlung" AT 1625.
       C.  DISPLAY "<ret-leer>= Programmende" AT 2301.
           CALL "CAUP" USING "1017402002" WH-CREG.
           MOVE WH-NUM TO WH-PG.
           MOVE 0 TO WM-LOE WM-FIN.
           IF ESC OR WH-PG = 0 GO X.
           IF not RET GO C.
           EVALUATE WH-PG
               WHEN 1 PERFORM SAKOLI
               WHEN 2 PERFORM DEBLIS
               WHEN 3 PERFORM KREDLIS
               WHEN 4 PERFORM JOURDRUCK
               WHEN 5 MOVE 49 TO WH-PG
                      MOVE "FIBKONTO10KTODRU" TO WT-TX GO X
               WHEN 6 PERFORM JAHRABS
               WHEN 7 PERFORM DEBOPS
               WHEN 8 PERFORM KREDOPS
               WHEN 9 PERFORM OPLOESCH
               WHEN 10 MOVE 49 TO WH-PG
                       MOVE "FIBGUST 10USTRE" TO WT-TX GO X
               WHEN 11 MOVE 49 TO WH-PG
                       MOVE "FIBGUST 20USTLI" TO WT-TX GO X
               WHEN 12 MOVE 49 TO WH-PG
                       MOVE "FIBGUST 30WAREN" TO WT-TX GO X
               WHEN 13 MOVE 49 TO WH-PG
                       MOVE "FIBGUST 40IGERW" TO WT-TX GO X
               WHEN 43 PERFORM LOEFIB
               WHEN 46 MOVE 49 TO WH-PG
                       MOVE "FICV200050LIST" TO WT-TX GO X.
           GO B.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           UNLOCK KONSTANT.
           UNLOCK BUCHZEIL.
       Z.  EXIT.
      ************************************** Wirtschatfsjahr ermitteln *
       WIJAHR SECTION.
       A.  MOVE 9 TO WH-KEY.
           READ KONSTANT NO LOCK INVALID STOP RUN.
           IF ZUGRIF PERFORM BESETZT GO A.
           PERFORM VARYING WI FROM 1 BY 1 UNTIL WI = 4
               OR WV-DATUM <= KO-WEND(WI) CONTINUE.
           MOVE KO-WBEG(WI) TO WV-DATUM.
       Z.  EXIT.
      *********************************************** Ultimoermittlung *
       ULTIMO SECTION.
       A.  MOVE 32 TO WZ-TAG.
           MOVE WZ-DATUM TO WV-DATUM.
       C.  MOVE WV-DATUM TO WZ-DATUM.
           ADD -1 TO WZ-TAG.
           MOVE WZ-DATUM TO WV-DATUM.
           CALL "CAUP" USING "05DATPRF" WH-CREG.
           IF WZ-DATUM = 0 GO C.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           MOVE WX-DATUM TO WS-DATUM.
           MOVE 1 TO WZ-TAG.
           CALL "CAUP" USING "05DATPRF" WH-CREG.
           MOVE WX-DATUM TO WV-DATUM.
       Z.  EXIT.
      ************************************************ Sachkontenliste *
       SAKOLI SECTION.
       A.  CALL "CAUP" USING "0707080550000" WH-CREG.
           MOVE 0 TO WP WM-OPEN WM-FIBEX.
       B.  ADD 103 VDU-ECK GIVING VDU-LP.
           MOVE "SACHLIST.LST" TO WH-DRUNAM.
           DISPLAY " Sachkontenlistung " with highlight AT VDU-LP.
           ADD 23 TO VDU-P.
           EVALUATE WP
               WHEN 0 DISPLAY "   Salden   " AT VDU-LP
               WHEN 1 DISPLAY "   UmsÑtze  " AT VDU-LP
               WHEN 2 DISPLAY " akk.Salden " AT VDU-LP
               WHEN 3 DISPLAY " EB/akk.Sal." AT VDU-LP.
           ADD 225 VDU-ECK GIVING VDU-LP.
           DISPLAY "Periodenende:" AT VDU-LP.
           IF WS-DATUM = 0 MOVE WM-DATUM TO WS-DATUM.
           MOVE WS-DATUM TO WZ-DATUM.
           ADD 239 VDU-ECK GIVING VDU-LP.
           PERFORM ULTIMO.
           DISPLAY "<esc>= Abbruch, <ret>= Datum, 0+<ret>= Gesamtlistung
      -        "" AT 2301.
           DISPLAY "<#>= Wechsel Salden/UmsÑtze/akk.Salden/EB+akk.Salden
      -        "" AT 2401.
           MOVE WM-DATUM TO WZ-DATUM WH-WERT.
           CALL "CAUP" USING "1102396006" WH-CREG.
           IF ESC GO X.
           IF KIST; ADD 1 TO WP
                    IF WP > 3 MOVE 0 TO WP.
           IF not RET GO B.
           IF WZ-DATUM = 0 MOVE 0 TO WV-DATUM WS-DATUM
               CALL "CAUP" USING "16CLRFEN" WH-CREG GO F.
           PERFORM ULTIMO.
           IF WP not = 0 PERFORM WIJAHR.
       F.  ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "<ret>= Start, <esc>= Abbruch < >" AT VDU-LP.
           DISPLAY "tab 0/1" AT 2401.
           CALL "CAUP" USING "1003336006" WH-CREG.
           IF ESC GO X.
           IF TABL PERFORM SACHRECH.
           IF not RET GO F.
           PERFORM OB-OFFJOUR.
           IF ESC GO X.
           ADD 403 VDU-ECK GIVING VDU-LP.
           INITIALIZE WS-REG.
      *    MOVE 0 TO WZ-SOLL(1) WZ-HABEN(1) WG-SOLL(1) WG-HABEN(1)
      *              WZ-SOLL(2) WZ-HABEN(2) WG-SOLL(2) WG-HABEN(2).
           MOVE 99 TO WG.
           MOVE 0 TO SA-KTONR WZ-SEITE WB WZ-ZEILEN WR.
           START SACHBUCH KEY not < SA-KEY INVALID GO Z.
       G.  READ SACHBUCH NEXT NO LOCK AT END MOVE 99 TO WB
               PERFORM SACHGRU GO Q.
           IF ZUGRIF PERFORM BESETZT GO G.
           MOVE 0 TO WM-SOLL WM-HABEN WM-EBIL.
           IF WP = 1 OR WS-DATUM not = 0 PERFORM PER-SACH.
           MOVE SA-KTONR TO WH-KAT.
           IF WH-KAT > 0 GO Q.
           PERFORM SAKO-KOPF.
           EVALUATE WM-SK
              WHEN 3 DIVIDE 1000 INTO SA-KTONR GIVING WF
              WHEN 4 DIVIDE 10000 INTO SA-KTONR GIVING WF
              WHEN 5 DIVIDE 100000 INTO SA-KTONR GIVING WF.
           IF WG = 99 MOVE WF TO WG
                      MOVE 1 TO WR.
           IF WF not = WG PERFORM SACHGRU
               MOVE WF TO WG.
           IF WM-DRU = 1 PERFORM SAKO-KOPF
                         MOVE 122 TO WZ-BREIT
                         PERFORM RASTRI.
           DIVIDE 10 INTO SA-KTONR GIVING DRS-KTONR.
           EVALUATE WM-SK
               WHEN 3 MOVE SPACE TO DRS-KTONR(1:3)
               WHEN 4 MOVE SPACE TO DRS-KTONR(1:1).
           MOVE SA-TX(1) TO WT-TX.
           PERFORM VARYING IX FROM 26 BY -1 UNTIL IX = 0
               OR WT-TX(IX:1) not = SPACE CONTINUE.
           IF WT-TX(IX:1) = "-" ADD 1 TO IX
                           else ADD 2 TO IX.
           MOVE SA-TX(2) TO WT-TX(IX:25).
           MOVE WT-TX TO DRS-BEZ.
           DISPLAY DRS-KTONR with highlight AT VDU-LP SA-TX(1).
           ADD SA-KTOART 3 GIVING WY.
           IF WM-EBIL not = 0 MOVE WM-EBIL TO DRS-EB.
      *---------------------------------> SachkontenumsÑtze und Saldo <-
           IF WS-DATUM = 0;
               IF SA-SOLL not = 0 MOVE SA-SOLL TO DRS-MSOLL
                   ADD SA-SOLL TO WZ-SOLL(1) WG-SOLL(1) WG-SOLL(WY)
               end-if
               IF SA-HABEN not = 0 MOVE SA-HABEN TO DRS-MHABEN
                   ADD SA-HABEN TO WZ-HABEN(1) WG-HABEN(1) WG-HABEN(WY)
               end-if
               IF SA-SOLL > SA-HABEN SUBTRACT SA-HABEN FROM SA-SOLL
                    MOVE 0 TO SA-HABEN
                    MOVE SA-SOLL TO DRS-GSOLL
                    ADD SA-SOLL TO WG-SOLL(2) WZ-SOLL(2) WZ-SOLL(WY)
               else SUBTRACT SA-SOLL FROM SA-HABEN
                    MOVE 0 TO SA-SOLL
                    MOVE SA-HABEN TO DRS-GHABEN
                    ADD SA-HABEN TO WG-HABEN(2) WZ-HABEN(2) WZ-HABEN(WY)
      *------------------------------> Periodensalden u. Gesamtsalden <-
           else IF WM-SOLL not = 0 MOVE WM-SOLL TO DRS-MSOLL
                    MOVE WM-SOLL TO DRS-MSOLL
                    MOVE WM-SOLL TO EXC-MBET
                    ADD WM-SOLL TO WG-SOLL(1) WZ-SOLL(1) WG-SOLL(WY)
                end-if
                IF WM-HABEN not = 0 MOVE WM-HABEN TO DRS-MHABEN
                    MOVE WM-HABEN TO DRS-MHABEN
                    COMPUTE EXC-MBET = WM-HABEN * -1
                    ADD WM-HABEN TO WG-HABEN(1) WZ-HABEN(1) WG-HABEN(WY)
                end-if
                IF SA-SOLL > SA-HABEN SUBTRACT SA-HABEN FROM SA-SOLL
                    MOVE 0 TO SA-HABEN
                    MOVE SA-SOLL TO DRS-GSOLL
                    MOVE SA-SOLL TO EXC-GBET
                    ADD SA-SOLL TO WG-SOLL(2) WZ-SOLL(2) WZ-SOLL(WY)
                else SUBTRACT SA-SOLL FROM SA-HABEN
                    MOVE 0 TO SA-SOLL
                    MOVE SA-HABEN TO DRS-GHABEN
                    COMPUTE EXC-GBET = SA-HABEN * -1
                    ADD SA-HABEN TO WG-HABEN(2) WZ-HABEN(2)
                                    WZ-HABEN(WY).
           IF SA-KTOART = 01 COMPUTE EXC-KTONR = SA-KTONR / 10
               MOVE SA-BEZ TO EXC-BEZ
               MOVE ";" TO EXC-TA EXC-TB EXC-TC EXC-TD
               MOVE SA-KER-NR TO EXC-KER
               MOVE EXC-MBET(13:1) TO EXC-MBET(1:1)
               MOVE EXC-GBET(13:1) TO EXC-GBET(1:1)
               MOVE " " TO EXC-MBET(13:1) EXC-GBET(13:1)
               WRITE EXC-SATZ
               MOVE SPACE TO EXC-SATZ.
           PERFORM DRUCK.
           GO G.
       Q.  PERFORM SACHGRU.
           PERFORM SACHSUM.
      *-------------------------------------> au·erbÅcherliche Konten <-
           MOVE 0 TO WM-SOLL WM-HABEN.
           IF WB = 99 GO W else GO S.
       R.  READ SACHBUCH NEXT NO LOCK AT END GO W.
           IF ZUGRIF PERFORM BESETZT GO G.
       S.  IF SA-SOLL = 0 AND SA-HABEN = 0 GO R.
           PERFORM SAKO-KOPF.
           DIVIDE 10 INTO SA-KTONR GIVING DRS-KTONR.
           EVALUATE WM-SK
               WHEN 3 MOVE SPACE TO DRS-KTONR(1:3)
               WHEN 4 MOVE SPACE TO DRS-KTONR(1:1).
           MOVE SA-TX(1) TO DRS-BEZ.
           DISPLAY DRS-KTONR with highlight AT VDU-LP SA-TX(1).
           IF WP = 1 OR WS-DATUM not = 0 PERFORM PER-SACH.
           IF WM-SOLL not = 0 MOVE WM-SOLL TO DRS-MSOLL.
           IF WM-HABEN not = 0 MOVE WM-HABEN TO DRS-MSOLL.
           IF SA-SOLL not = 0 MOVE SA-SOLL TO DRS-GSOLL.
           IF SA-HABEN not = 0 MOVE SA-HABEN TO DRS-GHABEN.
           PERFORM DRUCK.
           GO R.
       W.  PERFORM LISDAT.
           IF WM-FIBEX = 1 CLOSE EXCELIS.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ********************************************** Listdatum drucken *
       LISDAT SECTION.
       A.  MOVE WM-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRS-STR.
           PERFORM DRUCK.
           PERFORM END-DRU.
       Z.  EXIT.
      ***************************************** Periodenwerte Sachbuch *
       PER-SACH SECTION.
       A.  MOVE 0 TO WM-SOLL WM-HABEN WM-EBIL.
           IF WP = 3 MOVE 0 TO SA-SOLL SA-HABEN.
           MOVE SA-KTONR TO BZ-KTONR.
           MOVE WV-DATUM TO BZ-BUDAT.
           MOVE 0 TO BZ-LFD.
           START BUCHZEIL KEY not < BZ-KEY INVALID GO Z.
       C.  READ BUCHZEIL NEXT NO LOCK AT END GO X.
           IF ZUGRIF PERFORM BESETZT GO C.
           IF BZ-BUDAT > WS-DATUM GO X.
           IF BZ-KTONR not = SA-KTONR GO X.
           IF WP = 3 GO F.
           IF BZ-SH = 0 ADD BZ-BUBET TO WM-SOLL
                   else ADD BZ-BUBET TO WM-HABEN.
           GO C.
      *--------------------------------------------> eb + akk. Salden <-
       F.  IF WE-SYM(BZ-SYM + 1) = "eb" OR "EB" OR "Eb";
               IF BZ-SH = 1 SUBTRACT BZ-BUBET FROM WM-EBIL
                       else ADD BZ-BUBET TO WM-EBIL
               end-if
           else IF BZ-SH = 0 ADD BZ-BUBET TO WM-SOLL
                        else ADD BZ-BUBET TO WM-HABEN.
           IF BZ-SH = 0 ADD BZ-BUBET TO SA-SOLL
                   else ADD BZ-BUBET TO SA-HABEN.
           GO C.
       X.  IF WP = 3 GO Z.                          *> eb + akk. Salden
           IF WP = 1 MOVE WM-SOLL TO SA-SOLL
                     MOVE WM-HABEN TO SA-HABEN GO Z.
           IF WM-SOLL > WM-HABEN SUBTRACT WM-HABEN FROM WM-SOLL
               MOVE 0 TO WM-HABEN
           else SUBTRACT WM-SOLL FROM WM-HABEN
               MOVE 0 TO WM-SOLL.
       Z.  EXIT.
      ******************************************* Summe je Kontogruppe *
       SACHGRU SECTION.
       A.  PERFORM SAKO-KOPF.
      *    IF WG-SOLL(1) = 0 AND WG-HABEN(1) = 0 AND WG-SOLL(2) = 0
      *        AND WG-HABEN(2) = 0 GO Z.
           MOVE ALL "ƒ" TO DRS-STR(63:) PERFORM DRUCK.
           MOVE 1 TO WR.
           PERFORM RASTRI.
           MOVE "Summe Gruppe:" TO DRS-STR(10:15).
           MOVE WG TO WD-KZ.
           MOVE WD-KZ TO DRS-STR(25:1).
           IF WG-SOLL(1) not = 0 MOVE WG-SOLL(1) TO DRS-MSOLL.
           IF WG-HABEN(1) not = 0 MOVE WG-HABEN(1) TO DRS-MHABEN.
           IF WG-SOLL(2) not = 0 MOVE WG-SOLL(2) TO DRS-GSOLL.
           IF WG-HABEN(2) not = 0 MOVE WG-HABEN(2) TO DRS-GHABEN.
           PERFORM DRUCK.
           MOVE 2 TO WZ-SCHALT.
           MOVE 0 TO WG-SOLL(1) WG-HABEN(1) WG-SOLL(2) WG-HABEN(2).
           MOVE 1 TO WR.
           IF WZ-ZEILEN not > WH-SIZE PERFORM DRUCK.
       Z.  EXIT.
      *************************************************** Gesamtsummen *
       SACHSUM SECTION.
       A.  IF WZ-SOLL(1) = 0 AND WZ-HABEN(1) = 0 AND WZ-SOLL(2) = 0
               AND WZ-HABEN(2) = 0 GO Z.
           IF (WZ-ZEILEN + 10) > WH-SIZE
               COMPUTE WZ-ZEILEN = WH-SIZE + 10
               PERFORM SAKO-KOPF.
           MOVE ALL "ƒ" TO DRS-STR(63:) PERFORM DRUCK.
           MOVE "S u m m e n:" TO DRS-STR(10:15)
      *    MOVE WG TO WD-KZ.
      *    MOVE WD-KZ TO DRS-STR(25:1).
           MOVE WZ-SOLL(1) TO DRS-MSOLL.
           MOVE WZ-HABEN(1) TO DRS-MHABEN.
           MOVE WZ-SOLL(2) TO DRS-GSOLL.
           MOVE WZ-HABEN(2) TO DRS-GHABEN.
           PERFORM DRUCK.
           SUBTRACT WZ-HABEN(1) FROM WZ-SOLL(1) GIVING WH-WERT WD-SALDO.
           IF WH-WERT not = 0 MOVE "Differenz" TO DRS-STR(10:15)
               MOVE WD-SALDO TO DRS-STR(72:).
           SUBTRACT WZ-HABEN(2) FROM WZ-SOLL(2) GIVING WH-WERT WD-SALDO.
           IF WH-WERT not = 0 MOVE "Differenz" TO DRS-STR(10:15)
               MOVE WD-SALDO TO DRS-STR(102:).
           IF DRS-STR(10:15) not = SPACE PERFORM DRUCK
           else MOVE SPACE TO DRA-SATZ.
           MOVE ALL "=" TO DRS-STR(63:) PERFORM DRUCK.
           MOVE 2 TO WZ-SCHALT.
           PERFORM SAKO-KOPF.
           MOVE "Bestandskonten" TO DRS-BEZ.
           SUBTRACT WG-HABEN(3) FROM WG-SOLL(3) GIVING WH-WERT WD-SALDO.
           MOVE WD-SALDO TO DRS-STR(72:).
      *    MOVE WG-SOLL(3) TO DRS-MSOLL.
      *    MOVE WG-HABEN(3) TO DRS-MHABEN.
      *    MOVE WZ-SOLL(3) TO DRS-GSOLL.
      *    MOVE WZ-HABEN(3) TO DRS-GHABEN.
           SUBTRACT WZ-HABEN(3) FROM WZ-SOLL(3) GIVING WH-WERT WD-SALDO.
           MOVE WD-SALDO TO DRS-STR(102:).
           PERFORM DRUCK.
           MOVE "Erfolgskonten" TO DRS-BEZ.
      *    MOVE WG-SOLL(4) TO DRS-MSOLL.
      *    MOVE WG-HABEN(4) TO DRS-MHABEN.
           SUBTRACT WG-HABEN(4) FROM WG-SOLL(4) GIVING WH-WERT WD-SALDO.
           MOVE WD-SALDO TO DRS-STR(72:).
      *    MOVE WZ-SOLL(4) TO DRS-GSOLL.
      *    MOVE WZ-HABEN(4) TO DRS-GHABEN.
           SUBTRACT WZ-HABEN(4) FROM WZ-SOLL(4) GIVING WH-WERT WD-SALDO.
           MOVE WD-SALDO TO DRS-STR(102:).
           PERFORM DRUCK.
           MOVE 2 TO WZ-SCHALT.
           MOVE 0 TO WZ-SOLL(1) WZ-HABEN(1) WZ-SOLL(2) WZ-HABEN(2).
           MOVE 0 TO WZ-SOLL(3) WZ-HABEN(3) WZ-SOLL(4) WZ-HABEN(4).
       Z.  EXIT.
      ***************************************** Kopf Sachkontenlistung *
       SAKO-KOPF SECTION.
       A.  IF WZ-ZEILEN > WH-SIZE WRITE DRA-SATZ AFTER PAGE
               MOVE 0 TO WZ-ZEILEN.
           IF WZ-ZEILEN > 0 GO Z.
           MOVE 2 TO WH-P1.
           IF WM-KO = 0 MOVE 5 TO WH-P2
                        MOVE 42 TO WH-SIZE
                        MOVE 0 TO WZ-SCHALT
                   else MOVE 4 TO WH-P2
                        MOVE 62 TO WH-SIZE.
           IF WM-DRU = 1 MOVE 40 TO WH-SIZE                    *> Laser
               MOVE 0 TO WZ-SCHALT
               MOVE 12 TO WH-P1.
           IF WM-OPEN = 0 MOVE 1 TO WM-FIBEX
               MOVE "FIBLxxxx.XLS" TO WH-FIBNAM
               MOVE WZ-DATUM(1:4) TO WH-FIBNAM(5:4)
               DELETE FILE EXCELIS
               OPEN EXTEND EXCELIS.
           PERFORM BEG-DRU.
           ADD 1 TO WZ-SEITE.
           IF WM-DRU not = 1 MOVE 1 TO WZ-SCHALT.
           MOVE WK-FIRMA TO DRS-STR(1:30).
           MOVE "Sachkontensalden per:" TO DRS-STR(30:).
           IF WS-DATUM not = 0 MOVE WS-DATUM TO WC-DATUM
                          else MOVE WH-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRS-STR(52:10).
           IF WP = 2 MOVE "akkum." TO DRS-STR(62:7).
           IF WP = 3 MOVE "EB+akkum." TO DRS-STR(62:9).
           MOVE "Seite:" TO DRS-STR(98:).
           MOVE WZ-SEITE TO WD-NUM.
           MOVE WD-NUM(5:) TO DRS-STR(105:).
           PERFORM DRUCK.
           MOVE ALL "ƒƒƒ" TO DRS-STR(1:).
           PERFORM DRUCK.
           MOVE "Konto-Nr B e z e i c h n u n g
      -       "                 SOLL periode  HABEN           SOLL  gesa
      -       "mt  HABEN" TO DRS-STR(1:).
           IF WS-DATUM = 0 or WP = 1 or WP = 3
                           MOVE "UmsÑtze" TO DRA-SATZ(82:7)
                           MOVE " Salden" TO DRA-SATZ(112:7).
           IF WP = 3 MOVE "E-BILANZ" TO DRA-SATZ(58:8).
           PERFORM DRUCK.
           MOVE ALL "ƒƒƒ" TO DRS-STR(1:).
           PERFORM DRUCK.
           MOVE 2 TO WZ-SCHALT.
       Z.  EXIT.
      ************************************************ BZ-SATZ lîschen *
       BZ-INIT SECTION.
       A.  INITIALIZE BZ-SATZ.
       Z.  EXIT.
      ******************************************** Saldendurchrechnung *
       SACHRECH SECTION.
       A.  IF WH-NUM = 0 MOVE 0 TO WM-ART
                    else MOVE 1 TO WM-ART.
           MOVE 0 TO SA-KTONR.
           START SACHBUCH KEY not < SA-KEY INVALID GO Z.
       G.  READ SACHBUCH NEXT NO LOCK AT END MOVE 0 TO BZ-BUDAT GO W.
           IF ZUGRIF PERFORM BESETZT GO G.
           MOVE SA-KTONR TO BZ-KTONR.
           DISPLAY SA-KTONR AT 1901.
           MOVE 0 TO BZ-LFD WM-SOLL WM-HABEN WM-EBIL
                     WB     WV-SOLL WV-HABEN WV-EBIL.
           IF WH-PG = 6 MOVE WV-WBEG TO BZ-BUDAT
                   else ADD 68 WE-WJAHR(3) GIVING BZ-BUDAT.
           START BUCHZEIL KEY not < BZ-KEY INVALID GO K.
       H.  READ BUCHZEIL NEXT AT END MOVE 99 TO WB GO K.
           IF ZUGRIF PERFORM BESETZT GO H.
           IF SA-KTONR not = BZ-KTONR GO K.
           IF WH-PG = 6 AND BZ-SYM > 97 PERFORM LOE-ABS
               IF BZ-SYM = 99 GO H.
      *-------------------------------------------> Summen altes Jahr <-
           IF WH-PG = 6; IF BZ-BUDAT > WV-WEND and WM-LOE = 0 GO I
               else IF BZ-SH = 0 ADD BZ-BUBET TO WV-SOLL
                           else ADD BZ-BUBET TO WV-HABEN
                    end-if
                    IF BZ-SYM = 18 ADD BZ-BUBET TO WV-EBIL
                    end-if GO H.
      *-------------------------------------------> Summen neues Jahr <-
       I.  IF BZ-SH = 0 ADD BZ-BUBET TO WM-SOLL
                  else ADD BZ-BUBET TO WM-HABEN.
           IF BZ-SYM = 18 ADD BZ-BUBET TO WM-EBIL.
           GO H.
      *----------------------------------------> Jahresabschlu·summen <-
       K.  IF WH-PG not = 6 GO Q.
           IF WH-PG = 6 AND WM-LOE = 1 GO Q.
           IF WV-SOLL = 0 AND WV-HABEN = 0 GO Q.
           PERFORM BZ-INIT.
           MOVE SA-KTONR TO BZ-KTONR.
           MOVE WV-WEND TO BZ-BUDAT BZ-BELDAT.
           ADD 10 TO BZ-BUDAT.
           MOVE WV-WKZ TO BZ-USE.
           ADD 9000000 BZ-KTONR GIVING BZ-SEITE.
           MOVE 99 TO BZ-SYM.
           MOVE WV-SOLL TO BZ-SOLL.
           MOVE WV-HABEN TO BZ-HABEN.
           MOVE WV-EBIL TO BZ-EBIL.
           MOVE 75 TO WZ-BUCH.                          *> Grî·e def.
           WRITE BZ-SATZ INVALID REWRITE BZ-SATZ INVALID
                  PERFORM WEITER.
           SUBTRACT WV-SOLL FROM SA-SOLL.
           SUBTRACT WV-HABEN FROM SA-HABEN.
           MOVE 0 TO WV-SOLL WV-HABEN.
       Q.  IF WM-SOLL not = SA-SOLL OR WM-HABEN not = SA-HABEN
               DISPLAY SA-SOLL AT 1920
               DISPLAY SA-HABEN AT 1940
               DISPLAY WM-SOLL AT 2020
               DISPLAY WM-HABEN AT 2040
               IF WH-PG not = 6 AND WM-ART = 1
                    CALL "CAUP" USING "0023300000" WH-CREG.
           MOVE WM-SOLL TO SA-SOLL.
           MOVE WM-HABEN TO SA-HABEN.
           MOVE WM-EBIL TO SA-EBIL.
           REWRITE SA-SATZ.
           IF BZ-BUDAT = 0 GO Z.
           GO G.
       W.  REWRITE SA-SATZ.
       Z.  EXIT.
      ******************************************************************
       FIRST-JOUR SECTION.
       A.  MOVE WE-USE TO BZ-USE.
           MOVE 0 TO BZ-SEITE.
           START BUCHZEIL KEY not < BZ-JKEY INVALID
               MOVE 0 TO BZ-SEITE GO E.
           MOVE SPACE TO BZ-SATZ(12:).
       C.  READ BUCHZEIL NEXT NO LOCK AT END MOVE 0 TO BZ-SEITE.
           IF ZUGRIF PERFORM BESETZT GO C.
       E.  IF BZ-USE not = WE-USE SET ESC TO TRUE GO Z.
       Z.  EXIT.
      *************************************************** Finanzexport *
       FINEX SECTION.
       A.  CALL "CAUP" USING "16CLRFEN" WH-CREG.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "Export von:           bis:          " AT VDU-LP.
       C.  DISPLAY "<esc>= Abbruch, <ret>= Datum" AT 2301.
           INITIALIZE BZ-SATZ.
           CALL "CAUP" USING "1103156006" WH-CREG.
           IF ESC GO Z.
           IF not RET GO C.
           IF WX-DATUM = 0 OR WX-DATUM > WM-DATUM GO C.
           MOVE WX-DATUM TO WV-ABDAT WV-DATUM.
           DISPLAY VDU-DATUM with highlight foreground-color 4
               AT VDU-LP.
           PERFORM WIJAHR.
           MOVE KO-WKZ(WI) TO BZ-USE.
           ADD 50 TO BZ-USE.
           MOVE "JOU-mmjj.FIN" TO WH-DRUNAM.
           MOVE WV-ABDAT TO WV-DATUM WZ-DATUM.
           MOVE WZ-MONAT TO WH-DRUNAM(5:2).
           MOVE WZ-JAHR TO WH-DRUNAM(7:2).
           ADD 236 VDU-ECK GIVING VDU-LP.
           DISPLAY WH-DRUNAM with highlight AT VDU-LP.
       E.  DISPLAY "<esc>= zurÅck, <ret>= Datum" AT 2301.
           CALL "CAUP" USING "1103306006" WH-CREG.
           IF WOLI or AUF or ESC GO C.
           IF not RET GO E.
           IF WX-DATUM = 0 OR WX-DATUM > WM-DATUM GO C.
           MOVE WX-DATUM TO WS-DATUM WV-BISDAT.
           DISPLAY VDU-DATUM with highlight foreground-color 4
               AT VDU-LP.
           IF WS-DATUM < WV-DATUM GO E.
       G.  DISPLAY "<esc>= Abbruch, <ret>= Start < >" AT 2301.
           CALL "CAUP" USING "0023310000" WH-CREG.
           IF ESC GO Z.
           IF not RET GO G.
           MOVE 1 TO WM-FIN.
           MOVE 05 TO WS-TASTE.
           SET RET TO TRUE.
           MOVE 1 TO WK-JS BZ-SEITE.
           MOVE 0 TO WV-VDAT WV-BDAT.
           DELETE FILE DRUCKER.
           MOVE 99999 TO WV-SEITE.
           MOVE BZ-USE TO WE-WKZ.
       Z.  EXIT.
      *************************************************** Journaldruck *
       JOURDRUCK SECTION.
       A.  CALL "CAUP" USING "0710100550000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           MOVE 0 TO WM-SOLL WM-HABEN WZ-ZEILEN WK-JS WJ-ZEIL WV-SEITE.
           DISPLAY " Journaldruck " with highlight AT VDU-LP.
       B.  CALL "CAUP" USING "16CLRFEN" WH-CREG.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "ab Journalseite:       bis: " AT VDU-LP.
           DISPLAY "<esc>= Abbruch, <ret>= Startseite, <Pos 1>= Fin. Exp
      -        "ort" AT 2301.
           CALL "CAUP" USING "1003204004" WH-CREG.
           IF ESC GO X.
           IF APOS PERFORM FINEX
                   IF RET GO F.
           IF not RET GO B.
           MOVE WH-NUM TO WD-JONUM WK-SEITE.
           DISPLAY WD-JONUM with highlight AT VDU-LP.
       C.  ADD 303 VDU-ECK GIVING VDU-LP.
           CALL "CAUP" USING "1003314004" WH-CREG.
           IF ESC GO X.
           IF WOLI or AUF GO B.
           IF KIST or TABL
               MOVE WX-TASTE TO WS-TASTE SET RET TO TRUE.
           IF not RET GO C.
           MOVE WH-NUM TO WD-JONUM WV-BISJ.
           DISPLAY WD-JONUM with highlight AT VDU-LP.
           IF WV-BISJ < WV-SEITE GO B.
           MOVE "JOURNAL.LST" TO WH-DRUNAM.
           ADD 403 VDU-ECK GIVING VDU-LP.
       D.  MOVE WK-JS TO BZ-SEITE.
           MOVE WE-WKZ TO BZ-USE.
           MOVE 0 TO WV-VDAT WV-BDAT.
       F.  START BUCHZEIL KEY not < BZ-JKEY INVALID
               MOVE 0 TO WS-TASTE GO W.
       G.  MOVE SPACE TO BZ-TX.
           READ BUCHZEIL NEXT AT END move 0 to wv-bisj GO Q.
           IF ZUGRIF PERFORM BESETZT GO G.
           MOVE BZ-SEITE TO WK-JS.
           DISPLAY WK-JS AT 2020.
           IF WM-FIN = 1;
               IF BZ-SEITE > 799999 MOVE 44 TO WE-WKZ GO Q
               end-if
               IF BZ-BUDAT < WV-ABDAT or BZ-BUDAT > WV-BISDAT GO G
               end-if
               IF BZ-SEITE > 799999 MOVE 44 TO WE-WKZ
                  GO Q.
           IF WK-SEITE > WV-BISJ GO Q.
           MOVE BZ-KTONR TO WD-KZ.

      *    if wm-datum = 20070204 and WD-KZ not = 2 GO G
      *       else if (bz-sym = 0 or bz-sym = 10) and bz-mw = 0
      *            NEXT SENTENCE
      *            else go g.
      *----------------------------------> fÅr Ende ohne au·erb. Kto. <-
           IF WV-VDAT = 0 MOVE BZ-BUDAT TO WV-VDAT.
           MOVE BZ-BUDAT TO WV-BDAT.
           IF WV-BDAT(1:4) not = WV-VDAT(1:4)
               DISPLAY "Datum: " AT 2401 WV-BDAT " / " WV-VDAT
               PERFORM WEITER GO Q.
           IF WS-TASTE < 6; IF WD-KZ = 1 GO Q
                            else PERFORM JO-DRU GO G.
           move bz-jkey to wh-bkey.
           move we-use to bz-use.
*******    add 2 to wj-zeil.
           add 1 to wj-zeil.
           move wj-zeil to bz-seite.
           display bz-seite at 1020.
           if bz-sym = 19 and bz-ggkto = 999999 and bz-belnr = 0
               delete buchzeil
               end-delete go i.
           rewrite bz-satz.
       i.  move wh-bkey to bz-jkey.
           if wd-kz = 1 go s.
           go f.
       Q.  IF WM-FIN = 1 and WM-SOLL = 0 AND WM-HABEN = 0 GO S.
           MOVE ALL "ƒ" TO DRA-STR(1:140).
           IF WZ-ZEILEN < 41 PERFORM DRUCK.
           MOVE " Journalsummen " TO DRA-TX.
           MOVE WM-SOLL TO DRA-SOLL.
           MOVE WM-HABEN TO DRA-HABEN.
           PERFORM DRUCK.
           SUBTRACT WM-HABEN FROM WM-SOLL GIVING WH-WERT.
           IF WH-WERT not = 0 MOVE "Differenz" TO DRA-TX
               IF WH-WERT > 0 MOVE WH-WERT TO DRA-HABEN
                         else MOVE WH-WERT TO DRA-SOLL
               end-if PERFORM DRUCK.
      *-------------------------------> Ausdruck Bemessungsgrundlagen <-
       S.  IF WD-KZ not = 1 GO W.
           IF WS-TASTE < 6 PERFORM JO-DRU GO T.
           move bz-jkey to wh-bkey.
           move we-use to bz-use.
      *    add 2 to wj-zeil.
           add 1 to wj-zeil.
           move wj-zeil to bz-seite.
           display bz-seite at 1020.
           delete buchzeil.
      *    rewrite bz-satz.
           move wh-bkey to bz-jkey.
       T.  MOVE SPACE TO BZ-TX.
           READ BUCHZEIL NEXT AT END move 0 to wv-bisj GO W.
           IF ZUGRIF PERFORM BESETZT GO T.
           MOVE BZ-SEITE TO WK-JS.
           IF WK-SEITE > WV-BISJ GO W.
           MOVE BZ-KTONR TO WD-KZ.
           GO S.
       W.  IF WS-TASTE = 5 or ws-taste = 6;
                    IF WK-SEITE < WV-BISJ;
                IF WK-ZEIL > 01 ADD 1 TO WK-SEITE end-if
                MOVE 0 TO WM-SOLL WM-HABEN WZ-ZEILEN
                MOVE 0 TO WK-ZEIL
                IF BZ-SEITE < 799999 and BZ-USE = WE-WKZ GO D.
           PERFORM END-DRU.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ********************************************* Druck Journalzeile *
       JO-DRU SECTION.
       A.  PERFORM JOURKOPF.
           IF WV-DATUM not = BZ-BUDAT MOVE BZ-BUDAT TO WC-DATUM WV-DATUM
               PERFORM DATDREH
               MOVE "Buchungsdatum:" TO DRA-STR(1:)
               MOVE VDU-DATUM TO DRA-STR(16:10)
               PERFORM DRUCK.
           MOVE 0 TO BZ-USE.
           DIVIDE 10 INTO BZ-KTONR GIVING DRA-KTONR.
           MOVE BZ-BELNR TO DRA-BELNR.
           MOVE BZ-BELDAT TO WC-DATUM.
           PERFORM DATDREH.
           MOVE WZ-DATUM TO DRA-BELDAT.
           MOVE BZ-REFNR TO DRA-REFNR.
           MOVE WE-SYM(BZ-SYM + 1) TO DRA-SYM.
           IF BZ-Z = 0 GO H.
           IF BZ-GGKTO > 999997 MOVE "sabu" TO DRA-GGKTO(2:)
                MOVE BZ-KTONR TO WH-NUM
                PERFORM LIESKTO
                MOVE SA-TX(1) TO DRA-TX
           else DIVIDE 10 INTO BZ-GGKTO GIVING DRA-GGKTO
                MOVE BZ-TX TO DRA-TX.
       H.  EVALUATE BZ-VM
               WHEN 0 CONTINUE
               WHEN 1 IF BZ-U1 not = 0 AND BZ-MW not = 0
                         MOVE WT-UST(BZ-U1) TO DRA-UST
                         MOVE "m" TO DRA-UST(3:)
                         MOVE BZ-MW TO DRA-MWSKT
               WHEN 2 IF BZ-U1 not = 0 MOVE WT-UST(BZ-U1) TO DRA-UST
                      end-if IF BZ-MW not = 0 MOVE "v" TO DRA-UST(3:)
                                       MOVE BZ-MW TO DRA-MWSKT
               WHEN 3 IF BZ-MW not = 0 or BZ-B1 not = 0 or BZ-B2 not = 0
                          MOVE "Ske" TO DRA-UST(1:)
                          ADD BZ-MW BZ-B1 BZ-B2 GIVING WH-WERT
                          ADD WH-WERT TO WM-SOLL
                          MOVE WH-WERT TO DRA-MWSKT
               WHEN 4 IF BZ-MW not = 0 or BZ-B1 not = 0 or BZ-B2 not = 0
                          MOVE "Ska" TO DRA-UST(1:)
                          ADD BZ-MW BZ-B1 BZ-B2 GIVING WH-WERT
                          ADD WH-WERT TO WM-HABEN
                          MOVE WH-WERT TO DRA-MWSKT
               WHEN 5 IF BZ-U1 not = 0 MOVE WT-UST(BZ-U1) TO DRA-UST
                      end-if IF BZ-MW not = 0 MOVE "i" TO DRA-UST(3:)
                                       MOVE BZ-MW TO DRA-MWSKT.
           IF BZ-SH = 0 MOVE BZ-BUBET TO DRA-SOLL
                        IF BZ-Z not = 3 ADD BZ-BUBET TO WM-SOLL
                        else CONTINUE
           else MOVE BZ-BUBET TO DRA-HABEN
                IF BZ-Z not = 3 ADD BZ-BUBET TO WM-HABEN.
           DISPLAY DRA-STR(1:40) with highlight AT VDU-LP.
           PERFORM DRUCK.
       Z.  EXIT.
      **************************************************** Journalkopf *
       JOURKOPF SECTION.
       A.  if wm-datum = 20070204 go z.
           IF WK-SEITE not = WV-SEITE;
               IF WZ-ZEILEN > 0 WRITE DRA-SATZ AFTER PAGE
               MOVE 0 TO WZ-ZEILEN.
           IF WZ-ZEILEN > 0 GO Z.
           MOVE 1 TO WZ-SCHALT.
           MOVE 2 TO WH-P1.
           IF WM-KO = 0 MOVE 5 TO WH-P2
                        MOVE 40 TO WH-SIZE
                        IF WM-DRU = 1 MOVE 12 TO WH-P1         *> Laser
                            MOVE 0 TO WZ-SCHALT
                        end-if
                   else MOVE 4 TO WH-P2
                        MOVE 61 TO WH-SIZE
                        IF WM-DRU = 1 MOVE 17 TO WH-P1         *> Laser
                            MOVE 0 TO WZ-SCHALT.
           PERFORM BEG-DRU.
           MOVE WK-FIRMA TO DRA-STR(1:30).
           MOVE "J o u r n a l   per:" TO DRA-STR(36:).
           MOVE BZ-BUDAT TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRA-STR(57:10).
           MOVE "Seite:" TO DRA-STR(97:).
           MOVE WK-SEITE TO WD-NUM WV-SEITE.
           MOVE WD-NUM TO DRA-STR(103:).
           PERFORM DRUCK.
           MOVE ALL "ƒƒƒ" TO DRA-STR(1:140).
           PERFORM DRUCK.
           MOVE "Konto-Nr Bel-Nr Bel-Dat. Ref.Nr Sy Geg.Kto Buchungstext
      -       "                             Kz  Ust/Skto(s)  Umsatz Soll
      -       "   Umsatz Haben " TO DRA-STR(1:).
           PERFORM DRUCK.
           MOVE ALL "ƒƒƒ" TO DRA-STR(1:140).
           PERFORM DRUCK.
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
               WHEN 0 MOVE WH-NUM TO SA-KTONR.
      *---------------------------------------------> lesen Sachkonto <-
       C.  READ SACHBUCH not INVALID MOVE SA-BEZ TO WT-TX
               MOVE SA-SOLL TO WE-SOLL
               MOVE SA-HABEN TO WE-HABEN
               ADD WE-SOLL WE-HABEN GIVING WE-SALDO GO Z.
           IF ZUGRIF PERFORM BESETZT GO X.
           CALL "FIBSACH" USING "21FIBSACH" WH-CREG.
           CANCEL "FIBSACH".
           IF SA-KTONR = 0 GO X else GO C.
      *---------------------------------------------> lesen Debitoren <-
       E.  READ DEBITOR not INVALID MOVE DE-BEZ TO WT-TX
               INSPECT WT-TX REPLACING ALL "#" BY ","
               MOVE DE-SOLL TO WE-SOLL
               MOVE DE-HABEN TO WE-HABEN
               ADD WE-SOLL WE-HABEN GIVING WE-SALDO GO Z.
           IF ZUGRIF PERFORM BESETZT GO X.
           CALL "FIBDEBIT" USING "31FIBDEB" WH-CREG.
           CANCEL "FIBDEBIT".
           IF DE-KTONR = 0 GO X else GO E.
      *--------------------------------------------> lesen Kreditoren <-
       G.  READ KREDIT not INVALID MOVE KR-BEZ TO WT-TX
               MOVE KR-SOLL TO WE-SOLL
               MOVE KR-HABEN TO WE-HABEN
               ADD WE-SOLL WE-HABEN GIVING WE-SALDO GO Z.
           IF ZUGRIF PERFORM BESETZT GO X.
           CALL "FIBKRED" USING "41FIBKRED" WH-CREG.
           CANCEL "FIBKRED".
           IF KR-KTONR not = 0 GO G.
       X.  SET FEHLER TO TRUE.
           MOVE "Konto fehlt" TO WT-TX.
       Z.  EXIT.
      ******************************************** Saldendurchrechnung *
       DEBRECH SECTION.
       A.  IF WH-NUM = 0 MOVE 0 TO WM-ART
                    else MOVE 1 TO WM-ART.
           MOVE 0 TO DE-KTONR.
           START DEBITOR KEY not < DE-KEY INVALID GO Z.
       G.  READ DEBITOR NEXT NO LOCK AT END MOVE 0 TO BZ-BUDAT GO W.
           IF ZUGRIF PERFORM BESETZT GO G.
           MOVE DE-KTONR TO BZ-KTONR.
           DISPLAY DE-KTONR AT 1902.
           MOVE 0 TO BZ-LFD WM-SOLL WM-HABEN WM-EBIL
                     WB     WV-SOLL WV-HABEN WV-EBIL WM-UMSATZ.
           IF WH-PG = 6 MOVE WV-WBEG TO BZ-BUDAT
                   else ADD 68 WE-WJAHR(3) GIVING BZ-BUDAT.
           START BUCHZEIL KEY not < BZ-KEY INVALID GO K.
       H.  READ BUCHZEIL NEXT AT END MOVE 99 TO WB GO K.
           IF ZUGRIF PERFORM BESETZT GO H.
           IF DE-KTONR not = BZ-KTONR GO K.
           IF WH-PG = 6 AND BZ-SYM > 97 PERFORM LOE-ABS
               IF BZ-SYM = 99 GO H.
           MOVE 0 TO WM-TEIL.
           IF BZ-VM = 4 ADD BZ-MW BZ-B1 BZ-B2 TO WM-TEIL.
      *-------------------------------------------> Summen altes Jahr <-
           IF WH-PG = 6; IF BZ-BUDAT > WV-WEND and WM-LOE = 0 GO I
               else IF BZ-SH = 0 ADD BZ-BUBET TO WV-SOLL
                           else ADD BZ-BUBET TO WV-HABEN
                    end-if
                    ADD WM-TEIL TO WV-HABEN
                    IF BZ-SYM = 18 ADD BZ-BUBET TO WV-EBIL
                    end-if GO H.
      *-------------------------------------------> Summen neues Jahr <-
       I.  IF WH-PG = 6 and BZ-SYM = 98 GO H.
           IF BZ-SH = 0 ADD BZ-BUBET TO WM-SOLL
                  else ADD BZ-BUBET TO WM-HABEN.
           ADD WM-TEIL TO WM-HABEN.
           IF BZ-SYM = 18 ADD BZ-BUBET TO WM-EBIL.
           IF WH-PG = 2 and BZ-SYM = 98 GO H.
           MOVE WE-SKZ(BZ-SYM + 1) TO WE-SYMT.
           IF WE-DIV = 1 or WE-DIV = 2 ADD BZ-BUBET TO WM-UMSATZ
                                       SUBTRACT BZ-MW FROM WM-UMSATZ.
           GO H.
      *----------------------------------------> Jahresabschlu·summen <-
       K.  IF WH-PG not = 6 GO Q.
           IF WH-PG = 6 AND WM-LOE = 1 GO Q.
           IF WV-SOLL = 0 AND WV-HABEN = 0 GO Q.
           PERFORM BZ-INIT.
           MOVE DE-KTONR TO BZ-KTONR.
           MOVE WV-WEND TO BZ-BUDAT BZ-BELDAT.
           ADD 10 TO BZ-BUDAT.
           MOVE WV-WKZ TO BZ-USE.
           ADD 9000000 BZ-KTONR GIVING BZ-SEITE.
           MOVE 99 TO BZ-SYM.
           MOVE WV-SOLL TO BZ-SOLL.
           MOVE WV-HABEN TO BZ-HABEN.
           MOVE WV-EBIL TO BZ-EBIL.
           MOVE 75 TO WZ-BUCH.                          *> Grî·e def.
           WRITE BZ-SATZ INVALID REWRITE BZ-SATZ.
           SUBTRACT WV-SOLL FROM DE-SOLL.
           SUBTRACT WV-HABEN FROM DE-HABEN.
           SUBTRACT WV-HABEN FROM WV-SOLL GIVING WH-WERT.
           MOVE 0 TO WV-SOLL WV-HABEN.
           IF WH-WERT = 0 GO Q.                    *> kein Saldovortrag
           PERFORM BZ-INIT.
           MOVE DE-KTONR TO BZ-KTONR.
           ADD 1 WV-WKZ TO BZ-USE.
           ADD 8000000 BZ-KTONR GIVING BZ-SEITE.   *> eindeutiger Key
           IF WH-PG = 6 MOVE WV-SALDAT TO BZ-BUDAT BZ-BELDAT
                   else MOVE WE-WJAHR(1) TO BZ-BUDAT BZ-BELDAT.
           MOVE 0 TO BZ-LFD.
           MOVE 98 TO BZ-SYM.
           IF WH-WERT > 0 MOVE 0 TO BZ-SH
                          ADD WH-WERT TO WM-SOLL
                     else MOVE 1 TO BZ-SH
                          MULTIPLY -1 BY WH-WERT
                          ADD WH-WERT TO WM-HABEN.
           IF WE-EURO = WV-SALDAT
               COMPUTE BZ-BUBET ROUNDED = WH-WERT / 13,7603
           else MOVE WH-WERT TO BZ-BUBET.
           MOVE 75 TO WZ-BUCH.
       O.  WRITE BZ-SATZ INVALID REWRITE BZ-SATZ.
      *-----------------------------------------------------------------
       Q.  IF WM-SOLL not = DE-SOLL OR WM-HABEN not = DE-HABEN
               DISPLAY WM-SOLL AT 1920
               DISPLAY WM-HABEN AT 1940
               DISPLAY DE-SOLL AT 2020
               DISPLAY DE-HABEN AT 2040
               IF WH-PG not = 6 AND WM-ART = 1
                    CALL "CAUP" USING "0023300000" WH-CREG.
           MOVE WM-SOLL TO DE-SOLL.
           MOVE WM-HABEN TO DE-HABEN.
           MOVE WM-UMSATZ TO DE-UMSATZ.
           REWRITE DE-SATZ.
           IF BZ-BUDAT = 0 GO Z.
           GO G.
       W.  REWRITE DE-SATZ.
       Z.  EXIT.
      ******************************************** Saldendurchrechnung *
       PRUEF-BUCH SECTION.
       A.  MOVE 0 TO DE-KTONR.
           START DEBITOR KEY not < DE-KEY INVALID GO K.
       C.  READ DEBITOR NEXT NO LOCK AT END MOVE 0 TO BZ-BUDAT GO K.
           IF ZUGRIF PERFORM BESETZT GO C.
           MOVE DE-KTONR TO BZ-KTONR.
           DISPLAY DE-KTONR AT 1902.
           MOVE 0 TO BZ-LFD.
           MOVE WE-WJAHR(1) TO BZ-BUDAT.
           START BUCHZEIL KEY not < BZ-KEY INVALID GO C.
       E.  READ BUCHZEIL NEXT AT END MOVE 99 TO WB GO C.
           IF ZUGRIF PERFORM BESETZT GO E.
           IF DE-KTONR not = BZ-KTONR GO C.
           IF BZ-SYM > 96 GO C.
           IF BZ-BUDAT = WE-WJAHR(1) AND BZ-LFD = 0
               PERFORM ERST-BUCH.
           GO C.
      *------------------------------------------> PrÅfing Kreditoren <-
       K.  MOVE 0 TO KR-KTONR.
           START KREDIT KEY not < KR-KEY INVALID GO P.
       M.  READ KREDIT NEXT NO LOCK AT END MOVE 0 TO BZ-BUDAT GO P.
           IF ZUGRIF PERFORM BESETZT GO M.
           MOVE KR-KTONR TO BZ-KTONR.
           DISPLAY KR-KTONR AT 1901.
           MOVE 0 TO BZ-LFD.
           MOVE WE-WJAHR(1) TO BZ-BUDAT.
           START BUCHZEIL KEY not < BZ-KEY INVALID GO M.
       O.  READ BUCHZEIL NEXT AT END MOVE 99 TO WB GO M.
           IF ZUGRIF PERFORM BESETZT GO O.
           IF KR-KTONR not = BZ-KTONR GO M.
           IF BZ-SYM > 96 GO M.
           IF BZ-BUDAT = WE-WJAHR(1) AND BZ-LFD = 0
               PERFORM ERST-BUCH.
           GO M.
      *------------------------------------------> PrÅfung Sachkonten <-
       P.  MOVE 0 TO SA-KTONR.
           START SACHBUCH KEY not < SA-KEY INVALID GO Z.
       Q.  READ SACHBUCH NEXT NO LOCK AT END MOVE 0 TO BZ-BUDAT GO Z.
           IF ZUGRIF PERFORM BESETZT GO Q.
           MOVE SA-KTONR TO BZ-KTONR.
           DISPLAY SA-KTONR AT 1901.
           MOVE 0 TO BZ-LFD.
           MOVE WE-WJAHR(1) TO BZ-BUDAT.
           START BUCHZEIL KEY not < BZ-KEY INVALID GO Q.
       R.  READ BUCHZEIL NEXT AT END MOVE 99 TO WB GO Q.
           IF ZUGRIF PERFORM BESETZT GO R.
           IF SA-KTONR not = BZ-KTONR GO Q.
           IF BZ-SYM > 96 GO Q.
           IF BZ-BUDAT = WE-WJAHR(1) AND BZ-LFD = 0
               PERFORM ERST-BUCH.
           GO Q.
       Z.  EXIT.
      ************************************* falls erste Buchung belegt *
       ERST-BUCH SECTION.
       A.  MOVE BZ-KEY TO WH-BKEY.
           ADD 1 TO WH-LFD.
           MOVE 899 TO BZ-LFD.
       C.  START BUCHZEIL KEY < BZ-KEY INVALID STOP RUN.
       E.  READ BUCHZEIL PREVIOUS AT END STOP RUN.
           IF ZUGRIF PERFORM BESETZT GO E.
           DELETE BUCHZEIL.
           ADD 1 TO BZ-LFD.
           WRITE BZ-SATZ.
           IF BZ-KEY not = WH-BKEY GO C.
           IF BZ-LFD > 1 GO C.
       Z.  EXIT.
      ********************************************** Listung Debitoren *
       DEBLIS SECTION.
       A.  CALL "CAUP" USING "0707080563000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Debitorensaldenlistung " with highlight AT VDU-LP.
           MOVE "KUNDLIST.LST" TO WH-DRUNAM.
           MOVE 0 TO WP WH-MOD.
       B.  ADD 230 VDU-ECK GIVING VDU-LP.
           DISPLAY "Periodenende:" AT VDU-LP.
           IF WS-DATUM = 0 MOVE WM-DATUM TO WS-DATUM.
           MOVE WS-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           ADD 244 VDU-ECK GIVING VDU-LP.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           DISPLAY "<esc>= Abbruch, <ret>= Datum, < >= Gesamtliste"
                AT 2301.
           MOVE WS-DATUM TO WZ-DATUM WH-WERT.
           CALL "CAUP" USING "1102446006" WH-CREG.
           IF ESC GO X.
           IF AB MOVE 0 TO WZ-DATUM
                 SET RET TO TRUE.
           IF not RET GO B.
           IF WZ-DATUM = 0 MOVE 0 TO WV-DATUM WS-DATUM WP
               CALL "CAUP" USING "16CLRFEN" WH-CREG
               DISPLAY "Gesamtliste" with highlight AT VDU-LP GO G.
           PERFORM ULTIMO.
           MOVE 1 TO WP.
           PERFORM WIJAHR.
       G.  ADD 209 VDU-ECK GIVING VDU-LP.
           DISPLAY WH-DRUNAM with highlight AT VDU-LP.
           ADD 133 VDU-ECK GIVING VDU-LP.
           IF WH-MOD = 1 DISPLAY "  numerisch   " AT VDU-LP
                    else DISPLAY " alphabetisch " AT VDU-LP.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "<#>= Start alle, <ret>= Start mit Saldo, <esc>= Abbr
      -        "uch < >" AT VDU-LP.
           MOVE 0 TO WE.
           IF WH-MOD = 1 DISPLAY "< >= Listung alphabetisch" AT 2301
                    else DISPLAY "< >= Listung numerisch" AT 2301.
           DISPLAY "tab 0/1" AT 2401.
           CALL "CAUP" USING "1003601001" WH-CREG.
           IF ESC GO X.
           IF TABL PERFORM DEBRECH.
           IF KIST; IF WP = 0 MOVE 1 TO WE
                              SET RET TO TRUE
                    else DISPLAY "Nur als Gesamtliste mîglich"
                            with highlight foreground-color 4 AT 2401
                            PERFORM WEITER GO B.
           IF AUF; IF WH-MOD = 1 MOVE 0 TO WH-MOD
                   else MOVE 1 TO WH-MOD.
           IF not RET GO G.
           IF WP = 1 MOVE 1 TO WE.
           MOVE 0 TO WS-ART(1) WS-ART(2) WS-ART(3) WG WZ-SEITE
               WM-SOLL WM-HABEN WZ-ZEILEN WG-SOLL(1) WG-HABEN(1).
       I.  ADD 403 VDU-ECK GIVING VDU-LP.
           MOVE 0 TO DE-KTONR WB.
           IF WH-MOD = 1 START DEBITOR KEY not < DE-KEY INVALID GO Z
                         end-start GO K.
           MOVE LOW-VALUES TO DE-MCODE.
           START DEBITOR KEY not < DE-MCODE INVALID GO Z.
       K.  READ DEBITOR NEXT NO LOCK AT END MOVE 99 TO WB
               PERFORM DEBGRU GO Q.
           IF ZUGRIF PERFORM BESETZT GO K.
           MOVE DE-KTOART TO WF.
           IF WE = 0; IF DE-SOLL = DE-HABEN GO K.
           IF WF not = WG ADD 1 TO WS-ART(WF + 1) GO K.
           IF WP = 1 PERFORM PER-DEBI
               IF DE-SOLL = DE-HABEN GO K.
           PERFORM DEB-KOPF.
           DIVIDE 10 INTO DE-KTONR GIVING DRP-KTONR.
           MOVE DE-BEZ TO WT-TX.
           INSPECT WT-TX REPLACING ALL "#" BY ",".
           MOVE WT-TX TO DRP-BEZ.
           DISPLAY DRP-KTONR with highlight AT VDU-LP DE-BEZ(1:47).
           IF DE-SOLL not = 0 MOVE DE-SOLL TO DRP-SOLL.
           IF DE-HABEN not = 0 MOVE DE-HABEN TO DRP-HABEN.
           SUBTRACT DE-HABEN FROM DE-SOLL GIVING DRP-SALDO.
           ADD DE-SOLL TO WM-SOLL WG-SOLL(1).
           ADD DE-HABEN TO WM-HABEN WG-HABEN(1).
           PERFORM DRUCK.
           GO K.
       Q.  IF WG not = 2 ADD 1 TO WG
               IF WS-ART(WG + 1) not = 0 GO I else GO Q.
       R.  IF WM-SOLL not = 0 OR WM-HABEN not = 0
               MOVE ALL "ƒ" TO DRP-STR(1:)
               PERFORM DRUCK
               MOVE "S u m m e n:" TO DRP-STR(10:15)
               MOVE WM-SOLL TO DRP-SOLL
               MOVE WM-HABEN TO DRP-HABEN
               SUBTRACT WM-HABEN FROM WM-SOLL GIVING DRP-SALDO
               PERFORM DRUCK
               MOVE ALL "=" TO DRP-STR(63:) PERFORM DRUCK
               MOVE 2 TO WZ-SCHALT
               MOVE 0 TO WM-SOLL WM-HABEN.
       W.  PERFORM LISDAT.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      **************************************** Periodenwerte Debitoren *
       PER-DEBI SECTION.
       A.  MOVE 0 TO WV-SOLL WV-HABEN.
           MOVE DE-KTONR TO BZ-KTONR.
           MOVE WV-DATUM TO BZ-BUDAT.
           MOVE 0 TO BZ-LFD.
           START BUCHZEIL KEY not < BZ-KEY INVALID GO Z.
       C.  READ BUCHZEIL NEXT NO LOCK AT END GO X.
           IF ZUGRIF PERFORM BESETZT GO C.
           IF BZ-BUDAT > WS-DATUM GO X.
           IF BZ-KTONR not = DE-KTONR GO X.
           IF BZ-VM = 4; IF BZ-SH = 1 ADD BZ-MW BZ-B1 BZ-B2 TO BZ-BUBET
                         else SUBTRACT BZ-MW BZ-B1 BZ-B2 FROM BZ-BUBET.
           IF BZ-SH = 0 ADD BZ-BUBET TO WV-SOLL
                   else ADD BZ-BUBET TO WV-HABEN.
           GO C.
       X.  IF WP = 1 MOVE WV-SOLL TO DE-SOLL
                     MOVE WV-HABEN TO DE-HABEN.
       Z.  EXIT.
      **************************************************** Journalkopf *
       DEBGRU SECTION.
       A.  IF WG-SOLL(1) not = 0 OR WG-HABEN(1) not = 0
               MOVE ALL "ƒ" TO DRP-STR(63:) PERFORM DRUCK
               MOVE "Summe Gruppe:" TO DRP-STR(10:15)
               MOVE WG TO WD-KZ
               MOVE WD-KZ TO DRP-STR(25:1)
               MOVE WG-SOLL(1) TO DRP-SOLL
               MOVE WG-HABEN(1) TO DRP-HABEN
               SUBTRACT WG-HABEN(1) FROM WG-SOLL(1) GIVING DRP-SALDO
               PERFORM DRUCK
               MOVE 2 TO WZ-SCHALT
               MOVE 0 TO WG-SOLL(1) WG-HABEN(1).
       Z.  EXIT.
      ************************************ Kopf Debitorenkontenlistung *
       DEB-KOPF SECTION.
       A.  IF WZ-ZEILEN > WH-SIZE WRITE DRA-SATZ AFTER PAGE
               MOVE 0 TO WZ-ZEILEN.
           IF WZ-ZEILEN > 0 GO Z.
           MOVE 2 TO WH-P1.
           IF WM-KO = 0 MOVE 5 TO WH-P2
                        MOVE 41 TO WH-SIZE
                        MOVE 0 TO WZ-SCHALT
                   else MOVE 4 TO WH-P2
                        MOVE 62 TO WH-SIZE.
           IF WM-DRU = 1 MOVE 40 TO WH-SIZE                    *> Laser
               MOVE 12 TO WH-P1.
           PERFORM BEG-DRU.
           ADD 1 TO WZ-SEITE.
           IF WM-DRU not = 1 MOVE 1 TO WZ-SCHALT.
           MOVE WK-FIRMA TO DRP-STR(1:30).
           MOVE "Debitorensalden per:" TO DRP-STR(30:).
           IF WS-DATUM not = 0 MOVE WS-DATUM TO WC-DATUM
                          else MOVE WH-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRP-STR(51:10).
           IF WE = 0 MOVE "nur mit Saldo" TO DRP-STR(69:).
           MOVE "Seite:" TO DRP-STR(98:).
           MOVE WZ-SEITE TO WD-NUM.
           MOVE WD-NUM(5:) TO DRP-STR(105:).
           PERFORM DRUCK.
           MOVE ALL "ƒƒƒ" TO DRP-STR(1:).
           PERFORM DRUCK.
           MOVE "Konto-Nr B e z e i c h n u n g
      -       "                 SOLL  UmsÑtze HABEN          Saldo"
               TO DRP-STR(1:).
           PERFORM DRUCK.
           MOVE ALL "ƒƒƒ" TO DRP-STR(1:).
           PERFORM DRUCK.
           MOVE 2 TO WZ-SCHALT.
       Z.  EXIT.
      ********************************************* Listung Kreditoren *
       KREDLIS SECTION.
       A.  CALL "CAUP" USING "07070805630000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Lieferantensaldenlistung " with highlight
               AT VDU-LP.
           MOVE "LIEFLIST.LST" TO WH-DRUNAM.
           MOVE 0 TO WP WH-MOD.
       B.  ADD 230 VDU-ECK GIVING VDU-LP.
           DISPLAY "Periodenende:" AT VDU-LP.
           IF WS-DATUM = 0 MOVE WM-DATUM TO WS-DATUM.
           MOVE WS-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           ADD 244 VDU-ECK GIVING VDU-LP.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           DISPLAY "<esc>= Abbruch, <ret>= Datum, < >= Gesamtliste"
                AT 2301.
           MOVE WM-DATUM TO WZ-DATUM WH-WERT.
           CALL "CAUP" USING "1102446006" WH-CREG.
           IF ESC GO X.
           IF AB MOVE 0 TO WZ-DATUM
                 SET RET TO TRUE.
           IF not RET GO B.
           IF WZ-DATUM = 0 MOVE 0 TO WV-DATUM WS-DATUM WP
               CALL "CAUP" USING "16CLRFEN" WH-CREG
               DISPLAY "Gesamtliste" with highlight AT VDU-LP GO G.
           PERFORM ULTIMO.
           MOVE 1 TO WP.
           PERFORM WIJAHR.
       G.  ADD 209 VDU-ECK GIVING VDU-LP.
           DISPLAY WH-DRUNAM with highlight AT VDU-LP.
           ADD 133 VDU-ECK GIVING VDU-LP.
           IF WH-MOD = 1 DISPLAY "  numerisch   " AT VDU-LP
                    else DISPLAY " alphabetisch " AT VDU-LP.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "<#>= Start alle, <ret>= Start mit Saldo, <esc>= Abbr
      -        "uch < >" AT VDU-LP.
           MOVE 0 TO WE.
           IF WH-MOD = 1 DISPLAY "< >= Listung alphabetisch" AT 2301
                    else DISPLAY "< >= Listung numerisch" AT 2301.
           DISPLAY "tab 0/1" AT 2401.
           CALL "CAUP" USING "1003601001" WH-CREG.
           IF ESC GO X.
           IF TABL PERFORM KREDRECH.
           IF KIST; IF WP = 0 MOVE 1 TO WE
                              SET RET TO TRUE
                    else DISPLAY "Nur als Gesamtliste mîglich"
                            with highlight foreground-color 4 AT 2401
                            PERFORM WEITER GO B.
           IF AUF; IF WH-MOD = 1 MOVE 0 TO WH-MOD
                   else MOVE 1 TO WH-MOD.
           IF not RET GO G.
           IF WP = 1 MOVE 1 TO WE.
           MOVE 0 TO WS-ART(1) WS-ART(2) WS-ART(3) WG WZ-SEITE
               WM-SOLL WM-HABEN WZ-ZEILEN WG-SOLL(1) WG-HABEN(1).
       I.  ADD 403 VDU-ECK GIVING VDU-LP.
           MOVE 0 TO KR-KTONR WB.
           IF WH-MOD = 1 START KREDIT KEY not < KR-KEY INVALID GO Z
                         end-start GO G.
           MOVE LOW-VALUES TO KR-MCODE.
           START KREDIT KEY not < KR-MCODE INVALID GO Z.
       K.  READ KREDIT NEXT NO LOCK AT END MOVE 99 TO WB
               PERFORM KREDGRU GO Q.
           IF ZUGRIF PERFORM BESETZT GO K.
           MOVE KR-KTOART TO WF.
           IF WE = 0; IF KR-SOLL = KR-HABEN GO K.
           IF WF not = WG ADD 1 TO WS-ART(WF + 1) GO K.
           IF WP = 1 PERFORM PER-KRED
               IF KR-SOLL = KR-HABEN GO K.
           PERFORM KRED-KOPF.
           DIVIDE 10 INTO KR-KTONR GIVING DRP-KTONR.
           MOVE KR-BEZ TO WT-TX.
           INSPECT WT-TX REPLACING ALL "#" BY ",".
           MOVE WT-TX TO DRP-BEZ.
           DISPLAY DRP-KTONR with highlight AT VDU-LP KR-BEZ(1:47).
           IF KR-SOLL not = 0 MOVE KR-SOLL TO DRP-SOLL.
           IF KR-HABEN not = 0 MOVE KR-HABEN TO DRP-HABEN.
           SUBTRACT KR-HABEN FROM KR-SOLL GIVING DRP-SALDO.
           ADD KR-SOLL TO WM-SOLL WG-SOLL(1).
           ADD KR-HABEN TO WM-HABEN WG-HABEN(1).
           PERFORM DRUCK.
           GO K.
       Q.  IF WG not = 2 ADD 1 TO WG
               IF WS-ART(WG + 1) not = 0 GO I else GO Q.
           IF WM-SOLL not = 0 OR WM-HABEN not = 0
               MOVE ALL "ƒ" TO DRP-STR(1:)
               PERFORM DRUCK
               MOVE "S u m m e n:" TO DRP-STR(10:15)
               MOVE WM-SOLL TO DRP-SOLL
               MOVE WM-HABEN TO DRP-HABEN
               SUBTRACT WM-HABEN FROM WM-SOLL GIVING DRP-SALDO
               PERFORM DRUCK
               MOVE ALL "=" TO DRP-STR(63:) PERFORM DRUCK
               MOVE 2 TO WZ-SCHALT
               MOVE 0 TO WM-SOLL WM-HABEN.
       W.  PERFORM LISDAT.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      *************************************** Periodenwerte Kreditoren *
       PER-KRED SECTION.
       A.  MOVE 0 TO WV-SOLL WV-HABEN.
           MOVE KR-KTONR TO BZ-KTONR.
           MOVE WV-DATUM TO BZ-BUDAT.
           MOVE 0 TO BZ-LFD.
           START BUCHZEIL KEY not < BZ-KEY INVALID GO Z.
       C.  READ BUCHZEIL NEXT NO LOCK AT END GO X.
           IF ZUGRIF PERFORM BESETZT GO C.
           IF BZ-BUDAT > WS-DATUM GO X.
           IF BZ-KTONR not = KR-KTONR GO X.
           IF BZ-VM = 3; IF BZ-SH = 0 ADD BZ-MW BZ-B1 BZ-B2 TO BZ-BUBET
                         else SUBTRACT BZ-MW BZ-B1 BZ-B2 FROM BZ-BUBET.
           IF BZ-SH = 0 ADD BZ-BUBET TO WV-SOLL
                   else ADD BZ-BUBET TO WV-HABEN.
           GO C.
       X.  IF WP = 1 MOVE WV-SOLL TO KR-SOLL
                     MOVE WV-HABEN TO KR-HABEN.
       Z.  EXIT.
      **************************************************** Journalkopf *
       KREDGRU SECTION.
       A.  IF WG-SOLL(1) not = 0 OR WG-HABEN(1) not = 0
               MOVE ALL "ƒ" TO DRP-STR(63:) PERFORM DRUCK
               MOVE "Summe Gruppe:" TO DRP-STR(10:15)
               MOVE WG TO WD-KZ
               MOVE WD-KZ TO DRP-STR(25:1)
               MOVE WG-SOLL(1) TO DRP-SOLL
               MOVE WG-HABEN(1) TO DRP-HABEN
               SUBTRACT WG-HABEN(1) FROM WG-SOLL(1) GIVING DRP-SALDO
               PERFORM DRUCK
               MOVE 2 TO WZ-SCHALT
               MOVE 0 TO WG-SOLL(1) WG-HABEN(1).
       Z.  EXIT.
      *************************************** Kopf Kundenkontenlistung *
       KRED-KOPF SECTION.
       A.  IF WZ-ZEILEN > WH-SIZE WRITE DRA-SATZ AFTER PAGE
               MOVE 0 TO WZ-ZEILEN.
           IF WZ-ZEILEN > 0 GO Z.
           MOVE 2 TO WH-P1.
           IF WM-KO = 0 MOVE 5 TO WH-P2
                        MOVE 41 TO WH-SIZE
                        MOVE 0 TO WZ-SCHALT
                   else MOVE 4 TO WH-P2
                        MOVE 62 TO WH-SIZE.
           IF WM-DRU = 1 MOVE 40 TO WH-SIZE                    *> Laser
               MOVE 12 TO WH-P1.
           PERFORM BEG-DRU.
           ADD 1 TO WZ-SEITE.
           IF WM-DRU not = 1 MOVE 1 TO WZ-SCHALT.
           MOVE WK-FIRMA TO DRP-STR(1:30).
           MOVE "Kreditorensalden per:" TO DRP-STR(30:).
           IF WS-DATUM not = 0 MOVE WS-DATUM TO WC-DATUM
                          else MOVE WH-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRP-STR(52:10).
           IF WE = 0 MOVE "nur mit Saldo" TO DRP-STR(79:).
           MOVE "Seite:" TO DRP-STR(98:).
           MOVE WZ-SEITE TO WD-NUM.
           MOVE WD-NUM(5:) TO DRP-STR(105:).
           PERFORM DRUCK.
           MOVE ALL "ƒƒƒ" TO DRP-STR(1:).
           PERFORM DRUCK.
           MOVE "Konto-Nr B e z e i c h n u n g
      -       "                 SOLL  UmsÑtze HABEN          Saldo"
               TO DRP-STR(1:).
           PERFORM DRUCK.
           MOVE ALL "ƒƒƒ" TO DRP-STR(1:).
           PERFORM DRUCK.
           MOVE 2 TO WZ-SCHALT.
       Z.  EXIT.
      ******************************************** Saldendurchrechnung *
       KREDRECH SECTION.
       A.  IF WH-NUM = 0 MOVE 0 TO WM-ART
                    else MOVE 1 TO WM-ART.
           MOVE 0 TO KR-KTONR.
           START KREDIT KEY not < KR-KEY INVALID GO Z.
       G.  READ KREDIT NEXT NO LOCK AT END MOVE 0 TO BZ-BUDAT GO W.
           IF ZUGRIF PERFORM BESETZT GO G.
           MOVE KR-KTONR TO BZ-KTONR.
           DISPLAY KR-KTONR AT 1901.
           MOVE 0 TO BZ-LFD WM-SOLL WM-HABEN WM-EBIL
                     WB     WV-SOLL WV-HABEN WV-EBIL.
           IF WH-PG = 6 MOVE WV-WBEG TO BZ-BUDAT
                   else ADD 68 WE-WJAHR(3) GIVING BZ-BUDAT.
           START BUCHZEIL KEY not < BZ-KEY INVALID GO K.
       H.  READ BUCHZEIL NEXT AT END MOVE 99 TO WB GO K.
           IF ZUGRIF PERFORM BESETZT GO H.
           IF KR-KTONR not = BZ-KTONR GO K.
           IF WH-PG = 6 AND BZ-SYM > 97 PERFORM LOE-ABS
               IF BZ-SYM = 99 GO H.
           MOVE 0 TO WM-TEIL.
           IF BZ-VM = 3 ADD BZ-MW BZ-B1 BZ-B2 TO WM-TEIL.
      *-------------------------------------------> Summen altes Jahr <-
           IF WH-PG = 6; IF BZ-BUDAT > WV-WEND and WM-LOE = 0 GO I
               else IF BZ-SH = 0 ADD BZ-BUBET TO WV-SOLL
                           else ADD BZ-BUBET TO WV-HABEN
                    end-if
                    ADD WM-TEIL TO WV-SOLL
                    IF BZ-SYM = 18 ADD BZ-BUBET TO WV-EBIL
                    end-if GO H.
      *-------------------------------------------> Summen neues Jahr <-
       I.  IF WH-PG = 6 and BZ-SYM = 98 GO H.
           IF BZ-SH = 0 ADD BZ-BUBET TO WM-SOLL
                  else ADD BZ-BUBET TO WM-HABEN.
           ADD WM-TEIL TO WM-SOLL.
           IF BZ-SYM = 18 ADD BZ-BUBET TO WM-EBIL.
           GO H.
      *----------------------------------------> Jahresabschlu·summen <-
       K.  IF WH-PG not = 6 GO Q.
           IF WH-PG = 6 AND WM-LOE = 1 GO Q.
           IF WV-SOLL = 0 AND WV-HABEN = 0 GO Q.
           PERFORM BZ-INIT.
           MOVE KR-KTONR TO BZ-KTONR.
           MOVE WV-WEND TO BZ-BUDAT BZ-BELDAT.
           ADD 10 TO BZ-BUDAT.
           MOVE WV-WKZ TO BZ-USE.
           ADD 9000000 BZ-KTONR GIVING BZ-SEITE.
           MOVE 99 TO BZ-SYM.
           MOVE WV-SOLL TO BZ-SOLL.
           MOVE WV-HABEN TO BZ-HABEN.
           MOVE WV-EBIL TO BZ-EBIL.
           MOVE 72 TO WZ-BUCH.                          *> Grî·e def.
           WRITE BZ-SATZ INVALID REWRITE BZ-SATZ.
           SUBTRACT WV-SOLL FROM KR-SOLL.
           SUBTRACT WV-HABEN FROM KR-HABEN.
           SUBTRACT WV-HABEN FROM WV-SOLL GIVING WH-WERT.
           MOVE 0 TO WV-SOLL WV-HABEN.
           IF WH-WERT = 0 GO Q.                    *> kein Saldovortrag
           PERFORM BZ-INIT.
           MOVE KR-KTONR TO BZ-KTONR.
           ADD 1 WV-WKZ TO BZ-USE.
           ADD 8000000 BZ-KTONR GIVING BZ-SEITE.   *> eindeutiger Key
           IF WH-PG = 6 MOVE WV-SALDAT TO BZ-BUDAT BZ-BELDAT
                   else MOVE WE-WJAHR(1) TO BZ-BUDAT BZ-BELDAT.
           MOVE 0 TO BZ-LFD.
           MOVE 98 TO BZ-SYM.
           IF WH-WERT > 0 MOVE 0 TO BZ-SH
                          ADD WH-WERT TO WM-SOLL
                     else MOVE 1 TO BZ-SH
                          MULTIPLY -1 BY WH-WERT
                          ADD WH-WERT TO WM-HABEN.
           IF WE-EURO = WV-SALDAT COMPUTE BZ-BUBET = WH-WERT / 13,7603
                             else MOVE WH-WERT TO BZ-BUBET.
           MOVE 72 TO WZ-BUCH.
       O.  WRITE BZ-SATZ INVALID REWRITE BZ-SATZ.
      *-----------------------------------------------------------------
       Q.  IF WM-SOLL not = KR-SOLL OR WM-HABEN not = KR-HABEN
               DISPLAY WM-SOLL AT 1920
               DISPLAY WM-HABEN AT 1940
               DISPLAY KR-SOLL AT 2020
               DISPLAY KR-HABEN AT 2040
               IF WH-PG not = 6 AND WM-ART = 1
                    CALL "CAUP" USING "0023300000" WH-CREG.
           MOVE WM-SOLL TO KR-SOLL.
           MOVE WM-HABEN TO KR-HABEN.
           REWRITE KR-SATZ.
           IF BZ-BUDAT = 0 GO Z.
           GO G.
       W.  REWRITE KR-SATZ.
       Z.  EXIT.
      ****************************************** lîschen Abschlu·zeile *
       LOE-ABS SECTION.
       A.  IF WM-LOE = 0 GO Z.
           IF BZ-SYM = 99 GO Q.
           IF BZ-SYM = 98; IF BZ-USE = (WV-WKZ + 1) GO Q.
           GO Z.
       Q.  DELETE BUCHZEIL INVALID GO Z.
           MOVE 99 TO BZ-SYM.
       Z.  EXIT.
      ******************************************************************
       WJ-WECHSEL SECTION.
       A.  IF AUF and WI > 1 ADD -1 TO WI.
           IF AB and KO-WEND(WI) not = 0 ADD 1 TO WI.
           MOVE KO-WEND(WI) TO KO-ABDAT.
      *    PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 4
      *        IF WV-WBEG = KO-WBEG(WX) MOVE WX TO WY.
      *    IF WY > 3 AND AB OR (WY = 1 AND AUF) GO X.
      *    IF AB  ADD 1 TO WY.
      *    IF AUF ADD -1 TO WY.
      *    IF KO-WBEG(WY) = 0 GO X.
           MOVE KO-WBEG(WI) TO WV-WBEG.
           MOVE KO-WEND(WI) TO WV-WEND.
           GO Z.
       X.  DISPLAY "keine Daten vorhanden" with highlight BLINK AT 2401.
       Z.  EXIT.
      ************************************************* Jahresabschlu· *
       JAHRABS SECTION.
       A.  CALL "CAUP" USING "0710100860000" WH-CREG.
           CALL "CAUP" USING "1303010980" WH-CREG.
           CALL "CAUP" USING "1318012580" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           MOVE 0 TO WH-VAR.
           DISPLAY " Jahresabschlu· "  with highlight AT VDU-LP.
           ADD -92 VDU-ECK GIVING VDU-LP.
           DISPLAY "  Neues Wirtschaftsjahr mu· eingetragen sein!  "
                with highlight reverse-video AT VDU-LP.
      *    ADD 203 VDU-ECK GIVING VDU-LP.
      *    DISPLAY "Abschlu·datum: " AT VDU-LP.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "fÅr das Wirtschaftsjahr: " AT VDU-LP.
           MOVE 9 TO WH-KEY.
       B.  READ KONSTANT NO LOCK INVALID STOP RUN.
           IF ZUGRIF PERFORM BESETZT GO B.
       C.  ADD 218 VDU-ECK GIVING VDU-LP.
      *    MOVE KO-ABDAT TO WC-DATUM.
      *    PERFORM DATDREH.
      *    DISPLAY VDU-DATUM with highlight AT VDU-LP.
           PERFORM VARYING WI FROM 4 BY -1 UNTIL WI = 0 OR
               KO-WEND(WI) = KO-ABDAT CONTINUE.
           IF WI = 0 and KO-WBEG(1) > KO-ABDAT ADD 1 TO WI.
           IF WI = 0 or WI > 3 or
               KO-WBEG(WI + 1) = 0 or KO-WEND(WI + 1) = 0
                  DISPLAY "Neues oder altes Wirtschaftsjahr fehlt"
                       with BLINK at 2401
                  PERFORM WEITER GO X.
      *    MOVE KO-WKZ(WI + 1) TO WD-PZ.
           MOVE KO-WKZ(WI) TO WD-PZ.
           ADD 328 VDU-ECK GIVING VDU-LP.
           MOVE KO-WBEG(WI + 1) TO WV-SALDAT.
           MOVE KO-WBEG(WI) TO WC-DATUM.
           PERFORM DATDREH.
           DISPLAY WD-PZ with highlight AT VDU-LP " / "
                VDU-DATUM with highlight " - ".
      *    MOVE KO-WEND(WI + 1) TO WC-DATUM.
           MOVE KO-WEND(WI) TO WC-DATUM.
           PERFORM DATDREH.
           DISPLAY VDU-DATUM with highlight AT 0000.
           MOVE KO-WBEG(WI) TO WV-WBEG.
           MOVE KO-WEND(WI) TO WV-WEND.
           MOVE KO-WKZ(WI) TO WV-WKZ.
           ADD 50 TO WV-WKZ.
           MOVE WV-WKZ TO WV-WKZ.
       E.  ADD 503 VDU-ECK GIVING VDU-LP.
           DISPLAY "<ret>= Start, <esc>= Abbruch < >" AT VDU-LP.
           ADD 603 VDU-ECK GIVING VDU-LP.
           DISPLAY "<tab>= PrÅfung, < / >= Wechsel Wirtschaftsjahr"
                AT VDU-LP.
           ADD 703 VDU-ECK GIVING VDU-LP.
           DISPLAY "<#>= Wechsel: alles/Sachkonten/Debitoren/Kreditoren"
                with highlight AT VDU-LP.
           ADD 145 VDU-ECK GIVING VDU-LP.
           EVALUATE WH-VAR
               WHEN 0 DISPLAY " alle Konten " with highlight AT VDU-LP
               WHEN 1 DISPLAY "  Sachkonten " with highlight AT VDU-LP
               WHEN 2 DISPLAY "  Debitoren  " with highlight AT VDU-LP
               WHEN 3 DISPLAY " Kreditoren  " with highlight AT VDU-LP.
           CALL "CAUP" USING "1005330000" WH-CREG.
           IF AUF or AB PERFORM WJ-WECHSEL GO C.
           IF ESC GO X.
           IF TABL PERFORM PRUEF-BUCH GO E.
           MOVE 0 TO WM-LOE.
           IF KIST; IF WH-VAR < 3 ADD 1 TO WH-VAR
                    else MOVE 0 TO WH-VAR.
           IF not RET and not SENT GO E.
           IF RET MOVE KO-WEND(WI) TO KO-ABDAT
      *    IF RET MOVE KO-WEND(WI + 1) TO KO-ABDAT
                  REWRITE KO-SATZ.
           IF SENT MOVE 1 TO WM-LOE
                   SET RET TO TRUE.
           EVALUATE WH-VAR
              WHEN 0 PERFORM SACHRECH
                     PERFORM DEBRECH
                     PERFORM KREDRECH
              WHEN 1 PERFORM SACHRECH
              WHEN 2 PERFORM DEBRECH
              WHEN 3 PERFORM KREDRECH
              WHEN OTHER GO E.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ******************************************************************
       OB-LFTEINZ SECTION.
       A.  MOVE 0 TO WV-KTONR.
           IF WH-NUM = 0 AND WH-MCODE = SPACE GO Z.
           IF WH-NUM not = 0 GO C.
           MOVE 3 TO WH-NUM.
           CALL "FIBANZ" USING "30KRED" WH-CREG.
           CANCEL "FIBANZ".
           IF KR-KTONR = 0 SET ESC TO TRUE GO Z.
           MOVE KR-KTONR TO WH-NUM.
       C.  MOVE WH-NUM TO KR-KTONR.
           READ KREDIT WITH NO LOCK INVALID PERFORM NO-REC
                SET ESC TO TRUE GO Z.
           IF ZUGRIF PERFORM BESETZT GO C.
           DIVIDE 10 INTO KR-KTONR GIVING WD-KTO
           MOVE KR-BEZ TO WT-TX.
           INSPECT WT-TX REPLACING ALL "#" BY ",".
           ADD 403 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-KTO with highlight AT VDU-LP " " WT-TX(1:47).
       F.  DISPLAY "<ret>= Start, <esc>= Abbruch < >" AT 2301.
           CALL "CAUP" USING "0023310000" WH-CREG.
           IF ESC GO Z.
           IF not RET GO F.
           MOVE KR-KTONR TO WV-KTONR.
           MOVE KR-KTOART TO WG.
       Z.  EXIT.
      **************************************** OP-Liste Personenkonten *
       KREDOPS SECTION.
       A.  CALL "CAUP" USING "07070805690000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " offene Posten Lieferanten "
               with highlight AT VDU-LP.
           MOVE "LIEF-OPS.LST" TO WH-DRUNAM.
           ADD 245 VDU-ECK GIVING VDU-LP.
           DISPLAY WH-DRUNAM with highlight AT VDU-LP.
           MOVE 0 TO WH-MOD.
       C.  ADD 133 VDU-ECK GIVING VDU-LP.
           CALL "CAUP" USING "16CLRFEN" WH-CREG.
           IF WH-MOD = 1 DISPLAY "  numerisch   " AT VDU-LP
                    else DISPLAY " alphabetisch " AT VDU-LP.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "<#>= Start alle, <ret>= Start fÑllige, <esc>= Abbruc
      -        "h <       >" AT VDU-LP.
           MOVE 0 TO WE.
           IF WH-MOD = 1 DISPLAY "< >= Listung alphabetisch" AT 2401
                    else DISPLAY "< >= Listung numerisch" AT 2401.
           CALL "CAUP" USING "1003586007" WH-CREG.
           IF ESC GO X.
           IF KIST MOVE 1 TO WE.
           IF AUF; IF WH-MOD = 1 MOVE 0 TO WH-MOD
                   else MOVE 1 TO WH-MOD.
           MOVE 0 TO WG.
           PERFORM OB-LFTEINZ.
           IF not RET AND not KIST GO C.
           PERFORM UNTER.
           MOVE 0 TO WS-ART(1) WS-ART(2) WS-ART(3) WZ-SEITE
               WM-SOLL WM-HABEN WZ-ZEILEN WG-SOLL(1) WG-HABEN(1).
       E.  ADD 403 VDU-ECK GIVING VDU-LP.
           IF WV-KTONR not = 0 MOVE 1 TO WE GO H.
           MOVE 0 TO KR-KTONR WB.
           IF WH-MOD = 1 START KREDIT KEY not < KR-KEY INVALID GO Z
                         end-start GO G.
           MOVE LOW-VALUES TO KR-MCODE.
           START KREDIT KEY not < KR-MCODE INVALID GO Z.
       G.  READ KREDIT NEXT NO LOCK AT END PERFORM PERSGRU GO Q.
           IF ZUGRIF PERFORM BESETZT GO G.
           PERFORM KB-STAT
               IF ESC PERFORM LISDAT
                      DELETE FILE DRUCKER
                      GO X.
       H.  SUBTRACT KR-HABEN FROM KR-SOLL GIVING WM-TEIL.
           MOVE KR-KTOART TO WF.
           IF WF not = WG ADD 1 TO WS-ART(WF + 1) GO G.
      *-------------------------------------> wenn Saldo dann drucken <-
           MOVE 0 TO WS-OPS OP-REF OP-LFD WK-ZEIL OP-VALDAT.
           MOVE KR-KTONR TO OP-KTONR.
           MOVE 3 TO OP-DK.
           START OPDATEI KEY not < OP-KEY INVALID GO G.
       J.  READ OPDATEI NEXT NO LOCK AT END GO P.
           IF ZUGRIF PERFORM BESETZT GO J.
           SUBTRACT OP-ZABETRAG FROM OP-REBETRAG GIVING WH-WERT.
           IF WH-WERT = 0 GO J.
           IF OP-KTONR not = KR-KTONR GO P.
      *----------------------------------------> FÑlligkeit ermitteln <-
           MOVE OP-KOND TO WH-KK.
           IF OP-KOND = 0 MOVE KR-KOND TO WH-KK.
           IF WH-TGN = 0 MOVE 30 TO WH-TGN.
           MOVE WH-TGN TO WH-TAG.
           PERFORM FAELLIG.
           IF WE = 0; IF WB-TAGE > WM-DATUM GO J.
           MOVE WB-TAGE TO WS-DATUM.
           PERFORM KREOPKOPF.
           MOVE OP-VALDAT TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRO-DAT.
           MOVE WE-SYM(OP-SYM + 1) TO DRO-SYM.
           MOVE OP-REF TO DRO-REFNR.
           MOVE OP-REBETRAG TO DRO-REBET.
           MOVE OP-ZABETRAG TO DRO-ZABET.
           SUBTRACT OP-ZABETRAG FROM OP-REBETRAG GIVING WH-WERT.
           ADD WH-WERT TO WS-OPS.
           MOVE WH-WERT TO DRO-OFFEN.
           MOVE WS-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRO-FAELLIG.
           IF OP-ZPBET not = 0 MOVE " Za" TO DRO-ZAHL.
           PERFORM DRUCK.
           IF WK-ZEIL = 0 MOVE 1 TO WK-ZEIL
                     else MOVE 2 TO WK-ZEIL.
           GO J.
       P.  SUBTRACT KR-SOLL FROM KR-HABEN GIVING WM-TEIL.
           IF WS-OPS not = 0 AND WK-ZEIL = 2 OR WM-TEIL not = 0
               OR WM-TEIL not = WS-OPS
               PERFORM KREOPKOPF
               MOVE "Summe OP's" TO DRP-STR(50:)
               MOVE WS-OPS TO DRO-OFFEN
               IF WS-OPS not = WM-TEIL AND WE = 1
                   MOVE "abw" TO DRO-FAELLIG end-if
               PERFORM DRUCK.
           MOVE 2 TO WZ-SCHALT.
           IF WV-KTONR = 0 GO G.
       Q.  IF WG not = 2 ADD 1 TO WG
               IF WS-ART(WG + 1) not = 0 GO E else GO Q.
           IF WM-SOLL not = 0 OR WM-HABEN not = 0
               MOVE ALL "ƒ" TO DRP-STR(1:)
               PERFORM DRUCK
               MOVE "S u m m e n:" TO DRP-STR(10:15)
               MOVE WM-SOLL TO DRP-SOLL
               MOVE WM-HABEN TO DRP-HABEN
               SUBTRACT WM-SOLL FROM WM-HABEN GIVING DRP-SALDO
               PERFORM DRUCK
               MOVE ALL "=" TO DRP-STR(63:) PERFORM DRUCK
               MOVE 2 TO WZ-SCHALT
               MOVE 0 TO WM-SOLL WM-HABEN.
       W.  PERFORM LISDAT.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ***************************************** Kreitoren-OP Kopfdruck *
       KREOPKOPF SECTION.
       A.  PERFORM OPPERS-KOPF.
           IF WK-ZEIL = 0 MOVE KR-BEZ TO WT-TX
               IF WZ-ZEILEN > (WH-SIZE - 2) MOVE 70 TO WZ-ZEILEN
                   PERFORM OPPERS-KOPF
               end-if
               DIVIDE 10 INTO KR-KTONR GIVING DRP-KTONR
               INSPECT WT-TX REPLACING ALL "#" BY ","
               MOVE WT-TX TO DRP-BEZ
               DISPLAY DRP-KTONR with highlight AT VDU-LP WT-TX(1:47)
               SUBTRACT KR-HABEN FROM KR-SOLL GIVING WM-TEIL
               MOVE WM-TEIL TO DRP-SALDO
               ADD KR-SOLL TO WM-SOLL WG-SOLL(1)
               ADD KR-HABEN TO WM-HABEN WG-HABEN(1)
               PERFORM DRUCK.
       Z.  EXIT.
      ******************************************************************
       OB-KDEINZ SECTION.
       A.  MOVE 0 TO WV-KTONR.
           IF WH-NUM = 0 AND WH-MCODE = SPACE GO Z.
           IF WH-NUM not = 0 GO C.
           MOVE 2 TO WH-NUM.
           CALL "FIBANZ" USING "30DEBSUCH" WH-CREG.
           CANCEL "FIBANZ".
           IF DE-KTONR = 0 SET ESC TO TRUE GO Z.
           MOVE DE-KTONR TO WH-NUM.
       C.  MOVE WH-NUM TO DE-KTONR.
           READ DEBITOR WITH NO LOCK INVALID PERFORM NO-REC
                SET ESC TO TRUE GO Z.
           IF ZUGRIF PERFORM BESETZT GO C.
           DIVIDE 10 INTO DE-KTONR GIVING WD-KTO
               MOVE DE-BEZ TO WT-TX.
               INSPECT WT-TX REPLACING ALL "#" BY ",".
               MOVE WT-TX TO DRP-BEZ.
               ADD 403 VDU-ECK GIVING VDU-LP
               DISPLAY WD-KTO with highlight AT VDU-LP " " WT-TX(1:47).
       F.  DISPLAY "<ret>= Start, <esc>= Abbruch < >" AT 2301.
           CALL "CAUP" USING "0023310000" WH-CREG.
           IF ESC GO Z.
           IF not RET GO F.
           MOVE DE-KTONR TO WV-KTONR.
           MOVE DE-KTOART TO WG.
       Z.  EXIT.
      ******************************************* von - bis Kunden-Nr. *
       VOBI SECTION.
       A.  ADD 203 VDU-ECK GIVING VDU-LP.
           MOVE 0 TO WH-VON WH-BIS.
           DISPLAY "von Kd.Nr.:          bis Kd.Nr.:        " AT VDU-LP.
           DISPLAY "<esc>= Abbruch, <ret>= von Kunden-Nr." AT 2301.
           CALL "CAUP" USING "1002156007" WH-CREG.
           IF ESC GO Z.
           IF not RET GO A.
           MOVE WH-WERT TO WH-VON WH-BIS.
           COMPUTE WD-KTO = WH-WERT / 10.
           DISPLAY WD-KTO with highlight AT VDU-LP.
       C.  DISPLAY "<esc>= Abbruch, <ret>= bis Kunden-Nr." AT 2301.
           CALL "CAUP" USING "1002366007" WH-CREG.
           IF ESC GO Z.
           IF WOLI or AUF GO A.
           IF not RET GO C.
           MOVE WH-WERT TO WH-BIS.
           COMPUTE WD-KTO = WH-WERT / 10.
           DISPLAY WD-KTO with highlight AT VDU-LP.
       Z.  EXIT.
      ***************************************** Listvariante Debitoren *
       VARIANTE SECTION.
       A.  ADD 133 VDU-ECK GIVING VDU-LP.
           IF WH-MOD = 1 DISPLAY "  numerisch   " AT VDU-LP
                    else DISPLAY " alphabetisch " AT VDU-LP.
           ADD 153 VDU-ECK GIVING VDU-LP.
           IF WG = 9 DISPLAY " ohne Gruppen " AT VDU-LP
                else DISPLAY " mit Gruppen  " AT VDU-LP.
       Z.  EXIT.
      ******************************************************************
       KB-STAT SECTION.
       A.  CALL "CBL_GET_KBD_STATUS" using WH-KB.
           IF WH-KB = 0 GO Z.
       C.  DISPLAY "Programm unterbrochen!" with highlight AT 2401
              "<esc>= Abbruch, <ret>= Fortsetzung " AT 2424.
           CALL "CAUP" USING "0024591000" WH-CREG.
           PERFORM UNTER.
           IF not RET and not ESC GO C.
       Z.  EXIT.
      ****************************************** Text 'Unterbrechnung' *
       UNTER SECTION.
       A.  DISPLAY "Unterbrechung mîglich" AT 2301.
       Z.  EXIT.
      ********************************************** Listung OP-Kunden *
       DEBOPS SECTION.
       A.  CALL "CAUP" USING "07070805690000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " offene Posten KUNDEN " with highlight AT VDU-LP.
           MOVE "KUND-OPS.LST" TO WH-DRUNAM.
           ADD 245 VDU-ECK GIVING VDU-LP.
           DISPLAY WH-DRUNAM with highlight AT VDU-LP.
           MOVE 0 TO WH-MOD WG WH-VON.
           MOVE 999999 TO WH-BIS.
           CALL "CAUP" USING "16CLRFEN" WH-CREG.
       C.  PERFORM VARIANTE.
           MOVE "KUND-OPS.LST" TO WH-DRUNAM.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "<#>= Start alle, <ret>= Start fÑllige, <esc>= Abbruc
      -        "h <       >" AT VDU-LP.
           MOVE 0 TO WE.
           DISPLAY "alpha+<ret>= Suchen, <ret>= Kunden-Kto.Nr." AT 2301.
           IF WH-MOD = 1 DISPLAY "< >= Listung alphabetisch" AT 2401
                    else DISPLAY "< >= Listung numerisch" AT 2401.
           IF WG = 9 DISPLAY ", < >= Wechsel mit Gruppen " AT 0000
                else DISPLAY ", < >= Wechsel ohne Gruppen" AT 0000.
           DISPLAY ", <>= von-bis" AT 0000.
           CALL "CAUP" USING "1003586007" WH-CREG.
           IF ESC GO X.
           IF APOS MOVE 5 TO WE
                   SET RET TO TRUE.
           IF KIST MOVE 1 TO WE.
           IF AUF; IF WH-MOD = 1 MOVE 0 TO WH-MOD
                   else MOVE 1 TO WH-MOD.
           IF AB; IF WG = 0 MOVE 9 TO WG
                       else MOVE 0 TO WG.
           IF WOLI PERFORM VOBI
                   IF ESC MOVE 0 TO WH-VON WH-BIS
                   else MOVE 1 TO WH-MOD
                        MOVE 9 TO WG
                        PERFORM VARIANTE GO C.
           IF WORE PERFORM OP-TERMIN-LISTE GO C.
           IF not RET AND not KIST GO C.
           PERFORM OB-KDEINZ.
           IF ESC GO C.
           PERFORM UNTER.
           MOVE 0 TO WS-ART(1) WS-ART(2) WS-ART(3) WZ-SEITE
               WM-SOLL WM-HABEN WZ-ZEILEN WG-SOLL(1) WG-HABEN(1).
       E.  ADD 403 VDU-ECK GIVING VDU-LP.
           IF WV-KTONR not = 0 MOVE 1 TO WE GO H.
           MOVE 0 TO DE-KTONR WB.
           MOVE WH-VON TO DE-KTONR.
           IF WH-MOD = 1 START DEBITOR KEY not < DE-KEY INVALID GO Z
                         end-start GO G.
           MOVE LOW-VALUES TO DE-MCODE.
           START DEBITOR KEY not < DE-MCODE INVALID GO Z.
       G.  READ DEBITOR NEXT NO LOCK AT END PERFORM PERSGRU GO Q.
           IF ZUGRIF PERFORM BESETZT GO G.
           PERFORM KB-STAT
               IF ESC PERFORM LISDAT
                      DELETE FILE DRUCKER
                      GO X.
           IF DE-KTONR > WH-BIS GO Q.
       H.  SUBTRACT DE-HABEN FROM DE-SOLL GIVING WM-TEIL.
      *    IF WE = 5 PERFORM FEHLOPS GO G.
           MOVE DE-KTOART TO WF.
           IF WG not = 9;
               IF WF not = WG ADD 1 TO WS-ART(WF + 1) GO G.
      *-------------------------------------> wenn Saldo dann drucken <-
           MOVE 0 TO WS-OPS OP-REF OP-LFD OP-VALDAT WK-ZEIL WS-EUR.
           MOVE DE-KTONR TO OP-KTONR.
           MOVE 2 TO OP-DK.
           START OPDATEI KEY not < OP-KEY INVALID GO G.
       J.  READ OPDATEI NEXT NO LOCK AT END GO P.
           IF ZUGRIF PERFORM BESETZT GO J.
           IF OP-KTONR not = DE-KTONR GO P.
280506     if wm-datum = 20060528 PERFORM exl-ops go j.
           SUBTRACT OP-ZABETRAG FROM OP-REBETRAG GIVING WH-WERT.
           IF WH-WERT = 0 GO J.
           IF OP-FSYM = "Ats"
               COMPUTE WM-EUR rounded = WH-WERT / 13,7603
               ADD WM-EUR TO WS-EUR.
      *----------------------------------------> FÑlligkeit ermitteln <-
           IF OP-VALDAT(1:3)= "0  " DELETE OPDATEI
                                    MOVE OP-REDAT TO OP-VALDAT
                                    WRITE OP-SATZ.
           IF OP-VALDAT = 0 MOVE OP-REDAT TO OP-VALDAT.
           MOVE OP-KOND TO WH-KK.
           IF OP-KOND = 0 MOVE DE-KOND TO WH-KK.
           IF WH-TGN = 0 MOVE 30 TO WH-TGN.
           MOVE WH-TGN TO WH-TAG.
           PERFORM FAELLIG.
           IF WE = 0; IF WB-TAGE > WM-DATUM GO J.
           MOVE WB-TAGE TO WS-DATUM.
           PERFORM DEBOPKOPF.
           MOVE OP-VALDAT TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRO-DAT.
           MOVE WE-SYM(OP-SYM + 1) TO DRO-SYM.
           MOVE OP-REF TO DRO-REFNR.
           MOVE OP-REBETRAG TO DRO-REBET.
           MOVE OP-ZABETRAG TO DRO-ZABET.
      *    ADD OP-REBETRAG TO WM-SOLL.
      *    ADD OP-ZABETRAG TO WM-HABEN.
           SUBTRACT OP-ZABETRAG FROM OP-REBETRAG GIVING WH-WERT.
990722*    MOVE OP-REBETRAG TO WH-WERT.
           ADD WH-WERT TO WS-OPS.
           MOVE WH-WERT TO DRO-OFFEN.
           MOVE WS-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRO-FAELLIG.
           PERFORM DRUCK.
           IF WK-ZEIL = 0 MOVE 1 TO WK-ZEIL
                     else MOVE 2 TO WK-ZEIL.
           GO J.
       P.  IF WS-OPS not = 0 AND WK-ZEIL = 2 OR WM-TEIL not = 0
               OR WM-TEIL not = WS-OPS
               PERFORM DEBOPKOPF
               MOVE "Summe OP's" TO DRP-STR(40:)
               MOVE WS-OPS TO DRO-OFFEN
               IF WS-OPS not = WM-TEIL AND WE = 1
                   MOVE "abw" TO DRO-FAELLIG end-if
               MOVE WS-EUR TO DRP-SALDO
               PERFORM DRUCK.
           MOVE 2 TO WZ-SCHALT.
           IF WV-KTONR = 0 GO G.
       Q.  IF WG not = 9;
               IF WG not = 2 ADD 1 TO WG
                   IF WS-ART(WG + 1) not = 0 GO E else GO Q.
           IF WM-SOLL not = 0 OR WM-HABEN not = 0
               MOVE ALL "ƒ" TO DRP-STR(1:)
               PERFORM DRUCK
               MOVE "S u m m e n:" TO DRP-STR(10:15)
               MOVE WM-SOLL TO DRP-SOLL
               MOVE WM-HABEN TO DRP-HABEN
               SUBTRACT WM-HABEN FROM WM-SOLL GIVING DRP-SALDO
               PERFORM DRUCK
               MOVE ALL "=" TO DRP-STR(63:) PERFORM DRUCK
               MOVE 2 TO WZ-SCHALT
               MOVE 0 TO WM-SOLL WM-HABEN.
       W.  PERFORM LISDAT.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ******************************* Op-Termin-Liste per: (Bilanz) ****
       OP-TERMIN-LISTE SECTION.
       A.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           CALL "CAUP" USING "270708056900013" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " OP-Liste " with highlight AT VDU-LP.
       C.  ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "OP-Liste per:        " AT VDU-LP.
           PERFORM WIJAHR.
           MOVE KO-ABDAT TO WV-DATUM WC-DATUM.
           PERFORM DATDREH.
           ADD 317 VDU-ECK GIVING VDU-LP.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           MOVE WV-DATUM TO WZ-DATUM WH-WERT.
           DISPLAY "<esc>= Abbruch, <ret>= Stichtagsdatum" AT 2301.
           CALL "CAUP" USING "1103176006" WH-CREG.
           IF ESC GO X.
           IF not RET GO C.
           IF WZ-DATUM = 0 GO C.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           ADD 249 VDU-ECK GIVING VDU-LP.
           MOVE "OPPERLI.LST" TO WH-DRUNAM.
           DISPLAY WH-DRUNAM with highlight AT VDU-LP.
       E.  DISPLAY "<esc>= Abbruch, < />= zurÅck, <ret>= Start < >"
               AT 2301.
           CALL "CAUP" USING "0023460000" WH-CREG.
           IF ESC GO X.
           IF WOLI or AUF GO C.
           IF not RET GO E.
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO DE-SATZ.
           START DEBITOR KEY > DE-KEY INVALID GO Z.
       G.  READ DEBITOR NEXT IGNORE LOCK AT END GO Q.
           IF ZUGRIF PERFORM BESETZT GO G.
       H.  SUBTRACT DE-HABEN FROM DE-SOLL GIVING WM-TEIL.
           IF WE = 5 PERFORM FEHLOPS GO G.
           MOVE DE-KTOART TO WF.
           IF WG not = 9;
               IF WF not = WG ADD 1 TO WS-ART(WF + 1) GO G.
      *-------------------------------------> wenn Saldo dann drucken <-
           MOVE 0 TO WS-OPS OP-REF OP-LFD OP-VALDAT WK-ZEIL WS-EUR.
           MOVE DE-KTONR TO OP-KTONR.
           MOVE 2 TO OP-DK.
           START OPDATEI KEY not < OP-KEY INVALID GO G.
       J.  READ OPDATEI NEXT NO LOCK AT END GO P.
           IF ZUGRIF PERFORM BESETZT GO J.
           IF OP-KTONR not = DE-KTONR GO P.
280506     if wm-datum = 20060528 PERFORM exl-ops go j.
           SUBTRACT OP-ZABETRAG FROM OP-REBETRAG GIVING WH-WERT.
           IF WH-WERT = 0 GO J.
           IF OP-FSYM = "Ats"
               COMPUTE WM-EUR rounded = WH-WERT / 13,7603
               ADD WM-EUR TO WS-EUR.
      *----------------------------------------> FÑlligkeit ermitteln <-
           IF OP-VALDAT(1:3)= "0  " DELETE OPDATEI
                                    MOVE OP-REDAT TO OP-VALDAT
                                    WRITE OP-SATZ.
           IF OP-VALDAT = 0 MOVE OP-REDAT TO OP-VALDAT.
           MOVE OP-KOND TO WH-KK.
           IF OP-KOND = 0 MOVE DE-KOND TO WH-KK.
           IF WH-TGN = 0 MOVE 30 TO WH-TGN.
           MOVE WH-TGN TO WH-TAG.
           PERFORM FAELLIG.
           IF WE = 0; IF WB-TAGE > WM-DATUM GO J.
           MOVE WB-TAGE TO WS-DATUM.
           PERFORM DEBOPKOPF.
           MOVE OP-VALDAT TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRO-DAT.
           MOVE WE-SYM(OP-SYM + 1) TO DRO-SYM.
           MOVE OP-REF TO DRO-REFNR.
           MOVE OP-REBETRAG TO DRO-REBET.
           MOVE OP-ZABETRAG TO DRO-ZABET.
      *    ADD OP-REBETRAG TO WM-SOLL.
      *    ADD OP-ZABETRAG TO WM-HABEN.
           SUBTRACT OP-ZABETRAG FROM OP-REBETRAG GIVING WH-WERT.
990722*    MOVE OP-REBETRAG TO WH-WERT.
           ADD WH-WERT TO WS-OPS.
           MOVE WH-WERT TO DRO-OFFEN.
           MOVE WS-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRO-FAELLIG.
           PERFORM DRUCK.
           IF WK-ZEIL = 0 MOVE 1 TO WK-ZEIL
                     else MOVE 2 TO WK-ZEIL.
           GO J.
       P.
       Q.
       X.
       Z.  EXIT.
      ************************************ Excelausgabe in Excel-datei *
       EXL-OPS SECTION.
       A.  IF OP-REDAT < 20050400 GO Z.
           IF WM-FIBEX = 0
               MOVE "OPXXXXX.TXT" TO WH-FIBNAM
               MOVE DE-KTONR TO WD-KTO
               MOVE WD-KTO TO WH-FIBNAM(3:5)
               OPEN OUTPUT EXCELIS
               MOVE 1 TO WM-FIBEX.
           MOVE OP-RENUM TO EXO-BELEG.
           ADD 20000000 OP-ZADAT GIVING EXO-ZADAT.
           MOVE OP-ZABETRAG TO EXO-ZABET.
           MOVE OP-REBETRAG TO EXO-OBET.
           MOVE x"09" TO EXO-TA EXO-TB EXO-TC.
           WRITE EXO-SATZ.
           MOVE SPACE TO EXC-SATZ.
       Z.  EXIT.
      ******************************************* fehlende OP`s suchen *
       FEHLOPS SECTION.
       A.  IF WM-TEIL = 0 GO Z.
           MOVE HIGH-VALUE TO BZ-SATZ.
           MOVE DE-KTONR TO BZ-KTONR.
           START BUCHZEIL KEY < BZ-KEY INVALID GO Z.
       C.  READ BUCHZEIL PREVIOUS IGNORE LOCK AT END GO Q.
           IF BZ-KTONR not = DE-KTONR GO Q.
           IF BZ-BUDAT < 20010701 GO Q.
           IF BZ-SYM not = 0 and not = 10 GO C.
           INITIALIZE OP-SATZ.
           MOVE BZ-KTONR TO OP-KTONR.
           MOVE 2 TO OP-DK.
           MOVE BZ-BELNR TO OP-RENUM.
      *    START OPDATEI KEY not < OP-RKEY INVALID GO Q.
       E.  READ OPDATEI NEXT IGNORE LOCK AT END GO K.
           IF BZ-KTONR not = OP-KTONR GO K.
           IF OP-RENUM = BZ-BELNR GO C.
      *----------------------------------------------> nicht gefunden <-
       K.  IF DE-KTONR = 995002 GO C.
           IF WM-OPEN = 0 MOVE "F-OPS.LST" TO WH-DRUNAM
               MOVE 13 TO WH-P1
               PERFORM BEG-DRU.
           DIVIDE 10 INTO DE-KTONR GIVING DRA-KTONR.
           MOVE BZ-BELNR TO DRA-BELNR.
           DISPLAY DRA-SATZ(1:35) AT 2301.
           PERFORM DRUCK.
      *--------------------------------------> OP automaitsch anlegen <-
           MOVE BZ-KTONR TO OP-KTONR.
           MOVE 0 TO OP-SA.
           MOVE 2 TO OP-DK.
           MOVE BZ-BELDAT TO OP-VALDAT OP-REDAT.
           MOVE 0 TO OP-LFD.
           MOVE BZ-BELNR TO OP-RENUM OP-REF.
           MOVE DE-KOND TO OP-KOND.
           MOVE BZ-MW TO OP-MW.
           MOVE BZ-U1 TO OP-U1.
           MOVE BZ-B1 TO OP-B1.
           MOVE BZ-U2 TO OP-U2.
           MOVE BZ-B2 TO OP-B2.
           MOVE BZ-FWBET TO OP-REBETRAG OP-SKTOBAS.
           MOVE 0 TO OP-ZABETRAG OP-SYM OP-ZUSTAND OP-ZADAT
             OP-ZPBET OP-ZPSKTO OP-MSTUFE OP-MAHNDAT OP-ZESS.
           MOVE SPACE TO OP-FREMDNR.
           MOVE "Ats" TO OP-FSYM.
           MOVE 7,2672835 TO OP-KURS.
       O.  WRITE OP-SATZ INVALID ADD 1 TO OP-LFD GO O.
           GO C.
       Q.
       Z.  EXIT.
      ***************************************** Debitoren-OP Kopfdruck *
       DEBOPKOPF SECTION.
       A.  PERFORM OPPERS-KOPF.
           IF WK-ZEIL = 0 MOVE DE-BEZ TO WT-TX
               IF WZ-ZEILEN > (WH-SIZE - 2) MOVE 70 TO WZ-ZEILEN
                   PERFORM OPPERS-KOPF
               end-if
               DIVIDE 10 INTO DE-KTONR GIVING DRP-KTONR
               INSPECT WT-TX REPLACING ALL "#" BY ","
               MOVE WT-TX TO DRP-BEZ
               DISPLAY DRP-KTONR with highlight AT VDU-LP WT-TX(1:47)
               SUBTRACT DE-HABEN FROM DE-SOLL GIVING WM-TEIL
               MOVE WM-TEIL TO DRP-SALDO
               ADD DE-SOLL TO WM-SOLL WG-SOLL(1)
               ADD DE-HABEN TO WM-HABEN WG-HABEN(1)
               PERFORM DRUCK.
       Z.  EXIT.
      **************************************************** Journalkopf *
       PERSGRU SECTION.
       A.  IF WH-PG = 7 and WG = 9 GO Z.
           IF WG-SOLL(1) not = 0 OR WG-HABEN(1) not = 0
               MOVE ALL "ƒ" TO DRP-STR(63:) PERFORM DRUCK
               MOVE "Summe Gruppe:" TO DRP-STR(10:15)
               MOVE WG TO WD-KZ
               MOVE WD-KZ TO DRP-STR(25:1)
               MOVE WG-SOLL(1) TO DRP-SOLL
               MOVE WG-HABEN(1) TO DRP-HABEN
               SUBTRACT WG-HABEN(1) FROM WG-SOLL(1) GIVING DRP-SALDO
               PERFORM DRUCK
               MOVE 2 TO WZ-SCHALT
               MOVE 0 TO WG-SOLL(1) WG-HABEN(1).
       Z.  EXIT.
      *************************************** Kopf Kundenkontenlistung *
       OPPERS-KOPF SECTION.
       A.  IF WZ-ZEILEN > WH-SIZE WRITE DRA-SATZ AFTER PAGE
               MOVE 0 TO WZ-ZEILEN.
           IF WZ-ZEILEN > 0 GO Z.
           MOVE 2 TO WH-P1.
           IF WM-KO = 0 MOVE 5 TO WH-P2
                        MOVE 41 TO WH-SIZE
                        MOVE 0 TO WZ-SCHALT
                   else MOVE 4 TO WH-P2
                        MOVE 62 TO WH-SIZE.
           IF WM-DRU = 1 MOVE 40 TO WH-SIZE                    *> Laser
               MOVE 12 TO WH-P1.
           PERFORM BEG-DRU.
           ADD 1 TO WZ-SEITE.
           IF WM-DRU not = 1 MOVE 1 TO WZ-SCHALT.
           MOVE WK-FIRMA TO DRP-STR(1:30).
           MOVE "offene Posten" TO DRP-STR(30:).
           IF WH-PG = 7 MOVE "Kunden      per:" TO DRP-STR(44:).
           IF WH-PG = 8 MOVE "Lieferanten per:" TO DRP-STR(44:).
           MOVE WH-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRP-STR(61:10).
           IF WE = 0 MOVE "nur fÑllige" TO DRP-STR(73:).
           MOVE "Seite:" TO DRP-STR(98:).
           MOVE WZ-SEITE TO WD-NUM.
           MOVE WD-NUM(5:) TO DRP-STR(105:).
           PERFORM DRUCK.
           MOVE ALL "ƒƒƒ" TO DRP-STR(1:).
           PERFORM DRUCK.
           MOVE "Konto-Nr B e z e i c h n u n g
      -       "                 Soll         Haben           Saldo"
               TO DRP-STR(1:).
           PERFORM DRUCK.
           MOVE "  Datum  Sy Ref.-Nr.   Re-Betrag    Za-Betrag        Of
      -         "fen   FÑlligk. " TO DRP-STR(10:).
           PERFORM DRUCK.
           MOVE ALL "ƒƒƒ" TO DRP-STR(1:).
           PERFORM DRUCK.
           MOVE 2 TO WZ-SCHALT.
       Z.  EXIT.
      ***************************************** Ermittlung Valutadatum *
       FAELLIG SECTION.
       A.  COMPUTE WA-TAGE = FUNCTION INTEGER-OF-DATE(OP-VALDAT).
           ADD WH-TAG TO WA-TAGE GIVING WB-TAGE.
           COMPUTE WB-TAGE = FUNCTION DATE-OF-INTEGER(WB-TAGE).
           MOVE WB-TAGE TO WH-VALUTA WC-DATUM WZ-DATUM.
       Z.  EXIT.
      ********************************************* lîschen einer Fibu *
       LOEFIB SECTION.
       A.  CALL "CAUP" USING "0710100553000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Fibulîschung " with highlight AT VDU-LP.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "<Entf>= Start, <esc>= Abbruch <    >" AT VDU-LP.
       C.  CALL "CAUP" USING "1003344004" WH-CREG.
           IF ESC GO X.
           IF not ENTF GO C.
           IF WH-NUM not = 4711 GO C.
           ADD 403 VDU-ECK GIVING VDU-LP.
           PERFORM SACHLOE.
           PERFORM DEBLOE.
           PERFORM KREDLOE.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ***************************************** lîschen Sachbuchsalden *
       SACHLOE SECTION.
       A.  MOVE 0 TO SA-KTONR.
           START SACHBUCH KEY not < SA-KEY INVALID GO Z.
       G.  READ SACHBUCH NEXT AT END GO Z.
           IF ZUGRIF PERFORM BESETZT GO G.
           MOVE SA-KTONR TO BZ-KTONR.
           DISPLAY SA-KTONR AT VDU-LP " " SA-BEZ WITH SIZE 40.
           MOVE 0 TO BZ-BUDAT BZ-LFD WM-SOLL WM-HABEN.
           START BUCHZEIL KEY not < BZ-KEY INVALID GO G.
       H.  READ BUCHZEIL NEXT AT END GO G.
           IF ZUGRIF PERFORM BESETZT GO H.
           IF SA-KTONR not = BZ-KTONR GO Q.
           DELETE BUCHZEIL.
           GO H.
       Q.  MOVE 0 TO SA-SOLL SA-HABEN SA-EBIL.
           REWRITE SA-SATZ.
           GO G.
       Z.  EXIT.
      ********************************************* lîschen Kreditoren *
       KREDLOE SECTION.
       A.  MOVE 0 TO KR-KTONR.
           START KREDIT KEY not < KR-KEY INVALID GO Z.
       G.  READ KREDIT NEXT AT END GO Z.
           IF ZUGRIF PERFORM BESETZT GO G.
           MOVE KR-KTONR TO BZ-KTONR.
           DISPLAY KR-KTONR AT VDU-LP " " KR-BEZ WITH SIZE 40.
           MOVE 0 TO BZ-BUDAT BZ-LFD WM-SOLL WM-HABEN.
           START BUCHZEIL KEY not < BZ-KEY INVALID GO G.
       H.  READ BUCHZEIL NEXT AT END GO G.
           IF ZUGRIF PERFORM BESETZT GO H.
           IF KR-KTONR not = BZ-KTONR GO Q.
           DELETE BUCHZEIL.
           GO H.
       Q.  MOVE 0 TO KR-SOLL KR-HABEN KR-UMSATZ.
           REWRITE KR-SATZ.
           GO G.
       Z.  EXIT.
      **************************************** lîschen Debitorensalden *
       DEBLOE SECTION.
       A.  MOVE 0 TO DE-KTONR.
           START DEBITOR KEY not < DE-KEY INVALID GO Z.
       G.  READ DEBITOR NEXT AT END GO Z.
           IF ZUGRIF PERFORM BESETZT GO G.
           MOVE DE-KTONR TO BZ-KTONR.
           DISPLAY DE-KTONR AT VDU-LP " " DE-BEZ WITH SIZE 40.
           MOVE 0 TO BZ-BUDAT BZ-LFD WM-SOLL WM-HABEN.
           START BUCHZEIL KEY not < BZ-KEY INVALID GO G.
       H.  READ BUCHZEIL NEXT AT END GO G.
           IF ZUGRIF PERFORM BESETZT GO H.
           IF DE-KTONR not = BZ-KTONR GO Q.
           DELETE BUCHZEIL.
           GO H.
       Q.  MOVE 0 TO DE-SOLL DE-HABEN DE-UMSATZ.
           REWRITE DE-SATZ.
           GO G.
       Z.  EXIT.
      ********************************************* ob offene Journale *
       OB-OFFJOUR SECTION.
       A.  MOVE 1 TO WX.
           MOVE SPACE TO WH-MCODE.
       C.  ADD WX 1 GIVING WH-KEY.
       E.  READ KONSTANT NO LOCK INVALID GO G.
           IF ZUGRIF PERFORM BESETZT GO E.
           PERFORM VARYING WY FROM 1 BY 1 UNTIL WY > 36
               IF KO-RS(WY) not = 0 MOVE 40 TO WY.
           IF WY < 39 GO G.
           MOVE WX TO WD-KZ.
           MOVE WD-KZ TO WH-MCODE(WX:1).
       G.  ADD 1 TO WX.
           IF WX < 8 GO C.
           IF WH-MCODE = SPACE GO Z.
           CALL "CAUP" USING "0712040470000" WH-CREG.
           ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "Offene Journale User: " AT VDU-LP.
           ADD 222 VDU-ECK GIVING VDU-LP.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 7
               IF WH-MCODE(WX:1) not = SPACE MOVE WX TO WD-KZ
                   IF VDU-P > 27 DISPLAY "," AT 0000
                   end-if
                   ADD 3 TO VDU-LP
                   DISPLAY WD-KZ with highlight AT VDU-LP.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "Auswertung bringt List-Differenz! <esc>= Abbruch, <r
      -        "et>= weiter < >" AT VDU-LP.
           CALL "CAUP" USING "1003680000" WH-CREG.
           IF not RET AND not ESC GO A.
           CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      *************************************** Lîschung erledigter OP's *
       OPLOESCH SECTION.
       A.  CALL "CAUP" USING "270712074000014" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Lîschung offener Posten " with highlight AT VDU-LP.
           ADD 303 VDU-ECK GIVING VDU-LP.
       B.  DISPLAY "bis Datum: " AT VDU-LP.
           CALL "CAUP" USING "1103146006" WH-CREG.
           IF ESC GO X.
           IF not RET GO C.
           IF WX-DATUM = 0 OR WX-DATUM > WM-DATUM GO B.
           MOVE WX-DATUM TO WV-DATUM.
           DISPLAY VDU-DATUM with highlight foreground-color 4
               AT VDU-LP.
       C.  ADD 403 VDU-ECK GIVING VDU-LP.
           DISPLAY "2= Debitoren, 3= Kreditoren  < >" AT VDU-LP.
           CALL "CAUP" USING "1004331001" WH-CREG.
           IF ESC GO X.
           IF not RET GO C.
           MOVE WH-NUM TO WE.
           IF WE not = 2 AND not = 3 GO C.
           MOVE WE TO WD-KZ.
           DISPLAY WD-KZ with highlight BLINK AT VDU-LP.
       D.  ADD 503 VDU-ECK GIVING VDU-LP.
           DISPLAY "<ret>= Start, <esc>= Abbruch < >" AT VDU-LP.
           CALL "CAUP" USING "1005330000" WH-CREG.
           IF ESC GO X.
           IF WOLI GO C.
           IF not RET AND not KIST GO D.
           ADD 503 VDU-ECK GIVING VDU-LP.
           MOVE 0 TO WM-SOLL WM-HABEN WZ-ZEILEN WG-SOLL(1) WG-HABEN(1).
           MOVE 99 TO WG.
           MOVE 0 TO OP-KTONR OP-REF OP-LFD OP-VALDAT.
           MOVE WE TO OP-DK.
           START OPDATEI KEY not < OP-KEY INVALID GO X.
       J.  READ OPDATEI NEXT AT END GO X.
           IF ZUGRIF PERFORM BESETZT GO J.
           IF WE not = OP-DK GO X.
           SUBTRACT OP-ZABETRAG FROM OP-REBETRAG GIVING WH-WERT.
           IF WH-WERT not = 0 GO J.
           DIVIDE 10 INTO OP-KTONR GIVING WD-KTO.
           ADD 603 VDU-ECK GIVING VDU-LP.
           MOVE OP-VALDAT TO WC-DATUM.
           PERFORM DATDREH.
           DISPLAY WD-KTO with highlight AT VDU-LP " " VDU-DATUM.
           IF OP-VALDAT > WV-DATUM or OP-REDAT > WV-DATUM GO J.
           DELETE OPDATEI INVALID GO J.
           GO J.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ******************************************************************
       RASTRI SECTION.
       A.  IF WM-DRU = 0 GO Z.
           IF WR = 0 GO C.
           MOVE 0 TO WR.
           MOVE "&f0S*p-01X*p+16Y*c3050a48b15g2P&f1S" TO DRS-STR.
           MOVE "&f0S*p-01X*p+16Y*c3050a46b15g2P&f1S" TO DRS-STR.
           COMPUTE WZ-GROSS = WZ-BREIT / 12 * 300.
           MOVE WZ-GROSS TO DRS-STR(23:4).
           GO Q.
      *---------------------------------> Schattierung Periodenspalte <-
       C.  MOVE 1 TO WR.
           MOVE "&f0S*p+1513X*p+15Y*c0775a53b15g2P&f1S" TO DRS-STR.
           MOVE "&f0S*p+1513X*p+12Y*c0775a56b15g2P&f1S" TO DRS-STR.
           COMPUTE WZ-GROSS = 62,0 / 12 * 300.              *> immer -1
           MOVE WZ-GROSS TO DRS-STR(10:4).
           COMPUTE WZ-GROSS = 29,5 / 12 * 300.
           MOVE WZ-GROSS TO DRS-STR(25:4).
       Q.  MOVE 0 TO WZ-SCHALT.
           PERFORM DRUCK.
       Z.  EXIT.
