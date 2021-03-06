      $SET LINKCOUNT"240" ANS85 BOUND AUTOLOCK NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     FIBSACH.
      ******************************************************************
      *              Anlage und �nderung Sachkonten                    *
      ******************************************************************
       ENVIRONMENT    DIVISION.
       CONFIGURATION   SECTION.
       SOURCE-COMPUTER.     pc.
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
      ******************************************************************
       WORKING-STORAGE SECTION.
       01  WH-CALL.
           03  WL-CA                   PIC 99.
           03  WL-REST                 PIC 9(13).
       COPY WHCREG.CPY.
       01  WF-REG.
           03  WN-EXCEL                PIC X(15).
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
           03  WH-MOD                  PIC 9         COMP   VALUE ZERO.
           03  WD-KTO                  PIC ZZ.ZZ9,9.
           03  WD-SEITE                PIC ZZ9.
           03  WH-SAKEY                PIC X(6).
           03  WD-KZ                   PIC 9.
           03  WD-ZL                   PIC Z9.
           03  WD-KER                  PIC ZZZ.
           03  WM-MERKER               PIC 9       COMP   VALUE ZERO.
                      88 ANLAGE   VALUE 1 3.   88  AEND   VALUE 0 2 3.
       COPY FIBUEXT.CPY.
       COPY FIBUDECL.CPY.
       END DECLARATIVES.
      ******************************************************************
       STEUER SECTION.
       A.  MOVE WL-CALL TO WH-CALL.
           MOVE WL-CREG TO WH-CREG.
           EVALUATE WL-CA
               WHEN 20
               WHEN 21
               WHEN 22 PERFORM SACH
               WHEN 66 PERFORM SACH-XLS
                       MOVE 9 TO WH-PG.
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
       A.  DISPLAY " weiter mit <ret>: " AT 0000.
           MOVE SPACE TO WH-X.
           ACCEPT WH-X AT 0000.
           CALL "CAUP" using "1324012480000" WH-CREG.
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
       SACH SECTION.
       A.  CALL "CAUP" USING "0711020675000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Verwaltung Sachkonten " with highlight AT VDU-LP.
       C.  ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "Nr.:" AT VDU-LP.
           IF WL-CA > 20 GO P.
           DISPLAY "<ret>= Anl./�nd., alpha+<ret>= suchen, <esc>= Ende,
      -        "< >= Stammdatendruck" AT 2301.
           DISPLAY "<Strg-Einfg>= Excel" AT 2401.
           CALL "CAUP" USING "1002085006" WH-CREG.
           ADD 208 VDU-ECK GIVING VDU-LP.
           DISPLAY "        " AT VDU-LP.
           IF SEIN PERFORM SACH-XLS
                   GO C.
           IF ESC MOVE "FIBSTAM 60STAM" TO WT-TX GO X.
           IF AB PERFORM STAMDRUCK GO C.
           IF NOT RET GO C.
       M.  MOVE 0 TO WM-MERKER.
           IF WH-NUM = 0 CALL "FIBANZ" USING "20SACHSUCH" WH-CREG
               CANCEL "FIBANZ".
           IF WH-NUM = 0 GO C.
           MOVE WH-NUM TO SA-KTONR WD-KZ.
           IF WD-KZ > 1 GO C.
       P.  READ SACHBUCH INVALID MOVE 1 TO WM-MERKER.
           IF ZUGRIF PERFORM BESETZT PERFORM WEITER GO C.
           PERFORM SACH-ANL.
           UNLOCK SACHBUCH.
           IF WL-CA > 20; IF ESC MOVE 0 TO SA-KTONR GO X ELSE GO X.
           CALL "CAUP" USING "16LOEFEN" WH-CREG.
           GO C.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ******************************************************************
       BEDMES SECTION.
       A.  DISPLAY "<esc>= Abbruch, <ret-leer>= keine �nderung, <ende>=
      -        "Ende" AT 2301.
       Z.  EXIT.
      ******************************************************************
       SACH-ANL SECTION.
       A.  COMPUTE WH-WERT = SA-KTONR / 10.
           MOVE WH-WERT TO WD-KTO.
           ADD 214 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-KTO with highlight AT VDU-LP.
           ADD 202 VDU-ECK GIVING VDU-LP.
           DISPLAY "Konto-Nr.: " AT VDU-LP.
           ADD 302 VDU-ECK GIVING VDU-LP.
           DISPLAY "Bezeichnung:" AT VDU-LP.
           ADD 225 VDU-ECK GIVING VDU-LP.
           DISPLAY "Kurzbez.:" AT VDU-LP.
           ADD 245 VDU-ECK GIVING VDU-LP.
           DISPLAY "Kontogruppe......:" AT VDU-LP.
           ADD 345 VDU-ECK GIVING VDU-LP.
           DISPLAY "Brutto/Netto.....:" AT VDU-LP.
           ADD 445 VDU-ECK GIVING VDU-LP.
           DISPLAY "KER-Zuordnung-Kz.:" AT VDU-LP.
           ADD 545 VDU-ECK GIVING VDU-LP.
           DISPLAY "Vorsteuer-Grp....:" AT VDU-LP.
           IF ANLAGE ADD 315 VDU-ECK GIVING VDU-LP
                DISPLAY "- Konto fehlt -" with highlight AT VDU-LP
                MOVE SA-KEY TO WH-SAKEY
                INITIALIZE SA-SATZ
                MOVE WH-SAKEY TO SA-KEY
                GO E.
           ADD 315 VDU-ECK GIVING VDU-LP.
           DISPLAY SA-TX(1) with highlight AT VDU-LP.
           ADD 415 VDU-ECK GIVING VDU-LP.
           DISPLAY SA-TX(2) with highlight AT VDU-LP.
           ADD 235 VDU-ECK GIVING VDU-LP.
           MOVE SA-MCODE TO WT-TX.
           CALL "CAUP" USING "21CONV08" WH-CREG.
           DISPLAY WT-TX WITH SIZE 8 HIGHLIGHT AT VDU-LP.
           IF ESC GO Z.
           ADD 264 VDU-ECK GIVING VDU-LP.
           MOVE SA-KTOART TO WD-KZ.
           DISPLAY WD-KZ with highlight AT VDU-LP.
           ADD 364 VDU-ECK GIVING VDU-LP.
           MOVE SA-BTNT TO WD-KZ.
           DISPLAY WD-KZ with highlight AT VDU-LP " "
           IF SA-BTNT = 0 DISPLAY "brutto" AT 0000
                     ELSE DISPLAY "netto " AT 0000.
           ADD 464 VDU-ECK GIVING VDU-LP.
           MOVE SA-KER-NR TO WD-KER.
           DISPLAY WD-KER with highlight AT VDU-LP.
           IF SA-KER-NR = 0 DISPLAY "ohne " AT VDU-LP.
           ADD 564 VDU-ECK GIVING VDU-LP.
           MOVE SA-VSTKZ TO WD-KER.
           DISPLAY WD-KER with highlight AT VDU-LP "/".
           EVALUATE SA-VSTKZ
               WHEN 0 DISPLAY "norm" AT 0000
               WHEN 28 DISPLAY "Geb." AT 0000
               WHEN 27 DISPLAY "Kfz " AT 0000.
       E.  MOVE SA-TX(1) TO WR-ADR(1).
           MOVE SA-TX(2) TO WR-ADR(2).
       F.  PERFORM BEDMES.
           CALL "CAUP" USING "1203150225" WH-CREG.
           IF HELP CALL "CAUP" USING "190003000401" WH-CREG GO F.
           MOVE WT-TX TO SA-BEZ.
           IF WOLI GO E.
           IF ENDE GO W.
           IF ESC GO Z.
           IF WR-ADR(1) = SPACE
               DISPLAY "Bezeichnung muss sein!" AT 2401
               PERFORM WEITER GO F.
           MOVE WR-ADR(1) TO SA-TX(1).
           MOVE WR-ADR(2) TO SA-TX(2).
       G.  PERFORM BEDMES.
           ADD 235 VDU-ECK GIVING VDU-LP.
           MOVE SA-MCODE TO WT-TX.
           CALL "CAUP" USING "21CONV08" WH-CREG.
           DISPLAY WT-TX WITH SIZE 8 HIGHLIGHT AT VDU-LP.
           CALL "CAUP" USING "1202350108" WH-CREG.
           IF HELP CALL "CAUP" USING "190005000601" WH-CREG GO G.
           IF ENDE GO W.
           IF AUF OR WOLI GO E.
           IF ESC GO Z.
           IF NOT RET GO G.
           CALL "CAUP" USING "20CONV08" WH-CREG.
           MOVE WT-TX TO SA-MCODE.
           CALL "CAUP" USING "21CONV08" WH-CREG.
           DISPLAY WT-TX(1:9) with highlight AT VDU-LP.
       H.  MOVE SA-KTOART TO WH-WERT.
           PERFORM BEDMES.
           DISPLAY "0= Bestandskonten, 1= Erfolgskonten" AT 2401.
           CALL "CAUP" USING "1002641001" WH-CREG.
           IF ENDE GO W.
           IF WOLI GO G.
           IF ESC GO Z.
           IF NOT RET GO H.
           IF WH-WERT > 2 GO H.
           MOVE WH-WERT TO SA-KTOART WD-KZ.
           DISPLAY WD-KZ with highlight AT VDU-LP.
       I.  MOVE SA-BTNT TO WH-WERT.
           PERFORM BEDMES.
           DISPLAY "Ust: 0= brutto, 1= netto" AT 2401.
           CALL "CAUP" USING "1003641001" WH-CREG.
           IF ENDE GO W.
           IF ESC GO Z.
           IF WOLI GO H.
           IF NOT RET GO I.
           IF WH-WERT > 1 GO I.
           MOVE WH-WERT TO SA-BTNT WD-KZ.
           DISPLAY WD-KZ with highlight AT VDU-LP " "
           IF SA-BTNT = 0 DISPLAY "brutto" AT 0000
                     ELSE DISPLAY "netto " AT 0000.
       J.  MOVE SA-KER-NR TO WH-WERT.
           PERFORM BEDMES.
           DISPLAY "KER-Gruppe: 0= ohne, 1-199 = KER-Zuordnung" AT 2401.
           CALL "CAUP" USING "1004643003" WH-CREG.
           IF ENDE GO W.
           IF ESC GO Z.
           IF WOLI GO I.
           IF not RET GO J.
           IF SA-KER-NR > 199 GO J.
           MOVE WH-WERT TO SA-KER-NR WD-KER.
           DISPLAY WD-KER with highlight AT VDU-LP
           IF SA-KER-NR = 0 DISPLAY "ohne " AT VDU-LP.
       L.  MOVE SA-VSTKZ TO WH-WERT.
           PERFORM BEDMES.
           DISPLAY "Umsatzsteuer-Kz.: 0 = normal, 27 = Kfz, 28= Geb�ude"
                 AT 2401.
           CALL "CAUP" USING "1005642002" WH-CREG.
           IF ENDE GO W.
           IF ESC GO Z.
           IF WOLI GO J.
           IF not RET GO L.
           EVALUATE WH-WERT
                WHEN 0
                WHEN 27
                WHEN 28 NEXT SENTENCE
                WHEN OTHER GO L.
           MOVE WH-WERT TO SA-VSTKZ WD-KER.
           DISPLAY WD-KER with highlight AT VDU-LP "/".
           EVALUATE SA-VSTKZ
               WHEN 0 DISPLAY "norm" AT 0000
               WHEN 28 DISPLAY "Geb." AT 0000
               WHEN 27 DISPLAY "Kfz " AT 0000.
       W.  DISPLAY "<ret>= Eingaben ok, <esc>= Abbruch, <tab>= l�schen"
               AT 2301.
           CALL "CAUP" USING "0023521001" WH-CREG.
           IF HELP CALL "CAUP" USING "190007000801" WH-CREG GO W.
           IF WOLI GO J.
           IF ESC  GO Z.
           IF TABL GO Y.
           IF NOT RET GO W.
           IF ANLAGE WRITE SA-SATZ GO Z.
           IF AEND REWRITE SA-SATZ GO Z.
       Y.  IF SA-SOLL NOT = 0 OR SA-HABEN NOT = 0 GO W.
           DISPLAY "Wirklich l�schen? <ret>= Abbruch, <esc>= l�schen"
               AT 2301.
           CALL "CAUP" USING "0023511001" WH-CREG.
           IF RET GO W.
           IF NOT ESC GO Y.
           DELETE SACHBUCH.
           DISPLAY "gel�scht!" AT 2401.
           PERFORM WEITER.
       Z.  EXIT.
      *********************************************** Druck Stammdaten *
       STAMDRUCK SECTION.
       A.  CALL "CAUP" USING "0710100466000" WH-CREG.
           PERFORM LADE-DRU.
           ADD 103 VDU-ECK GIVING VDU-LP.
           MOVE "SACHSTAM.LST" TO WH-DRUNAM.
           DISPLAY " Stammdatendruck " AT VDU-LP.
       C.  ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "<#>= nach Ker-Gruppen" AT VDU-LP.
           ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "<esc>= Abbruch, <ret>= Nummerndruck, <tab>= Matchcod
      -        "edruck < >" AT VDU-LP.
           CALL "CAUP" USING "1102630000" WH-CREG.
           IF ESC GO X.
           IF NOT RETA and not KIST GO C.
           MOVE LOW-VALUES TO SA-SATZ.
           EVALUATE TRUE
               WHEN RET MOVE 0 TO SA-KTONR
                    MOVE 1 TO WH-MOD
                    START SACHBUCH KEY NOT < SA-KEY INVALID GO C
               WHEN KIST MOVE 0 TO SA-KER-NR
                    MOVE 2 TO WH-MOD
                    START SACHBUCH KEY NOT < SA-KER INVALID GO C
               WHEN TABL MOVE LOW-VALUES TO SA-MCODE
                    MOVE 3 TO WH-MOD
                    START SACHBUCH KEY NOT < SA-MCODE INVALID GO C.
       E.  READ SACHBUCH NEXT NO LOCK AT END GO W.
           IF ZUGRIF PERFORM BESETZT GO E.
           ADD 302 VDU-ECK GIVING VDU-LP.
           MOVE SA-KTONR TO WD-KZ.
           PERFORM SACHKOPF.
           DIVIDE 10 INTO SA-KTONR GIVING WD-KTO.
           MOVE WD-KTO TO DRA-STR(1:10).
           MOVE SA-BEZ TO DRA-STR(10:50).
           DISPLAY WD-KTO with highlight AT VDU-LP DRA-STR(09:50).
           MOVE SA-MCODE TO WT-TX.
           CALL "CAUP" USING "21CONV08" WH-CREG.
           MOVE WT-TX TO DRA-STR(61:8).
           EVALUATE SA-KTOART
               WHEN 0 MOVE "B" TO DRA-STR(70:1)
               WHEN 1 MOVE "E" TO DRA-STR(70:1).
           EVALUATE SA-BTNT
               WHEN 0 MOVE "B" TO DRA-STR(74:1)
               WHEN 1 MOVE "N" TO DRA-STR(74:1).
           IF SA-KER-NR not = 0 MOVE SA-KER-NR TO WD-KER
               MOVE WD-KER TO DRA-STR(77:).
           PERFORM DRUCK.
           GO E.
       W.  PERFORM END-DRU.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ******************************************************************
       SACHKOPF SECTION.
       A.  IF WZ-ZEILEN > 62 WRITE DRA-SATZ AFTER PAGE
               MOVE 0 TO WZ-ZEILEN.
           IF WZ-ZEILEN > 0 GO Z.
           MOVE 2 TO WH-P1.
           MOVE 4 TO WH-P2.
           IF WM-DRU = 1 MOVE 15 TO WH-P1.
           PERFORM BEG-DRU.
           IF WM-DRU = 0 MOVE 1 TO WZ-SCHALT.
           MOVE WK-FIRMA TO DRA-STR(1:30).
           MOVE "Sachkontenliste per:" TO DRA-STR(39:).
           MOVE WH-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRA-STR(60:10).
           MOVE "Seite:" TO DRA-STR(70:).
           ADD 1 TO WZ-SEITE.
           MOVE WZ-SEITE TO WD-SEITE.
           MOVE WD-SEITE TO DRA-STR(77:).
           PERFORM DRUCK.
           MOVE ALL "���" TO DRA-STR(1:80).
           PERFORM DRUCK.
           MOVE "Konto-Nr Bezeichnung
      -       "     M-code  B/E b/n KER" TO DRA-STR(1:).
           EVALUATE WH-MOD
               WHEN 1 MOVE "(numerisch)" TO DRA-STR(33:11)
               WHEN 2 MOVE "(KER-Grp. )" TO DRA-STR(33:11)
               WHEN 3 MOVE "(alphabet.)" TO DRA-STR(33:11).
           PERFORM DRUCK.
           MOVE ALL "���" TO DRA-STR(1:80).
           PERFORM DRUCK.
       Z.  EXIT.
      ***************************************** XLS-Ausgabe Sachkonten *
       SACH-XLS SECTION.
       A.  ADD 509 VDU-ECK GIVING VDU-LP.
           DISPLAY ALL " " with size 50 at vdu-lp.
           INITIALIZE SA-SATZ.
           START SACHBUCH KEY not < SA-KEY INVALID GO Z.
       C.  READ SACHBUCH NEXT IGNORE LOCK AT END
                CLOSE SACHBUCH
                GO X.
           IF SA-KTONR = SPACE MOVE 0 TO SA-KTONR.
           DIVIDE 10 INTO SA-KTONR GIVING WD-KTO WH-NUM.
           ADD 509 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-KTO with highlight AT VDU-LP.
           IF WN-EXCEL = SPACE MOVE "EXC-TO.TXT" TO WN-EXCEL WT-TX
               MOVE "SACHKTO.TXT" TO WT-TX
               MOVE "Kto.Nr;Bezeichnung;Mcode;B/E;SH;BtNt;Steuer"
                  TO WT-ADR
               INSPECT WT-ADR REPLACING ALL ";" BY "|"
               CALL "CA-TAGE" USING "90OPEN" WH-CREG.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 10
               MOVE "N" TO WH-X
               MOVE 0 TO WH-NEG
               EVALUATE WX
                   WHEN  1 MOVE WH-NUM TO WH-WERT
                   WHEN  2 MOVE SA-BEZ TO WT-TX
                           MOVE "U" TO WH-X
                   WHEN  3 MOVE SA-MCODE TO WT-TX
                           CALL "CAUP" USING "21CONV08" WH-CREG
                           MOVE "U" TO WH-X
                   WHEN  4 MOVE SA-KTOART TO WD-KZ
                           MOVE WD-KZ TO WT-TX
                           MOVE "A" TO WH-X
                   WHEN  5 MOVE SA-SH TO WD-KZ
                           MOVE WD-KZ TO WT-TX
                           MOVE "A" TO WH-X
                   WHEN  6 MOVE SA-BTNT TO WD-KZ
                           MOVE WD-KZ TO WT-TX
                           MOVE "A" TO WH-X
                   WHEN  7 MOVE SA-VSTKZ TO WD-KZ
                           MOVE WD-KZ TO WT-TX
                           MOVE "A" TO WH-X
                   WHEN  8 MOVE "W" TO WH-X
               end-evaluate
               PERFORM EXC-AUSGABE.
           GO C.
       X.  CALL "CA-TAGE" USING "95CLOSE" WH-CREG.
       Z.  EXIT.
      **************************************** Softline-Export-Ausgabe *
       EXC-AUSGABE SECTION.
       A.  EVALUATE WH-X
               WHEN "N" CALL "CA-TAGE" USING "91NUMER" WH-CREG
               WHEN "A" CALL "CA-TAGE" USING "92ALPHA" WH-CREG
               WHEN "E" CALL "CA-TAGE" USING "94EAN14" WH-CREG
               WHEN "U" CALL "CA-TAGE" USING "96UMLAU" WH-CREG
               WHEN "D" CALL "CA-TAGE" USING "98DATUM" WH-CREG
               WHEN "W" CALL "CA-TAGE" USING "93WRITE" WH-CREG
                            MOVE 50 TO WX.
       Z.  EXIT.
