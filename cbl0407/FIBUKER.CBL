      $SET LINKCOUNT"320" ANS85 BOUND AUTOLOCK NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     FIBUKER.
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SX-BUCH  ASSIGN WN-SACH
                           ORGANIZATION INDEXED, ACCESS DYNAMIC
                           RECORD KEY SX-KEY
                           ALTERNATE KEY SX-MCODE DUPLICATES
                           ALTERNATE KEY SX-KER = SX-KER-NR SX-KTONR
                               DUPLICATES
                           FILE STATUS WF-STATUS.
           SELECT OLDSACH  ASSIGN "FIBUSACH.DAT"
                           ORGANIZATION INDEXED, ACCESS DYNAMIC
                           RECORD KEY OS-KEY
                           ALTERNATE KEY OS-MCODE DUPLICATES
                           FILE STATUS WF-STATUS.
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
       FD  SX-BUCH     external       LABEL RECORD STANDARD.
       01  SX-SATZ.
           03  SX-KEY.
               05 SX-KTONR             PIC 9(7)      COMP.
           03  SX-BEZ.
               05 SX-TX                PIC X(25)     OCCURS 2.
           03  SX-MCODE                PIC X(8).
           03  SX-KTOART               PIC 99        COMP.
           03  SX-SH                   PIC 99        COMP.
           03  SX-SOLL                 PIC S9(9)V99  COMP-3.
           03  SX-HABEN                PIC S9(9)V99  COMP-3.
           03  SX-EBIL                 PIC S9(9)V99  COMP-3.
           03  SX-KER-RES              PIC 99        COMP.
           03  SX-KER-NR               PIC 999       COMP.
           03  SX-BTNT                 PIC 99        COMP.
           03  SX-MGKZ                 PIC 99        COMP.
           03  SX-MENGENEB             PIC S9(9)V99  COMP-3.
           03  SX-MENGE-JAHR           PIC S9(9)V99  COMP-3.
      ******************************************** Sachkonten ohne KER *
       FD  OLDSACH      external       LABEL RECORD STANDARD.
       01  OS-SATZ.
           03  OS-KEY.
               05 OS-KTONR             PIC 9(7)      COMP.
           03  OS-BEZ.
               05 OS-TX                PIC X(25)     OCCURS 2.
           03  OS-MCODE                PIC X(8).
           03  OS-KTOART               PIC 99        COMP.
           03  OS-SH                   PIC 99        COMP.
           03  OS-SOLL                 PIC S9(9)V99  COMP-3.
           03  OS-HABEN                PIC S9(9)V99  COMP-3.
           03  OS-EBIL                 PIC S9(9)V99  COMP-3.
           03  OS-KER-RES              PIC 99        COMP.
           03  OS-KER-NR               PIC 999       COMP.
           03  OS-BTNT                 PIC 99        COMP.
           03  OS-MGKZ                 PIC 99        COMP.
           03  OS-MENGENEB             PIC S9(9)V99  COMP-3.
           03  OS-MENGE-JAHR           PIC S9(9)V99  COMP-3.
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
      ******************************************************************
       FD  DRUCKER                     LABEL RECORD STANDARD.
       01  DRA-SATZ.
           03  FILLER                  PIC X(2).
           03  DRA-STR.
               05 DRA-TEXT             PIC X(26).
               05 DRA-BKOL                         OCCURS 4.
                  07 DRA-BET           PIC ZZZ.ZZZ.ZZ9-.
                  07 DRA-PZ            PIC ZZ9,9-.
           03  FILLER                  PIC X(69).
       01  DRK-SATZ.
           03  FILLER                  PIC X(2).
           03  DRK-STR.
               05 DRK-ZEIL             PIC ZZ9-.
               05 DRK-ART              PIC ZZ9-.
               05 FILLER               PIC XX.
               05 DRK-TEXT             PIC X(27).
               05 DRK-RECH             PIC X.
               05 FILLER               PIC XX.
               05 DRK-PZ               PIC X.
               05 FILLER               PIC XX.
               05 DRK-HILFE            PIC X(23).
       01  DRS-SATZ.
           03  FILLER                  PIC X(2).
           03  DRS-STR.
               05 FILLER               PIC X(80).
      ******************************************************************
       WORKING-STORAGE SECTION.
       01  WH-CALL.
           03  WL-CA                   PIC 99.
           03  WL-REST                 PIC 9(13).
       COPY WHCREG.CPY.
       01  WH-REGI.
           03  WH-FIBNAM               PIC X(12).
           03  WM-FIBEX                PIC 9         COMP   VALUE ZERO.
           03  WH-FIKEY                PIC X(5).
           03  WD-SEITE                PIC Z9-.
           03  WD-ZEIL                 PIC ZZ9-.
           03  WD-KER                  PIC ZZ9-.
           03  WD-ZL                   PIC Z9-.
           03  WD-KZ                   PIC 9.
           03  WM-ZEIL                 PIC 999        COMP.
           03  WV-PER                  PIC 9999       COMP-3.
           03  WK-PER                  PIC 9999       COMP-3.
           03  WH-MOD                  PIC 99         COMP.
           03  WD-PERIOD               PIC ZZZ9/99-.
           03  WM-START                PIC 9(8)       COMP.
           03  WM-ENDE                 PIC 9(8)       COMP.
           03  WX-PER                  PIC X(15).
           03  WA-TAGE                 PIC 9(8)      COMP.
      *--------------------------------------------> Bildschirmfelder <-
           03  WI-ATTR                 PIC X(2000).
           03  WI-ZEICH                PIC X(2000).
           03  WI-ERFTAB               PIC X(204).
           03  WX-ATTRIB               PIC X(80).
           03  WM-ATTRIB               PIC X(80).
           03  WX-LNG                  PIC 9(4)      COMP-X.
           03  WX-PAR.
               05 WX-L                 PIC 99        COMP-X.
               05 WX-P                 PIC 99        COMP-X.
           03  WX-PS REDEFINES WX-PAR  PIC 9999      COMP.
           03  WX-LG                   PIC 9999      COMP.
       01  WF-REG.
           03  WM-ANZ                  PIC 9(5).
           03  WD-ANZ                  PIC ZZ.ZZ9.
           03  WH-SIZE                 PIC 99        COMP   VALUE 62.
           03  WH-P1                   PIC 99        COMP.
           03  WH-P2                   PIC 99        COMP.
           03  WK-DSTG.
               05 WK-STG               PIC X(10)     OCCURS 10.
           03  WX-PRNO                 PIC 99        COMP-X.
           03  WX-PRSTAT               PIC 99        COMP-X.
           03  WZ-SEITE                PIC 99        COMP   VALUE ZERO.
           03  WZ-SCHALT               PIC 99        COMP   VALUE ZERO.
           03  WZ-ZEILEN               PIC 99        COMP   VALUE ZERO.
           03  WH-DRUNAM               PIC X(12)     VALUE   "LPT1:".
           03  WM-OPEN                 PIC 9         COMP   VALUE ZERO.
           03  WT-ERFTAB.
               05  WT-KEY              PIC X(12)         OCCURS 17.
           03  WE                      PIC 999      COMP.
           03  WI                      PIC 99       COMP.
           03  WQ                      PIC S99      COMP.
           03  WL                      PIC 999      COMP.
           03  WS                      PIC 99       COMP.
           03  IX                      PIC 99       COMP.
           03  IY                      PIC 99       COMP.
           03  XS                      PIC 99       COMP.
           03  WD-POS                  PIC Z9.
           03  WD-KTO                  PIC ZZ.ZZ9,9.
           03  WV-VDAT                 PIC 9(6).
           03  WV-BDAT                 PIC 9(6).
           03  WD-SALDO                PIC ZZ.ZZZ.ZZ9,99-.
           03  WK-BET                  PIC S9(9)V99  COMP.
           03  WM-SUM                  PIC S9(9)V99  COMP.
           03  WM-VSUM                 PIC S9(9)V99  COMP.
           03  WS-SUM                  PIC S9(9)V99  COMP.
           03  WS-VSUM                 PIC S9(9)V99  COMP.
           03  WG-SUM                  PIC S9(9)V99  COMP.
           03  WG-VSUM                 PIC S9(9)V99  COMP.
           03  WZ-SUM                  PIC S9(9)V99  COMP.
           03  WZ-VSUM                 PIC S9(9)V99  COMP.
           03  WT-SUM                  PIC S9(9)V99  COMP.
           03  WT-VSUM                 PIC S9(9)V99  COMP.
           03  WM-PZL                  PIC 999       COMP.
           03  WT-KERTAB.
               05 WT-KER                             OCCURS 200.
                  07 WKM-OLD           PIC 999       COMP.
                  07 WKM-ZEIL          PIC 999       COMP.
                  07 WKM-TEXT          PIC X(25).
                  07 WKM-ART           PIC 99        COMP.
                  07 WKM-RECH          PIC X.
                  07 WKM-PZ            PIC X.
                  07 WKM-TSUM.
                     09 WKM-SUM        PIC S9(9)V99   COMP OCCURS 25.
           03  HI-SATZ.
               05 HI-ATTR              PIC X(2000).
               05 HI-ZEICH             PIC X(2000).
               05 HI-ERFTAB            PIC X(120).
       COPY FIBUEXT.CPY.
       COPY FIBUDECL.CPY.
       DECL-T SECTION.         USE AFTER ERROR PROCEDURE ON KERMODEL.
       A.  CALL "CADECL" USING "FIBUKER.DAT " WH-CREG.
       Z.  EXIT.
       END DECLARATIVES.
      ******************************************************************
       STEUER SECTION.
       A.  MOVE WL-CALL TO WH-CALL.
           MOVE WL-CREG TO WH-CREG.
           if wl-ca = 98 PERFORM OP-KONVERT GO Z.
           IF WL-CA = 99 PERFORM SA-CONV GO Z.
           OPEN I-O KERMODEL.
           EVALUATE WL-CA
               WHEN 10 PERFORM K-E-R.
           CLOSE KERMODEL.
           MOVE WH-CREG TO WL-CREG.
       Z.  EXIT PROGRAM.
      ******************************************************************
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
           CALL "CAUP" using "1324012480000" WH-CREG.
       Z.  EXIT.
      *****************************************************************
       NO-REC SECTION.
       A.  DISPLAY "keine Daten vorhanden," AT 2401
           PERFORM WEITER.
       Z.  EXIT.
      ******************************************************* Drucker *
       DRUCK SECTION.
      *A.  IF WM-DRU = 1 MOVE DRA-STR TO DRA-SATZ(4:).
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
           READ KONSTANT WITH NO LOCK not INVALID GO X.
           IF ZUGRIF PERFORM BESETZT GO A.
       X.  MOVE KOP-TX TO WK-DSTG.
       Z.  EXIT.
      ******************************************************************
       KER-ANL SECTION.
       A.  CALL "CAUP" USING "270612145001012" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " KER-Gliederung " with highlight foreground-color 6
                AT VDU-LP.
           ADD 202 VDU-ECK GIVING VDU-LP.
           DISPLAY " Gruppe Art Bezeichnung                R   %    "
              with highlight foreground-color 3 reverse-video AT VDU-LP.
       B.  MOVE LOW-VALUE TO WT-ERFTAB.
           MOVE 0 TO WR.
           CALL "CAUP" USING "16CLRFEN" WH-CREG.
           PERFORM LAST-KER.
           IF KM-KEY not = LOW-VALUE MOVE 1 TO IX
               PERFORM KER-ANZ
               PERFORM AUF-ZEIL.
       C.  PERFORM VARYING IX FROM 12 BY -1 UNTIL IX = 0
               OR WT-KEY(IX) not = LOW-VALUE
               END-PERFORM.
       E.  IF not XPOS PERFORM BS-ZEIL.
           MOVE 0 TO WS.
       F.  DISPLAY "<ret>= Pos.-Nr., <esc>= Ende < / >= auf/ab" AT 2301.
           DISPLAY "<Bild  / >= Seite auf/ab, <Entf>= lîschen, <Einfg>=
      -        "Zeile einfÅgen" AT 2401.
           PERFORM ANZEIG.
           MOVE KM-ZEIL TO WM-ZEIL WH-WERT.
           COMPUTE VDU-LP = IX * 100 + 204.
           PERFORM OLDPIC.
           PERFORM ZEIHEL.
           CALL "CAUP" USING "1000003009" WH-CREG.
           PERFORM ZEIDUN.
       G.  EVALUATE TRUE
               WHEN ESC  GO X
               WHEN ENDE GO X
               WHEN SAPO PERFORM KER-REORG
                         GO A
               WHEN APOS MOVE 1 TO IX GO F
               WHEN BAUF PERFORM AUF-ZEIL GO F
               WHEN AUF  IF IX > 1 SUBTRACT 1 FROM IX GO F
                         else PERFORM AUF-ZEIL GO F
               WHEN BAB  IF WH-NUM not = 0 MULTIPLY 1000 BY WH-NUM
                             SET SAPO TO TRUE
                         else PERFORM ABZEIL
                              IF FINE GO C
                              else SUBTRACT 1 FROM IX GO F
               WHEN AB   IF WT-KEY(IX + 1) not = LOW-VALUE GO E
                         else PERFORM ABZEIL
                              IF FINE GO C
                              else GO F
               WHEN ENTF PERFORM LOEBEW
                         PERFORM AUSGAB
                         GO F
               WHEN EINF PERFORM EINFUEG
                         IF ESC PERFORM ALTBILD
                                GO F
               WHEN RET NEXT SENTENCE
               WHEN WORE; IF KM-ART = 0 MOVE KM-ZEIL TO WY
                             CALL "FIBANZ" USING "40KER" WH-CREG
                             CANCEL "FIBANZ"
                          end-if GO F
               WHEN OTHER GO F.
           MOVE 0 TO WL WI WE.
           IF WH-WERT = 0 GO F.
           IF KM-ZEIL not = 0 and WH-WERT not = KM-ZEIL GO F.
           MOVE WH-WERT TO KM-ZEIL WD-ZEIL WM-ZEIL.
           READ KERMODEL not INVALID MOVE 1 TO WS GO H.
           INITIALIZE KM-SATZ.
           MOVE 0 TO WS.
           MOVE WH-WERT TO KM-ZEIL.
       H.  DISPLAY WD-ZEIL with highlight foreground-color 3 AT VDU-LP.
           EVALUATE WS
               WHEN 0 GO I                          *> neue Zeile
               WHEN 1 PERFORM KER-ANZ.              *> vorhandene Zeile
       I.  DISPLAY "<esc>= Abbruch, <ret>= Art, < />= zurÅck, <#>= Hilf
      -        "e" AT 2301.
           MOVE KM-ART TO WH-WERT.
           COMPUTE VDU-LP = IX * 100 + 210.
           CALL "CAUP" USING "1000001001" WH-CREG.
           IF ESC or WOLI or AUF PERFORM ALTBILD GO F.
           IF KIST PERFORM HI-ZEIG GO I.
           IF not RET GO I.
           MOVE WH-WERT TO WD-KER.
           DISPLAY WD-KER with highlight AT VDU-LP.
           MOVE WH-WERT TO KM-ART.
       K.  DISPLAY "<esc>= Abbruch, <ret>= Text, < >= zurÅck" AT 2301.
           MOVE KM-TEXT TO WT-TX.
           COMPUTE VDU-LP = IX * 100 + 214.
           CALL "CAUP" USING "1200000125" WH-CREG.
           IF AUF GO I.
           IF ESC GO F.
           IF not RET GO K.
           MOVE WT-TX TO KM-TEXT.
           DISPLAY KM-TEXT with highlight AT VDU-LP.
           IF KM-ART not = 0 and not = 3 GO O.
       M.  DISPLAY "<esc>= Abbruch, <ret>= Art, < />= zurÅck" AT 2301.
           DISPLAY """+"" fÅr Erlîs, ""-"" fÅr Aufwand" with highlight
               AT 2401.
           MOVE KM-RECH TO WT-TX.
           COMPUTE VDU-LP = IX * 100 + 241.
           CALL "CAUP" USING "1200000101" WH-CREG.
           IF ESC or WOLI or AUF GO K.
           IF not RET GO M.
           EVALUATE WT-TX(1:1)
               WHEN "-"
               WHEN " "
               WHEN "+" NEXT SENTENCE
               WHEN OTHER GO I.
           MOVE WT-TX TO KM-RECH.
           DISPLAY KM-RECH with highlight foreground-color 4 AT VDU-LP.
       N.  DISPLAY "<esc>= Abbruch, <ret>= Art, < />= zurÅck" AT 2301.
           DISPLAY "* = Basis" with highlight AT 2401.
           MOVE KM-PZ TO WT-TX.
           COMPUTE VDU-LP = IX * 100 + 245.
           CALL "CAUP" USING "1200000101" WH-CREG.
           IF ESC or WOLI or AUF GO M.
           IF not RET GO N.
           EVALUATE WT-TX(1:1)
               WHEN "*"
               WHEN " " NEXT SENTENCE
               WHEN OTHER GO N.
           MOVE WT-TX TO KM-PZ.
           DISPLAY KM-PZ with highlight foreground-color 6 AT VDU-LP.
       O.  IF WS = 1 REWRITE KM-SATZ INVALID GO F
                     end-rewrite GO S.
       Q.  WRITE KM-SATZ INVALID ADD 1 TO KM-ZEIL GO Q.
       S.  IF WL-CA = 50 GO X.
           COMPUTE WD-KER = KM-ZEIL * 10.
           MOVE KM-KEY TO WT-KEY(IX).
           MOVE WT-KEY(1) TO KM-KEY.
           PERFORM AUSGAB.
           GO E.
       W.  PERFORM ALTBILD.
           GO F.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ******************************************** Hilfetext enazeigen *
       HI-ZEIG SECTION.
       A.  CALL "CAUP" USING "270440103900015" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Zeilenart " with highlight AT VDU-LP.
           ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "0= Kontensummenzeile" with highlight AT VDU-LP.
           ADD 100 TO VDU-LP.
           DISPLAY "1= Gruppentextierung" with highlight AT VDU-LP.
           ADD 100 TO VDU-LP.
           DISPLAY "2= Unterstreichung" with highlight AT VDU-LP.
           ADD 100 TO VDU-LP.
           DISPLAY "3= Gruppensumme" with highlight AT VDU-LP.
           ADD 100 TO VDU-LP.
           DISPLAY "4= Zwischensumme" with highlight AT VDU-LP.
           ADD 100 TO VDU-LP.
           DISPLAY "5= Ergebnis " with highlight AT VDU-LP.
       C.  DISPLAY "Weiter mit beliebiger Taste oder autom. nach 9 Sekun
      -        "den < >" AT 2301.
           ACCEPT WD-KZ AT 2358 TIMEOUT AFTER +9 with NO-ECHO
              SIZE 1 AUTO-SKIP NOT ON EXCEPTION CONTINUE.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ****************************************** nÑchsten freien lesen *
       READ-NEXT SECTION.
       A.  INITIALIZE KM-SATZ.
           MOVE 999,9 TO KM-ZEIL.
           START KERMODEL KEY < KM-KEY INVALID GO Q.
           READ KERMODEL PREVIOUS IGNORE LOCK AT END GO Q.
       Q.  INITIALIZE KM-SATZ.
           MOVE WM-ZEIL TO KM-ZEIL.
       Z.  EXIT.
      ******************************************************************
       LAST-KER SECTION.
       A.  MOVE 999,9 TO KM-ZEIL
           START KERMODEL KEY < KM-KEY INVALID MOVE 0 TO KM-ZEIL GO Z.
       B.  READ KERMODEL PREVIOUS IGNORE LOCK AT END
               MOVE 0 TO KM-ZEIL.
       Z.  EXIT.
      **************************************** Leistungszeile anzeigen *
       KER-ANZ SECTION.
       A.  COMPUTE VDU-LP = IX * 100 + 204 + VDU-ECK.
           MOVE KM-ZEIL TO WD-ZEIL.
           DISPLAY WD-ZEIL with highlight foreground-color 3 AT VDU-LP.
           ADD 7 TO VDU-LP.
           MOVE KM-ART TO WD-ZL.
           DISPLAY WD-ZL with highlight AT VDU-LP.
           ADD 3 TO VDU-LP.
           DISPLAY KM-TEXT with highlight AT VDU-LP.
           ADD 27 TO VDU-LP.
           DISPLAY KM-RECH with highlight foreground-color 4 AT VDU-LP.
           ADD 4 TO VDU-LP.
           DISPLAY KM-PZ with highlight foreground-color 6 AT VDU-LP.
           ADD 1 TO WR.
           MOVE KM-KEY TO WT-KEY(IX).
       Z.  EXIT.
      ******************************************************************
       AUSGAB SECTION.
       A.  CALL "CAUP" USING "16CLREFN" WH-CREG.
           MOVE IX TO WE.
           MOVE 1 TO IX.
           MOVE WT-KEY(IX) TO KM-KEY.
           MOVE LOW-VALUE TO WT-ERFTAB.
           START KERMODEL KEY not < KM-KEY INVALID SET FINE TO TRUE
               GO X.
           SET BAB TO TRUE.
           MOVE 0 TO WI WR.
       E.  PERFORM NEXTAB WITH TEST AFTER UNTIL WI = 8 OR WR = 11.
           IF WI not = 8 MOVE 9 TO WI.
           MOVE WE TO IX.
       H.  IF IX = 0 GO X.
           IF WT-KEY(IX) = LOW-VALUE ADD -1 TO IX GO H.
       X.  IF FINE PERFORM NODATA.
       Z.  EXIT.
      **************************************************** weiterlesen *
       ABZEIL SECTION.
       A.  PERFORM VARYING IX FROM 12 BY -1 UNTIL IX = 0
               OR WT-KEY(IX) not = LOW-VALUE CONTINUE.
           IF IX = 0 MOVE 1 TO IX SET FINE TO TRUE GO X.
       C.  MOVE WT-KEY(IX) TO KM-KEY.
           IF AB OR (BAB and WH-NUM = 0) PERFORM READBEW
               ADD 1 TO IX
               MOVE 0 TO WI WR
               IF AB MOVE 10 TO WR.
       E.  PERFORM NEXTAB WITH TEST AFTER UNTIL WI = 8 OR WR = 11.
           IF WI not = 8 MOVE 9 TO WI.
       X.  IF FINE PERFORM NODATA.
       Z.  EXIT.
      ******************************************************************
       NEXTAB SECTION.
       E.  READ KERMODEL NEXT IGNORE LOCK AT END MOVE 8 TO WI GO Z.
           IF WI < 3 PERFORM VARYING WF FROM 1 BY 1 UNTIL WF > 11
               IF KM-KEY = WT-KEY(WF) GO E.
           IF IX = 12 CALL "CAUP" USING "17SCROLAUF" WH-CREG
               PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 10
                   MOVE WT-KEY(IX + 1) TO WT-KEY(IX)
                   END-PERFORM.
           PERFORM KER-ANZ.
           IF BAB ADD 1 TO IX.
       Z.  EXIT.
      ******************************************************************
       AUF-ZEIL SECTION.
       A.  IF WT-KEY(1) = LOW-VALUE SET FINE TO TRUE GO X.
           MOVE WT-KEY(1) TO KM-KEY.
           MOVE 0 TO WR.
           MOVE IX TO WX.
           IF AUF MOVE 09 TO WR.
           START KERMODEL KEY < KM-KEY INVALID SET FINE TO TRUE GO X.
           MOVE 1 TO IX WI.
           PERFORM NEXTAUF WITH TEST AFTER UNTIL WI = 2 OR WR = 10.
           MOVE 1 TO IX.
       X.  IF FINE PERFORM NODATA.
       Z.  EXIT.
      ******************************************************************
       NEXTAUF SECTION.
       N.  READ KERMODEL PREVIOUS IGNORE LOCK AT END MOVE 2 TO WI GO Z.
           IF WI > 8 PERFORM VARYING WF FROM 12 BY -1 UNTIL WF < 1
               IF KM-KEY = WT-KEY(WF) GO N.
           CALL "CAUP" USING "22SCROLLAB" WH-CREG.
           PERFORM VARYING IX FROM 11 BY -1 UNTIL IX < 2
               MOVE WT-KEY(IX - 1) TO WT-KEY(IX).
           COMPUTE VDU-LP = IX * 100 + 100.
           PERFORM KER-ANZ.
       Z.  EXIT.
      ******************************************************************
       NODATA SECTION.
       A.  DISPLAY "** keine Daten **" with highlight AT 2562.
           SET FINE TO TRUE.
       Z.  EXIT.
      ******************************************************************
       READBEW SECTION.
       C.  READ KERMODEL INVALID MOVE LOW-VALUE TO WT-ERFTAB.
           IF ZUGRIF PERFORM BESETZT GO C.
       Z.  EXIT.
      ******************************************* Einzelzeile anzeigen *
       ANZEIG SECTION.
       A.  SET RET TO TRUE.
           IF IX = 0 GO Z.
           COMPUTE VDU-LP = IX * 100 + 100.
           IF WT-KEY(IX) = LOW-VALUE INITIALIZE KM-SATZ
                IF WS NOT = 2 MOVE 0 TO WS
                end-if
                GO Z.
           MOVE WT-KEY(IX) TO KM-KEY.
       C.  READ KERMODEL KEY IS KM-KEY INVALID PERFORM NO-REC
               SET ESC TO TRUE GO Z.
           IF ZUGRIF PERFORM BESETZT GO C.
           MOVE 1 TO WS.
           PERFORM KER-ANZ.
       Z.  EXIT.
      ************************************************** Zeile lîschen *
       LOEBEW SECTION.
       A.  DISPLAY "Wirklich lîschen? " with highlight at 2301
               " < >, <esc>= Abbruch, <Strg+Entf>= lîschen".
           CALL "CAUP" USING "0023210000" WH-CREG.
           IF ESC GO Z.
           IF not SENT GO A.
           DELETE KERMODEL INVALID NEXT SENTENCE.
       Z.  EXIT.
      ********************************************** Position einfÅgen *
       EINFUEG SECTION.
       A.  IF WH-NUM = 0 GO X.
           PERFORM ALTBILD.
           CALL "CAUP" USING "1323012580" WH-CREG.
           MOVE WT-KEY(IX) TO KM-KEY.
           IF WH-NUM not < KM-ZEIL GO X.
           IF IX = 1 MOVE WT-KEY(IX) TO KM-KEY
                     START KERMODEL KEY < KM-KEY INVALID GO C
                     end-start
                     READ KERMODEL PREVIOUS AT END GO C
                     end-read
                     IF WH-NUM not > KM-ZEIL GO X
                     else GO C.
           IF KM-ZEIL = 0 MOVE WT-KEY(IX - 1) TO KM-KEY
                          IF WH-NUM < KM-ZEIL GO X.
           MOVE WT-KEY(IX - 1) TO KM-KEY.
           IF WH-NUM not > KM-ZEIL GO X.
           IF IX = 11 MOVE WT-KEY(IX) TO KM-KEY
                      START KERMODEL KEY NOT < KM-KEY INVALID GO C
                      end-start
                      READ KERMODEL NEXT AT END GO C
                      end-read
                      IF WH-NUM not < KM-ZEIL GO X.
       C.  MOVE WH-NUM TO KM-ZEIL.
           READ KERMODEL NOT INVALID
               DISPLAY "bereits vorhanden" AT 2401
               PERFORM WEITER
               SET ESC TO TRUE
               GO Z.
           IF WT-KEY(IX) = LOW-VALUE GO X.
           MOVE 2 TO WS.
           IF WH-NUM = 0 MOVE 0 TO WH-WERT.
           PERFORM OLDPIC.
           MOVE WT-KEY(IX) TO WH-FIKEY.
           PERFORM VARYING XS FROM 11 BY -1 UNTIL XS < IX
               MOVE WT-KEY(XS) TO WT-KEY(XS + 1).
           MOVE LOW-VALUE TO WT-KEY(IX).
           SET RET TO TRUE.
           GO Z.
       X.  DISPLAY "Nicht mîglich!" with highlight foreground-color 4
               AT 2401.
           PERFORM WEITER.
           SET ESC TO TRUE.
       Z.  EXIT.
      ******************************************* altes Bild abstellen *
       OLDPIC SECTION.
       A.  MOVE WT-ERFTAB TO WI-ERFTAB.
           MOVE 2000 TO WX-LG.
           MOVE 0 TO WX-PS.
           CALL "CBL_READ_SCR_CHATTRS" USING
                WX-PS WI-ZEICH WI-ATTR WX-LG.
           IF not EINF GO Z.
           CALL "CAUP" USING "220000SCRAB" WH-CREG.
      *    COMPUTE VDU-LP = IX * 100 + 302 + VDU-ECK.
      *    CALL "CAUP" USING "1400000061" WH-CREG.
       Z.  EXIT.
      ************************************* altes Bild wieder ausgeben *
       ALTBILD SECTION.
       A.  MOVE 2000 TO WX-LG.
           MOVE 0 TO WX-PS.
           CALL "CBL_WRITE_SCR_CHATTRS"
               USING WX-PS WI-ZEICH WI-ATTR WX-LG.
           MOVE WI-ERFTAB TO WT-ERFTAB.
       Z.  EXIT.
      ******************************************************************
       ZEIHEL SECTION.
       A.  COMPUTE WX-L = VDU-EL + 1 + IX.
           COMPUTE WX-P = VDU-EP + 1.
           MOVE 48 TO WX-LNG.
           CALL "CBL_READ_SCR_ATTRS" USING WX-PAR WM-ATTRIB WX-LNG.
           MOVE ALL x"71" TO WX-ATTRIB.
           CALL "CBL_WRITE_SCR_ATTRS" USING WX-PAR WX-ATTRIB WX-LNG.
       Z.  EXIT.
      ******************************************************************
       ZEIDUN SECTION.
       A.  COMPUTE WX-L = VDU-EL + 1 + IX.
           COMPUTE WX-P = VDU-EP + 1.
           MOVE 48 TO WX-LNG.
           CALL "CBL_WRITE_SCR_ATTRS" USING WX-PAR WM-ATTRIB WX-LNG.
       Z.  EXIT.
      ******************************************************************
       BS-ZEIL SECTION.
       A.  ADD 1 TO IX.
           IF IX < 12 GO Z.
       B.  CALL "CAUP" USING "17SCROLAUF" WH-CREG.
           PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 10
               MOVE WT-KEY(IX + 1) TO WT-KEY(IX).
           MOVE LOW-VALUE TO WT-KEY(IX).
       Z.  EXIT.
      ************************************ kurzfristige Erfolgsrechung *
       K-E-R SECTION.
       A.  MOVE "    K  E  R" TO WK-GEB.
           CALL "CAUP" USING "06KOPF" WH-CREG.
           CALL "CAUP" USING "270511134000011" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Kurzfristige Erfolgsrechnung " with highlight
              AT VDU-LP.
           DISPLAY "1- Bearbeitung"     AT 0717
                   "2- KER-Zuordnung"   AT 0017
                   "3- KER-Listung"     AT 0017
                   "4- KER-öbersicht"   AT 0017
                   "5- KER-Reorg    "   AT 0017.
           DISPLAY "bitte wÑhlen Sie:" with highlight AT 1322.
       C.  DISPLAY "<ret-leer/esc>= Programmende" AT 2301.
           CALL "CAUP" USING "1009301001" WH-CREG.
           IF ESC MOVE 0 TO WH-NUM.
                  SET RET TO TRUE.
           IF not RET or WH-NUM > 5 GO C.
           MOVE WH-NUM TO WH-PG.
           EVALUATE WH-PG
               WHEN 0 GO X
               WHEN 1 PERFORM KER-ANL
               WHEN 2 PERFORM KER-ZUORD
               WHEN 3 PERFORM KER-LIST
               WHEN 4 PERFORM KER-UEBDRU
               WHEN 5 PERFORM KER-REORG
               WHEN 6 PERFORM KER-DATEN.
           GO C.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ********************************************** Ker-Zeile lîschen *
       KER-UEBDRU SECTION.
       A.  MOVE 0 TO WZ-ZEILEN WM-OPEN.
           MOVE "KERUEB.LST" TO WH-DRUNAM.
           MOVE LOW-VALUE TO KM-SATZ.
           START KERMODEL KEY not < KM-KEY INVALID GO Z.
       C.  READ KERMODEL NEXT IGNORE LOCK AT END GO W.
           IF WZ-ZEILEN > WH-SIZE WRITE DRA-SATZ AFTER PAGE
               MOVE 0 TO WZ-ZEILEN.
           IF WZ-ZEILEN > 0 GO G.
           MOVE 2 TO WH-P1.
           IF WM-KO = 0 MOVE 5 TO WH-P2
                        MOVE 42 TO WH-SIZE
                        MOVE 0 TO WZ-SCHALT
                   else MOVE 4 TO WH-P2
                        MOVE 62 TO WH-SIZE.
           IF WM-DRU = 1 MOVE 61 TO WH-SIZE                    *> Laser
               MOVE 0 TO WZ-SCHALT
               MOVE "&l26a6d0O(s0p13h0b0s3T" TO WE-STG(8)
               MOVE 18 TO WH-P1.
           PERFORM BEG-DRU.
           IF WM-DRU not = 1 MOVE 1 TO WZ-SCHALT.
           MOVE "(s1p0s3b4101T" TO DRA-SATZ.
           MOVE WK-FIRMA TO DRA-SATZ(21:).
           MOVE "ErgebnisÅbersicht " TO DRA-SATZ (70:).
           MOVE WX-PER TO DRA-SATZ(89:)
           MOVE "(s0p0s0b0T" TO DRA-SATZ(104:).
           MOVE 0 TO WZ-SCHALT.
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRK-STR PERFORM DRUCK.
           MOVE " Zl. Art  Bezeichnung               Re  X  Hilfe
      -       "   " TO DRK-STR.
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRK-STR PERFORM DRUCK.
           MOVE 2 TO WZ-SCHALT.
       G.  MOVE KM-ZEIL TO DRK-ZEIL.
           MOVE KM-ART TO DRK-ART.
           EVALUATE KM-ART
               WHEN 0 MOVE "Kontozeile" TO DRK-HILFE
               WHEN 1 MOVE "Gruppentextierung" TO DRK-HILFE
               WHEN 2 MOVE "Unterstreichung" TO DRK-HILFE
               WHEN 3 MOVE "Gruppensumme" TO DRK-HILFE
               WHEN 4 MOVE "Zwischensumme" TO DRK-HILFE
               WHEN 5 MOVE "Ergebnis " TO DRK-HILFE.
           MOVE KM-TEXT TO DRK-TEXT.
           MOVE KM-RECH TO DRK-RECH.
           MOVE KM-PZ TO DRK-PZ.
           PERFORM DRUCK.
           GO C.
       W.  MOVE ALL "ƒ" TO DRK-STR.
           MOVE WM-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRA-STR.
           PERFORM DRUCK.
           PERFORM END-DRU.
           DISPLAY "Ker-öbersicht gedruck" AT 2401.
           PERFORM WEITER.
       Z.  EXIT.
      ******************************************* KER-Abgrenzungsdaten *
       KER-DATEN SECTION.
       A.  OPEN I-O KERMODEL.
           CLOSE KERMODEL.
       Z.  EXIT.
      ******************************************************************
       COMP-START SECTION.
       A.  MOVE 9 TO WH-KEY.
           READ KONSTANT IGNORE LOCK INVALID MOVE 0 TO KO-WBEG(1).
           COMPUTE WM-START = WK-PER * 100 + 20000001.
           PERFORM VARYING WX FROM 4 BY -1 UNTIL WX = 0
               IF KO-WBEG(WX) not = 0 and <= WM-START
                   MOVE KO-WBEG(WX) TO WM-START
                   MOVE KO-WEND(WX) TO WM-ENDE
               exit perform.
           COMPUTE WA-TAGE = FUNCTION INTEGER-OF-DATE(WM-START).
           ADD -1 TO WA-TAGE.
           COMPUTE WS-DATUM = FUNCTION DATE-OF-INTEGER(WA-TAGE).
           IF WX = 0 DISPLAY "Periode nicht vorhanden!" AT 2401
               SET ESC TO TRUE
               GO Z.
           COMPUTE WM-START = WM-START - 10000.
       Z.  EXIT.
      ****************************************** Anlage KER-Auswertung *
       KER-LIST SECTION.
       A.  CALL "CAUP" USING "270612075500003" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " KER-Auswertung " with highlight AT VDU-LP.
       C.  ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "Periode: " AT VDU-LP.
           DISPLAY "<esc>= Abbruch, <ret>= Periode (JJMM)" AT 2301.
           CALL "CAUP" USING "1003124004" WH-CREG.
           IF ESC GO X.
           IF not RET GO C.
       E.  COMPUTE WM-START = WH-WERT * 100 + 20000100.
           COMPUTE WA-TAGE = FUNCTION INTEGER-OF-DATE(WM-START).
           COMPUTE WS-DATUM = FUNCTION DATE-OF-INTEGER(WA-TAGE).
           IF WS-DATUM > WM-DATUM GO C.
      *    MOVE WM-DATUM TO WZ-DATUM.
      *    IF WC-JAHR > 12 or WC-JAHR = 0 or WC-JAHR > WZ-MONAT GO C.
      *    IF WC-MONAT > WZ-JAHR GO C.
           ADD 200000 WH-WERT GIVING WD-PERIOD WK-PER WC-DATUM.
           DISPLAY WD-PERIOD with highlight AT VDU-LP.
           PERFORM COMP-START.
           IF ESC GO X.
           MOVE WC-JAHR TO WI WQ.
           EVALUATE WI
               WHEN  1 MOVE "JÑnner   " TO WX-PER
               WHEN  2 MOVE "Februar  " TO WX-PER
               WHEN  3 MOVE "MÑrz     " TO WX-PER
               WHEN  4 MOVE "April    " TO WX-PER
               WHEN  5 MOVE "Mai      " TO WX-PER
               WHEN  6 MOVE "Juni     " TO WX-PER
               WHEN  7 MOVE "Juli     " TO WX-PER
               WHEN  8 MOVE "August   " TO WX-PER
               WHEN  9 MOVE "September" TO WX-PER
               WHEN 10 MOVE "Oktober  " TO WX-PER
               WHEN 11 MOVE "November " TO WX-PER
               WHEN 12 MOVE "Dezember " TO WX-PER.
           PERFORM VARYING WZ FROM 11 BY -1 UNTIL
               WX-PER(WZ:1) not = SPACE CONTINUE.
           ADD 2 TO WZ.
           MOVE WD-PERIOD(1:4) TO WX-PER(WZ:).
      *-----------------------------> Wirtschaftsjahr -> Kalenderjahr <-
           MOVE WS-DATUM TO WC-DATUM.
           SUBTRACT WC-MONAT FROM WQ.
           IF WQ < 1 ADD 12 TO WQ.
           MOVE WQ TO WI.
       G.  DISPLAY "<esc>= Abbruch, <ret>= Start, < />= zurÅck < >"
               AT 2301.
           CALL "CAUP" USING "0023460000" WH-CREG.
           IF ESC GO X.
           IF WOLI GO C.
           IF not RET and not AB GO G.
           IF RET PERFORM KER-SUM.
           MOVE 0 TO WM-PZL.
           PERFORM VARYING WM-PZL FROM 1 BY 1 UNTIL WM-PZL > 199
               or WKM-PZ(WM-PZL) = "*" CONTINUE.
           PERFORM KER-DRUCK.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ********************************************** KER-Summen bilden *
       KER-SUM SECTION.
       A.  MOVE 0 TO WM-SUM WM-VSUM WS-SUM WS-VSUM
                     WZ-SUM WZ-VSUM WG-SUM WG-VSUM
                     WT-SUM WT-VSUM WZ-SEITE WZ-ZEILEN.
           MOVE LOW-VALUE TO KM-SATZ WT-KERTAB.
           START KERMODEL KEY not < KM-KEY INVALID GO Z.
       C.  READ KERMODEL NEXT AT END GO Z.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 25
               MOVE 0 TO KM-SUMMEN(WX).
           MOVE WK-PER TO KM-PERIODE.
           IF KM-ART not = 0 GO J.
           MOVE LOW-VALUE TO SA-SATZ.
           MOVE KM-ZEIL TO SA-KER-NR.
           START SACHBUCH KEY > SA-KER INVALID GO C.
       E.  READ SACHBUCH NEXT IGNORE LOCK AT END GO J.
           IF SA-KER-NR > KM-ZEIL GO J.                  *> keine Daten
           IF KM-ZEIL not = SA-KER-NR GO J.
           ADD 403 VDU-ECK GIVING VDU-LP.
           COMPUTE WD-KTO = SA-KTONR / 10.
           DISPLAY WD-KTO AT VDU-LP " " SA-BEZ(1:39).
           MOVE SA-KTONR TO BZ-KTONR.
           MOVE WM-START TO BZ-BUDAT.
           MOVE 0 TO BZ-LFD.
           ADD 503 VDU-ECK GIVING VDU-LP.
           START BUCHZEIL KEY not < BZ-KEY INVALID GO E.
       G.  READ BUCHZEIL NEXT IGNORE LOCK AT END GO E.
           IF BZ-KTONR not = SA-KTONR or BZ-BUDAT > WM-ENDE GO E.
           IF BZ-SYM > 97 GO G.
           MOVE BZ-BUDAT TO WC-DATUM.
           MOVE WC-MONAT TO WQ.
      *-----------------------------> Wirtschaftsjahr -> Kalenderjahr <-
           MOVE WS-DATUM TO WC-DATUM.
           SUBTRACT WC-MONAT FROM WQ.
           IF WQ < 1 ADD 12 TO WQ.
           MOVE WQ TO WZ.
           PERFORM DATDREH.
           DISPLAY VDU-DATUM AT VDU-LP.
           IF BZ-BUDAT > WS-DATUM ADD 12 TO WZ.         *> Folgejahr
           IF BZ-SH = 1 COMPUTE BZ-BUBET = BZ-BUBET * -1.
           IF BZ-BUDAT not < WE-EURO ADD BZ-BUBET TO KM-SUMMEN(WZ)
           else COMPUTE WK-BET rounded = BZ-BUBET / 13,7603
                ADD WK-BET TO KM-SUMMEN(WZ).
           GO G.
       J.  PERFORM BILDSUM.
      *    REWRITE KM-SATZ.
           GO C.
       M.  PERFORM BILDSUM.
      *    REWRITE KM-SATZ.
       Z.  EXIT.
      ************************************************** Summen bilden *
       BILDSUM SECTION.
       A.  MOVE KM-ZEIL TO WL.
           MOVE KM-ART TO WKM-ART(WL).
           MOVE KM-TEXT TO WKM-TEXT(WL).
           MOVE KM-RECH TO WKM-RECH(WL).
           MOVE KM-PZ TO WKM-PZ(WL).
           EVALUATE KM-ART
               WHEN 0 MOVE KM-SUMMEN(WI + 12) TO WK-BET
                      IF KM-RECH = "-"
                          SUBTRACT WK-BET FROM WKM-SUM(WL, 1) WZ-SUM
                      else ADD WK-BET TO WKM-SUM(WL, 1) WZ-SUM
                      end-if
                      MOVE 0 TO WK-BET
                      PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ  > WI
                          ADD KM-SUMMEN(WZ + 12) TO WK-BET
                      end-perform
                      IF KM-RECH = "-"
                          SUBTRACT WK-BET FROM WKM-SUM(WL, 2) WG-SUM
                      else ADD WK-BET TO WKM-SUM(WL, 2) WG-SUM
                      end-if
                      MOVE KM-SUMMEN(WI) TO WK-BET
                      IF KM-RECH = "-"
                          SUBTRACT WK-BET FROM WKM-SUM(WL, 3) WZ-VSUM
                      else ADD WK-BET TO WKM-SUM(WL, 3) WZ-VSUM
                      end-if
                      MOVE 0 TO WK-BET
                      PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ  > WI
                          ADD KM-SUMMEN(WZ) TO WK-BET
                      end-perform
                      IF KM-RECH = "-"
                          SUBTRACT WK-BET FROM WKM-SUM(WL, 4) WG-VSUM
                      else ADD WK-BET TO WKM-SUM(WL, 4) WG-VSUM
                      end-if
               WHEN 2 MOVE ALL "ƒ" TO WKM-TEXT(WL)
               WHEN 3 MOVE WZ-SUM TO WKM-SUM(WL, 1)
                      MOVE WG-SUM TO WKM-SUM(WL, 2)
                      MOVE WZ-VSUM TO WKM-SUM(WL, 3)
                      MOVE WG-VSUM TO WKM-SUM(WL, 4)
                      IF KM-RECH = "-" SUBTRACT WZ-SUM FROM WM-SUM
                                       SUBTRACT WG-SUM FROM WT-SUM
                                       SUBTRACT WZ-VSUM FROM WM-VSUM
                                       SUBTRACT WG-VSUM FROM WT-VSUM
                                  else ADD WZ-SUM TO WM-SUM
                                       ADD WG-SUM TO WT-SUM
                                       ADD WZ-VSUM TO WM-VSUM
                                       ADD WG-VSUM TO WT-VSUM
                                  end-if
                      MOVE 0 TO WZ-SUM WZ-VSUM WG-SUM WG-VSUM
               WHEN 4 MOVE WM-SUM TO WKM-SUM(WL, 1)
                      MOVE WT-SUM TO WKM-SUM(WL, 2)
                      MOVE WM-VSUM TO WKM-SUM(WL, 3)
                      MOVE WT-VSUM TO WKM-SUM(WL, 4)
               WHEN 5 IF KM-RECH = "-" SUBTRACT WZ-SUM FROM WM-SUM
                                       SUBTRACT WG-SUM FROM WT-SUM
                                       SUBTRACT WZ-VSUM FROM WM-VSUM
                                       SUBTRACT WG-VSUM FROM WT-VSUM
                                  else ADD WZ-SUM TO WM-SUM
                                       ADD WG-SUM TO WT-SUM
                                       ADD WZ-VSUM TO WM-VSUM
                                       ADD WG-VSUM TO WT-VSUM
                                  end-if
                      MOVE WM-SUM TO WKM-SUM(WL, 1)
                      MOVE WT-SUM TO WKM-SUM(WL, 2)
                      MOVE WM-VSUM TO WKM-SUM(WL, 3)
                      MOVE WT-VSUM TO WKM-SUM(WL, 4).
           REWRITE KM-SATZ.
       Z.  EXIT.
      ************************************* Ausruck Ergebnisauswertung *
       KER-DRUCK SECTION.
       A.  MOVE 0 TO WE.
           MOVE 0 TO WM-SUM WM-VSUM WS-SUM WS-VSUM
                     WZ-SUM WZ-VSUM WG-SUM WG-VSUM
                     WT-SUM WT-VSUM WZ-SEITE WZ-ZEILEN.
           MOVE "FIBKER.LST" TO WH-DRUNAM.
       C.  ADD 1 TO WE.
           IF WE > 199 GO W.
           IF WKM-TEXT(WE) = LOW-VALUES GO C.
           PERFORM KER-KOPF.
           EVALUATE WKM-ART(WE)
               WHEN 0 MOVE WKM-TEXT(WE) TO DRA-TEXT
                      MOVE WKM-SUM(WE, 1) TO DRA-BET(1) WK-BET
                      COMPUTE DRA-PZ(1) rounded =
                           WK-BET * 100 / WKM-SUM(WM-PZL, 1)
                      MOVE WKM-SUM(WE, 2) TO DRA-BET(2) WK-BET
                      COMPUTE DRA-PZ(2) rounded =
                           WK-BET * 100 / WKM-SUM(WM-PZL, 2)
                      MOVE WKM-SUM(WE, 3) TO DRA-BET(3) WK-BET
                      COMPUTE DRA-PZ(3) rounded =
                           WK-BET * 100 / WKM-SUM(WM-PZL, 3)
                      MOVE WKM-SUM(WE, 4) TO DRA-BET(4) WK-BET
                      COMPUTE DRA-PZ(4) rounded =
                           WK-BET * 100 / WKM-SUM(WM-PZL, 4)
               WHEN 1 MOVE 2 TO WZ-SCHALT
                      MOVE "(s1p0s1b3T" TO DRA-STR
                      MOVE WKM-TEXT(WE) TO DRA-STR(12:)
                      MOVE "(s0p0s0b0T" TO DRA-STR(44:)
               WHEN 2 MOVE ALL "ƒ" TO DRA-STR
               WHEN 3 MOVE "(s1p0s1b3T" TO DRA-STR
                      MOVE WKM-TEXT(WE) TO DRA-STR(12:)
                      MOVE "(s0p0s0b0T" TO DRA-STR(44:)
                      PERFORM DRUCK
                      MOVE 0 TO WZ-SCHALT
                      MOVE WKM-SUM(WE, 1) TO DRA-BET(1) WK-BET
                      COMPUTE DRA-PZ(1) rounded =
                           WK-BET * 100 / WKM-SUM(WM-PZL, 1)
                      MOVE WKM-SUM(WE, 2) TO DRA-BET(2) WK-BET
                      COMPUTE DRA-PZ(2) rounded =
                           WK-BET * 100 / WKM-SUM(WM-PZL, 2)
                      MOVE WKM-SUM(WE, 3) TO DRA-BET(3) WK-BET
                      COMPUTE DRA-PZ(3) rounded =
                           WK-BET * 100 / WKM-SUM(WM-PZL, 3)
                      MOVE WKM-SUM(WE, 4) TO DRA-BET(4) WK-BET
                      COMPUTE DRA-PZ(4) rounded =
                           WK-BET * 100 / WKM-SUM(WM-PZL, 4)
                      IF WKM-PZ(WE) = "*" MOVE 100,0 TO DRA-PZ(1)
                                          DRA-PZ(2) DRA-PZ(3) DRA-PZ(4)
               WHEN 4 MOVE "(s1p0s1b3T" TO DRA-STR
                      MOVE WKM-TEXT(WE) TO DRA-STR(12:)
                      MOVE "(s0p0s0b0T" TO DRA-STR(44:)
                      MOVE 2 TO WZ-SCHALT
                      PERFORM DRUCK
                      MOVE 0 TO WZ-SCHALT
                      MOVE WKM-SUM(WE, 1) TO DRA-BET(1) WK-BET
                      COMPUTE DRA-PZ(1) rounded =
                           WK-BET * 100 / WKM-SUM(WM-PZL, 1)
                      MOVE WKM-SUM(WE, 2) TO DRA-BET(2) WK-BET
                      COMPUTE DRA-PZ(2) rounded =
                           WK-BET * 100 / WKM-SUM(WM-PZL, 2)
                      MOVE WKM-SUM(WE, 3) TO DRA-BET(3) WK-BET
                      COMPUTE DRA-PZ(3) rounded =
                           WK-BET * 100 / WKM-SUM(WM-PZL, 3)
                      MOVE WKM-SUM(WE, 4) TO DRA-BET(4) WK-BET
                      COMPUTE DRA-PZ(4) rounded =
                           WK-BET * 100 / WKM-SUM(WM-PZL, 4)
               WHEN 5 MOVE "(s1p0s1b3T" TO DRA-STR
                      MOVE WKM-TEXT(WE) TO DRA-STR(12:)
                      MOVE "(s0p0s0b0T" TO DRA-STR(44:)
                      PERFORM DRUCK
                      MOVE 0 TO WZ-SCHALT
                      MOVE WKM-SUM(WE, 1) TO DRA-BET(1) WK-BET
                      COMPUTE DRA-PZ(1) rounded =
                           WK-BET * 100 / WKM-SUM(WM-PZL, 1)
                      MOVE WKM-SUM(WE, 2) TO DRA-BET(2) WK-BET
                      COMPUTE DRA-PZ(2) rounded =
                           WK-BET * 100 / WKM-SUM(WM-PZL, 2)
                      MOVE WKM-SUM(WE, 3) TO DRA-BET(3) WK-BET
                      COMPUTE DRA-PZ(3) rounded =
                           WK-BET * 100 / WKM-SUM(WM-PZL, 3)
                      MOVE WKM-SUM(WE, 4) TO DRA-BET(4) WK-BET
                      COMPUTE DRA-PZ(4) rounded =
                           WK-BET * 100 / WKM-SUM(WM-PZL, 4).
           PERFORM DRUCK.
           GO C.
       W.  MOVE ALL "Õ" TO DRA-STR PERFORM DRUCK.
           MOVE WM-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRA-STR.
           PERFORM DRUCK.
           PERFORM END-DRU.
       Z.  EXIT.
      ************************************* Ausruck Ergebnisauswertung *
       KER-DRUCK-ALT SECTION.
       A.  MOVE LOW-VALUES TO KM-SATZ.
           MOVE 0 TO WM-SUM WM-VSUM WS-SUM WS-VSUM
                     WZ-SUM WZ-VSUM WG-SUM WG-VSUM
                     WT-SUM WT-VSUM WZ-SEITE WZ-ZEILEN.
           MOVE "FIBKER.LST" TO WH-DRUNAM.
           START KERMODEL KEY not < KM-KEY INVALID GO Z.
       C.  READ KERMODEL NEXT IGNORE LOCK AT END GO W.
           PERFORM KER-KOPF.
      *    DISPLAY "1= Gruppentextierung" with highlight AT VDU-LP.
      *    DISPLAY "2= Unterstreichung" with highlight AT VDU-LP.
      *    DISPLAY "3= Gruppensumme" with highlight AT VDU-LP.
      *    DISPLAY "4= Zwischensumme" with highlight AT VDU-LP.
      *    DISPLAY "5= Ergebnis " with highlight AT VDU-LP.
           EVALUATE KM-ART
               WHEN 0 MOVE KM-TEXT TO DRA-TEXT
                      MOVE KM-SUMMEN(WI + 12) TO DRA-BET(1) WK-BET
                      IF KM-RECH = "-" SUBTRACT WK-BET FROM WZ-SUM
                                  else ADD WK-BET TO WZ-SUM
                                  end-if
                      MOVE 0 TO WK-BET
                      PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ  > WI
                          ADD KM-SUMMEN(WZ + 12) TO WK-BET
                      end-perform
                      MOVE WK-BET TO DRA-BET(2)
                      IF KM-RECH = "-" SUBTRACT WK-BET FROM WG-SUM
                                  else ADD WK-BET TO WG-SUM
                                  end-if
                      MOVE KM-SUMMEN(WI) TO DRA-BET(3) WK-BET
                      IF KM-RECH = "-" SUBTRACT WK-BET FROM WZ-VSUM
                                  else ADD WK-BET TO WZ-VSUM
                                  end-if
                      MOVE 0 TO WK-BET
                      PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ  > WI
                          ADD KM-SUMMEN(WZ) TO WK-BET
                      end-perform
                      MOVE WK-BET TO DRA-BET(4)
                      IF KM-RECH = "-" SUBTRACT WK-BET FROM WG-VSUM
                                  else ADD WK-BET TO WG-VSUM
                                  end-if
               WHEN 1 MOVE 2 TO WZ-SCHALT
                      MOVE "(s1p0s1b3T" TO DRA-STR
                      MOVE KM-TEXT TO DRA-STR(12:)
                      MOVE "(s0p0s0b0T" TO DRA-STR(44:)
               WHEN 2 MOVE ALL "ƒ" TO DRA-STR
               WHEN 3 MOVE "(s1p0s1b3T" TO DRA-STR
                      MOVE KM-TEXT TO DRA-STR(12:)
                      MOVE "(s0p0s0b0T" TO DRA-STR(44:)
                      PERFORM DRUCK
                      MOVE 0 TO WZ-SCHALT
                      MOVE WZ-SUM TO DRA-BET(1)
                      MOVE WG-SUM TO DRA-BET(2)
                      MOVE WZ-VSUM TO DRA-BET(3)
                      MOVE WG-VSUM TO DRA-BET(4)
                      IF KM-RECH = "-" SUBTRACT WZ-SUM FROM WM-SUM
                                       SUBTRACT WG-SUM FROM WT-SUM
                                       SUBTRACT WZ-VSUM FROM WM-VSUM
                                       SUBTRACT WG-VSUM FROM WT-VSUM
                                  else ADD WZ-SUM TO WM-SUM
                                       ADD WG-SUM TO WT-SUM
                                       ADD WZ-VSUM TO WM-VSUM
                                       ADD WG-VSUM TO WT-VSUM
                                  end-if
                      MOVE 0 TO WZ-SUM WZ-VSUM WG-SUM WG-VSUM
               WHEN 4 MOVE "(s1p0s1b3T" TO DRA-STR
                      MOVE KM-TEXT TO DRA-STR(12:)
                      MOVE "(s0p0s0b0T" TO DRA-STR(44:)
                      MOVE 2 TO WZ-SCHALT
                      PERFORM DRUCK
                      MOVE 0 TO WZ-SCHALT
                      MOVE WM-SUM TO DRA-BET(1)
                      MOVE WT-SUM TO DRA-BET(2)
                      MOVE WM-VSUM TO DRA-BET(3)
                      MOVE WT-VSUM TO DRA-BET(4)
               WHEN 5 MOVE "(s1p0s1b3T" TO DRA-STR
                      MOVE KM-TEXT TO DRA-STR(12:)
                      MOVE "(s0p0s0b0T" TO DRA-STR(44:)
                      PERFORM DRUCK
                      MOVE 0 TO WZ-SCHALT
                      MOVE WM-SUM TO DRA-BET(1)
                      MOVE WT-SUM TO DRA-BET(2)
                      MOVE WM-VSUM TO DRA-BET(3)
                      MOVE WT-VSUM TO DRA-BET(4).
           PERFORM DRUCK.
           GO C.
       W.  MOVE ALL "Õ" TO DRA-STR PERFORM DRUCK.
           MOVE WM-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRA-STR.
           PERFORM DRUCK.
           PERFORM END-DRU.
       Z.  EXIT.
      ***************************************** Kopf Sachkontenlistung *
       KER-KOPF SECTION.
       A.  IF WZ-ZEILEN > WH-SIZE WRITE DRA-SATZ AFTER PAGE
               MOVE 0 TO WZ-ZEILEN.
           IF WZ-ZEILEN > 0 GO Z.
           MOVE 2 TO WH-P1.
           IF WM-KO = 0 MOVE 5 TO WH-P2
                        MOVE 42 TO WH-SIZE
                        MOVE 0 TO WZ-SCHALT
                   else MOVE 4 TO WH-P2
                        MOVE 62 TO WH-SIZE.
           IF WM-DRU = 1 MOVE 61 TO WH-SIZE                    *> Laser
               MOVE 0 TO WZ-SCHALT
               MOVE "&l26a6d0O(s0p13h0b0s3T" TO WE-STG(8)
               MOVE 18 TO WH-P1.
           IF WM-OPEN = 0 MOVE 1 TO WM-FIBEX
               MOVE "FIBUKER.XLS" TO WH-FIBNAM
               DELETE FILE EXCELIS
               OPEN EXTEND EXCELIS.
           PERFORM BEG-DRU.
           IF WM-DRU not = 1 MOVE 1 TO WZ-SCHALT.
           MOVE "(s1p0s3b4101T" TO DRA-SATZ.
           MOVE WK-FIRMA TO DRA-SATZ(19:).
           MOVE "ErgebnisÅbersicht " TO DRA-SATZ (68:).
           MOVE WX-PER TO DRA-SATZ(87:)
           MOVE "(s0p0s0b0T" TO DRA-SATZ(102:).
           MOVE "Seite:" TO DRA-SATZ(126:).
           ADD 1 TO WZ-SEITE.
           MOVE WZ-SEITE TO WD-SEITE.
           MOVE WD-SEITE TO DRA-SATZ(133:).
           MOVE 0 TO WZ-SCHALT.
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRA-STR PERFORM DRUCK.
           MOVE "B e z e i c h n u n g           Monat   %          Jahr
      -       "   %      Vj-Monat   %       Vorjahr   %" TO DRA-STR.
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRA-STR PERFORM DRUCK.
           MOVE 2 TO WZ-SCHALT.
       Z.  EXIT.
      ***************************************** serielle KER-Zuordnung *
       KER-ZUORD SECTION.
       A.  CALL "CAUP" USING "270709066800014" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " KER-Zuordnung " with highlight AT VDU-LP.
           INITIALIZE SA-SATZ.
           START SACHBUCH KEY not < SA-KEY INVALID GO X.
       C.  ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "Kto: " AT VDU-LP.
           DISPLAY "<esc>= Abbruch, <ret>= Kto-Nr., alpha+<ret>= suchen,
      -        "< / >= nÑchstes/vorheriges" AT 2301.
           CALL "CAUP" USING "1003086009" WH-CREG.
           IF ESC GO X.
       D.  IF AUF or AB PERFORM READ-SEQ
                  IF FINE GO C
                  else GO G.
           IF not RET GO C.
           IF WH-NUM = 0 CALL "FIBANZ" USING "20SASUCH" WH-CREG
                         CANCEL "FIBANZ".
           IF WH-NUM = 0 GO C.
           MOVE WH-NUM TO SA-KTONR WD-KZ.
           IF WD-KZ > 1 GO C.
       E.  READ SACHBUCH IGNORE LOCK INVALID GO C.
       G.  CALL "CAUP" USING "16CLRFEN" WH-CREG.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "Kto: " AT VDU-LP.
           COMPUTE WD-KTO = SA-KTONR / 10.
           ADD 308 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-KTO with highlight AT VDU-LP " "
                   SA-BEZ with highlight.
           ADD 403 VDU-ECK GIVING VDU-LP.
           DISPLAY "KER.: " AT VDU-LP.
           MOVE SA-KER-NR TO WD-KER WH-WERT KM-ZEIL.
           READ KERMODEL IGNORE LOCK INVALID INITIALIZE KM-SATZ
                MOVE "-- nicht angelegt! --" TO KM-TEXT.
           IF SA-KER-NR = 0 DISPLAY WD-KER with highlight AT 0000 " "
                                  "ohne " AT 0000
                   else DISPLAY WD-KER with highlight AT 0000 " "
                           KM-TEXT with highlight foreground-color 3.
           DISPLAY "<esc/>= zurÅck, <ret>= KER, < / >= nÑchstes/vorheri
      -        "ges, < >= KER anzeigen" AT 2301.
           DISPLAY "KER-Gruppe: 0= ohne, 1-199 = KER-Zuordnung" AT 2401.
           CALL "CAUP" USING "1004093003" WH-CREG.
           IF ESC or WOLI GO C.
           IF AUF or AB GO D.
           IF WORE MOVE KM-ZEIL TO WH-NUM
                   CALL "FIBANZ" USING "45KERANZ" WH-CREG
                   CANCEL "KERANZ"
                   MOVE WH-NUM TO WH-WERT
                   IF WH-NUM = 0 GO G.
           IF NOT RET GO G.
           IF WH-WERT > 199 GO G.
           ADD 0409 VDU-ECK GIVING VDU-LP.
           MOVE WH-WERT TO WD-KER SA-KER-NR KM-ZEIL.
           IF KM-ZEIL = 0 GO K.
           READ KERMODEL IGNORE LOCK INVALID
                DISPLAY "-- nicht angelegt! --" AT 2401
                PERFORM WEITER GO G.
       K.  IF SA-KER-NR = 0 DISPLAY WD-KER with highlight AT VDU-LP " "
                                  "ohne " AT 0000
                   else DISPLAY WD-KER with highlight AT VDU-LP " "
                           KM-TEXT with highlight foreground-color 3.
           REWRITE SA-SATZ.
           SET AB TO TRUE.
           GO D.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ******************************************************************
       READ-SEQ SECTION.
       A.  IF FINE GO Z.
           IF AB GO G.
       c.  READ SACHBUCH PREVIOUS AT END GO Z.
           IF SA-KTOART not = 1 GO C.
           GO Z.
       G.  READ SACHBUCH NEXT AT END GO Z.
           IF SA-KTOART not = 1 GO G.
       Z.  EXIT.
      *************************************** Konvertierung Sachkonten *
       SA-CONV SECTION.
       A.  OPEN INPUT SX-BUCH.
           IF WF-STATUS not = "39" CLOSE SX-BUCH GO Z.
           CALL "CAUP" USING "270612056000004" WH-CREG.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY " KER-Konvertierung " with highlight AT VDU-LP.
           OPEN INPUT OLDSACH.
           MOVE "N-SACH.DAT" TO WN-SACH.
           OPEN OUTPUT SACHBUCH.
           INITIALIZE OS-SATZ.
           START OLDSACH KEY not < OS-KEY INVALID GO Z.
       C.  READ OLDSACH NEXT AT END GO Q.
           MOVE OS-SATZ TO SA-SATZ.
           MOVE 0 TO SA-KER-RES SA-KER-NR.
           WRITE SA-SATZ INVALID STOP RUN.
           ADD 403 VDU-ECK GIVING VDU-LP.
           DISPLAY SA-BEZ(1:40) AT VDU-LP.
           GO C.
       Q.  DISPLAY "Konvertierung beednet!" AT 2401.
           PERFORM WEITER.
           CLOSE OLDSACH.
           CLOSE SACHBUCH.
           CALL "CBL_COPY_FILE" using "N-SACH.DAT " "FIBUSACH.DAT ".
           CALL "CBL_COPY_FILE" using "N-SACH.IDX " "FIBUSACH.IDX ".
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           MOVE "FIBUSACH.DAT" TO WN-SACH.
       Z.  EXIT.
      *************************************** Reorganisation KER-Daten *
       KER-REORG SECTION.
       A.  CALL "CAUP" USING "270612056000004" WH-CREG.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "Reorganisation der KER-Daten!" with highlight
               AT VDU-LP.
           ADD 403 VDU-ECK GIVING VDU-LP.
           DISPLAY "ACHTUNG! Kann nicht rÅckgÑngig gemacht werden!"
                with highlight foreground-color 4 AT VDU-LP.
           CLOSE KERMODEL.
           OPEN I-O KERMODEL WITH LOCK.
       C.  DISPLAY "<esc>= Abruch, <Strg-Pos 1>= starten < >" at 2301.
           CALL "CAUP" USING "0023390000" WH-CREG.
           IF ESC GO X.
           IF not SAPO GO C.
           CALL "CAUP" USING "16CLRFEN" WH-CREG.
           INITIALIZE KM-SATZ WT-KERTAB.
           PERFORM LADE-KER.
           IF ESC GO X.
           CLOSE KERMODEL.
           OPEN OUTPUT KERMODEL.
           CLOSE KERMODEL.
           OPEN I-O KERMODEL.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 199
               IF WKM-ZEIL(WX) not = 0 MOVE WKM-ART(WX) TO KM-ART
                   MOVE WKM-RECH(WX) TO KM-RECH
                   MOVE WKM-TEXT(WX) TO KM-TEXT
                   MOVE WKM-PZ(WX) TO KM-PZ
                   MOVE WKM-TSUM(WX) TO KM-TSUM
                   MOVE WKM-ZEIL(WX) TO KM-ZEIL
                   WRITE KM-SATZ.
           PERFORM SACH-REORG.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ******************************************** énderung Sachkonten *
       SACH-REORG SECTION.
       A.  MOVE LOW-VALUES TO SA-SATZ.
           START SACHBUCH KEY not < SA-KEY INVALID GO Z.
       E.  READ SACHBUCH NEXT AT END GO Z.
           IF ZUGRIF PERFORM BESETZT GO E.
           IF SA-KER-NR = 0 GO E.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 198
              IF WKM-OLD(WX) = SA-KER-NR
                  MOVE WKM-ZEIL(WX) TO SA-KER-NR
                  MOVE 200 TO WX.
           REWRITE SA-SATZ.
           ADD 303 VDU-ECK GIVING VDU-LP.
           COMPUTE WD-KTO = SA-KTONR / 10.
           DISPLAY WD-KTO AT VDU-LP " " SA-BEZ.
           GO E.
       Z.  EXIT.
      ******************************************************************
       LADE-KER SECTION.
       A.  INITIALIZE WT-KERTAB KM-SATZ.
           MOVE 0 TO WX.
           MOVE 0 TO WH-MOD.
           START KERMODEL KEY not < KM-KEY INVALID GO Z.
       C.  READ KERMODEL NEXT IGNORE LOCK AT END GO Z.
           IF WX = 0 and KM-ZEIL > 19 MOVE 1 TO WH-MOD.
           ADD 1 TO WX.
           IF WX > 198 DISPLAY "Reorg nicht mîglich!"  with highlight
                          foreground-color 4 AT 2401
               PERFORM WEITER
               SET ESC TO TRUE
               GO Z.
           IF WH-MOD = 1 COMPUTE KM-ZEIL = KM-ZEIL / 10.
           MOVE KM-ZEIL TO WKM-OLD(WX).
           COMPUTE WKM-ZEIL(WX) = WX * 2.
           MOVE KM-ART TO WKM-ART(WX).
           MOVE KM-RECH TO WKM-RECH(WX).
           MOVE KM-TEXT TO WKM-TEXT(WX).
           MOVE KM-PZ TO WKM-PZ(WX).
           MOVE KM-TSUM TO WKM-TSUM(WX).
           GO C.
       Z.  EXIT.
      ********************************************* OP-Konvertiererung *
       OP-KONVERT SECTION.
       A.  CALL "CAUP" USING "0707100660000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Op: Ats --> Eur " with highlight AT VDU-LP.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "Kto-Nr.:" AT VDU-LP.
       C.  DISPLAY "<esc>= Abbruch, <ret>= Start < >" AT 2301.
           CALL "CAUP" USING "0023310000" WH-CREG.
           IF ESC GO X.
           IF not RET GO C.
           MOVE 0 TO WM-ANZ.
           MOVE LOW-VALUES TO OP-SATZ.
           ADD 312 VDU-ECK GIVING VDU-LP.
           START OPDATEI KEY not < OP-KEY INVALID STOP RUN.
       E.  READ OPDATEI NEXT AT END GO X.
           COMPUTE WD-KTO = OP-KTONR / 10.
           ADD 1 TO WM-ANZ.
           MOVE WM-ANZ TO WD-ANZ.
           DISPLAY WD-KTO with highlight AT VDU-LP " " WD-ANZ.
           IF OP-DK = 0 DELETE OPDATEI
                        GO E.
           IF OP-FSYM = "Eur" GO E.
           COMPUTE OP-MW rounded =  OP-MW / 13,7603.
      *    COMPUTE OP-U1 rounded =  OP-U1 / 13,7603.
           COMPUTE OP-B1 rounded =  OP-B1 / 13,7603.
      *    COMPUTE OP-U2 rounded =  OP-U2 / 13,7603.
           COMPUTE OP-B2 rounded =  OP-B2 / 13,7603.
           COMPUTE OP-REBETRAG rounded =  OP-REBETRAG / 13,7603.
           COMPUTE OP-ZABETRAG rounded =  OP-ZABETRAG / 13,7603.
           COMPUTE OP-SKTOBAS rounded =  OP-SKTOBAS / 13,7603.
           COMPUTE OP-ZPBET rounded =  OP-ZPBET / 13,7603.
           COMPUTE OP-ZPSKTO rounded =  OP-ZPSKTO / 13,7603.
           MOVE 100 TO OP-KURS.
           MOVE "Eur" TO OP-FSYM.
           REWRITE OP-SATZ.
           GO E.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
