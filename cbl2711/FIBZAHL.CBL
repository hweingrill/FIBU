      $SET LINKCOUNT"240" SEGSIZE"65000" ANS85 BOUND AUTOLOCK NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     FIBZAHL.
      ******************************************************************
      *         Zahlungsprogramm öberweisungstrÑger od. DatentrÑger    *
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
           SELECT DRUCKER   ASSIGN TO PRINTER WH-DRUNAM
                            FILE STATUS IS WF-STATUS.
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
      *    03  ZP-BLZ                  PIC 9(5)       COMP.
           03  ZP-FSYM                 PIC XXX.
           03  ZP-VERWEND              PIC X(60).
           03  ZP-DATUM                PIC 9(8)       COMP.
           03  ZP-REDAT                PIC 9(6)       COMP.
           03  ZP-BAKTO                PIC X(15).
           03  ZP-BABEZ                PIC X(30).
      ******************************************************************
       FD  DRUCKER                     LABEL RECORD OMITTED.
       01  DRA-SATZ.
           03  FILLER                  PIC X(5).
           03  DRA-STR                 PIC X(89).
           03  FILLER                  PIC X(20).
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
           03  WM-OPEN                 PIC 9        COMP-3 VALUE ZERO.
           03  WX-PRNO                 PIC 99       COMP-X.
           03  WX-PRSTAT               PIC 99       COMP-X.
           03  WZ-SEITE                PIC 99       COMP-X VALUE ZERO.
           03  WZ-SCHALT               PIC 99       COMP-X VALUE ZERO.
           03  WZ-ZEILEN               PIC 99       COMP-X VALUE ZERO.
           03  WH-SIZE                 PIC 99       COMP   VALUE 62.
           03  WD-KTO                  PIC ZZ.ZZ9,9.
           03  WD-BET                  PIC ZZ.ZZZ.ZZ9,99-.
           03  WD-VBET                 PIC ZZ.ZZZ.ZZ9,99-.
           03  VDU-AB                  PIC 9999.
           03  WM-FSYM                 PIC XXX.
           03  WH-MOD                  PIC 99        COMP.
           03  WI                      PIC 99        COMP.
           03  WE                      PIC 99        COMP.
           03  WL                      PIC 99        COMP.
           03  WS                      PIC 99        COMP.
           03  IW                      PIC 99        COMP.
           03  WH-TAG                  PIC S999      COMP.
           03  WH-UST                  PIC S99V99    COMP.
           03  WC-UST                  PIC S99V9999  COMP.
           03  WK-BASIS                PIC S9(9)V99  COMP.
           03  WT-ERFTAB.
               05 WT-KEY                          occurs 20.
                  07 WT-DK             PIC 999       COMP.
                  07 WT-KTONR          PIC 9(6)      COMP.
                  07 WT-VALDAT         PIC 9(8)      COMP.
                  07 WT-LFD            PIC 9999      COMP.
           03  WH-VALUTA               PIC 9(6)      COMP.
           03  WA-TAGE                 PIC 9(7)      COMP.
           03  WB-TAGE                 PIC 9(8)      COMP.
           03  IX                      PIC 99        COMP-5.
           03  IY                      PIC 99        COMP-5.
           03  WH-L                    PIC 99        COMP.
           03  WV-POS                  PIC 99        COMP-X.
           03  WS-LF                   PIC 9999      COMP.
           03  WV-BISDAT               PIC 9(6)      COMP.
           03  WV-DK                   PIC 99        COMP.
           03  WV-KONTO                PIC 9(6)      COMP.
           03  WM-KONTO                PIC 9(6)      COMP.
           03  WB-KTONR                PIC X(22).
           03  WB-FBEZ                 PIC X(110).
           03  WH-OPKEY.
               05 WH-OPDK              PIC 99        COMP.
               05 WH-OPKTONR           PIC 9(6)      COMP.
               05 WH-OPDAT             PIC 9(8)      COMP.
               05 WH-OPLFD             PIC 9999      COMP.
           03  WD-BELNR                PIC ZZZZZZ9.
           03  WD-PZ                   PIC Z,9.
           03  WD-SKTO                 PIC Z,99.
           03  WD-SKBET                PIC ZZ.ZZZ,99.
           03  WD-SK                   PIC Z,9.
           03  WD-TG                   PIC Z9.
           03  WD-TGN                  PIC ZZ9.
           03  WD-KZ                   PIC 9.
           03  WD-UST                  PIC Z9.
           03  WD-KURS                 PIC ZZZ9,9999.
           03  WU-ADR                  PIC X(100).
           03  WU-KTO                  PIC 999999.
           03  WV-KTO                  PIC 999999.
      *-----------------------------------------> Konditionentabelle <-
           03  WH-KK                   PIC 9(13).
           03  WR-KK REDEFINES WH-KK.
               05  WH-SK1              PIC 9V9.
               05  WH-TG1              PIC 999.
               05  WH-SK2              PIC 9V9.
               05  WH-TG2              PIC 999.
               05  WH-TGN              PIC 999.
           03  WX-ZEI                  PIC X(57).
           03  WH-BET                  PIC S9(9)V99  COMP.
           03  WR-BET                  PIC S9(9)     COMP.
           03  WS-BET                  PIC S9(9)V99  COMP.
           03  WZ-BET                  PIC S9(9)V99  COMP.
           03  WZ-SKTO                 PIC S9(9)V99  COMP.
           03  WR-REST                 PIC S9(9)V99  COMP.
           03  WH-OLD                  PIC S9(9)V99  COMP.
       COPY FIBUEXT.CPY.
       COPY FIBUDECL.CPY.
       DECL-P SECTION.         USE AFTER ERROR PROCEDURE ON ZAHLPOS.
       A.  CALL "CADECL" USING "FIBUZAHL.DAT" WH-CREG.
       DECL-Y SECTION.         USE AFTER ERROR PROCEDURE ON DRUCKER.
       A.  CALL "CADECL" USING "1DRUCKER    " WH-CREG.
       Z.  EXIT.
       END DECLARATIVES.
      *****************************************************************
       STEUER SECTION.
       A.  MOVE WL-CALL TO WH-CALL.
           MOVE WL-CREG TO WH-CREG.
           IF WL-CA = 88 PERFORM RE-SUCHE GO X.
           OPEN I-O ZAHLPOS.
           PERFORM ZAHLWAHL.
           CLOSE ZAHLPOS.
       X.  MOVE WH-CREG TO WL-CREG.
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
           READ KONSTANT WITH NO LOCK not INVALID GO X.
           IF ZUGRIF PERFORM BESETZT GO A.
       X.  MOVE KOP-TX TO WK-DSTG.
       Z.  EXIT.
      ******************************************************************
       BS-ZEIL SECTION.
       A.  ADD 1 TO IX.
           IF IX < 17 GO Z.
       B.  CALL "CAUP" USING "1406012480" WH-CREG.
           PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 15
               MOVE WT-KEY(IX + 1) TO WT-KEY(IX).
           MOVE LOW-VALUE TO WT-KEY(IX).
       Z.  EXIT.
      ******************************************************************
       HOL-EURO SECTION.
       A.  MOVE 80 TO WH-KEY.
           READ KONSTANT IGNORE LOCK INVALID STOP RUN.
           MOVE KW-FRWTAB TO WE-FRWTAB.
           IF WM-DATUM < WE-EURO NEXT SENTENCE
           else MOVE WF-SYM(2) TO WF-SYM(1)
                MOVE WF-KURS(2) TO WF-KURS(1)
                MOVE "Ats" TO WF-SYM(2)          *> Ats = 1.Fremdwhg.
                MOVE 100 TO WF-KURS(2)
                PERFORM VARYING WX FROM 20 BY -1 UNTIL WX = 1
                   IF WF-SYM(WX) not = SPACE
                   COMPUTE WF-KURS(WX) rounded =
                       WF-KURS(WX) / 13,7602999
                end-perform
                MOVE 100 TO WF-KURS(1).            *> Eur = HauswÑhrung
           PERFORM EURO-ATS.
       Z.  EXIT.
      ******************************************************************
       EURO-ATS SECTION.
       A.  IF WF-SYM(1) = "Ats"
                DISPLAY "Ats" with highlight foreground-color 2 AT 0177
           else DISPLAY "Eur" with highlight foreground-color 6 AT 0177.
       Z.  EXIT.
      ******************************************************************
       ZAHLWAHL SECTION.
       A.  MOVE "   Zahlungsverkehr" TO WK-GEB.
           CALL "CAUP" USING "06KOPF" WH-CREG.
           PERFORM LADE-DRU.
           PERFORM HOL-EURO.
           CALL "CAUP" USING "0705191745000" WH-CREG.
           MOVE 0 TO WR.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Zahlungsverkehr " with highlight AT VDU-LP.
       C.  ADD 305 VDU-ECK GIVING VDU-LP.
           DISPLAY "Programmwahl" AT VDU-LP
                    ALL "ƒ" with SIZE 12     AT 0024
                   "1 - Zahlungsfreigabe"    AT 0024
                   "2 - öberweisung"         AT 0024
                   "3 - OP-Wartung"          AT 0024
                   "4 - OP-Liste f. Zahlung" AT 0024
                   " "                       AT 0024
                   "5 - Zahlungseingang"     AT 0024
                   "6 - Bankeinzug"          AT 0024
                   "7 - OP-Liste f. Einzug"  AT 0024
                   "8 - DTT-Diskette"        AT 0024
                   " "                       AT 0024
                   "bitte wÑhlen Sie: "  with highlight AT 0040.
       G.  DISPLAY "<esc>= Abbruch, <ret-leer>= Programmende" AT 2301.
           CALL "CAUP" USING "1015402002" WH-CREG.
           IF HELP CALL "CAUP" USING "190017001801" WH-CREG GO G.
           IF ESC MOVE 0 TO WH-NUM.
           MOVE WH-NUM TO WH-PG.
           IF WH-PG > 7 GO G.
           CALL "CAUP" USING "08CLOFEN" WH-CREG.
           EVALUATE WH-PG
               WHEN 0 GO Z
               WHEN 1 PERFORM RE-ERF
               WHEN 2 MOVE "FIBUEWS 10UEWSG" TO WT-TX
                      MOVE 49 TO WH-PG GO Z
               WHEN 3 MOVE 10 TO WL-CA
                      PERFORM RE-ERF
               WHEN 4 PERFORM OP-LIST
               WHEN 5 MOVE "FIBUEWS 70DTTCP" TO WT-TX
                      MOVE 49 TO WH-PG GO Z
               WHEN 6 MOVE "FIBUEWS 60EINZG" TO WT-TX
                      MOVE 49 TO WH-PG GO Z
               WHEN 7 PERFORM OP-LIST-B
               WHEN 5 MOVE "FIBUEWS 70DTTCP" TO WT-TX
                      MOVE 49 TO WH-PG GO Z.
           GO A.
       Z.  EXIT.
      ******************************************************************
       LAST-OP SECTION.
       A.  MOVE WK-KTONR TO OP-KTONR WD-KZ.
           MOVE WD-KZ TO OP-DK.
           IF WS = 0 MOVE WH-OPDAT TO OP-VALDAT
                  ELSE MOVE 99999999 TO OP-VALDAT.
           MOVE 9999 TO OP-LFD.
           START OPDATEI KEY < OP-KEY INVALID GO Z.
       B.  READ OPDATEI PREVIOUS NO LOCK AT END GO Z.
           IF ZUGRIF PERFORM BESETZT GO B.
       Z.  EXIT.
      ******************************************************************
       FIRST-OP SECTION.
       A.  MOVE WD-KZ TO OP-DK.
           MOVE KR-KTONR TO OP-KTONR.
           START OPDATEI KEY not < OP-KEY INVALID
                 MOVE 0 TO OP-KTONR GO E.
       C.  READ OPDATEI NEXT AT END MOVE 0 TO OP-KTONR.
           IF ZUGRIF PERFORM BESETZT GO C.
       E.  IF OP-KTONR not = KR-KTONR SET ESC TO TRUE GO Z.
       Z.  EXIT.
      *********************************** Auswahl der öberweisungsbank *
       BANKWAHL SECTION.
       A.  CALL "CAUP" USING "1303012480" WH-CREG.
           DISPLAY " Zahlung per:" with reverse-video AT 0358.
           MOVE WM-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           DISPLAY VDU-DATUM with highlight AT 0372.
           MOVE WM-DATUM TO WZ-DATUM WH-WERT.
       C.  DISPLAY "<esc>= Abbruch, <ret>= DurchfÅhrungsdatum" AT 2301.
           DISPLAY "<Entf>= lîchen nicht durchgefÅhrter öberweisungen"
               AT 2401.
           MOVE 0 TO WH-MOD.
           CALL "CAUP" USING "0103726006" WH-CREG.
           IF ESC GO Z.
           IF not RET and not ENTF GO A.
           IF WZ-DATUM = 0 GO A.
           IF not ENTF and WZ-DATUM < WH-DATUM GO A.
           MOVE WX-DATUM TO WM-DATUM.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           IF ENTF PERFORM LOE-UEWS GO Z.
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
           MOVE WH-NUM TO WD-TGN.
           DISPLAY WD-TGN with highlight AT VDU-LP.
           ADD WH-NUM 70 GIVING WH-KEY.
       D.  READ KONSTANT INVALID PERFORM NO-REC GO E.
           IF ZUGRIF PERFORM BESETZT GO D.
       Z.  EXIT.
      ************************************************ Bank einblenden *
       BANK-ANZ SECTION.
       A.  CALL "CAUP" USING "1303010357" WH-CREG.
           CALL "CAUP" USING "1304012480" WH-CREG.
           DISPLAY "Bank:" AT 0401.
           DISPLAY KB-KTONR AT 0407.
           DISPLAY KB-BEZ AT 0423.
           PERFORM ZAHLSUM.
           MOVE WS-BET TO WD-BET.
           DISPLAY WM-FSYM with highlight foreground-color 6 AT 0453
                   WD-BET AT 0000.
       Z.  EXIT.
      ************************************ lîschen alter öberweisungen *
       LOE-UEWS SECTION.
       A.  CALL "CAUP" USING "0707100360000" WH-CREG.
           ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "Lîschung nicht durchfÅhrter öberweisung bis: "
               AT VDU-LP VDU-DATUM with highlight.
           DISPLAY "<esc>= Abbruch, <Entf>= lîschen < >" AT 2301.
       B.  CALL "CAUP" USING "0023340000" WH-CREG.
           IF ESC GO X.
           IF not ENTF GO B.
           MOVE LOW-VALUE TO ZP-SATZ.
           START ZAHLPOS KEY not < ZP-AKEY INVALID GO X.
       C.  READ ZAHLPOS NEXT AT END GO X.
           IF ZUGRIF PERFORM BESETZT GO C.
           IF ZP-DATUM > WM-DATUM GO C.
       E.  DELETE ZAHLPOS INVALID GO C.
           IF ZUGRIF PERFORM BESETZT GO E.
           GO C.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           SET ESC TO TRUE.
       Z.  EXIT.
      ******************************************************* op-Suche *
       RE-SUCHE SECTION.
       A.  CALL "CAUP" USING "271701048001114" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Suche nach Beleg-Nr.:          " AT VDU-LP.
           MOVE WH-NUM TO WD-BELNR.
           ADD 126 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-BELNR with highlight AT VDU-LP.
           ADD 202 VDU-ECK GIVING VDU-LP.
           DISPLAY " Z Kto.Nr.     Datum      Betrag        offen  Bezei
      -       "chnung                    " with reverse-video AT VDU-LP.
           MOVE HIGH-VALUES TO OP-SATZ.
           MOVE WH-NUM TO OP-RENUM.
           SET ESC TO TRUE.
           MOVE 0 TO WZ.
      *    START OPDATEI KEY < OP-RKEY INVALID GO X.
       C.  READ OPDATEI NEXT IGNORE LOCK AT END GO X.
           IF OP-RENUM not = WH-NUM GO G.
           PERFORM OP-DIS.
           GO C.
       G.  DISPLAY "<ESC>= Abbruch, <ret>= Daten ok < >" AT 2301.
           CALL "CAUP" USING "0023340000" WH-CREG.
           IF not RET and not ESC GO G.
           MOVE WT-KEY(1) TO OP-KEY.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ******************************************************************
       OP-DIS SECTION.
       A.  COMPUTE VDU-LP = WZ * 100 + 304 + VDU-ECK.
           COMPUTE WD-KTO = OP-KTONR / 10.
           MOVE OP-KTONR TO DE-KTONR.
           READ DEBITOR IGNORE LOCK INVALID
               MOVE "-- Kunde fehlt --" TO DE-BEZ.
           DISPLAY WD-KTO with highlight AT VDU-LP.
           ADD 10 TO VDU-LP.
           MOVE OP-REDAT TO WC-DATUM.
           PERFORM DATDREH
           DISPLAY VDU-DATUM AT VDU-LP.
           MOVE OP-REBETRAG TO WD-BET.
           ADD 10 TO VDU-LP.
           DISPLAY WD-BET(4:) with highlight AT VDU-LP.
           ADD 13 TO VDU-LP.
           COMPUTE WD-BET WH-WERT = OP-REBETRAG - OP-ZABETRAG.
           DISPLAY WD-BET(4:) with highlight AT VDU-LP.
           ADD 12 TO VDU-LP.
           MOVE DE-BEZ TO WT-TX.
           INSPECT WT-TX REPLACING ALL "#" BY ",".
           DISPLAY WT-TX(1:31) with highlight AT VDU-LP.
           ADD 1 TO WR WZ.
           MOVE OP-KEY TO WT-KEY(WZ).
       Z.  EXIT.
      *********************************************** Bearbeitung OP`s *
       RE-ERF SECTION.
       A.  IF WH-PG = 1 MOVE "  Freigabe zur Zahlung" TO WK-GEB
                   ELSE MOVE "  Bearbeiten OP's" TO WK-GEB.
           MOVE WH-DATUM TO WZ-DATUM.
           CALL "CAUP" USING "05DATPRF" WH-CREG.
           MOVE WX-DATUM TO WM-DATUM.
       B.  CALL "CAUP" USING "06KOPF" WH-CREG.
           PERFORM EURO-ATS.
           IF WH-PG = 1 PERFORM BANKWAHL
               IF ESC GO Z.
       C.  IF WH-PG = 1 PERFORM BANK-ANZ
           else CALL "CAUP" USING "1303012480" WH-CREG.
       E.  DISPLAY "Kto.Nr: " AT 0301.
           UNLOCK KREDIT.
           MOVE 0 TO WH-NUM WH-MOD.
           DISPLAY "<esc>= Ende, <ret>= Kto.Nr., <alpha+ret>= suchen"
               AT 2301.
           IF WH-PG = 1 MOVE 40 TO WL-CA
               DISPLAY ", <#>= letzter Zahlungsemfg." AT 0000
               DISPLAY "ab Lieferanten-Nr." with highlight AT 2401.
           DISPLAY "<Bild- >= Belegsuche Debitroen" AT 2501.
           MOVE 0 TO WH-WERT IX.
           CALL "CAUP" USING "0003096010" WH-CREG.
           IF HELP CALL "CAUP" USING "190019002001" WH-CREG GO B.
           IF BAB PERFORM RE-SUCHE GO E.
           IF KIST AND WH-PG = 1 MOVE KR-KTONR TO WH-NUM GO G.
           IF ESC OR WOLI GO Z.
           IF WH-WERT = 2 OR 3 CALL "FIBANZ" USING "99SUCH" WH-CREG
               CANCEL "FIBANZ".
           IF WH-NUM = 0 and WH-MCODE = SPACE;
               IF WH-PG = 1 MOVE 3 TO WH-NUM else GO C.
           MOVE WH-NUM TO WD-KZ.
           IF WD-KZ not = 3 AND not = 2 GO C.
           IF WH-NUM > 3 MOVE 0 TO OP-VALDAT OP-LFD
                         GO G.
           IF WH-PG = 1 AND WD-KZ not = 3 GO C.
           IF WH-PG = 1 PERFORM FREIGAB
              MOVE KR-KTONR TO WH-NUM
              IF KR-KTONR = 0 GO C.
       G.  PERFORM LIESKTO.
           IF FEHLER GO C.
           IF WH-PG = 1 AND WD-KZ = 3 PERFORM OB-FEHLT.
           CALL "CAUP" USING "1303080357" WH-CREG.
           CALL "CAUP" USING "1304012580" WH-CREG.
           DISPLAY WD-KTO AT 0309.
           MOVE WT-TX TO WX-ZEI.
           INSPECT WX-ZEI REPLACING ALL "#" BY ",".
           DISPLAY WX-ZEI with SIZE 40 highlight AT 0318.
           IF WH-PG = 1 DISPLAY "Bank: " AT 0401
               DISPLAY KR-BANKKTO AT 0407
               DISPLAY KR-BANKLTZ AT 0422
               DISPLAY KR-BANK    AT 0430
               IF KR-BANKKTO(1:1)= "*" PERFORM READ-UEWGADR.
           IF WH-PG not = 3;
               IF KR-BANKKTO = SPACE OR KR-BANKLTZ = SPACE GO R.
           MOVE 4 TO VDU-L.
           PERFORM KONDANZ.
       M.  DISPLAY " Re-Datum Rechngs.-Nr.   Re-Betrag     Za-Betrag
      -        "%  Tg  %  Tg   Nt.  fÑllig " with reverse-video AT 0501.
           MOVE 5 TO WH-L.
           MOVE 6 TO VDU-L.
           MOVE LOW-VALUE TO WT-ERFTAB.
           MOVE 9 TO WS.
           MOVE 1 TO WH-MOD.
           IF WL-CA = 10 PERFORM LAST-OP.
           IF WL-CA = 33 MOVE 0 TO WS
               PERFORM LAST-OP.
      *    IF WL-CA = 40 PERFORM FIRST-OP.
           IF WL-CA = 40 PERFORM LAST-OP.
           IF OP-KTONR = WK-KTONR AND OP-DK = WD-KZ MOVE 1 TO IX
               PERFORM OP-ANZ
           else INITIALIZE OP-SATZ.
           MOVE 2 TO WH-MOD.
           IF WL-CA = 40 PERFORM AUFZEIL
               PERFORM ANWEIS
               EVALUATE TRUE
                   WHEN WOLI CALL "CAUP" USING "1303012480" WH-CREG
                             GO E
                   WHEN KIST
                   WHEN TABL GO R
                   WHEN OTHER GO C.
           PERFORM ERF-OP.
           IF WL-CA = 33 GO Z.
           IF KIST OR ESC GO B.
           IF WL-CA not = 40 GO B.
       Q.  IF WH-MOD = 8 GO Z.
           SET RET TO TRUE.
           READ OPDATEI NEXT AT END GO B.
           IF ZUGRIF PERFORM BESETZT GO Q.
       R.  IF OP-KTONR = WK-KTONR; IF FINE PERFORM NO-REC GO B
                                   ELSE GO Q.
           MOVE OP-KTONR TO WK-KTONR WH-NUM.
           CALL "CAUP" USING "1306012580" WH-CREG.
           GO G.
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
           PERFORM OB-FEHLT.
           MOVE WV-KTO TO KR-KTONR.
           READ KREDIT NO LOCK.
       Z.  EXIT.
      ******************************************************************
       OB-FEHLT SECTION.
       A.  IF KR-BANKKTO = SPACE OR KR-BANKLTZ = SPACE
               DISPLAY "ACHTUNG!" with highlight AT 2301
               DISPLAY "Bank-Ktonr. oder Bank-Lzl. fehlen" at 2401
               PERFORM WEITER
               CALL "CAUP" USING "1323012580" WH-CREG.
       Z.  EXIT.
      ******************************************************************
       LIESKTO SECTION.
       A.  SET RET TO TRUE.
           IF WH-NUM = 0 GO X.
           DIVIDE 10 INTO WH-NUM GIVING WD-KTO.
           MOVE WH-NUM TO WD-KZ.
           INITIALIZE KR-SATZ DE-SATZ.
           EVALUATE WD-KZ
               WHEN 3 MOVE WH-NUM TO KR-KTONR GO G
               WHEN 2 MOVE WH-NUM TO DE-KTONR GO E
               WHEN OTHER GO X.
      *---------------------------------------------> lesen Debitoren <-
       E.  READ DEBITOR WITH NO LOCK not INVALID MOVE DE-BEZ TO WT-TX
               INSPECT WT-TX REPLACING ALL "#" BY ","
               MOVE DE-SOLL TO WE-SOLL
               MOVE DE-HABEN TO WE-HABEN
               MOVE DE-KTONR TO WH-NUM
               MOVE DE-KOND TO WH-KK
               MOVE DE-KTONR TO WK-KTONR
               SUBTRACT WE-HABEN FROM WE-SOLL GIVING WE-SALDO
               PERFORM DEB-BANK
               IF WH-PG > 2 GO Z
               else IF BK-KTONR not = 0 GO Z.
           IF ZUGRIF PERFORM BESETZT
                     PERFORM WEITER GO X.
           MOVE 0 TO DE-KTONR WK-KTONR.
           IF DE-KTONR = 0 GO X ELSE GO E.
      *--------------------------------------------> lesen Kreditoren <-
       G.  READ KREDIT WITH NO LOCK not INVALID MOVE KR-BEZ TO WT-TX
               MOVE KR-SOLL TO WE-SOLL
               MOVE KR-HABEN TO WE-HABEN
               MOVE KR-KTONR TO WH-NUM
               MOVE KR-KOND TO WH-KK
               MOVE KR-KTONR TO WK-KTONR
               SUBTRACT WE-SOLL FROM WE-HABEN GIVING WE-SALDO GO Z.
           IF ZUGRIF PERFORM BESETZT
                     PERFORM WEITER GO X.
           MOVE 0 TO KR-KTONR WK-KTONR.
           IF KR-KTONR not = 0 GO G.
       X.  SET FEHLER TO TRUE.
       Z.  EXIT.
      ******************************************************************
       DEB-BANK SECTION.
       A.  OPEN INPUT BANKDAT.
           MOVE DE-KTONR TO BK-KTONR WD-KZ.
           MOVE WD-KZ TO BK-DK.
           READ BANKDAT INVALID MOVE LOW-VALUES TO BK-SATZ.
           MOVE BK-BANKKTO TO KR-BANKKTO.
           MOVE BK-BANKLTZ TO KR-BANKLTZ.
           MOVE BK-BANK TO KR-BANK.
           CLOSE BANKDAT.
       Z.  EXIT.
      ******************************************************************
       FREIGAB SECTION.
       A.  MOVE WH-NUM TO OP-KTONR.
           MOVE 3 TO OP-DK.
           MOVE 0 TO OP-VALDAT KR-KTONR OP-LFD.
           START OPDATEI KEY not < OP-KEY INVALID GO Z.
       C.  READ OPDATEI NEXT AT END GO Z.
           IF ZUGRIF PERFORM BESETZT GO C.
           IF OP-REBETRAG = OP-ZABETRAG GO C.
           MOVE OP-KTONR TO KR-KTONR.
           IF OP-DK not = 3 MOVE 0 TO KR-KTONR.
       Z.  EXIT.
      ******************************************* Summe offener Posten *
       OFFSUM SECTION.
       A.  CALL "CAUP" USING "0710100740000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Summe offener Posten " with highlight  AT VDU-LP.
           ADD 303 VDU-ECK GIVING VDU-LP.
           MOVE WE-SALDO TO WD-BET.
           DISPLAY "Kontosaldo: " AT VDU-LP WD-BET with highlight.
           ADD 403 VDU-ECK GIVING VDU-LP.
           DISPLAY "OP - Saldo: " AT VDU-LP.
           MOVE 0 TO WS-BET OP-VALDAT OP-LFD OP-SA WH-BET.
           MOVE WK-KTONR TO OP-KTONR WD-KZ.
           MOVE WD-KZ TO OP-DK.
           START OPDATEI KEY not < OP-AKEY INVALID GO X.
       C.  READ OPDATEI NEXT WITH NO LOCK AT END GO Q.
           IF ZUGRIF PERFORM BESETZT GO C.
           IF OP-KTONR not = WK-KTONR or OP-SA not = 0 GO Q.
           ADD 608 VDU-ECK GIVING VDU-LP.
           DISPLAY OP-RENUM AT VDU-LP.
           IF WM-DATUM > WE-EURO and OP-FSYM = "Ats"
               COMPUTE WH-BET ROUNDED =
                  (OP-REBETRAG - OP-ZABETRAG) / 13,7603
           else COMPUTE WH-BET ROUNDED =
                  (OP-REBETRAG - OP-ZABETRAG) * OP-KURS / 100.
      *    ADD OP-REBETRAG TO WS-BET.
      *    SUBTRACT OP-ZABETRAG FROM WS-BET.
           ADD WH-BET TO WS-BET.
           MOVE WS-BET TO WD-BET.
           ADD 415 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-BET with highlight AT VDU-LP.
           GO C.
       Q.  ADD 503 VDU-ECK GIVING VDU-LP.
           DISPLAY "Differenz:" AT VDU-LP.
           ADD 515 VDU-ECK GIVING VDU-LP.
           COMPUTE WD-BET = WE-SALDO - WS-BET.
           IF WS-BET < 0 DISPLAY WD-BET with highlight
                            foreground-color 4 AT VDU-LP
           else DISPLAY WD-BET with highlight AT VDU-LP.
           ADD 603 VDU-ECK GIVING VDU-LP.
           DISPLAY "Weiter mit beliebiger Taste - < >" AT VDU-LP.
           CALL "CAUP" USING "1006340000" WH-CREG.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ************************************************ Summe Zahlungen *
       ZAHLSUM SECTION.
       A.  IF WH-PG not = 2 CALL "CAUP" USING "0711100640000" WH-CREG.
               ADD 103  VDU-ECK GIVING VDU-LP
               DISPLAY " Summe Zahlungsfreigabe " with highlight
                   AT VDU-LP.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "Summe: " AT VDU-LP.
           MOVE 0 TO WS-BET ZP-DK ZP-KTONR ZP-VALDAT ZP-LFD.
           MOVE SPACE TO WM-FSYM.
           MOVE KB-NUM TO ZP-BAKEY.
           START ZAHLPOS KEY not < ZP-AKEY INVALID GO X.
       C.  READ ZAHLPOS NEXT WITH NO LOCK AT END GO Q.
           IF ZUGRIF PERFORM BESETZT GO C.
           IF ZP-BAKEY not = KB-NUM GO Q.
           IF ZP-DATUM not = WM-DATUM GO C.
           IF WH-PG not = 2 ADD 403 VDU-ECK GIVING VDU-LP
               DISPLAY ZP-EMPFG with SIZE 36 AT VDU-LP.
           ADD ZP-BETRAG TO WS-BET.
           MOVE WS-BET TO WD-BET.
           ADD 310 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-BET with highlight AT VDU-LP.
           IF WM-FSYM = SPACE MOVE ZP-FSYM TO WM-FSYM.
           GO C.
       Q.  IF WH-PG = 2 GO Z.
           IF WH-PG not = 1 GO X.
           ADD 403 VDU-ECK GIVING VDU-LP.
           DISPLAY "Weiter mit beliebiger Taste - < >" AT VDU-LP.
           CALL "CAUP" USING "1004340000" WH-CREG.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ************************************************** Zeile lîschen *
       LOEBEW SECTION.
       A.  CALL "CAUP" USING "0710080353000" WH-CREG.
           IF IX = 0 GO Q.
           MOVE WT-KEY(IX) TO OP-KEY WH-OPKEY.
           IF WH-OPKEY = LOW-VALUE GO Q.
       B.  ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "Wirklich lîschen?" with highlight AT VDU-LP
               " <esc>= Abbruch, <Entf>= ja! < >" AT 0000.
           CALL "CAUP" USING "1002500000" WH-CREG.
           IF not ENTF GO X.
       C.  DELETE OPDATEI INVALID GO X.
           IF ZUGRIF PERFORM BESETZT GO C.
           GO X.
       Q.  ADD 004 VDU-ECK GIVING VDU-LP.
           DISPLAY " Alle Op's zu diesem Konto werden gelîscht! "
               with reverse-video BLINK AT VDU-LP.
           ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "Wirklich lîschen?" with highlight AT VDU-LP
               " <esc>= Abbruch, <Entf>= ja! < >" AT 0000.
           CALL "CAUP" USING "1002500000" WH-CREG.
           IF not ENTF GO X.
           CALL "CAUP" USING "16CLRFEN" WH-CREG.
           ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "R-Nr.:" AT VDU-LP.
           ADD 7 TO VDU-LP.
           ADD -1 TO WH-L.
           MOVE WK-KTONR TO OP-KTONR WD-KZ.
           MOVE WD-KZ TO OP-DK.
           START OPDATEI KEY not < OP-KEY INVALID GO X.
       R.  READ OPDATEI NEXT WITH NO LOCK AT END GO X.
           IF ZUGRIF PERFORM BESETZT GO R.
           IF OP-KTONR not = WK-KTONR GO X.
           DISPLAY OP-RENUM AT VDU-LP.
       S.  DELETE OPDATEI INVALID GO R.
           IF ZUGRIF PERFORM BESETZT GO S.
           GO R.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           MOVE 6 TO VDU-L.
           PERFORM AUSGAB.
       Z.  EXIT.
      ***************************************** wenn BS-Zeile gelîscht *
       AUSGAB SECTION.
       A.  MOVE IX TO WE.
           MOVE 1 TO IX.
           MOVE WT-KEY(IX) TO OP-KEY.
           MOVE LOW-VALUE TO WT-ERFTAB.
           START OPDATEI KEY not < OP-KEY INVALID
               SET FINE TO TRUE GO X.
           CALL "CAUP" USING "1306012480" WH-CREG.
           SET BAB TO TRUE.
           MOVE 0 TO WI WR.
       E.  PERFORM NEXTAB WITH TEST AFTER UNTIL WI = 8 OR WR = 15.
           IF WI not = 8 MOVE 9 TO WI.
           MOVE WE TO IX.
       H.  IF IX = 0 GO X.
           IF WT-KEY(IX) = LOW-VALUE ADD -1 TO IX GO H.
       X.  IF FINE PERFORM NODATA.
       Z.  EXIT.
      **************************************************** weiterlesen *
       ABZEIL SECTION.
       A.  PERFORM VARYING IX FROM 16 BY -1 UNTIL IX = 0
               OR WT-KEY(IX) not = LOW-VALUE CONTINUE.
           IF IX = 0 MOVE 1 TO IX SET FINE TO TRUE GO X.
       C.  MOVE WT-KEY(IX) TO OP-KEY.
           IF AB OR (BAB AND WH-NUM = 0) PERFORM READBEW
               ADD 1 TO IX
               MOVE 0 TO WI WR
               IF AB MOVE 13 TO WR.
       E.  PERFORM NEXTAB WITH TEST AFTER UNTIL WI = 8 OR WR = 14.
           IF WI not = 8 MOVE 9 TO WI.
       X.  IF FINE PERFORM NODATA.
       Z.  EXIT.
      ******************************************************************
       NEXTAB SECTION.
       E.  READ OPDATEI NEXT AT END MOVE 8 TO WI WH-MOD GO Z.
           IF ZUGRIF PERFORM BESETZT GO E.
           IF WH-PG = 1 AND OP-REBETRAG = OP-ZABETRAG GO E.
           IF OP-KTONR not = WK-KTONR MOVE 8 TO WI
                SET FINE TO TRUE GO Z.
           IF WI < 3 PERFORM VARYING WF FROM 1 BY 1 UNTIL WF > 15
               MOVE OP-KEY TO WH-OPKEY
               IF WH-OPKEY = WT-KEY(WF) GO E.
           IF IX = 16 CALL "CAUP" USING "1406012280" WH-CREG
               PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 14
                   MOVE WT-KEY(IX + 1) TO WT-KEY(IX)
                   END-PERFORM.
           PERFORM OP-ANZ.
           IF BAB ADD 1 TO IX.
       Z.  EXIT.
      ******************************************************************
       AUFZEIL SECTION.
       A.  IF WT-KEY(1) = LOW-VALUE MOVE 0 TO WS
                   PERFORM LAST-OP
                   IF WK-KTONR not = OP-KTONR OR WD-KZ not = OP-DK
                       SET FINE TO TRUE GO X
                   else MOVE 99999999 TO OP-VALDAT
                       MOVE 9999 TO OP-LFD
                       MOVE OP-KEY TO WT-KEY(1).
           MOVE WT-KEY(1) TO WH-OPKEY OP-KEY.
           MOVE 0 TO WR.
           MOVE IX TO WX.
           IF AUF MOVE 13 TO WR.
           START OPDATEI KEY < OP-KEY INVALID SET FINE TO TRUE GO X.
           MOVE 1 TO IX WI.
           PERFORM NEXTAUF WITH TEST AFTER UNTIL WI = 2 OR WR = 14.
           MOVE 1 TO IX.
       X.  IF FINE PERFORM NODATA.
       Z.  EXIT.
      ******************************************************************
       NEXTAUF SECTION.
       N.  READ OPDATEI PREVIOUS AT END MOVE 2 TO WI GO Z.
           IF ZUGRIF PERFORM BESETZT GO N.
           IF WH-PG = 1 AND OP-REBETRAG = OP-ZABETRAG GO N.
           IF OP-KTONR not = WK-KTONR SET FINE TO TRUE
               MOVE 2 TO WI GO Z.
           IF WI > 8 PERFORM VARYING WF FROM 16 BY -1 UNTIL WF < 1
               MOVE OP-KEY TO WH-OPKEY
               IF WH-OPKEY = WT-KEY(WF) GO N.
           MOVE VDU-LP TO VDU-AB.
           MOVE 1500 TO VDU-LP.
           CALL "CAUP" USING "18CLRWIN" WH-CREG.
           MOVE VDU-AB TO VDU-LP.
           CALL "CAUP" USING "2206012280SCR" WH-CREG.
           PERFORM VARYING IX FROM 15 BY -1 UNTIL IX < 2
              MOVE WT-KEY(IX - 1) TO WT-KEY(IX).
           COMPUTE VDU-LP = IX * 100 + 500.
           PERFORM OP-ANZ.
       Z.  EXIT.
      ******************************************************************
       NODATA SECTION.
       A.  DISPLAY "* Datenende *" with highlight blink AT 2468.
           SET FINE TO TRUE.
       Z.  EXIT.
      ******************************************************************
       READBEW SECTION.
       A.  MOVE WH-OPKEY TO OP-KEY.
       C.  READ OPDATEI KEY OP-KEY INVALID MOVE LOW-VALUE TO WT-ERFTAB.
           IF ZUGRIF PERFORM BESETZT GO C.
       Z.  EXIT.
      ******************************************* Einzelzeile anzeigen *
       ANZEIG SECTION.
       A.  SET RET TO TRUE.
           IF IX = 0 GO X.
           COMPUTE VDU-LP = IX * 100 + 519.
           IF WT-KEY(IX) = LOW-VALUE INITIALIZE OP-SATZ
               CALL "CAUP" USING "1300012280" WH-CREG
               MOVE 0 TO WS GO X.
           IF ESC CALL "CAUP" USING "1300010080" WH-CREG.
           MOVE 1 TO WS.
           MOVE WT-KEY(IX) TO OP-KEY.
       C.  READ OPDATEI KEY IS OP-KEY INVALID PERFORM NO-REC
               SET ESC TO TRUE GO Z.
           IF ZUGRIF PERFORM BESETZT GO C.
           PERFORM OP-ANZ.
       X.  MOVE VDU-L TO WH-L.
       Z.  EXIT.
      *********************************************** Zahlungsfreigabe *
       ANWEIS SECTION.
       A.  PERFORM VARYING IX FROM 16 BY -1 UNTIL IX = 0
               OR WT-KEY(IX) not = LOW-VALUE
               END-PERFORM.
       F.  PERFORM ANZEIG.
           MOVE OP-DK TO ZP-DK.
           MOVE OP-KTONR TO ZP-KTONR.
           MOVE OP-VALDAT TO ZP-VALDAT.
           MOVE OP-LFD TO ZP-LFD.
           READ ZAHLPOS INVALID MOVE ZP-KEY TO WT-TX
               INITIALIZE ZP-SATZ
               MOVE WT-TX TO ZP-KEY.
           IF IX = 0 PERFORM NO-REC GO Z.
       L.  DISPLAY "<esc>= Ende, <#>= Ende Lieft., <Einfg>= Summe OP's,
      -        "<ret>= Anweisung" AT 2301.
           DISPLAY "< >= weiter, < >= zurÅck, < >= KontoÑndg., <Ende>= S
      -        "umme, <>= zur Kto. Nr." AT 2401.
           CALL "CAUP" USING "0000360000" WH-CREG.
           IF HELP CALL "CAUP" USING "190021002201" WH-CREG GO L.
           IF ENDE PERFORM ZAHLSUM GO F.
           IF KIST OR ESC GO Z.
           IF APOS MOVE 1 TO IX GO F.
           IF XPOS GO A.
           IF WOLI GO Z.
           IF WORE CALL "FIBKRED" USING "41AENDG" WH-CREG
                   CANCEL "FIBKRED" GO L.
           IF BAUF PERFORM AUFZEIL GO F.
           IF AUF; IF IX > 1 SUBTRACT 1 FROM IX GO F
                   ELSE PERFORM AUFZEIL
                        IF FINE SET TABL TO TRUE GO Z
                        ELSE GO F.
           IF BAB PERFORM ABZEIL
                  SUBTRACT 1 FROM IX GO F.
           IF AB; IF WT-KEY(IX + 1) not = LOW-VALUE
                     ADD 1 TO IX GO F
                  ELSE PERFORM ABZEIL
                       IF FINE SET KIST TO TRUE GO Z
                       ELSE GO F.
           MOVE 0 TO WI WE.
           IF EINF MOVE KR-KTONR TO WK-KTONR
                   PERFORM OFFSUM GO A.
           IF not RET AND not WORE GO A.
           MOVE VDU-L TO WH-L.
           PERFORM ERMITTLE.
           IF WOLI OR ENDE OR ESC GO L.
           MOVE WH-L TO VDU-L.
      *----------------------------------------------> Zahlungsbetrag <-
       M.  MOVE WZ-SKTO TO ZP-SKTO OP-ZPSKTO.
           MOVE WZ-BET TO ZP-BETRAG OP-ZPBET.
           COMPUTE WD-BET = OP-ZABETRAG + OP-ZPBET + OP-ZPSKTO.
           MOVE 36 TO VDU-P.
           IF OP-ZPBET = 0 DISPLAY WD-BET with highlight AT VDU-LP
           else DISPLAY WD-BET with highlight foreground-color 3
                    AT VDU-LP.
           MOVE KB-NUM TO ZP-BAKEY.
           IF ZP-DATUM = 0 MOVE WM-DATUM TO ZP-DATUM.
           IF OP-ZPBET = 0 MOVE 0 TO ZP-DATUM.
           MOVE ZP-DATUM TO OP-MAHNDAT.                 *> Planungsdatum
           MOVE KB-KTONR TO ZP-BAKTO.
           MOVE KB-BEZ TO ZP-BABEZ.
           REWRITE OP-SATZ.
           MOVE OP-FSYM TO ZP-FSYM.
           IF ZP-SKTO not = 0 OR ZP-BETRAG not = 0 GO Q.
           DELETE ZAHLPOS INVALID GO W.
           GO W.
       Q.  REWRITE ZP-SATZ INVALID WRITE ZP-SATZ.
       W.  SET AB TO TRUE.
           GO L.
       Z.  EXIT.
      ******************************** Ermittlung des Zahlungsbetrages *
       ERMITTLE SECTION.
       A.  CALL "CAUP" USING "0715010871000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Zahlungsermittlung " with highlight AT VDU-LP.
           MOVE 0 TO WZ-SKTO WZ-BET.
           MOVE WH-SK1 TO WZ-SKTO.
           COMPUTE WR-REST = OP-REBETRAG - OP-ZABETRAG.
240208*    IF OP-REBETRAG < 0 MOVE 0 TO WR-REST.
           MOVE ZP-BETRAG TO WH-OLD.
       C.  CALL "CAUP" USING "16CLRFEN" WH-CREG.
           ADD 207 VDU-ECK GIVING VDU-LP.
           DISPLAY "Re-Betrag:" AT VDU-LP.
           ADD 308 VDU-ECK GIVING VDU-LP.
           DISPLAY "% Skonto:" AT VDU-LP.
           ADD 409 VDU-ECK GIVING VDU-LP.
           DISPLAY "zahlbar:" AT VDU-LP.
           ADD 503 VDU-ECK GIVING VDU-LP.
           DISPLAY "Teilzahlungen:" AT VDU-LP.
           ADD 604 VDU-ECK GIVING VDU-LP.
           DISPLAY "Zahlungsrest:" AT VDU-LP.
           ADD 634 VDU-ECK GIVING VDU-LP.
           DISPLAY "Zahlbetrag:" AT VDU-LP.
           ADD 702 VDU-ECK GIVING VDU-LP.
           DISPLAY "Verwendungszweck:" AT VDU-LP.
           IF OP-SKTOBAS = 0 MOVE OP-REBETRAG TO OP-SKTOBAS.
      *    IF OP-SKTOBAS < 0 MOVE 0 TO OP-SKTOBAS.
           IF ZP-BETRAG not = 0 ADD 234 VDU-ECK GIVING VDU-LP
               DISPLAY "Zahlung: " AT VDU-LP
               IF ZP-BAKEY > 800 DISPLAY "bereits gedruckt"
                       with highlight BLINK AT 0000
                   else DISPLAY "bereits erfa·t" AT 0000
               end-if
               ADD 334 VDU-ECK GIVING VDU-LP
               IF ZP-BAKEY not = KB-NUM
                   DISPLAY ZP-BABEZ with BLINK AT VDU-LP
                   ADD 434 VDU-ECK GIVING VDU-LP
                   DISPLAY ZP-BAKTO with BLINK AT VDU-LP
               else DISPLAY ZP-BABEZ AT VDU-LP
                   ADD 434 VDU-ECK GIVING VDU-LP
                   DISPLAY ZP-BAKTO AT VDU-LP
               end-if
               ADD 446 VDU-ECK GIVING VDU-LP
               MOVE ZP-DATUM TO WC-DATUM
               PERFORM DATDREH
               MOVE ZP-BETRAG TO WD-BET
               DISPLAY WD-BET AT VDU-LP " " VDU-DATUM.
           ADD 218 VDU-ECK GIVING VDU-LP.
           MOVE OP-REBETRAG TO WD-BET.
           DISPLAY WD-BET with highlight AT VDU-LP.
           MOVE WH-SK1 TO WD-SKTO WD-SK.
           COMPUTE WZ-SKTO ROUNDED = OP-SKTOBAS * WZ-SKTO / 100.
      *----------------------------------------> runden nur bei "Ats" <-
           IF OP-FSYM = "Ats" COMPUTE WR-BET = OP-REBETRAG - WZ-SKTO
               COMPUTE WH-WERT = OP-REBETRAG - WR-BET - WZ-SKTO
               IF WZ-SKTO not = 0 ADD WH-WERT TO WZ-SKTO
                             else MOVE 0 TO WZ-SKTO.
240408*    IF WR-REST < WZ-SKTO MOVE WR-REST TO WZ-SKTO.
           IF WH-OLD not = 0 MOVE ZP-SKTO TO WZ-SKTO.
240408*    IF WH-SK1 = 0 MOVE 0 TO WZ-SKTO.
       D.  DIVIDE 100 INTO OP-REBETRAG GIVING WH-WERT.
           COMPUTE WD-SKTO = WZ-SKTO / WH-WERT.
           ADD 304 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-SKTO with highlight AT VDU-LP.
           MOVE WZ-SKTO TO WD-BET.
           ADD 318 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-BET with highlight AT VDU-LP.
           SUBTRACT WZ-SKTO FROM OP-REBETRAG GIVING WH-WERT.
           MOVE WH-WERT TO WD-BET.
           ADD 418 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-BET with highlight AT VDU-LP.
           MOVE OP-ZABETRAG TO WD-BET.
           ADD 518 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-BET with highlight AT VDU-LP.
           COMPUTE WH-WERT = WH-WERT - OP-ZABETRAG.
           MOVE WH-WERT TO WD-BET.
      *    IF WH-OLD not = 0 MOVE WH-OLD TO WH-WERT.
           ADD 618 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-BET with highlight AT VDU-LP.
           MOVE WH-WERT TO WD-BET WS-BET WZ-BET.
           IF KIST MOVE WH-BET TO WH-WERT WS-BET WZ-BET WD-BET.
           PERFORM VERWEND.
           ADD 720 VDU-ECK GIVING VDU-LP.
           DISPLAY ZP-VERWEND with SIZE 40 highlight AT VDU-LP.
       F.  ADD 646 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-BET with highlight AT VDU-LP.
           DISPLAY " " AT 0000
               OP-FSYM with highlight foreground-color 6.
           IF WM-FSYM not = SPACE and WM-FSYM not = OP-FSYM
               DISPLAY "WÑhrungsfehler" with highlight AT 2401
               PERFORM WEITER
               SET ESC TO TRUE
               GO X.
       G.  DISPLAY "<esc>= Abbruch, <ret-leer>= Betrag ok!, <#>= Teilzah
      -        "lung, <Entf>= Zahlg. lîschen" AT 2301.
           DISPLAY "< >= Verwendungszweck, <ret>= Betrag (Skto-Diff.), <
      -        " >= Skto-%, <>= zurÅck " AT 2401.
           CALL "CAUP" USING "1006468212" WH-CREG.
           IF HELP CALL "CAUP" USING "190023002401" WH-CREG GO G.
           IF WOLI OR ESC OR ENDE GO X.
           IF AUF; IF WH-WERT > 9,99 GO G;
                   else MOVE WH-WERT TO WZ-SKTO WH-SK1
                        IF OP-SKTOBAS = 0 MOVE OP-REBETRAG TO OP-SKTOBAS
                        end-if GO C.
           IF ENTF MOVE 0 TO WZ-BET WZ-SKTO
                   MOVE 0 TO ZP-SKTO ZP-BETRAG
                   SET RET TO TRUE GO X.
           IF KIST MOVE WH-WERT TO WH-BET
                   MOVE 0 TO ZP-SKTO ZP-BETRAG
                   MOVE 0 TO WZ-SKTO GO D.
           IF RET AND WH-NUM = 0
                   MOVE 0 TO ZP-SKTO ZP-BETRAG
                   GO X.
           IF AB GO K.
           IF not RET GO G.
           SUBTRACT WH-WERT FROM WS-BET.
           ADD WS-BET TO WZ-SKTO.
           MOVE WH-WERT TO WD-BET WS-BET.
           GO D.
      *--------------------------------------------> Verwendungszweck <-
       K.  IF KB-ART not = 2 GO M.
           MOVE ZP-VERWEND TO WT-TX.
           ADD 720 VDU-ECK GIVING VDU-LP.
           DISPLAY ZP-VERWEND with SIZE 40 highlight AT VDU-LP.
           CALL "CAUP" USING "1207200140" WH-CREG.
           IF ESC GO G.
           IF not RET GO K.
           MOVE WT-TX TO ZP-VERWEND.
           DISPLAY ZP-VERWEND with SIZE 40 highlight AT VDU-LP.
           GO G.
      *---------------------------> Verwendungszweck neue öberweisung <-
       M.  MOVE ZP-VERWEND(1:21) TO WT-TX.
           ADD 720 VDU-ECK GIVING VDU-LP.
           DISPLAY ZP-VERWEND(1:21) with highlight AT VDU-LP
               "Æ Ø" ZP-VERWEND(22:21) with highlight "Æ".
           DISPLAY "<esc>= Abbruch" AT 2301.
           DISPLAY "Text 1. Zeile" AT 2401.
           CALL "CAUP" USING "1207200121" WH-CREG.
           IF ESC GO G.
           IF not RET GO K.
           MOVE WT-TX(1:21) TO ZP-VERWEND(1:21).
           DISPLAY WT-TX(1:21) with highlight AT VDU-LP.
       N.  MOVE ZP-VERWEND(22:21) TO WT-TX.
           ADD 720 VDU-ECK GIVING VDU-LP.
           DISPLAY ZP-VERWEND(1:21) with highlight AT VDU-LP
               "Æ Ø" ZP-VERWEND(22:21) with highlight "Æ".
           DISPLAY "<>= zurÅck, <esc>= Abbruch" AT 2301.
           DISPLAY "Text 2. Zeile" AT 2401.
           CALL "CAUP" USING "1207440121" WH-CREG.
           IF WOLI GO M.
           IF ESC GO G.
           IF not RET GO N.
           MOVE WT-TX TO ZP-VERWEND(22:21).
           DISPLAY WT-TX(1:21) with highlight AT VDU-LP.
           GO G.
       X.
      *    PERFORM PRUEF
           CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ******************************************************************
       VERWEND SECTION.
       A.  IF WH-OLD not = 0 GO G.
           MOVE OP-REDAT TO WC-DATUM.
           PERFORM DATDREH.
           IF OP-RENUM = 0 MOVE "Re.v.:" TO ZP-VERWEND
               MOVE VDU-DATUM TO ZP-VERWEND(8:9)
           else MOVE OP-RENUM TO WD-BELNR
               IF WD-BELNR(1:1) = " " MOVE WD-BELNR(2:) TO ZP-VERWEND
               else MOVE WD-BELNR TO ZP-VERWEND.
           PERFORM VARYING WX FROM 40 BY -1 UNTIL WX = 0 OR
               ZP-VERWEND(WX:1) not = SPACE CONTINUE.
240408*    IF OP-REBETRAG < 0 AND WZ-SKTO = 0 GO Z.
           IF OP-REBETRAG = 0 AND WZ-SKTO = 0 GO Z.
           IF KB-ART = 2 GO C.                        *> bei Bankeinzug
           IF WZ-SKTO not = 0 ADD 1 TO WX
               MOVE " -Skto" TO ZP-VERWEND(WX:6).
           GO Z.
       C.  PERFORM VARYING WX FROM 40 BY -1 UNTIL WX = 1 or
               ZP-VERWEND(WX:1) not = SPACE CONTINUE.
           ADD 2 TO WX.
           MOVE WR-REST TO WD-VBET.
           PERFORM VARYING WY FROM 1 BY 1 UNTIL WY > 14
               or WD-VBET(WY:1) not = SPACE CONTINUE.
           PERFORM VARYING WY FROM WY BY 1 UNTIL WY > 14
               MOVE WD-VBET(WY:1) TO ZP-VERWEND(WX:1)
               ADD 1 TO WX.
           ADD -1 TO WX.
           IF ZP-VERWEND(WX:1) not = SPACE ADD 1 TO WX.
           IF WZ-SKTO not = 0 ADD 1 TO WX
               MOVE "-" TO ZP-VERWEND(WX:1)
               ADD 1 TO WX
      *        IF WD-SKTO(1:2) not = WD-SK(1:2)
                   MOVE WD-SK TO WD-SKTO
      *        end-if
               MOVE WD-SKTO(1:3) TO ZP-VERWEND(WX:3)
               ADD 3 TO WX
               MOVE "% =" TO ZP-VERWEND(WX:3)
               ADD 4 TO WX
               MOVE WZ-SKTO TO WD-SKBET
               PERFORM VARYING WY FROM 1 BY 1 UNTIL WY > 9
                   or WD-SKBET(WY:1) not = SPACE CONTINUE
               end-perform
               PERFORM VARYING WY FROM WY BY 1 UNTIL WY > 9
                   MOVE WD-SKBET(WY:1) TO ZP-VERWEND(WX:1)
                   ADD 1 TO WX.
           GO Z.
      *-----------------------------------------> alten Text ausgeben <-
       G.  ADD 720 VDU-ECK GIVING VDU-LP.
           IF KB-ART = 2
               DISPLAY ZP-VERWEND with highlight SIZE 40 AT VDU-LP
               GO Z.
           DISPLAY ZP-VERWEND(1:21) with highlight AT VDU-LP
               "Æ Ø" ZP-VERWEND(22:21) with highlight "Æ".
       Z.  EXIT.
      ******************************************************************
       ERF-OP SECTION.
       B.  PERFORM VARYING IX FROM 16 BY -1 UNTIL IX = 0
               OR WT-KEY(IX) not = LOW-VALUE
               END-PERFORM.
           IF WL-CA = 40 GO F.
       C.  IF not XPOS PERFORM BS-ZEIL.
       F.  IF WL-CA = 40; IF FINE SET ESC TO TRUE GO Z
               ELSE IF OP-REBETRAG = OP-ZABETRAG
                        SET AB TO TRUE GO L
                    ELSE SET RET TO TRUE.
           IF WL-CA = 33 PERFORM HOLWERTE GO K.
           PERFORM ANZEIG.
           IF WS = 0 MOVE WH-DATUM TO WH-WERT WZ-DATUM
           ELSE MOVE OP-REDAT TO WH-WERT WZ-DATUM.
       K.  DISPLAY "<esc>= Ende, <#>= Ende Lieft., <Einfg>= Summe OP's,
      -        "<ret>= Rechnungsdatum" AT 2301.
           DISPLAY "<Strg.+Ende>= Druck OP's" AT 2401.
           CALL "CAUP" USING "0100026006" WH-CREG.
       L.  IF HELP CALL "CAUP" USING "190025002601" WH-CREG GO K.
           IF ENTF PERFORM LOEBEW GO F.
           IF KIST OR ESC GO Z.
           IF APOS MOVE 1 TO IX GO F.
           IF XPOS GO B.
           IF BAUF PERFORM AUFZEIL GO F.
           IF AUF; IF IX > 1 SUBTRACT 1 FROM IX GO F
               ELSE PERFORM AUFZEIL GO F.
           IF BAB PERFORM ABZEIL; IF FINE GO F
                ELSE SUBTRACT 1 FROM IX GO F.
           IF AB; IF WT-KEY(IX + 1) not = LOW-VALUE GO C
                  ELSE PERFORM ABZEIL
                       IF FINE GO B ELSE GO F.
           MOVE 0 TO WI WE WS-DATUM.
           IF SEPO PERFORM OP-LIST GO F.
           IF EINF PERFORM OFFSUM GO B.
           IF not RET AND not TABL GO B.
           IF WX-DATUM = 0 GO K.
           IF WS = 1 GO M.
           IF WM-DATUM < WE-EURO; IF WX-DATUM < WE-EURO GO M.
           IF WM-DATUM not < WE-EURO; IF WX-DATUM > WE-EURO GO M.
      *    IF WM-DATUM < WE-EURO and WX-DATUM not > WM-DATUM GO M.
           DISPLAY "Datumsfehler! Siehe Tagesdatum zu Eurodatum!"
               AT 2401 PERFORM WEITER GO K.
       M.  DISPLAY VDU-DATUM with highlight AT VDU-LP.
           MOVE WX-DATUM TO WH-OPDAT.
           MOVE WD-KZ TO WH-OPDK.
           MOVE WK-KTONR TO WH-OPKTONR.
           MOVE 0 TO WH-OPLFD.
           IF WS = 1 GO Q.                                  *> OP-Ñndern
           INITIALIZE OP-SATZ.
           MOVE WH-OPKEY TO OP-KEY.
           IF WS = 0 MOVE WH-OPDAT TO OP-REDAT OP-VALDAT.
           IF WL-CA = 33 PERFORM HOLWERTE.
       Q.  PERFORM OPZEIL.
           IF ENDE GO Z.
           IF ESC SUBTRACT 1 FROM IX GO C.
           PERFORM ANZEIG.
           IF IX = 15 SET AB TO TRUE GO K.
           IF WL-CA = 40 SET AB TO TRUE GO K.
           IF WL-CA not = 33 GO C.
       Z.  EXIT.
      ***************************** Werte aus Buchungszeilen abstellen *
       HOLWERTE SECTION.
       A.  INITIALIZE OP-SATZ.
           MOVE KR-KTONR TO OP-KTONR.
           MOVE 0 TO OP-RENUM.
           MOVE KR-KOND TO WH-KK.
           MOVE WH-KK TO OP-KOND.
           MOVE WH-TG1 TO WH-NUM.
           IF WH-NUM = 0 MOVE WH-TGN TO WH-NUM.
           COMPUTE VDU-LP = IX * 100 + 502.
           PERFORM FAELLIG.
           PERFORM OP-ANZ.
           MOVE VDU-L TO WH-L.
           MOVE 0 TO WS.
           MOVE OP-REDAT TO WZ-DATUM WH-WERT.
       Z.  EXIT.
      ******************************************************************
       OP-BILD SECTION.
       A.  ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "Kto.-Nr.:           Re-Datum:           Val.Datum:"
               AT VDU-LP.
           ADD 1 TO VDU-L.
           DISPLAY "Re-Nr.:          Ref-Nr.:         Symb.:     W.:"
               AT VDU-LP.
           ADD 1 TO VDU-L.
           DISPLAY "Re-Betrag:                    Za-Betrag:" AT VDU-LP.
           ADD 1 TO VDU-L.
           DISPLAY "Kondition:                        Saldo:" AT VDU-LP.
           ADD 1 TO VDU-L.
           DISPLAY "Ust-1:  /  %  Bmg.: " AT VDU-LP.
           ADD 1 TO VDU-L.
           DISPLAY "Ust-2:  /  %  Bmg.:                 Ust:" AT VDU-LP.
           ADD 1 TO VDU-L.
           DISPLAY "Skontobasis:" AT VDU-LP.
      *-------------------------------------------------> Op-anzeigen <-
           MOVE WK-KTONR TO OP-KTONR.
           DIVIDE 10 INTO OP-KTONR GIVING WD-KTO.
           ADD 213 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-KTO with highlight AT VDU-LP.
           MOVE OP-REDAT TO WC-DATUM.
           PERFORM DATDREH.
           ADD 233 VDU-ECK GIVING VDU-LP.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           IF WS-DATUM not = 0 MOVE WS-DATUM TO WC-DATUM
           else MOVE OP-VALDAT TO WC-DATUM.
           PERFORM DATDREH.
           ADD 254 VDU-ECK GIVING VDU-LP.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           MOVE OP-RENUM TO WD-BELNR.
           ADD 311 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-BELNR with highlight AT VDU-LP.
           MOVE OP-REF TO WD-BELNR.
           ADD 329 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-BELNR with highlight AT VDU-LP.
           ADD 344 VDU-ECK GIVING VDU-LP.
           DISPLAY WE-SYM(OP-SYM + 1) with highlight AT VDU-LP.
           ADD 352 VDU-ECK GIVING VDU-LP.
           DISPLAY OP-FSYM with highlight foreground-color 6 AT VDU-LP.
           MOVE OP-KURS TO WD-KURS.
           DISPLAY "/" AT 0000
               WD-KURS with highlight foreground-color 6 AT 0000.
           MOVE OP-REBETRAG TO WD-BET.
           ADD 414 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-BET with highlight AT VDU-LP.
           MOVE OP-ZABETRAG TO WD-BET.
           ADD 444 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-BET with highlight AT VDU-LP.
           SUBTRACT OP-ZABETRAG FROM OP-REBETRAG GIVING WD-BET WH-WERT.
           ADD 544 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-BET with highlight AT VDU-LP.
           IF WF-SYM(1) not = OP-FSYM
               ADD 644 VDU-ECK GIVING VDU-LP
               COMPUTE WD-BET ROUNDED = WH-WERT * OP-KURS / 100
               DISPLAY WD-BET with highlight foreground-color 3
                   AT VDU-LP.
           PERFORM ANZ-OPKOND.
           IF OP-U1 not = 0 MOVE OP-U1 TO WD-KZ
               ADD 610 VDU-ECK GIVING VDU-LP
               DISPLAY WD-KZ with highlight AT VDU-LP
               MOVE WT-UST(OP-U1) TO WD-UST
               ADD 2 TO VDU-LP
               DISPLAY WD-UST with highlight AT VDU-LP
               MOVE OP-B1 TO WD-BET
               ADD 623 VDU-ECK GIVING VDU-LP
               DISPLAY WD-BET with highlight AT VDU-LP.
           IF OP-U2 not = 0 MOVE OP-U2 TO WD-KZ
               ADD 710 VDU-ECK GIVING VDU-LP
               DISPLAY WD-KZ with highlight AT VDU-LP
               MOVE WT-UST(OP-U2) TO WD-UST
               ADD 2 TO VDU-LP
               DISPLAY WD-UST with highlight AT VDU-LP
               MOVE OP-B2 TO WD-BET
               ADD 723 VDU-ECK GIVING VDU-LP
               DISPLAY WD-BET with highlight AT VDU-LP.
           MOVE OP-MW TO WD-BET.
           ADD 744 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-BET with highlight AT VDU-LP.
           MOVE OP-SKTOBAS TO WD-BET.
           ADD 816 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-BET with highlight AT VDU-LP.
           IF OP-ZADAT not = 0 ADD 843 VDU-ECK GIVING VDU-LP
               DISPLAY "Zahlg. am: " AT VDU-LP
               MOVE OP-ZADAT TO WC-DATUM
               PERFORM DATDREH
               DISPLAY VDU-DATUM with highlight AT 0000.
       Z.  EXIT.
      ******************************************************************
       ANZ-OPKOND SECTION.
           ADD 514 VDU-ECK GIVING VDU-LP.
           CALL "CAUP" USING "1300230041" WH-CREG.
           IF WH-SK1 not = 0 MOVE WH-SK1 TO WD-PZ
               MOVE WH-TG1 TO WD-TGN
               DISPLAY WD-PZ with highlight AT VDU-LP "/"
                       WD-TGN with highlight.
           IF WH-SK2 not = 0 MOVE WH-SK2 TO WD-PZ
               MOVE WH-TG2 TO WD-TGN
               ADD 522 VDU-ECK GIVING VDU-LP
               DISPLAY WD-PZ with highlight AT VDU-LP "/"
                       WD-TGN with highlight.
           IF WH-TGN not = 0 MOVE WH-TGN TO WD-TGN
               ADD 531 VDU-ECK GIVING VDU-LP
               DISPLAY WD-TGN with highlight AT VDU-LP.
       Z.  EXIT.
      ******************************************************************
       BEDMES SECTION.
       A.  DISPLAY "<esc>= Abbruch, <ret-leer>= Wiederholung, <>= zurÅc
      -        "k, <Ende>= Ende" AT 2301.
       Z.  EXIT.
      ******************************************************************
       OPZEIL SECTION.
       A.  CALL "CAUP" USING "0712080965000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " OP-Korrektur " with highlight AT VDU-LP.
           PERFORM OP-BILD.
       C.  PERFORM BEDMES.
           DISPLAY "= geÑnderte FÑlligkeit" AT 2401.
           MOVE OP-VALDAT TO WZ-DATUM WH-WERT.
           CALL "CAUP" USING "1102546006" WH-CREG.
           IF ESC OR WOLI GO X.
           IF ENDE GO W.
           IF not RET GO C.
           IF WZ-DATUM = 0 MOVE OP-REDAT TO WS-DATUM
                      else MOVE WZ-DATUM TO WS-DATUM.
           MOVE WS-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
       E.  PERFORM BEDMES.
           MOVE OP-RENUM TO WH-WERT.
           CALL "CAUP" USING "1003117007" WH-CREG.
           IF ESC GO X.
           IF AB GO J.
           IF WOLI GO C.
           IF ENDE GO W.
           IF not RET GO E.
           MOVE WH-WERT TO OP-RENUM WD-BELNR.
           DISPLAY WD-BELNR with highlight AT VDU-LP.
           IF WS = 0 MOVE OP-RENUM TO OP-REF
               MOVE OP-REF TO WD-BELNR
               ADD 329 VDU-ECK GIVING VDU-LP
               DISPLAY WD-BELNR with highlight AT VDU-LP.
       F.  PERFORM BEDMES.
           MOVE OP-REF TO WH-WERT.
           CALL "CAUP" USING "1003297007" WH-CREG.
           IF ESC GO X.
           IF WOLI GO E.
           IF ENDE GO W.
           IF not RET GO F.
           MOVE WH-WERT TO OP-REF WD-BELNR.
           DISPLAY WD-BELNR with highlight AT VDU-LP.
       G.  PERFORM BEDMES.
      *------------------------------------------------------> Symbol <-
           DISPLAY "<#>= Symbole anzeigen" AT 2401.
           MOVE OP-SYM TO WH-WERT.
           CALL "CAUP" USING "1003442002" WH-CREG.
           IF KIST CALL "FIBSTAM" USING "20SYMBOL" WH-CREG
                   CANCEL "FIBSTAM"
                   IF WH-WERT > 19 GO G.
           IF WOLI GO F.
           IF ESC GO X.
           IF ENDE GO W.
           IF not RET GO G.
           MOVE WH-WERT TO BZ-SYM.
           MOVE BZ-SYM TO OP-SYM.
           IF BZ-SYM > 30 GO F.
           DISPLAY WE-SYM(BZ-SYM + 1) with highlight AT VDU-LP.
           MOVE WE-SKZ(BZ-SYM + 1) TO WE-SYMT.
           IF WE-SYM(BZ-SYM + 1) = SPACE GO G.
       H.  PERFORM BEDMES.
      *------------------------------------------------> FremdwÑhrung <-
           DISPLAY "<#>= WÑhrungstabelle anzeigen" AT 2401.
           MOVE OP-FSYM TO WT-TX.
           MOVE 0 TO WH-WERT.
           CALL "CAUP" USING "1203520103" WH-CREG.
           IF KIST CALL "FIBSTAM" USING "25FREMDWHG" WH-CREG
                CANCEL "FIBSTAM"
                IF not ESC and WH-WERT = 0 MOVE "0" TO WT-TX
                else MOVE WH-WERT(1:13) TO WT-TX(1:1).
           IF WOLI GO G.
           IF ESC GO X.
           IF ENDE GO W.
           IF not RET GO H.
           MOVE WT-TX TO WV-MCODE.
           CALL "CAUP" USING "15NUMCV" WH-CREG.
           IF WH-WERT > 20 GO H.
           MOVE 0 TO IW.
           IF WH-WERT not = 0 or WV-MCODE(1:1) = "0"
               ADD WH-WERT 1 GIVING IW
               MOVE WF-SYM(IW) TO WT-TX
               MOVE WF-KURS(IW) TO OP-KURS.
           MOVE WT-TX TO BZ-FSYM.
           MOVE BZ-FSYM TO OP-FSYM.
           DISPLAY OP-FSYM with highlight foreground-color 6 AT VDU-LP.
           ADD 356 VDU-ECK GIVING VDU-LP.
           MOVE OP-KURS TO WD-KURS.
           DISPLAY WD-KURS with highlight foreground-color 6 AT VDU-LP.
           IF OP-FSYM = SPACE GO H.
      *-------------------------------------------------> Kurseingabe <-
       I.  IF OP-FSYM ="Ats" or "Eur"; IF WOLI GO G else GO J.
           PERFORM BEDMES.
           IF OP-KURS = 0 and IW not = 0 MOVE WF-KURS(IW) TO OP-KURS.
           MOVE OP-KURS TO WH-WERT.
           CALL "CAUP" USING "1003565208" WH-CREG.
           IF ESC GO X.
           IF WOLI GO H.
           IF ENDE GO W.
           IF not RET GO I.
           IF WH-WERT = 0 GO I.
           MOVE WH-WERT TO OP-KURS WD-KURS.
           DISPLAY WD-KURS with highlight foreground-color 6 AT VDU-LP.
       J.  PERFORM BEDMES.
           MOVE OP-REBETRAG TO WH-WERT.
           CALL "CAUP" USING "1004148212" WH-CREG.
           IF ESC GO X.
           IF WOLI GO I.
           IF ENDE GO W.
           IF not RET GO J.
           IF WH-WERT = 0 GO J.
           MOVE WH-WERT TO OP-REBETRAG WD-BET.
           DISPLAY WD-BET with highlight AT VDU-LP.
       K.  PERFORM BEDMES.
           MOVE OP-ZABETRAG TO WH-WERT.
           IF OP-REBETRAG = OP-ZABETRAG
                DISPLAY "bereits bezahlt!" AT 2401.
           CALL "CAUP" USING "1004448212" WH-CREG.
           IF WOLI GO J.
           IF ESC GO X.
           IF ENDE GO W.
           IF not RET GO K.
           MOVE WH-WERT TO OP-ZABETRAG WD-BET.
           DISPLAY WD-BET with highlight AT VDU-LP.
           SUBTRACT OP-ZABETRAG FROM OP-REBETRAG GIVING WD-BET WH-WERT.
           ADD 544 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-BET with highlight AT VDU-LP.
           COMPUTE WD-BET ROUNDED = WH-WERT * OP-KURS / 100.
           ADD 644 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-BET with highlight foreground-color 3 AT VDU-LP.
      *-------------------------------------------------> Konditionen <-
           IF OP-KOND = 0 MOVE WH-KK TO OP-KOND.
           PERFORM ANZ-OPKOND.
       L.  DISPLAY "<ret>= Skto-%, <#>= Konto anzeigen, <ret-leer>= Zahl
      -        "ungskond. oK! "  AT 2401.
           MOVE WH-SK1 TO WH-WERT.
           CALL "CAUP" USING "1005141103" WH-CREG.
           IF KIST PERFORM ANZ-KTO GO L.
           IF ESC GO X.
           IF WOLI GO K.
           IF WH-WERT = 0 MOVE 0 TO WH-SK1 WH-TG1 WH-SK2 WH-SK2 GO R.
           MOVE WH-WERT TO WH-SK1 WD-PZ.
           IF not RET GO L.
           DISPLAY WD-PZ with highlight AT VDU-LP "/".
       M.  DISPLAY "Skto-Tage" AT 2401.
           MOVE WH-TG1 TO WH-WERT.
           CALL "CAUP" USING "1005183003" WH-CREG.
           IF KIST PERFORM ANZ-KTO GO M.
           IF ESC GO X.
           IF WOLI GO L.
           IF ENDE GO W.
           IF not RET GO M.
           MOVE WH-WERT TO WH-TG1 WD-TGN.
           IF WH-TG1 = 0 GO M.
           DISPLAY WD-TGN with highlight AT VDU-LP.
       O.  DISPLAY "Skto-%, <#>= Konto anzeigen"  AT 2401.
           MOVE WH-SK2 TO WH-WERT.
           CALL "CAUP" USING "1005221103" WH-CREG.
           IF KIST PERFORM ANZ-KTO GO O.
           IF ESC GO X.
           IF WOLI GO M.
           IF ENDE GO W.
           IF WH-WERT = 0 MOVE 0 TO WH-SK2 WH-SK2 GO R.
           MOVE WH-WERT TO WH-SK2 WD-PZ.
           IF not RET GO O.
           DISPLAY WD-PZ with highlight AT VDU-LP "/".
       Q.  DISPLAY "Skto-Tage" AT 2401.
           MOVE WH-TG2 TO WH-WERT.
           CALL "CAUP" USING "1005263003" WH-CREG.
           IF KIST PERFORM ANZ-KTO GO Q.
           IF ESC GO X.
           IF WOLI GO O.
           IF not RET GO Q.
           MOVE WH-WERT TO WH-TG2 WD-TGN.
           IF WH-TG2 = 0 GO Q.
           DISPLAY WD-TGN with highlight AT VDU-LP.
       R.  DISPLAY "Netto-Tage" AT 2401.
           MOVE WH-TGN TO WH-WERT.
           CALL "CAUP" USING "1005313003" WH-CREG.
           IF TABL PERFORM ANZ-KTO GO R.
           IF ESC GO X.
           IF WOLI GO Q.
           IF ENDE GO W.
           IF not RET GO R.
           MOVE WH-WERT TO WH-TGN WD-TGN.
           IF WH-TGN = 0 GO R.
           DISPLAY WD-TGN with highlight AT VDU-LP.
           MOVE WH-KK TO OP-KOND.
       S.  PERFORM UST-EIN.
           IF WOLI GO R.
           IF ESC GO X.
       T.  IF OP-SKTOBAS = 0 MOVE OP-REBETRAG TO OP-SKTOBAS.
           ADD 816 VDU-ECK GIVING VDU-LP.
           MOVE OP-SKTOBAS TO WH-WERT WD-BET.
           DISPLAY WD-BET with highlight AT VDU-LP.
           CALL "CAUP" USING "1008168212" WH-CREG.
           IF ESC GO X.
           IF WOLI GO S.
           IF not RET GO T.
           MOVE WH-WERT TO OP-SKTOBAS WD-BET.
           DISPLAY WD-BET with highlight AT VDU-LP.
       W.  IF WS = 1 DELETE OPDATEI.
           MOVE WX-DATUM TO OP-VALDAT.
           MOVE 0 TO OP-SA.
           IF OP-REBETRAG = OP-ZABETRAG MOVE OP-DK TO OP-SA.
       U.  WRITE OP-SATZ INVALID ADD 1 TO OP-LFD GO U.
           MOVE OP-KEY TO WH-OPKEY.
           MOVE OP-KEY TO WT-KEY(IX).
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           IF WS > 0 GO Z.
           IF OP-KEY < WT-KEY(1) MOVE OP-KEY TO WT-KEY(1)
               MOVE 1 TO IX.
           PERFORM AUSGAB.
       Z.  EXIT.
      *********************************************** Umsatzsteuer-Kz. *
       UST-EIN SECTION.
       A.  DISPLAY "Ust-Kz., <#>= Ust-Tabelle" AT 2401.
           MOVE OP-U1 TO WH-WERT.
           CALL "CAUP" USING "1006101002" WH-CREG.
           IF KIST CALL "FIBSTAM" USING "33UST" WH-CREG
                   CANCEL "FIBSTAM" GO A.
           IF WOLI GO Z.
           IF ENDE GO Q.
           IF ESC GO Z.
           IF not RET GO A.
           MOVE WH-WERT TO OP-U1 WD-KZ.
           IF OP-U1 not = 0 MOVE WT-UST(OP-U1) TO WD-UST WH-UST
               DISPLAY WD-KZ with highlight AT VDU-LP
               ADD 2 TO VDU-LP
               DISPLAY WD-UST with highlight AT VDU-LP
           else MOVE 0 TO OP-U1 OP-B1 OP-U2 OP-B2 OP-MW GO Q.
           IF OP-B1 = 0 DIVIDE 100 INTO WH-UST GIVING WC-UST
               ADD 1 TO WC-UST
               DIVIDE WC-UST INTO OP-REBETRAG GIVING OP-B1.
       C.  MOVE OP-B1 TO WH-WERT WD-BET.
           ADD 623 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-BET with highlight AT VDU-LP.
           DISPLAY "0 = autom. Ermittlung - wenn nur 1 Ust-Satz"
               AT 2401.
           CALL "CAUP" USING "1006238212" WH-CREG.
           IF WOLI GO A.
           IF ESC GO Z.
           IF ENDE GO Q.
           IF not RET GO C.
           MOVE WH-WERT TO OP-B1 WD-BET.
           DISPLAY WD-BET with highlight AT VDU-LP.
      *------------------------------------------> event. 2. Ust-Satz <-
       E.  DISPLAY "Ust-Kz., <#>= Ust-Tabelle" AT 2401.
           MOVE OP-U2 TO WH-WERT.
           CALL "CAUP" USING "1007101002" WH-CREG.
           IF KIST CALL "FIBSTAM" USING "33UST" WH-CREG
                   CANCEL "FIBSTAM" GO E.
           IF WOLI GO C.
           IF ENDE GO Q.
           IF ESC GO Z.
           IF not RET GO E.
           MOVE WH-WERT TO OP-U2 WD-KZ.
           IF OP-U2 not = 0 MOVE WT-UST(OP-U2) TO WD-UST
               DISPLAY WD-KZ with highlight AT VDU-LP
               ADD 2 TO VDU-LP
               DISPLAY WD-UST with highlight AT VDU-LP
           else MOVE 0 TO OP-U2 OP-B2 GO Q.
           IF OP-B2 = 0 PERFORM RECH-UST
               COMPUTE WH-WERT = OP-REBETRAG - OP-B1 - OP-MW
               MOVE WT-UST(OP-U2) TO WH-UST
               DIVIDE 100 INTO WH-UST GIVING WC-UST
               ADD 1 TO WC-UST
               DIVIDE WC-UST INTO WH-WERT GIVING OP-B2.
       F.  MOVE OP-B2 TO WH-WERT WD-BET.
           ADD 723 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-BET with highlight AT VDU-LP.
           DISPLAY "0 = autom. Ermittlung - wenn nur 1 Ust-Satz"
               AT 2401.
           CALL "CAUP" USING "1007238212" WH-CREG.
           IF WOLI GO E.
           IF ESC GO Z.
           IF ENDE GO Q.
           IF not RET GO F.
           MOVE WH-WERT TO OP-B2 WD-BET.
           DISPLAY WD-BET with highlight AT VDU-LP.
      *----------------------------------------------> Ust-ermittlung <-
       Q.  IF OP-U1 = 0 AND OP-U2 = 0 GO E.
           IF OP-U2 = 0 MOVE 0 TO OP-B2.
           IF OP-U1 = 0 MOVE 0 TO OP-B1.
       S.  PERFORM RECH-UST.
      *-------------------------------------> Bmg + Ust = OP-REBETRAG <-
           SUBTRACT OP-B1 OP-B2 OP-MW FROM OP-REBETRAG GIVING WH-WERT.
           IF OP-U1 < 5 and OP-KURS = 100;
               IF WH-WERT not = 0 ADD WH-WERT TO OP-B1.
           IF OP-MW not = 0 MOVE OP-MW TO WD-BET
               ADD 744 VDU-ECK GIVING VDU-LP
               DISPLAY WD-BET with highlight AT VDU-LP.
           PERFORM OP-BILD.
       Z.  EXIT.
      ******************************************************************
       RECH-UST SECTION.
       A.  MOVE 0 TO OP-MW.
           IF OP-U1 not = 0 AND OP-U2 = 0 MOVE 0 TO OP-B1.
           IF OP-U2 not = 0 AND OP-U1 = 0 MOVE 0 TO OP-B2.
           IF OP-U1 > 0; MOVE WT-UST(OP-U1) TO WH-UST
               DIVIDE 100 INTO WH-UST GIVING WC-UST
               ADD 1 TO WC-UST
               IF OP-B1 = 0
                   COMPUTE OP-B1 = OP-REBETRAG / WC-UST * OP-KURS / 100
                   end-if
                   COMPUTE OP-MW =  OP-B1 * (WH-UST / 100)
990104*            COMPUTE OP-MW = OP-MW * OP-KURS / 100
990104*            COMPUTE OP-B1 = OP-B1 * OP-KURS / 100
                   IF OP-U1 > 4 MOVE OP-REBETRAG TO OP-B1
                       MOVE WT-UST(OP-U1) TO WH-UST
                       COMPUTE OP-MW =  OP-B1 * (WH-UST / 100).
           IF OP-U2 > 0 MOVE WT-UST(OP-U2) TO WH-UST
               DIVIDE 100 INTO WH-UST GIVING WC-UST
               ADD 1 TO WC-UST
               IF OP-B2 = 0
                   COMPUTE OP-B2 =
                       (OP-REBETRAG - OP-B1) / WC-UST * OP-KURS / 100
                   end-if
                   COMPUTE OP-MW = OP-MW + OP-B2 * (WH-UST / 100)
990104*            COMPUTE OP-MW = OP-MW * OP-KURS / 100
990104*            COMPUTE OP-B2 = OP-B2 * OP-KURS / 100
                   IF OP-U1 > 4 MOVE OP-REBETRAG TO OP-B1
                       MOVE WT-UST(OP-U1) TO WH-UST
                       COMPUTE OP-MW =  OP-B1 * (WH-UST / 100).
       Z.  EXIT.
      ******************************************************************
       ANZ-KTO SECTION.
       A.  MOVE WD-KZ TO WH-NUM.
           CALL "FIBANZ" USING "03KRANZ" WH-CREG.
           CANCEL "FIBANZ".
       Z.  EXIT.
      ******************************************************************
       KONDANZ SECTION.
       A.  IF WH-KK = 0 CALL "CAUP" USING "1300540070" WH-CREG GO Z.
           IF WH-SK1 not = 0 MOVE WH-SK1 TO WD-PZ
               MOVE WH-TG1 TO WD-TGN
               MOVE 51 TO VDU-P
               DISPLAY WD-PZ with highlight AT VDU-LP "/"
                       WD-TGN with highlight
           ELSE CALL "CAUP" USING "1300510059" WH-CREG.
           IF WH-SK2 not = 0 MOVE WH-SK2 TO WD-PZ
               MOVE WH-TG2 TO WD-TGN
               MOVE 59 TO VDU-P
               DISPLAY WD-PZ with highlight AT VDU-LP "/"
                       WD-TGN with highlight
           ELSE CALL "CAUP" USING "1300590066" WH-CREG.
           IF WH-TGN not = 0 MOVE WH-TGN TO WD-TGN
               MOVE 67 TO VDU-P
               DISPLAY WD-TGN with highlight AT VDU-LP
           ELSE CALL "CAUP" USING "1300670069" WH-CREG.
           IF WD-KZ = 2 and DE-OPKZ = 20 and VDU-L < 5
               MOVE 75 TO VDU-P
               DISPLAY "*E*" with highlight foreground-color 4
                   AT VDU-LP.
           IF WH-MOD = 0 GO Z.
           IF OP-VALDAT = 0 MOVE OP-REDAT TO OP-VALDAT.
           IF WH-SK1 not = 0 MOVE WH-TG1 TO WH-NUM GO X.
           IF WH-TGN not = 0 MOVE WH-TGN TO WH-NUM
                        else MOVE KR-KOND TO WH-KK
                             MOVE WH-TGN TO WH-NUM.
       X.  MOVE OP-VALDAT TO WZ-DATUM.
           PERFORM FAELLIG.
           MOVE 72 TO VDU-P.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           GO Z.
           IF WS not = 9 AND not = 1 GO Z.
       Z.  EXIT.
      ***************************************** Ermittlung Valutadatum *
       FAELLIG SECTION.
       A.  MOVE WH-NUM TO WH-TAG.
           MOVE WZ-DATUM TO WH-VALUTA.
           IF WZ-JAHR > 50 ADD 19000000 WH-VALUTA GIVING WH-NUM
                      else ADD 20000000 WH-VALUTA GIVING WH-NUM.
           COMPUTE WA-TAGE = FUNCTION INTEGER-OF-DATE(WH-NUM).
           ADD WH-TAG TO WA-TAGE GIVING WB-TAGE.
           COMPUTE WB-TAGE = FUNCTION DATE-OF-INTEGER(WB-TAGE).
           MOVE WB-TAGE TO WH-VALUTA WC-DATUM WZ-DATUM.
           MOVE WR TO WL.
           CALL "CAUP" USING "03DATUM" WH-CREG.
           MOVE WL TO WR.
           IF WO-TGN = 7 MOVE 1 TO WH-NUM GO A.
           PERFORM DATDREH.
       Z.  EXIT.
      **************************************** anzeigen einer Position *
       OP-ANZ SECTION.
       A.  COMPUTE VDU-LP = IX * 100 + 502.
           IF OP-REDAT > 0 MOVE OP-REDAT TO WC-DATUM
               PERFORM DATDREH
               DISPLAY VDU-DATUM with highlight AT VDU-LP.
           ADD 9 TO VDU-P.
           MOVE OP-RENUM TO WD-BELNR.
           DISPLAY WD-BELNR with highlight AT VDU-LP.
           IF OP-REBETRAG not = 0 MOVE 22 TO VDU-P
               MOVE OP-REBETRAG TO WD-BET
               IF OP-FSYM not = "Ats"
                  DISPLAY WD-BET with highlight AT VDU-LP
               else DISPLAY WD-BET with highlight foreground-color 4
                  AT VDU-LP.
           COMPUTE WH-WERT = OP-ZABETRAG + OP-ZPBET + OP-ZPSKTO.
           IF WH-WERT not = 0 MOVE 36 TO VDU-P
               MOVE WH-WERT TO WD-BET
               IF OP-ZPBET = 0 DISPLAY WD-BET with highlight AT VDU-LP
               else DISPLAY WD-BET with highlight foreground-color 3
                  AT VDU-LP.
           MOVE OP-KOND TO WH-KK.
           PERFORM KONDANZ.
           MOVE OP-KEY TO WH-OPKEY WT-KEY(IX).
           if op-valdat < 991232 delete opdatei
                                 end-delete
                add 19000000 to op-valdat
                if op-valdat < 19900000 add 1000000 to op-valdat
                end-if
                write op-satz.
           ADD 1 TO WR.
       Z.  EXIT.
      ******************************************************* OP-Liste *
       OP-LIST SECTION.
       A.  IF WH-PG = 3 MOVE 999999 TO WV-BISDAT;
               IF WD-KZ = 2 MOVE DE-KTONR TO WV-KONTO
                            MOVE DE-BEZ TO WX-ZEI GO H
                       else MOVE KR-KTONR TO WV-KONTO
                            MOVE KR-BEZ TO WX-ZEI GO H.
           MOVE " Offene Postenliste " TO WK-GEB.
           CALL "CAUP" USING "06KOPF" WH-CREG.
           DISPLAY "Listung der fÑlligen Posten bis:" AT 0611.
           DISPLAY "<ret>= FÑlligkeitsdatum, <esc>= Abbruch" AT 2301.
           DISPLAY "0-Datum = alle FÑlligkeiten" AT 2401.
           MOVE WH-DATUM TO WZ-DATUM WH-WERT.
           CALL "CAUP" USING "0106446006" WH-CREG.
           IF ESC GO Z.
           IF not RET GO A.
           MOVE WZ-DATUM TO WV-BISDAT.
           IF WV-BISDAT = 0 MOVE 999999 TO WV-BISDAT
               DISPLAY "alle  " with highlight AT VDU-LP
           else DISPLAY VDU-DATUM with highlight AT VDU-LP.
       E.  DISPLAY "fÅr ZahlungsempfÑnger:" AT 0811.
           DISPLAY "<ret-leer>= alle, <Nr.+ ret>= ausgew. EmpfÑng., <esc
      -        ">= Abbruch" AT 2301.
           CALL "CAUP" USING "0008346006" WH-CREG.
           IF WOLI GO A.
           IF ESC GO Z.
           IF not RET GO E.
           MOVE 0 TO WS-BET WZ-BET.
           IF WH-NUM = 0 MOVE 0 TO WV-KONTO
               DISPLAY "alle" with highlight AT VDU-LP GO J.
           IF RET AND WH-NUM = 0 AND WV-MCODE not = SPACE
               PERFORM ANZ-KTO GO G.
           MOVE WH-NUM TO KR-KTONR.
       G.  IF KR-KTONR < 4 GO E.
           READ KREDIT INVALID GO E.
           IF ZUGRIF PERFORM BESETZT GO G.
           MOVE 3 TO WV-DK.
           MOVE KR-KTONR TO WV-KONTO.
           DIVIDE 10 INTO WV-KONTO GIVING WD-KTO.
       H.  INSPECT WX-ZEI REPLACING ALL "#" BY ",".
           IF WH-PG not = 3 DISPLAY WD-KTO with highlight AT VDU-LP
               DISPLAY WX-ZEI with highlight AT 0911.
       J.  MOVE 0 TO WZ-ZEILEN WZ-SEITE WS-BET WZ-BET.
           MOVE WV-KONTO TO OP-KTONR WM-KONTO.
           DIVIDE 10 INTO WV-KONTO GIVING WH-NUM REMAINDER WV-DK.
           IF WV-DK = 0 MOVE 3 TO WV-DK.
           MOVE WV-DK TO OP-DK.
           MOVE 0 TO OP-VALDAT OP-LFD.
           MOVE 1 TO WH-MOD.
           START OPDATEI KEY not < OP-KEY INVALID GO Z.
       L.  READ OPDATEI NEXT NO LOCK AT END MOVE 999 TO WV-KONTO GO Q.
           IF ZUGRIF PERFORM BESETZT GO L.
           IF OP-DK not = WV-DK MOVE 999 TO WV-KONTO GO Q.
           IF WM-KONTO = 0 MOVE OP-KTONR TO WM-KONTO.
           IF WM-KONTO not = 0 AND WM-KONTO not = OP-KTONR GO Q.
           MOVE OP-KOND TO WH-KK.
           MOVE WH-TG1 TO WH-NUM.
           MOVE OP-VALDAT TO WZ-DATUM.
           IF OP-DK = 3 SUBTRACT OP-ZABETRAG FROM OP-REBETRAG
                            GIVING WH-WERT
                        SUBTRACT OP-ZPBET OP-ZPSKTO FROM WH-WERT.
           IF OP-DK = 2 SUBTRACT OP-ZABETRAG FROM OP-REBETRAG
               GIVING WH-WERT.
           IF WH-WERT = 0 GO L.
           PERFORM FAELLIG.
           MOVE WC-DATUM TO WS-DATUM.
           IF WC-DATUM > WV-BISDAT GO L.
           IF WH-PG = 3 GO O.
           IF WH-MOD = 0 GO O.
       M.  MOVE OP-KTONR TO KR-KTONR.
       N.  READ KREDIT NO LOCK INVALID MOVE "Lft. gelîscht" TO KR-BEZ.
           IF ZUGRIF PERFORM BESETZT GO N.
           MOVE KR-BEZ TO WX-ZEI.
           INSPECT WX-ZEI REPLACING ALL "#" BY ",".
           DISPLAY WX-ZEI with highlight AT 0911.
       O.  PERFORM DRU-OP.
           GO L.
       Q.  IF WS-BET = 0 GO R.
           MOVE ALL "ƒ" TO DRA-STR.
           PERFORM DRUCK.
           MOVE "S u m m e:" TO DRA-STR(10:12).
           MOVE WS-BET TO WD-BET.
           MOVE WD-BET TO DRA-STR(45:14).
           PERFORM DRUCK.
           ADD WS-BET TO WZ-BET.
           IF WS-BET not = WZ-BET
               MOVE WZ-BET TO WD-BET
               MOVE WD-BET TO DRA-STR(45:14)
               PERFORM DRUCK.
           MOVE 3 TO WZ-SCHALT.
       R.  IF WV-KONTO = 0 MOVE 1 TO WH-MOD
               MOVE OP-KTONR TO WM-KONTO
               MOVE 0 TO WS-BET
               GO M.
           IF WM-OPEN = 1 WRITE DRA-SATZ BEFORE PAGE
               PERFORM END-DRU.
           CALL "CAUP" USING "1322012480" WH-CREG.
       Z.  EXIT.
      ************************************************* Druck OP-Liste *
       DRU-OP SECTION.
       A.  IF WZ-ZEILEN > WH-SIZE WRITE DRA-SATZ AFTER PAGE
               MOVE 0 TO WZ-ZEILEN.
           IF WZ-ZEILEN > 0 GO G.
           MOVE 2 TO WH-P1.
           IF WM-KO = 0 MOVE 5 TO WH-P2
                        MOVE 40 TO WH-SIZE
                   ELSE MOVE 4 TO WH-P2
                        MOVE 61 TO WH-SIZE.
           IF WM-DRU = 1 MOVE 15 TO WH-P1.
           MOVE "OPLIST.LST" TO WH-DRUNAM.
           PERFORM BEG-DRU.
           MOVE 1 TO WZ-SCHALT.
           MOVE WK-FIRMA TO DRA-STR.
           MOVE "Liste fÑlliger Posten bis:" TO DRA-STR(30:).
           MOVE WV-BISDAT TO WC-DATUM.
           PERFORM DATDREH.
           IF WV-BISDAT = 999999 MOVE " alle" TO DRA-STR(58:8)
           ELSE MOVE VDU-DATUM TO DRA-STR(58:8).
           MOVE WH-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRA-STR(80:8).
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRA-STR.
           PERFORM DRUCK.
           MOVE "Re-Datum Rechnungsnr.     Re-Betrag  Skonto   Nettobetr
      -         "ag fÑllig am:  A n m e r k u n g " TO DRA-STR.
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRA-STR.
           PERFORM DRUCK.
           MOVE 2 TO WZ-SCHALT.
       G.  IF WH-MOD = 1
               MOVE WD-KTO TO DRA-STR(1:9)
               MOVE WX-ZEI TO DRA-STR(11:)
               MOVE 0 TO WH-MOD
               PERFORM DRUCK
               MOVE 2 TO WZ-SCHALT.
           MOVE OP-REDAT TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRA-STR(1:8).
           MOVE OP-RENUM TO WD-BELNR.
           MOVE WD-BELNR TO DRA-STR(10:12).
           MOVE OP-REBETRAG TO WD-BET.
           MOVE WD-BET TO DRA-STR(23:14)
           MOVE OP-KOND TO WH-KK.
           MOVE OP-REBETRAG TO WH-BET WD-BET WR-BET.
           IF WH-SK1 not = 0 MOVE WH-SK1 TO WD-PZ
               MOVE WD-PZ TO DRA-STR(40:3)
               MOVE "%" TO DRA-STR(43:1)
               MULTIPLY WH-SK1 BY OP-REBETRAG GIVING WH-BET
               COMPUTE WH-BET ROUNDED = OP-REBETRAG * WH-SK1 * 0,01
               COMPUTE WR-BET = OP-REBETRAG - WH-BET
               MOVE WR-BET TO WD-BET WH-BET.
           ADD WH-BET TO WS-BET.
           MOVE WD-BET TO DRA-STR(45:14).
           MOVE WS-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRA-STR(60:8).
           DISPLAY DRA-STR(1:80) AT 2301.
           PERFORM DRUCK.
       Z.  EXIT.
      **************************************** OP-Liste fÅr Bankeinzug *
       OP-LIST-B SECTION.
       A.  MOVE 999999 TO WV-BISDAT.
           MOVE DE-KTONR TO WV-KONTO.
           MOVE DE-BEZ TO WX-ZEI.
           MOVE "    Bankeinzugliste " TO WK-GEB.
           CALL "CAUP" USING "06KOPF" WH-CREG.
           DISPLAY "Listung der fÑlligen Posten bis:" AT 0611.
           DISPLAY "<ret>= Einzugsdatum, <esc>= Abbruch" AT 2301.
           MOVE WH-DATUM TO WC-DATUM WV-BISDAT.
           PERFORM DATDREH.
           DISPLAY VDU-DATUM with highlight AT 0644.
           MOVE WH-DATUM TO WZ-DATUM WH-WERT.
           CALL "CAUP" USING "0106446006" WH-CREG.
           IF ESC GO Z.
           IF not RET GO A.
           MOVE WZ-DATUM TO WV-BISDAT.
           IF WV-BISDAT = 0 MOVE WM-DATUM TO WV-BISDAT.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           DISPLAY "fÅr " AT 0811 "alle" with highlight " Kunden".
       F.  DISPLAY "<esc>= Abbruch, </ >= zurÅck <ret>= Start < >"
               AT 2301.
           CALL "CAUP" USING "0023450000" WH-CREG.
           IF ESC GO Z.
           IF WOLI or AUF GO A.
           IF not RET GO F.
           OPEN INPUT BANKDAT.
           MOVE 0 TO WZ-ZEILEN WZ-SEITE WS-BET WZ-BET WV-KONTO WM-KONTO.
           MOVE LOW-VALUE TO DE-SATZ.
           START DEBITOR KEY not < DE-MCODE INVALID PERFORM NO-REC GO A.
       H.  READ DEBITOR NEXT AT END MOVE 99999 TO WV-KONTO GO R.
           IF ZUGRIF PERFORM BESETZT GO H.
           MOVE DE-BEZ TO WX-ZEI.
           INSPECT WX-ZEI REPLACING ALL "#" BY ",".
           DISPLAY WX-ZEI with highlight AT 0911.
           MOVE DE-KTONR TO BK-KTONR.
           MOVE 2 TO BK-DK.
           IF DE-OPKZ not = 20 GO H.
           MOVE WV-KONTO TO WM-KONTO.
           MOVE 2 TO WV-DK OP-DK.
           MOVE DE-KTONR TO OP-KTONR.
           MOVE 0 TO OP-VALDAT OP-LFD.
           MOVE 1 TO WH-MOD.
           START OPDATEI KEY not < OP-KEY INVALID GO H.
       L.  READ OPDATEI NEXT NO LOCK AT END MOVE 999 TO WV-KONTO GO Q.
           IF ZUGRIF PERFORM BESETZT GO L.
           IF OP-DK not = WV-DK MOVE 999 TO WV-KONTO GO Q.
080306*    IF WM-KONTO = 0 MOVE OP-KTONR TO WM-KONTO.
      *    IF WM-KONTO not = 0 AND WM-KONTO not = OP-KTONR GO Q.
           IF DE-KTONR not = OP-KTONR GO Q.
           MOVE OP-KOND TO WH-KK.
           MOVE WH-TG1 TO WH-NUM.
           MOVE OP-VALDAT TO WZ-DATUM.
           SUBTRACT OP-ZABETRAG FROM OP-REBETRAG GIVING WH-WERT.
           IF WH-WERT = 0; IF OP-ZPBET not = 0
                               MOVE 0 TO OP-ZPBET OP-ZPSKTO
                               REWRITE OP-SATZ.
           SUBTRACT OP-ZPBET OP-ZPSKTO FROM WH-WERT.
           IF WH-WERT = 0 GO L.
           PERFORM FAELLIG.
           MOVE WC-DATUM TO WS-DATUM.
           IF WC-DATUM > WV-BISDAT GO L.
           PERFORM DRU-OP-EINZ.
           GO L.
       Q.  IF WS-BET = 0 GO R.
           MOVE ALL "ƒ" TO DRA-STR.
           PERFORM DRUCK.
           MOVE "S u m m e:" TO DRA-STR(10:12).
           MOVE WS-BET TO WD-BET.
           MOVE WD-BET TO DRA-STR(45:14).
           PERFORM DRUCK.
           ADD WS-BET TO WZ-BET.
           IF WS-BET not = WZ-BET
               MOVE WZ-BET TO WD-BET
               MOVE WD-BET TO DRA-STR(45:14)
               PERFORM DRUCK.
           MOVE 3 TO WZ-SCHALT.
       R.  IF WV-KONTO = 0 MOVE 1 TO WH-MOD
               MOVE 0 TO WS-BET
               GO H.
           IF WM-OPEN = 1 WRITE DRA-SATZ BEFORE PAGE
               PERFORM END-DRU.
           CALL "CAUP" USING "1322012480" WH-CREG.
       X.  CLOSE BANKDAT.
       Z.  EXIT.
      ************************************************* Druck OP-Liste *
       DRU-OP-EINZ SECTION.
       A.  IF WZ-ZEILEN > WH-SIZE WRITE DRA-SATZ AFTER PAGE
               MOVE 0 TO WZ-ZEILEN.
           IF WZ-ZEILEN > 0 GO G.
           MOVE 2 TO WH-P1.
           IF WM-KO = 0 MOVE 5 TO WH-P2
                        MOVE 40 TO WH-SIZE
                   ELSE MOVE 4 TO WH-P2
                        MOVE 61 TO WH-SIZE.
           IF WM-DRU = 1 MOVE 15 TO WH-P1.
           MOVE "EINZUG.LST" TO WH-DRUNAM.
           PERFORM BEG-DRU.
           MOVE 1 TO WZ-SCHALT.
           MOVE WK-FIRMA TO DRA-STR.
           MOVE "Liste fÑlliger Posten bis:" TO DRA-STR(30:).
           MOVE WV-BISDAT TO WC-DATUM.
           PERFORM DATDREH.
           IF WV-BISDAT = 999999 MOVE " alle" TO DRA-STR(58:8)
           ELSE MOVE VDU-DATUM TO DRA-STR(58:8).
           MOVE WH-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRA-STR(80:8).
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRA-STR.
           PERFORM DRUCK.
           MOVE "Re-Datum Rechnungsnr.     Re-Betrag  Skonto   Nettobetr
      -         "ag fÑllig am:  A n m e r k u n g " TO DRA-STR.
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRA-STR.
           PERFORM DRUCK.
           MOVE 2 TO WZ-SCHALT.
       G.  IF WH-MOD = 1
               MOVE WD-KTO TO DRA-STR(1:9)
               MOVE WX-ZEI TO DRA-STR(11:)
               MOVE 0 TO WH-MOD
               PERFORM DRUCK
               MOVE 2 TO WZ-SCHALT.
           MOVE OP-REDAT TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRA-STR(1:8).
           MOVE OP-RENUM TO WD-BELNR.
           MOVE WD-BELNR TO DRA-STR(10:12).
           MOVE OP-REBETRAG TO WD-BET.
           MOVE WD-BET TO DRA-STR(23:14)
           MOVE OP-KOND TO WH-KK.
           MOVE OP-REBETRAG TO WH-BET.
           IF WH-WERT not = OP-REBETRAG             *> Teilbetrag offen
               MOVE WH-WERT TO WH-BET GO H.
           IF WH-SK1 not = 0 MOVE WH-SK1 TO WD-PZ
               MOVE WD-PZ TO DRA-STR(40:3)
               MOVE "%" TO DRA-STR(43:1)
               MULTIPLY WH-SK1 BY OP-REBETRAG GIVING WH-BET
               COMPUTE WH-BET ROUNDED = OP-REBETRAG * WH-SK1 * 0,01
               SUBTRACT WH-BET FROM WH-WERT GIVING WH-BET.
       H.  ADD WH-BET TO WS-BET
           MOVE WH-BET TO WD-BET.
           MOVE WD-BET TO DRA-STR(45:14).
           MOVE WS-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRA-STR(60:8).
           DISPLAY DRA-STR(1:80) AT 2301.
           PERFORM DRUCK.
       Z.  EXIT.
