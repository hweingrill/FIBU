      $SET LINKCOUNT "420" ANS85 BOUND AUTOLOCK NOALTER SEGSIZE "55000"
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     FIBJEND.
      ******************************************************************
      *        Journalende mit autom. Sammelbuchungen                  *
      *-------------------- und ---------------------------------------*
      *        öbernahme ER oder AR (Fremdbuchungen)                   *
      *  Ats-Zeit:  Ats-Re = Ats-Bubet - 0 oder Ats-Fwbet              *
      *             Eur-Re = Ats-Bubet - Eur-Fwbet         +)          *
      *  Eur-Zeit:  Ats-Re = Eur-Bubet - Ats-Fwbet                     *
      *             Eur-Re = Eur-Bubet - 0 oder Eur-Fwbet              *
      *  +) mu· zwingend aus jeweiliger FAKT kommen - d. h. jede FAKT  *
      *  mu· geÑndert werden sobald EUR-Rechnungen erstellt werden!    *
      ******************************************************************
       ENVIRONMENT    DIVISION.
       CONFIGURATION   SECTION.
       SOURCE-COMPUTER.  PC.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY FIBUSEC.CPY.
           SELECT ZAHLPOS   ASSIGN TO "FIBUZAHL.DAT"
                            ORGANIZATION INDEXED ACCESS DYNAMIC
                            RECORD KEY ZP-KEY
                            ALTERNATE RECORD ZP-AKEY
                            FILE STATUS IS WF-STATUS.
           SELECT KASSA     ASSIGN "FIBUKAS.DAT"
                            ORGANIZATION INDEXED, ACCESS DYNAMIC
                            RECORD KEY KA-KEY
                            ALTERNATE KEY KA-RKEY DUPLICATES
                            FILE STATUS WF-STATUS.
           SELECT DRUCKER   ASSIGN TO PRINTER WH-DRUNAM
                            FILE STATUS WF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       COPY FIBUFD.CPY.
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
      ************************************************* Kassabuchungen *
       FD  KASSA                       LABEL RECORD STANDARD.
       01  KA-SATZ.
           03  KA-KEY.
               05 KA-NUM               PIC 9(6)       COMP.
           03  KA-RKEY.
               05 KA-SA                PIC X.
               05 KA-REN               PIC 9(7)       COMP.
           03  KA-KTONR                PIC 9(6)       COMP.
           03  KA-KOND                 PIC 9(13)      COMP.
           03  KA-DAT                  PIC 9(6)       COMP.
           03  KA-SY                   PIC 99         COMP.
           03  KA-GK                   PIC 9(6)       COMP.
           03  KA-Z                    PIC 9          COMP.
           03  KA-VM                   PIC 9          COMP.
           03  KA-RF                   PIC 9(7)       COMP.
           03  KA-U1                   PIC 9          COMP.
           03  KA-U2                   PIC 9          COMP.
           03  KA-MW                   PIC S9(7)V99   COMP-3.
           03  KA-B1                   PIC S9(9)V99   COMP-3.
           03  KA-B2                   PIC S9(9)V99   COMP-3.
           03  KA-SH                   PIC S9         COMP-3.
           03  KA-BET                  PIC S9(9)V99   COMP-3.
           03  KA-KST                  PIC S9(5)      COMP-3.
           03  KA-TX                   PIC X(40).
           03  KA-SKTOBAS              PIC S9(9)V99   COMP-3.
           03  KA-VALDAT               PIC 9(6)       COMP.
      *--------------------------------------------> fÅr FremdwÑhrung <-
           03  KA-FWBET                PIC S9(9)V99   COMP.
           03  KA-KURS                 PIC 9(4)V9(7)  COMP.
           03  KA-FSYM                 PIC XXX.
      ******************************************************************
       FD  DRUCKER                     LABEL RECORD STANDARD.
       01  DRA-SATZ.
           03  FILLER                  PIC XXX.
           03  DRA-STR.
               05 DRA-KTONR            PIC ZZZZZZ,Z-.
               05 DRA-BELNR            PIC ZZZZZZ-.
               05 FILLER               PIC X.
               05 DRA-BELDAT           PIC X(8).
               05 DRA-REFNR            PIC ZZZZZZ-.
               05 DRA-SYM              PIC XX.
               05 DRA-GGKTO            PIC ZZZZZZ,Z-.
               05 DRA-TX               PIC X(39).
               05 FILLER               PIC X.
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
       01  WH-REG.
           03  WK-DSTG.
               05 WK-STG               PIC X(10)     OCCURS 10.
           03  WD-Z                    PIC 9.
           03  WH-P1                   PIC 99        COMP.
           03  WH-P2                   PIC 99        COMP.
           03  WX-PRNO                 PIC 99        COMP-X.
           03  WX-PRSTAT               PIC 99        COMP-X.
           03  WZ-SEITE                PIC 99        COMP   VALUE ZERO.
           03  WZ-SCHALT               PIC 99        COMP   VALUE ZERO.
           03  WZ-ZEILEN               PIC 99        COMP   VALUE ZERO.
           03  WH-DRUNAM               PIC X(12)     VALUE   "LPT1:".
           03  WM-OPEN                 PIC 9         COMP   VALUE ZERO.
           03  WH-SIZE                 PIC 99        COMP   VALUE 63.
           03  WM-L                    PIC 99        COMP.
           03  WL                      PIC 99        COMP.
           03  WS                      PIC 99        COMP.
           03  WO                      PIC 99        COMP.
           03  IX                      PIC 99        COMP.
           03  WI                      PIC 99        COMP.
           03  WM-UEANZ                PIC 99        COMP.
           03  WK-LP                   PIC 9999      COMP.
           03  WK-MCODE                PIC X(10).
           03  WH-BZSATZ               PIC X(128).
           03  WV-BAS                  PIC S9(9)V99  COMP.
           03  WH-TEIL                 PIC S999V9(7) COMP.
           03  WK-BET                  PIC S9(9)V99  COMP.
           03  WK-SKTO                 PIC S9(9)V99  COMP.
           03  WP-SKTO                 PIC S9(9)V99  COMP.
           03  WK-MWST                 PIC S9(7)V99  COMP.
           03  WK-BASIS                PIC S9(9)V99  COMP.
           03  WK-SOLL                 PIC S9(9)V99  COMP.
           03  WK-HABEN                PIC S9(9)V99  COMP.
           03  WK-BRUTTO               PIC S9(9)V99  COMP.
           03  WK-NETTO                PIC S9(9)V99  COMP.
           03  WK-BUBET                PIC S9(9)V99  COMP.
           03  WS-SAMMEL               PIC S9(9)V99  COMP.
           03  WS-FWSAML               PIC S9(9)V99  COMP.
           03  WM-ZABET                PIC S9(9)V99  COMP.
           03  WV-BAKEY                PIC 999       COMP.
           03  WM-ARSYM                PIC 99        COMP.
           03  WM-GSSYM                PIC 99        COMP.
           03  WM-ZASYM                PIC 99        COMP.
           03  WH-AKAT                 PIC 9         COMP.
           03  WH-ABN                  PIC 99        COMP.
           03  WH-GKAT                 PIC 9         COMP.
           03  WH-GBN                  PIC 99        COMP.
           03  WH-BN                   PIC 99        COMP.
           03  WM-USE                  PIC 9         COMP.
           03  WM-Z                    PIC 99        COMP.
                     88 KAS-ZAHL   VALUE 7.
           03  WM-GB                   PIC 99        COMP.
             88 OHNE      VALUE 0.   88 EINZEL     VALUE 1.
             88 SAMMEL    VALUE 2.   88 AUFTEIL    VALUE 4.
           03  WK-USTKTO               PIC 9(6)      COMP.
           03  WH-BKEY.
               05 WH-KTONR             PIC 9(6)      COMP.
               05 WH-BUDAT             PIC 9(8)      COMP.
               05 WH-LFD               PIC 9999      COMP.
           03  WK-LFD                  PIC 9999      COMP.
           03  WH-JOKEY                PIC X(4).
           03  WH-KAT                  PIC 9         COMP.
           03  WC-UST                  PIC S99V9999  COMP.
           03  WH-UST                  PIC S99V99    COMP.
           03  WH-JS                   PIC 9(7)      COMP-X.
           03  WT-SH.
               05 WR-SH                PIC 9     OCCURS 38.
           03  WK-JS                   PIC 9(7).
           03  WR-JS REDEFINES WK-JS.
               05 WK-SEITE             PIC 99999.
               05 WK-ZEIL              PIC 99.
           03  WM-SEITE                PIC 9(5)      COMP.
           03  WM-MT                   PIC 999       COMP.
           03  WV-FPER                 PIC 9999.
           03  WM-FPER                 PIC 9999.
      *-----------------------------------------------> Anzeigefelder <-
           03  WD-SALDO                PIC ZZ.ZZZ.ZZ9,99-.
           03  WD-NUM                  PIC ZZZZZ9.
           03  WD-KTO                  PIC ZZZZ9,9.
           03  WD-KZ                   PIC 9.
           03  WH-OP                   PIC 9.
           03  WD-POS                  PIC Z9-.
           03  WD-BET                  PIC ZZZ.ZZ9,99-.
           03  WD-NR                   PIC ZZZ.ZZ9.
       01  WO-TSATZ.
           03  FILLER                  PIC 999      COMP-3.
           03  WO-KTONR                PIC 9(6)     COMP   OCCURS 20.
           03  WO-SUMM                 PIC S9(9)V99 COMP   OCCURS 20.
           03  WO-DAT                  PIC 9(8)     COMP   OCCURS 20.
       COPY FIBUEXT.CPY.
       COPY FIBUDECL.CPY.
       DECL-P SECTION.         USE AFTER ERROR PROCEDURE ON ZAHLPOS.
       A.  CALL "CADECL" USING "FIBUZAHL.DAT" WH-CREG.
       Z.  EXIT.
       END DECLARATIVES.
      *****************************************************************
       STEUER SECTION.
       A.  MOVE WL-CALL TO WH-CALL.
           MOVE WL-CREG TO WH-CREG.
           MOVE 0 TO WM-Z.
           EVALUATE WL-CA
               WHEN 10 PERFORM JOUREND
               WHEN 50 PERFORM UEBERNAHME.
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
           PERFORM WEITER.
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
           READ KONSTANT IGNORE LOCK INVALID NEXT SENTENCE.
       X.  MOVE KOP-TX TO WK-DSTG.
       Z.  EXIT.
      ******************************************************************
       JOUREND SECTION.
       A.  CALL "CAUP" USING "0710050673000" WH-CREG.
           MOVE "JOURNAL.LST" TO WH-DRUNAM.
           PERFORM LADE-DRU.
           OPEN EXTEND DRUCKER.
           MOVE 48 TO WH-PG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Journalabschlu· " AT VDU-LP.
           ADD 203 VDU-ECK GIVING VDU-LP.
           MOVE 9 TO WH-KEY.
       B.  READ KONSTANT INVALID STOP RUN.
           IF ZUGRIF PERFORM BESETZT GO Z.
           MOVE KO-TSATZ TO WO-TSATZ.
           PERFORM LAST-JOUR.
           IF ESC GO X.
           MOVE BZ-BUDAT TO WE-PER.
           ADD WE-USE 1 GIVING WH-KEY.
       C.  READ KONSTANT INVALID INITIALIZE KO-BSATZ
               MOVE WH-KEY TO KO-NUM WRITE KO-SATZ GO C.
           IF KO-SPERR not = 0 ADD 303 VDU-ECK GIVING VDU-LP
               DISPLAY "Bitte Journal durchrechnen! < >" with highlight
                   AT VDU-LP  " Weiter mit beliebiger Taste!"
               CALL "CAUP" USING "1003320000" WH-CREG
               GO X.
           IF ZUGRIF STOP RUN.
           DISPLAY "Buchungsdatum f. autom. Buchungen: " AT VDU-LP.
           MOVE WE-PER TO KO-BUDAT WC-DATUM WX-DATUM.
           PERFORM DATDREH.
           DISPLAY VDU-DATUM with highlight AT 0000.
           MOVE KO-BUDAT TO WZ-DATUM.
           GO F.
      *-----------------------------------------> neues Buchungsdatum <-
       E.  DISPLAY "<ret>= Datum, <esc>= Abbruch" AT 2301.
           MOVE KO-BUDAT TO WZ-DATUM WH-WERT.
           CALL "CAUP" USING "1102386006" WH-CREG.
           IF ESC GO X.
           IF not RET GO E.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
       F.  MOVE WE-PER TO WC-DATUM.
           IF WZ-JAHR not = WC-TAG OR WZ-MONAT not = WC-MONAT
               DISPLAY "Datumsfehler" with BLINK highlight AT 2401
               PERFORM WEITER
               MOVE KO-BUDAT TO WC-DATUM
               PERFORM DATDREH
               ADD 238 VDU-ECK GIVING VDU-LP
               DISPLAY VDU-DATUM with highlight AT VDU-LP
               GO E.
           MOVE WX-DATUM TO BZ-BUDAT KO-BUDAT WS-DATUM.
           REWRITE KO-SATZ.
       K.  ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "S:                H:" AT VDU-LP.
           ADD KO-VST KO-SKTOAUF(1) KO-SKTOAUF(2) KO-SKTOAUF(3)
                      KO-SKTOAUF(4) KO-SKTOAUF(5) KO-SKTOAUF(6)
                            KO-JSOLL GIVING WD-SALDO WH-WERT.
           ADD 3 TO VDU-LP.
           DISPLAY WD-SALDO with highlight AT VDU-LP.
           ADD KO-MWST KO-SKTOERL KO-KRDIF
                            KO-JHABEN GIVING WD-SALDO WK-BET.
           ADD 18 TO VDU-LP.
           DISPLAY WD-SALDO with highlight AT VDU-LP " Saldo: ".
           SUBTRACT WK-BET FROM WH-WERT GIVING WD-SALDO WK-BET.
           DISPLAY WD-SALDO with highlight AT 0000.
           IF WK-BET not = 0 ADD 510 VDU-ECK GIVING VDU-LP
              DISPLAY "Journalsaldo nicht null - Kein Abschlu· mîglich!"
                     with highlight AT VDU-LP.
           ADD 403 VDU-ECK GIVING VDU-LP.
           MOVE 0 TO WM-SEITE.
       Q.  ADD 403 VDU-ECK GIVING VDU-LP.
           IF WK-BET not = 0
               DISPLAY "Weiter mit beliebiger Taste! < >" AT VDU-LP
           else DISPLAY "Journalabschlu· durchfÅhren? < >, <esc>= nein,
      -             "<Ende>= ja, <>= Datum" AT VDU-LP.
           CALL "CAUP" USING "1004330000" WH-CREG.
           IF WOLI GO E.
           IF AB PERFORM JOUR-NUM;
                 IF WM-SEITE not = 0 ADD 265 VDU-ECK GIVING VDU-LP
                      DISPLAY WM-SEITE with highlight AT VDU-LP
                 else DISPLAY "     " AT VDU-LP
                 end-if GO Q.
           IF ESC GO X.
           IF EINF GO R.
           IF not ENDE GO Q.
           IF WK-BET not = 0 GO X.
       R.  DISPLAY ALL SPACES with size 50 at 2501.
           ADD 503 VDU-ECK GIVING VDU-LP.
           CALL "CAUP" USING "1300000570" WH-CREG.
      *    MOVE WH-DATUM TO WM-DATUM WZ-DATUM.
           INITIALIZE BZ-SATZ.
      *    MOVE "000111000111220122222222211000000222" TO WT-SH.
           MOVE "00011100011122012222222221100000020101" TO WT-SH.
      *------------------------------------------> Aufbuchen je Konto <-
           ADD 503 VDU-ECK GIVING VDU-LP.
           DISPLAY "Konto: " AT VDU-LP.
           ADD 8 TO VDU-LP.
           PERFORM VARYING WS FROM 38 BY -1 UNTIL WS = 0
               IF KO-RS(WS) not = 0;
                  IF WS < 17 OR > 25 PERFORM SAMBUCH.
           REWRITE KO-SATZ.
           PERFORM JOURDRUCK.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           UNLOCK KONSTANT.
           UNLOCK BUCHZEIL.
       Z.  EXIT.
      ****************************************** manuelle Journalseite *
       JOUR-NUM SECTION.
       A.  CALL "CAUP" USING "0715100325000" WH-CREG.
       C.  ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "Journalseite:      " AT VDU-LP.
           DISPLAY "<ret>= Seite, <esc>= Abbruch" AT 2301.
           CALL "CAUP" USING "1002174004" WH-CREG.
           IF ESC GO X.
           IF not RET OR WH-NUM = 0 GO C.
           MOVE WH-NUM TO WK-SEITE WM-SEITE.
           MOVE 0 TO WK-ZEIL.
           MOVE WE-WKZ TO BZ-USE.
           MOVE WK-JS TO BZ-SEITE.
           START BUCHZEIL KEY not < BZ-JKEY INVALID GO X.
           MOVE SPACE TO BZ-SATZ(14:).
       G.  READ BUCHZEIL NEXT IGNORE LOCK AT END GO X.
           MOVE BZ-SEITE TO WK-JS.
           IF WK-SEITE = WM-SEITE OR WE-WKZ not = BZ-USE
               DISPLAY "Bereits vorhanden" AT 2401
               PERFORM WEITER
               GO C.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      **************************************** autom. Journalbuchungen *
       SAMBUCH SECTION.
       A.  EVALUATE WS
               WHEN 1  MOVE WO-KTONR(11) TO SA-KTONR      *> Debit.  0 S
               WHEN 2  MOVE WO-KTONR(12) TO SA-KTONR      *> Debit.  1 S
               WHEN 3  MOVE WO-KTONR(13) TO SA-KTONR      *> Debit.  2 S
               WHEN 4  MOVE WO-KTONR(11) TO SA-KTONR      *> Debit.  0 H
               WHEN 5  MOVE WO-KTONR(12) TO SA-KTONR      *> Debit.  1 H
               WHEN 6  MOVE WO-KTONR(13) TO SA-KTONR      *> Debit.  2 H
               WHEN 7  MOVE WO-KTONR(14) TO SA-KTONR      *> Kredit. 0 S
               WHEN 8  MOVE WO-KTONR(15) TO SA-KTONR      *> Kredit. 1 S
               WHEN 9  MOVE WO-KTONR(16) TO SA-KTONR      *> Kredit. 2 S
               WHEN 10 MOVE WO-KTONR(14) TO SA-KTONR      *> Kredit. 0 H
               WHEN 11 MOVE WO-KTONR(15) TO SA-KTONR      *> Kredit. 1 H
               WHEN 12 MOVE WO-KTONR(16) TO SA-KTONR      *> Kredit. 2 H
               WHEN 15 MOVE WO-KTONR(01) TO SA-KTONR      *> Vorsteuer S
               WHEN 16 MOVE WO-KTONR(02) TO SA-KTONR      *> Mehrwst.  H
               WHEN 26 MOVE WO-KTONR(03) TO SA-KTONR      *> Sktoerl.  H
               WHEN 27 MOVE WO-KTONR(04) TO SA-KTONR      *> Sktoerl.  H
               WHEN 28 MOVE WO-KTONR(05) TO SA-KTONR      *> Sktoaufw. S
               WHEN 29 MOVE WO-KTONR(06) TO SA-KTONR      *> Sktoaufw. S
               WHEN 30 MOVE WO-KTONR(07) TO SA-KTONR      *> Sktoaufw. S
               WHEN 31 MOVE WO-KTONR(08) TO SA-KTONR      *> Sktoaufw. S
               WHEN 32 MOVE WO-KTONR(09) TO SA-KTONR      *> Sktoaufw. S
               WHEN 33 MOVE WO-KTONR(10) TO SA-KTONR      *> Sktoaufw. S
               WHEN 34 ADD KO-SKTOVST TO KO-VST
                       ADD KO-SKTOVST TO KO-SKTOERL GO Z
               WHEN 35 MOVE WO-KTONR(17) TO SA-KTONR      *> Erwerbst. S
               WHEN 36 MOVE WO-KTONR(18) TO SA-KTONR      *> Erwerbst. H
               WHEN 37 MOVE WO-KTONR(19) TO SA-KTONR      *> ustfreie  S
               WHEN 38 MOVE WO-KTONR(20) TO SA-KTONR      *> ustfreie  H
               WHEN OTHER GO W.
           INITIALIZE BZ-SATZ.
           MOVE SA-KTONR TO BZ-KTONR WH-NUM.
           PERFORM LIESKTO.
           DIVIDE 10 INTO SA-KTONR GIVING WD-KTO.
           DISPLAY WD-KTO with highlight AT VDU-LP.
           MOVE KO-BUDAT TO BZ-BUDAT BZ-BELDAT.
           MOVE 0 TO BZ-LFD BZ-MW BZ-VM BZ-OPLFD.
           MOVE 999999 TO BZ-GGKTO.
           MOVE 19 TO BZ-SYM.
           MOVE WR-SH(WS) TO BZ-SH.
           MOVE KO-RS(WS) TO BZ-BUBET.
           IF WS > 27 and < 35 ADD WS -27 GIVING BZ-U1
               MOVE WT-UST(BZ-U1) TO WH-UST
               DIVIDE 100 INTO WH-UST GIVING WC-UST
               ADD 1 TO WC-UST
               DIVIDE WC-UST INTO BZ-BUBET GIVING BZ-B1
               ADD -1 TO WC-UST
               COMPUTE BZ-MW ROUNDED = WC-UST * BZ-B1
               SUBTRACT BZ-MW FROM BZ-BUBET
               MOVE 1 TO BZ-VM
               SUBTRACT BZ-MW FROM KO-MWST GO G.
           IF WS > 25 and < 35 SUBTRACT KO-SKTOVST FROM BZ-MW
                      MOVE 0 TO KO-SKTOVST
                      IF BZ-MW not = 0 MOVE 2 TO BZ-VM.
       G.  MOVE 2 TO BZ-Z.
           IF WS < 13 MOVE 3 TO BZ-Z.
           MOVE WE-USE TO BZ-USE.
           PERFORM BZ-WRITE.
           MOVE 0 TO BZ-MW BZ-VM.
           IF BZ-SH = 0 ADD BZ-BUBET TO SA-SOLL
                   ELSE ADD BZ-BUBET TO SA-HABEN.
           REWRITE SA-SATZ.
       W.  MOVE 0 TO KO-RS(WS).
       Z.  EXIT.
      **************************************** Buchungszeile speichern *
       BZ-WRITE SECTION.
       A.  PERFORM VARYING WS-BUCH FROM 115 BY -1 UNTIL WS-BUCH = 75
               OR BZ-SATZ(WS-BUCH:1) not = SPACE CONTINUE.
           MOVE BZ-SATZ TO WH-BZSATZ.
           MOVE BZ-KEY TO WH-BKEY.
           MOVE 900 TO BZ-LFD.
           START BUCHZEIL KEY < BZ-KEY INVALID
               MOVE 1 TO WH-LFD GO E.
       C.  READ BUCHZEIL PREVIOUS WITH NO LOCK AT END
               MOVE 1 TO WH-LFD GO E.
           IF ZUGRIF PERFORM BESETZT GO C.
           ADD 1 BZ-LFD TO WH-LFD.
           IF BZ-KTONR not = WH-KTONR OR BZ-BUDAT not = WH-BUDAT
               MOVE 1 TO WH-LFD.
       E.  MOVE WH-BZSATZ TO BZ-SATZ.
           MOVE WH-BKEY TO BZ-KEY.
           MOVE WS-BUCH TO WZ-BUCH.
           ADD 1 TO WH-JS.
           MOVE WH-JS TO BZ-SEITE.
           WRITE BZ-SATZ.
       Z.  EXIT.
      ******************************************************************
       LAST-JOUR SECTION.
       A.  MOVE 1 TO IX.
           MOVE 999999 TO BZ-SEITE.
           MOVE WE-USE TO BZ-USE.
           START BUCHZEIL KEY < BZ-JKEY INVALID MOVE 99 TO BZ-USE GO C.
           MOVE SPACE TO BZ-SATZ(14:).
           READ BUCHZEIL PREVIOUS NO LOCK AT END MOVE 99 TO BZ-USE GO C.
       C.  IF WE-USE = BZ-USE MOVE BZ-SEITE TO WH-JS GO Z.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "Keine Journaldaten vorhanden!" with highlight
               AT VDU-LP " < > weiter mit <ret>".
           CALL "CAUP" USING "1003340000" WH-CREG.
           SET ESC TO TRUE.
       Z.  EXIT.
      ******************************************************************
       FIRST-JOUR SECTION.
       A.  MOVE WE-USE TO BZ-USE.
           MOVE 0 TO BZ-SEITE.
           START BUCHZEIL KEY not < BZ-JKEY INVALID
               MOVE 0 TO BZ-SEITE GO E.
           MOVE SPACE TO BZ-SATZ(14:).
       C.  READ BUCHZEIL NEXT NO LOCK AT END MOVE 0 TO BZ-SEITE.
           IF ZUGRIF PERFORM BESETZT GO C.
       E.  IF BZ-USE not = WE-USE SET ESC TO TRUE GO Z.
       Z.  EXIT.
      ****************************** suchen der aktuellen Journalseite *
       JOUR-SUCH SECTION.
       A.  MOVE WE-WKZ TO BZ-USE.
           MOVE 8000000 TO BZ-SEITE.
           MOVE 0 TO WK-SEITE.
           START BUCHZEIL KEY <= BZ-JKEY INVALID GO X.
           MOVE SPACE TO BZ-TX.
       C.  READ BUCHZEIL PREVIOUS AT END GO X.
           IF ZUGRIF PERFORM BESETZT GO C.
           IF WM-SEITE not = 0
               ADD WM-SEITE -1 GIVING WK-SEITE GO X.
           IF BZ-USE = WE-WKZ MOVE BZ-SEITE TO WK-JS.
       X.  MOVE 0 TO WK-ZEIL.
       Z.  EXIT.
      *************************************************** Journaldruck *
       JOURDRUCK SECTION.
       A.  ADD 511 VDU-ECK GIVING VDU-LP.
           MOVE 0 TO WK-SOLL WK-HABEN.
           MOVE 0 TO WV-DATUM WZ-ZEILEN.
           PERFORM JOUR-SUCH.
           MOVE 1 TO WH-KEY.
       C.  READ KONSTANT INVALID STOP RUN.
           IF ZUGRIF PERFORM BESETZT GO C.
           PERFORM FIRST-JOUR.
           MOVE BZ-JKEY TO WH-JOKEY.
       E.  MOVE WH-JOKEY TO BZ-JKEY.
           START BUCHZEIL KEY not < BZ-JKEY INVALID GO Q.
           MOVE SPACE TO BZ-TX.
       G.  READ BUCHZEIL NEXT AT END GO Q.
           IF ZUGRIF PERFORM BESETZT GO G.
           IF BZ-USE not = WE-USE GO Q.
           MOVE BZ-JKEY TO WH-JOKEY.
           PERFORM JOURKOPF.
           IF WV-DATUM not = BZ-BUDAT MOVE BZ-BUDAT TO WC-DATUM WV-DATUM
               PERFORM DATDREH
               IF WZ-ZEILEN = WH-SIZE ADD 1 TO WZ-ZEILEN
                   PERFORM JOURKOPF
               end-if
               MOVE "Buchungsdatum:" TO DRA-STR(1:)
               MOVE VDU-DATUM TO DRA-STR(16:10)
               PERFORM DRUCK.
           MOVE WE-WKZ TO BZ-USE.
           ADD 1 TO WK-ZEIL.
           MOVE WK-JS TO BZ-SEITE.
           DIVIDE 10 INTO BZ-KTONR GIVING DRA-KTONR.
           MOVE BZ-BELNR TO DRA-BELNR.
           MOVE BZ-BELDAT TO WC-DATUM.
           PERFORM DATDREH.
           MOVE WZ-DATUM TO DRA-BELDAT.
           MOVE BZ-REFNR TO DRA-REFNR.
           MOVE WE-SYM(BZ-SYM + 1) TO DRA-SYM.
           MOVE BZ-TX TO DRA-TX.
      *----------------------------------
      *    IF BZ-B1 = -0,01 OR BZ-B1 = 0,01 OR BZ-MW = -0,01
      *       OR BZ-B2 = -0,01 OR BZ-B2 = 0,01 OR BZ-MW = 0,01
      *       CALL "CAUP" USING "0023010000" WH-CREG.
      *----------------------------------
           IF BZ-Z = 0 GO H.
           IF BZ-Z = 4 AND BZ-GGKTO = 0
                MOVE " *auft*" TO DRA-GGKTO(2:) GO H.
           IF BZ-GGKTO > 999997 MOVE "sabu" TO DRA-GGKTO(2:)
                MOVE BZ-KTONR TO WH-NUM
                PERFORM LIESKTO
                MOVE SA-TX(1) TO DRA-TX
           else DIVIDE 10 INTO BZ-GGKTO GIVING DRA-GGKTO
                MOVE BZ-TX TO DRA-TX.
       H.  EVALUATE BZ-VM
               WHEN 0 CONTINUE
               WHEN 1 IF BZ-U1 not = 0 MOVE WT-UST(BZ-U1) TO DRA-UST
                                       MOVE "m" TO DRA-UST(3:)
                                       MOVE BZ-MW TO DRA-MWSKT
               WHEN 2 IF BZ-U1 not = 0 MOVE WT-UST(BZ-U1) TO DRA-UST
                      end-if IF BZ-MW not = 0 MOVE "v" TO DRA-UST(3:)
                                       MOVE BZ-MW TO DRA-MWSKT
               WHEN 3 IF BZ-MW not = 0 or BZ-B1 not = 0 or BZ-B2 not = 0
                          MOVE "Ske" TO DRA-UST(1:)
                          ADD BZ-MW BZ-B1 BZ-B2 GIVING WH-WERT
                          ADD WH-WERT TO WK-SOLL
                          MOVE WH-WERT TO DRA-MWSKT
               WHEN 4 IF BZ-MW not = 0 or BZ-B1 not = 0 or BZ-B2 not = 0
                          MOVE "Ska" TO DRA-UST(1:)
                          ADD BZ-MW BZ-B1 BZ-B2 GIVING WH-WERT
                          ADD WH-WERT TO WK-HABEN
                          MOVE WH-WERT TO DRA-MWSKT
               WHEN 5 IF BZ-U1 not = 0 MOVE WT-UST(BZ-U1) TO DRA-UST
                      end-if IF BZ-MW not = 0 MOVE "i" TO DRA-UST(3:)
                                       MOVE BZ-MW TO DRA-MWSKT
               WHEN 6 IF BZ-U1 not = 0 MOVE WT-UST(BZ-U1) TO DRA-UST
                                       MOVE "n" TO DRA-UST(3:)
                                       MOVE BZ-MW TO DRA-MWSKT
               WHEN 9 IF BZ-U1 not = 0 MOVE WT-UST(BZ-U1) TO DRA-UST
                                       MOVE "f" TO DRA-UST(3:)
                                       MOVE BZ-MW TO DRA-MWSKT.
           IF BZ-SH = 0 MOVE BZ-BUBET TO DRA-SOLL
                        IF BZ-Z not = 3 ADD BZ-BUBET TO WK-SOLL
                        ELSE CONTINUE
           ELSE MOVE BZ-BUBET TO DRA-HABEN
                IF BZ-Z not = 3 ADD BZ-BUBET TO WK-HABEN.
           DISPLAY DRA-STR(1:40) with highlight AT VDU-LP.
           PERFORM DRUCK.
           REWRITE BZ-SATZ.
      *-----------------------------------> Zinsendatei bei Debitoren <-
           GO E.
       Q.  MOVE ALL "ƒ" TO DRA-STR.
           PERFORM DRUCK.
           MOVE "Journalsummen" TO DRA-TX.
           MOVE WK-SOLL TO DRA-SOLL.
           MOVE WK-HABEN TO DRA-HABEN.
           PERFORM DRUCK.
           SUBTRACT WK-HABEN FROM WK-SOLL GIVING WH-WERT.
           IF WH-WERT not = 0 MOVE "Differenz" TO DRA-TX
               IF WH-WERT > 0 MOVE WH-WERT TO DRA-HABEN
                         else MOVE WH-WERT TO DRA-SOLL
               end-if PERFORM DRUCK.
           MOVE WK-SEITE TO KO-JONUM.
           REWRITE KO-SATZ.
           PERFORM MWST-BMG.
           MOVE 1 TO WH-KEY.
       S.  READ KONSTANT INVALID STOP RUN.
           IF ZUGRIF PERFORM BESETZT GO S.
           MOVE WK-SEITE TO KO-JONUM.
           REWRITE KO-SATZ.
           PERFORM END-DRU.
       Z.  EXIT.
      **************************************************** Journalkopf *
       JOURKOPF SECTION.
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
                        IF WM-DRU = 1 MOVE 17 TO WH-P1         *> Laser
                            MOVE 0 TO WZ-SCHALT.
           PERFORM BEG-DRU.
           MOVE 0 TO WZ-SCHALT.
           IF WM-DRU not = 1 MOVE 1 TO WZ-SCHALT.
           MOVE WK-FIRMA TO DRA-STR(1:30).
           MOVE "J o u r n a l   per:" TO DRA-STR(36:).
           MOVE BZ-BUDAT TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRA-STR(57:10).
           MOVE "Seite:" TO DRA-STR(97:).
           ADD 1 TO WK-SEITE.
           MOVE 0 TO WK-ZEIL.
           MOVE WK-SEITE TO WD-NUM.
           MOVE WD-NUM TO DRA-STR(103:).
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRA-STR.
           PERFORM DRUCK.
           MOVE "Konto-Nr Bel-Nr Bel-Dat. Ref.Nr Sy Geg.Kto Buchungstext
      -       "                             Kz  Ust/Skto(s)  Umsatz Soll
      -       "   Umsatz Haben " TO DRA-STR(1:).
           PERFORM DRUCK.
           MOVE ALL "ƒ" TO DRA-STR.
           PERFORM DRUCK.
       Z.  EXIT.
      ******************************************************************
       MWST-BMG SECTION.
       A.  ADD WE-USE 1 GIVING WH-KEY.
           READ KONSTANT INVALID STOP RUN.
           IF ZUGRIF PERFORM BESETZT GO A.
           INITIALIZE BZ-SATZ.
           MOVE 1 TO WS.
       C.  IF KO-MWSTBMG(WS) = 0 AND WS > 1 GO Q.
           PERFORM JOURKOPF.
           ADD WS 9990,1 GIVING WH-WERT.
           MOVE WH-WERT TO DRA-KTONR.
           MULTIPLY 10 BY WH-WERT GIVING SA-KTONR.
       E.  READ SACHBUCH INVALID PERFORM SACHANL.
           IF ZUGRIF PERFORM BESETZT GO E.
           MOVE 0 TO BZ-BUBET.
           ADD KO-MWSTBMG(WS) TO SA-SOLL BZ-BUBET.
           REWRITE SA-SATZ INVALID WRITE SA-SATZ.
           MOVE WS-DATUM TO DRA-BELDAT BZ-BELDAT BZ-BUDAT.
           MOVE SA-KTONR TO BZ-GGKTO.
           MOVE "sabu" TO DRA-GGKTO(2:).
           MOVE "Ust-Bemessungsgrundlagen" TO BZ-TX.
           MOVE BZ-BUBET TO DRA-SOLL.
      *-------------------------------------> Journalseite einstellen <-
           MOVE 19 TO BZ-SYM.
           MOVE 0 TO BZ-SH BZ-OPLFD.
           MOVE WE-WKZ TO BZ-USE.
           MOVE WK-JS TO BZ-SEITE WH-JS.
           ADD 1 TO WK-ZEIL.
           MOVE SA-KTONR TO BZ-KTONR.
           MOVE 2 TO BZ-Z.
           PERFORM BZ-WRITE.
           PERFORM DRUCK.
           MOVE 0 TO KO-MWSTBMG(WS).
       Q.  IF WS < 9 ADD 1 TO WS GO C.
           MOVE 0 TO KO-BUDAT.             *> entschd. f. Per.Wechsel <-
           REWRITE KO-SATZ.
       Z.  EXIT.
      ************************************** automat. BMG-Kontenanlage *
       SACHANL SECTION.
       A.  MOVE SA-KTONR TO WH-NUM.
           INITIALIZE SA-SATZ.
           MOVE WH-NUM TO SA-KTONR.
           MOVE "MWST-BMG Ust-Kz." TO SA-TX(1).
           MOVE WS TO WD-KZ.
           MOVE WD-KZ TO SA-TX(1)(18:1).
           MOVE "autom. au·erb. Sammelkonto" TO SA-TX(2).
           MOVE "U-BMG- " TO SA-MCODE.
           MOVE WD-KZ TO SA-MCODE(7:1).
           MOVE SA-MCODE TO WT-TX.
           CALL "CAUP" USING "20CONV08" WH-CREG.
           MOVE WT-TX TO SA-MCODE.
           MOVE 9 TO SA-KTOART.
           WRITE SA-SATZ.
       Z.  EXIT.
      ******************************************************************
       LIESKTO SECTION.
       A.  SET RET TO TRUE.
           IF WH-NUM = 0 GO X.
           MOVE WH-NUM TO WD-KZ.
           MOVE WD-KZ TO WH-KAT.
           DIVIDE 10 INTO WH-NUM GIVING WD-KTO.
           EVALUATE WD-KZ
               WHEN 3 MOVE WH-NUM TO KR-KTONR GO G
               WHEN 2 MOVE WH-NUM TO DE-KTONR GO E
               WHEN 1 SET ESC TO TRUE GO X
               WHEN 0 MOVE WH-NUM TO SA-KTONR GO C
               WHEN OTHER GO X.
      *---------------------------------------------> lesen Sachkonto <-
       C.  READ SACHBUCH not INVALID MOVE SA-BEZ TO WT-TX
               MOVE SA-MCODE TO WK-MCODE
               MOVE SA-BTNT TO WH-BN
               MOVE SA-SOLL TO WE-SOLL
               MOVE SA-HABEN TO WE-HABEN
               ADD WE-SOLL WE-HABEN GIVING WE-SALDO GO Z.
           IF ZUGRIF PERFORM BESETZT GO C.
           INITIALIZE SA-SATZ.
           MOVE WH-NUM TO SA-KTONR.
           GO X.
      *---------------------------------------------> lesen Debitoren <-
       E.  READ DEBITOR not INVALID MOVE DE-BEZ TO WT-TX
               INSPECT WT-TX REPLACING ALL "#" BY ","
               MOVE DE-MCODE TO WK-MCODE
               MOVE 0 TO WH-BN
               MOVE DE-SOLL TO WE-SOLL
               MOVE DE-HABEN TO WE-HABEN
               MOVE DE-MAHNT TO WM-MT
               ADD WE-SOLL WE-HABEN GIVING WE-SALDO GO Z.
           IF ZUGRIF PERFORM BESETZT GO E.
           INITIALIZE DE-SATZ.
           MOVE WH-NUM TO DE-KTONR.
           MOVE 10 TO WM-MT.
           GO X.
      *--------------------------------------------> lesen Kreditoren <-
       G.  READ KREDIT not INVALID MOVE KR-BEZ TO WT-TX
               MOVE KR-MCODE TO WK-MCODE
               MOVE 0 TO WH-BN
               MOVE KR-SOLL TO WE-SOLL
               MOVE KR-HABEN TO WE-HABEN
               ADD WE-SOLL WE-HABEN GIVING WE-SALDO GO Z.
           IF ZUGRIF PERFORM BESETZT GO G.
           INITIALIZE KR-SATZ.
           MOVE WH-NUM TO KR-KTONR.
       X.  SET FEHLER TO TRUE.
           MOVE "Konto fehlt" TO WT-TX.
       Z.  EXIT.
      ******************************************************************
       UEBERNAHME SECTION.
       A.  CALL "CAUP" USING "1303012580" WH-CREG.
           CALL "CAUP" USING "0707111248000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Fibu-öbernahmen " with highlight AT VDU-LP.
           ADD 308 VDU-ECK GIVING VDU-LP.
           DISPLAY "1 - A-Rechnungen" AT VDU-LP.
           ADD 1 TO VDU-L.
           DISPLAY "2 - Zahlungen"  AT VDU-LP.
           ADD 1 TO VDU-L.
           DISPLAY "3 - E-Rechnung" AT VDU-LP.
           ADD 1 TO VDU-L.
           DISPLAY "4 - BankeinzÅge" AT VDU-LP.
           ADD 1 TO VDU-L.
           DISPLAY "5 - KassaÅbernahme" AT VDU-LP.
           ADD 1 TO VDU-L.
           DISPLAY "<ret-leer>= Programmende" AT VDU-LP.
           ADD 215 TO VDU-LP.
           DISPLAY "bitte wÑhlen Sie:" with highlight AT VDU-LP.
       C.  CALL "CAUP" USING "1110411001" WH-CREG.
           IF ESC MOVE 0 TO WH-NUM
               SET RET TO TRUE.
           MOVE WH-NUM TO WH-PG.
           IF not RET GO C.
           CALL "CAUP" USING "08CLOFEN" WH-CREG.
           EVALUATE WH-PG
               WHEN 0 GO Z
               WHEN 1
               WHEN 3 PERFORM FREMDBUCH
               WHEN 2
               WHEN 4 PERFORM ZAHLUNGEN
               WHEN 5 PERFORM KASSAUEB.
           GO A.
       Z.  EXIT.
      ************************************************** SymbolprÅfung *
       SYM-PRUEF SECTION.
       A.  MOVE 99 TO WM-ARSYM WM-GSSYM WM-ZASYM.
           PERFORM VARYING BZ-SYM FROM 0 BY 1 UNTIL BZ-SYM > 19
               MOVE WE-SKZ(BZ-SYM + 1) TO WE-SYMT
               EVALUATE WE-DIV
                   WHEN 1 MOVE BZ-SYM TO WM-ARSYM
                   WHEN 2 MOVE BZ-SYM TO WM-GSSYM
                   WHEN 3 MOVE BZ-SYM TO WM-ZASYM.
       Z.  EXIT.
      **************** öbernahme aus Fakturierung - sihe Kopfanmerkung *
       FREMDBUCH SECTION.
       A.  MOVE " BuchungsÅbernahme" TO WK-GEB.
           CALL "CAUP" USING "06KOPF" WH-CREG.
           PERFORM SYM-PRUEF.
           IF WN-BUEB = SPACE DISPLAY "Stammdaten Pkt. 16 nicht angelegt
      -        " " AT 2401 PERFORM WEITER GO Z.
           IF ESC GO Z.
           IF WM-ARSYM = 99 OR WM-GSSYM = 99
               DISPLAY " öbernahmesymbole NICHT angelegt "
                   with reverse-video AT 1010 PERFORM WEITER GO Z.
           MOVE 10 TO WH-KEY.
           READ KONSTANT IGNORE LOCK INVALID STOP RUN.
           MOVE KO-TR-SR(3) TO WK-USTKTO.
           MOVE WE-USE TO WM-USE.
           CALL "FIBUMW" USING "10UMWAND" WH-CREG
                                          on overflow next sentence.
           CANCEL "FIBUMW".
           DISPLAY "Alle Buchung bis inkl.          werden Åbernommen."
               AT 0810.
           MOVE WH-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           DISPLAY VDU-DATUM with highlight AT 0833.
           PERFORM OB-FREI.                       *> ob User 7 frei ist
           IF ESC GO Z.
       B.  MOVE WH-DATUM TO WZ-DATUM WH-WERT.
           DISPLAY "<esc>= Abbruch, <ret>= Datum" AT 2301.
           DISPLAY "Datum = Buchungsdatum" with highlight AT 2401.
           CALL "CAUP" USING "0108336006" WH-CREG.
           IF ESC GO Z.
           IF not RET GO B.
           IF WZ-DATUM = 0 GO B.
           INITIALIZE BZ-SATZ.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           MOVE WX-DATUM TO WS-DATUM.
      *------------------------> PrÅfung auf  periodenreine öbernahme <-
           IF WV-FPER = 0 COMPUTE WV-FPER = WS-DATUM / 100.
           COMPUTE WM-FPER = WS-DATUM / 100.
           IF WM-FPER not = WV-FPER DISPLAY "nicht mîglich!" AT 2501
                                    GO B.
           PERFORM WI-PRUEF
               IF ESC GO Z.
           OPEN I-O BUCHUNG.
           IF WF-STATUS >= "09" GO X.
           MOVE WS-DATUM TO WE-PER.
           PERFORM KTO-UPDATE.                 *> Kontenwartung aus FAKT
      *---------------------> erst Kontrolle ob alle Konten vorhanden <-
           PERFORM KONTPRUEF.
           IF WM-L = 9 GO X.
           MOVE 7 TO WE-USE WM-L.
       C.  DISPLAY "<esc>= Abbruch, <Ende>= Start öbernahme" AT 2301.
           CALL "CAUP" USING "0023410000" WH-CREG.
           IF ESC GO Y.
           IF not ENDE GO C.
           MOVE 0 TO BU-KEY.
           MOVE 0 TO WK-SKTO WK-MWST WK-BASIS WK-SOLL WK-HABEN
                     WK-BRUTTO WK-NETTO WK-BUBET BZ-SEITE.
           START BUCHUNG KEY > BU-KEY INVALID GO X.
       E.  INITIALIZE BU-SATZ.
           READ BUCHUNG NEXT AT END GO Q.
           IF ZUGRIF PERFORM BESETZT GO E.
           IF BU-DAT > WS-DATUM GO E.
           ADD 19000000 BU-DAT GIVING WV-DATUM.
           IF BU-DAT < 900000 ADD 1000000 TO WV-DATUM.
           IF WV-DATUM > WS-DATUM GO E.
           IF BU-SA = "K" GO E.
           COMPUTE WM-FPER = WV-DATUM / 100.
           IF WM-FPER not = WV-FPER DISPLAY "nicht mîglich!" AT 2501
                                    GO B.
           MOVE BU-KTONR TO WH-NUM WD-KZ.
           EVALUATE WH-PG
               WHEN 1; IF WD-KZ = 3 GO E
               WHEN 3; IF WD-KZ = 2 GO E.
           IF WD-KZ = 0;
               EVALUATE WH-PG
                   WHEN 1; IF BU-SH > 4 GO E
                   WHEN 3; IF BU-SH < 5 GO E
                           else ADD -5 TO BU-SH.
           PERFORM LIESKTO.
           IF FEHLER GO E.
           UNLOCK SACHBUCH.
           UNLOCK KREDIT.
           UNLOCK DEBITOR.
           MOVE WH-KAT TO WH-AKAT.
           MOVE WH-BN TO WH-ABN.
           DIVIDE 10 INTO BU-KTONR GIVING WD-KTO.
           DISPLAY WD-KTO AT 1020.
           MOVE BU-KTONR TO BZ-KTONR WK-KTONR.
           MOVE WS-DATUM TO BZ-BUDAT.                 *> öbernahmedatum
           IF BU-VALDAT(1:) = SPACE MOVE 0 TO BU-VALDAT.
           MOVE 0 TO BZ-LFD.
           MOVE WV-DATUM TO BZ-BELDAT.
           MOVE BU-REN TO BZ-BELNR.
           MOVE BU-RF TO BZ-REFNR.
           MOVE BU-SY TO BZ-SYM.                     *> kommt aus FAKT
           IF BZ-SYM > 49 ADD -50 TO BZ-SYM.         *> Merker ohne OP
           MOVE BU-SH TO BZ-SH.
           MOVE BU-GK TO BZ-GGKTO.
           IF BU-FSYM = LOW-VALUE MOVE SPACE TO BU-FSYM.  *> spÑter weg
           MOVE 0 TO BZ-KSTNR BZ-KURS BZ-OPLFD.
           MOVE BU-Z TO BZ-Z.                        *> Gegenbuchungsart
           IF BZ-Z = 7 ADD -5 TO BZ-Z.
           MOVE BU-VM TO BZ-VM.                      *> Steuermerker
      *-------------------------------------> auf Eurobuchung umbauen <-
           IF BU-FSYM = SPACE;              *> fÅr nicht geÑnderte Fakt
               IF BZ-BUDAT < WE-EURO MOVE "Ats" TO BU-FSYM
                                else MOVE "Eur" TO BU-FSYM.
           IF BZ-BUDAT < WE-EURO;
              EVALUATE BU-FSYM
                 WHEN "Eur" MOVE 1376,03 TO BU-KURS
                 WHEN "Ats" MOVE 100,000 TO BU-KURS
                            IF BU-FWBET = 0 MOVE BU-BET TO BU-FWBET.
           IF BZ-BUDAT not < WE-EURO;
              EVALUATE BU-FSYM
                 WHEN "Eur" MOVE 100,00000 TO BU-KURS
                 WHEN "Ats" MOVE 7,2672835 TO BU-KURS.
      *                     IF WH-PG = 3 MOVE BU-BET TO BU-FWBET
      *                        COMPUTE BU-BET rounded = BU-BET / 13,7603
      *                        COMPUTE BU-B1 rounded = BU-B1 / 13,7603
      *                        COMPUTE BU-B2 rounded = BU-B2 / 13,7603.
      *------------------------------------> Mwst. wg. Umrechnung neu <-
       F.  IF BU-VM not = 9;
               IF BU-MW not = 0 COMPUTE BU-MW = BU-BET - BU-B1 - BU-B2.
           MOVE BU-FSYM TO BZ-FSYM.
           MOVE BU-KURS TO BZ-KURS.
           MOVE BU-FWBET TO BZ-FWBET.
           MOVE BU-BET TO BZ-BUBET WK-BUBET.
           MOVE BU-MW TO BZ-MW WK-MWST.
           MOVE BU-U1 TO BZ-U1.
           MOVE BU-U2 TO BZ-U2.
           MOVE BU-B1 TO BZ-B1.
           MOVE BU-B2 TO BZ-B2.
           MOVE BU-KOND TO BZ-KOND.
           MOVE BU-TX TO BZ-TX.
           MOVE WE-SKZ(BZ-SYM + 1) TO WE-SYMT.
           ADD WE-USE 1 GIVING WH-KEY.
       G.  READ KONSTANT INVALID INITIALIZE KO-BSATZ
               MOVE WH-KEY TO KO-NUM WRITE KO-SATZ GO G.
           IF ZUGRIF PERFORM BESETZT GO G.
           MOVE WS-DATUM TO KO-BUDAT.
           MOVE DE-OPKZ TO WH-OP.
           IF WH-OP = 0 AND BU-SY < 50 PERFORM MACHOP.
           MOVE BZ-BUBET TO WK-BRUTTO WK-NETTO.
           SUBTRACT WK-MWST FROM WK-NETTO.
           MOVE LOW-VALUES TO AZ-SATZ.
           IF BZ-Z = 5 MOVE 2 TO BZ-Z.
           PERFORM BZ-BUCH.
           EVALUATE WH-AKAT
               WHEN 0 PERFORM SACHSUM
               WHEN 2 PERFORM DEBSUM
               WHEN 3 PERFORM KREDSUM.
120103     IF BU-VM = 9 MOVE 8 TO WE-UST.          *> bei AR nicht erf.
           PERFORM ADD-UST.
           MOVE BU-GK TO WH-NUM WK-KTONR.
           IF BU-Z = 5 MOVE 0 TO BZ-Z.
           IF BU-VM = 9 MOVE 1 TO BZ-Z
               MOVE WK-BRUTTO TO WK-BUBET WK-NETTO
               MOVE DE-BEZ TO BZ-TX
               INSPECT BZ-TX REPLACING ALL "#" BY ",".
           MOVE BZ-Z TO WM-GB.
           EVALUATE TRUE
               WHEN EINZEL PERFORM GEGENBUCH
                           IF not ESC ADD 1 TO IX
                                      PERFORM BZ-BUCH
               WHEN SAMMEL PERFORM SAMMELBUCH.
           IF BU-VM = 6 PERFORM USTBUCH.
      *--------------------------------> RÅckschreiben Speichersummen <-
           REWRITE KO-SATZ.
           DELETE BUCHUNG INVALID GO X.
      *-----------------------------------> Zinsendatei bei Debitoren <-
           GO E.
       Q.  PERFORM JOUREND.
           GO Y.
       X.  DISPLAY " Keine öbernhame mîglich " with reverse-video
               AT 2401 PERFORM WEITER.
       Y.  CLOSE BUCHUNG.
           MOVE WM-USE TO WE-USE.
       Z.  EXIT.
      ************************** Ust-Einzelgegebuchung wg. Teilrchnung *
       USTBUCH SECTION.
       A.  IF WK-USTKTO = 0 GO Z.
           MOVE 0 TO BZ-SH.
           MOVE WK-USTKTO TO BZ-GGKTO.
           MOVE BU-KTONR TO BZ-KTONR.
           MOVE BU-MW TO WK-BRUTTO WK-BUBET WK-NETTO.
           PERFORM GEGENBUCH.
           PERFORM BZ-BUCH.
       Z.  EXIT.
      ******************************************* ob User "7" frei ist *
       OB-FREI SECTION.
       A.  MOVE 1 TO IX.
           MOVE 999999 TO BZ-SEITE.
           MOVE 7 TO BZ-USE.
           IF WH-PG = 5 MOVE WE-KUEB TO BZ-USE
                        MOVE BZ-USE TO WM-USE.
           SET RET TO TRUE.
           MOVE 0 TO WM-FPER WV-FPER WK-JS.
           START BUCHZEIL KEY < BZ-JKEY INVALID MOVE 99 TO BZ-USE GO C.
           MOVE SPACE TO BZ-SATZ(14:).
           READ BUCHZEIL PREVIOUS NO LOCK AT END MOVE 99 TO BZ-USE GO C.
       C.  IF BZ-USE not = WE-KUEB GO Z.
           COMPUTE WV-FPER = BZ-BUDAT / 100.
           IF BZ-USE not = 7 DISPLAY "Alte Buchung im User vorhanden!"
                             with highlight foreground-color 6 AT 2501
                             MOVE BZ-SEITE TO WK-JS
                             GO Z.
           DISPLAY "Alte öbernahmebuchung vorhanden!" with highlight
               AT 2301 " < > weiter mit <ret>".
           DISPLAY "Bitte User 7 anwÑhlen und Journalabschlu· durchfÅhre
      -        "n!" with highlight AT 2401.
           IF WH-PG = 5 DISPLAY "<Einfg>= Buchung anfÅgen!" AT 2501.
           CALL "CAUP" USING "0023350000" WH-CREG.
           IF SEIN or EINF GO Z.
           SET ESC TO TRUE.
       Z.  EXIT.
      ********************************************** Erstellen OP-Satz *
       MACHOP SECTION.
       A.  IF WH-AKAT < 2 GO Z.
           INITIALIZE OP-SATZ.
           MOVE BZ-BELNR TO BZ-REFNR OP-REF OP-RENUM.
           MOVE BZ-BELDAT TO OP-VALDAT OP-REDAT.
           IF BU-VALDAT not = 0 ADD 19000000 BU-VALDAT GIVING OP-VALDAT
               IF BU-VALDAT < 900000 ADD 1000000 TO OP-VALDAT.
           MOVE BZ-SYM TO OP-SYM.
           MOVE BZ-FWBET TO OP-REBETRAG.           *> OP in FremdwÑhrg.
           IF BZ-FWBET = 0 MOVE BZ-BUBET TO OP-REBETRAG.
           MOVE BZ-KURS TO OP-KURS.
           MOVE BZ-FSYM TO OP-FSYM.
           MOVE BZ-U1 TO OP-U1.
           MOVE BZ-U2 TO OP-U2.
           MOVE BZ-B1 TO OP-B1.
           MOVE BZ-B2 TO OP-B2.
           MOVE BZ-MW TO OP-MW.
           MOVE BZ-KOND TO OP-KOND.
           MOVE BU-SKTOBAS TO OP-SKTOBAS.
           MOVE BZ-KTONR TO OP-KTONR.
           MOVE WH-AKAT TO OP-DK.
       W.  WRITE OP-SATZ INVALID ADD 1 TO OP-LFD GO W.
           IF ZUGRIF PERFORM BESETZT GO W.
           MOVE OP-LFD TO BZ-OPLFD.
       Z.  EXIT.
      ********************************************** Erstellen OP-Satz *
       ZAHL-OP SECTION.
       A.  IF WH-AKAT < 2 GO Z.
           MOVE 0 TO BZ-BUBET.
           ADD -5 TO BZ-Z.                         *> OP-Ausgl. / Kassa
           INITIALIZE OP-SATZ.
           MOVE WD-KZ TO OP-DK.
           MOVE DE-KTONR TO OP-KTONR.
           MOVE BU-VALDAT TO OP-VALDAT.
           IF BU-VALDAT not = 0 ADD 19000000 BU-VALDAT GIVING OP-VALDAT
               IF BU-VALDAT < 900000 ADD 1000000 TO OP-VALDAT.
           START OPDATEI KEY >= OP-KEY INVALID GO X.
       C.  READ OPDATEI NEXT AT END GO X.
           IF ZUGRIF PERFORM BESETZT GO C.
           IF DE-KTONR not = OP-KTONR or OP-DK not = WD-KZ GO X.
           IF OP-RENUM not = BU-RF GO C.
           MOVE OP-RENUM TO WD-NUM.
           DISPLAY " " AT 0000 WD-NUM.
           if bu-skto > 9 move 0 to bu-skto.
           EVALUATE BU-SKTO
               WHEN 0                              *> keine Differenz
               WHEN 1 PERFORM KAS-SKTO             *> Skontoausgleich
               WHEN 5 GO X.                        *> ohne OP-Verarb.
           MOVE BU-DAT TO OP-ZADAT.
       W.  REWRITE OP-SATZ INVALID ADD 1 TO OP-LFD GO W.
           IF ZUGRIF PERFORM BESETZT GO W.
           MOVE OP-LFD TO BZ-OPLFD.
       X.  UNLOCK OPDATEI.
       Z.  EXIT.
      ************************************* Skonto bei Kassa-öbernahme *
       KAS-SKTO SECTION.
       A.  MOVE 0 TO BZ-OPLFD.
           MOVE 0 TO WI WK-SKTO.
           MOVE 1 TO WO.
           MOVE OP-KURS TO BZ-KURS.
           MOVE OP-FSYM TO BZ-FSYM.
           MOVE BU-BET TO BZ-FWBET WM-ZABET.
      *------------------------------------------> Skonto = Differenz <-
           COMPUTE WP-SKTO = OP-REBETRAG - OP-ZABETRAG - WM-ZABET.
           IF BU-SKTO = 0 MOVE 0 TO WP-SKTO.             *> kein Skonto
           ADD WP-SKTO TO WK-SKTO.
           MOVE WP-SKTO TO WE-SKONTO(1).
           ADD BZ-FWBET TO WS-FWSAML.
           ADD WM-ZABET TO BZ-BUBET WS-SAMMEL.
           MOVE WP-SKTO TO ZP-SKTO.
           MOVE BU-BET TO ZP-BETRAG.
           PERFORM AUS-OP.
       Z.  EXIT.
      *************************************************** Addition UST *
       ADD-UST SECTION.
       A.  EVALUATE WE-UST
               WHEN 1 ADD WK-MWST TO KO-MWST
                    IF BZ-U1 not = 0 ADD BZ-B1 TO KO-MWSTBMG(BZ-U1)
                    end-if
                    IF BZ-U2 not = 0 ADD BZ-B2 TO KO-MWSTBMG(BZ-U2)
               WHEN 2 ADD WK-MWST TO KO-VST
               WHEN 9 ADD WK-MWST TO KO-FREIUST(1) KO-FREIUST(2).
           MOVE 0 TO WK-MWST WK-BASIS.
       Z.  EXIT.
      ******************************* Summenbildung Sachkonten Kat.= 0 *
       SACHSUM SECTION.
       A.  MOVE WK-KTONR TO SA-KTONR.
           IF WM-L = 0 GO C.
           READ SACHBUCH INVALID PERFORM KTO-FEHL.
           IF ZUGRIF PERFORM BESETZT GO A.
       C.  EVALUATE BZ-SH ALSO WH-ABN
               WHEN 0 ALSO 1 ADD WK-NETTO TO KO-JSOLL
                             ADD WK-NETTO TO SA-SOLL
               WHEN 0 ALSO 0 ADD WK-BUBET TO KO-JSOLL
                             ADD WK-BRUTTO TO SA-SOLL
               WHEN 1 ALSO 1 ADD WK-NETTO TO KO-JHABEN
                             ADD WK-NETTO TO SA-HABEN
               WHEN 1 ALSO 0 ADD WK-BUBET TO KO-JHABEN
                             ADD WK-BRUTTO TO SA-HABEN.
           IF WE-SYM(BZ-SYM + 1) =
                 "eb" OR "EB" OR "Eb" ADD BZ-BUBET TO SA-EBIL.
           REWRITE SA-SATZ.
       Z.  EXIT.
      ******************************** Summenbildung Debitoren Kat.= 2 *
       DEBSUM SECTION.
       A.  MOVE WK-KTONR TO DE-KTONR.
           IF WM-L = 0 GO C.
           READ DEBITOR INVALID PERFORM KTO-FEHL.
           IF ZUGRIF PERFORM BESETZT GO A.
       C.  COMPUTE WL = DE-KTOART + 1 + (BZ-SH * 3).
           ADD BZ-BUBET TO KO-DEBSUM(WL).
           ADD WK-SKTO  TO KO-DEBSUM(WL).
           IF BZ-SH = 0 ADD BZ-BUBET TO KO-JSOLL
                        ADD BZ-BUBET TO DE-SOLL
                   ELSE ADD BZ-BUBET TO KO-JHABEN
                        ADD BZ-BUBET TO DE-HABEN
                        ADD WK-SKTO TO KO-JHABEN
                        ADD WK-SKTO TO DE-HABEN.
           MOVE WE-SKZ(BZ-SYM + 1) TO WE-SYMT.
           IF WE-DIV = 1 or 2 ADD BZ-BUBET TO DE-UMSATZ
                              SUBTRACT BZ-MW FROM DE-UMSATZ.
           REWRITE DE-SATZ.
       Z.  EXIT.
      ******************************* Summenbildung Kreditoren Kat.= 2 *
       KREDSUM SECTION.
       A.  MOVE WK-KTONR TO KR-KTONR.
           IF WM-L = 0 GO C.
           READ KREDIT INVALID PERFORM KTO-FEHL.
           IF ZUGRIF PERFORM BESETZT GO A.
       C.  COMPUTE WL = KR-KTOART + 1 + (BZ-SH * 3).
           ADD BZ-BUBET TO KO-KREDSUM(WL).
           ADD WK-SKTO  TO KO-KREDSUM(WL).
           IF BZ-SH = 0 ADD BZ-BUBET TO KO-JSOLL
                        ADD BZ-BUBET TO KR-SOLL
                        ADD WK-SKTO TO KO-JSOLL
                        ADD WK-SKTO TO KR-SOLL
                   ELSE ADD BZ-BUBET TO KO-JHABEN
                        ADD BZ-BUBET TO KR-HABEN.
           REWRITE KR-SATZ.
       Z.  EXIT.
      ******************************************************************
       KTO-FEHL SECTION.
       A.  DISPLAY "Konto fehlt oder fehlerhaft" AT 2401.
           PERFORM WEITER.
           STOP RUN.
       Z.  EXIT.
      ******************************************************************
       KTO-UPDATE SECTION.
       A.  MOVE 0 TO BU-REN.
           MOVE "K" TO BU-SA.
           START BUCHUNG KEY not < BU-RKEY INVALID GO Z.
       C.  UNLOCK SACHBUCH.
           UNLOCK KREDIT.
           UNLOCK DEBITOR.
           READ BUCHUNG NEXT AT END GO Z.
           IF ZUGRIF PERFORM BESETZT GO C.
           IF BU-SA not = "K" GO Z.
           MOVE BU-KTONR TO WH-NUM WD-KZ.
           EVALUATE WH-PG
               WHEN 1; IF WD-KZ = 3 GO C
               WHEN 3; IF WD-KZ = 2 GO C.
           MOVE 0 TO WM-MT.
           PERFORM LIESKTO.
           DISPLAY WD-KTO AT 0542.
           IF WH-KAT = 0 GO C.
      *------------------------------------> Konto mu· angelegt weden <-
           EVALUATE WH-KAT
               WHEN 0 GO C
               WHEN 3 MOVE BU-KTONR TO KR-KTONR
                      MOVE BU-BEZ TO KR-BEZ
                      MOVE BU-MCODE TO KR-MCODE
                      MOVE BU-KOND TO KR-KOND
                      WRITE KR-SATZ INVALID REWRITE KR-SATZ
               WHEN 2 MOVE BU-KTONR TO DE-KTONR
                      MOVE BU-BEZ TO DE-BEZ
                      MOVE BU-ANREDE TO DE-ANREDE
                      MOVE BU-MCODE TO DE-MCODE
                      MOVE BU-KOND TO DE-KOND
                      MOVE WM-MT TO DE-MAHNT
                      MOVE BU-TEL TO DE-TEL
                      WRITE DE-SATZ INVALID REWRITE DE-SATZ.
       H.  DELETE BUCHUNG INVALID GO C.
           GO C.
       Z.  EXIT.
      ******************************************************************
       KONTPRUEF SECTION.
       A.  CALL "CAUP" USING "270610056000011" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Bitte warten " AT VDU-LP.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY " KontenprÅfung: " with highlight reverse-video
                AT VDU-LP.
           MOVE 0 TO WM-L.
           INITIALIZE BU-SATZ.
           MOVE WE-PER TO WC-DATUM.
           START BUCHUNG KEY not < BU-RKEY INVALID MOVE 9 TO WM-L GO X.
       C.  UNLOCK SACHBUCH.
           UNLOCK KREDIT.
           UNLOCK DEBITOR.
           READ BUCHUNG NEXT AT END GO X.
           IF ZUGRIF PERFORM BESETZT GO C.
           IF WH-PG = 5 and BU-DAT not = WC-DATUM GO C.
           IF BU-SA not = " " and not = "S" GO X.
           MOVE BU-KTONR TO WH-NUM WD-KZ.
           IF WH-PG = 3 and WD-KZ not = 3 GO C.
           PERFORM LIESKTO.
           DIVIDE 10 INTO BU-KTONR GIVING WD-KTO.
           ADD 320 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-KTO AT VDU-LP.
           IF WD-KZ = 2 and WH-PG = 5 PERFORM OP-PRUEF
              IF ESC MOVE 9 TO WM-L GO X
              else GO C.
           ADD 319 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-KTO AT VDU-LP.
           IF FEHLER PERFORM ERGAENZ
              IF RET GO C
              else MOVE 9 TO WM-L
                   GO X.
           UNLOCK SACHBUCH.
           UNLOCK KREDIT.
           UNLOCK DEBITOR.
           IF BU-GK not = 0 and not = 999999
               MOVE BU-GK TO WH-NUM
               PERFORM LIESKTO
               DIVIDE 10 INTO BU-GK GIVING WD-KTO
               ADD 335 VDU-ECK GIVING VDU-LP
               DISPLAY "Gegenkto: " AT VDU-LP WD-KTO
               IF FEHLER GO W.
           GO C.
       W.  ADD 403 VDU-ECK GIVING VDU-LP.
           DISPLAY "Konto:           nicht vorhanden - bitte anlegen!"
               AT VDU-LP.
           ADD 410 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-KTO with highlight AT VDU-LP.
           DISPLAY " Bitte vormerken " with reverse-video AT 2401.
           PERFORM WEITER.
           ADD 403 VDU-ECK GIVING VDU-LP.
           DISPLAY ALL SPACE with SIZE 56 AT VDU-LP.
           MOVE 9 TO WM-L.
           GO C.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      **************************************** fehlende Daten ergÑnzen *
       ERGAENZ SECTION.
       A.  CALL "CAUP" USING "270810106000012" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " ErgÑnzungen " with highlight AT VDU-LP.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "Text....: " AT VDU-LP.
           DISPLAY BU-TX with highlight AT 0000.
           ADD 403 VDU-ECK GIVING VDU-LP.
           DISPLAY "Kto.-Nr.: " AT VDU-LP.
           DIVIDE 10 INTO BU-KTONR GIVING WD-KTO.
           DISPLAY WD-KTO with highlight AT 0000.
           ADD 503 VDU-ECK GIVING VDU-LP.
           DISPLAY "Bel.-Nr.: " AT VDU-LP.
           MOVE BU-REN TO WD-NR.
           DISPLAY WD-NR with highlight AT 0000.
           ADD 603 VDU-ECK GIVING VDU-LP.
           DISPLAY "Betrag..: " AT VDU-LP.
           MOVE BU-BET TO WD-BET.
           DISPLAY WD-BET with highlight AT 0000
           IF BU-MW = 0
               DISPLAY "inkl. event. UST" with highlight AT 0000
           else DISPLAY "exkl. Ust" with highlight AT 0000.
           ADD 703 VDU-ECK GIVING VDU-LP.
           DISPLAY "Kz.Ust-1:  /   %" AT VDU-LP.
           IF BU-U1 not = 0 MOVE WT-UST(BU-U1) TO WH-UST WD-POS
               MOVE BU-U1 TO WD-KZ
               ADD 10 TO VDU-LP
               DISPLAY WD-KZ AT VDU-LP "/ " WD-POS
               ADD 725 VDU-ECK GIVING VDU-LP
               MOVE BU-B2 TO WH-WERT WD-BET
               DISPLAY "Bmg: " AT VDU-LP WD-BET with highlight.
           ADD 803 VDU-ECK GIVING VDU-LP.
           DISPLAY "Kz.Ust-2:  /   %" AT VDU-LP.
           IF BU-U2 not = 0 MOVE WT-UST(BU-U2) TO WH-UST WD-POS
               MOVE BU-U2 TO WD-KZ
               ADD 10 TO VDU-LP
               DISPLAY WD-KZ AT VDU-LP "/ " WD-POS
               ADD 825 VDU-ECK GIVING VDU-LP
               MOVE BU-B2 TO WH-WERT WD-BET
               DISPLAY "Bmg: " AT VDU-LP WD-BET with highlight.
           GO D.
       C.  DISPLAY "<esc>= Abbruch, <ret>= Text" AT 2301.
           MOVE BU-TX TO WT-TX.
           CALL "CAUP" USING "1203130126" WH-CREG.
           IF ESC GO X.
           IF not RET GO C.
           MOVE WT-TX TO BU-TX.
           DISPLAY BU-TX with highlight AT VDU-LP.
       D.  DISPLAY "<esc>= Abbruch, <ret>= Kto.-Nr., alpha+Kat+<ret>= su
      -        "chen" AT 2301.
           DISPLAY "Konto-Nr. ohne ," AT 2401.
           COMPUTE WH-WERT = BU-KTONR.
           CALL "CAUP" USING "1004136006" WH-CREG.
           IF ESC GO X.
           IF WOLI or AUF GO C.
           IF not RET GO D.
           IF WH-NUM = 0 and WH-MCODE not = SPACE
               CALL "FIBANZ" USING "99SUCH" WH-CREG
               CANCEL "FIBANZ"
               MOVE WH-NUM TO WH-WERT.
           IF WH-WERT = 0 GO D.
           MOVE WH-WERT TO BU-KTONR WH-NUM.
           COMPUTE WD-KTO = WH-WERT / 10.
           DISPLAY WD-KTO with highlight AT VDU-LP " ".
           PERFORM LIESKTO.
           IF FEHLER GO D.
           DISPLAY SA-BEZ(1:35) with highlight AT 0000.
       E.  DISPLAY "<esc>= Abbruch, <ret>= Beleg-Nr., < />= zurÅck"
               AT 2301.
           MOVE BU-REN TO WH-WERT.
           CALL "CAUP" USING "1005136006" WH-CREG.
           IF ESC GO X.
           IF WOLI or AUF GO D.
           IF not RET GO E.
           IF WH-WERT = 0 GO E.
           MOVE WH-WERT TO BU-REN WD-NR.
           DISPLAY WD-NR with highlight AT VDU-LP.
       G.  DISPLAY "<esc>= Abbruch, <ret>= Ust-Kz., < />= zurÅck"
               AT 2301.
           MOVE BU-U1 TO WH-WERT.
           CALL "CAUP" USING "1007136006" WH-CREG.
           IF ESC GO X.
           IF WOLI or AUF GO E.
           IF not RET GO G.
           IF WH-WERT > 4 GO G.
           MOVE WH-WERT TO BU-U1 WD-KZ.
           IF BU-VM = 2 GO K.
           IF BU-U1 not = 0 MOVE WT-UST(BU-U1) TO WD-POS WH-UST
               DISPLAY WD-KZ AT VDU-LP "/ " WD-POS "%"
               DIVIDE 100 INTO WH-UST GIVING WC-UST
               ADD 1 TO WC-UST
               DIVIDE WC-UST INTO BU-BET GIVING BU-B1
               ADD -1 TO WC-UST
               COMPUTE BU-MW ROUNDED = WC-UST * BU-B1
               SUBTRACT BU-MW FROM BU-BET GIVING BZ-MW
               MOVE 1 TO BU-VM
           else DISPLAY " " AT VDU-LP "/    ".
       I.  DISPLAY "<esc>= Abbruch, <ret>= Ust-Bmg., < />= zurÅck"
               AT 2301.
           MOVE BU-B1 TO WH-WERT WD-BET.
           ADD 725 VDU-ECK GIVING VDU-LP.
           DISPLAY "Bmg: " AT VDU-LP WD-BET with highlight.
           CALL "CAUP" USING "1007306208" WH-CREG.
           IF ESC GO X.
           IF WOLI or AUF GO G.
           IF not RET GO I.
           IF BU-U1 not = 0 and WH-WERT = 0
               DISPLAY "Ust-Basis mu· sein!" with highlight AT 2401
               GO I.
           MOVE WH-WERT TO BU-B1 WD-BET.
           DISPLAY WD-BET with highlight AT VDU-LP.
       K.  DISPLAY "<esc>= Abbruch, <ret>= Ust-Kz., < />= zurÅck"
               AT 2301.
           MOVE BU-B2 TO WH-WERT.
           CALL "CAUP" USING "1008136006" WH-CREG.
           IF ESC GO X.
           IF WOLI or AUF GO G.
           IF not RET GO I.
           IF WH-WERT > 4 GO I.
           MOVE WH-WERT TO BU-U2 WD-KZ.
           IF BU-VM = 2 GO O.
           IF BU-U2 not = 0 MOVE WT-UST(BU-U2) TO WD-POS
               DISPLAY WD-KZ AT VDU-LP "/ " WD-POS "%"
               MOVE WT-UST(BZ-U2) TO WH-UST
               DIVIDE 100 INTO WH-UST GIVING WC-UST
               ADD 1 TO WC-UST
               COMPUTE BU-B2 = BU-BET - BU-B1 - BU-MW
               DIVIDE WC-UST INTO BU-B2 GIVING BU-B2
               ADD -1 TO WC-UST
               COMPUTE BU-MW ROUNDED = BU-MW + (WC-UST * BU-B2)
               MOVE 1 TO BU-VM
           else DISPLAY " " AT VDU-LP "/    ".
       M.  DISPLAY "<esc>= Abbruch, <ret>= Ust-Bmg., < />= zurÅck"
               AT 2301.
           MOVE BU-B2 TO WH-WERT WD-BET.
           ADD 825 VDU-ECK GIVING VDU-LP.
           DISPLAY "Bmg: " AT VDU-LP WD-BET with highlight.
           CALL "CAUP" USING "1008306208" WH-CREG.
           IF ESC GO X.
           IF WOLI or AUF GO I.
           IF not RET GO M.
           IF BU-U2 not = 0 and WH-WERT = 0
               DISPLAY "Ust-Basis mu· sein!" with highlight AT 2401
               GO M.
           MOVE WH-WERT TO BU-B2 WD-BET.
           DISPLAY WD-BET with highlight AT VDU-LP.
       O.  DISPLAY "<esc>= Abbruch, <ret>= speichern, < >= zurÅck < >"
               AT 2301.
           CALL "CAUP" USING "0023480000" WH-CREG.
           IF ESC GO X.
           IF WOLI or AUF GO K.
           IF not RET GO O.
           REWRITE BU-SATZ.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ***************************************************** Op-PrÅfung *
       OP-PRUEF SECTION.
       A.  IF WD-KZ < 2 GO Z.
           INITIALIZE OP-SATZ.
           MOVE WD-KZ TO OP-DK.
       B.  MOVE DE-KTONR TO OP-KTONR.
           MOVE BU-VALDAT TO OP-VALDAT.
           IF BU-VALDAT not = 0 ADD 19000000 BU-VALDAT GIVING OP-VALDAT
               IF BU-VALDAT < 900000 ADD 1000000 TO OP-VALDAT.
           START OPDATEI KEY >= OP-KEY INVALID GO Z.
       C.  READ OPDATEI NEXT IGNORE LOCK AT END GO Z.
       D.  IF DE-KTONR not = OP-KTONR or OP-DK not = WD-KZ
               CALL "CAUP" USING "271105057000010" WH-CREG
               ADD 103 VDU-ECK GIVING VDU-LP
               DISPLAY " Op-Diffenrenz " with highlight AT VDU-LP
               ADD 203 VDU-ECK GIVING VDU-LP
               MOVE BU-RF TO WD-NR
               DISPLAY "Re-Nr.: " AT VDU-LP WD-NR with highlight " "
                   "Kunde: " WD-KTO with highlight " " DE-BEZ(1:30)
               MOVE BU-BET TO WD-BET
               ADD 303 VDU-ECK GIVING VDU-LP
               DISPLAY "Zahlung..: " AT VDU-LP WD-BET with highlight
               ADD 403 VDU-ECK GIVING VDU-LP
               DISPLAY "Offener Posten nicht vorhanden! <      >"
                   AT VDU-LP
               DISPLAY "<esc>= Abruch, <ret>= Re-Nr., <#>= OpÔs "
                   AT 2301
               DISPLAY "< >= ohne OP-Verarbeitung, <Einfg>= Konto-Nr."
                   AT 2401
               CALL "CAUP" USING "1004366107" WH-CREG
               CALL "CAUP" USING "08CLOFEN" WH-CREG
               EVALUATE TRUE
                   WHEN ESC GO X
                   WHEN AB MOVE 5 TO BU-SKTO          *> ohne OP-Verarb.
                           GO Q
                   WHEN EINF MOVE WH-WERT TO DE-KTONR
                             PERFORM LIESKTO
                             MOVE DE-KTONR TO BU-KTONR
                             REWRITE BU-SATZ
                             end-rewrite
                             GO B
                   WHEN RET MOVE WH-WERT TO BU-RF
                            REWRITE BU-SATZ
                            end-rewrite
                   WHEN KIST MOVE BU-KTONR TO WK-KTONR
                             CALL "FIBOPES" USING "15OPANZ" WH-CREG
                             CANCEL "FIBOPES"
                             IF WH-NUM = 0 GO D
                             else MOVE OP-RENUM TO BU-RF
                                  REWRITE BU-SATZ
                                  end-rewrite
                                  GO B
                   WHEN OTHER GO D.
           IF OP-RENUM not = BU-RF GO C.
           COMPUTE WH-WERT = OP-ZABETRAG + BU-BET - OP-REBETRAG.
           IF WH-WERT = 0 GO Z.
      *----------------------------------------------> Op-Differenzen <-
           CALL "CAUP" USING "271105047000010" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Op-Diffenrenz " with highlight AT VDU-LP.
           ADD 203 VDU-ECK GIVING VDU-LP.
           MOVE BU-RF TO WD-NR.
           DISPLAY "Re-Nr.: " AT VDU-LP WD-NR with highlight " "
               "Kunde: " WD-KTO with highlight " " DE-BEZ(1:30).
           ADD 303 VDU-ECK GIVING VDU-LP.
           MOVE OP-REBETRAG TO WD-BET.
           DISPLAY "OP-Betrag: " AT VDU-LP WD-BET with highlight
                " Zahlung: ".
           COMPUTE WD-BET = OP-ZABETRAG + BU-BET.
           DISPLAY WD-BET with highlight AT 0000 " Saldo: ".
           MOVE WH-WERT TO WD-BET.
           DISPLAY WD-BET with highlight AT 0000.
           MOVE 0 TO BU-SKTO.
       E.  DISPLAY "<esc>= Abbruch, <ret>= Skonto, < >= Diffenrenz als R
      -        "est < >" AT 2301.
           CALL "CAUP" USING "0023580000" WH-CREG.
           EVALUATE TRUE
           WHEN ESC GO X
                WHEN AB MOVE 0 TO BU-SKTO            *> kein Skto.
                WHEN RET MOVE 1 TO BU-SKTO           *> Skonto
                WHEN OTHER GO E.
       Q.  REWRITE BU-SATZ.
       X.  UNLOCK OPDATEI.
      *    CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ********************************************* Einzelgegenbuchung *
       GEGENBUCH SECTION.
       A.  IF BZ-SH = 0 MOVE 1 TO BZ-SH
                   ELSE MOVE 0 TO BZ-SH.
           MOVE BZ-GGKTO TO WH-NUM.
           PERFORM LIESKTO.
           IF FEHLER PERFORM NO-REC GO A.
           MOVE WH-KAT TO WH-AKAT.
           IF WH-KAT < 2 MOVE 0 TO BZ-KOND.
           MOVE WH-BN TO WH-ABN.
           MOVE WH-NUM TO WK-KTONR.
           MOVE BZ-KTONR TO BZ-GGKTO.
           MOVE WK-KTONR TO BZ-KTONR.
           IF WH-ABN = 0 MOVE WK-BRUTTO TO BZ-BUBET
                    else MOVE WK-NETTO TO BZ-BUBET WK-BUBET
                         ADD WK-NETTO WK-MWST GIVING WK-BRUTTO.
           MOVE 0 TO BZ-U1 BZ-B1 BZ-U2 BZ-B2 BZ-MW.
           EVALUATE WH-AKAT
               WHEN 0 PERFORM SACHSUM
               WHEN 2 PERFORM DEBSUM
               WHEN 3 PERFORM KREDSUM.
           PERFORM ADD-UST.
           IF BU-VM not = 6 and not = 9 MOVE "öbgb." TO BZ-TX.
       Z.  EXIT.
      ******************************** automatische Sammelgegenbuchung *
       SAMMELBUCH SECTION.
       A.  MOVE BZ-GGKTO TO WH-NUM.
           PERFORM LIESKTO.
           IF FEHLER PERFORM NO-REC GO A.
           MOVE WH-KAT TO WH-AKAT.
           MOVE WH-BN TO WH-ABN.
           MOVE WH-NUM TO WK-KTONR.
           MOVE 19 TO BZ-SYM.
           MOVE 999999   TO BZ-GGKTO.                   *> Sammelbuchung
           MOVE WK-KTONR TO BZ-KTONR.                  *> Gegenkonto-Nr.
           IF WH-ABN = 0 MOVE WK-BRUTTO TO BZ-BUBET
                    else MOVE WK-NETTO TO BZ-BUBET WK-BUBET
                         ADD WK-NETTO WK-MWST GIVING WK-BRUTTO.
           MOVE 0 TO BZ-U1 BZ-B1 BZ-U2 BZ-B2 BZ-MW BZ-VM.
           IF BZ-SH = 0 MOVE 1 TO BZ-SH
                        MOVE 900 TO BZ-LFD
                   ELSE MOVE 0 TO BZ-SH
                        MOVE 901 TO BZ-LFD.
           MOVE "öbsb." TO BZ-TX.
           ADD 1 TO BZ-SEITE.
           MOVE BZ-SEITE TO WH-JS.
       C.  MOVE BZ-SATZ TO WH-BZSATZ.
           MOVE SPACE TO BZ-SATZ(14:).
           READ BUCHZEIL INVALID MOVE WH-BZSATZ TO BZ-SATZ
               MOVE WK-BUBET TO BZ-BUBET GO E.
           IF ZUGRIF PERFORM BESETZT GO C.
           IF WE-USE not = BZ-USE MOVE WH-BZSATZ TO BZ-SATZ
               ADD -2 TO BZ-LFD GO C.
      *------------------------> Achtung auf keygleiche Sammelbuchung <-
           ADD WK-BUBET TO BZ-BUBET.
       E.  EVALUATE WH-AKAT
               WHEN 0 PERFORM SACHSUM
               WHEN 2 PERFORM DEBSUM
               WHEN 3 PERFORM KREDSUM.
           IF BZ-BUBET = 0 GO Q.
           MOVE 0 TO BZ-OPLFD BZ-BELNR BZ-REFNR.
           MOVE BZ-BUBET TO BZ-FWBET.
       F.  MOVE WH-JS TO BZ-SEITE.
           IF WL-CA = 50 ADD 2000 BZ-SEITE GIVING BZ-SEITE.
           MOVE WE-BELNR TO BZ-BELNR.
           IF WE-BELNR = 0 MOVE BU-REN TO BZ-BELNR.
           PERFORM VARYING WZ-BUCH FROM 115 BY -1 UNTIL WZ-BUCH = 75
               OR BZ-SATZ(WZ-BUCH:1) not = SPACE CONTINUE.
           WRITE BZ-SATZ INVALID REWRITE BZ-SATZ.
           IF KO-JOKEY = LOW-VALUE MOVE BZ-JKEY TO KO-JOKEY.
           GO X.
      *-----------------------------> lîschen wenn Sammelbuchung leer <-
       Q.  DELETE BUCHZEIL INVALID GO F.
           IF ZUGRIF PERFORM BESETZT GO Q.
       X.  UNLOCK BUCHZEIL.
       Z.  EXIT.
      **************************************** Buchungszeile speichern *
       BZ-BUCH SECTION.
       A.  MOVE WE-USE TO BZ-USE.
280106     if bz-seite = 0 move wk-js to bz-seite.
           ADD 1 TO BZ-SEITE.
           IF WL-CA = 50 and BZ-SEITE > 2000 ADD -2000 TO BZ-SEITE.
           PERFORM VARYING WS-BUCH FROM 115 BY -1 UNTIL WS-BUCH = 75
               OR BZ-SATZ(WS-BUCH:1) not = SPACE CONTINUE.
           IF KAS-ZAHL MOVE BZ-U1 TO BU-U1
                       MOVE BZ-U2 TO BU-U2
                       MOVE BZ-B1 TO BU-B1
                       MOVE BZ-B2 TO BU-B2
                       MOVE BZ-MW TO BU-MW
                       MOVE BZ-FWBET TO BU-FWBET
                       MOVE 0 TO BZ-BUBET BZ-MW BZ-B1 BZ-B2 BZ-FWBET.
           MOVE BZ-SATZ TO WH-BZSATZ.
           MOVE BZ-KEY TO WH-BKEY.
           MOVE 900 TO BZ-LFD.
           IF KAS-ZAHL ADD WK-LFD 1 GIVING BZ-LFD.
           START BUCHZEIL KEY < BZ-KEY INVALID MOVE 1 TO WH-LFD GO E.
       C.  READ BUCHZEIL PREVIOUS WITH NO LOCK AT END
               MOVE 1 TO WH-LFD GO E.
           IF ZUGRIF PERFORM BESETZT GO C.
           IF KAS-ZAHL GO G.
           ADD 1 BZ-LFD TO WH-LFD.
           IF BZ-KTONR NOT = WH-KTONR OR BZ-BUDAT NOT = WH-BUDAT
               MOVE 1 TO WH-LFD.
       E.  MOVE WH-BZSATZ TO BZ-SATZ.
           MOVE WH-BKEY TO BZ-KEY.
           MOVE WS-BUCH TO WZ-BUCH.
           WRITE BZ-SATZ INVALID GO A.
           IF AZ-SATZ not = LOW-VALUE AND BZ-OPLFD = 999
               PERFORM AZ-WRITE.
           IF KO-JOKEY = LOW-VALUE MOVE BZ-JKEY TO KO-JOKEY.
           GO Z.
      *-------------------------------> Behandlung der Kassazahlungen <-
       G.  IF BZ-KTONR not = WH-KTONR or
               BZ-BUDAT not = WH-BUDAT or BZ-LFD not = WK-LFD;
               PERFORM NEXT-JOUR
               MOVE WH-BZSATZ TO BZ-SATZ
               MOVE WK-JS TO BZ-SEITE
               MOVE BZ-SATZ TO WH-BZSATZ
               IF BZ-LFD > WK-LFD MOVE BZ-LFD TO WK-LFD
                             else MOVE WH-BZSATZ TO BZ-SATZ.
           MOVE WK-LFD TO WH-LFD.
           MOVE WH-BKEY TO BZ-KEY.
           MOVE WS-BUCH TO WZ-BUCH.
           ADD BU-BET TO BZ-BUBET.
           IF BZ-U1 = 0 or BZ-U1 = BU-U1 MOVE BU-U1 TO BZ-U1
           else IF BZ-U2 = 0 or BZ-U2 = BU-U1 MOVE BU-U1 TO BZ-U2.
           IF BZ-U2 = 0 or BZ-U2 = BU-U2 MOVE BU-U2 TO BZ-U2
           else IF BZ-U1 = 0 or BZ-U1 = BU-U2 MOVE BU-U2 TO BZ-U1.
           IF BZ-U1 = BU-U1 ADD BU-B1 TO BZ-B1.
           IF BZ-U2 = BU-U1 ADD BU-B1 TO BZ-B2.
           IF BZ-U1 = BU-U2 ADD BU-B2 TO BZ-B1.
           IF BZ-U2 = BU-U2 ADD BU-B2 TO BZ-B2.
           ADD BU-MW TO BZ-MW.
           ADD BU-FWBET TO BZ-FWBET.
           IF AZ-SATZ not = LOW-VALUE MOVE 999 TO BZ-OPLFD.
           WRITE BZ-SATZ INVALID REWRITE BZ-SATZ.
           MOVE BU-BET TO BZ-BUBET.
      *---------------------------------> abstellen in Ausziff.-Datei <-
           MOVE BZ-KEY TO AZ-TKEY.
       I.  READ AUSZIFF INVALID INITIALIZE AZ-SATZ
               MOVE BZ-KEY TO AZ-TKEY.
           IF ZUGRIF PERFORM BESETZT GO I.
           PERFORM VARYING WO FROM 1 BY 1 UNTIL WO > 50
               IF AZ-OPNR(WO) not = 0;
                   IF OP-RENUM = AZ-OPNR(WO) end-perform.
           IF WO < 50 GO K.
           PERFORM VARYING WO FROM 1 BY 1 UNTIL WO > 50
               or AZ-OPNR(WO) = 0 end-perform.
           IF WO > 50 GO X.
       K.  MOVE ZP-BETRAG TO AZ-OPBET(WO).
           MOVE ZP-SKTO TO AZ-OPSKTO(WO).
           MOVE OP-RENUM TO AZ-OPNR(WO).
           MOVE OP-SYM TO AZ-OPSYM(WO).
           PERFORM VARYING WZ-AUSZ FROM 50 BY -1 UNTIL WZ-AUSZ = 1
               OR AZ-RETAB(WZ-AUSZ) not = LOW-VALUE CONTINUE.
           COMPUTE WZ-AUSZ = WZ-AUSZ * 21 + 10.
           WRITE AZ-SATZ INVALID REWRITE AZ-SATZ.
           MOVE LOW-VALUE TO AZ-SATZ.
       X.  UNLOCK AUSZIFF.
       Z.  EXIT.
      ************************************* nÑchste Jounalseite suchen *
       NEXT-JOUR SECTION.
       A.  MOVE WE-USE TO BZ-USE.
           MOVE 8000000 TO BZ-SEITE.
           MOVE 0 TO WK-SEITE.
           START BUCHZEIL KEY <= BZ-JKEY INVALID GO X.
       C.  READ BUCHZEIL PREVIOUS AT END GO X.
           IF ZUGRIF PERFORM BESETZT GO C.
           MOVE BZ-SEITE TO WK-JS.
      *X.  ADD 1 TO WK-ZEIL.
       X.  ADD 1 TO WK-JS.
       Z.  EXIT.
      *********************************** Auszifferungsdatei schreiben *
       AZ-WRITE SECTION.
       A.  PERFORM VARYING WZ-AUSZ FROM 50 BY -1 UNTIL WZ-AUSZ = 1
               OR AZ-RETAB(WZ-AUSZ) not = LOW-VALUE CONTINUE.
           MOVE BZ-KEY TO AZ-TKEY.
           COMPUTE WZ-AUSZ = WZ-AUSZ * 21 + 10.
           WRITE AZ-SATZ INVALID REWRITE AZ-SATZ.
           MOVE LOW-VALUE TO AZ-SATZ.
           MOVE 0 TO BZ-OPLFD.
       Z.  EXIT.
      ******************************************* suchen öberweisungen *
       SUCHZAHL SECTION.
       A.  CALL "CAUP" USING "0702472232010" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           EVALUATE WH-PG
               WHEN 2 DISPLAY " vorhandene Zahlungen " AT VDU-LP
               WHEN 4 DISPLAY " vorhandene EinzÅge " AT VDU-LP.
           ADD 202 VDU-ECK GIVING VDU-LP.
           DISPLAY " Zl.  Za-Datum         Betrag " with reverse-video
               AT VDU-LP.
           ADD 505 VDU-ECK GIVING VDU-LP.
           DISPLAY "Bitte warten!" with highlight AT VDU-LP.
       B.  MOVE WV-BAKEY TO ZP-BAKEY.
           MOVE 0 TO ZP-KTONR ZP-VALDAT ZP-LFD.
           MOVE LOW-VALUES TO WO-TSATZ.
           START ZAHLPOS KEY not < ZP-AKEY INVALID
               SET ESC TO TRUE GO Z.
       C.  READ ZAHLPOS NEXT AT END MOVE 99 TO WM-L GO E.
           IF ZUGRIF PERFORM BESETZT GO C.
           IF ZP-BAKEY = 900
               DELETE ZAHLPOS
               end-delete
               ADD 1 TO ZP-BAKEY
               GO B.
           IF WV-BAKEY not = ZP-BAKEY GO C.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 19
               OR WO-DAT(WX) = 0 OR ZP-DATUM < WO-DAT(WX)
               OR WO-DAT(WX) = ZP-DATUM MOVE WX TO WY.
           IF WO-DAT(WX) = ZP-DATUM OR WO-DAT(WX) = 0
               MOVE ZP-DATUM TO WO-DAT(WX)
               ADD ZP-BETRAG TO WO-SUMM(WX) GO C.
           PERFORM VARYING WY FROM 19 BY -1 UNTIL WY = (WX - 1)
               MOVE WO-DAT(WY) TO WO-DAT(WY + 1)
               MOVE WO-SUMM(WY) TO WO-SUMM(WY + 1).
           MOVE ZP-DATUM TO WO-DAT(WX).
           MOVE ZP-BETRAG TO WO-SUMM(WX).
           GO C.
       E.  CALL "CAUP" USING "16CLRFEN" WH-CREG.
           PERFORM VARYING WY FROM 1 BY 1 UNTIL WY > 19
               OR WO-DAT(WY) = 0
               MOVE WY TO WD-POS
               COMPUTE VDU-LP = WY * 100 + 203 + VDU-ECK
               DISPLAY WD-POS with highlight AT VDU-LP
               ADD 5 TO VDU-LP
               MOVE WO-DAT(WY) TO WC-DATUM
               PERFORM DATDREH
               DISPLAY VDU-DATUM AT VDU-LP
               MOVE WO-SUMM(WY) TO WD-SALDO
               ADD 10 TO VDU-LP
               DISPLAY WD-SALDO with highlight AT VDU-LP.
       Z.  EXIT.
      ******************************************************************
       ZAHLUNGEN SECTION.
       A.  MOVE " ZahlungsÅbernahme" TO WK-GEB.
           CALL "CAUP" USING "06KOPF" WH-CREG.
           PERFORM SYM-PRUEF.
           IF WM-ZASYM = 99 DISPLAY " öbernahmesymbole NICHT angelegt "
                   with reverse-video AT 1010 PERFORM WEITER GO Z.
           MOVE WE-USE TO WM-USE.
           MOVE 7 TO WE-USE.
           OPEN I-O ZAHLPOS.
           DISPLAY "Zahlungsbuchung vom:" AT 0810
                   "Bank:   = Fibukonto-Nr:" AT 1010
                   "werden mit Åbernommen." AT 1310
                   "Belegnummer:         Belegdatum: " AT 1510.
           MOVE WH-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           DISPLAY VDU-DATUM with highlight AT 0831.
           PERFORM OB-FREI.
           IF ESC GO X.
      *-----------------------------------------------> welche Bank ? <-
       C.  DISPLAY "<esc>= Abbruch, <ret>= Nr. lt. Stammdatenanl."
               AT 2301.
           DISPLAY "<ret-leer>= Banken anzeigen" AT 2401.
           CALL "CAUP" USING "0010161001" WH-CREG.
           IF ESC GO X.
           IF RET AND WH-NUM = 0 CALL "FIBSTAM" USING "10BANK" WH-CREG
                                 CANCEL "FIBSTAM".
           IF not RET GO C.
           IF WH-NUM = 0 GO C.
           MOVE WH-NUM TO WD-KZ.
           DISPLAY WD-KZ with highlight AT VDU-LP.
           ADD WH-NUM 70 GIVING WH-KEY.
       D.  READ KONSTANT INVALID PERFORM NO-REC GO C.
           IF ZUGRIF PERFORM BESETZT GO C.
           IF KB-FIBKTO = 0 DISPLAY "Bankstammdaten (Pkt. 7) unvollstÑnd
      -        "ig" AT 2401 PERFORM WEITER GO C.
           MOVE KB-FIBKTO TO WE-GGKTO.
           IF WH-PG = 2 ADD 900 KB-NUM GIVING WV-BAKEY
                   else ADD 950 KB-NUM GIVING WV-BAKEY.
           EVALUATE KB-ART
               WHEN 0 MOVE 4 TO WM-UEANZ
               WHEN 1 MOVE 6 TO WM-UEANZ
               WHEN 2 MOVE 11 TO WM-UEANZ.
           DISPLAY KB-BEZ with highlight AT 1120.
           DIVIDE 10 INTO KB-FIBKTO GIVING WD-KTO.
           DISPLAY WD-KTO with highlight AT 1034.
           PERFORM SUCHZAHL.
      *-------------------------------------------------------> Datum <-
           IF WO-DAT(1) = 0
               DISPLAY "Keine Zahlungen offen" with highlight AT 2401
               PERFORM WEITER
               CALL "CAUP" USING "08CLOFEN" WH-CREG GO X.
           MOVE WO-DAT(1) TO WM-DATUM WC-DATUM.
           PERFORM DATDREH.
           DISPLAY VDU-DATUM with highlight AT 0831.
       E.  MOVE WM-DATUM TO WZ-DATUM WH-WERT.
           DISPLAY "<esc>= Abbruch, <ret>= Datum" AT 2301.
           CALL "CAUP" USING "0108316006" WH-CREG.
           IF ESC CALL "CAUP" USING "08CLOFEN" WH-CREG GO X.
           IF not RET GO E.
           IF WZ-DATUM = 0 GO E.
           MOVE WM-DATUM TO WS-DATUM WM-DATUM WE-PER.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 20
               OR WO-DAT(WX) = WM-DATUM CONTINUE.
           IF WX > 20 DISPLAY "Datum falsch" with highlight AT 2401
               PERFORM  WEITER GO E.
           CALL "CAUP" USING "08CLOFEN" WH-CREG.
           DISPLAY VDU-DATUM with highlight AT 0831.
      *-------------------------------------------------> Belegnummer <-
       F.  DISPLAY "<esc>= Abbruch, <ret>= Beleg-Nr." AT 2301.
           CALL "CAUP" USING "0015236006" WH-CREG.
           IF WOLI GO C.
           IF ESC GO X.
           IF not RET OR WH-NUM = 0 GO F.
           MOVE WH-WERT TO WD-NUM WE-BELNR.
           DISPLAY WD-NUM with highlight AT VDU-LP.
      *--------------------------------------------------> Belegdatum <-
       G.  DISPLAY "<esc>= Abbruch, <ret>= Belegdatum" AT 2301.
           MOVE WM-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           DISPLAY VDU-DATUM with highlight AT 1543
           MOVE WM-DATUM TO WZ-DATUM WH-WERT.
           CALL "CAUP" USING "0115436006" WH-CREG.
           IF WOLI GO F.
           IF ESC GO X.
           IF not RET GO G.
           IF WZ-DATUM = 0 GO G.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           MOVE WX-DATUM TO WS-DATUM WM-DATUM.
           PERFORM WI-PRUEF
               IF ESC GO X.
       H.  DISPLAY "<esc>= Abbruch, <ret>= Start Verbuchung" AT 2301.
           CALL "CAUP" USING "0023410000" WH-CREG.
           IF ESC GO X.
           IF WOLI GO G.
           IF not RET GO H.
           ADD WE-USE 1 GIVING WH-KEY.
       I.  READ KONSTANT INVALID INITIALIZE KO-BSATZ
               MOVE WH-KEY TO KO-NUM WRITE KO-SATZ GO I.
           IF ZUGRIF PERFORM BESETZT GO I.
           MOVE WS-DATUM TO KO-BUDAT.
           MOVE 1 TO WM-L.                   *> Konto mu· gelesen werden
           MOVE 0 TO WS-SAMMEL WS-FWSAML WH-JS.
       J.  MOVE WV-BAKEY TO ZP-BAKEY.
           MOVE 0 TO ZP-KTONR ZP-VALDAT ZP-LFD.
           START ZAHLPOS KEY not < ZP-AKEY INVALID GO X.
           INITIALIZE BZ-SATZ.
           MOVE LOW-VALUES TO AZ-SATZ.
           MOVE 0 TO BZ-OPLFD.
           MOVE 0 TO WI WK-SKTO.
           MOVE 1 TO WO.
       K.  READ ZAHLPOS NEXT AT END MOVE 99 TO WM-L GO Q.
           IF ZUGRIF PERFORM BESETZT GO K.
           IF ZP-BAKEY not = WV-BAKEY MOVE 99 TO WM-L GO Q.
           IF ZP-DATUM < 991232 and > 900000 ADD 19000000 TO ZP-DATUM.
           IF ZP-DATUM not = WM-DATUM GO K.
      *    add -900 to zp-bakey rewrite zp-satz go k.
           IF BZ-KTONR = 0 MOVE ZP-KTONR TO BZ-KTONR WK-KTONR.
           IF BZ-KTONR not = ZP-KTONR GO Q.
           MOVE ZP-KTONR TO WD-KZ.
           MOVE WD-KZ TO WH-KAT WH-AKAT.
           DIVIDE 10 INTO ZP-KTONR GIVING WD-KTO.
           DISPLAY "Konto-Nr.: " AT 1810 WD-KTO AT 0000.
      *----------------------------------------> Beginn der öbernahme <-
           MOVE ZP-KEY TO OP-KEY.
       M.  READ OPDATEI INVALID PERFORM OP-FEHL GO J.
           IF ZUGRIF PERFORM BESETZT GO M.
           MOVE ZP-KTONR TO BZ-KTONR.
           MOVE WS-DATUM TO BZ-BUDAT.
           MOVE WE-BELNR TO BZ-BELNR.
           MOVE ZP-DATUM TO BZ-BELDAT.
           MOVE 2 TO WM-GB BZ-Z.
           MOVE WE-GGKTO TO BZ-GGKTO.          *> aber als Sammelbuchng.
           MOVE WM-ZASYM TO BZ-SYM.
           MOVE WE-SKZ(BZ-SYM + 1) TO WE-SYMT.
           IF WH-PG = 4 MOVE 1 TO BZ-SH
                        MOVE "Bankeinzug" TO BZ-TX
                   else MOVE 0 TO BZ-SH
                        MOVE ZP-VERWEND TO BZ-TX.
           IF WI = 1 MOVE "Sammler" TO BZ-TX.
           MOVE 1 TO WX.
           MOVE OP-KURS TO BZ-KURS.
           MOVE OP-FSYM TO BZ-FSYM.
           IF BZ-BUDAT < WE-EURO;
              EVALUATE BZ-FSYM
                 WHEN "Eur" MOVE 1376,03 TO BZ-KURS
                 WHEN "Ats" MOVE 100,000 TO BZ-KURS.
           IF BZ-BUDAT not < WE-EURO;
              EVALUATE BZ-FSYM
                 WHEN "Eur" MOVE 100,00000 TO BZ-KURS
                 WHEN "Ats" MOVE 7,2672835 TO BZ-KURS.
           MOVE ZP-BETRAG TO BZ-FWBET.
           COMPUTE WM-ZABET rounded = ZP-BETRAG * BZ-KURS / 100.
      *------------------------------------------> Skonto = Differenz <-
           COMPUTE WP-SKTO = (OP-REBETRAG - OP-ZABETRAG) * BZ-KURS
                             / 100 - WM-ZABET.
           ADD WP-SKTO TO WK-SKTO.
           MOVE WP-SKTO TO WE-SKONTO(1).
           ADD BZ-FWBET TO WS-FWSAML.
           ADD WM-ZABET TO BZ-BUBET WS-SAMMEL.
           PERFORM AUS-OP.                             *> OP verbuchen
           DELETE ZAHLPOS.
           IF WI < WM-UEANZ ADD 1 TO WI GO K.
      *--------------------------------> öbergabe Skontosumme u. OP's <-
       Q.  IF BZ-BUDAT = 0 GO X.
           EVALUATE WH-AKAT
               WHEN 0 PERFORM SACHSUM
               WHEN 2 PERFORM DEBSUM
               WHEN 3 PERFORM KREDSUM.
           MOVE WH-JS TO BZ-SEITE.
           MOVE WS-FWSAML TO BZ-FWBET.
           MOVE 0 TO WS-FWSAML.
           PERFORM BZ-BUCH.
           MOVE BZ-SEITE TO WH-JS.
           IF WM-L = 1 GO J.
           MOVE 0 TO BZ-FWBET.
           MOVE WE-GGKTO TO BZ-GGKTO WK-KTONR.
           MOVE WS-SAMMEL TO BZ-BUBET WK-BRUTTO WK-BUBET.
           MOVE LOW-VALUES TO AZ-SATZ.
           MOVE 0 TO BZ-OPLFD.
           PERFORM SAMMELBUCH.
           REWRITE KO-SATZ.              *> wg. Zahlungsbuchung verlegt!
           PERFORM JOUREND.
       X.  CLOSE ZAHLPOS.
       Y.  MOVE WM-USE TO WE-USE.
       Z.  EXIT.
      ****************************************************** OP-Fehler *
       OP-FEHL SECTION.
       A.  DISPLAY "Zahlungssatz wird gelîscht!" AT 2301.
           DISPLAY "Op-Satz zu obiger Konto-Nr fehlt. " AT 2401.
           PERFORM WEITER.
           DELETE ZAHLPOS INVALID GO Z.
       Z.  EXIT.
      *********************************** prÅfen ob WE-WKZ richtig ist *
       WI-PRUEF SECTION.
       K.  MOVE 9 TO WH-KEY.
           READ KONSTANT INVALID MOVE 0 TO KO-WBEG(1).
           IF ZUGRIF PERFORM BESETZT GO K.
           UNLOCK KONSTANT.
           MOVE 0 TO WY WE-WKZ WE-WJAHR(1) WE-WJAHR(2).
           PERFORM VARYING WX FROM 4 BY -1 UNTIL WX = 0
               if ko-wbeg(wx) not = 0;
                  if ws-datum >= ko-wbeg(wx) and ws-datum <= ko-wend(wx)
                      move wx to wy.
           if wy not = 0
               ADD KO-WKZ(WY) 50 GIVING WF           *> wegen Jahr 2000
               MOVE WF TO WE-WKZ
               MOVE KO-WBEG(WY) TO WE-WJAHR(1)
               MOVE KO-WEND(WY) TO WE-WJAHR(2).
           IF WE-WJAHR(1) = 0 OR WE-WJAHR(2) = 0
               DISPLAY "Wirtschaftjahr nicht angelegt" AT 2401
               PERFORM WEITER
               SET ESC TO TRUE.
      *------------------------------------> FremdwÑhrungen abstellen <-
           MOVE 80 TO WH-KEY.
           READ KONSTANT IGNORE LOCK INVALID STOP RUN.
           MOVE KW-FRWTAB TO WE-FRWTAB.
           IF BZ-BUDAT = 0 MOVE WM-DATUM TO BZ-BUDAT.
           IF BZ-BUDAT < WE-EURO NEXT SENTENCE
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
           DISPLAY LOW-VALUE AT 0176.
           EVALUATE WF-SYM(1)
               WHEN "Ats" DISPLAY "Ats" with highlight
                                        foreground-color 2 AT 0000
               WHEN "Eur" DISPLAY "Eur" with highlight
                                        foreground-color 6 AT 0000.
       Z.  EXIT.
      ************************************************** OP-Abstattung *
       AUS-OP SECTION.
       A.  ADD ZP-BETRAG ZP-SKTO TO OP-ZABETRAG.
           MOVE 0 TO OP-ZPSKTO OP-ZPBET.
           MOVE BZ-BUDAT TO OP-ZADAT.
      *--------------------------> Auszifferungsdatei immer abstellen <-
           IF WO > 0
               MOVE 999 TO BZ-OPLFD
               MOVE ZP-BETRAG TO AZ-OPBET(WO)
               MOVE ZP-SKTO TO AZ-OPSKTO(WO)
               MOVE OP-RENUM TO AZ-OPNR(WO)
               MOVE OP-SYM TO AZ-OPSYM(WO)
               ADD 1 TO WO.
           MOVE 0 TO OP-SA.
           IF OP-REBETRAG = OP-ZABETRAG MOVE OP-DK TO OP-SA.
           REWRITE OP-SATZ.
      *-------------------------> Mehrwertsteuerermittlung aus Skonto <-
           ADD OP-B1 OP-B2 OP-MW GIVING WV-BAS.
           DIVIDE WV-BAS INTO OP-MW GIVING WH-TEIL ROUNDED.
           MULTIPLY WH-TEIL BY WP-SKTO GIVING WK-MWST ROUNDED.
           DIVIDE WV-BAS INTO WP-SKTO GIVING WH-TEIL ROUNDED.
           EVALUATE WH-AKAT
              WHEN 3 ADD WP-SKTO TO KO-SKTOERL
                     MOVE WK-SKTO TO BZ-MW
                     SUBTRACT WK-MWST FROM KO-SKTOVST BZ-MW
                     MOVE 3 TO BZ-VM
                     MOVE WK-MWST TO BZ-B1
              WHEN 2 ADD WK-MWST TO BZ-MW
                     MOVE 4 TO BZ-VM
                     MULTIPLY OP-B1 BY WH-TEIL GIVING WK-BET ROUNDED
                     ADD WK-BET TO BZ-B1
                     compute we-skonto(2) =
                         we-skonto(1) - wk-mwst - wk-bet
                     MOVE OP-U1 TO BZ-U1
                     IF BZ-U1 not = 0
                         SUBTRACT WK-BET FROM KO-MWSTBMG(BZ-U1)
                         end-if
                     MULTIPLY OP-B2 BY WH-TEIL GIVING WK-BET ROUNDED
                     ADD WK-BET TO BZ-B2
                     MOVE OP-U2 TO BZ-U2
                     IF BZ-U2 not = 0
                         SUBTRACT WK-BET FROM KO-MWSTBMG(BZ-U2)
                         end-if
                     ADD OP-B1 OP-B2 GIVING WV-BAS
                     DIVIDE WV-BAS INTO WE-SKONTO(WX) GIVING WH-TEIL
                         ROUNDED
                     MULTIPLY WH-TEIL BY OP-B1 GIVING WK-BET ROUNDED
                     IF OP-U1 not = 0
                         ADD WK-BET TO KO-SKTOAUF(OP-U1)
                     end-if
                     IF WE-SKONTO(2) not = 0 ADD WE-SKONTO(2) TO BZ-MW.
       Z.  EXIT.
      ****************************************** suchen Kassabuchungen *
       SUCHKAS SECTION.
       A.  CALL "CAUP" USING "270247223201012" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Kassabuchungen " AT VDU-LP.
           ADD 202 VDU-ECK GIVING VDU-LP.
           DISPLAY " Zl.  Ka-Datum         Betrag " with reverse-video
               AT VDU-LP.
           ADD 505 VDU-ECK GIVING VDU-LP.
           DISPLAY "Bitte warten!" with highlight AT VDU-LP.
           MOVE LOW-VALUES TO WO-TSATZ.
           INITIALIZE BU-SATZ.
       B.  START BUCHUNG KEY not < BU-KEY INVALID
               SET ESC TO TRUE GO Z.
       C.  READ BUCHUNG NEXT AT END MOVE 99 TO WM-L GO E.
           IF ZUGRIF PERFORM BESETZT GO C.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 19
               OR WO-DAT(WX) = 0 OR BU-DAT < WO-DAT(WX)
               OR WO-DAT(WX) = BU-DAT MOVE WX TO WY.
           IF WO-DAT(WX) = BU-DAT OR WO-DAT(WX) = 0
               MOVE BU-DAT TO WO-DAT(WX)
               IF BU-REN = 999990 COMPUTE BU-BET = BU-BET * -1
               end-if
               ADD BU-BET TO WO-SUMM(WX) GO C.
           PERFORM VARYING WY FROM 19 BY -1 UNTIL WY = (WX - 1)
               MOVE WO-DAT(WY) TO WO-DAT(WY + 1)
               MOVE WO-SUMM(WY) TO WO-SUMM(WY + 1).
           MOVE BU-DAT TO WO-DAT(WX).
           MOVE BU-BET TO WO-SUMM(WX).
           GO C.
      *------------------------------------------> gefundene anzeigen <-
       E.  CALL "CAUP" USING "16CLRFEN" WH-CREG.
           PERFORM VARYING WY FROM 1 BY 1 UNTIL WY > 19
               OR WO-DAT(WY) = 0
               ADD 20000000 TO WO-DAT(WY)
               MOVE WY TO WD-POS
               COMPUTE VDU-LP = WY * 100 + 203 + VDU-ECK
               DISPLAY WD-POS with highlight AT VDU-LP
               ADD 5 TO VDU-LP
               MOVE WO-DAT(WY) TO WC-DATUM
               PERFORM DATDREH
               DISPLAY VDU-DATUM AT VDU-LP
               MOVE WO-SUMM(WY) TO WD-SALDO
               ADD 10 TO VDU-LP
               DISPLAY WD-SALDO with highlight AT VDU-LP.
       Z.  EXIT.
      *************************************** öbernahme Kassabuchungen *
       KASSAUEB SECTION.
       A.  CALL "CAUP" USING "0710100860000" WH-CREG.
           MOVE 55 TO WL-CA.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " KassaÅbernahme " with highlight AT VDU-LP.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "Kassabuchung vom:          " AT VDU-LP.
           MOVE "FIBUKAS.DAT" TO WN-BUEB.
           MOVE WE-USE TO WM-USE.
           PERFORM OB-FREI.
           IF ESC OPEN INPUT BUCHUNG GO Y.
           OPEN I-O BUCHUNG.
           IF WF-STATUS = "05" CLOSE BUCHUNG
               DELETE FILE BUCHUNG
               GO X.
           INITIALIZE BZ-SATZ.
           MOVE VDU-ECK TO WK-LP.
           PERFORM SUCHKAS.
           MOVE WK-LP TO VDU-ECK.
      *-------------------------------------------------------> Datum <-
           IF WO-DAT(1) = 0 CLOSE BUCHUNG GO X.
           ADD 321 VDU-ECK GIVING VDU-LP.
           MOVE WO-DAT(1) TO WS-DATUM WC-DATUM.
           PERFORM DATDREH.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
       C.  MOVE WS-DATUM TO WZ-DATUM WH-WERT.
           DISPLAY "<esc>= Abbruch, <ret>= Datum" AT 2301.
           CALL "CAUP" USING "1103216006" WH-CREG.
           IF ESC CALL "CAUP" USING "08CLOFEN" WH-CREG
                  GO Y.
           IF not RET GO C.
           IF WZ-DATUM = 0 GO C.
           MOVE WX-DATUM TO WS-DATUM WE-PER.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           IF WV-FPER = 0 COMPUTE WV-FPER = WS-DATUM / 100.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 20
               or WO-DAT(WX) = WS-DATUM CONTINUE.
           IF WX > 20 DISPLAY "Datum falsch" with highlight AT 2401
               PERFORM  WEITER GO C.
           CALL "CAUP" USING "08CLOFEN" WH-CREG.
      *-------------------------------------------> Eingabe BUBEL-Nr. <-
           MOVE 0 TO WK-LFD WE-BELNR.
           MOVE WS-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           ADD 321 VDU-ECK GIVING VDU-LP.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           CALL "CAUP" USING "04DATPRF" WH-CREG.
           MOVE WX-DATUM TO WS-DATUM.
       E.  ADD 503 VDU-ECK GIVING VDU-LP.
           DISPLAY "Beleg-Nr.: " AT VDU-LP.
           MOVE 0 TO WE-BELNR WH-WERT.
           COMPUTE VDU-LP = VDU-LP - VDU-ECK.
           DISPLAY "<esc>= Abbruch, <ret>= Beleg-Nr." AT 2301.
           DISPLAY "ohne Beleg-Nr. = Nr. lt. Kassabuch" AT 2401.
           CALL "CAUP" USING "1005146006" WH-CREG.
           IF ESC or WOLI GO Y.
           IF not RET GO E.
           MOVE WH-WERT TO WD-NUM WE-BELNR.
           IF WE-BELNR not = 0 MOVE 2500 TO WK-LFD
                DISPLAY WD-NUM with highlight AT VDU-LP
           else DISPLAY "lt. Kassabuch" with highlight AT VDU-LP.
      *-----------------------------------------> Start der öbernahme <-
       G.  DISPLAY "<esc>= Abbruch, <ret>= Start < >"  AT 2301.
           CALL "CAUP" USING "0023310000" WH-CREG.
           IF ESC GO Y.
           IF not RET GO G.
           MOVE 0 TO WS-SAMMEL WS-FWSAML.
           INITIALIZE BU-SATZ.
           START BUCHUNG KEY not < BU-KEY INVALID GO Z.
       I.  READ BUCHUNG NEXT AT END GO Z.
           IF ZUGRIF PERFORM BESETZT GO I.
           INITIALIZE BZ-SATZ.
           PERFORM WI-PRUEF
               IF ESC GO Z.
           MOVE WS-DATUM TO WE-PER.
      *---------------------> erst Kontrolle ob alle Konten vorhanden <-
           PERFORM KONTPRUEF.
           IF WM-L = 9 GO Z.
           MOVE WE-KUEB TO WE-USE WM-L.
       K.  DISPLAY "<esc>= Abbruch, <Ende>= Start öbernahme" AT 2301.
           CALL "CAUP" USING "0023410000" WH-CREG.
           IF ESC GO Y.
           IF not ENDE GO K.
           MOVE 0 TO BU-KEY.
           MOVE 0 TO WK-SKTO WK-MWST WK-BASIS WK-SOLL WK-HABEN
                     WK-BRUTTO WK-NETTO WK-BUBET BZ-SEITE.
           START BUCHUNG KEY > BU-KEY INVALID GO Z.
       M.  INITIALIZE BU-SATZ.
           READ BUCHUNG NEXT AT END GO Q.
           IF ZUGRIF PERFORM BESETZT GO M.
           MOVE 0 TO WK-SKTO.
           MOVE BU-Z TO WM-Z.
           ADD 19000000 BU-DAT GIVING WV-DATUM.
           IF BU-DAT < 900000 ADD 1000000 TO WV-DATUM.
           IF WV-DATUM > WS-DATUM GO M.
           IF WV-DATUM not = WS-DATUM GO M.
           IF BU-SA not = "S" GO M.
           MOVE BU-KTONR TO WH-NUM WD-KZ.
           IF WD-KZ = 0;
               EVALUATE WH-PG
                   WHEN 1; IF BU-SH > 4 GO M
                   WHEN 3; IF BU-SH < 5 GO M
                           else ADD -5 TO BU-SH.
           COMPUTE WM-FPER = WV-DATUM / 100.
           IF WM-FPER not = WV-FPER DISPLAY "nicht mîglich!" AT 2501
                                    GO K.
           PERFORM LIESKTO.
           IF FEHLER GO M.
           UNLOCK SACHBUCH.
           UNLOCK KREDIT.
           UNLOCK DEBITOR.
           MOVE WH-KAT TO WH-AKAT.
           MOVE WH-BN TO WH-ABN.
           DIVIDE 10 INTO BU-KTONR GIVING WD-KTO.
           ADD 522 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-KTO AT VDU-LP.
           MOVE BU-KTONR TO BZ-KTONR WK-KTONR.
           MOVE WS-DATUM TO BZ-BUDAT.                 *> öbernahmedatum
           IF BU-VALDAT(1:) = SPACE MOVE 0 TO BU-VALDAT.
           MOVE 0 TO BZ-LFD.
           MOVE WV-DATUM TO BZ-BELDAT.
           IF WE-BELNR not = 0 MOVE WE-BELNR TO BZ-BELNR
                          else MOVE BU-REN TO BZ-BELNR.
           MOVE BU-RF TO BZ-REFNR.
           MOVE BU-SY TO BZ-SYM.                     *> kommt aus FAKT
           IF BZ-SYM > 49 ADD -50 TO BZ-SYM.         *> Merker ohne OP
           MOVE BU-SH TO BZ-SH.
           MOVE BU-GK TO BZ-GGKTO.
           IF BU-FSYM = LOW-VALUE MOVE SPACE TO BU-FSYM.  *> spÑter weg
           MOVE 0 TO BZ-KSTNR BZ-KURS BZ-OPLFD.
           MOVE BU-Z TO BZ-Z.                        *> Gegenbuchungsart
           MOVE BU-VM TO BZ-VM.                      *> Steuermerker
      *-------------------------------------> auf Eurobuchung umbauen <-
           IF BU-FSYM = SPACE;              *> fÅr nicht geÑnderte Fakt
               IF BZ-BUDAT < WE-EURO MOVE "Ats" TO BU-FSYM
                                else MOVE "Eur" TO BU-FSYM.
           IF BZ-BUDAT < WE-EURO;
              EVALUATE BU-FSYM
                 WHEN "Eur" MOVE 1376,03 TO BU-KURS
                 WHEN "Ats" MOVE 100,000 TO BU-KURS
                            IF BU-FWBET = 0 MOVE BU-BET TO BU-FWBET.
           IF BZ-BUDAT not < WE-EURO;
              EVALUATE BU-FSYM
                 WHEN "Eur" MOVE 100,00000 TO BU-KURS
                 WHEN "Ats" MOVE 7,2672835 TO BU-KURS.
      *------------------------------------> Mwst. wg. Umrechnung neu <-
           MOVE BU-FSYM TO BZ-FSYM.
           MOVE BU-KURS TO BZ-KURS.
           MOVE BU-FWBET TO BZ-FWBET.
           MOVE BU-BET TO BZ-BUBET WK-BUBET.
           MOVE BU-MW TO BZ-MW WK-MWST.
           MOVE BU-U1 TO BZ-U1.
           MOVE BU-U2 TO BZ-U2.
           MOVE BU-B1 TO BZ-B1.
           MOVE BU-B2 TO BZ-B2.
           MOVE BU-KOND TO BZ-KOND.
           MOVE BU-TX TO BZ-TX.
           MOVE WE-SKZ(BZ-SYM + 1) TO WE-SYMT.
           ADD WE-USE 1 GIVING WH-KEY.
       O.  READ KONSTANT INVALID INITIALIZE KO-BSATZ
               MOVE WH-KEY TO KO-NUM WRITE KO-SATZ GO O.
           IF ZUGRIF PERFORM BESETZT GO O.
           MOVE WS-DATUM TO KO-BUDAT.
           MOVE DE-OPKZ TO WH-OP.
           IF WH-OP = 0 and WH-AKAT > 1 PERFORM ZAHL-OP.
      *    IF WH-OP = 0; IF BU-Z = 7 PERFORM ZAHL-OP
      *                  else IF BU-SY < 50 PERFORM MACHOP.
           MOVE BZ-BUBET TO WK-BRUTTO WK-NETTO.
           IF BZ-Z = 7 ADD -5 TO BZ-Z.                   *> Zahlungen
           IF BZ-Z = 5 MOVE 2 TO BZ-Z.
           PERFORM BZ-BUCH.                              *> Erstbuchung
           EVALUATE WH-AKAT
               WHEN 0 PERFORM SACHSUM
               WHEN 2 PERFORM DEBSUM
               WHEN 3 PERFORM KREDSUM.
           PERFORM ADD-UST.
           IF not KAS-ZAHL ADD BZ-MW TO BZ-BUBET.  *> da Netto Åbergeben
           MOVE BZ-BUBET TO WK-BRUTTO WK-NETTO WK-BUBET.
           MOVE 0 TO WK-MWST BZ-MW.
           MOVE BU-GK TO WH-NUM WK-KTONR.
           IF BU-Z = 5 MOVE 0 TO BZ-Z.
           MOVE BZ-Z TO WM-GB.
           EVALUATE TRUE
               WHEN EINZEL PERFORM GEGENBUCH
                     IF not ESC ADD 1 TO IX
                                PERFORM BZ-BUCH
               WHEN SAMMEL IF WK-JS > BZ-SEITE MOVE WK-JS TO BZ-SEITE
                           end-if
                           PERFORM SAMMELBUCH.
      *--------------------------------> RÅckschreiben Speichersummen <-
           REWRITE KO-SATZ.
           DELETE BUCHUNG INVALID GO X.
      *-----------------------------------> Zinsendatei bei Debitoren <-
           GO M.
       Q.  PERFORM JOUREND.
           GO Y.
       X.  DISPLAY " Keine Kassadaten vorhanden " with reverse-video
               AT 2401 PERFORM WEITER.
           MOVE WM-USE TO WE-USE.
           CALL "CAUP" USING "08CLOFEN" WH-CREG.
           GO Z.
       Y.  CLOSE BUCHUNG.
           MOVE WM-USE TO WE-USE.
           MOVE WK-BS(1:1) TO WD-Z.
           COMPUTE WK-BS = WD-Z * 10 + WE-USE.
           CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
