      $SET LINKCOUNT"320" ANS85 BOUND AUTOLOCK NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     FIBKONT.
      *--------------------------------------------> Journalkontrolle <-
       ENVIRONMENT    DIVISION.
       CONFIGURATION   SECTION.
       OBJECT-COMPUTER.     PC.
       SPECIAL-NAMES.   DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       COPY FIBUSEC.CPY.
           SELECT BUNEU    ASSIGN "BUNEU.DAT"
                           ORGANIZATION INDEXED, ACCESS DYNAMIC
                           RECORD KEY BN-KEY
                           ALTERNATE KEY IS BN-JKEY
                           FILE STATUS WF-STATUS.
           SELECT RECHNUM  ASSIGN "FIBRECH.DAT"
                           ORGANIZATION INDEXED, ACCESS DYNAMIC
                           RECORD KEY RE-KEY DUPLICATES
                           FILE STATUS WF-STATUS.
           SELECT DRUCKER  ASSIGN TO PRINTER WH-DRUNAM
                           ORGANIZATION RECORD SEQUENTIAL
                           FILE STATUS WF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       COPY FIBUFD.CPY.
      ************************************* Journal- u. Buchungszeilen *
       FD  BUNEU        external       RECORD VARYING FROM 68 TO 124
                                       DEPENDING ON WZ-BUCH.
       01  BN-SATZ.
           03  BN-KEY.
               05 BN-KTONR             PIC 9(6)      COMP.
               05 BN-BUDAT             PIC 9(8)      COMP.
               05 BN-LFD               PIC 9999      COMP.
           03  BN-JKEY.
               05 BN-USE               PIC 99        COMP.
               05 BN-SEITE             PIC 9(7)      COMP-X.
           03  BN-BELNR                PIC 9(6)      COMP.
           03  BN-BELDAT               PIC 9(8)      COMP.
           03  BN-REFNR                PIC 9(7)      COMP.
           03  BN-SYM                  PIC 99        COMP.
           03  BN-GGKTO                PIC 9(6)      COMP.
           03  BN-SH                   PIC 9         COMP.
           03  BN-BUBET                PIC S9(9)V99  COMP.
           03  BN-Z                    PIC 9         COMP.
           03  BN-VM                   PIC 9         COMP.
           03  BN-MW                   PIC S9(7)V99  COMP.
           03  BN-U1                   PIC 99        COMP.
           03  BN-B1                   PIC S9(9)V99  COMP.
           03  BN-U2                   PIC 99        COMP.
           03  BN-B2                   PIC S9(9)V99  COMP.
           03  BN-KSTNR                PIC 9(5)      COMP.
           03  BN-KOND                 PIC 9(13)     COMP.
           03  BN-OPLFD                PIC 9999      COMP.
           03  BN-FWBET                PIC S9(9)V99  COMP.
           03  BN-KURS                 PIC 9(5)V9999 COMP.
           03  BN-FSYM                 PIC XXX.
           03  BN-TX                   PIC X(40).
      *----------------------------------------> Jahresabschlu·summen <-
       01  BN-ASATZ.
           03  FILLER                  PIC X(52).
           03  BN-SOLL                 PIC S9(9)V99  COMP.
           03  BN-HABEN                PIC S9(9)V99  COMP.
           03  BN-EBIL                 PIC S9(9)V99  COMP.
      ***************************************** Rechnungskontrolldatei *
       FD  RECHNUM                     LABEL RECORD STANDARD.
       01  RE-SATZ.
           03  RE-KEY.
               05 RE-BELNR             PIC 9(6)       COMP.
           03  RE-KTONR                PIC 9(6)       COMP.
           03  RE-BUBET                PIC S9(7)V99   COMP.
      ******************************************************************
       FD  DRUCKER                     LABEL RECORD STANDARD.
       01  DRA-SATZ.
           03  FILLER                  PIC X(10).
           03  DRA-BELNR               PIC ZZ.ZZ9-.
           03  DRA-KTONR               PIC ZZ.ZZ9,9-.
           03  DRA-BUBET               PIC ZZZ.ZZ9,99-.
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
           03  WH-DRUNAM               PIC X(12)     VALUE   "LPT1:".
           03  WM-OPEN                 PIC 9         COMP   VALUE ZERO.
           03  WT-ERFTAB.
               05  WT-KEY              PIC X(12)         OCCURS 17.
           03  VDU-REL                 PIC 9999.
           03  VDR-REL REDEFINES VDU-REL.
               05  VDU-RL              PIC 99.
               05  VDU-RP              PIC 99.
           03  WI                      PIC 99       COMP.
           03  WL                      PIC 99       COMP.
           03  WS                      PIC 99       COMP.
           03  IX                      PIC 99       COMP.
           03  wh-ust                  pic 99.
           03  WD-JONUM                PIC Z.ZZ9.
           03  WD-POS                  PIC Z9.
           03  WD-KZ                   PIC 9.
           03  WD-KTO                  PIC ZZ.ZZ9,9.
           03  WD-SAKO                 PIC 99.999,9.
           03  WV-VDAT                 PIC 9(6).
           03  WV-BDAT                 PIC 9(6).
           03  WD-SALDO                PIC ZZ.ZZZ.ZZ9,99-.
           03  WC-UST                  PIC S9(9)V99  COMP.
           03  WK-NETTO                PIC S9(9)V99  COMP.
           03  WK-BUBET                PIC S9(9)V99  COMP.
           03  WK-SKTO                 PIC S9(9)V99  COMP.
           03  WM-SOLL                 PIC S9(9)V99   COMP.
           03  WM-HABEN                PIC S9(9)V99   COMP.
           03  WK-BET                  PIC S9(9)V99  COMP.
           03  WK-UDIF                 PIC S9(7)V99  COMP.
           03  WV-S93                  PIC 9(7)      COMP-X.
           03  WV-S94                  PIC 9(7)      COMP-X.
           03  WV-S95                  PIC 9(7)      COMP-X.
           03  WM-S93                  PIC 9(7)      COMP-X.
           03  WM-S94                  PIC 9(7)      COMP-X.
           03  WM-S95                  PIC 9(7)      COMP-X.
           03  WK-MCODE                PIC X(10).
           03  WH-BN                   PIC 99        COMP.
           03  WH-KAT                  PIC 9         COMP.
           03  wd-ust                  pic zzz.zz9,99-.
           03  WH-SEITE                PIC 9(7)      COMP.
           03  WK-JS                   PIC 9(7).
           03  WR-JS REDEFINES WK-JS.
               05 WK-SEITE             PIC 99999.
               05 WK-ZEIL              PIC 99.
           03  WV-BISJ                 PIC 9(5)      COMP.
           03  wj-zeil                 pic 9(6)      COMP-x.
           03  WV-BELNR                PIC 9(6)      COMP.
      *---------------------------------------------> Journalrechnung <-
           03  WH-BKEY.
               05 WH-KTONR             PIC 9(6)      COMP.
               05 WH-BUDAT             PIC 9(6)      COMP.
               05 WH-LFD               PIC 9999      COMP.
           03  WH-JOKEY                PIC X(4).
           03  WR-KEY                  PIC 9(5).
           03  WBZ-BUBET               PIC S9(9)V99  COMP-3.
           03  WBZ-VM                  PIC 9         COMP.
           03  WBZ-MW                  PIC S9(7)V99  COMP-3.
           03  WBZ-U1                  PIC 99        COMP.
           03  WBZ-B1                  PIC S9(9)V99  COMP-3.
       01  WO-TSATZ.
           03  FILLER                  PIC 999      COMP-3.
           03  WO-KTONR                PIC 9(6)     COMP   OCCURS 20.
       COPY FIBUEXT.CPY.
       COPY FIBUDECL.CPY.
       DECL-S SECTION.         USE AFTER ERROR PROCEDURE ON RECHNUM.
       A.  CALL "CADECL" USING "FIBRECH.DAT " WH-CREG.
       DECL-T SECTION.         USE AFTER ERROR PROCEDURE ON KERMODEL.
       A.  CALL "CADECL" USING "FIBUKER.DAT " WH-CREG.
       END DECLARATIVES.
      ******************************************************************
       STEUER SECTION.
       A.  MOVE WL-CALL TO WH-CALL.
           MOVE WL-CREG TO WH-CREG.
           EVALUATE WL-CA
               WHEN 95 PERFORM REPRUEF
               WHEN 96 PERFORM JOURECH.
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
       NO-REC SECTION.
       A.  DISPLAY "keine Daten vorhanden," AT 2401
           PERFORM WEITER.
       Z.  EXIT.
      ******************************************************************
       ALLANZ SECTION.
       A.  PERFORM VARYING WX FROM 13 BY -1 UNTIL WX = 1
               OR WV-MCODE(WX:1) NOT = SPACE CONTINUE.
           IF WV-MCODE(WX:1) NUMERIC MOVE SPACE TO WV-MCODE(WX:1).
           ADD 202 VDU-ECK GIVING VDU-REL.
           PERFORM VARYING WI FROM 1 BY 1 UNTIL WI > 9
               MOVE WI TO WD-POS
               DISPLAY WD-POS with highlight AT VDU-REL
               ADD 100 TO VDU-REL.
           MOVE LOW-VALUE TO WT-ERFTAB.
           MOVE 0 TO WX WR WF.
           MOVE 3 TO WI.
       Z.  EXIT.
      ***************************************** Journalsummen anzeigen *
       JOURSUM SECTION.
       A.  CALL "CAUP" USING "0701102256000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " offene Sammelbuchungen "
               with highlight AT VDU-LP.
           ADD WE-USE 1 GIVING WH-KEY.
       C.  READ KONSTANT INVALID GO Z.
           IF ZUGRIF STOP RUN.
           DISPLAY " Soll         Haben " with highlight reverse-video
                AT 0144.
           DISPLAY "Vorsteuer.............:" AT 0212.
           DISPLAY "Mehrwertsteuer........:" AT 0312.
           DISPLAY "Skontoerlîs.(w).......:" AT 0412.
           DISPLAY "Kurs-/Rundungsdif. ...:" AT 0512.
           DISPLAY "Erwerbsteuer..........:" AT 0612.
           DISPLAY "Skto.-Aufwand Ust-Kz.1:" AT 0712.
           DISPLAY "Skto.-Aufwand Ust-Kz.2:" AT 0812.
           DISPLAY "Skto.-Aufwand Ust-Kz.3:" AT 0912.
           DISPLAY "Skto.-Aufwand Ust-Kz.4:" AT 1012.
           DISPLAY "Skto.-Aufwand Ust-Kz.5:" AT 1112.
           DISPLAY "Skto.-Aufwand Ust-Kz.6:" AT 1212.
           DISPLAY "Sammelkonto Deb.- 0...:" AT 1312.
           DISPLAY "    ''      Deb.- 1...:" AT 1412.
           DISPLAY "    ''      Deb.- 2...:" AT 1512.
           DISPLAY "Sammelkonto Kred.- 0..:" AT 1612.
           DISPLAY "    ''      Kred.- 1..:" AT 1712.
           DISPLAY "    ''      Kred.- 2..:" AT 1812.
           DISPLAY "Journalsummen.........:" AT 1912.
           DISPLAY ALL "ƒ" with SIZE 28 AT 2036.
           DISPLAY "Totalsummen...........:" AT 2112.
           MOVE KO-VST TO WD-SALDO.
           DISPLAY WD-SALDO with highlight AT 0236.
           MOVE KO-MWST TO WD-SALDO.
           DISPLAY WD-SALDO with highlight AT 0350.
           MOVE KO-SKTOERL TO WD-SALDO.
           DISPLAY WD-SALDO with highlight AT 0450.
           MOVE KO-KRDIF TO WD-SALDO.
           DISPLAY WD-SALDO with highlight AT 0550.
           MOVE KO-IGMUST(1) TO WD-SALDO.
           DISPLAY WD-SALDO with highlight AT 0636.
           MOVE KO-IGMUST(2) TO WD-SALDO.
           DISPLAY WD-SALDO with highlight AT 0650.
           MOVE 0636 TO VDU-LP.
           PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 6
               MOVE KO-SKTOAUF(IX) TO WD-SALDO
               ADD 100 TO VDU-LP
               DISPLAY WD-SALDO with highlight AT VDU-LP.
           PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 6
               MOVE KO-DEBSUM(IX) TO WD-SALDO
               COMPUTE VDU-L = IX + 12
               IF IX > 3 MOVE 50 TO VDU-P
                         SUBTRACT 3 FROM VDU-L
                    else MOVE 36 TO VDU-P
               end-if DISPLAY WD-SALDO AT VDU-LP.
           PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 6
               MOVE KO-KREDSUM(IX) TO WD-SALDO
               COMPUTE VDU-L = IX + 15
               IF IX > 3 MOVE 50 TO VDU-P
                         SUBTRACT 3 FROM VDU-L
                    else MOVE 36 TO VDU-P
               end-if DISPLAY WD-SALDO AT VDU-LP.
           MOVE KO-JSOLL TO WD-SALDO.
           DISPLAY WD-SALDO with highlight AT 1936.
           MOVE KO-JHABEN TO WD-SALDO.
           DISPLAY WD-SALDO with highlight AT 1950.
           ADD KO-VST KO-SKTOAUF(1) KO-SKTOAUF(2) KO-SKTOAUF(3)
                      KO-SKTOAUF(4) KO-SKTOAUF(5) KO-SKTOAUF(6)
                            KO-JSOLL GIVING WD-SALDO WE-SOLL.
           DISPLAY WD-SALDO with highlight AT 2136.
           ADD KO-MWST KO-SKTOERL KO-KRDIF
                            KO-JHABEN GIVING WD-SALDO WE-HABEN.
           DISPLAY WD-SALDO with highlight AT 2150.
           SUBTRACT WE-HABEN FROM WE-SOLL GIVING WD-SALDO.
           DISPLAY WD-SALDO with highlight AT 2243.
           DISPLAY "weiter mit beliebiger Taste < >" AT 2301.
           CALL "CAUP" USING "0023300000" WH-CREG.
           IF SF1 or EINF PERFORM JOURECH GO C.
       Z.  EXIT.
      ******************************** Durchrechnung der Journalsummen *
       JOURECHA SECTION.
       A.  CALL "CAUP" USING "0712200550000" WH-CREG.
           ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY " Journalkontrolle " with highlight AT VDU-LP.
           MOVE 0 TO IX.
       C.  ADD 403 VDU-ECK GIVING VDU-LP.
           DISPLAY "<esc>= Abbruch, <ret>= Start <    >" AT VDU-LP.
           DISPLAY "<#>= J-Zeile f. 0-Vergleich" AT 2401.
           CALL "CAUP" USING "1004334004" WH-CREG.
           IF ESC GO X.
           IF KIST MOVE 1 TO IX
               SET RET TO TRUE MOVE 0 TO BZ-USE BZ-SEITE.
           IF NOT RET GO C.
           MOVE WH-NUM TO WH-SEITE.
           MOVE 0 TO WV-DATUM.
           INITIALIZE KO-SASUM.
           IF IX = 0 PERFORM FIRST-JOUR
                     IF ESC GO W.
           MOVE BZ-JKEY TO WH-JOKEY.
       E.  MOVE WH-JOKEY TO BZ-JKEY.
           START BUCHZEIL KEY NOT < BZ-JKEY INVALID GO W.
       G.  MOVE SPACE TO BZ-TX.
           READ BUCHZEIL NEXT AT END GO W.
           IF ZUGRIF PERFORM BESETZT GO G.
           IF BZ-USE NOT = WE-USE GO W.
           if ix = 1; if bz-use > 0 go w
                      ELSE move 1 to bz-use rewrite bz-satz go e.
       i.  ADD 230 VDU-ECK GIVING VDU-LP.
           DISPLAY BZ-USE AT VDU-LP "/" BZ-SEITE.
           MOVE BZ-JKEY TO WH-JOKEY.
           MOVE WE-SYM(BZ-SYM + 1) TO WT-TX.
      *------------------------------------> TEST <-
           EVALUATE WT-TX(1:2)
               WHEN "Ga" IF BZ-BUBET > 0 MULTIPLY -1 BY BZ-BUBET
                             MULTIPLY -1 BY BZ-MW
                             MULTIPLY -1 BY BZ-B1
                             MULTIPLY -1 BY BZ-B2
                             IF BZ-SH = 1 MOVE 0 TO BZ-SH
                             else MOVE 1 TO BZ-SH
                             end-if REWRITE BZ-SATZ
               WHEN "Ge" IF BZ-BUBET > 0 MULTIPLY -1 BY BZ-BUBET
                             MULTIPLY -1 BY BZ-MW
                             MULTIPLY -1 BY BZ-B1
                             MULTIPLY -1 BY BZ-B2
                             IF BZ-SH = 0 MOVE 1 TO BZ-SH
                             else MOVE 0 TO BZ-SH
                             end-if REWRITE BZ-SATZ.
           MOVE BZ-KTONR TO WH-NUM.
           PERFORM LIESKTO.
           MOVE 0 TO WK-SKTO.
           MOVE BZ-BUBET TO WK-BUBET WK-NETTO.
           MOVE BZ-VM TO WE-UST.
           EVALUATE BZ-VM                      *> nicht bei innerg. UST
               WHEN 8 ADD BZ-BUBET TO KO-KRDIF
               WHEN 3 IF BZ-MW NOT = 0 ADD BZ-MW BZ-B1 TO WK-SKTO
                      ADD BZ-MW TO KO-SKTOERL
                      SUBTRACT BZ-B1 FROM KO-SKTOVST
                      ADD BZ-B1 TO KO-SKTOERL
               WHEN 4 ADD BZ-MW BZ-B1 BZ-B2 GIVING WK-SKTO
                     MOVE BZ-MW TO WK-UDIF
                     IF BZ-U1 NOT = 0 MOVE BZ-B1 TO WK-BET
                         ADD WK-BET TO KO-SKTOAUF(BZ-U1)
                         SUBTRACT WK-BET FROM KO-MWSTBMG(BZ-U1)
                         MOVE WT-UST(BZ-U1) TO WH-UST
                         COMPUTE WK-BET = (WH-UST / 100) * BZ-B1
                         ADD WK-BET TO KO-SKTOAUF(BZ-U1)
                         SUBTRACT WK-BET FROM WK-UDIF
                         end-if
                     IF BZ-U2 NOT = 0 MOVE BZ-B2 TO WK-BET
                         ADD WK-BET TO KO-SKTOAUF(BZ-U2)
                         SUBTRACT WK-BET FROM KO-MWSTBMG(BZ-U2)
                         MOVE WT-UST(BZ-U2) TO WH-UST
                         COMPUTE WK-BET = (WH-UST / 100) * BZ-B2
                         ADD WK-BET TO KO-SKTOAUF(BZ-U2)
                         SUBTRACT WK-BET FROM WK-UDIF
                         end-if
                     IF BZ-U1 NOT = 0 ADD WK-UDIF TO KO-SKTOAUF(BZ-U1)
                     else IF BZ-U2 NOT = 0
                              ADD WK-UDIF TO KO-SKTOAUF(BZ-U2).

           EVALUATE WH-KAT
               WHEN 0 PERFORM SACHSUM
               WHEN 2 PERFORM DEBSUM
               WHEN 3 PERFORM KREDSUM.
           PERFORM ADD-UST.
      *--------------------------------------> Journalsummenvergleich <-
           ADD KO-VST KO-SKTOAUF(1) KO-SKTOAUF(2) KO-SKTOAUF(3)
                      KO-SKTOAUF(4) KO-SKTOAUF(5) KO-SKTOAUF(6)
                            KO-JSOLL GIVING WD-SALDO WE-SOLL.
           DISPLAY WD-SALDO with highlight AT 2136.
           ADD KO-MWST KO-SKTOERL KO-KRDIF
                            KO-JHABEN GIVING WD-SALDO WE-HABEN.
           DISPLAY WD-SALDO with highlight AT 2150.
           SUBTRACT WE-HABEN FROM WE-SOLL GIVING WD-SALDO.
           DISPLAY WD-SALDO with highlight AT 2243.
           if (we-soll - we-haben) = -890,40 go v.
           IF WE-SOLL NOT = WE-HABEN GO G.
       v.  IF WH-SEITE NOT = 0; IF BZ-SEITE < WH-SEITE GO G.
      *-----------------------------------------> ob Journalsaldo = 0 <-
           DISPLAY "weiter mit beliebiger Taste <    > "
               AT 2301 WH-SEITE.
           CALL "CAUP" USING "0023304004" WH-CREG.
           MOVE WH-NUM TO WH-SEITE.
           GO G.
       W.  MOVE 0 TO KO-SPERR.
           REWRITE KO-SATZ.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ******************************** Durchrechnung der Journalsummen *
       JOURECH SECTION.
       A.  PERFORM AUTKONT.
           CALL "CAUP" USING "0712200550000" WH-CREG.
           ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY " Journalkontrolle " with highlight AT VDU-LP.
           MOVE 0 TO IX WH-SEITE
               WBZ-BUBET WBZ-VM WBZ-MW WBZ-U1 WBZ-B1.
      *    open output drucker.
       B.  CALL "CAUP" USING "16CLRFEN" WH-CREG.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "ab Journalseite:       bis: " AT VDU-LP.
           CALL "CAUP" USING "1003204004" WH-CREG.
           IF ESC GO X.
           IF NOT RET GO B.
           MOVE WH-NUM TO WD-JONUM WK-SEITE.
           DISPLAY WD-JONUM with highlight AT VDU-LP.
       C.  ADD 303 VDU-ECK GIVING VDU-LP.
           CALL "CAUP" USING "1003314004" WH-CREG.
           IF ESC GO X.
           IF NOT RET GO C.
           MOVE WH-NUM TO WD-JONUM WV-BISJ.
           DISPLAY WD-JONUM with highlight AT VDU-LP.
           IF WV-BISJ < WK-SEITE GO B.
           MOVE "KONTROLL.LST" TO WH-DRUNAM.
           ADD 403 VDU-ECK GIVING VDU-LP.
           MOVE 0 TO WK-ZEIL.
       D.  MOVE WK-JS TO BZ-SEITE.
           MOVE WE-WKZ TO BZ-USE.
           MOVE 0 TO WV-VDAT WV-BDAT.
           MOVE 0 TO WV-DATUM.
           INITIALIZE KO-SASUM.
      *    IF IX = 0 PERFORM FIRST-JOUR
      *              IF ESC GO W.
       F.  START BUCHZEIL KEY NOT < BZ-JKEY INVALID
               MOVE 0 TO WS-TASTE GO Z.
       G.  MOVE SPACE TO BZ-TX.
           READ BUCHZEIL NEXT AT END move 0 to wv-bisj GO Q.
           IF ZUGRIF PERFORM BESETZT GO G.
           IF BZ-USE NOT = WE-WKZ GO Q.
           MOVE BZ-SEITE TO WK-JS.
      *    DISPLAY wk-js AT 2020.
           IF WK-SEITE > WV-BISJ GO Q.
      *    if bz-budat < 941101 or bz-budat > 941130 go g.
           MOVE BZ-KTONR TO WD-KZ.
           IF WD-KZ = 1 GO K.
      *----------------------------------> fÅr Ende ohne au·erb. Kto. <-
      *    IF WV-VDAT = 0 MOVE BZ-BUDAT TO WV-VDAT.
      *    MOVE BZ-BUDAT TO WV-BDAT.
      *    IF WV-BDAT(1:4) NOT = WV-VDAT(1:4) GO Q.
      *    display bz-seite at 1020.

           if bz-sym = 19 and bz-ggkto = 999999 and bz-belnr = 0
               perform sammelbuch go g.

       i.  ADD 230 VDU-ECK GIVING VDU-LP.
           DISPLAY BZ-USE AT VDU-LP "/" BZ-SEITE.
           MOVE BZ-JKEY TO WH-JOKEY.
           MOVE WE-SYM(BZ-SYM + 1) TO WT-TX.
           MOVE BZ-KTONR TO WH-NUM.
           PERFORM LIESKTO.
           MOVE 0 TO WK-SKTO.
           MOVE BZ-BUBET TO WK-BUBET WK-NETTO.
           MOVE BZ-VM TO WE-UST.
           EVALUATE BZ-VM
               WHEN 3 IF BZ-MW NOT = 0 ADD BZ-MW BZ-B1 TO WK-SKTO
                      ADD BZ-MW TO KO-SKTOERL
                      SUBTRACT BZ-B1 FROM KO-SKTOVST
                      ADD BZ-B1 TO KO-SKTOERL
               WHEN 4 ADD BZ-MW BZ-B1 BZ-B2 GIVING WK-SKTO
                     MOVE BZ-MW TO WK-UDIF
                     IF BZ-U1 NOT = 0 MOVE BZ-B1 TO WK-BET
                         ADD WK-BET TO KO-SKTOAUF(BZ-U1)
                         SUBTRACT WK-BET FROM KO-MWSTBMG(BZ-U1)
                         MOVE WT-UST(BZ-U1) TO WH-UST
                         COMPUTE WK-BET = (WH-UST / 100) * BZ-B1
                         ADD WK-BET TO KO-SKTOAUF(BZ-U1)
                         SUBTRACT WK-BET FROM WK-UDIF
                         end-if
                     IF BZ-U2 NOT = 0 MOVE BZ-B2 TO WK-BET
                         ADD WK-BET TO KO-SKTOAUF(BZ-U2)
                         SUBTRACT WK-BET FROM KO-MWSTBMG(BZ-U2)
                         MOVE WT-UST(BZ-U2) TO WH-UST
                         COMPUTE WK-BET = (WH-UST / 100) * BZ-B2
                         ADD WK-BET TO KO-SKTOAUF(BZ-U2)
                         SUBTRACT WK-BET FROM WK-UDIF
                         end-if
                     IF BZ-U1 NOT = 0 ADD WK-UDIF TO KO-SKTOAUF(BZ-U1)
                     else IF BZ-U2 NOT = 0
                              ADD WK-UDIF TO KO-SKTOAUF(BZ-U2).
           EVALUATE WH-KAT
               WHEN 0 PERFORM SACHSUM
               WHEN 2 PERFORM DEBSUM
               WHEN 3 PERFORM KREDSUM.
           PERFORM ADD-UST.
      *--------------------------------------> Journalsummenvergleich <-
       K.  ADD KO-VST KO-SKTOAUF(1) KO-SKTOAUF(2) KO-SKTOAUF(3)
                      KO-SKTOAUF(4) KO-SKTOAUF(5) KO-SKTOAUF(6)
                            KO-JSOLL GIVING WD-SALDO WE-SOLL.
           DISPLAY WD-SALDO with highlight AT 2136.
           ADD KO-MWST KO-SKTOERL KO-KRDIF
                            KO-JHABEN GIVING WD-SALDO WE-HABEN.
           DISPLAY WD-SALDO with highlight AT 2150.
           SUBTRACT WE-HABEN FROM WE-SOLL GIVING WD-SALDO.
           DISPLAY WD-SALDO with highlight AT 2243.
           IF WE-SOLL NOT = WE-HABEN;
               IF WD-KZ NOT = 1 GO G
               else DISPLAY "Differenz -1 " AT 2401 WD-SALDO
                    PERFORM WEITER GO G.
      *    IF WD-KZ = 1 INITIALIZE KO-SASUM.
           IF WH-SEITE NOT = 0; IF BZ-SEITE < WH-SEITE GO G.
      *-----------------------------------------> ob Journalsaldo = 0 <-
           DISPLAY "weiter mit beliebiger Taste <      > "
               AT 2301 WH-SEITE.
           CALL "CAUP" USING "0023306006" WH-CREG.
           MOVE WH-NUM TO WH-SEITE.
           GO G.
       N.  move wh-bkey to bz-jkey.
           if wd-kz = 1 go s.
           go f.
       Q.

       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      *************************************************** Addition UST *
       ADD-UST SECTION.
       A.  EVALUATE WE-UST
               WHEN 1 if wh-kat = 0;
                          if bz-sh = 1 add bz-mw TO KO-MWST
                          else subtract bz-mw from ko-mwst
                      else
                      if wh-kat > 1;
                          if bz-sh = 0 add bz-mw TO KO-MWST
                          else subtract bz-mw from ko-mwst end-if
                    IF BZ-U1 NOT = 0 ADD BZ-B1 TO KO-MWSTBMG(BZ-U1)
                    end-if
                    IF BZ-U2 NOT = 0 ADD BZ-B2 TO KO-MWSTBMG(BZ-U2)
               WHEN 2 if wh-kat = 0;
                          if bz-sh = 0 add bz-mw TO KO-VST
                          else subtract bz-mw from ko-vst
                      else
                      if wh-kat > 1;
                          if bz-sh = 1 add bz-mw TO KO-VST
                      else subtract bz-mw from ko-vst end-if.
       Z.  EXIT.
      ******************************* Summenbildung Sachkonten Kat.= 0 *
       SACHSUM SECTION.
       A.  EVALUATE BZ-SH ALSO WH-BN
               WHEN 0 ALSO 1 ADD WK-NETTO TO KO-JSOLL
               WHEN 0 ALSO 0 ADD WK-BUBET TO KO-JSOLL
               WHEN 1 ALSO 1 ADD WK-NETTO TO KO-JHABEN
               WHEN 1 ALSO 0 ADD WK-BUBET TO KO-JHABEN.
       Z.  EXIT.
      ******************************** Summenbildung Debitoren Kat.= 2 *
       DEBSUM SECTION.
       A.  COMPUTE WL = DE-KTOART + 1 + (BZ-SH * 3).
           ADD BZ-BUBET TO KO-DEBSUM(WL).
           ADD WK-SKTO  TO KO-DEBSUM(WL).
           IF BZ-SH = 0 ADD BZ-BUBET TO KO-JSOLL
                   ELSE ADD BZ-BUBET TO KO-JHABEN
                        ADD WK-SKTO TO KO-JHABEN.
       Z.  EXIT.
      ******************************* Summenbildung Kreditoren Kat.= 2 *
       KREDSUM SECTION.
       A.  COMPUTE WL = KR-KTOART + 1 + (BZ-SH * 3).
           ADD BZ-BUBET TO KO-KREDSUM(WL).
           ADD WK-SKTO  TO KO-KREDSUM(WL).
           IF BZ-SH = 0 ADD BZ-BUBET TO KO-JSOLL
                        ADD WK-SKTO TO KO-JSOLL
                   ELSE ADD BZ-BUBET TO KO-JHABEN.
       Z.  EXIT.
      ******************************************************************
       FIRST-JOUR SECTION.
       A.  MOVE WE-USE TO BZ-USE.
           MOVE 0 TO BZ-SEITE.
           START BUCHZEIL KEY NOT < BZ-JKEY INVALID
               MOVE 0 TO BZ-SEITE BZ-USE GO E.
           MOVE SPACE TO BZ-SATZ(14:).
       C.  READ BUCHZEIL NEXT NO LOCK AT END MOVE 0 TO BZ-SEITE.
           IF ZUGRIF PERFORM BESETZT GO C.
       E.  IF BZ-USE NOT = WE-USE SET ESC TO TRUE GO Z.
       Z.  EXIT.
      *****************************************************************
       DATDREH SECTION.
       A.  MOVE WC-TAG  TO WZ-TAG VDU-JAHR.
           MOVE WC-MONAT TO WZ-MONAT VDU-MONAT.
           MOVE WC-JAHR TO WZ-JAHR VDU-TAG.
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
               WHEN 1 MOVE WH-NUM TO SA-KTONR GO C
      *        WHEN 1 SET ESC TO TRUE GO X
               WHEN 0 MOVE WH-NUM TO SA-KTONR GO C
               WHEN OTHER GO X.
      *---------------------------------------------> lesen Sachkonto <-
       C.  READ SACHBUCH NOT INVALID MOVE SA-BEZ TO WT-TX
               MOVE SA-MCODE TO WK-MCODE
               MOVE SA-BTNT TO WH-BN
               MOVE SA-SOLL TO WE-SOLL
               MOVE SA-HABEN TO WE-HABEN
               ADD WE-SOLL WE-HABEN GIVING WE-SALDO GO Z.
           IF ZUGRIF PERFORM BESETZT GO X.
           GO X.
      *---------------------------------------------> lesen Debitoren <-
       E.  READ DEBITOR NOT INVALID MOVE DE-BEZ TO WT-TX
               INSPECT WT-TX REPLACING ALL "#" BY ","
               MOVE DE-MCODE TO WK-MCODE
               MOVE 0 TO WH-BN
               MOVE DE-SOLL TO WE-SOLL
               MOVE DE-HABEN TO WE-HABEN
               ADD WE-SOLL WE-HABEN GIVING WE-SALDO GO Z.
           IF ZUGRIF PERFORM BESETZT GO X.
           GO X.
      *--------------------------------------------> lesen Kreditoren <-
       G.  READ KREDIT NOT INVALID MOVE KR-BEZ TO WT-TX
               MOVE KR-MCODE TO WK-MCODE
               MOVE 0 TO WH-BN
               MOVE KR-SOLL TO WE-SOLL
               MOVE KR-HABEN TO WE-HABEN
               ADD WE-SOLL WE-HABEN GIVING WE-SALDO GO Z.
           IF ZUGRIF PERFORM BESETZT GO X.
       X.  SET FEHLER TO TRUE.
           DISPLAY "Konto fehlt: " at 2401 WH-NUM.
           PERFORM WEITER.
       Z.  EXIT.
      *************************************** Korrektur Buchungszeilen *
       KORREKT SECTION.
       A.  CALL "CAUP" USING "0701010365000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Korrektur BZ " with highlight AT VDU-LP.
           ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "Betrag:                +/-:                Neu:"
               AT VDU-LP.
           MOVE BZ-BUBET TO WD-SALDO.
           ADD 211 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-SALDO with highlight AT VDU-LP.
           IF BZ-SH = 0 SUBTRACT BZ-BUBET FROM WE-SOLL
                   else SUBTRACT BZ-BUBET FROM WE-HABEN.
       C.  DISPLAY "<esc>= Abbruch, <ret>= Betrag" AT 2301.
           MOVE 0 TO WH-WERT.
           CALL "CAUP" USING "1002318210" WH-CREG.
           IF ESC GO X.
           IF NOT RET GO C.
           MOVE WH-WERT TO WD-SALDO.
           DISPLAY WD-SALDO with highlight AT VDU-LP.
           ADD WH-WERT TO BZ-BUBET.
           MOVE BZ-BUBET TO WD-SALDO.
           ADD 251 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-SALDO with highlight AT VDU-LP.
           IF BZ-SH = 0 ADD BZ-BUBET TO WE-SOLL
                   else ADD BZ-BUBET TO WE-HABEN.
       E.  DISPLAY "<esc>= Abbruch, <ret>= ok!" AT 2301.
           CALL "CAUP" USING "0023280000" WH-CREG.
           IF ESC GO X.
           IF NOT RET GO E.
           REWRITE BZ-SATZ.
           UNLOCK BUCHZEIL.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      **************************************** Kontrolle Sammelbuchung *
       SAMMELBUCH SECTION.
       A.  PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 20 OR
               WO-KTONR(WX) = BZ-KTONR CONTINUE.
           IF WX > 20 GO Z.
           EVALUATE WX
               WHEN 11 MOVE 01 TO WS    *> Debit.  0 S
               WHEN 12 MOVE 02 TO WS    *> Debit.  1 S
               WHEN 13 MOVE 03 TO WS    *> Debit.  2 S
               WHEN 11 MOVE 04 TO WS    *> Debit.  0 H
               WHEN 12 MOVE 05 TO WS    *> Debit.  1 H
               WHEN 13 MOVE 06 TO WS    *> Debit.  2 H
               WHEN 14 MOVE 07 TO WS    *> Kredit. 0 S
               WHEN 15 MOVE 08 TO WS    *> Kredit. 1 S
               WHEN 16 MOVE 09 TO WS    *> Kredit. 2 S
               WHEN 14 MOVE 10 TO WS    *> Kredit. 0 H
               WHEN 15 MOVE 11 TO WS    *> Kredit. 1 H
               WHEN 16 MOVE 12 TO WS    *> Kredit. 2 H
               WHEN 01 MOVE 15 TO WS    *> Vorsteuer S
               WHEN 02 MOVE 16 TO WS    *> Mehrwst.  H
               WHEN 03 MOVE 26 TO WS    *> Sktoerl.  H
               WHEN 04 MOVE 27 TO WS    *> Sktoerl.  H
               WHEN 05 MOVE 28 TO WS    *> Sktoaufw. S
               WHEN 06 MOVE 29 TO WS    *> Sktoaufw. S
               WHEN 07 MOVE 30 TO WS    *> Sktoaufw. S
               WHEN 08 MOVE 31 TO WS    *> Sktoaufw. S
               WHEN 09 MOVE 32 TO WS    *> Sktoaufw. S
               WHEN 10 MOVE 33 TO WS    *> Sktoaufw. S
               WHEN 34 ADD KO-SKTOVST TO KO-VST
                       ADD KO-SKTOVST TO KO-SKTOERL GO Z
               WHEN OTHER GO X.
           IF WS > 27 ADD WS -27 GIVING WBZ-U1
               MOVE WT-UST(WBZ-U1) TO WH-UST
               DIVIDE 100 INTO WH-UST GIVING WC-UST
               ADD 1 TO WC-UST
               move ko-rs(ws) to wbz-bubet
               DIVIDE WC-UST INTO WBZ-BUBET GIVING WBZ-B1
               ADD -1 TO WC-UST
               COMPUTE WBZ-MW ROUNDED = WC-UST * WBZ-B1
               SUBTRACT WBZ-MW FROM WBZ-BUBET
               MOVE 1 TO WBZ-VM
               if wbz-bubet = bz-bubet go x.
           IF WS > 25 SUBTRACT KO-SKTOVST FROM WBZ-MW
                      MOVE KO-RS(WS) TO WBZ-BUBET
                      MOVE 0 TO KO-SKTOVST
                      IF WBZ-MW NOT = 0 MOVE 2 TO WBZ-VM
                      IF WBZ-BUBET NOT = BZ-BUBET OR
                         WBZ-MW NOT = BZ-MW
                         DISPLAY "Skto-dif." AT 2401
                         GO X.
           IF KO-RS(WS) = BZ-BUBET GO X
               else add 3 to ws
                    IF KO-RS(WS) = BZ-BUBET go x.
      *-------------------------------------------> Buchungskorrektur <-
           if ws > 16 and ws < 25 go z.
           move ko-rs(ws) to bz-bubet.
      *-------------------------> hier stoppen wenn Sammelkontofehler <-
      *    rewrite bz-satz.
       X.
      *    MOVE 0 TO KO-RS(WS).
       Z.  EXIT.
      ***************************************** autom. Konten einlesen *
       AUTKONT SECTION.
       A.  MOVE 9 TO WH-KEY.
       B.  READ KONSTANT INVALID STOP RUN.
           IF ZUGRIF PERFORM BESETZT GO Z.
           MOVE KO-TSATZ TO WO-TSATZ.
       Z.  EXIT.
      *************************** PrÅfung ob alle Rechnungen vorhanden *
       REPRUEF SECTION.
       A.  CALL "CAUP" USING "0707100550000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " RechnungsprÅfung " AT VDU-LP.
           OPEN I-O RECHNUM.
           PERFORM LADE-DRU.
           MOVE "RELIST.LST" TO WH-DRUNAM.
           MOVE 2 TO WH-P1.
           MOVE 5 TO WH-P2
           IF WM-DRU = 1 MOVE 15 TO WH-P1                      *> Laser
                         MOVE 0 TO WZ-SCHALT.
           PERFORM BEG-DRU.
       C.  DISPLAY "<ret>= Start, <esc>= Abbruch < >" AT 2301.
           CALL "CAUP" USING "0023310000" WH-CREG.
           IF ESC GO X.
           IF ENDE GO Q.
           IF NOT RET GO C.
           CLOSE RECHNUM.
           OPEN OUTPUT RECHNUM.
           CLOSE RECHNUM.
           OPEN I-O RECHNUM.
           MOVE LOW-VALUES TO BZ-JKEY.
           MOVE WE-WKZ TO BZ-USE.
           START BUCHZEIL KEY > BZ-JKEY INVALID GO X.
           MOVE SPACE TO BZ-SATZ(14:).
       E.  READ BUCHZEIL NEXT NO LOCK AT END MOVE 0 TO BZ-SEITE.
           IF ZUGRIF PERFORM BESETZT GO E.
           IF WE-WKZ NOT = BZ-USE GO Q.
           IF BZ-SYM NOT = 0 AND BZ-SYM NOT = 10 GO E.
           DISPLAY BZ-BELNR AT 2020.
           MOVE BZ-BELNR TO RE-BELNR.
           MOVE BZ-BUBET TO RE-BUBET.
           MOVE BZ-KTONR TO RE-KTONR WD-KZ.
           IF WD-KZ NOT = 2 GO E.
           WRITE RE-SATZ.
           IF WF-STATUS NOT = 02 GO E.
           MOVE BZ-BELNR TO DRA-BELNR.
           DIVIDE 10 INTO BZ-KTONR GIVING DRA-KTONR.
           MOVE BZ-BUBET TO DRA-BUBET.
           PERFORM DRUCK.
           GO E.
      *-----------------------------------> fehlende Nummern anzeigen <-
       Q.  MOVE 0 TO RE-BELNR WV-BELNR.
           START RECHNUM KEY NOT < RE-KEY INVALID GO X.
       R.  READ RECHNUM NEXT AT END GO W.
           IF ZUGRIF PERFORM BESETZT GO R.
           DISPLAY RE-BELNR AT 2120.
           IF WV-BELNR = RE-BELNR GO R.
       S.  ADD 1 TO WV-BELNR.
           IF WV-BELNR = RE-BELNR GO R.
           DISPLAY "Soll: " AT 2020 WV-BELNR with highlight.
           MOVE WV-BELNR TO DRA-BELNR.
           IF RE-BELNR > 15000 GO W.
           PERFORM DRUCK.
           GO S.
       W.  DISPLAY "PrÅfung beendet" AT 2401.
           PERFORM WEITER.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           CLOSE RECHNUM.
           PERFORM END-DRU.
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
           READ KONSTANT WITH NO LOCK NOT INVALID GO X.
           IF ZUGRIF PERFORM BESETZT GO A.
       X.  MOVE KOP-TX TO WK-DSTG.
       Z.  EXIT.
