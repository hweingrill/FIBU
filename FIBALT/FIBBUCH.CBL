      $SET LINKCOUNT"312" SEGSIZE "55000" ANS85 BOUND AUTOLOCK NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     FIBBUCH.
      ******************************************************************
      * - Erstkonto wird gelesen u. freigegeben und vor SaldoverÑnderung
      *   neuerlich gelesen. Gegenkto. (Einzel-, Sammel- u. Aufteilungs-
      *   buchng.) wird als Erstkonto ohne freigabe gelesen.
      * - offen: OP-Verwaltung u. Zahlungssbuchng. mit Skontoberichtg.
      ******************************************************************
       ENVIRONMENT    DIVISION.
       CONFIGURATION   SECTION.
       SOURCE-COMPUTER.  PC.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY FIBUSEC.CPY.
       DATA DIVISION.
       FILE SECTION.
       COPY FIBUFD.CPY.
      ******************************************************************
       WORKING-STORAGE SECTION.
       01  WH-CALL.
           03  WL-CA                   PIC 99.
           03  WL-REST                 PIC 9(13).
       COPY WHCREG.CPY.
       01  WH-REG.
           03  VDU-AB                  PIC 9999.
           03  VDU-REL                 PIC 9999.
           03  WU-MOD                  PIC 99        COMP.
           03  WR-MOD                  PIC 99        COMP.
           03  WBX                     PIC 99        COMP.
           03  WB                      PIC 99        COMP.
           03  WE                      PIC 99        COMP.
           03  WI                      PIC 99        COMP.
           03  WL                      PIC 99        COMP.
           03  WS                      PIC 99        COMP.
           03  WU                      PIC 99        COMP.
           03  IW                      PIC 99        COMP.
           03  IX                      PIC 99        COMP.
           03  IY                      PIC 99        COMP.
           03  IZ                      PIC 99        COMP.
           03  WM-G                    PIC 99        COMP.
           03  WM-L                    PIC 99        COMP.
           03  WM-M                    PIC 99        COMP.
           03  WM-GB                   PIC 99        COMP.
             88 OHNE      VALUE 0.   88 EINZEL     VALUE 1.
             88 SAMMEL    VALUE 2.   88 AUFTEIL    VALUE 4.
             88 NEUPER    VALUE 99.
           03  WK-MCODE                PIC X(10).
           03  WK-AMCODE               PIC X(10).
           03  WT-JOTAB.                        *> Journalzeilenanzeige
               05 WT-JKEY              PIC X(4)   occurs 17.
           03  WT-ERFTAB.                       *> Buchungszeilenanzeige
               05 WT-KEY               PIC X(9)   occurs 17.
           03  WH-BZSATZ               PIC X(128).
           03  WH-GGSATZ               PIC X(128).
           03  WH-KTOSATZ              PIC X(190).
      *    03  WM-U                    PIC 9         COMP.
      *    03  WM-U2                   PIC 9         COMP.
           03  WM-B                    PIC S9(9)V99  COMP-3.
      *    03  WM-B2                   PIC S9(9)V99  COMP-3.
           03  WH-TEIL                 PIC S999V9(7) COMP.
           03  WV-BAS                  PIC S9(9)V99  COMP.
           03  WK-UST                  PIC 99        COMP.
           03  WH-UST                  PIC S99V99    COMP.
           03  WC-UST                  PIC S99V9999  COMP.
           03  WH-ERG                  PIC S9(7)V99  COMP.
           03  WK-MWST                 PIC S9(7)V99  COMP.
           03  WK-BASIS                PIC S9(9)V99  COMP.
           03  WK-BRUTTO               PIC S9(9)V99  COMP.
           03  WK-NETTO                PIC S9(9)V99  COMP.
           03  WK-BUBET                PIC S9(9)V99  COMP.
           03  WK-SOLL                 PIC S9(9)V99  COMP.
           03  WK-HABEN                PIC S9(9)V99  COMP.
           03  WK-SKTO                 PIC S9(9)V99  COMP.
           03  WK-BET                  PIC S9(9)V99  COMP.
           03  WK-HIUST                PIC S9(7)V99  COMP.
           03  WK-FDIF                 PIC S9(7)V99  COMP.
           03  WF-MWST                 PIC S9(7)V99  COMP.
           03  WF-BASIS                PIC S9(9)V99  COMP.
           03  WF-BRUTTO               PIC S9(9)V99  COMP.
           03  WF-NETTO                PIC S9(9)V99  COMP.
           03  WF-BUBET                PIC S9(9)V99  COMP.
           03  WF-SOLL                 PIC S9(9)V99  COMP.
           03  WF-HABEN                PIC S9(9)V99  COMP.
           03  WF-SKTO                 PIC S9(9)V99  COMP.
           03  WF-BET                  PIC S9(9)V99  COMP.
           03  WF-HIUST                PIC S9(7)V99  COMP.
           03  WZ-UST                  PIC S9(7)V99  COMP.
           03  WZ-BET                  PIC S9(9)V99  COMP.
           03  WK-NFUST                PIC 9(6)      COMP.
           03  WK-SRKTO                PIC 9(6)      COMP.
           03  WK-TRKTO                PIC 9(6)      COMP.
           03  WK-UMBSY                PIC 99        COMP.
           03  WK-U1                   PIC 99        COMP.
           03  WH-BKEY.
               05 WH-KTONR             PIC 9(6)      COMP.
               05 WH-BUDAT             PIC 9(8)      COMP.
               05 WH-LFD               PIC 9999      COMP.
           03  WH-JOKEY                PIC X(4).
           03  WH-KAT                  PIC 9         COMP.
           03  WH-BN                   PIC 99        COMP.
           03  WH-AKAT                 PIC 9         COMP.
           03  WH-ABN                  PIC 99        COMP.
           03  WH-GKAT                 PIC 9         COMP.
           03  WH-GBN                  PIC 99        COMP.
           03  WH-U1                   PIC 99        COMP.
           03  WH-VM                   PIC 99        COMP.
           03  WH-JS                   PIC 9(7)      COMP-X.
           03  WM-KTONR                PIC 9(6)      COMP.
           03  WM-FDKTO                PIC 9(6)      COMP.
           03  WM-UST                  PIC S9(8)V99  COMP.
      *-----------------------------------------------> Anzeigefelder <-
           03  WD-JONUM                PIC Z.ZZ9.
           03  WD-SALDO                PIC ZZ.ZZZ.ZZ9,99-.
           03  WD-UST                  PIC ZZZZZ.ZZ9,99-.
           03  WD-NUM                  PIC ZZZZZZ9.
           03  WD-KTO                  PIC ZZZZ9,9.
           03  WD-SAKO                 PIC 99999,9.
           03  WD-KZ                   PIC 9.
           03  WD-PROZ                 PIC Z9.
           03  WD-PZ                   PIC 9,9.
           03  WD-TG                   PIC ZZ9.
           03  WD-KURS                 PIC ZZZZ,ZZZZZ.
           03  WV-TX                   PIC X(40).
      *------------------------------------------> Konditionentabelle <-
           03  WH-KK                   PIC 9(13).
           03  WR-KK REDEFINES WH-KK.
               05  WH-SK1              PIC 9V9.
               05  WH-TG1              PIC 999.
               05  WH-SK2              PIC 9V9.
               05  WH-TG2              PIC 999.
               05  WH-TGN              PIC 999.
           03  WZ-SYMT                 PIC 9999.
       COPY FIBUEXT.CPY.
       COPY FIBUDECL.CPY.
       END DECLARATIVES.
      *****************************************************************
       STEUER SECTION.
       A.  MOVE WL-CALL TO WH-CALL.
           MOVE WL-CREG TO WH-CREG.
           PERFORM BUCHEN.
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
       A.  DISPLAY "** Datenende **" with highlight blink AT 2466.
           SET FINE TO TRUE.
       Z.  EXIT.
      ************************************** fÅr Journalbuchungszeilen *
       BS-JZEIL SECTION.
       A.  ADD 1 TO IX.
       B.  IF IX > 16 CALL "CAUP" USING "17SCROLLF" WH-CREG
               PERFORM VARYING IX FROM 1 BY 1 UNTIL IX = 16
                   MOVE WT-JKEY(IX + 1) TO WT-JKEY(IX)
               end-perform MOVE LOW-VALUE TO WT-JKEY(IX).
       Z.  EXIT.
      **************************************** fÅr Kontobuchungszeilen *
       BS-BZEIL SECTION.
       A.  ADD 1 TO IY.
           IF IY < 9 GO Z.
       B.  CALL "CAUP" USING "17SCROLLF" WH-CREG.
           CALL "CAUP" USING "17SCROLLF" WH-CREG.
           PERFORM VARYING IY FROM 1 BY 1 UNTIL IY > 7
               MOVE WT-KEY(IY + 1) TO WT-KEY(IY).
           MOVE LOW-VALUE TO WT-KEY(IY).
       Z.  EXIT.
      ***************************************** BZ-SATZ initialisieren *
       INIT-BZ SECTION.
       A.  INITIALIZE BZ-SATZ.
       Z.  EXIT.
      ************************************** eigentl. Buchungsprogramm *
       BUCHEN SECTION.
       A.  MOVE "   Buchungsprogramm" TO WK-GEB.
           MOVE 0 TO WE-BELNR WR.
           MOVE 9 TO WH-KEY.
           READ KONSTANT IGNORE LOCK INVALID NEXT SENTENCE.
           MOVE KO-KTONR(4) TO WM-FDKTO.
           PERFORM INIT-BZ.
           MOVE 0 TO VDU-ECK.
       C.  DISPLAY ALL SPACES with size 50 at 2501.
           CALL "CAUP" USING "06KOPF" WH-CREG.
           UNLOCK KONSTANT.
           UNLOCK OPDATEI.
           UNLOCK DEBITOR.
           UNLOCK KREDIT.
           UNLOCK SACHBUCH.
           UNLOCK BUCHZEIL.
           DISPLAY "<ret-leer>   = Buchen"             AT 0725
                   "Kto-Nr+<ret> = Kontoabfrage"       AT 0925
                   "<Ende>       = Journalabschlu·"    AT 1125
                   "<Einf>       = Journalsummen"      AT 1325
                   "<esc>        = Ende"               AT 1525
                   "bitte wÑhlen Sie:"      with highlight AT 1725.
           CALL "CAUP" USING "0017436110" WH-CREG.
           EVALUATE TRUE
               WHEN HELP CALL "CAUP" USING "190073007401" WH-CREG GO C
               WHEN ENDE PERFORM P-PRUEF
                         IF ESC GO X
                         else MOVE 49 TO WH-PG
                              MOVE "FIBJEND 10JEND" TO WT-TX GO Z
               WHEN ESC  GO X
               WHEN ENTF MOVE 49 TO WH-PG
                         MOVE "FIBREG  50AUSL" TO WT-TX GO Z
               WHEN RET; IF WH-MCODE = SPACE GO D
                         ELSE GO Q
               WHEN EINF CALL "FIBANZ" USING "10JOUR" WH-CREG
                         CANCEL "FIBANZ".
           GO C.
       D.  PERFORM P-PRUEF.
           IF ESC GO X.
           PERFORM JOUR-ZEIL.
           GO C.
      *-----------------------------------------------> Kontenabfrage <-
       Q.  IF WH-WERT < 0,4 OR WH-NUM < 4
               CALL "FIBANZ" USING "99SUCH" WH-CREG
               CANCEL "FIBANZ".
           IF WH-NUM = 0 GO C.
           PERFORM LIESKTO-NOLOCK.
           IF FEHLER GO C.
      *    move wh-kat to wh-akat.
           MOVE 1 TO WH-KAT.                        *> Kateg. Erstkonto
           MOVE WH-BN TO WH-ABN.                    *> brutto/netto - KZ
           MOVE WH-NUM TO WK-KTONR BZ-KTONR.
           MOVE 0 TO KO-BUDAT.
           PERFORM BUCH-ZEIL.                       *> nur Abfrage
           GO C.
       X.  MOVE 48 TO WH-PG.
       Z.  EXIT.
      **************************************************** weiterlesen *
       ABZEIL SECTION.
       A.  PERFORM VARYING IX FROM 16 BY -1 UNTIL IX = 0
               OR WT-JKEY(IX) NOT = LOW-VALUE CONTINUE.
           IF IX = 0 MOVE 1 TO IX SET FINE TO TRUE GO X.
       C.  MOVE WT-JKEY(IX) TO WH-JOKEY.
           IF AB OR (BAB AND WH-NUM = 0) PERFORM READBEW
               ADD 1 TO IX
               MOVE 0 TO WI WR
               IF AB MOVE 14 TO WR.
       E.  PERFORM NEXTAB WITH TEST AFTER UNTIL WI = 8 OR WR = 15.
           IF WI NOT = 8 MOVE 9 TO WI.
       X.  IF FINE PERFORM NODATA.
       Z.  EXIT.
      ******************************************************************
       NEXTAB SECTION.
       E.  MOVE SPACE TO BZ-SATZ(14:).
           READ BUCHZEIL NEXT AT END MOVE 8 TO WI GO Z.
           IF ZUGRIF PERFORM BESETZT GO E.
           IF WE-USE NOT = BZ-USE MOVE 8 TO WI SET FINE TO TRUE GO Z.
           IF WI < 3 PERFORM VARYING WF FROM 1 BY 1 UNTIL WF > 16
               IF BZ-JKEY = WT-JKEY(WF) GO E.
           IF IX = 17 CALL "CAUP" USING "17SCRAUF" WH-CREG
               PERFORM VARYING IX FROM 1 BY 1 UNTIL IX = 16
                   MOVE WT-JKEY(IX + 1) TO WT-JKEY(IX)
                   END-PERFORM.
           PERFORM JOUR-DIS.
           IF BAB ADD 1 TO IX.
       Z.  EXIT.
      ******************************************************************
       AUFZEIL SECTION.
       A.  IF WT-JKEY(1) = LOW-VALUE SET FINE TO TRUE GO X.
           MOVE WT-JKEY(1) TO WH-JOKEY BZ-JKEY.
           IF WR = -999 MOVE LOW-VALUE TO WT-KEY(1) WT-JKEY(1).
           MOVE 0 TO WR.
           IF AUF MOVE 14 TO WR.
           START BUCHZEIL KEY < BZ-JKEY INVALID SET FINE TO TRUE GO X.
           MOVE 1 TO IX WI.
           PERFORM NEXTAUF WITH TEST AFTER UNTIL WI = 2 OR WR = 15.
           MOVE 1 TO IX.
       X.  IF FINE PERFORM NODATA.
       Z.  EXIT.
      ******************************************************************
       NEXTAUF SECTION.
       N.  MOVE SPACE TO BZ-SATZ(14:).
           READ BUCHZEIL PREVIOUS AT END MOVE 2 TO WI GO Z.
           IF ZUGRIF PERFORM BESETZT GO N.
           IF WE-USE NOT = BZ-USE SET FINE TO TRUE MOVE 2 TO WI GO Z.
           IF WI > 8 PERFORM VARYING WF FROM 16 BY -1 UNTIL WF < 1
               IF BZ-JKEY = WT-JKEY(WF) GO N.
           MOVE VDU-LP TO VDU-AB.
           MOVE 1500 TO VDU-LP.
           CALL "CAUP" USING "18CLRWIN" WH-CREG.
           MOVE VDU-AB TO VDU-LP.
           CALL "CAUP" USING "22SCRAB" WH-CREG.
           PERFORM VARYING IX FROM 16 BY -1 UNTIL IX < 2
              MOVE WT-JKEY(IX - 1) TO WT-JKEY(IX).
           COMPUTE VDU-LP = IX * 200 + 300.
           PERFORM JOUR-DIS.
       Z.  EXIT.
      ******************************************************************
       READBEW SECTION.
       A.  MOVE WH-JOKEY TO BZ-JKEY.
       C.  MOVE SPACE TO BZ-SATZ(14:).
           READ BUCHZEIL KEY BZ-JKEY INVALID MOVE LOW-VALUE TO WT-JOTAB.
           IF ZUGRIF PERFORM BESETZT GO C.
       Z.  EXIT.
      ******************************************* Einzelzeile anzeigen *
       ANZEIG SECTION.
       A.  SET RET TO TRUE.
           IF IX = 0 OR WT-JKEY(IX) = LOW-VALUE PERFORM INIT-BZ
               COMPUTE VDU-L = IX + 4
               CALL "CAUP" USING "1300012280" WH-CREG GO Z.
           MOVE WT-JKEY(IX) TO WH-JOKEY BZ-JKEY.
       C.  MOVE SPACE TO BZ-SATZ(14:).
           READ BUCHZEIL KEY IS BZ-JKEY INVALID PERFORM NO-REC
               SET ESC TO TRUE GO Z.
           IF ZUGRIF PERFORM BESETZT GO C.
           PERFORM JOUR-DIS.
       Z.  EXIT.
      ******************************************* Einzelzeile anzeigen *
       BUZEIG SECTION.
       A.  SET RET TO TRUE.
           IF IY = 0 OR WT-KEY(IY) = LOW-VALUE PERFORM INIT-BZ GO Z.
           MOVE WT-KEY(IY) TO WH-BKEY BZ-KEY.
       C.  MOVE SPACE TO BZ-SATZ(14:).
           READ BUCHZEIL KEY IS BZ-KEY INVALID PERFORM NO-REC
               SET ESC TO TRUE GO Z.
           IF ZUGRIF PERFORM BESETZT GO C.
           PERFORM BUCH-DIS.
       Z.  EXIT.
      *********************************************** Saldo Gegenkonto *
       GKSALDO SECTION.
       A.  IF WE-GGKTO = 0 GO Z.
           MOVE VDU-ECK TO VDU-AB.
           MOVE WE-GGKTO TO WH-NUM.
           PERFORM LIESKTO-NOLOCK.
           IF FEHLER GO Z.
           PERFORM ANZ-GGKTO.
           MOVE VDU-AB TO VDU-ECK.
       Z.  EXIT.
      ******************************************************************
       JOUR-ZEIL SECTION.
       A.  PERFORM VARYING IX FROM 17 BY -1 UNTIL IX = 0
               OR WT-JKEY(IX) NOT = LOW-VALUE
               END-PERFORM.
       C.  IF NOT XPOS PERFORM BS-JZEIL.
      *-------------------------------------------------> Kontonummer <-
       D.  DISPLAY "<ret>= Kto.-Nr., <esc>= zurÅck, <Ende>= Ende, <ret-l
      -        "eer>= letzte Kto.-Nr." AT 2301.
           DISPLAY "Suchen: <alpha+Kat+ret>, Nr.+<Pos 1>= Belegsuche, <#
      -        ">= GGkto." AT 2401.
           DISPLAY "Journalsummen:" AT 0303.
           DISPLAY "S:                H:" AT 0322.
           ADD KO-VST KO-SKTOAUF(1) KO-SKTOAUF(2) KO-SKTOAUF(3)
                      KO-SKTOAUF(4) KO-SKTOAUF(5) KO-SKTOAUF(6)
                            KO-JSOLL GIVING WD-SALDO WH-WERT.
           DISPLAY WD-SALDO with highlight AT 0325.
           ADD KO-MWST KO-SKTOERL KO-KRDIF
                            KO-JHABEN GIVING WD-SALDO WK-BRUTTO.
           DISPLAY WD-SALDO with highlight AT 0343 " Saldo: ".
           SUBTRACT WK-BRUTTO FROM WH-WERT GIVING WD-SALDO.
           DISPLAY WD-SALDO with highlight AT 0000.
           IF WM-G = 1 GO E.
           PERFORM ANZEIG.
           IF BZ-OPLFD = 999 DISPLAY "<>= Zahlungszuordnung" AT 2501.
           IF WM-M = 2 PERFORM BUTEX.

           if bz-kurs = 0 or bz-kurs = 100 go e.
           if bz-kurs = 7,2672834 move 7,2672835 to bz-kurs
               end-if
               compute wf-bet rounded = bz-fwbet * bz-kurs / 100
               move wf-bet to wd-saldo
               display wd-saldo with highlight at 2545.


       E.  COMPUTE VDU-L = IX + 4.
           CALL "CAUP" USING "1000026108" WH-CREG.
           CALL "CAUP" USING "1325012580" WH-CREG.
           MOVE LOW-VALUE TO WE-RETAB AZ-SATZ.
      *-------------------------------> * Zeilen war Korrekturzeilen <-

      *    compute wh-erg rounded = bz-b1 * 0,10.
      *    COMPUTE wh-erg rounded = wh-erg + (bz-b2 * 0,20).
      *    if bz-mw NOT = 0;
      *        if bz-mw not = wh-erg
      *            DISPLAY "UST?" AT 2401.

      *    if kist; if bz-kurs = 7,2672834 move 7,2672835 to bz-kurs
      *             end-if set ret to true
      *        compute wf-bet rounded = bz-fwbet * bz-kurs / 100
      *        if bz-bubet not = wf-bet
      *        if bz-bubet < 0 add -0,01 to bz-bubet
      *                   else add 0,01 to bz-bubet
      *        end-if
      *        set auf to true
      *        rewrite bz-satz.

           IF KIST AND WM-G = 0 PERFORM GKSALDO
                                MOVE 1 TO WM-G GO D.
       F.  IF WM-G = 1 MOVE 0 TO WM-G
               CALL "CAUP" USING "08CLOFEN" WH-CREG.
           IF WORE AND WM-M = 1 PERFORM TEXKOR GO D.
           IF WM-M = 1 ADD 1 TO VDU-L
                       CALL "CAUP" USING "1400002280" WH-CREG
                       SUBTRACT 1 FROM VDU-L
                       MOVE 0 TO WM-M.
           IF EINF CALL "FIBANZ" USING "10JOUR" WH-CREG
                   CANCEL "FIBANZ" GO D.
           IF ESC OR ENDE GO Z.
           IF APOS; IF WH-NUM NOT = 0 PERFORM FIRST-JOUR
                                      SET BAB TO TRUE GO F
                    else MOVE 1 TO IX GO D.
           IF XPOS GO A.
           IF BAUF; IF WH-NUM = 0 PERFORM AUFZEIL GO D
                    else PERFORM FIRST-JOUR
                         SET BAB TO TRUE GO F.
           IF AUF; IF IX > 1 SUBTRACT 1 FROM IX
                   ELSE PERFORM AUFZEIL
                   end-if MOVE 2 TO WM-M GO D.
           IF BAB PERFORM ABZEIL; IF FINE GO A
                  ELSE SUBTRACT 1 FROM IX GO D.
           IF AB; IF WT-JKEY(IX + 1) NOT = LOW-VALUE GO C
                  ELSE PERFORM ABZEIL
                       IF FINE GO A ELSE GO D.
           MOVE 0 TO WI WM-GB.
           IF SAPO PERFORM FIRST-JOUR
                   SET BAB TO TRUE GO F.
           IF SEPO PERFORM AUSGAB GO C.
           IF RET AND WH-NUM = 0 AND WH-MCODE = SPACE
               MOVE WM-KTONR TO WH-WERT WH-NUM.
           IF ENTF PERFORM LOEBUZL GO D.
           IF WOLI and WH-MCODE = "*" PERFORM BU-KORR GO D.
           IF WOLI and BZ-OPLFD = 999
               CALL "FIBZUS" USING "25AUSZIF" WH-CREG
               CANCEL "FIBZUS" GO D.
           IF NOT RET GO D.
           IF WT-JKEY(IX) NOT = LOW-VALUE PERFORM LAST-JOUR GO A.
           IF WH-WERT < 0,4 OR WH-NUM < 4
               CALL "FIBANZ" USING "99SUCH" WH-CREG
               CANCEL "FIBANZ".
           IF WH-NUM = 0 GO D.
           PERFORM LIESKTO-NOLOCK.
      *-------------------------------------> Konten wieder freigeben <-
           IF FEHLER GO D.
           MOVE WH-KAT TO WH-AKAT.                  *> Kateg. Erstkonto
           MOVE WH-BN TO WH-ABN.                    *> brutto/netto - KZ
           MOVE WK-MCODE TO WK-AMCODE.
           MOVE WH-NUM TO WK-KTONR BZ-KTONR WM-KTONR.
           ADD WE-USE 1 GIVING WH-KEY.
       G.  READ KONSTANT INVALID INITIALIZE KO-BSATZ
               MOVE WH-KEY TO KO-NUM WRITE KO-SATZ GO G.
           IF ZUGRIF STOP RUN.
           MOVE WE-PER TO WC-DATUM.
           MOVE KO-BUDAT TO WZ-DATUM.
           IF WC-DATUM(1:4) NOT = WZ-DATUM(1:4) MOVE WE-PER TO KO-BUDAT.
           MOVE LOW-VALUE TO KO-JOKEY.
           PERFORM BUCH-ZEIL.
           DISPLAY WK-DATUM with highlight foreground-color 11
                background-color 1 AT 0153.
           IF ESC GO A.
           MOVE BZ-TX TO WV-TX.
           PERFORM BZ-WRITE.
           IF WZ-UST not = 0 MOVE BZ-SATZ TO WH-GGSATZ.
           MOVE 0 TO WR-MOD.
           IF BZ-VM = 6 MOVE WZ-UST TO BZ-BUBET
                             MOVE BZ-SATZ TO WH-GGSATZ
                             MOVE 1 TO WR-MOD.
           IF BZ-VM > 2 MOVE 0 TO BZ-VM.
           IF WK-FDIF not = 0 PERFORM DIF-BUCH.
           EVALUATE TRUE
               WHEN EINZEL PERFORM GEGENBUCH
                     IF NOT ESC ADD 1 TO IX
                         PERFORM BZ-WRITE
               WHEN SAMMEL PERFORM SAMMELBUCH
                           PERFORM AUSGAB
               WHEN AUFTEIL PERFORM AUFBUCH.
           IF WR-MOD = 1 PERFORM TR-UST.
           IF WZ-UST not = 0 PERFORM UMB-UST.
      *--------------------------------> RÅckschreiben Speichersummen <-
           REWRITE KO-SATZ.
           GO A.
       Z.  EXIT.
      *************************************** Korrektur Buchungszeilen *
       BU-KORR SECTION.
       A.  CALL "CAUP" USING "0701010365000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Korrektur BZ " with highlight AT VDU-LP.
           ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "Betrag:                +/-:                Neu:"
               AT VDU-LP.
           COMPUTE WH-WERT WD-SALDO = BZ-BUBET + BZ-MW.
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
           MOVE WH-WERT TO BZ-BUBET BZ-FWBET.
           MOVE BZ-BUBET TO WD-SALDO.
           ADD 251 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-SALDO with highlight AT VDU-LP.
           IF BZ-SH = 0 ADD BZ-BUBET TO WE-SOLL
                   else ADD BZ-BUBET TO WE-HABEN.
       E.  DISPLAY "<esc>= Abbruch, <Einfg>= Ust. Prfg., <ret>= ok! < >"
               AT 2301.
           CALL "CAUP" USING "0023500000" WH-CREG.
           IF ESC GO X.
           IF RET GO W.
           IF not EINF GO E.
           MOVE 0 TO WK-SKTO.
           MOVE BZ-U1 TO WH-U1.
           MOVE WH-U1 TO WH-WERT WK-UST.
           MOVE WH-WERT TO WK-UST.
           MOVE WT-UST(WH-U1) TO WH-UST.
           MOVE 0 TO WU.
           ADD 0200 VDU-ECK GIVING VDU-LP.
           MOVE BZ-BUBET TO WK-BUBET WK-BRUTTO WK-NETTO.
               PERFORM RECH-UST.
           MOVE WK-NETTO TO BZ-BUBET.
           MOVE WK-MWST TO BZ-MW.
       W.  REWRITE BZ-SATZ.
           UNLOCK BUCHZEIL.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ****************************** Ust-u. Erlîsumbuchung bei TR-Zlg. *
       UMB-UST SECTION.
       A.  MOVE WH-GGSATZ TO BZ-SATZ.
           IF WU-MOD = 1 MOVE 0 TO WU-MOD            *> nur Ust-Buchung
               MOVE 0 TO BZ-SH BZ-VM BZ-B1 BZ-B2 BZ-U1 BZ-U2 BZ-MW
               GO G.
      *-------------------------------> autom. Gegenbuchung SR-Erlîse <-
           MOVE WK-TRKTO TO BZ-GGKTO.                      *> Erlîse TR
           MOVE WK-SRKTO TO BZ-KTONR.                      *> Erlîse SR
           MOVE 1 TO BZ-SH BZ-Z BZ-VM.
           MOVE WK-U1 TO BZ-U1.
           MOVE WZ-UST TO WK-MWST BZ-MW.
           MOVE WK-UMBSY TO BZ-SYM.
           MOVE WE-SKZ(BZ-SYM + 1) TO WE-SYMT.
           MOVE WZ-BET TO WK-NETTO BZ-B1.            *> nur TR-Op-Summe
           MOVE WK-SRKTO TO WH-NUM WK-KTONR.
           PERFORM LIESKTO.
           IF FEHLER PERFORM BESETZT
                     PERFORM WEITER GO A.
           MOVE WH-KAT TO WH-AKAT.
           MOVE WH-BN TO WH-ABN.
           MOVE WK-TRKTO TO BZ-GGKTO.
           IF WH-ABN = 0 MOVE WK-BRUTTO TO BZ-BUBET
                    else MOVE WK-NETTO TO BZ-BUBET WK-BUBET
                         ADD WK-NETTO WK-MWST GIVING WK-BRUTTO.
           ADD 1 TO IX.
           PERFORM BZ-WRITE.
           PERFORM ADD-UST.
           EVALUATE WH-AKAT
               WHEN 0 PERFORM SACHSUM
               WHEN 2 PERFORM DEBSUM
               WHEN 3 PERFORM KREDSUM.
      *-------------------------------> autom. Gegenbuchung TR-Erlîse <-
           MOVE WK-TRKTO TO BZ-KTONR WK-KTONR.
           MOVE WK-SRKTO TO BZ-GGKTO.
           MOVE 1 TO BZ-Z.
           MOVE 0 TO BZ-SH BZ-VM BZ-B1 BZ-B2 BZ-U1 BZ-U2 BZ-MW WK-MWST.
           MOVE WK-TRKTO TO WH-NUM.
           PERFORM LIESKTO.
           IF FEHLER PERFORM BESETZT
                     PERFORM WEITER GO A.
           IF WH-ABN = 0 MOVE WK-BRUTTO TO BZ-BUBET
                    else MOVE WK-NETTO TO BZ-BUBET WK-BUBET
                         ADD WK-NETTO WK-MWST GIVING WK-BRUTTO.
           ADD 1 TO IX.
           PERFORM BZ-WRITE.
           EVALUATE WH-AKAT
               WHEN 0 PERFORM SACHSUM
               WHEN 2 PERFORM DEBSUM
               WHEN 3 PERFORM KREDSUM.
      *---------------------------------------> autom. Umbuchung MWST <-
       G.  MOVE WK-NFUST TO BZ-KTONR WK-KTONR.
           MOVE WK-SRKTO TO BZ-GGKTO.
           MOVE WZ-UST TO WK-BRUTTO WK-BUBET BZ-BUBET WK-NETTO.
           MOVE 0 TO WZ-UST WK-U1 BZ-FWBET.
           MOVE WK-NFUST TO WH-NUM.
           PERFORM LIESKTO.
           IF FEHLER PERFORM BESETZT
                     PERFORM WEITER GO A.
           ADD 1 TO IX.
           PERFORM BZ-WRITE.
           EVALUATE WH-AKAT
               WHEN 0 PERFORM SACHSUM
               WHEN 2 PERFORM DEBSUM
               WHEN 3 PERFORM KREDSUM.
       Z.  EXIT.
      ******************************************************************
       TR-UST SECTION.
       A.  MOVE 0 TO BZ-VM BZ-B1 BZ-B2 BZ-U1 BZ-U2 BZ-MW.
           MOVE WK-NFUST TO BZ-KTONR WK-KTONR.
           MOVE WZ-UST TO WK-BRUTTO WK-BUBET BZ-BUBET WK-NETTO.
           MOVE 0 TO WZ-UST WK-U1 BZ-FWBET.
           MOVE WK-NFUST TO WH-NUM.
           PERFORM LIESKTO.
           IF FEHLER PERFORM BESETZT
                     PERFORM WEITER GO A.
           IF WH-AKAT = 2 MOVE 1 TO BZ-SH.
           IF WH-AKAT = 3 MOVE 0 TO BZ-SH.
      *    MOVE WH-KAT TO WH-AKAT.
           ADD 1 TO IX.
           PERFORM BZ-WRITE.
           EVALUATE WH-AKAT
               WHEN 0 PERFORM SACHSUM
               WHEN 2 PERFORM DEBSUM
               WHEN 3 PERFORM KREDSUM.
       Z.  EXIT.
      ******************************************************************
       AUSGAB SECTION.
       A.  MOVE 1 TO IX.
           MOVE WT-JKEY(IX) TO BZ-JKEY.
           MOVE LOW-VALUE TO WT-JOTAB.
           START BUCHZEIL KEY NOT < BZ-JKEY INVALID
               SET FINE TO TRUE GO X.
           CALL "CAUP" USING "16CLRFEN" WH-CREG.
           SET BAB TO TRUE.
           MOVE 0 TO WI WR.
           PERFORM NEXTAB WITH TEST AFTER UNTIL WI = 8.
           IF WI NOT = 8 MOVE 9 TO WI.
       H.  IF IX = 0 GO X.
           IF WT-JKEY(IX) = LOW-VALUE ADD -1 TO IX GO H.
       X.  IF FINE PERFORM NODATA.
       Z.  EXIT.
      ************************************************** Buchungszeile *
       BUCH-ZEIL SECTION.
       A.  CALL "CAUP" USING "0703012080020" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " K-Nr.:         " AT VDU-LP.
           ADD 8 TO VDU-LP.
           DISPLAY WD-KTO with highlight AT VDU-LP.
           ADD 8 TO VDU-LP.
           DISPLAY ALL " " with SIZE 52 reverse-video AT VDU-LP.
           ADD 1 TO VDU-LP.
           INSPECT WT-TX REPLACING ALL "#" BY SPACE.
           DISPLAY WT-TX with SIZE 50 AT VDU-LP.
           ADD 301 VDU-ECK GIVING VDU-LP.
           DISPLAY " Bu-Datum Sy Bel-Nr.- Datum  Geg-Kto.      Soll
      -        "     Haben Kz Mwst/Skt.-Bet." with background-colour 2
                          highlight AT VDU-LP.
           MOVE BZ-SATZ TO WH-BZSATZ.
           PERFORM LAST-BUCH.
           ADD 1 TO IY.
           PERFORM SALDO.
           MOVE WH-BZSATZ TO BZ-SATZ.
           IF EINZEL PERFORM AUTOBUCH
                     PERFORM UPSALDO  GO T. *> autom. Einzelgegenbuchung
       B.  PERFORM VARYING IY FROM 10 BY -1 UNTIL IY = 0
               OR WT-KEY(IY) NOT = LOW-VALUE
               END-PERFORM.
           IF NOT XPOS PERFORM BS-BZEIL.
      *-----------------------------------------------> Buchungsdatum <-
       C.  DISPLAY "<ret>= Buchungsdatum, <esc>= Ende, <Einf>= Saldo"
               AT 2301.
           IF WH-AKAT > 1 DISPLAY ", <#>= OP's, < >= Kto.éndg." AT 0000.
           DISPLAY "< /  - Bild- / >= anzeigen, mit Datum = anzeigen ab"
              AT 2401.
           PERFORM BUZEIG.
           IF BZ-OPLFD = 999 or 255
                DISPLAY "<>= Zahlungszuordnung" AT 2501.
           MOVE WK-KTONR TO BZ-KTONR.
           IF KO-BUDAT = 0 MOVE WE-PER TO WC-DATUM
                      else MOVE KO-BUDAT TO WC-DATUM.
           PERFORM DATDREH.
           COMPUTE VDU-LP = IY * 200 + 202 + VDU-ECK.
           IF WT-KEY(IY) = LOW-VALUE AND WC-DATUM NOT = 0
                DISPLAY VDU-DATUM AT VDU-LP.
           COMPUTE VDU-LP = IY * 200 + 200.
           MOVE WC-DATUM TO WZ-DATUM WH-WERT.
           IF WD-KZ = 2 DISPLAY "Warenumsatz: " AT 2554
               MOVE DE-UMSATZ TO WD-SALDO
               DISPLAY WD-SALDO with highlight foreground-color 6
                   AT 2566.
           CALL "CAUP" USING "1100026006" WH-CREG.
      *    CALL "CAUP" USING "1325012580" WH-CREG.

      *    if kist delete buchzeil
      *            end-delete
      *            move 19990217 to bz-budat bz-beldat
      *            move 0 to bz-lfd
      *            write bz-satz invalid add 1 to bz-lfd
      *                          write bz-satz end-write
      *            end-write go c.

           if wm-datum = 20040727; if bz-budat > 20040430 and bz-u1 = 5
                   move 3 to bz-u1
                   rewrite bz-satz invalid stop run.

           IF KIST AND WH-AKAT > 1
               CALL "FIBOPES" USING "10OPANZ" WH-CREG
               CANCEL "FIBOPES" GO C.
           IF WOLI; IF BZ-OPLFD = 999 or 255
               CALL "FIBZUS" USING "25AUSZIF" WH-CREG
               CANCEL "FIBZUS" GO C.

           if tabl and wm-datum = 20020126
               move bz-fwbet to bz-bubet
               move 100 to bz-kurs
               move "Eur" to bz-fsym
               CALL "CAUP" USING "0023016208" WH-CREG
               move wh-wert to bz-mw
               compute bz-b1 = bz-bubet - bz-mw
               rewrite bz-satz.

           if sapo and wm-datum = 20020126 move 7,2672835 to bz-kurs
               move "Ats" to bz-fsym
               move 0 to bz-mw
               move 0 to bz-b1
               compute bz-bubet rounded = bz-bubet * bz-kurs / 100
               rewrite bz-satz.


           IF ESC GO X.
           IF APOS MOVE 1 TO IY GO C.
           IF XPOS GO B.
           IF SENT PERFORM LOEBUCH.
           IF EINF; IF BZ-BUDAT = 0 MOVE 99999999 TO BZ-BUDAT BZ-LFD
                    end-if
                   CALL "FIBANZ" USING "95SALDO" WH-CREG
                   CANCEL "FIBANZ".
           IF SF3 CALL "FIBANZ" USING "90KTOSUM" WH-CREG
                  CANCEL "FIBANZ"
                  SET ESC TO TRUE GO X.
           IF BAUF PERFORM AUFBUZEI GO C.
           IF AUF; IF IY > 1 SUBTRACT 1 FROM IY GO C
                   ELSE PERFORM AUFBUZEI GO C.
           IF BAB PERFORM ABBUZEI; IF FINE GO B
                  ELSE SUBTRACT 1 FROM IY GO C.
           IF AB; IF WT-KEY(IY + 1) NOT = LOW-VALUE
                     PERFORM BS-BZEIL GO C
                  ELSE PERFORM ABBUZEI
                       IF FINE GO B ELSE GO C.
           IF WORE; EVALUATE WH-AKAT
               WHEN 3 CALL "FIBKRED" USING "42AEN" WH-CREG
                      CANCEL "FIBKRED"
               WHEN 2 CALL "FIBDEBIT" USING "32AEN" WH-CREG
                      CANCEL "FIBDEBIT"
                    end-evaluate GO C.
           MOVE 0 TO WI WZ-UST WK-U1.
           IF NOT RET AND NOT SEIN GO C.
           IF SEIN CALL "FIBANZ" USING "80KORR" WH-CREG
                   CANCEL "FIBANZ"
                   SET ESC TO TRUE GO X.
           IF WH-KAT = 1 SET ESC TO TRUE GO X.
           IF WT-KEY(IY) NOT = LOW-VALUE PERFORM LAST-BUCH GO B.
           IF WZ-DATUM = 0 GO C.
           MOVE WX-DATUM TO BZ-BUDAT.                       *> fÅr 2000
           MOVE WE-PER TO WC-DATUM.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           IF WZ-JAHR NOT = WC-TAG OR WZ-MONAT NOT = WC-MONAT
                IF IX = 1 CALL "FIBANZ" USING "51PERWE" WH-CREG
                          IF not ESC SET NEUPER TO TRUE
                               PERFORM P-PRUEF
                               IF ESC GO C
                               else MOVE BZ-BUDAT TO WE-PER
                                    end-if
                          else CONTINUE
                     else CALL "FIBANZ" USING "50PERWE" WH-CREG
                end-if
                CANCEL "FIBANZ"
                IF ESC GO C.
           MOVE BZ-BUDAT TO KO-BUDAT.
      *------------------------------------------------------> Symbol <-
       E.  DISPLAY "<esc>= Abruch, <ret>= Symbol, <#>= Tabelle anzeigen"
               AT 2301.
           MOVE WE-SY TO WH-WERT.
           COMPUTE VDU-LP = IY * 200 + 200.
           CALL "CAUP" USING "1000112002" WH-CREG.
           IF KIST CALL "FIBSTAM" USING "20SYMBOL" WH-CREG
                   CANCEL "FIBSTAM"
                   IF WH-WERT > 29 GO E.
           IF WOLI GO C.
           IF ESC GO X.
           IF NOT RET GO E.
           MOVE WH-WERT TO BZ-SYM.
           MOVE BZ-SYM TO WE-SY.
           IF BZ-SYM > 29 GO E.
           DISPLAY WE-SYM(BZ-SYM + 1) with highlight AT VDU-LP.
           MOVE WE-SKZ(BZ-SYM + 1) TO WE-SYMT.
           IF WE-SYM(BZ-SYM + 1) = SPACE GO E.
      *-------------------------------------------------> Belegnummer <-
       G.  DISPLAY "<esc>= Abbruch, < >= alter Nr. + 1, <ret>= Beleg-Nr.
      -        "" AT 2301.
           IF WE-BELNR NOT = 0 MOVE WE-BELNR TO WD-NUM WH-WERT
               COMPUTE VDU-LP = IY * 200 + 213 + VDU-ECK
               DISPLAY WD-NUM AT VDU-LP.
           COMPUTE VDU-LP = IY * 200 + 213.
           CALL "CAUP" USING "1000007007" WH-CREG.
           IF WOLI GO E.
           IF ESC GO X.
           IF AB ADD WE-BELNR 1 GIVING WH-WERT.
           IF NOT RET AND NOT AB GO G.
           MOVE WH-WERT TO WD-NUM BZ-BELNR.
           MOVE BZ-BELNR TO WE-BELNR.
           DISPLAY WD-NUM AT VDU-LP.
      *--------------------------------------------------> Belegdatum <-
       I.  DISPLAY "<esc>= Abbruch, <ret>= Belegdatum" AT 2301.
           MOVE BZ-BUDAT TO WC-DATUM.
           PERFORM DATDREH.
           COMPUTE VDU-LP = IY * 200 + 221 + VDU-ECK.
           DISPLAY VDU-DATUM AT VDU-LP.
           COMPUTE VDU-LP = IY * 200 + 221.
           MOVE BZ-BUDAT TO WZ-DATUM WH-WERT.
           CALL "CAUP" USING "1100216006" WH-CREG.
           IF WOLI GO G.
           IF ESC GO X.
           IF NOT RET GO I.
           IF WZ-DATUM = 0 GO I.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           MOVE WX-DATUM TO BZ-BELDAT.
           EVALUATE WH-AKAT
               WHEN 0
               WHEN 1 MOVE SA-SATZ TO WH-KTOSATZ
               WHEN 2 MOVE DE-SATZ TO WH-KTOSATZ
               WHEN 3 MOVE KR-SATZ TO WH-KTOSATZ.
      *---------------------------------------------------> Gegekonto <-
       J.  DISPLAY "<ret>= Gg.-Konto-Nr., <esc>= zurÅck, <Ende>= Ende"
               AT 2301.
           DISPLAY "Gegenbuchung: <ret>= einzel, < >= Sammel, <#>= ohne,
      -        " < >= Aufteilung" AT 2401.
           MOVE 0 TO WH-WERT.
           IF WE-GGKTO NOT = 0 MOVE WE-GGKTO TO WH-WERT
               DIVIDE 10 INTO WE-GGKTO GIVING WD-KTO
               COMPUTE VDU-LP = IY * 200 + 230 + VDU-ECK
               DISPLAY WD-KTO with highlight AT VDU-LP
               ADD 7 TO VDU-P
               MOVE WE-GB TO WM-GB
               EVALUATE WE-GB
                   WHEN 0 DISPLAY " " AT VDU-LP
                   WHEN 1 DISPLAY "e" AT VDU-LP
                   WHEN 2 DISPLAY "s" AT VDU-LP.
           COMPUTE VDU-LP = IY * 200 + 200.
           CALL "CAUP" USING "1000305108" WH-CREG.
           IF HELP CALL "CAUP" USING "190079008001" WH-CREG GO J.
           IF WOLI GO I.
           IF ESC GO X.
           IF EINF AND WH-NUM = 99999 AND BZ-SYM = 19
               MOVE 999999 TO WH-NUM
               MOVE 0 TO WM-GB WH-GKAT
               DISPLAY "       " AT VDU-LP
               SET RET TO TRUE GO L.
           IF RET AND WH-NUM = 0 AND WH-MCODE = SPACE
                MOVE WH-WERT TO WH-NUM
                IF WH-NUM = 0 SET KIST TO TRUE
                else GO K.
           IF RET MOVE 1 TO WM-GB.
           IF AB MOVE 2 TO WM-GB
               IF WE-BELNR = 0 DISPLAY "Beleg-Nr. mu· sein!" AT 2401
                   PERFORM WEITER GO G
               else SET RET TO TRUE.
           IF KIST MOVE 0 TO WM-GB WH-GKAT WH-NUM
               DISPLAY "       " AT VDU-LP
               SET RET TO TRUE GO L.
           IF AUF MOVE 4 TO WM-GB
               MOVE 0 TO WH-GKAT WH-NUM
               DISPLAY "       " AT VDU-LP
               SET RET TO TRUE GO L.
           IF NOT RET GO J.
           IF WH-WERT < 0,4 OR WH-NUM < 4
               CALL "FIBANZ" USING "99SUCH" WH-CREG
               CANCEL "FIBANZ".
       K.  IF WH-NUM = 0 GO J.
           IF WH-NUM = WM-KTONR DISPLAY "Buchung sinnlos!" AT 2401
               PERFORM WEITER
               GO J.
           PERFORM LIESKTO-NOLOCK.
           IF FEHLER PERFORM ERSTKTO GO J.
           DISPLAY WD-KTO with highlight AT VDU-LP.
       L.  MOVE WH-KAT TO WH-GKAT.
           MOVE WH-NUM TO BZ-GGKTO.
           MOVE BZ-GGKTO TO WE-GGKTO.
           ADD 7 TO VDU-P.
           MOVE WM-GB TO BZ-Z.
           MOVE VDU-L TO VDU-AB.
           MOVE WM-GB TO WE-GB.
           EVALUATE WM-GB
               WHEN 0 DISPLAY " " AT VDU-LP GO LX
               WHEN 1 DISPLAY "e" AT VDU-LP
               WHEN 2 DISPLAY "s" AT VDU-LP
               WHEN 4 ADD -6 TO VDU-LP
                      DISPLAY "*auft* " AT VDU-LP GO LX.
           PERFORM ANZ-GGKTO.
      *----------------------------------> Erstkonto wiederherstellen <-
           PERFORM ERSTKTO.
       LX. EVALUATE WE-OP also WH-AKAT
               WHEN 1 also 2 MOVE 0 TO WE-SH
               WHEN 2 also 2 MOVE 1 TO WE-SH
               WHEN 1 also 3 MOVE 1 TO WE-SH
               WHEN 2 also 3 MOVE 0 TO WE-SH.
           IF WE-SH = 1 GO N.
      *-----------------------------------------> Buchungsbetrag SOLL <-
       M.  DISPLAY "<esc>= Abbruch, <ret>= Soll, < >= z. Haben" AT 2301.
           DISPLAY "HauswÑhrung = " AT 2401.
           PERFORM CURDIS.
           MOVE 0 TO WE-SH.
           DISPLAY ", <#>= FremdwÑhrungseingabe," AT 0000.
           DISPLAY " < >= Ats-Buchung" with highlight at 0000.
           MOVE 0 TO WH-WERT WH-NUM WF-BET WK-FDIF.
           MOVE VDU-AB TO VDU-L.
           CALL "CAUP" USING "0000388212" WH-CREG.
           IF AB CALL "CAUP" USING "1300380079" WH-CREG GO N.
           IF WM-G = 1 MOVE 0 TO WM-G
               CALL "CAUP" USING "08CLOFEN" WH-CREG.
           IF WOLI CALL "CAUP" USING "1300380079" WH-CREG GO J.
           IF ESC GO X.
           MOVE WF-KURS(1) TO BZ-KURS.
           MOVE WF-SYM(1) TO BZ-FSYM.

           IF WORE MOVE WF-SYM(2) TO BZ-FSYM          *> Eingabe in Ats
               MOVE WF-KURS(2) TO BZ-KURS
               MOVE WH-WERT TO WF-BET
               PERFORM FREMDWHG
               IF WH-WERT = 0 GO M.
      *        COMPUTE WH-ERG rounded = WF-BET * WF-KURS(2) / 100
      *        MOVE WH-ERG TO WH-WERT
      *        SET RET TO TRUE.

           IF KIST MOVE 0 TO BZ-SH
               PERFORM FREMDWHG
               IF WH-WERT = 0 GO M.
           IF NOT RET GO M.
           COMPUTE VDU-LP = VDU-AB * 100 + 38.
           MOVE VDU-LP TO VDU-REL.
           IF WF-BET = 0 MOVE WH-WERT TO WF-BET BZ-FWBET
                    else MOVE WF-BET TO BZ-FWBET.
           MOVE WH-WERT TO WD-SALDO BZ-BUBET WK-BUBET.
           IF WH-WERT NOT = 0 DISPLAY WD-SALDO with highlight AT VDU-LP
               MOVE 0 TO BZ-SH GO P.
           CALL "CAUP" USING "1300380079" WH-CREG.
           MOVE 0 TO BZ-SH.                   *> wegen Skontoausbuchung
           GO P.
      *----------------------------------------> Buchungsbetrag HABEN <-
       N.  DISPLAY "<esc>= Abbruch, <ret>= Haben, < >= z. Soll" AT 2301.
           DISPLAY "HauswÑhrung = " AT 2401.
           PERFORM CURDIS.
           MOVE 1 TO WE-SH.
           DISPLAY ", <#>= FremdwÑhrungseingabe," AT 0000.
           DISPLAY " < >= Ats-Buchung" with highlight at 0000.
           MOVE 0 TO WH-WERT WH-NUM WF-BET WK-FDIF.
           MOVE VDU-AB TO VDU-L.
           CALL "CAUP" USING "0000508212" WH-CREG.
           IF WM-G = 1 MOVE 0 TO WM-G
               CALL "CAUP" USING "08CLOFEN" WH-CREG.
           IF AB OR WOLI CALL "CAUP" USING "1300380079" WH-CREG
               IF WE-SH = 2 GO M else GO J.
           IF ESC GO X.
           MOVE WF-KURS(1) TO BZ-KURS.
           MOVE WF-SYM(1) TO BZ-FSYM.

           IF WORE MOVE WF-SYM(2) TO BZ-FSYM          *> Eingabe in Ats
               MOVE WF-KURS(2) TO BZ-KURS
               MOVE WH-WERT TO WF-BET
               PERFORM FREMDWHG.
      *        COMPUTE WH-ERG rounded = WF-BET * WF-KURS(2) / 100
      *        MOVE WH-ERG TO WH-WERT
      *        SET RET TO TRUE.

           IF KIST MOVE 1 TO BZ-SH
               PERFORM FREMDWHG.
      *        IF WH-WERT = 0 GO N.
           IF NOT RET GO N.
           COMPUTE VDU-LP = VDU-AB * 100 + 50.
           MOVE VDU-LP TO VDU-REL.
           IF WF-BET = 0 MOVE WH-WERT TO WF-BET BZ-FWBET
                    else MOVE WF-BET TO BZ-FWBET.
           MOVE WH-WERT TO WD-SALDO BZ-BUBET WK-BUBET.
           IF WH-WERT NOT = 0 DISPLAY WD-SALDO with highlight AT VDU-LP
               MOVE 1 TO BZ-SH GO P.
           CALL "CAUP" USING "1300380079" WH-CREG.
           MOVE 1 TO BZ-SH.                   *> wegen Skontoausbuchung
           GO P.
      *--------------------------------------------> Umsatzsteuer-Kz. <-
       P.  DISPLAY "<esc>= Abbruch, <ret>= Ust-Kz., <#>= Ust-Tabelle"
               AT 2301.
           IF WE-OP > 1 DISPLAY "<Bild- >= OP-Ausgleich" AT 2401
           else DISPLAY "< >= UST v. 100, < >= man. UST" AT 2401.
           DISPLAY "Ustfrei lt. Symbol" with highlight AT 2501.
           COMPUTE VDU-LP = IY * 200 + 264 + VDU-ECK.
           MOVE WH-U1 TO WH-WERT WK-UST.
           MOVE 0 TO WK-SKTO BZ-U1 BZ-VM.
      *------------------------------------> fÅr automatische Anzeige <-
           IF WK-UST not = 0 MOVE WT-UST(WK-UST) TO WH-UST WD-PROZ
               DISPLAY WD-PROZ AT VDU-LP
               EVALUATE WH-VM
                   WHEN 1 DISPLAY "m" AT 0000
                   WHEN 2 IF WK-UST > 4 DISPLAY "i" AT 0000
                                   else DISPLAY "v" AT 0000
                   WHEN 10 DISPLAY "f" AT 0000.
           COMPUTE VDU-LP = IY * 200 + 200.
           CALL "CAUP" USING "1000641002" WH-CREG.
           IF KIST CALL "FIBSTAM" USING "33UST" WH-CREG
                   CANCEL "FIBSTAM" GO P.
           IF WOLI CALL "CAUP" USING "1300380079" WH-CREG
                   PERFORM LOE-FWZEIL
                   IF WE-SH = 0 GO M
                   else GO N.
           IF BAB AND WE-OP > 1 MOVE VDU-LP TO VDU-REL
                  MOVE WK-BUBET TO WK-BRUTTO WK-NETTO
                  CALL "FIBOPES" USING "20ANZ" WH-CREG
                  CANCEL "FIBOPES"
                  MOVE VDU-REL TO VDU-LP
                  IF ESC GO P else
                  IF WH-WERT NOT = 0 MOVE WH-WERT TO WD-SALDO WK-SKTO
                      DISPLAY "Sk" AT VDU-LP
                      ADD 2 TO VDU-LP
                      DISPLAY WD-SALDO with highlight AT VDU-LP
                   end-if GO R.
           IF ESC GO X.
           IF WH-WERT > 4 or UST-BAR or USTFREI
               SET AUF TO TRUE.
           MOVE 0 TO WU.
           IF AUF MOVE 1 TO WU SET RET TO TRUE.           *> UST von 100
           IF AB  MOVE 2 TO WU SET RET TO TRUE.           *> man. UST
           IF NOT RET OR WH-WERT > 6 GO P.
           MOVE WH-WERT TO WK-UST BZ-U1.
           IF WE-UST = 0 AND BZ-U1 NOT = 0
               DISPLAY "Symbol ohne UST angewÑhlt," AT 2401
               PERFORM WEITER GO P.
           if we-ust not = 0 and bz-u1 = 0
               DISPLAY "Ust ? " with highlight BLINK BEEP AT 2401
               PERFORM WEITER.
           MOVE WK-BUBET TO WK-BRUTTO WK-NETTO.
           IF WK-UST NOT = 0 MOVE WT-UST(WK-UST) TO WH-UST WD-PROZ
               DISPLAY WD-PROZ AT VDU-LP
               MOVE WE-UST TO BZ-VM
               IF WE-UST = 3 MOVE 6 TO BZ-VM        *> siehe Teilrechng.
               end-if
               IF UST-BAR MOVE 9 TO BZ-VM          *> Bauleister AR
               end-if
               IF USTFREI MOVE 10 TO BZ-VM         *> Bauleister ER
               end-if
               EVALUATE BZ-VM
                   WHEN 1 DISPLAY "m" AT 0000
                   WHEN 2 IF WK-UST > 4 DISPLAY "i" AT 0000
                                        MOVE 5 TO BZ-VM
                                   else DISPLAY "v" AT 0000
                                   end-if
                   WHEN 6 DISPLAY "n" AT 0000
                   WHEN 9 DISPLAY "f" AT 0000
               end-evaluate
               PERFORM RECH-UST
               PERFORM USTBERECHT
               IF ESC GO P
               end-if
               MOVE WK-NETTO TO BZ-B1
               MOVE WK-MWST TO BZ-MW
           else MOVE 0 TO BZ-U1 BZ-B1 BZ-U2 BZ-B2 BZ-MW WK-MWST.
           IF WK-MWST NOT = 0 MOVE WK-MWST TO WD-UST
               DISPLAY WD-UST with highlight AT VDU-LP
           else CALL "CAUP" USING "1300640079" WH-CREG.
       R.  PERFORM UPSALDO.
           IF WM-GB = 4 CALL "FIBZUS" USING "50AUFTEIL" WH-CREG
              CANCEL "FIBZUS"
              EVALUATE TRUE
                  WHEN ESC GO X
                  WHEN WOLI CALL "CAUP" USING "1300380079" WH-CREG GO M.
           PERFORM LOE-FWZEIL.
       T.  DISPLAY "<esc>= Abbruch, <#>= lîschen, < >= Text duplizieren"
               AT 2301.
           MOVE BZ-TX TO WT-TX.
           COMPUTE VDU-LP = IY * 200 + 302 + VDU-ECK.
           IF BZ-TX = SPACE MOVE WV-TX TO BZ-TX.
           DISPLAY BZ-TX AT VDU-LP.
           COMPUTE VDU-LP = IY * 200 + 302.
           CALL "CAUP" USING "1200020140" WH-CREG.
           IF ESC GO X.
           IF AB MOVE WV-TX TO BZ-TX GO T.
           IF AUF OR WOLI MOVE WK-SOLL TO WE-SOLL
                   MOVE WK-HABEN TO WE-HABEN
                   PERFORM SALDO
                   IF WE-OP > 0 ADD -1 TO VDU-L
                       MOVE LOW-VALUE TO WE-RETAB
                       CALL "CAUP" USING "1300640079" WH-CREG
                   end-if GO P.
           MOVE WT-TX TO BZ-TX.
           CALL "CAUP" USING "1300020040" WH-CREG.
           IF KIST MOVE SPACE TO BZ-TX GO T.
           IF NOT RET GO T.
           DISPLAY BZ-TX AT VDU-LP.
       W.  DISPLAY "<ret>= Buchung ok!, <esc>= Abbruch" AT 2301.
           CALL "CAUP" USING "0023360000" WH-CREG.
           IF ESC GO X.
           IF WOLI GO T.
           IF NOT RET GO W.
           MOVE LOW-VALUE TO AZ-SATZ.
           IF WE-OP = 1 PERFORM MACHOP.
           IF WE-OP > 1 PERFORM AUS-OP.
           IF ESC GO X.
           EVALUATE WH-AKAT
               WHEN 0 PERFORM SACHSUM
               WHEN 2 PERFORM DEBSUM
               WHEN 3 PERFORM KREDSUM.
           MOVE 0 TO WU-MOD.
           IF WE-DIV = 8 MOVE 1 TO WU-MOD
                         MOVE WK-MWST TO WZ-UST.
           IF BZ-VM = 6 MOVE WK-MWST TO WZ-UST.
           PERFORM ADD-UST.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ************************************* FremdwÑhrungszeile lîschen *
       LOE-FWZEIL SECTION.
       A.  COMPUTE VDU-LP = IY * 200 + 302 + VDU-ECK.
           CALL "CAUP" USING "1300000078" WH-CREG.
       Z.  EXIT.
      ******************************** Eingabe FremwÑhrungskennzeichen *
      *  HauswÑhrung ATS: 0 = Ats    *> Buchung in Ats/Op`s im Fremdwhg.
      *                   1 = Eur
      *              bis 19 = FremdwÑhrung x Kurs lt. Tabelle
      *  HauswÑhrung EUR: 0 = Eur    *> Buchung in Eur/Op`s im Fremdwhg.
      *                   1 = Ats
      *              bis 19 = FremdwÑhrung : Kurs lt. Tabelle
      ******************************************************************
       FREMDWHG SECTION.
       A.  COMPUTE VDU-LP = IY * 200 + 503.
           DISPLAY " FremdwÑhrungsbuchung - WÑhrung: " with
                   background-color 4 highlight AT VDU-LP.
       C.  MOVE 36 TO VDU-P.
           DISPLAY " "  with background-color 4 SIZE 40 AT VDU-LP.
           IF WH-WERT not = 0 GO G.
      *-----------------------------------> ob ATS oder EUR lt. Stamm <-
       E.  DISPLAY "<ret>= WÑhrungskz., <#>= WÑhrungstabelle, <esc>= Abb
      -        "ruch" AT 2301.
           COMPUTE VDU-LP = IY * 200 + 536.
           CALL "CAUP" USING "0000002002" WH-CREG.
           IF ESC MOVE 0 TO BZ-KURS WH-WERT GO X.
           IF KIST CALL "FIBSTAM" USING "25FRWSYM" WH-CREG
                   CANCEL "FIBSTAM".
           IF not RET GO E.
       G.  ADD 1 WH-WERT GIVING IW.
           IF WORE MOVE 2 TO IW WD-KZ              *> Ats - Kurzversion
                   GO H.
           IF WH-WERT > 19 MOVE 0 TO WH-WERT GO C.
           MOVE WH-WERT TO WD-KZ.
      *-----------------------------------------------> 20= man. Kurs <-
           IF IW = 20 PERFORM MANKURS
                      IF ESC GO E.
           IF WF-KURS(IW) = 0 MOVE 0 TO WH-WERT GO C.
           MOVE WF-SYM(IW) TO BZ-FSYM.
       H.  DISPLAY WD-KZ AT VDU-LP "/" BZ-FSYM AT 0000.
           MOVE WF-KURS(IW) TO BZ-KURS WD-KURS.
           MOVE 42 TO VDU-P.
           DISPLAY WD-KURS with highlight foreground-color 6
                background-color 4 AT VDU-LP.
           ADD 12 TO VDU-LP.
           IF BZ-SH = 0 DISPLAY "Soll: " AT VDU-LP
                   else DISPLAY "Haben:" AT VDU-LP.
           IF WORE SET RET TO TRUE GO J.
       I.  DISPLAY "<esc>= Abbruch, <ret>= Betrag, < />= zurÅck"
               AT 2301.
           CALL "CAUP" USING "0000618212" WH-CREG.
           IF ESC GO X.
           IF WOLI MOVE 0 TO WH-WERT GO C.
           IF not RET and not AB GO I.
       J.  MOVE WH-WERT TO WD-SALDO WF-BET.
           DISPLAY WD-SALDO AT VDU-LP.
           COMPUTE WH-ERG rounded = WF-BET * WF-KURS(IW) / 100.
           MOVE WH-ERG TO WH-WERT.
           IF not AB GO Z.
       Q.  DISPLAY "<esc>= Abbruch, <ret>= Betrag, < />= zurÅck"
               AT 2301.
           DISPLAY "man. FremdwÑhrungsbetrag" with highlight AT 2401.
           COMPUTE VDU-LP = VDU-AB * 100 + 38 + (BZ-SH * 12).
           CALL "CAUP" USING "0000388212" WH-CREG.
           IF ESC GO X.
           IF NOT RET GO Q.
           GO Z.
       X.  CALL "CAUP" USING "1300020079" WH-CREG.
       Z.  EXIT.
      ******************************************************************
       MANKURS SECTION.
       A.  CALL "CAUP" USING "0719300330000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " man. Kurs " with highlight AT VDU-LP.
           ADD 203 VDU-ECK GIVING VDU-LP.

       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      *************> Erstkonto: Kat. und BN-Kz. Ggkto: Kat. und BN-Kz <*
       ustberecht section.
       a.  if wk-mwst = 0 go z.
           evaluate wh-akat also wh-abn also wh-kat also wh-bn
               when 0 also 0 also 2 thru 3 also 0
               when 0 also 0 also 0 also 0
               when 2 thru 3 also 0 also 0 also 0
                   display "keine UST mîglich" with BLINK AT 2401
                   perform weiter
                   set esc to true.
       z.  exit.
      ****************************** Kontensalden updaten und anzeigen *
       UPSALDO SECTION.
       A.  MOVE WE-SOLL TO WK-SOLL.
           MOVE WE-HABEN TO WK-HABEN.
           EVALUATE WH-AKAT ALSO BZ-SH
               WHEN 0 ALSO 0 ADD WK-NETTO TO WE-SOLL
               WHEN 0 ALSO 1 ADD WK-NETTO TO WE-HABEN
               WHEN 2 ALSO 0 ADD BZ-BUBET TO WE-SOLL
               WHEN 2 ALSO 1 ADD BZ-BUBET TO WE-HABEN
                             ADD WK-SKTO  TO WE-HABEN
               WHEN 3 ALSO 0 ADD BZ-BUBET TO WE-SOLL
                             ADD WK-SKTO  TO WE-SOLL
               WHEN 3 ALSO 1 ADD BZ-BUBET TO WE-HABEN.
           PERFORM SALDO.
       Z.  EXIT.
      ******************************************** Gegenkonto anzeigen *
       ANZ-GGKTO SECTION.
       A.  MOVE 1 TO WM-G.
           IF VDU-L > 18 CALL "CAUP" USING "0711050572000" WH-CREG
                    else CALL "CAUP" USING "0720050372000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Gegenkonto-Nr.:         " AT VDU-LP.
           ADD 15 TO VDU-LP.
           DISPLAY WD-KTO with highlight AT VDU-LP.
           ADD 8 TO VDU-LP.
           DISPLAY ALL " " with SIZE 32 reverse-video AT VDU-LP.
           ADD 1 TO VDU-LP.
           DISPLAY WT-TX with SIZE 30 AT VDU-LP.
           ADD 212 VDU-ECK GIVING VDU-LP.
           DISPLAY "S:                H:" AT VDU-LP.
           MOVE WE-SOLL TO WD-SALDO.
           ADD 3 TO VDU-P.
           DISPLAY WD-SALDO with highlight AT VDU-LP.
           MOVE WE-HABEN TO WD-SALDO.
           ADD 18 TO VDU-P.
           DISPLAY WD-SALDO with highlight AT VDU-LP " Saldo: ".
           SUBTRACT WE-HABEN FROM WE-SOLL GIVING WE-SALDO.
           MOVE WE-SALDO TO WD-SALDO.
           DISPLAY WD-SALDO with highlight AT 0000.
       Z.  EXIT.
      ************************************* Erstkonto wiederherstellen *
       ERSTKTO SECTION.
       A.  EVALUATE WH-AKAT
               WHEN 0
               WHEN 1 MOVE WH-KTOSATZ TO SA-SATZ
                      MOVE SA-SOLL TO WE-SOLL
                      MOVE SA-HABEN TO WE-HABEN
               WHEN 2 MOVE WH-KTOSATZ TO DE-SATZ
                      MOVE DE-SOLL TO WE-SOLL
                      MOVE DE-HABEN TO WE-HABEN
               WHEN 3 MOVE WH-KTOSATZ TO KR-SATZ
                      MOVE KR-SOLL TO WE-SOLL
                      MOVE KR-HABEN TO WE-HABEN.
       Z.  EXIT.
      ******************************************** Ausgabe Kontensaldo *
       SALDO SECTION.
       A.  DISPLAY "S:                H:" AT 0422.
           MOVE WE-SOLL TO WD-SALDO.
           DISPLAY WD-SALDO with highlight AT 0425.
           MOVE WE-HABEN TO WD-SALDO.
           DISPLAY WD-SALDO with highlight AT 0443 " Saldo: ".
           SUBTRACT WE-HABEN FROM WE-SOLL GIVING WE-SALDO.
           MOVE WE-SALDO TO WD-SALDO.
           DISPLAY WD-SALDO with highlight AT 0000.
           IF WH-AKAT < 2 AND SA-EBIL NOT = 0
               DISPLAY "Eb: " AT 0402
               MOVE SA-EBIL TO WD-SALDO
               DISPLAY WD-SALDO with highlight AT 0000.
       Z.  EXIT.
      **************************************** Buchungszeile speichern *
       BZ-WRITE SECTION.
       A.  IF IX > 1 MOVE WT-JKEY(IX - 1) TO BZ-JKEY.
       B.  MOVE WE-USE TO BZ-USE.
           ADD 1 TO BZ-SEITE.
           PERFORM VARYING WS-BUCH FROM 115 BY -1 UNTIL WS-BUCH = 75
               OR BZ-SATZ(WS-BUCH:1) NOT = SPACE CONTINUE.
           MOVE BZ-SATZ TO WH-BZSATZ.
           MOVE BZ-KEY TO WH-BKEY.
           MOVE 900 TO BZ-LFD.
           START BUCHZEIL KEY < BZ-KEY INVALID MOVE 1 TO WH-LFD GO E.
       C.  READ BUCHZEIL PREVIOUS WITH NO LOCK AT END
               MOVE 1 TO WH-LFD GO E.
           IF ZUGRIF PERFORM BESETZT GO C.
           ADD 1 BZ-LFD TO WH-LFD.
           IF BZ-KTONR NOT = WH-KTONR OR BZ-BUDAT NOT = WH-BUDAT
               MOVE 1 TO WH-LFD.
       E.  MOVE WH-BZSATZ TO BZ-SATZ.
           MOVE WH-BKEY TO BZ-KEY.
           MOVE WS-BUCH TO WZ-BUCH.
           WRITE BZ-SATZ INVALID GO B.
           IF AZ-SATZ NOT = LOW-VALUE AND BZ-OPLFD = 999
               PERFORM AZ-WRITE.
           IF KO-JOKEY = LOW-VALUE MOVE BZ-JKEY TO KO-JOKEY.
           PERFORM JOUR-DIS.
       Z.  EXIT.
      *************************** automatische Aufteilungsgegenbuchung *
       AUFBUCH SECTION.
       A.  MOVE 1 TO WE.
           IF BZ-SH = 0 MOVE 1 TO BZ-SH
                   ELSE MOVE 0 TO BZ-SH.
           MOVE BZ-KTONR TO BZ-GGKTO.
       B.  IF WE-REBET(WE) = 0 GO Z.
       C.  MOVE WE-REF(WE) TO BZ-KTONR WH-NUM.
           PERFORM LIESKTO.
           IF FEHLER PERFORM BESETZT
                     PERFORM WEITER GO C.
           IF BZ-KURS = 100,00 MOVE 0 TO BZ-FWBET
                MOVE WE-REBET(WE) TO BZ-BUBET WK-BRUTTO WK-NETTO
           else MOVE WE-SKONTO(WE) TO BZ-BUBET WK-BRUTTO WK-NETTO
                MOVE WE-REBET(WE) TO BZ-FWBET.
           MOVE WH-KAT TO WH-AKAT.
           MOVE WH-BN TO WH-ABN.
           MOVE WH-NUM TO WK-KTONR.
           IF WH-ABN = 0 MOVE WK-BRUTTO TO BZ-BUBET WK-BUBET
                    else MOVE WK-NETTO TO BZ-BUBET WK-BUBET
                         ADD WK-NETTO WK-MWST GIVING WK-BRUTTO.
           MOVE 0 TO BZ-U1 BZ-B1 BZ-U2 BZ-B2 BZ-MW BZ-VM.
           MOVE WE-SKONTO(WE) TO BZ-MW.
           MOVE WE-UKZ(WE) TO BZ-VM.
           MOVE LOW-VALUE TO AZ-SATZ.
           ADD 1 TO IX.
           PERFORM BZ-WRITE.
           EVALUATE WH-AKAT
               WHEN 0 PERFORM SACHSUM
               WHEN 2 PERFORM DEBSUM
               WHEN 3 PERFORM KREDSUM.
           ADD 1 TO WE.
           IF WE < 17 GO B.
       X.  UNLOCK BUCHZEIL.
       Z.  EXIT.
      ********************************************* Einzelgegenbuchung *
       GEGENBUCH SECTION.
       A.  MOVE BZ-GGKTO TO WH-NUM.
           PERFORM LIESKTO.
           IF FEHLER PERFORM BESETZT
               PERFORM WEITER GO A.
           MOVE WH-KAT TO WH-AKAT.
           MOVE WH-BN TO WH-ABN.
           MOVE WH-NUM TO WK-KTONR.
           MOVE BZ-KTONR TO BZ-GGKTO.
           MOVE WK-KTONR TO BZ-KTONR.
           IF WH-ABN = 0 MOVE WK-BRUTTO TO BZ-BUBET
                    else MOVE WK-NETTO TO BZ-BUBET WK-BUBET
                         ADD WK-NETTO WK-MWST GIVING WK-BRUTTO.
260208     MOVE BZ-MW TO WH-WERT.
           MOVE 0 TO BZ-U1 BZ-B1 BZ-U2 BZ-B2 BZ-MW BZ-VM.
260208     IF WH-KAT = 0 MOVE WH-WERT TO BZ-MW
                         MOVE SA-VSTKZ TO BZ-VM.
260208     MOVE 0 TO WH-WERT.
           PERFORM BUCH-ZEIL.
       Z.  EXIT.
      *********************************** autom. Kursdiffenrenzbuchung *
       DIF-BUCH SECTION.
       A.  COMPUTE WK-FDIF = WK-FDIF * -1.
           ADD WK-FDIF TO KO-KRDIF.
           MOVE BZ-GGKTO TO WM-KTONR.
           IF WH-AKAT = 3 GO G.
           READ DEBITOR INVALID STOP RUN.
           IF ZUGRIF PERFORM BESETZT
                     PERFORM WEITER GO A.
           COMPUTE WL = DE-KTOART + 1 + (BZ-SH * 3).
           ADD WK-FDIF TO KO-DEBSUM(WL).
           MOVE WK-FDIF TO BZ-BUBET.
           ADD BZ-BUBET TO KO-JHABEN.
           ADD BZ-BUBET TO DE-HABEN.
           REWRITE DE-SATZ.
           GO Q.
       G.  READ KREDIT INVALID STOP RUN.
           IF ZUGRIF PERFORM BESETZT
                     PERFORM WEITER GO G.
           COMPUTE WL = KR-KTOART + 1 + (BZ-SH * 3).
           ADD WK-FDIF TO KO-KREDSUM(WL)
           MOVE WK-FDIF TO BZ-BUBET.
           ADD BZ-BUBET TO KO-JSOLL.
           ADD BZ-BUBET TO KR-SOLL.
           REWRITE KR-SATZ.
       Q.  MOVE 0 TO BZ-U1 BZ-B1 BZ-U2 BZ-B2 BZ-MW BZ-VM.
           MOVE WM-FDKTO TO BZ-GGKTO.                *> Konto Kursdiff.
           MOVE "autom. Kurs-/Rundungsdif." TO BZ-TX.
           MOVE 8 TO BZ-VM.
           ADD 1 TO IX.
           PERFORM BZ-WRITE.
           MOVE 0 TO BZ-VM.
           MOVE WM-KTONR TO BZ-GGKTO.
           MOVE SPACE TO BZ-TX.
       Z.  EXIT.
      ********************** automatische Einzel-Gegenbuchung anzeigen *
       AUTOBUCH SECTION.
       A.  MOVE BZ-BUDAT TO WC-DATUM.
           PERFORM DATDREH.
           COMPUTE VDU-LP = IY * 200 + 202 + VDU-ECK.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           ADD 9 TO VDU-LP.
           DISPLAY WE-SYM(BZ-SYM + 1) with highlight AT VDU-LP.
           MOVE WE-SKZ(BZ-SYM + 1) TO WE-SYMT.
           MOVE BZ-BELNR TO WD-NUM.
           ADD 3 TO VDU-P.
           DISPLAY WD-NUM AT VDU-LP.
           MOVE BZ-BUDAT TO WC-DATUM.
           PERFORM DATDREH.
           ADD 7 TO VDU-P.
           DISPLAY VDU-DATUM AT VDU-LP.
           ADD 9 TO VDU-LP.
           DIVIDE 10 INTO BZ-GGKTO GIVING WD-KTO.
           DISPLAY WD-KTO with highlight AT VDU-LP.
           IF BZ-SH = 0 MOVE 1 TO BZ-SH
                        COMPUTE VDU-LP = IY * 200 + 248 + VDU-ECK
                   ELSE MOVE 0 TO BZ-SH
                        COMPUTE VDU-LP = IY * 200 + 238 + VDU-ECK.
           MOVE BZ-BUBET TO WD-SALDO.
           DISPLAY WD-SALDO  with highlight AT VDU-LP.
           MOVE WK-AMCODE TO WT-TX.
           CALL "CAUP" USING "21CONV08" WH-CREG.
           MOVE WT-TX TO WH-MCODE.
           PERFORM VARYING WX FROM 40 BY -1 UNTIL WX = 0
               OR BZ-TX(WX:1) NOT = SPACE CONTINUE.
           IF WX = 0 MOVE 1 TO WX
           else ADD 2 TO WX.
           MOVE WH-MCODE TO BZ-TX(WX:8).
           COMPUTE VDU-LP = IY * 200 + 302 + VDU-ECK.
           DISPLAY BZ-TX AT VDU-LP.
       Z.  EXIT.
      ******************************** automatische Sammelgegenbuchung *
       SAMMELBUCH SECTION.
       A.  MOVE BZ-GGKTO TO WH-NUM.
           PERFORM LIESKTO.
           IF FEHLER PERFORM BESETZT
                     PERFORM WEITER GO A.
           MOVE WH-KAT TO WH-AKAT.
           MOVE WH-BN TO WH-ABN.
           MOVE WH-NUM TO WK-KTONR.
           MOVE 19 TO BZ-SYM.
           MOVE 0 TO BZ-FWBET BZ-KURS BZ-FSYM.
           MOVE 999999   TO BZ-GGKTO.                  *> Sammelbuchung
           MOVE WK-KTONR TO BZ-KTONR.                  *> Gegenkonto-Nr.
           IF WH-ABN = 0 MOVE WK-BRUTTO TO BZ-BUBET
                    else MOVE WK-NETTO TO BZ-BUBET WK-BUBET
                         ADD WK-NETTO WK-MWST GIVING WK-BRUTTO.
           MOVE 0 TO BZ-U1 BZ-B1 BZ-U2 BZ-B2 BZ-MW BZ-VM.
           IF BZ-SH = 0 MOVE 1 TO BZ-SH
                        MOVE 900 TO BZ-LFD
                   ELSE MOVE 0 TO BZ-SH
                        MOVE 901 TO BZ-LFD.
           MOVE SPACE TO BZ-TX.
           ADD 1 TO BZ-SEITE.
           MOVE BZ-SEITE TO WH-JS.
       C.  MOVE BZ-SATZ TO WH-BZSATZ.
           MOVE SPACE TO BZ-SATZ(14:).
           READ BUCHZEIL INVALID MOVE WH-BZSATZ TO BZ-SATZ
               MOVE WK-BUBET TO BZ-BUBET GO E.
           IF ZUGRIF PERFORM BESETZT GO C.
           IF WE-USE NOT = BZ-USE MOVE WH-BZSATZ TO BZ-SATZ
               ADD -2 TO BZ-LFD GO C.
      *------------------------> Achtung auf keygleiche Sammelbuchung <-
           ADD WK-BUBET TO BZ-BUBET.
       E.  EVALUATE WH-AKAT
               WHEN 0 PERFORM SACHSUM
               WHEN 2 PERFORM DEBSUM
               WHEN 3 PERFORM KREDSUM.
           IF BZ-BUBET = 0 GO Q.
       F.  MOVE WH-JS TO BZ-SEITE.
           MOVE WE-BELNR TO BZ-BELNR.
           PERFORM VARYING WZ-BUCH FROM 115 BY -1 UNTIL WZ-BUCH = 75
               OR BZ-SATZ(WZ-BUCH:1) NOT = SPACE CONTINUE.
           WRITE BZ-SATZ INVALID REWRITE BZ-SATZ.
           IF KO-JOKEY = LOW-VALUE MOVE BZ-JKEY TO KO-JOKEY.
           GO X.
      *-----------------------------> lîschen wenn Sammelbuchung leer <-
       Q.  DELETE BUCHZEIL INVALID GO F.
       X.  UNLOCK BUCHZEIL.
       Z.  EXIT.
      ********************************************** Erstellen OP-Satz *
       MACHOP SECTION.
       A.  IF WH-AKAT < 2 GO Z.
           CALL "CAUP" USING "0717040473000" WH-CREG.
           INITIALIZE OP-SATZ.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Erstellen OP-Satz " with highlight AT VDU-LP.
           ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "Sy.Ref.-Nr. Datum    Valuta             Betrag  %-1
      -       "Tg. %-2 Tg. TgN. < >" AT VDU-LP.
           MOVE BZ-BELNR TO BZ-REFNR WD-NUM OP-REF OP-RENUM.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY WE-SYM(BZ-SYM + 1) with highlight AT VDU-LP.
           ADD 3 TO VDU-LP.
           DISPLAY WD-NUM with highlight AT VDU-LP.
           MOVE BZ-BELDAT TO WC-DATUM OP-VALDAT OP-REDAT.
           MOVE BZ-SYM TO OP-SYM.
           MOVE BZ-FSYM TO OP-FSYM.
           MOVE BZ-KURS TO OP-KURS.
           PERFORM DATDREH.
           ADD 7 TO VDU-P.
           DISPLAY VDU-DATUM AT VDU-LP.
           ADD 322 VDU-ECK GIVING VDU-LP.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           ADD 9 TO VDU-P
           DISPLAY OP-FSYM with highlight foreground-color 6 AT VDU-LP.
      *    MOVE BZ-BUBET TO WD-SALDO OP-REBETRAG.
           MOVE WF-BET TO BZ-FWBET OP-REBETRAG WD-SALDO.
           ADD 336 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-SALDO with highlight AT VDU-LP.
           MOVE BZ-U1 TO OP-U1.
           MOVE BZ-U2 TO OP-U2.
           MOVE BZ-B1 TO OP-B1.
           MOVE BZ-B2 TO OP-B2.
           MOVE BZ-MW TO OP-MW.
      *--------------------> wenn GA im Haben oder GE im Soll gebucht <-
           if wh-akat = 2 and bz-sh = 1 or (wh-akat = 3 and bz-sh = 0)
               multiply -1 by op-rebetrag
               multiply -1 by OP-U1
               multiply -1 by OP-U2
               multiply -1 by OP-B1
               multiply -1 by OP-B2
               multiply -1 by OP-MW.
      *-----------------------------------------------------------------
       B.  IF WH-AKAT = 2 MOVE DE-KOND TO BZ-KOND WH-KK
                     else MOVE KR-KOND TO BZ-KOND WH-KK.
       C.  ADD 351 VDU-ECK GIVING VDU-LP.
           IF WH-SK1 NOT = 0 MOVE WH-SK1 TO WD-PZ
               DISPLAY WD-PZ with highlight AT VDU-LP
               MOVE WH-TG1 TO WD-TG
               ADD 4 TO VDU-LP
               DISPLAY WD-TG with highlight AT VDU-LP.
           ADD 359 VDU-ECK GIVING VDU-LP.
           IF WH-SK2 NOT = 0 MOVE WH-SK2 TO WD-PZ
               DISPLAY WD-PZ with highlight AT VDU-LP
               MOVE WH-TG2 TO WD-TG
               ADD 4 TO VDU-LP
               DISPLAY WD-TG with highlight AT VDU-LP.
           ADD 367 VDU-ECK GIVING VDU-LP.
           IF WH-TGN NOT = 0 MOVE WH-TGN TO WD-TG
               DISPLAY WD-TG with highlight AT VDU-LP.
      *------------------------------------------------> ob Daten ok? <-
       D.  DISPLAY "<ret>= OP ok!, < >= zur Valuta, < >= éndg. Kondition
      -        "en, <esc>= Abbruch" AT 2301.
           DISPLAY "<>= zur Ref.Nr." AT 2401.
           CALL "CAUP" USING "1002730000" WH-CREG.
           IF ESC GO X.
           IF RET AND WH-NUM = 0 GO W.
           IF WOLI GO M.
           IF KIST CALL "FIBSTAM" USING "12KOND" WH-CREG
               CANCEL "FIBSTAM"
               IF WH-NUM = 0 GO D
               else MOVE WT-KONDIT(WH-NUM) TO WH-KK GO C.
           IF AUF GO K.
           IF NOT AB GO D.
       E.  DISPLAY "<ret>= Skonto-1 (x,x), <esc>= Abbruch" AT 2301.
           MOVE WH-SK1 TO WH-WERT.
           CALL "CAUP" USING "1003512103" WH-CREG.
           IF ESC GO B.
           IF NOT RET GO E.
           MOVE WH-WERT TO WH-SK1 WD-PZ.
           DISPLAY WD-PZ with highlight AT VDU-LP.
       F.  DISPLAY "<ret>= Tage-1, <esc>= Abbruch" AT 2301.
           MOVE WH-TG1 TO WH-WERT.
           CALL "CAUP" USING "1003553003" WH-CREG.
           IF ESC GO B.
           IF WOLI GO E.
           IF NOT RET GO F.
           MOVE WH-WERT TO WH-TG1 WD-TG.
           DISPLAY WD-TG with highlight AT VDU-LP.
       G.  DISPLAY "<ret>= Skonto-2 (x,x), <esc>= Abbruch" AT 2301.
           MOVE WH-SK2 TO WH-WERT.
           CALL "CAUP" USING "1003592103" WH-CREG.
           IF ESC GO B.
           IF WOLI GO F.
           IF NOT RET GO G.
           MOVE WH-WERT TO WH-SK2 WD-PZ.
           DISPLAY WD-PZ with highlight AT VDU-LP.
       H.  DISPLAY "<ret>= Tage-2, <esc>= Abbruch" AT 2301.
           MOVE WH-TG2 TO WH-WERT.
           CALL "CAUP" USING "1003633003" WH-CREG.
           IF ESC GO B.
           IF WOLI GO G.
           IF NOT RET GO E.
           MOVE WH-WERT TO WH-TG1 WD-TG.
           DISPLAY WD-TG with highlight AT VDU-LP.
       I.  DISPLAY "<ret>= Nettotage, <esc>= Abbruch" AT 2301.
           MOVE WH-TGN TO WH-WERT.
           CALL "CAUP" USING "1003673003" WH-CREG.
           IF ESC GO B.
           IF WOLI GO H.
           IF NOT RET GO E.
           MOVE WH-WERT TO WH-TGN WD-TG.
           DISPLAY WD-TG with highlight AT VDU-LP.
           GO D.
      *---------------------------------------> énderung Valuta-Datum <-
       K.  DISPLAY "<ret-leer>= ok, <ret>= neue Valuta" AT 2301.
           MOVE OP-VALDAT TO WZ-DATUM WH-WERT.
           CALL "CAUP" USING "1103226006" WH-CREG.
           IF RET AND WH-NUM = 0 GO D.
           IF NOT RET GO K.
           IF WZ-DATUM = 0 MOVE OP-VALDAT TO WZ-DATUM.
           MOVE WX-DATUM TO WC-DATUM OP-VALDAT.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           GO D.
      *------------------------------------------------> Referenz-Nr. <-
       M.  DISPLAY "<ret>= Referenz-Nr., <esc>= Abbruch" AT 2301.
           MOVE OP-REF TO WH-WERT.
           CALL "CAUP" USING "1003066006" WH-CREG.
           IF ESC GO B.
           IF WOLI GO B.
           IF NOT RET GO M.
           MOVE WH-WERT TO OP-REF WD-NUM.
           DISPLAY WD-NUM with highlight AT VDU-LP.
           GO D.
       W.  MOVE WH-KK TO OP-KOND.
           MOVE WK-KTONR TO OP-KTONR WH-KAT.
           MOVE WH-KAT TO OP-DK.
           WRITE OP-SATZ INVALID ADD 1 TO OP-LFD GO W.
           IF ZUGRIF PERFORM BESETZT GO W.
           MOVE OP-LFD TO BZ-OPLFD.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ************************************************** OP-Abstattung *
       AUS-OP SECTION.
       A.  IF WH-AKAT < 2 GO Z.
           CALL "CAUP" USING "0721200330000" WH-CREG.
           ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "Bitte warten" with BLINK AT VDU-LP.
           MOVE 0 TO BZ-B1 BZ-B2 BZ-MW BZ-U1 BZ-U2 WK-BET WK-FDIF.
           IF WH-AKAT = 3 MOVE WK-SKTO TO BZ-MW.
       C.  PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 50
               OR WE-OPKEY(WX) NOT = LOW-VALUE CONTINUE.
           IF WX > 50 GO X.
           MOVE WE-OPKEY(WX) TO OP-KEY.
       E.  READ OPDATEI INVALID GO G.
           IF ZUGRIF PERFORM BESETZT GO E.
240302     ADD WE-REBET(WX) TO OP-ZABETRAG.             *> wz entfernt
990813*    if wz not = wx ADD WE-REBET(WX) TO OP-ZABETRAG.
      *--------------------------------------> fÅr Debitoren getestet <-
           COMPUTE WH-ERG rounded = WE-REBET(WX) * WE-KURS(WX) / 100.
           ADD WH-ERG TO WK-FDIF.
           MOVE BZ-BUDAT TO OP-ZADAT.
      *--------------------------> Auszifferungsdatei immer abstellen <-
           MOVE 999 TO BZ-OPLFD.
           MOVE WE-REBET(WX) TO AZ-OPBET(WX).
           MOVE WE-SKONTO(WX) TO AZ-OPSKTO(WX).
           MOVE OP-RENUM TO AZ-OPNR(WX).
           MOVE OP-SYM TO AZ-OPSYM(WX).
           MOVE OP-FSYM TO AZ-OPFSYM(WX).
           MOVE WE-KURS(WX) TO AZ-KURS(WX).
           MOVE 0 TO OP-SA.
           IF OP-REBETRAG = OP-ZABETRAG MOVE OP-DK TO OP-SA.
           REWRITE OP-SATZ.
      *-------------------------------> Rundungsdiffenrenz bei Skonto <-
           MOVE WE-SYMT TO WZ-SYMT.
           MOVE WE-SKZ(OP-SYM + 1) TO WE-SYMT.
           IF WE-DIV = 5 MOVE WT-UST(OP-U1) TO WH-UST
               COMPUTE WZ-BET = WE-REBET(WX) / (WH-UST / 100 + 1)
                       COMPUTE WZ-UST = WZ-UST + WE-REBET(WX) - WZ-BET
                       MOVE OP-U1 TO WK-U1.
           MOVE WZ-SYMT TO WE-SYMT.
      *-----------------------------------------> Vorsteuerermittlung <-
           IF OP-B1 = 0 AND OP-B2 = 0 GO G.
           ADD OP-B1 OP-B2 OP-MW GIVING WV-BAS.
           DIVIDE WV-BAS INTO OP-MW GIVING WH-TEIL ROUNDED.
           COMPUTE WE-SKONTO(WX) rounded =
               WE-SKONTO(WX) * WE-KURS(WX) / 100.
           MULTIPLY WH-TEIL BY WE-SKONTO(WX) GIVING WK-MWST ROUNDED.
           DIVIDE WV-BAS INTO WE-SKONTO(WX) GIVING WH-TEIL ROUNDED.
           EVALUATE WH-AKAT
              WHEN 3
      *              COMPUTE KO-SKTOERL = KO-SKTOERL +
      *                  (WE-SKONTO(WX) * WE-KURS(WX)) / 100
      *              ADD WE-SKONTO(WX) TO KO-SKTOERL
                     SUBTRACT WK-MWST FROM KO-SKTOVST
                     MOVE 3 TO BZ-VM
                     ADD WK-MWST TO BZ-B1
      *----------------> Absicherung gleiche bz-u1 -addition !!!!! <-
              WHEN 2 MOVE 4 TO BZ-VM
                     IF OP-U1 not = 0 PERFORM UST-RECH-OP
                     end-if
                     IF OP-U2 not = 0 PERFORM UST-RECH-OP.

      *              IF OP-U1 not = 0 and BZ-U1 = 0
      *                  MOVE OP-U1 TO BZ-U1
      *                  MULTIPLY OP-B1 BY WH-TEIL GIVING WK-BET ROUNDED
      *                  ADD WK-BET TO BZ-B1
      *                  ADD WK-BET TO KO-SKTOAUF(OP-U1)
      *                  SUBTRACT WK-BET FROM KO-MWSTBMG(BZ-U1)
      *                  MOVE WT-UST(BZ-U1) TO WH-UST
      *                  COMPUTE WK-BET = (WH-UST / 100) * BZ-B1
      *                  ADD WK-BET TO BZ-MW
      *                  ADD WK-BET TO KO-SKTOAUF(OP-U1)
      *              else move op-u1 to op-u2
      *                   move op-b1 to op-b2
      *                  end-if
      *              IF OP-U2 not = 0
      *                  MOVE OP-U2 TO BZ-U2
      *                  MULTIPLY OP-B2 BY WH-TEIL GIVING WK-BET ROUNDED
      *                  ADD WK-BET TO BZ-B2
      *                  ADD WK-BET TO KO-SKTOAUF(OP-U2)
      *                  SUBTRACT WK-BET FROM KO-MWSTBMG(BZ-U2)
      *                  MOVE WT-UST(BZ-U2) TO WH-UST
      *                  COMPUTE WK-BET = (WH-UST / 100) * BZ-B2
      *                  ADD WK-BET TO BZ-MW
      *                  ADD WK-BET TO KO-SKTOAUF(OP-U2).
       G.  MOVE LOW-VALUE TO WH-RT(WX).
           GO C.
       X.  IF WK-FDIF not = 0
               COMPUTE WK-FDIF = BZ-BUBET - WK-FDIF + WK-SKTO.
           IF WK-FDIF < 0,15 and > -0,15
               ADD WK-FDIF TO WK-SKTO
               MOVE 0 TO WK-FDIF.
           IF WH-AKAT = 3 ADD WK-SKTO TO KO-SKTOERL.
           EVALUATE WH-AKAT
               WHEN 3 SUBTRACT BZ-B1 BZ-B2 FROM WK-SKTO GIVING BZ-MW
               WHEN 2 SUBTRACT BZ-B1 BZ-B2 BZ-MW FROM WK-SKTO
                           GIVING WK-BET
                      ADD WK-BET TO BZ-MW
                      IF BZ-U1 NOT = 0 ADD WK-BET TO KO-SKTOAUF(BZ-U1)
                      else IF BZ-U2 NOT = 0
                               ADD WK-BET TO KO-SKTOAUF(BZ-U2).
           CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      **************************************** Skonto / Ust-Aufteilung *
       UST-RECH-OP SECTION.
       A.  MOVE 0 TO WM-B.
           IF OP-U1 = 0 GO D.
      *-------------------------------------> 1. OP-Ust nach 1 oder 2 <-
           IF OP-U1 = BZ-U1 or BZ-U1 = 0
                MOVE OP-U1 TO BZ-U1
                MOVE OP-B1 TO WM-B
                PERFORM UST-U1
           else IF OP-U1 = BZ-U2 or BZ-U2 = 0
                    MOVE OP-U1 TO BZ-U2
                    MOVE OP-B1 TO WM-B
                    PERFORM UST-U2.
      *-------------------------------------> 2. OP-Ust nach 1 oder 2 <-
       D.  IF OP-U2 = 0 GO Z.
           IF OP-U2 = BZ-U1 or BZ-U1 = 0
                MOVE OP-U2 TO BZ-U1
                MOVE OP-B2 TO WM-B
                PERFORM UST-U1
           else IF OP-U2 = BZ-U2 or BZ-U2 = 0
                MOVE OP-U2 TO BZ-U2
                MOVE OP-B2 TO WM-B
                PERFORM UST-U2.
       Z.  EXIT.
      ******************************************* echte UST-Aufteilung *
       UST-U1 SECTION.
       A.  MULTIPLY WM-B BY WH-TEIL GIVING WK-BET ROUNDED.
           ADD WK-BET TO BZ-B1.
           ADD WK-BET TO KO-SKTOAUF(BZ-U1).
           SUBTRACT WK-BET FROM KO-MWSTBMG(BZ-U1).
           MOVE WT-UST(BZ-U1) TO WH-UST.
           COMPUTE WK-BET = (WH-UST / 100) * BZ-B1.
           ADD WK-BET TO BZ-MW.
           ADD WK-BET TO KO-SKTOAUF(BZ-U1).
       Z.  EXIT.
      ******************************************* echte UST-Aufteilung *
       UST-U2 SECTION.
       A.  MULTIPLY WM-B BY WH-TEIL GIVING WK-BET ROUNDED.
           ADD WK-BET TO BZ-B2.
           ADD WK-BET TO KO-SKTOAUF(BZ-U2).
           SUBTRACT WK-BET FROM KO-MWSTBMG(BZ-U2).
           MOVE WT-UST(BZ-U2) TO WH-UST.
           COMPUTE WK-BET = (WH-UST / 100) * BZ-B2.
           ADD WK-BET TO BZ-MW.
           ADD WK-BET TO KO-SKTOAUF(BZ-U2).
       Z.  EXIT.
      ************************************************** Ust-errechnen *
       RECH-UST SECTION.
       A.  MOVE 0 TO WH-NUM WH-WERT.
           DIVIDE 100 INTO WH-UST GIVING WC-UST.
           ADD 1 TO WC-UST.
           EVALUATE WU
               WHEN 0 DIVIDE WC-UST INTO WK-BRUTTO GIVING WK-BASIS
                      COMPUTE WK-MWST rounded =
                           (WH-UST / 100) * WK-BASIS
               WHEN 1 MOVE WK-BRUTTO TO WK-BASIS
                      COMPUTE WK-MWST  rounded =
                           (WH-UST / 100) * WK-BASIS
                      IF BZ-VM = 5 PERFORM IG-UST
               WHEN 2 DISPLAY "<esc>=Abbruch, <ret>= Steuer" AT 2301
                      MOVE 0 TO WH-WERT
                      COMPUTE VDU-LP = IY * 200 + 267
                      CALL "CAUP" USING "1000007211" WH-CREG
                      MOVE WH-WERT TO WK-MWST.
           MOVE 67 TO VDU-P.
           MOVE WK-MWST TO WD-UST.
           DISPLAY WD-UST with highlight AT VDU-LP
           IF BZ-VM = 5 or 9 MOVE WK-BRUTTO TO WK-NETTO
           else SUBTRACT WK-MWST FROM WK-BRUTTO GIVING WK-NETTO.
           IF WH-AKAT NOT = 0 GO Z.
      *-----------------------------------> Brutto/Netto-Kz auswerten <-
           IF WH-ABN = 1 MOVE WK-NETTO TO BZ-BUBET WD-SALDO
               DISPLAY WD-SALDO with highlight AT VDU-REL.
       Z.  EXIT.
      **************************************** innergemeinschaftl. UST *
       IG-UST SECTION.
       A.  EVALUATE WH-AKAT also BZ-SH
               WHEN 0 also 1 COMPUTE WK-MWST = WK-MWST * -1
               WHEN 3 also 0 COMPUTE WK-MWST = WK-MWST * -1.
       Z.  EXIT.
      ********************************************* Ust + Bmg addieren *
       ADDUST-BMG SECTION.
       A.  IF BZ-VM = 9 ADD WK-MWST TO KO-FREIUST(1) KO-FREIUST(2)
               GO Z.
           ADD WK-MWST TO KO-MWST.
           IF BZ-U1 NOT = 0 ADD BZ-B1 TO KO-MWSTBMG(BZ-U1).
           IF BZ-U2 NOT = 0 ADD BZ-B2 TO KO-MWSTBMG(BZ-U2).
       Z.  EXIT.
      ***************************************** Ust + Bmg subtrahieren *
       SUBUST-BMG SECTION.
       A.  IF BZ-VM = 9
               SUBTRACT WK-MWST FROM KO-FREIUST(1) KO-FREIUST(2)
               GO Z.
           SUBTRACT WK-MWST FROM KO-MWST.
           IF BZ-U1 NOT = 0 SUBTRACT BZ-B1 FROM KO-MWSTBMG(BZ-U1).
           IF BZ-U2 NOT = 0 SUBTRACT BZ-B2 FROM KO-MWSTBMG(BZ-U2).
       Z.  EXIT.
      *************************************************** Addition UST *
       ADD-UST SECTION.
       A.  IF WE-OP = 2 AND BZ-VM > 1 GO X.
           EVALUATE WE-UST
               WHEN 1 if wh-akat = 0;
                          if bz-sh = 1 PERFORM ADDUST-BMG
                          else PERFORM SUBUST-BMG
                      else
                      if wh-akat > 1;
                          if bz-sh = 0 PERFORM ADDUST-BMG
                          else PERFORM SUBUST-BMG
                      move wk-mwst to wd-ust
               WHEN 2 if wh-akat = 0;
                          if bz-vm = 5
                              add wk-mwst TO KO-IGMUST(1) KO-IGMUST(2)
                          else
                          if bz-sh = 0 add wk-mwst TO KO-VST
                          else subtract wk-mwst from ko-vst
                      else
                      if wh-akat > 1;
                          if bz-vm = 5
                              add wk-mwst TO KO-IGMUST(1) KO-IGMUST(2)
                          else
                          if bz-vm = 9 PERFORM ADDUST-BMG
                          else
                          if bz-sh = 1 add wk-mwst TO KO-VST
                      else subtract wk-mwst from ko-vst
                      end-if
                      move wk-mwst to wd-ust
               WHEN 6 if wh-akat > 1 move wk-mwst to wz-ust.
       X.  MOVE 0 TO WK-MWST WK-BASIS.
       Z.  EXIT.
      ******************************* Summenbildung Sachkonten Kat.= 0 *
       SACHSUM SECTION.
       A.  MOVE WK-KTONR TO SA-KTONR.
           IF WM-L = 0 GO C.
           READ SACHBUCH INVALID PERFORM KTO-FEHL.
           IF ZUGRIF PERFORM BESETZT
                     PERFORM WEITER GO A.
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
           IF ZUGRIF PERFORM BESETZT
                     PERFORM WEITER GO A.
       C.  COMPUTE WL = DE-KTOART + 1 + (BZ-SH * 3).
           ADD BZ-BUBET TO KO-DEBSUM(WL).
           ADD WK-SKTO  TO KO-DEBSUM(WL).
           IF BZ-SH = 0 ADD BZ-BUBET TO KO-JSOLL
                        ADD BZ-BUBET TO DE-SOLL
                   ELSE ADD BZ-BUBET TO KO-JHABEN
                        ADD BZ-BUBET TO DE-HABEN
                        ADD WK-SKTO TO KO-JHABEN
                        ADD WK-SKTO TO DE-HABEN.
           IF WE-DIV = 1 or 2 ADD BZ-BUBET TO DE-UMSATZ
                              SUBTRACT BZ-MW FROM DE-UMSATZ.
           REWRITE DE-SATZ.
           MOVE 0 TO WK-SKTO.
       Z.  EXIT.
      ******************************* Summenbildung Kreditoren Kat.= 2 *
       KREDSUM SECTION.
       A.  MOVE WK-KTONR TO KR-KTONR.
           IF WM-L = 0 GO C.
           READ KREDIT INVALID PERFORM KTO-FEHL.
           IF ZUGRIF PERFORM BESETZT
                     PERFORM WEITER GO A.
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
       JOUR-DIS SECTION.
       A.  IF IX = 0 GO Z.
       C.  IF IX > 16 PERFORM B IN BS-JZEIL GO C.
           DIVIDE 10 INTO BZ-KTONR GIVING WD-KTO WD-SAKO.
           MOVE BZ-KTONR TO WD-KZ.
           IF WD-KZ < 2 EVALUATE WM-SK
                            WHEN 3 MOVE SPACE TO WD-SAKO(1:2)
                            WHEN 4 MOVE SPACE TO WD-SAKO(1:1)
                        end-evaluate MOVE WD-SAKO(1:) TO WD-KTO(1:).
           COMPUTE VDU-LP = IX * 100 + 402 + VDU-ECK.
           DISPLAY WD-KTO with highlight AT VDU-LP.
           MOVE BZ-BUDAT TO WC-DATUM.
           ADD 8 TO VDU-P.
           PERFORM BU-TEIL.
           MOVE BZ-KTONR TO WD-KZ.
           IF WM-JO = 1 and WD-KZ = 0 and BZ-VM not = 5 and BZ-VM < 10
                ADD BZ-MW BZ-BUBET GIVING WD-SALDO
           else MOVE BZ-BUBET TO WD-SALDO.
           IF BZ-SH = 1 ADD 4 TO VDU-LP.
           DISPLAY WD-SALDO with highlight AT VDU-LP.
           MOVE 64 TO VDU-P.
           MOVE 0 TO WM-UST.
           EVALUATE BZ-VM
               WHEN 0 CONTINUE
               WHEN 1 IF BZ-U1 NOT = 0
                          MOVE WT-UST(BZ-U1) TO WD-PROZ
                          DISPLAY WD-PROZ AT VDU-LP "m"
                          MOVE BZ-MW TO WM-UST
               WHEN 2 IF BZ-U1 NOT = 0
                          MOVE WT-UST(BZ-U1) TO WD-PROZ
                          IF BZ-U1 > 4 DISPLAY WD-PROZ AT VDU-LP "i"
                          else DISPLAY WD-PROZ AT VDU-LP "v"
                          end-if
                          MOVE BZ-MW TO WM-UST
               WHEN 3 IF BZ-MW NOT = 0 or BZ-B1 NOT = 0 or BZ-B2 NOT = 0
                         DISPLAY "Ske" AT VDU-LP
                         ADD BZ-MW BZ-B1 BZ-B2 GIVING WM-UST
               WHEN 4 IF BZ-MW NOT = 0 or BZ-B1 NOT = 0 or BZ-B2 NOT = 0
                         DISPLAY "Ska" AT VDU-LP
                         ADD BZ-MW BZ-B1 BZ-B2 GIVING WM-UST
               WHEN 5 IF BZ-U1 NOT = 0
                          MOVE WT-UST(BZ-U1) TO WD-PROZ
                          DISPLAY WD-PROZ AT VDU-LP "i"
                          end-if
                          MOVE BZ-MW TO WM-UST
               WHEN 6 IF BZ-U1 NOT = 0
                          MOVE WT-UST(BZ-U1) TO WD-PROZ
                          DISPLAY WD-PROZ AT VDU-LP "n"
                          MOVE BZ-MW TO WM-UST
               WHEN 9 IF BZ-U1 NOT = 0
                          MOVE WT-UST(BZ-U1) TO WD-PROZ
                          DISPLAY WD-PROZ AT VDU-LP "f"
                          MOVE BZ-MW TO WM-UST.
           IF WM-UST NOT = 0 MOVE WM-UST TO WD-UST
               MOVE 67 TO VDU-P
               DISPLAY WD-UST with highlight AT VDU-LP.
           MOVE BZ-JKEY TO WH-JOKEY WT-JKEY(IX).
           ADD 1 TO WR.
       Z.  EXIT.
      **************************************** Datum - Symbol - Ggkto. *
       BU-TEIL SECTION.
       A.  PERFORM DATDREH.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           ADD 9 TO VDU-P.
           DISPLAY WE-SYM(BZ-SYM + 1) with highlight AT VDU-LP.
           ADD 3 TO VDU-P.
           MOVE BZ-BELNR TO WD-NUM.
           DISPLAY WD-NUM with highlight AT VDU-LP.
           ADD 7 TO VDU-P.
           MOVE BZ-BELDAT TO WC-DATUM.
           PERFORM DATDREH.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           ADD 9 TO VDU-P.
           IF BZ-GGKTO = 999999 DISPLAY " *sabu*" AT VDU-LP GO X.
           IF BZ-GGKTO = 0 AND BZ-Z = 4
               DISPLAY " *auft*" AT VDU-LP GO X.
           IF BZ-Z > 0 AND BZ-GGKTO NOT = 0
                   DIVIDE 10 INTO BZ-GGKTO GIVING WD-KTO
                   DISPLAY WD-KTO with highlight AT VDU-LP
                else DISPLAY "       " AT VDU-LP
                end-if
                EVALUATE BZ-Z
                   WHEN 0 DISPLAY " " AT 0000
                   WHEN 1 DISPLAY "e" AT 0000
                   WHEN 2 DISPLAY "s" AT 0000
                   WHEN 4 DISPLAY "a" AT 0000.
       X.  ADD 8 TO VDU-P.
       Z.  EXIT.
      ********************************* letzte Buchung je Konto suchen *
       LAST-BUCH SECTION.
       A.  MOVE 1 TO IY.
           MOVE LOW-VALUE TO WT-ERFTAB.
           MOVE WK-KTONR TO BZ-KTONR.
           MOVE 99999999 TO BZ-BUDAT.
           MOVE 0 TO WH-U1 WH-VM.
           START BUCHZEIL KEY < BZ-KEY INVALID GO C.
       B.  MOVE SPACE TO BZ-SATZ(14:).
           READ BUCHZEIL PREVIOUS NO LOCK AT END GO C.
           IF ZUGRIF PERFORM BESETZT GO B.
           IF BZ-KTONR = WK-KTONR CALL "CAUP" USING "16CLRFEN" WH-CREG
               PERFORM BUCH-DIS
               MOVE BZ-U1 TO WH-U1
               MOVE BZ-VM TO WH-VM
               GO Z.
       C.  PERFORM INIT-BZ.
           MOVE WK-KTONR TO BZ-KTONR.
           MOVE 0 TO IY.
       Z.  EXIT.
      ************************************** Buchung je Konto anzeigen *
       BUCH-DIS SECTION.
       A.  IF IY = 0 GO Z.
           if bz-budat = 0 delete buchzeil go z.
           COMPUTE VDU-LP = IY * 200 + 202 + VDU-ECK.
           IF BZ-SYM = 99 MOVE BZ-BELDAT TO WC-DATUM
               PERFORM DATDREH
               DISPLAY VDU-DATUM with highlight AT VDU-LP
               MOVE BZ-SOLL TO WD-SALDO
               ADD 20 TO VDU-LP
               DISPLAY "S: " AT VDU-LP WD-SALDO with highlight
               MOVE BZ-HABEN TO WD-SALDO
               ADD 17 TO VDU-LP
               DISPLAY " H: " AT VDU-LP WD-SALDO with highlight
                    " Saldo: "
               SUBTRACT BZ-HABEN FROM BZ-SOLL GIVING WD-SALDO
               DISPLAY WD-SALDO with highlight at 0000
               ADD 100 TO VDU-LP
               MOVE 2 TO VDU-LP
               DISPLAY "Eb-VJ: " AT VDU-LP
               MOVE BZ-EBIL TO WD-SALDO
               DISPLAY WD-SALDO with highlight AT 0000 "  "
                   "** Wirtschaftsjahrende **" AT 0000
               GO X.
           IF BZ-SYM = 98 MOVE BZ-BELDAT TO WC-DATUM
               PERFORM DATDREH
               DISPLAY VDU-DATUM with highlight AT VDU-LP
               ADD 9 TO VDU-LP
               DISPLAY "Saldovortrag" AT VDU-LP
               ADD 27 TO VDU-LP
               IF BZ-SH = 1 ADD 12 TO VDU-LP end-if
               MOVE BZ-BUBET TO WD-SALDO
               DISPLAY WD-SALDO with highlight at VDU-LP
               ADD 100 TO VDU-LP
               MOVE 2 TO VDU-LP
      *        DISPLAY "** autom. Saldovortrag **" AT VDU-LP
               GO X.
           MOVE BZ-BUDAT TO WC-DATUM.
           PERFORM BU-TEIL.
           MOVE BZ-BUBET TO WD-SALDO.
           IF BZ-SH = 1 ADD 12 TO VDU-LP.
           DISPLAY WD-SALDO with highlight AT VDU-LP.
           MOVE 64 TO VDU-P.
      *    MOVE 0 TO WD-UST.
           MOVE 0 TO WM-UST.
           MOVE SPACE TO WD-PROZ(1:).
           EVALUATE BZ-VM
               WHEN 0 CONTINUE
               WHEN 1 IF BZ-U1 NOT = 0 MOVE WT-UST(BZ-U1) TO WD-PROZ
                                       DISPLAY WD-PROZ AT VDU-LP "m"
                                       MOVE BZ-MW TO WM-UST
               WHEN 2 IF BZ-U1 NOT = 0 MOVE WT-UST(BZ-U1) TO WD-PROZ
                      end-if IF BZ-MW NOT = 0 MOVE BZ-MW TO WM-UST
                                       DISPLAY WD-PROZ AT VDU-LP "v"
               WHEN 3 IF BZ-MW NOT = 0
                      or BZ-B1 NOT = 0 or BZ-B2 NOT = 0
                         DISPLAY "Ske" AT VDU-LP
                         ADD BZ-MW BZ-B1 BZ-B2 GIVING WM-UST
               WHEN 4 IF BZ-MW NOT = 0
                      or BZ-B1 NOT = 0 or BZ-B2 NOT = 0
                         DISPLAY "Ska" AT VDU-LP
                         ADD BZ-MW BZ-B1 BZ-B2 GIVING WM-UST
               WHEN 5 IF BZ-U1 NOT = 0 MOVE WT-UST(BZ-U1) TO WD-PROZ
                      end-if IF BZ-MW NOT = 0 MOVE BZ-MW TO WM-UST
                                 DISPLAY WD-PROZ AT VDU-LP "i"
               WHEN 6 IF BZ-U1 NOT = 0 MOVE WT-UST(BZ-U1) TO WD-PROZ
                                       DISPLAY WD-PROZ AT VDU-LP "n"
                                       MOVE BZ-MW TO WM-UST
120103         WHEN 9 IF BZ-U1 NOT = 0
                          MOVE WT-UST(BZ-U1) TO WD-PROZ
                          DISPLAY WD-PROZ AT VDU-LP "f"
                          MOVE BZ-MW TO WM-UST.
           IF WM-UST NOT = 0 MOVE WM-UST TO WD-UST
               MOVE 67 TO VDU-P
               DISPLAY WD-UST with highlight AT VDU-LP.
           ADD 100 TO VDU-LP.
           MOVE 3 TO VDU-P.
           IF BZ-USE > 10 OR BZ-USE = 0
               DIVIDE 100 INTO BZ-SEITE GIVING WD-JONUM
               DISPLAY WD-JONUM AT VDU-LP "  " BZ-TX
           else MOVE BZ-SEITE TO WD-JONUM
               DISPLAY WD-JONUM AT VDU-LP "-" BZ-USE " " BZ-TX.
           IF BZ-BUBET not = BZ-FWBET and BZ-FWBET not = 0
               MOVE 63 TO VDU-P
               MOVE BZ-FWBET TO WD-SALDO
               DISPLAY BZ-FSYM with highlight foreground-color 6
                   AT VDU-LP WD-SALDO with highlight foreground-color 6.
      *------------------------------------> Berichtigung Kz. 27 / 28 <-
      *    if bz-budat > 20080000 and sa-vstkz = 27 or 28 and bz-vm = 0
      *        move sa-vstkz to bz-vm
      *        if bz-mw = 0 compute bz-mw = bz-fwbet - bz-bubet
      *        end-if
      *        rewrite bz-satz.
       X.  MOVE BZ-KEY TO WH-BKEY WT-KEY(IY).
           ADD 1 TO WB.
       Z.  EXIT.
      ****************************************** Lesen ohne Satzsperre *
       LIESKTO-NOLOCK SECTION.
       A.  PERFORM LIESKTO.
           UNLOCK SACHBUCH.
           UNLOCK DEBITOR.
           UNLOCK KREDIT.
           MOVE 1 TO WM-L.                 *> Flag f. freigegebens Konto
       Z.  EXIT.
      ******************************************************************
       LIESKTO SECTION.
       A.  SET RET TO TRUE.
           MOVE 0 TO WM-L.                   *> Flag f. gesperrtes Konto
           IF WH-NUM = 0 GO X.
           MOVE WH-NUM TO WD-KZ.
           MOVE WD-KZ TO WH-KAT.
           DIVIDE 10 INTO WH-NUM GIVING WD-KTO.
           EVALUATE WD-KZ
               WHEN 3 MOVE WH-NUM TO KR-KTONR GO G
               WHEN 2 MOVE WH-NUM TO DE-KTONR GO E
               WHEN 1
               WHEN 0 MOVE WH-NUM TO SA-KTONR
                      DIVIDE 10 INTO WH-NUM GIVING WD-SAKO
                      EVALUATE WM-SK
                          WHEN 3 MOVE SPACE TO WD-SAKO(1:2)
                          WHEN 4 MOVE SPACE TO WD-SAKO(1:1)
                      end-evaluate
                      MOVE WD-SAKO(1:) TO WD-KTO(1:)
               WHEN OTHER GO X.
      *---------------------------------------------> lesen Sachkonto <-
       C.  READ SACHBUCH NOT INVALID MOVE SA-BEZ TO WT-TX
               MOVE SA-BTNT TO WH-BN
               MOVE SA-MCODE TO WK-MCODE
               MOVE SA-SOLL TO WE-SOLL
               MOVE SA-HABEN TO WE-HABEN
               MOVE SA-KTONR TO WH-NUM
               ADD WE-SOLL WE-HABEN GIVING WE-SALDO GO Z.
           IF ZUGRIF PERFORM BESETZT
                     PERFORM WEITER GO X.
           IF WH-AKAT = 1 SET ESC TO TRUE GO X.
           CALL "FIBSACH" USING "21FIBSACH" WH-CREG.
           CANCEL "FIBSACH".
           IF SA-KTONR = 0 GO X ELSE GO C.
      *---------------------------------------------> lesen Debitoren <-
       E.  READ DEBITOR NOT INVALID MOVE DE-BEZ TO WT-TX
               INSPECT WT-TX REPLACING ALL "#" BY ","
               MOVE DE-MCODE TO WK-MCODE
               MOVE DE-SOLL TO WE-SOLL
               MOVE DE-HABEN TO WE-HABEN
               MOVE 0 TO WH-BN
               MOVE DE-KTONR TO WH-NUM
               ADD WE-SOLL WE-HABEN GIVING WE-SALDO GO Z.
           IF ZUGRIF PERFORM BESETZT
                     PERFORM WEITER GO X.
           CALL "FIBDEBIT" USING "31FIBDEB" WH-CREG.
           CANCEL "FIBDEBIT".
           IF DE-KTONR = 0 GO X ELSE GO E.
      *--------------------------------------------> lesen Kreditoren <-
       G.  READ KREDIT NOT INVALID MOVE KR-BEZ TO WT-TX
               MOVE KR-MCODE TO WK-MCODE
               MOVE KR-SOLL TO WE-SOLL
               MOVE KR-HABEN TO WE-HABEN
               MOVE 0 TO WH-BN
               MOVE KR-KTONR TO WH-NUM
               ADD WE-SOLL WE-HABEN GIVING WE-SALDO GO Z.
           IF ZUGRIF PERFORM BESETZT
                     PERFORM WEITER GO X.
           CALL "FIBKRED" USING "41FIBKRED" WH-CREG.
           CANCEL "FIBKRED".
           IF KR-KTONR NOT = 0 GO G.
       X.  SET FEHLER TO TRUE.
       Z.  EXIT.
      *********************************************** WÑhrung anzeigen *
       CURDIS SECTION.
       A.  EVALUATE WF-SYM(1)
               WHEN "Ats" DISPLAY "Ats" with highlight
                                        foreground-color 2 AT 0000
               WHEN "Eur" DISPLAY "Eur" with highlight
                                        foreground-color 6 AT 0000.
       Z.  EXIT.
      *********************************** Auszifferungsdatei schreiben *
       AZ-WRITE SECTION.
       A.  PERFORM VARYING WZ-AUSZ FROM 50 BY -1 UNTIL WZ-AUSZ = 1
               OR AZ-RETAB(WZ-AUSZ) NOT = LOW-VALUE CONTINUE.
           MOVE BZ-KEY TO AZ-TKEY.
           COMPUTE WZ-AUSZ = WZ-AUSZ * 21 + 10.
           WRITE AZ-SATZ INVALID REWRITE AZ-SATZ.
           MOVE LOW-VALUE TO AZ-SATZ.
           MOVE 0 TO BZ-OPLFD.
       Z.  EXIT.
      ******************************************************************
       READBZ SECTION.
       A.  MOVE WH-BKEY TO BZ-KEY.
       C.  MOVE SPACE TO BZ-SATZ(14:).
           START BUCHZEIL KEY NOT < BZ-KEY INVALID GO X.
           READ BUCHZEIL NEXT AT END GO X.
           IF ZUGRIF PERFORM BESETZT GO C.
           IF BZ-KTONR = WK-KTONR PERFORM BUCH-DIS GO Z.
       X.  MOVE LOW-VALUE TO WT-ERFTAB.
       Z.  EXIT.
      **************************************************** weiterlesen *
       ABBUZEI SECTION.
       A.  IF WH-MCODE NOT = SPACE CALL "CAUP" USING "16CLRFEN" WH-CREG
                MOVE SPACE TO WH-MCODE
                MOVE LOW-VALUE TO WT-ERFTAB
                MOVE WK-KTONR TO BZ-KTONR
                MOVE WX-DATUM TO BZ-BUDAT
                MOVE 0 TO BZ-LFD WH-NUM
                MOVE BZ-KEY TO WT-KEY(1) WH-BKEY.
           PERFORM VARYING IY FROM 8 BY -1 UNTIL IY = 0
               OR WT-KEY(IY) NOT = LOW-VALUE CONTINUE.
           IF IY = 0 MOVE 1 TO IY SET FINE TO TRUE GO X.
       C.  MOVE WT-KEY(IY) TO WH-JOKEY BZ-KEY WH-BKEY.
           IF AB OR (BAB AND WH-NUM = 0) PERFORM READBZ
               ADD 1 TO IY
               MOVE 0 TO WI WB
               IF AB MOVE 6 TO WB.
       E.  PERFORM NEXTABZ WITH TEST AFTER UNTIL WI = 8 OR WB = 7.
           IF WI NOT = 8 MOVE 9 TO WI.
       X.  IF FINE PERFORM NODATA.
       Z.  EXIT.
      ******************************************************************
       NEXTABZ SECTION.
       E.  MOVE SPACE TO BZ-SATZ(14:).
           READ BUCHZEIL NEXT AT END MOVE 8 TO WI GO Z.
           IF ZUGRIF PERFORM BESETZT GO E.
           IF WK-KTONR NOT = BZ-KTONR MOVE 8 TO WI
               SET FINE TO TRUE GO Z.
           IF WI < 3 PERFORM VARYING WF FROM 1 BY 1 UNTIL WF > 7
               IF BZ-KEY = WT-KEY(WF) GO E.
           IF IY = 9 CALL "CAUP" USING "17SCRAUF" WH-CREG
                     CALL "CAUP" USING "17SCRAUF" WH-CREG
               PERFORM VARYING IY FROM 1 BY 1 UNTIL IY > 7
                   MOVE WT-KEY(IY + 1) TO WT-KEY(IY)
                   END-PERFORM.
           PERFORM BUCH-DIS.
           IF BAB ADD 1 TO IY.
       Z.  EXIT.
      **************************************************** Kontozeilen *
       AUFBUZEI SECTION.
       A.  IF WT-KEY(1) = LOW-VALUE SET FINE TO TRUE GO X.
           MOVE 7 TO WBX.
           IF WH-MCODE = SPACE MOVE WT-KEY(1) TO WH-JOKEY BZ-KEY
           else CALL "CAUP" USING "16CLRFEN" WH-CREG
                MOVE SPACE TO WH-MCODE
                MOVE LOW-VALUE TO WT-ERFTAB
                MOVE WK-KTONR TO BZ-KTONR
                MOVE WX-DATUM TO BZ-BUDAT
                MOVE 8 TO WBX
                MOVE 0 TO BZ-LFD.
           MOVE 0 TO WB.
           MOVE IY TO WX.
           IF AUF MOVE 6 TO WB.
           START BUCHZEIL KEY < BZ-KEY INVALID SET FINE TO TRUE GO X.
           MOVE 1 TO IY WI.
           PERFORM NEXTAUBZ WITH TEST AFTER UNTIL WI = 2 OR WB = WBX.
           MOVE 1 TO IY.
       X.  IF FINE PERFORM NODATA.
       Z.  EXIT.
      ******************************************************************
       NEXTAUBZ SECTION.
       N.  MOVE SPACE TO BZ-SATZ(14:).
           READ BUCHZEIL PREVIOUS AT END MOVE 2 TO WI GO Z.
           IF ZUGRIF PERFORM BESETZT GO N.
           IF WK-KTONR NOT = BZ-KTONR SET FINE TO TRUE
               MOVE 2 TO WI GO Z.
           IF WI > 8 PERFORM VARYING WF FROM 16 BY -1 UNTIL WF < 1
               IF BZ-KEY = WT-KEY(WF) GO N.
           MOVE VDU-LP TO VDU-AB.
           MOVE 1400 TO VDU-LP.
           CALL "CAUP" USING "18CLRWIN" WH-CREG.
           MOVE VDU-AB TO VDU-LP.
           CALL "CAUP" USING "22SCRAB" WH-CREG.
           CALL "CAUP" USING "22SCRAB" WH-CREG.
           PERFORM VARYING IY FROM 8 BY -1 UNTIL IY < 2
              MOVE WT-KEY(IY - 1) TO WT-KEY(IY).
           COMPUTE VDU-LP = IY * 100 + 500.
           PERFORM BUCH-DIS.
       Z.  EXIT.
      ******************************************************************
       LAST-JOUR SECTION.
       A.  MOVE 1 TO IX.
           MOVE LOW-VALUE TO WT-JOTAB.
           MOVE 999999 TO BZ-SEITE.
           MOVE WE-USE TO BZ-USE.
           START BUCHZEIL KEY < BZ-JKEY INVALID MOVE 99 TO BZ-USE GO C.
       B.  MOVE SPACE TO BZ-SATZ(14:).
           READ BUCHZEIL PREVIOUS NO LOCK AT END MOVE 99 TO BZ-USE GO C.
           IF ZUGRIF PERFORM BESETZT GO B.
       C.  IF WE-USE = BZ-USE CALL "CAUP" USING "16CLREFN" WH-CREG
               PERFORM JOUR-DIS
               GO Z.
           PERFORM INIT-BZ.
           MOVE 0 TO IX.
       Z.  EXIT.
      ******************************************************************
       FIRST-JOUR SECTION.
       A.  MOVE WE-USE TO BZ-USE.
           MOVE 0 TO BZ-SEITE.
           IF BAUF MOVE WH-NUM TO BZ-SEITE GO B.
           IF WH-NUM NOT = 0 MOVE WH-NUM TO WD-NUM
               DISPLAY "Belegnr.: " AT 2120 WD-NUM with highlight " "
                   "wird gesucht. - " "Bitte warten" with BLINK.
       B.  START BUCHZEIL KEY NOT < BZ-JKEY INVALID
               MOVE 0 TO BZ-SEITE WH-NUM
               IF BAUF SET RET TO TRUE GO A
               else GO E.
       C.  MOVE SPACE TO BZ-SATZ(14:).
           READ BUCHZEIL NEXT NO LOCK AT END MOVE 0 TO BZ-SEITE
               IF WH-NUM NOT = 0 MOVE 0 TO WH-NUM GO A.
           IF ZUGRIF PERFORM BESETZT GO C.
           IF BAUF MOVE BZ-BELNR TO WH-NUM.
       E.  IF BZ-USE NOT = WE-USE SET ESC TO TRUE GO Z.
           IF WH-NUM NOT = 0; IF BZ-BELNR NOT = WH-NUM GO C
                              else MOVE 0 TO WH-NUM.
           CALL "CAUP" USING "16CLRFEN" WH-CREG.
           MOVE LOW-VALUE TO WT-JOTAB.
           MOVE 1 TO IX.
           MOVE BZ-JKEY TO WT-JKEY(IX).
           PERFORM JOUR-DIS.
       Z.  EXIT.
      ******************************************************************
       TEXKOR SECTION.
       A.  ADD 1 TO VDU-L.
       B.  MOVE BZ-TX TO WT-TX.
           CALL "CAUP" USING "1200090140" WH-CREG.
           IF ESC MOVE BZ-TX TO WT-TX
               SET RET TO TRUE.
           IF NOT RET GO B.
           MOVE WT-TX TO BZ-TX.
           PERFORM VARYING WZ-BUCH FROM 115 BY -1 UNTIL WZ-BUCH = 75
               OR BZ-SATZ(WZ-BUCH:1) NOT = SPACE CONTINUE.
           REWRITE BZ-SATZ.
           DISPLAY BZ-TX with reverse-video AT VDU-LP.
           SUBTRACT 1 FROM VDU-L.
       Z.  EXIT.
      ******************************************************************
       KTO-FEHL SECTION.
       A.  DISPLAY "Konto fehlt oder fehlerhaft" AT 2401.
           PERFORM WEITER.
           STOP RUN.
       Z.  EXIT.
      ******************************************** hol Buchungsperiode *
       HOL-PER SECTION.
       A.  MOVE 10 TO WH-KEY.
           READ KONSTANT IGNORE LOCK INVALID MOVE 0 TO KO-WBEG(1).
           MOVE KO-TR-SR(1) TO WK-TRKTO.           *> Erlîse TR
           MOVE KO-TR-SR(2) TO WK-SRKTO.           *> Erlîse SR
           MOVE KO-TR-SR(3) TO WK-NFUST.           *> nicht fÑllige Ust
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 29
              MOVE WE-SKZ(WX + 1) TO WE-SYMT
              IF WE-DIV = 8 MOVE WX TO WK-UMBSY.   *> Symbol autom. UMB
           MOVE 9 TO WH-KEY.
           READ KONSTANT IGNORE LOCK INVALID MOVE 0 TO KO-WBEG(1).
           MOVE 0 TO WY WE-WKZ WE-WJAHR(1) WE-WJAHR(2).
           PERFORM VARYING WX FROM 4 BY -1 UNTIL WX = 0
               IF KO-WBEG(WX) NOT = 0;
                  IF WM-DATUM >= KO-WBEG(WX) AND WM-DATUM <= KO-WEND(WX)
                      MOVE WX TO WY.
           IF WY NOT = 0
               ADD KO-WKZ(WY) 50 GIVING WF           *> wegen Jahr 2000
               MOVE WF TO WE-WKZ
               MOVE KO-WBEG(WY) TO WE-WJAHR(1)
               MOVE KO-WEND(WY) TO WE-WJAHR(2).
       Z.  EXIT.
      ************************************************ PeriodenprÅfung *
       P-PRUEF SECTION.
       A.  IF not NEUPER CALL "CAUP" USING "06KOPF" WH-CREG.
           IF WE-WJAHR(1) = 0 OR WE-WJAHR(2) = 0 SET ESC TO TRUE GO Z.
           PERFORM HOL-PER.
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
           PERFORM CURDIS.
           ADD WE-USE 1 GIVING WH-KEY.
       F.  READ KONSTANT INVALID INITIALIZE KO-BSATZ
               MOVE WH-KEY TO KO-NUM WRITE KO-SATZ GO F.
           IF ZUGRIF STOP RUN.
           ADD 401 VDU-ECK GIVING VDU-LP.
           SET RET TO TRUE.
           IF NEUPER SET OHNE TO TRUE GO W.
      *------------------------------------------------------------------------
           DISPLAY "  Konto  Bu-Datum Sy Belegnr.- Datum Geg-Kto    Soll
      -        "     Haben Kz Mwst/Skt.-Bet." with background-colour 4
                     highlight AT VDU-LP.
           PERFORM LAST-JOUR.
           MOVE WM-DATUM TO WZ-DATUM.
           MOVE BZ-BUDAT TO WC-DATUM WE-PER.
           IF BZ-BUDAT = 0 MOVE WM-DATUM TO WE-PER KO-BUDAT GO W.
           IF WC-DATUM(1:4) = WZ-DATUM(1:4) GO W.
           CALL "CAUP" USING "0706070565000" WH-CREG.
           ADD 205 VDU-ECK GIVING VDU-LP.
           PERFORM DATDREH.
           DISPLAY "Buchungsdatum: " AT VDU-LP VDU-DATUM with highlight
               " stimmt mit " "      (= Bu-Datum)" with highlight.
           ADD 303 VDU-ECK GIVING VDU-LP.
           MOVE WM-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           DISPLAY "Buchungsperiode: " AT VDU-LP VDU-DATUM
                with highlight " nicht Åberein!" "   (= Tagesdatum)"
                 with highlight.
           ADD 403 VDU-ECK GIVING VDU-LP.
           DISPLAY "Bitte Tagesdatum Pkt. 8 Ñndern oder Bu-Datum ÅberprÅ
      -        "fen! < >" AT VDU-LP.
           ACCEPT WH-X AT 2318 TIMEOUT AFTER +2 with NO-ECHO
              SIZE 1 AUTO-SKIP ON EXCEPTION CONTINUE.
      *    CALL "CAUP" USING "1004610000" WH-CREG.
      *    IF RET SET EINF TO TRUE.
      *    IF not EINF SET ESC TO TRUE.
           CALL "CAUP" USING "08CLOFEN" WH-CREG.
      *    IF EINF MOVE BZ-BUDAT TO WZ-DATUM
               MOVE BZ-BUDAT TO WZ-DATUM
               MOVE 0 TO WH-PG
               CALL "CAUP" USING "03DATUM" WH-CREG
               MOVE 1 TO WH-PG GO A.
       W.  IF WE-BELNR = 0 MOVE BZ-BELNR TO WE-BELNR.
           MOVE 0 TO WM-M WM-G.
       Z.  EXIT.
      ************************************ lîschen Konto-Buchungszeile *
       LOEBUCH SECTION.
       A.  CALL "FIBZUS" USING "35ZL-LOE" WH-CREG.
           CANCEL "FIBZUS".
           IF ESC GO Z.
       X.  SET SF3 TO TRUE.
       Z.  EXIT.
      ****************************************** lîschen Journalzeilen *
       LOEBUZL SECTION.
       A.  CALL "FIBZUS" USING "35ZL-LOE" WH-CREG.
           CANCEL "FIBZUS".
           IF ESC GO Z.
           IF IX = 1; IF WT-JKEY(2) = LOW-VALUE SET AUF TO TRUE
                           CALL "CAUP" USING "16CLRFEN" WH-CREG
                           MOVE -999 TO WR
                           PERFORM AUFZEIL GO Z
                      else MOVE WT-JKEY(2) TO WH-JOKEY
           else MOVE WT-JKEY(1) TO WH-JOKEY.
           MOVE LOW-VALUE TO WT-JOTAB.
           MOVE WH-JOKEY TO WT-JKEY(1).
           SET BAB TO TRUE.
           MOVE 0 TO WH-NUM.
           CALL "CAUP" USING "16CLRFEN" WH-CREG.
           MOVE 1 TO IX.
           PERFORM ANZEIG.
           SET BAB TO TRUE.
           PERFORM ABZEIL.
       Z.  EXIT.
      ******************************************************************
       BUTEX SECTION.
       A.  IF WT-JKEY(IX) = LOW-VALUE GO Z.
           MOVE 1 TO WM-M.
           COMPUTE VDU-LP = IX * 100 + 502.
           CALL "CAUP" USING "220000SCRAB" WH-CREG.
           CALL "CAUP" USING "1300010080" WH-CREG.
           MOVE BZ-SEITE TO WD-JONUM.
           DISPLAY WD-JONUM AT VDU-LP "  " BZ-TX with reverse-video.
           IF BZ-BUBET not = BZ-FWBET and BZ-FWBET not = 0
               MOVE 63 TO VDU-P
               MOVE BZ-FWBET TO WD-SALDO
               DISPLAY BZ-FSYM with highlight foreground-color 6
                   AT VDU-LP WD-SALDO with highlight foreground-color 6.
           SUBTRACT 1 FROM VDU-L.
       Z.  EXIT.
