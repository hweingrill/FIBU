      $SET LINKCOUNT"240" ANS85 BOUND AUTOLOCK NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.      FIBZUS.
       ENVIRONMENT    DIVISION.
       CONFIGURATION   SECTION.
       OBJECT-COMPUTER.     PC.
       SPECIAL-NAMES.   DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       COPY FIBUSEC.CPY.
       DATA DIVISION.
       FILE SECTION.
       COPY FIBUFD.CPY.
      *****************************************************************
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
       01  WF-REG.
           03  WT-ERFTAB.
               05  WT-KEY              PIC X(12)         OCCURS 15.
               05  WT-ZL               PIC 99       COMP OCCURS 12.
           03  VDU-REL                 PIC 9999.
           03  VDR-REL REDEFINES VDU-REL.
               05  VDU-RL              PIC 99.
               05  VDU-RP              PIC 99.
           03  WI                      PIC 99       COMP.
           03  WL                      PIC 99       COMP.
           03  IX                      PIC 99       COMP.
           03  WD-POS                  PIC Z9.
           03  WD-KZ                   PIC 9.
           03  WD-OPNR                 PIC ZZZZ.ZZ9-.
           03  WD-SKTO                 PIC ZZ.ZZZ.ZZ9,99-.
           03  WD-BET                  PIC ZZZ.ZZZ.ZZ9,99-.
           03  WH-BN                   PIC 99        COMP.
           03  WK-USTPZ                PIC 9V9(4)    COMP.
      *----------------------------------------------> f�r Aufteilung <-
           03  WH-AKAT                 PIC 9         COMP.
           03  WH-KAT                  PIC 9.
           03  WK-MCODE                PIC X(10).
           03  IY                      PIC 99        COMP.
           03  WK-BET                  PIC S9(9)V99   COMP.
           03  WH-ZAHLBET              PIC S9(9)V99   COMP.
           03  WK-REST                 PIC S9(9)V99   COMP.
           03  WH-REST                 PIC S9(9)V99   COMP.
           03  WH-FREST                PIC S9(9)V99   COMP.
           03  WD-KTO                  PIC ZZZZ9,9.
           03  WD-SALDO                PIC ZZZZZ.ZZ9,99-.
       COPY FIBUEXT.CPY.
       COPY FIBUDECL.CPY.
       END DECLARATIVES.
      ******************************************************************
       STEUER SECTION.
       A.  MOVE WL-CALL TO WH-CALL.
           MOVE WL-CREG TO WH-CREG.
           EVALUATE WL-CA
               WHEN 25 PERFORM AUS-ZIFF
               WHEN 35 PERFORM LOE-BUCH
               WHEN 50 PERFORM AUFTEIL
               WHEN 99 PERFORM HINWEISE.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Y.  MOVE WH-CREG TO WL-CREG.
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
      *****************************************************************
       DATDREH SECTION.
       A.  MOVE WC-TAG  TO WZ-TAG VDU-JAHR.
           MOVE WC-MONAT TO WZ-MONAT VDU-MONAT.
           MOVE WC-JAHR TO WZ-JAHR VDU-TAG.
       Z.  EXIT.
      *****************************************************************
       WASNUN SECTION.
       A.  IF WX = 0 PERFORM NO-REC
               SET ESC TO TRUE GO Z.
       B.  DISPLAY "Zeile+<ret>= Auswahl, < >= weiterlisten, < >= zur�ck
      -        "listen < >" AT 2301.
           IF FINE DISPLAY "***** keine Daten mehr *****"
               with highlight AT 2450.
           DISPLAY "<ret-leer>/<Esc>= Ende" AT 2401.
           CALL "CAUP" USING "0023611001" WH-CREG.
           MOVE 1 TO WR.
           IF WL-CA = 25 and RET or WOLI SET RET TO TRUE
                                         GO Z.
           IF ESC OR ENDE SET ESC TO TRUE MOVE 0 TO WH-NUM GO Z.
           IF WH-NUM > 0; IF WT-KEY(WH-NUM) = LOW-VALUE GO B ELSE GO Z.
           IF RET SET AB TO TRUE.
           IF FINE; IF AB  AND WI = 8 GO B
               ELSE IF AUF AND WI = 2 GO B.
           IF NOT AB AND NOT AUF GO A.
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
      *****************************************************************
       AUS-ZIFF SECTION.
       A.  CALL "CAUP" USING "0702101264010" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-REL.
           DISPLAY " Zahlungszuordnung " with highlight AT VDU-REL.
           ADD 202 VDU-ECK GIVING VDU-REL.
           DISPLAY " Zl Rechn.Nr.  Whg.       Betrag        Skonto
      -        "   Gesamt " with reverse-video AT VDU-REL.
           MOVE LOW-VALUE TO WT-ERFTAB.
           MOVE 0 TO WX WR WF WK-BET.
           MOVE 3 TO WI.
           MOVE BZ-KEY TO AZ-TKEY.
           MOVE 0 TO AZ-SA.
       C.  MOVE LOW-VALUES TO AZ-SATZ(11:).
           READ AUSZIFF INVALID PERFORM NO-REC GO Z.
           IF ZUGRIF PERFORM BESETZT GO C.
           MOVE 0 TO WL.
       E.  ADD 1 TO WL.
           IF WI < 3 PERFORM VARYING WF FROM 1 BY 1 UNTIL WF > 9
                        IF WL = WT-ZL(WF) GO E.
           IF WL > 50 OR (AZ-OPNR(WL) = 0 AND AZ-OPBET(WL) = 0)
                SET FINE TO TRUE MOVE 8 TO WI GO L.
           IF WI = 3 GO K.
           CALL "CAUP" USING "17SCROLLF" WH-CREG.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 8
               MOVE WT-ZL(WX + 1) TO WT-ZL(WX).
           MOVE 8 TO WX.
       K.  PERFORM AUSZ-DIS.
           IF WR NOT = 9 GO E.
           MOVE 9 TO WI.
       L.  PERFORM WASNUN.
           IF ESC or KIST or RET GO Z.
           IF AB GO E.
       N.  ADD -1 TO WL.
           IF WL = 0 SET FINE TO TRUE MOVE 2 TO WI GO L.
           IF WI > 7 PERFORM VARYING WF FROM 9 BY -1 UNTIL WF < 1
                        IF WL = WT-ZL(WF) GO N.
           CALL "CAUP" USING "22SCROLAB" WH-CREG.
           PERFORM VARYING WX FROM 8 BY -1 UNTIL WX < 1
              MOVE WT-ZL(WX) TO WT-ZL(WX + 1).
           MOVE 0 TO WX.
           PERFORM AUSZ-DIS.
           IF WR NOT = 9 GO N.
           MOVE 1 TO WI.
           GO L.
       Z.  EXIT.
      ******************************************************************
       AUSZ-DIS SECTION.
       A.  COMPUTE VDU-REL = WX * 100 + 303 + VDU-ECK.
           MOVE WL TO WD-POS.
           DISPLAY WD-POS AT VDU-REL.
           ADD 3 TO VDU-REL.
           MOVE AZ-OPNR(WL) TO WD-OPNR.
           DISPLAY WD-OPNR with highlight AT VDU-REL "  "
                AZ-OPFSYM(WL) with highlight foreground-color 6.
           ADD 14 TO VDU-REL.
           MOVE AZ-OPBET(WL) TO WD-BET.
           DISPLAY WD-BET with highlight AT VDU-REL.
           ADD 15 TO VDU-REL.
           MOVE AZ-OPSKTO(WL) TO WD-SKTO.
           DISPLAY WD-SKTO with highlight AT VDU-REL.
           ADD 14 TO VDU-REL.
           COMPUTE WD-BET = AZ-OPBET(WL) - AZ-OPSKTO(WL).
           DISPLAY WD-BET with highlight AT VDU-REL.
           ADD 1 TO WR WX.
           MOVE WL TO WT-ZL(WX).
           MOVE 0 TO WF-STATUS.
           COMPUTE WK-BET = WK-BET + AZ-OPBET(WL) - AZ-OPSKTO(WL).
           MOVE WK-BET TO WD-BET.
           DISPLAY WD-BET with highlight AT 2550 "  "
           MOVE BZ-BUBET TO WD-BET.
           DISPLAY WD-BET with highlight AT 0000.
       Z.  EXIT.
      ***************************************** l�schen Buchungszeilen *
       LOE-BUCH SECTION.
       A.  CALL "CAUP" USING "0704100360000" WH-CREG.
       C.  ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "Code: <     >, <esc>= Abbruch" AT VDU-LP.
           CALL "CAUP" USING "100210404" WH-CREG.
           IF ESC GO Z.
           IF EINF MOVE BZ-SEITE TO WH-NUM
                   SET RET TO TRUE.
           IF NOT RET GO C.
           IF WH-NUM NOT =  BZ-SEITE GO A.
           MOVE 1 TO KO-SPERR.
           REWRITE KO-SATZ.
           CALL "CAUP" USING "16CLRFEN" WH-CREG.
           ADD 208 VDU-ECK GIVING VDU-LP.
           DISPLAY "-- bitte warten --" with blink highlight AT VDU-LP.
       E.  DELETE BUCHZEIL INVALID SET ESC TO TRUE GO Z.
           IF ZUGRIF PERFORM BESETZT GO E.
           IF BZ-OPLFD NOT = 999 GO Z.
      *-----------------------------------> Zahlungszuordnung l�schen <-
           MOVE BZ-KEY TO AZ-TKEY.
           MOVE 0 TO AZ-SA.
       G.  READ AUSZIFF INVALID GO Z.
           IF ZUGRIF PERFORM BESETZT GO G.
           MOVE BZ-KTONR TO OP-KTONR WD-KZ.
           MOVE WD-KZ TO OP-DK.
           MOVE BZ-BUDAT TO OP-VALDAT.
           MOVE BZ-OPLFD TO OP-LFD.
           START OPDATEI KEY = OP-KEY INVALID GO Z.

      *    03  OP-REF                  PIC 9(7)       COMP.
      *    03  OP-RENUM                PIC 9(7)       COMP.
      *    03  OP-REBETRAG             PIC S9(9)V99   COMP.

       Z.  EXIT.
      ******************************************* Aufteilungsbuchungen *
       AUFTEIL SECTION.
       A.  CALL "CAUP" USING "0702022133020" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Aufteilung " with highlight AT VDU-LP.
           ADD 206 VDU-ECK GIVING VDU-LP.
           DISPLAY "Konto" AT VDU-LP.
           ADD 17 TO VDU-LP.
           DISPLAY "Betrag" AT VDU-LP.
           MOVE WK-KTONR TO WD-KTO.
           MOVE BZ-BUBET TO WH-REST.
           IF BZ-VM not = 5 SUBTRACT BZ-MW FROM WH-REST.
           MOVE WH-REST TO WH-FREST.
           COMPUTE WH-REST ROUNDED = WH-REST * 100 / BZ-KURS.
       C.  MOVE WH-REST TO WD-SALDO.
           MOVE 0 TO IX.
           ADD 317 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-SALDO with highlight AT VDU-LP.
           INITIALIZE WE-RETAB.
       D.  MOVE 0 TO WK-BET WK-REST.
           PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 16
                or WE-REBET(IX) = 0 ADD WE-REBET(IX) TO WK-BET
                                    ADD WE-SKONTO(IX) TO WK-REST.
           COMPUTE VDU-LP = IX * 100 + 305 + VDU-ECK.
           DISPLAY " Rest    " AT VDU-LP.
           ADD 12 TO VDU-LP.
           SUBTRACT WK-BET FROM WH-REST GIVING WD-SALDO WH-ZAHLBET.
           COMPUTE WK-REST = WH-FREST - WK-REST.
           DISPLAY WD-SALDO with highlight AT VDU-LP.
           DISPLAY "<esc>= Abbruch, <Entf>= l�schen, <ret>= Gegenkto.,"
               AT 2301.
           IF WH-ZAHLBET = 0 DISPLAY "<Ende>= Aufteilungsende" AT 2401.
           COMPUTE VDU-LP = IX * 100 + 304.
           CALL "CAUP" USING "1000045108" WH-CREG.
           IF ENTF; IF IX > 1 ADD -1 TO IX
                    CALL "CAUP" USING "1300000030" WH-CREG
                    ADD -1 TO VDU-L
                    MOVE 0 TO WE-REF(IX) WE-REBET(IX)
                              WE-SKONTO(IX) WE-UKZ(IX)
                    CALL "CAUP" USING "1300000030" WH-CREG GO D.
           IF ENDE; IF WH-ZAHLBET = 0 MOVE 4 TO BZ-Z
                    GO W.
           IF IX > 16 DISPLAY "max. 16 Aufteilungen m�glich" AT 2401
               PERFORM WEITER
               CALL "CAUP" USING "1300000017" WH-CREG GO D.
           IF ESC GO Z.
           CALL "CAUP" USING "1300000017" WH-CREG.
           IF NOT RET GO D.
           IF WH-WERT < 0,4 OR WH-NUM < 4
               CALL "FIBANZ" USING "99SUCH" WH-CREG
               CANCEL "FIBANZ".
           IF WH-NUM = 0 GO D.
           PERFORM LIESKTO.
           IF FEHLER GO D.
           MOVE WH-NUM TO WE-REF(IX).
260208     IF WH-KAT = 0 MOVE SA-VSTKZ TO WE-UKZ(IX).
           DIVIDE 10 INTO WH-NUM GIVING WD-KTO.
           DISPLAY WD-KTO with highlight AT VDU-LP.
           PERFORM GGKTO-ANZ.
           IF ESC GO D.
       G.  DISPLAY "<esc>= Abbruch, <Entf>= Wiederholung, <ret>= Betrag,
      -        " <ret-leer>= Restbetrag" AT 2301.
           MOVE WH-ZAHLBET TO WH-WERT.
           COMPUTE VDU-LP = IX * 100 + 317.
           MOVE VDU-REL TO VDU-ECK.
           CALL "CAUP" USING "1000008210" WH-CREG.
           IF IY = 1 CALL "CAUP" USING "08CLOFEN" WH-CREG
               MOVE 0 TO IY.
           IF ESC GO Z.
           IF ENTF CALL "CAUP" USING "16CLRFEN" WH-CREG GO C.
           IF WOLI GO D.
           IF NOT RET GO G.
           MOVE WH-WERT TO WD-SALDO WE-REBET(IX).
           IF WH-WERT = WH-ZAHLBET MOVE WK-REST TO WE-SKONTO(IX)
           else COMPUTE WE-SKONTO(IX) = WH-WERT * BZ-KURS / 100.
           COMPUTE VDU-LP = IX * 100 + 317 + VDU-ECK.
           DISPLAY WD-SALDO with highlight AT VDU-LP.
           ADD 1 TO IX.
           GO D.
      *----------------------------------------------> Ust-Aufteilung <-
       W.  COMPUTE WK-USTPZ = BZ-MW / WK-BET.                 *> %-Satz
           MOVE 0 TO WH-ZAHLBET.
           PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 16
              IF WE-REF(IX) not = 0 COMPUTE WE-SKONTO(IX) rounded
                                    = WE-REBET(IX) * WK-USTPZ
                  ADD WE-SKONTO(IX) TO WH-ZAHLBET
                  IF WE-UKZ(IX) not = 99 MOVE IX TO WH-HILF.
           MOVE WH-HILF TO IX.
           IF WH-ZAHLBET not = BZ-MW SUBTRACT BZ-MW FROM WH-ZAHLBET
               SUBTRACT WH-ZAHLBET FROM WE-SKONTO(IX).
           MOVE 0 TO WH-ZAHLBET.
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
      *        WHEN 1 SET ESC TO TRUE GO X
               WHEN 1
               WHEN 0 MOVE WH-NUM TO SA-KTONR
               WHEN OTHER GO X.
      *---------------------------------------------> lesen Sachkonto <-
       C.  READ SACHBUCH NOT INVALID MOVE SA-BEZ TO WT-TX
               MOVE SA-BTNT TO WH-BN
               MOVE SA-MCODE TO WK-MCODE
               MOVE SA-SOLL TO WE-SOLL
               MOVE SA-HABEN TO WE-HABEN
               MOVE SA-KTONR TO WH-NUM
               ADD WE-SOLL WE-HABEN GIVING WE-SALDO
               GO Z.
           IF ZUGRIF PERFORM BESETZT GO X.
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
           IF ZUGRIF PERFORM BESETZT GO X.
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
           IF ZUGRIF PERFORM BESETZT GO X.
           CALL "FIBKRED" USING "41FIBKRED" WH-CREG.
           CANCEL "FIBKRED".
           IF KR-KTONR NOT = 0 GO G.
       X.  SET FEHLER TO TRUE.
       Z.  EXIT.
      ******************************************** Gegenkonto anzeigen *
       GGKTO-ANZ SECTION.
       A.  MOVE VDU-ECK TO VDU-REL.
           IF IX > 12 CALL "CAUP" USING "0703050372000" WH-CREG
           else CALL "CAUP" USING "0719050372000" WH-CREG.
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
           MOVE WK-KTONR TO WH-NUM.
           PERFORM LIESKTO.
           IF FEHLER CALL "CAUP" USING "08CLOFEN" WH-CREG
                PERFORM BESETZT GO Z.
           MOVE 1 TO IY.
       Z.  EXIT.
      ********************************************* Bedienungshinweise *
       HINWEISE SECTION.
       A.  MOVE WL-KO TO IX.
           MOVE 7 TO WL-CA.
           MOVE 0 TO WL-KO WL-MA.
           CALL "CAUP" USING WH-CALL WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Programmhinweis " AT VDU-LP.
           ADD 303 VDU-ECK GIVING VDU-LP.
           EVALUATE IX
               WHEN 1 PERFORM FENSTER-1
               WHEN 2 PERFORM FENSTER-2
               WHEN 3 PERFORM FENSTER-3
               WHEN 4 PERFORM FENSTER-4
               WHEN 5 PERFORM FENSTER-5.
       Q.  DISPLAY "Weiter mit beliebiger Taste! < >" AT 2301.
           CALL "CAUP" USING "0023310000" WH-CREG.
       Z.  EXIT.
      ******************************************************************
       FENSTER-1 SECTION.
       A.  DISPLAY "In diesem Fenster sind alle f�r die Umsatzsteuerbere
      -        "chnung" with highlight AT VDU-LP.
           ADD 100 TO VDU-LP.
           DISPLAY "relevanten Konten aus dem Sachkontenbereich anzulege
      -        "n." with highlight AT VDU-LP.
           ADD 100 TO VDU-LP.
           DISPLAY "Die au�erb�cherlichen Konten 9991,1 etc. k�nnen f�r
      -        "Ver-" with highlight AT VDU-LP.
           ADD 100 TO VDU-LP.
           DISPLAY "gleichszwecke ebenfalls angelegt werden."
                with highlight AT VDU-LP.
           ADD 100 TO VDU-LP.
           DISPLAY "Nicht erforderlich sind die bereits 'zwingenden ange
      -        "legten" with highlight AT VDU-LP.
           ADD 100 TO VDU-LP.
           DISPLAY "Konten' (Vor-, Mehrwertsteuer u. Skontoaufwandskonte
      -        "n)." with highlight AT VDU-LP.
           ADD 100 TO VDU-LP.
           DISPLAY "Das Auswertungsprogramm finden Sie unter Pg. 7/10"
               with highlight AT VDU-LP.
       Z.  EXIT.
      ******************************************************************
       FENSTER-2 SECTION.
       A.  DISPLAY "Das Tagesdatum (= Buchungsdatum) bezieht sich auf ei
      -        "n Buchungs-" with highlight AT VDU-LP.
           ADD 100 TO VDU-LP.
           DISPLAY "jahr, da� in den Stammdaten Pkt. 10 - Wirtschaftsjah
      -        "re noch" with highlight AT VDU-LP.
           ADD 100 TO VDU-LP.
           DISPLAY "nicht angelegt ist!" with highlight AT VDU-LP.
       Z.  EXIT.
      ******************************************************************
       FENSTER-3 SECTION.
       A.  DISPLAY "In diesem Fenster sind alle f�r die Getr�nkeabgabebe
      -        "-    " with highlight AT VDU-LP.
           ADD 100 TO VDU-LP.
           DISPLAY "rechnung relevanten Konten aus dem Sachkontenbereich
      -        "" with highlight AT VDU-LP.
           ADD 100 TO VDU-LP.
           DISPLAY "anzulegen. In der %-Satzspalte sind die %-S�tze f�r
      -        "die " with highlight AT VDU-LP.
           ADD 100 TO VDU-LP.
           DISPLAY "Ermittlung der Getr�nkeabge festzulegen."
               with highlight AT VDU-LP.
           ADD 100 TO VDU-LP.
           DISPLAY "Das Auswertungsprogramm finden Sie unter Pg. 7/10"
               with highlight AT VDU-LP.
       Z.  EXIT.
      ******************************************************************
       FENSTER-4 SECTION.
       A.  DISPLAY "Das Tagesdatum (= Buchungsdatum) bezieht sich auf ei
      -        "n Buchungs-" with highlight AT VDU-LP.
           ADD 100 TO VDU-LP.
           DISPLAY "jahr, da� in den Stammdaten Pkt. 10 - Wirtschaftsjah
      -        "re nicht" with highlight AT VDU-LP.
           ADD 100 TO VDU-LP.
           DISPLAY "das aktuelle Wirtschaftsjahr ist."
               with highlight AT VDU-LP.
       Z.  EXIT.
      ******************************************************************
       FENSTER-5 SECTION.
       A.  DISPLAY "Das Tagesdatum (= Buchungsdatum) bezieht sich auf ei
      -        "n Buchungs-" with highlight AT VDU-LP.
           ADD 100 TO VDU-LP.
           DISPLAY "jahr, da� in den Stammdaten Pkt. 10 - Wirtschaftsjah
      -        "re bereits" with highlight AT VDU-LP.
           ADD 100 TO VDU-LP.
           DISPLAY "abgeschlossen ist." with highlight AT VDU-LP.
       Z.  EXIT.
