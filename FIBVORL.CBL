      $SET LINKCOUNT"340" ANS85 BOUND AUTOLOCK NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     FIBVORL.
      ******************************************************************
       ENVIRONMENT    DIVISION.
       CONFIGURATION   SECTION.
       SOURCE-COMPUTER.     pc.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA
                         ENVIRONMENT-NAME IS ENV-SET
                         ENVIRONMENT-VALUE IS ENV-WERT.
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
       01  WF-REG.
           03  WK-DSTG.
               05 WK-STG               PIC X(10)     OCCURS 10.
           03  WH-P1                   PIC 99       COMP.
           03  WH-P2                   PIC 99       COMP.
           03  WZ-SCHALT               PIC 99       COMP-3 VALUE ZERO.
           03  WZ-ZEILEN               PIC 99       COMP-3 VALUE ZERO.
           03  WZ-SEITE                PIC 99       COMP-3 VALUE ZERO.
           03  WM-OPEN                 PIC 9        COMP-3 VALUE ZERO.
           03  WH-SEC                  PIC 9(14)    COMP-X.
           03  WE-EXTN                 PIC X(5).
           03  WE-EXTV.
               05 WR-VAL               PIC 9.
               05 WR-KUEB              PIC 9.
           03  WA-TAGE                 PIC 9(7)      COMP.
           03  WI                      PIC 99        COMP-5.
           03  WO-SYMT                 PIC 9(4).
           03  WO-Y REDEFINES WO-SYMT.
               05 WO-SH                PIC 9.
               05 WO-UST               PIC 9.
               05 WO-OP                PIC 9.
               05 WO-DIV               PIC 9.
       COPY FIBUEXT.CPY.
       COPY FIBUDECL.CPY.
       END DECLARATIVES.
      ******************************************************************
       STEUER SECTION.
       A.  MOVE WL-CALL TO WH-CALL.
           MOVE WL-CREG TO WH-CREG.
           PERFORM VORLAUF.
           MOVE WH-CREG TO WL-CREG.
       Z.  EXIT PROGRAM.
      *****************************************************************
       VORLAUF SECTION.
       A.  MOVE "  *- F I B U 15.02 -*" TO WK-GEB.
           PERFORM LADE-DRU.
           IF WH-PG = 9; IF WN-KOST not = "*ANL*" GO B
                    else MOVE 0 TO WH-PG GO S.
           MOVE "FIBNR" TO WE-EXTN.
           CALL "_EXTNAME" USING WE-EXTN WE-EXTV.
           IF WR-VAL(1:) = SPACE MOVE 1 TO WR-VAL.         *> Startuser
           IF WR-KUEB(1:) = SPACE MOVE 7 TO WR-KUEB.       *> Kassauser
           IF WR-KUEB = 0 MOVE 7 TO WR-KUEB.
           IF WK-BS(1:1) = " " MOVE 0 TO WK-BS(1:1).
           MOVE WR-VAL TO WE-USE.
           MOVE WR-KUEB TO WE-KUEB.
      *    MOVE WK-BS(1:1) TO WR-VAL.
           COMPUTE WK-BS = WR-VAL * 10 + WE-USE.
           MOVE "-" TO WK-FREI.
           MOVE WR-KUEB TO WK-FREI(3:)
           MOVE "FIBUKONS.DAT" TO WN-KONS.
           MOVE "FIBUOPES.DAT" TO WN-OPES.
           MOVE "FIBUDEBI.DAT" TO WN-DEBI.
           MOVE "FIBUKRED.DAT" TO WN-KRED.
           MOVE "FIBUSACH.DAT" TO WN-SACH.
           MOVE "FIBUZEIL.DAT" TO WN-BZEI.
           MOVE "FIBUAUSZ.DAT" TO WN-AUSZ.
           MOVE "FIBUBANK.DAT" TO WN-BANK.
           IF WE-USE > 7 DISPLAY "User zu gro·" with highlight BLINK
                AT 0533 STOP RUN.
      *    CALL "FIBUKER" USING "98OPCONV" WH-CREG.
      *    CANCEL "FIBUKER".
           CALL "FIBUKER" USING "99CONV" WH-CREG.
           CANCEL "FIBUKER".
           OPEN I-O DEBITOR.
           OPEN I-O KREDIT.
           OPEN I-O SACHBUCH.
           OPEN I-O KONSTANT.
           OPEN I-O OPDATEI.
           OPEN I-O BUCHZEIL.
           OPEN I-O AUSZIFF.
           PERFORM FIBUEB.
           MOVE SPACE TO WX-CODE(1) WX-CODE(2).
       B.  MOVE 1 TO WH-KEY.
           READ KONSTANT IGNORE LOCK not INVALID GO J.
           CALL "CAUP" USING "06KOPF" WH-CREG.
           DISPLAY "bitte Kopf anlegen -" AT 2401.
           INITIALIZE KO-SATZ.
           MOVE 1 TO KO-NUM.
       C.  ACCEPT KO-FIRMA WITH HIGHLIGHT AT 0127.
           IF KO-FIRMA = SPACE GO C.
           DISPLAY KO-FIRMA WITH HIGHLIGHT AT 0127.
           MOVE "jowe" TO KO-SPERRE.
           MOVE 20 TO KO-UST(1).
           MOVE 10 TO KO-UST(2).
           MOVE 30 TO KO-UST(3).
           MOVE 0 TO KO-UST(4) KO-UST(5) KO-UST(6).
           MOVE "Firma    Familie  Herrn    Frau     An das   An die
      -        "An" TO KO-ANR.
           INITIALIZE WH-TABK.
           MOVE 1001400000030 TO WT-KONDIT(1).
           MOVE 2001400000030 TO WT-KONDIT(2).
           MOVE 3001400000030 TO WT-KONDIT(3).
           MOVE 4001400000030 TO WT-KONDIT(4).
           MOVE 5001400000030 TO WT-KONDIT(5).
           MOVE WH-TABK TO KO-TABK.
           WRITE KO-SATZ.
           MOVE 10 TO WH-KEY.
           INITIALIZE KO-MSATZ.
           MOVE WH-KEY TO KO-NUM.
           WRITE KO-SATZ.
           GO B.
       J.  MOVE KO-FIRMA TO WK-FIRMA.
           MOVE KO-SPERRE TO WX-CODE(2).
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 6
               MOVE KO-UST(WX) TO WT-UST(WX).
           MOVE KO-ANR TO WH-TABTX.
           MOVE KO-TABK TO WH-TABK.
           MOVE KO-MERK TO WK-M.
           MOVE KO-KST TO WE-KST.
           MOVE WH-DATUM TO WC-DATUM.
           CALL "CAUP" USING "04DATDREH" WH-CREG.
           MOVE WX-DATUM TO WM-DATUM.
       K.  MOVE 9 TO WH-KEY.
           READ KONSTANT IGNORE LOCK INVALID MOVE 0 TO KO-WBEG(1).
           MOVE KO-DRU TO WM-DRU.                *> Nadel- od. Laserdr.
           CALL "CAUP" USING "06KOPF" WH-CREG.
           MOVE KO-EURO TO WE-EURO.
           MOVE 0 TO WY WE-WKZ WE-WJAHR(1) WE-WJAHR(2) WE-WJAHR(3).
           PERFORM VARYING WX FROM 4 BY -1 UNTIL WX = 0 or
               KO-WBEG(WX) NOT = 0 CONTINUE.
           IF WX = 0 GO L.
           MOVE KO-WBEG(WX) TO WE-WJAHR(1).
           MOVE KO-WEND(WX) TO WE-WJAHR(2).
           COMPUTE WA-TAGE = FUNCTION INTEGER-OF-DATE(WM-DATUM).
           ADD 90 TO WA-TAGE.
           COMPUTE WS-DATUM = FUNCTION DATE-OF-INTEGER(WA-TAGE).
           IF WE-WJAHR(1) > WS-DATUM
               CALL "FIBZUS" USING "9906060767040" WH-CREG
               CANCEL "FIBZUS".
           PERFORM VARYING WX FROM 4 BY -1 UNTIL WX = 0
               IF KO-WBEG(WX) NOT = 0;
                  IF WM-DATUM >= KO-WBEG(WX) AND WM-DATUM <= KO-WEND(WX)
                      MOVE WX TO WY.
           IF WY NOT = 0
               ADD KO-WKZ(WY) 50 GIVING WF           *> wegen Jahr 2000
               MOVE WF TO WE-WKZ
               MOVE KO-WBEG(WY) TO WE-WJAHR(1)
               MOVE KO-WEND(WY) TO WE-WJAHR(2).
           MOVE KO-ABDAT TO WE-WJAHR(3).
           IF WE-WJAHR(1) < KO-ABDAT
                   CALL "FIBZUS" USING "9906060767050" WH-CREG
                   CANCEL "FIBZUS".
       L.  MOVE 0 TO WS-DATUM.
           CALL "CAUP" USING "06KOPF" WH-CREG.
           IF WE-WJAHR(1) = 0 OR WE-WJAHR(2) = 0 OR KO-ABDAT(1:2) = " "
               CALL "FIBZUS" USING "9906060767020" WH-CREG
               CANCEL "FIBZUS".
       N.  MOVE 10 TO WH-KEY.
           READ KONSTANT IGNORE LOCK INVALID GO P.
           IF KOM-NEU not = "v2!" PERFORM NEUSYM.
           MOVE KO-TS TO WE-TS.
           IF WH-MCODE = "*ANL*" MOVE "FIBSTAM 60STAM" TO WT-TX
               MOVE WH-MCODE TO WN-KOST
               MOVE SPACE TO WH-MCODE
               GO Z.
       P.  IF WX-CODE(2) = WX-CODE(1) GO Q.
           DISPLAY "<esc>= Abbruch" AT 2401.
           MOVE 99 TO WH-PG.
           DISPLAY "Codeworteingabe: " AT 2301.
           MOVE SPACE TO WT-TX.
           CALL "CAUP" USING "0223180106" WH-CREG.
           MOVE WH-NUM TO WH-PG.
           IF ESC MOVE 0 TO WH-PG GO R.
           MOVE WT-TX TO WX-CODE(1).
           GO P.
       Q.  DISPLAY "<esc/ret-leer>= Programmende" AT 2301.
           DISPLAY "Hauptprogrammwahl"    AT 0524
                   "ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ"    AT 0024
                   "1 - Buchen"           AT 0024
                   "2 - Mahnung / Zinsen" AT 0024
                   "3 - Zahlungsverkehr"  AT 0024
                   "4 - öbernahme"        AT 0024
                   "5 - Druckprogramm"    AT 0024
                   "6 - Stammdaten"       AT 0024
                   "7 - Auswertungen"     AT 0024
                   "8 - DatumsÑnderung"   AT 0024
                   "9 - Userwechsel"      AT 0024.
           IF WE-KST = 0 DISPLAY " " AT 0024
                    else DISPLAY "10 - KER-Auswertung" at 0023.
           DISPLAY " bitte wÑhlen Sie:   " with highlight AT 1839.
           CALL "CAUP" USING "0018582002" WH-CREG.
       R.  IF ESC MOVE 0 TO WH-NUM
                  SET RET TO TRUE.
           MOVE WH-NUM TO WH-PG.
      *    IF SEPO MOVE "FIBLIS  20EIN" TO WT-TX
      *            GO Z.                             *> Op-Konvertg.
      *    IF SAPO MOVE "FIBUKER 98KER" TO WT-TX
      *            GO Z.                             *> Op-Konvertg.
           IF SENT PERFORM TESTI.                    *> Test Kopffarben
           IF KIST and WH-PG = 93 GO S.
           IF WE-KST = 1; IF WH-PG > 9 GO S.
           IF WH-PG = 90 and EINF GO S.
           IF RET and WH-PG > 9 GO Q.
           IF SAPO SET RET TO TRUE
                   MOVE 86 TO WH-PG.
           IF SEPO SET RET TO TRUE
                   MOVE 87 TO WH-PG.
           IF BAUF SET RET TO TRUE
                   MOVE 88 TO WH-PG.
           IF BAB SET RET TO TRUE
                  MOVE 89 TO WH-PG.
           IF NOT RET GO Q.
       S.  EVALUATE WH-PG
               WHEN 0 CALL "CAUP" USING "1301012480000" WH-CREG
                 CLOSE DEBITOR KREDIT SACHBUCH KONSTANT BUCHZEIL OPDATEI
                 DISPLAY " Programm beendet " AT 1231
                 STOP RUN
               WHEN 1 MOVE "FIBBUCH 10BUCH" TO WT-TX
               WHEN 2 MOVE "FIBMAHN 10MAHN" TO WT-TX
               WHEN 3 MOVE "FIBZAHL 30ZAHL" TO WT-TX
               WHEN 4 MOVE "FIBJEND 50UEBE" TO WT-TX
               WHEN 5 MOVE "CADRUCK 40DRUK" TO WT-TX
               WHEN 6 MOVE "FIBSTAM 60STAM" TO WT-TX
               WHEN 7 MOVE "FIBLIST 70LIST" TO WT-TX
               WHEN 86 CALL "FIBLI" USING "50BANKVG" WH-CREG
               WHEN 87 CALL "FIBLI" USING "30OPVERG" WH-CREG
               WHEN 88 CALL "FIBLI" USING "10ARLI  " WH-CREG
               WHEN 89 CALL "FIBLI" USING "20EINLAG" WH-CREG
               WHEN 9 PERFORM WECHSEL
               WHEN 10 MOVE "FIBUKER 10KER" TO WT-TX             *> KER
               WHEN 90 MOVE "FIBSOFT 90UEB" TO WT-TX
               WHEN 93 CALL "FIBLI" USING "40FINANZ" WH-CREG
               WHEN 94 MOVE "FIBREG  60KON" TO WT-TX
               WHEN 95 MOVE "FIBKONT 95RNR" TO WT-TX
               WHEN 96 MOVE "FIBKONT 96JOU" TO WT-TX
               WHEN 97 MOVE "FIBUEB  97UEB" TO WT-TX
               WHEN 98 MOVE "FIBREG  98REG" TO WT-TX
               WHEN 99 MOVE "FIBOPSU 98REG" TO WT-TX
               WHEN 8 MOVE 0 TO WZ-DATUM WH-PG
                      CALL "CAUP" USING "03DATUM" WH-CREG GO B.
       Z.  EXIT.
      ******************************************************************
       NEUSYM SECTION.
       A.  IF KOM-NEU = "nUe" GO C.
           MOVE KO-TS TO WX-TS.
           PERFORM VARYING WX FROM 21 BY 1 UNTIL WX > 30
               MOVE SPACE TO KO-SYM(WX)
               MOVE 0 TO KO-SKZ(WX).
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 20
               MOVE WY-SY(WX) TO KO-SYM(WX)
               MOVE WY-SZ(WX) TO KO-SKZ(WX).
           MOVE LOW-VALUES TO KO-TSR.
           MOVE "nUe" TO KOM-NEU.
       C.  PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 30
               MOVE KO-SKZ(WX) TO WO-SYMT
               MOVE WO-SH TO WE-SH
               MOVE WO-UST TO WE-UST
               MOVE WO-OP TO WE-OP
               MOVE WO-DIV TO WE-DIV
               MOVE WE-SYMT TO KO-SKZ(WX).
           MOVE "v2!" TO KOM-NEU.
           REWRITE KO-SATZ.
       Z.  EXIT.
      ******************************************************************
       TESTI SECTION.
       A.  MOVE "06 KOPF" TO WH-CALL.
           MOVE 00 TO WI.
       C.  ADD 1 TO WI.
           MOVE WI(1:) TO WH-CALL(3:1).
           CALL "CAUP" USING WH-CALL WH-CREG.
           DISPLAY "weiter" AT 2401 " " WI " ".
           CALL "CAUP" USING "0024000000" WH-CREG.
           IF ESC GO Z.
           GO C.
       Z.  EXIT.
      **************************************************** Userwechsel *
       WECHSEL SECTION.
       A.  CALL "CAUP" USING "0712320335000" WH-CREG.
           ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "Userwechsel von:    auf:" AT VDU-LP.
           ADD 17 TO VDU-LP.
           MOVE WE-USE TO WR-VAL WH-WERT.
           DISPLAY WR-VAL with highlight AT VDU-LP.
           ADD 8 TO VDU-P.
       C.  DISPLAY WR-VAL with highlight AT VDU-LP.
           CALL "CAUP" USING "1102281001" WH-CREG.
           IF NOT RET GO C.
           IF WH-WERT < 1 OR > 7 GO C.
           MOVE WK-BS(1:1) TO WR-VAL.
           MOVE WH-WERT TO WE-USE.
           COMPUTE WK-BS = WR-VAL * 10 + WE-USE.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           MOVE "FIBBUCH 10BUCH" TO WT-TX.
       Z.  EXIT.
      ******************************************************************
       FIBUEB SECTION.
      *---------------------------------------> Pfad fÅr FIBUEBER.DAT <-
       A.  MOVE 11 TO WH-KEY.
           READ KONSTANT IGNORE LOCK INVALID GO X.
           IF KOD-VERZ(2:1) not = ":" and KOD-VERZ(1:1) not = "\" GO X.
           MOVE KOD-VERZ TO WN-BUEB.
           PERFORM VARYING WX FROM 30 BY -1 UNTIL WX = 0
               OR WN-BUEB(WX:1) = "\" CONTINUE.
           ADD 1 TO WX.
      *    MOVE "FIBUEBER.DAT" TO WN-BUEB(WX:).
           GO Z.
       X.  DISPLAY "Stammdaten Pkt. 16 nicht angelegt" AT 2401.
           PERFORM WEITER.
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
      ******************************** Parameter fÅr Laserdrucker PCL5 *
       LADE-DRU SECTION.
      *-----------------------------------> Steuerzeichen LASER laden <-
      *                                  11  *> quer A4 /10" 6 Zeilen <-
       A.  MOVE "&l26a6d1O(s0p10h0b0s3T" TO WE-STG(1).
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
