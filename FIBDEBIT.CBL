      $SET LINKCOUNT"240" ANS85 BOUND AUTOLOCK NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     FIBDEBIT.
      ******************************************************************
      *              Anlage und énderung Kundenkonten                  *
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
           SELECT BZLTAB   ASSIGN TO PRINTER "BZLBIC.DAT"
                           ORGANISATION LINE SEQUENTIAL
                           FILE STATUS WF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       COPY FIBUFD.CPY.
      ********************************************* BZL-Liste (STUZZA) *
       FD  BZLTAB                      LABEL RECORD STANDARD.
       01  BL-SATZ                     PIC X(128).
      ******************************************************************
       FD  DRUCKER                     LABEL RECORD STANDARD.
       01  DRA-SATZ.
           03  FILLER                  PIC XXXX.
           03  DRA-STR                 PIC X(135).
      ******************************************************************
       WORKING-STORAGE SECTION.
       01  WH-CALL.
           03  WL-CA                   PIC 99.
           03  WL-REST                 PIC 9(13).
       COPY WHCREG.CPY.
       01  WF-REG.
           03  WB-BZL                  PIC ZZ999.
           03  WB-KEN                  PIC X(15).
           03  WB-BIC                  PIC X(15).
           03  WB-NAME                 PIC X(50).
           03  WT-IBAN                 PIC X(24).
           03  IX                      PIC 9(4)    COMP.
           03  WM-IBA                  PIC 9(13).        *> IBAN-Rechng
           03  WM-IBB                  PIC 9(13).        *> IBAN-Rechng
           03  WM-IBR                  PIC 99.
           03  WM-BANKLTZ              PIC X(6).
           03  WM-IBAN                 PIC X(24).
           03  WM-IB                   PIC 9.
           03  WN-BANKLTZ              PIC X(6).
           03  WN-IBAN                 PIC X(24).
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
           03  WD-SEITE                PIC ZZ9.
           03  WD-KTO                  PIC ZZ.ZZ9,9.
      *---------------------------------------> Adapt.Feld aus FIBU <--
           03  WH-KETTD                PIC 9(5)    VALUE ZERO.
           03  RED REDEFINES WH-KETTD.
               05  WH-ANREDE           PIC 9.
               05  WH-STATIS           PIC 9.
               05  WH-FAKART           PIC 9.
               05  WH-TOUR             PIC 99.
           03  WH-DEKEY                PIC X(6).
           03  WH-BKEY                 PIC 9(6).
           03  WH-FREC                 PIC 9(5).
           03  WK-ANR                  PIC 9.
      *----------------------------------------> Konditionentabelle <--
           03  WH-KK                   PIC 9(13).
           03  WR-KK REDEFINES WH-KK.
               05  WH-SK1              PIC 9V9.
               05  WH-TG1              PIC 999.
               05  WH-SK2              PIC 9V9.
               05  WH-TG2              PIC 999.
               05  WH-TGN              PIC 999.
           03  WD-TG                   PIC ZZ9.
           03  WD-PZ                   PIC 9,9.
           03  WD-OP                   PIC Z9.
           03  WD-KZ                   PIC 9.
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
               WHEN 10 PERFORM IBAN-BIC
               WHEN 20 PERFORM AUTOIBAN
               WHEN 30
               WHEN 31
               WHEN 32 PERFORM DEBIT.
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
           READ KONSTANT IGNORE LOCK NOT INVALID GO X.
       X.  MOVE KOP-TX TO WK-DSTG.
       Z.  EXIT.
      *************************************** Adresszerlegung f. Druck *
       HOLADR SECTION.
       A.  ADD 415 VDU-ECK GIVING VDU-LP.
           MOVE DE-BEZ TO WT-BEZ.
           MOVE SPACE TO WT-ADR.
           MOVE "#" TO WR-BEZ(131).
           UNSTRING WT-BEZ DELIMITED BY "#" INTO WR-ADR(1) WR-ADR(2)
               WR-ADR(3) WR-ADR(4) WR-ADR(5) WR-ADR(6).
           PERFORM VARYING WR FROM 1 BY 1 UNTIL WR > 5
               DISPLAY WR-ADR(WR) with SIZE 34 highlight AT VDU-LP
               ADD 100 TO VDU-LP.
       Z.  EXIT.
      ******************************************************************
       DEBIT SECTION.
       A.  CALL "CAUP" USING "0711011080000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Verwaltung Debitoren " with highlight AT VDU-LP.
       C.  ADD 202 VDU-ECK GIVING VDU-LP.
           DISPLAY "Nr.:" AT VDU-LP.
           IF WL-CA = 31 or 32 GO P.
           DISPLAY "<ret>= Anl./énd., alpha+<ret>= suchen, <tab>= nÑchts
      -        "e freie Kd.Nr., <esc>= Ende" AT 2301.
           DISPLAY "< >= Stammdatendruck" AT 2401.
           CALL "CAUP" USING "1002086006" WH-CREG.
           ADD 208 VDU-ECK GIVING VDU-LP.
           DISPLAY "      " AT VDU-LP.
           IF HELP CALL "CAUP" USING "190009001001" WH-CREG GO C.
           IF ESC MOVE "FIBSTAM 60STAM" TO WT-TX GO X.
           IF AB PERFORM STAMDRUCK GO C.
           IF NOT RET GO C.
           IF RET GO M.
           MOVE HIGH-VALUE TO DE-KEY.
           START DEBITOR KEY < DE-KEY INVALID MOVE 2 TO DE-KTONR GO G.
       F.  READ DEBITOR PREVIOUS NO LOCK AT END MOVE 2 TO DE-KTONR.
           IF ZUGRIF PERFORM BESETZT GO F.
       G.  COMPUTE WH-WERT = (DE-KTONR - 2) / 10.
           IF WH-WERT = 99999
               DISPLAY "hîchste mîgliche Nr. schon vergeben" AT 2401
               GO C.
           ADD 1 WH-WERT GIVING WH-NUM.
       M.  MOVE 0 TO WM-MERKER.
           IF WH-NUM = 0 MOVE 2 TO WH-NUM
               CALL "FIBANZ" USING "30DEBSUCH" WH-CREG
               CANCEL "FIBANZ".
           IF WH-NUM = 0 GO C.
           MOVE WH-NUM TO DE-KTONR WD-KZ.
           IF WD-KZ NOT = 2 GO C.
       P.  READ DEBITOR INVALID MOVE 1 TO WM-MERKER.
           IF ZUGRIF PERFORM BESETZT PERFORM WEITER GO C.
           MOVE SPACE TO BK-SATZ.
           IF DE-OPKZ not = 20 GO R.
      *--------------------------------------> zusÑtzliche Bank lesen <-
           MOVE DE-KTONR TO BK-KTONR.
           MOVE 2 TO BK-DK.
           OPEN I-O BANKDAT.
       Q.  READ BANKDAT INVALID MOVE SPACE TO BK-SATZ.
           IF ZUGRIF PERFORM BESETZT GO C.
           CLOSE BANKDAT.
       R.  PERFORM DEB-ANL.
           UNLOCK DEBITOR.
           IF WL-CA = 31; IF ESC MOVE 0 TO DE-KTONR GO X ELSE GO X.
           IF WL-CA = 32 GO X.
           CALL "CAUP" USING "16LOEFEN" WH-CREG.
           GO C.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           UNLOCK DEBITOR.
       Z.  EXIT.
      ******************************************************************
       BEDMES SECTION.
       A.  DISPLAY "<esc>= Abbruch, <ret-leer>= keine énderung, <ende>=
      -        "Ende" AT 2301.
       Z.  EXIT.
      ******************************************************************
       DEB-ANL SECTION.
       A.  COMPUTE WH-WERT = DE-KTONR / 10.
           MOVE WH-WERT TO WD-KTO.
           ADD 214 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-KTO with highlight AT VDU-LP.
           ADD 202 VDU-ECK GIVING VDU-LP.
           DISPLAY "Konto-Nr.: " AT VDU-LP.
           ADD 302 VDU-ECK GIVING VDU-LP.
           DISPLAY "Anrede:" AT VDU-LP.
           ADD 402 VDU-ECK GIVING VDU-LP.
           DISPLAY "Bezeichnung:" AT VDU-LP.
           ADD 902 VDU-ECK GIVING VDU-LP.
           DISPLAY "Tel.Anm:" AT VDU-LP.
           ADD 223 VDU-ECK GIVING VDU-LP.
           DISPLAY "Kurzname:" AT VDU-LP.
           ADD 250 VDU-ECK GIVING VDU-LP.
           DISPLAY "Kontogruppe......:" AT VDU-LP.
           ADD 350 VDU-ECK GIVING VDU-LP.
           DISPLAY "WÑhrungs-Kz......:" AT VDU-LP.
           ADD 450 VDU-ECK GIVING VDU-LP.
           DISPLAY "OP-Verarb.Kz.....:" AT VDU-LP.
           ADD 550 VDU-ECK GIVING VDU-LP.
           DISPLAY "Mahnintervall.Tg.:" AT VDU-LP.
           ADD 650 VDU-ECK GIVING VDU-LP.
           DISPLAY "Zahlungskond.Kz..:" AT VDU-LP.
           IF ANLAGE GO B.
           ADD 311 VDU-ECK GIVING VDU-LP.
           MOVE DE-ANREDE TO WD-KZ.
           DISPLAY WD-KZ AT VDU-LP.
           ADD 315 VDU-ECK GIVING VDU-LP.
           DISPLAY WT-TXT(DE-ANREDE + 1) with highlight AT VDU-LP.
           PERFORM HOLADR.
           ADD 235 VDU-ECK GIVING VDU-LP.
           MOVE DE-MCODE TO WT-TX.
           CALL "CAUP" USING "21CONV08" WH-CREG.
           DISPLAY WT-TX WITH SIZE 8 HIGHLIGHT AT VDU-LP.
           IF ESC GO Z.
           ADD 270 VDU-ECK GIVING VDU-LP.
           MOVE DE-KTOART TO WD-KZ.
           DISPLAY WD-KZ with highlight AT VDU-LP.
           ADD 370 VDU-ECK GIVING VDU-LP.
           MOVE DE-FWKZ TO WD-KZ.
           DISPLAY WD-KZ with highlight AT VDU-LP.
           ADD 469 VDU-ECK GIVING VDU-LP.
           MOVE DE-OPKZ TO WD-OP.
           DISPLAY WD-OP with highlight AT VDU-LP "-".
           PERFORM OPZI.
           ADD 569 VDU-ECK GIVING VDU-LP.
           MOVE DE-MAHNT TO WD-TG.
           DISPLAY WD-TG with highlight AT VDU-LP.
           PERFORM KOND-ANZ.
           ADD 0911 VDU-ECK GIVING VDU-LP.
           INSPECT DE-TEL REPLACING ALL X"00" BY SPACE.
           DISPLAY DE-TEL with highlight AT VDU-LP.
           GO C.
       B.  MOVE DE-KEY TO WH-DEKEY.
           INITIALIZE DE-SATZ.
           MOVE WH-DEKEY TO DE-KEY.
           MOVE 15 TO DE-MAHNT.
       C.  MOVE DE-ANREDE TO WH-WERT.
       D.  DISPLAY "<esc>= Abbruch, <ret-leer>= keine énderung" AT 2301.
           MOVE 2401 TO VDU-LP.
           PERFORM VARYING WY FROM 1 BY 1 UNTIL WY > 7
               SUBTRACT 1 FROM WY GIVING WK-ANR
               DISPLAY WK-ANR with highlight AT VDU-LP ":" WT-TXT(WY)
               ADD 10 TO VDU-LP.
           CALL "CAUP" USING "1003111001" WH-CREG.
           IF ESC OR WOLI GO Z.
           IF NOT RET OR WH-WERT > 6 GO D.
           MOVE WH-WERT TO DE-ANREDE WD-KZ.
           DISPLAY WD-KZ AT VDU-LP.
           ADD 315 VDU-ECK GIVING VDU-LP.
           DISPLAY WT-TXT(DE-ANREDE + 1) with highlight AT VDU-LP.
       E.  PERFORM HOLADR.
       F.  PERFORM BEDMES.
           CALL "CAUP" USING "1204150534" WH-CREG.
           MOVE WT-BEZ TO DE-BEZ.
           IF WOLI GO C.
           IF ENDE GO W.
           IF ESC GO Z.
           IF WR-ADR(1) = SPACE
               DISPLAY "Bezeichnung muss sein!" AT 2401
               PERFORM WEITER GO F.
           IF HELP CALL "CAUP" USING "190011001201" WH-CREG GO F.
       G.  PERFORM BEDMES.
           ADD 235 VDU-ECK GIVING VDU-LP.
           MOVE DE-MCODE TO WT-TX.
           CALL "CAUP" USING "21CONV08" WH-CREG.
           DISPLAY WT-TX WITH SIZE 8 HIGHLIGHT AT VDU-LP.
           CALL "CAUP" USING "1202350108" WH-CREG.
           IF ENDE GO W.
           IF AUF OR WOLI GO E.
           IF ESC GO Z.
           IF NOT RET GO G.
           CALL "CAUP" USING "20CONV08" WH-CREG.
           MOVE WT-TX TO DE-MCODE.
           CALL "CAUP" USING "21CONV08" WH-CREG.
           DISPLAY WT-TX(1:9) with highlight AT VDU-LP.
       H.  MOVE DE-KTOART TO WH-WERT.
           PERFORM BEDMES.
           DISPLAY "max. 3 Anzahl Deb.-Sammelkonten" AT 2401.
           CALL "CAUP" USING "1002701001" WH-CREG.
           IF ENDE GO W.
           IF WOLI GO G.
           IF ESC GO Z.
           IF NOT RET GO H.
           IF WH-WERT > 2 GO H.
           MOVE WH-WERT TO DE-KTOART WD-KZ.
           DISPLAY WD-KZ with highlight AT VDU-LP.
       I.  MOVE DE-FWKZ TO WH-WERT.
           PERFORM BEDMES.
           CALL "CAUP" USING "1003701001" WH-CREG.
           IF ENDE GO W.
           IF ESC GO Z.
           IF WOLI GO H.
           IF NOT RET GO I.
           MOVE WH-WERT TO DE-FWKZ WD-KZ.
           DISPLAY WD-KZ with highlight AT VDU-LP.
       J.  MOVE DE-OPKZ TO WH-WERT.
           PERFORM BEDMES.
           DISPLAY "0= mit, 1= ohne OP's, + 10= mit Zinsen, 20= Einzug,
      -        "30= Ig-Erwerb" AT 2401.
           CALL "CAUP" USING "1004692002" WH-CREG.
           IF ENDE GO W.
           IF ESC GO Z.
           IF WOLI GO I.
           IF NOT RET GO J.
           EVALUATE WH-WERT
               WHEN 0
               WHEN 1
               WHEN 10
               WHEN 11
               WHEN 20 MOVE WH-WERT TO DE-OPKZ WD-OP
               WHEN 30 MOVE WH-WERT TO DE-OPKZ WD-OP
               WHEN OTHER GO J.
           DISPLAY WD-OP with highlight AT VDU-LP "-".
           PERFORM OPZI.
           IF DE-OPKZ = 20 PERFORM IBAN-BIC
               IF BK-BANKKTO = SPACE and BK-BANK = SPACE GO J.
       K.  MOVE DE-MAHNT TO WH-WERT.
           PERFORM BEDMES.
           CALL "CAUP" USING "1005693003" WH-CREG.
           IF ENDE GO W.
           IF ESC GO Z.
           IF WOLI GO J.
           IF NOT RET GO K.
           MOVE WH-WERT TO DE-MAHNT WD-TG.
           DISPLAY WD-TG with highlight AT VDU-LP.
       M.  MOVE DE-KOND TO WH-KK.
           IF DE-KOND NOT = 0 PERFORM KOND-ANZ.
           PERFORM BEDMES.
           DISPLAY "<#>= Konditionstabelle anzeigen" AT 2401.
           MOVE 0 TO WH-NUM WH-WERT.
           CALL "CAUP" USING "1006691001" WH-CREG.
           IF KIST CALL "FIBSTAM" USING "12KOND" WH-CREG
               CANCEL "FIBSTAM" GO M.
           IF ENDE GO W.
           IF ESC GO Z.
           IF WOLI GO K.
           IF NOT RET GO M.
           MOVE WH-WERT TO WD-KZ WY.
           DISPLAY WD-KZ with highlight AT VDU-LP.
           IF WY NOT = 0 MOVE WT-KONDIT(WY) TO DE-KOND
               PERFORM KOND-ANZ
           ELSE IF WV-MCODE = "0" PERFORM MAN-KOND.
           IF WOLI GO K.
           IF ESC GO Z.
       O.  MOVE DE-TEL TO WT-TX.
           PERFORM BEDMES.
           CALL "CAUP" USING "1209110135" WH-CREG.
           IF WOLI OR AUF GO M.
           IF NOT RET GO O.
           MOVE WT-TX TO DE-TEL.
       W.  DISPLAY "<ret>= Eingaben ok, <esc>= Abbruch, <tab>= lîschen,
      -        "<>= zurÅck" AT 2301.
           CALL "CAUP" USING "0023650000" WH-CREG.
           IF HELP CALL "CAUP" USING "190013001401" WH-CREG GO W.
           IF ENDE GO W.
           IF WOLI GO O.
           IF ESC  GO Z.
           IF TABL GO Y.
           IF NOT RET GO W.
           IF DE-OPKZ = 20; IF BK-BANKKTO = SPACE and BK-BANK = SPACE
               DISPLAY "Bankverbindung fehlt!" with highlight AT 2401
               PERFORM WEITER GO J.
           IF ANLAGE WRITE DE-SATZ GO Z.
           IF AEND REWRITE DE-SATZ GO Z.
       Y.  DISPLAY "Wirklich lîschen? <ret>= Abbruch, <esc>= lîschen"
               AT 2301.
           CALL "CAUP" USING "0023511001" WH-CREG.
           IF RET GO W.
           IF NOT ESC GO Y.
           DELETE DEBITOR.
           DISPLAY "gelîscht!" AT 2401.
           PERFORM WEITER.
       Z.  EXIT.
      *****************************************************************
       OPZI SECTION.
       A.  EVALUATE DE-OPKZ
               WHEN 00 DISPLAY "mit OP " AT 0000
               WHEN 01 DISPLAY "ohne OP" AT 0000
               WHEN 10 DISPLAY "m.Op+Zi" AT 0000
               WHEN 11 DISPLAY "nur Zi"  AT 0000
               WHEN 20 DISPLAY "Einzug"  AT 0000
               WHEN 30 DISPLAY "Ig.Erw"  AT 0000.
       Z.  EXIT.
      ************************************** autom. IBAN/BIC ermitteln *
       AUTOIBAN SECTION.
       A.  IF WL-CA = 20 MOVE KR-KTONR TO BK-KTONR
                         MOVE 3 TO BK-DK
                    else MOVE DE-KTONR TO BK-KTONR
                         MOVE 2 TO BK-DK.
       C.  READ BANKDAT INVALID MOVE SPACE TO BK-BANK BK-IBAN BK-BIC
                                              BK-BANKKTO BK-BANKLTZ.
           IF ZUGRIF PERFORM BESETZT GO C.
           IF WL-CA = 20 MOVE KR-BANKKTO TO BK-BANKKTO
               MOVE KR-BANKLTZ TO BK-BANKLTZ
               MOVE KR-BANK TO BK-BANK.
           MOVE BK-BANKKTO TO WT-TX.
           IF BK-BANKLTZ = SPACE or BK-BANKKTO = SPACE GO Z.
           PERFORM VARYING WX FROM 20 BY -1 UNTIL WX = 1
               or WT-TX(WX:1) not = SPACE CONTINUE.
           IF WX not = 11 MOVE 11 TO WZ
               MOVE SPACE TO WH-MCODE
               PERFORM VARYING WX FROM WX BY -1 UNTIL WX = 0
                   MOVE WT-TX(WX:1) TO WH-MCODE(WZ:1)
                   ADD -1 TO WZ
               end-perform
               INSPECT WH-MCODE(1:11) REPLACING ALL " " BY "0"
               MOVE WH-MCODE TO WT-TX
           MOVE WT-TX TO BK-BANKKTO.
           OPEN INPUT BZLTAB.
           PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 1172 or
               BL-SATZ(1:5) = BK-BANKLTZ or BL-SATZ(1:3) = BK-BANKLTZ
               READ BZLTAB AT END MOVE 1172 TO IX
               end-read.
           CLOSE BZLTAB.
           IF IX > 1171 GO I.
           UNSTRING BL-SATZ DELIMITED BY ";" INTO WB-BZL WB-KEN WB-BIC
               WB-NAME.
           INSPECT WB-NAME REPLACING ALL "‰" by "Ñ"
                     ALL "ˆ" by "î" ALL "¸" by "Å"
                     ALL "ﬂ" by "·" ALL "ƒ" by "é"
                     ALL "÷" by "ô" ALL"‹"  by "ö".
           MOVE WB-BIC TO BK-BIC.
           MOVE WB-NAME TO BK-BANK.
       I.  PERFORM IBAN-RECH.
           MOVE WT-IBAN TO BK-IBAN.
           WRITE BK-SATZ INVALID REWRITE BK-SATZ.
           IF WL-CA = 10 MOVE BK-BANKKTO TO KR-BANKKTO
               MOVE BK-BANKLTZ TO KR-BANKLTZ
               MOVE BK-BANK TO KR-BANK.
       Z.  EXIT.
      ****************************************** Einzugsbank je Kunden *
       IBAN-BIC SECTION.
       A.  OPEN I-O BANKDAT.
           CALL "CAUP" USING "0719010480000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Bankverbindung " with highlight foreground-color 6
               AT VDU-LP.
           ADD 202 VDU-ECK GIVING VDU-LP.
           DISPLAY "Bank-Kto.:                 Bank-Lzl.:        Bez.:"
               AT VDU-LP.
           ADD 302 VDU-ECK GIVING VDU-LP.
           DISPLAY "IBAN:                                     BIC:"
               AT VDU-LP.
           IF WL-CA = 10 MOVE KR-KTONR TO BK-KTONR
                         MOVE 3 TO BK-DK
                    else MOVE DE-KTONR TO BK-KTONR
                         MOVE 2 TO BK-DK.
       C.  READ BANKDAT INVALID MOVE SPACE TO BK-BANK BK-IBAN BK-BIC
                                              BK-BANKKTO BK-BANKLTZ.
           IF ZUGRIF PERFORM BESETZT GO C.
           IF WL-CA = 10 MOVE KR-BANKKTO TO BK-BANKKTO
               MOVE KR-BANKLTZ TO BK-BANKLTZ
               MOVE KR-BANK TO BK-BANK.
           ADD 213 VDU-ECK GIVING VDU-LP.
           DISPLAY BK-BANKKTO with highlight AT VDU-LP.
           ADD 240 VDU-ECK GIVING VDU-LP.
           DISPLAY BK-BANKLTZ with highlight AT VDU-LP.
           ADD 253 VDU-ECK GIVING VDU-LP.
           DISPLAY BK-BANK with highlight AT VDU-LP.
           ADD 308 VDU-ECK GIVING VDU-LP.
           DISPLAY BK-IBAN with highlight AT VDU-LP.
           ADD 349 VDU-ECK GIVING VDU-LP.
           DISPLAY BK-BIC with highlight AT VDU-LP.
      *    IF BK-IBAN not = SPACE and BK-BIC not = SPACE GO R.
       E.  MOVE BK-BANKKTO TO WT-TX.
           DISPLAY "Bankkonto des Kd./Lft." AT 2401.
           DISPLAY "<esc>= Ende, < >= zurÅck" AT 2401.
           CALL "CAUP" USING "1202130115" WH-CREG.
           IF ESC or AUF GO R.
           IF NOT RET GO K.
           IF WT-TX(1:5) = "*2999" GO F.
           PERFORM VARYING WX FROM 20 BY -1 UNTIL WX = 1
               or WT-TX(WX:1) not = SPACE CONTINUE.
           IF WX not = 11 MOVE 11 TO WZ
               MOVE SPACE TO WH-MCODE
               PERFORM VARYING WX FROM WX BY -1 UNTIL WX = 0
                   MOVE WT-TX(WX:1) TO WH-MCODE(WZ:1)
                   ADD -1 TO WZ
               end-perform
               INSPECT WH-MCODE(1:11) REPLACING ALL " " BY "0"
               MOVE WH-MCODE TO WT-TX
               DISPLAY "ACHTUNG!! keine 11 stellige Kontonummer"
                    with highlight foreground-color 4 AT 2501.
       F.  MOVE WT-TX TO BK-BANKKTO.
           ADD 0213 VDU-ECK GIVING VDU-LP.
           DISPLAY BK-BANKKTO with highlight AT VDU-LP.

       G.  MOVE BK-BANKLTZ TO WT-TX.
           CALL "CAUP" USING "1202400107" WH-CREG.
           DISPLAY "<esc>= Ende, < >= zurÅck" AT 2401.
           IF AUF GO E.
           IF ESC GO R.
           IF NOT RET GO G.
           MOVE WT-TX TO BK-BANKLTZ.
           IF WT-TX(1:4) = "2999" GO I.
           PERFORM HOL-BIC.
           IF IX > 1171
               DISPLAY "BIC zur Bankleitzahl nicht gefunden!" at 2401
               PERFORM WEITER
               GO G.
           UNSTRING BL-SATZ DELIMITED BY ";" INTO WB-BZL WB-KEN WB-BIC
               WB-NAME.
           INSPECT WB-NAME REPLACING ALL "‰" by "Ñ"
                     ALL "ˆ" by "î" ALL "¸" by "Å"
                     ALL "ﬂ" by "·" ALL "ƒ" by "é"
                     ALL "÷" by "ô" ALL"‹"  by "ö".
           MOVE WB-BIC TO BK-BIC.
           ADD 349 VDU-ECK GIVING VDU-LP.
           DISPLAY BK-BIC with highlight AT VDU-LP.
           DISPLAY WB-NAME with highlight foreground-color 6 AT 2501.

           PERFORM IBAN-RECH.
           MOVE WT-IBAN TO WM-IBAN.
           MOVE BK-BANKLTZ TO WM-BANKLTZ.
           MOVE 0 TO WM-IB.
           IF BK-BANKLTZ(1:2) = 12 MOVE 11 TO BK-BANKLTZ(1:2)
               MOVE 1 TO WM-IB
               PERFORM IBAN-RECH
               MOVE WT-IBAN TO WN-IBAN
               MOVE BK-BANKLTZ TO WN-BANKLTZ.
       I.  MOVE BK-BANK TO WT-TX.
           DISPLAY "Bankverbindung des Kd./Lft." AT 2301.
           DISPLAY "<esc>= Ende, < >= zurÅck <Einfg>= gelben Text Åberne
      -        "hmen" AT 2401.
           CALL "CAUP" USING "1202530124" WH-CREG.
           IF AUF MOVE WM-BANKLTZ TO BK-BANKLTZ
                  GO G.
           IF ESC GO R.
           IF EINF
               MOVE WB-NAME TO BK-BANK
               ADD 253  VDU-ECK GIVING VDU-LP
               DISPLAY BK-BANK with highlight AT VDU-LP
               GO I.
           MOVE WT-TX TO BK-BANK.
           MOVE WM-IBAN TO WT-IBAN.
       K.  MOVE BK-IBAN TO WT-TX.
           IF BK-BANKLTZ(1:4) = "2999" MOVE SPACE TO BK-IBAN BK-BIC
               IF AUF GO G
               else GO R.
           DISPLAY WM-IBAN with highlight foreground-color 6 AT 2501.
           IF WM-IB not = 0 DISPLAY "oder 2= " AT 0000
              DISPLAY WN-IBAN with highlight foreground-color 5 AT 0000.
           DISPLAY "IBAN des Kd./Lft." AT 2301.
           DISPLAY "<esc>= Ende, < >= zurÅck, <Einfg>= gelben IBAN, <APO
      -        "S>= roten IBAN Åbernehmen" AT 2401.
           CALL "CAUP" USING "1203080135" WH-CREG.
           IF AUF GO I.
           MOVE WM-BANKLTZ TO BK-BANKLTZ.
           IF APOS MOVE WN-IBAN TO BK-IBAN WT-IBAN
               MOVE WN-BANKLTZ TO BK-BANKLTZ
               ADD 0308 VDU-ECK GIVING VDU-LP
               DISPLAY BK-IBAN with highlight AT VDU-LP
               GO K.
           IF EINF MOVE WM-IBAN TO BK-IBAN WT-IBAN
               MOVE WM-BANKLTZ TO BK-BANKLTZ
               ADD 0308 VDU-ECK GIVING VDU-LP
               DISPLAY BK-IBAN with highlight AT VDU-LP
               GO K.
           IF ESC GO R.
           MOVE WT-TX TO BK-IBAN.
       M.  MOVE BK-BIC TO WT-TX.
           DISPLAY "BIC des Kd./Lft." AT 2301.
           DISPLAY "<esc>= Ende, < >= zurÅck" AT 2401.
           CALL "CAUP" USING "1203490115" WH-CREG.
           IF AUF GO K.
           IF ESC GO R.
           MOVE WT-TX TO BK-BIC.
       R.  DISPLAY "<Entf>= lîschen, <>= Ñndern, <esc>= Abbruch, <ret-l
      -        "eer>= ok < >" AT 2301.
           DISPLAY "Bitte IBAN und BIC prÅfen!" with highlight
               foreground-color 6 AT 2401.
           CALL "CAUP" USING "0023630000" WH-CREG.
           IF ESC GO X.
           IF WOLI GO E.
           IF RET and BK-BANKKTO = SPACE and BK-BANK = SPACE
               SET ENTF TO TRUE.
           IF ENTF DELETE BANKDAT INVALID MOVE SPACE TO BK-BANK
                   end-delete MOVE SPACE TO BK-SATZ
                              GO X.
           IF NOT RET GO R.
           WRITE BK-SATZ INVALID REWRITE BK-SATZ.
           IF WL-CA = 10 MOVE BK-BANKKTO TO KR-BANKKTO
               MOVE BK-BANKLTZ TO KR-BANKLTZ.
               MOVE BK-BANK TO KR-BANK.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           CLOSE BANKDAT.
       Z.  EXIT.
      ******************************************************************
       KOND-ANZ SECTION.
       A.  ADD 750 VDU-ECK GIVING VDU-LP.
           MOVE DE-KOND TO WH-KK.
           IF DE-KOND = 0
               DISPLAY "keine Skontierung    " AT VDU-LP
               ADD 100 TO VDU-LP
               DISPLAY "                     " AT VDU-LP GO Z.
           DISPLAY "                             " AT VDU-LP.
           ADD 100 TO VDU-LP.
           DISPLAY "                      " AT VDU-LP.
           SUBTRACT 100 FROM VDU-LP.
           IF DE-KOND = 9999999 MOVE 0 TO DE-KOND WH-KK.
           DISPLAY " %-1 Tg. %-2 Tg. TgN. " with reverse-video
               AT VDU-LP.
           ADD 101 TO VDU-LP.
           IF WH-SK1 NOT = 0 MOVE WH-SK1 TO WD-PZ
               DISPLAY WD-PZ with highlight AT VDU-LP
               MOVE WH-TG1 TO WD-TG
               ADD 4 TO VDU-LP
               DISPLAY WD-TG with highlight AT VDU-LP
               ADD 4 TO VDU-P
           else ADD 8 TO VDU-LP.
           IF WH-SK2 NOT = 0 MOVE WH-SK2 TO WD-PZ
               DISPLAY WD-PZ with highlight AT VDU-LP
               MOVE WH-TG2 TO WD-TG
               ADD 4 TO VDU-LP
               DISPLAY WD-TG with highlight AT VDU-LP
               ADD 4 TO VDU-P
           else ADD 8 TO VDU-LP.
           IF WH-TGN NOT = 0 MOVE WH-TGN TO WD-TG
               DISPLAY WD-TG with highlight AT VDU-LP.
       Z.  EXIT.
      ***************************************** man. Konditionseingabe *
       MAN-KOND SECTION.
       A.  IF DE-KOND = 0 MOVE 9999999 TO DE-KOND.
           PERFORM KOND-ANZ.
           DISPLAY "<ret>= Skonto-1 (x,x), <esc>= Abbruch" AT 2301.
           MOVE WH-SK1 TO WH-WERT.
           CALL "CAUP" USING "1008512103" WH-CREG.
           IF ESC OR WOLI GO Z.
           IF NOT RET GO A.
           MOVE WH-WERT TO WH-SK1 WD-PZ.
           DISPLAY WD-PZ with highlight AT VDU-LP.
       F.  DISPLAY "<ret>= Tage-1, <esc>= Abbruch" AT 2301.
           MOVE WH-TG1 TO WH-WERT.
           CALL "CAUP" USING "1008553003" WH-CREG.
           IF ESC GO Z.
           IF WOLI GO A.
           IF NOT RET GO F.
           MOVE WH-WERT TO WH-TG1 WD-TG.
           DISPLAY WD-TG with highlight AT VDU-LP.
       G.  DISPLAY "<ret>= Skonto-2 (x,x), <esc>= Abbruch" AT 2301.
           MOVE WH-SK2 TO WH-WERT.
           CALL "CAUP" USING "1008592103" WH-CREG.
           IF ESC GO Z.
           IF WOLI GO F.
           IF NOT RET GO G.
           MOVE WH-WERT TO WH-SK2 WD-PZ.
           DISPLAY WD-PZ with highlight AT VDU-LP.
       H.  DISPLAY "<ret>= Tage-2, <esc>= Abbruch" AT 2301.
           MOVE WH-TG2 TO WH-WERT.
           CALL "CAUP" USING "1008633003" WH-CREG.
           IF ESC GO Z.
           IF WOLI GO G.
           IF NOT RET GO H.
           MOVE WH-WERT TO WH-TG2 WD-TG.
           DISPLAY WD-TG with highlight AT VDU-LP.
       I.  DISPLAY "<ret>= Nettotage, <esc>= Abbruch" AT 2301.
           MOVE WH-TGN TO WH-WERT.
           CALL "CAUP" USING "1008673003" WH-CREG.
           IF ESC GO Z.
           IF WOLI GO H.
           IF NOT RET GO I.
           MOVE WH-WERT TO WH-TGN WD-TG.
           DISPLAY WD-TG with highlight AT VDU-LP.
           MOVE WH-KK TO DE-KOND.
       Z.  EXIT.
      *********************************************** Druck Stammdaten *
       STAMDRUCK SECTION.
       A.  CALL "CAUP" USING "0710100466000" WH-CREG.
           PERFORM LADE-DRU.
           MOVE "DEBSTAM.LST" TO WH-DRUNAM.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Stammdatendruck " AT VDU-LP.
       C.  ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "<esc>= Abbruch, <ret>= Nummerndruck, <tab>= Matchcod
      -        "edruck < >" AT VDU-LP.
           CALL "CAUP" USING "1102630000" WH-CREG.
           IF ESC GO X.
           IF NOT RETA GO C.
           IF RET MOVE 0 TO DE-KTONR
               START DEBITOR KEY NOT < DE-KEY INVALID GO C
           else MOVE LOW-VALUES TO DE-MCODE
               START DEBITOR KEY NOT < DE-MCODE INVALID GO C.
       E.  READ DEBITOR NEXT NO LOCK AT END GO W.
           IF ZUGRIF PERFORM BESETZT GO E.
           ADD 302 VDU-ECK GIVING VDU-LP.
           IF DE-OPKZ > 19 and WM-DATUM < 20111232 OPEN I-O BANKDAT
                 PERFORM AUTOIBAN
                 CLOSE BANKDAT.
           MOVE DE-KTONR TO WD-KZ.
           PERFORM DEBKOPF.
           DIVIDE 10 INTO DE-KTONR GIVING WD-KTO.
           MOVE WD-KTO TO DRA-STR(1:10).
           INSPECT DE-BEZ REPLACING ALL "#" BY SPACE.
           MOVE DE-BEZ TO DRA-STR(10:50).
           DISPLAY WD-KTO with highlight AT VDU-LP DRA-STR(09:50).
           MOVE DE-MCODE TO WT-TX.
           CALL "CAUP" USING "21CONV08" WH-CREG.
           MOVE WT-TX TO DRA-STR(61:8).
           MOVE DE-KOND TO WH-KK.
           IF WH-SK1 not = 0 MOVE WH-SK1 TO WD-PZ
               MOVE WD-PZ TO DRA-STR(70:).
           IF WH-TG1 not = 0 MOVE WH-TG1 TO WD-TG
               MOVE WD-TG TO DRA-STR(73:).
           IF WH-SK2 not = 0 MOVE WH-SK2 TO WD-PZ
               MOVE WD-PZ TO DRA-STR(78:).
           IF WH-TG2 not = 0 MOVE WH-TG2 TO WD-TG
               MOVE WD-TG TO DRA-STR(81:).
           IF WH-TGN not = 0 MOVE WH-TGN TO WD-TG
               MOVE WD-TG TO DRA-STR(86:).
           MOVE DE-TEL TO DRA-STR(110:).
           PERFORM DRUCK.
           GO E.
       W.  PERFORM END-DRU.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ******************************************************************
       DEBKOPF SECTION.
       A.  IF WZ-ZEILEN > 62 WRITE DRA-SATZ AFTER PAGE
               MOVE 0 TO WZ-ZEILEN.
           IF WZ-ZEILEN > 0 GO Z.
           MOVE 2 TO WH-P1.
           MOVE 4 TO WH-P2.
           IF WM-DRU = 1 MOVE 15 TO WH-P1.
           PERFORM BEG-DRU.
           IF WM-DRU = 0 MOVE 1 TO WZ-SCHALT.
           MOVE WK-FIRMA TO DRA-STR(1:30).
           MOVE "Debitorenliste" TO DRA-STR(33:).
           MOVE WH-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRA-STR(87:10).
           MOVE "Seite:" TO DRA-STR(125:).
           ADD 1 TO WZ-SEITE.
           MOVE WZ-SEITE TO WD-SEITE.
           MOVE WD-SEITE TO DRA-STR(132:).
           PERFORM DRUCK.
           MOVE ALL "ƒƒƒ" TO DRA-STR(1:135).
           PERFORM DRUCK.
           MOVE "Konto-Nr Bezeichnung
      -       "     M-code   %-1 Tg. %-2 Tg. TgN. A op Mt Telefon
      -       "                     " TO DRA-STR(1:).
           PERFORM DRUCK.
           MOVE ALL "ƒƒƒ" TO DRA-STR(1:135).
           PERFORM DRUCK.
       Z.  EXIT.
      *************************************************** BIC - bilden *
       HOL-BIC SECTION.
       A.  OPEN INPUT BZLTAB.
110112*    IF BK-BANKLTZ(1:2) = 12 MOVE 11 TO BK-BANKLTZ(1:2).
           PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 1172 or
               BL-SATZ(1:5) = BK-BANKLTZ or BL-SATZ(1:3) = BK-BANKLTZ
               READ BZLTAB AT END MOVE 1172 TO IX
               end-read.
           CLOSE BZLTAB.
       Z.  EXIT.
      *********************************************** IBAN - errechnen *
       IBAN-RECH SECTION.
       A.  MOVE 0 TO WM-IBA WM-IBB.
           MOVE BK-BANKLTZ TO WT-IBAN(5:5).
           MOVE BK-BANKKTO TO WH-MCODE.
           MOVE "00000000000" TO BK-BANKKTO.
           MOVE 11 TO WX.
           PERFORM VARYING WZ FROM 11 BY -1 UNTIL WZ = 0
               EVALUATE TRUE
                   WHEN WH-MCODE(WZ:1) = 0
                   WHEN WH-MCODE(WZ:1) = 1
                   WHEN WH-MCODE(WZ:1) = 2
                   WHEN WH-MCODE(WZ:1) = 3
                   WHEN WH-MCODE(WZ:1) = 4
                   WHEN WH-MCODE(WZ:1) = 5
                   WHEN WH-MCODE(WZ:1) = 6
                   WHEN WH-MCODE(WZ:1) = 7
                   WHEN WH-MCODE(WZ:1) = 8
                   WHEN WH-MCODE(WZ:1) = 9
                       MOVE WH-MCODE(WZ:1) TO BK-BANKKTO(WX:1)
                       ADD -1 TO WX.
           INSPECT BK-BANKKTO(1:11) REPLACING ALL " " BY "0".
           MOVE BK-BANKKTO TO WT-IBAN(10:11).
           MOVE WT-IBAN(5:13) TO WM-IBA.
           MOVE WT-IBAN(18:10) TO WM-IBB(5:10).
           MOVE 102900 TO WM-IBB(8:).
           DIVIDE 97 INTO WM-IBA GIVING WH-NUM REMAINDER WM-IBR.
           MOVE WM-IBR TO WM-IBB(3:2).
           DIVIDE 97 INTO WM-IBB GIVING WH-NUM REMAINDER WM-IBR.
           COMPUTE WM-IBR = 98 - WM-IBR.
           MOVE "AT" TO WT-IBAN.
           MOVE WM-IBR TO WT-IBAN(3:2).
           MOVE BK-BANKLTZ TO WT-IBAN(5:).
           MOVE BK-BANKKTO TO WT-IBAN(10:).
       Z.  EXIT.
