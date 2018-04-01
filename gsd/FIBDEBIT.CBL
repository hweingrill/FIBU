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
           SELECT BANKDAT   ASSIGN TO "FIBUBANK.DAT"
                            ORGANIZATION INDEXED ACCESS DYNAMIC
                            RECORD KEY BK-KEY
                            FILE STATUS IS WF-STATUS.
           SELECT EXCELIS   ASSIGN TO PRINTER WH-FIBNAM
                            FILE STATUS WF-STATUS.
           SELECT DRUCKER   ASSIGN TO PRINTER WH-DRUNAM
                            FILE STATUS WF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       COPY FIBUFD.CPY.
      *************************************** sontige Bankverbindungen *
       FD  BANKDAT      external       label record standard.
       01  BK-SATZ.
           03  BK-KEY.
               05 BK-DK                PIC 99         COMP.
               05 BK-KTONR             PIC 9(6)       COMP.
           03  BK-BANKKTO              PIC X(13).
           03  BK-BANKLTZ              PIC X(6).
           03  BK-BANK                 PIC X(24).
      ******************************************************************
       FD  DRUCKER                     LABEL RECORD STANDARD.
       01  DRA-SATZ.
           03  FILLER                  PIC XXXX.
           03  DRA-STR                 PIC X(135).
      ******************************************** Excel-öbergabedatei *
       FD  EXCELIS                     LABEL RECORD OMITTED.
       01  EXC-SATZ.
           03  EXC-KAT                 PIC 9.
           03  EXC-A                   PIC X.
           03  EXC-KTONR               PIC ZZZZ9.
           03  EXC-B                   PIC X.
           03  EXC-MCODE               PIC X(15).
           03  EXC-C                   PIC X.
           03  EXC-GRP                 PIC 9.
           03  EXC-D                   PIC X.
           03  EXC-OPKZ                PIC 9.
           03  EXC-E                   PIC X.
           03  EXC-ANR                 PIC X(10).
           03  EXC-F                   PIC X.
      *    03  EXC-SOLL                PIC -ZZZZZZZZ9,99.
      *    03  EXC-G                   PIC X.
      *    03  EXC-HABEN               PIC -ZZZZZZZZ9,99.
      *    03  EXC-H                   PIC X.
      *    03  EXC-UMSATZ              PIC -ZZZZZZZZ9,99.
      *    03  EXC-I                   PIC X.
           03  EXC-PZ1                 PIC 99,9.
           03  EXC-J                   PIC X.
           03  EXC-TG1                 PIC 999.
           03  EXC-K                   PIC X.
           03  EXC-PZ2                 PIC 99,9.
           03  EXC-L                   PIC X.
           03  EXC-TG2                 PIC 999.
           03  EXC-M                   PIC X.
           03  EXC-TGN                 PIC 999.
           03  EXC-N                   PIC X.
           03  EXC-MAHNT               PIC ZZZ.
           03  EXC-NN                  PIC X.
           03  EXC-MAHNDAT             PIC ZZZZZZZZ.
           03  EXC-O                   PIC X.
           03  EXC-TEL                 PIC X(35).
           03  EXC-P                   PIC X.
           03  EXC-NAME                PIC X(35).
           03  EXC-Q                   PIC X.
           03  EXC-ZUSATZ              PIC X(35).
           03  EXC-R                   PIC X.
           03  EXC-STR                 PIC X(35).
           03  EXC-S                   PIC X.
           03  EXC-PLZL                PIC Z9999.
           03  EXC-T                   PIC X.
           03  EXC-ORT                 PIC X(30).
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
           03  WH-FIBNAM               PIC X(12).
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
       DECL-M SECTION.         USE AFTER ERROR PROCEDURE ON BANKDAT.
       A.  CALL "CADECL" USING "FIBUBANK.DAT" WH-CREG.
       Z.  EXIT.
       END DECLARATIVES.
      ******************************************************************
       STEUER SECTION.
       A.  MOVE WL-CALL TO WH-CALL.
           MOVE WL-CREG TO WH-CREG.
           EVALUATE WL-CA
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
           DISPLAY "0= mit, 1= ohne OP's, + 10= mit Zinsen, 20= Einzug"
               AT 2401.
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
               WHEN OTHER GO J.
           DISPLAY WD-OP with highlight AT VDU-LP "-".
           PERFORM OPZI.
           IF DE-OPKZ = 20 PERFORM EINZUGBANK
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
               WHEN 20 DISPLAY "Einzug"  AT 0000.
       Z.  EXIT.
      ****************************************** Einzugsbank je Kunden *
       EINZUGBANK SECTION.
       A.  OPEN I-O BANKDAT.
           CALL "CAUP" USING "0720010380000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Bankverbindung " with highlight foreground-color 6
               AT VDU-LP.
           ADD 202 VDU-ECK GIVING VDU-LP.
           DISPLAY "Bank-Kto.:                 Bank-Lzl.:        Bez.:"
               AT VDU-LP.
           MOVE DE-KTONR TO BK-KTONR.
           MOVE 2 TO BK-DK.
       C.  READ BANKDAT INVALID MOVE SPACE TO BK-BANK
                                              BK-BANKKTO BK-BANKLTZ.
           IF ZUGRIF PERFORM BESETZT GO C.
           ADD 213 VDU-ECK GIVING VDU-LP.
           DISPLAY BK-BANKKTO with highlight AT VDU-LP.
           ADD 240 VDU-ECK GIVING VDU-LP.
           DISPLAY BK-BANKLTZ with highlight AT VDU-LP.
           ADD 253 VDU-ECK GIVING VDU-LP.
           DISPLAY BK-BANK with highlight AT VDU-LP.
           IF BK-BANK not = SPACE and BK-BANKKTO not = SPACE GO R.
       E.  MOVE BK-BANKKTO TO WT-TX.
           DISPLAY "Bankkonto des Kunden" AT 2401.
           CALL "CAUP" USING "1202130115" WH-CREG.
           IF ESC or WOLI GO R.
           IF NOT RET GO K.
           MOVE WT-TX TO BK-BANKKTO.
       G.  MOVE BK-BANKLTZ TO WT-TX.
           CALL "CAUP" USING "1202400107" WH-CREG.
           IF WOLI GO E.
           IF ESC GO R.
           IF NOT RET GO G.
           MOVE WT-TX TO BK-BANKLTZ.
       I.  MOVE BK-BANK TO WT-TX.
           DISPLAY "Bankverbindung des Kunden" AT 2301.
           CALL "CAUP" USING "1202530122" WH-CREG.
           IF WOLI GO G.
           IF ESC GO R.
           MOVE WT-TX TO BK-BANK.
       R.  DISPLAY "<esc>= Abbruch, <ret-leer>= ok < >" AT 2301.
           DISPLAY "<Entf>= lîschen, <>= énderung" AT 2401.
           CALL "CAUP" USING "0023330000" WH-CREG.
           IF ESC GO X.
           IF WOLI GO G.
           IF RET and BK-BANKKTO = SPACE and BK-BANK = SPACE
               SET ENTF TO TRUE.
           IF ENTF DELETE BANKDAT INVALID MOVE SPACE TO BK-BANK
                   end-delete MOVE SPACE TO BK-SATZ
                              GO X.
           IF NOT RET GO R.
           WRITE BK-SATZ INVALID REWRITE BK-SATZ.
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
           MOVE "DEBIT.LST" TO WH-FIBNAM.
           OPEN OUTPUT EXCELIS.
           IF WM-OPEN = 0 MOVE "Kat;Kto-Nr.;Matchcode;Grp.;OP-Kz;Anr.;%-
      -        "Sk1;Tg-1;%-Sk2;TG-2;TG-N;Mahn-Tg;Mahn-dat.;Anmerkg.;Name
      -        ";Zusatz;Stra·e;Plzl;Ort" TO EXC-SATZ
               INSPECT EXC-SATZ REPLACING ALL ";" BY X"09".
               WRITE EXC-SATZ
               MOVE SPACE TO EXC-SATZ.
           IF RET MOVE 0 TO DE-KTONR
               START DEBITOR KEY NOT < DE-KEY INVALID GO C
           else MOVE LOW-VALUES TO DE-MCODE
               START DEBITOR KEY NOT < DE-MCODE INVALID GO C.
       E.  READ DEBITOR NEXT NO LOCK AT END GO W.
           IF ZUGRIF PERFORM BESETZT GO E.
           ADD 302 VDU-ECK GIVING VDU-LP.
           MOVE DE-KTONR TO WD-KZ.
           PERFORM DEBKOPF.
           MOVE ALL x"09" TO EXC-SATZ.

           IF WN-BUEB(9:2) = "BG" ADD 900000 TO DE-KTONR.

           DIVIDE 10 INTO DE-KTONR GIVING WD-KTO EXC-KTONR.
           MOVE DE-KTONR TO EXC-KAT.
           MOVE WD-KTO TO DRA-STR(1:10).
           MOVE DE-BEZ TO WT-BEZ.
           INSPECT DE-BEZ REPLACING ALL "#" BY SPACE.
           MOVE DE-BEZ TO DRA-STR(10:50).
           DISPLAY WD-KTO with highlight AT VDU-LP DRA-STR(09:50).
           MOVE DE-MCODE TO WT-TX.
           CALL "CAUP" USING "21CONV08" WH-CREG.
           MOVE WT-TX TO DRA-STR(61:8) EXC-MCODE.
           MOVE DE-KOND TO WH-KK.
           MOVE WH-SK1 TO WD-PZ EXC-PZ1.
           MOVE WD-PZ TO DRA-STR(70:).
           MOVE WH-TG1 TO WD-TG EXC-TG1.
           MOVE WD-TG TO DRA-STR(73:).
           MOVE WH-SK2 TO WD-PZ EXC-PZ2.
           MOVE WD-PZ TO DRA-STR(78:).
           MOVE WH-TG2 TO WD-TG EXC-TG2.
           MOVE WD-TG TO DRA-STR(81:).
           MOVE WH-TGN TO WD-TG EXC-TGN.
           MOVE WD-TG TO DRA-STR(86:).
           MOVE DE-TEL TO DRA-STR(110:) EXC-TEL.
           PERFORM DRUCK.
      *--------------------------------------> restl. Daten fÅr Excel <-
           MOVE DE-MAHNT TO EXC-MAHNT.
           MOVE DE-MAHNDAT TO EXC-MAHNDAT.
           MOVE DE-KTOART TO EXC-GRP.
           MOVE WT-TXT(DE-ANREDE + 1) TO EXC-ANR.
           MOVE DE-OPKZ TO EXC-OPKZ.
      *    MOVE DE-SOLL TO EXC-SOLL.
      *    MOVE DE-HABEN TO EXC-HABEN.
      *    MOVE DE-UMSATZ TO EXC-UMSATZ.
           MOVE SPACE TO WT-ADR.
           MOVE "#" TO WR-BEZ(131).
           UNSTRING WT-BEZ DELIMITED BY "#" INTO WR-ADR(1) WR-ADR(2)
               WR-ADR(3) WR-ADR(4) WR-ADR(5) WR-ADR(6).
           MOVE WR-ADR(1) TO EXC-NAME.
           MOVE WR-ADR(2) TO EXC-ZUSATZ.
           MOVE 0 TO EXC-PLZL.
           MOVE SPACE TO EXC-ORT EXC-STR.
           IF WR-ADR(6)(1:10) not = SPACE
               MOVE WR-ADR(6) TO WT-TX
               PERFORM PLZL-ORT
               MOVE WR-ADR(5) TO EXC-STR GO G.
           IF WR-ADR(5) not = SPACE
               MOVE WR-ADR(5) TO WT-TX
               PERFORM PLZL-ORT
               MOVE WR-ADR(4) TO EXC-STR
               GO G.
           IF WR-ADR(4) not = SPACE
               MOVE WR-ADR(4) TO WT-TX
               PERFORM PLZL-ORT
               MOVE WR-ADR(3) TO EXC-STR
               GO G.
           IF WR-ADR(3) not = SPACE
               MOVE WR-ADR(3) TO WT-TX
               PERFORM PLZL-ORT
               MOVE WR-ADR(2) TO EXC-STR
               MOVE SPACE TO EXC-ZUSATZ.
       G.  WRITE EXC-SATZ.
           GO E.
       W.  PERFORM END-DRU.
           CLOSE EXCELIS.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ***************************************** PLZL und ORT abstellen *
       PLZL-ORT SECTION.
       A.  PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 5
               IF WT-TX(WX:1) = SPACE or LOW-VALUES MOVE WX TO WZ.
           ADD -1 WZ GIVING WX.
           IF WT-TX(1:4) is NUMERIC MOVE WT-TX(1:WX) TO EXC-PLZL.
           ADD 1 TO WZ.
           MOVE WT-TX(WZ:) TO EXC-ORT.
           IF EXC-ORT(1:1) = SPACE MOVE EXC-ORT(2:) TO EXC-ORT(1:).
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
