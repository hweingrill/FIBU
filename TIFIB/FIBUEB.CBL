      $SET LINKCOUNT "240" ANS85 BOUND AUTOLOCK NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.      FIBUEB.
      ******************************************************************
      *   FIBU - öbernahme von fibu4000    Fa. Spitzer
      ******************************************************************
       ENVIRONMENT    DIVISION.
       CONFIGURATION   SECTION.
       SOURCE-COMPUTER.  PC.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY FIBUSEC.CPY.
           SELECT SACHUEB  ASSIGN TO "UEBSACH.DAT"
                           ORGANIZATION RECORD SEQUENTIAL
                           FILE STATUS WF-STATUS.
           SELECT DEBUEB   ASSIGN TO "UEBDEB.DAT"
                           ORGANIZATION RECORD SEQUENTIAL
                           FILE STATUS WF-STATUS.
           SELECT KREDUEB  ASSIGN TO "UEBKRED.DAT"
                           ORGANIZATION RECORD SEQUENTIAL
                           FILE STATUS WF-STATUS.
           SELECT OPUEB    ASSIGN TO "UEBOP.DAT"
                           ORGANIZATION RECORD SEQUENTIAL
                           FILE STATUS WF-STATUS.
           SELECT SALDEB   ASSIGN TO "UEBSALDE.DAT"
                           ORGANIZATION RECORD SEQUENTIAL
                           FILE STATUS WF-STATUS.
           SELECT SALKRED  ASSIGN TO "UEBSALKR.DAT"
                           ORGANIZATION RECORD SEQUENTIAL
                           FILE STATUS WF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       COPY FIBUFD.CPY.
      ************************************************ Sachkontenstamm *
       FD  SACHUEB                     LABEL RECORD STANDARD.
       01  SU-SATZ.
           03  SU-FNR                  PIC S999     COMP-3.
           03  SU-KTONR                PIC S9(9)    COMP-3.
           03  SU-KAT                  PIC 9.
           03  SU-MCODE                PIC X(7).
           03  SU-UST                  PIC S999    COMP-3.
           03  SU-BEZ                  PIC X(40).
           03  SU-ART                  PIC S9      COMP-3.
           03  SU-BN                   PIC S9      COMP-3.
      ************************************************* Debotirenstamm *
       FD  DEBUEB                      LABEL RECORD STANDARD.
       01  DU-SATZ.
           03  DU-FNR                  PIC S999     COMP-3.
           03  DU-KTONR                PIC S9(9)    COMP-3.
           03  DU-KAT                  PIC 9.
           03  DU-MCODE                PIC X(7).
           03  DU-ADR                  PIC X(30)  OCCURS 7.
           03  FILLER                  PIC X(5).
      ************************************************ Kreditorenstamm *
       FD  KREDUEB                     LABEL RECORD STANDARD.
       01  KU-SATZ.
           03  KU-FNR                  PIC S999     COMP-3.
           03  KU-KTONR                PIC S9(9)    COMP-3.
           03  KU-KAT                  PIC 9.
           03  KU-MCODE                PIC X(7).
           03  KU-ADR                  PIC X(30)  OCCURS 7.
           03  FILLER                  PIC X(5).
      ***************************************************** OP-Dateien *
       FD  OPUEB                       LABEL RECORD STANDARD.
       01  UP-SATZ.
           03  UP-FNR                  PIC S999       COMP-3.
           03  UP-KTONR                PIC S9(9)      COMP-3.
           03  UP-BEZUG                PIC S9(7)      COMP-3.
           03  UP-PZ1                  PIC S99V9     COMP-3.
           03  UP-TG1                  PIC S999       COMP-3.
           03  UP-PZ2                  PIC S99V9     COMP-3.
           03  UP-TG2                  PIC S999       COMP-3.
           03  UP-TGN                  PIC S999       COMP-3.
           03  UP-STUFE                PIC S9         COMP-3.
           03  UP-BELDAT               PIC S9(6)      COMP-3.
           03  UP-VALDAT               PIC S9(6)      COMP-3.
           03  UP-FAELLIG              PIC S9(6)      COMP-3.
           03  UP-SYM                  PIC XX.
           03  UP-ZUS                  PIC S9         COMP-3.
           03  UP-UST                  PIC S99       COMP-3.
           03  UP-REBET                PIC S9(11)V99 COMP-3.
           03  UP-ZABET                PIC S9(11)V99 COMP-3.
           03  UP-SKTO                 PIC S9(09)V99 COMP-3.
           03  UP-SKTOBAS              PIC S9(10)V99 COMP-3.
      **************************************************** Kudensalden *
       FD  SALDEB                      LABEL RECORD STANDARD.
       01  SD-SATZ.
           03  SD-FNR                  PIC S999      COMP-3.
           03  SD-KTONR                PIC S9(9)     COMP-3.
           03  SD-MCODE                PIC X(7).
           03  SD-SALDO                PIC S9(11)V99  COMP-3.
      ************************************************ Kreditorenstamm *
       FD  SALKRED                     LABEL RECORD STANDARD.
       01  SK-SATZ.
           03  SK-FNR                  PIC S999      COMP-3.
           03  SK-KTONR                PIC S9(9)     COMP-3.
           03  SK-MCODE                PIC X(7).
           03  SK-SALDO                PIC S9(11)V99  COMP-3.
      ******************************************************************
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
       COPY $COBDIR\WHCREG.CPY.
       01  WH-REG.
      *    03  WZ-BUCH                 PIC 9999      COMP.
           03  WS-BUCH                 PIC 9999      COMP.
           03  WH-KK                   PIC 9(13).
           03  WR-KK REDEFINES WH-KK.
               05 WH-SK1               PIC 9V9.
               05 WH-TG1               PIC 999.
               05 WH-SK2               PIC 9V9.
               05 WH-TG2               PIC 999.
               05 WH-TGN               PIC 999.
           03  WH-TAG                  PIC 999.
           03  WH-BZSATZ               PIC X(120).
      *----------------------------------------------> Command-Line <-
           03  RESULT                  PIC 99         COMP-X.
           03  FUNKT                   PIC 99         COMP-X VALUE 35.
           03  PARAM.
               05 SUB                  PIC 99         COMP-X VALUE 0.
               05 PAR                  PIC X(6)       VALUE  SPACE.
           03  IX                      PIC 99        COMP-X.
           03  wn-num                  pic 9(14)     comp.
           03  ws                      PIC 99        COMP.
           03  iy                      PIC 99        COMP-X.
           03  WI                      PIC 99        COMP.
           03  WN                      PIC X.
           03  WV-FNR                  PIC S999      COMP-3.
           03  WM-FNR                  PIC S999      COMP-3.
           03  WM-SOLL                 PIC S9(11)V99 COMP-3.
           03  WM-HABEN                PIC S9(11)V99 COMP-3.
           03  WD-FNR                  PIC ZZ9.
           03  WD-KZ                   PIC 9.
           03  wh-anz                  pic 99999.
           03  wh-cv.
               05 wh-dk                pic 99      comp-x  occurs 7.
           03  WB-NUM                  PIC 9(4)    VALUE 0.
           03  WT-X.
               05 wx-x                 pic 99      comp-x
                                               occurs 130 indexed ax ay.
           03  WT-ACON                 PIC X(128)     VALUE "
      -    "                     !""#$%&'()*+,-./0123456789:;<=>?@ABCDEF
      -    "GHIJKLMNOPQRSTUVWXYZéôö^_`abcdefghijklmnopqrstuvwxyzÑîÅ·".
           03  wr-cv redefines wt-acon pic 99        comp-x  occurs 128.
       01  WT-DK    external.
           03  WR-TN                   PIC X(10)  OCCURS 7.
           03  WT-KON                  PIC 99     OCCURS 12.
       COPY FIBUEXT.CPY.
       COPY FIBUDECL.CPY.
       DECL-A SECTION.         USE AFTER ERROR PROCEDURE ON SACHUEB.
       A.  CALL "CADECL" USING "UEBSACH.DAT " WH-CREG.
       DECL-B SECTION.         USE AFTER ERROR PROCEDURE ON DEBUEB.
       A.  CALL "CADECL" USING "UEBDEBI.DAT " WH-CREG.
       DECL-C SECTION.         USE AFTER ERROR PROCEDURE ON KREDUEB.
       A.  CALL "CADECL" USING "UEBKRED.DAT " WH-CREG.
       DECL-E SECTION.         USE AFTER ERROR PROCEDURE ON OPUEB.
       A.  CALL "CADECL" USING "UEBOP.DAT   " WH-CREG.
       DECL-F SECTION.         USE AFTER ERROR PROCEDURE ON SALDEB.
       A.  CALL "CADECL" USING "UEBSALDE.DAT" WH-CREG.
       DECL-G SECTION.         USE AFTER ERROR PROCEDURE ON SALKRED.
       A.  CALL "CADECL" USING "UEBSALKR.DAT" WH-CREG.
       Z.  EXIT.
       END DECLARATIVES.
      *****************************************************************
       STEUER SECTION.
       A.  MOVE WL-CALL TO WH-CALL.
           MOVE WL-CREG TO WH-CREG.
           MOVE " FibuÅbernahme" TO WK-GEB.
           CALL "CAUP" USING "06KOPF" WH-CREG.
       C.  DISPLAY "1 - öbernahme Sachkonten"  AT 0510
           DISPLAY "2 - öbernahme Kunden     " AT 0710
           DISPLAY "3 - öbernahme Lieferanten" AT 0910
           DISPLAY "< >               " AT 1040
           DISPLAY "4 - öbernahme OP`s"        AT 1110.
           DISPLAY "5 - öbernahme Kd-Salden"   AT 1310
           DISPLAY "6 - öbernahme Lft-Salden"  AT 1510
           DISPLAY "<ret-leer>= alles" AT 2301.
           CALL "CAUP" USING "0010411001" WH-CREG.
           IF ESC GO Z.
           IF NOT RET GO C.
           MOVE WH-NUM TO WH-PG.
       E.  DISPLAY "<ret>= Firmen-Nr., 0+<ret>= ohne Firmen-Nr.,<esc>=
      -        "Abbruch" AT 2301.
           DISPLAY "Firmen-Nr.: " AT 1051.
           CALL "CAUP" USING "0010633003" WH-CREG.
           IF ESC GO Z.
           IF NOT RET GO E.
           MOVE WH-WERT TO WV-FNR WD-FNR.
           DISPLAY WD-FNR with highlight AT VDU-LP.
           EVALUATE WH-PG
               WHEN 0 DISPLAY "alles" with highlight AT 0365
                      PERFORM SACH-UEBER
                      PERFORM DEB-UEBER
                      PERFORM KRED-UEBER
                      PERFORM OP-UEBER
                      PERFORM SAL-DEB
                      PERFORM SAL-KRED
               WHEN 1 DISPLAY "Sachkonten" with highlight AT 0365
                      PERFORM SACH-UEBER
               WHEN 2 DISPLAY "Kundenkonten" with highlight AT 0365
                      PERFORM DEB-UEBER
               WHEN 3 DISPLAY "Lieferanten" with highlight AT 0365
                      PERFORM KRED-UEBER
               WHEN 4 DISPLAY "offene Posten" with highlight AT 0365
                      PERFORM OP-UEBER
               WHEN 5 DISPLAY "Kundensalden" with highlight AT 0365
                      PERFORM SAL-DEB
               WHEN 6 DISPLAY "Lft.Salden" with highlight AT 0365
                      PERFORM SAL-KRED.
           GO C.
       Z.  EXIT PROGRAM.
      ****************************************************** Sackonten *
       SACH-UEBER SECTION.
       A.  OPEN INPUT SACHUEB.
           IF WF-STATUS > "00" GO Z.
       C.  READ SACHUEB NEXT AT END GO X.
           DISPLAY SU-KTONR AT 2220.
           MOVE SU-FNR TO WM-FNR.
           IF WV-FNR NOT = 0 AND WM-FNR NOT = WV-FNR GO C.
           INITIALIZE SA-SATZ.
           move SU-KTONR TO SA-KTONR.
           MOVE SU-MCODE TO WT-X.
           SET AY TO 7.
           PERFORM ALCON MOVE WT-X TO WT-TX.
           CALL "CAUP" USING "20CONV08" WH-CREG.
           MOVE WT-TX TO SA-MCODE.
           MOVE SU-BEZ TO WT-X.
           SET AY TO 40.
           PERFORM ALCON MOVE WT-X TO SA-BEZ.
           MOVE SU-ART TO SA-KTOART.
      *    MOVE SU-UST TO SA-UST.
           MOVE SU-BN TO SA-BTNT.
           MOVE SU-KAT TO SA-SH.
           DISPLAY SA-KTONR AT 1510 " " SA-BEZ.
           WRITE SA-SATZ INVALID GO C.
           GO C.
       X.  CLOSE SACHUEB.
       Z.  EXIT.
      ***************************************************** Kreditoren *
       KRED-UEBER SECTION.
       A.  OPEN INPUT KREDUEB.
           IF WF-STATUS > "00" GO Z.
       C.  READ KREDUEB NEXT AT END GO X.
           DISPLAY KU-KTONR AT 2220.
           IF WV-FNR NOT = 0 AND KU-FNR NOT = WV-FNR GO C.
           INITIALIZE KR-SATZ.
           MOVE KU-KTONR TO KR-KTONR.
           MOVE KU-MCODE TO WT-X.
           SET AY TO 7.
           PERFORM ALCON MOVE WT-X TO WT-TX.
           CALL "CAUP" USING "20CONV08" WH-CREG.
           MOVE WT-TX TO KR-MCODE.
      *    03  KR-KOND                 PIC 9(13)     COMP.
      *    03  KR-BANKKTO              PIC X(13).
      *    03  KR-BANKLTZ              PIC X(6).
           MOVE KU-KAT TO KR-KTOART.
           MOVE SPACE TO WT-ADR.
           PERFORM VARYING WY FROM 1 BY 1 UNTIL WY > 6
               MOVE KU-ADR(WY) TO WT-X
               IF WT-X NOT = SPACE SET AY TO 30
                   PERFORM ALCON MOVE WT-X TO WR-ADR(WY).
           PERFORM ABSTELLEN.
           MOVE WT-BEZ TO KR-BEZ.
           DISPLAY KR-KTONR AT 1510 " " KR-BEZ with SIZE 30.
           MOVE 0 TO KR-KTOART.
           WRITE KR-SATZ INVALID GO C.
           GO C.
       X.  CLOSE KREDUEB.
       Z.  EXIT.
      ****************************************************** Debitoren *
       DEB-UEBER SECTION.
       A.  OPEN INPUT DEBUEB.
           IF WF-STATUS > "00" GO Z.
       C.  READ DEBUEB NEXT AT END GO X.
           DISPLAY DU-KTONR AT 2220.
           IF WV-FNR NOT = 0 AND DU-FNR NOT = WV-FNR GO C.
           INITIALIZE DE-SATZ.
           MOVE DU-KTONR TO DE-KTONR.
           MOVE DU-MCODE TO WT-X.
           SET AY TO 7.
           PERFORM ALCON MOVE WT-X TO WT-TX.
           CALL "CAUP" USING "20CONV08" WH-CREG.
           MOVE WT-TX TO DE-MCODE.
           MOVE SPACE TO WT-ADR.
           PERFORM VARYING WY FROM 1 BY 1 UNTIL WY > 6
               MOVE DU-ADR(WY) TO WT-X
               IF WT-X NOT = SPACE SET AY TO 30
                   PERFORM ALCON MOVE WT-X TO WR-ADR(WY).
           PERFORM ABSTELLEN.
           MOVE WT-BEZ TO DE-BEZ.
           IF DE-BEZ = SPACE MOVE "*#####" TO DE-BEZ.
           MOVE DU-KAT TO DE-KTOART.
           DISPLAY DE-KTONR AT 1510 " " DE-BEZ with SIZE 30.
           MOVE 0 TO DE-KTOART.
           WRITE DE-SATZ INVALID GO C.
           GO C.
       X.  CLOSE DEBUEB.
       Z.  EXIT.
      *********************************************************** OP's *
       OP-UEBER SECTION.
       A.  OPEN INPUT OPUEB.
           move 0 to wh-anz.
           IF WF-STATUS > "00" GO Z.
       C.  READ OPUEB NEXT AT END GO X.
           DISPLAY UP-KTONR AT 2220 " " wh-anz.
           IF WV-FNR NOT = 0 AND UP-FNR NOT = WV-FNR GO C.
           add 1 to wh-anz.
           INITIALIZE OP-SATZ.
           MOVE UP-KTONR TO OP-KTONR WD-KZ.
           MOVE WD-KZ TO OP-DK.
           MOVE UP-REBET TO OP-REBETRAG.
           MOVE UP-ZABET TO OP-ZABETRAG.
           ADD UP-REBET UP-ZABET GIVING WH-WERT.
           IF WH-WERT = 0 GO C.
           MOVE UP-BEZUG TO OP-RENUM OP-REF.
           MOVE UP-PZ1 TO WH-SK1.
           MOVE UP-TG1 TO WH-TG1.
           MOVE UP-PZ2 TO WH-SK2.
           MOVE UP-TG2 TO WH-TG2.
           MOVE UP-TGN TO WH-TGN.
           MOVE WH-KK TO OP-KOND.
           MOVE UP-STUFE TO OP-MSTUFE.
           MOVE UP-BELDAT TO OP-REDAT.
           MOVE UP-VALDAT TO WZ-DATUM.
           MOVE WH-TGN TO WH-TAG.
           PERFORM FAELLIG.
           MOVE WZ-DATUM TO OP-VALDAT.
           EVALUATE UP-SYM                            *> Spitzer-Symbole
               WHEN "AR" MOVE 01 TO OP-SYM
                         IF WD-KZ = 2 MULTIPLY -1 BY OP-ZABETRAG
               WHEN "ER" MOVE 02 TO OP-SYM
                         IF WD-KZ = 3 MULTIPLY -1 BY OP-REBETRAG
               WHEN "GA" MOVE 03 TO OP-SYM
               WHEN "GE" MOVE 04 TO OP-SYM
                         IF WD-KZ = 3 MULTIPLY -1 BY OP-REBETRAG
               WHEN "PG" MOVE 05 TO OP-SYM
               WHEN "BA" MOVE 15 TO OP-SYM
                         IF WD-KZ = 2 MULTIPLY -1 BY OP-ZABETRAG.
           MOVE UP-ZUS TO OP-ZUSTAND.
           MOVE UP-UST TO OP-U1.
           IF OP-U1 > 6 MOVE 1 TO OP-U1.
           MOVE UP-SKTOBAS TO OP-B1 OP-SKTOBAS.
           IF OP-B1 = 0 AND OP-U1 > 0
               MOVE OP-REBETRAG TO OP-SKTOBAS OP-B1
               COMPUTE OP-MW = OP-REBETRAG * WT-UST(OP-U1) / 100
               SUBTRACT OP-MW FROM OP-B1.
           DISPLAY OP-KTONR AT 1710 " " OP-RENUM .
       W.  WRITE OP-SATZ INVALID add 1 to op-lfd GO W.
           GO C.
       X.  CLOSE OPUEB.
       Z.  EXIT.
      ******************************** errechnen des FÑlligkeitsdatums *
       FAELLIG SECTION.
       A.  IF WH-TAG = 0 GO Z.
       B.  IF WZ-TAG > WH-TAG SUBTRACT WH-TAG FROM WZ-TAG GO Z.
           SUBTRACT WZ-TAG FROM WH-TAG
           ADD -1 TO WZ-MONAT
           IF WZ-MONAT = 0 ADD -1 TO WZ-JAHR
               MOVE 12 TO WZ-MONAT
               MOVE WZ-DATUM TO WC-DATUM
               CALL "CAUP" USING "03DATUM" WH-CREG.
           MOVE WT-KON(WZ-MONAT) TO WZ-TAG.
           GO B.
       Z.  EXIT.
      *************************************************** Kundensalden *
       SAL-DEB SECTION.
       A.  OPEN INPUT SALDEB.
           IF WF-STATUS > "00" GO Z.
       C.  READ SALDEB NEXT AT END GO X.
           DISPLAY SD-KTONR AT 2220.
           IF SD-SALDO(1:) = LOW-VALUE GO C.
           IF SD-SALDO = 0 GO C.
           IF WV-FNR NOT = 0 AND SD-FNR NOT = WV-FNR GO C.
           MOVE SD-KTONR TO DE-KTONR BZ-KTONR.
       E.  READ DEBITOR INVALID DISPLAY DE-KTONR AT 0460 GO C.
           IF ZUGRIF GO E.
           DISPLAY DE-KTONR AT 1710 " " DE-BEZ with SIZE 30.
           MOVE SD-SALDO TO WH-WERT.
           PERFORM SALDOBUCH.
           ADD WM-SOLL TO DE-SOLL.
           ADD WM-HABEN TO DE-HABEN.
           REWRITE DE-SATZ INVALID GO C.
           GO C.
       X.  CLOSE SALDEB.
       Z.  EXIT.
      ****************************************************** Debitoren *
       SAL-KRED SECTION.
       A.  OPEN INPUT SALKRED.
           IF WF-STATUS > "00" GO Z.
       C.  READ SALKRED NEXT AT END GO X.
           DISPLAY SK-KTONR AT 2220.
           IF SK-SALDO(1:) = LOW-VALUE GO C.
           IF SK-SALDO = 0 GO C.
           IF WV-FNR NOT = 0 AND SK-FNR NOT = WV-FNR GO C.
           MOVE SK-KTONR TO KR-KTONR BZ-KTONR.
       E.  READ KREDIT INVALID DISPLAY KR-KTONR AT 0460 GO C.
           IF ZUGRIF GO E.
           DISPLAY KR-KTONR AT 1810 " " DE-BEZ with SIZE 30.
           MOVE SK-SALDO TO WH-WERT.
           PERFORM SALDOBUCH.
           ADD WM-SOLL TO KR-SOLL.
           ADD WM-HABEN TO KR-HABEN.
           REWRITE KR-SATZ INVALID GO C.
           GO C.
       X.  CLOSE SALKRED.
       Z.  EXIT.
      ******************************************************************
       SALDOBUCH SECTION.
       A.  MOVE BZ-SATZ(1:11) TO WH-MCODE.
           INITIALIZE BZ-SATZ.
           MOVE WH-MCODE TO BZ-SATZ(1:11).
           ADD 8000000 BZ-KTONR GIVING BZ-SEITE.   *> eindeutiger Key
           MOVE WE-WKZ TO BZ-USE.
           MOVE WE-WJAHR(1) TO BZ-BUDAT BZ-BELDAT.
           MOVE 0 TO BZ-LFD WM-SOLL WM-HABEN.
           MOVE 98 TO BZ-SYM.
           IF WH-WERT > 0 MOVE 0 TO BZ-SH
                          ADD WH-WERT TO WM-SOLL
                     else MOVE 1 TO BZ-SH
                          MULTIPLY -1 BY WH-WERT
                          ADD WH-WERT TO WM-HABEN.
           MOVE WH-WERT TO BZ-BUBET.
           MOVE BZ-SATZ TO WH-BZSATZ.
       C.  READ BUCHZEIL INVALID GO F.
           IF ZUGRIF GO C.
           MOVE BZ-BUBET TO WH-WERT.
           IF BZ-SH = 0 SUBTRACT WH-WERT FROM WM-SOLL
                   else SUBTRACT WH-WERT FROM WM-HABEN.
       F.  MOVE WH-BZSATZ TO BZ-SATZ.
           MOVE 68 TO WZ-BUCH.
           WRITE BZ-SATZ INVALID REWRITE BZ-SATZ.
       Z.  EXIT.
      ******************************************************************
       ALCON SECTION.
       A.  perform  varying ax from 1 by 1 until ax > ay
               move wx-x(ax) to iy
               if iy = 0 OR > 128 move iy to wx-x(ax)
               else move wr-cv(iy) to wx-x(ax).
       Z.  EXIT.
      ******************************************************************
       ABSTELLEN SECTION.
       A.  MOVE 30 TO WL-SZ.
           SET FX TO 6.
           SET BX TX TO 1.
           MOVE SPACE TO WT-BEZ.
       B.  MOVE WR-ADR(TX) TO WT-TX.
           SET TY TO WL-SZ.
           MOVE 1 TO WX.
       C.  IF WR-TX(TY) NOT = SPACE SET WX TO TY
               SET BX UP BY WX
               MOVE "#" TO WR-BEZ(BX)
               SET BX DOWN BY 1
               ADD 1 TO WX
               GO D.
           IF TY NOT = 1 SET TY DOWN BY 1 GO C.
           MOVE "#" TO WR-BEZ(BX).
           GO E.
       D.  MOVE WR-TX(TY) TO WR-BEZ(BX).
           IF TY NOT = 1 SET TY BX DOWN BY 1 GO D.
       E.  SET BX UP BY WX.
       F.  IF BX > 240 DISPLAY "Zuviele Zeichen eingegeben! " AT 2401
                DISPLAY "STOP" AT 2020 GO Z.
           IF TX < FX SET TX UP BY 1 GO B.
       Z.  EXIT.
