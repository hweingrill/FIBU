      $SET LINKCOUNT "320" ANS85 BOUND AUTOLOCK NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     FIBKREG.
      ******************************************************************
       ENVIRONMENT    DIVISION.
       CONFIGURATION   SECTION.
       SOURCE-COMPUTER.  PC.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY FIBUSEC.CPY.
           SELECT BANKDAT   ASSIGN TO "FIBUBANK.DAT"
                            ORGANIZATION INDEXED ACCESS DYNAMIC
                            RECORD KEY BK-KEY
                            FILE STATUS IS WF-STATUS.
           SELECT DKSFIBU  ASSIGN TO "DKSFIBU.DAT"
                           ORGANIZATION LINE SEQUENTIAL
                           FILE STATUS WF-STATUS.
           SELECT KUNDEN   ASSIGN TO "OA-KUNDE.DAT"
                           ORGANIZATION INDEXED, ACCESS DYNAMIC
                           RECORD KEY KD-KEY
                           ALTERNATE KEY KD-OEKV DUPLICATES
                           FILE STATUS WF-STATUS.
           SELECT DRUCKER  ASSIGN TO PRINTER "dksdru.lst"
                           ORGANIZATION RECORD SEQUENTIAL
                           FILE STATUS WF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       COPY FIBUFD.CPY.
      ******************************************************************
       FD  DRUCKER                     LABEL RECORD STANDARD.
       01  DRA-SATZ.
           03  FILLER                  PIC XXXX.
           03  DRA-STR.
               05 DRA-KTONR            PIC ZZZZZZ,Z-.
               05 DRA-BELNR            PIC ZZZZZZ-.
               05 FILLER               PIC X.
               05 DRA-BELDAT           PIC X(8).
               05 DRA-MWSKT            PIC ZZZZZ.ZZZ,ZZ-.
               05 DRA-SOLL             PIC ZZ.ZZZ.ZZZ,ZZ-.
               05 DRA-HABEN            PIC ZZ.ZZZ.ZZZ,ZZ-.
               05 DRA-DIF              PIC ZZ.ZZZ.ZZZ,ZZ-.
           03  FILLER                  PIC X.
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
       FD  KUNDEN       external       LABEL RECORDS STANDARD.
       01  KD-SATZ.
           03  KD-KEY.
               05 KD-SA                PIC X.
               05 KD-KTONR             PIC 9(6)       COMP.
           03  KD-STAT                 PIC 99         COMP.
      *------------------------------> 0 = keine Stat. 1 - 6 = Gruppe <-
           03  KD-RAB                  PIC S99V99     COMP  OCCURS 64.
           03  KD-ZRAB                 PIC S99V99     COMP  OCCURS 64.
           03  KD-STVGT                PIC S99V99     COMP.
           03  KD-FRAPZ                PIC S99V99     COMP.
           03  KD-FRACHT               PIC S9(4)V99   COMP.
           03  KD-PRVPZ                PIC S99V99     COMP.
           03  KD-STBRF                PIC 99         COMP.
           03  KD-STAND                PIC S9(7)V99   COMP.
           03  KD-OEKV                 PIC 9(8)       COMP.
           03  KD-LIMIT                PIC 9999       COMP.
           03  KD-VORM                 PIC X(240).
      ***************************** BewegungssÑtze fÅr FibuÅberleitung *
       FD  DKSFIBU      external       LABEL RECORD STANDARD.
       01  DKS-BSATZ.
      *----------------------- DKS-SA= "B" ---------------> Buchungen <-
           03  FILLER                  PIC X.
           03  FILLER                  PIC 9(6).
      *    03  DKS-SA                  PIC X.
      *    03  DKS-KTONR               PIC X(6).
           03  DKS-RENR                PIC X(7).
           03  DKS-DATUM               PIC 9(6).
           03  DKS-SYMBOL              PIC 99.      *>RE / GU
           03  DKS-GEGENKTO            PIC X(6).
           03  DKS-Z                   PIC 9.       *> ohne Verwendg.
           03  DKS-VM                  PIC 9.       *>1= Vst./ 2= Mwst.
           03  DKS-REFENZNR            PIC X(7).
           03  DKS-UST-KZ1             PIC 9.        *>0=0% 1=10% 2=20%
           03  DKS-UST-KZ2             PIC 9.        *>0=0% 1=10% 2=20%
           03  DKS-SH                  PIC 9.        *>0= SOLL 1= HABEN
           03  DKS-UST                 PIC S9(7)V99 SIGN TRAILING
                                                         SEPARATE.
           03  DKS-UST-BASIS1          PIC S9(9)V99 SIGN TRAILING
                                                         SEPARATE.
           03  DKS-UST-BASIS2          PIC S9(9)V99 SIGN TRAILING
                                                         SEPARATE.
           03  DKS-BETRAG              PIC S9(9)V99 SIGN TRAILING
                                                         SEPARATE.
           03  DKS-MENGE               PIC 9(9)V99.
           03  DKS-VALDAT              PIC 9(6).     *>Valutadatum
           03  DKS-TX                  PIC X(40).
      *--------------------- DKS-SA= "K" -----------> KontenÅbernahme <-
       01  DKS-KSATZ.
           03  DKS-SA                  PIC X.
           03  DKS-KTONR               PIC X(6).
           03  DKS-MCODE               PIC X(8).
           03  DKS-ADR                 PIC X(30)     OCCURS 4.
           03  DKS-PLZL                PIC 9999.
           03  DKS-BKLZL               PIC X(6).
           03  DKS-BANK                PIC X(24).
           03  DKS-BKTO                PIC X(13).
           03  DKS-SK1                 PIC 9V9.
           03  DKS-TG1                 PIC 99.
           03  DKS-SK2                 PIC 9V9.
           03  DKS-TG2                 PIC 99.
           03  DKS-TGN                 PIC 999.
           03  DKS-OEKV                PIC 9(8).          *> OEKV-Nr.
           03  DKS-LIMIT               PIC 9999.          *> in Tausend
      ******************************************************************
       WORKING-STORAGE SECTION.
       01  WH-CALL.
           03  WL-CA                   PIC 99.
           03  WL-REST                 PIC 9(13).
       COPY WHCREG.CPY.
       01  WH-REG.
           03  WX-J                    PIC 9999.
           03  WD-KZ                   PIC 9.
           03  WH-KK                   PIC 9(13).
           03  WR-KK REDEFINES WH-KK.
               05  WH-SK1              PIC 9V9.
               05  WH-TG1              PIC 999.
               05  WH-SK2              PIC 9V9.
               05  WH-TG2              PIC 999.
               05  WH-TGN              PIC 999.
           03  wm-soll                 pic s9(9)v99.
           03  wm-haben                pic s9(9)v99.
           03  wd-soll                 pic zzz.zzz.zzz,99-.
           03  wd-haben                pic zzz.zzz.zzz,99-.
           03  wd-dif                  pic zzz.zzz.zzz,99-.
      *----------------------------------------------> Command-Line <-
           03  RESULT                  PIC 99         COMP-X.
           03  FUNKT                   PIC 99         COMP-X VALUE 35.
           03  PARAM.
               05 SUB                  PIC 99         COMP-X VALUE 0.
               05 PAR                  PIC X(6)       VALUE  SPACE.
       COPY FIBUEXT.CPY.
       DECL-K SECTION.         USE AFTER ERROR PROCEDURE ON KONSTANT.
       A.  CALL "CADECL" USING "FIBUKONS.DAT" WH-CREG.
       Z.  EXIT.
       END DECLARATIVES.
      *****************************************************************
       STEUER SECTION.
       A.  MOVE "FIBUDEBI.DAT" TO WN-DEBI.
           MOVE "FIBUKRED.DAT" TO WN-KRED.
           OPEN I-O DEBITOR KREDIT KUNDEN BANKDAT.
           OPEN OUTPUT DKSFIBU.
           DISPLAY SPACE UPON CRT.
           DISPLAY "öbergabe Fibustam an DKS" AT 0529.
           DISPLAY LOW-VALUE AT 0601.
           MOVE LOW-VALUES TO DE-SATZ KR-SATZ.
           GO G.
           START KREDIT KEY NOT < KR-KEY INVALID GO E.
           START DEBITOR KEY NOT < DE-KEY INVALID GO E.
       C.  READ DEBITOR NEXT IGNORE LOCK AT END GO E.
           MOVE DE-KTONR TO KD-KTONR.
           MOVE " " TO KD-SA.
           READ KUNDEN IGNORE LOCK INVALID INITIALIZE KD-SATZ.
           INITIALIZE DKS-KSATZ.
           MOVE "K" TO DKS-SA.
           MOVE DE-KTONR TO DKS-KTONR.
           MOVE KD-OEKV TO DKS-OEKV.
           MOVE KD-LIMIT TO DKS-LIMIT.
           MOVE DE-KOND TO WH-KK.
           MOVE WH-SK1 TO DKS-SK1.
           MOVE WH-SK2 TO DKS-SK2.
           MOVE WH-TG1 TO DKS-TG1.
           MOVE WH-TG2 TO DKS-TG2.
           MOVE WH-TGN TO DKS-TGN.
           MOVE DE-MCODE TO WT-TX.
           CALL "CAUP" USING "21CONV08" WH-CREG.
           MOVE WT-TX TO DKS-MCODE.
           MOVE DE-BEZ TO WT-BEZ.
           PERFORM ADRESSE.
           IF DE-OPKZ > 19 PERFORM EINZUGBANK.
           DISPLAY DKS-KTONR AT 1810.
           WRITE DKS-KSATZ.
           GO C.
       E.  READ KREDIT NEXT AT END GO G.
           INITIALIZE DKS-KSATZ.
           MOVE "K" TO DKS-SA.
           MOVE KR-KTONR TO DE-KTONR.
           MOVE DE-KTONR TO DKS-KTONR.
           MOVE KR-MCODE TO WT-TX.
           CALL "CAUP" USING "21CONV08" WH-CREG.
           MOVE WT-TX TO DKS-MCODE.
           MOVE KR-BEZ TO WT-BEZ.
           PERFORM ADRESSE.
           MOVE KR-BANK TO DKS-BANK.
           MOVE KR-BANKKTO TO DKS-BKTO.
           MOVE KR-BANKLTZ(1:) TO DKS-BKLZL(1:).
           MOVE KR-KOND TO WH-KK.
           MOVE WH-SK1 TO DKS-SK1.
           MOVE WH-SK2 TO DKS-SK2.
           MOVE WH-TG1 TO DKS-TG1.
           MOVE WH-TG2 TO DKS-TG2.
           MOVE WH-TGN TO DKS-TGN.
           MOVE WH-TGN TO DKS-TGN.
           DISPLAY DKS-KTONR AT 1910.
           WRITE DKS-KSATZ.
           GO E.
      *------------------------------------> einlesen FibuÅberleitung <-
       G.  MOVE "FIBUEBER.DAT" TO WN-BUEB.
           MOVE 4 TO WR.
           move 0 to wm-soll wm-haben.
           OPEN I-O BUCHUNG.
           OPEN OUTPUT DRUCKER.
           MOVE LOW-VALUE TO BU-SATZ.
       H.  READ BUCHUNG NEXT AT END GO W.
           MOVE BU-KTONR TO WD-KZ.
           if bu-sa = "K" go h.
           IF BU-DAT < 000400 GO H.
           MOVE BU-DAT TO WZ-DATUM.
           IF WR not = WZ-MONAT GO H.
       K.  MOVE "B" TO DKS-SA.
           MOVE BU-KTONR TO DKS-KTONR.
           MOVE BU-REN TO DKS-RENR.
           MOVE BU-REN TO DKS-REFENZNR.
           MOVE BU-DAT TO DKS-DATUM.
           IF BU-SY > 50 ADD -50 TO BU-SY.
           MOVE BU-SY TO DKS-SYMBOL.
           IF BU-SH > 4 ADD -5 TO BU-SH.
           MOVE BU-GK TO DKS-GEGENKTO.
           MOVE 0 TO DKS-UST-KZ2 DKS-Z.
           EVALUATE BU-VM                          *> 1 = Vst, 2 = Mwst
               WHEN 1 MOVE 2 TO DKS-VM
               WHEN 2 MOVE 1 TO DKS-VM.
           EVALUATE BU-U1
              WHEN 1 MOVE 2 TO DKS-UST-KZ1
              WHEN 2 MOVE 1 TO DKS-UST-KZ1.
           EVALUATE BU-U2
              WHEN 1 MOVE 2 TO DKS-UST-KZ2
              WHEN 2 MOVE 1 TO DKS-UST-KZ2.
           MOVE BU-MW TO DKS-UST.
           MOVE BU-B1 TO DKS-UST-BASIS1.
           MOVE BU-B2 TO DKS-UST-BASIS2.
           MOVE BU-SH TO DKS-SH.
           MOVE BU-BET TO DKS-BETRAG.
           MOVE BU-TX TO DKS-TX.
           MOVE BU-VALDAT TO DKS-VALDAT.
           DISPLAY DKS-BSATZ(1:30) AT 2020.
           WRITE DKS-BSATZ.
           MOVE SPACE TO DRA-SATZ.
           if bu-sh = 0 add bu-bet to wm-soll
                        move bu-bet to dra-soll
                        add bu-mw to wm-haben
                        move bu-mw to dra-haben
                   ELSE add bu-bet to wm-haben
                        move bu-bet to dra-haben
                        add bu-mw to wm-soll
                        move bu-mw to dra-soll.
           move wm-soll to wd-soll.
           move wm-haben to wd-haben.
           subtract wm-haben from wm-soll giving wd-dif dra-dif.
           display wd-soll at 1010 " " wd-haben " " wd-dif.
           DIVIDE 10 INTO BU-KTONR GIVING DRA-ktonr.
           MOVE BU-REN TO DRA-belnr.
           MOVE BU-DAT TO DRA-beldat.
           write dra-satz after 1.
           GO H.
       W.  CLOSE BUCHUNG.
           move 0 to wm-soll wm-haben.
           move space to dra-satz.
           write dra-satz after 2.
           IF WR NOT = 4 ADD 1 TO WR
                         OPEN INPUT BUCHUNG
                         GO H.
           CLOSE DKSFIBU.
           close drucker.
       Z.  stop run.
      ******************************************************************
       ADRESSE SECTION.
       A.  MOVE SPACE TO WT-ADR.
           MOVE "#" TO WR-BEZ(131).
           UNSTRING WT-BEZ DELIMITED BY "#" INTO WR-ADR(1) WR-ADR(2)
               WR-ADR(3) WR-ADR(4) WR-ADR(5) WR-ADR(6).
           IF WR-ADR(5) = ".  " MOVE SPACE TO WR-ADR(5).
           IF WR-ADR(4) = ".  " MOVE SPACE TO WR-ADR(4).
           IF WR-ADR(3) = ".  " MOVE SPACE TO WR-ADR(3).
           MOVE 3 TO WZ.
           IF WR-ADR(5) not = SPACE MOVE WR-ADR(5) TO DKS-ADR(4)
                                    MOVE WR-ADR(4) TO DKS-ADR(3)
                                    IF DKS-ADR(3) not = SPACE
                                        MOVE 2 TO WZ
                                    end-if
           else IF WR-ADR(4) not = SPACE MOVE WR-ADR(4) TO DKS-ADR(4)
                                    MOVE WR-ADR(3) TO DKS-ADR(3)
                                    IF DKS-ADR(3) not = SPACE
                                        MOVE 2 TO WZ
                                    end-if
              else IF WR-ADR(3) not = SPACE MOVE WR-ADR(3) TO DKS-ADR(4)
                                            MOVE 2 TO WZ.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > WZ
               IF WR-ADR(WX) not = SPACE
                   MOVE WR-ADR(WX) TO DKS-ADR(WX).
           MOVE DKS-ADR(4) TO WT-TX.
       Q.  IF WT-TX(1:4) IS NUMERIC and WT-TX(5:2) = "  "
               MOVE WT-TX(6:) TO WT-TX(5:) GO Q.
           MOVE WT-TX TO DKS-ADR(4).
           IF WT-TX(1:4) IS NUMERIC MOVE WT-TX(1:4) TO WH-NUM
               MOVE WH-NUM TO DKS-PLZL.
       Z.  EXIT.
      ************************************************** Einzugsbanken *
       EINZUGBANK SECTION.
       A.  MOVE DE-KTONR TO BK-KTONR.
           MOVE 2 TO BK-DK.
       C.  READ BANKDAT INVALID MOVE SPACE TO BK-BANK
                                              BK-BANKKTO BK-BANKLTZ.
           MOVE BK-BANKKTO TO DKS-BKTO.
           MOVE BK-BANKLTZ TO DKS-BKLZL.
           MOVE BK-BANK TO DKS-BANK.
       Z.  EXIT.
      ******************************************************************
       WEITER SECTION.
       A.  DISPLAY " weiter mit <ret>: " with highlight AT 0000.
           MOVE SPACE TO WH-X.
           ACCEPT WH-X AT 0000.
           CALL "CAUP" USING "1324012480000" WH-CREG.
       Z.  EXIT.
