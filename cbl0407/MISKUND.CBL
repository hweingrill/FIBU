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
           SELECT DEBITOR  ASSIGN WN-DEBI
                           ORGANIZATION INDEXED, ACCESS DYNAMIC
                           RECORD KEY DE-KEY
                           ALTERNATE KEY DE-MCODE DUPLICATES
                           FILE STATUS WF-STATUS.
           SELECT DKSFIBU  ASSIGN TO WN-DKS
                           ORGANIZATION LINE SEQUENTIAL
                           FILE STATUS WF-STATUS.
           SELECT KUNDEN   ASSIGN TO "OA-KUNDE.DAT"
                           ORGANIZATION INDEXED, ACCESS DYNAMIC
                           RECORD KEY KD-KEY
                           ALTERNATE KEY KD-OEKV DUPLICATES
                           FILE STATUS WF-STATUS.
       DATA DIVISION.
       FILE SECTION.
      ******************************************************************
       FD  DEBITOR      external       LABEL RECORDS STANDARD.
       01  DE-SATZ.
           03  DE-KEY.
               05 DE-KTONR             PIC 9(6)      COMP.
           03  DE-MCODE                PIC X(8).
           03  DE-KTOART               PIC 99        COMP.
           03  DE-OPKZ                 PIC 99        COMP.
           03  DE-ANREDE               PIC 99        COMP.
           03  DE-SOLL                 PIC S9(9)V99  COMP-3.
           03  DE-HABEN                PIC S9(9)V99  COMP-3.
           03  DE-UMSATZ               PIC S9(9)V99  COMP-3.
           03  DE-KOND                 PIC 9(13)     COMP.
           03  DE-FWKZ                 PIC 99        COMP.
           03  DE-MAHNT                PIC 999       COMP.
           03  DE-MAHNDAT              PIC 9(6)      COMP.
           03  DE-BEZ                  PIC X(110).
           03  DE-TEL                  PIC X(35).
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
      ***************************** Bewegungss„tze fr Fibuberleitung *
       FD  DKSFIBU      external       LABEL RECORD STANDARD.
       01  DKS-KSATZ.
           03  DKS-KTONR               PIC X(6).
           03  DKS-MCODE               PIC X(8).
           03  DKS-ADR                 PIC X(30)     OCCURS 4.
           03  DKS-VKF                 PIC 9.                   *> Vkf
           03  DKS-KGRP                PIC 9.                   *> Kdn.
           03  DKS-SK1                 PIC 9V9.
           03  DKS-TG1                 PIC 99.
           03  DKS-SK2                 PIC 9V9.
           03  DKS-TG2                 PIC 99.
           03  DKS-TGN                 PIC 999.
      ******************************************************************
       WORKING-STORAGE SECTION.
       01  WH-CALL.
           03  WL-CA                   PIC 99.
           03  WL-REST                 PIC 9(13).
       COPY WHCREG.CPY.
       01  WH-REG.
           03  WX-J                    PIC 9999.
           03  WD-KZ                   PIC 9.
           03  WD-STAT                 PIC 99.
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
           03  WN-DKS                  PIC X(20).
      *----------------------------------------------> Command-Line <-
           03  RESULT                  PIC 99         COMP-X.
           03  FUNKT                   PIC 99         COMP-X VALUE 35.
           03  PARAM.
               05 SUB                  PIC 99         COMP-X VALUE 0.
               05 PAR                  PIC X(6)       VALUE  SPACE.
      *COPY FIBUEXT.CPY.
      *----------------------------------------------------------------*
       LINKAGE SECTION.
       01  WL-CALL                     PIC X(15).
       01  WL-CREG                     PIC X(1152).
       PROCEDURE DIVISION using WL-CALL WL-CREG.
      *END DECLARATIVES.
      *****************************************************************
       STEUER SECTION.
       A.  MOVE "FIBUDEBI.DAT" TO WN-DEBI.
           OPEN I-O DEBITOR KUNDEN.
           MOVE "KD.TXT" TO WN-DKS.
           OPEN OUTPUT DKSFIBU.
           DISPLAY SPACE UPON CRT.
           DISPLAY "šbergabe Kundenstam an MIS" AT 0529.
           DISPLAY LOW-VALUE AT 0601.
           MOVE LOW-VALUES TO DE-SATZ.
           START DEBITOR KEY NOT < DE-KEY INVALID GO E.
       C.  READ DEBITOR NEXT IGNORE LOCK AT END GO E.
           MOVE DE-KTONR TO KD-KTONR.
           MOVE " " TO KD-SA.
           READ KUNDEN IGNORE LOCK INVALID INITIALIZE KD-SATZ.
           INITIALIZE DKS-KSATZ.
           MOVE DE-KTONR TO DKS-KTONR.
           MOVE KD-STAT TO WD-STAT.
           MOVE WD-STAT(2:1) TO DKS-VKF.
           MOVE WD-STAT(1:1) TO DKS-KGRP.
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
           DISPLAY DKS-KTONR AT 1810.
           WRITE DKS-KSATZ.
           GO C.
       E.  CLOSE DKSFIBU.
       Z.  EXIT PROGRAM.
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
       Z.  EXIT.
      ******************************************************************
       WEITER SECTION.
       A.  DISPLAY " weiter mit <ret>: " with highlight AT 0000.
           MOVE SPACE TO WH-X.
           ACCEPT WH-X AT 0000.
           CALL "CAUP" USING "1324012480000" WH-CREG.
       Z.  EXIT.
