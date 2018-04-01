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
           SELECT KREDIT   ASSIGN WN-KRED
                           ORGANIZATION INDEXED, ACCESS DYNAMIC
                           RECORD KEY KR-KEY
                           ALTERNATE KEY KR-MCODE DUPLICATES
                           FILE STATUS WF-STATUS.
           SELECT DKSFIBU  ASSIGN TO WN-DKS
                           ORGANIZATION LINE SEQUENTIAL
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
      *****************************************************************
       FD  KREDIT       external       LABEL RECORD STANDARD.
       01  KR-SATZ.
           03  KR-KEY.
               05  KR-KTONR            PIC 9(7)      COMP.
           03  KR-MCODE                PIC X(8).
           03  KR-KTOART               PIC 99        COMP.
           03  KR-OPKZ                 PIC 99        COMP.
           03  KR-SOLL                 PIC S9(9)V99  COMP-3.
           03  KR-HABEN                PIC S9(9)V99  COMP-3.
           03  KR-UMSATZ               PIC S9(9)V99  COMP-3.
           03  KR-UEWTRKZ              PIC 99        COMP.
           03  KR-KOND                 PIC 9(13)     COMP.
           03  KR-FWKZ                 PIC 99        COMP.
           03  KR-ZUS                  PIC 99        COMP.
           03  KR-BANKKTO              PIC X(13).
           03  KR-BANKLTZ              PIC X(6).
           03  KR-BEZ                  PIC X(110).
           03  KR-BANK                 PIC X(24).
      ***************************** Bewegungss„tze fr Fibuberleitung *
       FD  DKSFIBU      external       LABEL RECORD STANDARD.
       01  DKS-KSATZ.
           03  DKS-KTONR               PIC 9(6).
           03  DKS-TA                  PIC X.
           03  DKS-ART                 PIC 9.
           03  DKS-TA                  PIC X.
           03  DKS-FWKZ                PIC 99.
           03  DKS-TB                  PIC X.
           03  DKS-UID                 PIC 9(14).
           03  DKS-TC                  PIC X.
           03  DKS-ANR                 PIC XX.
           03  DKS-TD                  PIC X.
           03  DKS-NAME                PIC X(40).
           03  DKS-TE                  PIC X.
           03  DKS-STR                 PIC X(25).
           03  DKS-TF                  PIC X.
           03  DKS-ORT                 PIC X(25).
           03  DKS-TG                  PIC X.
           03  DKS-MSP                 PIC X.              *> immer "X"
           03  DKS-TH                  PIC X.
           03  DKS-TG1                 PIC 99.
           03  DKS-TI                  PIC X.
           03  DKS-TG2                 PIC 99.
           03  DKS-TJ                  PIC X.
           03  DKS-TGN                 PIC 99.
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
       A.  CALL "CAUP" USING "9901012480000" WH-CREG.
           CALL "CAUP" USING "270101228003000" WH-CREG.
           ACCEPT WZ-DATUM FROM DATE.
           CALL "CAUP" USING "03DATUM" WH-CREG.
           MOVE "FIBUDEBI.DAT" TO WN-DEBI.
           MOVE "FIBUKRED.DAT" TO WN-KRED.
           OPEN I-O DEBITOR.
           OPEN I-O KREDIT.
           MOVE "RZL.TXT" TO WN-DKS.
           OPEN OUTPUT DKSFIBU.
           DISPLAY "šbergabe Kunden an RZL" AT 0529.
           DISPLAY LOW-VALUE AT 0601.
           MOVE LOW-VALUES TO DE-SATZ.
           START DEBITOR KEY NOT < DE-KEY INVALID GO E.
       C.  READ DEBITOR NEXT IGNORE LOCK AT END GO E.
           MOVE all ";" TO DKS-KSATZ.
           MOVE DE-KTONR TO WD-KZ.
           COMPUTE DKS-KTONR = DE-KTONR / 10 + (WD-KZ * 100000).
           MOVE 5 TO DKS-ART.
           MOVE "X" TO DKS-MSP.
           MOVE DE-KOND TO WH-KK.
           MOVE WH-TG1 TO DKS-TG1.
           MOVE WH-TG2 TO DKS-TG2.
           MOVE WH-TGN TO DKS-TGN.
           MOVE DE-BEZ TO WT-BEZ.
           PERFORM ADRESSE.
           MOVE 0 TO DKS-FWKZ DKS-UID DKS-ANR.
           DISPLAY DKS-KTONR AT 0560.
           WRITE DKS-KSATZ.
           GO C.
       E.  DISPLAY "šbergabe Lieferanten an RZL" AT 0629.
           DISPLAY LOW-VALUE AT 0701.
           MOVE LOW-VALUES TO KR-SATZ.
           START KREDIT KEY NOT < KR-KEY INVALID GO X.
       G.  READ KREDIT NEXT IGNORE LOCK AT END GO X.
           MOVE ALL ";" TO DKS-KSATZ.
           MOVE KR-KTONR TO WD-KZ.
           COMPUTE DKS-KTONR = KR-KTONR / 10 + (WD-KZ * 100000).
           MOVE 9 TO DKS-ART.
           MOVE "X" TO DKS-MSP.
           MOVE KR-KOND TO WH-KK.
           MOVE WH-TG1 TO DKS-TG1.
           MOVE WH-TG2 TO DKS-TG2.
           MOVE WH-TGN TO DKS-TGN.
           MOVE KR-BEZ TO WT-BEZ.
           PERFORM ADRESSE.
           DISPLAY DKS-KTONR AT 0660.
           MOVE 0 TO DKS-FWKZ DKS-UID DKS-ANR.
           WRITE DKS-KSATZ.
           GO G.
       X.  CLOSE DKSFIBU DEBITOR KREDIT.
       Z.  STOP RUN.
      ******************************************************************
       ADRESSE SECTION.
       A.  MOVE SPACE TO WT-ADR.
           MOVE "#" TO WR-BEZ(131).
           UNSTRING WT-BEZ DELIMITED BY "#" INTO WR-ADR(1) WR-ADR(2)
               WR-ADR(3) WR-ADR(4) WR-ADR(5)
           IF WR-ADR(5) = ".  " MOVE SPACE TO WR-ADR(5).
           IF WR-ADR(4) = ".  " MOVE SPACE TO WR-ADR(4).
           IF WR-ADR(3) = ".  " MOVE SPACE TO WR-ADR(3).
           MOVE WR-ADR(1) TO DKS-NAME.
           IF WR-ADR(5) not = SPACE MOVE WR-ADR(5) TO DKS-ORT
               IF WR-ADR(4) not = SPACE MOVE WR-ADR(4) TO DKS-STR
               end-if
               GO X.
           IF WR-ADR(4) not = SPACE MOVE WR-ADR(4) TO DKS-ORT
               IF WR-ADR(3) not = SPACE MOVE WR-ADR(3) TO DKS-STR
               end-if
               GO X.
           MOVE WR-ADR(3) TO DKS-ORT.
           MOVE WR-ADR(2) TO DKS-STR.
       X.  IF DKS-ORT(1:2) = ";;" MOVE SPACE TO DKS-ORT.
           IF DKS-STR(1:2) = ";;" MOVE SPACE TO DKS-STR.
       Z.  EXIT.
      ******************************************************************
       WEITER SECTION.
       A.  DISPLAY " weiter mit <ret>: " with highlight AT 0000.
           MOVE SPACE TO WH-X.
           ACCEPT WH-X AT 0000.
           CALL "CAUP" USING "1324012480000" WH-CREG.
       Z.  EXIT.
