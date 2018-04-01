      $SET LINKCOUNT "128" ANS85 BOUND AUTOLOCK NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.        FIBU.
      ******************************************************************
      *  Datumsfelder: WM-DATUM = gewÑhltes Tagesdatum JJJJMMTT        *
      *                WX-DATUM = aus Datumseingaben mit JJJJMMTT      *
      ******************************************************************
       ENVIRONMENT    DIVISION.
       CONFIGURATION   SECTION.
       SOURCE-COMPUTER.     pc.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.
      ******************************************************************
       WORKING-STORAGE SECTION.
       COPY WHCREG.CPY.
       01  WT-CALL                     PIC X(8).
       PROCEDURE DIVISION.
      *****************************************************************
       STEUER SECTION.
       A.  CALL "CAUP" USING "9901012480000" WH-CREG.
           CALL "CAUP" USING "270101228003000" WH-CREG.
           ACCEPT WZ-DATUM FROM DATE.
           ACCEPT WH-MCODE FROM COMMAND-LINE.
           CALL "CAUP" USING "03DATUM" WH-CREG.
       B.  CALL "FIBVORL" USING "99VORLAUF" WH-CREG.
           CANCEL "FIBVORL".
       C.  MOVE WT-TX TO WT-CALL.
           IF WT-TX NOT = SPACE CALL WT-CALL USING WT-TX(9:15) WH-CREG
                                CANCEL WT-CALL.
           IF WH-PG = 49 GO C.
           IF WH-PG = 48 MOVE 0 TO WH-PG          *> DatumsrÅckstellung
               ACCEPT WZ-DATUM FROM DATE
               CALL "CAUP" USING "03DATUM" WH-CREG.
           MOVE 9 TO WH-PG.
           GO B.
       Z.  EXIT.
