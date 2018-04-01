      $SET LINKCOUNT "240" ANS85 BOUND AUTOLOCK NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     FIBOPSU.
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
           03  WX-J                    PIC 9999.
           03  WD-KZ                   PIC 9.
           03  WH-BET                  PIC S9(6)V99   COMP-3.
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
      *------------------------------------------------> OP-Korrektur <-
       H.  MOVE "FIBUOPES.DAT" TO WN-OPES.
           OPEN I-O OPDATEI.
           IF WF-STATUS = "00" OR "05" GO W.
           DISPLAY "West-Star-Korrektur" AT 0529.
           DISPLAY LOW-VALUE AT 0601.
           MOVE HIGH-VALUES TO OP-SATZ.
           move 2 to op-DK.
           move 995003 to op-ktonr.
           START OPDATEI KEY < OP-KEY INVALID GO Z.
       K.  READ OPDATEI PREVIOUS AT END GO Q.
           IF OP-KTONR not = 995002 go k.
           IF OP-U2 = 0 and OP-B2 = 0 GO K.
           COMPUTE WH-BET = (OP-B1 * ,10) + (OP-B2 * ,20).
           IF OP-MW = WH-BET GO K.
      *    03  OP-U2                   PIC 99         COMP.
      *    03  OP-B2                   PIC S9(9)V99   COMP.
      *    03  OP-MW                   PIC S9(7)V99   COMP.

           DISPLAY OP-RENUM AT 2210.
           PERFORM WEITER.
           GO K.
       Q.
       W.  CLOSE OPDATEI.
       Z.  EXIT PROGRAM.
      ******************************************************************
       WEITER SECTION.
       A.  DISPLAY " weiter mit <ret>: " with highlight AT 0000.
           MOVE SPACE TO WH-X.
           ACCEPT WH-X AT 0000.
           CALL "CAUP" USING "1324012480000" WH-CREG.
       Z.  EXIT.
