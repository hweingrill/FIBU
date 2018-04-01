      $SET LINKCOUNT "240" ANS85 BOUND AUTOLOCK NOALTER
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
           SELECT AOPDATEI ASSIGN TO "FIBUOPAL.DAT"
                           ORGANIZATION INDEXED, ACCESS DYNAMIC
                           RECORD KEY AOP-KEY
                           FILE STATUS WF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       COPY FIBUFD.CPY.
      ******************************************************************
       FD  AOPDATEI     external       LABEL RECORD STANDARD.
       01  AOP-SATZ.
           03  AOP-KEY.
               05 AOP-DK                PIC 99         COMP.
               05 AOP-KTONR             PIC 9(6)       COMP.
               05 AOP-VALDAT            PIC 9(8)       COMP.
               05 AOP-LFD               PIC 99         COMP-X.
           03  AOP-REF                  PIC 9(7)       COMP.
           03  AOP-RENUM                PIC 9(7)       COMP.
           03  AOP-REDAT                PIC 9(6)       COMP.
           03  AOP-SYM                  PIC 99         COMP.
           03  AOP-ZUSTAND              PIC 9          COMP.
           03  AOP-KOND                 PIC 9(13)      COMP.
           03  AOP-MW                   PIC S9(7)V99   COMP-3.
           03  AOP-U1                   PIC 99         COMP.
           03  AOP-B1                   PIC S9(9)V99   COMP-3.
           03  AOP-U2                   PIC 99         COMP.
           03  AOP-B2                   PIC S9(9)V99   COMP-3.
           03  AOP-REBETRAG             PIC S9(9)V99   COMP-3.
           03  AOP-ZABETRAG             PIC S9(9)V99   COMP-3.
           03  AOP-SKTOBAS              PIC S9(9)V99   COMP-3.
           03  AOP-ZADAT                PIC 9(6)       COMP.
           03  AOP-ZPBET                PIC S9(9)V99   COMP-3.
           03  AOP-ZPSKTO               PIC S9(7)V99   COMP-3.
           03  AOP-FW-KURS              PIC 9(4)V9999  COMP.
           03  AOP-MSTUFE               PIC 9          COMP.
           03  AOP-MAHNDAT              PIC 9(6)       COMP.
           03  AOP-ZESS                 PIC 99         COMP.
           03  AOP-FREMDNR              PIC X(15).
      ******************************************************************
       WORKING-STORAGE SECTION.
       01  WH-CALL.
           03  WL-CA                   PIC 99.
           03  WL-REST                 PIC 9(13).
       COPY WHCREG.CPY.
       01  WH-REG.
           03  WX-J                    PIC 9999.
           03  WD-KZ                   PIC 9.
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
           DISPLAY "Reorg Offene Posten" AT 0529.
           DISPLAY LOW-VALUE AT 0601.
           DISPLAY "COPY fibuopes.* fibuopal.*" UPON COMMAND-LINE.
           CALL X"91" USING RESULT FUNKT PARAM.
           IF RESULT NOT = 0 DISPLAY "Fehler!!!" at 1036 STOP RUN.
           IF RETURN-CODE NOT = 0 STOP RUN.
           OPEN INPUT AOPDATEI.
           IF WF-STATUS > "00" STOP RUN.
           OPEN OUTPUT OPDATEI.
           MOVE 0 TO AOP-KTONR AOP-REF AOP-LFD WX-J.
           START AOPDATEI KEY NOT < AOP-KEY INVALID GO Z.
       K.  READ AOPDATEI NEXT AT END GO Q.
           MOVE AOP-SATZ(1:) TO OP-SATZ(2:).
           DISPLAY OP-KTONR AT 2210 " " WX-J.
           MOVE 0 TO OP-SA.
           IF OP-REBETRAG = OP-ZABETRAG MOVE OP-DK TO OP-SA.
       M.  WRITE OP-SATZ INVALID STOP RUN.
           GO K.
       Q.  CLOSE AOPDATEI.
           DISPLAY "del fibuopal.*" UPON COMMAND-LINE.
           CALL X"91" USING RESULT FUNKT PARAM.
       W.  CLOSE OPDATEI.
       Z.  EXIT PROGRAM.
      ******************************************************************
       WEITER SECTION.
       A.  DISPLAY " weiter mit <ret>: " with highlight AT 0000.
           MOVE SPACE TO WH-X.
           ACCEPT WH-X AT 0000.
           CALL "CAUP" USING "1324012480000" WH-CREG.
       Z.  EXIT.
