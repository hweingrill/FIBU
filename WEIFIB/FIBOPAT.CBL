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
           SELECT OPX      ASSIGN "OPS.DAT"
                           ORGANIZATION INDEXED, ACCESS DYNAMIC
                           RECORD KEY OX-KEY
                           ALTERNATE KEY OX-AKEY DUPLICATES
      *                     ALTERNATE KEY OX-RKEY =
      *                               OX-RENUM OX-KEY DUPLICATES
                           FILE STATUS WF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       COPY FIBUFD.CPY.
      ***************************************************** OP - Datei *
       FD  OPX          external       LABEL RECORD STANDARD.
       01  OX-SATZ.
           03  OX-AKEY.
               05 OX-SA                PIC 99         COMP.
               05 OX-KEY.
                  07 OX-DK             PIC 99         COMP.
                  07 OX-KTONR          PIC 9(6)       COMP.
                  07 OX-VALDAT         PIC 9(8)       COMP.
                  07 OX-LFD            PIC 9999       COMP.
           03  OX-REF                  PIC 9(7)       COMP.
           03  OX-RENUM                PIC 9(7)       COMP.
           03  OX-REDAT                PIC 9(8)       COMP.
           03  OX-SYM                  PIC 99         COMP.
           03  OX-ZUSTAND              PIC 99         COMP.
           03  OX-KOND                 PIC 9(13)      COMP.
           03  OX-MW                   PIC S9(7)V99   COMP.
           03  OX-U1                   PIC 99         COMP.
           03  OX-B1                   PIC S9(9)V99   COMP.
           03  OX-U2                   PIC 99         COMP.
           03  OX-B2                   PIC S9(9)V99   COMP.
           03  OX-REBETRAG             PIC S9(9)V99   COMP.
           03  OX-ZABETRAG             PIC S9(9)V99   COMP.
           03  OX-SKTOBAS              PIC S9(9)V99   COMP.
           03  OX-ZADAT                PIC 9(6)       COMP.
           03  OX-ZPBET                PIC S9(9)V99   COMP.
           03  OX-ZPSKTO               PIC S9(7)V99   COMP.
           03  OX-KURS                 PIC 9(4)V9(7)  COMP.
           03  OX-FSYM                 PIC XXX.              *> W„hrung
           03  OX-MSTUFE               PIC 99         COMP.
           03  OX-MAHNDAT              PIC 9(6)       COMP.
           03  OX-ZESS                 PIC 99         COMP.
           03  OX-FREMDNR              PIC X(15).
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
           OPEN INPUT OPX.
           DISPLAY "OP-AtsKorrektur" AT 0529.
           DISPLAY LOW-VALUE AT 0601.
           MOVE LOW-VALUES TO OP-SATZ.
           START OPX KEY NOT > OX-KEY INVALID GO Z.
       I.  READ OPX NEXT AT END GO Q.
           MOVE OX-SATZ TO OP-SATZ.
       K.  READ OPDATEI INVALID GO I.
           MOVE OX-U1 TO OP-U1.
           MOVE OX-U2 TO OP-U2.
           DISPLAY OP-RENUM AT 2210.
           REWRITE OP-SATZ.
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
