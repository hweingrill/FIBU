      $SET LINKCOUNT "320" ANS85 BOUND AUTOLOCK NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     FIBOREG.
      ******************************************************************
       ENVIRONMENT    DIVISION.
       CONFIGURATION   SECTION.
       SOURCE-COMPUTER.  PC.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPDATEI  ASSIGN "FIBUOPES.DAT"
                           ORGANIZATION INDEXED, ACCESS DYNAMIC
                           RECORD KEY OP-KEY
                           ALTERNATE KEY OP-AKEY  DUPLICATES
                           FILE STATUS WF-STATUS.
           SELECT AOPDAT   ASSIGN "\FIB1112\FIBUOPES.DAT"
                           ORGANIZATION INDEXED, ACCESS DYNAMIC
                           RECORD KEY AP-KEY
                           ALTERNATE KEY AP-AKEY DUPLICATES
                           FILE STATUS WF-STATUS.
       DATA DIVISION.
       FILE SECTION.
      ***************************************************** OP - Datei *
       FD  OPDATEI      external       LABEL RECORD STANDARD.
       01  OP-SATZ.
           03  OP-AKEY.
               05 OP-SA                PIC 99         COMP.
               05 OP-KEY.
                  07 OP-DK             PIC 99         COMP.
                  07 OP-KTONR          PIC 9(6)       COMP.
                  07 OP-VALDAT         PIC 9(8)       COMP.
                  07 OP-LFD            PIC 9999       COMP.
           03  OP-REF                  PIC 9(7)       COMP.
           03  OP-RENUM                PIC 9(7)       COMP.
           03  OP-REDAT                PIC 9(8)       COMP.
           03  OP-SYM                  PIC 99         COMP.
           03  OP-ZUSTAND              PIC 99         COMP.
           03  OP-KOND                 PIC 9(13)      COMP.
           03  OP-MW                   PIC S9(7)V99   COMP.
           03  OP-U1                   PIC 99         COMP.
           03  OP-B1                   PIC S9(9)V99   COMP.
           03  OP-U2                   PIC 99         COMP.
           03  OP-B2                   PIC S9(9)V99   COMP.
           03  OP-REBETRAG             PIC S9(9)V99   COMP.
           03  OP-ZABETRAG             PIC S9(9)V99   COMP.
           03  OP-SKTOBAS              PIC S9(9)V99   COMP.
           03  OP-ZADAT                PIC 9(6)       COMP.
           03  OP-ZPBET                PIC S9(9)V99   COMP.
           03  OP-ZPSKTO               PIC S9(7)V99   COMP.
           03  OP-KURS                 PIC 9(4)V9(7)  COMP.
           03  OP-FSYM                 PIC XXX.              *> W„hrung
           03  OP-MSTUFE               PIC 99         COMP.
           03  OP-MAHNDAT              PIC 9(6)       COMP.
           03  OP-ZESS                 PIC 99         COMP.
           03  OP-FREMDNR              PIC X(15).
      ***************************************************** OP - Datei *
       FD  AOPDAT      external       LABEL RECORD STANDARD.
       01  AP-SATZ.
           03  AP-AKEY.
               05 AP-SA                PIC 99         COMP.
               05 AP-KEY.
                  07 AP-DK             PIC 99         COMP.
                  07 AP-KTONR          PIC 9(6)       COMP.
                  07 AP-VALDAT         PIC 9(8)       COMP.
                  07 AP-LFD            PIC 9999       COMP.
           03  AP-REF                  PIC 9(7)       COMP.
           03  AP-RENUM                PIC 9(7)       COMP.
           03  AP-REDAT                PIC 9(8)       COMP.
           03  AP-SYM                  PIC 99         COMP.
           03  AP-ZUSTAND              PIC 99         COMP.
           03  AP-KOND                 PIC 9(13)      COMP.
           03  AP-MW                   PIC S9(7)V99   COMP.
           03  AP-U1                   PIC 99         COMP.
           03  AP-B1                   PIC S9(9)V99   COMP.
           03  AP-U2                   PIC 99         COMP.
           03  AP-B2                   PIC S9(9)V99   COMP.
           03  AP-REBETRAG             PIC S9(9)V99   COMP.
           03  AP-ZABETRAG             PIC S9(9)V99   COMP.
           03  AP-SKTOBAS              PIC S9(9)V99   COMP.
           03  AP-ZADAT                PIC 9(6)       COMP.
           03  AP-ZPBET                PIC S9(9)V99   COMP.
           03  AP-ZPSKTO               PIC S9(7)V99   COMP.
           03  AP-KURS                 PIC 9(4)V9(7)  COMP.
           03  AP-FSYM                 PIC XXX.              *> W„hrung
           03  AP-MSTUFE               PIC 99         COMP.
           03  AP-MAHNDAT              PIC 9(6)       COMP.
           03  AP-ZESS                 PIC 99         COMP.
           03  AP-FREMDNR              PIC X(15).
      ******************************************************************
       WORKING-STORAGE SECTION.
       01  WH-CALL.
           03  WL-CA                   PIC 99.
           03  WL-REST                 PIC 9(13).
       COPY WHCREG.CPY.
       01  WH-REG.
           03  WX-J                    PIC 9(5).
           03  WA-USE                  PIC 99        COMP.
           03  WB-USE                  PIC 99        COMP.
           03  WH-MOD                  PIC 99        COMP.
           03  WD-SEITE                PIC ZZZZ.ZZZ-.
           03  WD-USE                  PIC Z9-.
           03  WM-BUCH                 PIC 9(5).
           03  WH-SEITE                PIC 9(7)      COMP-X.
           03  WD-KZ                   PIC 9.
           03  WK-CODE                 PIC 9999.
           03 WD-ANZ                   PIC 999999.
      *----------------------------------------------> Command-Line <-
           03  RESULT                  PIC 99         COMP-X.
           03  FUNKT                   PIC 99         COMP-X VALUE 35.
           03  PARAM.
               05 SUB                  PIC 99         COMP-X VALUE 0.
               05 PAR                  PIC X(6)       VALUE  SPACE.
       COPY FIBUEXT.CPY.
       DECL-K SECTION.         USE AFTER ERROR PROCEDURE ON OPDATEI.
       A.  CALL "CADECL" USING "FIBUKONS.DAT" WH-CREG.
       Z.  EXIT.
       END DECLARATIVES.
      *****************************************************************
       STEUER SECTION.
      *------------------------------------------------> OP-Korrektur <-
       H.  DISPLAY "Reorg O`PES" AT 0529.
           DISPLAY LOW-VALUE AT 0601.
           open i-o OPDATEI.
           OPEN INPUT AOPDAT.
           move 0 to wd-anz.
      *    START BUCHZEIL KEY > BZ-JKEY INVALID GO Q.
       K.  move space to OP-satz.
           READ AOPDAT next AT END GO Q.
           add 1 to wd-anz.
           move AP-satz to OP-satz.
           IF OP-KTONR(1:) = SPACE GO K.
           DISPLAY OP-KTONR AT 2210 " " wd-anz.
       n.  WRITE OP-satz INVALID go K.
           GO K.
       Q.  CLOSE OPDATEI.
       Z.  stop run.
      ******************************************************************
       WEITER SECTION.
       A.  DISPLAY " weiter mit <ret>: " with highlight AT 0000.
           MOVE SPACE TO WH-X.
           ACCEPT WH-X AT 0000.
           CALL "CAUP" USING "1324012480000" WH-CREG.
       Z.  EXIT.
