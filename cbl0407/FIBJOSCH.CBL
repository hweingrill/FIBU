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
      *COPY FIBUSEC.CPY.
           SELECT BUCHZEIL ASSIGN "FIBUZEIL.DAT"
                           ORGANIZATION RECORD SEQUENTIAL
      *                    ORGANIZATION INDEXED, ACCESS DYNAMIC
      *                    RECORD KEY BZ-KEY
      *                    ALTERNATE KEY IS BZ-JKEY
                           FILE STATUS WF-STATUS.
           SELECT BxZEIL   ASSIGN "fibneu.dat"
                           ORGANIZATION INDEXED, ACCESS DYNAMIC
                           RECORD KEY Bx-KEY
                           ALTERNATE KEY IS Bx-JKEY
                           FILE STATUS WF-STATUS.
       DATA DIVISION.
       FILE SECTION.
      *COPY FIBUFD.CPY.
      ************************************* Journal- u. Buchungszeilen *
       FD  BUCHZEIL     external       RECORD VARYING FROM 68 TO 115
                                       DEPENDING ON WZ-BUCH.
       01  BZ-SATZ.
           03  BZ-KEY.
               05 BZ-KTONR             PIC 9(6)      COMP.
               05 BZ-BUDAT             PIC 9(8)      COMP.
               05 BZ-LFD               PIC 9999      COMP.
           03  BZ-JKEY.
               05 BZ-USE               PIC 99        COMP.
               05 BZ-SEITE             PIC 9(7)      COMP-X.
           03  BZ-BELNR                PIC 9(6)      COMP.
           03  BZ-BELDAT               PIC 9(8)      COMP.
           03  BZ-REFNR                PIC 9(7)      COMP.
           03  BZ-SYM                  PIC 99        COMP.
           03  BZ-GGKTO                PIC 9(6)      COMP.
           03  BZ-SH                   PIC 9         COMP.
           03  BZ-BUBET                PIC S9(9)V99  COMP.
           03  BZ-Z                    PIC 9         COMP.
           03  BZ-VM                   PIC 9         COMP.
           03  BZ-MW                   PIC S9(7)V99  COMP.
           03  BZ-U1                   PIC 99        COMP.
           03  BZ-B1                   PIC S9(9)V99  COMP.
           03  BZ-U2                   PIC 99        COMP.
           03  BZ-B2                   PIC S9(9)V99  COMP.
           03  BZ-KSTNR                PIC 9(5)      COMP.
           03  BZ-KOND                 PIC 9(13)     COMP.
           03  BZ-OPLFD                PIC 9999      COMP.
           03  BZ-FWBET                PIC S9(9)V99  COMP.
           03  BZ-KURS                 PIC 9(4)V9(7) COMP.
           03  BZ-FSYM                 PIC XXX.
           03  BZ-TX                   PIC X(40).
      *----------------------------------------> Jahresabschluásummen <-
       01  BZ-ASATZ.
           03  FILLER                  PIC X(52).
           03  BZ-SOLL                 PIC S9(9)V99  COMP.
           03  BZ-HABEN                PIC S9(9)V99  COMP.
           03  BZ-EBIL                 PIC S9(9)V99  COMP.
      ************************************* Journal- u. Buchungszeilen *
       FD  BxZEIL     external       RECORD VARYING FROM 68 TO 115
                                       DEPENDING ON WZ-BUCH.
       01  bx-SATZ.
           03  bx-KEY.
               05 bx-KTONR             PIC 9(6)      COMP.
               05 bx-BUDAT             PIC 9(8)      COMP.
               05 bx-LFD               PIC 9999      COMP.
           03  bx-JKEY.
               05 bx-USE               PIC 99        COMP.
               05 bx-SEITE             PIC 9(7)      COMP-X.
           03  bx-BELNR                PIC 9(6)      COMP.
           03  bx-BELDAT               PIC 9(8)      COMP.
           03  bx-REFNR                PIC 9(7)      COMP.
           03  bx-SYM                  PIC 99        COMP.
           03  bx-GGKTO                PIC 9(6)      COMP.
           03  bx-SH                   PIC 9         COMP.
           03  bx-BUBET                PIC S9(9)V99  COMP.
           03  bx-Z                    PIC 9         COMP.
           03  bx-VM                   PIC 9         COMP.
           03  bx-MW                   PIC S9(7)V99  COMP.
           03  bx-U1                   PIC 99        COMP.
           03  bx-B1                   PIC S9(9)V99  COMP.
           03  bx-U2                   PIC 99        COMP.
           03  bx-B2                   PIC S9(9)V99  COMP.
           03  bx-KSTNR                PIC 9(5)      COMP.
           03  bx-KOND                 PIC 9(13)     COMP.
           03  bx-OPLFD                PIC 9999      COMP.
           03  bx-FWBET                PIC S9(9)V99  COMP.
           03  bx-KURS                 PIC 9(4)V9(7) COMP.
           03  bx-FSYM                 PIC XXX.
           03  bx-TX                   PIC X(40).
      *----------------------------------------> Jahresabschluásummen <-
       01  bx-ASATZ.
           03  FILLER                  PIC X(52).
           03  bx-SOLL                 PIC S9(9)V99  COMP.
           03  bx-HABEN                PIC S9(9)V99  COMP.
           03  bx-EBIL                 PIC S9(9)V99  COMP.
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
       DECL-K SECTION.         USE AFTER ERROR PROCEDURE ON BXZEIL.
       A.  CALL "CADECL" USING "FIBUKONS.DAT" WH-CREG.
       Z.  EXIT.
       END DECLARATIVES.
      *****************************************************************
       STEUER SECTION.
      *------------------------------------------------> OP-Korrektur <-
       H.  MOVE "FIBUZEIL.DAT" TO WN-bzei.
           OPEN I-O BUCHZEIL.
           DISPLAY "Reorg Fibujahre" AT 0529.
           DISPLAY LOW-VALUE AT 0601.
           MOVE low-VALUES to bz-satz.
           delete file bxzeil.
           open i-o bxzeil.
      *    START buchzeil KEY not < bz-jkey INVALID GO Z.
       K.  READ buchzeil next AT END GO Q.
           DISPLAY bz-budat AT 2210.
      *    if bz-budat < 19950400 move 43 to bz-use go m.
      *    if bz-budat < 19950400 move 44 to bz-use go m.
      *    if bz-budat < 19960400 move 45 to bz-use go m.
      *    if bz-budat < 19960900 move 46 to bz-use go m.
      *    if bz-budat < 19970900 move 47 to bz-use go m.
      *    if bz-budat < 19980900 move 48 to bz-use go m.
      *    if bz-budat < 19990900 move 49 to bz-use go m.

      *    if bz-budat > 19990900 move 50 to bz-use go m.
      *    if bz-budat > 19980900 move 49 to bz-use go m.
      *    if bz-budat > 19970900 move 48 to bz-use go m.
      *    if bz-budat > 19960900 move 47 to bz-use go m.
      *    if bz-budat > 19960400 move 46 to bz-use go m.
      *    if bz-budat > 19950400 move 45 to bz-use go m.
      *    move 50 to bz-use.
       M.  move bz-satz to bx-satz.
       n.  WRITE bx-satz INVALID add 1 to bx-lfd go n.
           GO K.
       Q.  CLOSE buchzeil.
       Z.  stop run.
      ******************************************************************
       WEITER SECTION.
       A.  DISPLAY " weiter mit <ret>: " with highlight AT 0000.
           MOVE SPACE TO WH-X.
           ACCEPT WH-X AT 0000.
           CALL "CAUP" USING "1324012480000" WH-CREG.
       Z.  EXIT.
