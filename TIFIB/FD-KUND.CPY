      **************************************************** Kundendatei *
       FD  KUNDEN       external       LABEL RECORD STANDARD.
       01  KU-SATZ.
           03  KU-KEY.
               05 KU-SA                PIC X.
               05 KU-KTONR             PIC 9(5)      COMP.
           03  KU-MCODE                PIC X(15).
           03  KU-DRUCK-KZ             PIC S99       COMP.
           03  KU-PREISE               PIC 9(5)      COMP.
           03  KU-FK.
               05 KU-REART             PIC 99        COMP.
               05 KU-FOLGE             PIC 9(5)      COMP.
           03  KU-PLASTKI-VERR         PIC 99        COMP.
           03  KU-BAN                  PIC 9(13)     COMP.
           03  KU-VERSART              PIC 99        COMP.
           03  KU-RAB                  PIC S99V99    COMP.
           03  KU-BONUS                PIC S99V99    COMP.
           03  KU-AMASTAT              PIC 99        COMP.
           03  KU-READR                PIC 9(6)      COMP.
           03  KU-LIEFERADR            PIC 9(6)      COMP.
           03  KU-FIBU                 PIC 9(6)      COMP.
           03  KU-EMB-STAND            PIC S9(5)     COMP.
           03  KU-EMB-DAT              PIC 9(6)      COMP.
           03  KU-ANLMERK              PIC 99        COMP.
           03  KU-RABKZ                PIC 99        COMP.
           03  KU-STAT                 PIC 9(6)      COMP.
           03  KU-STATKZ               PIC 99        COMP.
           03  KU-LFKZ                 PIC 99        COMP.
      *--------------------------------------------> FibuÅbergabeteil <-
           03  KU-SAMKZ                PIC 99        COMP.
           03  KU-KOND                 PIC 9(13)     COMP.
           03  KU-BEZ                  PIC X(110).
           03  KU-TEL                  PIC X(33).
           03  KU-TELRES               PIC X.
           03  KU-ZAHLK                PIC 99        COMP.
           03  KU-ANMERK               PIC X(34).
           03  KU-LASTLFG              PIC 9(8)      COMP. *> l. Liefg.
           03  KU-NAD-DP               PIC 9(13)     COMP.
           03  KU-ANREST               PIC X(4).           *> wÑre frei
           03  KU-AWKZ-A               PIC 9(4)      COMP.
           03  KU-AWKZ-B               PIC 9(4)      COMP.
           03  KU-EMBADR               PIC 9(6)      COMP.
           03  KU-RABADR               PIC 9(6)      COMP.
           03  KU-SUBART               PIC 99        COMP.
           03  KU-BESTKZ               PIC 99        COMP.
           03  KU-SCAN                 PIC 9(6)      COMP.
           03  KU-LFKOP                PIC 99        COMP.
           03  KU-KAL                  PIC 99        COMP.
           03  KU-SL                   PIC X.
           03  KU-BEMERK               PIC X(73).
           03  KU-TRANS                PIC X(30).
           03  KU-EMAIL                PIC X(33).
           03  KU-UID                  PIC X(15).
           03  KU-FILNR                PIC 9(4)      COMP.
           03  KU-PAL-STAND            PIC S9(5)     COMP.
           03  KU-FREI                 PIC X(4).
