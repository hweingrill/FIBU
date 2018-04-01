      ***************************** Buchs„tze f. Fibuueberleitung FAKT *
       FD  SOFTFIB      external       LABEL RECORD STANDARD.
       01  SF-SATZ.
           03  SF-FIRMA                PIC X(20).
           03  SF-ANWEND               PIC X(20).
           03  SF-PROG                 PIC X(20).
           03  SF-USER                 PIC X(20).
           03  SF-BUNUM                PIC 9(5).
           03  SF-LFDNR                PIC 9(5).
           03  BU-KAT                  PIC 9(5).
           03  BU-NUMMER               PIC 9(5).
           03  BU-SUBNR                PIC 9(5).
           03  BU-KOWHG                PIC 9(10).
           03  BU-BELDAT               PIC 9(8).
           03  BU-REFNR                PIC X(20).
           03  BU-LAND                 PIC X(20).
           03  BU-USTCODE              PIC X(10).
           03  BU-BELNR                PIC 9(6).
           03  BU-ZAHLKOND             PIC X(20).
           03  BU-TG1                  PIC 999.
           03  BU-SK1                  PIC 99,99.
           03  BU-TG2                  PIC 999.
           03  BU-SK3                  PIC 99,99.
           03  BU-NTG                  PIC 999.
           03  BU-KZZM                 PIC X.
           03  BU-SOLLMG               PIC 9(13)V99.
           03  BU-HABENMG              PIC 9(13)V99.
           03  BU-KURS                 PIC 9(5)V9(6).
           03  BU-NTG                  PIC 999.
           03  BU-VALUTA               PIC 9(8).
           03  BU-SYMB                 PIC XXXX.
           03  BU-MAHNST               PIC 999.
           03  BU-JTEXT                PIC 999.
           03  BU-KZSPERR              PIC X.              *> "N"
           03  BU-ZUORDNG              PIC X(10).
           03  BU-VAR                  PIC 999      OCCURS 5.
           03  BU-ZESSBK               PIC X(20).
           03  BU-KZ                   PIC X.              *> "B"
           03  BU-BUCHPER              PIC 9(8).
           03  BU-KOREPER              PIC 9(8).
           03  BU-BEREICH              PIC X(20).
           03  BU-ZAHLART              PIC X(20).
           03  BU-BEMERK               PIC X(20).
           03  BU-EUSTDAT              PIC 9(8).
           03  BU-KZOP                 PIC X.              *> "N"
           03  BU-BARCODE              PIC X(35).
           03  BU-WAEHRG               PIC X(20).
           03  BU-SOLL                 PIC 9(11)V99.
           03  BU-HABEN                PIC 9(11)V99.
           03  BU-VST                  PIC 9(9)V99.
           03  BU-MWST                 PIC 9(9)V99.
           03  BU-SKTO                 PIC 9(9)V99.
           03  BU-ERLOES               PIC 9(11)V99      OCCURS 2.
           03  BU-EUST                 PIC 9(11)V99.
           03  BU-KURSDIF              PIC 9(11)V99.
