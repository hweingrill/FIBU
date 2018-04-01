      *************************************** sontige Bankverbindungen *
       FD  BANKDAT      external       label record standard.
       01  BK-SATZ.
           03  BK-KEY.
               05 BK-DK                PIC 99         COMP.
               05 BK-KTONR             PIC 9(6)       COMP.
           03  BK-BANKKTO              PIC X(13).
           03  BK-BANKLTZ              PIC X(6).
           03  BK-BANK                 PIC X(24).
           03  BK-IBAN                 PIC X(35).
           03  BK-BIC                  PIC X(15).
