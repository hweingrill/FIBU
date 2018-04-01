           SELECT KONSTANT ASSIGN WN-KONS
                           ORGANIZATION RELATIVE, ACCESS DYNAMIC
                           RELATIVE KEY WH-KEY
                           FILE STATUS WF-STATUS.
           SELECT BUCHUNG  ASSIGN WN-BUEB
                           ORGANIZATION INDEXED, ACCESS DYNAMIC
                           RECORD KEY BU-KEY
                           ALTERNATE KEY BU-RKEY DUPLICATES
                           FILE STATUS WF-STATUS.
           SELECT OPDATEI  ASSIGN WN-OPES
                           ORGANIZATION INDEXED, ACCESS DYNAMIC
                           RECORD KEY OP-KEY
                           ALTERNATE KEY OP-AKEY DUPLICATES
      *                    ALTERNATE KEY OP-RKEY = OP-RENUM OP-VALDAT
      *                               OP-DK OP-KTONR OP-LFD DUPLICATES
                           FILE STATUS WF-STATUS.
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
           SELECT SACHBUCH ASSIGN WN-SACH
                           ORGANIZATION INDEXED, ACCESS DYNAMIC
                           RECORD KEY SA-KEY
                           ALTERNATE KEY SA-MCODE DUPLICATES
                           ALTERNATE KEY SA-KER = SA-KER-NR SA-KTONR
                               DUPLICATES
                           FILE STATUS WF-STATUS.
           SELECT BUCHZEIL ASSIGN WN-BZEI
                           ORGANIZATION INDEXED, ACCESS DYNAMIC
                           RECORD KEY BZ-KEY
                           ALTERNATE KEY IS BZ-JKEY
                           FILE STATUS WF-STATUS.
           SELECT AUSZIFF  ASSIGN WN-AUSZ
                           ORGANIZATION INDEXED, ACCESS DYNAMIC
                           RECORD KEY AZ-KEY
                           FILE STATUS WF-STATUS.
           SELECT KOSTEN   ASSIGN WN-KOST
                           ORGANIZATION INDEXED, ACCESS DYNAMIC
                           RECORD KEY KS-KEY
                           FILE STATUS WF-STATUS.
           SELECT KERMODEL ASSIGN "FIBUKER.DAT"
                           ORGANIZATION INDEXED, ACCESS DYNAMIC
                           RECORD KEY KM-KEY
                           FILE STATUS WF-STATUS.
           SELECT BANKDAT  ASSIGN TO WN-BANK
                           ORGANIZATION INDEXED ACCESS DYNAMIC
                           RECORD KEY BK-KEY
                           FILE STATUS IS WF-STATUS.
