           SELECT KUNDEN   ASSIGN TO "TIKUNDEN.DAT"
                           ORGANIZATION INDEXED ACCESS DYNAMIC
                           RECORD KEY KU-KEY
                           ALTERNATE RECORD KU-AKEY =
                                     KU-SA, KU-MCODE DUPLICATES
                           ALTERNATE RECORD KU-FKEY =
                                     KU-FK KU-KTONR DUPLICATES
                           ALTERNATE RECORD KU-FILKEY =
                                     KU-PREISE KU-BAN DUPLICATES
                           ALTERNATE RECORD KU-PKEY =
                                     KU-PREISE KU-MCODE DUPLICATES
                           ALTERNATE RECORD KU-RKEY = KU-STAT KU-KEY
                           ALTERNATE RECORD KU-SKEY = KU-PREISE KU-KEY
                           ALTERNATE RECORD KU-EANK = KU-BAN DUPLICATES
                           ALTERNATE RECORD KU-FILNR DUPLICATES
                               SUPPRESS WHEN ZERO
                           FILE STATUS IS WF-STATUS.
