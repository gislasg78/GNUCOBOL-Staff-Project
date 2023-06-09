       IDENTIFICATION DIVISION.
       PROGRAM-ID.  numbrlit.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  ws-variables-ambiente.
           05  ws-constantes-num.
               10  ws-ctes-decimas.
                   15  ws-cte-0V99     PIC 9V99       VALUE 0.99.
                   15  ws-cte-1V99     PIC 9V99       VALUE 1.99.
               10  ws-ctes-enteras.
                   15  ws-cte-01       PIC 9          VALUE 01.
                   15  ws-cte-02       PIC 9          VALUE 02.
                   15  ws-cte-03       PIC 9          VALUE 03.
                   15  ws-cte-04       PIC 9          VALUE 04.
                   15  ws-cte-05       PIC 9          VALUE 05.
                   15  ws-cte-06       PIC 9          VALUE 06.
                   15  ws-cte-10       PIC 9(02)      VALUE 10.
                   15  ws-cte-29       PIC 9(02)      VALUE 29.
                   15  ws-cte-30       PIC 9(02)      VALUE 30.
                   15  ws-cte-99       PIC 9(02)      VALUE 99.
                   15  ws-cte-100      PIC 9(03)      VALUE 100.
               
           05  ws-constantes-alfa.
               10  ws-cte-diversas.
                   15  ws-asterisco    PIC X(01)      VALUE '*'.
                   15  ws-cte-Y        PIC X(02)      VALUE 'Y*'.
                   15  ws-cte-mil      PIC X(04)      VALUE 'MIL*'.
               10  ws-cte-mllns.
                   15  ws-cte-millon   PIC X(07)      VALUE 'MILLON*'.
                   15  ws-cte-millones PIC X(09)      VALUE 'MILLONES*'.
               10  ws-cte-bllns.
                   15  ws-cte-billon   PIC X(07)      VALUE 'BILLON*'.
                   15  ws-cte-billones PIC X(09)      VALUE 'BILLONES*'.
               10  ws-cte-cnm-sng-plr-mll-bll.
                   15  ws-cnm-sgn-mll-bll  PIC X(07)  VALUE SPACES.
                   15  ws-cnm-plr-mll-bll  PIC X(09)  VALUE SPACES.
               10  ws-cte-lit-monedas.
                   15  ws-cte-mda-singular PIC X(14)  VALUE
                       'PESO*MEXICANO*'.
                   15  ws-cte-mda-plural   PIC X(16)  VALUE
                       'PESOS*MEXICANOS*'.
               10  ws-cte-cent-sdin.
                   15  ws-cte-centavo      PIC X(09)  VALUE '/100*M.N.'.
                   15  ws-cte-sin-dinero   PIC X(21)  VALUE
                       'CERO*PESOS*MEXICANOS*'.

           05  ws-variables-resultado.
               10  ws-letras           PIC X(225)     VALUE SPACES.
               10  ws-letras-aux       PIC X(225)     VALUE SPACES.
               10  ws-letras-xcar      REDEFINES  ws-letras-aux
                                       OCCURS 225 TIMES
                                       INDEXED BY idx-letras-xcar
                                       PIC X.
               10  ws-letras-long      PIC 9(03)      VALUE ZEROES.
               10  ws-idx-num-grp-t-cur-nxt.
                   15  ws-idx-num-grp-t-cur PIC 9     VALUE ZERO.
                   15  ws-idx-num-grp-t-nxt PIC 9     VALUE ZERO.
               10  ws-num-eds-dsply.
                   15  ws-numero-dl    PIC $$$$,$$$,$$$,$$$,$$$,$$9.99
                                                      VALUE ZEROES.
                   15  ws-let-lng-dl   PIC Z(03)      VALUE ZEROES.

           05  ws-variables-trabajo.
               10  ws-numero           PIC 9(18)V9(2) VALUE ZEROES.
               10  FILLER              REDEFINES  ws-numero.
                   15  FILLER          OCCURS  06 TIMES
                                       INDEXED BY idx-num-grp-tri.
                       20  ws-num-grp-tri.
                           25  ws-num-grp-tri-digcent     PIC 9.
                           25  ws-num-grp-tri-decenas.
                               30  ws-num-grp-tri-digdec  PIC 9.
                               30  ws-num-grp-tri-diguni  PIC 9.
                   15  ws-num-decimas                     PIC 9(2).
               10  FILLER              REDEFINES  ws-numero.
                   15  FILLER          OCCURS  06 TIMES.
                       20  FILLER                         PIC X.
                       20  ws-num-grp-tri-dec-num         PIC 9(2).
                   15  FILLER                             PIC X(2).
               10  FILLER              REDEFINES  ws-numero.
                   15  FILLER          OCCURS  06 TIMES.
                       20  ws-num-grp-tri-num             PIC 9(3).
                   15  FILLER                             PIC X(2).

           05  ws-tabla-unidades.
               10  ws-tab-uno.
                   15  FILLER         PIC X(14) VALUE 'UN*'.
                   15  FILLER         PIC X(14) VALUE 'DOS*'.
                   15  FILLER         PIC X(14) VALUE 'TRES*'.
                   15  FILLER         PIC X(14) VALUE 'CUATRO*'.
                   15  FILLER         PIC X(14) VALUE 'CINCO*'.
                   15  FILLER         PIC X(14) VALUE 'SEIS*'.
                   15  FILLER         PIC X(14) VALUE 'SIETE*'.
                   15  FILLER         PIC X(14) VALUE 'OCHO*'.
                   15  FILLER         PIC X(14) VALUE 'NUEVE*'.
               10  ws-tab-diez.
                   15  FILLER         PIC X(14) VALUE 'DIEZ*'.
                   15  FILLER         PIC X(14) VALUE 'ONCE*'.
                   15  FILLER         PIC X(14) VALUE 'DOCE*'.
                   15  FILLER         PIC X(14) VALUE 'TRECE*'.
                   15  FILLER         PIC X(14) VALUE 'CATORCE*'.
                   15  FILLER         PIC X(14) VALUE 'QUINCE*'.
                   15  FILLER         PIC X(14) VALUE 'DIECISEIS*'.
                   15  FILLER         PIC X(14) VALUE 'DIECISIETE*'.
                   15  FILLER         PIC X(14) VALUE 'DIECIOCHO*'.
                   15  FILLER         PIC X(14) VALUE 'DIECINUEVE*'.
               10  ws-tab-veinte. 
                   15  FILLER         PIC X(14) VALUE 'VEINTE*'.
                   15  FILLER         PIC X(14) VALUE 'VEINTIUN*'.
                   15  FILLER         PIC X(14) VALUE 'VEINTIDOS*'.
                   15  FILLER         PIC X(14) VALUE 'VEINTITRES*'.
                   15  FILLER         PIC X(14) VALUE 'VEINTICUATRO*'.
                   15  FILLER         PIC X(14) VALUE 'VEINTICINCO*'.
                   15  FILLER         PIC X(14) VALUE 'VEINTISEIS*'.
                   15  FILLER         PIC X(14) VALUE 'VEINTISIETE*'.
                   15  FILLER         PIC X(14) VALUE 'VEINTIOCHO*'.
                   15  FILLER         PIC X(14) VALUE 'VEINTINUEVE*'.
           05  ws-tabla-unidades-red   REDEFINES  ws-tabla-unidades.
               10  ws-tabla-unidades-o OCCURS  29 TIMES
                                       INDEXED BY idx-tab-unid-let.
                   15  ws-tab-unid-let PIC X(14).

           05  ws-tabla-decenas.
               10  FILLER              PIC X(11) VALUE 'DIEZ*'.
               10  FILLER              PIC X(11) VALUE 'VEINTE*'.
               10  FILLER              PIC X(11) VALUE 'TREINTA*'.
               10  FILLER              PIC X(11) VALUE 'CUARENTA*'.
               10  FILLER              PIC X(11) VALUE 'CINCUENTA*'.
               10  FILLER              PIC X(11) VALUE 'SESENTA*'.
               10  FILLER              PIC X(11) VALUE 'SETENTA*'.
               10  FILLER              PIC X(11) VALUE 'OCHENTA*'.
               10  FILLER              PIC X(11) VALUE 'NOVENTA*'.
               10  FILLER              PIC X(11) VALUE 'CIEN*'.
           05  ws-tabla-decenas-red    REDEFINES  ws-tabla-decenas.
               10  ws-tabla-decenas-o  OCCURS  10 TIMES
                                       INDEXED BY idx-tab-dece-let.
                   15  ws-tab-dece-let PIC X(11).

           05  ws-tabla-centenas.
               10  FILLER              PIC X(15) VALUE 'CIENTO*'.
               10  FILLER              PIC X(15) VALUE 'DOSCIENTOS*'.
               10  FILLER              PIC X(15) VALUE 'TRESCIENTOS*'.
               10  FILLER              PIC X(15) VALUE 'CUATROCIENTOS*'.
               10  FILLER              PIC X(15) VALUE 'QUINIENTOS*'.
               10  FILLER              PIC X(15) VALUE 'SEISCIENTOS*'.
               10  FILLER              PIC X(15) VALUE 'SETECIENTOS*'.
               10  FILLER              PIC X(15) VALUE 'OCHOCIENTOS*'.
               10  FILLER              PIC X(15) VALUE 'NOVECIENTOS*'.
           05  ws-tabla-centenas-red   REDEFINES  ws-tabla-centenas.
               10  ws-tabla-centenas-o OCCURS  09 TIMES
                                       INDEXED BY idx-tab-cent-let.
                   15  ws-tab-cent-let PIC X(15).

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           PERFORM 100000-inicio
              THRU 100000-fin-inicio
           PERFORM 200000-proceso
              THRU 200000-fin-proceso
           PERFORM 300000-final
              THRU 300000-fin-final
           STOP RUN.

       100000-inicio.
           MOVE	   ZEROES                              TO ws-numero
                                                          ws-numero-dl
                                                          ws-let-lng-dl
                                                          ws-letras-long
           
           MOVE    SPACES                              TO ws-letras
                                                          ws-letras-aux

           ACCEPT  ws-numero.
       100000-fin-inicio.
           EXIT.

       200000-proceso.
           PERFORM 210000-rutina-letras
              THRU 210000-fin-rutina-letras
           VARYING idx-num-grp-tri
              FROM ws-cte-01                                BY ws-cte-01
             UNTIL idx-num-grp-tri             IS GREATER THAN ws-cte-06

           PERFORM 220000-completar-cadena
              THRU 220000-fin-completar-cadena.
       200000-fin-proceso.
           EXIT.

        210000-rutina-letras.
           IF ws-num-grp-tri-num     (idx-num-grp-tri) GREATER ZEROES
              PERFORM 211000-asigna-letras
                 THRU 211000-fin-asigna-letras.
        210000-fin-rutina-letras.
           EXIT.

         211000-asigna-letras.
           IF ws-num-grp-tri-num     (idx-num-grp-tri) GREATER ws-cte-99
              PERFORM 211100-asigna-centenas
                 THRU 211100-fin-asigna-centenas.

           IF ws-num-grp-tri-dec-num (idx-num-grp-tri) GREATER ws-cte-29
              PERFORM 211200-asigna-decenas
                 THRU 211200-fin-asigna-decenas.

           IF ws-num-grp-tri-num     (idx-num-grp-tri) GREATER ZEROES
              PERFORM 211300-asigna-unidades
                 THRU 211300-fin-asigna-unidades.

           PERFORM 211400-asigna-millares
              THRU 211400-fin-asigna-millares

           PERFORM 211500-asigna-millones
              THRU 211500-fin-asigna-millones

           PERFORM 211600-asigna-billones
              THRU 211600-fin-asigna-billones.
         211000-fin-asigna-letras.
           EXIT.

          211100-asigna-centenas.
             IF ws-num-grp-tri  (idx-num-grp-tri) IS EQUAL TO ws-cte-100
                 SET idx-tab-dece-let
                  TO ws-cte-10
                MOVE ws-tab-dece-let        (idx-tab-dece-let)
                  TO ws-letras-aux
             ELSE
                 SET idx-tab-cent-let
                  TO ws-num-grp-tri-digcent (idx-num-grp-tri)
                MOVE ws-tab-cent-let        (idx-tab-cent-let)
                  TO ws-letras-aux.

             PERFORM 211110-concatenar.
          211100-fin-asigna-centenas.
             EXIT.

          211110-concatenar.
             STRING ws-letras                DELIMITED BY SPACE
                    ws-letras-aux            DELIMITED BY SPACE
               INTO ws-letras

             MOVE   SPACES                             TO ws-letras-aux.

          211200-asigna-decenas.
              SET idx-tab-dece-let
               TO ws-num-grp-tri-digdec (idx-num-grp-tri)
             MOVE ws-tab-dece-let       (idx-tab-dece-let)
               TO ws-letras-aux

             PERFORM 211110-concatenar

             IF  ws-num-grp-tri-diguni  (idx-num-grp-tri) GREATER ZERO
             AND ws-num-grp-tri-dec-num (idx-num-grp-tri)
                 IS GREATER THAN ws-cte-30
                    MOVE ws-cte-Y                       TO ws-letras-aux
                    PERFORM 211110-concatenar.
          211200-fin-asigna-decenas.
             EXIT.

          211300-asigna-unidades.
             IF  ws-num-grp-tri-dec-num (idx-num-grp-tri) LESS ws-cte-30
             AND ws-num-grp-tri-dec-num (idx-num-grp-tri) GREATER ZEROES
                    SET idx-tab-unid-let
                     TO ws-num-grp-tri-dec-num (idx-num-grp-tri)
                   MOVE ws-tab-unid-let        (idx-tab-unid-let)
                     TO ws-letras-aux
                   PERFORM 211110-concatenar
             ELSE
                IF ws-num-grp-tri-diguni (idx-num-grp-tri) GREATER ZERO
                    SET idx-tab-unid-let
                     TO ws-num-grp-tri-diguni  (idx-num-grp-tri)
                   MOVE ws-tab-unid-let        (idx-tab-unid-let)
                     TO ws-letras-aux
                   PERFORM 211110-concatenar.
          211300-fin-asigna-unidades.
             EXIT.

          211400-asigna-millares.
             IF idx-num-grp-tri   IS EQUAL TO ws-cte-01 OR ws-cte-03 OR
                ws-cte-05
                  IF ws-num-grp-tri-num (idx-num-grp-tri) GREATER ZEROES
                     MOVE  ws-cte-mil                   TO ws-letras-aux
                     PERFORM  211110-concatenar. 
          211400-fin-asigna-millares.
             EXIT.

          211500-asigna-millones.
              MOVE ws-cte-03                     TO ws-idx-num-grp-t-cur
              MOVE ws-cte-04                     TO ws-idx-num-grp-t-nxt
              MOVE ws-cte-millon                 TO ws-cnm-sgn-mll-bll
              MOVE ws-cte-millones               TO ws-cnm-plr-mll-bll
              
              PERFORM 211510-asigna-mill-bill
                 THRU 211510-fin-asigna-mill-bill.
          211500-fin-asigna-millones.
             EXIT.

          211510-asigna-mill-bill.
             IF idx-num-grp-tri         IS EQUAL TO ws-idx-num-grp-t-cur
                IF ws-num-grp-tri-num       (ws-idx-num-grp-t-cur) 
                   IS GREATER THAN ZEROES
                   IF ws-num-grp-tri-num    (ws-idx-num-grp-t-nxt)
                   IS EQUAL TO ZEROES
                      MOVE ws-cnm-plr-mll-bll        TO ws-letras-aux
                      PERFORM 211110-concatenar.
                      
             IF idx-num-grp-tri         IS EQUAL TO ws-idx-num-grp-t-nxt
                IF ws-num-grp-tri-num       (ws-idx-num-grp-t-nxt)
                   IS EQUAL TO ws-cte-01
                      IF ws-num-grp-tri-num (ws-idx-num-grp-t-cur)
                         IS EQUAL TO ZEROES
                            MOVE ws-cnm-sgn-mll-bll  TO ws-letras-aux
                            PERFORM 211110-concatenar
                      ELSE
                            MOVE ws-cnm-plr-mll-bll  TO ws-letras-aux
                            PERFORM 211110-concatenar
                ELSE
                   MOVE ws-cnm-plr-mll-bll           TO ws-letras-aux
                   PERFORM 211110-concatenar.
          211510-fin-asigna-mill-bill.
             EXIT.

          211600-asigna-billones.
             MOVE ws-cte-01                      TO ws-idx-num-grp-t-cur
             MOVE ws-cte-02                      TO ws-idx-num-grp-t-nxt
             MOVE ws-cte-billon                  TO ws-cnm-sgn-mll-bll
             MOVE ws-cte-billones                TO ws-cnm-plr-mll-bll
              
              PERFORM 211510-asigna-mill-bill
                 THRU 211510-fin-asigna-mill-bill.
          211600-fin-asigna-billones.
             EXIT.

        220000-completar-cadena.
             IF ws-numero                 IS GREATER THAN ws-cte-1V99
                MOVE ws-cte-mda-plural                 TO ws-letras-aux
             ELSE
                IF  ws-numero                IS LESS THAN ws-cte-02
                AND                       IS GREATER THAN ws-cte-0V99
                      MOVE ws-cte-mda-singular         TO ws-letras-aux
                ELSE
                      MOVE ws-cte-sin-dinero           TO ws-letras-aux.

             STRING  ws-letras               DELIMITED BY SPACE
                     ws-letras-aux           DELIMITED BY SPACE
                     ws-num-decimas          DELIMITED BY SPACE
                     ws-cte-centavo          DELIMITED BY SPACE
               INTO  ws-letras

             INSPECT ws-letras               TALLYING     ws-letras-long
                 FOR CHARACTERS
              BEFORE INITIAL SPACE
                                             REPLACING ALL ws-asterisco
                  BY SPACE.
        220000-fin-completar-cadena.
             EXIT.

       300000-final.
             MOVE    ws-numero                          TO ws-numero-dl
             MOVE    ws-letras                          TO ws-letras-aux
             MOVE    ws-letras-long                     TO ws-let-lng-dl

             DISPLAY SPACE
             DISPLAY ws-numero-dl

             PERFORM 310000-muestra-letras-xcar
                THRU 310000-fin-muestra-letras-xcar
             VARYING idx-letras-xcar
                FROM ws-cte-01                          BY ws-cte-01
               UNTIL idx-letras-xcar    IS GREATER THAN ws-letras-long

             DISPLAY SPACE
             DISPLAY ws-let-lng-dl
             DISPLAY SPACE.
       300000-fin-final.
             EXIT.

        310000-muestra-letras-xcar.
             DISPLAY ws-letras-xcar (idx-letras-xcar) WITH NO ADVANCING.
        310000-fin-muestra-letras-xcar.     
             EXIT.
