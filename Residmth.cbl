       IDENTIFICATION DIVISION.
       PROGRAM-ID. Residmth.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       78  cte-100                            VALUE 100.

       01  ws-code-data          UNSIGNED-INT VALUE ZEROES.
       01  ws-quottient          UNSIGNED-INT VALUE ZEROES.

       01  ws-key-sat-perc       PIC 9(02)    VALUE ZEROES.
       01  ws-key-max-sat        UNSIGNED-INT VALUE ZEROES.

       01  ws-fac-pos-addr       UNSIGNED-INT VALUE ZEROES.
       01  ws-rel-pos-addr       UNSIGNED-INT VALUE ZEROES.

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           DISPLAY "Prog that converts a key into a relative position"
           DISPLAY "Maximum number of records : " WITH NO ADVANCING
            ACCEPT ws-key-max-sat
           DISPLAY "Key saturation percentage : " WITH NO ADVANCING
            ACCEPT ws-key-sat-perc
           DISPLAY "Enter a numeric int code  : " WITH NO ADVANCING
            ACCEPT ws-code-data

           DISPLAY SPACE
           DISPLAY "Calculation information."
           DISPLAY "Maximum number of records : [" ws-key-max-sat "]."
           DISPLAY "Key saturation percentage : [" ws-key-sat-perc "]."

           COMPUTE ws-fac-pos-addr = ws-key-max-sat /
                   (ws-key-sat-perc / cte-100)
                ON SIZE ERROR
                   DISPLAY "Error in calculation. "
                           "The result is invalid."
           END-COMPUTE

           DISPLAY SPACE
           DISPLAY "Calculation of the fit factor."
           DISPLAY "Estimated file density    : [" ws-fac-pos-addr "]."

           DIVIDE ws-fac-pos-addr INTO ws-code-data GIVING ws-quottient
           REMAINDER ws-rel-pos-addr
               ON SIZE ERROR
                  DISPLAY "Error in calculation. "
                          "The result is invalid."
           END-DIVIDE

           DISPLAY SPACE
           DISPLAY "Obtaining the registration number."
           DISPLAY "Code to be converted      : [" ws-code-data "]."
           DISPLAY "Relative position address : [" ws-rel-pos-addr "]."

           STOP RUN.

       END PROGRAM Residmth.
