       IDENTIFICATION DIVISION.
       PROGRAM-ID. Residmth.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       78  cte-100                                         VALUE 100.

       01  ws-adjustment-factor-calculation.
           03  ws-code-data                   UNSIGNED-INT VALUE ZEROES.
           03  ws-estimate-file-density       UNSIGNED-INT VALUE ZEROES.
           03  ws-key-saturation-percentage   PIC 9(02)    VALUE ZEROES.
           03  ws-maximum-number-records      UNSIGNED-INT VALUE ZEROES.
           03  ws-relative-address-position   UNSIGNED-INT VALUE ZEROES.
           03  ws-saturation-percent-quotient UNSIGNED-INT VALUE ZEROES.

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           DISPLAY "Program that converts a given key "
                   "into its relative position"
           DISPLAY "- Maximum number of records : " WITH NO ADVANCING
            ACCEPT ws-maximum-number-records
           DISPLAY "- Key saturation percentage : " WITH NO ADVANCING
            ACCEPT ws-key-saturation-percentage
           DISPLAY "- Enter a numeric int code  : " WITH NO ADVANCING
            ACCEPT ws-code-data

           DISPLAY SPACE
           DISPLAY "Calculation information."
           DISPLAY "+ Maximum number of records : "
                   "[" ws-maximum-number-records "]."
           DISPLAY "+ Key saturation percentage : "
                   "[" ws-key-saturation-percentage "]."

           COMPUTE ws-estimate-file-density = ws-maximum-number-records
                   / (ws-key-saturation-percentage / cte-100)
                ON SIZE ERROR
                   DISPLAY "Estimated file density "
                           "calculated with errors."
               NOT ON SIZE ERROR
                   DISPLAY "Estimated file density "
                           "calculated correctly."
           END-COMPUTE

           DISPLAY SPACE
           DISPLAY "Calculation of the fit factor."
           DISPLAY "+ Estimated file density    : "
                   "[" ws-estimate-file-density "]."

           DIVIDE ws-estimate-file-density INTO ws-code-data
                  GIVING ws-saturation-percent-quotient
                  REMAINDER ws-relative-address-position
                         ON SIZE ERROR
                            DISPLAY "Relative direction position "
                                    "calculated with errors."
                        NOT ON SIZE ERROR
                            DISPLAY "Relative direction position "
                                    "calculated correctly."
           END-DIVIDE

           DISPLAY SPACE
           DISPLAY "Obtaining the registration number."
           DISPLAY "+ Code to be converted      : "
                   "[" ws-code-data "]."
           DISPLAY "+ Relative position address : "
                   "[" ws-relative-address-position "]."

           STOP RUN.

       END PROGRAM Residmth.
