       IDENTIFICATION DIVISION.
       PROGRAM-ID. Strings.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       78  cte-01                            VALUE 01.
       78  cte-07                            VALUE 07.
       78  cte-12                            VALUE 12.

       01  ws-accountants-date-fmt.
           03  ws-accountants-field-string.
               05  ws-account-field-length   PIC 9(02) VALUE ZEROES.
               05  ws-account-string-length  PIC 9(02) VALUE ZEROES.
           03  ws-accountants-spaces.
               05  ws-account-backspace-len  PIC 9(02) VALUE ZEROES.
               05  ws-account-frontspace-len PIC 9(02) VALUE ZEROES.

       01  ws-comma-type-delimiters.
           03  ws-comma-and-normal-space     PIC X(02) VALUE X'2C20'.
           03  ws-comma-punctuation-mark     PIC X(01) VALUE X'2C'.

       01  ws-current-date.
           03  ws-current-date-year          PIC 9(04) VALUE ZEROES.
           03  ws-current-date-month         PIC 9(02) VALUE ZEROES.
           03  ws-current-date-day           PIC 9(02) VALUE ZEROES.
       01  ws-current-date-num     REDEFINES ws-current-date PIC 9(08).

       01  ws-current-date-chains-formatted.
           03  ws-current-date-num-formatted PIC 9999/99/99
                                             VALUE ZEROES.
           03  ws-current-date-str-formatted PIC X(29) VALUE SPACES.

       01  ws-date-counters-delimiters.
           03  ws-date-counter-fields        PIC 9(02) VALUE ZEROES.
           03  ws-date-counters.
               05  ws-date-count-dayname     PIC 9(02) VALUE ZEROES.
               05  ws-date-count-monthname   PIC 9(02) VALUE ZEROES.
               05  ws-date-count-numday      PIC 9(02) VALUE ZEROES.
               05  ws-date-count-year        PIC 9(02) VALUE ZEROES.
           03  ws-date-delimiters.
               05  ws-date-delimit-dayname   PIC X(02) VALUE SPACES.
               05  ws-date-delimit-monthname PIC X(02) VALUE SPACES.
               05  ws-date-delimit-numday    PIC X(02) VALUE SPACES.
               05  ws-date-delimit-year      PIC X(02) VALUE SPACES.

       01  ws-date-fields.
           03  ws-date-dayofweek             PIC 9(01) VALUE ZERO.
           03  ws-date-dayname               PIC A(09) VALUE SPACES.
           03  ws-date-monthname             PIC A(09) VALUE SPACES.
           03  ws-date-numday                PIC 9(02) VALUE ZEROES.
           03  ws-date-pointer-string        PIC 9(02) VALUE ZEROES.
           03  ws-date-pointer-unstring      PIC 9(02) VALUE ZEROES.
           03  ws-date-year                  PIC 9(04) VALUE ZEROES.

       01  ws-date-name-of-days.
           03  FILLER              PIC A(09) VALUE "Monday".
           03  FILLER              PIC A(09) VALUE "Tuesday".
           03  FILLER              PIC A(09) VALUE "Wednesday".
           03  FILLER              PIC A(09) VALUE "Thursday".
           03  FILLER              PIC A(09) VALUE "Friday".
           03  FILLER              PIC A(09) VALUE "Saturday".
           03  FILLER              PIC A(09) VALUE "Sunday".
       01  ws-date-name-of-days-red REDEFINES ws-date-name-of-days.
           03  ws-date-name-of-days-array    OCCURS cte-07 TIMES
               INDEXED BY idx-date-name-of-days-array      PIC A(09).

       01  ws-date-name-of-months.
           03  FILLER              PIC A(09) VALUE "January".
           03  FILLER              PIC A(09) VALUE "February".
           03  FILLER              PIC A(09) VALUE "March".
           03  FILLER              PIC A(09) VALUE "April".
           03  FILLER              PIC A(09) VALUE "May".
           03  FILLER              PIC A(09) VALUE "June".
           03  FILLER              PIC A(09) VALUE "July".
           03  FILLER              PIC A(09) VALUE "August".
           03  FILLER              PIC A(09) VALUE "September".
           03  FILLER              PIC A(09) VALUE "October".
           03  FILLER              PIC A(09) VALUE "November".
           03  FILLER              PIC A(09) VALUE "December".
       01  ws-date-name-of-months-red REDEFINES ws-date-name-of-months.
           03  ws-date-name-of-months-array  OCCURS cte-12 TIMES
               INDEXED BY idx-date-name-of-months-array    PIC A(09).

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           DISPLAY "Program that dismantles a given string of "
                   "characters."

           ACCEPT ws-current-date   FROM DATE YYYYMMDD
           ACCEPT ws-date-dayofweek FROM DAY-OF-WEEK

           PERFORM 100000-begin-format-current-date
              THRU 100000-end-format-current-date

           PERFORM 200000-begin-string-check-length
              THRU 200000-end-string-check-length

           PERFORM 300000-begin-trigger-chain
              THRU 300000-end-trigger-chain

           PERFORM 400000-begin-display-breakdowns
              THRU 400000-end-display-breakdowns

           STOP RUN.

       100000-begin-format-current-date.
           MOVE SPACES
             TO ws-current-date-str-formatted
           MOVE cte-01                       TO ws-date-pointer-string

           SET idx-date-name-of-days-array   TO ws-date-dayofweek
           SET idx-date-name-of-months-array TO ws-current-date-month

           STRING ws-date-name-of-days-array
                  (idx-date-name-of-days-array)
                  DELIMITED BY SPACE
                  ws-comma-punctuation-mark
                  DELIMITED BY SIZE
                  SPACE
                  DELIMITED BY SIZE
                  ws-date-name-of-months-array
                  (idx-date-name-of-months-array)
                  DELIMITED BY SPACE
                  SPACE
                  DELIMITED BY SIZE
                  ws-current-date-day
                  DELIMITED BY SIZE
                  ws-comma-punctuation-mark
                  DELIMITED BY SIZE
                  SPACE
                  DELIMITED BY SIZE
                  ws-current-date-year
                  DELIMITED BY SIZE
             INTO ws-current-date-str-formatted
             WITH POINTER ws-date-pointer-string

               ON OVERFLOW
                  DISPLAY "The destination string is too short."
              NOT ON OVERFLOW
                  DISPLAY "The target string was successfully "
                          "constructed."
           END-STRING.
       100000-end-format-current-date.
           EXIT.

       200000-begin-string-check-length.
           INITIALIZE ws-account-backspace-len
                      ws-account-frontspace-len
                      ws-account-string-length

           INSPECT ws-current-date-str-formatted
                   TALLYING ws-account-backspace-len
                        FOR LEADING SPACE
                            ws-account-frontspace-len
                        FOR TRAILING SPACE
                            ws-account-string-length
                        FOR CHARACTERS.

           MOVE LENGTH OF ws-current-date-str-formatted
             TO ws-account-field-length

           DISPLAY SPACE
           DISPLAY "Field Accounting Statistics."
           DISPLAY "Lengths:"
           DISPLAY "+ Field:     [" ws-account-field-length "]."
           DISPLAY "+ String:    [" ws-account-string-length "]."

           DISPLAY SPACE
           DISPLAY "Spaces:"
           DISPLAY "+ Back:      [" ws-account-backspace-len "]."
           DISPLAY "+ Front:     [" ws-account-frontspace-len "]."

           DISPLAY SPACE
           DISPLAY "[" FUNCTION TRIM(ws-current-date-str-formatted) "]."
           DISPLAY SPACE.
       200000-end-string-check-length.
           EXIT.

       300000-begin-trigger-chain.
           MOVE cte-01                       TO ws-date-pointer-unstring
           MOVE SPACES                       TO ws-date-dayname
                                                ws-date-monthname
           MOVE ZEROES                       TO ws-date-numday
                                                ws-date-year

           UNSTRING FUNCTION TRIM(ws-current-date-str-formatted)
                    DELIMITED BY SPACE OR ws-comma-and-normal-space
               INTO ws-date-dayname
                        DELIMITER IN ws-date-delimit-dayname
                        COUNT IN ws-date-count-dayname
                    ws-date-monthname
                        DELIMITER IN ws-date-delimit-monthname
                        COUNT IN ws-date-count-monthname
                    ws-date-numday
                        DELIMITER IN ws-date-delimit-numday
                        COUNT IN ws-date-count-numday
                    ws-date-year
                        DELIMITER IN ws-date-delimit-year
                        COUNT IN ws-date-count-year
               WITH POINTER  ws-date-pointer-unstring
                    TALLYING ws-date-counter-fields

                 ON OVERFLOW
                    DISPLAY "The destination fields are too short."
                NOT ON OVERFLOW
                    DISPLAY "The target fields were successfully "
                            "constructed."
           END-UNSTRING.
       300000-end-trigger-chain.
           EXIT.

       400000-begin-display-breakdowns.
           MOVE ws-current-date-num TO ws-current-date-num-formatted

           DISPLAY SPACE
           DISPLAY "Date formats generated."
           DISPLAY "[" ws-current-date-num-formatted "]."
           DISPLAY "[" FUNCTION TRIM(ws-current-date-str-formatted) "]."

           DISPLAY SPACE
           DISPLAY "Extraction Statistics."

           DISPLAY SPACE
           DISPLAY "Pointers."
           DISPLAY "+ String:    [" ws-date-pointer-string "]."
           DISPLAY "+ Unstring:  [" ws-date-pointer-unstring "]."
           DISPLAY "+ Fields:    [" ws-date-counter-fields "]."   

           DISPLAY SPACE
           DISPLAY "Day Name."
           DISPLAY "+ Value:     [" ws-date-dayname "]."
           DISPLAY "+ Delimiter: [" ws-date-delimit-dayname "]."
           DISPLAY "+ Count:     [" ws-date-count-dayname "]."

           DISPLAY SPACE
           DISPLAY "Month Name."
           DISPLAY "+ Value:     [" ws-date-monthname "]."
           DISPLAY "+ Delimiter: [" ws-date-delimit-monthname "]."
           DISPLAY "+ Count:     [" ws-date-count-monthname "]."

           DISPLAY SPACE
           DISPLAY "Num Day."
           DISPLAY "+ Value:     [" ws-date-numday "]."
           DISPLAY "+ Delimiter: [" ws-date-delimit-numday "]."
           DISPLAY "+ Count:     [" ws-date-count-numday "]."

           DISPLAY SPACE
           DISPLAY "Year."
           DISPLAY "+ Value:     [" ws-date-year "]."
           DISPLAY "+ Delimiter: [" ws-date-delimit-year "]."
           DISPLAY "+ Count:     [" ws-date-count-year "].".
       400000-end-display-breakdowns.
           EXIT.

       END PROGRAM Strings.
