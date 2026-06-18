       IDENTIFICATION DIVISION.
       PROGRAM-ID. Strings.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       78  cte-01                                          VALUE 01.
       78  cte-07                                          VALUE 07.
       78  cte-12                                          VALUE 12.
       78  cte-43                                          VALUE 43.

       01  ws-accountants-date-and-time-fmt.
           03  ws-accountants-field-string.
               05  ws-account-field-length       PIC 9(02) VALUE ZEROES.
               05  ws-account-string-length      PIC 9(02) VALUE ZEROES.
           03  ws-accountants-spaces.
               05  ws-account-backspace-len      PIC 9(02) VALUE ZEROES.
               05  ws-account-frontspace-len     PIC 9(02) VALUE ZEROES.

       01  ws-current-date-and-time-values.
           03  ws-current-date.
               05  ws-current-date-year          PIC 9(04) VALUE ZEROES.
               05  ws-current-date-month         PIC 9(02) VALUE ZEROES.
               05  ws-current-date-day           PIC 9(02) VALUE ZEROES.
           03  ws-current-date-num REDEFINES ws-current-date PIC 9(08).
           03  ws-current-date-str REDEFINES ws-current-date PIC X(08).
           03  ws-current-time.
               05  ws-current-time-hour          PIC 9(02) VALUE ZEROES.
               05  ws-current-time-minute        PIC 9(02) VALUE ZEROES.
               05  ws-current-time-second        PIC 9(02) VALUE ZEROES.
               05  ws-current-time-hundredths    PIC 9(02) VALUE ZEROES.

       01  ws-current-date-and-time-chains-formatted.
           03  ws-current-date-num-formatted     PIC 9999/99/99
                                                 VALUE ZEROES.
           03  ws-current-date-str-formatted     REDEFINES
               ws-current-date-num-formatted     PIC XXXX/XX/XX.
           03  ws-current-time-formatted.
               05  ws-current-time-hour          PIC 9(02) VALUE ZEROES.
               05  FILLER                        PIC X(01) VALUE X'3A'.
               05  ws-current-time-minute        PIC 9(02) VALUE ZEROES.
               05  FILLER                        PIC X(01) VALUE X'3A'.
               05  ws-current-time-second        PIC 9(02) VALUE ZEROES.
               05  FILLER                        PIC X(01) VALUE X'2E'.
               05  ws-current-time-hundredths    PIC 9(02) VALUE ZEROES.

       01  ws-date-and-time-counters-delimiters.
           03  ws-date-time-counter-fields       PIC 9(02) VALUE ZEROES.
           03  ws-date-counters.
               05  ws-date-count-dayname         PIC 9(02) VALUE ZEROES.
               05  ws-date-count-monthname       PIC 9(02) VALUE ZEROES.
               05  ws-date-count-numday          PIC 9(02) VALUE ZEROES.
               05  ws-date-count-year            PIC 9(02) VALUE ZEROES.
           03  ws-date-delimiters.
               05  ws-date-delimit-dayname       PIC X(02) VALUE SPACES.
               05  ws-date-delimit-monthname     PIC X(02) VALUE SPACES.
               05  ws-date-delimit-numday        PIC X(02) VALUE SPACES.
               05  ws-date-delimit-year          PIC X(03) VALUE SPACES.
           03  ws-time-counters.
               05  ws-time-count-hour            PIC 9(02) VALUE ZEROES.
               05  ws-time-count-minute          PIC 9(02) VALUE ZEROES.
               05  ws-time-count-second          PIC 9(02) VALUE ZEROES.
               05  ws-time-count-hundredths      PIC 9(02) VALUE ZEROES.
           03  ws-time-delimiters.
               05  ws-time-delimit-hour          PIC X(01) VALUE SPACE.
               05  ws-time-delimit-minute        PIC X(01) VALUE SPACE.
               05  ws-time-delimit-second        PIC X(01) VALUE SPACE.
               05  ws-time-delimit-hundredths    PIC X(01) VALUE SPACE.

       01  ws-date-and-time-extraction-fields.
           03  ws-current-date-extractions.
               05  ws-date-dayofweek         PIC 9(01) VALUE ZERO.
               05  ws-date-dayname           PIC A(09) VALUE SPACES.
               05  ws-date-monthname         PIC A(09) VALUE SPACES.
               05  ws-date-numday            PIC 9(02) VALUE ZEROES.
               05  ws-date-year              PIC 9(04) VALUE ZEROES.
           03  ws-current-date-time-pointers.
               05  ws-date-pointer-string    PIC 9(02) VALUE ZEROES.
               05  ws-date-pointer-unstring  PIC 9(02) VALUE ZEROES.
           03  ws-current-time-extractions.
               05  ws-time-hour              PIC 9(02) VALUE ZEROES.
               05  ws-time-minute            PIC 9(02) VALUE ZEROES.
               05  ws-time-second            PIC 9(02) VALUE ZEROES.
               05  ws-time-hundredths        PIC 9(02) VALUE ZEROES.

       01  ws-current-date-messages.
           03  ws-current-date-msg-formatted PIC X(43) VALUE SPACES.
           03  ws-current-date-msg-charbychar    REDEFINES
               ws-current-date-msg-formatted OCCURS cte-43 TIMES
               INDEXED BY idx-current-date-msg-charbychar  PIC X(01).

       01  ws-names-days-and-months-arrays.
           03  ws-date-name-of-days.
               05  FILLER          PIC A(09) VALUE "Monday".
               05  FILLER          PIC A(09) VALUE "Tuesday".
               05  FILLER          PIC A(09) VALUE "Wednesday".
               05  FILLER          PIC A(09) VALUE "Thursday".
               05  FILLER          PIC A(09) VALUE "Friday".
               05  FILLER          PIC A(09) VALUE "Saturday".
               05  FILLER          PIC A(09) VALUE "Sunday".
           03  ws-date-name-of-days-red REDEFINES ws-date-name-of-days.
               05  ws-date-name-of-days-array      OCCURS cte-07 TIMES
                   INDEXED BY idx-date-name-of-days-array    PIC A(09).
           03  ws-date-name-of-months.
               05  FILLER          PIC A(09) VALUE "January".
               05  FILLER          PIC A(09) VALUE "February".
               05  FILLER          PIC A(09) VALUE "March".
               05  FILLER          PIC A(09) VALUE "April".
               05  FILLER          PIC A(09) VALUE "May".
               05  FILLER          PIC A(09) VALUE "June".
               05  FILLER          PIC A(09) VALUE "July".
               05  FILLER          PIC A(09) VALUE "August".
               05  FILLER          PIC A(09) VALUE "September".
               05  FILLER          PIC A(09) VALUE "October".
               05  FILLER          PIC A(09) VALUE "November".
               05  FILLER          PIC A(09) VALUE "December".
           03  ws-date-name-of-months-red
               REDEFINES ws-date-name-of-months.
               05  ws-date-name-of-months-array    OCCURS cte-12 TIMES
                   INDEXED BY idx-date-name-of-months-array  PIC A(09).
           03  ws-zodiac-signs-name-of-signs.
               05  ws-zodiac-signs-aries.
                   07  FILLER      PIC A(12) VALUE "Aries".
                   07  ws-zodiac-signs-aries-start.
                       09  FILLER  PIC 9(02) VALUE 03.
                       09  FILLER  PIC 9(02) VALUE 21.
                   07  ws-zodiac-signs-aries-finish.
                       09  FILLER  PIC 9(02) VALUE 04.
                       09  FILLER  PIC 9(02) VALUE 19.
               05  ws-zodiac-signs-taurus.
                   07  FILLER      PIC A(12) VALUE "Taurus".
                   07  ws-zodiac-signs-taurus-start.
                       09  FILLER  PIC 9(02) VALUE 04.
                       09  FILLER  PIC 9(02) VALUE 20.
                   07  ws-zodiac-signs-aries-finish.
                       09  FILLER  PIC 9(02) VALUE 05.
                       09  FILLER  PIC 9(02) VALUE 20.
               05  ws-zodiac-signs-gemini.
                   07  FILLER      PIC A(12) VALUE "Gemini".
                   07  ws-zodiac-signs-gemini-start.
                       09  FILLER  PIC 9(02) VALUE 05.
                       09  FILLER  PIC 9(02) VALUE 21.
                   07  ws-zodiac-signs-gemini-finish.
                       09  FILLER  PIC 9(02) VALUE 06.
                       09  FILLER  PIC 9(02) VALUE 20.
               05  ws-zodiac-signs-cancer.
                   07  FILLER      PIC A(12) VALUE "Cancer".
                   07  ws-zodiac-signs-cancer-start.
                       09  FILLER  PIC 9(02) VALUE 06.
                       09  FILLER  PIC 9(02) VALUE 21.
                   07  ws-zodiac-signs-cancer-finish.
                       09  FILLER  PIC 9(02) VALUE 07.
                       09  FILLER  PIC 9(02) VALUE 22.
               05  ws-zodiac-signs-leo.
                   07  FILLER      PIC A(12) VALUE "Leo".
                   07  ws-zodiac-signs-leo-start.
                       09  FILLER  PIC 9(02) VALUE 07.
                       09  FILLER  PIC 9(02) VALUE 23.
                   07  ws-zodiac-signs-leo-finish.
                       09  FILLER  PIC 9(02) VALUE 08.
                       09  FILLER  PIC 9(02) VALUE 22.
               05  ws-zodiac-signs-virgo.
                   07  FILLER      PIC A(12) VALUE "Virgo".
                   07  ws-zodiac-signs-virgo-start.
                       09  FILLER  PIC 9(02) VALUE 08.
                       09  FILLER  PIC 9(02) VALUE 23.
                   07  ws-zodiac-signs-virgo-finish.
                       09  FILLER  PIC 9(02) VALUE 09.
                       09  FILLER  PIC 9(02) VALUE 22.
               05  ws-zodiac-signs-libra.
                   07  FILLER      PIC A(12) VALUE "Libra".
                   07  ws-zodiac-signs-libra-start.
                       09  FILLER  PIC 9(02) VALUE 09.
                       09  FILLER  PIC 9(02) VALUE 23.
                   07  ws-zodiac-signs-libra-finish.
                       09  FILLER  PIC 9(02) VALUE 10.
                       09  FILLER  PIC 9(02) VALUE 22.
               05  ws-zodiac-signs-scorpio.
                   07  FILLER      PIC A(12) VALUE "Scorpio".
                   07  ws-zodiac-signs-scorpio-start.
                       09  FILLER  PIC 9(02) VALUE 10.
                       09  FILLER  PIC 9(02) VALUE 23.
                   07  ws-zodiac-signs-scorpio-finish.
                       09  FILLER  PIC 9(02) VALUE 11.
                       09  FILLER  PIC 9(02) VALUE 21.
               05  ws-zodiac-signs-sagittarius.
                   07  FILLER      PIC A(12) VALUE "Sagittarius".
                   07  ws-zodiac-signs-sagittarius-start.
                       09  FILLER  PIC 9(02) VALUE 11.
                       09  FILLER  PIC 9(02) VALUE 22.
                   07  ws-zodiac-signs-sagittarius-finish.
                       09  FILLER  PIC 9(02) VALUE 12.
                       09  FILLER  PIC 9(02) VALUE 21.
               05  ws-zodiac-signs-capricorn.
                   07  FILLER      PIC A(12) VALUE "Capricorn".
                   07  ws-zodiac-signs-capricorn-start.
                       09  FILLER  PIC 9(02) VALUE 12.
                       09  FILLER  PIC 9(02) VALUE 22.
                   07  ws-zodiac-signs-capricorn-finish.
                       09  FILLER  PIC 9(02) VALUE 01.
                       09  FILLER  PIC 9(02) VALUE 19.
               05  ws-zodiac-signs-aquarius.
                   07  FILLER      PIC A(12) VALUE "Aquarius".
                   07  ws-zodiac-signs-aquarius-start.
                       09  FILLER  PIC 9(02) VALUE 01.
                       09  FILLER  PIC 9(02) VALUE 20.
                   07  ws-zodiac-signs-aquarius-finish.
                       09  FILLER  PIC 9(02) VALUE 02.
                       09  FILLER  PIC 9(02) VALUE 18.
               05  ws-zodiac-signs-pisces.
                   07  FILLER      PIC A(12) VALUE "Pisces".
                   07  ws-zodiac-signs-pisces-start.
                       09  FILLER  PIC 9(02) VALUE 02.
                       09  FILLER  PIC 9(02) VALUE 19.
                   07  ws-zodiac-signs-pisces-finish.
                       09  FILLER  PIC 9(02) VALUE 03.
                       09  FILLER  PIC 9(02) VALUE 20.
           03  ws-zodiac-signs-name-of-signs-red
               REDEFINES ws-zodiac-signs-name-of-signs.
               05  ws-zodiac-signs-array     OCCURS cte-12 TIMES
                   INDEXED BY idx-zodiac-signs-array.
                   07  ws-zodiac-name                  PIC A(12).
                   07  ws-start-zodiac-date.
                       09  ws-start-zodiac-month       PIC 9(02).
                       09  ws-start-zodiac-day         PIC 9(02).
                   07  ws-finish-zodiac-date.
                       09  ws-finish-zodiac-month      PIC 9(02).
                       09  ws-finish-zodiac-day        PIC 9(02).

       01  ws-punctuation-mark-delimiters.
           03  ws-punctuations-marks.
               05  ws-colon-punctuation-mark PIC X(01) VALUE X'3A'.
               05  ws-comma-punctuation-mark PIC X(01) VALUE X'2C'.
               05  ws-dash-punctuation-mark  PIC X(01) VALUE X'2D'.
               05  ws-dot-punctuation-mark   PIC X(01) VALUE X'2E'.
           03  ws-punctuations-combinated-marks.
               05  ws-comma-and-normal-space PIC X(02) VALUE X'2C20'.
               05  ws-space-dash-space       PIC X(03) VALUE X'202D20'.

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           PERFORM 100000-begin-get-date-time-system
              THRU 100000-end-get-date-time-system

           PERFORM 200000-begin-string-current-date
              THRU 200000-end-string-current-date

           PERFORM 300000-begin-inspect-current-date
              THRU 300000-end-inspect-current-date

           PERFORM 400000-begin-unstring-current-date
              THRU 400000-end-unstring-current-date

           PERFORM 500000-begin-display-breakdowns
              THRU 500000-end-display-breakdowns

           PERFORM 600000-begin-program-completion
              THRU 600000-end-program-completion

           STOP RUN.

       000000-begin-press-enter-key-to-continue.
           DISPLAY "Press the ENTER key to continue..."
              WITH NO ADVANCING
            ACCEPT OMITTED.
       000000-end-press-enter-key-to-continue.
           EXIT.

       100000-begin-get-date-time-system.
           DISPLAY "Program that dismantles a given string of "
                   "characters."

           ACCEPT ws-current-date   FROM DATE YYYYMMDD
           ACCEPT ws-date-dayofweek FROM DAY-OF-WEEK
           ACCEPT ws-current-time   FROM TIME.
       100000-end-get-date-time-system.
           EXIT.

       200000-begin-string-current-date.
           MOVE CORRESPONDING ws-current-time
             TO ws-current-time-formatted
           MOVE SPACES
             TO ws-current-date-msg-formatted
           MOVE cte-01                         TO ws-date-pointer-string

           SET idx-date-name-of-days-array     TO ws-date-dayofweek
           SET idx-date-name-of-months-array   TO ws-current-date-month

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
                  SPACE
                  DELIMITED BY SIZE
                  ws-dash-punctuation-mark
                  DELIMITED BY SIZE
                  SPACE
                  DELIMITED BY SIZE
                  ws-current-time-hour       OF ws-current-time
                  DELIMITED BY SIZE
                  ws-colon-punctuation-mark
                  DELIMITED BY SIZE
                  ws-current-time-minute     OF ws-current-time
                  DELIMITED BY SIZE
                  ws-colon-punctuation-mark
                  DELIMITED BY SIZE
                  ws-current-time-second     OF ws-current-time
                  DELIMITED BY SIZE
                  ws-dot-punctuation-mark
                  DELIMITED BY SIZE
                  ws-current-time-hundredths OF ws-current-time
                  DELIMITED BY SIZE
             INTO ws-current-date-msg-formatted
             WITH POINTER ws-date-pointer-string
               ON OVERFLOW
                  DISPLAY "The destination string is too short."
              NOT ON OVERFLOW
                  DISPLAY "The target string was successfully "
                          "constructed."
           END-STRING

           DISPLAY SPACE
           DISPLAY "[" FUNCTION TRIM(ws-current-date-msg-formatted)"]."

           PERFORM 000000-begin-press-enter-key-to-continue
              THRU 000000-end-press-enter-key-to-continue.
       200000-end-string-current-date.
           EXIT.

       300000-begin-inspect-current-date.
           INITIALIZE ws-account-backspace-len
                      ws-account-frontspace-len
                      ws-account-string-length

           INSPECT ws-current-date-msg-formatted
                   TALLYING ws-account-backspace-len
                        FOR LEADING SPACE
                            ws-account-frontspace-len
                        FOR TRAILING SPACE
                            ws-account-string-length
                        FOR CHARACTERS.

           PERFORM 310000-begin-show-inspection-results
              THRU 310000-end-show-inspection-results.
       300000-end-inspect-current-date.
           EXIT.

        310000-begin-show-inspection-results.
           MOVE LENGTH   OF ws-current-date-msg-formatted
             TO ws-account-field-length

           DISPLAY SPACE
           DISPLAY "Field Accounting Statistics."
           DISPLAY "Lengths."
           DISPLAY "+ Field:     [" ws-account-field-length "]."
           DISPLAY "+ String:    [" ws-account-string-length "]."
           DISPLAY "Spaces."
           DISPLAY "+ Back:      [" ws-account-backspace-len "]."
           DISPLAY "+ Front:     [" ws-account-frontspace-len "]."

           PERFORM 000000-begin-press-enter-key-to-continue
              THRU 000000-end-press-enter-key-to-continue.
        310000-end-show-inspection-results.
           EXIT.

       400000-begin-unstring-current-date.
           MOVE cte-01                       TO ws-date-pointer-unstring
           MOVE SPACES                       TO ws-date-dayname
                                                ws-date-monthname
           MOVE ZEROES                       TO ws-date-numday
                                                ws-date-year
                                                ws-time-hour
                                                ws-time-minute
                                                ws-time-second
                                                ws-time-hundredths

           UNSTRING ws-current-date-msg-formatted
                    DELIMITED BY ws-comma-and-normal-space
                              OR ws-colon-punctuation-mark
                              OR ws-dot-punctuation-mark
                              OR ws-space-dash-space
                              OR ALL SPACE
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
                    ws-time-hour
                        DELIMITER IN ws-time-delimit-hour
                        COUNT IN ws-time-count-hour
                    ws-time-minute
                        DELIMITER IN ws-time-delimit-minute
                        COUNT IN ws-time-count-minute
                    ws-time-second
                        DELIMITER IN ws-time-delimit-second
                        COUNT IN ws-time-count-second
                    ws-time-hundredths
                        DELIMITER IN ws-time-delimit-hundredths
                        COUNT IN ws-time-count-hundredths
               WITH POINTER  ws-date-pointer-unstring
                    TALLYING ws-date-time-counter-fields
                 ON OVERFLOW
                    DISPLAY "The destination fields are too short."
                NOT ON OVERFLOW
                    DISPLAY "The target fields were successfully "
                            "constructed."
           END-UNSTRING

           PERFORM 000000-begin-press-enter-key-to-continue
              THRU 000000-end-press-enter-key-to-continue.
       400000-end-unstring-current-date.
           EXIT.

       500000-begin-display-breakdowns.
           PERFORM 510000-begin-display-current-date-and-time
              THRU 510000-end-display-current-date-and-time

           PERFORM 520000-begin-display-statistics-extraction
              THRU 520000-end-display-statistics-extraction

           PERFORM 530000-begin-display-current-date-extractions
              THRU 530000-end-display-current-date-extractions

           PERFORM 540000-begin-display-current-time-extractions
              THRU 540000-end-display-current-time-extractions

           PERFORM 550000-begin-translate-current-date-time-char-by-char
              THRU 550000-end-translate-current-date-time-char-by-char.
       500000-end-display-breakdowns.
           EXIT.

        510000-begin-display-current-date-and-time.
           DISPLAY SPACE
           DISPLAY "Date formats generated."

           MOVE ws-current-date-num TO ws-current-date-num-formatted
           DISPLAY "[" ws-current-date-num-formatted "]."

           MOVE ws-current-date-str TO ws-current-date-str-formatted
           DISPLAY "[" ws-current-date-str-formatted "] - "
                   "[" ws-current-time-formatted "]."

           DISPLAY "[" FUNCTION TRIM(ws-current-date-msg-formatted) "]."

           PERFORM 000000-begin-press-enter-key-to-continue
              THRU 000000-end-press-enter-key-to-continue.
        510000-end-display-current-date-and-time.
           EXIT.

        520000-begin-display-statistics-extraction.
           DISPLAY SPACE
           DISPLAY "Extraction Statistics."
           DISPLAY "Pointers."
           DISPLAY "+ String:    [" ws-date-pointer-string "]."
           DISPLAY "+ Unstring:  [" ws-date-pointer-unstring "]."
           DISPLAY "+ Fields:    [" ws-date-time-counter-fields "]."   

           PERFORM 000000-begin-press-enter-key-to-continue
              THRU 000000-end-press-enter-key-to-continue.
        520000-end-display-statistics-extraction.
           EXIT.

        530000-begin-display-current-date-extractions.
           DISPLAY SPACE
           DISPLAY "Date."
           DISPLAY "Day Name."
           DISPLAY "+ Value:     [" ws-date-dayname "]."
           DISPLAY "+ Delimiter: [" ws-date-delimit-dayname "]."
           DISPLAY "+ Count:     [" ws-date-count-dayname "]."

           DISPLAY "Month Name."
           DISPLAY "+ Value:     [" ws-date-monthname "]."
           DISPLAY "+ Delimiter: [" ws-date-delimit-monthname "]."
           DISPLAY "+ Count:     [" ws-date-count-monthname "]."

           DISPLAY "Day Number."
           DISPLAY "+ Value:     [" ws-date-numday "]."
           DISPLAY "+ Delimiter: [" ws-date-delimit-numday "]."
           DISPLAY "+ Count:     [" ws-date-count-numday "]."

           DISPLAY "Year."
           DISPLAY "+ Value:     [" ws-date-year "]."
           DISPLAY "+ Delimiter: [" ws-date-delimit-year "]."
           DISPLAY "+ Count:     [" ws-date-count-year "]."

           PERFORM 000000-begin-press-enter-key-to-continue
              THRU 000000-end-press-enter-key-to-continue.
        530000-end-display-current-date-extractions.
           EXIT.

        540000-begin-display-current-time-extractions.
           DISPLAY SPACE
           DISPLAY "Time."
           DISPLAY "Hour."
           DISPLAY "+ Value:     [" ws-time-hour "]."
           DISPLAY "+ Delimiter: [" ws-time-delimit-hour "]."
           DISPLAY "+ Count:     [" ws-time-count-hour "]."

           DISPLAY "Minute."
           DISPLAY "+ Value :    [" ws-time-minute "]."
           DISPLAY "+ Delimiter: [" ws-time-delimit-minute "]."
           DISPLAY "+ Count:     [" ws-time-count-minute "]."

           DISPLAY "Second."
           DISPLAY "+ Value:     [" ws-time-second "]."
           DISPLAY "+ Delimiter: [" ws-time-delimit-second "]."
           DISPLAY "+ Count:     [" ws-time-count-second "]."

           DISPLAY "Hundredths."
           DISPLAY "+ Value:     [" ws-time-hundredths "]."
           DISPLAY "+ Delimiter: [" ws-time-delimit-hundredths "]."
           DISPLAY "+ Count:     [" ws-time-count-hundredths "]."

           PERFORM 000000-begin-press-enter-key-to-continue
              THRU 000000-end-press-enter-key-to-continue.
        540000-end-display-current-time-extractions.
           EXIT.

        550000-begin-translate-current-date-time-char-by-char.
           DISPLAY SPACE
           DISPLAY "Current date strings."

           PERFORM WITH TEST BEFORE
           VARYING idx-current-date-msg-charbychar
              FROM cte-01 BY cte-01
             UNTIL idx-current-date-msg-charbychar
                IS GREATER THAN cte-43
                   DISPLAY ws-current-date-msg-charbychar
                           (idx-current-date-msg-charbychar)
                      WITH NO ADVANCING
           END-PERFORM

           DISPLAY SPACE
           PERFORM 000000-begin-press-enter-key-to-continue
              THRU 000000-end-press-enter-key-to-continue

           DISPLAY SPACE
           PERFORM WITH TEST AFTER
           VARYING idx-current-date-msg-charbychar
              FROM cte-01 BY cte-01
             UNTIL idx-current-date-msg-charbychar
                IS GREATER THAN OR EQUAL TO cte-43
                   DISPLAY ws-current-date-msg-charbychar
                           (idx-current-date-msg-charbychar)
                      WITH NO ADVANCING
           END-PERFORM

           DISPLAY SPACE
           PERFORM 000000-begin-press-enter-key-to-continue
              THRU 000000-end-press-enter-key-to-continue

           DISPLAY SPACE
           PERFORM 551000-begin-find-zodiac-sign-based-on-date
              THRU 551000-end-find-zodiac-sign-based-on-date
           VARYING idx-zodiac-signs-array
              FROM cte-01 BY cte-01
             UNTIL idx-zodiac-signs-array
                IS GREATER THAN cte-12

           DISPLAY SPACE
           PERFORM 000000-begin-press-enter-key-to-continue
              THRU 000000-end-press-enter-key-to-continue.
        550000-end-translate-current-date-time-char-by-char.
           EXIT.

         551000-begin-find-zodiac-sign-based-on-date.
           IF  (ws-current-date-month IS EQUAL TO
                ws-start-zodiac-month  (idx-zodiac-signs-array)
           AND  ws-current-date-day   IS GREATER OR EQUAL TO
                ws-start-zodiac-day    (idx-zodiac-signs-array))
            OR (ws-current-date-month IS EQUAL TO
                ws-finish-zodiac-month (idx-zodiac-signs-array)
           AND  ws-current-date-day   IS LESS OR EQUAL TO
                ws-finish-zodiac-day   (idx-zodiac-signs-array))
            OR (ws-current-date-month IS GREATER THAN
                ws-start-zodiac-month  (idx-zodiac-signs-array)
           AND  ws-current-date-month IS LESS THAN
                ws-finish-zodiac-month (idx-zodiac-signs-array))
                DISPLAY "Zodiac Sign."
                DISPLAY "+ Name:      ["
                         FUNCTION TRIM(
                         ws-zodiac-name (idx-zodiac-signs-array))
                        "]."

                DISPLAY "Start."
                SET idx-date-name-of-months-array
                 TO ws-start-zodiac-month (idx-zodiac-signs-array)
                DISPLAY "+ Month:     ["
                        ws-start-zodiac-month
                        (idx-zodiac-signs-array)
                        "] = ["
                        FUNCTION TRIM(
                        ws-date-name-of-months-array
                        (idx-date-name-of-months-array))
                        "]."
                DISPLAY "+ Day:       ["
                         ws-start-zodiac-day (idx-zodiac-signs-array)
                        "]."

                DISPLAY "Finish."
                SET idx-date-name-of-months-array
                 TO ws-finish-zodiac-month (idx-zodiac-signs-array)
                DISPLAY "+ Month:     ["
                        ws-finish-zodiac-month
                        (idx-zodiac-signs-array)
                        "] = ["
                        FUNCTION TRIM(
                        ws-date-name-of-months-array
                        (idx-date-name-of-months-array))
                        "]."
                DISPLAY "+ Day:       ["
                        ws-finish-zodiac-day
                        (idx-zodiac-signs-array)
                        "]."

                SET idx-zodiac-signs-array TO cte-12
           END-IF.
         551000-end-find-zodiac-sign-based-on-date.
           EXIT.

       600000-begin-program-completion.
           DISPLAY SPACE
           DISPLAY "Done!"
           DISPLAY "This program has ended."

           PERFORM 000000-begin-press-enter-key-to-continue
              THRU 000000-end-press-enter-key-to-continue.
       600000-end-program-completion.
           EXIT.

       END PROGRAM Strings.
