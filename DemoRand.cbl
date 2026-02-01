       IDENTIFICATION DIVISION.
       PROGRAM-ID. RandomNumbers.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       78  cte-01                                           VALUE 01.

       01  ws-random-number-generator-vars.
           03  ws-amounts-tickets.
               05  ws-amount-num-tickets       USAGE COMP-1 VALUE ZEROS.
               05  ws-amount-srs-by-ticket     USAGE COMP-1 VALUE ZEROS.
               05  ws-amount-num-srs-by-ticket USAGE COMP-1 VALUE ZEROS.
           03  ws-indexes-tickets.
               05  ws-idx-num-tickets          USAGE COMP-1 VALUE ZEROS.
               05  ws-idx-srs-by-ticket        USAGE COMP-1 VALUE ZEROS.
               05  ws-idx-num-srs-by-ticket    USAGE COMP-1 VALUE ZEROS.
           03  ws-range-random-values.
               05  ws-difference-range-value   USAGE COMP-1 VALUE ZEROS.
               05  ws-final-range-value        USAGE COMP-1 VALUE ZEROS.
               05  ws-format-final-range-value PIC -(10)    VALUE ZEROS.
               05  ws-product-range-value      USAGE COMP-1 VALUE ZEROS.
           03  ws-regenerated-values.
               05  ws-counter-random-numbers   SIGNED-INT   VALUE ZEROS.
               05  ws-counter-series-by-ticket SIGNED-INT   VALUE ZEROS.
               05  ws-maximum-random-value     USAGE COMP-1 VALUE ZEROS.
               05  ws-minimum-random-value     USAGE COMP-1 VALUE ZEROS.
               05  ws-pseudo-random-number     USAGE COMP-1 VALUE ZEROS.
               05  ws-seed-rnd-numbers         USAGE COMP-1 VALUE ZEROS.

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           DISPLAY "Random Number Generator Program."

           PERFORM 100000-start-request-performance-data
              THRU 100000-finish-request-performance-data

           PERFORM 200000-start-set-random-seed
              THRU 200000-finish-set-random-seed

           PERFORM 300000-start-ticket-generator
              THRU 300000-finish-ticket-generator
           VARYING ws-idx-num-tickets
              FROM cte-01 BY cte-01
             UNTIL ws-idx-num-tickets
                IS GREATER THAN ws-amount-num-tickets

           DISPLAY "Final accounting statistics."
           DISPLAY X'5B' ws-counter-series-by-ticket X'5D'
                   X'20' 'Accumulated series created' X'2E'
           DISPLAY X'5B' ws-counter-random-numbers X'5D'
                   X'20' 'Generated output numbers' X'2E'

           DISPLAY SPACE
           DISPLAY "This program has ended."
           DISPLAY "Press the ENTER key to end the program..."
              WITH NO ADVANCING
           ACCEPT OMITTED

           STOP RUN.

       100000-start-request-performance-data.
           DISPLAY SPACE
           DISPLAY "+---+----+---+----+---+----+"
           DISPLAY "|Random Number Stabilizer. |"
           DISPLAY "+---+----+---+----+---+----+"

           DISPLAY "How many tickets do you want to generate? : "
              WITH NO ADVANCING
            ACCEPT ws-amount-num-tickets

           DISPLAY "How many series per ticket do you want to generate?"
                   " : "
              WITH NO ADVANCING
            ACCEPT ws-amount-srs-by-ticket

           DISPLAY "How many numbers per series for each ticket "
                   "do you want to generate? : "
              WITH NO ADVANCING 
            ACCEPT ws-amount-num-srs-by-ticket

           DISPLAY "Minimum value: " WITH NO ADVANCING
            ACCEPT ws-minimum-random-value

           DISPLAY "Maximum value: " WITH NO ADVANCING
            ACCEPT ws-maximum-random-value

           DISPLAY "Seed number to generate the numbers  : "
              WITH NO ADVANCING
            ACCEPT ws-seed-rnd-numbers.
       100000-finish-request-performance-data.
           EXIT.

       200000-start-set-random-seed.
           MOVE FUNCTION RANDOM (ws-seed-rnd-numbers)
             TO ws-pseudo-random-number

           DISPLAY SPACE
           DISPLAY "+---+----+---+----+---+----+"
           DISPLAY "| Random Number Generator. |"
           DISPLAY "+---+----+---+----+---+----+"
           DISPLAY "| Random seed  set     : "
                   "[" ws-seed-rnd-numbers "]."
           DISPLAY "| First random startup : "
                   "[" ws-pseudo-random-number "]."
           DISPLAY "+---+----+---+----+---+----+"
           DISPLAY "Press the ENTER key to begin..."
              WITH NO ADVANCING
           ACCEPT OMITTED

           DISPLAY SPACE.
       200000-finish-set-random-seed.
           EXIT.

       300000-start-ticket-generator.
           DISPLAY "Ticket" X"20" X"23" X"3A" X"20" X"5B"
                   ws-idx-num-tickets
                   X"5D" X"20"
                   "of"
                   X"3A" X"20" X"5B"
                   ws-amount-num-tickets
                   X"5D" X"2E"

           PERFORM 310000-start-series-by-ticket-generator
              THRU 310000-finish-series-by-ticket-generator
            VARYING ws-idx-srs-by-ticket
               FROM cte-01 BY cte-01
              UNTIL ws-idx-srs-by-ticket
                 IS GREATER THAN ws-amount-srs-by-ticket

           DISPLAY SPACE.
       300000-finish-ticket-generator.
           EXIT.

        310000-start-series-by-ticket-generator.
           ADD cte-01           TO ws-counter-series-by-ticket

           DISPLAY X"23" X"3A" X"20" X"5B"
                   ws-idx-srs-by-ticket
                   X"5D" X"20"
                   "of"
                   X"3A" X"20" X"5B"
                   ws-amount-srs-by-ticket
                   X"5D" X"2E" X"09"
              WITH NO ADVANCING

           PERFORM 311000-start-nums-by-series-by-ticket-generator
              THRU 311000-finish-nums-by-series-by-ticket-generator
           VARYING ws-idx-num-srs-by-ticket
              FROM cte-01 BY cte-01
             UNTIL ws-idx-num-srs-by-ticket
                IS GREATER THAN ws-amount-num-srs-by-ticket

           DISPLAY SPACE.
        310000-finish-series-by-ticket-generator.
           EXIT.

         311000-start-nums-by-series-by-ticket-generator.
           ADD cte-01           TO ws-counter-random-numbers
           MOVE FUNCTION RANDOM TO ws-pseudo-random-number

           SUBTRACT ws-minimum-random-value FROM ws-maximum-random-value
             GIVING ws-difference-range-value
           ADD cte-01                      TO ws-difference-range-value
           MULTIPLY ws-difference-range-value BY ws-pseudo-random-number
             GIVING ws-product-range-value
           ADD ws-product-range-value         TO ws-minimum-random-value
           GIVING ws-final-range-value
                  ws-format-final-range-value

           DISPLAY X"5B" FUNCTION TRIM(ws-format-final-range-value)
                   X"5D" X"2E" X"09" WITH NO ADVANCING.
         311000-finish-nums-by-series-by-ticket-generator.
           EXIT.

       END PROGRAM RandomNumbers.
