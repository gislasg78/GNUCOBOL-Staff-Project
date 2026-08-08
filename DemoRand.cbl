       IDENTIFICATION DIVISION.
       PROGRAM-ID. RandomNumbers.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       78  cte-01                                           VALUE 01.
       78  cte-num-tickets                                  VALUE 12.
       78  cte-srs-by-ticket                                VALUE 12.
       78  cte-num-srs-by-ticket                            VALUE 12.

       01  ws-random-number-generator-vars.
           03  ws-amounts-variables.
               05  ws-amount-num-tickets       USAGE COMP-1 VALUE ZEROS.
               05  ws-amount-srs-by-ticket     USAGE COMP-1 VALUE ZEROS.
               05  ws-amount-num-srs-by-ticket USAGE COMP-1 VALUE ZEROS.
           03  ws-count-variables.
               05  ws-count-num-tickets        SIGNED-INT   VALUE ZEROS.
               05  ws-count-srs-by-ticket      SIGNED-INT   VALUE ZEROS.
               05  ws-count-num-srs-by-ticket  SIGNED-INT   VALUE ZEROS.
           03  ws-index-variables.
               05  ws-idx-num-tickets          USAGE COMP-1 VALUE ZEROS.
               05  ws-idx-srs-by-ticket        USAGE COMP-1 VALUE ZEROS.
               05  ws-idx-num-srs-by-ticket    USAGE COMP-1 VALUE ZEROS.
           03  ws-range-random-values.
               05  ws-difference-range-value   USAGE COMP-1 VALUE ZEROS.
               05  ws-final-range-value        USAGE COMP-1 VALUE ZEROS.
               05  ws-format-final-range-value PIC -(11)    VALUE ZEROS.
               05  ws-product-range-value      USAGE COMP-1 VALUE ZEROS.
           03  ws-regenerated-values.
               05  ws-maximum-random-value     USAGE COMP-1 VALUE ZEROS.
               05  ws-minimum-random-value     USAGE COMP-1 VALUE ZEROS.
               05  ws-pseudo-random-number     USAGE COMP-1 VALUE ZEROS.
               05  ws-seed-rnd-numbers         USAGE COMP-1 VALUE ZEROS.

       01  ws-table-group-random-numbers.
           03  ws-array-random-num-tickets
               OCCURS cte-num-tickets TIMES
               INDEXED BY idx-num-tickets.
               05  ws-array-random-srs-by-ticket
                   OCCURS cte-srs-by-ticket TIMES
                   INDEXED BY idx-srs-by-ticket.
                   07  ws-array-random-num-srs-by-ticket
                       OCCURS cte-num-srs-by-ticket TIMES
                       INDEXED BY idx-num-srs-by-ticket.
                       09  ws-array-random-value USAGE COMP-1
                                                 VALUE ZEROES.

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           DISPLAY "Random Number Generator Program."

           PERFORM 100000-start-request-performance-data
              THRU 100000-finish-request-performance-data

           PERFORM 200000-start-set-random-seed
              THRU 200000-finish-set-random-seed

           PERFORM 300000-start-num-tickets-srs-by-ticket-num-generator
              THRU 300000-finish-num-tickets-srs-by-ticket-num-generator
           VARYING idx-num-tickets
              FROM cte-01 BY cte-01
             UNTIL idx-num-tickets
                IS GREATER THAN ws-amount-num-tickets
                OR IS GREATER THAN cte-num-tickets
             AFTER idx-srs-by-ticket
              FROM cte-01 BY cte-01
             UNTIL idx-srs-by-ticket
                IS GREATER THAN ws-amount-srs-by-ticket
                OR IS GREATER THAN cte-srs-by-ticket
             AFTER idx-num-srs-by-ticket
              FROM cte-01 BY cte-01
             UNTIL idx-num-srs-by-ticket
                IS GREATER THAN ws-amount-num-srs-by-ticket
                OR IS GREATER THAN cte-num-srs-by-ticket

           PERFORM 400000-start-print-num-tickets
              THRU 400000-finish-print-num-tickets
           VARYING idx-num-tickets
              FROM cte-01 BY cte-01
             UNTIL idx-num-tickets
                IS GREATER THAN ws-amount-num-tickets
                OR IS GREATER THAN cte-num-tickets

           PERFORM 120000-start-enter-a-key-to-continue
              THRU 120000-finish-enter-a-key-to-continue

           PERFORM 500000-start-final-accounting-statistics
              THRU 500000-finish-final-accounting-statistics

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
            ACCEPT ws-seed-rnd-numbers

           PERFORM 110000-start-maximum-limits-validator
              THRU 110000-finish-maximum-limits-validator

           PERFORM 120000-start-enter-a-key-to-continue
              THRU 120000-finish-enter-a-key-to-continue.
       100000-finish-request-performance-data.
           EXIT.

        110000-start-maximum-limits-validator.
           DISPLAY SPACE
           DISPLAY "Limit validation process under review..."

           DISPLAY "Tickets."
           DISPLAY X"2B" X"20" "Maximum"       X"3A" X"20" X"20" X"5B"
                   cte-num-tickets             X"5D" X"2E" 
           DISPLAY X"2B" X"20" "Quantity"      X"3A" X"20" X"5B"
                   ws-amount-num-tickets       X"5D" X"2E"
           
           IF (ws-amount-num-tickets       IS GREATER THAN
               cte-num-tickets)
              MOVE cte-num-tickets         TO ws-amount-num-tickets
           END-IF

           DISPLAY "Series by ticket."
           DISPLAY X"2B" X"20" "Maximum"       X"3A" X"20" X"20" X"5B"
                   cte-srs-by-ticket           X"5D" X"2E" 
           DISPLAY X"2B" X"20" "Quantity"      X"3A" X"20" X"5B"
                   ws-amount-srs-by-ticket     X"5D" X"2E"

           IF (ws-amount-srs-by-ticket     IS GREATER THAN
               cte-srs-by-ticket)
              MOVE cte-srs-by-ticket       TO ws-amount-srs-by-ticket
           END-IF

           DISPLAY "Num series by ticket."
           DISPLAY X"2B" X"20" "Maximum"       X"3A" X"20" X"20" X"5B"
                   cte-num-srs-by-ticket       X"5D" X"2E" 
           DISPLAY X"2B" X"20" "Quantity"      X"3A" X"20" X"5B"
                   ws-amount-num-srs-by-ticket X"5D" X"2E"

           IF (ws-amount-num-srs-by-ticket IS GREATER THAN
               cte-num-srs-by-ticket)
              MOVE cte-num-srs-by-ticket  TO ws-amount-num-srs-by-ticket
           END-IF.
        110000-finish-maximum-limits-validator.
           EXIT.

        120000-start-enter-a-key-to-continue.
           DISPLAY "Press the ENTER key to continue..."
              WITH NO ADVANCING
           ACCEPT OMITTED.
        120000-finish-enter-a-key-to-continue.
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

           PERFORM 120000-start-enter-a-key-to-continue
              THRU 120000-finish-enter-a-key-to-continue

           DISPLAY SPACE.
       200000-finish-set-random-seed.
           EXIT.

       300000-start-num-tickets-srs-by-ticket-num-generator.
           MOVE FUNCTION RANDOM          TO ws-pseudo-random-number

           SUBTRACT ws-minimum-random-value FROM ws-maximum-random-value
             GIVING ws-difference-range-value
           ADD cte-01                    TO ws-difference-range-value
           MULTIPLY ws-difference-range-value BY ws-pseudo-random-number
             GIVING ws-product-range-value
           ADD ws-product-range-value    TO ws-minimum-random-value
           GIVING ws-final-range-value

           MOVE ws-final-range-value     TO ws-array-random-value
                                           (idx-num-tickets,
                                            idx-srs-by-ticket,
                                            idx-num-srs-by-ticket).
       300000-finish-num-tickets-srs-by-ticket-num-generator.

       400000-start-print-num-tickets.
           ADD cte-01                    TO ws-count-num-tickets
                                            ws-idx-num-tickets
           MOVE ZEROES                   TO ws-idx-srs-by-ticket

           DISPLAY "Ticket"
                   X"20" X"23" X"3A" X"20" X"5B"
                   ws-idx-num-tickets
                   X"5D" X"20"
                   "of"
                   X"3A" X"20" X"5B"
                   ws-amount-num-tickets
                   X"5D" X"2E"

           PERFORM 410000-start-print-srs-by-ticket
              THRU 410000-finish-print-srs-by-ticket
           VARYING idx-srs-by-ticket
              FROM cte-01 BY cte-01
             UNTIL idx-srs-by-ticket
                IS GREATER THAN ws-amount-srs-by-ticket
                OR IS GREATER THAN cte-srs-by-ticket

           DISPLAY SPACE.
       400000-finish-print-num-tickets.
           EXIT.

        410000-start-print-srs-by-ticket.
           ADD cte-01                    TO ws-count-srs-by-ticket
                                            ws-idx-srs-by-ticket
           MOVE ZEROES                   TO ws-idx-num-srs-by-ticket

           DISPLAY X"23" X"3A" X"20" X"5B"
                   ws-idx-srs-by-ticket
                   X"5D" X"20"
                   "of"
                   X"3A" X"20" X"5B"
                   ws-amount-srs-by-ticket
                   X"5D" X"2E" X"09"
              WITH NO ADVANCING

           PERFORM 411000-start-print-num-srs-by-ticket
              THRU 411000-finish-print-num-srs-by-ticket
           VARYING idx-num-srs-by-ticket
              FROM cte-01 BY cte-01
             UNTIL idx-num-srs-by-ticket
                IS GREATER THAN ws-amount-num-srs-by-ticket
                OR IS GREATER THAN cte-num-srs-by-ticket

           DISPLAY SPACE.
        410000-finish-print-srs-by-ticket.
           EXIT.

         411000-start-print-num-srs-by-ticket.
           ADD cte-01                    TO ws-count-num-srs-by-ticket

           MOVE ws-array-random-value
               (idx-num-tickets, idx-srs-by-ticket,
                idx-num-srs-by-ticket)
             TO ws-format-final-range-value

           DISPLAY X"5B" FUNCTION TRIM(ws-format-final-range-value)
                   X"5D" X"2E" X"09" WITH NO ADVANCING.
         411000-finish-print-num-srs-by-ticket.
           EXIT.

       500000-start-final-accounting-statistics.
           DISPLAY SPACE
           DISPLAY "Final accounting statistics."
           DISPLAY X"5B" ws-count-num-tickets         X"5D"
                   X"20" "Tickets generated"          X"2E"
           DISPLAY X"5B" ws-count-srs-by-ticket       X"5D"
                   X"20" "Accumulated series created" X"2E"
           DISPLAY X"5B" ws-count-num-srs-by-ticket   X"5D"
                   X"20" "Generated output numbers"   X"2E"

           PERFORM 120000-start-enter-a-key-to-continue
              THRU 120000-finish-enter-a-key-to-continue

           PERFORM 410000-start-program-termination-message
              THRU 410000-finish-program-termination-message.
       500000-finish-final-accounting-statistics.
           EXIT.

        410000-start-program-termination-message.
           DISPLAY SPACE
           DISPLAY "Done!"
           DISPLAY "This program has ended."

           PERFORM 120000-start-enter-a-key-to-continue
              THRU 120000-finish-enter-a-key-to-continue.
        410000-finish-program-termination-message.
           EXIT.

       END PROGRAM RandomNumbers.
