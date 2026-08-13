       IDENTIFICATION DIVISION.
       PROGRAM-ID. PrimeNumber.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       78  cte-01                                            VALUE 01.
       78  cte-02                                            VALUE 02.
       78  cte-1000                                          VALUE 1000.

       01  ws-working-variables.
           03  ws-prime-numbers-group.
               05  ws-group-indexes-prime-numbers.
                   07  ws-idx-acc-prime-numbers USAGE INDEX  VALUE ZERO.
                   07  ws-idx-cnt-prime-numbers USAGE INDEX  VALUE ZERO.
                   07  ws-idx-num-prime-numbers USAGE INDEX  VALUE ZERO.
               05  ws-group-print-prime-numbers.
                   07  ws-print-prime-index     PIC -Z(11)   VALUE ZERO.
                   07  ws-print-prime-number    PIC -Z(11)   VALUE ZERO.
               05  ws-quantity-prime-numbers    UNSIGNED-INT VALUE ZERO.
               05  ws-quottients-prime-numbers  UNSIGNED-INT VALUE ZERO.
           03  ws-prime-numbers-division-results.
               05  ws-quottient-cnt-num-p-num   UNSIGNED-INT VALUE ZERO.
               05  ws-remainder-cnt-num-p-num   UNSIGNED-INT VALUE ZERO.

       01  ws-prime-numbers-table.
           03  ws-array-prime-numbers          OCCURS cte-01 TO cte-1000
               DEPENDING ON ws-quantity-prime-numbers
               ASCENDING KEY ws-array-prime-numbers-value
               INDEXED BY idx-array-prime-numbers.
               05  ws-array-prime-numbers-value USAGE INDEX  VALUE ZERO.

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           PERFORM 100000-start-begin-program
              THRU 100000-finish-begin-program

           PERFORM 200000-start-process-program
              THRU 200000-finish-process-program

           PERFORM 300000-start-end-program
              THRU 300000-finish-end-program

           STOP RUN.

       100000-start-begin-program.
           SET idx-array-prime-numbers TO cte-01

           DISPLAY "Prime Number Generator."
           DISPLAY "Number of primes to generate ("
                   cte-01 " - " cte-1000 "): "
              WITH NO ADVANCING
            ACCEPT ws-quantity-prime-numbers

           PERFORM 110000-start-press-enter-key-to-continue
              THRU 110000-finish-press-enter-key-to-continue.           
       100000-finish-begin-program.
           EXIT.

        110000-start-press-enter-key-to-continue.
           DISPLAY "Press the ENTER key to continue..."
              WITH NO ADVANCING
           ACCEPT OMITTED.
        110000-finish-press-enter-key-to-continue.
           EXIT.

       200000-start-process-program.
           DISPLAY SPACE
           DISPLAY "Prime Numbers List."

           DISPLAY SPACE
           DISPLAY "Creating list..."
           SET ws-idx-acc-prime-numbers TO ZERO

           PERFORM 210000-start-prime-numbers-generator
              THRU 210000-finish-prime-numbers-generator
           VARYING ws-idx-cnt-prime-numbers
              FROM cte-01 BY cte-01
             UNTIL ws-idx-acc-prime-numbers
                IS GREATER THAN OR IS EQUAL TO
                   ws-quantity-prime-numbers
                OR idx-array-prime-numbers
                IS GREATER THAN cte-1000

           DISPLAY X"5B" ws-idx-acc-prime-numbers   X"5D"
                   X"20" "Output results generated" X"2E"

            PERFORM 110000-start-press-enter-key-to-continue
               THRU 110000-finish-press-enter-key-to-continue

           DISPLAY SPACE
           DISPLAY "Viewing list..."
           MOVE ZEROES TO ws-idx-acc-prime-numbers

           PERFORM 220000-start-print-array-prime-numbers
              THRU 220000-finish-print-array-prime-numbers
           VARYING idx-array-prime-numbers
              FROM cte-01 BY cte-01
             UNTIL idx-array-prime-numbers
                IS GREATER THAN ws-quantity-prime-numbers
                OR idx-array-prime-numbers
                IS GREATER THAN cte-1000

           DISPLAY X"5B" ws-idx-acc-prime-numbers   X"5D"
                   X"20" "Output results generated" X"2E"

            PERFORM 110000-start-press-enter-key-to-continue
               THRU 110000-finish-press-enter-key-to-continue.
       200000-finish-process-program.
           EXIT.

        210000-start-prime-numbers-generator.
           MOVE ZEROES    TO ws-quottients-prime-numbers

           PERFORM 211000-start-prime-numbers-check-cycle
              THRU 211000-finish-prime-numbers-check-cycle
           VARYING ws-idx-num-prime-numbers
              FROM cte-01 BY cte-01
             UNTIL ws-idx-num-prime-numbers
                IS GREATER THAN ws-idx-cnt-prime-numbers

           IF (ws-quottients-prime-numbers   IS EQUAL TO cte-02)
               SET ws-idx-acc-prime-numbers  UP BY cte-01

               MOVE idx-array-prime-numbers  TO ws-print-prime-index
               MOVE ws-idx-cnt-prime-numbers TO ws-print-prime-number

               DISPLAY X"28"
                       FUNCTION TRIM(ws-print-prime-index)
                       X"29" X"20" X"3A"
                       X"20" X"5B"
                       FUNCTION TRIM(ws-print-prime-number)
                       X"5D" X"2E"
               END-DISPLAY

               MOVE ws-idx-cnt-prime-numbers
                 TO ws-array-prime-numbers-value
                    (idx-array-prime-numbers)

                SET idx-array-prime-numbers  UP BY cte-01
           END-IF.
        210000-finish-prime-numbers-generator.
           EXIT.

         211000-start-prime-numbers-check-cycle.
           DIVIDE ws-idx-num-prime-numbers
             INTO ws-idx-cnt-prime-numbers
                  GIVING ws-quottient-cnt-num-p-num
                  REMAINDER ws-remainder-cnt-num-p-num
           END-DIVIDE

           IF ws-remainder-cnt-num-p-num IS EQUAL TO ZEROS
              ADD cte-01                 TO ws-quottients-prime-numbers
           END-IF.
         211000-finish-prime-numbers-check-cycle.
           EXIT.

        220000-start-print-array-prime-numbers.
           ADD cte-01                    TO ws-idx-acc-prime-numbers

           MOVE idx-array-prime-numbers  TO ws-print-prime-index
           MOVE ws-array-prime-numbers-value
               (idx-array-prime-numbers) TO ws-print-prime-number

           DISPLAY X"28"
                   FUNCTION TRIM(ws-print-prime-index)
                   X"29" X"20" X"3A"
                   X"20" X"5B"
                   FUNCTION TRIM(ws-print-prime-number)
                   X"5D" X"2E"
           END-DISPLAY.
        220000-finish-print-array-prime-numbers.
           EXIT.

       300000-start-end-program.
           DISPLAY SPACE
           DISPLAY "Done" X"21"	
           DISPLAY "This program has ended" X"2E"

            PERFORM 110000-start-press-enter-key-to-continue
               THRU 110000-finish-press-enter-key-to-continue.
       300000-finish-end-program.
           EXIT.

       END-PROGRAM. PrimeNumber.
