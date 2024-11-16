       IDENTIFICATION DIVISION.
       PROGRAM-ID. DemoRand.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  ws-cte-01                         USAGE COMP-1 VALUE 1.

       01  ws-random-number-generator-vars.
           03  ws-amount-rnd-numbers         USAGE COMP-1 VALUE ZEROES.
           03  ws-idx-rnd-numbers            USAGE COMP-1 VALUE ZEROES.
           03  ws-max-idx-rnd-numbers        USAGE COMP-1 VALUE 100000.
           03  ws-pseudo-random-number       USAGE COMP-1 VALUE ZEROES.
           03  ws-seed-rnd-numbers           USAGE COMP-1 VALUE ZEROES.

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           DISPLAY "Random Number Generator Program."

           PERFORM 100000-start-request-performance-dat
              THRU 100000-finish-request-performance-dat

           PERFORM 200000-start-set-random-seed
              THRU 200000-finish-set-random-seed

           PERFORM 300000-start-random-number-germinator
              THRU 300000-finish-random-number-germinator
           VARYING ws-idx-rnd-numbers
              FROM ws-cte-01 BY ws-cte-01
             UNTIL ws-idx-rnd-numbers
                IS GREATER THAN ws-amount-rnd-numbers
                OR ws-amount-rnd-numbers
                IS GREATER THAN ws-max-idx-rnd-numbers

           STOP "This program has ended..."
           STOP RUN.

       100000-start-request-performance-dat.
           DISPLAY SPACE
           DISPLAY "+---+----+---+----+---+----+"
           DISPLAY "| Random Number Stabilizer.|"
           DISPLAY "+---+----+---+----+---+----+"
           DISPLAY "Enter only ranges between: ["
                   ws-cte-01 "] and [" ws-max-idx-rnd-numbers 
                   "] numbers."

           DISPLAY "How many random numbers do you want? : "
              WITH NO ADVANCING
            ACCEPT ws-amount-rnd-numbers

           DISPLAY "Seed number to generate the numbers  : "
              WITH NO ADVANCING
            ACCEPT ws-seed-rnd-numbers.
       100000-finish-request-performance-dat.
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

       300000-start-random-number-germinator.
           MOVE FUNCTION RANDOM TO ws-pseudo-random-number

           DISPLAY "# ["    ws-idx-rnd-numbers
                   "] of [" ws-amount-rnd-numbers 
                   "] = {"  ws-pseudo-random-number
                   "}.".
       300000-finish-random-number-germinator.
           EXIT.

       END PROGRAM DemoRand.
