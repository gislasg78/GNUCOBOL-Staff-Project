       IDENTIFICATION DIVISION.
       PROGRAM-ID. NumberGreater.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  ws-environmental-variables.
           03  ws-numbers.
               05  ws-number-1 USAGE FLOAT VALUE ZEROES.
               05  ws-number-2 USAGE FLOAT VALUE ZEROES.
               05  ws-number-3 USAGE FLOAT VALUE ZEROES.
           03  ws-major-number USAGE FLOAT VALUE ZEROES.

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           PERFORM 100000-start-capture-data-numbers
              THRU 100000-finish-capture-data-numbers

           PERFORM 200000-start-compare-data-numbers
              THRU 200000-finish-compare-data-numbers

           DISPLAY SPACE
           DISPLAY "This programa has ended..." WITH NO ADVANCING
           ACCEPT OMITTED

           STOP RUN.

       100000-start-capture-data-numbers.
           DISPLAY "Program that validates the largest of three "
                   "integers.".

           DISPLAY "Enter the first number  : "
              WITH NO ADVANCING
            ACCEPT ws-number-1

           DISPLAY "Enter the second number : "
              WITH NO ADVANCING
            ACCEPT ws-number-2

           DISPLAY "Enter the third number  : "
              WITH NO ADVANCING
            ACCEPT ws-number-3.
       100000-finish-capture-data-numbers.
           EXIT.

       200000-start-compare-data-numbers.
           PERFORM 210000-start-first-type-of-compare-data
              THRU 210000-finish-first-type-of-compare-data

           PERFORM 220000-start-second-type-of-compare-data
              THRU 220000-finish-second-type-of-compare-data.
       200000-finish-compare-data-numbers.
           EXIT.

        210000-start-first-type-of-compare-data.
           DISPLAY SPACE

           IF  ws-number-1 >= ws-number-2
           AND ws-number-1 >= ws-number-3
               DISPLAY "The first number: ["
                       ws-number-1
                       "] is greater than second number: ["
                       ws-number-2
                       "] and third number: ["
                       ws-number-3
                       "]."
           ELSE
               IF  ws-number-2 >= ws-number-1
               AND ws-number-2 >= ws-number-3
                   DISPLAY "The second number: ["
                           ws-number-2
                           "] is greater than first number: ["
                           ws-number-1
                           "] and third number: ["
                           ws-number-3
                           "]."
               ELSE
                   IF  ws-number-3 >= ws-number-1
                   AND ws-number-3 >= ws-number-1
                       DISPLAY "The third number: ["
                               ws-number-3 
                               "] is greater than first number: ["
                               ws-number-1 
                               "] and second number: ["
                               ws-number-2
                               "]."
                   END-IF
               END-IF
           END-IF.
        210000-finish-first-type-of-compare-data.
           EXIT.

        220000-start-second-type-of-compare-data.
           DISPLAY SPACE

           IF ws-number-1 IS GREATER THAN ws-number-2
              IF ws-number-1 IS GREATER THAN ws-number-3
                 MOVE ws-number-1 TO ws-major-number
                 DISPLAY "The first number is the largest: ["
                          ws-number-1 "]."
              ELSE
                 MOVE ws-number-2 TO ws-major-number
                 DISPLAY "The second number is the largest: ["
                         ws-number-2 "]."
              END-IF
           ELSE
              IF ws-number-2 IS GREATER THAN ws-number-3
                 MOVE ws-number-2 TO ws-major-number
                 DISPLAY "The second number is the largest: ["
                         ws-number-2 "]."
              ELSE
                 MOVE ws-number-3 TO ws-major-number
                 DISPLAY "The third number is the largest: ["
                         ws-number-3 "]."
              END-IF
           END-IF.
        220000-finish-second-type-of-compare-data.
           EXIT.

       END PROGRAM NumberGreater.
