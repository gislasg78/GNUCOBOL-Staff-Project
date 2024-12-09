       IDENTIFICATION DIVISION.
       PROGRAM-ID. NumGrTrT.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  ws-environmental-variables.
           05  ws-number-1  PIC S9(05)V9(02) COMP-3 VALUE ZEROES.
           05  ws-number-2  PIC S9(05)V9(02) COMP-3 VALUE ZEROES.
           05  ws-number-3  PIC S9(05)V9(02) COMP-3 VALUE ZEROES.

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           DISPLAY "Program that validates the largest "
                   "of three integers.".

           DISPLAY "Enter the first number : "
              WITH NO ADVANCING
            ACCEPT ws-number-1

           DISPLAY "Enter the second number: "
              WITH NO ADVANCING
            ACCEPT ws-number-2

           DISPLAY "Enter the third number : "
              WITH NO ADVANCING
            ACCEPT ws-number-3

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
           END-IF

           STOP RUN.

       END PROGRAM NumGrTrT.
