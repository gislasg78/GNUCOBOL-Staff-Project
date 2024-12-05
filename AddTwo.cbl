       IDENTIFICATION DIVISION.
       PROGRAM-ID. AddTwo.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  ws-environment-variables.
           03  ws-num1    PIC S9(06)V9(04)    VALUE ZEROES.
           03  ws-num2    PIC S9(06)V9(04)    VALUE ZEROES.
           03  ws-result  PIC +Z,ZZZ,ZZZ.ZZZZ VALUE ZEROES.

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           DISPLAY "Program that adds two numbers."
           DISPLAY "Enter the first  number: "
              WITH NO ADVANCING
            ACCEPT ws-num1

           DISPLAY "Enter the second number: "
              WITH NO ADVANCING
            ACCEPT ws-num2

           ADD ws-num1 TO ws-num2 GIVING ws-result
            ON SIZE ERROR
               DISPLAY "The calculation is too large "
                       "to be stored in the receiving"
                       "variable."
           END-ADD

           DISPLAY SPACE
           DISPLAY "The result of adding the first "
           "number: [" ws-num1 "] plus the "
           "second number: [" ws-num2 "] is: "
           "[" ws-result "]."

           STOP RUN.

       END PROGRAM AddTwo.
