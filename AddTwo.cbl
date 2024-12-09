       IDENTIFICATION DIVISION.
       PROGRAM-ID. AddTwo.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  ws-environment-variables.
           03  ws-amount1  PIC S9(09)V9(04)       COMP   VALUE ZEROES.
           03  ws-amount2  PIC S9(09)V9(04)       COMP-3 VALUE ZEROES.
           03  ws-result   PIC $---,---,---,--9.9999     VALUE ZEROES.

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           DISPLAY "Program that adds two numbers."
           DISPLAY "Enter the first  number: "
              WITH NO ADVANCING
            ACCEPT ws-amount1

           DISPLAY "Enter the second number: "
              WITH NO ADVANCING
            ACCEPT ws-amount2

           ADD ws-amount1 TO ws-amount2 GIVING ws-result
            ON SIZE ERROR
               DISPLAY SPACE
               DISPLAY "The calculation is too large to be "
                       "stored in the receiving variable."
           END-ADD

           DISPLAY SPACE
           DISPLAY "Information on added numbers."
           DISPLAY "First Number.  Type COMP."
           DISPLAY "Length: [" LENGTH OF ws-amount1 "]."
           DISPLAY "Value : [" ws-amount1 "]."

           DISPLAY SPACE
           DISPLAY "Second Number. Type COMP-3."
           DISPLAY "Length: [" LENGTH OF ws-amount2 "]."
           DISPLAY "Value : [" ws-amount2 "]."

           DISPLAY SPACE
           DISPLAY "The result of their sum is: ["
                   ws-result "]."

           STOP RUN.

       END PROGRAM AddTwo.
