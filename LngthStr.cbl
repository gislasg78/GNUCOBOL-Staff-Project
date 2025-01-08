       IDENTIFICATION DIVISION.
       PROGRAM-ID. LngthStr.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  ws-str                     PIC X(60) VALUE SPACES.

       01  ws-accountants.
           03  ws-chain-length.
               05  ws-fld-len         PIC 9(03) VALUE ZEROES.
               05  ws-str-len         PIC 9(03) VALUE ZEROES.
           03  ws-space-length.
               05  ws-back-space-len  PIC 9(03) VALUE ZEROES.
               05  ws-front-space-len PIC 9(03) VALUE ZEROES.

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           DISPLAY "Correct detection of the string length."
           DISPLAY "Enter a free text string: "
              WITH NO ADVANCING
            ACCEPT ws-str

           PERFORM string-check
           PERFORM display-accounting-results

           STOP RUN.

       string-check SECTION.
           INSPECT ws-str TALLYING ws-back-space-len
                               FOR LEADING SPACE
                                   ws-front-space-len
                               FOR TRAILING SPACE
                                   ws-str-len
                               FOR CHARACTERS.

       display-accounting-results SECTION.
           MOVE LENGTH OF ws-str      TO ws-fld-len

           DISPLAY SPACE
           DISPLAY "Accounting statistics."
           DISPLAY "Lengths:"
           DISPLAY "- Field   : [" ws-fld-len "]."
           DISPLAY "- String  : [" ws-str-len "]."

           DISPLAY SPACE
           DISPLAY "Number of spaces:"
           DISPLAY "- Back    : [" ws-back-space-len  "]."
           DISPLAY "- Front   : [" ws-front-space-len "]."

           DISPLAY SPACE
           DISPLAY "String without preceding or successor spaces:"
           DISPLAY "[" FUNCTION TRIM(ws-str) "].".

       END PROGRAM LngthStr.
