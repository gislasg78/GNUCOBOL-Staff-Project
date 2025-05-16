       IDENTIFICATION DIVISION.
       PROGRAM-ID. ViewStr IS INITIAL.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           SYMBOLIC CHARACTERS asterisk IS 43.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  ws-cte-01        CONSTANT AS 01.

       78  ws-cte-10        VALUE 10.
       78  ws-my-name       VALUE 'Gustavo'.

       77  ws-message       PIC A(10) JUST RIGHT VALUE 'Gustavo'.
           88  sw-my-name   VALUE 'Gustavo'.
       77  ws-message-rocc  REDEFINES ws-message OCCURS ws-cte-10 TIMES
                            INDEXED   BY ws-idx-message PIC A(01).
           88  sw-message-rocc-alph   VALUES ARE 'A' THRU 'Z',
                                                 'a' THRU 'z'.

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           DISPLAY "Original String Value."
           DISPLAY "[" ws-message "]."

           DISPLAY SPACE
           DISPLAY "First Name: " WITH NO ADVANCING
           ACCEPT ws-message

           DISPLAY SPACE
           DISPLAY "[" ws-message "]."

           DISPLAY SPACE
           PERFORM VARYING ws-idx-message FROM ws-cte-01 BY ws-cte-01
             UNTIL ws-idx-message IS GREATER THAN ws-cte-10

                   IF sw-message-rocc-alph (ws-idx-message) THEN
                      DISPLAY "[" ws-message-rocc (ws-idx-message) "] "
                         WITH NO ADVANCING
                   ELSE
                      DISPLAY "[" asterisk "] " WITH NO ADVANCING
                   END-IF

           END-PERFORM

           DISPLAY SPACE
           MOVE ws-my-name TO ws-message
           DISPLAY SPACE
           DISPLAY "[" ws-message "]."

           SET sw-my-name  TO TRUE
           DISPLAY SPACE
           DISPLAY "[" ws-message "]."

           DISPLAY SPACE
           DISPLAY "[" REVERSE(TRIM(ws-message)) "]."

           STOP RUN.

       END PROGRAM ViewStr.
