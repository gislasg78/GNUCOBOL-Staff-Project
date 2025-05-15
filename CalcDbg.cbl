       IDENTIFICATION DIVISION.
       PROGRAM-ID. CalcDebug IS INITIAL.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
           SOURCE-COMPUTER. IBM-PC WITH DEBUGGING MODE.

       REPOSITORY.
           FUNCTION ALL INTRINSIC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       78  ws-cte-01                          VALUE 01.

       01  ws-environmental-variables.
           03  ws-my-values.
               05  ws-acum-value FLOAT        VALUE ZEROES.
               05  ws-my-value   FLOAT        VALUE ZEROES.
           03  ws-iter-controls.
               05  ws-entries    BINARY-LONG  VALUE ZEROES.
               05  ws-quantity   BINARY-LONG  VALUE ZEROES.

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           READY TRACE

           DISPLAY "Sum of a series of given values."
           DISPLAY "How many values do you want to enter? : "
              WITH NO ADVANCING
            ACCEPT ws-quantity

           DISPLAY SPACE
      D    EXHIBIT NAMED "Entries    : [" ws-entries "]."
      D    EXHIBIT NAMED "Quantity   : [" ws-quantity "]."
           DISPLAY SPACE

           DISPLAY "Enter the integer values:"

           PERFORM 100-Begin-Capture-Routine
              THRU 100-End-Capture-Routine
           VARYING ws-entries FROM ws-cte-01 BY ws-cte-01
             UNTIL ws-entries IS GREATER THAN ws-quantity

           DISPLAY SPACE
      D    EXHIBIT NAMED "Accumulate : [" ws-acum-value "]."
           DISPLAY SPACE
           DISPLAY "Summatory  : [" ws-acum-value "]."
           DISPLAY SPACE
           EXHIBIT NAMED "[" ws-acum-value "]."

           RESET TRACE

           STOP RUN.

      * Routine that captures each floating point value.
       100-Begin-Capture-Routine.
           DISPLAY "+ #: [" ws-entries "] of: [" ws-quantity "] : "
             WITH NO ADVANCING

           ACCEPT ws-my-value
           ADD ws-my-value TO ws-acum-value.
       100-End-Capture-Routine.
           EXIT.

       END PROGRAM CalcDebug.
