       IDENTIFICATION DIVISION.
       PROGRAM-ID. bmi.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  ws-working-variables.
           03  ws-BMI                 USAGE FLOAT     VALUE ZERO.
           03  ws-height-group.
               05  ws-height          PIC 9(03)V9(04) VALUE ZEROES.
               05  ws-height-squared  USAGE FLOAT     VALUE ZERO.
           03  ws-weight              PIC 9(03)V9(04) VALUE ZEROES.

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           PERFORM 100000-start-request-preliminary-input-data
              THRU 100000-finish-request-preliminary-input-data

           PERFORM 200000-start-process-BMI-calculations
              THRU 200000-finish-process-BMI-calculations

           PERFORM 300000-start-show-final-calculations-results
              THRU 300000-finish-show-final-calculations-results

           STOP RUN.

       100000-start-request-preliminary-input-data.
           DISPLAY "(BMI) Body Mass Index."
           DISPLAY "Enter your height (m): "  WITH NO ADVANCING
            ACCEPT ws-height
           DISPLAY "Enter your weight (kg): " WITH NO ADVANCING
            ACCEPT ws-weight

           PERFORM 110000-start-request-a-pause-at-the-entrance
              THRU 110000-finish-request-a-pause-at-the-entrance.
       100000-finish-request-preliminary-input-data.
           EXIT.

        110000-start-request-a-pause-at-the-entrance.
           DISPLAY "Press the ENTER key to continue..."
              WITH NO ADVANCING
            ACCEPT OMITTED.
        110000-finish-request-a-pause-at-the-entrance.
           EXIT.

       200000-start-process-BMI-calculations.
           MULTIPLY ws-height         BY ws-height
             GIVING ws-height-squared
           END-MULTIPLY

           DISPLAY SPACE
           DIVIDE ws-height-squared INTO ws-weight
           GIVING ws-BMI
               ON SIZE ERROR
                  DISPLAY "Fatal error!" X'20'
                          "Division by zero not allowed."
           NOT ON SIZE ERROR
                  DISPLAY "Correct!"     X'20'
                          "Calculations successfully performed."
           END-DIVIDE.
       200000-finish-process-BMI-calculations.
           EXIT.

       300000-start-show-final-calculations-results.
           DISPLAY SPACE
           DISPLAY "(BMI) BMI Information."
           DISPLAY X"2B" X'20' "BMI"    X"3A" X'09' X"09" X'7B'
                   ws-BMI      X'7D'    X"2E"
           DISPLAY X"2D" X'20' "Height" X"3A" X'09' X"5B"
                   ws-height   X"5D"    X'20' "meters"    X"2E"
           DISPLAY X"2D" X'20' "Weight" X"3A" X'09' X"5B"
                   ws-weight   X"5D"    X"20" "kilograms" X"2E"

           PERFORM 110000-start-request-a-pause-at-the-entrance
              THRU 110000-finish-request-a-pause-at-the-entrance

           PERFORM 310000-start-program-conclusion-pause
              THRU 310000-finish-program-conclusion-pause.
       300000-finish-show-final-calculations-results.
           EXIT.

        310000-start-program-conclusion-pause.
           DISPLAY SPACE
           DISPLAY "Done!"
           DISPLAY "This program has ended."

           PERFORM 110000-start-request-a-pause-at-the-entrance
              THRU 110000-finish-request-a-pause-at-the-entrance.
        310000-finish-program-conclusion-pause.
           EXIT.

       END-PROGRAM. bmi.
