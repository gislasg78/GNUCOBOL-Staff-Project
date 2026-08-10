       IDENTIFICATION DIVISION.
       PROGRAM-ID. bmi.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       78  cte-overweight                                 VALUE 25.
       78  cte-underweight                                VALUE 18.5.

       01  ws-working-variables.
           03  ws-calculations.
               05  ws-BMI                 USAGE FLOAT     VALUE ZERO.
               05  ws-height-group.
                   07  ws-height          PIC 9(03)V9(04) VALUE ZEROES.
                   07  ws-height-squared  USAGE FLOAT     VALUE ZERO.
               05  ws-weight              PIC 9(03)V9(04) VALUE ZEROES.
           03  ws-health-assessment-tags.
               05  ws-health-legend       PIC A(11)       VALUE SPACES.
               05  ws-health-evaluations.
                   07  ws-health-ev-fail  PIC A(04) VALUE "Fail".
                   07  ws-health-ev-norm  PIC A(06) VALUE "Normal".
                   07  ws-health-ev-over  PIC A(10) VALUE "Overweight".
                   07  ws-health-ev-under PIC A(11) VALUE "Underweight".

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
           DISPLAY "(BMI) Body Mass Index Program."
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
           MULTIPLY ws-height                BY ws-height
             GIVING ws-height-squared
           END-MULTIPLY

           DISPLAY SPACE
           DIVIDE ws-height-squared        INTO ws-weight
           GIVING ws-BMI
               ON SIZE ERROR
                  DISPLAY "Fatal error!" X'20'
                          "Division by zero not allowed."

           NOT ON SIZE ERROR
                  DISPLAY "Correct!"     X'20'
                          "Calculations successfully performed."
                  PERFORM 210000-start-health-assessment
                     THRU 210000-finish-health-assessment

           END-DIVIDE.
       200000-finish-process-BMI-calculations.
           EXIT.

        210000-start-health-assessment.
           EVALUATE ws-BMI
               WHEN IS GREATER THAN OR EQUAL TO cte-underweight
                AND IS LESS    THAN OR EQUAL TO cte-overweight
                    MOVE ws-health-ev-norm   TO ws-health-legend

               WHEN IS GREATER THAN cte-overweight
                    MOVE ws-health-ev-over   TO ws-health-legend

               WHEN IS LESS    THAN cte-underweight
                    MOVE ws-health-ev-under  TO ws-health-legend

               WHEN OTHER
                    MOVE ws-health-ev-fail   TO ws-health-legend

           END-EVALUATE.
        210000-finish-health-assessment.
           EXIT.

       300000-start-show-final-calculations-results.
           DISPLAY SPACE
           DISPLAY "(BMI) BMI Information Calculator."
           DISPLAY X"2B" X"20" "BMI"    X"3A" X"09" X"09" X"7B"
                   ws-BMI      X"7D"    X"2E"
           DISPLAY X"2D" X"20" "Height" X"3A" X"09" X"5B"
                   ws-height   X"5D"    X'20' "meters"    X"2E"
           DISPLAY X"2D" X"20" "Weight" X"3A" X"09" X"5B"
                   ws-weight   X"5D"    X"20" "kilograms" X"2E"
           DISPLAY X"2A" X"20" "Health" X"3A" X"09" X"5B"
                   FUNCTION TRIM(ws-health-legend)  X"5D" X"2E"

           PERFORM 110000-start-request-a-pause-at-the-entrance
              THRU 110000-finish-request-a-pause-at-the-entrance

           PERFORM 310000-start-program-conclusion-pause
              THRU 310000-finish-program-conclusion-pause.
       300000-finish-show-final-calculations-results.
           EXIT.

        310000-start-program-conclusion-pause.
           DISPLAY SPACE
           DISPLAY "Done" X'21'
           DISPLAY "This program has ended" X'2E'

           PERFORM 110000-start-request-a-pause-at-the-entrance
              THRU 110000-finish-request-a-pause-at-the-entrance.
        310000-finish-program-conclusion-pause.
           EXIT.

       END-PROGRAM. bmi.
