       IDENTIFICATION DIVISION.
       PROGRAM-ID. CalculateCircle.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01   ws-circle-group-outcomes.
            03  ws-alterante-results.
                05  ws-circumference      FLOAT-LONG       VALUE ZEROES.
                05  ws-diameter           FLOAT-LONG       VALUE ZEROES.
                05  ws-surface            FLOAT-LONG       VALUE ZEROES.
            03  ws-area-group.
                05  ws-area-middle        FLOAT-LONG       VALUE ZEROES.
                05  ws-area-final         FLOAT-LONG       VALUE ZEROES.
            03  ws-coefficients-group.
                05  ws-cte-two            FLOAT-SHORT      VALUE 2.
                05  ws-show-PI            FLOAT-LONG       VALUE ZEROES.
            03  ws-perimeter-group.
                05  ws-perimeter-middle   FLOAT-LONG       VALUE ZEROES.
                05  ws-perimeter-final    FLOAT-LONG       VALUE ZEROES.
            03  ws-radius-group.
                05  ws-radius-numeric     FLOAT-LONG       VALUE ZEROES.
                05  ws-radius-string      PIC X(29)        VALUE SPACES.

       01   ws-dates-group.
            03  ws-date-today-now         PIC 9(08)        VALUE ZEROES.
            03  ws-date-formatted         PIC 9999/99/99   VALUE ZEROES.

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           PERFORM 100000-start-get-string-radius
              THRU 100000-finish-get-string-radius

           PERFORM 200000-start-obtain-circle-calculations
              THRU 200000-finish-obtain-circle-calculations

           PERFORM 300000-start-review-circle-calculations-results
              THRU 300000-finish-review-circle-calculations-results

           PERFORM 400000-start-program-completion
              THRU 400000-finish-program-completion

           STOP RUN.

       100000-start-get-string-radius.
           DISPLAY "Calculation of the area and perimeter of a circle."
           DISPLAY "Enter radius: " WITH NO ADVANCING
            ACCEPT  ws-radius-string.
       100000-finish-get-string-radius.
           EXIT.

       200000-start-obtain-circle-calculations.
           MOVE FUNCTION NUMVAL(ws-radius-string) TO ws-radius-numeric
           MOVE FUNCTION PI()                     TO ws-show-PI

           MULTIPLY ws-cte-two                    BY ws-radius-numeric
             GIVING ws-diameter ROUNDED

           PERFORM 210000-start-find-area-and-perimeter-circle
              THRU 210000-finish-find-area-and-perimeter-circle

           PERFORM 220000-start-find-circumference-and-surface-circle
              THRU 220000-finish-find-circumference-and-surface-circle.
       200000-finish-obtain-circle-calculations.
           EXIT.

        210000-start-find-area-and-perimeter-circle.
           MULTIPLY ws-cte-two                    BY FUNCTION PI()
             GIVING ws-perimeter-middle ROUNDED
           MULTIPLY ws-perimeter-middle           BY ws-radius-numeric
             GIVING ws-perimeter-final  ROUNDED

           MULTIPLY ws-radius-numeric             BY ws-radius-numeric
             GIVING ws-area-middle ROUNDED
           MULTIPLY FUNCTION PI()                 BY ws-area-middle
             GIVING ws-area-final  ROUNDED.
        210000-finish-find-area-and-perimeter-circle.
           EXIT.

        220000-start-find-circumference-and-surface-circle.
           COMPUTE ws-circumference ROUNDED = ws-cte-two
                   * FUNCTION PI() * ws-radius-numeric

           COMPUTE ws-surface ROUNDED = FUNCTION PI()
                   * (ws-radius-numeric ** ws-cte-two).
        220000-finish-find-circumference-and-surface-circle.
           EXIT.

       300000-start-review-circle-calculations-results.
           ACCEPT ws-date-today-now          FROM DATE YYYYMMDD
             MOVE ws-date-today-now            TO ws-date-formatted

           DISPLAY SPACE
           DISPLAY "[" ws-date-formatted "]."
           DISPLAY "Circle information details."

           DISPLAY "+ Coefficient   : [" ws-cte-two "]."
           DISPLAY "+ PI            : [" ws-show-PI "]."
           DISPLAY "+ Radius        : [" ws-radius-numeric "]"
           DISPLAY "+ Diameter      : [" ws-diameter "]."

           DISPLAY SPACE
           DISPLAY "+ Perimeter     : [" ws-perimeter-final "]."
           DISPLAY "+ Area          : [" ws-area-final "]."

           DISPLAY SPACE
           DISPLAY "+ Circumference : [" ws-circumference "]."
           DISPLAY "+ Surface       : [" ws-surface       "].".
       300000-finish-review-circle-calculations-results.
           EXIT.

       400000-start-program-completion.
           DISPLAY SPACE
           DISPLAY "This program has ended."
           DISPLAY "Press ENTER to end this program..."
              WITH NO ADVANCING
            ACCEPT OMITTED.
       400000-finish-program-completion.
           EXIT.
	        
       END PROGRAM CalculateCircle.
