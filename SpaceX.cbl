       IDENTIFICATION DIVISION.
       PROGRAM-ID. SpaceX.

       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       77  ls-counter   PIC 9(02) VALUE ZEROES.

       78  cte-10                 VALUE 10.
       78  cte-01                 VALUE 01.
       78  cte-minus-01           VALUE -1.

       PROCEDURE DIVISION.
       MAIN SECTION.
           ALTER spacex TO PROCEED TO launchpad
           GO TO spacex.

       spacex.
           GO TO despair
           DISPLAY "Where is my Tesla?".

       despair.
           DISPLAY "Lost in space.".

       launchpad.
           DISPLAY "SpaceX is on the launchpad..."

           PERFORM VARYING ls-counter FROM cte-10 BY cte-minus-01
                     UNTIL ls-counter IS LESS THAN cte-01
                   DISPLAY "[" ls-counter "] " WITH NO ADVANCING
           END-PERFORM

           DISPLAY SPACE

           ALTER spacex TO PROCEED TO orbit
           GO TO spacex.

       orbit.
           DISPLAY "Reached space and in orbit."
           ALTER spacex TO PROCEED TO transit
           GO TO spacex.

       transit.
           DISPLAY "Accelerating out of orbit to transit to Mars."
           ALTER spacex TO mars
           GO TO spacex.

       mars.
           DISPLAY "Arrived at Mars... SpaceX rocks."
           STOP RUN.

       END PROGRAM SpaceX.
