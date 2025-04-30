       IDENTIFICATION DIVISION.
       PROGRAM-ID. AddX.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       78  cte-01                  VALUE 01.

       01  ws-var-group.
           03  ws-sum-group.
               05  ws-sum          PIC 9(09)       VALUE ZEROES.
               05  ws-sum-edited   PIC ZZZ,ZZZ,ZZZ VALUE ZEROES.
           03  ws-idx              USAGE INDEX.
           03  ws-max              PIC 9(09)       VALUE ZEROES.

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           DISPLAY "Sequential Addition."
           DISPLAY "Enter a maximum number to count: "
              WITH NO ADVANCING
            ACCEPT ws-max

           PERFORM VARYING ws-idx FROM cte-01 BY cte-01
              UNTIL ws-idx     IS GREATER THAN ws-max
                    ADD ws-idx TO ws-sum
           END-PERFORM

           MOVE ws-sum         TO ws-sum-edited

           DISPLAY "Sum: [" ws-sum-edited "]."

           STOP RUN.

       END PROGRAM AddX.
