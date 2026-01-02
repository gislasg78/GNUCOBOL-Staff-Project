       IDENTIFICATION DIVISION.
       PROGRAM-ID. AddX.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       78  cte-01                  VALUE 01.
       78  cte-02                  VALUE 02.

       01  ws-var-group.
           03  ws-sum-group.
               05  ws-sum          USAGE UNSIGNED-INT  VALUE ZEROES.
               05  ws-sum-edited   PIC ZZZ,ZZZ,ZZZ,ZZZ VALUE ZEROES.
               05  ws-sum-formula  USAGE UNSIGNED-INT  VALUE ZEROES.
               05  ws-sum-product  USAGE UNSIGNED-INT  VALUE ZEROES.
           03  ws-idx              USAGE INDEX.
           03  ws-max-group.
               05  ws-max          USAGE UNSIGNED-INT  VALUE ZEROES.
               05  ws-max-formula  USAGE UNSIGNED-INT  VALUE ZEROES.
               05  ws-max-product  USAGE UNSIGNED-INT  VALUE ZEROES.

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           PERFORM 100000-start-sequential-iterative-summation
              THRU 100000-finish-sequential-iterative-summation

           PERFORM 200000-start-sequential-summation-formula
              THRU 200000-finish-sequential-summation-formula

           PERFORM 300000-start-display-terminal-results
              THRU 300000-finish-display-terminal-results

           STOP RUN.

       100000-start-sequential-iterative-summation.
           DISPLAY "Sequential Addition."
           DISPLAY "Enter a maximum number to count: "
              WITH NO ADVANCING
            ACCEPT ws-max

           PERFORM VARYING ws-idx FROM cte-01 BY cte-01
              UNTIL ws-idx     IS GREATER THAN ws-max
                    ADD ws-idx TO ws-sum
           END-PERFORM.
       100000-finish-sequential-iterative-summation.
           EXIT.

       200000-start-sequential-summation-formula.
           ADD cte-01 TO ws-max GIVING ws-max-formula
           MULTIPLY ws-max BY ws-max-formula GIVING ws-max-product
           DIVIDE cte-02 INTO ws-max-product GIVING ws-sum-product.
       200000-finish-sequential-summation-formula.
           EXIT.

       300000-start-display-terminal-results.
           DISPLAY SPACE
           DISPLAY "Results."

           MOVE ws-max         TO ws-sum-edited
           DISPLAY "+ Num: [" FUNCTION TRIM(ws-sum-edited) "]."

           MOVE ws-sum         TO ws-sum-edited
           DISPLAY "- Sum: [" FUNCTION TRIM(ws-sum-edited) "]."

           MOVE ws-sum-product TO ws-sum-edited
           DISPLAY "- Sum: [" FUNCTION TRIM(ws-sum-edited) "].".
       300000-finish-display-terminal-results.
           EXIT.

       END PROGRAM AddX.
