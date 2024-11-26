       IDENTIFICATION DIVISION.
       PROGRAM-ID.  Business.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  ws-business-vars.
           03  ws-work-constants.
               05  ws-cte-01               PIC 9(01)      VALUE 01.
               05  ws-bs-staff-max-lim     PIC 9(04)      VALUE 9999.
               05  ws-bs-staff-cap-cnt     PIC S9(10)     VALUE ZEROES.
               05  ws-tbl-emp-cat-max-lim  PIC 9(03)      VALUE 999.
               05  ws-tbl-emp-cat-cap-cnt  PIC S9(10)     VALUE ZEROES.
               
           03  ws-auxiliary-variables.
               05  ws-capture-response     PIC A(01)      VALUE SPACE.
                   88 sw-capt-resp-N                      VALUES ARE
                                                          "N", "n".
               05  ws-edited-fields.
                   10  ws-ed-fld-tbl-emp-cat-fmt
                                           PIC $-,---,---,--9.9999
                                                          VALUE ZEROES.
                   10  ws-ed-fld-tbl-emp-cat-edit
                                           PIC $-,---,--9.99
                                                          VALUE ZEROES.
       
       01  ws-bs-staff               OCCURS  0001 TO 9999 TIMES
                                     DEPENDING ON ws-bs-staff-cap-cnt
                                     INDEXED   BY idx-bs-staff.
           03  ws-bs-employee-cod-cat      PIC 9(04)      VALUE ZEROES.
           03  ws-bs-employee-salary       PIC S9(6)V9(2) VALUE ZEROES.

       01  ws-tbl-emp-category-st    OCCURS  001  TO 999  TIMES
                                     DEPENDING ON ws-tbl-emp-cat-cap-cnt
                                     INDEXED   BY idx-tbl-emp-category.
           05  ws-tbl-emp-cat-cod          PIC 9(04)      VALUE ZEROES.
           05  ws-tbl-emp-cat-cnt          PIC S9(6)      VALUE ZEROES.
           05  ws-tbl-emp-cat-max          PIC S9(6)V9(2) VALUE ZEROES.
           05  ws-tbl-emp-cat-min          PIC S9(6)V9(2) VALUE ZEROES.
           05  ws-tbl-emp-cat-rng          PIC S9(6)V9(2) VALUE ZEROES.
           05  ws-tbl-emp-cat-addt         PIC S9(9)V9(4) VALUE ZEROES.
           05  ws-tbl-emp-cat-avg          PIC S9(6)V9(4) VALUE ZEROES.

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           PERFORM 100000-begin-enter-dat-emp
              THRU 100000-end-enter-dat-emp
           VARYING idx-bs-staff
              FROM ws-cte-01            BY ws-cte-01
             UNTIL idx-bs-staff         GREATER ws-bs-staff-max-lim
                OR sw-capt-resp-N

           PERFORM 200000-begin-calc-stat-emp
              THRU 200000-end-calc-stat-emp
           VARYING idx-bs-staff
              FROM ws-cte-01            BY ws-cte-01
             UNTIL idx-bs-staff IS GREATER THAN ws-bs-staff-cap-cnt
                OR idx-bs-staff IS GREATER THAN ws-bs-staff-max-lim
             AFTER idx-tbl-emp-category
              FROM ws-cte-01            BY ws-cte-01
             UNTIL idx-tbl-emp-category GREATER ws-tbl-emp-cat-max-lim

           PERFORM 300000-begin-see-stat-emp
              THRU 300000-end-see-stat-emp
           VARYING idx-tbl-emp-category
              FROM ws-cte-01            BY ws-cte-01
             UNTIL idx-tbl-emp-category GREATER ws-tbl-emp-cat-cap-cnt
                OR idx-tbl-emp-category GREATER ws-tbl-emp-cat-max-lim

           STOP RUN.

       100000-begin-enter-dat-emp.
       	   ADD ws-cte-01                TO ws-bs-staff-cap-cnt

           DISPLAY SPACE
           DISPLAY '+===+===+===+===+===+===+===+===+===+===+====+'
           DISPLAY '| Employee Capture Process: # ', idx-bs-staff
           DISPLAY '+===+===+===+===+===+===+===+===+===+===+====+'
           DISPLAY '| Employee job category code : '  WITH NO ADVANCING
           ACCEPT ws-bs-employee-cod-cat (idx-bs-staff)
           DISPLAY '| Employee''s salary amount   : ' WITH NO ADVANCING
           ACCEPT ws-bs-employee-salary  (idx-bs-staff)
           DISPLAY '+===+===+===+===+===+===+===+===+===+===+====+'
           
           DISPLAY ' Continue capturing more data records? (Y/N) : '
                   WITH NO ADVANCING
           ACCEPT ws-capture-response.
       100000-end-enter-dat-emp.
           EXIT.

       200000-begin-calc-stat-emp.
           IF ws-tbl-emp-cat-cod          (idx-tbl-emp-category) EQUAL
              ZEROES
                 PERFORM 210000-begin-assgn-fr-vls
                    THRU 210000-end-assgn-fr-vls
           ELSE 
              IF ws-tbl-emp-cat-cod       (idx-tbl-emp-category) EQUAL
                 ws-bs-employee-cod-cat   (idx-bs-staff)
                  PERFORM 220000-begin-accum-vls
                     THRU 220000-end-accum-vls.
       200000-end-calc-stat-emp.
           EXIT.

        210000-begin-assgn-fr-vls.
           ADD ws-cte-01                TO ws-tbl-emp-cat-cap-cnt
              
           MOVE ws-bs-employee-cod-cat    (idx-bs-staff)
             TO ws-tbl-emp-cat-cod        (idx-tbl-emp-category)
           MOVE ws-cte-01
             TO ws-tbl-emp-cat-cnt        (idx-tbl-emp-category)
                
           MOVE ws-bs-employee-salary     (idx-bs-staff)
             TO ws-tbl-emp-cat-addt       (idx-tbl-emp-category)
                ws-tbl-emp-cat-max        (idx-tbl-emp-category)
                ws-tbl-emp-cat-min        (idx-tbl-emp-category)
                ws-tbl-emp-cat-avg        (idx-tbl-emp-category)
              
           SET idx-tbl-emp-category     TO ws-tbl-emp-cat-max-lim.
        210000-end-assgn-fr-vls.
           EXIT.
           
        220000-begin-accum-vls.
           ADD ws-cte-01
            TO ws-tbl-emp-cat-cnt         (idx-tbl-emp-category)
           ADD ws-bs-employee-salary      (idx-bs-staff)
            TO ws-tbl-emp-cat-addt        (idx-tbl-emp-category)
               ws-tbl-emp-cat-avg         (idx-tbl-emp-category)
            
           IF ws-tbl-emp-cat-max          (idx-tbl-emp-category) LESS
              ws-bs-employee-salary       (idx-bs-staff)
               MOVE ws-bs-employee-salary (idx-bs-staff)
                 TO ws-tbl-emp-cat-max    (idx-tbl-emp-category).
                
           IF ws-tbl-emp-cat-min          (idx-tbl-emp-category) GREATER
              ws-bs-employee-salary       (idx-bs-staff)
               MOVE ws-bs-employee-salary (idx-bs-staff)
                 TO ws-tbl-emp-cat-min    (idx-tbl-emp-category).
            
           SET idx-tbl-emp-category     TO ws-tbl-emp-cat-max-lim.
        220000-end-accum-vls.
           EXIT.

       300000-begin-see-stat-emp.
           SUBTRACT ws-tbl-emp-cat-min    (idx-tbl-emp-category)
               FROM ws-tbl-emp-cat-max    (idx-tbl-emp-category)
             GIVING ws-tbl-emp-cat-rng    (idx-tbl-emp-category)
       
           DIVIDE   ws-tbl-emp-cat-cnt    (idx-tbl-emp-category)
             INTO   ws-tbl-emp-cat-avg    (idx-tbl-emp-category)

           DISPLAY SPACE
           DISPLAY '+---+----+---+----+---+----+---+----+---+----+'
           DISPLAY '| Employee Category Process Statistics: '
           DISPLAY '|     [',
                    idx-tbl-emp-category, '] of [',
                    ws-tbl-emp-cat-cap-cnt, ']'
           DISPLAY '+---+----+---+----+---+----+---+----+---+----+'
           DISPLAY '| Code    :  [',
                    ws-tbl-emp-cat-cod    (idx-tbl-emp-category)
                   '].'
           DISPLAY '| Count   :  [',
                    ws-tbl-emp-cat-cnt    (idx-tbl-emp-category)
                   '].'

              MOVE ws-tbl-emp-cat-addt    (idx-tbl-emp-category)
                TO ws-ed-fld-tbl-emp-cat-fmt
           DISPLAY '| Sum     :  [',
                   ws-ed-fld-tbl-emp-cat-fmt
                   '].'

              MOVE  ws-tbl-emp-cat-max    (idx-tbl-emp-category)
                TO  ws-ed-fld-tbl-emp-cat-edit
           DISPLAY '| Maximum :      [',
                    ws-ed-fld-tbl-emp-cat-edit
                   '].'

              MOVE  ws-tbl-emp-cat-min    (idx-tbl-emp-category)
                TO  ws-ed-fld-tbl-emp-cat-edit
           DISPLAY '| Minimum :      [',
                    ws-ed-fld-tbl-emp-cat-edit
                   '].'

              MOVE  ws-tbl-emp-cat-rng    (idx-tbl-emp-category)
                TO  ws-ed-fld-tbl-emp-cat-edit
           DISPLAY '| Range   :      [',
                    ws-ed-fld-tbl-emp-cat-edit
                   '].'

              MOVE  ws-tbl-emp-cat-avg    (idx-tbl-emp-category)
                TO  ws-ed-fld-tbl-emp-cat-edit
           DISPLAY '| Average :      [',
                    ws-ed-fld-tbl-emp-cat-edit
                   '].'
           DISPLAY '+---+----+---+----+---+----+---+----+---+----+'

           DISPLAY "Press the ENTER key to continue..."
              WITH NO ADVANCING
           ACCEPT OMITTED.
       300000-end-see-stat-emp.
           EXIT.

       END PROGRAM Business.
