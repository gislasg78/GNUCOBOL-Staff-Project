       IDENTIFICATION DIVISION.
       PROGRAM-ID.  ScreenOC.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CONSOLE IS CRT.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  ws-key-pause         PIC X(01)       VALUE SPACE.

       78  cte-01                               VALUE 1.
       78  cte-10                               VALUE 10.

       01  ws-lines.
           03  ws-line-add      PIC 9(02)       VALUE ZEROES COMP-3.
           03  ws-line-pos      PIC 9(02)       VALUE 12     COMP.

       01  ws-names-table.
           05  FILLER           PIC 9(02)       VALUE 01.
           05  FILLER           PIC A(15)       VALUE "Betty Lewis".
           05  FILLER           PIC 9(02)       VALUE 02.
           05  FILLER           PIC A(15)       VALUE "Cynthia Turner".
           05  FILLER           PIC 9(02)       VALUE 03.
           05  FILLER           PIC A(15)       VALUE "Debra Watson".
           05  FILLER           PIC 9(02)       VALUE 04.
           05  FILLER           PIC A(15)       VALUE "Dorothy Collins".
           05  FILLER           PIC 9(02)       VALUE 05.
           05  FILLER           PIC A(15)       VALUE "Heather Wood".
           05  FILLER           PIC 9(02)       VALUE 06.
           05  FILLER           PIC A(15)       VALUE "Kimberly James".
           05  FILLER           PIC 9(02)       VALUE 07.
           05  FILLER           PIC A(15)       VALUE "Lauren Marshall".
           05  FILLER           PIC 9(02)       VALUE 08.
           05  FILLER           PIC A(15)       VALUE "Patricia Davis".
           05  FILLER           PIC 9(02)       VALUE 09.
           05  FILLER           PIC A(15)       VALUE "Sophia Bennett".
           05  FILLER           PIC 9(02)       VALUE 10.
           05  FILLER           PIC A(15)       VALUE "Victoria Parker".

       01  ws-names-red-occ     REDEFINES ws-names-table
                                OCCURS  cte-10  TIMES
                                INDEXED BY idx-names-array.
           05  ws-numbers-array PIC 9(02).
           05  ws-names-array   PIC A(15).

       01  ws-variables.
           05  ws-address       PIC X(30)       VALUE SPACES.
           05  ws-code          PIC 9(06)       VALUE ZEROES.
           05  ws-date-num      PIC 9(08)       VALUE ZEROES.
           05  ws-date          REDEFINES ws-date-num.
               10  ws-year      PIC 9(04)       VALUE ZEROES.
               10  ws-month     PIC 9(02)       VALUE ZEROES.
               10  ws-day       PIC 9(02)       VALUE ZEROES.
           05  ws-name          PIC A(20)       VALUE SPACES.
           05  ws-phone         PIC 9(10)       VALUE ZEROES.
           05  ws-salary        PIC 9(06)V9(02) VALUE ZEROES.
           05  ws-status        PIC X(01)       VALUE SPACE.
           05  ws-type-emp      PIC 9(01)       VALUE ZERO.

       SCREEN SECTION.
       01  scr-screen-01.
           05  LINE 01 COLUMN 3  VALUE "Capture of employee data.".
           05  LINE 03.
               10  COLUMN 03     VALUE "Employee code   :".
               10  COLUMN 21     PIC 9(06)               TO ws-code.
           05  LINE 04.
               10  COLUMN 03     VALUE "Admission date  :".
               10  COLUMN 21     PIC 9(08)               TO ws-date-num.
               10  COLUMN 35     PIC 9999/99/99        FROM ws-date-num.
           05  LINE 05.
               10  COLUMN 03     VALUE "Full name       :".
               10  COLUMN 21     PIC A(20)               TO ws-name.
           05  LINE 06.
               10  COLUMN 03     VALUE "Address         :".
               10  COLUMN 21     PIC X(30)               TO ws-address.
           05  LINE 07.
               10  COLUMN 03     VALUE "Phone number    :".
               10  COLUMN 21     PIC 9(10)               TO ws-phone.
               10  COLUMN 35     PIC 9(2)B9(2)B9(3)B9(3) FROM ws-phone.
           05  LINE 08.
               10  COLUMN 03     VALUE "Employee Type   :".
               10  COLUMN 21     PIC 9(01)               TO ws-type-emp.
           05  LINE 09.
               10  COLUMN 03     VALUE "Employee's state:".
               10  COLUMN 21     PIC X(01)               TO ws-status.
           05  LINE 10 COLUMN 3  VALUE "Salary received :".
           05  LINE 10 COLUMN 21 PIC $(04),$(03).$(02)   TO ws-salary.
           05  LINE 10 COLUMN 35 PIC 9(06)V9(02)         FROM ws-salary.

       01  scr-screen-02.
           05  LINE 01 COLUMN 53 PIC A(22)
                                 VALUE "List of employee names".
           05  LINE 02 COLUMN 53 PIC X(22)
                                 VALUE "======================".

       01  scr-screen-03.
           05  LINE 02.
               10  LINE PLUS ws-line-add.
                   15 COLUMN 53 PIC 9(02)
                                FROM ws-numbers-array (idx-names-array).
                   15 COLUMN 56 PIC A(15)
                                FROM ws-names-array   (idx-names-array).

       01  scr-screen-04.
           05  LINE ws-line-pos.
               10  COLUMN 03    VALUE "Press ENTER to continue...".
               10  COLUMN 29    TO ws-key-pause. 

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           DISPLAY SPACE
                AT LINE cte-01 COLUMN cte-01 WITH BLANK SCREEN
           ACCEPT ws-date-num FROM DATE YYYYMMDD

           DISPLAY scr-screen-01
            ACCEPT scr-screen-01
           DISPLAY scr-screen-01

           DISPLAY scr-screen-02

           PERFORM VARYING idx-names-array FROM cte-01  BY cte-01
                     UNTIL idx-names-array IS GREATER THAN cte-10

                     SET ws-line-add TO idx-names-array
                     DISPLAY scr-screen-03
           END-PERFORM

           DISPLAY scr-screen-04
            ACCEPT scr-screen-04

           GOBACK.
       END PROGRAM ScreenOC.
