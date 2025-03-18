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
       78  cte-05                               VALUE 5.

       01  ws-lines.
           03  ws-line-add      PIC 9(02)       VALUE ZEROES COMP-3.
           03  ws-line-pos      PIC 9(02)       VALUE 17     COMP.

       01  ws-names-table.
           05  FILLER           PIC 9(01)       VALUE 1.
           05  FILLER           PIC A(15)       VALUE "Dorothy Collins".
           05  FILLER           PIC 9(01)       VALUE 2.
           05  FILLER           PIC A(15)       VALUE "Lauren Marshall".
           05  FILLER           PIC 9(01)       VALUE 3.
           05  FILLER           PIC A(15)       VALUE "Sophia Bennett".
           05  FILLER           PIC 9(01)       VALUE 4.
           05  FILLER           PIC A(15)       VALUE "Patricia Davis".
           05  FILLER           PIC 9(01)       VALUE 5.
           05  FILLER           PIC A(15)       VALUE "Victoria Parker". 
       01  ws-names-red-occ     REDEFINES ws-names-table
                                OCCURS  cte-05  TIMES
                                INDEXED BY idx-names-array.
           05  ws-numbers-array PIC 9(01).
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

       SCREEN SECTION.
       01  scr-screen-01.
           05  LINE 1 COLUMN 3   VALUE "Capture of employee data.".
           05  LINE 3 COLUMN 3   VALUE "Employee code   :".
           05  LINE 3 COLUMN 21  PIC 9(06)               TO ws-code.
           05  LINE 4 COLUMN 3   VALUE "Admission date  :".
           05  LINE 4 COLUMN 21  PIC 9(08)         USING ws-date-num.
           05  LINE 4 COLUMN 35  PIC 9999/99/99     FROM ws-date-num.
           05  LINE 5 COLUMN 3   VALUE "Full name       :".
           05  LINE 5 COLUMN 21  PIC X(20)               TO ws-name.
           05  LINE 6 COLUMN 3   VALUE "Address         :".
           05  LINE 6 COLUMN 21  PIC X(30)               TO ws-address.
           05  LINE 7 COLUMN 3   VALUE "Phone number    :".
           05  LINE 7 COLUMN 21  PIC 9(10)               TO ws-phone.
           05  LINE 7 COLUMN 35  PIC 9(2)B9(2)B9(3)B9(3) FROM ws-phone.
           05  LINE 8 COLUMN 3   VALUE "Salary received :".
           05  LINE 8 COLUMN 21  PIC $(04),$(03).$(02)   TO ws-salary.
           05  LINE 8 COLUMN 35  PIC 9(06)V9(02)         FROM ws-salary.

       01  scr-screen-02.
           05  LINE 10 COLUMN 3  PIC A(22)
                                 VALUE "List of employee names".
           05  LINE 11 COLUMN 3  PIC X(22)
                                 VALUE "======================".

       01  scr-screen-03.
           05  LINE 11.
               10  LINE PLUS ws-line-add.
                   15 COLUMN 3  PIC 9(02)
                                FROM ws-numbers-array (idx-names-array).
                   15 COLUMN 6  PIC A(15)
                                FROM ws-names-array   (idx-names-array).

       01  scr-screen-04.
           05  LINE ws-line-pos COLUMN 3
               VALUE "Press ENTER to continue...".
           05  LINE ws-line-pos COLUMN 29 TO ws-key-pause. 

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           DISPLAY SPACE AT LINE 01 COLUMN 01 WITH BLANK SCREEN
           ACCEPT ws-date-num FROM DATE YYYYMMDD

           DISPLAY scr-screen-01
            ACCEPT scr-screen-01
           DISPLAY scr-screen-01

           DISPLAY scr-screen-02

           PERFORM VARYING idx-names-array FROM cte-01  BY cte-01
                     UNTIL idx-names-array IS GREATER THAN cte-05

                     SET ws-line-add TO idx-names-array
                     DISPLAY scr-screen-03
           END-PERFORM

           DISPLAY scr-screen-04
            ACCEPT scr-screen-04

           GOBACK.
       END PROGRAM ScreenOC.
