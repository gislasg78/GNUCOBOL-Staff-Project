       IDENTIFICATION DIVISION.
       PROGRAM-ID. DemoRWCS.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
           REPOSITORY. FUNCTION ALL INTRINSIC.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT cpu-file    ASSIGN TO WS-File-CPUData
                              LINE SEQUENTIAL.

           SELECT report-file ASSIGN TO DISPLAY
                              LINE SEQUENTIAL.

           SELECT sort-file   ASSIGN TO DISK.

       DATA DIVISION.
       FILE SECTION.
       FD  cpu-file.
       01  cpu-rec                 PIC X(26).

       FD  report-file
           REPORT IS CPU-Report.

       SD  sort-file.
       01  sort-rec.
           03  F-SR-Score-NUM      PIC 9(05).
           03  F-SR-Vendor-TXT     PIC X(05).
           03  F-SR-Family-TXT     PIC X(07).
           03  F-SR-Model-TXT      PIC X(06).

       WORKING-STORAGE SECTION.
       77  WS-File-CPUData         PIC X(13) VALUE SPACES.

       01  WS-Date                 PIC 9(08).

       01  WS-Family-Counters.
           03  WS-FC-AVE           PIC 9(05)V99.
           03  WS-FC-Qty           BINARY-LONG.
           03  WS-FC-Total-NUM     BINARY-LONG.

       01  WS-Flags.
           03  WS-F-EOF            PIC X(01).

       01  WS-One-Const            PIC 9(01) VALUE 1.

       01  WS-Overall-Counters.
           03  WS-OC-AVE           PIC 9(05)V99.
           03  WS-OC-Qty           BINARY-LONG.
           03  WS-OC-Total-Num     BINARY-LONG.

       01  WS-Underline-lines.
           03  WS-Comma            PIC X(01) VALUE X'2C'.
           03  WS-Equal-Signs      PIC X(06) VALUE ALL X'3D'.
           03  WS-Equality-Signs   PIC X(29) VALUE ALL X'3D'.
           03  WS-Starz            PIC X(44) VALUE ALL X'2A'.
           03  WS-Y                PIC A(01) VALUE X'59'.

       01  WS-Vendor-Counters.
           03  WS-VC-AVE           PIC 9(05)V99.
           03  WS-VC-Qty           BINARY-LONG.
           03  WS-VC-Total-NUM     BINARY-LONG.

       REPORT SECTION.
       RD  CPU-Report
           CONTROLS ARE FINAL
                        F-SR-Vendor-TXT
                        F-SR-Family-TXT

           PAGE LIMIT IS 25 LINES
                HEADING 1
                FIRST DETAIL 6
                LAST DETAIL 25.

       01  TYPE IS PAGE HEADING.
           03  LINE NUMBER PLUS 1.
               05  COL 1  SOURCE WS-Starz            PIC X(44).
           03  LINE NUMBER PLUS 1.
               05  COL 1  SOURCE WS-Date             PIC 9999/99/99.
               05  COL 14 VALUE 'CPU Benchmark Scores'.
               05  COL 37 VALUE 'Page:'.
               05  COL 43 SOURCE PAGE-COUNTER        PIC Z9.
           03  LINE NUMBER PLUS 1.
               05  COL 1  SOURCE WS-Starz            PIC X(44).
           03  LINE NUMBER PLUS 1.
               05  COL 1  VALUE '**'.
               05  COL 6  VALUE 'All CPU Data From cpubenchmark.net'.
               05  COL 43 VALUE '**'.
           03  LINE NUMBER PLUS 1.
               05  COL 1  SOURCE WS-Starz            PIC X(44).

       01  TYPE CONTROL HEADING F-SR-Family-TXT.
           03  LINE NUMBER PLUS 2.
               05  COL 1  SOURCE F-SR-Vendor-TXT     PIC X(06).
               05  COL 8  SOURCE F-SR-Family-TXT     PIC X(07).
           03  LINE NUMBER PLUS 1.
               05  COL 1  VALUE 'Family'.
               05  COL 8  VALUE 'Model'.
               05  COL 15 VALUE 'Benchmark Score (High to Low)'.
           03  LINE NUMBER PLUS 1.
               05  COL 1  SOURCE WS-Equal-Signs      PIC X(06).
               05  COL 8  SOURCE WS-Equal-Signs      PIC X(06).
               05  COL 15 SOURCE WS-Equality-Signs   PIC X(29).

       01  Detail-Line TYPE IS DETAIL.
           03  LINE NUMBER PLUS 1.
               05  COL 1                SOURCE F-SR-Family-TXT
                          PIC X(07)     GROUP INDICATE.
               05  COL 8  PIC X(06)     SOURCE F-SR-Model-TXT.
               05  COL 14 PIC ZZ,ZZ9    SOURCE F-SR-Score-NUM.

       01  End-Family   TYPE IS CONTROL FOOTING F-SR-Family-TXT.
           03  LINE NUMBER PLUS 1.
               05  COL 8                VALUE 'Ave...'.
               05  COL 14 PIC ZZ,ZZ9.99 SOURCE WS-FC-AVE.
               05  COL 25               VALUE '('.
               05  COL 26 PIC ZZ9       SUM    WS-One-Const.
               05  COL 30               VALUE 'Family CPUs)'.

       01  End-Vendor   TYPE IS CONTROL FOOTING F-SR-Vendor-TXT.
           03  LINE NUMBER PLUS 1.
               05  COL 8                VALUE 'Ave...'.
               05  COL 14 PIC ZZ,ZZ9.99 SOURCE WS-VC-AVE.
               05  COL 25               VALUE '('.
               05  COL 26 PIC ZZ9       SUM    WS-One-Const.
               05  COL 30               VALUE 'Vendor CPUs)'.

       01  End-Overall  TYPE IS CONTROL FOOTING FINAL.
           03  LINE NUMBER PLUS 1.
               05  COL 8                VALUE 'Ave...'.
               05  COL 14 PIC ZZ,ZZ9.99 SOURCE WS-OC-AVE.
               05  COL 25               VALUE '('.
               05  COL 26 PIC ZZ9       SUM    Ws-One-Const.
               05  COL 30               VALUE 'CPUs)'.

       PROCEDURE DIVISION.
       DECLARATIVES.
       000-End-Family SECTION.
           USE BEFORE REPORTING End-Family.
       1.  IF WS-FC-Qty > ZERO
              COMPUTE WS-FC-AVE = WS-FC-Total-NUM / WS-FC-Qty
           ELSE
              MOVE ZERO TO WS-FC-AVE
           END-IF
           MOVE ZERO    TO WS-FC-Qty
                           WS-FC-Total-NUM.

       000-End-Vendor SECTION.
           USE BEFORE REPORTING End-Vendor.
       1.  IF WS-VC-Qty > ZERO
              COMPUTE WS-VC-AVE = WS-VC-Total-NUM / WS-VC-Qty
           ELSE
              MOVE ZERO TO WS-VC-AVE
           END-IF
           MOVE ZERO    TO WS-VC-Qty
                           WS-VC-Total-NUM.

       000-End-Overall SECTION.
           USE BEFORE REPORTING End-Overall.
       1.  IF WS-OC-Qty > ZERO
              COMPUTE WS-OC-AVE = WS-OC-Total-NUM / WS-OC-Qty
           ELSE
              MOVE ZERO TO WS-OC-AVE
           END-IF
           MOVE ZERO    TO WS-OC-Qty
                           WS-OC-Total-NUM.

       END DECLARATIVES.

       010-Main SECTION.
       1.  ACCEPT WS-Date FROM DATE YYYYMMDD

           DISPLAY "CPU Benchmark Scores Report Generator."
           DISPLAY "Enter the name of the input file: "
              WITH NO ADVANCING
            ACCEPT WS-File-CPUData

           SORT sort-file
                ASCENDING  KEY   F-SR-Vendor-TXT
                                 F-SR-Family-TXT
                DESCENDING KEY   F-SR-Score-NUM
                ASCENDING  KEY   F-SR-Model-TXT
                INPUT PROCEDURE  100-Pre-Process-Data
                OUTPUT PROCEDURE 200-Generate-Report

           STOP RUN.

       100-Pre-Process-Data SECTION.
       1.  OPEN INPUT cpu-file
           PERFORM FOREVER
                   READ cpu-file
                     AT END
                        EXIT PERFORM
                   END-READ

                   MOVE SPACES TO sort-rec
                   UNSTRING cpu-rec DELIMITED BY WS-Comma
                       INTO F-SR-Score-NUM
                            F-SR-Vendor-TXT
                            F-SR-Family-TXT
                            F-SR-Model-TXT
                   END-UNSTRING

                   RELEASE sort-rec
           END-PERFORM
           CLOSE cpu-file.

       200-Generate-Report SECTION.
       1.  INITIALIZE WS-Family-Counters
                      WS-Flags

           OPEN OUTPUT report-file

           INITIATE cpu-report

           RETURN sort-file
               AT END
                  MOVE WS-Y    TO WS-F-EOF
           END-RETURN

           PERFORM UNTIL WS-F-EOF = WS-Y
                   GENERATE Detail-Line
                   ADD WS-One-Const   TO WS-FC-Qty
                                         WS-OC-Qty
                                         WS-VC-Qty

                   ADD F-SR-Score-NUM TO WS-FC-Total-NUM
                                         WS-OC-Total-NUM
                                         WS-VC-Total-NUM 

                   RETURN sort-file
                       AT END
                          MOVE WS-Y   TO WS-F-EOF
                   END-RETURN
           END-PERFORM

           TERMINATE CPU-Report

           CLOSE report-file.

       END PROGRAM DemoRWCS.
