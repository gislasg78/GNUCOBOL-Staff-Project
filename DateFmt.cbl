       IDENTIFICATION DIVISION.
       PROGRAM-ID. DateFmt.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  ws-my-date-num     PIC 9(08)      VALUE ZEROES.
       77  ws-my-date-str     REDEFINES ws-my-date-num
                              PIC X(08)      VALUE SPACES.

       77  ws-my-date-num-edt PIC 9999B99B99 VALUE ZEROES.
       77  ws-my-date-num-fmt REDEFINES ws-my-date-num-edt
                              PIC 9999/99/99 VALUE ZEROES.

       77  ws-my-date-str-edt PIC XXXXBXXBXX VALUE SPACES.
       77  ws-my-date-str-fmt REDEFINES ws-my-date-str-edt
                              PIC XXXX/XX/XX VALUE SPACES.
       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           DISPLAY "Example Date Formatting Program."
           ACCEPT ws-my-date-num FROM DATE YYYYMMDD

           DISPLAY SPACE
           DISPLAY "Numeric fields to edited numeric fields."
           MOVE ws-my-date-num     TO ws-my-date-num-edt
           DISPLAY "Current Date: ["  ws-my-date-num-edt "]."
           MOVE ws-my-date-num     TO ws-my-date-num-fmt
           DISPLAY "Today's Date: ["  ws-my-date-num-fmt "]."

           DISPLAY SPACE
           DISPLAY "Alphanumeric fields to edited alphanumeric fields."
           MOVE ws-my-date-str     TO ws-my-date-str-edt
           DISPLAY "Current Date: ["  ws-my-date-str-edt "]."
           MOVE ws-my-date-str     TO ws-my-date-str-fmt
           DISPLAY "Today's Date: ["  ws-my-date-str-fmt "]."

           STOP RUN.

       END PROGRAM DateFmt.
