       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBLPtrs.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  ws-group-vars.
           03  ws-var-1           PIC 9(04) VALUE 1956.
           03  ws-var-2           PIC 9(04) VALUE 1978.
           03  ws-var-3           PIC 9(04) VALUE 1985.

       01  ws-group-pointers.
           03  ws-pointer-1       POINTER.
           03  ws-pointer-2       POINTER.
           03  ws-pointer-3       POINTER.

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
      * Assign memory addresses to pointers.
           SET ws-pointer-1    TO ADDRESS OF ws-var-1
           SET ws-pointer-2    TO ADDRESS OF ws-var-2
           SET ws-pointer-3    TO ADDRESS OF ws-var-3

      * Display the value of variables using pointers.
           DISPLAY "Variable's values."
           DISPLAY " - Value #1: " ws-var-1
           DISPLAY " - Value #2: " ws-var-2
           DISPLAY " - Value #3: " ws-var-3

           DISPLAY SPACE
           DISPLAY "Variable # : 1"
           DISPLAY " - Address : " ws-pointer-1
           DISPLAY " - Storage : " FUNCTION CONTENT-OF(ws-pointer-1)

           DISPLAY SPACE
           DISPLAY "Variable # : 2"
           DISPLAY " - Address : " ws-pointer-2
           DISPLAY " - Storage : " FUNCTION CONTENT-OF(ws-pointer-2)

           DISPLAY SPACE
           DISPLAY "Variable # : 3"
           DISPLAY " - Address : " ws-pointer-3
           DISPLAY " - Storage : " FUNCTION CONTENT-OF(ws-pointer-3)

           STOP RUN.

       END PROGRAM CBLPtrs.
