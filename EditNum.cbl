       IDENTIFICATION DIVISION.
       PROGRAM-ID. EditNum.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  cte-01              CONSTANT AS 01.
       
       01  ws-numeric-data     PIC S9(4) COMP VALUE ZEROES.
       01  ws-numeric-data-rdf REDEFINES ws-numeric-data.
           03  ws-numeric-data-higher         PIC X(01).
           03  ws-numeric-data-lower          PIC X(01).

       01  ws-numeric-data-fmt PIC ++,++9     VALUE ZEROES. 

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           DISPLAY "System for coding numerical values."
           PERFORM GetValue
           PERFORM ShowValue

           PERFORM ChangeValue
           PERFORM ShowValue

           PERFORM AddValue
           PERFORM ShowValue
           
           GOBACK.

       GetValue SECTION.
           DISPLAY "Enter any numeric value: " WITH NO ADVANCING
           ACCEPT ws-numeric-data.

       ShowValue SECTION.
           DISPLAY SPACE
           DISPLAY "Numeric value  : [" ws-numeric-data  "]."
           DISPLAY "String  value  : [" ws-numeric-data-rdf "]."
           DISPLAY "- High part    : [" ws-numeric-data-higher "]."
           DISPLAY "- Low  part    : [" ws-numeric-data-lower  "]."

           MOVE ws-numeric-data TO ws-numeric-data-fmt

           DISPLAY "Formatted Data : [" ws-numeric-data-fmt "]." 
           DISPLAY "Press ENTER to continue..." WITH NO ADVANCING
           ACCEPT OMITTED.

       ChangeValue SECTION.
           DISPLAY SPACE
           DISPLAY "Alteration of his alphanumeric parts."
           DISPLAY "Enter a first  character : " WITH NO ADVANCING
           ACCEPT ws-numeric-data-higher
           DISPLAY "Enter a second character : " WITH NO ADVANCING
           ACCEPT ws-numeric-data-lower.

       AddValue SECTION.
           DISPLAY SPACE
           DISPLAY "Add the unit to the numerical value."
           ADD cte-01 TO ws-numeric-data.

       END PROGRAM EditNum.
