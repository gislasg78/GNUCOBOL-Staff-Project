       IDENTIFICATION DIVISION.
       PROGRAM-ID. Searcher.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  ws-fruit-code          PIC 9(02) VALUE ZEROES.
       77  ws-continue-response   PIC A(01) VALUE SPACE.
           88  sw-continue-response-N VALUES ARE 'N' 'n'.

       01  ws-cte-01              PIC 9(01) VALUE 01.
       01  ws-cte-10              PIC 9(02) VALUE 10.
       
       01  ws-fruit-table.
           03  ws-block-fruit-01.
               05  FILLER         PIC 9(02) VALUE 01.
               05  FILLER         PIC A(13) VALUE "Apple".
           03  ws-block-fruit-02.
               05  FILLER         PIC 9(02) VALUE 02.
               05  FILLER         PIC A(13) VALUE "Apricot".
           03  ws-block-fruit-03.
               05  FILLER         PIC 9(02) VALUE 03.
               05  FILLER         PIC A(13) VALUE "Avocado".
           03  ws-block-fruit-04.
               05  FILLER         PIC 9(02) VALUE 04.
               05  FILLER         PIC A(13) VALUE "Banana".
           03  ws-block-fruit-05.
               05  FILLER         PIC 9(02) VALUE 05.
               05  FILLER         PIC A(13) VALUE "Blackberry".
           03  ws-block-fruit-06.
               05  FILLER         PIC 9(02) VALUE 06.
               05  FILLER         PIC A(13) VALUE "Blueberry".
           03  ws-block-fruit-07.
               05  FILLER         PIC 9(02) VALUE 07.
               05  FILLER         PIC A(13) VALUE "Cherry".
           03  ws-block-fruit-08.
               05  FILLER         PIC 9(02) VALUE 08.
               05  FILLER         PIC A(13) VALUE "Coconut".
           03  ws-block-fruit-09.
               05  FILLER         PIC 9(02) VALUE 09.
               05  FILLER         PIC A(13) VALUE "Cranberry".
           03  ws-block-fruit-10.
               05  FILLER         PIC 9(02) VALUE 10.
               05  FILLER         PIC A(13) VALUE "Cucumber".
           03  ws-block-fruit-11.
               05  FILLER         PIC 9(02) VALUE 11.
               05  FILLER         PIC A(13) VALUE "Dragon fruit".
           03  ws-block-fruit-12.
               05  FILLER         PIC 9(02) VALUE 12.
               05  FILLER         PIC A(13) VALUE "Grape".
           03  ws-block-fruit-13.
               05  FILLER         PIC 9(02) VALUE 13.
               05  FILLER         PIC A(13) VALUE "Grapefruit".
           03  ws-block-fruit-14.
               05  FILLER         PIC 9(02) VALUE 14.
               05  FILLER         PIC A(13) VALUE "Jackfruit".
           03  ws-block-fruit-15.
               05  FILLER         PIC 9(02) VALUE 15.
               05  FILLER         PIC A(13) VALUE "Kiwi".
           03  ws-block-fruit-16.
               05  FILLER         PIC 9(02) VALUE 16.
               05  FILLER         PIC A(13) VALUE "Lemon".
           03  ws-block-fruit-17.
               05  FILLER         PIC 9(02) VALUE 17.
               05  FILLER         PIC A(13) VALUE "Lime".
           03  ws-block-fruit-18.
               05  FILLER         PIC 9(02) VALUE 18.
               05  FILLER         PIC A(13) VALUE "Mandarin".
           03  ws-block-fruit-19.
               05  FILLER         PIC 9(02) VALUE 19.
               05  FILLER         PIC A(13) VALUE "Mango".
           03  ws-block-fruit-20.
               05  FILLER         PIC 9(02) VALUE 20.
               05  FILLER         PIC A(13) VALUE "Melon".
           03  ws-block-fruit-21.
               05  FILLER         PIC 9(02) VALUE 21.
               05  FILLER         PIC A(13) VALUE "Nectarin".
           03  ws-block-fruit-22.
               05  FILLER         PIC 9(02) VALUE 22.
               05  FILLER         PIC A(13) VALUE "Orange".
           03  ws-block-fruit-23.
               05  FILLER         PIC 9(02) VALUE 23.
               05  FILLER         PIC A(13) VALUE "Papaya".
           03  ws-block-fruit-24.
               05  FILLER         PIC 9(02) VALUE 24.
               05  FILLER         PIC A(13) VALUE "Passion fruit".
           03  ws-block-fruit-25.
               05  FILLER         PIC 9(02) VALUE 25.
               05  FILLER         PIC A(13) VALUE "Peach".
           03  ws-block-fruit-26.
               05  FILLER         PIC 9(02) VALUE 26.
               05  FILLER         PIC A(13) VALUE "Pear".
           03  ws-block-fruit-27.
               05  FILLER         PIC 9(02) VALUE 27.
               05  FILLER         PIC A(13) VALUE "Pineapple".
           03  ws-block-fruit-28.
               05  FILLER         PIC 9(02) VALUE 28.
               05  FILLER         PIC A(13) VALUE "Plantain".
           03  ws-block-fruit-29.
               05  FILLER         PIC 9(02) VALUE 29.
               05  FILLER         PIC A(13) VALUE "Plum".
           03  ws-block-fruit-30.
               05  FILLER         PIC 9(02) VALUE 30.
               05  FILLER         PIC A(13) VALUE "Pomegranate".
           03  ws-block-fruit-31.
               05  FILLER         PIC 9(02) VALUE 31.
               05  FILLER         PIC A(13) VALUE "Raspberry".
           03  ws-block-fruit-32.
               05  FILLER         PIC 9(02) VALUE 32.
               05  FILLER         PIC A(13) VALUE "Starfruit".
           03  ws-block-fruit-33.
               05  FILLER         PIC 9(02) VALUE 33.
               05  FILLER         PIC A(13) VALUE "Strawberry".
           03  ws-block-fruit-34.
               05  FILLER         PIC 9(02) VALUE 34.
               05  FILLER         PIC A(13) VALUE "Tomato".
           03  ws-block-fruit-35.
               05  FILLER         PIC 9(02) VALUE 35.
               05  FILLER         PIC A(13) VALUE "Watermelon".

       01  ws-fruit-table-red REDEFINES ws-fruit-table.
           03  ws-data-fruit  OCCURS 35 TIMES
                              ASCENDING KEY ws-data-fruit-num
                              INDEXED    BY idx-data-fruit.
               05  ws-data-fruit-num    PIC 9(02).
               05  ws-data-fruit-name   PIC A(13).

       PROCEDURE DIVISION.
           DISPLAY "Fruit search in English."

           PERFORM UNTIL sw-continue-response-N
               DISPLAY SPACE
               DISPLAY "Enter a fruit code from (01 - 35) : "
                       WITH NO ADVANCING
               ACCEPT ws-fruit-code
           
               PERFORM Sequentially-Searching
               PERFORM Binaryly-Searching

               DISPLAY SPACE
               DISPLAY "Do you want to continue looking for other "
                       "fruits? (y/n) : " WITH NO ADVANCING
               ACCEPT ws-continue-response
           END-PERFORM

           STOP RUN.

        Sequentially-Searching.
           DISPLAY SPACE
           DISPLAY "Sequential Search."
           
           SET idx-data-fruit TO ws-cte-01
           SEARCH ws-data-fruit
               AT END
                  DISPLAY "Fruit code: [" ws-fruit-code "] not located."
                  
             WHEN ws-data-fruit-num(idx-data-fruit) IS EQUAL TO
                  ws-fruit-code
                  PERFORM Showing-Info

           END-SEARCH.
           
        Binaryly-Searching.
           DISPLAY SPACE
           DISPLAY "Binary Search."

           SET idx-data-fruit TO ws-cte-01
           SEARCH ALL ws-data-fruit
               AT END
                  DISPLAY "Fruit code: [" ws-fruit-code "] not located."
                  
             WHEN ws-data-fruit-num(idx-data-fruit) IS EQUAL TO
                  ws-fruit-code
                  PERFORM Showing-Info
                           
           END-SEARCH.

        Showing-Info.
           DISPLAY "Index : [" idx-data-fruit "]."
           DISPLAY "Code  : [" ws-data-fruit-num  (idx-data-fruit) "]."
           DISPLAY "Name  : [" ws-data-fruit-name (idx-data-fruit) "].".

       END PROGRAM Searcher.
