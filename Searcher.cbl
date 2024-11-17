       IDENTIFICATION DIVISION.
       PROGRAM-ID. Searcher.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       78  cte-35                                         VALUE 35.
       77  ws-cte-02                            PIC 9(01) VALUE 2.
       77  ws-cte-01                            PIC 9(01) VALUE 1.

       01  ws-environmental-variables.
           03  ws-chosen-option                 PIC 9(01) VALUE ZERO.
               88  sw-ch-opt-binsrch                      VALUE 1.
               88  sw-ch-opt-seqsrch                      VALUE 2.
               88  sw-ch-opt-dirloc                       VALUE 3.
               88  sw-ch-opt-uncluttertab                 VALUE 4.
               88  sw-ch-opt-bubblesort                   VALUE 5.
               88  sw-ch-opt-insertionsort                VALUE 6.
               88  sw-ch-opt-selectionsort                VALUE 7.
               88  sw-ch-opt-viewallitems                 VALUE 8.
               88  sw-ch-opt-exitprog                     VALUE 9.
           03  ws-ft-data-fruit-info.
               05  ws-ft-data-fruit-code     PIC 9(02)    VALUE ZEROES.
               05  ws-idx-ft-data-fruit      USAGE IS INDEX.
           03  ws-ft-data-fruit-aux.
               05  ws-ft-data-fruit-num-aux  PIC 9(02)    VALUE ZEROES.
               05  ws-ft-data-fruit-name-aux PIC A(13)    VALUE SPACES.
           03  ws-random-number-generator-vars.
               05  ws-amount-rnd-numbers     USAGE COMP-1 VALUE ZEROES.
               05  ws-idx-rnd-numbers        USAGE IS INDEX.
               05  ws-pseudo-random-number   USAGE COMP-1 VALUE ZEROES.  
               05  ws-rnd-n-destination-idx  USAGE IS INDEX.
               05  ws-rnd-n-origin-idx       USAGE IS INDEX.
               05  ws-rnd-n-temporal-idx     USAGE COMP-1 VALUE ZEROES.
               05  ws-seed-rnd-numbers       USAGE COMP-1 VALUE ZEROES.
       
       01  ws-fruit-table.
           03  ws-block-fruit-01.
               05  FILLER               PIC 9(02) VALUE 01.
               05  FILLER               PIC A(13) VALUE "Apple".
           03  ws-block-fruit-02.
               05  FILLER               PIC 9(02) VALUE 02.
               05  FILLER               PIC A(13) VALUE "Apricot".
           03  ws-block-fruit-03.
               05  FILLER               PIC 9(02) VALUE 03.
               05  FILLER               PIC A(13) VALUE "Avocado".
           03  ws-block-fruit-04.
               05  FILLER               PIC 9(02) VALUE 04.
               05  FILLER               PIC A(13) VALUE "Banana".
           03  ws-block-fruit-05.
               05  FILLER               PIC 9(02) VALUE 05.
               05  FILLER               PIC A(13) VALUE "Blackberry".
           03  ws-block-fruit-06.
               05  FILLER               PIC 9(02) VALUE 06.
               05  FILLER               PIC A(13) VALUE "Blueberry".
           03  ws-block-fruit-07.
               05  FILLER               PIC 9(02) VALUE 07.
               05  FILLER               PIC A(13) VALUE "Cherry".
           03  ws-block-fruit-08.
               05  FILLER               PIC 9(02) VALUE 08.
               05  FILLER               PIC A(13) VALUE "Coconut".
           03  ws-block-fruit-09.
               05  FILLER               PIC 9(02) VALUE 09.
               05  FILLER               PIC A(13) VALUE "Cranberry".
           03  ws-block-fruit-10.
               05  FILLER               PIC 9(02) VALUE 10.
               05  FILLER               PIC A(13) VALUE "Cucumber".
           03  ws-block-fruit-11.
               05  FILLER               PIC 9(02) VALUE 11.
               05  FILLER               PIC A(13) VALUE "Dragon fruit".
           03  ws-block-fruit-12.
               05  FILLER               PIC 9(02) VALUE 12.
               05  FILLER               PIC A(13) VALUE "Grape".
           03  ws-block-fruit-13.
               05  FILLER               PIC 9(02) VALUE 13.
               05  FILLER               PIC A(13) VALUE "Grapefruit".
           03  ws-block-fruit-14.
               05  FILLER               PIC 9(02) VALUE 14.
               05  FILLER               PIC A(13) VALUE "Jackfruit".
           03  ws-block-fruit-15.
               05  FILLER               PIC 9(02) VALUE 15.
               05  FILLER               PIC A(13) VALUE "Kiwi".
           03  ws-block-fruit-16.
               05  FILLER               PIC 9(02) VALUE 16.
               05  FILLER               PIC A(13) VALUE "Lemon".
           03  ws-block-fruit-17.
               05  FILLER               PIC 9(02) VALUE 17.
               05  FILLER               PIC A(13) VALUE "Lime".
           03  ws-block-fruit-18.
               05  FILLER               PIC 9(02) VALUE 18.
               05  FILLER               PIC A(13) VALUE "Mandarin".
           03  ws-block-fruit-19.
               05  FILLER               PIC 9(02) VALUE 19.
               05  FILLER               PIC A(13) VALUE "Mango".
           03  ws-block-fruit-20.
               05  FILLER               PIC 9(02) VALUE 20.
               05  FILLER               PIC A(13) VALUE "Melon".
           03  ws-block-fruit-21.
               05  FILLER               PIC 9(02) VALUE 21.
               05  FILLER               PIC A(13) VALUE "Nectarin".
           03  ws-block-fruit-22.
               05  FILLER               PIC 9(02) VALUE 22.
               05  FILLER               PIC A(13) VALUE "Orange".
           03  ws-block-fruit-23.
               05  FILLER               PIC 9(02) VALUE 23.
               05  FILLER               PIC A(13) VALUE "Papaya".
           03  ws-block-fruit-24.
               05  FILLER               PIC 9(02) VALUE 24.
               05  FILLER               PIC A(13) VALUE "Passion fruit".
           03  ws-block-fruit-25.
               05  FILLER               PIC 9(02) VALUE 25.
               05  FILLER               PIC A(13) VALUE "Peach".
           03  ws-block-fruit-26.
               05  FILLER               PIC 9(02) VALUE 26.
               05  FILLER               PIC A(13) VALUE "Pear".
           03  ws-block-fruit-27.
               05  FILLER               PIC 9(02) VALUE 27.
               05  FILLER               PIC A(13) VALUE "Pineapple".
           03  ws-block-fruit-28.
               05  FILLER               PIC 9(02) VALUE 28.
               05  FILLER               PIC A(13) VALUE "Plantain".
           03  ws-block-fruit-29.
               05  FILLER               PIC 9(02) VALUE 29.
               05  FILLER               PIC A(13) VALUE "Plum".
           03  ws-block-fruit-30.
               05  FILLER               PIC 9(02) VALUE 30.
               05  FILLER               PIC A(13) VALUE "Pomegranate".
           03  ws-block-fruit-31.
               05  FILLER               PIC 9(02) VALUE 31.
               05  FILLER               PIC A(13) VALUE "Raspberry".
           03  ws-block-fruit-32.
               05  FILLER               PIC 9(02) VALUE 32.
               05  FILLER               PIC A(13) VALUE "Starfruit".
           03  ws-block-fruit-33.
               05  FILLER               PIC 9(02) VALUE 33.
               05  FILLER               PIC A(13) VALUE "Strawberry".
           03  ws-block-fruit-34.
               05  FILLER               PIC 9(02) VALUE 34.
               05  FILLER               PIC A(13) VALUE "Tomato".
           03  ws-block-fruit-35.
               05  FILLER               PIC 9(02) VALUE 35.
               05  FILLER               PIC A(13) VALUE "Watermelon".

       01  ws-fruit-table-red       REDEFINES ws-fruit-table.
           03  ws-ft-data-fruit     OCCURS cte-35 TIMES
                                    ASCENDING KEY ws-ft-data-fruit-num
                                    INDEXED    BY idx-ft-data-fruit.
               05  ws-ft-data-fruit-num           PIC 9(02).
               05  ws-ft-data-fruit-name          PIC A(13).

       PROCEDURE DIVISION.
           DISPLAY SPACE
           DISPLAY "Tracking items in a table."

           PERFORM 100000-begin-main-options-menu
              THRU 100000-end-main-options-menu
             UNTIL sw-ch-opt-exitprog

           STOP "This program has ended..."
           STOP RUN.

       100000-begin-main-options-menu.
           PERFORM 110000-begin-show-main-options-menu
              THRU 110000-end-show-main-options-menu

           PERFORM 120000-begin-validate-option-chosen-main-menu
              THRU 120000-end-validate-option-chosen-main-menu.
       100000-end-main-options-menu.
           EXIT.

        110000-begin-show-main-options-menu.
           DISPLAY SPACE
           DISPLAY "+---+----+---+----+---+---+"
           DISPLAY "|      Option Menu.       |"
           DISPLAY "+---+----+---+----+---+---+"
           DISPLAY "| [1]. Binary Search.     |"
           DISPLAY "| [2]. Sequential Search. |"
           DISPLAY "| [3]. Locate directly.   |"
           DISPLAY "| [4]. Unclutter table.   |"
           DISPLAY "| [5]. Bubble sort.       |"
           DISPLAY "| [6]. Insertion sort.    |"
           DISPLAY "| [7]. Selection sort.    |"
           DISPLAY "| [8]. View all items.    |"
           DISPLAY "| [9]. Exit this program. |"
           DISPLAY "+---+----+---+----+---+---+"
           DISPLAY "Enter your choice: " WITH NO ADVANCING
           ACCEPT ws-chosen-option

           DISPLAY "The chosen option was: " ws-chosen-option.
        110000-end-show-main-options-menu.
           EXIT.

        120000-begin-validate-option-chosen-main-menu.
           EVALUATE TRUE
               WHEN sw-ch-opt-binsrch
                    PERFORM 121000-begin-binary-search
                       THRU 121000-end-binary-search

               WHEN sw-ch-opt-seqsrch
                    PERFORM 122000-begin-sequential-search
                       THRU 122000-end-sequential-search

               WHEN sw-ch-opt-dirloc
                    PERFORM 123000-begin-locate-directly
                       THRU 123000-end-locate-directly

               WHEN sw-ch-opt-uncluttertab
                    PERFORM 124000-begin-unclutter-initial-table
                       THRU 124000-end-unclutter-initial-table

               WHEN sw-ch-opt-bubblesort
                    SUBTRACT ws-cte-01 FROM cte-35
                      GIVING ws-rnd-n-temporal-idx

                    PERFORM 125000-begin-bubblesort
                       THRU 125000-end-bubblesort
                    VARYING ws-rnd-n-origin-idx
                       FROM ws-cte-01
                         BY ws-cte-01
                      UNTIL ws-rnd-n-origin-idx
                         IS GREATER THAN ws-rnd-n-temporal-idx
                      AFTER ws-rnd-n-destination-idx
                       FROM ws-rnd-n-origin-idx
                         BY ws-cte-01
                      UNTIL ws-rnd-n-destination-idx
                         IS GREATER THAN cte-35

               WHEN sw-ch-opt-insertionsort
                    PERFORM 126000-begin-insertionsort
                       THRU 126000-end-insertionsort
                    VARYING idx-ft-data-fruit
                       FROM ws-cte-02
                         BY ws-cte-01
                      UNTIL idx-ft-data-fruit
                         IS GREATER THAN OR IS EQUAL TO cte-35

               WHEN sw-ch-opt-selectionsort
                    PERFORM 127000-begin-selectionsort
                       THRU 127000-end-selectionsort
                    VARYING idx-ft-data-fruit
                       FROM ws-cte-01
                         BY ws-cte-01
                      UNTIL idx-ft-data-fruit
                         IS GREATER THAN cte-35

               WHEN sw-ch-opt-viewallitems
                    PERFORM 121110-begin-request-index-position
                       THRU 121110-end-request-index-position

                    PERFORM 121300-begin-show-table-record-info
                       THRU 121300-end-show-table-record-info
                    VARYING idx-ft-data-fruit
                       FROM ws-idx-ft-data-fruit
                         BY ws-cte-01
                      UNTIL idx-ft-data-fruit
                         IS GREATER THAN cte-35

               WHEN sw-ch-opt-exitprog
                    DISPLAY "Leaving this program..."

               WHEN OTHER
                    DISPLAY "Incorrect option. Please correct your "
                            "choice..."
           END-EVALUATE.
        120000-end-validate-option-chosen-main-menu.
           EXIT.

         121000-begin-binary-search.
           DISPLAY "Binary Search."

           PERFORM 121100-begin-request-search-data
              THRU 121100-end-request-search-data

           SET idx-ft-data-fruit TO ws-idx-ft-data-fruit
           SEARCH ALL ws-ft-data-fruit
               AT END
                  PERFORM 121200-start-element-not-located
                     THRU 121200-finish-element-not-located
                  
             WHEN ws-ft-data-fruit-num (idx-ft-data-fruit) 
               IS EQUAL TO              ws-ft-data-fruit-code
                  PERFORM 121300-begin-show-table-record-info
                     THRU 121300-end-show-table-record-info
                           
           END-SEARCH.
         121000-end-binary-search.
           EXIT.

          121100-begin-request-search-data.
            PERFORM 121110-begin-request-index-position
               THRU 121110-end-request-index-position

            PERFORM 121120-begin-request-value-code-search
               THRU 121120-end-request-value-code-search.
          121100-end-request-search-data.
            EXIT.

          121110-begin-request-index-position.
            DISPLAY "Enter the logical position to start ("
                    ws-cte-01 " - " cte-35 ") : "
               WITH NO ADVANCING

            ACCEPT ws-idx-ft-data-fruit.
          121110-end-request-index-position.
            EXIT.

          121120-begin-request-value-code-search.
            DISPLAY "Enter the value code you want to search for  : "
               WITH NO ADVANCING

            ACCEPT ws-ft-data-fruit-code.
          121120-end-request-value-code-search.
            EXIT.

          121200-start-element-not-located.
            DISPLAY SPACE
            DISPLAY "+---+----+---+----+---+----+"
            DISPLAY "|     Item not located.    |"
            DISPLAY "+---+----+---+----+---+----+"
            DISPLAY "| Indicated position : ["
                    ws-idx-ft-data-fruit "]."
            DISPLAY "| Searched item code : ["
                    ws-ft-data-fruit-code "]."
            DISPLAY "| Search idx reached : ["
                    idx-ft-data-fruit "]."
            DISPLAY "+---+----+---+----+---+----+"
            DISPLAY "Press the ENTER key to continue..."
               WITH NO ADVANCING
            ACCEPT OMITTED.
          121200-finish-element-not-located.
            EXIT.

          121300-begin-show-table-record-info.
            DISPLAY SPACE
            DISPLAY "+---+----+---+----+---+----+---+"
            DISPLAY "| Retrieved record information.|"
            DISPLAY "+---+----+---+----+---+----+---+"
            DISPLAY "| Start : [" ws-idx-ft-data-fruit "]."
            DISPLAY "| Index : [" idx-ft-data-fruit "]."
            DISPLAY "+------------------------------+"
            DISPLAY "| Code  : [" ws-ft-data-fruit-num 
                                 (idx-ft-data-fruit) "]."
            DISPLAY "| Name  : [" ws-ft-data-fruit-name
                                 (idx-ft-data-fruit) "].".
            DISPLAY "+---+----+---+----+---+----+---+"
            DISPLAY "Press the ENTER key to continue..."
               WITH NO ADVANCING
            ACCEPT OMITTED.
          121300-end-show-table-record-info.
            EXIT.

         122000-begin-sequential-search.
           DISPLAY "Sequential Search."

           PERFORM 121100-begin-request-search-data
              THRU 121100-end-request-search-data

           SET idx-ft-data-fruit TO ws-idx-ft-data-fruit
           SEARCH ws-ft-data-fruit
               AT END
                  PERFORM 121200-start-element-not-located
                     THRU 121200-finish-element-not-located
                  
             WHEN ws-ft-data-fruit-num (idx-ft-data-fruit) 
               IS EQUAL TO              ws-ft-data-fruit-code
                  PERFORM 121300-begin-show-table-record-info
                     THRU 121300-end-show-table-record-info

           END-SEARCH.
         122000-end-sequential-search.
           EXIT.

         123000-begin-locate-directly.
           DISPLAY "Direct element location."

           PERFORM 121110-begin-request-index-position
              THRU 121110-end-request-index-position

           SET idx-ft-data-fruit TO ws-idx-ft-data-fruit

           PERFORM 121300-begin-show-table-record-info
              THRU 121300-end-show-table-record-info.
         123000-end-locate-directly.
           EXIT.

         124000-begin-unclutter-initial-table.
           DISPLAY "Table clutter."

           PERFORM 124100-start-request-performance-dat
              THRU 124100-finish-request-performance-dat

           PERFORM 124200-start-random-number-germinator
              THRU 124200-finish-random-number-germinator
           VARYING ws-idx-rnd-numbers
              FROM ws-cte-01 BY ws-cte-01
             UNTIL ws-idx-rnd-numbers
                IS GREATER THAN ws-amount-rnd-numbers.
         124000-end-unclutter-initial-table.
           EXIT.

          124100-start-request-performance-dat.
            DISPLAY "How many random numbers do you want? : "
               WITH NO ADVANCING
             ACCEPT ws-amount-rnd-numbers

            DISPLAY "Seed number to generate the numbers  : "
               WITH NO ADVANCING
             ACCEPT ws-seed-rnd-numbers

            MOVE FUNCTION RANDOM (ws-seed-rnd-numbers)
              TO ws-pseudo-random-number.
          124100-finish-request-performance-dat.
            EXIT.

          124200-start-random-number-germinator.
            MOVE ZEROES                  TO ws-rnd-n-temporal-idx
            PERFORM 124210-start-assign-random-swap-index
               THRU 124210-finish-assign-random-swap-index
            SET ws-rnd-n-origin-idx      TO ws-rnd-n-temporal-idx

            MOVE ZEROES                  TO ws-rnd-n-temporal-idx
            PERFORM 124210-start-assign-random-swap-index
               THRU 124210-finish-assign-random-swap-index
            SET ws-rnd-n-destination-idx TO ws-rnd-n-temporal-idx
 
            PERFORM 124220-start-exchange-table-positions-values
               THRU 124220-finish-exchange-table-position-values.
          124200-finish-random-number-germinator.
            EXIT.

          124210-start-assign-random-swap-index.
            MOVE FUNCTION RANDOM TO ws-pseudo-random-number

            SUBTRACT ws-cte-01 FROM cte-35 GIVING ws-rnd-n-temporal-idx
            MULTIPLY ws-pseudo-random-number   BY ws-rnd-n-temporal-idx

            ADD ws-cte-01        TO ws-rnd-n-temporal-idx.
          124210-finish-assign-random-swap-index.
            EXIT.

          124220-start-exchange-table-positions-values.
            INITIALIZE ws-ft-data-fruit-aux

            MOVE ws-ft-data-fruit (ws-rnd-n-origin-idx)
              TO ws-ft-data-fruit-aux
            MOVE ws-ft-data-fruit (ws-rnd-n-destination-idx)
              TO ws-ft-data-fruit (ws-rnd-n-origin-idx)
            MOVE ws-ft-data-fruit-aux
              TO ws-ft-data-fruit (ws-rnd-n-destination-idx).
          124220-finish-exchange-table-position-values.
             EXIT.

          125000-begin-bubblesort.
            IF ws-ft-data-fruit (ws-rnd-n-origin-idx) IS GREATER THAN
               ws-ft-data-fruit (ws-rnd-n-destination-idx)
                 PERFORM 124220-start-exchange-table-positions-values
                    THRU 124220-finish-exchange-table-position-values
            END-IF.
          125000-end-bubblesort.
            EXIT.

          126000-begin-insertionsort.
            MOVE ws-ft-data-fruit      (idx-ft-data-fruit)
              TO ws-ft-data-fruit-aux

            SET ws-rnd-n-origin-idx            TO idx-ft-data-fruit
            SET ws-rnd-n-origin-idx            DOWN BY ws-cte-01

            PERFORM UNTIL ws-rnd-n-origin-idx IS LESS THAN ws-cte-01
                 OR ws-ft-data-fruit   (ws-rnd-n-origin-idx)
                 IS LESS THAN ws-ft-data-fruit-aux

                  SET ws-rnd-n-destination-idx TO ws-rnd-n-origin-idx
                  SET ws-rnd-n-destination-idx UP BY ws-cte-01

                 MOVE ws-ft-data-fruit (ws-rnd-n-origin-idx)
                   TO ws-ft-data-fruit (ws-rnd-n-destination-idx)

                  SET ws-rnd-n-origin-idx      DOWN BY ws-cte-01
            END-PERFORM

            SET ws-rnd-n-destination-idx       TO ws-rnd-n-origin-idx
            SET ws-rnd-n-destination-idx       UP BY ws-cte-01

            MOVE ws-ft-data-fruit-aux
              TO ws-ft-data-fruit (ws-rnd-n-destination-idx).
          126000-end-insertionsort.
            EXIT.

          127000-begin-selectionsort.
            SET ws-rnd-n-origin-idx
                ws-rnd-n-destination-idx       TO idx-ft-data-fruit
            SET ws-rnd-n-destination-idx       UP BY ws-cte-01

            PERFORM UNTIL ws-rnd-n-destination-idx 
                 IS GREATER THAN OR    IS EQUAL TO cte-35
                    IF ws-ft-data-fruit (ws-rnd-n-destination-idx) 
                    IS LESS THAN 
                       ws-ft-data-fruit (ws-rnd-n-origin-idx)
                          SET ws-rnd-n-origin-idx 
                           TO ws-rnd-n-destination-idx
                    END-IF

                    SET ws-rnd-n-destination-idx   UP BY ws-cte-01
            END-PERFORM

            SET ws-rnd-n-destination-idx        TO idx-ft-data-fruit
            PERFORM 124220-start-exchange-table-positions-values
               THRU 124220-finish-exchange-table-position-values.
          127000-end-selectionsort.
            EXIT.

       END PROGRAM Searcher.
