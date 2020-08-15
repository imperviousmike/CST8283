       IDENTIFICATION DIVISION.
       PROGRAM-ID. RECORD-UPDATE.
       AUTHOR. MICHAEL DALY.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       SELECT INVENT-IN
           ASSIGN TO "/home/mike/project4/INVENT6"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS RANDOM
           RECORD KEY IS PART-NUMBER
           FILE STATUS IS INVENT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  INVENT-IN
           DATA RECORD IS INVENT-RECORD.
       01  INVENT-RECORD.
           05  PART-NUMBER     PIC 9(5).
           05  PART-NAME       PIC X(20).
           05  QUANTITY        PIC 9(3).
           05  UNIT-PRICE      PIC 9(2)V99.
           05  SUPPLIER-CODE   PIC X(5).
           05  REORDER         PIC X(3).

       WORKING-STORAGE SECTION.
       01  SWITCHES.
           05  INVALID-SWITCH          PIC X(1) VALUE SPACES.
               88  INVAL-NO            VALUE "N".
           05  CONFIRM-SWITCH          PIC X(1) VALUE SPACES.
               88  VALID-SWITCH        VALUE "Y" "N".
               88  NO-SWITCH           VALUE "N".

       01  SCREEN-COLORS.
           05  BLACK               PIC S9(4) COMP-5 VALUE 0.
           05  GREEN               PIC S9(4) COMP-5 VALUE 2.
           05  RED                 PIC S9(4) COMP-5 VALUE 4.

       01  OTHER-FIELDS.
           05  INVENT-STATUS             PIC X(2).
           05  VALID-CODE                PIC X(2).
                88  SALE-CODE            VALUE "S".
                88  RECEIPT-CODE         VALUE "R".
           05  INPUT-VALUE               PIC 9(3).

       01  UPDATE-MESSAGE.
           05  CONFIRM-MESSAGE     PIC X(16)
               VALUE "UPDATE A RECORD?".

       SCREEN SECTION.
       01  OPENING-SCREEN.
           05  SCREEN-BASICS.
               10  LINE  1 COLUMN 30
                           VALUE "ONLINE UPDATE PROGRAM".
               10  LINE  3 COLUMN  30   VALUE "PART NUMBER:".
               10  LINE  5 COLUMN  20
                           VALUE
                               "TRANSACTION CODE:     (S)ale (R)eceipt".
               10  LINE  7 COLUMN  28   VALUE "TRANSACTION AMOUNT:".
               10  LINE 9 COLUMN  25
                           VALUE "******************************".

           05  SCREEN-VALUES.
               10  INPUT-PART-NUMBER   PIC 9(5) TO    PART-NUMBER
                   LINE  3 COLUMN 45   FOREGROUND-COLOR GREEN
                                       REVERSE-VIDEO AUTO.
               10  TRANSACTION-TYPE    PIC X(1) TO    VALID-CODE
                   LINE  5 COLUMN 39   FOREGROUND-COLOR GREEN
                                       REVERSE-VIDEO AUTO.
               10  AMOUNT              PIC 9(3) TO    INPUT-VALUE
                   LINE  7 COLUMN 48   FOREGROUND-COLOR GREEN
                                       REVERSE-VIDEO AUTO.
               10  LINE 12 BLANK LINE.
               10  LINE 13 BLANK LINE.
               10  LINE 15 BLANK LINE.

       01  CONFIRM-SCREEN.
           05  LINE 15 BLANK LINE      BACKGROUND-COLOR BLACK.
           05                          PIC X(16) FROM CONFIRM-MESSAGE
               LINE 15 COLUMN  25
               BACKGROUND-COLOR BLACK.
           05                          PIC X(1) USING CONFIRM-SWITCH
               LINE 15 COLUMN 42       BLINK AUTO
               BACKGROUND-COLOR BLACK  FOREGROUND-COLOR GREEN.
           05  LINE 13 BLANK LINE      BACKGROUND-COLOR BLACK.

       01  ERROR-SCREEN.
           05  LINE 15 BLANK LINE      BACKGROUND-COLOR RED.
           05  LINE 15 COLUMN  30
                       VALUE "ERROR:PRIMARY KEY NOT FOUND"
               BACKGROUND-COLOR RED    FOREGROUND-COLOR BLACK.

       01  CLEAR-ERROR-SCREEN.
            05  LINE 15 BLANK LINE.

       PROCEDURE DIVISION.
       100-UPDATE-RECORD.
           PERFORM 200-INITIATE-INVENTORY-UPDATE.
           PERFORM 200-START-INVENTORY-UPDATE UNTIL NO-SWITCH.
           PERFORM 200-TERMINATE-INVENTORY-UPDATE.
           STOP RUN.

       200-INITIATE-INVENTORY-UPDATE.
           PERFORM 300-OPEN-INVENTORY-FILE.
           PERFORM 300-INITIALIZE-SWITCHES.
           PERFORM 300-PROMPT-UPDATE.

       200-START-INVENTORY-UPDATE.
           PERFORM 300-ENTER-UPDATE-RECORD.
           PERFORM 300-UPDATE-AMOUNT.
           PERFORM 300-REWRITE-INVENTORY-RECORD.
           PERFORM 300-PROMPT-UPDATE.


       200-TERMINATE-INVENTORY-UPDATE.
           PERFORM 300-CLOSE-INVENTORY-FILE.

       300-OPEN-INVENTORY-FILE.
           OPEN I-O INVENT-IN.

       300-INITIALIZE-SWITCHES.
           INITIALIZE SWITCHES.

       300-PROMPT-UPDATE.
           DISPLAY CONFIRM-SCREEN.
           ACCEPT CONFIRM-SCREEN.

       300-ENTER-UPDATE-RECORD.
           PERFORM 400-DISPLAY-OPENING-SCREEN.
           PERFORM 400-INITIALIZE-INVALID-SWITCH.
           PERFORM 400-ACCEPT-PART-NUMBER UNTIL INVAL-NO.
           PERFORM 400-ACCEPT-TRANSACTION-TYPE
               UNTIL SALE-CODE OR RECEIPT-CODE.
           PERFORM 400-ACCEPT-AMOUNT.

       300-UPDATE-AMOUNT.
           IF  SALE-CODE
               SUBTRACT INPUT-VALUE FROM QUANTITY
           ELSE
               ADD INPUT-VALUE TO QUANTITY.

       300-REWRITE-INVENTORY-RECORD.
           REWRITE INVENT-RECORD
               INVALID KEY DISPLAY ERROR-SCREEN.

       300-CLOSE-INVENTORY-FILE.
           CLOSE INVENT-IN.

       400-DISPLAY-OPENING-SCREEN.
           INITIALIZE OTHER-FIELDS INVENT-RECORD.
           DISPLAY OPENING-SCREEN.

       400-INITIALIZE-INVALID-SWITCH.
           MOVE SPACES TO INVALID-SWITCH.

       400-ACCEPT-PART-NUMBER.
           ACCEPT  INPUT-PART-NUMBER.
           READ INVENT-IN KEY IS PART-NUMBER
               INVALID KEY MOVE "Y" TO INVALID-SWITCH
                               DISPLAY ERROR-SCREEN
               NOT INVALID KEY MOVE "N" TO INVALID-SWITCH
                               DISPLAY CLEAR-ERROR-SCREEN.

       400-ACCEPT-TRANSACTION-TYPE.
           ACCEPT  TRANSACTION-TYPE.

       400-ACCEPT-AMOUNT.
           ACCEPT  AMOUNT.
