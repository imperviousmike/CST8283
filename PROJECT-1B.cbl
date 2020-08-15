       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROJECT-1-B.
       AUTHOR. MICHAEL DALY.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *This will be the output file
           SELECT INVENT-FILE-OUT
               ASSIGN TO "C:\INVFILE.TXT"
                   ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  INVENT-FILE-OUT.
      *Record to be written here before being saved.
       01  INVENT-RECORD-OUT       PIC X(18).

       WORKING-STORAGE SECTION.
      *Store the values inputted by the user here before writing .
       01  INVENT-RECORD-IN.
           05  PART-NUM              PIC 9(7)      VALUE 0.
           05  FILLER                PIC X(1)      VALUE SPACES.
           05  QUANTITY              PIC 9(4)      VALUE 0.
           05  FILLER                PIC X(1)      VALUE SPACES.
           05  UNIT-PRICE            PIC 9(4)      VALUE 0.

      *The response by the user if they want to enter more records.
       01  FLAGS-AND-CONTROLS.
           05  USER-RESPONSE         PIC a(1)        VALUE "Y".

      *Holds the values for the prompts to output to the user.
       01  PROMPTS.
           05  RECORD-PROMPT         PIC X(30)
                                  VALUE "ENTER ANOTHER RECORD? Y  or N".
           05  PART-NUM-PROMPT       PIC X(21)
                                  VALUE "ENTER PART NUMBER".
           05  QUANTITY-PROMPT       PIC X(23)
                                  VALUE "ENTER QUANTITY ON HAND".
           05  UNIT-PRICE-PROMPT    PIC X(16)
                                  VALUE "ENTER UNIT-PRICE".

       PROCEDURE DIVISION.

      *Main procedure that determines the process the applicaton follows.
       100-CREATE-INVENT-FILE.
           PERFORM 200-INIT-CREATE-INVENT-FILE.
           PERFORM 200-CREATE-INVENT-RECORD
               UNTIL USER-RESPONSE = "N" OR "n".
           PERFORM 200-CLOSE-INVENT-FILE.
           STOP RUN.
      *Opens File and clears screen from any legacy inputs.
       200-INIT-CREATE-INVENT-FILE.
           PERFORM  200-OPEN-INVENT-FILE
           PERFORM  300-CLEAR-SCREEN.
      *Prompt user to enter a record, write it, then clear for the next potential input
       200-CREATE-INVENT-RECORD.
           PERFORM  300-ENTER-INVENT-DATA.
           PERFORM  300-WRITE-INVENT-RECORD.
           PERFORM  300-CLEAR-SCREEN.

      * Close the file
       200-CLOSE-INVENT-FILE.
           CLOSE INVENT-FILE-OUT.
      *Opens the file for writing.
       200-OPEN-INVENT-FILE.
           OPEN OUTPUT INVENT-FILE-OUT.
      *Clear any inputs from a previous record entry or application.
       300-CLEAR-SCREEN.
           DISPLAY " " WITH BLANK SCREEN.
      *Point of interaction between user and application to get the record values.
       300-ENTER-INVENT-DATA.
           DISPLAY PART-NUM-PROMPT     LINE 4 COLUMN 5.
           ACCEPT  PART-NUM            LINE 5 COLUMN 10.

           DISPLAY QUANTITY-PROMPT     LINE 6 COLUMN 5.
           ACCEPT  QUANTITY            LINE 7 COLUMN 10.

           DISPLAY UNIT-PRICE-PROMPT   LINE 8 COLUMN 5.
           ACCEPT  UNIT-PRICE          LINE 9 COLUMN 10.

           DISPLAY RECORD-PROMPT       LINE 16 COLUMN 10.
           ACCEPT  USER-RESPONSE       LINE 17 COLUMN 10.
      *User has now input values, move to output record write it, and prepare for next input.
       300-WRITE-INVENT-RECORD.
           MOVE   INVENT-RECORD-IN  TO  INVENT-RECORD-OUT.
           WRITE  INVENT-RECORD-OUT.
           INITIALIZE INVENT-RECORD-IN.
