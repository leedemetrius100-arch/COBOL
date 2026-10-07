      ***************************************************************
      **            I D E N T I F I C A T I O N                    **
      **                  D I V I S I O N                          **
      ***************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.   ledgerinput.
       AUTHOR.       Lee.
       DATE-WRITTEN. 03/10/26.
      
      ***************************************************************
      ***************************************************************
      **   Program Information :-                                  **
      **                                                           **
      **   System   : Batch Ledger System                          **
      **                                                           **
      **   Function.: Allows input of the day's transactions.      **
      **                                                           **
      **   Invocation: Called Home Screen Program.                 **
      **                                                           **
      **   Usage:      On request                                  **
      ***************************************************************
      ***************************************************************

      ***************************************************************
      ***************************************************************
      **                                                           **
      **   Amendment History :                                     **
      **                                                           **
      **   Developer     Date     Change No Details of change      **
      **                                                           **
      **   XXXXXXXXXXX   DD/MM/YY XXXXXXXX  XXXXXXXXXXXXXXXXXXXXXX ** 
      **                                    XXXXXXXXXXXXXXXXXXXXXX **
      **                                    XXXXXXXXXXXXXXXXXXXXXX **
      ***************************************************************
      ***************************************************************

      ***************************************************************
      **               E N V I R O N M E N T                       **
      **                  D I V I S I O N                          **
      ***************************************************************
       ENVIRONMENT DIVISION.
         CONFIGURATION SECTION.
            SOURCE-COMPUTER. APPLE IMAC.
            OBJECT-COMPUTER. APPLE IMAC.

         INPUT-OUTPUT SECTION.
             FILE-CONTROL.
   
      **
      ** ledger.txt : Accounts Master File
      **
                SELECT LEDGER-FILE ASSIGN TO "ledger.txt"
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS SEQUENTIAL
                   RECORD KEY IS LEDGER-TRANSACT-NUMBER
                   FILE STATUS IS LEDGER-FILE-STATUS.       

      ***************************************************************
      **                      D A T A                              **
      **                  D I V I S I O N                          **
      ***************************************************************
         DATA DIVISION.
         FILE SECTION.
              FD LEDGER-FILE
                     LABEL RECORDS ARE STANDARD.
               01 LEDGER-RECORD.
                  03 LEDGER-ACC-NUMBER PIC 9(5).
                  03 LEDGER-TRANSACT-NUMBER PIC 9(5).
                  03 LEDGER-TRANSACT-AMOUNT PIC 9(5)V99.
                  03 LEDGER-TRANSACT-DATE PIC 9(8). 
                  03 LEDGER-TRANSACT-TYPE PIC X(1).
                  
      *** 
      *ABOVE SHOULD BE A COPY DD-ALL-FORMATS STATEMENT IN AS/400 WORLD
      *** 

         WORKING-STORAGE SECTION.
              01 LEDGER-FILE-STATUS PIC XX.
              01 WS-LEDGER-RECORD.
                  03 WS-LEDGER-ACC-NUMBER PIC 9(5).
                  03 WS-LEDGER-TRANSACT-NUMBER PIC 9(5).
                  03 WS-LEDGER-TRANSACT-AMOUNT PIC 9(5)V99.
                  03 WS-LEDGER-TRANSACT-DATE PIC 9(8).
                  03 WS-LEDGER-TRANSACT-TYPE PIC X(1).
                  
              77  WS-ERROR PIC X(30) VALUE SPACES.
              77  WS-TRANSACT-NUMBER PIC 9(5) VALUE 76000.
      ****
      **** NB. TRANSACTION NUMBER WILL NOT BE UNIQUE IF THIS PROGRAM
      **** IF RUN MORE THAN ONCE. BUT THIS IS ACCEPTABLE BECAUUSE 
      **** WE ARE JUST CREATING TEST DATA. IN REALITY WE WOULD HAVE TO 
      **** SET THE ABOVE VARIABLE TO THE LAST TRANSACTION ON THE 
      **** LEDGER FILE AND INCREMENT IT BY 1 FOR EACH NEW TRANSACTION

       LINKAGE SECTION.
      **
      ** HERE WE WOULD DEFINE PARAMETERS PASSED FROM THE CALLING PROGRAM
      ** TO BE USED IN USING STATEMENT OF PROCEDURE DIVISION  
      ** IN THIS CASE WE ARE NOT PASSING ANY PARAMETERS SO THIS SECTION 
      ** IS EMPTY

      ***************************************************************
      **                P R O C E D U R E                          **
      **                  D I V I S I O N                          **
      ***************************************************************
        PROCEDURE DIVISION.
        A1-MAIN-PROGRAM.
           PERFORM B1-START.
           PERFORM B2-MAIN-PROCESS.
           PERFORM B3-ENDING.

           STOP RUN.

        B1-START.
            OPEN EXTEND LEDGER-FILE.
            ACCEPT WS-LEDGER-TRANSACT-DATE FROM DATE YYYYMMDD.

        B2-MAIN-PROCESS.
            DISPLAY "Enter Account Number (5 digits) or 0 to end: " 
-               WITH NO ADVANCING.            
            ACCEPT WS-LEDGER-ACC-NUMBER.
            PERFORM UNTIL WS-LEDGER-ACC-NUMBER = 0
                DISPLAY "Enter Transaction type (D or C): "
-                   WITH NO ADVANCING            
                ACCEPT WS-LEDGER-TRANSACT-TYPE
                
                DISPLAY "Enter Transaction Amount: "
-                   WITH NO ADVANCING           
                ACCEPT WS-LEDGER-TRANSACT-AMOUNT
                
                MOVE WS-LEDGER-ACC-NUMBER TO LEDGER-ACC-NUMBER
                MOVE WS-LEDGER-TRANSACT-AMOUNT TO LEDGER-TRANSACT-AMOUNT
                MOVE WS-LEDGER-TRANSACT-DATE TO LEDGER-TRANSACT-DATE
                MOVE WS-LEDGER-TRANSACT-TYPE TO LEDGER-TRANSACT-TYPE
                ADD 1 TO WS-TRANSACT-NUMBER
                MOVE WS-TRANSACT-NUMBER TO LEDGER-TRANSACT-NUMBER
                PERFORM C1-WRITE-LEDGER-RECORD
                DISPLAY "Enter Account Number (5 digits) or 0 to end: "
-                   WITH NO ADVANCING           
                ACCEPT WS-LEDGER-ACC-NUMBER
            END-PERFORM.

        B3-ENDING.
            CLOSE LEDGER-FILE.

        C1-WRITE-LEDGER-RECORD.
            WRITE LEDGER-RECORD INVALID KEY
                MOVE "C1- ERROR WRITING TO LEDGER FILE " TO WS-ERROR
                PERFORM ZZ-ABORT.

        ZZ-ABORT.
            DISPLAY WS-ERROR
            CLOSE LEDGER-FILE
            STOP RUN.

            

