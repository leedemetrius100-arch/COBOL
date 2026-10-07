      ***************************************************************
      **            I D E N T I F I C A T I O N                    **
      **                  D I V I S I O N                          **
      ***************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.   MASTER-FILE-INPUT.
       AUTHOR.       Lee.
       DATE-WRITTEN. 02/10/26.
      
      ***************************************************************
      ***************************************************************
      **   Program Information :-                                  **
      **                                                           **
      **   System   : Batch Ledger System                          **
      **                                                           **
      **   Function.: Allows input of master file details.         **
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
      ** Master.txt : Accounts Master File
      **
                SELECT MASTER-FILE ASSIGN TO "master.txt"
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS SEQUENTIAL
                   RECORD KEY IS MASTER-ACC-NUMBER
                   FILE STATUS IS MASTER-FILE-STATUS.       

      ***************************************************************
      **                      D A T A                              **
      **                  D I V I S I O N                          **
      ***************************************************************
         DATA DIVISION.
         FILE SECTION.
              FD MASTER-FILE
                     LABEL RECORDS ARE STANDARD.
               01 MASTER-RECORD.
                  03 MASTER-ACC-NUMBER PIC 9(5).
                  03 MASTER-ACC-NAME PIC X(20).
                  03 MASTER-ACC-BALANCE PIC 9(5)V99.
                  
      *** 
      *ABOVE SHOULD BE A COPY DD-ALL-FORMATS STATEMENT IN AS/400 WORLD
      *** 

         WORKING-STORAGE SECTION.
              01 MASTER-FILE-STATUS PIC XX.
              01 WS-MASTER-RECORD.
                  03 WS-MASTER-ACC-NUMBER PIC 9(5).
                  03 WS-MASTER-ACC-NAME PIC X(20).
                  03 WS-MASTER-ACC-BALANCE PIC 9(5)V99.
                  
              77  WS-ERROR PIC X(30) VALUE SPACES.

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
            OPEN EXTEND MASTER-FILE.

        B2-MAIN-PROCESS.
            DISPLAY "Enter Account Number (5 digits) or 0 to end: " 
-               WITH NO ADVANCING.            
            ACCEPT WS-MASTER-ACC-NUMBER.
            PERFORM UNTIL WS-MASTER-ACC-NUMBER = 0
                DISPLAY "Enter Account Name: "
-                   WITH NO ADVANCING            
                ACCEPT WS-MASTER-ACC-NAME
                DISPLAY "Enter Account Balance: "
-                   WITH NO ADVANCING           
                ACCEPT WS-MASTER-ACC-BALANCE
                MOVE WS-MASTER-ACC-NUMBER TO MASTER-ACC-NUMBER
                MOVE WS-MASTER-ACC-NAME TO MASTER-ACC-NAME
                MOVE WS-MASTER-ACC-BALANCE TO MASTER-ACC-BALANCE
                PERFORM C1-WRITE-MASTER-RECORD
                DISPLAY "Enter Account Number (5 digits) or 0 to end: "
-                   WITH NO ADVANCING           
                ACCEPT WS-MASTER-ACC-NUMBER
            END-PERFORM.

        B3-ENDING.
            CLOSE MASTER-FILE.

        C1-WRITE-MASTER-RECORD.
            WRITE MASTER-RECORD INVALID KEY
                MOVE "C1- ERROR WRITING TO MASTER FILE " TO WS-ERROR
                PERFORM ZZ-ABORT.

        ZZ-ABORT.
            DISPLAY WS-ERROR
            CLOSE MASTER-FILE
            STOP RUN.

            

