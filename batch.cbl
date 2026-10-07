      ***************************************************************
      **            I D E N T I F I C A T I O N                    **
      **                  D I V I S I O N                          **
      ***************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.   batch.
       AUTHOR.       Lee.
       DATE-WRITTEN. 05/10/26.
      
      ***************************************************************
      ***************************************************************
      **   Program Information :-                                  **
      **                                                           **
      **   System   : Batch Ledger System                          **
      **                                                           **
      **   Function.: Update Account Balances based on the day's.  **
      **.             Transacitons.                                **
      **                                                           **
      **   Invocation: Called from daily batch processes.          **
      **                                                           **
      **   Usage:      Daily                                       **
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
      **
      ** Master.txt : Accounts Master File
      **
           SELECT MASTER-FILE ASSIGN TO "master.txt"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS MASTER-ACC-NUMBER
               FILE STATUS IS MASTER-FILE-STATUS.

      **
      ** Master.txt : AReport File
      **
           SELECT REPORT-FILE ASSIGN TO "report.txt"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS REPORT-FILE-STATUS.

 
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
 
              FD  MASTER-FILE
                   LABEL RECORDS ARE STANDARD.
               01  MASTER-RECORD.
                  03 MASTER-ACC-NUMBER PIC 9(5).
                  03 MASTER-ACC-NAME PIC X(20).
                  03 MASTER-ACC-BALANCE PIC 9(5)V99.

              FD  REPORT-FILE
                   LABEL RECORDS ARE STANDARD.
               01  REPORT-RECORD.
                  03 REPORT-ACC-NUMBER PIC 9(5).
                  03 REPORT-TRANSACT-NUMBER PIC 9(5).
                  03 REPORT-REJECTED-AMOUNT PIC 9(5)V99.
                  03 REPORT-DATE PIC 9(8).

                  
      *** 
      *ABOVE SHOULD BE A COPY DD-ALL-FORMATS STATEMENT IN AS/400 WORLD
      *** 

         WORKING-STORAGE SECTION.
              01  LEDGER-FILE-STATUS PIC X(2) VALUE "00".
                  88 EOF-LEDGER-FILE VALUE "10".
              01  MASTER-FILE-STATUS PIC X(2) VALUE "00".
              01  REPORT-FILE-STATUS PIC X(2) VALUE "00".
                  
              01  WS-MASTER-KEY PIC 9(5).
              01  WS-LEDGER-KEY PIC 9(5).

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
           PERFORM B2-MAIN-PROCESS UNTIL EOF-LEDGER-FILE.
           PERFORM B3-ENDING.

           STOP RUN.

        B1-START.
      ***************************************************************
      **  OPEN FILES AND PERFORM INITIAL READ OF LEDGER FILE       **
      **  IF LEDGER FILE IS EMPTY ABORT PROGRAM WITH ERROR MSG     **
      ***************************************************************
            OPEN INPUT  LEDGER-FILE.
            IF LEDGER-FILE-STATUS NOT = "00"
                MOVE "B1- ERROR OPENING LEDGER FILE " TO WS-ERROR
                PERFORM ZZ-ABORT
            END-IF.

            READ LEDGER-FILE.
            IF LEDGER-FILE-STATUS  = "10"
                MOVE "B1- LEDGER FILE IS EMPTY " TO WS-ERROR
                PERFORM ZZ-ABORT
            END-IF.

            OPEN I-O    MASTER-FILE.
            IF MASTER-FILE-STATUS NOT = "00"
                MOVE "B1- ERROR OPENING MASTER FILE " TO WS-ERROR
                PERFORM ZZ-ABORT
            END-IF.

            OPEN OUTPUT REPORT-FILE.
            IF REPORT-FILE-STATUS NOT = "00"
                MOVE "B1- ERROR OPENING REPORT FILE " TO WS-ERROR
                PERFORM ZZ-ABORT
            END-IF.


        B2-MAIN-PROCESS.
      ***************************************************************
      **  USE ACCOUNT NUMBER OF LEDGER FILE TO READ MASTER FILE    **
      **  IF MASTER FILE RECORD MISSING WE ABORT THE PROGRAM WITH  **
      **  AN ERROR MSG AS SITUATION IS DEEMED A FATAL ERROR THAT   **
      **  TO BE INVESTIGATED.                                      **
      **  IF MASTER FILE RECORD FOUND WE CHECK IF THE TRANSACTION  **
      **  WILL CAUSE AN OVERDRAFT. IF SO WE WRITE TO THE REPORT FILE*
      **  WITHOUT UPDATING THE MASTER FILE RECORD.                 **
      **  IF ACCOUNT WILL REMAIN IN CREDIT WE UPDATE MASTER FILE.  **
      ***************************************************************
            MOVE LEDGER-ACC-NUMBER TO MASTER-ACC-NUMBER.
            READ MASTER-FILE
               INVALID KEY
                   MOVE "B2- ERROR READING MASTER FILE " TO WS-ERROR
                   DISPLAY "ACC NUM" LEDGER-ACC-NUMBER
                   DISPLAY "ACC NUM 2" MASTER-ACC-NUMBER
                   PERFORM ZZ-ABORT
            END-READ.
            IF LEDGER-TRANSACT-TYPE = "D"
                IF MASTER-ACC-BALANCE - LEDGER-TRANSACT-AMOUNT < 0
                    PERFORM C1-WRITE-REPORT-FILE
                ELSE
                    SUBTRACT LEDGER-TRANSACT-AMOUNT 
-                                   FROM MASTER-ACC-BALANCE
                    REWRITE MASTER-RECORD
                       INVALID KEY
                           MOVE "B2- ERROR REWRITING DEBIT MASTER FILE " 
-                                      TO WS-ERROR
                           PERFORM ZZ-ABORT
                    END-REWRITE
                END-IF
            ELSE
                ADD LEDGER-TRANSACT-AMOUNT TO MASTER-ACC-BALANCE
                REWRITE MASTER-RECORD
                   INVALID KEY
                       MOVE "B2- ERROR REWRITING CREDIT MASTER FILE " 
-                                      TO WS-ERROR
                       PERFORM ZZ-ABORT
                END-REWRITE
            END-IF.

            READ LEDGER-FILE.

        B3-ENDING.
            CLOSE LEDGER-FILE MASTER-FILE REPORT-FILE.

        C1-WRITE-REPORT-FILE.
            MOVE LEDGER-ACC-NUMBER TO REPORT-ACC-NUMBER
            MOVE LEDGER-TRANSACT-NUMBER TO REPORT-TRANSACT-NUMBER
            MOVE LEDGER-TRANSACT-AMOUNT TO REPORT-REJECTED-AMOUNT
            MOVE LEDGER-TRANSACT-DATE TO REPORT-DATE
            WRITE REPORT-RECORD.

        ZZ-ABORT.
            DISPLAY WS-ERROR
            CLOSE LEDGER-FILE MASTER-FILE REPORT-FILE
            STOP RUN.

            

