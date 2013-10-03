
         IDENTIFICATION    DIVISION.
         PROGRAM-ID.       TESTPG01.
         ENVIRONMENT       DIVISION.
         INPUT-OUTPUT      SECTION.
         FILE-CONTROL.
                 SELECT INPUT-FILE  ASSIGN TO FILE1
                 ORGANIZATION IS  SEQUENTIAL
                 ACCESS MODE IS   SEQUENTIAL
                FILE STATUS  IS    S-A
         DATA              DIVISION.
         FILE              SECTION. 
         FD  INPUT-FILE.
         01 RECB.
                05   FACCOUNT-ID		 PIC X(20).	
                05   FACCOUNT-TYPE	 PIC X(1).
                05   FACCOUNT-KBN		 PIC X(1).
                05   FCRE-DATE			 PIC X(8)
                05   FACCESS-TYPE		 PIC X(1). 
                05   FACCESS-DATE    PIC X(8)
                05   FAMOUNT     		 PIC X(12).
                05   BLANK-BIT       PIC  X(29).        
         WORKING-STORAGE   SECTION.
         01 WK-AREA.
              05 WK-DATE   PIC  X(8).
              05 WK-TIME   PIC  X(6).
              05 S-A  PIC  99.
              05              
         01 HOST-AREA .
              05 TACCOUNT.                                                 
                10 HOST-ACCOUNT         PIC X(20).                   
                10 HOST-CRE-DATE        PIC X(10).                   
                10  HOST-ACC-TYPE1       PIC X(1).                    
                10 HOST-AMOUNT          PIC S9(10)V9(2) USAGE COMP-3.  
                10 HOST-ACCRUAL         PIC S9(10)V9(2) USAGE COMP-3.  
                10 HOST-IN-DATE         PIC X(20).                   
                10 HOST-UP-DATE         PIC X(20).  
            05 TRETA.                 
               10 HOST-ACC-TYPE2       PIC X(1).                     
                10 HOST-RATE            PIC S9(1)V9(3) USAGE COMP-3.
               10 HOST-IN-DATE2        PIC X(20).                   
               10 HOST-UP-DATE2        PIC X(20).
             05 THACCOUNT.
               10 HOST-ACCOUNTH     PIC X(20).
               10 HOST-CRE-DATEH    PIC X(10).
               10 HOST-ACC-TYPE     PIC X.
               10 HOST-ACCESS-TYPE   PIC X.
                10 HOST-AMOUNTH     PIC S9(13)V9(2) COMP-3.
                10 HOST-REM-AMOUNTH  PIC S9(13)V9(2) COMP-3.
                10 HOST-UP-DATEH     PIC X(20).                 
          EXEC SQL END DECLARE SECTION  END-EXEC .      
         PROCEDURE         DIVISION.
         MAIN-RTN              SECTION.
                 PERFORM INI-RTN 
        *************************************************
                 PERFORM READ-RTN 
        *************************************************
                 STOP RUN.
        *************************************************
         INI-RTN              SECTION.
                 INITIALIZE  HOST-AREA WK-AREA
                 EXIT.
         READ-RTN            SECTION.
         READ-A. 
                    OPEN INPUT FILE1 
                     PERFORM READ-B UNTIL S-A =  10 
                     EXIT.
         READ-B.                  
                           READ INPUTFILE     
                           PERFORM CHECK-FILE-RTN 
                           PERFORM READ-C
                           EXIT.
          READ-C.
        * *********************OLD/NEW ACCOUNT******************************************                  
                     IF  ACCOUNT-KBN = 1 
                     PERFORM READ-NEW .
                     IF  ACCOUNT-KBN = 2
                     PERFORM READ-OLD .
                     EXIT.
          READ-NEW.
         ** ***********IM A NEW ONE ********************************************************
                     MOVE  FACCOUNT  TO  HOST-ACCOUNT
                     MOVE  FCRE-DATE TO HOST-CRE-DATE
                     MOVE  FACC-TYPE1 TO HOST-ACC-TYPE1
                     MOVE  FAMOUNT    TO HOST-AMOUNT
                     MOVE  0  TO   HOST-ACCRUAL
                     PERFORM READ-TIME
                     EXIT.
         *****************JISUAN YUE **************************************************                       
          READ-OLD. 
                     MOVE FACCOUNT TO HOST-ACCOUNT
                     EXEC SQL SELECT AMOUNT FROM TACCOUNT 
                      INTO :HOST-AMOUNT
                      WHERE ACCOUNT = :HOST-ACCOUNT END-EXEC 
                      IF  FACCESS-TYPE = 1
                     COMPUTE HOST-AMOUNT = FAMOUNT + HOST-AMOUNT 
                        END-IF
                     IF FACCESS-TYPE = 2
                        IF  FAMOUNT > HOST-ACCOUNT  
                        DISPLAY 'NO MAONEY'  
                   ELSE   COMPUTE HOST-AMOUNT = HOST-AMOUNT - FAMOUNT
                       END-IF 
                       END-IF
                      PERFORM READ-TIME.
                      EXIT.
          READ-TIME.
                     ACCEPT DATE INTO WK-DATE YYYYMMDD
                     ACCEPT TIME INTO WK-TIME
                     IF FACCESS-TYPE = 1
                     STRING WK-DATE(1:4), '/', WK-DATE(5:2), '/',
                            WK-DATE(7:2), '-'
                            WK-TIME(1:2), ':', WK-TIME(3:2), ':',
                            WK-TIME(5:6), '/'  DELIMITED BY SIZE INTO 
                            HOST-IN-DATE 
                      MOVE  SPACES TO HOST-UP-DATE      
                            END-IF
                     IF FACESS-TYPE = 2
                     STRING WK-DATE(1:4), '/', WK-DATE(5:2), '/',
                            WK-DATE(7:2), '-'
                            WK-TIME(1:2), ':', WK-TIME(3:2), ':',
                            WK-TIME(5:6), '/'  DELIMITED BY SIZE INTO 
                            HOST-UP-DATE
                            END-IF
                       PERFORM READ-UP-TACCOUNT
                       EXIT.
          READ-UP-TACCOUNT.
                   IF FACCESS-TYPE = 1
                    EXEC SQL INSERT INTO TACCOUNT 
                   VALUES(:HOST-ACCOUNT,:HOST-CRD-DATE,:HOST-AMOUNT,
                         :HOST-ACRUAL,:HOST-IN-DATE,:HOST-UP-DATE)
                       END-EXEC 
                       END-IF
                   IF  FACCESS-TYPE = 2
                    EXEC SQL UPDATE TACCOUNT SET 
                   (AMOUNT,UP_DATE) = ( HOST-AMOUNT,:HOST-UP-DATE)
                    WHERE ACCOUNT = :HSOT-ACCOUNT
                    END-EXEC    
                  PERFORM  READ-UP-THACCOUNT.
                  EXIT.
           READ-UP-THACCOUNT.
                  IF FACCESS-TYPE = 1               
                  MOVE  FACCOUNT TO HOST-ACCOUNTH
                  MOVE  FCRE-DATE TO HOST-CRE-DATEH
                  MOVE  FACC-TYPE TO HOST-ACC-TYPEH
                  MOVE  FACCESS-TYPE TO HOST-ACCESS-TYPEH
                  MOVE  FACCESS-DATE TO HOST-ACCESS-DATEH
                  MOVE  FAMOUNT   TO HOST-AMOUNTH
                  MOVE  FAMOUNT   TO HOST-REM-AMOUNTH          
                  STRING  WK-DATE(1:4), '/', WK-DATE(5:2), '/',
                          WK-DATE(7:2) INTO HOST-UP-DATEH    
                  END-IF
                  IF FACCESS-TYPE = 2
                  MOVE  FACCOUNT TO HOST-ACCOUNTH
                  MOVE  FCRE-DATE TO HOST-CRE-DATEH
                  MOVE  FACCESS-TYPE TO HOST-ACCESS-TYPEH
                  MOVE  FACCESS-DATE TO HOST-ACCESS-DATEH
                  MOVE  FAMOUNT   TO HOST-AMOUNTH
                  MOVE  HOST-AMOUNT  TO HOST-REM-AMOUNTH
                  MOVE  HOST-UP-DATE TO HOST-UP-DATEH
                  END-IF
                  EXEC SQL  INSERT INTO THACCOUNT VALUES
                  (:HOST-ACCOUNTH,:HOST-CRE-DATEH,:HOST-ACC-TYPE,
                   :HOST-ACC-DATEH,:HOST-AMOUNTH,:HOST-REM-AMOUNTH,
                   :HOST-UP-DATEH)  END-EXEC
                   PERFORM CHECK-SQLCODE-RTN
                   EXIT.
       *****************************************************************
        CHCK-SQLCODE-RTN       SECTION. 
                 IF SQLCODE =  0 
                   CONTINUE 
                   ELSE  DISPLAY 'SQLCODE ERROR' 
                   EXEC SQL ROLLBACK'  END-EXEC
                   MOVE 'S' TO MSG
                   PERFORM ERROR-RTN
                   END-IF.
                   EXIT.
        ERROR-RTN            SECTION.
                   EVALUATE MSG
                   WHEN             
                                   
                   
                         
                            
                                            
                            
                                         
                       
                      
                               				
  
         
         
