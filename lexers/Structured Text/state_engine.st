//------------------------------------------------------------------------------
//
//  State engine
//
//------------------------------------------------------------------------------




IF STATE = eFaulted THEN

    

    IF SEV2 OR SEV3                                 THEN STATE := eRoomFaulted;

        

    ELSIF      RESET_req

       AND NOT (SEV2 OR SEV3)                         THEN STATE := eInit;

    

    END_IF



ELSIF STATE = eInit THEN       


    IF      STARTED_on                              THEN STATE := ePreVent;

    END_IF

    

    

ELSIF STATE = ePreVent THEN

        

    IF      tonPreVent.Q                            THEN STATE := ePreTest;

    END_IF



ELSIF STATE = ePreTest THEN //CHECK TEST COMPLETE

    

    IF       TestComplete                           THEN STATE := eBleedDown;

    END_IF



ELSIF STATE = eBleedDown THEN //CHECK VXX<VBUS 

    

    IF       Vxx < Vbus                             THEN STATE := ePreStandby;

    END_IF


ELSIF STATE = ePreStandby THEN 

       

    IF     stS1.RUN_on

        OR stS2.RUN_on

        OR stS3.RUN_on                              THEN STATE := ePreStartPurge;    

    END_IF

   

ELSIF STATE = ePreStartPurge THEN 

        

    IF       tonPreStartPurge.Q                     THEN STATE := eStandBy;

    END_IF


ELSIF STATE = eStandby THEN    

          

    IF     stS1.STATE = eStarting

        OR stS2.STATE = eStarting

        OR stS3.STATE = eStarting                   THEN STATE := eStarting;  

    END_IF



ELSIF STATE = eStarting THEN 

        

    IF     stS1.STATE = eRunning

        OR stS2.STATE = eRunning

        OR stS3.STATE = eRunning                    THEN STATE := eRunning;

    END_IF


       

END_IF;


// ------------- Starting/Running to ShuttingDown ---------------------------------


IF STATE = eStarting or STATE = eRunning THEN

    

    IF   NOT ((stS1.STATE = eStarting)    OR (stS1.STATE = eRunning))

        AND NOT ((stS2.STATE = eStarting) OR (stS2.STATE = eRunning))

        AND NOT ((stS3.STATE = eStarting) OR (stS3.STATE = eRunning))                    THEN STATE := eShuttingDown;

    END_IF

        

END_IF 


// ------------- ShuttingDown to Standby ------------------------------------------


IF STATE = eShuttingDown THEN

    

    IF     stS1.STATE <= eStandby                 

       AND stS2.STATE <= eStandby 

       AND stS3.STATE <= eStandby                  THEN STATE := eFaulted;

    END_IF

    

 END_IF



// ------------- Standby and lower to Faulted/Init --------------------------------


IF STATE <= eStandby THEN

    

    IF SEV2 OR SEV3                                 THEN STATE := eFaulted;

    

    ELSIF NOT STARTED_on

          AND NOT SEV2

          AND NOT SEV3                              THEN STATE := eInit;

    

    END_IF

        

 END_IF