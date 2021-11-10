	LOGICAL FUNCTION FATAL(DUMMY)
C
C ASK PLAYER IF WANTS TO RECONSIDER
C
	IMPLICIT INTEGER(A-Z)
	BYTE YES(2)
	DATA YES/"Y","y"/

	CALL LIB$SET_CURSOR(2,1)
	GOTO (100,200,300,400,500,600) DUMMY
100	CALL STROUT("The troops cannot swim too well, Sir'
	1 Are you sure you want to GOTO sea?",13)
	GOTO 700
200	CALL STROUT("SIR' Those are OUR men' Do you really want to attack
	1 them?",13)
	GOTO 700
300	CALL STROUT("That""s NEVER worked before, Sir'  Are sure you want
	1 to try?",13)
	GOTO 700
400	CALL STROUT("Ships need SEA to float, Sir' Do you really want
	1 go on shore?",13)
	GOTO 700
500	CALL STROUT("That""s OUR city, Sir'  Do you really want to attack
	1 the garrison?",13)
	GOTO 700
600	CALL STROUT("Sorry Sir, there is no room left on the transport.
	1  Do you insist?",13)

700	E=GETCHX()
	CALL LIB$ERASE_LINE(2,1)
	IF (E.EQ.YES(1) .OR. E.EQ.YES(2)) THEN 
		FATAL=.TRUE.
	   ELSE	
		FATAL=.FALSE.
	   ENDIF
	RETURN
	END