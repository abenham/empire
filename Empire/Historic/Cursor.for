C
C CURSOR POSITIONING AND OUTPUT SUBROUTINES
C
	SUBROUTINE CURSOR(N)
C
C POSITION CURSOR TO MAP LOCATION N
C
	INCLUDE "EMPIRE.INC/NOLIST"
	IF (GIGI) THEN
	 CALL LIB$GSET_CURSOR(N/100+1,MOD(N,100)+1)
	ELSE
	 CALL LIB$SET_CURSOR(N/100+1,MOD(N,100)+1)
	ENDIF
	END