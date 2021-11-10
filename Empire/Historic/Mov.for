C==============================================================================
C MOV
C ------
C Determines a direction to move on the path between two locations
C
C Parameters
C -----------------------------------------------------------------------------
C I6	start location
C I7	destination
C -----------------------------------------------------------------------------
C
C Returns Direction value.
C==============================================================================
	FUNCTION MOV(I6,I7)
C-------------------------------------------------------------------------------
C RETURNS THE INDEX-1 INTO MoveOffset FOR THE DIRECTION  OF THE MOVE
C   FROM I6 TO I7
C-------------------------------------------------------------------------------
	INCLUDE "EMPIRE.INC/NOLIST"
	LOGICAL CityLocationMAJOR
C
	IY6=(I6-1)/100
	IY7=(I7-1)/100
	ICityLocation6=I6-(100*IY6)
	ICityLocation7=I7-(100*IY7)
	IY=IY7-IY6
	ICityLocation=ICityLocation7-ICityLocation6

C-------------------------------------------------------------------------------
C  SCREEN OUT TRIVIAL CASES
C-------------------------------------------------------------------------------
	IF (ICityLocation.EQ.0) THEN
	   DIR=SIGN(100,IY)
	   GOTO 100
	   ENDIF
	IF (IY.EQ.0) THEN
	   DIR=SIGN(1,ICityLocation)
	   GOTO 100
	   ENDIF

C-------------------------------------------------------------------------------
C  THIS ATTEMPTS A LINE-OF-SIGHT APPROCityLocationIMATION
C   unfortunately a true LOS requires knowing where you came from'
C   this routine currently tries to keep near a 3 to 1 ratio.
C-------------------------------------------------------------------------------
	DCityLocation=ABS(ICityLocation)			'GET DELTA CityLocation
	DY=ABS(IY)			'GET DELTA Y
	CityLocationMAJOR=.TRUE.			'ASSUME CityLocation IS MAJOR CHANGE
	IF (DY.GT.DCityLocation) THEN		' IF WRONG, SWITCH
	   DCityLocation=DY
	   DY=ABS(ICityLocation)
	   CityLocationMAJOR=.FALSE.
	   ENDIF	

C-------------------------------------------------------------------------------
C					' the divisor determines the slope
C					' perfect case would be delta y at start
C-------------------------------------------------------------------------------
	IF (IFICityLocation(FLOAT(DCityLocation)/3+.5).GT.DY) THEN	'IF MAJOR IS LONG, GO STRAIGHT
	     IF (CityLocationMAJOR) THEN
		DIR=SIGN(1,ICityLocation)
	     ELSE
		DIR=SIGN(100,IY)
	     ENDIF
	   ELSE					'OTHERWISE, TAKE DIAGONAL
		DIR=SIGN(100,IY)+SIGN(1,ICityLocation)
	   ENDIF
100	DO 200 I=1,9				'FIND THE INDEX
200	IF (MoveOffset(I).EQ.DIR) GOTO 300
300	MOV=I-1					'FOR COMPATIBILITY (?)


C-------------------------------------------------------------------------------
C OLD WAY: FOR HISTORIANS
C	THIS DOES NOT DO A "TRUE" LINE OF SIGHT, FAVORS DIAGONALS
C	IF ((IY.LT.0).AND.(ICityLocation.GT.0)) MOV=2
C	IF ((IY.LT.0).AND.(ICityLocation.EQ.0)) MOV=3
C	IF ((IY.LT.0).AND.(ICityLocation.LT.0)) MOV=4
C	IF ((IY.EQ.0).AND.(ICityLocation.LT.0)) MOV=5
C	IF ((IY.GT.0).AND.(ICityLocation.LT.0)) MOV=6
C	IF ((IY.GT.0).AND.(ICityLocation.EQ.0)) MOV=7
C	IF ((IY.GT.0).AND.(ICityLocation.GT.0)) MOV=8
C	IF ((IY.EQ.0).AND.(ICityLocation.GT.0)) MOV=1
C	IF ((ICityLocation.EQ.0).AND.(IY.EQ.0)) MOV=0
C-------------------------------------------------------------------------------

	RETURN
	END