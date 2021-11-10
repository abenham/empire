C==============================================================================
C ARMJMP
C ------
C For a computer controlled army...
C
C THIS SUBROUTINE DETERMINES WHETHER OR NOT AN ARMY SHOULD GET OFF
C THE TROOP TRANSPORT IT IS ON. 0=NO, 1=YES
C
C Parameters
C -----------------------------------------------------------------------------
C Name		Description
C -----------------------------------------------------------------------------
C Z6		Location of the army and troop transport
C UnitTimerC		Indication of how long army has been on board transport
C==============================================================================
	FUNCTION ARMJMP(Z6,UnitTimerC)
C 
	INCLUDE "EMPIRE.INC/NOLIST"
C
C Default is stay on board
C
	ARMJMP=0
C
C Are we surrounded by sea? If we are, stay on board and do no more
C 
	DO 100 I=1,8
100	IF (MasterMap(Z6+MoveOffset(I+1)).NE.".") GOTO 200	'** NOT ALL SEA SURROUND
	RETURN
C 
C Non-sea location is adjacent... decided what to do
C
C If army has been on board for a long time, get off to relieve the boredom
C
200	IF (UnitTimerC.EQ.0) GOTO 400		'BEEN ON TROOP TRANSPORT FOR A LONG TIME

C
C Examine each adjacent location again
C
	DO 300 I=1,8
	LOC=Z6+MoveOffset(I+1)			'**

C
C Ignore more sea
C
	IF (MasterMap(LOC).EQ.".") GOTO 300

C
C If location is off the map, ignore it
C
	IF (ORDER(LOC).NE.0) GOTO 300

C
C What is at this location?
C
	AB=ReferenceMap(LOC)

C
C Player army or fighter... disembark and attack
C
	IF ((AB.EQ."A").OR.(AB.EQ."F")) GOTO 400

C
C If neutral city or player city... disembark and attack
C
	IF ((AB.EQ."*").OR.(AB.EQ."O")) GOTO 400

C
C Look further out...
C
	LOC=Z6+2*MoveOffset(I+1)			'**

C
C Get the terrain at this further location
C
	AB=EMAP(LOC)

C
C If it is blank (unexplored) disembark to explore
C
	IF (AB.EQ." ") GOTO 400

C
C Next direction
C
300	CONTINUE

C
C No action identified, so remain on board
C
	RETURN					'DON"T JUMP

C
C Action identified, so disembark
C
400	ARMJMP=1
	RETURN					'JUMP
	END