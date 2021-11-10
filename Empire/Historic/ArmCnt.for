C==============================================================================
C ARMCNT
C ------
C For the computer controlled armies, counts the number of units assigned to 
C attack a city.
C==============================================================================
	SUBROUTINE ARMCNT
C
	INCLUDE "EMPIRE.INC/NOLIST"
C
C Initialise array - LIMIT appears to be a count of each unit type. Each array
C element is the count for a particular type. 9 is computer controlled army.
C
	DO 100 I=1,20
100	ARMTOT(I)=0

C
C For each computer army...
C
	DO 300 I=1,UpperIndex(9)
C
C UnitAction is an action assigned to a unit... this from ARMYEN...
C
C	0: MOVE IN CERTAIN DIRECTION, OR FOLLOW SHORE
C	1: MOVE TOWARDS TargetCity CITY
C	2: MOVE TOWARDS AN ENEMY ARMY
C	3: MOVE TOWARDS A TROOP TRANSPORT
C
C So... we're only looking for armies that are moving against a city.
C
	IF (UnitAction(I).NE.1) GOTO 300
C
C UnitActionTarget is a target location for the army
C TargetCity is an array of targets... up to 20 it would appear
C ARMTOT is the number of armies assigned to that target
C
C Get the target location for this army
C Find the target location in the target list and increment the
C number of armies assigned to the target
C
	ILA=UnitActionTarget(I)
	DO 200 I2=1,20
200	IF (TargetCity(I2).EQ.ILA) ARMTOT(I2)=ARMTOT(I2)+1
300	CONTINUE
	RETURN
	END