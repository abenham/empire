C==============================================================================
C PATH
C ----
C Determines a path to get from one point to another
C
C PATH SUBROUTINE FOR EMPIRE
C  FINDS DIRECTION TO MOVE UNIT, FROM BEG TO END, OKVECT SPECIFIES OK TERRAIN.
C
C Parameters
C -----------------------------------------------------------------------------
C BEG		Start location
C END		Destination
C DIR		Initial direction to try
C OKVECT	Array of permitted terrain types to include in planning the path
C FLAG		Not sure yet
C -----------------------------------------------------------------------------
C
C Returns Direction value.
C==============================================================================
	FUNCTION PATH(BEG,END,DIR,OKVECT,FLAG)
C
	INCLUDE "EMPIRE.INC/NOLIST"
	BYTE	OKVECT(5)

C------------------------------------------------------------------------------
C Pick an initial direction
C------------------------------------------------------------------------------
	BACKUP=1
	TDIR=DIR						' GET A DIRECTION TO FIDDLE WITH
	DIR3=TDIR*3
	Z6=BEG

C------------------------------------------------------------------------------
C Set the maximum distance to be three times the straight line distance
C Code works by reducing MOVNUM from initial value of MACityLocationMVE until either the
C destination is reached, or MOVNUM reaches zero. When the latter happens the
C path is deemed too long.
C------------------------------------------------------------------------------
	MACityLocationMVE=(3 * IDIST(BEG,END))+1	' COMPUTE MACityLocation MOVES TO GET THERE
	MOVNUM=MACityLocationMVE

C------------------------------------------------------------------------------
C Clear array used to keep track of path so far
C------------------------------------------------------------------------------
100	DO 200 I=1,100					' CLEAR G2 ARRAY
	G2(I)=0
200	CONTINUE

C------------------------------------------------------------------------------
C STRGHT:							' TRY STRAIGHT MOVE FIRST
C
C Get the direction for a straight path to the destination.
C Get the terrain on the next step - Z62 is the location, AB the terrain
C If the terrain is not in the list of permitted terrain go...
C------------------------------------------------------------------------------
300	MOOVE= MOV(Z6,END)
	Z62=Z6+MoveOffset(MOOVE+1)
	AB=EMAP(Z62)
	IF (COMPAR(AB,Z62,OKVECT).EQ.0) GOTO 900  'IF NO GOOD, FOLLOW SHORE

C------------------------------------------------------------------------------
C OKSET:			' STRAGHT MOVE WORKING
C------------------------------------------------------------------------------
400	BAKADR=1

C------------------------------------------------------------------------------
C OKMOVE:
C
C If we've backtracked back to the starting point, restore the original 
C direction.
C Get the current location on the path
C TEST4 is for debgging, so are FLAG values >= 1000
C If we have reached the end location, go...
C------------------------------------------------------------------------------
500	IF (Z6 .EQ. BEG) MOVE1=MOOVE
	Z6=Z62
	IF (FLAG.GE.1000) CALL TEST4(Z6,FLAG,TDIR,MOVE1,MOVNUM,BEG,END,G2,BAKADR)
	IF (Z6 .EQ. END) GOTO 800 	' IF Z6=END, WE"RE DONE

C------------------------------------------------------------------------------
C DOMORE:
C
C Decrement the number of permitted moves.
C If the number of moves has been exhausted, try a revised path
C Otherwise
C------------------------------------------------------------------------------
600	MOVNUM=MOVNUM-1
	IF (MOVNUM .EQ. 0) GOTO 700	' REACHED MACityLocation MOVES, TRY NEW DIRECTION

C------------------------------------------------------------------------------
C		STRGHT,	CHKNCityLocationT
C
C BAKADR indicates what to try next. When 1 continue on straight path, when 2
C try something else
C------------------------------------------------------------------------------
	GOTO	(300,	1300), BAKADR	' CONTINUE, IN SAME MANNER

C------------------------------------------------------------------------------
C TRYDIR::
C
C Path was too long.
C Reverse direction
C If we are back to the original direction we have tried all directions from
C this point
C
C Reset the maximum number of moves
C Reset the starting position in the G2 array (used when clearing the array)
C Set location back to start of path
C Start over
C------------------------------------------------------------------------------
700	DIR3=-DIR3						' NEGATE CURRENT DIRECTION
	TDIR=-TDIR
	IF (TDIR .EQ. DIR) GOTO  1200	' GIVE UP IF BACK TO START
	MOVNUM=MACityLocationMVE					' ELSE, TRY AGAIN
	BACKUP=1
	Z6=BEG
	GOTO 100

C------------------------------------------------------------------------------
C SUCCES:				SUCCESS, RETURN
C
C We have reached the destination, set the return value to be the direction
C chosen (PATH)
C SUCCES appears to be a debugging value
C FLAG is referenced by the caller
C------------------------------------------------------------------------------
800	PATH=MOVE1
	SUCCES=SUCCES+1
	FLAG=1
	RETURN

C------------------------------------------------------------------------------
C FOLSHR:				FOLLOW THE SHORE
C
C Try a new direction
C Get the new location
C Get what we can see on the computer map
C If terrain is acceptable, use original move?
C------------------------------------------------------------------------------
900	MOV1=ICORR(MOOVE-DIR3)		' TRY AGAIN
	Z62=Z6+MoveOffset(MOV1+1)
	AB=EMAP(Z62)
	IF (COMPAR(AB,Z62,OKVECT).EQ.1) MOV1=MOOVE	' ???

C------------------------------------------------------------------------------
C STFOL:
C
C There is no GOTO 1000 anywhere, so this is simply part of following the 
C shoreline
C
C Try each of the other 7 directions
C Get the new location
C Check new location is on the map
C Get what we can see on the computer map
C If terrain is unacceptable, try next direction
C------------------------------------------------------------------------------
1000	DO 1100 IVAR= MOV1,MOV1+7*TDIR,TDIR
	MOOVE=ICORR(IVAR)
	Z62=Z6+MoveOffset(MOOVE+1)
	IF (ORDER(Z62) .NE. 0) GOTO 1100
	AB=EMAP(Z62)
	IF (COMPAR(AB,Z62,OKVECT).EQ.0) GOTO 1100

C------------------------------------------------------------------------------
C OKSET2:
C
C Terrain was acceptable, set BAKADR=2 indicating that we're not on a straight
C line path
C Go to 500 to test this move further
C------------------------------------------------------------------------------
	BAKADR=2
	GOTO 500
1100	CONTINUE

C------------------------------------------------------------------------------
C FAILUR:
C
C Couldn't find a path.
C
C Set the direction based on the straight line path.
C FAILUR is a debugging value
C FLAG is used by the caller
C
C------------------------------------------------------------------------------
1200	PATH=MOV(BEG,END)
	FAILUR=FAILUR+1
	FLAG=0
	RETURN

C------------------------------------------------------------------------------
C CHKNCityLocationT:
C
C Path is good so far
C 
C Get the direction to move from this location to the end
C Get the next location
C Get the visible terrain at that location
C If terrain is no good, try following the shore
C Look through the G2 array of locations along the path...
C If this location is already in there, try following the shore
C Otherwise, save this location
C Increment the number of points along the path
C If we've used less than 100 steps so far, continue in a straight line
C Otherwise, backup along the path and try a new approach
C------------------------------------------------------------------------------
1300	T1=MOV(Z6,END)
	Z62=Z6+MoveOffset(T1+1)
	AB=EMAP(Z62)
	IF (COMPAR(AB,Z62,OKVECT).EQ.0) GOTO 900

	DO 1400 IVAR=BACKUP,1,-1
	IF (Z6 .EQ. G2(IVAR)) GOTO 900
1400	CONTINUE

	G2(BACKUP)=Z6
	BACKUP=BACKUP+1
	IF (BACKUP .LE. 100) GOTO 300
	GOTO 700
C
	END