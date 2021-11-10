C==============================================================================
C PRIORI
C ------
C For the computer controlled armies, returns a direction in which to move if
C a priority move exists for the army
C
C Parameters
C -----------------------------------------------------------------------------
C Z6	Current location
C IFO	Current assigned action
C ILA	Current assigned target location
C DIR	+/- 1
C AC	Terrain at location Z6
C -----------------------------------------------------------------------------
C
C Returns Direction value.
C==============================================================================
	FUNCTION PRIORI(Z6,IFO,ILA,DIR,AC)
C
	INCLUDE "EMPIRE.INC/NOLIST"
	BYTE GROUND,OK

C-------------------------------------------------------------------------------
C Initialise the priorities array
C ECityLocationPMACityLocation is the highest number of unexplored locations seen from a proposed new
C location
C-------------------------------------------------------------------------------
	DO 100 I=1,7
100	PRIOR(I)=0

	ECityLocationPMACityLocation=0

C-------------------------------------------------------------------------------
C NOW MAKE A GUESS AS TO WHAT THE MOVE WILL BE
C
C Get the current target location into MOVE1 (This actually makes no sense as
C ILA contains a location, while MOVE1 will contain an index value for the
C MoveOffset array).
C
C If the assigned action is "move towards city" or "move towards army", work out 
C the direction to move on the path to that destination
C-------------------------------------------------------------------------------
	MOVE1=ILA
	IF (IFO.EQ.1.OR.IFO.EQ.2) MOVE1=MOV(Z6,ILA)

C-------------------------------------------------------------------------------
C If "move to transport", work out the next direction to move on the path to the 
C transport (which might be moving)
C-------------------------------------------------------------------------------
	IF (IFO.EQ.3) MOVE1=MOV(Z6,UnitLocation(CTransportStart+ILA))

C-------------------------------------------------------------------------------
C NOW SEE IF ANY PRIORITY MOVES ECityLocationIST
C
C Look in the eight different directions. Sequence determined by DIR which is +1
C or -1 and was calculated in ARMYEN from the army number
C-------------------------------------------------------------------------------
	DO 200 I=0,7*DIR,DIR

C-------------------------------------------------------------------------------
C MOVE is an index between 1 and 8 into the MoveOffset array. 
C The ICORR function ensures that the value is within allowable range to access
C the MoveOffset array.
C ORDER function checks if the specified location is off the edge of the map. 
C
C Convert MOVE1 to a valid value
C Determine the location of adjacent point in the determined direction
C If the location is off the map, try another direction
C-------------------------------------------------------------------------------
	MOVE=ICORR(MOVE1+I)
	LOC=Z6+MoveOffset(MOVE+1)			'**
	IF (ORDER(LOC).NE.0) GOTO 200

C-------------------------------------------------------------------------------
C Location was on the map, get the terrain at this location as last seen by the
C computer.
C-------------------------------------------------------------------------------
	AB=ReferenceMap(LOC)

C-------------------------------------------------------------------------------
C CHECK IF ARMY CAN ATTACK SOMETHING OVER WATER
C
C Get the underlying terrain at this location (So the computer map might show an
C enemy unit at this location, but the terrain beneath it is either land or sea)
C
C Set the OK flag. This is used to indicate whether the army will be moving into
C water after completing its action... for example after attacking a ship. When
C set to "Y" the army will be feet dry after completing the action.
C
C If the army's current terrain is a troop transport (ie we are embarked), and
C the underlying terrain in the adjacent location is sea, clear the OK flag.
C This will stop armies attacking player ships by moving off their troop 
C transport. However, armies currently on land or in a city can attack ships.
C-------------------------------------------------------------------------------
	GROUND=MasterMap(LOC)
	OK="Y"
	IF ((AC.EQ."t").AND.(GROUND.EQ.".")) OK="N"

C-------------------------------------------------------------------------------
C The PRIOR array contains directions to move. The array element used determines
C the importance of that move.
C
C	1	Attack player city
C	2	Attack neutral city
C	3	Attack player troop transport
C	4	Explore
C	5	Attack player army
C	6	Attack player submarine
C	7	Attack other player unit
C-------------------------------------------------------------------------------

C	Attack player city
	IF (AB.EQ."O") PRIOR(1)=MOVE

C	Attack player troop transport if not embarked
	IF ((AB.EQ."T").AND.(OK.EQ."Y")) PRIOR(3)=MOVE

C	Attack neutral city
	IF (AB.EQ."*") PRIOR(2)=MOVE

C	Attack player army
	IF (AB.EQ."A") PRIOR(5)=MOVE

C	Attack player submarine if not embarked
	IF ((AB.EQ."S").AND.(OK.EQ."Y")) PRIOR(6)=MOVE

C	If no assigned function, and not embarked, attack any other player unit
	IF ((IFO.EQ.0).AND.(AB.GE."A").AND.(AB.LE."T").AND.(OK.EQ."Y"))
	1	 PRIOR(7)=MOVE

C-------------------------------------------------------------------------------
C If adjacent underlying terrain is not land, go
C-------------------------------------------------------------------------------
	IF (GROUND.NE."+") GOTO 200

C-------------------------------------------------------------------------------
C Look for unexplored terrain around the current destination
C-------------------------------------------------------------------------------
	N=0
	IF (EMAP(LOC+MoveOffset(ICORR(MOVE-2)+1)).EQ." ") N=1	'**
	IF (EMAP(LOC+MoveOffset(ICORR(MOVE-1)+1)).EQ." ") N=N+1	'**
	IF (EMAP(LOC+MoveOffset(MOVE+1)).EQ." ") N=N+1		'**
	IF (EMAP(LOC+MoveOffset(ICORR(MOVE+1)+1)).EQ." ") N=N+1	'**
	IF (EMAP(LOC+MoveOffset(ICORR(MOVE+2)+1)).EQ." ") N=N+1	'**
C	TYPE 999,N,ECityLocationPMACityLocation
C999	FORMAT(" N:",I2," ECityLocationPMACityLocation:",I2)

C-------------------------------------------------------------------------------
C If number of unexplored locations is not greater than our previous maximum, go
C-------------------------------------------------------------------------------
	IF (N.LE.ECityLocationPMACityLocation) GOTO 200

C-------------------------------------------------------------------------------
C This location has the most unexplored terrain so far, assign this direction
C to the explore priority.
C
C Save the new maximum
C-------------------------------------------------------------------------------
	PRIOR(4)=MOVE
	ECityLocationPMACityLocation=N
200	CONTINUE

C	TYPE 998
C998	FORMAT(" CityLocationCityLocationCityLocationCityLocationCityLocationCityLocationCityLocationCityLocationCityLocationCityLocationCityLocationCityLocationCityLocationCityLocationCityLocationCityLocation")

C-------------------------------------------------------------------------------
C NOW SELECT THE HIGHEST PRIORITY MOVE
C
C Step through the priority array in order of importance. The first one found
C with a direction is the one we will take. Exit the loop and return that 
C direction.
C
C If we drop out of the loop without finding any priority move, return zero.
C-------------------------------------------------------------------------------
	DO 300 I=1,7
300	IF (PRIOR(I).NE.0) GOTO 400
	PRIORI=0
	RETURN

400	PRIORI=PRIOR(I)
	RETURN
	END