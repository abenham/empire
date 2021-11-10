C==============================================================================
C ARMYEN
C ------
C For a computer controlled army...
C
C THIS SUBROUTINE HANDLES ENEMY ARMY MOVES
C
C Parameters
C -----------------------------------------------------------------------------
C Name		Description
C -----------------------------------------------------------------------------
C None
C==============================================================================
	SUBROUTINE ARMYEN
C 
	INCLUDE "EMPIRE.INC/NOLIST"

	integer*4	Aboard			' Was NUMARM
	integer*4	Action			' Was IFO
	integer*4	ActionTarget		' Was ILA
	integer*4	AdjacentLocation	' Was LOC
	integer*4	ArmyID			' Was Y
	integer*4	Closest
	integer*4	CurrentArmy		' was MONKEY
	integer*4	Distance		' Was ID
	integer*4	Dither			' Was DIR
	integer*4	Reachable		' Was FLAG
	integer*4	Hits1			' Was H1
	integer*4	Hits2			' Was H2
	integer*4	I
	integer*4	J
	integer*4	I1
	integer*4	IStart			' Was IA
	integer*4	IEnd			' Was IB
	integer*4	Index			' Was IC
	integer*4	Location		' was global Z6
	integer*4	Move			' Was MOVE
	integer*4	PathCount		' was NPATH
	integer*4	PlayerTarget		' Was P
	integer*4	PriorityMove		' Was MOVE1
	integer*4	SavedLocation		' Was Z7
	integer*4	Temp			' Was TEMP
	integer*4	TransportID		' Was IZ
	integer*4	Z8

	byte		AdjacentTerrain		' was AC
	byte		Combatant1		' Was OWN1
	byte		Combatant2		' Was OWN2
	byte		Terrain			' was AB

C-------------------------------------------------------------------------------
C CurrentArmy is used to save the current army number when looking at other 
C armies
C Initialise number of armies
C-------------------------------------------------------------------------------
	CurrentArmy=0
	EnemyCount(1)=0

C-------------------------------------------------------------------------------
C START ARMY Move LOOP
C
C For each computer-controlled army...
C-------------------------------------------------------------------------------
	DO 4200 ArmyID=1,UpperIndex(9)

C-------------------------------------------------------------------------------
C CArmyStart is a constant. It is an index offset into the UnitLocation array 
C for the computer controlled armies.
C UnitLocation contains the location of each and every unit
C
C Get the location of this army
C-------------------------------------------------------------------------------
	Location=UnitLocation(CArmyStart+ArmyID)

C-------------------------------------------------------------------------------
C If the unit has no location it either hasn't been created or it has been 
C destroyed so go to the next unit
C-------------------------------------------------------------------------------
	IF (Location.EQ.0) GOTO 4200

C-------------------------------------------------------------------------------
C Debugging code. Display unit Distance and number of path attempts
C-------------------------------------------------------------------------------
	IF (DebugLevel.NE.1) GOTO 200
	CALL LIB$SET_CURSOR(1,50)
	CALL DECPRT(ArmyID)
	CALL LIB$SET_CURSOR(1,60)
	CALL DECPRT(PathCount)
	PathCount=0

C-------------------------------------------------------------------------------
C Save this army's location and identity
C-------------------------------------------------------------------------------
200	SavedLocation=Location
	CurrentArmy=ArmyID

C-------------------------------------------------------------------------------
C Dither is used in the PRIORI function to determine the order in which
C priority moves are determined. All even ArmyID values produce a value of
C -1, while all odd values yield +1
C
C Not sure why this is done, maybe to introduce the appearance of
C randomness when assigning a priority move
C
C PlayerTarget is used when attacking a player owned unit, when this happens the
C player is shown what is happening
C-------------------------------------------------------------------------------
	Dither=MOD(ArmyID,2)*2-1		'SET Dither TO 1 OR -1
	PlayerTarget=0

C-------------------------------------------------------------------------------
C What is at the army's location?
C-------------------------------------------------------------------------------
	Terrain=ReferenceMap(Location)		' WHAT IS SHOWING WHERE THE ARMY IS
	AdjacentTerrain=0

C-------------------------------------------------------------------------------
C If the army is not out in the open "a", or in a transport "t", or in a city 
C "X", go!
C-------------------------------------------------------------------------------
	IF ((Terrain.NE."a").AND.(Terrain.NE."t").AND.(Terrain.NE."X")) GOTO 3700

C-------------------------------------------------------------------------------
C The army is in the open, on a transport, or in a city...
C 
C AGE UnitTimer
C Reduce the timer UnitTimer that indicates how long this army has been "idle".
C Seems to be two timers in operation, one that counts down from 100 and another 
C to 1000
C
C If either timer limit is still in operation, decrement the timer
C-------------------------------------------------------------------------------
	IF ((UnitTimer(ArmyID).LE.100).OR.(UnitTimer(ArmyID).GT.1000)) UnitTimer(ArmyID)=UnitTimer(ArmyID)-1

C-------------------------------------------------------------------------------
C If we reach either timer limit, set the timer to zero
C-------------------------------------------------------------------------------
	IF ((UnitTimer(ArmyID).LT.0).OR.(UnitTimer(ArmyID).EQ.1000)) UnitTimer(ArmyID)=0

C-------------------------------------------------------------------------------
C If the army is out in the open... go
C-------------------------------------------------------------------------------
	IF (Terrain.EQ."a") GOTO 300

C-------------------------------------------------------------------------------
C The army is in a city...
C-------------------------------------------------------------------------------
	IF (Terrain.EQ."X") THEN

C-------------------------------------------------------------------------------
C Is the army in a troop transport that is docked in the city?
C
C Look for a troop transport that is in the same location as the army
C-------------------------------------------------------------------------------
	  DO 250 I=1,UpperIndex(13)

C-------------------------------------------------------------------------------
C If we find the transport, go
C-------------------------------------------------------------------------------
	  IF (UnitLocation(CTransportStart+I).EQ.Location) GOTO 270
250	  CONTINUE

C-------------------------------------------------------------------------------
C Not on a transport, choose a move to get the army out of the city
C-------------------------------------------------------------------------------
	  GOTO 300
	ENDIF

C-------------------------------------------------------------------------------
C ARMJMP used to decide whether to disembark a troop transport, if it 
C returns 0, then we're staying on board so go...
C-------------------------------------------------------------------------------
270	IF (ARMJMP(Location,UnitTimer(ArmyID)).EQ.0) GOTO 4150

C-------------------------------------------------------------------------------
C Move SELECTION
C
C We got here because we've decided to...
C	• Move out of a city
C	• Disembark a troop transport
C	• Move from an idle position
C
C Get the current assigned action (Action)
C Get the current target location (ActionTarget)
C-------------------------------------------------------------------------------
300	Action=UnitAction(ArmyID)
	ActionTarget=UnitActionTarget(ArmyID)

C-------------------------------------------------------------------------------
C IF A PRIORITY Move EXISTS, PICK IT AND DON"T BOTHER SLUGGING
C THROUGH CODE SELECTION AND Move SELECTION
C
C Get a priority move direction.
C If one was found, go and carry out that action
C-------------------------------------------------------------------------------
	PriorityMove=PRIORI(Location,Action,ActionTarget,Dither,Terrain)
	IF (PriorityMove.NE.0) GOTO 2400

C-------------------------------------------------------------------------------
C Follow existing assigned action...
C
C Action=0: Move IN CERTAIN DIRECTION, OR FOLLOW SHORE
C Action=1: Move TOWARDS TargetCity CITY
C Action=2: Move TOWARDS AN ENEMY ARMY
C Action=3: Move TOWARDS A TROOP TRANSPORT
C-------------------------------------------------------------------------------
	GOTO (400,500,600,700) Action+1

C-------------------------------------------------------------------------------
C No action at present, go and select a mission
C-------------------------------------------------------------------------------
400	GOTO 800		'LOOK FOR TargetCityS, Sightings, TT"S

C-------------------------------------------------------------------------------
C Action was to attack city, but city has been captured, go and select a new
C mission
C-------------------------------------------------------------------------------
500	IF (ReferenceMap(ActionTarget).EQ."X") GOTO 800	'CITY HAS BEEN CAPTURED

C-------------------------------------------------------------------------------
C Continue towards target city
C-------------------------------------------------------------------------------
	GOTO 1600			'Move

C-------------------------------------------------------------------------------
C Action was to attack player units, but none at the location, go and select a
C new mission
C-------------------------------------------------------------------------------
600	IF (ActionTarget.EQ.Location) GOTO 800		'ARRIVED AT ENEMY CONCENTRATION

C-------------------------------------------------------------------------------
C Continue towards target unit
C-------------------------------------------------------------------------------
	GOTO 1600			'Move

C-------------------------------------------------------------------------------
C Action is to move to a troop transport. 
C If transport location is invalid, get new mission
C If the transport is already full, look for another transport
C If the transport has been sunk, look for another transport
C If the transport has been damaged, look for another transport
C Otherwise, continue towards transport
C-------------------------------------------------------------------------------
700	IF (ActionTarget.GT.100) GOTO 800					' Invalid target value for this action (TT Distance out of range)
	IF (UnitAction(ActionTarget+CTransportStart-1500).GE.6) GOTO 1200
	IF (UnitLocation(ActionTarget+CTransportStart).EQ.0) GOTO 1200		'TT SUNK
	IF (UnitHits(ActionTarget+CHitsTransportStart).LT.3) GOTO 1200	'TT DAMAGED
	GOTO 1700

C-------------------------------------------------------------------------------
C SELECT A NEW CODE
C
C Assign a new mission to the army
C-------------------------------------------------------------------------------
800	CONTINUE

C-------------------------------------------------------------------------------
C LOOK FOR TargetCity CITY
C
C EnemyCount array contains a variety of counters used by the computer player.
C Element 10 contains the number of target cities
C
C If number of target cities is zero, go an look for enemy units to attack
C-------------------------------------------------------------------------------
	IF (EnemyCount(10).EQ.0) GOTO 1050

C-------------------------------------------------------------------------------
C Randomly select a start point to work through the target list, determine an 
C end point also
C For each target city...
C If the index goes out of range, bring it back
C If this target city has no location, try the next
C If the target city is more than 14 distant, try the next
C Determine a move to the target
C Increment number of paths evaluated (this appears to be for debugging only)
C If no path was found, try the next city
C-------------------------------------------------------------------------------
	IStart=RND(EnemyCount(10))+1
	IEnd=IStart+EnemyCount(10)-1

	DO 1000 Index=IStart,IEnd
	I=Index
	IF (I.GT.EnemyCount(10)) I=I-EnemyCount(10)
	IF (TargetCity(I).EQ.0) GOTO 1000
	IF (IDIST(Location,TargetCity(I)).GT.14) GOTO 1000
	Move=PATH(Location,TargetCity(I),Dither,EnemyArmyTerrain,Reachable)
	PathCount=PathCount+1
	IF (Reachable.EQ.0) GOTO 1000		'CAN"T GET TO IT

C-------------------------------------------------------------------------------
C Otherwise, set army action to "attack city"
C and set location of target city
C Start moving towards new target
C-------------------------------------------------------------------------------
	Action=1
	ActionTarget=TargetCity(I)
	GOTO 1800						'Move

1000	CONTINUE

C-------------------------------------------------------------------------------
C LOOK FOR AN ARMY THAT IS ON YOUR CONTINENT
C
C Why is documenting code so important, the Sightings array is a good example
C 
C "THE Sightings ARRAY. THE FIRST INDEX IS THE CONTINENT, THE SECOND IS THE NTH ARMY 
C DISCOVERED ON THAT CONTINENT - 1. THE (N,1) ARGUMENT IS THE DATE OF THE LAST 
C ARMY DISCOVERED ON THE NTH CONTINENT. THUS WE HAVE A MEANS OF DETERMINING THE 
C AGE OF THE DATA"
C 
C It looks like the first index is used to group sightings of armies. 
C
C We have no idea (yet) what is special about Sightings(10,11).
C Based on the usage of the value from the Sightings array, it appears to conatin
C locations.
C
C If 1st element is most recent sighting, and 11 has some special significance
C then it would allow the tracking of 9 armies per "continent".
C  
C Loop through the array
C Randomly choose and element
C If there's no location stored in that element, try element 2
C If there's nothing there either, try a new random choice
C Get the location
C Map a path from this unit's position to the target
C Increment PathCount. Appears to be count of attempted paths for debugging
C If no path to target, try new random target in the Sightings array
C Otherwise we have successfully identified an accessible target
C Set the units function to move towards this unit
C Set the target location
C Go to plan the move in detail
C-------------------------------------------------------------------------------
1050	IF (Sightings(10,11).NE.0) Sightings(10,11)=0

	DO 1100 I=1,10
	Temp=RND(10)+2
	IF (Sightings(I,Temp).EQ.0) Temp=2
	IF (Sightings(I,Temp).NE.0) THEN
		Temp=Sightings(I,Temp)
		Move=PATH(Location,Temp,Dither,EnemyArmyTerrain,Reachable)
		PathCount=PathCount+1

		IF (Reachable.NE.0) THEN
			Action=2
			ActionTarget=Temp
			GOTO 1800
		ENDIF
	ENDIF
1100	CONTINUE

C-------------------------------------------------------------------------------
C LOOK FOR TT THAT IS SHORT OF ARMIES
C
C If active timer in operation for this army, go to choose a random direction
C Generate a random start point
C For each possible troop transport...
C If the index number is greater than the max used, bring it back to the start
C If the troop transport has no location, ignore (doesn't exist)
C If the troop transport is damaged, ignore
C If full(?) ignore
C If further away than 20, ignore
C Plan a move to the transport
C If inacessible, try another
C Get a direction to the transport
C Set function for army to proceed to transport
C Set location of transport
C Assign army to transport?
C Go to plan detailed route
C-------------------------------------------------------------------------------
1200	IF (UnitTimer(ArmyID).NE.0) GOTO 1400			'INELIGIBLE TO GET ON A TT
	IStart=RND(UpperIndex(13))+1				'**
	DO 1300 Index=IStart,IStart+UpperIndex(13)
	I=Index
	IF (I.GT.UpperIndex(13)) I=I-UpperIndex(13)
	IF (UnitLocation(CTransportStart+I).EQ.0) GOTO 1300		'TT DOESN"T ECityLocationIST
	IF (UnitHits(CHitsTransportStart+I).LT.3) GOTO 1300		'DAMAGED, I.E. UNSUITABLE
	IF (IABS(UnitAction(CTransportStart+I-1500)).GE.6) GOTO 1300	'Assigned a function? Or full?
	IF (IDIST(Location,UnitLocation(CTransportStart+I)).GT.20) GOTO 1300	'TOO FAR AWAY
	Move=PATH(Location,UnitLocation(CTransportStart+I),Dither,EnemyArmyTerrain,Reachable)
	PathCount=PathCount+1
	IF (Reachable.EQ.0) GOTO 1300			'CAN"T GET TO IT
	Move=MOV(Location,UnitLocation(CTransportStart+I))
	Action=3
	ActionTarget=I
	UnitActionTarget(CTransportStart+I-1500)=ArmyID
	GOTO 1800
1300	CONTINUE


C-------------------------------------------------------------------------------
C PICK A RANDOM DIRECTION (Action=0)
C
C If this army is already moving randomly, go and do that
C Change army function to random
C Pick a random direction
C-------------------------------------------------------------------------------
1400	IF ((Action.EQ.0).AND.(ActionTarget.NE.0)) GOTO 1500	'IF ALREADY ASS"D DIREC
	Action=0
	ActionTarget=RND(8)+1				'**

C-------------------------------------------------------------------------------
C Save the chose direction
C Generate another random direction
C If the terrain in the 2nd chose direction is not land, use the direction first
C chosen (!)
C Go!
C-------------------------------------------------------------------------------
1500	Move=ActionTarget
	I1=ICORR(Move-Dither*3)
	IF (ReferenceMap(Location+MoveOffset(I1+1)).NE."+") Move=I1	'**
	GOTO 1800

C-------------------------------------------------------------------------------
C Continue moving towards a specific location
C
C Plan the move
C If path is bad go and choose a random direction...
C Go to execute move
C-------------------------------------------------------------------------------
1600	Move=PATH(Location,ActionTarget,Dither,EnemyArmyTerrain,Reachable)
	PathCount=PathCount+1
	IF (Reachable.EQ.0) GOTO 1400
	GOTO 1800

C-------------------------------------------------------------------------------
C Move towards troop transport
C
C Plan move
C For each adjacent location...
C Get direction to adjacent location
C Get location of adjacent location
C Get visible terrain at this location
C If this is not the troop transport, go
C-------------------------------------------------------------------------------
1700	Move=PATH(Location,UnitLocation(ActionTarget+CTransportStart),Dither,EnemyArmyTerrain,Reachable)
	PathCount=PathCount+1
C 
1800	DO 2300 I=0,7*Dither,Dither
	PriorityMove=ICORR(Move+I)
	AdjacentLocation=Location+MoveOffset(PriorityMove+1)
	AdjacentTerrain=ReferenceMap(AdjacentLocation)
	IF (AdjacentTerrain.NE."t") GOTO 2200

C-------------------------------------------------------------------------------
C Troop transport is adjacent
C If we were looking for a transport, clear that action
C Initialise count of armies on board
C Identify the troop transpoprt found
C-------------------------------------------------------------------------------
	IF (Action.EQ.3) Action=0
	IF (UnitTimer(ArmyID).NE.0) GOTO 2300
	Aboard=0
	
	DO 1900 TransportID=CTransportStart+1,UpperIndex(13)+CTransportStart
1900	IF (UnitLocation(TransportID).EQ.AdjacentLocation) GOTO 2000

C-------------------------------------------------------------------------------
C If the transport is damaged, go
C-------------------------------------------------------------------------------
2000	IF (UnitHits(CHitsTransportStart-CTransportStart+TransportID).LT.3) GOTO 2300

C-------------------------------------------------------------------------------
C Count armies aboard this transport
C If the transport is full, go
C Otherwise, go to move aboard transport
C-------------------------------------------------------------------------------
	DO 2100 J=CArmyStart+1,UpperIndex(9)+CArmyStart
	IF (UnitLocation(J).EQ.AdjacentLocation) Aboard=Aboard+1
2100	IF (Aboard.GE.6) GOTO 2300
	GOTO 2400

C-------------------------------------------------------------------------------
C If adjacent terrain is land, and its on the map, go to execute move
C Otherwise, try next adjacent location
C-------------------------------------------------------------------------------
2200	IF ((AdjacentTerrain.EQ."+").AND.(ORDER(AdjacentLocation).EQ.0)) GOTO 2400
2300	CONTINUE
	
C-------------------------------------------------------------------------------
C Failed to find a mission
C-------------------------------------------------------------------------------
	PriorityMove=0

C-------------------------------------------------------------------------------
C If no assigned action, set the target direction
C Set army's action to that determined
C Set army's target location/direction to that determined
C Get location of next move in this direction
C-------------------------------------------------------------------------------
2400	IF (Action.EQ.0) ActionTarget=IABS(PriorityMove)
	UnitAction(ArmyID)=Action
	UnitActionTarget(ArmyID)=ActionTarget
	IF (DebugLevel.EQ.1) TYPE 998,Action,ActionTarget
998	FORMAT(1X,7I,3CityLocation)
	Location=Location+MoveOffset(PriorityMove+1)

C-------------------------------------------------------------------------------
C Get visible terrain at this location
C If current location is not a troop transport, go
C If next location is not a troop transport, go
C Begin inactivity timer
C go
C-------------------------------------------------------------------------------
	AdjacentTerrain=ReferenceMap(Location)
	IF (Terrain.NE."t") GOTO 2500
	IF (AdjacentTerrain.EQ."t") GOTO 3600
	UnitAction(ArmyID)=0
	UnitActionTarget(ArmyID)=0
	UnitTimer(ArmyID)=1020
	GOTO 2600

C-------------------------------------------------------------------------------
C If the current location is not a city, update the visible map from the master
C If the next location is not a transport, go
C Begin inactivity time
C go
C-------------------------------------------------------------------------------
2500	IF (MasterMap(SavedLocation).NE."*") ReferenceMap(SavedLocation)=MasterMap(SavedLocation)
	IF (AdjacentTerrain.NE."t") GOTO 2600
	UnitTimer(ArmyID)=100
	GOTO 3600

C-------------------------------------------------------------------------------
C If next location is land, go
C If next location is computer city, or ocean go
C If next location is not a city go
C-------------------------------------------------------------------------------
2600	IF (AdjacentTerrain.EQ."+") GOTO 3500
	IF ((AdjacentTerrain.EQ."X").OR.(AdjacentTerrain.EQ.".")) GOTO 3700
	IF (MasterMap(Location).NE."*") GOTO 3400

C-------------------------------------------------------------------------------
C Find an idle army to join with
C
C Take a chance! If bottom half of a 100% chance...
C Set a range of 10... this is the range to the closest idle army
C For each army...
C If army is invalid, try the next
C If army is this army, try the next
C If army has an assigned action, try the next
C If army is further than set range, try the next
C Plan a move to this army.
C If unreachable, try the next
C Save the distance to this army
C Record the index of this army - Appears to be unused after this
C-------------------------------------------------------------------------------
	IF (RND(100).LT.50) THEN
		Distance=10

		DO 2650 I=1,UpperIndex(9)
			IF (UnitLocation(CArmyStart+I).EQ.0) GOTO 2650
			IF (I.EQ.ArmyID) GOTO 2650
			IF (UnitAction(I).NE.0) GOTO 2650
			IF (IDIST(UnitLocation(CArmyStart+I),Location).GE.Distance) GOTO 2650
			Move=PATH(UnitLocation(CArmyStart+I),Location,1,EnemyArmyTerrain,Reachable)
			PathCount=PathCount+1
			IF (Reachable.NE.0) THEN
				Distance=IDIST(UnitLocation(CArmyStart+I),Location)
				Closest=I
			END IF
2650		CONTINUE

C-------------------------------------------------------------------------------
C So we've finished scanning above, so what we will have is the distance to the
C closest idle army.
C
C If this is less than 10, then
C Set army's function to 1 "Move towards city"
C Set the target location
C-------------------------------------------------------------------------------
		IF (Distance.LT.10) THEN
			Action=1
			ActionTarget=Location
		ENDIF

		GOTO 3700
	ENDIF

C-------------------------------------------------------------------------------
C If we got the 50+ on the 50% chance earlier we end up here
C
C Remove target location from target list
C-------------------------------------------------------------------------------
	DO 2700 I=1,70
2700	IF (TargetCity(I).EQ.Location) TargetCity(I)=0

C-------------------------------------------------------------------------------
C For each possible army that was targeting this city
C 
C The original code won't work. It should probably have been
C
C		do 2800, i=1, UpperIndex(9)
C		    if (UnitAction(i) .eq. 1 .and. UnitActionTarget(i) .eq. Location) then
C			UnitAction(i) = 0
C			UnitActionTarget(i) = 0
C		    endif
C	2800	continue
C
C-------------------------------------------------------------------------------
	DO 2800 I=1,UpperIndex(9)
2800	IF (UnitAction(I).NE.1.OR.UnitActionTarget(I).NE.Location) GOTO 2900

C-------------------------------------------------------------------------------
C This doesn't make any sense. These two lines will affect only the final army
C from the loop
C-------------------------------------------------------------------------------
	UnitAction(I)=0
	UnitActionTarget(I)=0

C-------------------------------------------------------------------------------
C Find the city at the location. This has the ability to run off the end of the
C array since CityLocation is only 70 elements long.
C
C Hopefully we always find the city and exit the loop with I set to the index of
C that city.
C-------------------------------------------------------------------------------
2900	DO 3000 I=1,100
3000	IF (CityLocation(I).EQ.Location) GOTO 3100

C-------------------------------------------------------------------------------
C Apparently we have taken a city...
C Change its owner to the computer.
C Cancel production
C If this was a player city, or the army was idle, and there are less than 8
C sea adjacent to this location, set production to armies
C-------------------------------------------------------------------------------
3100	CityOwner(I)=2
	CityProduction(I)=0
	IF (((AdjacentTerrain.EQ."O").OR.(UnitTimer(ArmyID).GT.0)).AND.(EDGER(Location).LT.8))
	1	CityProduction(I)=-1

C-------------------------------------------------------------------------------
C If this wasn't previously a player city... go
C Let the player know they lost a city
C Set the reference map to show a computer city
C Update the player's map around the captured city
C-------------------------------------------------------------------------------
	IF (AdjacentTerrain.NE."O") GOTO 3200
	CALL LIB$SET_CURSOR(3,1)
	CALL STROUT("City at",10)
	CALL DECPRT(Location)
	CALL STROUT(" surrendered to enemy forces.",1)
	ReferenceMap(Location)="X"
	CALL SENSOR(Location)
	GOTO 3700

C-------------------------------------------------------------------------------
C Set the reference map to show a computer city
C-------------------------------------------------------------------------------
3200	ReferenceMap(Location)="X"
	GOTO 3700

C-------------------------------------------------------------------------------
C Set army inactivity timer
C-------------------------------------------------------------------------------
3300	UnitTimer(ArmyID)=100
	GOTO 3600

3400	Hits1=1
	IF (SavedLocation.EQ.Location) GOTO 3600
997	FORMAT(1H+,/," ERROR: ATTACKED ",A1,4I,1X)
	PlayerTarget=1
	Combatant1="a"
	Combatant2=AdjacentTerrain
	Hits2=30
C-------------------------------------------------------------------------------
C Z8??? Where did this come from? Required by FIND routine, and looks like it
C is only used for ship combatants.
C-------------------------------------------------------------------------------
	CALL FIND(Combatant2,Location,Z8,Hits2)
	CALL FGHT(Location,Hits1,Hits2,Combatant1,Combatant2)
	CALL FIND(Combatant2,Location,Z8,Hits2)
	IF (Hits1.LE.0) GOTO 3700
	ReferenceMap(Location)=MasterMap(Location)
	IF (ReferenceMap(Location).EQ.".") GOTO 3700
3500	ReferenceMap(Location)="a"
3600	UnitLocation(CArmyStart+ArmyID)=Location
	IF (PlayerTarget.EQ.1) CALL SENSOR(Location)
	GOTO 4100
3700	UnitLocation(CArmyStart+ArmyID)=0
	IF (AdjacentTerrain.NE."X") GOTO 3900
	DO 3800 I=1,70
3800	IF (CityLocation(I).EQ.Location) CityProduction(I)=0
3900	IF (PlayerTarget.EQ.1) CALL SENSOR(Location)
	IF (ReferenceMap(Location).NE."O") GOTO 4000
	CALL LIB$SET_CURSOR(3,1)
	CALL STROUT("City at",10)
	CALL DECPRT(Location)
	CALL STROUT(" repelled enemy invasion.",1)
4000	UnitAction(ArmyID)=0
	UnitActionTarget(ArmyID)=0
	UnitTimer(ArmyID)=0
4100	CALL SONAR(Location)
4150	IF (UnitLocation(CArmyStart+ArmyID).NE.0) EnemyCount(1)=EnemyCount(1)+1
4200	CONTINUE
	UpperIndex(9)=CurrentArmy
	RETURN
	END