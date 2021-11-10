	FUNCTION SET(CityLocationPOS,YPOS,AREA,LS,LIMIT)
	IMPLICIT INTEGER(A-Z)
	PARAMETER WIDTH=100,HEIGHT=60
	BYTE MAP(WIDTH,HEIGHT)
	BYTE CityOwner(WIDTH,HEIGHT)
	BYTE CityLocationSTACK(12000)
	BYTE YSTACK(12000)
	BYTE CSTACK(12000)
	BYTE LS
	INTEGER CityLocationADDS(8),YADDS(8)
	DATA CityLocationADDS/-1,0,1,-1,1,-1,0,1/
	DATA YADDS/-1,-1,-1,0,0,1,1,1/
	BYTE	EMAP(6000),ReferenceMap(6000),PlayerMap(6000),MasterMap(6000)
	INTEGER UnitLocation(3000)
	COMMON/MAPBLK/EMAP,ReferenceMap,PlayerMap,UnitLocation
	COMMON/MasterMap/MasterMap
	EQUIVALENCE (MAP(1,1),MasterMap(1)),(CityOwner(1,1),ReferenceMap(1))

C
C For this location, set the body ID
C 
	CityOwner(CityLocationPOS,YPOS)=AREA
	LEVEL=1
	CityLocation=CityLocationPOS
	Y=YPOS

C
C Initialise the direction we're going to examine... 1 is up and left
C
100	K=1

C
C Examine the terrain in the chosen direction, skip to the next location 
C if any of the following are true...
C
C The location is on either the left or right edge of the map
C The location is on either the top or bottom edge of the map
C The location is not the specified terrain type
C The location already has a body ID assigned to it
C
200	IF ((CityLocation+CityLocationADDS(K).LT.2).OR.(CityLocation+CityLocationADDS(K).GT.99)) GOTO 300
	IF ((Y+YADDS(K).LT.2).OR.(Y+YADDS(K).GT.59)) GOTO 300
	IF (MAP(CityLocation+CityLocationADDS(K),Y+YADDS(K)).NE.LS) GOTO 300
	IF (CityOwner(CityLocation+CityLocationADDS(K),Y+YADDS(K)).NE.0) GOTO 300


	CityOwner(CityLocation+CityLocationADDS(K),Y+YADDS(K))=AREA
	CityLocationSTACK(LEVEL)=CityLocation
	YSTACK(LEVEL)=Y
	CSTACK(LEVEL)=K
	LEVEL=LEVEL+1

C
C If the number of examined locations exceeds the maximum body area, give up
C
	 IF (LEVEL.GT.LIMIT) THEN
	   SET=0
	   RETURN
	 ENDIF

C
C Use the examined location as the new source location, and explore from 
C
	CityLocation=CityLocation+CityLocationADDS(K)
   	Y=Y+YADDS(K)
	GOTO 100

C
C The examined location did not meet the conditions...
C Try the next direction.
C
300	K=K+1

C
C If we havent't tried all directions from this location, go back and explore
C from this location in the new direction
C
	IF (K.LE.8) GOTO 200

C
C We've explored each direction from this location, so go back to the 
C previous location
C
	LEVEL=LEVEL-1

C
C If we're back to the original location, we're finished
C
	IF (LEVEL.EQ.0) THEN
	   SET=1
	   RETURN
	ENDIF

C
C Otherwise, get the coordinates and last used direction of the
C new location
C
	CityLocation=CityLocationSTACK(LEVEL)
	Y=YSTACK(LEVEL)
	K=CSTACK(LEVEL)
	GOTO 300

	END