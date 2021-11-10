C
C RANDOM MAP GENERATION SUBROUTINES
C
	SUBROUTINE GEN
	IMPLICIT INTEGER(A-Z)
	PARAMETER WIDTH=100,HEIGHT=60
	BYTE MAP(WIDTH,HEIGHT)
	BYTE SUBMAP(39,39)
	BYTE CityOwner(WIDTH,HEIGHT)
	INTEGER SIZES(128)
	COMMON/CITIES/CITIES(128)
	COMMON/SMAP/SUBMAP
	BYTE	EMAP(6000),ReferenceMap(6000),PlayerMap(6000),MasterMap(6000)
	INTEGER UnitLocation(3000)
	COMMON/MAPBLK/EMAP,ReferenceMap,PlayerMap,UnitLocation
	COMMON/MasterMap/MasterMap
	EQUIVALENCE (MAP(1,1),MasterMap(1)),(CityOwner(1,1),ReferenceMap(1))

C
C Fill the map with sea
C
100	DO 200 I=1,WIDTH
	DO 200 J=1,HEIGHT
200	MAP(I,J)="."

C
C Generate random numbers of horizontal and vertical sections
C
	HSECTS=3+RND(4)
	VSECTS=3+RND(3)

C
C Divide the map into these sections
C
	HSPACE=WIDTH/HSECTS
	VSPACE=HEIGHT/VSECTS

	DO 400 I=1,HSECTS
	DO 400 J=1,VSECTS
	DO 400 K=1,RND(2)+RND(3)
C
C Make a "blob" of land
C
	CALL MAKELAND

C
C Generate a random positon on the main map
C
	YPOS=(J-1)*VSPACE+RND(VSPACE)
	CityLocationPOS=(I-1)*HSPACE+RND(HSPACE)

C
C Transfer the "blob" of land to the main map
C
	DO 300 L=1,39
	DO 300 M=1,39

C
C Only transfer land
C
	IF (SUBMAP(L,M).EQ." ") GOTO 300

C
C If the proposed CityLocation coordinate is inside the map boundary
C and the propose Y coordinate is inside the map boundary
C set the terrain at the proposed location to land.
C
	IF (((CityLocationPOS+L-20).LE.0).OR.((CityLocationPOS+L-20).GT.100)) GOTO 300
	IF (((YPOS+M-20).LE.0).OR.((YPOS+M-20).GT.60)) GOTO 300
	MAP(CityLocationPOS+L-20,YPOS+M-20)=SUBMAP(L,M)

300	CONTINUE
400	CONTINUE

C
C Determine total wet area of the map
C
	COUNT=0
	DO 500 I=1,100
	DO 500 J=1,60
	IF (MAP(I,J).EQ.".") COUNT=COUNT+1
500	CONTINUE

C
C If too wet or too dry, try again
C
	IF (COUNT.LT.4000.AND.COUNT.GT.2500) GOTO 600
C	TYPE 999,COUNT
C	WRITE (1,999) COUNT
C999	FORMAT(" FAILED SEA CHECK, COUNT=",I5)
	GOTO 100

C600	TYPE 998,COUNT
C	WRITE (1,998) COUNT
C998	FORMAT(" COUNT=",I5)

C
C Identify contiguous bodies of land and sea and determine the area of each
C
C "CityOwner" array identifies which body owns each location
C
600	DO 800 I=1,100
	DO 800 J=1,60
	CityOwner(I,J)=0
800	CONTINUE

C
C Initial land body is ID 1
C Initial sea body is ID 33
C
	LAREA=1
	WAREA=33

C
C For each map location...
C
	DO 1000 I=2,99
	DO 1000 J=2,59
C
C If owner already determined, go to the next location
C
	IF (CityOwner(I,J).NE.0) GOTO 1000

C
C If this location is sea...
C
	 IF (MAP(I,J).EQ.".") THEN

C
C If area of this body of water exceeds 12000, fail map 
C build and start again
C
	   IF (SET(I,J,WAREA,".",12000).EQ.0) GOTO 100
	   WAREA=WAREA+1
	   GOTO 1000
	  ELSE
	   IF (SET(I,J,LAREA,"+",1200).EQ.1) GOTO 900
C	   TYPE 997
C	   WRITE (1,997)
C997	   FORMAT(" FAILED SINGLE LAND MASS TEST")
	   GOTO 100
	 ENDIF
900	LAREA=LAREA+1
1000	CONTINUE

C
C If the number of islands is not between 10 and 30
C try again as the map is too sparse or too dense
C
	IF (LAREA.GE.10.AND.LAREA.LE.30) GOTO 1100
C	TYPE 996
C	WRITE(1,996)
C996	FORMAT(" FAILED SEPARATION TEST")
	GOTO 100

C1100	TYPE 995,(("@"+CityOwner(I,J),I=1,100),J=1,60)
C	WRITE(1,995) (("@"+CityOwner(I,J),I=1,100),J=1,60)
C995	FORMAT(1X,100A1)

C
C get the total area of each island and sea
C
1100	DO 1300 I=1,128
1300	SIZES(I)=0

	DO 1400 I=2,99
	DO 1400 J=2,59
	SIZES(CityOwner(I,J))=SIZES(CityOwner(I,J))+1
1400	CONTINUE

C
C We require one large ocean that takes 80% of the wet area
C (SCOUNT is the target area)
C
C Loop through the oceans until we find the first big ocean
C (SEA then identifies this large ocean)
C
	SCOUNT=COUNT*40/50
	DO 1500 SEA=33,WAREA
1500	IF (SIZES(SEA).GE.SCOUNT) GOTO 1600
C	TYPE 994
C	WRITE (1,994)
C994	FORMAT(" FAILURE- OCEANS ARE SEPARATED")
	GOTO 100

C
C City generation...
C
C Number of cities to generate... 1/50th of the land area
C But limit to somewhere between 52 and 70
C
C 60% of cities are to be on the big ocean, and the rest are
C to be inland
C
1600	CITS=(6000-COUNT)/50+1
	CITS=MACityLocation(52,CITS)
	CITS=MIN(70,CITS)
	SEACITS=CITS*60/100+RND(12)
	LANDCITS=CITS-SEACITS

C
C Place the coastal cities... for each coastal city...
C
	DO 2100 K=1,SEACITS

C
C Get some random coordinates
C If it's not land at these coordinates generate some more
C
1700	I=RND(98)+2
	J=RND(58)+2
	IF (MAP(I,J).NE."+") GOTO 1700

C
C Examine the adjacent locations, and determine if one
C belongs to the large ocean.
C
	DO 1800 L=MACityLocation(2,I-1),MIN(99,I+1)
	DO 1800 M=MACityLocation(2,J-1),MIN(59,J+1)
	IF (CityOwner(L,M).EQ.SEA) GOTO 1900
1800	CONTINUE

C
C Location was not on the large ocean, try again
C
	GOTO 1700

C
C Check that the city location is not close to another city
C
1900	DO 2000 L=MACityLocation(2,I-3),MIN(99,I+3)
	DO 2000 M=MACityLocation(2,J-3),MIN(59,J+3)

C
C If nearby location is in a different body, keep checking.
C If nearby location is another city, try new coordinates
C
	IF (CityOwner(L,M).NE.CityOwner(I,J)) GOTO 2000
	IF (MAP(L,M).EQ."*") GOTO 1700
2000	CONTINUE

C
C OK, we have a city on the coastline of the big ocean with no
C nearby neighbours
C
C Set the map location to a city
C Give points to the continent on which the city is located
C
	MAP(I,J)="*"
	CITIES(CityOwner(I,J))=CITIES(CityOwner(I,J))+100
2100	CONTINUE

C
C All coastal cities have been placed, now place remaining cities
C
	DO 2500 K=1,LANDCITS

C
C Generate some random coordinates for the city
C
2200	I=RND(98)+2
	J=RND(58)+2

C
C If this location is not land, try new coordinates
C Check adjacent locations, ensure that this location is land locked
C
	IF (MAP(I,J).NE."+") GOTO 2200
	DO 2300 L=MACityLocation(2,I-1),MIN(99,I+1)
	DO 2300 M=MACityLocation(2,J-1),MIN(59,J+1)
	IF (MAP(L,M).EQ.".") GOTO 2200
2300	CONTINUE

C
C City is land locked, check for nearby cities
C
	DO 2400 L=MACityLocation(2,I-2),MIN(99,I+2)
	DO 2400 M=MACityLocation(2,J-2),MIN(59,J+2)

C
C If nearby location is on a different body, keep looking
C If a city is nearby, try new coordinates
C
	IF (CityOwner(L,M).NE.CityOwner(I,J)) GOTO 2400
	IF (MAP(L,M).EQ."*") GOTO 2200
2400	CONTINUE

C
C City is land locked and isolated
C
C Place city on map
C Increase cities score of continent
C
	MAP(I,J)="*"
	CITIES(CityOwner(I,J))=CITIES(CityOwner(I,J))+1
2500	CONTINUE

C	TYPE 993,((MAP(I,J),I=1,100),J=1,60)
D	WRITE(1,993) ((MAP(I,J),I=1,100),J=1,60)
D993	FORMAT(1X,100A1)

	END