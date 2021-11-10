	SUBROUTINE GAME(ICODE,NUM)
C
C THIS SUBROUTINE READS IN THE GAME MAP AND INITIALIZES THE MAP ARRAYS
C  IT ALSO SAVES AND RESTORES THE GAME FROM THE SAVE FILE
C	ICODE: FUNCTION, -1=RESTORE, 0=INIT, 1=SAVE
C
	INCLUDE "EMPIRE.INC/NOLIST"
	COMMON/CITIES/CITIES(128)
C
	DATA IFILE/"G","A","M","E","S",":","E","M","R","A",0/
C
	IF (ICODE) 1800,100,1500		'-1/0/+1 = RESTORE/INIT/SAVE
C
C HERE TO INITIALIZE THE GAME
C
100	DO 200 I=1,70			'CLEAR ARRAYS
	 CityLocation(I)=0
	 FOUND(I)=0
	 CityOwner(I)=0
	 CityProduction(I)=0
	 TargetCity(I)=0
	 FIPATH(I)=0
200	CONTINUE
	DO 300 I=1,1500
	 UnitAction(I)=0
	 UnitActionTarget(I)=0
	 MYCODE(I)=0
300	CONTINUE
	DO 400 I=1,200
	 RANGE(I)=0
	 RANG(I)=0
400	CONTINUE
	DO 500 I=1,500
500	 UnitTimer(I)=0
	DO 600 I=1,3000
	 UnitLocation(I)=0
600	CONTINUE
	DO 700 I=1,6000
	 EMAP(I)=" "
	 PlayerMap(I)=" "
700	CONTINUE
C
	MODE=1
	ISEC=-1
	CALL TIME(PAMELA)
	CALL DATE(REEED)
					'THIS GETS UPDATED IN TWO PLACES''''
	VERSION=7			'VERSION OF DATA WITHIN EMSAVE.DAT FILE
	IB=1

C-----MAP SELECTION-------
C PICK ONE OF THE MAPS RANDOMLY.  MAPS ARE IN FILES A-F
C
	TRY=0				'TRY AGAIN 
900	TRY=TRY+1
	IFILE(10)="A"
	IFILE(10)=IFILE(10)+RND(10)	'CURRENTLY SICityLocation MAPS, ALLOW 4 ECityLocationTRA
	IF (TRY .LE. 8) GOTO 1000	'TRY AGAIN IF YOU DON"T HAVE ALL OF THEM
	TYPE 999
999	FORMAT(" Generating new map ...")
	CALL GEN
	TRY=0
	GOTO 1100
1000	OPEN(UNIT=1,NAME=IFILE,ACCESS="SEQUENTIAL",FORM="UNFORMATTED",
	1	TYPE="OLD",READONLY,ERR=900)
	READ(1) (D(I),I=1,223)
	READ(1) (D(I),I=224,446)
	READ(1) (D(I),I=447,667)
 	CLOSE(UNIT=1)
C
C-----CITY AND A-MAP INITIALIZATION--------
C
1100	CALL INITIA(TRY)		' TRANSFER MAP FROM D() INTO MAPBUF
C
C Choose a player city "C" and an enemy city "ID"
C 
1200	C=RND(70)+1				'** PICK OUR CITY
	ID=RND(70)+1				' PICK ENEMY CITY

C
C If either selected city has no location, try again
C
	IF (CityLocation(C).EQ.0.OR.CityLocation(ID).EQ.0) GOTO 1200

C
C If player and enemy city are in the same location, try again
C
	IF (CityLocation(C).EQ.CityLocation(ID)) GOTO 1200

C
C If either city is surrounded by water, try again
C
	IF ((EDGER(CityLocation(C)).EQ.8).OR.(EDGER(CityLocation(ID)).EQ.8)) GOTO 1200

C 
C "TRY" only seems to apply to file opening attempts, it should always be zero as set after label 999
C
	IF (TRY.NE.0) GOTO 1300

C
C Get city scores for the continents on which each city is located
C
1250	PCON=CITIES(ReferenceMap(CityLocation(ID)))
	ECON=CITIES(ReferenceMap(CityLocation(C)))

C
C If either is under 100, then there are no coastal cities, so start again
C
	IF (PCON.LE.100) GOTO 1200	' NOTE ReferenceMap IS REALLY CityOwner
	IF (ECON.LE.100) GOTO 1200	' FROM MAP GENERATOR

C
C Reverse-engineer the city scores to get the number of cities on the continent
C
	PTOT=PCON/100+MOD(PCON,100)
	ETOT=ECON/100+MOD(ECON,100)

C
C If the player has more cities, give the good city to the enemy!
C (The CON scores should have been swapped to avoid the goto)
C
	IF (PTOT.LE.ETOT) GOTO 1275
	I=C
	C=ID
	ID=I
	GOTO 1250

C
C Give a difficulty estimate
C
1275	DIFF=MIN(11,((ETOT*2*100+45)/PTOT)/100)-1
	IF (PCON.EQ.ECON) DIFF=3
	TYPE 994,DIFF
994	FORMAT(" Difficulty estimate:",I3,
	1 " where 1 is easy and 10 is most challenging")

C
C Get production for city from player
C
1300	Z6=CityLocation(ID)
	TYPE 998,CityLocation(ID)				' TELL US WHERE IT IS
998	FORMAT(" Your city is at ",I4)

C
C Copy the master map to the reference map
C
	DO 1400 I=1,6000
1400	ReferenceMap(I)=MasterMap(I)

C
C Set the two cities on the reference map
C
	ReferenceMap(Z6)="O"				' MARK IT ON MAP
	ReferenceMap(CityLocation(C))="X"


	CALL SONAR(CityLocation(C))			' DO SENSOR SCANS
	CALL SENSOR(Z6)
	MODE=0
	CALL LTR(Z6,0)				' SHOW THE CITY
	MODE=1
	CALL STROUT("What do you demand that this city produce?",13)
	CityOwner(ID)=1
	MDATE=0
	CALL PHASIN(ID,E)
	TYPE 993,E
993	FORMAT("+",A1,$)
	CityOwner(C)=2
	CityProduction(C)=1
	FOUND(C)=5
	Z6=CityLocation(ID)
	RETURN			' RETURN TO ORDERS MODE
C
C HERE TO SAVE A GAME
C 
1500	IF (MODE.EQ.0) TYPE  997
997	FORMAT(" A few moments please ..."/)
1600	CONTINUE
	CALL TIME(PAMELA)
	CALL DATE(REEED)
	OPEN(UNIT=1,NAME="EMSAVE",ACCESS="SEQUENTIAL",FORM="UNFORMATTED",
	1	STATUS="UNKNOWN")
	WRITE(1) LIMIT,MDATE,VERSION,PAMELA,REEED
	 WRITE(1) EMAP,ReferenceMap,PlayerMap,MasterMap
	 WRITE(1) UnitLocation
	WRITE(1) TROOPT
	WRITE(1) NUMBER
	WRITE(1) CityLocation,TargetCity,FOUND
	WRITE(1) CityOwner,CityProduction
	DO 1700 I=1,16
1700	CALL WRITE(IOTAB(I),UpperIndex(I),I)
	WRITE(1) UnitHits
	WRITE(1) NUM
	WRITE(1) Sightings
	WRITE(1) NSHIFT,FIPATH
	write(1) ccityrounds,pcityrounds,producedunits
	CLOSE(UNIT=1)
	RETURN
C
C HERE TO RESTORE A GAME
C
1800	TYPE 997
	OPEN(UNIT=1,NAME="EMSAVE",ACCESS="SEQUENTIAL",FORM="UNFORMATTED",
	1 STATUS="OLD",ERR=2200)
	READ(1) LIMIT,MDATE,VERSION,PAMELA,REEED
	READ(1) EMAP,ReferenceMap,PlayerMap,MasterMap
	IF(VERSION.GE.6) GOTO 1850
	DO 1850 I=1,6000
	   IF((EMAP(I).GE."1").AND.(EMAP(I).LE."8")) CALL TRAN(EMAP(I))
	   IF((ReferenceMap(I).GE."1").AND.(ReferenceMap(I).LE."8")) CALL TRAN(ReferenceMap(I))
	   IF((PlayerMap(I).GE."1").AND.(PlayerMap(I).LE."8")) CALL TRAN(PlayerMap(I))
1850	CONTINUE
	READ(1) UnitLocation
	READ(1) TROOPT
	READ(1) NUMBER
	READ(1) CityLocation,TargetCity,FOUND
	READ(1) CityOwner,CityProduction
	DO 1900 I=1,16
1900	CALL READ(IOTAB(I),UpperIndex(I),I)
	IF (VERSION.LE.4) READ(1) (UnitHits(I),I=1,1500)
	IF (VERSION.GE.5) READ(1) UnitHits
	READ(1) NUM
	READ(1) Sightings
	READ(1) NSHIFT,FIPATH
	if (version.ge.7) read(1,END=2000)ccityrounds,pcityrounds,producedunits
2000	VERSION=7			'TRANSLATE TO NEW VERSION
	CLOSE(UNIT=1)
	TYPE  996,PAMELA,REEED
996	FORMAT(" Ready to resume game terminated at ",8A1," on ",
	1	9A1,/)

	MODE=1
	ISEC=-1
	RETURN

2200	TYPE 995
995	FORMAT (" Unable to open save file, EMSAVE.DAT, Starting new game.")
	GOTO 100	
	END