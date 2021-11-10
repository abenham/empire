C 
	SUBROUTINE CITYPH(I)
C
	INCLUDE "EMPIRE.INC/NOLIST"
C 
C EnemyCount(1-8): NUMBERS OF UNITS
C EnemyCount(11-18): NUMBERS OF CITIES WITH EACH CityProduction
C EnemyCount(9): NUMBER OF CITIES
C EnemyCount(10): NUMBER OF TargetCity CITIES
C 
	INT=CityProduction(I)
	IF (CityProduction(I).NE.-1) GOTO 100
	CityProduction(I)=1
	GOTO 1400
100	EDGE=EDGER(CityLocation(I))
C 
C IF WE HAVE A CityProduction OF 0, MAKE SOMETHING'
C
	IF (CityProduction(I).EQ.0) GOTO 600
C 
C IF CITY IS SURROUNDED BY ARMIES, MAKE SOMETHING ELSE
C
	IF (CityProduction(I).NE.1) GOTO 300
	DO 200 J=1,8
200	IF (ReferenceMap(CityLocation(I)+MoveOffset(J+1)).EQ."+") GOTO 300	'**
	GOTO 600
C 
C IF CRAFT NUMBERS ARE GETTING GROSSLY LARGE, PRODUCE SOMETHING ELSE
C
300	IF (EnemyCount(OVRPOP(CityProduction(I)+1,1)).GT.
	1	OVRPOP(CityProduction(I)+1,2)) GOTO 600		'**
C 
	IF (EDGE.NE.8) GOTO 400
	IF ((EnemyCount(9).GT.1).AND.(CityProduction(I).EQ.1)) GOTO 1100
	IF (EnemyCount(9).GT.1) GOTO 1400
	IF (EnemyCount(5).LT.1) CityProduction(I)=6
	IF (EnemyCount(5).GT.0) CityProduction(I)=1
	GOTO 1400
C 
400	IF (CityProduction(I).NE.1) GOTO 600
	N=0
	DO 500 J=CArmyStart+1,CArmyStart+UpperIndex(9)
	Z=UnitLocation(J)
	IF (Z.EQ.0) GOTO 500
	IF (IDIST(CityLocation(I),Z).GT.6) GOTO 500
	IF (EMAP(Z).EQ."t") GOTO 500
	MOVE=PATH(CityLocation(I),Z,1,OKA,FLAG)
	IF (FLAG.EQ.0) GOTO 500
	N=N+1
	IF ((N.GT.6).AND.(EnemyCount(11).GT.1)) GOTO 800
500	CONTINUE
	IF ((N.GT.3).AND.(EnemyCount(11).GT.1)) GOTO 600
	GOTO 1400
C 
C SELECT A NEW CityProduction FOR THE CITY
C
600	CONTINUE
C
C IF THERE ARE ENEMY ARMIES ON THE CONTINENT, PRODUCE ARMIES'
C
	IF (EDGE.EQ.8) GOTO 1050
	DO 700 J=1,10
	IF (Sightings(J,2).EQ.0) GOTO 700
	MOVE=PATH(CityLocation(I),Sightings(J,2),1,OKA,FLAG)
	IF (FLAG.EQ.0) GOTO 700
	CityProduction(I)=1
	GOTO 1300
700	CONTINUE
C 
800	CityProduction(I)=2
	IF (EDGE.GT.0) GOTO 900			'IF NOT LANDLOCKED
	IF (EnemyCount(1).LE.4*EnemyCount(2)) CityProduction(I)=1'4 TO 1 IN FAVOR OF ARMIES
	GOTO 1300
C 
900	CityProduction(I)=1
	N=0
	DO 1000 J=CArmyStart+1,CArmyStart+UpperIndex(9)
	Z=UnitLocation(J)
	IF (Z.EQ.0) GOTO 1000
	IF (IDIST(CityLocation(I),Z).GT.6) GOTO 1000
	IF (EMAP(Z).EQ."t") GOTO 1000		'IF ON TROOP TRANSPORT
	MOVE=PATH(CityLocation(I),Z,1,OKA,FLAG)
	IF (FLAG.EQ.0) GOTO 1000
	N=N+1
1000	CONTINUE
	IF (N.LT.3) GOTO 1300
1050	CityProduction(I)=2
	IF (EnemyCount(2)*2.GT.EnemyCount(9)) GOTO 1100
	IF ((EnemyCount(5).LT.3).AND.(EnemyCount(15).LT.2)) GOTO 1100
	IF (EnemyCount(2)*4.LT.EnemyCount(9)) GOTO 1300
	IF (INT.EQ.2) GOTO 1300
	IF (INT.GT.2) GOTO 1100
	IF (RND(100).LT.50) GOTO 1300
C 
C SELECT A SHIP, GUARANTEEING AT LEAST TWO CITIES PRODUCING TROOP TRANSPORTS
C
1100	CityProduction(I)=PH(8)
	DO 1200 J=8,4,-1
1200	IF (EnemyCount(J+10).GE.EnemyCount(J+9)) CityProduction(I)=PH(J-1)
	IF (EnemyCount(9).GT.(4*EnemyCount(15)+2*EnemyCount(5))) CityProduction(I)=6
	IF (INT.GT.2) CityProduction(I)=INT
	IF (EnemyCount(17).EQ.0) CityProduction(I)=12
	IF (EnemyCount(15).LT.2) CityProduction(I)=6
C 
1300	IF ((EnemyCount(9).GT.1).AND.(EnemyCount(15).EQ.0).AND.(EDGE.GT.0))
	1	CityProduction(I)=6
1400	FOUND(I)=5*CityProduction(I)+MDATE
	IF (INT.EQ.CityProduction(I)) GOTO 1500
	FOUND(I)=6*CityProduction(I)+MDATE
	CALL CITYCT
	IF (DebugLevel.NE.9) GOTO 1500
	CALL LIB$SET_CURSOR(2,1)
	TYPE 999,CityLocation(I),INT,CityProduction(I),EDGE
999	FORMAT("+CITY:",I4," FROM:",I2," TO:",I2," EDGE:",I1,3CityLocation,$)
	CALL GETCHX(E)
1500	RETURN
	END