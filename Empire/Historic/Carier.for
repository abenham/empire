	SUBROUTINE CARIER
C
C THIS SUBROUTINE HANDLES ENEMY CARRIER MOVES
C 
	INCLUDE "EMPIRE.INC/NOLIST"
C 
	EnemyCount(7)=0
	IF (DebugLevel.EQ.7) TYPE 999
999	FORMAT(" CARRIER CODES")
	OWN1="c"
	MONKEY=0
C 
C BEGIN LOOP
C
	DO 2700 Y=1,UpperIndex(15)
	Z6=UnitLocation(ICA2+Y)
	IF (Z6.EQ.0) GOTO 2700
	DIR=MOD(Y,2)*2-1
	H1=UnitHits(ICA2H+Y)
	IF (ReferenceMap(Z6).EQ."X") H1=H1+1
	IF (H1.GT.8) H1=8
C 
	ORIG=Z6
	DO 2600 TURN=1,2
	IF ((TURN.EQ.2).AND.(H1.LE.4)) GOTO 2700	'MOVE AT 1/2 SPEED
	P="NS"
	N=0
	Z7=Z6
	AB=ReferenceMap(Z6)
	IF ((AB.NE."c").AND.(AB.NE."X")) GOTO 1800
C 
C MOVE SELECTION
C
	IFO=UnitAction(Y+ICA2-1500)
	ILA=UnitActionTarget(Y+ICA2-1500)
	IF (H1.EQ.8) GOTO 100
	IF ((IFO.EQ.8).AND.(ReferenceMap(ILA).EQ."X")) GOTO 1300
	IFO=8
	ILA=IPORT(Z6)
	GOTO 1300
C 
C IFO=7: RANDOM DIRECTION
C IFO=6: HEADING TOWARDS STATION
C IFO=8: DAMAGED
C IFO=9: STATIONED
C 
C DOES A NEW CODE NEED TO BE SELETED? 800:YES, 1300:NO
C
100	GOTO (200,300,400,500) IFO-5
	GOTO 800
C 
200	GOTO 1300
C 
300	GOTO 800
C
400	GOTO 800
C 
500	DO 600 I=1,70
	IF (TargetCity(I).EQ.0) GOTO 600
	IF ((EMAP(TargetCity(I)).EQ."O").AND.(IDIST(Z6,TargetCity(I)).LE.10))
	1	 GOTO 1300
600	CONTINUE
	DO 700 I=1,10
700	IF (IDIST(Z6,Sightings(I,2)).LE.10) GOTO 1300
	GOTO 800
C 
C NEW CODE SELECTION
C 
800	DO 1200 J=1,10
	IF (Sightings(J,2).EQ.0) GOTO 1200
	LOC=Sightings(J,2)
	KDORK=0
	ID=500
	DO 900 K=1,70
	IF (CityOwner(K).NE.2) GOTO 900
	IF (IDIST(CityLocation(K),LOC).GE.ID) GOTO 900
	ID=IDIST(CityLocation(K),LOC)
	IF (ID.LT.10) GOTO 1200
	KDORK=CityLocation(K)
900	CONTINUE
	DO 1000 K=ICA2+1,ICA2+UpperIndex(15)
	IS=UnitLocation(K)
	IF (IS.EQ.0) GOTO 1000
	IF (IDIST(IS,LOC).GE.ID) GOTO 1000
	IF (UnitAction(K-1500).NE.9) GOTO 1000
	ID=IDIST(IS,LOC)
	IF (ID.LT.10) GOTO 1200
	KDORK=IS
1000	CONTINUE
	IF (KDORK.EQ.0) GOTO 1200
1100	IF (IDIST(KDORK,LOC).LT.1) GOTO 1200
	LOC=LOC+MoveOffset(MOV(LOC,KDORK)+1)	'**
	IF (IDIST(KDORK,LOC).GT.19) GOTO 1100
	AD=EMAP(LOC)
	IF ((AD.NE." ").AND.(AD.NE.".")) GOTO 1100
	IFO=6
	ILA=LOC
	GOTO 1300
1200	CONTINUE
C 
C RANDOM DIRECTION SELECTION
C
	IF (IFO.EQ.7) GOTO 1300
	IFO=7
	KDORK=0
	ILA=RND(8)+1				'**
C 
C NOW PICK THE MOVE SPECIFIED BY IFO AND ILA
C
1300	IF (IFO.EQ.8) GOTO 1500
	IF (IFO.NE.7) GOTO 1400
	MOVE=ILA
	 GOTO 1700
1400	IF (IFO.NE.6) GOTO 1600
	IF (ILA.NE.Z6) GOTO 1500
	IFO=9
	GOTO 1600
1500	MOVE=PATH(Z6,ILA,DIR,OKC,FLAG)
	GOTO 1700
1600	IF (Z6.NE.ILA) MOVE=MOV(Z6,ILA)
	IF (Z6.EQ.ILA) MOVE=RND(8)+1		'**
C 
C MOVE CORRECTION
C
1700	AGGR=0
	IF ((EnemyCount(7).GT.3).AND.(IFO.NE.9)) AGGR=5
	ECityLocationPLOR=1
	IF (H1.LT.6) ECityLocationPLOR=0
	MOVE=MOVCOR(IFO,TURN,Z6,MOVE,H1,AGGR,"c",ECityLocationPLOR,DIR,-1,ORIG,8)
	IF (IFO.EQ.7) ILA=IABS(MOVE)
	UnitAction(Y+ICA2-1500)=IFO
	UnitActionTarget(Y+ICA2-1500)=ILA
	IF (DebugLevel.EQ.7) TYPE 998,IFO,ILA
998	FORMAT(1X,I)
C 
C MOVE EVALUATION
C
	Z6=Z6+MoveOffset(IABS(MOVE)+1)
	IF (MasterMap(Z7).NE."*") ReferenceMap(Z7)=MasterMap(Z7)
	AB=ReferenceMap(Z6)
	IF (AB.EQ.".") GOTO 2000
	IF (AB.EQ."X") GOTO 2100
	IF ((AB.GE."A").AND.(AB.LE."T")) GOTO 1900
	TYPE 997,OWN1,Z6,AB
997	FORMAT(" ENEMY ",A1," AT ",I4," RAN AGROUND ON ",A1)
1800	H1=0
	GOTO 2200
1900	H2=30
	P="SE"
	OWN2=AB
	CALL FIND(OWN2,Z6,Z8,H2)
	CALL FGHT(Z6,H1,H2,"c",OWN2)
	CALL FIND(OWN2,Z6,Z8,H2)
	IF (H1.LE.0) GOTO 2200
2000	ReferenceMap(Z6)=OWN1
2100	UnitLocation(Y+ICA2)=Z6
	UnitHits(Y+ICA2H)=H1
	IF (TURN.EQ.1) EnemyCount(7)=EnemyCount(7)+1
2200	N=0
	IF (P.EQ."SE") CALL SENSOR(Z6)
	DO 2300 I=1,UpperIndex(10)
	IF (Z7.NE.UnitLocation(I+2000)) GOTO 2300
	IF (N+1.GT.H1) THEN
	  IF (ReferenceMap(Z7).NE."X") UnitLocation(I+2000)=0
	  GOTO 2300
	ENDIF
	N=N+1
	UnitLocation(I+2000)=Z6
2300	CONTINUE
	IF (H1.LE.0) GOTO 2400
	MONKEY=Y
	GOTO 2500
2400	UnitLocation(Y+ICA2)=0
	UnitAction(Y+ICA2-1500)=0
	UnitActionTarget(Y+ICA2-1500)=0
	UnitHits(ICA2H+Y)=0
2500	CALL SONAR(Z6)
2600	CONTINUE
2700	CONTINUE
	UpperIndex(15)=MONKEY
	RETURN
	END