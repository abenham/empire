	SUBROUTINE ENEMYM(OWN1,HITMAX,ACRAHIT,ACRALOC,NUM)
C
C THIS SUBROUTINE HANDLES ENEMY SHIP MOVES OTHER THAN T"S AND C"S
C
	INCLUDE "EMPIRE.INC/NOLIST"
C 
C NSHPRF IS AN ARRAY WHICH IS REFERENCED TO DETERMINE
C WHETHER A CERTAIN SHIP (D=1,S=2,R=3,B=4) WANTS TO ATTACK
C ANOTHER CERTAIN TYPE OF SHIP. 1 MEANS YES, 0 MEANS NO.
C SECOND VARIABLE: 1=D,2=S,3=T,4=R,5=C,6=B
C
	DATA NSHPRF/1,1,1,0,0,0,1,1,1,0,0,0,1,1,1,1,1,0,1,1,1,1,1,1/
C 
CTHE FOLLOWING NUMBERS ARE IFO VARIABLES RELATING TO
C CERTAIN TYPES OF MOVEMENT (CODES)
C 7: RANDOM DIRECTION
C 3: CITY TargetCity LOC.
C 4: TT NUMBER ESCORT
C 5: TargetCity
C 8: DAMAGED
C 10: LOOK AT UNECityLocationPLORED TERRITORY
C
	IF (NUM.EQ.3) NUMSHP=1
	IF (NUM.EQ.4) NUMSHP=2
	IF (NUM.EQ.6) NUMSHP=3
	IF (NUM.EQ.8) NUMSHP=4
C 
	EnemyCount(NUM)=0
	IF (DebugLevel.EQ.NUM) TYPE 999,OWN1
999	FORMAT(1X,A1," CODES")
	MONKEY=0
C 
	DO 2400 Y=1,UpperIndex(NUM+8)
	Z6=UnitLocation(Y+ACRALOC)
	IF (Z6.EQ.0) GOTO 2400
	DIR=MOD(Y,2)*2-1
	H1=UnitHits(Y+ACRAHIT)
	AB=ReferenceMap(Z6)
	IF (AB.EQ."X") H1=H1+1
	IF (H1.GT.HITMAX) H1=HITMAX
C 
	ORIG=Z6
	DO 2300 ITURN=1,2
	P="NS"
	IF ((ITURN.EQ.2).AND.(H1.LE.HITMAX/2)) GOTO 2400
	Z7=Z6
C 
C MOVE SELECTION
C
	IFO=UnitAction(Y+ACRALOC-1500)
	ILA=UnitActionTarget(Y+ACRALOC-1500)
C
C DOES A NEW CODE NEED TO BE SELECTED? 800:YES, 1600:NO
C
	IF ((IFO.EQ.8).AND.(H1.EQ.HITMAX)) IFO=0
	IF ((IFO.EQ.8).AND.(ReferenceMap(ILA).EQ."X")) GOTO 1600
	IF (H1.EQ.HITMAX) GOTO 100
	IFO=8
	ILA=IPORT(Z6)
	GOTO 1600
100	GOTO (800,200,300,400,500,800,800,800,800,700) IFO
	GOTO 800
C 
200	GOTO 800
C 
300	IF (ReferenceMap(ILA).EQ."X") GOTO 800
	IF (IDIST(Z6,ILA).EQ.1) GOTO 800
	GOTO 1600
C 
400	IF (UnitLocation(2600+ILA).EQ.0) GOTO 800
	IF (UnitAction(1100+ILA).LT.7) GOTO 800
	GOTO 1600
C 
500	IF (ILA.NE.Z6) GOTO 1600
	DO 600 I1=1,6
	DO 600 I2=1,5
	IF (TROOPT(I1,I2).NE.ILA) GOTO 600
	TROOPT(I1,I2)=0
600	CONTINUE
	GOTO 800
C 
700	IF (EMAP(ILA).NE." ") GOTO 800
	GOTO 1600
C 
C NEW CODE SELECTION
C 5:TargetCity
C
800	ID=500
	DO 900 N=1,6
	IF (NSHPRF(NUMSHP,N).EQ.0) GOTO 900
	DO 900 N2=1,5
	IF (TROOPT(N,N2).EQ.0) GOTO 900
	IF (IDIST(Z6,TROOPT(N,N2)).GE.ID) GOTO 900
	ID=IDIST(Z6,TROOPT(N,N2))
	ILA=TROOPT(N,N2)
	IFO=5
900	CONTINUE
	IF (ID.NE.500) GOTO 1600
	IF (RND(100).GT.40) GOTO 1200		'**
C
C 3:CITY TargetCity LOC.
C
	IA=RND(20)+1				'**
	IB=IA+70
	DO 1100 IC=IA,IB
	I=IC
	IF (I.GT.70) I=IC-70
	IF (TargetCity(I).EQ.0) GOTO 1100
	IF (ReferenceMap(TargetCity(I)).NE."O") GOTO 1100
	IF (EDGER(TargetCity(I)).EQ.0) GOTO 1100
	IFO=3
	ILA=TargetCity(I)
	GOTO 1600
1100	CONTINUE
C
C 4:TT NUMBER ESCORT
C
1200	IA=RND(UpperIndex(13))+1			'**
	IB=IA+UpperIndex(13)
	DO 1300 IC=IA,IB
	I=IC
	IF (I.GT.UpperIndex(13)) I=IC-UpperIndex(13)
	IF (UnitLocation(2600+I).EQ.0) GOTO 1300
	IF (UnitAction(1100+I).LT.9) GOTO 1300
	IFO=4
	ILA=I
	GOTO 1600
1300	CONTINUE
C 
C 10: ECityLocationPLORE
C
1400	I1=EXPL()
	IF (I1.EQ.0) GOTO 1500
	ILA=I1
	IFO=10
	GOTO 1600
C 
C 1: RANDOM DIRECTION
C
1500	IF (IFO.EQ.7) GOTO 1600
	ILA=RND(8)+1				'**
	IFO=7
C 
C MOVE CORRECTION
C
1600	IF (IFO.EQ.7) MOOV=ILA
	FLAG=1
	IF ((IFO.EQ.8).OR.(IFO.EQ.3).OR.(IFO.EQ.5))
	1	 MOOV=PATH(Z6,ILA,DIR,OKC,FLAG)
	IF (IFO.EQ.4) MOOV=PATH(Z6,UnitLocation(CTransportStart+ILA),DIR,OKC,FLAG)
	IF (FLAG.EQ.0) GOTO 1400
	IF (IFO.EQ.10) MOOV=PATH(Z6,ILA,DIR,OKC,FLAG)
	IF (FLAG.EQ.0) GOTO 1500
	IF (IFO.NE.2) GOTO 1700
	MOOV=0
	IF (IDIST(Z6,ILA).GT.4) MOOV=MOV(Z6,ILA)
	IF (IDIST(Z6,ILA).LT.4) MOOV=ICORR(MOV(Z6,ILA)-4)
1700	AGGR=0
	MOOV=MOOV*DIR
	ECityLocationPLOR=1
	IF (OWN1.EQ."d") GOTO 1750
	IF (H1.LE.HITMAX/2) ECityLocationPLOR=0
1750	MOOV=
	1 MOVCOR(IFO,ITURN,Z6,MOOV,H1,AGGR,OWN1,ECityLocationPLOR,DIR,-1,ORIG,HITMAX)
	IF (IFO.EQ.7) ILA=IABS(MOOV)
	UnitAction(Y+ACRALOC-1500)=IFO
	UnitActionTarget(Y+ACRALOC-1500)=ILA
	MOOV=IABS(MOOV)
	IF (DebugLevel.EQ.NUM) TYPE 998,IFO,ILA
998	FORMAT(I)
C 
C MOVE EVALUATION
C
	Z6=Z6+MoveOffset(MOOV+1)			'**
	IF (MasterMap(Z7).NE."*") ReferenceMap(Z7)=MasterMap(Z7)
	AD=ReferenceMap(Z6)
	IF (AD.EQ.".") GOTO 1900
	IF (AD.EQ."X") GOTO 2000
	IF ((AD.GE."A").AND.(AD.LE."T")) GOTO 1800
	TYPE 997,OWN1,Z6,AD
997	FORMAT(" ENEMY ",A1," AT ",I4," RAN AGROUND ON ",A1)
	GOTO 2100
1800	H2=30
	P="SE"
	OWN2=AD
	CALL FIND(OWN2,Z6,Z8,H2)
	CALL FGHT(Z6,H1,H2,OWN1,OWN2)
	CALL FIND(OWN2,Z6,Z8,H2)
	IF (H1.LE.0) GOTO 2100
1900	ReferenceMap(Z6)=OWN1
2000	UnitLocation(Y+ACRALOC)=Z6
	UnitHits(Y+ACRAHIT)=H1
	IF (ITURN.EQ.1) EnemyCount(NUM)=EnemyCount(NUM)+1
	MONKEY=Y
	GOTO 2200
2100	UnitLocation(Y+ACRALOC)=0
	UnitAction(Y+ACRALOC-1500)=0
	UnitActionTarget(Y+ACRALOC-1500)=0
	UnitHits(Y+ACRAHIT)=0
2200	CALL SONAR(Z6)
	IF (P.EQ."SE") CALL SENSOR(Z6)
2300	CONTINUE
2400	CONTINUE
	UpperIndex(NUM+8)=MONKEY
	RETURN
	END