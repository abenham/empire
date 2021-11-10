	SUBROUTINE SHIPMV (ACRALOC,ACRAHIT,NUM,OWN1,HITMAX)
C
C THIS SUBROUTINE HANDLES PLAYER"S SHIP MOVES
C
	INCLUDE "EMPIRE.INC/NOLIST"
C 
	DO 2600 Y=1,UpperIndex(NUM)
	LOC=ACRALOC+Y
	IF (MOVEDFLAG(LOC).NE.0) GOTO 2600
	Z6=UnitLocation(LOC)
	IF (Z6.EQ.0) GOTO 2600
	IF ((MODE.EQ.1).AND.(POSCHK(Z6,OWN1).EQ.0)) GOTO 2600
	MOVEDFLAG(LOC)=1

	DO 2500 ITURN=1,2
	LOC=ACRALOC+Y
	Z6=UnitLocation(LOC)
	IF (Z6.EQ.0) GOTO 2600
	JIT=ACRAHIT+Y
	H1=UnitHits(JIT)
	IF ((ITURN.EQ.2).AND.(H1.LE.HITMAX/2)) GOTO 2600
	Z7=Z6
	AB=ReferenceMap(Z6)
C 
C CHECK TO SEE IF SHIP WAS DESTROYED (IF THE CITY IT WAS IN WAS CAPTURED).
C
	IF ((AB.EQ.OWN1).OR.(AB.EQ."O")) GOTO 100
	CALL HEAD(OWN1,Y,LOC,Z6,H1)
	CALL STROUT(" Destroyed.",1)
	GOTO 1500
C 
100	IF ((ITURN.EQ.1).AND.(AB.EQ."O")) H1=H1+1	'REPAIR IF IN PORT
	IF (H1.GT.HITMAX) H1=HITMAX
	CALL STASIS(Z6,LOC)
200	MYCOD=MYCODE(LOC)			'GET MY FUNCTION CODE
	IF (MYCOD.EQ.0) GOTO 900		'IF ZERO, SKIP AHEAD
	IF ((MYCOD.NE.9997).OR.((OWN1.NE."T").AND.(OWN1.NE."C")))
	1	GOTO 500		'CHECK Transports and Carriers
	N=0				'FOR OVERLOADING
	NT=2
	IA=1
	IB=UpperIndex(1)
	IF (OWN1.NE."C") GOTO 300
	NT=1
	IA=501
	IB=UpperIndex(2)+500
300	DO 400 J=IA,IB
400	IF (UnitLocation(J).EQ.Z6) N=N+1
	IF (N.LT.NT*H1) GOTO 500
	MYCODE(LOC)=0
	GOTO 900

500	IF ((MYCOD.LT.101).OR.(MYCOD.GT.6108)) GOTO 1100
	IF (MYCOD.LE.6000) GOTO 600
	IF (MYCOD.GT.6100) GOTO 700
	GOTO 1100

600	Z6=Z6+MoveOffset(MOV(Z6,MYCOD)+1)		'DESTINATION MOVE
	GOTO 800

700	Z6=Z6+MoveOffset(MYCOD-6100+1)		'DIRECTIONAL MOVE
800	AD=ReferenceMap(Z6)
	IF (((AD.EQ.".").OR.(AD.EQ."O")).AND.(ORDER(Z6).EQ.0)) GOTO 1100
	Z6=Z7
900	CALL SECTOR(PlayerMap(1))
1000	CALL LTR(Z6,ITURN)
	CALL MVE(OWN1,MDATE,Y,LOC,JIT,Z6,Z7,DISAS,Z6-IADJST)
	IF (DISAS.EQ.-2) GOTO 200
C 
C MOVE EVALUATION. Z6=TO, Z7=FROM, CHECK OUT NEW LOCATION
C
1100	IF (MasterMap(Z7).NE."*") ReferenceMap(Z7)=MasterMap(Z7)	'REMOVE UNIT FROM MAP
	AC=ReferenceMap(Z6)
	AO=MasterMap(Z6)
	IF (Z6.EQ.MYCODE(LOC)) MYCODE(LOC)=0	'ARRIVED AT DESTINATION
	IF (AC.NE."O") GOTO 1200		'IS IT OUR CITY?
	CALL LIB$SET_CURSOR(3,1)		'DOCKED IN CITY
	CALL STROUT(" Ship is docked.",1)
	GOTO 1800

1200	IF (AO.EQ.".") GOTO 1600 		'IF SEA, SKIP AHEAD
1300	IF (.NOT. FATAL(4)) GOTO 2700
	IF ((AC.NE."+").AND.(AO.NE."*")) GOTO 2400 'CHECK FOR ENEMY TO FIGHT
1400	CALL LIB$SET_CURSOR(2,1)
	CALL IDEN(OWN1)
	CALL STROUT("broke up on the shore.",1)
	GOTO 1500

1600	IF (AC.NE.".") GOTO 2400		'
	ReferenceMap(Z6)=OWN1				'NORMAL MOVE
1800	UnitLocation(LOC)=Z6
	UnitHits(JIT)=H1

1900	IF ((OWN1.NE."T").AND.(OWN1.NE."C")) GOTO 2500
	N=0			'IF WE"RE CARRING SOMETHING, BRING IT ALONG
	IA=0			'SET UP FOR TRANSPORT
	IB=UpperIndex(1)
	NT=2
	IF (OWN1.NE."C") GOTO 2000
	IA=500			'SET UP FOR CARRIER
	IB=UpperIndex(2)
	NT=1
2000	DO 2300 I=IA+1,IA+IB		'FIND PIECES AND MOVE THEM
	IF (UnitLocation(I).NE.Z7) GOTO 2300
	IF (N+1.GT.NT*H1) GOTO 2050
	UnitLocation(I)=Z6
	N=N+1
	GOTO 2300
2050	IF (ReferenceMap(Z7).EQ."O") GOTO 2300
	UnitLocation(I)=0
	CALL LIB$SET_CURSOR(2,1)
	IF (OWN1.EQ."C") GOTO 2100
	CALL STROUT("Army #",0)
	GOTO 2200
2100	CALL STROUT("Fighter #",0)
2200	CALL DECPRT(I-IA)
	CALL STROUT(" was sunk.",1)
2300	CONTINUE
	GOTO 2500

2400	IF ((AC.GE."A").AND.(AC.LE."T")) THEN
		 IF (.NOT.FATAL(2)) GOTO 2700
		ENDIF
	H2=30				'GOING TO FIGHT ANOTHER UNIT
	OWN2=AC
	CALL FIND(OWN2,Z6,Z8,H2)
	CALL FGHT(Z6,H1,H2,OWN1,OWN2)
	CALL FIND(OWN2,Z6,Z8,H2)
	IF (H1.LE.0) GOTO 1500
	ReferenceMap(Z6)=OWN1			'PUT US ON THE MAP
	IF ((OWN2.GE."a").AND.(OWN2.LE."t")) CALL SONAR(Z6)
	IF (AO.EQ.".") GOTO 1800
	ReferenceMap(Z6)=AO			'WON THE BATTLE, BUT....
	IF ((OWN2.GE."a").AND.(OWN2.LE."t")) CALL SONAR(Z6)
	CALL LIB$SET_CURSOR(2,1)
	CALL STROUT("Your ship successfully clears the enemy from the beach
	1 before, CRUNCH', grounding itself.",1)

1500	UnitLocation(LOC)=0			'KILL MY UNIT
	MYCODE(LOC)=0
	CALL SENSOR(Z6)
	H1=0
	GOTO 1900

2500	CALL SENSOR(Z6)
2600	CONTINUE
	RETURN
C
C RECOVER FROM FATAL MOVES
2700	Z6=Z7			'RESTORE OLD LOCATION
	ReferenceMap(Z6)=AB		'RESTORE MAP
	GOTO 900		'TRY AGAIN

	END