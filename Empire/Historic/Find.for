C 
	SUBROUTINE FIND(OWN,Z6,Z8,H2)
C
C CROSS-REFERENCE SUBROUTINE, IT FINDS DATA ON WHATEVER
C CRAFT IS AT POINT Z6.
C 
	INCLUDE "EMPIRE.INC/NOLIST"
C
	IF (H2.GT.0) GOTO 1100
C 
C NOW WE MUST DESTROY OWN
C FIRST OF ALL, UPDATE TROOPT
C
	ISHP=0
	IF (OWN.EQ."D") ISHP=1
	IF (OWN.EQ."S") ISHP=2
	IF (OWN.EQ."T") ISHP=3
	IF (OWN.EQ."R") ISHP=4
	IF (OWN.EQ."C") ISHP=5
	IF (OWN.EQ."B") ISHP=6
	IF (ISHP.EQ.0) GOTO 200
	DO 100 Z=1,5
100	IF (TROOPT(ISHP,Z).EQ.Z6) TROOPT(ISHP,Z)=0
C 
C  NOW DESTROY THE CRAFT, SET UnitLocation(N)=0
C 
200	IF (OWN.NE."C") GOTO 400
	DO 300 Z=1,200
	IF (UnitLocation(500+Z).NE.Z6) GOTO 300
	UnitLocation(500+Z)=0
	IF (MODE.EQ.1) CALL LIB$SET_CURSOR(2,60)
	TYPE 999,Z
999	FORMAT("+Fighter #"I3" sunk."$)
300	CONTINUE
C 
400	IF (OWN.NE."T") GOTO 600
	DO 500 Z=1,500
	IF (UnitLocation(Z).NE.Z6) GOTO 500
	UnitLocation(Z)=0
	IF (MODE.EQ.1) CALL LIB$SET_CURSOR(2,60)
	TYPE 998,Z
998	FORMAT("+Army #"I3" sunk."$)
500	CONTINUE
C 
600	IF (OWN.NE."t") GOTO 800
	DO 700 Z=1501,2000
700	IF (UnitLocation(Z).EQ.Z6) UnitLocation(Z)=0
C 
800	IF (OWN.NE."c") GOTO 1000
	DO 900 Z=2001,2200
900	IF (UnitLocation(Z).EQ.Z6) UnitLocation(Z)=0
C 
1000	UnitLocation(Z8)=0
	IF ((OWN.GE."a").AND.(OWN.LE."t")) CALL SONAR(Z6)
	IF ((OWN.GE."A").AND.(OWN.LE."T")) CALL SENSOR(Z6)
	RETURN
C 
1100	IF (H2.EQ.30) GOTO 1200
	IF ((OWN.EQ."A").OR.(OWN.EQ."F").OR.(OWN.EQ."a").OR.(OWN.EQ."f"))
	1	 GOTO 1500
	IF ((OWN.GE."A").AND.(OWN.LE."T")) UnitHits(Z8-700)=H2
	IF ((OWN.GE."a").AND.(OWN.LE."t")) UnitHits(Z8-1400)=H2
	GOTO 1500

1200	H2=0
	IA=1
	IF (OWN.EQ."T") IA=1101
	IF (OWN.EQ."C") IA=1301
	IF (OWN.EQ."a") IA=1501
	IF (OWN.EQ."f") IA=2001
	IF (OWN.EQ."t") IA=2601
	IF (OWN.EQ."c") IA=2801
	DO 1300 Z8=IA,3000
1300	IF (UnitLocation(Z8).EQ.Z6) GOTO 1400
	PAUSE " ERROR IN SUBROUTINE FIND, "CONTINUE" TO CONTINUE"
997	FORMAT(" ERROR IN SUB. FIND")
	GOTO 1500
1400	IF ((OWN.EQ."A").OR.(OWN.EQ."F").OR.(OWN.EQ."a").OR.(OWN.EQ."f"))
	1	H2=1
	IF (H2.EQ.1) GOTO 1500
	IF ((OWN.GE."A").AND.(OWN.LE."T")) H2=UnitHits(Z8-700)
	IF ((OWN.GE."a").AND.(OWN.LE."t")) H2=UnitHits(Z8-1400)
1500	RETURN
	END