 	FUNCTION IPORT(Z6)
C
	INCLUDE "EMPIRE.INC/NOLIST"
C
	IPORT=0
	ID=500
	DO 100 I=1,70
	IF (CityLocation(I).EQ.0) GOTO 100
	IF (ReferenceMap(CityLocation(I)).NE."X") GOTO 100
	IF (EDGER(CityLocation(I)).EQ.0) GOTO 100
	IF (IDIST(CityLocation(I),Z6).GE.ID) GOTO 100
	IPORT=CityLocation(I)
	ID=IDIST(CityLocation(I),Z6)
100	CONTINUE
	IF (IPORT.NE.0) RETURN
	IPORT=RND(5798)+102
	RETURN
	END