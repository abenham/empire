C 
	FUNCTION COST(OWN,H)
	IMPLICIT INTEGER(A-Z)
	INTEGER COSVAL(14)
	BYTE	COSTAB(14),OWN
	DATA COSVAL/0,2,4,6,3,5,4,1,3,3,7,5,11,11/
	DATA COSTAB/"F","D","S","T","R","C","B",
	1			    "f","d","s","t","r","c","b"/
	DO 100 I=1,14
100	IF (OWN.EQ.COSTAB(I)) GOTO 200
	PAUSE "BAD CALL TO FUNCTION COST'"
	COST=0
	RETURN
200	COST=COSVAL(I)
	IF (I.GE.9)COST=COST-H
	RETURN
	END