C 
	FUNCTION ATTACK(OWN1,OWN2,IH1,AGGR)
	IMPLICIT INTEGER(A-Z)
	BYTE OWN1,OWN2
C
	H1=IH1
	C1=COST(OWN1,H1)
	C2=COST(OWN2,0)
	S1=1
	S2=1
	IF (OWN1.EQ."s") S1=3
	IF (OWN2.EQ."S") S2=3
	H2=HITS(OWN2)
	H1=(H1+S2-1)/S2
	H2=(H2+S1-1)/S1
	ATTACK=(((C2*100)*H1)/H2)-(C1*100)+(AGGR*100)
	RETURN
	END