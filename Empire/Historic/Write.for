C 
	SUBROUTINE WRITE(BEG,LIM,NUM)
C
	INCLUDE "EMPIRE.INC/NOLIST"
C
	DO 100 J=BEG+1,BEG+LIM
	K=UnitLocation(J)
	WRITE(1) K
	IF (NUM.LT.9) WRITE(1) MYCODE(J)
	IF (NUM.GT.8) WRITE(1) UnitAction(J-1500),UnitActionTarget(J-1500)
	IF (NUM.EQ.9) WRITE(1) UnitTimer(J-1500)
	IF (NUM.EQ.2) WRITE(1) RANGE(J-500)
	IF (NUM.EQ.10) WRITE(1) RANG(J-2000)
100	CONTINUE
	RETURN
	END