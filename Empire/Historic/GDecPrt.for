C
	SUBROUTINE GDECPRT(I)
	INCLUDE "EMPIRE.INC/NOLIST"
	IF (.NOT.GIGI) THEN
	 CALL DECPRT(I)
	 RETURN
	ENDIF
	if (i.eq.100) then
	 type 992,I
	else if (i.lt.10) then
	 type 993,I
	else
	 type 994,I
	ENDIF
992	format("+T"",I3,""",$)
993	format("+T"",I1,""",$)
994	format("+T"",I2,""",$)
	END