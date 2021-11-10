C===============================================================================
C COMPAR
C ------
C
C Checks if the specified location is suitable for the path being planned.
C
C Parameters
C ------------------------------------------------------------------------------
C AB		The terrain type being checked
C Z62		The location being checked
C OKVECT	Array containing the permitted terrain types
C ------------------------------------------------------------------------------
C Returns
C	0	Location is bad
C	1	Location is good
C===============================================================================
	FUNCTION COMPAR(AB,Z62,OKVECT)
C
C USED BY PATH, CHECKS IF AB OR LOCATION Z62 IS A TYPE CONTAINED IN OKVECT
C 
	IMPLICIT INTEGER(A-Z)
	BYTE OKVECT(5),AB
	BYTE MasterMap(6000)
	COMMON/MasterMap/MasterMap
C
	COMPAR = 1
	IF (AB .EQ. OKVECT(1)) RETURN
	IF (MasterMap(Z62) .EQ. OKVECT(1)) RETURN
	IF (AB .EQ. OKVECT(2)) RETURN
	IF (AB .EQ. OKVECT(3)) RETURN
	IF (AB .EQ. OKVECT(4)) RETURN
	IF (AB .EQ. OKVECT(5)) RETURN
	COMPAR = 0
	RETURN
	END