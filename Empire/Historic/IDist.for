C==============================================================================
C IDIST
C ------
C Returns the distance between two points
C
C Parameters
C -----------------------------------------------------------------------------
C N1	Start location
C N2	End location
C -----------------------------------------------------------------------------
C
C Returns distance between points
C==============================================================================
	FUNCTION IDIST(N1,N2)
C
C RETURN DISTANCE BETWEEN LOCATION N1 AND N2
C
	IMPLICIT INTEGER(A-Z)
C
	CityLocation1=IABS(MOD(N1-1,100)-MOD(N2-1,100))
	Y1=IABS(((N1-1)/100)-((N2-1)/100))
	IDIST=MACityLocation0(CityLocation1,Y1)
	RETURN
	END