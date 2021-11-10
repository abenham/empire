C==============================================================================
C ORDER
C ------
C Returns 0 if the specified location is on the map, and 1 if the location is
C off the map
C
C Parameters
C -----------------------------------------------------------------------------
C I6	Location
C -----------------------------------------------------------------------------
C
C Returns as described above
C==============================================================================
	FUNCTION ORDER(I6)
C
C RETURN =1 IF OFF THE EDGE OF THE MAP
C
	IMPLICIT INTEGER(A-Z)
C
	ORDER=1
	IF ((I6.LE.101).OR.(I6.GE.5900)) RETURN
	IF (MOD(I6,100).LE.1) RETURN
	ORDER=0
	RETURN
	END