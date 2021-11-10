C==============================================================================
C ICORR
C ------
C Ensures that a direction value is between 1 and 8. This means that the 9th
C possible value (no movement) is never used
C
C Parameters
C -----------------------------------------------------------------------------
C N		The value to check
C -----------------------------------------------------------------------------
C
C Returns	A valid value between 1 and 8
C==============================================================================
	FUNCTION ICORR(N)
	IMPLICIT INTEGER(A-Z)
C
	ICORR=N
	IF (ICORR.GT.8) ICORR=ICORR-8
	IF (ICORR.LT.1) ICORR=ICORR+8
	RETURN
	END
