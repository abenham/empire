	SUBROUTINE SENSOR(Location)
C
C UPDATES PLAYER"S MAP AROUND LOCATION Location 
C	AND SCREEN IF CURRENT SECTOR IS DISPLAYED
C
	INCLUDE "EMPIRE.INC/NOLIST"
C
	int Adjacent	' Was I1
	int Location	' Was Z6
	byte Terrain	' Was AB
	int I

	IBEFOR=-100

C
C Examine each adjacent location
C
	DO 100 I=1,9

C
C Get the location, and terrain at that location from the reference map
C
	Adjacent=Location+ARROW(I)
	Terrain=ReferenceMap(Adjacent)
C
C If the player and reference maps match, nothing to do, next location
C
	IF (Terrain.EQ.PlayerMap(Adjacent)) GOTO 100

C
C Update the player's map
C
	PlayerMap(Adjacent)=Terrain

	IF (JECTOR.EQ.-1) GOTO 100
	IF (ISEC.EQ.-1) GOTO 100
	LINE=KLINE(KI,ISEC)
	IY=(Adjacent-1)/100*100
	IX=Adjacent-IY
	IF ((IY.LT.LINE).OR.(IY.GT.LINE+1900)
	1	.OR.(IX.LE.KI).OR.(IX.GT.KI+SWIDTH)) GOTO 100
	Adjacent=Adjacent-LINE-KI
C	IF (GIGI.AND.(IBEFOR.EQ.-100)) CALL REGIS_ON
	IF (IBEFOR+1.NE.Adjacent) CALL CURSOR(Adjacent+300)
	IBEFOR=Adjacent

C-------------------------------------------------------------------------------
C Ignore anything to do with GiGi!
C	IF (.NOT.GIGI) GOTO 200
C	color=7 'WHITE
C	if (ab.eq."+") color=4
C	if (ab.eq.".") color=5
C	if (ab.ge."a".and.ab.le."z") color=2
C	if (ab.eq."X") color=2
C	if (ab.eq."*") color=6
C	type 995,"T(W(I0)(N0)(R)S[12])" ""
C	CALL CURSOR(Adjacent+300)	'POSITION CURSOR
C	if (color.eq.4.or.color.eq.5) then
C	if (ab.eq.".") ab=" "
C	type 995,"T(W(I",48+COLOR,")(N1)S[12])"",ab,""T(S[9])"
C	else
C	type 995,"T(W(I",48+COLOR,")(N0)(R)S[9])"",ab,"""
C	end if
C	GOTO 100
C995	format("+",$,100(A))
C200	TYPE 999,Terrain
999	FORMAT("+",A1$)
C-------------------------------------------------------------------------------

100	CONTINUE
	IF (IBEFOR.EQ.-100) RETURN
C	IF (GIGI) CALL REGIS_OFF
	END