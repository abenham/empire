C 
	SUBROUTINE SECTOR(AMAP)
C
C THIS SUBROUTINE DISPLAY SECTOR JECTOR FROM MAP II
C  IF ISEC=JECTOR, MAP WILL NOT BE DISPLAYED AGAIN
C
	INCLUDE "EMPIRE.INC/NOLIST"
	BYTE AMAP(6000)
C
	IF (JECTOR.EQ.-1) GOTO 200
	IF (MODE.NE.1) RETURN
	IF (ISEC.NE.JECTOR) GOTO 100
	IF (NEWRND.EQ.1) GOTO 1300
	RETURN
100	ISEC=JECTOR
	GOTO 300
200	CALL STROUT("Sector?",12)
	JECTOR=ICityProduction(GETCHX())
	IF ((JECTOR.LT.0).OR.(JECTOR.GT.9)) GOTO 200
	ISEC=JECTOR
	JECTOR=-1		'LET MAIN KNOW THAT UPDATING SECTOR ISNT USED
300	CALL LIB$GERASE_PAGE(1,1)
	IF (GIGI) CALL REGIS_ON
	LINE=KLINE(KI,ISEC)
	LINEFI=LINE+2000	'LINEFI=LINE AFTER LAST LINE OF SECTOR
	LINEC=LINE-100		'GET SET FOR LINE 400
400	LINEC=LINEC+100		'GOTO NECityLocationT LINE
	IF (LINEC.GE.LINEFI) GOTO 1000 'CHECK FOR END OF SECTOR
	KSTART=KI+1		'IF LINE IS BROKEN, KSTART WILL BE MODIFIED
500	DO 600 J=KSTART,KI+SWIDTH 'KI ITSELF IS NOT IN SECTOR
	AB=AMAP(J+LINEC)	'GET CHARACTER
600	IF (AB.NE." ") GOTO 700	'FIND FIRST NON-BLANK SPOT
	GOTO 400		'NO CHARACTERS IN THIS LINE
700	KINIT=J			'AB IS ALREADY CALCULATED
	G2(J)=AB		'AVOIDS REPITITION
	DO 800 J=KINIT+1,KI+SWIDTH 'LOOK FOR BLANK CHARACTER
	AB=AMAP(J+LINEC)	'GET CHARACTER
	IF (AB.EQ." ") GOTO 900	'ECityLocationIT LOOP IF BLANK
800	G2(J)=AB		'PUT CHAR. STRING IN AN ARRAY
900	KFINAL=J-1		'SET END  OF CHAR. STRING
	CALL CURSOR(KINIT-LINE+LINEC-KI+300)	'POSITION CURSOR
	IF (.NOT.GIGI) GOTO 970
	j=kinit
905	ab=g2(j)
	if (ab.eq."+") goto 910
	if (ab.eq.".") goto 920
	goto 950
910	color=4	'green
	goto 930
920	color=5 'cyan
	ab=" "
930	do 940 i=j+1,kfinal
	if (g2(i).ne.g2(j)) goto 945
940	continue
	i=kfinal+1
945	type 995,"T(W(I",48+COLOR,")(N1)S[12])"",(ab,j=j,i-1),""T(S[9])"
995	format("+",$,100(A))
	j=i-1
	goto 980
950	color=7 'WHITE
	if (ab.ge."a".and.ab.le."z") color=2
	if (ab.eq."X") color=2
	if (ab.eq."*") color=6
	type 995,"T(W(I",48+COLOR,"))"",ab,"""
980	j=j+1
	if (j.le.kfinal) goto 905
	GOTO 990
970	TYPE 999,(G2(J),J=KINIT,KFINAL)
999	FORMAT("+",<KFINAL-KINIT+1>A1,$)
990	IF (KFINAL.GE.KI+SWIDTH) GOTO 400 'NECityLocationT LINE
	KSTART=KFINAL+1		'LOOK AT REST OF LINE
	GOTO 500

1000	KURSOR=2300
	DO 1100 I=KI,KI+SWIDTH,SWIDTH/7	'PRINT CityLocation COORDINATES
	CALL CURSOR(KURSOR)
	CALL GDECPRT(I)
	KURSOR=KURSOR+SWIDTH/7
1100	CONTINUE
C 
	KURSOR=302+SWIDTH
	IF (GIGI) KURSOR=KURSOR-1
	DO 1200 I=LINE/100,LINE/100+19,2	'PRINT Y COORDINATES
	CALL CURSOR(KURSOR)
	CALL GDECPRT(I)
	KURSOR=KURSOR+200
1200	CONTINUE
C 
	IF (GIGI) THEN
	CALL REGIS_OFF
	SCOL=83
	ELSE
	SCOL=78
	ENDIF
	CALL LIB$SET_CURSOR(5,SCOL)
	TYPE 998,"S",10,8,"e",10,8,"c",10,8,"t",10,8,"o",10,8,"r",10,10,8,
	1 ISEC,10,10,10,8,"R",10,8,"o",10,8,"u",10,8,"n",10,8,"d"
998	FORMAT("+",$,19A1,I1,17A1)
1300	CALL LIB$SET_CURSOR(20,SCOL+1)
	K=0
	DO 1400 I=3,0,-1
	J=MDATE/(10**I)
	IF (K.EQ.0.AND.J.EQ.0) GOTO 1400
	K=1
	TYPE 997,10,8,MOD(J,10)
997	FORMAT("+",$,2A1,I1)
1400	CONTINUE
C
	NEWRND=0
	CALL LIB$SET_CURSOR(1,1)		'SET CURSOR TO BEG. OF SCREEN
	RETURN
	END