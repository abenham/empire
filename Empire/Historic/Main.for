 	PROGRAM EMPIRE
C
C This program is a war game simulation for video terminals.
C The game was originally written outside of Digital, probably a university.
C This version of the game was made runnable on Digital Equipment VACityLocation/VMS
C FORTRAN by conversion from the TOPS-10/20 sources available around fall 1979.
C After debugging it, numerous changes have been made.
C
C Now that you are the proud owner of the source and you are all gung ho
C to do things right, there are a few things you should be aware of.
C Unfortunately, there are many magic numbers controlling how many different
C kinds of units can exist and how many of each, so think well before you
C attempt to add another unit type. Also, "slight changes" to the way the units
C work will typically have a fairly devastating affect on the computers
C strategy.  If you are interested in really hacking this, there is a plenty
C of room for enhanced computer strategy.  As you"ll see, there are some
C very good debugging tools tucked inside, and you will soon discover weak
C points and bugs, that up until you, have remained problems (all the previous
C programmers got lazy or lost interest).  Finally, please be careful with
C the version number and identification at start up to avoid confusion of
C ongoing versions with private copies.  If you make a change don"t remove
C the major version id, but rather add something like (V4.0 site.1 20-JUL-80).
C
	INCLUDE "EMPIRE.INC/NOLIST"
	BYTE ODOR(2)
	EQUIVALENCE (ODOR(1),ORDERS)
C 
	INTEGER*4 BDESC(2),BUFFER(3)
	ECityLocationTERNAL IO$_SETMODE,TT$M_WRAP
C
	integer WIN = 0
	integer NCYCLE = 1
	logical PASS = False
	logical AUTOMV = False
C
C  TEMPORARILY FORCE PROGRAM INTO NOWRAP MODE BECAUSE OF BUG IN TERMINAL DRIVER
C
	CALL SYS$ASSIGN("TT",CHAN,,)
	BDESC(1)=12
	BDESC(2)=%LOC(BUFFER(1))
	CALL SYS$GETCHN(%VAL(CHAN),,BDESC(1),,)
	I=%LOC(TT$M_WRAP)
	BUFFER(3)=(BUFFER(3).OR.I)-I
	CALL SYS$QIOW(,%VAL(CHAN),%VAL(%LOC(IO$_SETMODE)),,,,BUFFER(2),,,,,)
C
C
	CALL LIB$GERASE_PAGE(1,1)
	TYPE 999
999	FORMAT(1X,//////////////)
	TYPE 998
998	FORMAT(" EMPIRE, Version 4.3 27-Aug-81"/)
C	TYPE 997
C997	FORMAT(" Please send bug reports to ELROND::EMPIRE"/)
C	TYPE 996
C996	FORMAT(" Known bugs/restrictions"/
C	1" None"/
C	9)

	CALL STROUT("Detailed directions are in EMPIRE.DOC",1)
C
C	-1/0/1 = RESTORE/START/SAVE GAME
C
	CALL GAME(-1,NUM)	!Try to restore a previous game
C 
C COMMAND LOOP STARTS HERE.
C
100	IF (AUTOMV) GOTO 4200	!Don"t ask if in auto move
	CALL LIB$SET_CURSOR(1,1)
	CALL STROUT("Your orders? ",13)
	ACCEPT 995, ORDERS
995	FORMAT(A2)
C
C 		SPECIAL HACK FOR JE COMMAND
	IF ((ODOR(1).GE."a").AND.(ODOR(1).LE."z")) ODOR(1)=ODOR(1)-32
	IF ((ODOR(2).GE."a").AND.(ODOR(2).LE."z")) ODOR(2)=ODOR(2)-32
	IF ((SPECAL).AND.(ORDERS.EQ."JE")) GOTO 3900
C LOOKUP COMMAND
	DO 200 I=1,20
200	IF (ORDERS.EQ.COMSCN(I)) GOTO 300
	IF (PASS) GOTO 2200
	GOTO 100
C	       M,N,O,S,T,V,P,Y,C, L, H, J, G, R, x, Q , +, A
300	GOTO (400,500,600,700,800,900,1000,1100,1200,1300,
	1 1400,1500,1600,1700,1800,1900,2000,2100) I
	GOTO 100

400	GOTO 4200			!M - MOVE MODE

500	CALL LIB$SET_CURSOR(2,1)		!N - FREE ENEMY MOVES
	CALL STROUT("Number of free enemy moves:",12)
	ACCEPT 993,NCYCLE
	GOTO 5300

600	GOTO 4200			!O - MOVE MODE (SYNOMN FOR M)

700	CALL LIB$GERASE_PAGE(1,1)		!S - CLEAR THE SCREEN
	ISEC=-1
	GOTO 100

800	CALL BLOCK(PlayerMap(1))		!T - PRINT OUT MAP
	GOTO 100

900	CALL GAME(+1,0)			!V - SAVE GAME
	CALL STROUT("Game Saved.",1)
	GOTO 100

1000	CALL SECTOR(PlayerMap(1))		!P - PRINT OUT A SECTOR
	GOTO 100

1100	CALL DIREC			!Y - ERROR MSG
	GOTO 100

1200	GOTO 5200			!C - GIVE ONE FREE ENEMY MOVE

1300	CALL DIREC			!L - ERROR MSG
	GOTO 100

1400	CALL HELP			!H - HELP
	ISEC=-1
	GOTO 100

1500	MODE=1				!J - EDIT MODE
	Z6=0
	CALL EDIT(Z6)
	GOTO 100

1600	IF (GIGI.EQ.0) THEN
	  GIGI=1
	  SWIDTH=56
	  TYPE 9939,27,"PpT[12]",27,"\"
	ELSE
	  GIGI=0
	  SWIDTH=70
	ENDIF
	ISEC=-1
	GOTO 100
9939	FORMAT("+",$,100(A))

1700	CALL LIB$SET_CURSOR(3,40)		!R - DISPLAY ROUND NUMBER
	CALL STROUT(" Round #",2)
	TYPE 994,MDATE
994	FORMAT("+",I4,1X,$)
	GOTO 100

1800	GOTO 100

1900	CALL LIB$SET_CURSOR(2,1)		!Q - QUIT
	CALL STROUT(" QUIT- Are you sure? ",2)
	E=GETCHX()
	IF (E.NE."Y" .AND. E.NE."y") GOTO 100
	CALL LIB$GERASE_PAGE(1,1)
	CALL ECityLocationIT

2000	E=GETCHX()			!+ - TURN ON PASS
	IF (E.EQ."+") PASS=.TRUE.
	IF (E.EQ."-") PASS=.FALSE.	! OR OFF
	GOTO 100

2100	AUTOMV=.TRUE.			!A - TURN ON AUTO MOVE MODE
	GOTO 4200

C 
2200	DO 2300 I=21,40			!DEBUGGING COMMANDS
2300	IF (ORDERS.EQ.COMSCN(I)) GOTO 2400
	GOTO 100
C	       LO,NU,LI,TR,AR,TA,PA,A1,T3,A0,CO,CH,Q0, Q1,JE,CY,ECityLocation
2400	GOTO (2500,2600,2700,2800,2900,3000,3100,3200,3300,
	1 3400,3500,3600,3700,3800,3900,4000,4100) I-20
	GOTO 100

2500	TYPE 986, ((Sightings(I,J),J=1,11),I=1,10) !LO -
	GOTO 100

2600	TYPE 989,NUMBER		!NU -
	GOTO 100

2700	TYPE 991, LIMIT		!LI -
	GOTO 100

2800	TYPE 990,TROOPT		!TR -
	GOTO 100

2900	TYPE 989,ARMTOT		!AR -
	GOTO 100

3000	TYPE 989,TargetCity		!TA -
	ISEC=-1
	GOTO 100

3100	TYPE 988, SUCCES,FAILUR	!PA -
	GOTO 100

3200	CALL BLOCK(ReferenceMap(1))		!A1 - PRINT REFERENCE MAP
	GOTO 100

3300	GOTO 100			!T3 - IGNORED

3400	CALL BLOCK(EMAP(1))		!A0 - PRINT COMPUTER"S MAP
	GOTO 100

3500	ACCEPT 993,I1			!CO -
	ACCEPT 993,I2
993	FORMAT(I)
	TYPE 987, (UnitAction(J),UnitActionTarget(J),J=I1,I1+I2)
	GOTO 100

3600	ACCEPT 985,DebugLevel		!CH - SET DebugLevel VARIABLE
	GOTO 100

3700	ISEC=-1				!Q0 - DISPLAY ENEMY MAP SECTOR
	CALL LIB$SET_CURSOR(1,40)
	CALL STROUT("Sector?",12)
	JECTOR=ICityProduction(GETCHX())
	CALL SECTOR(EMAP(1))
	GOTO 100

3800	ISEC=-1				!Q1 - DISPLAY REFERENCE MAP SECTOR
	CALL LIB$SET_CURSOR(1,40)
	CALL STROUT("Sector?",12)
	JECTOR=ICityProduction(GETCHX())
	CALL SECTOR(ReferenceMap(1))
	GOTO 100

3900	ISEC=-1
	JECTOR=ICityProduction(GETCHX())		!JE - DISPLAY ENEMY SECTOR OF CHOICE
	IF (JECTOR.LT.0.OR.JECTOR.GT.9) GOTO 3900
	CALL SECTOR(EMAP(1))
	ISEC=-1
	GOTO 100

4000	type *,"total player   city rounds=",pcityrounds
	type *,"total computer city rounds=",ccityrounds
	p=0
	c=0
	do 4010 i=1,8
	p=p+producedunits(1,i)*ph(i)
	c=c+producedunits(2,i)*ph(i)
4010	continue
	type *,"total player   production",p
	type *,"total computer production",c
	type 991,((producedunits(i,j),j=1,8),i=1,2)
	ISEC=-1
	GOTO 100			!CY - show running production totals

4100	ECityLocation=EXPL()			!ECityLocation - DISPLY ECityLocationPLORE FUNCTION VALUE
	TYPE 992,ECityLocation
	GOTO 100
C 
992	FORMAT("+ECityLocationP VALUE:",I5$)
991	FORMAT(1X,8I4)
990	FORMAT(1X,5I6)
989	FORMAT(1X,10I5)
988	FORMAT(" SUCCESS:",I6," FAILURE:",I6)
987	FORMAT(1X,10I7)
986	FORMAT(11I5)
985	FORMAT(I)
C 
C BEGIN MOVEMENT
C 
C USER MOVE
C 
4200	IF (MODE.EQ.0) GOTO 4400
	IF (JECTOR.NE.-1) GOTO 4300
	CALL LIB$GERASE_PAGE(1,1)
	JECTOR=0
	ISEC=-1
4300	ISTART=ISEC
	IF (ISEC.LT.0) ISTART=0
4400	DO 4500 I=1,1500
4500	MOVEDFLAG(I)=0
	DO 4550 Y=1,70
	IF (CityOwner(Y).eq.1) CALL SENSOR(CityLocation(Y))
4550	continue
	DO 4700 JECT=ISTART,ISTART+9
	IF (MODE.EQ.0) GOTO 4600
	JECTOR=JECT
	IF (JECT.GT.9) JECTOR=JECT-10
	LINE=KLINE(KI,JECTOR)
	IADJST=LINE+KI-300
4600	CALL SHIPMV(ITT,ITTH,5,"T",3)
	CALL SHIPMV(ICA,ICAH,7,"C",8)
	CALL SHIPMV(IBA,IBAH,8,"B",12)
	CALL SHIPMV(ICR,ICRH,6,"R",8)
	CALL SHIPMV(ISU,ISUH,4,"S",2)
	CALL SHIPMV(IDE,IDEH,3,"D",3)
	CALL ARMYMV
	CALL FIGHMV
	IF (MODE.EQ.0) GOTO 4800
4700	CONTINUE
4800	CONTINUE
C 
C HARDWARE PRODUCTION
C
	DO 5100 Y=1,70
	IF (CityOwner(Y).NE.1) GOTO 5100
c	CALL SENSOR(CityLocation(Y))
	IF (CityProduction(Y).EQ.8) GOTO 4900
	IF ((CityProduction(Y).LT.1).OR.(CityProduction(Y).GT.15)) GOTO 4900
	IF (MOD(CityProduction(Y),2).EQ.0) GOTO 5000
	IF (MOD(CityProduction(Y),5).EQ.0) GOTO 5000
	IF (CityProduction(Y).EQ.1) GOTO 5000
C
C   CITY CityProduction INCORRECT OR WE JUST TOOK IT
C
4900	CALL LIB$GERASE_PAGE(1,1)
	ISEC=-1
	TYPE 984,CityLocation(Y)
984	FORMAT(" Readout around city at",I5)
	I1=MODE
	MODE=0
	CALL LTR(CityLocation(Y),0)
	MODE=I1
	CALL STROUT("What are your production demands for this city? ",13)
	CALL PHASIN(Y,E)
	TYPE 977,E
977	FORMAT ("+",A1,$)
	GOTO 5100
5000	pcityrounds=pcityrounds+1
	IF (MDATE.LT.FOUND(Y)) GOTO 5100
	FOUND(Y)=MDATE+CityProduction(Y)*5
	CALL LIB$SET_CURSOR(3,1)
	CALL STROUT("City #",10)
	CALL DECPRT(Y)
	CALL STROUT(" at",10)
	CALL DECPRT(CityLocation(Y))
	CALL STROUT(" has completed a",0)
	K=CityProduction(Y)
CD	TYPE 983, HITS(K),CityLocation(Y),TIPE(K),CRAHIT(K),CRALOC(K),
CD	1 LOPMAX(K),K
CD983	FORMAT(" HITS:",I5," CityLocation(Y):",I5," TIPE(K):",I5," CRAHIT(K):",I5,/
CD	1 ," CRALOC(K):",I5," LOPMAX(K):",I5," K:",I)
	CALL PROD(HITS(K),CityLocation(Y),UpperIndex(TIPE(K)),CRAHIT(K),CRALOC(K),
	1	LOPMAX(K),UnitTimer,TIPE(K)+1,RANGE)
	producedunits(1,tipe(k))=producedunits(1,tipe(k))+1
5100	CONTINUE
5200	CONTINUE
C 
C COMPUTER MOVE
C 
5300	CONTINUE
D	CALL PME_INIT
	DO 5500 I=1,NCYCLE
C
	CALL ARMCNT
	CALL TROOPM
C
	CALL LIB$SET_CURSOR(1,1)
	CALL STROUT("... My turn, thinking ...",3)
C
	CALL ARMYEN
C 
	CALL LIB$ERASE_LINE(1,1)	'BLANK THE THINKING
C
	CALL CARIER
	CALL ENEMYM("b",12,IBA2H,IBA2,8)
	CALL ENEMYM("r",8,ICR2H,ICR2,6)
	CALL ENEMYM("s",2,ISU2H,ISU2,4)
	CALL ENEMYM("d",3,IDE2H,IDE2,3)
C 
	CALL LIB$SET_CURSOR(1,1)
	CALL STROUT("... My turn, thinking ...",3)
	CALL FIGHTR
C
C AGE KNOWN ENEMY ARMY LOCATIONS
C
	DO 5350 K=1,10
	IF (Sightings(K,1)+21.GT.MDATE) GOTO 5350	'IF DATA IS NOT OLD
	DO 5340 J=1,11
5340	Sightings(K,J)=0				'ZERO THAT LINE
5350	CONTINUE
C 
C PRODUCTION OF ENEMY HARDWARE
C
	CALL CITYCT
	DO 5400 Y=1,70
	IF ((CityLocation(Y).EQ.0).OR.(CityOwner(Y).NE.2)) GOTO 5400
	ccityrounds=ccityrounds+1
	CALL SONAR(CityLocation(Y))
	IF ((CityProduction(Y).LE.0).OR.(MDATE.LT.FOUND(Y))) GOTO 5380
	K=CityProduction(Y)
	J=0
	IF (K.EQ.1) J=1
	CALL PROD(HITS(K),CityLocation(Y),UpperIndex(TIPE(K)+8),CRAHIT(K)+IDE2H,
	1	CRALOC(K)+1500,LOPMAX(K),UnitTimer,J,RANG)
	producedunits(2,tipe(k))=producedunits(2,tipe(k))+1
5380	IF ((CityProduction(Y).LE.0).OR.(MDATE.GE.FOUND(Y))) CALL CITYPH(Y)
5400	CONTINUE
	MDATE=MDATE+1
	NEWRND=1
	IF (MOD(MDATE,4).EQ.0.OR.(MDATE.GT.160)) CALL GAME (+1,0)
5500	CONTINUE
D	CALL PME_ECityLocationIT
	NCYCLE=1
C 
	IF (WIN.EQ.1) GOTO 100
	IF (WIN.EQ.2) GOTO 5700
	N=0
	DO 5600 J=1,70
5600	IF (CityOwner(J).EQ.1) N=N+1
	IF (N.LT.30) GOTO 5700
	IF (EnemyCount(9).GT.N/2) GOTO 5700
	TYPE 982
982	FORMAT(" The computer acknowledges defeat. Do"/
	1	" you wish to smash the rest of the enemy?")
	ACCEPT 981,ORDERS
981	FORMAT(4A1)
	IF ((ORDERS.NE."Y").AND.(ORDERS.NE."y")) CALL ECityLocationIT
	TYPE 980
980	FORMAT(" The enemy inadvertantly revealed its code used for"/
	1	" receiving battle information. You can display what they""ve"/
	1	" learned through the command ""JE""(cr)(lf), followed by the"/
	1	" sector number.")
	SPECAL=.TRUE.
	WIN=2
	AUTOMV=.FALSE.
	GOTO 100
5700	IF ((EnemyCount(9).GT.0).OR.(UpperIndex(9).GT.0)) GOTO 5800
	CALL LIB$GERASE_PAGE(1,1)
	TYPE 979
979	FORMAT(" The enemy is incapable of defeating you."/
	1	" You are free to rape the empire as you wish."/
	1	" There may be, however, remnants of the enemy fleet"/
	1	" to be routed out and destroyed.")
	WIN=1
	AUTOMV=.FALSE.
	GOTO 100
5800	DO 5900 I=1,70
5900	IF (CityOwner(I).EQ.1) GOTO 100
	DO 6000 I=1,UpperIndex(1)
6000	IF (UnitLocation(I).NE.0) GOTO 100
	CALL LIB$GERASE_PAGE(1,1)
	WIN=1
	TYPE 978
978	FORMAT(" You have been rendered incapable of"/
	1	" defeating the rampaging enemy fascists' The"/
	1	" empire is lost. If you have any ships left, you may"/
	1	" attempt to harass enemy shipping.")
	AUTOMV=.FALSE.
	GOTO 100
	END