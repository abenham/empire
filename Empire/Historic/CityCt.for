	SUBROUTINE CITYCT
C
	INCLUDE "EMPIRE.INC/NOLIST"
C
	EnemyCount(9)=0
	DO 100 I=11,18
100	EnemyCount(I)=0
	DO 200 I=1,70
	IF (CityOwner(I).NE.2) GOTO 200
	EnemyCount(9)=EnemyCount(9)+1
	IF (CityProduction(I).LE.0) GOTO 200		'HANDLES JUST CAPTURED CITY
	INDEXCityLocation=INDEX(CityProduction(I))
	EnemyCount(INDEXCityLocation)=EnemyCount(INDEXCityLocation)+1
200	CONTINUE
C 
C NOW LET EnemyCount(10)=LAST FILLED SLOT IN TargetCity
C
	DO 300 I=70,1,-1
	IF (TargetCity(I).EQ.0) GOTO 300
	EnemyCount(10)=I
	GOTO 400
300	CONTINUE
	EnemyCount(10)=0
400	RETURN
	END