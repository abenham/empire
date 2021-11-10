C BLOCK DATA FOR EMPIRE
C
	SUBROUTINE BLOCK_DATA_EMPDAT
C
	INCLUDE "EMPIRE.INC/LIST"
C
	DATA COMMAN/"S","R","I","K","O","L","F","G","P","H",
	1	 "Y","T","V","J","?",0,0,"U","N","+"/

	DATA COMM/"D","E","W","Q","A","Z","X","C","S",
	1	 "L","B","F","T","G","V","J","U",-1,-1,
	2	 "O","P","R","I","M","K","N",
	3	 "S","?","Y","H"/

	DATA IOTAB/0,500,700,900,1100,1200,1300,1400,1500,2000,
	1	 2200,2400,2600,2700,2800,2900/

	DATA OVRPOP/
	1	 9,001,002,9,003,004,05,9,9,9,06,9,07,9,9,08,
	2	 0,499,199,0,199,199,83,0,0,0,99,0,99,0,0,99/

	DATA COMSCN/"M","N","O","S","T","V","P","Y","C","L","H","J",
	1	"G","R","0","Q","+","A","0",0,
	2	"LO","NU","LI","TR","AR","TA","PA","A1",
	3	"T3","A0","CO","CH","Q0","Q1","JE","CY","ECityLocation",0,0,0/

	DATA ARROW/-101,-100,-99,-1,0,1,99,100,101/
	DATA CMYTBL/6104,6103,6102,6105,6101,6106,6107,6108,0/
	DATA CRAHIT/0,0,0,0, 200, 400,0,0,0, 500,0, 600,0,0, 700/
	DATA CRALOC/0,500,0,700,900,1100,0,0,0,1200,0,1300,0,0,1400/
	DATA HITS/1,1,0,3,2,3,0,0,0,8,0,8,0,0,12/
	DATA MoveOffset/0,1,-99,-100,-101,-1,99,100,101,0/
	DATA INDEX/11,12,0,13,14,15,0,0,0,16,0,17,0,0,18/
	DATA KBFUDG/-101,-100,-99,-1,1,99,100,101,0/
	DATA KBTBL/"Q","W","E","A","D","Z","X","C"," "/
	DATA LOPMAX/500,200,0,200,200,100,0,0,0,100,0,100,0,0,100/
	DATA OKA/"+"," ","*","X","O"/
	DATA EnemyArmyTerrain/"+"," ","O","t","*"/
	DATA OKC/"."," ","O","*","X"/
	DATA PH/1,2,4,5,6,10,12,15/
	DATA PHAZE/"A","F","D","S","T","R","C","B"/
	DATA STEP/37/,POSIT/65/,START/102/,GIGI/0/,SWIDTH/70/
	DATA TIPE/1,2,0,3,4,5,0,0,0,6,0,7,0,0,8/

	END
