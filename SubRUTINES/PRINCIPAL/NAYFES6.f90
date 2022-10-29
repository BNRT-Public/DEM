!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!
      SUBROUTINE NAYFES6
!     ==================
!
!  ESTA SUBROTINA ESTIMA AS PROPRIEDADES EQUIVALENTES DAS BARRAS, PARA
!  REPRESENTAR UM MEIO CONTÍNUO, CONSIDERANDO QUE E E GF  TÊM UMA
!  DISTRIBUIÇÃO DE  WEIBULL.
!
!--------------------------------------------------------------------------
      PARAMETER (INN=1.2E6,INB=6E6,IRU=6,INCON=10)
!--------------------------------------------------------------------------
!     DEFINIÇÃO DE VARIÁVEIS E GRUPOS COMUNS
      INCLUDE 'COMMON.FOR'
!--------------------------------------------------------------------------
      INTEGER B,NOI,NOF,LABELN
      DOUBLE PRECISION POA,ALFA,DELT
      DOUBLE PRECISION FYNG,YNGB(INB)
      DOUBLE PRECISION FGFR,GFRB(INB)
      DOUBLE PRECISION UNIF,STA101,BT,GM
	DOUBLE PRECISION RO_MED,DT_CRIT,DT_CMIN,DT_CRIT1,DT_CRIT2
      DOUBLE PRECISION DU,DV,DW,LMIN,PHIE
!--------------------------------------------------------------------------
      POA=1.0D00-PSN**2.0D00
      DELTA=9.0D00*PSN/(4.0D00-8.0D00*PSN)
      ALFA=(9.0D00+8.0D00*DELTA)/(18.0D00+24.0D00*DELTA)
      EPR=RFC*DSQRT(GFR/YNG/POA)
!--------------------------------------------------------------------------
      IF (CVYNG.GE.0.04) THEN
      CALL FPROB(CVYNG,GM,BT)
      BT=YNG/BT
      DO B=1,NBT   
   10 UNIF = RAN0(SEED)
      AUX = 1.0D00-UNIF
	IF(AUX.LT.1.0D-15) GO TO 10
	PHIE=BT*(-LOG(AUX))**(1.0D00/GM)
      YNGB(B)=PHIE
      WRITE(*,*) YNGB(B)
      END DO
      END IF
!
      IF (CVGFR.GE.0.04) THEN
      CALL FPROB(CVGFR,GM,BT)
      BT=GFR/BT
      DO B=1,NBT   
   20 UNIF = RAN0(SEED)
      AUX = 1.0D00-UNIF
	IF(AUX.LT.1.0D-15) GO TO 20
	PHIG=BT*(-LOG(AUX))**(1.0D00/GM)
      GFRB(B)=PHIG
      END DO
      END IF
!--------------------------------------------------------------------------
!     CÁLCULO DA RIGIDEZ DAS BARRAS
!--------------------------------------------------------------------------
      DO B=1,NBT
!     ENR = RIGIDEZ DAS BARRAS NORMAIS (FÓRMULA 3.16 DA TESE DO IGNACIO)
      ENR(B)=YNGB(B)*LI(B)*LI(B)*ALFA
!     EDG = RIGIDEZ DAS BARRAS DIAGONAIS (FÓRMULA 3.16 DA TESE DO IGNACIO)
      EDG(B)=2.0D00*DELTA*(4.0D00/3.0D00)*ENR(B)/DSQRT(3.0D00)
      LCR=2.0D00*CAF*POA/ALFA/RFC/RFC
      !CALL CHE_LCR(KR0,REET,RFC,ALFA,CAF,POA,LCR,LI,B)
      END DO
      DO B=1,NBT
        EPRB(B)=RFC*DSQRT(GFRB(B)/YNGB(B)/POA)
      END DO
!
!     CÁLCULO DO DELTA T CRÍTICO MÍNIMO (PARA VER SE ELE NÃO É MAIOR QUE O MÁXIMO CALCULADO)
      DT_CMIN=1.0E20
      LMIN=1.0D99

      DO B=1,NBT 
      NOI=CN(2*B-1)
      NOF=CN(2*B)
      DU=U(1,NOF)-U(1,NOI)
      DV=V(1,NOF)-V(1,NOI)
      DW=W(1,NOF)-W(1,NOI)
      LI(B)=DSQRT(DU*DU+DV*DV+DW*DW)
!
      IF (LI(B).LT.LMIN) THEN
      LMIN=LI(B)
      END IF

      RO_MED=(ROHN(NOI)+ROHN(NOF))/2.0

	DT_CRIT1=0.6*LCO/(DSQRT(YNGB(B)/RO_MED))
    	DT_CRIT2=LMIN/(DSQRT(YNGB(B)/RO_MED))

      IF (DT_CRIT1.LT.DT_CRIT2) THEN
      DT_CRIT=DT_CRIT1
      ELSE
      DT_CRIT=DT_CRIT2
      END IF

	IF(DT_CRIT.LT.DT_CMIN)THEN 
	   DT_CMIN=DT_CRIT
	END IF
	END DO
!
      IF (DT_CMIN.LT.DT) THEN
      WRITE (*,*)"DT_CMIN=",DT_CMIN,"DT=",DT
      PAUSE
      STOP
      END IF
!
	RETURN
      END
!
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------