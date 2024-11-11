!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!
      SUBROUTINE NAYFES3
!     ==================
!
!  Esta subrotina estima as propriedades equivalentes das barras, para
!  representar um meio contínuo.
!
!--------------------------------------------------------------------------
      PARAMETER (INN=1.2E6,INB=6E6,IRU=6,INCON=10)
!--------------------------------------------------------------------------
!     DEFINIÇÃO DE VARIÁVEIS E GRUPOS COMUNS
      INCLUDE 'COMMON.FOR'
!--------------------------------------------------------------------------
      INTEGER B,NOI,NOF,LABELN
      DOUBLE PRECISION POA,ALFA,DELT
      DOUBLE PRECISION FYNG,YNGB(inb)
      DOUBLE PRECISION FGFR,GFRB(inb)
      DOUBLE PRECISION UNIF,STA101,BT,GM
      DOUBLE PRECISION Ro_MED,dt_crit,dt_cmin,dt_crit1,dt_crit2
      DOUBLE PRECISION DU,DV,DW,LMIN
!--------------------------------------------------------------------------
      POA=1.0D00-PSN**2.0D00
      DELTA=9.0D00*PSN/(4.0D00-8.0D00*PSN)
      ALFA=(9.0D00+8.0D00*DELTA)/(18.0D00+24.0D00*DELTA)
      EPR=RFC*DSQRT(GFR/YNG/POA)
!--------------------------------------------------------------------------
!     GERAÇÃO DO CAMPO ALEATÓRIO PARA O MÓDULO DE YOUNG
!--------------------------------------------------------------------------
       Write (*,*)"estou dentro da nafes3"
      IF (CVYNG.NE.0.0D00) THEN
!
        CALL INIYNG (SEED,D2,PI,NK1,NK2,NK3,K1,K2,K3,DK1,DK2,DK3,YNG,CVYNG,LCO,PHI1,PHI2,PHI3,PHI4,AN,LABELN)
!
      Write (*,*)"passei a iniyng"
        OPEN (UNIT=15,FILE='2ALEATYNG.DAT')
!
!     SE LABELN = 1 --> GERAÇÃO DE VARIÁVEIS ALEATÓRIAS INDEPENDENTES
!     SE LABELN = 2 --> GERAÇÃO DE VARIÁVEIS ALEATÓRIAS DEPENDENTES
!
        IF (LABELN.EQ.1) THEN
!       GERAÇÃO DE VARIÁVEIS INDEPENDENTES, POIS LCORR < 2.5*LCO (confirmar)
!
          DO B=1,NBT
            UNIF=RAN0(SEED) 
            FYNG=STA101(UNIF,1.0D-50)
            YNGB(B)=YNG+FYNG*CVYNG*YNG
!
          IF (YNGB(B).LT.0.1D00*YNG) THEN
            WRITE (*,*)
            WRITE (*,*) B,'  FOI CORRIGIDO YNG < 0.1*Média  '
            WRITE (*,*)
            YNGB(B) = 0.1D00*YNG
          END IF
!
          IF (YNGB(B).GT.(YNG+2.0D00*CVYNG*YNG)) THEN
            WRITE (*,*)
            WRITE (*,*) B,'  FOI CORRIGIDO YNG > Média + 2*Desvio  '
            WRITE (*,*)
            YNGB(B) = YNG+2.0D00*CVYNG*YNG
          END IF
!
          WRITE (15,'(I5,2(3X,E12.6))') B,FYNG*CVYNG*YNG,YNGB(B)

          WRITE (*,*) B,FYNG*CVYNG*YNG,YNGB(B)

          END DO
!
        CLOSE (15)
!
        ELSE
!       GERAÇÃO DE VARIÁVEIS DEPENDENTES, POIS LCORR > 2.5*LCO (confirmar)
!
          DO B=1,NBT
!
          CALL CAMPOYNG (XB(B),YB(B),ZB(B),D2,PI,NK1,NK2,NK3,K1,K2,K3,     &
     &         DK1,DK2,DK3,PHI1,PHI2,PHI3,PHI4,AN,FYNG)
!
          YNGB(B)=FYNG+YNG
!
          IF (YNGB(B).LT.0.1D00*YNG) THEN
            WRITE (*,*)
            WRITE (*,*) B,'  FOI CORRIGIDO YNG < 0.1*Média  '
            WRITE (*,*)
            YNGB(B) = 0.1D00*YNG
          END IF
!
          IF (YNGB(B).GT.(YNG+2.0D00*CVYNG*YNG)) THEN
            WRITE (*,*)
            WRITE (*,*) B,'  FOI CORRIGIDO YNG > Média + 2*Desvio  '
            WRITE (*,*)
            YNGB(B) = YNG+2.0D00*CVYNG*YNG
          END IF
!
          WRITE (15,'(I5,2(3X,E12.6))') B,FYNG,YNGB(B)
!
          END DO
!
        CLOSE (15)
!    
        END IF
!
      ELSE
!
        DO B=1,NBT
          YNGB(B)=YNG
        END DO
!
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
      END DO
!
!--------------------------------------------------------------------------
!     GERAÇÃO DO CAMPO ALEATÓRIO PARA A ENERGIA ESPECÍFICA DE FRATURA
!--------------------------------------------------------------------------
      IF (CVGFR.NE.0.0D00) THEN
!
        CALL INIGFR (SEED,D2,PI,NK1,NK2,NK3,K1,K2,K3,DK1,DK2,DK3,        &
     &               GFR,CVGFR,LCO,PHI1,PHI2,PHI3,PHI4,AN,LABELN)
!
        OPEN (UNIT=17,FILE='4ALEATGFR.DAT')
!
!     SE LABELN = 1 --> GERAÇÃO DE VARIÁVEIS ALEATÓRIAS INDEPENDENTES
!     SE LABELN = 2 --> GERAÇÃO DE VARIÁVEIS ALEATÓRIAS DEPENDENTES
!
        IF (LABELN.EQ.1) THEN
!       GERAÇÃO DE VARIÁVEIS INDEPENDENTES, POIS LCORR < 2.5*LCO (confirmar)
!
          DO B=1,NBT
!!!!!!!!Comentar isto se os campos de YNG e GFR tem Corr=1
            UNIF=RAN0(SEED)
            FGFR=STA101(UNIF,1.0D-50)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!Descomentar isto se os campos de YNG e GFR tem Corr=0
!            FGFR=(YNGB(B)-YNG)/(FYNG*CVYNG*YNG)     
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            GFRB(B)=GFR+FGFR*CVGFR*GFR
!
          IF (GFRB(B).LT.0.1D00*GFR) THEN
            WRITE (*,*)
            WRITE (*,*) B,'  FOI CORRIGIDO GFR < 0.1*Média  '
            WRITE (*,*)
            GFRB(B) = 0.1D00*GFR
          END IF
!
          WRITE (17,'(I5,2(3X,E12.6))') B,FGFR*CVGFR*GFR,GFRB(B)
!
          END DO
!
          CLOSE (17)
!
        ELSE
!       GERAÇÃO DE VARIÁVEIS DEPENDENTES, POIS LCORR > 2.5*LCO (confirmar)
!
          DO B=1,NBT
!!!!!!!!Comentar isto se os campos de YNG e GFR tem Corr=1
         CALL CAMPOGFR (XB(B),YB(B),ZB(B),D2,PI,NK1,NK2,NK3,K1,K2,K3,    &
     &                   DK1,DK2,DK3,PHI1,PHI2,PHI3,PHI4,AN,FGFR)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!Descomentar isto se os campos de YNG e GFR tem Corr=0
!         FGFR=((YNGB(B)-YNG)/(FYNG))*CVGFR*GFR     
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         GFRB(B)=FGFR+GFR
! 
          IF (GFRB(B).LT.0.1D00*GFR) THEN
            WRITE (*,*)
            WRITE (*,*) B,'  FOI CORRIGIDO GFR < 0.1*Média  '
            WRITE (*,*)
            GFRB(B) = 0.1D00*GFR
          END IF
!
          WRITE (17,'(I5,2(3X,E12.6))') B,FGFR,GFRB(B)
!
          END DO
!
          CLOSE (17)
!
        END IF
!
      ELSE
!
        DO B=1,NBT
          GFRB(B)=GFR
        END DO
!
      END IF
!
      DO B=1,NBT
        EPRB(B)=RFC*DSQRT(GFRB(B)/YNGB(B)/POA)
      END DO
!
!     Cálculo do delta t crítico mínimo (para ver se ele não é maior que o máximo calculado)
      dt_cmin=1.0e20
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

      Ro_MED=(ROHN(NOI)+ROHN(NOF))/2.0

	dt_crit1=0.6*lco/(dsqrt(yngb(b)/ro_med))
    	dt_crit2=LMIN/(dsqrt(yngb(b)/ro_med))

      IF (dt_crit1.LT.dt_crit2) THEN
      dt_crit=dt_crit1
      ELSE
      dt_crit=dt_crit2
      END IF

	IF(dt_crit.LT.dt_cmin)then 
	   dt_cmin=dt_crit
	END IF
	END DO
! 
      IF (dt_cmin.LT.dt) THEN
      WRITE (*,*)"dt_cmin=",dt_cmin,"dt=",dt
      PAUSE
      STOP
      END IF
! 
      RETURN
      END
!
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------