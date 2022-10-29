	!-------------------------------------------------------------------------!
	!-------------------------------------------------------------------------!
	!
	SUBROUTINE ALEATPGFR
	!
	! DATA:  15/09/2019
	! AUTOR: Boris N. Rojo Tanzi
	!
	! CREA POLOS EN EL MODELO PARA LA ENERGIA ESPECIFICA
	!
	!--------------------------------------------------------------------------
	! VARIABLES LOCALES
	!--------------------------------------------------------------------------
	DOUBLE PRECISION AUX,NOR
	DOUBLE PRECISION BT,GM

	INTEGER I,J,K,IPT

	!--------------------------------------------------------------------------
	! CREACION DE POLOS EN ENERGIA DE FRACTURA
	!--------------------------------------------------------------------------

	CALL SIZEPOLO(M1,N1,L1,LCO,LCXGFR,LCYGFR,LCZGFR,NPTGFR)

	ALLOCATE(PHIGFR(NPTGFR(4)))
	ALLOCATE(PPGFR(NPTGFR(4),3))
	PPGFR = 0.0
    PHIGFR=GFR

	CALL POSPOLO(M1,N1,L1,LCO,LCXGFR,LCYGFR,LCZGFR,NPTGFR,PPGFR)

	AUX = 0.0D0
	IF (CVGFR.GE.0.05) THEN
		CALL FPROB(CVGFR,GM,BT)
		BT=GFR/BT

		!$OMP PARALLEL DO PRIVATE(IPT,AUX,NOR) SHARED(PHIGFR,BT,GM,SEED)
		DO IPT=1,NPTGFR(4)
10			AUX = 0.0
			DO WHILE (AUX.LT.1.0D-15)
				NOR = RAN0(SEED)
				AUX=1.0D00-NOR
			END DO
			PHIGFR(IPT)=BT*(-LOG(AUX))**(1/GM)
            
            IF (PHIGFR(IPT).LT.0.0) GO TO 10

			IF (ISNAN( PHIGFR(IPT)))THEN
                !$OMP CRITICAL
				WRITE(*,*)  PHIGFR(IPT), IPT
                WRITE(*,*) 'ERROR: VALUE GFR IN POLO'
				WRITE(*,*) 'DO YOU WISH TO CONTINUE?'
                PAUSE
                !$OMP END CRITICAL
				GO TO 10
			END IF

		END DO
		!$OMP END PARALLEL DO
	END IF

	RETURN
	END

	!--------------------------------------------------------------------------
	!--------------------------------------------------------------------------