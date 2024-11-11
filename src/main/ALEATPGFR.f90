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
	DOUBLE PRECISION AUX, BT,GM
    DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  RAND
    
	INTEGER I,J,K,IPT

	!--------------------------------------------------------------------------
	! CREACION DE POLOS EN ENERGIA DE FRACTURA
	!--------------------------------------------------------------------------

	CALL SIZEPOLO(M,N,L,LCO,LCXGFR,LCYGFR,LCZGFR,NPTGFR)

    ALLOCATE(RAND(NPTGFR(4)))
	ALLOCATE(PHIGFR(NPTGFR(4)))
	ALLOCATE(PPGFR(NPTGFR(4),3))
	PPGFR=0.0
    RAND=0.0
    AUX = 0.0D0
    PHIGFR=GFR

	CALL POSPOLO(M,N,L,LCO,LCXGFR,LCYGFR,LCZGFR,NPTGFR,PPGFR)

	IF (CVGFR.GE.0.05) THEN
		CALL FPROB(CVGFR,GM,BT)
		BT=GFR/BT
        
        CALL RAND1(SEED,RAND)

        !$OMP PARALLEL DO PRIVATE(I,AUX) SHARED(NPTGFR,PHIGFR,BT,GM,SEED,RAND)
        DO I=1,NPTGFR(4)
            AUX = 1.0-RAND(I)
            PHIGFR(I)=BT*(-LOG(AUX))**(1/GM)

            IF ((ISNAN(PHIGFR(I))).AND.(PHIGFR(I).LT.0.0))THEN
                !$OMP CRITICAL
                WRITE(*,*) PHIGFR(I), I
                WRITE(*,*) 'ERROR: VALUE GFR IN POLO'
                WRITE(*,*) 'DO YOU WISH TO CONTINUE?'
                PAUSE
                STOP
                !$OMP END CRITICAL
            END IF
        ENDDO
        !$OMP END PARALLEL DO
        
    END IF
    
	RETURN
	END

	!--------------------------------------------------------------------------
	!--------------------------------------------------------------------------