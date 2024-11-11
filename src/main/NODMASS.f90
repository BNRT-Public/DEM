    !-------------------------------------------------------------------------!
    !-------------------------------------------------------------------------!
    !
    SUBROUTINE NODMASS
    !
    ! DATA:  17/02/2023
    ! AUTOR: Boris N. Rojo Tanzi
    !
    !--------------------------------------------------------------------------
    ! VARIABLES LOCALES
    !--------------------------------------------------------------------------
    DOUBLE PRECISION MSS(NNT) ! MASAS TEMPORALES

    INTEGER I,J,K,NO,LABELN
    DOUBLE PRECISION RED,FROH,UNIF,STA101
    INTEGER FLAG
    LOGICAL,ALLOCATABLE,DIMENSION(:) :: FLAGNOD

    !--------------------------------------------------------------------------
    ! INICIO DE VARIABLES LOCALES
    !--------------------------------------------------------------------------
    FLAG=0
    
    !--------------------------------------------------------------------------
    ! ASIGNA TAMAÑO A LOS ARREGLO DE MASA Y DENSIDAD
    !--------------------------------------------------------------------------
    SELECT CASE (MODELTYPE)
    CASE ('DEM') ! MODEL DEM
        
    ALLOCATE(MAS(NNT))
    ALLOCATE(ROHN(NNT))
    
    !--------------------------------------------------------------------------
    !     VERIFICA SE HÁ PROPRIEDADES ALEATÓRIAS
    !--------------------------------------------------------------------------
    IF ((CVROH.NE.0.0D00).OR.(CVYNG.NE.0.0D00).OR.(CVGFR.NE.0.0D00))THEN
        WRITE (*,*)
        WRITE (*,*) '  ****** GENERATING THE RANDOM PROPERTIES ******  '
        WRITE (*,*)
    END IF

    !--------------------------------------------------------------------------
    ! GENERACION DEL CAMPO ALEATOREO PARA LA DENSIDAD
    !--------------------------------------------------------------------------
    IF (CVROH.NE.0.0D00) THEN
        ALLOCATE (FLAGNOD(NNT+1))
        FLAGNOD=.FALSE.
        
        CALL INIROH(SEED,NK1,NK2,NK3,K1,K2,K3,DK1,DK2,DK3, &
            & ROH,CVROH,LCO,PHI1,PHI2,PHI3,PHI4,AN,LABELN)
        ! LABELN = 1 : GENERACION DE VARIABLES INDEPENDIENTES
        ! LABELN = 2 : GENERACION DE VARIABLES DEPENDIENTES
        IF (LABELN.EQ.1) THEN
            ! GENERACION DE VARIABLES INDEPENDIENTES, PARA LCORR < 2.5LCO
            !$OMP PARALLEL DO PRIVATE(I,UNIF,FROH) SHARED(MSS,ROHN,ROH,FLAG,FLAGNOD)
            DO I=1,NNT
                UNIF=RAN0(SEED)
                FROH=STA101(UNIF,1.0D-50)
                ROHN(I) = ROH+FROH*CVROH*ROH

                IF (ROHN(I).LT.0.1D00*ROH) THEN
                    ROHN(I) = 0.1D00*ROH
                    FLAG=1
                    FLAGNOD(I)=.TRUE.
                END IF
                MSS(I)=0.5*(LCO**3.0)*ROHN(I)
            END DO
            !$OMP END PARALLEL DO

        ELSE IF (LABELN.EQ.2) THEN
            ! GENERACION DE VARIABLES DEPENDIENTES, PARA LCORR >2.5LCO
            !$OMP PARALLEL DO PRIVATE(I,UNIF,FROH) SHARED(MSS,ROHN,ROH,FLAG,FLAGNOD,U0,V0,W0,NK1,NK2,NK3,K1,K2,K3,DK1,DK2,DK3,PHI1,PHI2,PHI3,PHI4,AN)
            DO I=1,NNT
                CALL CAMPOROH(U0(I),V0(I),W0(I),NK1,NK2,NK3,K1,K2,K3,&
                    & DK1,DK2,DK3,PHI1,PHI2,PHI3,PHI4,AN,FROH)
                ROHN(I)=FROH+ROH

                IF (ROHN(I).LT.0.1D00*ROH) THEN
                    ROHN(I) = 0.1D00*ROH
                    FLAG=1
                    FLAGNOD(I)=.TRUE.
                END IF
                MSS(I)=0.5*(LCO**3.0)*ROHN(I)
            END DO
            !$OMP END PARALLEL DO

        END IF

        IF (FLAG.EQ.1) THEN
            DO I=1,NNT
                IF (FLAGNOD(I)) THEN
                    WRITE (*,*) I,'  FUE CORREGIDO ROH < 0.1*MEDIA  '
                END IF
            END DO
            DEALLOCATE (FLAGNOD)
        END IF

    ELSE
        MSS=0.5*(LCO**3.0)*ROH
    END IF

    !--------------------------------------------------------------------------
    ! CARGAR VALORES DE MASA CALCULADOS
    !--------------------------------------------------------------------------
    !$OMP PARALLEL DO PRIVATE(I,J,K,RED,NO) SHARED(L,M,N,MAS,MSS)
    DO K=1,L
        DO J=1,N
            DO I=1,M
                RED=1.0
                IF ((K.EQ.1).OR.(K.EQ.L)) RED=RED*0.5
                IF ((J.EQ.1).OR.(J.EQ.N)) RED=RED*0.5
                IF ((I.EQ.1).OR.(I.EQ.M)) RED=RED*0.5
                NO=(K-1)*M*N+(J-1)*M+I
                MAS(NO)=RED*MSS(NO)
            END DO
        END DO
    END DO
    !$OMP END PARALLEL DO

    !$OMP PARALLEL DO PRIVATE(I,J,K,NO) SHARED(M1,N1,L1,MNL,MSS,MAS)
    DO K=1,L1
        DO J=1,N1
            DO I=1,M1
                NO=MNL+(K-1)*M1*N1+(J-1)*M1+I
                MAS(NO)=MSS(NO)
            END DO
        END DO
    END DO
    !$OMP END PARALLEL DO

    IF (NNT.GT.NNA) THEN
        MAS(NNA+1:NNT) = MSS(NNA+1:NNT)
    END IF
    END SELECT
    
    RETURN
    END
    !
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------