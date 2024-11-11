    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------
    !
    SUBROUTINE INIROH(SEED,NK1,NK2,NK3,K1,K2,K3,DK1,DK2,DK3, &
                     & ROH,CVROH,LCO,PHI1,PHI2,PHI3,PHI4,AN,LABELN)
    !
    ! DATA:  02/07/2019
    ! AUTOR: Ignacio Iturrioz
    !        Boris N. Rojo Tanzi
    !
    !  ESTA SUBROTINA CALCULA LOS PARAMETROS AN Y LAS MATRICES DE FASES ALEATORIAS,
    !  PARA USARLAS EN LAS RUTINAS CAMPO.
    !
    !--------------------------------------------------------------------------
    ! VARIABLES LOCALES
    !--------------------------------------------------------------------------
    IMPLICIT NONE
    INTEGER I,J,K
    DOUBLE PRECISION PI

    !--------------------------------------------------------------------------
    ! VARIABLES DE ENTRADA SALIDA
    !--------------------------------------------------------------------------
    INTEGER SEED,SEEDROH,LABELN
    INTEGER NK1,NK2,NK3
    DOUBLE PRECISION K1,DK1,KI1,K2,DK2,KJ2,K3,DK3,KK3
    DOUBLE PRECISION COMPCORRROH,BROH
    DOUBLE PRECISION RAN0,SPECTRALROH
    DOUBLE PRECISION ROH,CVROH,LCO
    DOUBLE PRECISION PHI1(16,16,16),PHI2(16,16,16)
    DOUBLE PRECISION PHI3(16,16,16),PHI4(16,16,16)
    DOUBLE PRECISION AN(16,16,16)

    !--------------------------------------------------------------------------
    ! CONSTANTES
    !--------------------------------------------------------------------------
    PI  = 4.0*DATAN(1.0D00)
    NK1 = 16
    NK2 = 16
    NK3 = 16

    !--------------------------------------------------------------------------
    ! CALCULOS INICIALES
    !--------------------------------------------------------------------------
    SEEDROH = SEED+2
    COMPCORRROH=2.50*LCO
    BROH=2.0*COMPCORRROH/DSQRT(PI)

    K1 = 2.0/BROH*3.6087576
    K2 = K1
    K3 = K1

    ! VERIFICACION DE LA EQ. 139 DEL PAPER DE SHINOZUKA
    IF (LCO.GT.(PI/K1)) THEN
        ! GENERACION DE PROPRIEDADES INDEPENDENTES
        LABELN=1
    ELSE
        ! GENERACION DE PROPRIEDADES DEPENDENTES
        LABELN=2
        DK1 = K1/NK1
        DK2 = K2/NK2
        DK3 = K3/NK3

        !--------------------------------------------------------------------------
        !     CÁLCULOS DAS MATRIZES DE FASES ALEATÓRIAS
        !--------------------------------------------------------------------------
        !$OMP PARALLEL DO PRIVATE(I,J,K,KI1,KJ2,KK3) SHARED(PHI1,PHI2,PHI3,PHI4,AN,PI,ROH,CVROH,BROH,DK1,DK2,DK3)
        DO I=1,NK1
            DO J=1,NK2
                DO K=1,NK3
                    KI1 = (I-1)*DK1
                    KJ2 = (J-1)*DK2
                    KK3 = (K-1)*DK3

                    PHI1(I,J,K) = 2.0*PI*RAN(SEEDROH)
                    PHI2(I,J,K) = 2.0*PI*RAN(SEEDROH)
                    PHI3(I,J,K) = 2.0*PI*RAN(SEEDROH)
                    PHI4(I,J,K) = 2.0*PI*RAN(SEEDROH)
                    
                    AN(I,J,K) = DSQRT(2.0*SPECTRALROH(KI1,KJ2,KK3,ROH,CVROH,BROH)* &
                        & DK1*DK2*DK3)
                END DO
            END DO
        END DO
        !$OMP END PARALLEL DO
    END IF

    RETURN
    END
    !
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------