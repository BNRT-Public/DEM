    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------
    !
    SUBROUTINE CAMPOROH (U0,V0,W0,NK1,NK2,NK3,K1,K2,K3,&
        &DK1,DK2,DK3,PHI1,PHI2,PHI3,PHI4,AN,FROH)
    !
    ! DATA:  02/07/2019
    ! AUTOR: Ignacio Iturrioz
    !        Boris N. Rojo Tanzi
    !
    ! ESTA SUBROTINA GENERA UN CAMPO ALEATOREO GAUSSIANO CON MEDIA CERO Y
    ! DESVIO PATRON ESPECIFICADO. ESTA BASADO EN EL PAPER DE SHINOZUKA E 
    ! DEODATIS (1996).
    !
    !--------------------------------------------------------------------------
    ! VARIABLES LOCALES
    !--------------------------------------------------------------------------
    IMPLICIT NONE

    INTEGER I,J,K
    DOUBLE PRECISION PI,D2

    !--------------------------------------------------------------------------
    ! VARIABLES ENTRADA SALIDA
    !--------------------------------------------------------------------------
    INTEGER NK1,NK2,NK3
    DOUBLE PRECISION U0,V0,W0
    DOUBLE PRECISION K1,DK1,KI1,K2,DK2,KJ2,K3,DK3,KK3
    DOUBLE PRECISION PHI1(16,16,16),PHI2(16,16,16)
    DOUBLE PRECISION PHI3(16,16,16),PHI4(16,16,16)
    DOUBLE PRECISION AN(16,16,16),FROH
    
    !--------------------------------------------------------------------------
    ! CONSTANTES
    !--------------------------------------------------------------------------
    D2  = DSQRT(2.0D00)
    PI  = 4.0*DATAN(1.0D00)
    
    !--------------------------------------------------------------------------
    ! CALCULA FUNCION ALEATOREA
    !--------------------------------------------------------------------------
    FROH = 0.0
    
    DO I=1,NK1
        DO J=1,NK2
            DO K=1,NK3
                KI1 = (I-1)*DK1
                KJ2 = (J-1)*DK2
                KK3 = (K-1)*DK3

                FROH = FROH + AN(I,J,K) * (DCOS(KI1*U0+KJ2*V0+KK3*W0+PHI1(I,J,K))+&
                    & DCOS(KI1*U0+KJ2*V0-KK3*W0+PHI2(I,J,K))+DCOS(KI1*U0-KJ2*V0+KK3*W0+PHI3(I,J,K))+&
                    & DCOS(KI1*U0-KJ2*V0-KK3*W0+PHI4(I,J,K)))
            END DO
        END DO
    END DO
    FROH = D2*FROH
    RETURN
    END
    !-------------------------------------------------------------------------
    !-------------------------------------------------------------------------