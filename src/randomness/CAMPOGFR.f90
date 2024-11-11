    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------
    !
    SUBROUTINE CAMPOGFR (XB,YB,ZB,D2,PI,NK1,NK2,NK3,K1,K2,K3,         &
        &DK1,DK2,DK3,PHI1,PHI2,PHI3,PHI4,AN,FGFR)
    !
    !  ESTA SUBROTINA GERA UM CAMPO ALEATÓRIO GAUSSIANO COM MÉDIA ZERO E
    !  DESVIO PADRÃO ESPECIFICADO. ESTÁ BASEADA NO PAPER DE SHINOZUKA E
    !  DEODATIS (1996).
    !
    !  SUBROTINAS CHAMADAS: NENHUMA
    !
    !--------------------------------------------------------------------------
    IMPLICIT NONE
    !
    INTEGER NK1,NK2,NK3
    DOUBLE PRECISION XB,YB,ZB
    DOUBLE PRECISION K1,DK1,KI1,K2,DK2,KJ2,K3,DK3,KK3
    DOUBLE PRECISION D2,PI
    DOUBLE PRECISION PHI1(16,16,16),PHI2(16,16,16)
    DOUBLE PRECISION PHI3(16,16,16),PHI4(16,16,16)
    DOUBLE PRECISION AN(16,16,16),FGFR
    INTEGER I,J,K
    !--------------------------------------------------------------------------
    !     SIMULA A FUNÇÃO ALEATÓRIA
    !--------------------------------------------------------------------------
    FGFR = 0.0D00
    !
    DO 10 I=1,NK1
        DO 10 J=1,NK2
            DO 10 K=1,NK3
                !
                KI1 = (I-1)*DK1
                KJ2 = (J-1)*DK2
                KK3 = (K-1)*DK3
                !
                FGFR = FGFR + AN(I,J,K) * (DCOS(KI1*XB+KJ2*YB+KK3*ZB+PHI1(I,J,K))+&
                    &                           DCOS(KI1*XB+KJ2*YB-KK3*ZB+PHI2(I,J,K))+&
                    &                           DCOS(KI1*XB-KJ2*YB+KK3*ZB+PHI3(I,J,K))+&
                    &                           DCOS(KI1*XB-KJ2*YB-KK3*ZB+PHI4(I,J,K)))
                !
10  CONTINUE
    !
    FGFR = D2*FGFR
    !
    RETURN
    END
    !-------------------------------------------------------------------------
    !-------------------------------------------------------------------------