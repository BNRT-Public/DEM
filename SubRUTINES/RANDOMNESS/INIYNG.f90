    !-------------------------------------------------------------------------
    !-------------------------------------------------------------------------
    !
    SUBROUTINE INIYNG (SEED,D2,PI,NK1,NK2,NK3,K1,K2,K3,DK1,DK2,DK3,   &
        &YNG,CVYNG,LCO,PHI1,PHI2,PHI3,PHI4,AN,LABELN)
    !
    !  ESTA SUBROTINA CALCULA O PARÂMETRO AN E AS MATRIZES DE FASES ALEATÓRIAS,
    !  PARA SEREM USADOS NA SUBROTINA CAMPO.
    !
    !  SUBROTINAS CHAMADAS: NENHUMA
    !  FUNÇÕES CHAMADAS: RAN0 (SEED) E SPECTRAL (KI1,KJ2,KK3,YNG,CVYNG,BYNG)
    !
    !--------------------------------------------------------------------------
    IMPLICIT NONE
    !
    INTEGER SEED,SEEDYNG,LABELN
    INTEGER NK1,NK2,NK3
    DOUBLE PRECISION K1,DK1,KI1,K2,DK2,KJ2,K3,DK3,KK3
    DOUBLE PRECISION D2,PI,COMPCORRYNG,BYNG
    DOUBLE PRECISION RAN0,SPECTRALYNG
    DOUBLE PRECISION YNG,CVYNG,LCO
    DOUBLE PRECISION PHI1(16,16,16),PHI2(16,16,16)
    DOUBLE PRECISION PHI3(16,16,16),PHI4(16,16,16)
    DOUBLE PRECISION AN(16,16,16)
    INTEGER I,J,K
    !--------------------------------------------------------------------------
    !     CÁLCULOS INICIAIS
    !--------------------------------------------------------------------------
    SEEDYNG = SEED+4
    !
    D2  = DSQRT(2.0D00)
    PI  = 4.0D00*DATAN(1.0D00)
    COMPCORRYNG = 2.50D00*LCO
    BYNG = 2.0D00*COMPCORRYNG/DSQRT(PI)
    !
    NK1 = 16
    NK2 = 16
    NK3 = 16
    !
    K1 = 2.0D00/BYNG*3.6087576D00
    K2 = K1
    K3 = K1
    !
    !     VERIFICAÇÃO DA EQ.(139) DO PAPER DE SHINOZUKA
    IF (LCO.GT.(PI/K1)) THEN
        !      WRITE (*,*) '  DIMINUIR O LCO OU AUMENTAR O COMPCORR  '
        !      STOP
        !     GERAÇÃO DE PROPRIEDADES INDEPENDENTES
        LABELN=1
    ELSE
        !     GERAÇÃO DE PROPRIEDADES DEPENDENTES CONFORME SHINOZUKA
        LABELN=2
        DK1 = K1/NK1
        DK2 = K2/NK2
        DK3 = K3/NK3
        !--------------------------------------------------------------------------
        !     CÁLCULOS DAS MATRIZES DE FASES ALEATÓRIAS
        !--------------------------------------------------------------------------
        DO 10 I=1,NK1
            DO 10 J=1,NK2
                DO 10 K=1,NK3
                    !
                    KI1 = (I-1)*DK1
                    KJ2 = (J-1)*DK2
                    KK3 = (K-1)*DK3
                    !
                    PHI1(I,J,K) = 2.0D00*PI*RAN(SEEDYNG)
                    PHI2(I,J,K) = 2.0D00*PI*RAN(SEEDYNG)
                    PHI3(I,J,K) = 2.0D00*PI*RAN(SEEDYNG)
                    PHI4(I,J,K) = 2.0D00*PI*RAN(SEEDYNG)
                    !
                    AN(I,J,K) = DSQRT(2.0D00*SPECTRALYNG(KI1,KJ2,KK3,YNG,CVYNG,BYNG)* &
                        &                  DK1*DK2*DK3)
                    !
10      CONTINUE
    END IF
    !
    RETURN
    END
    !
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------