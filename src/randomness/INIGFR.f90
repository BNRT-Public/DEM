    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------
    !
    SUBROUTINE INIGFR (SEED,D2,PI,NK1,NK2,NK3,K1,K2,K3,DK1,DK2,DK3,   &
        &GFR,CVGFR,LCO,PHI1,PHI2,PHI3,PHI4,AN,LABELN)
    !
    !  ESTA SUBROTINA CALCULA O PARÂMETRO AN E AS MATRIZES DE FASES ALEATÓRIAS,
    !  PARA SEREM USADOS NA SUBROTINA CAMPO.
    !
    !  SUBROTINAS CHAMADAS: NENHUMA
    !  FUNÇÕES CHAMADAS: RAN0 (SEED) E SPECTRAL (KI1,KJ2,KK3,GFR,CVGFR,BGFR)
    !
    !--------------------------------------------------------------------------
    IMPLICIT NONE
    !
    INTEGER SEED,SEEDGFR,LABELN
    INTEGER NK1,NK2,NK3
    DOUBLE PRECISION K1,DK1,KI1,K2,DK2,KJ2,K3,DK3,KK3
    DOUBLE PRECISION D2,PI,COMPCORRGFR,BGFR
    DOUBLE PRECISION RAN0,SPECTRALGFR
    DOUBLE PRECISION GFR,CVGFR,LCO
    DOUBLE PRECISION PHI1(16,16,16),PHI2(16,16,16)
    DOUBLE PRECISION PHI3(16,16,16),PHI4(16,16,16)
    DOUBLE PRECISION AN(16,16,16)
    INTEGER I,J,K
    !--------------------------------------------------------------------------
    !     CÁLCULOS INICIAIS
    !--------------------------------------------------------------------------
    SEEDGFR = SEED+8
    !
    D2  = DSQRT(2.0D00)
    PI  = 4.0D00*DATAN(1.0D00)
    COMPCORRGFR = 2.5D00*LCO
    BGFR = 2.0D00*COMPCORRGFR/DSQRT(PI)
    !
    NK1 = 16
    NK2 = 16
    NK3 = 16
    !
    K1 = 2.0D00/BGFR*3.6087576D00
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
                    PHI1(I,J,K) = 2.0D00*PI*RAN(SEEDGFR)
                    PHI2(I,J,K) = 2.0D00*PI*RAN(SEEDGFR)
                    PHI3(I,J,K) = 2.0D00*PI*RAN(SEEDGFR)
                    PHI4(I,J,K) = 2.0D00*PI*RAN(SEEDGFR)
                    !
                    AN(I,J,K) = DSQRT(2.0D00*SPECTRALGFR(KI1,KJ2,KK3,GFR,CVGFR,BGFR)* &
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