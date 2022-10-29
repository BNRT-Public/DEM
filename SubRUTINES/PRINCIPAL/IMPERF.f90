    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------
    !
    SUBROUTINE IMPERF
    !
    ! DATA:  02/07/2019
    ! AUTOR: BORIS N. ROJO TANZI
    !
    ! ESTA SUBRUTINA INTRODUCE INPERFECCIONES EN LA MALLA
    !
    !--------------------------------------------------------------------------
    ! VARIABLES LOCALES
    !--------------------------------------------------------------------------
    INTEGER I,NI,NO,LABELN
    DOUBLE PRECISION FUVW,CVUVW,UNIF,STA101
    
    !--------------------------------------------------------------------------
    ! GENERACION DEL CAMPO ALEATORIEO EN LA MALLA
    !--------------------------------------------------------------------------
    !
    !  SE LABELN = 1 --> GERAÇÃO DE VARIÁVEIS ALEATÓRIAS INDEPENDENTES
    !  SE LABELN = 2 --> GERAÇÃO DE VARIÁVEIS ALEATÓRIAS DEPENDENTES
    !
    SELECT CASE (TEST)
    CASE (801)
        LABELN=1
        CVUVW=1.000D-02
        IF (LABELN.EQ.1) THEN
            DO I=1,NNT
                UNIF=RAN0(SEED)
                FUVW=STA101(UNIF,1.0D-50)
                U0(I)=U0(I)+FUVW*CVUVW*LCO
                UNIF=RAN0(SEED)
                FUVW=STA101(UNIF,1.0D-50)
                V0(I)=V0(I)+FUVW*CVUVW*LCO
                UNIF=RAN0(SEED)
                FUVW=STA101(UNIF,1.0D-50)
                W0(I)=W0(I)+FUVW*CVUVW*LCO
            END DO
        END IF
    CASE (331)
    ! TEST = 331 - CUBOS COMPRESSÃO TRIAXIAL
        LABELN=1
        CVUVW=2.50E-2 !2.75D-02
        IF (LABELN.EQ.1) THEN
            !
            DO I=1,NNT
                UNIF=RAN0(SEED)
                FUVW=STA101(UNIF,1.0D-50)
                U0(I)=U0(I)+FUVW*CVUVW*LCO
                UNIF=RAN0(SEED)
                FUVW=STA101(UNIF,1.0D-50)
                V0(I)=V0(I)+FUVW*CVUVW*LCO
                UNIF=RAN0(SEED)
                FUVW=STA101(UNIF,1.0D-50)
                W0(I)=W0(I)+FUVW*CVUVW*LCO
            ENDDO
        END IF
    END SELECT

    RETURN
    END
    !
    !-------------------------------------------------------------------------!
    !-------------------------------------------------------------------------!