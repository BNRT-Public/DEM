    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------
    !
    SUBROUTINE IMPERF
    IMPLICIT NONE
    !
    ! DATA:  19/11/2022
    ! AUTOR: BORIS N. ROJO TANZI
    !
    ! ESTA SUBRUTINA INTRODUCE INPERFECCIONES EN LA MALLA
    !
    !--------------------------------------------------------------------------
    ! VARIABLES LOCALES
    !--------------------------------------------------------------------------
    INTEGER I,NI,NO,LABELN
    DOUBLE PRECISION FUVW,UNIF,STA101
    LOGICAL LOGIC(4)
    
    !--------------------------------------------------------------------------
    ! GENERACION DEL CAMPO ALEATORIEO EN LA MALLA
    !--------------------------------------------------------------------------
    !
    !  SE LABELN = 1 --> GERA��O DE VARI�VEIS ALEAT�RIAS INDEPENDENTES
    !  SE LABELN = 2 --> GERA��O DE VARI�VEIS ALEAT�RIAS DEPENDENTES
    !
        
    LABELN=1    
    IF (MAXVAL(CVUVW).GT.0.0) THEN
        IF (LABELN.EQ.1) THEN
            !
            DO I=1,NNT
                logic = .true.
                LOGIC(1) = ((U0(I).GE.BORDERUVW*LCO).AND.(U0(I).LE.(M1*LCO-BORDERUVW*LCO)))
                LOGIC(2) = ((V0(I).GE.BORDERUVW*LCO).AND.(V0(I).LE.(N1*LCO-BORDERUVW*LCO)))
                LOGIC(3) = ((W0(I).GE.BORDERUVW*LCO).AND.(W0(I).LE.(L1*LCO-BORDERUVW*LCO)))
                
                if (test.eq.813) then
                    logic(4) = ((v0(I).le.(50e-3 - 0.25*lco)).or.(v0(I).ge.(50e-3 + 0.25*lco)))
                end if
                if (test.eq.814) then
                    logic(4) = ((v0(I).le.(37.50e-3 - 0.25*lco)).or.(v0(I).ge.(37.50e-3 + 0.25*lco)))
                end if
                if (test.eq.812) then
                    logic(4) = ((v0(I).le.(75e-3 - 0.25*lco)).or.(v0(I).ge.(75e-3 + 0.25*lco)))
                end if
                if (test.eq.823) then
                    logic(4) = ((v0(I).le.(50e-3 - 0.25*lco)).or.(v0(I).ge.(100e-3 + 0.25*lco)).or.((v0(I).ge.(50e-3 + 0.25*lco)).and.(v0(I).le.(100e-3 - 0.25*lco))))
                end if
                if (test.eq.824) then
                    logic(4) = ((v0(I).le.(37.50e-3 - 0.25*lco)).or.(v0(I).ge.(112.50e-3 + 0.25*lco)).or.((v0(I).ge.(37.50e-3 + 0.25*lco)).and.(v0(I).le.(112.50e-3 - 0.25*lco))))
                end if

                IF (ALL(LOGIC)) THEN
                    UNIF=RAN0(SEED)
                    FUVW=STA101(UNIF,1.0D-50)
                    U0(I)=U0(I)+FUVW*CVUVW(1)*LCO
                    UNIF=RAN0(SEED)
                    FUVW=STA101(UNIF,1.0D-50)
                    V0(I)=V0(I)+FUVW*CVUVW(2)*LCO
                    UNIF=RAN0(SEED)
                    FUVW=STA101(UNIF,1.0D-50)
                    W0(I)=W0(I)+FUVW*CVUVW(3)*LCO
                END IF
            ENDDO
        END IF
    END IF

    RETURN
    END
    !
    !-------------------------------------------------------------------------!
    !-------------------------------------------------------------------------!