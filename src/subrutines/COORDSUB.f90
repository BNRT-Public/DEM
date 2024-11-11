    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------
    !
    SUBROUTINE COORDSUB (MI,MF,NI,NF,LI,LF,ST,MX,NX,X1,Y1,X2,Y2,X3,Y3)
    !
    ! DATA:  02/07/2019
    ! AUTOR: Ignacio Iturrioz
    !        Boris N. Rojo Tanzi
    !
    !--------------------------------------------------------------------------
    ! VARIABLES LOCALES
    !--------------------------------------------------------------------------
    INTEGER I,J,K,LI,LF,NI,NF,MI,MF,MX,NX
    INTEGER X1,Y1,X2,Y2,X3,Y3,NO,ST
    !--------------------------------------------------------------------------
    !$OMP PARALLEL DO PRIVATE(I,J,K,NO) SHARED(LI,LF,NI,NF,MI,MF,ST,MX,NX,LCO,U,V,W,U0,V0,W0)
    DO K=LI,LF
        DO J=NI,NF
            DO I=MI,MF
                NO=ST+(K-1)*MX*NX+(J-1)*MX+I

                U0(NO)=(X1*I-Y1*0.5)*LCO
                V0(NO)=(X2*J-Y2*0.5)*LCO
                W0(NO)=(X3*K-Y3*0.5)*LCO
            END DO
        END DO  
    END DO 
    !$OMP END PARALLEL DO

    RETURN
    END
    !
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------