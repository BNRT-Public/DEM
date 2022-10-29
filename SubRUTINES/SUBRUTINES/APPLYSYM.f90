    !-------------------------------------------------------------------------
    !-------------------------------------------------------------------------
    !
    SUBROUTINE APPLYSYM
    !
    ! DATA:  05/02/2020
    ! AUTOR: Boris N. Rojo Tanzi
    !
    !--------------------------------------------------------------------------
    ! VARIABLES LOCALES
    !--------------------------------------------------------------------------
    INTEGER B,BI
    INTEGER NI,NE !NODO INTERNO Y EXTERNO DEL MODELO

    !--------------------------------------------------------------------------
    ! APLICAR LAS CONDICIONES DE BORDE PARA CADA CASO
    !--------------------------------------------------------------------------
    ! SHEAR X
    IF (BDSYM(1)) THEN
    END IF
    ! SHEAR Y
    IF (BDSYM(2)) THEN
        BI=NBA
        IF (BDSYM(1)) THEN
            BI=BI+2*N1*L1 ! SUMA VALORES POR SYM EN X
        END IF
        
        ! PLANO XZ ENTRE Y=0
        !$OMP PARALLEL DO PRIVATE(B,NI,NE) SHARED(U,V,W,CN,LCO,M1,N1,L1,BI)
        DO B=BI,BI+M1*L1
            NI=CN(1,B)
            NE=CN(2,B)
            
            U(3,NE)=U(3,NI)
            V(3,NE)=V(3,NI)-LCO
            W(3,NE)=W(3,NI)
        END DO
        !$OMP END PARALLEL DO
        
        ! PLANO XZ ENTRE Y=MAX
        !$OMP PARALLEL DO PRIVATE(B,NI,NE) SHARED(U,V,W,CN,LCO,M1,N1,L1,BI)
        DO B=BI+M1*L1+1,BI+2*M1*L1
            NI=CN(1,B)
            NE=CN(2,B)
            
            U(3,NE)=U(3,NI)
            V(3,NE)=V(3,NI)+LCO
            W(3,NE)=W(3,NI)
        ENDDO
        !$OMP END PARALLEL DO
    END IF
    ! SHEAR Z
    IF (BDSYM(3)) THEN
        BI=NBA
        IF (BDSYM(1)) THEN
            BI=BI+2*N1*L1 ! SUMA VALORES POR SYM EN X
        END IF
        IF (BDSYM(2)) THEN
            BI=BI+2*M1*L1 ! SUMA VALORES POR SYM EN Y
        END IF
        
        ! PLANO XY ENTRE Z=0
        !$OMP PARALLEL DO PRIVATE(B,NI,NE) SHARED(U,V,W,CN,LCO,M1,N1,L1,BI)
        DO B=BI,BI+M1*N1
            NI=CN(1,B)
            NE=CN(2,B)
            
            U(3,NE)=U(3,NI)
            V(3,NE)=V(3,NI)
            W(3,NE)=W(3,NI)-LCO
        ENDDO
        !$OMP END PARALLEL DO
    
        ! PLANO XY ENTRE Z=MAX
        !$OMP PARALLEL DO PRIVATE(B,NI,NE) SHARED(U,V,W,CN,LCO,M1,N1,L1,BI)
        DO B=BI+M1*N1+1,BI+2*M1*N1
            NI=CN(1,B)
            NE=CN(2,B)
            
            U(3,NE)=U(3,NI)
            V(3,NE)=V(3,NI)
            W(3,NE)=W(3,NI)+LCO
        ENDDO
        !$OMP END PARALLEL DO
    END IF

    RETURN
    END