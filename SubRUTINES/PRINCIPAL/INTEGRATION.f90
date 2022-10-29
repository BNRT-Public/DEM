    !-------------------------------------------------------------------------
    !-------------------------------------------------------------------------
    !
    SUBROUTINE INTEGRATION
    !
    ! DATA:  02/07/2019
    ! AUTOR: Boris N. Rojo Tanzi
    !
    ! ESTA SUBRUTINA CALCULA LA POSICION DE LOS NODOS EN CADA PASO DE INTEGRACION
    !
    !--------------------------------------------------------------------------
    ! VARIABLES LOCALES
    !--------------------------------------------------------------------------
    INTEGER I
    DOUBLE PRECISION TM

    !--------------------------------------------------------------------------
    ! CALCULO DE COORDENADAS POR DIFERENCIA FINITAS CENTRADAS
    !--------------------------------------------------------------------------
    !$OMP PARALLEL DO PRIVATE(I,TM,DF,DMP1,DMP2) SHARED(MAS,U0,V0,W0,U,V,W,DT,FRX,FRY,FRZ,BDX,BDY,BDZ,LCO,TIME,DF1,DF2,STEP,NDAMF)
    DO I=1,NNT
        IF (MAS(I).NE.0.0) THEN
            TM=(DT**2.0)/MAS(I)
        END IF

        ! CALCULO DE AMORTIGUAMIENTO PARA DOMINIO INFINITO
        CALL DOMINF(DF1,DF2,STEP,NDAMF,DT,LCO,U0(I),V0(I),W0(I),M1,N1,L1,DF,DMP1,DMP2,TIME)

        ! POSICION EN X
        IF (.NOT.(ANY(BDX(I,:)))) THEN
            U(3,I)=(TM*FRX(I)+2.0D00*U(2,I)-DMP1*U(1,I))/DMP2
        ELSE
            U(3,I)=U(2,I)
        END IF

        ! POSICION EN Y
        IF (.NOT.(ANY(BDY(I,:)))) THEN
            V(3,I)=(TM*FRY(I)+2.0D00*V(2,I)-DMP1*V(1,I))/DMP2
        ELSE
            V(3,I)=V(2,I)
        END IF

        ! POSICION EN Z
        IF (.NOT.(ANY(BDZ(I,:)))) THEN
            W(3,I)=(TM*FRZ(I)+2.0D00*W(2,I)-DMP1*W(1,I))/DMP2
        ELSE
            W(3,I)=W(2,I)
        END IF
        
    END DO
    !$OMP END PARALLEL DO
    
    ! RUTINA PARA AGREGAR CONDICIONES DE BORDE POR DESPLAZAMIENTOS
    CALL APPLYSYM
        
    !$OMP PARALLEL DO PRIVATE(I) SHARED(U,V,W,DT,VLX,VLY,VLZ,ACX,ACY,ACZ,DELTAPOS)
    DO I=1,NNT
        !--------------------------------------------------------------------------
        ! CALCULO DE VELOCIDADES Y ACELERACIONES
        !--------------------------------------------------------------------------
        VLX(I)=(U(3,I)-U(1,I))/(2.0*DT)
        VLY(I)=(V(3,I)-V(1,I))/(2.0*DT)
        VLZ(I)=(W(3,I)-W(1,I))/(2.0*DT)
        ACX(I)=(U(3,I)-2.0D00*U(2,I)+U(1,I))/(DT**2.0)
        ACY(I)=(V(3,I)-2.0D00*V(2,I)+V(1,I))/(DT**2.0)
        ACZ(I)=(W(3,I)-2.0D00*W(2,I)+W(1,I))/(DT**2.0)

        !--------------------------------------------------------------------------
        ! ACTUALIZACION DE COORDENADAS
        !--------------------------------------------------------------------------
        DELTAPOS(1,I)=U(3,I)-U(1,I)
        DELTAPOS(2,I)=V(3,I)-V(1,I)
        DELTAPOS(3,I)=W(3,I)-W(1,I)
        
        U(1,I)=U(2,I)
        V(1,I)=V(2,I)
        W(1,I)=W(2,I)
        U(2,I)=U(3,I)
        V(2,I)=V(3,I)
        W(2,I)=W(3,I)
    END DO
    !$OMP END PARALLEL DO

    RETURN
    END
    !
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------