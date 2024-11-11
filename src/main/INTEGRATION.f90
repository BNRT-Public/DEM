    !-------------------------------------------------------------------------
    !-------------------------------------------------------------------------
    !
    SUBROUTINE INTEGRATION
    IMPLICIT NONE
    !
    ! DATA:  21/01/2023
    ! AUTOR: Boris N. Rojo Tanzi
    !
    ! ESTA SUBRUTINA CALCULA LA POSICION DE LOS NODOS EN CADA PASO DE INTEGRACION
    !
    !--------------------------------------------------------------------------
    ! VARIABLES LOCALES
    !--------------------------------------------------------------------------
    INTEGER I
    INTEGER STEP,NDAMF
    DOUBLE PRECISION TM
    DOUBLE PRECISION DTS,IDTS,IDTD
    DOUBLE PRECISION DF,DFN
    DOUBLE PRECISION TDMP1,TDMP2
    DOUBLE PRECISION DOMINF

    !--------------------------------------------------------------------------
    ! INICIO DE VARIABLES
    !--------------------------------------------------------------------------
    DTS=DT**2.0D0
    IDTS=1.0D0/(DT**2.0D0)
    IDTD=1.0D0/(2.0D0*DT)
    
    !--------------------------------------------------------------------------
    ! CALCULA EL COEFICIENTE DE AMORTIGUAMIENTO
    !--------------------------------------------------------------------------
    ! CONTROL DE AMORTIGUAMIENTO
    IF (STEP.LT.NDAMF) THEN
        DF=DF1
    ELSE
        DF=DF2
    END IF
    DFN=DF
        
    !--------------------------------------------------------------------------
    ! CALCULO DE COORDENADAS POR DIFERENCIA FINITAS CENTRADAS
    !--------------------------------------------------------------------------
    !$OMP PARALLEL DO PRIVATE(I,TM,DFN,TDMP1,TDMP2) &
    !$OMP& SHARED(MAS,U0,V0,W0,U,V,W,DT,FRX,FRY,FRZ,BDX,BDY,BDZ,LCO,DF,DTS)
    DO I=1,NNT
        IF (MAS(I).NE.0.0) TM=DTS/MAS(I)

        ! CALCULO DE AMORTIGUAMIENTO PARA DOMINIO INFINITO
        DFN = DOMINF(DF,LCO,U0(I),V0(I),W0(I),M1,N1,L1)
        
        TDMP1=1.00D0-DFN*DT*0.5D0
        TDMP2=1.00D0+DFN*DT*0.5D0
        
        ! POSICION EN X
        IF (.NOT.(ANY(BDX(I,:)))) THEN
            U(3,I)=(TM*FRX(I)+2.0D00*U(2,I)-TDMP1*U(1,I))/TDMP2
        ELSE
            U(3,I)=U(2,I)
        END IF

        ! POSICION EN Y
        IF (.NOT.(ANY(BDY(I,:)))) THEN
            V(3,I)=(TM*FRY(I)+2.0D00*V(2,I)-TDMP1*V(1,I))/TDMP2
        ELSE
            V(3,I)=V(2,I)
        END IF

        ! POSICION EN Z
        IF (.NOT.(ANY(BDZ(I,:)))) THEN
            W(3,I)=(TM*FRZ(I)+2.0D00*W(2,I)-TDMP1*W(1,I))/TDMP2
        ELSE
            W(3,I)=W(2,I)
        END IF        
    END DO
    !$OMP END PARALLEL DO
    
    ! RUTINA PARA AGREGAR CONDICIONES DE BORDE POR DESPLAZAMIENTOS
    !CALL APPLYSYM
        
    !$OMP PARALLEL DO PRIVATE(I) SHARED(U,V,W,VLX,VLY,VLZ,ACX,ACY,ACZ,IDTS,IDTD)
    DO I=1,NNT
        !--------------------------------------------------------------------------
        ! CALCULO DE VELOCIDADES Y ACELERACIONES
        !--------------------------------------------------------------------------
        VLX(I)=(U(3,I)-U(1,I))*IDTD
        VLY(I)=(V(3,I)-V(1,I))*IDTD
        VLZ(I)=(W(3,I)-W(1,I))*IDTD
        ACX(I)=(U(3,I)-2.0D00*U(2,I)+U(1,I))*IDTS
        ACY(I)=(V(3,I)-2.0D00*V(2,I)+V(1,I))*IDTS
        ACZ(I)=(W(3,I)-2.0D00*W(2,I)+W(1,I))*IDTS
    END DO
    !$OMP END PARALLEL DO

    !--------------------------------------------------------------------------
    ! ACTUALIZACION DE COORDENADAS
    !--------------------------------------------------------------------------
    DELTAPOS(1,:)=U(3,:)-U(1,:)
    DELTAPOS(2,:)=V(3,:)-V(1,:)
    DELTAPOS(3,:)=W(3,:)-W(1,:)
        
    U(1,:)=U(2,:)
    V(1,:)=V(2,:)
    W(1,:)=W(2,:)
    U(2,:)=U(3,:)
    V(2,:)=V(3,:)
    W(2,:)=W(3,:)
        
    RETURN
    END
    !
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------