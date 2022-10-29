    !-------------------------------------------------------------------------
    !-------------------------------------------------------------------------
    !
    SUBROUTINE CREATENBSYM
    !
    ! DATA:  05/02/2020
    ! AUTOR: Boris N. Rojo Tanzi
    !
    !--------------------------------------------------------------------------
    ! VARIABLES LOCALES
    !--------------------------------------------------------------------------
    INTEGER I,J,NN,BB
    INTEGER ANN ! NUMERO DE NODOS Y BARRAS A AGREGAR
    INTEGER NI,BI ! NODO Y BARRA INICIAL PARA AGREGAR
    INTEGER D1,D2 ! DIMENCIONES DE MATRICES
    LOGICAL,ALLOCATABLE,DIMENSION(:,:) :: LOGIC ! VALORES DE BCs
    
    !--------------------------------------------------------------------------
    ! CREAR NODOS Y BARRAS PARA APLICAR CONDICIONES DE SYM/ASYM
    !--------------------------------------------------------------------------   
    ! PLANO YZ ENTRE X=0 y X=MAX
    IF (BDSYM(1).OR.BDASYM(1)) THEN
        NI = SIZE(U0)
        BI = SIZE(CN,2)
        ANN=N1*L1
        
        CALL RESIZESYM(2*ANN)
        NNT=2*ANN+NNT
        NBT=2*ANN+NBT
        
        !$OMP PARALLEL DO PRIVATE(I,J,NN,BB) SHARED(U0,V0,W0,CN,LCO,ANN,MNL,M1,N1,L1,NI,BI)
        DO I=1,L1
            DO J=1,N1
                NN=NI+J+(I-1)*N1
                BB=BI+J+(I-1)*N1
                ! COORDENADAS PARA X=0
                U0(NN)=-0.5*LCO
                V0(NN)=0.5*LCO+(J-1)*LCO
                W0(NN)=0.5*LCO+(I-1)*LCO
                ! COORDENADAS PARA X=MAX
                U0(NN+ANN)=0.5*LCO+M1*LCO
                V0(NN+ANN)=V0(NN)
                W0(NN+ANN)=W0(NN)
                ! CONECTIVIDAD
                CN(1,BB)=MNL+1+(I-1)*M1*N1+(J-1)*M1
                CN(2,BB)=NN
                CN(1,BB+ANN)=MNL+1+(I-1)*M1*N1+(J-1)*M1+M1-1
                CN(2,BB+ANN)=NN+ANN
            END DO
        END DO
        !$OMP END PARALLEL DO       
    END IF

    ! PLANO XZ ENTRE Y=0 y Y=MAX
    IF (BDSYM(2).OR.BDASYM(2)) THEN
        NI = SIZE(U0)
        BI = SIZE(CN,2)
        ANN=M1*L1
        
        CALL RESIZESYM(2*ANN)
        NNT=2*ANN+NNT
        NBT=2*ANN+NBT
        
        !$OMP PARALLEL DO PRIVATE(I,J,NN,BB) SHARED(U0,V0,W0,CN,LCO,ANN,MNL,M1,N1,L1,NI,BI)
        DO I=1,L1
            DO J=1,M1
                NN=NI+J+(I-1)*M1
                BB=BI+J+(I-1)*M1
                ! COORDENADAS PARA Y=0
                U0(NN)=0.5*LCO+(J-1)*LCO
                V0(NN)=-0.5*LCO
                W0(NN)=0.5*LCO+(I-1)*LCO
                ! COORDENADAS PARA Y=MAX
                U0(NN+ANN)=U0(NN)
                V0(NN+ANN)=0.5*LCO+N1*LCO
                W0(NN+ANN)=W0(NN)
                ! CONECTIVIDAD
                CN(1,BB)=MNL+J+(I-1)*M1*N1
                CN(2,BB)=NN
                CN(1,BB+ANN)=MNL+J+(I-1)*M1*N1+M1*(N1-1)
                CN(2,BB+ANN)=NN+ANN
            END DO
        END DO
        !$OMP END PARALLEL DO       
    END IF
    
    ! PLANO XY ENTRE Z=0 y Z=MAX
    IF (BDSYM(3).OR.BDASYM(3)) THEN
        NI = SIZE(U0)
        BI = SIZE(CN,2)
        ANN=M1*N1
        
        CALL RESIZESYM(2*ANN)
        NNT=2*ANN+NNT
        NBT=2*ANN+NBT
        
        !$OMP PARALLEL DO PRIVATE(I,J,NN,BB) SHARED(U0,V0,W0,CN,LCO,ANN,MNL,M1,N1,L1,NI,BI)
        DO I=1,N1
            DO J=1,M1
                NN=NI+J+(I-1)*M1
                BB=BI+J+(I-1)*M1
                ! COORDENADAS PARA Y=0
                U0(NN)=0.5*LCO+(J-1)*LCO
                V0(NN)=0.5*LCO+(I-1)*LCO
                W0(NN)=-0.5*LCO
                ! COORDENADAS PARA Y=MAX
                U0(NN+ANN)=U0(NN)
                V0(NN+ANN)=V0(NN)
                W0(NN+ANN)=0.5*LCO+L1*LCO
                ! CONECTIVIDAD
                CN(1,BB)=MNL+J+(I-1)*M1
                CN(2,BB)=NN
                CN(1,BB+ANN)=MNL+J+(I-1)*M1+M1*N1*(L1-1)
                CN(2,BB+ANN)=NN+ANN
            END DO
        END DO
        !$OMP END PARALLEL DO       
    END IF
    
    !--------------------------------------------------------------------------
    ! REORGANIZACION DE VARIABLES DE CONDICIONES DE BORDE
    !--------------------------------------------------------------------------
    D1=SIZE(BDX,1)
    D2=SIZE(BDX,2)
    
    IF (D1.NE.NNT) THEN
        ALLOCATE(LOGIC(D1,D2))
        LOGIC=BDX
        DEALLOCATE(BDX)
        ALLOCATE(BDX(NNT,D2))
        BDX=.FALSE.
        BDX(1:D1,:)=LOGIC
        
        LOGIC=BDY
        DEALLOCATE(BDY)
        ALLOCATE(BDY(NNT,D2))
        BDY=.FALSE.
        BDY(1:D1,:)=LOGIC
        
        LOGIC=BDZ
        DEALLOCATE(BDZ)
        ALLOCATE(BDZ(NNT,D2))
        BDZ=.FALSE.
        BDZ(1:D1,:)=LOGIC
        DEALLOCATE(LOGIC)
        
        D1=SIZE(LOX,1)
        D2=SIZE(LOX,2)
        ALLOCATE(LOGIC(D1,D2))
        LOGIC=LOX
        DEALLOCATE(LOX)
        ALLOCATE(LOX(NNT,D2))
        LOX=.FALSE.
        LOX(1:D1,:)=LOGIC
        
        LOGIC=LOY
        DEALLOCATE(LOY)
        ALLOCATE(LOY(NNT,D2))
        LOY=.FALSE.
        LOY(1:D1,:)=LOGIC
        
        LOGIC=LOZ
        DEALLOCATE(LOZ)
        ALLOCATE(LOZ(NNT,D2))
        LOZ=.FALSE.
        LOZ(1:D1,:)=LOGIC
    END IF
    
    RETURN
    END
    !
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------