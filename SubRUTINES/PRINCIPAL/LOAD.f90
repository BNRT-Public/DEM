    !-------------------------------------------------------------------------
    !-------------------------------------------------------------------------
    !
    SUBROUTINE LOAD
    !
    ! DATA:  02/07/2019
    ! AUTOR: Boris N. Rojo Tanzi
    !
    !--------------------------------------------------------------------------
    ! VARIABLES LOCALES
    !--------------------------------------------------------------------------
    INTEGER I
    
    LOGICAL,ALLOCATABLE,DIMENSION(:) :: LOGIC
    
    !--------------------------------------------------------------------------
    ! APLICAR LAS CONDICIONES DE BORDE PARA CADA CASO
    !--------------------------------------------------------------------------
    SELECT CASE (TEST)
    CASE (101,102) ! TEST = 101/102 - VIGA EMBOLADIZO
        ! CONDICIONES DE BORDE
        ALLOCATE(LOX(NNT,1),LOY(NNT,1),LOZ(NNT,1))
        LOX=.FALSE.
        LOY=.FALSE.
        LOZ=.FALSE.

        !$OMP PARALLEL DO PRIVATE(I) SHARED (U0,V0,W0,LOX,LOY,LOZ)
        DO I=1,NNT
            IF ((U0(I).LE.(M1-0.44)*LCO).AND.(U0(I).GE.(M1-0.54)*LCO)) THEN
                !LOX(I,1)=.TRUE.
                LOY(I,1)=.TRUE.
                !LOZ(I,1)=.TRUE.
            END IF
        END DO
        !$OMP END PARALLEL DO
    
    CASE (110) ! TEST = 337 - TRACCION
        ! CONDICIONES DE BORDE
        ALLOCATE(LOX(NNT,2),LOY(NNT,2),LOZ(NNT,2))
        LOX=.FALSE.
        LOY=.FALSE.
        LOZ=.FALSE.

        !$OMP PARALLEL DO PRIVATE(I) SHARED (U0,V0,W0,LOX,LOY,LOZ)
        DO I=1,NNT
            IF ((V0(I).LE.(N1-0.44)*LCO).AND.(V0(I).GE.(N1-0.54)*LCO)) THEN
                LOX(I,1)=.TRUE.
                !LOY(I,1)=.TRUE.
                !LOZ(I,1)=.TRUE.
            END IF
            IF (V0(I).GE.(N1-0.54)*LCO) THEN
                !LOX(I,1)=.TRUE.
                LOY(I,1)=.TRUE.
                !LOZ(I,1)=.TRUE.
            END IF
        END DO
        !$OMP END PARALLEL DO
        
    CASE (337,338) ! TEST = 337 - TRACCION
        ! CONDICIONES DE BORDE
        ALLOCATE(LOX(NNT,2),LOY(NNT,2),LOZ(NNT,2))
        LOX=.FALSE.
        LOY=.FALSE.
        LOZ=.FALSE.

        !$OMP PARALLEL DO PRIVATE(I) SHARED (U0,V0,W0,LOX,LOY,LOZ)
        DO I=1,NNT
            IF ((V0(I).LE.(N1-0.44)*LCO).AND.(V0(I).GE.(N1-0.54)*LCO)) THEN
                LOX(I,1)=.TRUE.
                !LOY(I,1)=.TRUE.
                !LOZ(I,1)=.TRUE.
            END IF
            IF (V0(I).GE.(N1-0.54)*LCO) THEN
                !LOX(I,1)=.TRUE.
                LOY(I,1)=.TRUE.
                !LOZ(I,1)=.TRUE.
            END IF
        END DO
        !$OMP END PARALLEL DO
    
    CASE (339) ! TEST = 339 MODEL PROTEINA SIMPLE
        ! CONDICIONES DE BORDE
        ALLOCATE(LOX(NNT,1),LOY(NNT,1),LOZ(NNT,1))
        ALLOCATE(LOGIC(6))
        LOX=.FALSE.
        LOY=.FALSE.
        LOZ=.FALSE.

        !$OMP PARALLEL DO PRIVATE(I) SHARED (U0,V0,W0,LOX,LOY,LOZ)
        DO I=1,NNT
            ! CENTRO CARGA
            LOGIC(1)=(U0(I).LE.(M1*LCO)) !EN X, MAYOR 
            LOGIC(2)=(U0(I).GE.(M1*LCO-0.6*LCO)) !EN X, MENOR
            LOGIC(3)=(V0(I).LE.(0.6*LCO)) !EN Y, MAYOR
            LOGIC(4)=(V0(I).GE.(0.4*LCO)) !EN Y, MENOR
            LOGIC(5)=(W0(I).LE.(0.6*LCO)) !EN Z, MAYOR
            LOGIC(6)=(W0(I).GE.(0.4*LCO)) !EN Z, MENOR
            
            IF (ALL(LOGIC)) THEN
                LOX(I,1)=.TRUE.
                !LOY(I,1)=.TRUE.
                !LOZ(I,1)=.TRUE.
            END IF
        END DO
        !$OMP END PARALLEL DO
        
    CASE (700) ! TEST = 700 - TRES PUNTOS
        ! CONDICIONES DE BORDE
        ALLOCATE(LOX(NNT,1),LOY(NNT,1),LOZ(NNT,1))
        LOX=.FALSE.
        LOY=.FALSE.
        LOZ=.FALSE.

        !$OMP PARALLEL DO PRIVATE(I) SHARED (U0,V0,W0,M1,N1,L1,LOX,LOY,LOZ)
        DO I=1,NNT
            IF (BDY(I,3)) THEN
                !LOX(I,1)=.TRUE.
                LOY(I,1)=.TRUE.
                !LOZ(I,1)=.TRUE.
            END IF
        END DO
        !$OMP END PARALLEL DO
        NODECTR(1)=FINDLOC(LOY(:,1),.TRUE.,DIM=1)
        
    CASE (1001) ! TEST = ENSAYO PARA ONDA S
        ! CONDICIONES DE BORDE
        ALLOCATE(LOX(NNT,1),LOY(NNT,1),LOZ(NNT,1))
        LOX=.FALSE.
        LOY=.FALSE.
        LOZ=.FALSE.

        !$OMP PARALLEL DO PRIVATE(I) SHARED (U0,V0,W0,LOX,LOY,LOZ,LCO)
        DO I=1,NNT
            IF ((U0(I).LT.0.55*LCO).AND.(U0(I).GT.0.10*LCO)) THEN
                !LOX(I,1)=.TRUE.
                LOY(I,1)=.TRUE.
                !LOZ(I,1)=.TRUE.
            END IF
        END DO
        !$OMP END PARALLEL DO
        
    CASE DEFAULT
        WRITE(*,*) 'ERROR: INVALID TEST SELECT (LOAD)'
        PAUSE
        STOP
    END SELECT

    RETURN
    END
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------