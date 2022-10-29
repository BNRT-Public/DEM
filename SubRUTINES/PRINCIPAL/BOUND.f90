    !-------------------------------------------------------------------------
    !-------------------------------------------------------------------------
    !
    SUBROUTINE BOUND
    !
    ! DATA:  02/07/2019
    ! AUTOR: Boris N. Rojo Tanzi
    !
    !--------------------------------------------------------------------------
    ! VARIABLES LOCALES
    !--------------------------------------------------------------------------
    INTEGER I

    LOGICAL,ALLOCATABLE,DIMENSION(:) :: LOGIC
    LOGICAL,ALLOCATABLE,DIMENSION(:,:) :: LOGICr
    LOGICAL,ALLOCATABLE,DIMENSION(:) :: LOGICrT
    real :: start, finish

    !--------------------------------------------------------------------------
    ! APLICAR LAS CONDICIONES DE BORDE PARA CADA CASO
    !--------------------------------------------------------------------------
    SELECT CASE (TEST)
    CASE (101,102) ! TEST = 101/102 - VIGA EMBOLADIZO
        ! CONDICIONES DE BORDE
        ALLOCATE(BDX(NNT,2),BDY(NNT,2),BDZ(NNT,2))
        BDX=.FALSE.
        BDY=.FALSE.
        BDZ=.TRUE.

        ! CONDICION DE BORDE
        !$OMP PARALLEL DO PRIVATE(I) SHARED(M1,N1,L1,LCO,BDX,BDY,BDZ)
        DO I=1,NNT
            ! IZQUIERDO FIJO
            IF (U0(I).LE.(0.51*LCO)) THEN
                BDX(I,2)=.TRUE.
                BDY(I,2)=.TRUE.
                BDZ(I,2)=.TRUE.
            END IF
        END DO
        !$OMP END PARALLEL DO
    CASE (110) ! TEST = 337 O 338 TRACCION
        ! CONDICIONES DE BORDE
        ALLOCATE(BDX(NNT,2),BDY(NNT,2),BDZ(NNT,2))
        BDX=.FALSE.
        BDY=.FALSE.
        BDZ=.FALSE.

        ! CONDICION DE BORDE
        !$OMP PARALLEL DO PRIVATE(I) SHARED(M1,N1,L1,LCO,BDX,BDY,BDZ)
        DO I=1,NNT
            ! SUPERIOR FIJO
            !IF (V0(I).GE.((N1-0.51)*LCO)) THEN
            !    BDX(I,1)=.TRUE.
            !    BDY(I,1)=.TRUE.
            !    BDZ(I,1)=.TRUE.
            !END IF
            ! INFERIOR FIJO MAS CONDICIONES DE BORDE
            IF (V0(I).LE.(0.51*LCO)) THEN
                BDX(I,2)=.TRUE.
                BDY(I,2)=.TRUE.
                BDZ(I,2)=.TRUE.
            END IF
        END DO
        !$OMP END PARALLEL DO
        
    CASE (337,338) ! TEST = 337 O 338 TRACCION
        ! CONDICIONES DE BORDE
        ALLOCATE(BDX(NNT,2),BDY(NNT,2),BDZ(NNT,2))
        BDX=.FALSE.
        BDY=.FALSE.
        BDZ=.FALSE.

        ! CONDICION DE BORDE
        !$OMP PARALLEL DO PRIVATE(I) SHARED(M1,N1,L1,LCO,BDX,BDY,BDZ)
        DO I=1,NNT
            ! SUPERIOR FIJO
            !IF (V0(I).GE.((N1-0.51)*LCO)) THEN
            !    BDX(I,1)=.TRUE.
            !    BDY(I,1)=.TRUE.
            !    BDZ(I,1)=.TRUE.
            !END IF
            ! INFERIOR FIJO MAS CONDICIONES DE BORDE
            IF (V0(I).LE.(0.51*LCO)) THEN
                BDX(I,2)=.TRUE.
                BDY(I,2)=.TRUE.
                BDZ(I,2)=.TRUE.
            END IF
        END DO
        !$OMP END PARALLEL DO
    
    CASE (339) ! TEST = 339 MODEL PROTEINA SIMPLE
        ! CONDICIONES DE BORDE
        ALLOCATE(BDX(NNT,1),BDY(NNT,1),BDZ(NNT,1))
        ALLOCATE(LOGIC(6))
        BDX=.FALSE.
        BDY=.FALSE.
        BDZ=.FALSE.
        
        ! CONDICION DE BORDE
        !$OMP PARALLEL DO PRIVATE(I,LOGIC) SHARED(M1,N1,L1,LCO,BDX,BDY,BDZ)
        DO I=1,NNT
            ! CENTRO FIJO
            LOGIC(1)=(U0(I).LE.(0.6*LCO)) !EN X, MAYOR 
            LOGIC(2)=(U0(I).GE.(0.0*LCO)) !EN X, MENOR
            LOGIC(3)=(V0(I).LE.(0.6*LCO)) !EN Y, MAYOR
            LOGIC(4)=(V0(I).GE.(0.4*LCO)) !EN Y, MENOR
            LOGIC(5)=(W0(I).LE.(0.6*LCO)) !EN Z, MAYOR
            LOGIC(6)=(W0(I).GE.(0.4*LCO)) !EN Z, MENOR
            
            IF (ALL(LOGIC)) THEN
                BDX(I,1)=.TRUE.
                BDY(I,1)=.TRUE.
                BDZ(I,1)=.TRUE.
            END IF
        END DO
        !$OMP END PARALLEL DO
        
    CASE (700) ! TEST = 700 TRES PUNTOS
        ! CONDICIONES DE BORDE
        ALLOCATE(BDX(NNT,3),BDY(NNT,3),BDZ(NNT,3))
        ALLOCATE(LOGIC(4))
        BDX=.FALSE.
        BDY=.FALSE.
        BDZ=.FALSE.

        ! CONDICION DE BORDE
        !$OMP PARALLEL DO PRIVATE(I,LOGIC) SHARED(M1,N1,L1,LCO,BDX,BDY,BDZ)
        DO I=1,NNT
            ! APOYO INFERIOR IZQUIERDO
            LOGIC(1)=(U0(I).LE.(3.60*LCO)) !EN X, MAYOR 
            LOGIC(2)=(U0(I).GE.(3.30*LCO)) !EN X, MENOR
            LOGIC(3)=(V0(I).GE.(0.44*LCO)) !EN Y, MENOR
            LOGIC(4)=(V0(I).LE.(0.56*LCO)) !EN Y, MAYOR
            
            IF (ALL(LOGIC)) THEN
                BDX(I,1)=.TRUE.
                BDY(I,1)=.TRUE.
                BDZ(I,1)=.TRUE.
            END IF
            
            !APOYO INFERIOR DERECHO
            LOGIC(1)=(U0(I).GE.(M1-3.60)*LCO) !EN X, MAYOR 
            LOGIC(2)=(U0(I).LE.(M1-3.30)*LCO) !EN X, MENOR
            
            IF (ALL(LOGIC)) THEN
                BDY(I,2)=.TRUE.
            END IF
            
            ! CARGA SUPERIOR CENTRO
            LOGIC(3)=(U0(I).LE.((M1*0.5+0.20)*LCO)) !EN X, MAYOR
            LOGIC(4)=(U0(I).GE.((M1*0.5-0.20)*LCO)) !EN X, MENOR
            LOGIC(1)=(V0(I).GE.((N1-0.60)*LCO)) !EN Y, MENOR 
            LOGIC(2)=(V0(I).LE.((N1-0.40)*LCO)) !EN Y, MAYOR
            
            IF (ALL(LOGIC)) THEN
                BDY(I,3)=.TRUE.
            END IF
        END DO
        !$OMP END PARALLEL DO

        CASE (1001) ! TEST = ENSAYO PARA ONDA S
        ! CONDICIONES DE BORDE
        ALLOCATE(BDX(NNT,2),BDY(NNT,2),BDZ(NNT,2))
        ALLOCATE(LOGIC(2))
        BDX=.FALSE.
        BDY=.FALSE.
        BDZ=.FALSE.

        ! CONDICION DE BORDE
        !$OMP PARALLEL DO PRIVATE(I,LOGIC) SHARED(M1,N1,L1,LCO,BDX,BDY,BDZ)
        DO I=1,NNT
            ! UX = 0 EN PLANO XZ=0
            LOGIC(1)=(V0(I).GT.(-0.10*LCO)) !EN Y, MENOR
            LOGIC(2)=(V0(I).LE.( 0.49*LCO)) !EN Y, MAYOR
            
            IF (ALL(LOGIC)) THEN
                BDX(I,1)=.TRUE.
                !BDY(I,1)=.FALSE.
                !BDZ(I,1)=.FALSE.
            END IF
            
            ! UX = 0 EN PLANO XZ=MAX
            LOGIC(1)=(V0(I).GE.(N1-0.49)*LCO) !EN X, MENOR 
            LOGIC(2)=(V0(I).LT.(N1+0.10)*LCO) !EN X, MAYOR
            
            IF (ALL(LOGIC)) THEN
                BDX(I,1)=.TRUE.
                !BDY(I,1)=.FALSE.
                !BDZ(I,1)=.FALSE.
            END IF
            
            ! UX = 0 EN PLANO XY=0
            LOGIC(1)=(W0(I).GT.(-0.10*LCO)) !EN Y, MENOR
            LOGIC(2)=(W0(I).LE.( 0.49*LCO)) !EN Y, MAYOR
            
            IF (ALL(LOGIC)) THEN
                BDX(I,2)=.TRUE.
                !BDY(I,2)=.FALSE.
                !BDZ(I,2)=.FALSE.
            END IF
            
            ! UX = 0 EN PLANO XY=MAX
            LOGIC(1)=(W0(I).GE.(L1-0.49)*LCO) !EN X, MENOR 
            LOGIC(2)=(W0(I).LT.(L1+0.10)*LCO) !EN X, MAYOR
            
            IF (ALL(LOGIC)) THEN
                BDX(I,2)=.TRUE.
                !BDY(I,2)=.FALSE.
                !BDZ(I,2)=.FALSE.
            END IF
        END DO
        !$OMP END PARALLEL DO
        
        CASE DEFAULT
        WRITE(*,*) 'ERROR: INVALID TEST SELECT (BOUND)'
        PAUSE
        STOP
        END SELECT

    IF (ALLOCATED(LOGIC)) DEALLOCATE(LOGIC)
    
    !--------------------------------------------------------------------------
    ! APLICAR LAS CONDICIONES DE BORDE SYM/ASYM
    !--------------------------------------------------------------------------
    BDSYM(1)=.FALSE. ! PLANO YZ ENTRE X=0 y X=MAX
    BDSYM(2)=.FALSE. ! PLANO XZ ENTRE Y=0 y Y=MAX
    BDSYM(3)=.FALSE. ! PLANO XY ENTRE Z=0 y Z=MAX
    BDASYM(1)=.FALSE. ! PLANO YZ ENTRE X=0 y X=MAX
    BDASYM(2)=.FALSE. ! PLANO XZ ENTRE Y=0 y Y=MAX
    BDASYM(3)=.FALSE. ! PLANO XY ENTRE Z=0 y Z=MAX
    
    RETURN
    END
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------