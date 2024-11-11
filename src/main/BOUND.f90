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
        
    CASE (110) ! TEST = ENSAYO ESTADO MIXTO CP ITALIA
        ! CONDICIONES DE BORDE
        ALLOCATE(BDX(NNT,2),BDY(NNT,2),BDZ(NNT,2))
        ALLOCATE(LOGIC(2))
        BDX=.FALSE.
        BDY=.FALSE.
        BDZ=.FALSE.

        ! CONDICION DE BORDE
        !$OMP PARALLEL DO PRIVATE(I,LOGIC) SHARED(M1,N1,L1,LCO,BDX,BDY,BDZ)
        DO I=1,NNT
            ! SUPERIOR FIJO
            LOGIC(1) = ((V0(I).GE.N1*LCO-0.55*LCO))
            !LOGIC(2) = ((U0(I).GE.M1*LCO*0.5+LCO))
            LOGIC(2) = ((U0(I).GE.M1*LCO-25e-3+LCO))
            IF (ALL(LOGIC)) THEN
                !BDX(I,1)=.TRUE.
                BDY(I,1)=.TRUE.
                !BDZ(I,1)=.TRUE.
            END IF
            ! INFERIOR FIJO
            LOGIC(1) = (V0(I).LE.(0.55*LCO))
            !LOGIC(2) = (U0(I).GE.(M1*LCO*0.5-20e-3*DSQRT(2.0D0)*0.5))
            !LOGIC(2) = (U0(I).GE.(M1*LCO*0.5-10e-3*DSQRT(2.0D0)*0.5))
            LOGIC(2) = (U0(I).GE.(M1*LCO*0.5-15e-3*DSQRT(2.0D0)*0.5))
            IF (ALL(LOGIC)) THEN
                BDX(I,2)=.TRUE.
                BDY(I,2)=.TRUE.
                BDZ(I,2)=.TRUE.
            END IF
            !! CARGA ALINEADA
            !LOGIC(1) = ((V0(I).GE.N1*LCO-0.55*LCO))
            !LOGIC(2) = ((U0(I).LE.M1*LCO*0.5-1.25*LCO))
            !IF (ALL(LOGIC)) THEN
            !    BDX(I,3)=.TRUE.
            !    !BDY(I,3)=.TRUE.
            !    BDZ(I,3)=.TRUE.
            !END IF
        END DO
        !$OMP END PARALLEL DO
    
    CASE (111) ! TEST = ENSAYO ESTADO MIXTO CP ITALIA
        ! CONDICIONES DE BORDE
        ALLOCATE(BDX(NNT,3),BDY(NNT,3),BDZ(NNT,3))
        ALLOCATE(LOGIC(2))
        BDX=.FALSE.
        BDY=.FALSE.
        BDZ=.FALSE.

        ! CONDICION DE BORDE
        !$OMP PARALLEL DO PRIVATE(I,LOGIC) SHARED(M1,N1,L1,LCO,BDX,BDY,BDZ)
        DO I=1,NNT
            ! SUPERIOR FIJO
            LOGIC(1) = ((V0(I).GE.N1*LCO-0.55*LCO))
            LOGIC(2) = ((U0(I).GE.M1*LCO-25e-3+LCO))
            IF (ALL(LOGIC)) THEN
                !BDX(I,1)=.TRUE.
                BDY(I,1)=.TRUE.
                !BDZ(I,1)=.TRUE.
            END IF
            ! INFERIOR FIJO
            LOGIC(1) = (V0(I).LE.(0.55*LCO))
            LOGIC(2) = (U0(I).GE.(M1*LCO*0.5))
            IF (ALL(LOGIC)) THEN
                BDX(I,2)=.TRUE.
                BDY(I,2)=.TRUE.
                BDZ(I,2)=.TRUE.
            END IF
            ! CARGA ALINEADA
            LOGIC(1) = ((V0(I).GE.N1*LCO-0.55*LCO))
            LOGIC(2) = ((U0(I).LE.M1*LCO*0.5-1.25*LCO))
            IF (ALL(LOGIC)) THEN
                BDX(I,3)=.TRUE.
                !BDY(I,3)=.TRUE.
                BDZ(I,3)=.TRUE.
            END IF
        END DO
        !$OMP END PARALLEL DO
    
    CASE (201) ! TEST = 201 - VIGA EMBOLADIZO ITALIA
        ! CONDICIONES DE BORDE
        ALLOCATE(BDX(NNT,1),BDY(NNT,1),BDZ(NNT,1))
        BDX=.FALSE.
        BDY=.FALSE.
        BDZ=.FALSE.

        ! CONDICION DE BORDE
        !$OMP PARALLEL DO PRIVATE(I) SHARED(M1,N1,L1,LCO,BDX,BDY,BDZ)
        DO I=1,NNT
            ! IZQUIERDO FIJO
            IF (U0(I).LE.(0.51*LCO)) THEN
                BDX(I,1)=.TRUE.
                BDY(I,1)=.TRUE.
                BDZ(I,1)=.TRUE.
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

    CASE (800, 812, 813, 814, 823, 824) ! TEST = 800 cuatro PUNTOS, concreto poroso
        ! CONDICIONES DE BORDE
        ALLOCATE(BDX(NNT,2),BDY(NNT,2),BDZ(NNT,2))
        ALLOCATE(LOGIC(4))
        BDX=.FALSE.
        BDY=.FALSE.
        BDZ=.FALSE.

        ! CONDICION DE BORDE
        !$OMP PARALLEL DO PRIVATE(I,LOGIC) SHARED(M1,N1,L1,LCO,BDX,BDY,BDZ)
        DO I=1,NNT
            ! APOYO INFERIOR IZQUIERDO
            LOGIC(1)=(U0(I).LE.(50e-3 + lco)) !EN X, MAYOR 
            LOGIC(2)=(U0(I).GE.(50e-3)) !EN X, MENOR
            LOGIC(3)=(V0(I).GE.(0.44*LCO)) !EN Y, MENOR
            LOGIC(4)=(V0(I).LE.(0.56*LCO)) !EN Y, MAYOR
            
            IF (ALL(LOGIC)) THEN
                BDX(I,1)=.TRUE.
                BDY(I,1)=.TRUE.
                BDZ(I,1)=.TRUE.
            END IF
            
            !APOYO INFERIOR DERECHO
            LOGIC(1)=(U0(I).LE.(500e-3 + lco)) !EN X, MAYOR 
            LOGIC(2)=(U0(I).GE.(500e-3)) !EN X, MENOR
            
            IF (ALL(LOGIC)) THEN
                BDY(I,2)=.TRUE.
            END IF
            
        END DO
        !$OMP END PARALLEL DO
    
    CASE (900, 913) ! TEST = 900 tres PUNTOS, concreto poroso
        ! condiciones de borde
        allocate(bdx(nnt,2),bdy(nnt,2),bdz(nnt,2))
        allocate(logic(4))
        bdx=.false.
        bdy=.false.
        bdz=.false.

        ! condicion de borde
        !$omp parallel do private(i,logic) shared(m1,n1,l1,lco,bdx,bdy,bdz)
        do i=1,nnt
            ! apoyo inferior izquierdo
            logic(1)=(u0(i).le.(50e-3 + lco)) !en x, mayor 
            logic(2)=(u0(i).ge.(50e-3)) !en x, menor
            logic(3)=(v0(i).ge.(0.44*lco)) !en y, menor
            logic(4)=(v0(i).le.(0.56*lco)) !en y, mayor
            
            if (all(logic)) then
                bdx(i,1)=.true.
                bdy(i,1)=.true.
                bdz(i,1)=.true.
            end if
            
            !apoyo inferior derecho
            logic(1)=(u0(i).le.(m1*lco-50e-3 + lco)) !en x, mayor 
            logic(2)=(u0(i).ge.(m1*lco-50e-3)) !en x, menor
            
            if (all(logic)) then
                bdy(i,2)=.true.
            end if
            
        end do
        !$omp end parallel do

    case (923) ! TEST = 900 tres PUNTOS, concreto poroso
        ! condiciones de borde
        allocate(bdx(nnt,1),bdy(nnt,1),bdz(nnt,1))
        allocate(logic(4))

        bdx=.false.
        bdy=.false.
        bdz=.false.

        !$omp parallel do private(i,logic) shared (u0,v0,w0,m1,n1,l1,bdx,bdy,bdz)
        do i=1,nnt
            
            ! carga
            logic(1)=(u0(i).le.(m1*lco*0.5 + lco)) !en x, mayor 
            logic(2)=(u0(i).ge.(m1*lco*0.5)) !en x, menor
            logic(3)=(v0(i).le.(n1*lco - 0.4*lco)) !en y, mayor
            logic(4)=(v0(i).ge.(n1*lco - 0.6*lco)) !en y, menor

            if (all(logic)) then
                bdx(i,1)=.true.
                bdy(i,1)=.true.
                bdz(i,1)=.true.
            end if            
        end do
        !$omp end parallel do
        
    case (930) ! test = 900 comprecion, concreto poroso
        ! condiciones de borde
        allocate(bdx(nnt,1),bdy(nnt,1),bdz(nnt,1))
        allocate(logic(2))
        bdx=.false.
        bdy=.false.
        bdz=.false.

        ! condicion de borde
        !$omp parallel do private(i,logic) shared(lco,u0,v0,w0,m1,n1,l1,bdx,bdy,bdz)
        do i=1,nnt
            ! apoyo inferior
            logic(1)=(v0(i).ge.(0.44*lco)) !en y, menor
            logic(2)=(v0(i).le.(0.56*lco)) !en y, mayor
            
            if (all(logic)) then
                bdx(i,1)=.true.
                bdy(i,1)=.true.
                bdz(i,1)=.true.
            end if
            
        end do
        !$omp end parallel do

    case (940) ! test = traccion indirecta, concreto poroso
        ! condiciones de borde
        allocate(bdx(nnt,1),bdy(nnt,1),bdz(nnt,1))
        allocate(logic(4))
        bdx=.false.
        bdy=.false.
        bdz=.false.

        ! condicion de borde
        !$omp parallel do private(i,logic) shared(lco,u0,v0,w0,m1,n1,l1,bdx,bdy,bdz)
        do i=1,nnt
            ! apoyo inferior
            logic(1)=(u0(i).ge.(0.5*m1*lco - 0.5*lco)) !en x, mayor 
            logic(2)=(u0(i).le.(0.5*m1*lco + 0.5*lco)) !en x, menor
            logic(3)=(v0(i).ge.(0.44*lco)) !en y, menor
            logic(4)=(v0(i).le.(0.56*lco)) !en y, mayor
            
            if (all(logic)) then
                bdx(i,1)=.true.
                bdy(i,1)=.true.
                bdz(i,1)=.true.
            end if
        end do
        !$omp end parallel do

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
            LOGIC(1)=(V0(I).GT.(-0.20*LCO)) !EN Y, MENOR
            LOGIC(2)=(V0(I).LE.( 0.49*LCO)) !EN Y, MAYOR
            
            IF (ALL(LOGIC)) THEN
                BDX(I,1)=.TRUE.
                !BDY(I,1)=.FALSE.
                !BDZ(I,1)=.FALSE.
            END IF
            
            ! UX = 0 EN PLANO XZ=MAX
            LOGIC(1)=(V0(I).GE.(N1-0.49)*LCO) !EN X, MENOR 
            LOGIC(2)=(V0(I).LT.(N1+0.20)*LCO) !EN X, MAYOR
            
            IF (ALL(LOGIC)) THEN
                BDX(I,1)=.TRUE.
                !BDY(I,1)=.FALSE.
                !BDZ(I,1)=.FALSE.
            END IF
            
            ! UX = 0 EN PLANO XY=0
            LOGIC(1)=(W0(I).GT.(-0.20*LCO)) !EN Y, MENOR
            LOGIC(2)=(W0(I).LE.( 0.49*LCO)) !EN Y, MAYOR
            
            IF (ALL(LOGIC)) THEN
                BDX(I,2)=.TRUE.
                !BDY(I,2)=.FALSE.
                !BDZ(I,2)=.FALSE.
            END IF
            
            ! UX = 0 EN PLANO XY=MAX
            LOGIC(1)=(W0(I).GE.(L1-0.49)*LCO) !EN X, MENOR 
            LOGIC(2)=(W0(I).LT.(L1+0.20)*LCO) !EN X, MAYOR
            
            IF (ALL(LOGIC)) THEN
                BDX(I,2)=.TRUE.
                !BDY(I,2)=.FALSE.
                !BDZ(I,2)=.FALSE.
            END IF
        END DO
        !$OMP END PARALLEL DO
        
        CASE (2001) ! TEST = ACELERACION EN PROTEINA
        ! CONDICIONES DE BORDE
        ALLOCATE(BDX(NNT,1),BDY(NNT,1),BDZ(NNT,1))
        BDX=.FALSE.
        BDY=.FALSE.
        BDZ=.FALSE.
        !bdX(170,1) = .true.
        !bdy(170,1) = .true.
        !bdz(170,1) = .true.
        !bdX(173,1) = .true.
        !bdy(173,1) = .true.
        !bdz(173,1) = .true.
        
        CASE (3001) ! TEST = AFRIDI SHEAR
        ! CONDICIONES DE BORDE
        ALLOCATE(LOGIC(2))
        ALLOCATE(BDX(NNT,3),BDY(NNT,3),BDZ(NNT,3))
        BDX=.FALSE.
        BDY=.FALSE.
        BDZ=.FALSE.
        ! CONDICION DE BORDE
        !$OMP PARALLEL DO PRIVATE(I,LOGIC) SHARED(M1,N1,L1,LCO,BDX,BDY,BDZ)
        DO I=1,NNT
            ! INFERIOR
            LOGIC = .true.
            LOGIC(1)=(W0(I).Lt.(0.55*LCO)) !EN Y, MAYOR
            
            IF (ALL(LOGIC)) THEN
                BDX(I,1)=.TRUE.
                BDY(I,1)=.FALSE.
                BDZ(I,1)=.FALSE.
            END IF
            
            ! PLANO XZ=0
            LOGIC(1)=(u0(I).LT.(0.55*LCO)) !EN X
            LOGIC(2)=(W0(I).LE.(0.5*((L1*LCO)-0.005))) !EN z
            
            IF (ALL(LOGIC)) THEN
                BDX(I,2)=.TRUE.
            END IF
            
            ! PLANO XZ=max
            LOGIC(1)=(u0(I).Gt.(M1-0.55)*LCO) !EN X
            LOGIC(2)=(W0(I).LE.(0.5*((L1*LCO)-0.005))) !EN z
                        
            IF (ALL(LOGIC)) THEN
                BDX(I,2)=.TRUE.
            END IF
            
            ! PLANO yZ=0
            LOGIC(1)=(v0(I).LT.(0.55*LCO)) !EN y
            LOGIC(2)=(W0(I).LE.(0.5*((L1*LCO)-0.005))) !EN z
            
            IF (ALL(LOGIC)) THEN
                BDY(I,3)=.TRUE.
            END IF
                        
            ! PLANO yZ=max
            LOGIC(1)=(v0(I).Gt.(n1-0.55)*LCO) !EN y
            LOGIC(2)=(W0(I).LE.(0.5*((L1*LCO)-0.005))) !EN z
                        
            IF (ALL(LOGIC)) THEN
                BDy(I,3)=.TRUE.
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