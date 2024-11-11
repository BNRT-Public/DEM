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
    
    CASE (110,111) ! TEST = ENSAYO ESTADO MIXTO CP ITALIA
        ! CONDICIONES DE BORDE
        ALLOCATE(LOX(NNT,1),LOY(NNT,1),LOZ(NNT,1))
        ALLOCATE(LOGIC(2))
        LOX=.FALSE.
        LOY=.FALSE.
        LOZ=.FALSE.

        !$OMP PARALLEL DO PRIVATE(I,LOGIC) SHARED (U0,V0,W0,LOX,LOY,LOZ)
        DO I=1,NNT
            LOGIC(1) = ((V0(I).GE.N1*LCO-0.55*LCO))
            LOGIC(2) = ((U0(I).LE.M1*LCO*0.5-2.5*LCO))

            IF (ALL(LOGIC)) THEN
                !LOX(I,1)=.TRUE.
                LOY(I,1)=.TRUE.
                !LOZ(I,1)=.TRUE.
            END IF
        END DO
        !$OMP END PARALLEL DO
        
    CASE (201) ! TEST = 201 - VIGA EMBOLADIZO ITALIA
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
    
    case (800, 812, 813, 814, 823, 824) ! test = 800 - cuatro pontos, concreto poroso
        ! condiciones de borde
        ALLOCATE(LOX(NNT,2),LOY(NNT,2),LOZ(NNT,2))
        ALLOCATE(LOGIC(4))

        LOX=.FALSE.
        LOY=.FALSE.
        LOZ=.FALSE.

        !$OMP PARALLEL DO PRIVATE(I,LOGIC) SHARED (U0,V0,W0,M1,N1,L1,LOX,LOY,LOZ)
        DO I=1,NNT
            
            ! carga izquierda
            LOGIC(1)=(U0(I).LE.(200e-3 + LCO)) !EN X, MAYOR 
            LOGIC(2)=(U0(I).GE.(200e-3)) !EN X, MENOR
            LOGIC(3)=(V0(I).LE.(n1*LCO - 0.4*lco)) !EN Y, MAYOR
            LOGIC(4)=(V0(I).GE.(n1*LCO - 0.6*LCO)) !EN Y, MENOR

            IF (ALL(LOGIC)) THEN
                !LOX(I,1)=.TRUE.
                LOY(I,1)=.TRUE.
                !LOZ(I,1)=.TRUE.
            END IF

            ! carga izquierda
            LOGIC(1)=(U0(I).LE.(350e-3 + LCO)) !EN X, MAYOR 
            LOGIC(2)=(U0(I).GE.(350e-3)) !EN X, MENOR

            IF (ALL(LOGIC)) THEN
                !LOX(I,2)=.TRUE.
                LOY(I,2)=.TRUE.
                !LOZ(I,2)=.TRUE.
            END IF
        END DO
        !$OMP END PARALLEL DO
        !NODECTR(1)=FINDLOC(LOY(:,1),.TRUE.,DIM=1)
        
    case (900, 913) ! test = 900 - tres pontos, concreto poroso
        ! condiciones de borde
        ALLOCATE(LOX(NNT,1),LOY(NNT,1),LOZ(NNT,1))
        ALLOCATE(LOGIC(4))

        LOX=.FALSE.
        LOY=.FALSE.
        LOZ=.FALSE.

        !$OMP PARALLEL DO PRIVATE(I,LOGIC) SHARED (U0,V0,W0,M1,N1,L1,LOX,LOY,LOZ)
        DO I=1,NNT
            
            ! carga
            LOGIC(1)=(U0(I).LE.(m1*LCO*0.5 + LCO)) !EN X, MAYOR 
            LOGIC(2)=(U0(I).GE.(m1*LCO*0.5)) !EN X, MENOR
            LOGIC(3)=(V0(I).LE.(n1*LCO - 0.4*lco)) !EN Y, MAYOR
            LOGIC(4)=(V0(I).GE.(n1*LCO - 0.6*LCO)) !EN Y, MENOR

            IF (ALL(LOGIC)) THEN
                !LOX(I,1)=.TRUE.
                LOY(I,1)=.TRUE.
                !LOZ(I,1)=.TRUE.
            END IF            
        END DO
        !$OMP END PARALLEL DO

    case (923)  ! test = 900 - tres pontos, concreto poroso
        ! condiciones de borde
        allocate(lox(nnt,2),loy(nnt,2),loz(nnt,2))
        allocate(logic(4))
        lox=.false.
        loy=.false.
        loz=.false.

        ! condicion de borde
        !$omp parallel do private(i,logic) shared(m1,n1,l1,lco,lox,loy,loz)
        do i=1,nnt
            ! apoyo inferior izquierdo
            logic(1)=(u0(i).le.(50e-3 + lco)) !en x, mayor 
            logic(2)=(u0(i).ge.(50e-3)) !en x, menor
            logic(3)=(v0(i).ge.(0.44*lco)) !en y, menor
            logic(4)=(v0(i).le.(0.56*lco)) !en y, mayor
            
            if (all(logic)) then
                ! lox(i,1)=.true.
                loy(i,1)=.true.
                ! loz(i,1)=.true.
            end if
            
            !apoyo inferior derecho
            logic(1)=(u0(i).le.(m1*lco-50e-3 + lco)) !en x, mayor 
            logic(2)=(u0(i).ge.(m1*lco-50e-3)) !en x, menor
            
            if (all(logic)) then
                ! lox(i,1)=.true.
                loy(i,1)=.true.
                ! loz(i,1)=.true.
            end if
            
        end do
        !$omp end parallel do

    case (930) ! test = 900 comprecion, concreto poroso
        ! condiciones de borde
        allocate(lox(nnt,1),loy(nnt,1),loz(nnt,1))
        allocate(logic(2))
        lox=.false.
        loy=.false.
        loz=.false.

        ! condicion de borde
        !$omp parallel do private(i,logic) shared(u0,v0,w0,m1,n1,l1,lox,loy,loz)
        do i=1,nnt
            ! apoyo inferior
            logic(1)=(v0(i).le.(n1*lco - 0.4*lco)) !en y, mayor
            logic(2)=(v0(i).ge.(n1*lco - 0.6*lco)) !en y, menor
            
            if (all(logic)) then
                !lox(i,1)=.true.
                loy(i,1)=.true.
                !loz(i,1)=.true.
            end if
            
        end do
        !$omp end parallel do

    case (940) ! test = traccion indirecta, concreto poroso
        ! condiciones de borde
        allocate(lox(nnt,1),loy(nnt,1),loz(nnt,1))
        allocate(logic(4))
        lox=.false.
        loy=.false.
        loz=.false.

        ! condicion de borde
        !$omp parallel do private(i,logic) shared(m1,n1,l1,lco,bdx,bdy,bdz)
        do i=1,nnt
            ! apoyo inferior
            logic(1)=(u0(i).ge.(0.5*m1*lco - 0.5*lco)) !en x, mayor 
            logic(2)=(u0(i).le.(0.5*m1*lco + 0.5*lco)) !en x, menor
            logic(3)=(v0(i).le.(n1*lco - 0.4*lco)) !en y, mayor
            logic(4)=(v0(i).ge.(n1*lco - 0.6*lco)) !en y, menor
            
            if (all(logic)) then
                !lox(i,1)=.true.
                loy(i,1)=.true.
                !loz(i,1)=.true.
            end if
        end do
        !$omp end parallel do

    CASE (1001) ! TEST = ENSAYO PARA ONDA S
        ! CONDICIONES DE BORDE
        ALLOCATE(LOX(NNT,1),LOY(NNT,1),LOZ(NNT,1))
        LOX=.FALSE.
        LOY=.FALSE.
        LOZ=.FALSE.

        !$OMP PARALLEL DO PRIVATE(I) SHARED (U0,V0,W0,LOX,LOY,LOZ,LCO)
        DO I=1,NNT
            IF ((U0(I).LT.0.55*LCO).AND.(U0(I).GT.0.20*LCO)) THEN
                !LOX(I,1)=.TRUE.
                LOY(I,1)=.TRUE.
                !LOZ(I,1)=.TRUE.
            END IF
        END DO
        !$OMP END PARALLEL DO
    
    CASE (2001) ! TEST = ACELERACION EN PROTEINA
        ! CONDICIONES DE BORDE
        ALLOCATE(LOX(NNT,1),LOY(NNT,1),LOZ(NNT,1))
        LOX=.FALSE.
        LOY=.FALSE.
        LOZ=.FALSE.
        ! lysozyme
        LOz(5,1) = .true.
        ! ATPase
        !LOy(784,1) = .true.
        
    CASE (3001) ! TEST = AFRIDI SHEAR
        ! CONDICIONES DE BORDE
        ALLOCATE(LOX(NNT,2),LOY(NNT,2),LOZ(NNT,2))
        ALLOCATE(LOGIC(2))
        LOX=.FALSE.
        LOY=.FALSE.
        LOZ=.FALSE.

        !$OMP PARALLEL DO PRIVATE(I,LOGIC) SHARED (U0,V0,W0,lco,LOX,LOY,LOZ)
        DO I=1,NNT
            ! Compracion
            LOGIC = .true.
            LOGIC(1)=(W0(I).Gt.((L1-0.55)*LCO))
            
            IF (LOGIC(1)) THEN
                LOz(I,1)=.True.
            END IF
                        
            ! Shear
            ! Superior
            LOGIC = .true.
            LOGIC(1)=(W0(I).Gt.((L1-0.55)*LCO)) !EN z
            IF (ALL(LOGIC)) THEN
                LOX(I,2)=.True.
            END IF
                        
            ! PLANO XZ=0
            LOGIC(1)=(u0(I).LT.(0.55*LCO)) !EN X
            LOGIC(2)=(W0(I).gE.(0.5*((L1*LCO)+5.0d-3))) !EN z
            IF (ALL(LOGIC)) THEN
                LOX(I,2)=.True.
            END IF
            
            ! PLANO XZ=max
            LOGIC(1)=(u0(I).Gt.(M1-0.55)*LCO) !EN X
            LOGIC(2)=(W0(I).gE.(0.5*((L1*LCO)+5.0d-3))) !EN z
            IF (ALL(LOGIC)) THEN
                LOX(I,2)=.True.
            END IF
            
            ! PLANO yZ=0
            LOGIC(1)=(v0(I).LT.(0.55*LCO)) !EN y
            LOGIC(2)=(W0(I).gE.(0.5*((L1*LCO)+5.0d-3))) !EN z
            IF (ALL(LOGIC)) THEN
                LOX(I,2)=.True.
            END IF
                       
            ! PLANO yZ=max
            LOGIC(1)=(v0(I).Gt.(n1-0.55)*LCO) !EN y
            LOGIC(2)=(W0(I).gE.(0.5*((L1*LCO)+5.0d-3))) !EN z
            IF (ALL(LOGIC)) THEN
                LOX(I,2)=.True.
            END IF
        !    
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