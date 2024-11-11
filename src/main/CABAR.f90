    !-------------------------------------------------------------------------
    !-------------------------------------------------------------------------
    !
    SUBROUTINE CABAR
    IMPLICIT NONE
    !
    ! DATA:  02/07/2019
    ! AUTOR: Ignacio Iturrioz
    !        Boris N. Rojo Tanzi
    !
    !--------------------------------------------------------------------------
    ! VARIABLES LOCALES
    !--------------------------------------------------------------------------
    DOUBLE PRECISION :: LBAR1,LBAR0 ! LONGITUD DE LA BARRA EN EN TIEMPO
    DOUBLE PRECISION :: DI ! PORCENTAJE DE LA FUERZA EN LA DIRECCION I
    DOUBLE PRECISION :: POSN(2,3) ! POSICION DE LOS NODOS
    DOUBLE PRECISION :: TFORCEN(3) ! DIFERENCIA DE DISTANCIAS
    DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:,:) :: REACTION
    
    INTEGER I
    INTEGER NOI,NOF

    !--------------------------------------------------------------------------
    ! CALCULO DE LONGITUD Y DEFORMACION
    !--------------------------------------------------------------------------
    !$OMP PARALLEL DO PRIVATE(I,LBAR1,LBAR0,POSN) &
    !$OMP& SHARED(U,V,W,CN,STR,DSTR,LBAR,DT,LI,POSB)
    DO I=1,NBT
        ! LONGITUD DE LA BARRA EN UN TIEMPO ANTERIOR
        POSN(:,1)=U(1,[CN(:,I)])
        POSN(:,2)=V(1,[CN(:,I)])
        POSN(:,3)=W(1,[CN(:,I)])
        LBAR1=NORM2(POSN(2,:)-POSN(1,:))
        
        ! LONGITUD DE LA BARRA EN EL TIEMPO ACTUAL
        POSN(:,1)=U(2,[CN(:,I)])
        POSN(:,2)=V(2,[CN(:,I)])
        POSN(:,3)=W(2,[CN(:,I)])
        LBAR0=NORM2(POSN(2,:)-POSN(1,:))
        LBAR(I)=LBAR0
        
        IF ((LBAR0.LE.0.0d0).OR.(LBAR1.LE.0.0d0)) THEN
            WRITE(*,*) 'ERROR: LONG BAR <= 0.0'
            PAUSE
            STOP
        END IF

        POSB(1,I)=SUM(U(2,[CN(:,I)]))
        POSB(2,I)=SUM(V(2,[CN(:,I)]))
        POSB(3,I)=SUM(W(2,[CN(:,I)]))
        POSB(:,I)= POSB(:,I)*0.5d0
        
        STR(I)=(LBAR0-LI(I))/LI(I)
        DSTR(I)=(STR(I)-((LBAR1-LI(I))/LI(I)))/DT
        
    END DO
    !$OMP END PARALLEL DO

    !--------------------------------------------------------------------------
    ! CALCULO DE FUERZA EN LEY CONSTITUTIVA
    !--------------------------------------------------------------------------
    SELECT CASE (LAW)
    CASE (1) ! LEY LINEAL
        CALL CONSTIT_LIN
    CASE (2) ! LEY BILINEAL
        CALL CONSTIT_BI
    CASE (3) ! LEY TRILINEAL
        CALL CONSTIT_TRI
    CASE (4) ! LEY BILINEAL SHEAR
        CALL CONSTIT_BI_SHEAR
    CASE (5)
        !CALL CONSTIT_WEIB(B,STR,DSTR,R) !MODELO WEIBULL
    CASE DEFAULT
        WRITE(*,*) 'ERROR: INVALID TEST LAW'
        PAUSE
        STOP
    END SELECT
    
    !--------------------------------------------------------------------------
    ! CALCULO DE COMPONENTES DE FUERZA NODALES
    !--------------------------------------------------------------------------
    ALLOCATE(REACTION(3,NNT))
    REACTION = 0.0D0
    
    !$OMP PARALLEL DO PRIVATE(I,POSN,TFORCEN) &
    !$OMP& SHARED(U,V,W,CN,FORCE) &
    !$OMP& REDUCTION(+:REACTION)
    DO I=1,NBT
        POSN(:,1)=U(2,[CN(:,I)])
        POSN(:,2)=V(2,[CN(:,I)])
        POSN(:,3)=W(2,[CN(:,I)])
        TFORCEN = (POSN(2,:)-POSN(1,:))/LBAR(I)
        TFORCEN = TFORCEN*FORCE(1,I)
        
        REACTION(:,CN(1,I)) = REACTION(:,CN(1,I))+TFORCEN
        TFORCEN = -TFORCEN
        REACTION(:,CN(2,I)) = REACTION(:,CN(2,I))+TFORCEN   
    END DO
    !$OMP END PARALLEL DO
    
    !$OMP PARALLEL DO PRIVATE(I) &
    !$OMP& SHARED(REACTION,FRX,FRY,FRZ)
    DO I=1,NNT
        frx(i) = FRX(i) + REACTION(1,i)
        frY(i) = FRY(i) + REACTION(2,i)
        frZ(i) = FRZ(i) + REACTION(3,i)
    END DO
    !$OMP END PARALLEL DO
    
    deallocate(reaction)
    
    RETURN
    END
    !
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------