    !-------------------------------------------------------------------------
    !-------------------------------------------------------------------------
    !
    SUBROUTINE CABAR
    !
    ! DATA:  02/07/2019
    ! AUTOR: Ignacio Iturrioz
    !        Boris N. Rojo Tanzi
    !
    !--------------------------------------------------------------------------
    ! VARIABLES LOCALES
    !--------------------------------------------------------------------------
    DOUBLE PRECISION :: LBAR1 ! LONGITUD DE LA BARRA EN EN TIEMPO ANTERIOR
    DOUBLE PRECISION :: DI ! PORCENTAJE DE LA FUERZA EN LA DIRECCION I
    DOUBLE PRECISION :: DU,DV,DW ! DIFERENCIA DE DISTANCIAS
    INTEGER I

    !--------------------------------------------------------------------------
    ! CALCULO DE LONGITUD Y DEFORMACION
    !--------------------------------------------------------------------------
    !$OMP PARALLEL DO PRIVATE(I,LBAR1,DU,DV,DW) SHARED(U,V,W,CN,STR,DSTR,LBAR,DT,LI,XB,YB,ZB)
    DO I=1,NBT
        ! LONGITUD DE LA BARRA EN UN TIEMPO ANTERIOR
        DU=(U(1,CN(2,I))-U(1,CN(1,I)))
        DV=(V(1,CN(2,I))-V(1,CN(1,I)))
        DW=(W(1,CN(2,I))-W(1,CN(1,I)))
        LBAR1=DSQRT(DU*DU+DV*DV+DW*DW)

        ! LONGITUD DE LA BARRA EN EL TIEMPO ACTUAL
        DU=(U(2,CN(2,I))-U(2,CN(1,I)))
        DV=(V(2,CN(2,I))-V(2,CN(1,I)))
        DW=(W(2,CN(2,I))-W(2,CN(1,I)))
        LBAR(I)=DSQRT(DU*DU+DV*DV+DW*DW)
        
        IF ((LBAR(I).LE.0.0).OR.(LBAR1.LE.0.0)) THEN
            WRITE(*,*) 'ERROR: LONG BAR <0'
            PAUSE
            STOP
        END IF

        XB(I)=(U(2,CN(2,I))+U(2,CN(1,I)))*0.5
        YB(I)=(V(2,CN(2,I))+V(2,CN(1,I)))*0.5
        ZB(I)=(W(2,CN(2,I))+W(2,CN(1,I)))*0.5

        STR(I)=(LBAR(I)-LI(I))/LI(I)
        LBAR1=(LBAR1-LI(I))/LI(I)
        DSTR(I)=(STR(I)-LBAR1)/DT

    END DO
    !$OMP END PARALLEL DO

    !--------------------------------------------------------------------------
    ! CALCULO DE FUERZA EN LEY CONSTITUTIVA
    !--------------------------------------------------------------------------
    SELECT CASE (LAW)
    CASE (1) ! LEY LINEAL
        !CALL CONSTIT_LIN
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
    !$OMP PARALLEL DO PRIVATE(I,DI) SHARED(U,V,W,CN,FORCE,FRX,FRY,FRZ,LBAR)
    DO I=1,NBT
        ! CALCULO DE FUERZAS EN X DE LA BARRA
        DI=U(2,CN(2,I))-U(2,CN(1,I))
        DI=DI/LBAR(I)
        FRX(CN(1,I))=FRX(CN(1,I))+FORCE(1,I)*DI
        FRX(CN(2,I))=FRX(CN(2,I))-FORCE(1,I)*DI

        ! CALCULO DE FUERZAS EN Y DE LA BARRA
        DI=V(2,CN(2,I))-V(2,CN(1,I))
        DI=DI/LBAR(I)
        FRY(CN(1,I))=FRY(CN(1,I))+FORCE(1,I)*DI
        FRY(CN(2,I))=FRY(CN(2,I))-FORCE(1,I)*DI

        ! CALCULO DE FUERZAS EN Z DE LA BARRA
        DI=W(2,CN(2,I))-W(2,CN(1,I))
        DI=DI/LBAR(I)
        FRZ(CN(1,I))=FRZ(CN(1,I))+FORCE(1,I)*DI
        FRZ(CN(2,I))=FRZ(CN(2,I))-FORCE(1,I)*DI
        
    END DO
    !$OMP END PARALLEL DO

    RETURN
    END
    !
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------