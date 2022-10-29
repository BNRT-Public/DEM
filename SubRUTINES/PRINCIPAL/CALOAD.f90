    !-------------------------------------------------------------------------
    !-------------------------------------------------------------------------
    !
    SUBROUTINE CALOAD
    IMPLICIT NONE
    !
    ! DATA:  06/07/2019
    ! AUTOR: Boris N. Rojo Tanzi
    !
    ! ESTA SUBRUTINA APLICA CARGA AL MODELO
    !
    !--------------------------------------------------------------------------
    ! VARIABLES LOCALES
    !--------------------------------------------------------------------------
    INTEGER I
    DOUBLE PRECISION PI
    DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:) :: AUX
    
    !--------------------------------------------------------------------------
    ! ASIGNACION DE VARIABLES
    !--------------------------------------------------------------------------
    PI=4.0*DATAN(1.0D00)
    FRX=0.0
    FRY=0.0
    FRZ=0.0
    
    !--------------------------------------------------------------------------
    ! APLICA CARGA Y/O DESPLAZAMIENTO
    !--------------------------------------------------------------------------
    SELECT CASE (TEST)
    CASE (110) ! TEST = 337 - TRACCION CON VELOCIDAD LINEAL
        ALLOCATE(AUX(1))
        AUX(1) = 1.0D0/120.0D0 ! VELOCIDAD: 10 m/min
        !$OMP PARALLEL DO PRIVATE(I) SHARED (U0,V0,W0,LOX,LOY,LOZ,TIME,AUX)
        DO I=1,NNT
            IF (LOY(I,1)) THEN
                V(2,I)=V0(I)-TIME*AUX(1)
            END IF
        END DO
        !$OMP END PARALLEL DO
        
    CASE (337) ! TEST = 337 - TRACCION CON VELOCIDAD LINEAL
        ALLOCATE(AUX(1))
        AUX(1) = 1.0D0/120.0D0 ! VELOCIDAD: 10 m/min
        !$OMP PARALLEL DO PRIVATE(I) SHARED (U0,V0,W0,LOX,LOY,LOZ,TIME,AUX)
        DO I=1,NNT
            IF (LOY(I,1)) THEN
                V(2,I)=V0(I)-TIME*AUX(1)
            END IF
        END DO
        !$OMP END PARALLEL DO
    
    CASE (338) ! TEST = 338 - TRACCION CON FUNCION CUADRATICA
        ALLOCATE(AUX(1))
        AUX(1) = -7.0D-3
        !$OMP PARALLEL DO PRIVATE(I) SHARED (U0,V0,W0,LOX,LOY,LOZ,TIME,AUX)
        DO I=1,NNT
            IF (LOX(I,1)) THEN
                U(2,I)=U0(I)+TIME*AUX(1)
            END IF
            IF (LOY(I,2)) THEN
                FRY(I)=-10
            END IF
        END DO
        !$OMP END PARALLEL DO
        
    CASE (339) ! TEST = 339 MODEL PROTEINA SIMPLE
        ALLOCATE(AUX(1))
        AUX(1) = 1.0D0/120.0D0 ! VELOCIDAD: 10 m/min
        !$OMP PARALLEL DO PRIVATE(I) SHARED (U0,V0,W0,LOX,LOY,LOZ,TIME,AUX)
        DO I=1,NNT
            IF (LOY(I,1)) THEN
                V(2,I)=V0(I)-TIME*AUX(1)
            END IF
        END DO
        !$OMP END PARALLEL DO
        
    CASE (700) ! TEST = 700 - TRES PUNTOS
        !$OMP PARALLEL DO PRIVATE(I) SHARED (U0,V0,W0,LOX,LOY,LOZ,TIME)
        DO I=1,NNT
            IF (LOY(I,1)) THEN
                V(2,I)=V0(I)-TIME/1200
            END IF
        END DO
        !$OMP END PARALLEL DO
        
    CASE (1001) ! TEST = ENSAYO PARA ONDA S
        ALLOCATE(AUX(5))
        AUX(1) = 100.0D-6*0.25 !Amplitud
        AUX(2) = 20.0 !Frecuencia Hz
        AUX(3) = 0.0 !Tiempo s
        AUX(4) = 0.1 !Tiempo s
        AUX(5) = AUX(4)-AUX(3) !Delta Tiempo
        
        !$OMP PARALLEL DO PRIVATE(I) SHARED (U0,V0,W0,LOX,LOY,LOZ,TIME,AUX,PI)
        DO I=1,NNT
            IF (LOY(I,1)) THEN
                IF((TIME.GT.AUX(3)).AND.(TIME.LT.AUX(4)))THEN
                    V(2,I)=V0(I)+AUX(1)*DSIN(2.0*PI*AUX(2)*TIME)*DSIN(PI*(TIME-AUX(3))/AUX(5))
                ELSE
                    V(2,I)=V0(I)
                END IF
            END IF
        END DO
        !$OMP END PARALLEL DO
        
    CASE DEFAULT
        WRITE(*,*) 'ERROR: INVALID TEST SELECT (CALOAD)'
        PAUSE
        STOP
    END SELECT
    
    !--------------------------------------------------------------------------
    ! APLICA UNA CARGA IMPULSIVA EN TODO EL DOMINIO
    !--------------------------------------------------------------------------
    IF (.FALSE.) THEN
        IF((TIME.GT.0.0).AND.(TIME.LT.10*DT))THEN
            !$OMP PARALLEL DO PRIVATE(I) SHARED (FRX,FRY,FRZ,MAS)
            DO I=1,NNT
                FRX(I)=200.0*MAS(I)
                FRY(I)=500.0*MAS(I)
                FRZ(I)=700.0*MAS(I)
            ENDDO
            !$OMP END PARALLEL DO
        END IF
    END IF

    DEALLOCATE(AUX)
    
    RETURN
    END
    !
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------