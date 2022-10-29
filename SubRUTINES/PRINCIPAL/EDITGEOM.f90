    !-------------------------------------------------------------------------
    !-------------------------------------------------------------------------
    !
    SUBROUTINE EDITGEOM
    !
    ! DATA:  28/09/2019
    ! AUTOR: Boris N. Rojo Tanzi
    !
    ! EN ESTA SUBRUTINA SE EDITA LA GEOMETRIA PUDIENDO CREAR FISURAS, AGUJEROS, ETC.
    !
    !--------------------------------------------------------------------------
    ! VARIABLES LOCALES
    !--------------------------------------------------------------------------
        
    DOUBLE PRECISION DIST(3,2) ! DISTANCIAS DEL PRISMA
    DOUBLE PRECISION POSEG(3) ! POSICION DEL SISTEMA DE COORDENADAS
    DOUBLE PRECISION ROTEG(3) ! ROTACION SISTEMA DE COORDENADAS
    
    !--------------------------------------------------------------------------
    ! CREACION DE UNA PRE TRINCA Y/O AGUJERO CUADRADO
    !
    ! SOLO ACTIVAR (.TRUE.) O DESACTIVAR (.FALSE.) LOS MODULOS QUE SE QUIEREN
    !--------------------------------------------------------------------------
    SELECT CASE (TEST)
    ! ELIMINACION DE ELEMENTOS A PARTIR DE UN PLANO CON ESPESURA
    CASE (110)
        IF (.TRUE.) THEN
        DIST(1,:)=(/-LCO, LCO/) ! PUNTO EN X (NEG,POS)
        DIST(2,:)=(/-0.02,+1.0E99/) ! PUNTO EN Y (NEG,POS)
        DIST(3,:)=(/-1.0E99,+1.0E99/) ! PUNTO EN Z (NEG,POS)
        
        POSEG = (/(M1*LCO)*0.5,(N1*LCO),(L1*LCO)*0.5/)
        ROTEG = (/0.0, 0.0, 45.0/)
                
        CALL PRISMEDITGEOM(POSEG,ROTEG,DIST,1.0D-99,1.0D-99,1.0D-99,.TRUE.,.TRUE.)

        END IF
        
    CASE (337) ! TEST = 337 - TRACCION CON VELOCIDAD LINEAL
        IF (.FALSE.) THEN
        DIST(1,:)=(/-LCO, 3.85D-03/) ! PUNTO EN X (NEG,POS)
        DIST(2,:)=(/0.5*N1*LCO-1.0D-03*0.5,0.5*N1*LCO+1.0D-03*0.5/) ! PUNTO EN Y (NEG,POS)
        DIST(3,:)=(/-LCO,LCO*L/) ! PUNTO EN Z (NEG,POS)
        
        POSEG = (/(M1*LCO)*0.5,(N1*LCO),(L1*LCO)*0.5/)
        ROTEG = (/0.0, 0.0, 0.0/)
                
        CALL PRISMEDITGEOM(POSEG,ROTEG,DIST,1.0D-99,1.0D-99,1.0D-99,.TRUE.,.TRUE.)
        END IF
    
        
    CASE DEFAULT
        WRITE(*,*) 'ERROR: INVALID TEST SELECT (EDITGEOM)'
        PAUSE
        !STOP
    END SELECT

    RETURN
    END
    !
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------