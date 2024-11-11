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

    DOUBLE PRECISION xmean(1)

    LOGICAL,ALLOCATABLE,DIMENSION(:) :: LOGIC
    integer i
    DOUBLE PRECISION nummod
    
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
        DIST(2,:)=(/-0.02,+1.0e99/) ! PUNTO EN Y (NEG,POS)
        DIST(3,:)=(/-1.0E99,+1.0e99/) ! PUNTO EN Z (NEG,POS)
        
        POSEG = (/(M1*LCO)*0.5,(N1*LCO),(L1*LCO)*0.5/)
        ROTEG = (/0.0, 0.0, 45.0/)
                
        CALL PRISMEDITGEOM(POSEG,ROTEG,DIST,1.11d-16,1.11d-16,1.11d-16,.TRUE.,.TRUE.,8)

        END IF
    
    CASE (111)
        IF (.TRUE.) THEN
        DIST(1,:)=(/-LCO, LCO/) ! PUNTO EN X (NEG,POS)
        DIST(2,:)=(/-0.02,1.11e16/) ! PUNTO EN Y (NEG,POS)
        DIST(3,:)=(/-1.11e16,1.11e16/) ! PUNTO EN Z (NEG,POS)
        
        POSEG = (/(M1*LCO)*0.5,(N1*LCO),(L1*LCO)*0.5/)
        ROTEG = (/0.0, 0.0, 0.0/)
               
        CALL PRISMEDITGEOM(POSEG,ROTEG,DIST,1.11d-16,1.11d-16,1.11d-16,.TRUE.,.TRUE.,8)

        END IF
        
    CASE (337) ! TEST = 337 - TRACCION CON VELOCIDAD LINEAL
        IF (.FALSE.) THEN
        DIST(1,:)=(/-LCO, 3.85D-03/) ! PUNTO EN X (NEG,POS)
        DIST(2,:)=(/0.5*N1*LCO-1.0D-03*0.5,0.5*N1*LCO+1.0D-03*0.5/) ! PUNTO EN Y (NEG,POS)
        DIST(3,:)=(/-LCO,LCO*L/) ! PUNTO EN Z (NEG,POS)
        
        POSEG = (/(M1*LCO)*0.5,(N1*LCO),(L1*LCO)*0.5/)
        ROTEG = (/0.0, 0.0, 0.0/)
                
        CALL PRISMEDITGEOM(POSEG,ROTEG,DIST,1.11d-16,1.11d-16,1.11d-16,.TRUE.,.TRUE.,8)
        END IF
            
    ! case (813) ! Concreto poroso, con refuerzo a 1/3 inferior
    ! case (823) ! Concreto poroso, con refuerzo a 2/3 inferior
    ! case (814) ! Concreto poroso, con refuerzo a 1/4 inferior
    ! case (824) ! Concreto poroso, con refuerzo a 2/4 inferior
    case (800,812,813,814,823,824)
        allocate(logic(1))
        nummod = 2.1
        
        !$OMP PARALLEL DO PRIVATE(I, logic) !$OMP SHARED(POSB0,lco,elemtype, test,nummod)
        do i = 1,nbt
            if (test.eq.800) then
                logic = .true.
            else if (test.eq.813) then
                logic(1) = (POSB0(2,I).le.(50e-3 - nummod*lco)).and.(POSB0(2,I).ge.(50e-3 + nummod*lco))
            else if (test.eq.814) then
                logic(1) = (POSB0(2,I).le.(37.50e-3 - nummod*lco)).and.(POSB0(2,I).ge.(37.50e-3 + nummod*lco))
            else if (test.eq.812) then
                logic(1) = (POSB0(2,I).le.(75e-3 - nummod*lco)).and.(POSB0(2,I).ge.(75e-3 + nummod*lco))
            else if (test.eq.823) then
                logic(1) = (POSB0(2,I).le.(50e-3 - nummod*lco)).or.(POSB0(2,I).ge.(100e-3 + nummod*lco)).or.((POSB0(2,I).ge.(50e-3 + nummod*lco)).and.(POSB0(2,I).le.(100e-3 - nummod*lco)))
            else if (test.eq.824) then
                logic(1) = (POSB0(2,I).le.(37.50e-3 - nummod*lco)).or.(POSB0(2,I).ge.(112.50e-3 + nummod*lco)).or.((POSB0(2,I).ge.(37.50e-3 + nummod*lco)).or.(POSB0(2,I).le.(112.50e-3 - nummod*lco)))
            end if
            
            if (all(logic).and.(elemtype(i) .eq. 3)) then
                ECOMP(I)=ECOMP(I)*REYNG
                ETRAC(:,I)=ETRAC(:,I)*REYNG
                elemtype(i) = 7
            end if
                        
        end do
        !$OMP END PARALLEL DO
            
    case (900) !test = 900 - tres pontos, concreto poroso
    case (913, 923) !test = 900 - tres pontos, concreto poroso
        xmean = sum(U0, dim=1, mask=LOY(:,1))
        xmean = xmean/count(LOY(:,1))

        DIST(1,:)=(/-0.65*LCO, 0.65*LCO/) ! PUNTO EN X (NEG,POS)
        DIST(2,:)=(/-(1/3.0)*N1*LCO, (1/3.0)*N1*LCO/) ! PUNTO EN Y (NEG,POS)
        DIST(3,:)=(/-LCO*L*2.0, LCO*L*2.0/) ! PUNTO EN Z (NEG,POS)
        
        POSEG = (/xmean,0.0d0,(L1*LCO)*0.5/)
        ROTEG = (/0.0, 0.0, 0.0/)
                
        CALL PRISMEDITGEOM(POSEG,ROTEG,DIST,1.0d-99,1.0d-99,1.0d-99,.TRUE.,.TRUE.,8)
    CASE (930) ! TEST = 900 comprecion, concreto poroso
    CASE (940) ! TEST = traccion indirecta, concreto poroso
    case (2001) !teste = 2001 - proteina
        
    CASE DEFAULT
        write(*,*) 
        write(*,*) 'WARNING: invalid test select (EDITGEOM),'
        write(*,*) '         will continue in a few seconds.'
        call sleep(10)
    END SELECT

    RETURN
    END
    !
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------
