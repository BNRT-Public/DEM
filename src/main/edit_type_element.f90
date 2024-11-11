    !-------------------------------------------------------------------------
    !-------------------------------------------------------------------------
    !
    SUBROUTINE edit_type_element
    IMPLICIT NONE
    !
    ! DATA:  24/08/2024
    ! AUTOR: Boris N. Rojo Tanzi
    !
    ! Cambia el tipo de elemnto para identificacion o cambio de propiedades
    !
    !--------------------------------------------------------------------------
    ! VARIABLES LOCALES
    !--------------------------------------------------------------------------
    INTEGER I
    
    LOGICAL,ALLOCATABLE,DIMENSION(:) :: logicx, logicy

    
    SELECT CASE (MODELTYPE)
    CASE ('DEM') ! MODEL DEM
        !--------------------------------------------------------------------------
        ! Cambio de tipo de barras
        !--------------------------------------------------------------------------
        SELECT CASE (TEST)
            
        case (812,813,814,823,824)
            allocate(logicx(2))
            allocate(logicy(2))
            
            !$OMP PARALLEL DO PRIVATE(I, logicx, logicy) !$OMP SHARED(POSB0,lco,elemtype, test)
            do i = 1,nbt
                ! Refuerzos (alturas)
                if (test.eq.813) then
                    logicx(1) = (POSB0(2,I).ge.(50e-3 - 0.2*lco)).and.(POSB0(2,I).le.(50e-3 + 0.2*lco))
                    !
                    logicy(1) = (POSB0(2,I).ge.(50e-3 - 0.2*lco)).and.(POSB0(2,I).le.(50e-3 + 0.2*lco))
                else if (test.eq.814) then
                    logicx(1) = (POSB0(2,I).ge.(37.50e-3 - 0.2*lco)).and.(POSB0(2,I).le.(37.50e-3 + 0.2*lco))
                    !
                    logicy(1) = (POSB0(2,I).ge.(37.50e-3 - 0.2*lco)).and.(POSB0(2,I).le.(37.50e-3 + 0.2*lco))
                else if (test.eq.812) then
                    logicx(1) = (POSB0(2,I).ge.(75e-3 - 0.2*lco)).and.(POSB0(2,I).le.(75e-3 + 0.2*lco))
                    !
                    logicy(1) = (POSB0(2,I).ge.(75e-3 - 0.2*lco)).and.(POSB0(2,I).le.(75e-3 + 0.2*lco))
                else if (test.eq.823) then
                    logicx(1) = (POSB0(2,I).ge.(50e-3 - 0.2*lco)).and.(POSB0(2,I).le.(50e-3 + 0.2*lco))
                    logicx(1) = logicx(1).or.(POSB0(2,I).ge.(100e-3 - 0.2*lco)).and.(POSB0(2,I).le.(100e-3 + 0.2*lco))
                    !
                    logicy(1) = (POSB0(2,I).ge.(50e-3 - 0.2*lco)).and.(POSB0(2,I).le.(50e-3 + 0.2*lco))
                    logicy(1) = logicy(1).or.(POSB0(2,I).ge.(100e-3 - 0.2*lco)).and.(POSB0(2,I).le.(100e-3 + 0.2*lco))
                else if (test.eq.824) then
                    logicx(1) = (POSB0(2,I).ge.(37.50e-3 - 0.2*lco)).and.(POSB0(2,I).le.(37.50e-3 + 0.2*lco))
                    logicx(1) = logicx(1).or.((POSB0(2,I).ge.(112.50e-3 - 0.2*lco)).and.(POSB0(2,I).le.(112.50e-3 + 0.2*lco)))
                    !
                    logicy(1) = (POSB0(2,I).ge.(37.50e-3 - 0.2*lco)).and.(POSB0(2,I).le.(37.50e-3 + 0.2*lco))
                    logicy(1) = logicy(1).or.((POSB0(2,I).ge.(112.50e-3 - 0.2*lco)).and.(POSB0(2,I).le.(112.50e-3 + 0.2*lco)))
                end if
                
                if ((test.eq.813).or.(test.eq.823).or.(test.eq.812)) then
                    logicx(2) = (POSB0(3,I).ge.(15e-3 - 0.2*lco)).and.(POSB0(3,I).le.(15e-3 + 0.2*lco))
                    logicx(2) = logicx(2).or.((POSB0(3,I).ge.(55e-3 - 0.2*lco)).and.(POSB0(3,I).le.(55e-3 + 0.2*lco)))
                    logicx(2) = logicx(2).or.((POSB0(3,I).ge.(95e-3 - 0.2*lco)).and.(POSB0(3,I).le.(95e-3 + 0.2*lco)))
                    logicx(2) = logicx(2).or.((POSB0(3,I).ge.(135e-3 - 0.2*lco)).and.(POSB0(3,I).le.(135e-3 + 0.2*lco)))
                    !
                    logicy(2) = (POSB0(1,I).ge.(10e-3 - 0.2*lco)).and.(POSB0(1,I).le.(10e-3 + 0.2*lco))
                    logicy(2) = logicy(2).or.(POSB0(1,I).ge.(50e-3 - 0.2*lco)).and.(POSB0(1,I).le.(50e-3 + 0.2*lco))
                    logicy(2) = logicy(2).or.(POSB0(1,I).ge.(90e-3 - 0.2*lco)).and.(POSB0(1,I).le.(90e-3 + 0.2*lco))
                    logicy(2) = logicy(2).or.(POSB0(1,I).ge.(130e-3 - 0.2*lco)).and.(POSB0(1,I).le.(130e-3 + 0.2*lco))
                    logicy(2) = logicy(2).or.(POSB0(1,I).ge.(170e-3 - 0.2*lco)).and.(POSB0(1,I).le.(170e-3 + 0.2*lco))
                    logicy(2) = logicy(2).or.(POSB0(1,I).ge.(210e-3 - 0.2*lco)).and.(POSB0(1,I).le.(210e-3 + 0.2*lco))
                    logicy(2) = logicy(2).or.(POSB0(1,I).ge.(250e-3 - 0.2*lco)).and.(POSB0(1,I).le.(250e-3 + 0.2*lco))
                    logicy(2) = logicy(2).or.(POSB0(1,I).ge.(290e-3 - 0.2*lco)).and.(POSB0(1,I).le.(290e-3 + 0.2*lco))
                    logicy(2) = logicy(2).or.(POSB0(1,I).ge.(330e-3 - 0.2*lco)).and.(POSB0(1,I).le.(330e-3 + 0.2*lco))
                    logicy(2) = logicy(2).or.(POSB0(1,I).ge.(370e-3 - 0.2*lco)).and.(POSB0(1,I).le.(370e-3 + 0.2*lco))
                    logicy(2) = logicy(2).or.(POSB0(1,I).ge.(410e-3 - 0.2*lco)).and.(POSB0(1,I).le.(410e-3 + 0.2*lco))
                    logicy(2) = logicy(2).or.(POSB0(1,I).ge.(450e-3 - 0.2*lco)).and.(POSB0(1,I).le.(450e-3 + 0.2*lco))
                    logicy(2) = logicy(2).or.(POSB0(1,I).ge.(490e-3 - 0.2*lco)).and.(POSB0(1,I).le.(490e-3 + 0.2*lco))
                    logicy(2) = logicy(2).or.(POSB0(1,I).ge.(530e-3 - 0.2*lco)).and.(POSB0(1,I).le.(530e-3 + 0.2*lco))
                else if ((test.eq.814).or.(test.eq.824)) then
                    logicx(2) = (POSB0(3,I).ge.(12.5e-3 - 0.2*lco)).and.(POSB0(3,I).le.(12.5e-3 + 0.2*lco))
                    logicx(2) = logicx(2).or.((POSB0(3,I).ge.(52.5e-3 - 0.2*lco)).and.(POSB0(3,I).le.(52.5e-3 + 0.2*lco)))
                    logicx(2) = logicx(2).or.((POSB0(3,I).ge.(92.5e-3 - 0.2*lco)).and.(POSB0(3,I).le.(92.5e-3 + 0.2*lco)))
                    logicx(2) = logicx(2).or.((POSB0(3,I).ge.(132.5e-3 - 0.2*lco)).and.(POSB0(3,I).le.(132.5e-3 + 0.2*lco)))
                    !
                    logicy(2) = (POSB0(1,I).ge.(7.5e-3 - 0.2*lco)).and.(POSB0(1,I).le.(7.5e-3 + 0.2*lco))
                    logicy(2) = logicy(2).or.(POSB0(1,I).ge.(47.5e-3 - 0.2*lco)).and.(POSB0(1,I).le.(47.5e-3 + 0.2*lco))
                    logicy(2) = logicy(2).or.(POSB0(1,I).ge.(87.5e-3 - 0.2*lco)).and.(POSB0(1,I).le.(87.5e-3 + 0.2*lco))
                    logicy(2) = logicy(2).or.(POSB0(1,I).ge.(127.5e-3 - 0.2*lco)).and.(POSB0(1,I).le.(127.5e-3 + 0.2*lco))
                    logicy(2) = logicy(2).or.(POSB0(1,I).ge.(167.5e-3 - 0.2*lco)).and.(POSB0(1,I).le.(167.5e-3 + 0.2*lco))
                    logicy(2) = logicy(2).or.(POSB0(1,I).ge.(207.5e-3 - 0.2*lco)).and.(POSB0(1,I).le.(207.5e-3 + 0.2*lco))
                    logicy(2) = logicy(2).or.(POSB0(1,I).ge.(247.5e-3 - 0.2*lco)).and.(POSB0(1,I).le.(247.5e-3 + 0.2*lco))
                    logicy(2) = logicy(2).or.(POSB0(1,I).ge.(287.5e-3 - 0.2*lco)).and.(POSB0(1,I).le.(287.5e-3 + 0.2*lco))
                    logicy(2) = logicy(2).or.(POSB0(1,I).ge.(327.5e-3 - 0.2*lco)).and.(POSB0(1,I).le.(327.5e-3 + 0.2*lco))
                    logicy(2) = logicy(2).or.(POSB0(1,I).ge.(367.5e-3 - 0.2*lco)).and.(POSB0(1,I).le.(367.5e-3 + 0.2*lco))
                    logicy(2) = logicy(2).or.(POSB0(1,I).ge.(407.5e-3 - 0.2*lco)).and.(POSB0(1,I).le.(407.5e-3 + 0.2*lco))
                    logicy(2) = logicy(2).or.(POSB0(1,I).ge.(447.5e-3 - 0.2*lco)).and.(POSB0(1,I).le.(447.5e-3 + 0.2*lco))
                    logicy(2) = logicy(2).or.(POSB0(1,I).ge.(487.5e-3 - 0.2*lco)).and.(POSB0(1,I).le.(487.5e-3 + 0.2*lco))
                    logicy(2) = logicy(2).or.(POSB0(1,I).ge.(527.5e-3 - 0.2*lco)).and.(POSB0(1,I).le.(527.5e-3 + 0.2*lco))
                end if
                
                if (all(logicx)) then
                    elemtype(i) = 7
                end if
                
                if (all(logicy)) then
                    elemtype(i) = 6
                end if
                
            end do
            !$OMP END PARALLEL DO
                
        end select
    end select
    
    RETURN
    END
    !
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------