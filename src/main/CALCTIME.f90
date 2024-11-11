    !-------------------------------------------------------------------------!
    !-------------------------------------------------------------------------!
    !
    SUBROUTINE CALCTIME
    IMPLICIT NONE
    !
    ! DATA:  28/02/2023
    ! AUTOR: Boris N. Rojo Tanzi
    !
    !--------------------------------------------------------------------------
    ! VARIABLES LOCALES
    !--------------------------------------------------------------------------
    double precision pi,mpi
    
    double precision k(2)
    double precision ms
    
    double precision dt_crit(2)
    double precision dt_cmin
    
    integer i
        
    !--------------------------------------------------------------------------
    pi=4.0d0*datan(1.0d00)
    mpi = 2.0d0/pi
    
    dt_cmin = huge(0.0d0)
            
    do i=1,nbt      
        k(1)=maxval(etrac(:,i))
        k(2)=ecomp(i)
        k=k/li(i)
            
        ms = minval(mas([cn(:,i)]))
    
        dt_crit(1) = minval(0.5d0*mpi*(dsqrt(k/ms)))
        dt_crit(2) = dt_cmin
        dt_cmin = minval(dt_crit)
    end do
    
    if (dt_cmin.lt.dt) then
        write(*,*) 'ERROR: dt > dt_(crit)'
        write(*,*) 'dt_cmin = ',dt_cmin
        write(*,*) 'dt = ',dt
        pause
        stop
    end if   
    
    RETURN
    END
    !
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------