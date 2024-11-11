    !-------------------------------------------------------------------------!
    !-------------------------------------------------------------------------!
    !
    SUBROUTINE CONECTI
    IMPLICIT NONE
    !
    ! DATA:  01/03/2023
    ! AUTOR: Boris N. Rojo Tanzi
    !
    !--------------------------------------------------------------------------
    ! VARIABLES LOCALES
    !--------------------------------------------------------------------------
    integer nb, i
    double precision dtc

    integer cnt1,cnt2,elemtypet
    double precision etract,ecompt,lit,ept,krt
    
    !--------------------------------------------------------------------------
    nb=0
    elemtype=0
    
    select case (modeltype)
    case ('DEM') ! model dem
        ! Elementos normales
        !    extco1 (mx,nx,lx,c1,c2,c3, st,nb)
        call extco1 (m1, n, l, 0, 1, 1,  1,nb)
        call extco1 (m, n1, l, 1, 0, 1,  m,nb)
        call extco1 (m,  n,l1, 1, 1, 0,m*n,nb)
    
        ! Elementos internas
        !    intco1 (mx,nx,lx,   st,nb)
        call intco1 (m2,n1,l1,    1,nb)
        call intco1 (m1,n2,l1,   m1,nb)
        call intco1 (m1,n1,l2,m1*n1,nb)
    
        ! Elementos diagonales
        elemtype(nb:nbt)=3
        call diaco1(nb)
    
    case ('PRO') ! model proteine
         ! Lectura del archivo de conectividades 'connect_list.dat'
        open (unit=1,access='sequential',file='connect_list.dat',&
            &form='formatted',status='old')
        
        do i=1, 6
           read (1,*) ! comentarios
        end do
        do i=1, nbt
           read (1,*) cnt1,cnt2,etract,ecompt,elemtypet,lit,ept,krt
           
           cn(1,i) = cnt1
           cn(2,i) = cnt2
           etrac(:,i) = etract
           ecomp(i) = ecompt
           elemtype(i) = elemtypet
           li(i) = lit
           ep(:,i) = ept
           kr(:,i) = krt
        end do
        
        close (1)
        
        select case (law)
        case(2,4)
            do i=1,nnt
                er(i) = ep(1,i)*kr(1,i)
                em(1,i) = ep(1,i)
            end do
        case(3)
            do i=1,nnt
                er(i) = ep(1,i)*kr(1,i)
                em(1,i) = ep(1,i)
            end do
        end select 
    end select
    
    RETURN
    END
    !
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------