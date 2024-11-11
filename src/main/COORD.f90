    !-------------------------------------------------------------------------!
    !-------------------------------------------------------------------------!
    !
    SUBROUTINE COORD
    IMPLICIT NONE
    !
    ! DATA:  16/02/2023
    ! AUTOR: Boris N. Rojo Tanzi
    !
    !--------------------------------------------------------------------------
    ! VARIABLES LOCALES
    !--------------------------------------------------------------------------
    INTEGER I,ii
    !--------------------------------------------------------------------------
    SELECT CASE (MODELTYPE)
    CASE ('DEM') ! MODEL DEM
        CALL COORDSUB (2,M1,1, N,1, L,  0, M, N,1,    2,1,2,1,2) ! NODOS QUE NO SE ENCUENTRAN EN X=0 NI X=MAX
        CALL COORDSUB (1, 1,1, N,1, L,  0, M, N,0,    0,1,2,1,2) ! NODOS QUE SE ENCUENTRAN EN X=0
        CALL COORDSUB (M, M,1, N,1, L,  0, M, N,0,-2*M1,1,2,1,2) ! NODOS QUE SE ENCUENTRAN EN X=MAX
        CALL COORDSUB (1,M1,1,N1,1,L1,MNL,M1,N1,1,    1,1,1,1,1) ! NODOS CENTRALES

    CASE ('PRO')
        ALLOCATE(MAS(NNT))
        
        ! LECTURA DEL ARCHIVO DE Nodos 'Node_List.DAT'
        OPEN (UNIT=1,ACCESS='SEQUENTIAL',FILE='Node_List.DAT',&
            &FORM='FORMATTED',STATUS='OLD')
        
        do i=1, 6
           read (1,*) ! comentarios
        end do
        
        do i=1, nnt
           read (1,*) ii,u0(i),v0(i),w0(i),mas(i)
        end do
        CLOSE (1)
                    
    END SELECT
    
    RETURN
    END
    !
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------