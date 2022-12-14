    !-------------------------------------------------------------------------!
    !-------------------------------------------------------------------------!
    !
    SUBROUTINE COORD
    !
    ! DATA:  02/07/2019
    ! AUTOR: Boris N. Rojo Tanzi
    !
    !--------------------------------------------------------------------------
    ! VARIABLES LOCALES
    !--------------------------------------------------------------------------
    INTEGER I
    !--------------------------------------------------------------------------

    CALL COORDSUB (2,M1,1, N,1, L,  0, M, N,1,    2,1,2,1,2) ! NODOS QUE NO SE ENCUENTRAN EN X=0 NI X=MAX
    CALL COORDSUB (1, 1,1, N,1, L,  0, M, N,0,    0,1,2,1,2) ! NODOS QUE SE ENCUENTRAN EN X=0
    CALL COORDSUB (M, M,1, N,1, L,  0, M, N,0,-2*M1,1,2,1,2) ! NODOS QUE SE ENCUENTRAN EN X=MAX
    CALL COORDSUB (1,M1,1,N1,1,L1,MNL,M1,N1,1,    1,1,1,1,1) ! NODOS CENTRALES
        
    RETURN
    END
    !
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------