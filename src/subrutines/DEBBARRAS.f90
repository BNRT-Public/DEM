    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------
    !
    SUBROUTINE DEBBARRAS
    !
    ! DATA:  28//09/2019
    ! AUTOR: Boris N. Rojo Tanzi
    !
    ! ESTA SUBRUTINA DEBILITAS LAS BARRAS
    !
    !--------------------------------------------------------------------------
    ! VARIABLES LOCALES
    !--------------------------------------------------------------------------
    INTEGER IBIN, IBFIN, I
    
    !--------------------------------------------------------------------------
    ! DEBILITACION DE BARRAS
    !--------------------------------------------------------------------------
    
    ! DEBILITACION DE LAS BARRAS VERTICALES EXTERNAS (DIRECCION DE Z)
    ! BARRAS EN DIR X [1...., M1*N*L]
    ! BARRAS EN DIR Y [M1*N*L+1, M1*N*L+ M*N1*L]
    ! BARRAS EN DIR Z [M1*N*L+ M*N1*L+1, M1*N*L+ M*N1*L+ M*N*L1]
    IBIN=M1*N*L+M*N1*L+1
    IBFIN=M1*N*L+M*N1*L+M*N*L1
    WRITE(*,*)"IBIN=",IBIN, "IBFIN=",IBFIN
    
    DO I=IBIN,IBFIN
        GFRB(I)=1.0*GFRB(I)
    ENDDO
    
    ! DEBILITACION DE LAS BARRAS VERTICALES INTERNAS (DIRECCION DE Z)
    ! BARRAS EN DIR X [1...., (M1-1)*N1*L1]
    ! BARRAS EN DIR Y [(M1-1)*N1*L1+1, (M1-1)*N1*L1+ M1*(N1-1)*L1]
    ! BARRAS EN DIR Z [(M1-1)*N1*L1+ M1*(N1-1)*L1+1,(M1-1)*N1*L1+ M1*(N1-1)*L1+ M1*N1*(L1-1)]
    IBIN=IBFIN+(M1-1)*N1*L1+ M1*(N1-1)*L1+1
    IBFIN=IBFIN+(M1-1)*N1*L1+ M1*(N1-1)*L1+ M1*N1*(L1-1)
    WRITE(*,*)"IBIN=",IBIN, "IBFIN=",IBFIN
    
    DO I=IBIN,IBFIN
        GFRB(I)=1.*GFRB(I)
    ENDDO


    RETURN
    END
    !
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------