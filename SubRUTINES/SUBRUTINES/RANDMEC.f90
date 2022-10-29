    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------
    !
    SUBROUTINE RANDMEC (B,TYPES)
    !
    ! DATA:  05/07/2019
    ! AUTOR: Ignacio Iturrioz
    !        Boris N. Rojo Tanzi
    !
    ! DEFINE LAS PROPIEDADES MECANICAS DE LAS BARRAS
    !
    !--------------------------------------------------------------------------
    ! VARIABLES LOCALES
    !--------------------------------------------------------------------------
    INTEGER B,TYPES
    DOUBLE PRECISION RED,NOR,AUX
    
    !--------------------------------------------------------------------------
    IF (TYPES.EQ.0) RED=1.00D00
    IF (TYPES.EQ.1) RED=0.50D00
    IF (TYPES.EQ.2) RED=0.25D00
    
    IF (TYPES.LT.3) THEN
        ECOMP(B)=ENR(B)*RED
    ELSE
        ECOMP(B)=EDG(B)
    END IF
    
    ETRAC(:,B)=ECOMP(B)
    KR(:,B)=LCR(B)/LI(B)
    
    IF (KR(1,B).LT.1.01) THEN
        !$OMP CRITICAL
        WRITE (*,*)
        WRITE (*,*) '  KR < 1.01  '
        WRITE (*,*)
        STOP
        !$OMP END CRITICAL
    END IF
  
    EM(:,B)=EP(1,B)
    
    RETURN
    END
    !
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------