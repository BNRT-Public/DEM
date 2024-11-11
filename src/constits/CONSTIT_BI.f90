    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------
    !
    SUBROUTINE CONSTIT_BI
    !
    ! DATA:  06/07/2019
    ! AUTOR: Ignacio Iturrioz
    !        Boris N. Rojo Tanzi
    !
    !  ESTA SUBROTINA � CHAMADA PELA SUBROTINA CABAR PARA MODELAR A RELA��O
    !  CONSTITUTIVA DE CADA BARRA DE ACORDO COM UM DIAGRAMA BILINEAR FOR�A VS.
    !  DEFORMA��O, COM AMOLECIMENTO ("STRAIN SOFTENING'). O PROCESSO DE
    !  DESCARGA � MODELADO DE ACORDO COM A HIP�TESE DE DANO-EL�STICO.
    !
    !--------------------------------------------------------------------------
    ! VARIABLES LOCALES
    !--------------------------------------------------------------------------
    DOUBLE PRECISION :: AUX ! VERIFICACION DE ESTADOS
    INTEGER I

    !--------------------------------------------------------------------------
    ! CONCIDERANDO EL AMORTIGUAMIENTO EN LA RELACION CONSTITUTIVA: CON > 0.0D00
    !--------------------------------------------------------------------------
    !$OMP PARALLEL DO PRIVATE(I) SHARED(STR,DSTR,EM,EP,KR,ECOMP,ETRAC,CON,FS,DM,FORCE)
    DO I=1,NBT

        IF (STR(I).GE.ER(I))  GO TO 30 ! ROTA
        IF (STR(I).GE.EM(1,I))  GO TO 20 ! CON DANO
        IF (STR(I).GE.0.0D00) GO TO 10 ! EN TRACCION

        !--------------------------------------------------------------------------
        ! EN COMPRESION
        FORCE(1,I)=ECOMP(I)*STR(I)+ECOMP(I)*CON*DSTR(I)
        elemtype(i) = 110
        GO TO 40

        !--------------------------------------------------------------------------
        ! EN TRACCION
10      FORCE(1,I)=ETRAC(1,I)*STR(I)+ETRAC(1,I)*CON*DSTR(I)
        elemtype(i) = 100
        GO TO 40

        !--------------------------------------------------------------------------
        ! CON DANO
20      AUX=(STR(I)-EP(1,I))/(KR(1,I)-1.0D00)
        AUX=ECOMP(I)*(EP(1,I)-AUX)
        ETRAC(:,I)=AUX/STR(I)
        EM(1,I)=STR(I)
        FORCE(1,I)=AUX+ETRAC(1,I)*CON*DSTR(I)
        DM(1,I)=.TRUE.
        elemtype(i) = 200
        GO TO 40

        !--------------------------------------------------------------------------
        ! ROTA        
30      ETRAC(:,I)=0.0d0
        FORCE(1,I)=0.0d0
        EM(1,I)=ER(I)
        FS(I)=.TRUE.
        DM(1,I)=.TRUE.
        elemtype(i) = 300

40      CONTINUE
    END DO
    !$OMP END PARALLEL DO

    RETURN
    END
    !
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------