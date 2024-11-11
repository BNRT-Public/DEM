    !-------------------------------------------------------------------------
    !-------------------------------------------------------------------------
    !
    SUBROUTINE WRIENE(IFILE)
    !
    ! DATA:  06/07/2019
    ! AUTOR: Boris N. Rojo Tanzi
    !
    ! ESTA SUBRUTINA ESCRIBE LOS VALORES DE ENERGIA DEL MODELO,
    ! - ENEX = TRABAJO EXTERNO REALIZADO EN EL MODELO
    ! - ENIN = ENERGIA INETERNA TOTAL DISIPADA
    ! - ENCN = ENERGIA CINETICA TOTAL
    ! - ENEL = ENERGIA ELASTICA TOTAL EN EL MODELO
    ! - ENDP = ENERGIA DISIPADA POR AMORTIGUAMIENTO
    ! - ENGD = ENERGIA DISIPADA POR EFECTO DE ABLANDAMIENTO (STRAIN SOFTENING)
    ! - DEN  = DIFERENCIA ENTRE ENERGIA INTERNA Y EXTERNA
    !
    !--------------------------------------------------------------------------
    ! VARIABLES ENTRADA SALIDA
    !--------------------------------------------------------------------------
    INTEGER IFILE
    DOUBLE PRECISION ENCND, ENELD, ENGDD, ENDPD
    !--------------------------------------------------------------------------
    IF (OU08) 10,10,11
11  IF (PR08.EQ.OU08) THEN
        
        ENCND=(ENCN-ENCNOLD)/DT
        ENELD=(ENEL-ENELOLD)/DT
        ENGDD=(ENGD-ENGDOLD)/DT
        ENDPD=(ENDP-ENDPOLD)/DT
        
        IF (ENCN.LE.1D-99) ENCN=0.0d0
        IF (ENEL.LE.1D-99) ENEL=0.0d0
        IF (ENGD.LE.1D-99) ENGD=0.0d0
        IF (ENCND.LE.1D-99) ENCND=0.0d0
        IF (ENELD.LE.1D-99) ENELD=0.0d0
        IF (ENGDD.LE.1D-99) ENGDD=0.0d0
        IF (ENDPD.LE.1D-99) ENDPD=0.0d0

        WRITE (IFILE,8) TIME,ENEX,ENCN,ENEL,ENGD,ENDP,ENIN,ENCND,ENELD,ENGDD,ENDPD
8       FORMAT((E21.14),',',10(2X,E21.14,','))
        PR08=1
    ELSE
        PR08=PR08+1
    END IF

10  CONTINUE

    RETURN
    END
    !
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------