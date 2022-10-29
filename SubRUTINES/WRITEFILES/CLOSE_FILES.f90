    !-------------------------------------------------------------------------
    !-------------------------------------------------------------------------
    !
    SUBROUTINE CLOSE_FILES
    !
    ! DATA:  06/07/2019
    ! AUTOR: Boris N. Rojo Tanzi
    !
    ! EN ESTA SUBRUTINA CIERRA LOS ARCHIVOS
    !
    !--------------------------------------------------------------------------
    ! ARCHIVO CON VALORES DE ACELERACION DE CADA NODO
    ! FILE: ACELERATION_Txx.dat
    IF (OU05.GT.0) CLOSE(5)

    ! ARCHIVO CON VALORES DE VELOCIDAD DE CADA NODO
    ! FILE: VELOCITY_Txx.dat
    IF (OU06.GT.0) CLOSE(6)

    ! ARCHIVO CON VALORES DE DESPLAZAMIENTO DE CADA NODO
    ! FILE: DISPLACEMENT_Txx.dat
    IF (OU07.GT.0) CLOSE(7)

    ! ARCHIVO CON VALORES DE ENERGIA PARA CADA TIEMPO
    ! FILE: ENERGY_Txx.dat
    IF (OU08.GT.0) CLOSE(8)

    ! ARCHIVO CON VALORES DE LAS BARRAS ROTAS PARA CADA TIEMPO
    ! FILE: BARBROKEN_Txx.dat
    IF (OU09.GT.0) CLOSE(9)

    ! ARCHIVO CON VALORES DE LAS BARRAS ROTAS PARA CADA TIEMPO
    ! FILE: BARDAMAGEN_Txx.dat
    IF (OU10.GT.0) CLOSE(10)

    ! ARCHIVO CON VALORES DE TENSION Y DEFORMACION PARA CADA TIEMPO
    ! FILE: STRESS_Txx.dat
    IF (OU11.GT.0) CLOSE(11)
    
    ! ARCHIVO CON VALORES DE LEY CONSTITUTIVA
    ! FILE: COSTIT_Txx.dat
    IF (IBCO.GT.0) CLOSE(14)

    RETURN
    END
    !
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------
