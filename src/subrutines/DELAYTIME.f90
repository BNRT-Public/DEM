    !-------------------------------------------------------------------------
    !-------------------------------------------------------------------------
    !
    SUBROUTINE DELAYTIME(SDATE, STIME, DD, HH, MM, SS)
    !
    ! DATA:  02/07/2019
    ! AUTOR: Boris N. Rojo Tanzi
    !
    ! ESTA FUNCION CALCULA EL TIEMPO DE COMPUTO
    !
    !--------------------------------------------------------------------------
    ! VARIABLES LOCALES
    !--------------------------------------------------------------------------
    IMPLICIT NONE
    CHARACTER(LEN = 08) :: FDATE ! YYYYMMDD FINAL
    CHARACTER(LEN = 10) :: FTIME ! HHMMSS.SSS FINAL

    INTEGER :: SYEAR, SMONTH, SDAY, SHOUR, SMINUTE
    DOUBLE PRECISION :: SSECOND
    INTEGER :: FYEAR, FMONTH, FDAY, FHOUR, FMINUTE
    DOUBLE PRECISION :: FSECOND

    !--------------------------------------------------------------------------
    ! VARIABLES DE ENTRADA SALIDA
    !--------------------------------------------------------------------------
    CHARACTER(LEN = 08) :: SDATE ! YYYYMMDD INICIO
    CHARACTER(LEN = 10) :: STIME ! HHMMSS.SSS INICIO
    INTEGER DD, HH, MM
    REAL SS

    !--------------------------------------------------------------------------
    ! CALCULO DE TIEMPOS
    !-------------------------------------------------------------------------
    CALL DATE_AND_TIME(FDATE, FTIME)

    READ(FDATE(1:4),*) FYEAR
    READ(FDATE(5:6),*) FMONTH
    READ(FDATE(7:8),*) FDAY
    READ(FTIME(1:2),*) FHOUR
    READ(FTIME(3:4),*) FMINUTE
    READ(FTIME(5:10),*) FSECOND

    READ(SDATE(1:4),*) SYEAR
    READ(SDATE(5:6),*) SMONTH
    READ(SDATE(7:8),*) SDAY
    READ(STIME(1:2),*) SHOUR
    READ(STIME(3:4),*) SMINUTE
    READ(STIME(5:10),*) SSECOND

    DD=(FYEAR-SYEAR)*365+(FMONTH-SMONTH)*30+(FDAY-SDAY)
    HH=FHOUR-SHOUR
    MM=FMINUTE-SMINUTE
    SS=FSECOND-SSECOND

    SS=SS+MM*60.0
    SS=SS+HH*3600.0
    SS=SS+DD*86400.0

    DD=INT(REAL(SS)/86400)
    HH=INT((REAL(SS)-DD*86400)/3600)
    MM=INT((REAL(SS)-DD*86400-HH*3600)/60)
    SS=REAL(SS)-DD*86400.0-HH*3600.0-MM*60.0

    RETURN
    END
    !
    !-----------------------------------------------------------------------
    !------------------------------------------------------------------------