    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------
    !
    FUNCTION ROTMAT(ROT)
    !
    ! DATA:  17/10/2022
    ! AUTOR: Boris N. Rojo Tanzi
    !
    ! ESTA FUNCION PARA CALCULO DE MATRIZ DE ROTACION DE SISTEMAS COORDENADAS
    !
    !--------------------------------------------------------------------------
    ! VARIABLES LOCALES
    !--------------------------------------------------------------------------
    IMPLICIT NONE
    DOUBLE PRECISION PI,G2R ! CONSTANTES
    DOUBLE PRECISION CX,SX,CY,SY,CZ,SZ ! FUNCIONES TRIGONOMETRICAS

    !--------------------------------------------------------------------------
    ! VARIABLES DE ENTRADA SALIDA
    !--------------------------------------------------------------------------
    DOUBLE PRECISION ROTMAT(3,3) ! MATRIZ DE ROTACION
    DOUBLE PRECISION ROT(3) ! ANG DE ROTACION

    !--------------------------------------------------------------------------
    ! CALCULO DE LA METRIZ DE ROTACION.
    !--------------------------------------------------------------------------
    PI=4.0*DATAN(1.0D0)
    G2R=PI/180.0

    SX=-DSIN(ROT(1)*G2R)
    CX=DCOS(ROT(1)*G2R)
    SY=DSIN(ROT(2)*G2R)
    CY=DCOS(ROT(2)*G2R)
    SZ=-DSIN(ROT(3)*G2R)
    CZ=DCOS(ROT(3)*G2R)
       
    ROTMAT(1,1)=CY*CZ
    ROTMAT(2,1)=-CX*SZ-CZ*SX*SY
    ROTMAT(3,1)=SX*SZ-CX*CZ*SY
    ROTMAT(1,2)=CY*SZ
    ROTMAT(2,2)=CX*CZ-SX*SY*SZ
    ROTMAT(3,2)=-CZ*SX-CX*SY*SZ
    ROTMAT(1,3)=SY
    ROTMAT(2,3)=CY*SX
    ROTMAT(3,3)=CX*CY

    RETURN
    END
    !
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------