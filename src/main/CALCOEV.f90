    !-------------------------------------------------------------------------
    !-------------------------------------------------------------------------
    !
    SUBROUTINE CALCOEV
    !
    ! DATA:  02/07/2019
    ! AUTOR: Boris N. Rojo Tanzi
    !
    !--------------------------------------------------------------------------
    SELECT CASE (LAW)
    CASE (2,4) ! LEY BILINEAL / BILINEAL-SHEAR
        EPMAX(1)=MAXVAL(EP(1,:))
        EPMIN(1)=MINVAL(EP(1,:))

        MEAN(1)=SUM(EP(1,:))/NBT
        COEV(1)=SUM((MEAN(1)-EP(1,:))**2.0)
        COEV(1)=DSQRT(COEV(1)/(NBT-1))/MEAN(1)
    CASE (3) ! LEY TRILINEAL
        EPMAX(1)=MAXVAL(EP(1,:))
        EPMAX(2)=MAXVAL(EP(2,:))
        EPMIN(1)=MINVAL(EP(1,:))
        EPMIN(2)=MINVAL(EP(2,:))

        MEAN(1)=SUM(EP(1,:))/NBT
        COEV(1)=SUM((MEAN(1)-EP(1,:))**2.0)
        COEV(1)=DSQRT(COEV(1)/(NBT-1))/MEAN(1)

        MEAN(2)=SUM(EP(2,:))/NBT
        COEV(2)=SUM((MEAN(2)-EP(1,:))**2.0)
        COEV(2)=DSQRT(COEV(2)/(NBT-1))/MEAN(2)
    END SELECT


    RETURN
    END
    !
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------