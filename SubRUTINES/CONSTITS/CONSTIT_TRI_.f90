    SUBROUTINE CONSTIT_TRI (B,STR,STRRT,R)
    !     ==================
    !
    !  ESTA É UMA VERSÃO DA LEI TRI-LINEAR PARA ELEMENTOS FRÁGEIS

    !   ---------------------------
    !
    !         F(EP)
    !       *
    !      *!   *     F(EP1)
    !     * !       *
    !    *  !       ! *
    !   *   !       !   *
    !  *    !       !     *
    ! *     !       !       *
    !-------!-------!-------!--------
    !                   EP    EP1      ER
    !
    !   C= F(EP1)/F(EP)
    !   F(EP1)=C*F(EP)
    !   ER= R*KR*EP
    !   EP1=(KR-C*RKR+C)EP

    ! PARAMETROS
    ! C= F(EP1)/F(EP)
    ! R=ER/KR/EP

    !--------------------------------------------------------------------------
    PARAMETER (INN=1.2E6,INB=6E6,IRU=8,INCON=8)
    !--------------------------------------------------------------------------
    !     DEFINIÇÃO DE VARIÁVEIS E GRUPOS COMUNS
    INCLUDE 'COMMON.FOR'
    !--------------------------------------------------------------------------
    INTEGER B,R,RESP
    DOUBLE PRECISION AUX,STR,STRRT,ER
    !--------------------------------------------------------------------------
    ! C=0.1  ! C= F(EP1)/F(EP) REET
    !  RR=2.0	! R=ER/KR/EP REYNG

    SS(B)=STR
    EP1=(KR(B)-REET*REYNG*KR(B)+REET)*EP(B)
    ER=KR(B)*EP(B)*REYNG
    R=0

    AA(B)=EP1
    BB(B)=ER
    CC(B)=REET
    !--------------------------------------------------------------------------

    IF (STR.GE.ER)     GO TO 40
    IF (STR.GE.EP1)    GO TO 30
    IF (STR.GE.EM(B))  GO TO 20 !COMPORTAMENTO EM CARGA
    IF (STR.GE.0.0D00) GO TO 10 !COMPORTAMENTO ELASTICO
    !--------------------------------------------------------------------------
    !  CONSIDERAÇÃO DO AMORTECIMENTO NA RELAÇÃO CONSTITUTIVA: CON > 0.0D00
    !--------------------------------------------------------------------------
    FORCE(B)=E1(B)*STR ! COMPORTAMENTO A COMPRESSÃO
    GO TO 50
    !--------------------------------------------------------------------------
10  FORCE(B)=E2(B)*STR
    GO TO 50
    !--------------------------------------------------------------------------
20  AUX=(EP1-STR)/(EP1-EP(B))*(1-REET)*EP(B)*E1(B)
    AUX=E1(B)*EP(B)*REET+AUX
    E2(B)=AUX/STR
    EM(B)=STR
    FORCE(B)=AUX
    IF (DM(B)) THEN
        DM(B)=.FALSE.
    END IF
    GO TO 50
    !--------------------------------------------------------------------------
30  AUX=(ER-STR)/(ER-EP1)*REET*EP(B)*E1(B)
    E2(B)=AUX/STR
    EM(B)=STR
    FORCE(B)=AUX
    IF (DM(B)) THEN
        DM(B)=.FALSE.
    END IF
    GO TO 50

40  E2(B)=0.0D00
    FORCE(B)=0.0D00
    EM(B)=ER
    IF (FS(B)) THEN
        R=1
        FS(B)=.FALSE.
        DM(B)=.FALSE.
    END IF
    !
50  RETURN
    END