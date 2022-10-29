    SUBROUTINE CONSTIT_EXP (B,STR,STRRT,R)
    !     ==================
    !
    !  ESTA SUBROTINA � CHAMADA PELA SUBROTINA CABAR PARA MODELAR A RELA��O
    !  CONSTITUTIVA DE CADA BARRA DE ACORDO COM UM DIAGRAMA EXPONENCIAL FOR�A VS.
    !  DEFORMA��O, COM AMOLECIMENTO ("STRAIN SOFTENING'). O PROCESSO DE
    !  DESCARGA � MODELADO DE ACORDO COM A HIP�TESE DE DANO-EL�STICO.
    !
    !  FORMA DA CURVA PROPOSTA PARA O AMOLECIMENTO:
    !      FORCE_AUX=AA*EXP[-BB*(STRAIN-AUX)]
    !

    !--------------------------------------------------------------------------
    PARAMETER (INN=1.2E6,INB=6E6,IRU=8,INCON=8)
    !--------------------------------------------------------------------------
    !     DEFINI��O DE VARI�VEIS E GRUPOS COMUNS
    INCLUDE 'COMMON.FOR'
    !--------------------------------------------------------------------------
    INTEGER B,R,RESP
    DOUBLE PRECISION AUX,STR,STRRT,ER
    !--------------------------------------------------------------------------
    SS(B)=STR

    BB(B)=2/EP(B)/(KR(B)-1)
    AA(B)=EP(B)*E1(B)*EXP(BB(B)*EP(B))

    ER=KR(B)*EP(B)*3
    R=0


    !--------------------------------------------------------------------------
    IF (STR.GE.EM(B))  GOTO 20 !COMPORTAMENTO EM CARGA
    IF (STR.GE.0.0D00)  GOTO 10 !COMPORTAMENTO ELASTICO
    !--------------------------------------------------------------------------
    !  CONSIDERA��O DO AMORTECIMENTO NA RELA��O CONSTITUTIVA: CON > 0.0D00
    !--------------------------------------------------------------------------
    FORCE(B)=E1(B)*STR ! COMPORTAMENTO A COMPRESS�O
    GO TO 40
    !--------------------------------------------------------------------------
10  FORCE(B)=E2(B)*STR
    GO TO 40
    !--------------------------------------------------------------------------
20  FORCE(B)=AA(B)*EXP(-BB(B)*STR)

    E2(B)=FORCE(B)/STR
    EM(B)=STR

    IF (DM(B)) THEN
        DM(B)=.FALSE.
    END IF

    !--------------------------------------------------------------------------
    IF (STR.GE.ER) THEN
        IF (FS(B)) THEN
            R=1
            FS(B)=.FALSE.
            DM(B)=.FALSE.
        END IF
    ENDIF
    !
40  RETURN
    END