    !-------------------------------------------------------------------------
    !-------------------------------------------------------------------------
    !
    SUBROUTINE RANDMEC2 (B,TYPES)
    !     ===================
    !
    !  ESTA SUBROTINA É CHAMADA PELAS SUBROTINAS RMECEX, RMECIN E RMECDI PARA
    !  DEFINIR A ALEATORIEDADE DAS PROPRIEDADES MECÂNICAS DAS BARRAS.
    !
    !--------------------------------------------------------------------------
    PARAMETER (INN=1.2E6,INB=6E6,IRU=6,INCON=10)
    !--------------------------------------------------------------------------
    !     DEFINIÇÃO DE VARIÁVEIS E GRUPOS COMUNS
    INCLUDE 'COMMON.FOR'
    !--------------------------------------------------------------------------
    INTEGER B,TYPES
    DOUBLE PRECISION RED,NOR,AUX,NOI,NOF
    !--------------------------------------------------------------------------
    IF (TYPES.EQ.0) RED=1.00D00
    IF (TYPES.EQ.1) RED=0.50D00
    IF (TYPES.EQ.2) RED=0.25D00
    !
    IF (TYPES.LT.3) THEN
        E1(B)=ENR(B)*RED
    ELSE
        E1(B)=EDG(B)
    END IF
    !
    E2(B)=E1(B)
    EP(B)=EPRB(B)
    !KR(B)=LCR/LI(B) !BILINEAL
    KR(B)=LCR(B)/LI(B) !TRILINEAL
    !
    IF (KR(B).LT.1.01) THEN
        WRITE (*,*)
        WRITE (*,*) '  KR < 1.01CCC  '
        WRITE (*,*)
        STOP ! CUIDADO COM ISTO. VER EXPLICAÇÃO NA DISSERTAÇÃO DO MARCELO
    END IF
    !
    !     PARA DETERMINAR QUAIS SÃO OS PÓLOS QUE PARTICIPAM DA INTERPOLAÇÃO PARA
    !     DEFINIR O PHII(B) DEVE-SE DETERMINAR O BARICENTRO DAS BARRAS
    NOI=CN(2*B-1)
    NOF=CN(2*B)
    UBAR=(U0(NOI)+U0(NOF))/2.0D00 !COORDENADA DOS BARICENTROS DAS BARRAS
    VBAR=(V0(NOI)+V0(NOF))/2.0D00 !COORDENADA DOS BARICENTROS DAS BARRAS
    WBAR=(W0(NOI)+W0(NOF))/2.0D00 !COORDENADA DOS BARICENTROS DAS BARRAS
    !
    !	LCORR MAIOR QUE LCO
    IF ((LCORX.GE.LCO).AND.(LCORY.GE.LCO).AND.(LCORZ.GE.LCO)) THEN
        !
        PMI=(UBAR*0.9999D00/LCORX) !COORDENADA NORMALIZADA DA BARRA NA DIR DE X
        PNI=(VBAR*0.9999D00/LCORY) !COORDENADA NORMALIZADA DA BARRA NA DIR DE Y
        PLI=(WBAR*0.9999D00/LCORZ) !COORDENADA NORMALIZADA DA BARRA NA DIR DE Z
        !
        MPI=INT(PMI)*LCORX !VALOR INICIAL INTEIRO DO PÓLO EM CADA DIREÇÃO
        NPI=INT(PNI)*LCORY
        LPI=INT(PLI)*LCORZ
        !
        MPJ=MPI+LCORX !VALOR FINAL INTEIRO DO PÓLO EM CADA DIREÇÃO
        NPJ=NPI+LCORY
        LPJ=LPI+LCORZ
        !
    ELSE
        !
        !	LCORR MENOR QUE LCO
        !
        PMI=(UBAR*1.0001D00)-LCORX
        PNI=(VBAR*1.0001D00)-LCORY
        PLI=(WBAR*1.0001D00)-LCORZ
        !
        IF (PMI.LT.0.0D00) THEN
            MPI=INT(PMI) !VALOR INICIAL INTEIRO DO PÓLO EM CADA DIREÇÃO
        ELSE
            MPI=PMI
        END IF
        !
        IF (PNI.LT.0.0D00) THEN
            NPI=INT(PNI)
        ELSE
            NPI=PNI
        END IF
        !
        IF (PLI.LT.0.0D00) THEN
            LPI=INT(PLI)
        ELSE
            LPI=PLI
        END IF
        !
    END IF
    !
    MPJ=MPI+LCORX !VALOR FINAL INTEIRO DO PÓLO EM CADA DIREÇÃO
    NPJ=NPI+LCORY
    LPJ=LPI+LCORZ
    !--------------------------------------------------------------------------
    !     IDENTIFICAÇÃO DOS PÓLOS EM SUAS RESPECTIVAS COORDENADAS
    !--------------------------------------------------------------------------
    !     PÓLO 1
    MP2=MPI
    NP2=NPI
    LP2=LPI
    IPT1=(NPTX*NPTY*(LP2/LCORZ))+((NPTX*NP2)/LCORY)+(MP2/LCORX)+1.0
    !
    !     PÓLO 2
    MP2=MPJ
    NP2=NPI
    LP2=LPI
    IPT2=(NPTX*NPTY*(LP2/LCORZ))+((NPTX*NP2)/LCORY)+(MP2/LCORX)+1.0
    !
    !     PÓLO 3
    MP2=MPI
    NP2=NPJ
    LP2=LPI
    IPT3=(NPTX*NPTY*(LP2/LCORZ))+((NPTX*NP2)/LCORY)+(MP2/LCORX)+1.0
    !
    !     PÓLO 4
    MP2=MPJ
    NP2=NPJ
    LP2=LPI
    IPT4=(NPTX*NPTY*(LP2/LCORZ))+((NPTX*NP2)/LCORY)+(MP2/LCORX)+1.0
    !
    !     PÓLO 5
    MP2=MPI
    NP2=NPI
    LP2=LPJ
    IPT5=(NPTX*NPTY*(LP2/LCORZ))+((NPTX*NP2)/LCORY)+(MP2/LCORX)+1.0
    !
    !     PÓLO 6
    MP2=MPJ
    NP2=NPI
    LP2=LPJ
    IPT6=(NPTX*NPTY*(LP2/LCORZ))+((NPTX*NP2)/LCORY)+(MP2/LCORX)+1.0
    !
    !     PÓLO 7
    MP2=MPI
    NP2=NPJ
    LP2=LPJ
    IPT7=(NPTX*NPTY*(LP2/LCORZ))+((NPTX*NP2)/LCORY)+(MP2/LCORX)+1.0
    !
    !     PÓLO 8
    MP2=MPJ
    NP2=NPJ
    LP2=LPJ
    IPT8=(NPTX*NPTY*(LP2/LCORZ))+((NPTX*NP2)/LCORY)+(MP2/LCORX)+1.0
    !
    !--------------------------------------------------------------------------
    !     CÁLCULO DAS INTERPOLAÇÕES LINEARES
    !--------------------------------------------------------------------------
    X=(UBAR-MPI)
    Y=(VBAR-NPI)
    Z=(WBAR-LPI)
    !
    CONJ1 = PHI(IPT1)
    CONJ2 = ((PHI(IPT2)-PHI(IPT1))/LCORX)*X
    CONJ3 = ((PHI(IPT3)-PHI(IPT1))/LCORY)*Y
    CONJ4 = ((PHI(IPT5)-PHI(IPT1))/LCORZ)*Z
    CONJ5 = ((PHI(IPT4)-PHI(IPT3)-PHI(IPT2)+PHI(IPT1))/(LCORX*LCORY))*X*Y
    CONJ6 = ((PHI(IPT6)-PHI(IPT5)-PHI(IPT2)+PHI(IPT1))/(LCORX*LCORZ))*X*Z
    CONJ7 = ((PHI(IPT7)-PHI(IPT5)-PHI(IPT3)+PHI(IPT1))/(LCORY*LCORZ))*Y*Z
    CONJ8 = ((PHI(IPT8)-PHI(IPT7)-PHI(IPT6)+PHI(IPT5)-PHI(IPT4)+&
        &PHI(IPT3)+PHI(IPT2)-PHI(IPT1))/(LCORX*LCORY*LCORZ))*X*Y*Z
    !
    PHIV=CONJ1+CONJ2+CONJ3+CONJ4+CONJ5+CONJ6+CONJ7+CONJ8
    !--------------------------------------------------------------------------
    EP(B)=PHIV*EP(B)

    !
    SS(B)=0.0D00
    EM(B)=EP(B)
    !
    IF (EP(B).GT.EPMAX) EPMAX=EP(B)
    IF (EP(B).LT.EPMIN) EPMIN=EP(B)
    !
    RETURN
    END
    !-------------------------------------------------------------------------
    !-------------------------------------------------------------------------