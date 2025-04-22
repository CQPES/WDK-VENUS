||******WDK-VENUS********||
||    source code in new     ||
||****************************||

Four files need to be compiled in this folder:

interface.f-------------|------venus-E.e
venus96c-E.f----------|
venus-wkb.f90--------|
pes.f-------------------|

There is a makefile file in the folder, rewrite the name of pes.f in the makefile according to the potential energy surface used.
Then just type 'make' to compile.

Note：
(1) The VENUS source code (venus96c-E.f) is not included in this source package because the Licence for the VENUS program 
      states that no user or site will redistribute the source code or executable code to a third party in original or modified form 
      without written permission of the principle investigator (William L. Hase). The VENUS source code can be found at 
      https://www.depts.ttu.edu/chemistry/Venus/.
(1) Because of the different potential surfaces used, interface.f needs to be modified according to the potential surfaces.
(2) venus-wkb.f90 was rewritten mainly with reference to the ewkb, vwkb, turn, diamin_max, rtox, and diapot subroutines in ANT2023. 
(4) Some changes to the source code are documented in this readme file, and researchers can make their own changes after applying to VENUS.

*******************************************************************************************************
*******************************************************************************************************
change the source code:
CALL INITEBK(NA,JA,RMINA,RMAXA,DH,RMASSA,ENJA,PTESTA,ALA)
to
CALL ewkb(JA,NA,RMASSA,ENJA,rin,rout)
SDUM=ENJA*627.5096080305927d0
RMINA=rin*0.529177100d0
RMAXA=rout*0.529177100d0
CALL INITEBK(NA,JA,RMINA,RMAXA,DH,RMASSA,ENJA,PTESTA,ALA)
*******************************************************************************************************
*******************************************************************************************************
change the source code:
CALL INITEBK(NB,JB,RMINB,RMAXB,DH,RMASSB,ENJB,PTESTB,ALB)
to
CALL ewkb(JB,NB,RMASSB,ENJB,rin,rout)
SDUM=ENJB*627.5096080305927d0
RMINB=rin*0.529177100d0
RMAXB=rout*0.529177100d0
CALL INITEBK(NB,JB,RMINB,RMAXB,DH,RMASSB,ENJB,PTESTB,ALB)
*******************************************************************************************************
*******************************************************************************************************
change the SUBROUTINE INITEBK to:
SUBROUTINE INITEBK(N,J,RMIN,RMAX,DH,RMASS,ENJ,PTEST,AL)
PARAMETER(ND1=100,NDP=10)
IMPLICIT DOUBLE PRECISION (A-H,O-Z)
COMMON/CONSTN/C1,C2,C3,C4,C5,C6,C7,PI,HALFPI,TWOPI
COMMON/WASTE/QQ(3*ND1),PP(3*ND1),WX,WY,WZ,L(ND1),NAM
COMMON/PQDOT/P(3*ND1),QDOT(3*ND1),W(ND1)
    
AM=DSQRT(DBLE(J*(J+1)))
AL=AM*C7
RMASS=W(L(1))*W(L(2))/(W(L(1))+W(L(2)))
ENJ=ENJ*627.5096080305927d0*C1
PTEST=DSQRT(0.0001D0*2.0D0*RMASS*ENJ)
RETURN
END
*******************************************************************************************************
*******************************************************************************************************
add the SUBROUTINE TEST designed by our group:
      SUBROUTINE TEST
      PARAMETER(ND1=100,NDP=10)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      CHARACTER*10 TYPE
      CHARACTER*15 COMP
      COMMON/TESTB/RMAX(NDP),RBAR(NDP),NTEST,NPATHS,NABJ(NDP),NABK(NDP),
     *NPATH,NAST
      COMMON/COORS/R(ND1*(ND1+1)/2),THETA(100),ALPHA(20),CTAU(20),
     *GR(20,5),TT(20,6),DANG(300)
      COMMON/PSN2/PESN2,GA,RA,RB
      COMMON/FORCES/N,I3N,NST,NM,NB,NA,NLJ,NTAU,NEXP,NGHOST,
     *NTET,NVRR,NVRT,NVTT,NANG,NAXT,NSN2,NRYD,NHFD,NLEPSA,NLEPSB,NDMBE
      COMMON/QPDOT/Q(3*ND1),PDOT(3*ND1)
      COMMON/PRLIST/T,V,H,TIME,NTZ,NT,ISEED0(8),NC,NX
      COMMON/PQDOT/P(3*ND1),QDOT(3*ND1),W(ND1)
      COMMON/FRAGB/WTA(NDP),WTB(NDP),LA(NDP,ND1),LB(NDP,ND1),
     *QZA(NDP,3*ND1),QZB(NDP,3*ND1),NATOMA(NDP),NATOMB(NDP)
      COMMON/WASTE/QQ(3*ND1),PP(3*ND1),WX,WY,WZ,L(ND1),NAM
      COMMON/TESTIN/VRELO,INTST
      COMMON/TESTSN2/GAO,NSAD,NCBA,NCAB,IBAR
      COMMON/FINALB/EROTA,EROTB,EA(3),EB(3),AMA(4),AMB(4),AN,AJ,BN,BJ,
     *OAM(4),EREL,ERELSQ,BF,SDA,SDB,DELH(NDP),ANG(16),NFINAL
      DIMENSION QCMA(3),VCMA(3),QCMB(3),VCMB(3),QR(3),VR(3)
C
      real*8 rnh1,rnh2,rnh3,rnh4,rh1h2,rh1h3,rh1h4,rh2h3,rh2h4,rh3h4,
     &ct(3,5),xvec(3,15)
      
 900  FORMAT(4X,' TURNING POINT #  ','  CYCLE ','  RCM(A)',
     *       '    EA     ','    EB     ','    EROTA  ','    EROTB  ',
     *       '    JA     ','    JB     ','    L      ')
 903  FORMAT(8X,A10,I4,I8,F8.3,7D11.4)
 905  FORMAT(/,5X,32H$$$$BARRIER CROSSING NUMBER$$$$ ,I6,
     &       10H  AT CYCLE,I8)
 906  FORMAT(5X,37H$$$$BARRIER CROSSING FROM B TO A $$$$)
 907  FORMAT(5X,37H$$$$BARRIER CROSSING FROM A TO B $$$$)
 935  FORMAT(7X,4HRA= ,F7.3,3X,4HRB= ,F7.3,3X,4HGA= ,F7.3)
 910  FORMAT(4X,' TURNING POINT #  ',5X,' COMPLEX ',6X,
     *'  CYCLE ','  RCM(A)',
     *       '    EA     ','    EB     ','    EROTA  ','    EROTB  ',
     *       '    JA     ','    JB     ','    L      ')
 913  FORMAT(8X,A10,I4,2X,A17,I8,F8.3,7D11.4)

      NTEST=0   ! if R(JK).le.RBAR, NTEST=0
      M=NPATHS+1
      DO 1 I=1,M
         NPATH=I

       ct(:,1)=Q(4:6)!-->O1
       ct(:,2)=Q(7:9)!-->O2
       ct(:,3)=Q(1:3)  !-->O3
        rO1O2=dsqrt(dot_product(ct(:,2)-ct(:,1),ct(:,2)-ct(:,1)))
        rO1O3=dsqrt(dot_product(ct(:,3)-ct(:,1),ct(:,3)-ct(:,1)))
        rO2O3=dsqrt(dot_product(ct(:,2)-ct(:,3),ct(:,2)-ct(:,3)))

       if(I==1)then !-->nonreactive channel  12+3
       if(rO1O2.le.1.9d0.and.rO1O3.ge.RBAR(I).and.rO2O3.ge.RBAR(I))
     &  NTEST=1
       if(rO1O2.le.1.9d0.and.rO1O3.ge.RMAX(I).and.rO2O3.ge.RMAX(I))
     &  NTEST=2
       endif

       if(I==2)then ! 1+23
       if(rO2O3.le.1.9d0.and.rO1O2.ge.RBAR(I).and.rO1O3.ge.RBAR(I))
     &  NTEST=1
       if(rO2O3.le.1.9d0.and.rO1O2.ge.RMAX(I).and.rO1O3.ge.RMAX(I))
     &  NTEST=2
       endif
       
       if(I==3)then ! 2+13
       if(rO1O3.le.1.9d0.and.rO1O2.ge.RBAR(I).and.rO2O3.ge.RBAR(I))
     &  NTEST=1
       if(rO1O3.le.1.9d0.and.rO1O2.ge.RMAX(I).and.rO2O3.ge.RMAX(I))
     &  NTEST=2
       endif
      
       if(I==4)then ! 1+2+3
       if(rO1O2.le.1.9d0.and.rO1O3.ge.RBAR(I).and.rO2O3.ge.RBAR(I))
     &  NTEST=1
       if(rO1O2.ge.6.0d0.and.rO1O3.ge.RMAX(I).and.rO2O3.ge.RMAX(I))
     &  NTEST=2
       endif
   
            IF (NTEST.GT.0) GOTO 3

    1 CONTINUE
      NPATH=1
    3 RETURN  !NPATH=I, the path number:and NTEST=0 no reaction
      END
*******************************************************************************************************
*******************************************************************************************************


References:
1. Hase, W. L.; Duchovic, R. J.; Hu, X.; Komornicki, A.; Lim, K. F.; Lu, D.-H.; Peslherbe, G. H.; Swamy, K. N.; Vande Linde, S. R.; Varandas, A., Wang, H.; Wolf, R. J. VENUS96: A general chemical dynamics computer program, Quantum Chemical Program Exchange (QCPE) Bulletin, 1996, 16(4), 671.
2. Hu, X.; Hase, W. L.; Pirraglia, T. Vectorization of the General Monte Carlo Classical Trajectory Program VENUS. J. Comput. Chem.  1991, 12, 1014– 1024. DOI: 10.1002/jcc.540120814.
3. Y. Shu, L. Zhang, J. Zheng, Z.-H. Li, A. W. Jasper, D. A. Bonhommeau, R. Valero, Meana-Pañeda, S. L. Mielke, and D. G. Truhlar ANT 2023, University of Minnesota, Minneapolis, 2023. doi.org/10.5281/zenodo.7807406.

