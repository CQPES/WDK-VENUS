***********************************************************************
C   System:                     O3
C   Functional form:            permutation-invariant polynomials
C   Common name:                O3 1 1Ap (1st adiab. singlet A' state)
C   Number of derivatives:      1
C   Number of bodies:           3
C   Number of electronic surfaces: 1
C   Interface: Section-2
C
C   References:: Z. Varga, Y. Paukku, and D. G. Truhlar,
C     "Potential energy surfaces for O + O2 collisions"
C      J. Chem. Phys. 147, 154312/1-17 (2017).
C
C   Notes:    -Mixed-exponential-Gaussian (MEG)
C              variables are applied
C             -Diatomic potential contains 
C              dispersion correction
C
C   Input: X(3),Y(3),Z(3)               in unit of bohr
C   Output: E                           in unit of hartree
C   Output: dEdX(3),dEdY(3),dEdZ(3)     hartree/bohr
***********************************************************************

      subroutine pot(X,Y,Z,E,dEdX,dEdY,dEdZ)

      implicit double precision (a-h,o-z)

C
C Convert factors
C Cconv: bohr to Angstrom 
C        1 bohr = 0.52917721092 angstrom
C Econv: kcal/mol to hartree 
C        1 kcal/mol = 0.159360144 * 10^-2 hartree
C Gconv: kcal/(mol*Angstrom) to hartree/bohr
C        1 kcal mol^-1 angstrom^-1 = 0.843297564 * 10^-3 hartree/bohr
C
      double precision Cconv
      double precision Econv
      double precision Gconv
      parameter(Cconv=0.52917721092d0)
      parameter(Econv=0.159360144d-2)
      parameter(Gconv=0.843297564d-3)
C
C Reference energy of infinitely separated O2 + O in hartree (taken
C from DSEC corrected calculations)
C
      double precision Eref
      parameter(Eref=-0.19172848d0)

      integer i
      double precision E,V
      double precision X(3),Y(3),Z(3),dEdX(3),dEdY(3),dEdZ(3)
      double precision Xcart(9),dVdX(9)

C Convert to local variables
      do i=1,3
        Xcart(3*i-2)=X(i)*Cconv
        Xcart(3*i-1)=Y(i)*Cconv
        Xcart(3*i)=Z(i)*Cconv
      enddo

      call o3pes(Xcart,v,dVdX,1)

C Convert local output to the ones ANT wants

      E = v*Econv + Eref
      do i=1,3
        dEdX(i)=dVdX(3*i-2)*Gconv
        dEdY(i)=dVdX(3*i-1)*Gconv
        dEdZ(i)=dVdX(3*i)*Gconv
      enddo

      end

C
C Local variables used in the O3 PES subroutine
C input coordinate matrix: X(9)in Ang
C                          o1: X(1),X(2),X(3)
C                          O2: X(4),X(5),X(6)
C                          O3: X(7),X(8),X(9)
C input flag: igrad     igrad=0 energy-only calculation
C                       igrad=1 energy + gradient
C output potential energy:      v    in kcal/mol
C output gradient:              dVdX in kcal/(mol*Ang)
C
      subroutine o3pes(X,v,dVdX,igrad)
***********************************************************************
* Subroutine to calculate the potential energy V and gradient dVdX
* for given Cartesian coordinates X(9)  
* R:            Interatomic bond distance (3)
* V:            Calculated potential energy
* dVdX:         The derivative of V w.r.t. X, dim(9)
* dVdR:         The derivative of V w.r.t. R, dim(3) 
* dPdR:         The derivative of basis functions w.r.t. R
*               dim(3*67)
* dMdR:         The derivative of monomials w.r.t. R
*               dim(3*8)
* dRdX:         The derivative of R w.r.t. X, dim(3*9)
***********************************************************************
      
      implicit double precision (a-h,o-z) 

      integer i,igrad,j,nob,k
      double precision V
      double precision dVdX(12),X(12) 

C      common /coord/    R(3)
C      common /epot/     rMs(3),rM(0:7),P(0:66),C(56),B(56)
C      common /gradt/    dMsdR(3,3),dMdR(3,0:7),dPdR(3,0:66),dVdR(3),
C     $                  dRdX(3,9),dBdR(3,56)
C      common /msprmt/   a,ab,ra,rb

C Read Cartesian coordinate from input file
      call coord_convt(X)

      if (igrad .le. 1) then
C Call subroutine Evv to evaluate potential energy V
        call evv(V)

        if (igrad .eq. 1) then
C Call EvdVdX to evaluate the derivatives of V w.r.t. X
          call evdvdx(X,dVdX)
        endif
      else
        write (*,*) 'Only igrad = 0, 1 is allowed!'
      endif

      end 
      subroutine coord_convt(X)
***********************************************************************
*  Program to calculate the six interatomic distance 
*  by reading XYZ coordinate
***********************************************************************
      implicit double precision (a-h,o-z) 

      integer i
      double precision X(12)

      common /coord/    R(3)
      
***********************************************************************
*  Now, calculate the inter-atomic distance
*  r1 = r(O1O2)
*  r2 = r(O1O3)
*  r3 = r(O2O3)       
***********************************************************************

      R(1)=Sqrt((X(4)-X(1))**2 + (X(5)-X(2))**2 + (X(6)-X(3))**2)
      R(2)=Sqrt((X(7)-X(1))**2 + (X(8)-X(2))**2 + (X(9)-X(3))**2)
      R(3)=Sqrt((X(4)-X(7))**2 + (X(5)-X(8))**2 + (X(6)-X(9))**2)

      return

      end
      subroutine EvV(V)
***********************************************************************
* Subroutine to evaluate V for given R 
* V(R) = C*P
* C:            Coefficients, stored in 'dim.inc' 
* P:            Basis functions evaluated for given R
* rMs:          rMs(3), six mixed exponential-Gaussian terms (MEG)
* a:            Nonlinear parameters in Morse terms(Angstrom)
* ab:           Nonlinear parameters in Gauss terms(Angstrom^2)
* re:           Equilibrium bond length(Angstrom)
* nop:          number of points
* nom:          number of monomials
* nob:          number of basis functions(polynomials)
* rM(0:7):      Array to store monomials
* P(0:66):      Array to store polynomials
* B(1:56):      Array to store basis functions
***********************************************************************
      
      implicit double precision (a-h,o-z)

      integer i,j,k
      double precision dist,dv2dr,V,V2,disp,dispdr(6)

      common /coord/    R(3)
      common /epot/     rMs(3),rM(0:7),P(0:66),C(56),B(56)

C Calculate the six MEG terms for each point
      call evmorse

C Calculate the monomials for each point by using six MEG terms
      call evmono

C Calculate the polynomials (basis functions) by using monomials
      call evpoly 

C Calculate the basis functions by removing unconnected and 2-body terms
      call evbas

C Initialized v to be 2De 2*120.243 kcal/mol
      v=240.486d0
C Evaluate 2-body interactions
      do i=1,3
        dist=r(i)
        call ev2gm2(dist,v2,dv2dr,4,0)
        v=v+v2
      enddo

C Add D3 dispersion correction
        call d3disp(r,disp,dispdr,0)
        v=v+disp

C Evaluate V by taken the product of C and Basis function array
      do i=1,56
        v=v + c(i)*b(i)
      enddo

C      Write(*,9999) V 
C 9999 Format('The potential energy is ',F20.14,' kcal/mol')

      return

      end 
      subroutine EvdVdX(X,dVdX)
***********************************************************************
* Subroutine to evaluate dRdX for given R and X 
* R:            R(3), 3 bond lengths
* X:            X(9), 9 Cartesian coordinates
* rM(0:7):      Array to store monomials
* P(0:66):      Array to store polynomials
* dVdX:         dVdX(9), derivatives of V w.r.t. Cartesian coordinates 
* dVdR:         dVdR(3), derivatives of V w.r.t. 3 bond lengths
* dRdX:         dRdX(3,9), derivatives of R(3) w.r.t. 9  
*               Cartesian coordinates
***********************************************************************
      
      implicit double precision (a-h,o-z)

      integer i,j
      double precision dVdX(9),X(9)

      common /coord/    R(3)
      common /epot/     rMs(3),rM(0:7),P(0:66),C(56),B(56)
      common /gradt/    dMsdR(3,3),dMdR(3,0:7),dPdR(3,0:66),dVdR(3),
     $                  dRdX(3,9),dBdR(3,56)

C Initialize dVdX
      do i=1,9
        dVdX(i)=0.0d0
      enddo

C Call EvdVdR to evaluate dVdR(3)
      Call evdvdr

C Call EvdRdX to evaluate dRdX(3,9)
      Call evdrdx(X)  

C Calculate dVdX by using chain rule: dV/dXi=(dV/dRj)*(dRj/dXi), j=1 to
C 3
      do i=1,9
        do j=1,3
          dVdX(i)=dVdX(i) + dVdR(j)*dRdX(j,i)
        enddo
      enddo

C      write(*,*) 'The 9 dVdX are:'
C      Write(*,9999) (dVdX(i),i=1,9) 
C 9999 Format(1x,3F15.8)

      return
      end 
      subroutine EvMorse
      
      implicit double precision (a-h,o-z)

      integer i

      common /coord/    R(3)
      common /msprmt/   a,ab,ra,rb
      common /epot/     rMs(3),rM(0:7),P(0:66),C(56),B(56)
      
C mixed exponential-Gaussian terms = exp(-(r-ra)/a-(r-rb)^2/ab)
C ra:   reference bond length
C rb:   reference bond length
C a:    nonlinear parameter, unit Anstrom
C ab:   nonlinear parameter, unit Anstrom^2
      do i=1,3
         rms(i)=Exp(-(r(i)-ra)/a-((r(i)-rb)**2.0d0)/ab)
      enddo

      end 
      subroutine EvMono
***********************************************************************
*  The subroutine reads six MEG variables(X) and calculates the
*  monomials(M) that do not have usable decomposition.
*  For A4 with max. degree 10, the number of monomials is nom.
***********************************************************************

      implicit double precision (a-h,o-z)

      common /epot/     rMs(3),rM(0:7),P(0:66),C(56),B(56)

      rm(0) = 1.0d0
      rm(1) = rms(3)
      rm(2) = rms(2)
      rm(3) = rms(1)
      rm(4) = rm(1)*rm(2)
      rm(5) = rm(1)*rm(3)
      rm(6) = rm(2)*rm(3)
      rm(7) = rm(1)*rm(6)

      return

      end subroutine EvMono
      subroutine EvPoly
***********************************************************************
*  The subroutine reads monomials(m) and calculates the
*  permutation invariant polynomials(p)
*  For A4 with max. degree 10, the number of polynomials is nob.
***********************************************************************

      implicit double precision (a-h,o-z)

      common /epot/     rMs(3),rM(0:7),P(0:66),C(56),B(56)

      p( 0) = rm(0)
      p( 1) = rm(1) + rm(2) + rm(3)
      p( 2) = rm(4) + rm(5) + rm(6)
      p( 3) = p( 1)*p( 1) - p( 2) - p( 2)
      p( 4) = rm(7)
      p( 5) = p( 1)*p( 2) - p( 4) - p( 4) - p( 4)
      p( 6) = p( 1)*p( 3) - p( 5)
      p( 7) = p( 1)*p( 4)
      p( 8) = p( 2)*p( 2) - p( 7) - p( 7)
      p( 9) = p( 2)*p( 3) - p( 7)
      p(10) = p( 1)*p( 6) - p( 9)
      p(11) = p( 2)*p( 4)
      p(12) = p( 3)*p( 4)
      p(13) = p( 1)*p( 8) - p(11)
      p(14) = p( 2)*p( 6) - p(12)
      p(15) = p( 1)*p(10) - p(14)
      p(16) = p( 4)*p( 4)
      p(17) = p( 4)*p( 5)
      p(18) = p( 4)*p( 6)
      p(19) = p( 2)*p( 8) - p(17)
      p(20) = p( 1)*p(13) - p(17) - p(19) - p(19)
      p(21) = p( 2)*p(10) - p(18)
      p(22) = p( 1)*p(15) - p(21)
      p(23) = p( 1)*p(16)
      p(24) = p( 4)*p( 8)
      p(25) = p( 3)*p(11) - p(23)
      p(26) = p( 4)*p(10)
      p(27) = p( 1)*p(19) - p(24)
      p(28) = p( 6)*p( 8) - p(23)
      p(29) = p( 2)*p(15) - p(26)
      p(30) = p( 1)*p(22) - p(29)
      p(31) = p( 2)*p(16)
      p(32) = p( 3)*p(16)
      p(33) = p( 1)*p(24) - p(31)
      p(34) = p( 4)*p(14)
      p(35) = p( 4)*p(15)
      p(36) = p( 2)*p(19) - p(33)
      p(37) = p( 3)*p(19) - p(31)
      p(38) = p( 8)*p(10) - p(32)
      p(39) = p( 2)*p(22) - p(35)
      p(40) = p( 1)*p(30) - p(39)
      p(41) = p( 4)*p(16)
      p(42) = p( 4)*p(17)
      p(43) = p( 4)*p(19)
      p(44) = p( 6)*p(16)
      p(45) = p( 4)*p(20)
      p(46) = p( 4)*p(21)
      p(47) = p( 4)*p(22)
      p(48) = p( 1)*p(36) - p(43)
      p(49) = p( 1)*p(37) - p(45) - p(48)
      p(50) = p( 8)*p(15) - p(44)
      p(51) = p( 2)*p(30) - p(47)
      p(52) = p( 1)*p(40) - p(51)
      p(53) = p( 1)*p(41)
      p(54) = p( 4)*p(24)
      p(55) = p( 3)*p(31) - p(53)
      p(56) = p( 1)*p(43) - p(54)
      p(57) = p(10)*p(16)
      p(58) = p( 4)*p(28)
      p(59) = p( 4)*p(29)
      p(60) = p( 4)*p(30)
      p(61) = p( 2)*p(36) - p(56)
      p(62) = p( 3)*p(36) - p(54)
      p(63) = p(10)*p(19) - p(53)
      p(64) = p( 8)*p(22) - p(57)
      p(65) = p( 2)*p(40) - p(60)
      p(66) = p( 1)*p(52) - p(65)

      return

      end subroutine EvPoly
      subroutine evbas
***********************************************************************
*  The subroutine eliminate the 2-body terms in Bowman's approach
***********************************************************************

      implicit double precision (a-h,o-z)

      common /epot/     rMs(3),rM(0:7),P(0:66),C(56),B(56)
      
      integer i
      double precision b1(67) 

C Pass P(0:66) to BM1(1:67)
      do i=1,67
        b1(i)=p(i-1)
      enddo

C Remove unconnected terms and 2-body terms and pass to B(1:430)
      b(1)=b1(3)

      do i=2,3
        b(i)=b1(i+3)
      enddo

      do i=4,6
        b(i)=b1(i+4)
      enddo

      do i=7,10
        b(i)=b1(i+5)
      enddo

      do i=11,16
        b(i)=b1(i+6)
      enddo

      do i=17,23
        b(i)=b1(i+7)
      enddo

      do i=24,32
        b(i)=b1(i+8)
      enddo

      do i=33,43
        b(i)=b1(i+9)
      enddo

      do i=44,56
        b(i)=b1(i+10)
      enddo

      return

      end 
      subroutine ev2gm2(r,v,grad,imol,igrad) 
***********************************************************************
*
* Compute the diatomic potential of ground-state triplet O2
*
* References: J. Chem. Phys. 132, 074307 (2010)
*
* Input:  r      interatomic distance in Angstrom
* Output: V      potential in kcal/mol
*         grad   gradient (kcal/mol)/Angstrom
*
***********************************************************************
      implicit none
      integer imol
      double precision  r
      double precision v, grad
C Parameters of analytical even-tempered Gaussian expansions for the
C ground
C state potential energy curve of O2 CBS+SR+SO+CV. Units: alpha in
C Anstromgs^-2, beta=dimensionless, a_k in milihartree.
      double precision :: alpha,beta,a(0:7)
      integer :: k, igrad
! Original parameters
!      alpha = 0.785d0
!      beta = 1.307d0
!      a(0) = -2388.5641690d0
!      a(1) = 18086.977116d0
!      a(2) = -71760.197585d0
!      a(3) = 154738.09175d0
!      a(4) = -215074.85646d0
!      a(5) = 214799.54567d0
!      a(6) = -148395.42850d0
!      a(7) = 73310.781453d0

! Modified parameters for D3(BJ)
       alpha = 9.439784362354936d-1
       beta =  1.262242998506810d0
       a(0) = -1.488979427684798d3
       a(1) =  1.881435846488955d4
       a(2) = -1.053475425838226d5
       a(3) =  2.755135591229064d5
       a(4) = -4.277588997761775d5
       a(5) =  4.404104009614092d5
       a(6) = -2.946204062950765d5
       a(7) =  1.176861219078620d5

      v=0.d0
      do k=0,7
       v= v + a(k)*dexp(-alpha*beta**k*r**2)
      enddo
C From milihartree to kcal/mol
      v=v*627.509523475149d-3
C Compute the gradient if needed
      if (igrad.eq.1) then
       grad=0.d0
       do k=0,7
         grad=grad-2.d0*a(k)*alpha*beta**k*r*dexp(-alpha*beta**k*r**2)
       enddo
C Convert from milihartree/A to i(kcal/mol)/A
         grad=grad*627.509523475149d-3
      endif
      return
      end

      subroutine EvdVdR
***********************************************************************
* Subroutine to evaluate dVdR for given R 
* dVdR = dV2dR + C*dBdR
* C:            Coefficients, stored in 'dim.inc' 
* P:            Basis functions evaluated for given R
* M:            Monomials evaluated for given R
* dV2dR:        Gradient of 2-body interactions
* dMsdR:        dMsdR(3,3), 3 MEG terms w.r.t. 3 bond lengths
* dMdR:         dMdR(3,nom), nom monomials w.r.t.3 bond length
* dPdR:         dPdR(3,nob), nop polynomial basis functions 
*               w.r.t. 3 bond lengths
* nom:          number of monomials
* nob:          number of basis functions(polynomials)
* M(nom):       Array to store monomials
* P(nob):       Array to store polynomials
***********************************************************************
      
      implicit double precision (a-h,o-z)
      
      integer i,j
      double precision dist,v2,dv2dr,disp,dispdr(3)

      common /coord/    R(3)
      common /epot/     rMs(3),rM(0:7),P(0:66),C(56),B(56)
      common /gradt/    dMsdR(3,3),dMdR(3,0:7),dPdR(3,0:66),dVdR(3),
     $                  dRdX(3,9),dBdR(3,56)

C Initialize dVdR(3)
      do i=1,3
        dVdR(i)=0.0d0
      enddo

C Add dV2dR(i) to dVdR
      do i=1,3
        dist=R(i)
        call ev2gm2(dist,v2,dv2dr,4,1)
        dVdR(i)=dv2dr
      enddo

C Add numerical gradient of D3 dispersion correction
      call d3disp(R,disp,dispdr,1)
      do i=1,3
        dVdR(i)= dVdR(i) + dispdr(i)
      enddo

C Calculate dMEG/dr(3,3) for given R(3)
      call evdmsdr

C Calculate the monomials for each point by using six MEG terms
      call evdmdr

C Calculate the polynomials by using monomials
      call evdpdr 

C Remove 2-body interactions and unconnected terms from polynomials
      call evdbdr

C Evaluate dVdR(3) by taken the product of C(j) and dPdR(i,j)
      do i=1,3      
        do j=1,56
         dVdR(i)=dVdR(i) + c(j)*dBdR(i,j)
        enddo
      enddo

      return
      end 
      subroutine EvdRdX(X)
***********************************************************************
* Subroutine to evaluate dRdX for given R and X 
* R:            R(3), 3 bond lengths
* X:            X(9), 9 Cartesian coordinates
* 
* dMdR:         dMdR(3,nom), nom monomials w.r.t.3 bond length
* dPdR:         dPdR(3,nob), nop polynomial basis functions 
*               w.r.t. 3 bond lengths
* M(nom):       Array to store monomials
* P(nob):       Array to store polynomials
***********************************************************************
      
      implicit double precision (a-h,o-z)

      integer i,j
      double precision X(9)

      common /coord/    R(3)
      common /gradt/    dMsdR(3,3),dMdR(3,0:7),dPdR(3,0:66),dVdR(3),
     $                  dRdX(3,9),dBdR(3,56)

C Initialize dRdX(3,9)
      do i=1,3
        do j=1,9
          dRdX(i,j)=0.0d0
        enddo
      enddo

C Start to calculate the non-zero dRdX
C dr1dx
      dRdX(1,1)=(x(1)-x(4))/r(1)
      dRdX(1,2)=(x(2)-x(5))/r(1)
      dRdX(1,3)=(x(3)-x(6))/r(1)
      dRdX(1,4)=-dRdX(1,1)
      dRdX(1,5)=-dRdX(1,2)
      dRdX(1,6)=-dRdX(1,3)

C dr2dx
      dRdX(2,1)=(x(1)-x(7))/r(2)
      dRdX(2,2)=(x(2)-x(8))/r(2)
      dRdX(2,3)=(x(3)-x(9))/r(2)
      dRdX(2,7)=-dRdX(2,1)
      dRdX(2,8)=-dRdX(2,2)
      dRdX(2,9)=-dRdX(2,3)

C dr3dx
      dRdX(3,4)=(x(4)-x(7))/r(3)
      dRdX(3,5)=(x(5)-x(8))/r(3)
      dRdX(3,6)=(x(6)-x(9))/r(3)
      dRdX(3,7)=-dRdX(3,4)
      dRdX(3,8)=-dRdX(3,5)
      dRdX(3,9)=-dRdX(3,6)
C Finish the calculation of non-zero dRdX

      return

      end 
      subroutine EvdMsdR
***********************************************************************
* Subroutine to evaluate the derivatives of MEG term X
* w.r.t. interatomic distance R(3)
* dmsdR:        Local variables, dirm(3,3)
* a:            Nonlinear pamameter(Angstrom)
* ab:           Nonlinear pamameter(Angstrom^2)
* ra:           reference bond length(Angstrom)
* rb:           reference bond length(Angstrom)
***********************************************************************
      
      implicit double precision (a-h,o-z)

      integer i,j

      common /coord/    R(3)
      common /gradt/    dMsdR(3,3),dMdR(3,0:7),dPdR(3,0:66),dVdR(3),
     $                  dRdX(3,9),dBdR(3,56)
      common /msprmt/   a,ab,ra,rb

C Initialize dmsdr
      do i=1,3
        do j=1,3
          dmsdr(i,j)=0.0d0
        enddo
      enddo
C
C MEG term dmsdr = exp(-(r-re)/a-(r-re)^2/ab)
C dmsdr(i,j)=0  i!=j
C
      do i=1,3
         dmsdr(i,i)=(-2.0d0*(r(i)-rb)/ab-1/a)*
     $Exp(-(r(i)-ra)/a-((r(i)-rb)**2.0d0)/ab)
      enddo 

      return

      end 
      subroutine EvdMdR
***********************************************************************
*  The subroutine reads M(nom) and dMSdR(3,3) and calculates the
*  dMdR(3,nom) that do not have usable decomposition.
*  For A4 with max. degree 10, the number of monomials is nom.
***********************************************************************

      implicit double precision (a-h,o-z)

      integer i

      common /epot/     rMs(3),rM(0:7),P(0:66),C(56),B(56)
      common /gradt/    dMsdR(3,3),dMdR(3,0:7),dPdR(3,0:66),dVdR(3),
     $                  dRdX(3,9),dBdR(3,56)

      do i=1,3
      dmdr(i,0) = 0.0d0
      dmdr(i,1) = dmsdr(i,3)
      dmdr(i,2) = dmsdr(i,2)
      dmdr(i,3) = dmsdr(i,1)
      dmdr(i,4) = dmdr(i,1)*rm(2) + rm(1)*dmdr(i,2)
      dmdr(i,5) = dmdr(i,1)*rm(3) + rm(1)*dmdr(i,3)
      dmdr(i,6) = dmdr(i,2)*rm(3) + rm(2)*dmdr(i,3)
      dmdr(i,7) = dmdr(i,1)*rm(6) + rm(1)*dmdr(i,6)
      enddo

      return

      end subroutine EvdMdR
      subroutine EvdPdr
***********************************************************************
*  The subroutine reads monomials(m) and calculates the
*  permutation invariant polynomials(p)
*  For A4 with max. degree 10, the number of polynomials is nob.
***********************************************************************

      implicit double precision (a-h,o-z)

      integer i

      common /epot/     rMs(3),rM(0:7),P(0:66),C(56),B(56)
      common /gradt/    dMsdR(3,3),dMdR(3,0:7),dPdR(3,0:66),dVdR(3),
     $                  dRdX(3,9),dBdR(3,56)

      do i=1,3
      dpdr(i, 0) = dmdr(i,0)
      dpdr(i, 1) = dmdr(i,1) + dmdr(i,2) + dmdr(i,3)
      dpdr(i, 2) = dmdr(i,4) + dmdr(i,5) + dmdr(i,6)
      dpdr(i, 3) = dpdr(i, 1)*p( 1) + p( 1)*dpdr(i, 1) - dpdr(i, 2)
     $ - dpdr(i, 2)
      dpdr(i, 4) = dmdr(i,7)
      dpdr(i, 5) = dpdr(i, 1)*p( 2) + p( 1)*dpdr(i, 2) - dpdr(i, 4)
     $ - dpdr(i, 4) - dpdr(i, 4)
      dpdr(i, 6) = dpdr(i, 1)*p( 3) + p( 1)*dpdr(i, 3) - dpdr(i, 5)
      dpdr(i, 7) = dpdr(i, 1)*p( 4) + p( 1)*dpdr(i, 4)
      dpdr(i, 8) = dpdr(i, 2)*p( 2) + p( 2)*dpdr(i, 2) - dpdr(i, 7)
     $ - dpdr(i, 7)
      dpdr(i, 9) = dpdr(i, 2)*p( 3) + p( 2)*dpdr(i, 3) - dpdr(i, 7)
      dpdr(i,10) = dpdr(i, 1)*p( 6) + p( 1)*dpdr(i, 6) - dpdr(i, 9)
      dpdr(i,11) = dpdr(i, 2)*p( 4) + p( 2)*dpdr(i, 4)
      dpdr(i,12) = dpdr(i, 3)*p( 4) + p( 3)*dpdr(i, 4)
      dpdr(i,13) = dpdr(i, 1)*p( 8) + p( 1)*dpdr(i, 8) - dpdr(i,11)
      dpdr(i,14) = dpdr(i, 2)*p( 6) + p( 2)*dpdr(i, 6) - dpdr(i,12)
      dpdr(i,15) = dpdr(i, 1)*p(10) + p( 1)*dpdr(i,10) - dpdr(i,14)
      dpdr(i,16) = dpdr(i, 4)*p( 4) + p( 4)*dpdr(i, 4)
      dpdr(i,17) = dpdr(i, 4)*p( 5) + p( 4)*dpdr(i, 5)
      dpdr(i,18) = dpdr(i, 4)*p( 6) + p( 4)*dpdr(i, 6)
      dpdr(i,19) = dpdr(i, 2)*p( 8) + p( 2)*dpdr(i, 8) - dpdr(i,17)
      dpdr(i,20) = dpdr(i, 1)*p(13) + p( 1)*dpdr(i,13) - dpdr(i,17)
     $ - dpdr(i,19) - dpdr(i,19)
      dpdr(i,21) = dpdr(i, 2)*p(10) + p( 2)*dpdr(i,10) - dpdr(i,18)
      dpdr(i,22) = dpdr(i, 1)*p(15) + p( 1)*dpdr(i,15) - dpdr(i,21)
      dpdr(i,23) = dpdr(i, 1)*p(16) + p( 1)*dpdr(i,16)
      dpdr(i,24) = dpdr(i, 4)*p( 8) + p( 4)*dpdr(i, 8)
      dpdr(i,25) = dpdr(i, 3)*p(11) + p( 3)*dpdr(i,11) - dpdr(i,23)
      dpdr(i,26) = dpdr(i, 4)*p(10) + p( 4)*dpdr(i,10)
      dpdr(i,27) = dpdr(i, 1)*p(19) + p( 1)*dpdr(i,19) - dpdr(i,24)
      dpdr(i,28) = dpdr(i, 6)*p( 8) + p( 6)*dpdr(i, 8) - dpdr(i,23)
      dpdr(i,29) = dpdr(i, 2)*p(15) + p( 2)*dpdr(i,15) - dpdr(i,26)
      dpdr(i,30) = dpdr(i, 1)*p(22) + p( 1)*dpdr(i,22) - dpdr(i,29)
      dpdr(i,31) = dpdr(i, 2)*p(16) + p( 2)*dpdr(i,16)
      dpdr(i,32) = dpdr(i, 3)*p(16) + p( 3)*dpdr(i,16)
      dpdr(i,33) = dpdr(i, 1)*p(24) + p( 1)*dpdr(i,24) - dpdr(i,31)
      dpdr(i,34) = dpdr(i, 4)*p(14) + p( 4)*dpdr(i,14)
      dpdr(i,35) = dpdr(i, 4)*p(15) + p( 4)*dpdr(i,15)
      dpdr(i,36) = dpdr(i, 2)*p(19) + p( 2)*dpdr(i,19) - dpdr(i,33)
      dpdr(i,37) = dpdr(i, 3)*p(19) + p( 3)*dpdr(i,19) - dpdr(i,31)
      dpdr(i,38) = dpdr(i, 8)*p(10) + p( 8)*dpdr(i,10) - dpdr(i,32)
      dpdr(i,39) = dpdr(i, 2)*p(22) + p( 2)*dpdr(i,22) - dpdr(i,35)
      dpdr(i,40) = dpdr(i, 1)*p(30) + p( 1)*dpdr(i,30) - dpdr(i,39)
      dpdr(i,41) = dpdr(i, 4)*p(16) + p( 4)*dpdr(i,16)
      dpdr(i,42) = dpdr(i, 4)*p(17) + p( 4)*dpdr(i,17)
      dpdr(i,43) = dpdr(i, 4)*p(19) + p( 4)*dpdr(i,19)
      dpdr(i,44) = dpdr(i, 6)*p(16) + p( 6)*dpdr(i,16)
      dpdr(i,45) = dpdr(i, 4)*p(20) + p( 4)*dpdr(i,20)
      dpdr(i,46) = dpdr(i, 4)*p(21) + p( 4)*dpdr(i,21)
      dpdr(i,47) = dpdr(i, 4)*p(22) + p( 4)*dpdr(i,22)
      dpdr(i,48) = dpdr(i, 1)*p(36) + p( 1)*dpdr(i,36) - dpdr(i,43)
      dpdr(i,49) = dpdr(i, 1)*p(37) + p( 1)*dpdr(i,37) - dpdr(i,45)
     $ - dpdr(i,48)
      dpdr(i,50) = dpdr(i, 8)*p(15) + p( 8)*dpdr(i,15) - dpdr(i,44)
      dpdr(i,51) = dpdr(i, 2)*p(30) + p( 2)*dpdr(i,30) - dpdr(i,47)
      dpdr(i,52) = dpdr(i, 1)*p(40) + p( 1)*dpdr(i,40) - dpdr(i,51)
      dpdr(i,53) = dpdr(i, 1)*p(41) + p( 1)*dpdr(i,41)
      dpdr(i,54) = dpdr(i, 4)*p(24) + p( 4)*dpdr(i,24)
      dpdr(i,55) = dpdr(i, 3)*p(31) + p( 3)*dpdr(i,31) - dpdr(i,53)
      dpdr(i,56) = dpdr(i, 1)*p(43) + p( 1)*dpdr(i,43) - dpdr(i,54)
      dpdr(i,57) = dpdr(i,10)*p(16) + p(10)*dpdr(i,16)
      dpdr(i,58) = dpdr(i, 4)*p(28) + p( 4)*dpdr(i,28)
      dpdr(i,59) = dpdr(i, 4)*p(29) + p( 4)*dpdr(i,29)
      dpdr(i,60) = dpdr(i, 4)*p(30) + p( 4)*dpdr(i,30)
      dpdr(i,61) = dpdr(i, 2)*p(36) + p( 2)*dpdr(i,36) - dpdr(i,56)
      dpdr(i,62) = dpdr(i, 3)*p(36) + p( 3)*dpdr(i,36) - dpdr(i,54)
      dpdr(i,63) = dpdr(i,10)*p(19) + p(10)*dpdr(i,19) - dpdr(i,53)
      dpdr(i,64) = dpdr(i, 8)*p(22) + p( 8)*dpdr(i,22) - dpdr(i,57)
      dpdr(i,65) = dpdr(i, 2)*p(40) + p( 2)*dpdr(i,40) - dpdr(i,60)
      dpdr(i,66) = dpdr(i, 1)*p(52) + p( 1)*dpdr(i,52) - dpdr(i,65)
      enddo

      return

      end subroutine EvdPdR
      subroutine evdbdr
***********************************************************************
*  The subroutine eliminate the 2-body terms in Bowman's approach
***********************************************************************

      implicit double precision (a-h,o-z)

      common /gradt/    dMsdR(3,3),dMdR(3,0:7),dPdR(3,0:66),dVdR(3),
     $                  dRdX(3,9),dBdR(3,56)
      
      integer i
      double precision db1dr(3,67) 

C Pass P(3,0:66) to BM1(3,1:67)
      do j=1,3
      do i=1,67
        db1dr(j,i)=dpdr(j,i-1)
      enddo
      enddo

C Remove unconnected terms and 2-body terms and pass to B(1:56)
      do j=1,3

        dbdr(j,1)=db1dr(j,3)

      do i=2,3
        dbdr(j,i)=db1dr(j,i+3)
      enddo

      do i=4,6
        dbdr(j,i)=db1dr(j,i+4)
      enddo

      do i=7,10
        dbdr(j,i)=db1dr(j,i+5)
      enddo

      do i=11,16
        dbdr(j,i)=db1dr(j,i+6)
      enddo

      do i=17,23
        dbdr(j,i)=db1dr(j,i+7)
      enddo

      do i=24,32
        dbdr(j,i)=db1dr(j,i+8)
      enddo

      do i=33,43
        dbdr(j,i)=db1dr(j,i+9)
      enddo

      do i=44,56
        dbdr(j,i)=db1dr(j,i+10)
      enddo

      enddo

      return

      end
 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Dispersion correction based on Grimme's D3(BJ) calculation for
C diatomic pairs
C
C Several subroutines of DFTD3 V3.1 Rev 1 by Grimme were merged into 
C subroutine edisp and they have been heavily modified to calculate
C only dispersion energy corrections that are needed.
C
C S. Grimme, J. Antony, S. Ehrlich and H. Krieg
C J. Chem. Phys, 132 (2010), 154104
C and 
C S. Grimme, S. Ehrlich and L. Goerigk, J. Comput. Chem, 32 (2011),
C 1456-1465
C
C The C6 values are fixed.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      subroutine d3disp(dist,disp,dispdr,igrad)

      double precision cn(3),s6,s8,rs6,rs8
      double precision dist(3), e6(3), e8(3), disp, dispdr(3), c6(3)
      double precision e6dr(3),e8dr(3)
      integer iz(3), mxc(94), i, j, igrad
      double precision c6ab(94,94,5,5,3)
      double precision r2r4(94)
      double precision autoang,autokcal

      autoang =0.52917726d0
      autokcal=627.509541d0

! Generalized parameters for BJ damping from P. Verma, B. Wang, 
! L. E. Fernandez, and D. G. Truhlar, J. Phys. Chem. A 121, 2855 (2017).
      s6= 1.0d0
      s8= 2.0d0
      rs6= 0.5299d0
      rs8= 2.20d0

      do i=1,3
      dist(i)=dist(i)/autoang
      enddo

C iz for O4 system
      iz(1)=8
      iz(2)=8
      iz(3)=8
C C6 for O4 system
      c6(1)=12.8d0
      c6(2)=12.8d0
      c6(3)=12.8d0

C Calculate dispersion correction
      call edisp(94,5,3,dist,iz,mxc,
     .     rs6,rs8,e6,e8,e6dr,e8dr,c6,0)

      disp = 0.0d0

      do i=1,3
      disp =disp + (-s6*e6(i)-s8*e8(i))*autokcal
      enddo

      if (igrad .eq. 1) then
      call edisp(94,5,3,dist,iz,mxc,
     .     rs6,rs8,e6,e8,e6dr,e8dr,c6,1)

      dispdr(:) = 0.0d0

      do i=1,3
      dispdr(i) =dispdr(i) + (-s6*e6dr(i)-s8*e8dr(i))*autokcal/autoang
      enddo
      endif

      do i=1,3
      dist(i)=dist(i)*autoang
      enddo

      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C compute energy
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      subroutine edisp(max_elem,maxc,n,dist,iz,mxc,
     .           rs6,rs8,e6,e8,e6dr,e8dr,c6a,igrad)

      implicit none  
      integer n,iz(3),max_elem,maxc,mxc(max_elem) 
      double precision dist(3),r2r4(max_elem),r0ab(max_elem,max_elem)
      double precision rs6,rs8,rcov(max_elem)
      double precision c6ab(max_elem,max_elem,maxc,maxc,3)
      double precision e6(3), e8(3), c6a(3), e6dr(3), e8dr(3)
       
      integer iat,jat,igrad
      double precision r,tmp,c6,c8,a1,a2
      double precision damp6,damp8
      double precision cn(n)                             
      double precision r2ab(n*n),cc6ab(n*n),dmp(n*n)
      integer step

      e6(:) =0.0d0
      e8(:) =0.0d0

      e6dr(:) =0.0d0
      e8dr(:) =0.0d0

      a1=rs6
      a2=rs8 

!  r2r4 =sqrt(0.5*r2r4(i)*dfloat(i)**0.5 ) with i=elementnumber
!  the large number of digits is just to keep the results consistent
!  with older versions. They should not imply any higher accuracy than
!  the old values
      r2r4(1:94)=(/
     . 2.00734898,  1.56637132,  5.01986934,  3.85379032,  3.64446594,
     . 3.10492822,  2.71175247,  2.59361680,  2.38825250,  2.21522516,
     . 6.58585536,  5.46295967,  5.65216669,  4.88284902,  4.29727576,
     . 4.04108902,  3.72932356,  3.44677275,  7.97762753,  7.07623947,
     . 6.60844053,  6.28791364,  6.07728703,  5.54643096,  5.80491167,
     . 5.58415602,  5.41374528,  5.28497229,  5.22592821,  5.09817141,
     . 6.12149689,  5.54083734,  5.06696878,  4.87005108,  4.59089647,
     . 4.31176304,  9.55461698,  8.67396077,  7.97210197,  7.43439917,
     . 6.58711862,  6.19536215,  6.01517290,  5.81623410,  5.65710424,
     . 5.52640661,  5.44263305,  5.58285373,  7.02081898,  6.46815523,
     . 5.98089120,  5.81686657,  5.53321815,  5.25477007, 11.02204549,
     .10.15679528,  9.35167836,  9.06926079,  8.97241155,  8.90092807,
     . 8.85984840,  8.81736827,  8.79317710,  7.89969626,  8.80588454,
     . 8.42439218,  8.54289262,  8.47583370,  8.45090888,  8.47339339,
     . 7.83525634,  8.20702843,  7.70559063,  7.32755997,  7.03887381,
     . 6.68978720,  6.05450052,  5.88752022,  5.70661499,  5.78450695,
     . 7.79780729,  7.26443867,  6.78151984,  6.67883169,  6.39024318,
     . 6.09527958, 11.79156076, 11.10997644,  9.51377795,  8.67197068,
     . 8.77140725,  8.65402716,  8.53923501,  8.85024712 /)

! these new data are scaled with k2=4./3. and converted to a_0 via
! autoang=0.52917726d0
      rcov(1:94)=(/
     . 0.80628308, 1.15903197, 3.02356173, 2.36845659, 1.94011865,
     . 1.88972601, 1.78894056, 1.58736983, 1.61256616, 1.68815527,
     . 3.52748848, 3.14954334, 2.84718717, 2.62041997, 2.77159820,
     . 2.57002732, 2.49443835, 2.41884923, 4.43455700, 3.88023730,
     . 3.35111422, 3.07395437, 3.04875805, 2.77159820, 2.69600923,
     . 2.62041997, 2.51963467, 2.49443835, 2.54483100, 2.74640188,
     . 2.82199085, 2.74640188, 2.89757982, 2.77159820, 2.87238349,
     . 2.94797246, 4.76210950, 4.20778980, 3.70386304, 3.50229216,
     . 3.32591790, 3.12434702, 2.89757982, 2.84718717, 2.84718717,
     . 2.72120556, 2.89757982, 3.09915070, 3.22513231, 3.17473967,
     . 3.17473967, 3.09915070, 3.32591790, 3.30072128, 5.26603625,
     . 4.43455700, 4.08180818, 3.70386304, 3.98102289, 3.95582657,
     . 3.93062995, 3.90543362, 3.80464833, 3.82984466, 3.80464833,
     . 3.77945201, 3.75425569, 3.75425569, 3.72905937, 3.85504098,
     . 3.67866672, 3.45189952, 3.30072128, 3.09915070, 2.97316878,
     . 2.92277614, 2.79679452, 2.82199085, 2.84718717, 3.32591790,
     . 3.27552496, 3.27552496, 3.42670319, 3.30072128, 3.47709584,
     . 3.57788113, 5.06446567, 4.56053862, 4.20778980, 3.98102289,
     . 3.82984466, 3.85504098, 3.88023730, 3.90543362 /)

C DFT-D3
      step=0
      do iat=1,n-1
         do jat=iat+1,n
         step=step+1
         r=dist(step)
         c6=c6a(step)
c r2r4 stored in main as sqrt
         c8 =3.0d0*c6*r2r4(iz(iat))*r2r4(iz(jat))

c energy for BJ damping
          tmp=sqrt(c8/c6)              
          e6(step)= c6/(r**6+(a1*tmp+a2)**6)
          e8(step)= c8/(r**8+(a1*tmp+a2)**8)
C calculate gradients
         if (igrad .eq. 1) then
c grad for BJ damping
          e6dr(step)=c6*(-6*r**5)/(r**6+(a1*tmp+a2)**6)**2
          e8dr(step)=c8*(-8*r**7)/(r**8+(a1*tmp+a2)**8)**2
         endif
         enddo
      enddo

      end subroutine edisp

C Begin
       block data prmt

      implicit double precision (a-h,o-z)

      common /coord/    R(3)
      common /epot/     rMs(3),rM(0:7),P(0:66),C(56),B(56)
      common /gradt/    dMsdR(3,3),dMdR(3,0:7),dPdR(3,0:66),dVdR(3),
     $                  dRdX(3,9),dBdR(3,56)
      common /msprmt/   a,ab,ra,rb

C Nonlinear parameters:
C a(in Ang)
C ab (in Ang^2)
C ra (in Ang)
C and rb (in Ang)
      data a    /1.24d0/
      data ab   /1.79d0/
      data ra   /1.18d0/
      data rb   /1.11d0/

C Linear parameters optimized by the weighted-least square fitting
      data C(   1)   /  -0.101038755197D+03 /
      data C(   2)   /  -0.173846060523D+04 /
      data C(   3)   /   0.182561485864D+04 /
      data C(   4)   /   0.128228081972D+05 /
      data C(   5)   /   0.131286683935D+04 /
      data C(   6)   /  -0.180148145918D+05 /
      data C(   7)   /   0.917232862414D+04 /
      data C(   8)   /  -0.898548422285D+05 /
      data C(   9)   /   0.236443117836D+04 /
      data C(  10)   /   0.743556379026D+05 /
      data C(  11)   /   0.169485321184D+06 /
      data C(  12)   /  -0.658211759764D+05 /
      data C(  13)   /   0.293154900522D+06 /
      data C(  14)   /   0.310179195421D+05 /
      data C(  15)   /  -0.174367178858D+05 /
      data C(  16)   /  -0.171278144114D+06 /
      data C(  17)   /  -0.196708992219D+06 /
      data C(  18)   /   0.135370348197D+06 /
      data C(  19)   /   0.126786481833D+06 /
      data C(  20)   /  -0.483041955100D+06 /
      data C(  21)   /  -0.421201296092D+05 /
      data C(  22)   /   0.133523045430D+05 /
      data C(  23)   /   0.237659552123D+06 /
      data C(  24)   /  -0.351906377925D+05 /
      data C(  25)   /   0.376297007075D+06 /
      data C(  26)   /  -0.149922827153D+06 /
      data C(  27)   /  -0.141832776431D+06 /
      data C(  28)   /   0.428298060452D+06 /
      data C(  29)   /   0.338495737904D+05 /
      data C(  30)   /   0.486620284500D+05 /
      data C(  31)   /   0.247392770653D+04 /
      data C(  32)   /  -0.195510275257D+06 /
      data C(  33)   /  -0.507721539659D+05 /
      data C(  34)   /   0.126435459710D+05 /
      data C(  35)   /  -0.227222097390D+05 /
      data C(  36)   /  -0.255415567632D+06 /
      data C(  37)   /   0.145033029297D+06 /
      data C(  38)   /   0.640437783008D+05 /
      data C(  39)   /  -0.193559554130D+06 /
      data C(  40)   /  -0.142880796980D+05 /
      data C(  41)   /  -0.368068499717D+05 /
      data C(  42)   /  -0.446680420671D+04 /
      data C(  43)   /   0.873374592726D+05 /
      data C(  44)   /   0.239902768734D+05 /
      data C(  45)   /  -0.108935637797D+06 /
      data C(  46)   /   0.564785581551D+05 /
      data C(  47)   /   0.124730127388D+05 /
      data C(  48)   /   0.288994701384D+05 /
      data C(  49)   /  -0.542994079358D+05 /
      data C(  50)   /  -0.387502623052D+04 /
      data C(  51)   /   0.341436319238D+05 /
      data C(  52)   /  -0.358667851341D+04 /
      data C(  53)   /   0.690127419637D+04 /
      data C(  54)   /   0.966576122637D+04 /
      data C(  55)   /   0.842319423210D+03 /
      data C(  56)   /  -0.162693146596D+05 /

      end

