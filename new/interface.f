!******************************************************************************

        subroutine POT0(i3n,V)

        PARAMETER(ND1=100,NDP=10)
        implicit real*8 (a-h,o-z)
        COMMON/QPDOT/Q(3*ND1),PDOT(3*ND1)
        real*8 X(3),Y(3),Z(3),rt(10),dvdx(3,10),V,E
        real*8 dEdX(3),dEdY(3),dEdZ(3)
        
        C1=0.04184000D0
        parmba=0.52917721092d0
        parmhkcal=627.5096080305927d0


        X(1)=Q(4)/parmba  !in OOO
        Y(1)=Q(5)/parmba
        Z(1)=Q(6)/parmba
        X(2)=Q(7)/parmba
        Y(2)=Q(8)/parmba
        Z(2)=Q(9)/parmba
        X(3)=Q(1)/parmba
        Y(3)=Q(2)/parmba
        Z(3)=Q(3)/parmba

        call pot(X,Y,Z,E,dEdX,dEdY,dEdZ)
        
!       write(6,*)V
!       stop
        E=E*parmhkcal
        V=E*C1          ! to integration unit

        !hartree/Bohr to kcal/mol/A
        PDOT(1) = dEdX(3)*parmhkcal*C1/parmba
        PDOT(2) = dEdY(3)*parmhkcal*C1/parmba
        PDOT(3) = dEdZ(3)*parmhkcal*C1/parmba
        PDOT(4) = dEdX(1)*parmhkcal*C1/parmba
        PDOT(5) = dEdY(1)*parmhkcal*C1/parmba
        PDOT(6) = dEdZ(1)*parmhkcal*C1/parmba
        PDOT(7) = dEdX(2)*parmhkcal*C1/parmba
        PDOT(8) = dEdY(2)*parmhkcal*C1/parmba
        PDOT(9) = dEdZ(2)*parmhkcal*C1/parmba

        return
        end subroutine POT0        
