!*****************************************************************************
!*****************************************************************************
!更新：计算N和J的子程序
!modified by Jun Li Group of CQU// 2024.03 
!*****************************************************************************
!*****************************************************************************
        subroutine ewkb(J,N,RMASS,ENJ,rin,rout)
        IMPLICIT DOUBLE PRECISION (A-H,O-Z)

        PARAMETER(ND1=100,NDP=10,vtol=1.0d-5,autoev=27.2113845d0)

        COMMON/CONSTN/C1,C2,C3,C4,C5,C6,C7,PI,HALFPI,TWOPI
        COMMON/WASTE/QQ(3*ND1),PP(3*ND1),WX,WY,WZ,L(ND1),NAM
        COMMON/PQDOT/P(3*ND1),QDOT(3*ND1),W(ND1)
        
        integer N, J, i
        double precision xv, xj, rmin, rmax, high, low, emin, emax, rin, rout
        double precision  xvguess
              
        xj=dble(J)
        xv=dble(N)
        !PI=4.0D0*DATAN(1.0D0)

        RMASS=W(L(1))*W(L(2))/(W(L(1))+W(L(2)))
        auRMASS=1822.844987d0*RMASS
              
        call diamin_max(rmin, emin, xj, auRMASS, 1)
        call diamin_max(rmax, emax, xj, auRMASS, -1)
        write(6,*)'******************Refer to the ANT-WKB (modified by jwy 5/2024)****************'
        write(6,*)
        !write(6,'(a,f4.0)')'For v = 0, j =',xj
        write(6,'(a,es17.10,a,f16.10,a)')' Minimum energy for diatom is ',emin*autoev,' eV at ',rmin,' bohr'
        write(6,*)'Local maximum or asym energy is ',emax*autoev,' eV'
        write(6,*)'*******************************************************************************'
        write(6,*)
          
        ! entering bisection search for root v
        high = emax
        low = emin
        do i = 1, ND1
          ENJ = 0.5d0*(high + low)
          ! need to recalculate the values of turning point
          ! after each iteration
          if (abs(ENJ - emax).lt.1.0d-15) then
          ! if we are too close to the maximum
          ! exit loop and just return the best value
          ! print*,'v is too close to the limit.'
            ENJ = -100d0
            return
          endif
          call vwkb(auRMASS, ENJ, xj, xvguess, rin, rout)
          if (abs(xvguess - dble(xv)).le.vtol) then
            goto 6
          else
            if (xvguess > dble(xv)) then
              ! guess is too high
              high = ENJ
              low = low
            else
              ! guess is too low
              low = ENJ
              high = high
            endif
            if (abs(ENJ - emax) < 1.0d0-10) then
              print*, 'value of e too close to the limit'
              return
            endif
          endif
        enddo
    6   return
        print*, 'too many iterations in bisection!'
        print*, 'returning the best value for e'
        end subroutine ewkb
  
!=============================================================================
!=============================================================================
  
        subroutine vwkb(auRMASS, ENJ, xj, xv, rin, rout)
        PARAMETER(ND1=100,NDP=10,ncheb=20)
        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
        COMMON/CONSTN/C1,C2,C3,C4,C5,C6,C7,PI,HALFPI,TWOPI
        COMMON/WASTE/QQ(3*ND1),PP(3*ND1),WX,WY,WZ,L(ND1),NAM
        COMMON/PQDOT/P(3*ND1),QDOT(3*ND1),W(ND1)      
        integer :: i, arr
        double precision :: ENJ, xv, xj, rin, rout
        double precision :: pot, val, r
        double precision sum_val, xe(ncheb), we(ncheb), arg, auRMASS, rmid, rdif
        
        sum_val = 0.0d0
        !PI=4.0D0*DATAN(1.0D0)
        
        call turn(auRMASS, ENJ, xj, rin, rout)
        
        do i = 1, ncheb
          arg = i*PI/dble(ncheb + 1)
          xe(i) = cos(arg)
          we(i) = (PI/dble(ncheb + 1))*(sin(arg)**2)
        enddo
        
        if (rin > rout) then
          print*, 'inner turning point is greater then outer!'
          stop
        else if (rin == rout) then
          xv = -0.5
          return
        else
          rmid = 0.5d0*(rin + rout)
          rdif = 0.5d0*(rout - rin)
          do i = 1, ncheb
            r = rmid + xe(i)*rdif
            call diapot(r, auRMASS, pot, xj)
            if (ENJ - pot < 0.0d0) then
              print*, 'Energy is less than the potential?',' Try diamin_max again'
              stop
            else
              val = we(i)*sqrt((ENJ - pot)/((r - rin)*(rout - r)))
              sum_val = sum_val + val
            endif
          enddo
          xv = -0.5d0 + (sqrt(2.0d0*auRMASS)*(rdif**2)*sum_val)/PI
        endif
        end subroutine vwkb
  
!=============================================================================
!=============================================================================
        module diaturn_mod
        ! saving several variables for calculating potential
        implicit none
        double precision, save ::  xj_pass, ENJ_pass, RMASS_pass
        end module diaturn_mod 
        
        subroutine turn(auRMASS, ENJ, xj, rin, rout)
        use diaturn_mod, only: xj_pass, ENJ_pass, RMASS_pass
        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
        PARAMETER(ND1=100,NDP=10,tol=1.0d-12)
        COMMON/CONSTN/C1,C2,C3,C4,C5,C6,C7,PI,HALFPI,TWOPI
        COMMON/WASTE/QQ(3*ND1),PP(3*ND1),WX,WY,WZ,L(ND1),NAM
        COMMON/PQDOT/P(3*ND1),QDOT(3*ND1),W(ND1)
        double precision rin, rout, ENJ, rmin, rmax, f_diaturn, brent
        double precision xj, emin, emax, ai, bi, auRMASS
        external brent, f_diaturn
        ENJ_pass = ENJ
        xj_pass = xj
        RMASS_pass = auRMASS
  
        ! get the minimum energy eMin and distance rMin
        call diamin_max(rmin, emin, xj, auRMASS, 1)
        ! get the maximum
        if (xj.ge.0) then
          ! if j >= 0 then get the local maximum/asymptotic energy
          call diamin_max(rmax, emax, xj, auRMASS, -1)
        else if (xj < 0) then
          ! the case of j < 0 is not legal
          write(6,*) 'j =', xj,', which cannot be < 0! Try again.'
          stop
        endif
        ! initialize some paramters for Brent's root finding
        ! bracketing the first turning point
        ai = 0.3d0
        bi = rmin
        if (ENJ < emin) then
          rIn = rmin
          rOut = rmin
          write(6,*) 'diatomic energy',ENJ,' is less than the',' minimum vib',' energy ',emin, 'for this value of j (',xj,')'
        else if (ENJ == emin) then
          rin = rmin
          rout = rmin
        else
          ! now compute the other turning point (outer)
          rin = brent(ai, bi, f_diaturn, tol, 0.d0, 0.d0)
          if (ENJ > emax) then
            print*, 'failure to bracket outer turning point'
            print*, 'energy level is larger than maximum energy'
            stop
          else
            ai = rmin
            bi = rmax
            rout = brent(ai, bi, f_diaturn, tol, 0.d0, 0.d0)
          endif
        endif
        end subroutine turn
  
        double precision function f_diaturn(r)
        use diaturn_mod, only: xj_pass, ENJ_pass, RMASS_pass
        implicit none
        double precision :: r, pot, RMASS
        call diapot(r, RMASS_pass, pot, xj_pass)
        f_diaturn = pot - ENJ_pass
        end function f_diaturn
  
!=============================================================================
!=============================================================================
  
        subroutine diamin_max(xopt, optval, xj, auRMASS, max_opt)
        implicit none
        ! general variables for calculating potential energy
        integer max_opt
        double precision xj
        ! variables for bracketing using mnbrak
        double precision ax, bx, cx, fa, fb, fc
        double precision GOLD, GLIMIT, TINY, dum
        double precision fu, q, r, u, ulim
        parameter (GOLD = 1.618034d0, GLIMIT = 100.0d0, TINY = 1.0d-20)
        ! additional parameters for brent optimization
        integer ITMAX
        double precision optval, xopt, tol, CGOLD
        double precision ZEPS
        parameter (ITMAX=100,CGOLD=.3819660,ZEPS=1.0e-10)
        integer iter
        double precision ai, bi, dx, e, etemp, fv
        double precision fw, fx, p, tol1, tol2
        double precision v, w, x, xm, auRMASS

        tol = 1d-15
        if (max_opt==-1) then
          ax = 3.0
          bx = 1.01d0*ax
        elseif (max_opt==1) then
          ax = 1.8
          bx = 1.05d0*ax
        else
          write(6,*) 'max_opt can only take value of 1 or -1. Try again.'
          stop
        endif
        
        if (xj==0) then
          if (max_opt==-1) then
            xopt = 100.0d0
            call diapot(xopt, auRMASS, optval, xj)
            return
          endif
        endif
          
        call diapot(ax, auRMASS, fa, xj)
        call diapot(bx, auRMASS, fb, xj)
        fa = max_opt*fa
        fb = max_opt*fb
        if (fb > fa) then
          dum = ax
          ax = bx
          bx = dum
          dum = fb
          fa = fb
          fb = dum
        endif
        ! move on to finding c
        ! first guess for c
        cx = bx + GOLD*(bx - ax)
        call diapot(cx, auRMASS, fc, xj)
        fc = max_opt*fc
          ! entering the loop until the third point is found
    2   if (fb > fc) then
          !quadratic interpolation
          r = (bx - ax)*(fb - fc)
          q = (bx - cx)*(fb - fa)
          u=bx-((bx-cx)*q-(bx-ax)*r)/(2d0*sign(max(abs(q-r),TINY),q-r))
          ! setting the limit for searching u
          ! won't be searching for u beyond this point
          ulim  = bx + GLIMIT*(cx - bx)
          ! checking of u is between b and c
            if ((bx-u)*(u-cx) > 0) then
              call diapot(u, auRMASS, fu, xj)
              fu = max_opt*fu
              ! consider if u is a minimum
              ! case 1: f(u) < f(c) < f(b) and u is between b and c
              ! then interval contains minimum is b, u, c
            if (fu < fc) then
              ax = bx
              fa = fb
              bx = u
              fb = fu
              go to 100
              ! case 2: if f(u) > f(b) and f(a) > f(b) then
              ! interval contain minimum is a, b, u
            else if (fu > fb) then
              cx = u
              fc = fu
              go to 100
            endif
            ! otherwise: parabolic interpolation yielded no result
            ! use the default maginification
            u = cx + GOLD*(cx - bx)
            call diapot(u, auRMASS, fu, xj)
            fu = max_opt*fu
          ! if u is not between b and c but
          ! between c and ulim
          else if ((cx-u)*(u-ulim) > 0) then
            call diapot(u, auRMASS, fu, xj)
            fu = max_opt*fu
            ! case 1: if f(u) < f(c)
            if (fu < fc) then
              bx = cx
              cx = u
              u = cx + GOLD*(cx - bx)
              fb = fc
              fc = fu
              call diapot(u, auRMASS, fu, xj)
              fu = max_opt*fu
            endif
            !case 2: parabolic value is to the limit value
          else if ((u - ulim)*(ulim - cx) > 0) then
            u = ulim
            call diapot(u, auRMASS, fu, xj)
            fu = max_opt*fu
          else
            u = cx + GOLD*(cx - bx)
            call diapot(u, auRMASS, fu, xj)
            fu = max_opt*fu
          endif
          ! eliminate the initial point and continue guessing
          ax = bx
          bx = cx
          cx = u
          fa = fb
          fb = fc
          fc = fu
          goto 2
        endif
          
        !***************************************
        ! BRENT OPTIMIZATION (MINIMIZATION)
        !***************************************
  100   continue
        ai = min(ax, cx)
        bi = max(ax, cx)
        v = bx
        w = v
        x = v
        e = 0.
        call diapot(x, auRMASS, fx, xj)
        fx = max_opt*fx
        fv = fx
        fw = fx
        do iter = 1, ITMAX
          xm = 0.5*(ai + bi)
          tol1 = tol*abs(x)+ZEPS
          tol2 = 2.*tol1
          if(abs(x-xm).le.(tol2-.5*(bi-ai))) goto 5
          if(abs(e).gt.tol1) then
            r = (x - w)*(fx - fv)
            q = (x - v)*(fx - fw)
            p = (x - v)*q - (x - w)*r
            q = 2.*(q - r)
            if(q.gt.0.) p = -p
            q = abs(q)
            etemp = e
            e = dx
            if(abs(p).ge.abs(.5*q*etemp).or.p.le.q*(ai-x).or.p.ge.q*(bi-x)) goto 3
            dx = p/q
            u = x + dx
            if(u-ai.lt.tol2 .or. bi-u.lt.tol2) dx=sign(tol1,xm-x)
            goto 4
          endif
    3     if (x.ge.xm) then
            e = ai - x
          else
            e = bi - x
          endif
          dx = CGOLD*e
    4     if (abs(dx).ge.tol1) then
            u = x + dx
          else
            u = x + sign(tol1,dx)
          endif
          call diapot(u, auRMASS, fu, xj)
          fu = max_opt*fu
          if (fu.le.fx) then
            if (u.ge.x) then
              ai = x
            else
              bi = x
            endif
            v = w
            fv = fw
            w = x
            fw = fx
            x = u
            fx = fu
          else
            if(u.lt.x) then
              ai = u
            else
              bi = u
            endif
            if(fu.le.fw .or. w.eq.x) then
              v = w
              fv = fw
              w = u
              fw = fu
            else if(fu.le.fv .or. v.eq.x .or. v.eq.w) then
              v = u
              fv = fu
            endif
          endif
        enddo
          
        print*, 'brent exceed maximum iterations'
    5   xopt = x
        call diapot(xopt, auRMASS, optval, xj)
        !write(*,*)xopt, RMASS, optval
        return
        end subroutine diamin_max
  
!*****************************************************************************
!*****************************************************************************
  
        double precision function brent(x1,x2,func,tol,fa0,fb0)
        integer :: iter
        integer, parameter :: itmax=100
        double precision :: a,b,c,d,e,fa,fb,fc,p,q,r,s,tol1,xm
        double precision, intent(in) :: x1,x2,tol,fa0,fb0
        double precision, external :: func
        double precision, parameter :: eps=epsilon(x1)
  
        a=x1
        b=x2
  
        if(fa0.ne.0d0)then
          fa=fa0
        else
          fa=func(a)
        endif
        if(fb0.ne.0d0)then
          fb=fb0
        else
          fb=func(b)
        endif
  
        if ((fa > 0.0 .and. fb > 0.0) .or. (fa < 0.0 .and. fb < 0.0)) &
          write(6,*) 'root must be bracketed for brent'
          !write(6,*) 'a,f(a)=',a,fa
          !write(6,*) 'b,f(b)=',b,fb
        c=b
        fc=fb
        do iter=1,itmax
          if ((fb > 0.0 .and. fc > 0.0) .or. (fb < 0.0 .and. fc < 0.0)) then
              c=a
              fc=fa
              d=b-a
              e=d
          end if
          if (abs(fc) < abs(fb)) then
              a=b
              b=c
              c=a
              fa=fb
              fb=fc
              fc=fa
          end if
          tol1=2.0d0*eps*abs(b)+0.5d0*tol
          xm=0.5d0*(c-b)
          if (abs(xm) <= tol1 .or. fb == 0.0) then
              brent=b
              return
          end if
          if (abs(e) >= tol1 .and. abs(fa) > abs(fb)) then
              s=fb/fa
              if (a == c) then
                  p=2.0d0*xm*s
                  q=1.0d0-s
              else
                  q=fa/fc
                  r=fb/fc
                  p=s*(2.0d0*xm*q*(q-r)-(b-a)*(r-1.0d0))
                  q=(q-1.0d0)*(r-1.0d0)*(s-1.0d0)
              end if
              if (p > 0.0) q=-q
              p=abs(p)
              if (2.0d0*p  <  min(3.0d0*xm*q-abs(tol1*q),abs(e*q))) then
                  e=d
                  d=p/q
              else
                  d=xm
                  e=d
              end if
          else
              d=xm
              e=d
          end if
          a=b
          fa=fb
          b=b+merge(d,sign(tol1,xm), abs(d) > tol1 )
          fb=func(b)
        end do
        write(6,*) 'brent: exceeded maximum iterations'
        brent=b
        end function brent
  
!*****************************************************************************
!*****************************************************************************

        subroutine rtox(xx,r,ido)
        PARAMETER(ND1=100,NDP=10)
        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
        integer :: i,j,ido
        double precision :: r(3),xx(3,ND1),costh,sinth
  
        if (ido.eq.1) then  !  transform xx to internals
          r(1)=0.d0
          r(2)=0.d0
          r(3)=0.d0
          do i=1,3
            r(1) = r(1) + (xx(i,1)-xx(i,2))**2
            r(2) = r(2) + (xx(i,2)-xx(i,3))**2
            r(3) = r(3) + (xx(i,1)-xx(i,3))**2
          enddo
          do i=1,3
            r(i)=sqrt(r(i))
          enddo
  
        else !     transform internals to xx
  
          costh = (-r(3)**2+r(1)**2+r(2)**2)/(2.d0*r(1)*r(2))
          costh = max(-1.d0,costh)
          costh = min(1.d0,costh)
          sinth = sqrt(1.d0-costh**2)
  
          xx(1,1) = r(1)
          xx(2,1) = 0.d0
          xx(3,1) = 0.d0
          xx(1,2) = 0.d0
          xx(2,2) = 0.d0
          xx(3,2) = 0.d0
          xx(1,3) = r(2)*costh
          xx(2,3) = r(2)*sinth
          xx(3,3) = 0.d0
        endif
  
        end subroutine rtox
  
        subroutine diapot(l,auRMASS,v,xj)
        PARAMETER(ND1=100,NDP=10)
        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
        integer :: im,j,i,icall,ii
        double precision :: v,xx(3,ND1),r(3),xj,erot,auRMASS,l
        character*2 :: symb(ND1)
  
        double precision :: dx(ND1),dy(ND1),dz(ND1),x(ND1),y(ND1),z(ND1)
  
  
        r(1) = l
        r(2) = 100.0d0
        r(3) = 100.0d0
  
        call rtox(xx,r,0)
        ! RMP14
        ! Calls specific diatomic subroutine
        do i=1,3
          x(i) = xx(1,i)
          y(i) = xx(2,i)
          z(i) = xx(3,i)
        enddo

        !write(6,*) x(1),y(1),z(1)
        !write(6,*) x(2),y(2),z(2)
        !write(6,*) x(3),y(3),z(3)
  
        call pot(x,y,z,v,dx,dy,dz)

        !write(6,*) 'Et==',v
  
        ! add rotation if j > 0
        if (xj.ge.0) then
          erot = 0.5d0*(xj+0.5d0)**2/(auRMASS*l**2)
          v = v + erot
        endif
  
        end subroutine diapot

!*****************************************************************************
!*****************************************************************************
