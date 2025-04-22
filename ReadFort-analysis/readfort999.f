
      program readtrajout
      implicit none
      logical s_eqi
      integer s_indexi
      integer nfile, ntrj
      integer,parameter:: sumpt=5
      integer i,j,k
      character*130 fout,si,line,key0
      character*130 s1,input
      real*8 htmax,htmin,deco
      real*8, allocatable :: deltaH(:,:)
      integer*8 nij
      integer*8, allocatable :: npath(:,:)
      integer,parameter:: zero=0,one=1
      integer*8 np(sumpt),nc(sumpt),ncx(sumpt)
      integer pt,nlont

c      call execute_command_line("cp ../input_?? ./input")

      input=trim('./')//trim('input')
      open(unit=1, file=input, status='old', action='read')
      read(1,*)
      read(1,*)
      read(1,*)
      read(1,*)
      read(1,*)
      read(1,*)
      read(1,*)
      read(1,*)
      read(1,*)
      read(1,*)
      read(1,*)nfile
      read(1,*)
      read(1,*)ntrj

c      write(*,*)nfile,ntrj

      allocate(deltaH(nfile,ntrj))
      allocate(npath(nfile,ntrj))

      close(1)

      np=zero
      nc=zero
      ncx=zero
      nlont=zero
      deco=0.001d0
c      deco=0.1d0
      key0='-kcal/mol-'
      npath=zero
      deltaH=0.0d0

      write(si,'(f12.3)')deco
      si=trim('energy.convergence-')//trim(adjustl(si))
c     write(*,*) si
c     stop
      open(100,file=si)

      do i=1,nfile
       write(si,'(i4.4)')i
       call s_blank_delete(si)
       fout=trim('./')//trim(si)//'/'//trim('fort.999')
!       if(mod(i,20).eq.0)then
!        write(*,'(a)')'  read '//trim(fout)
!       endif
       
       open(22,file=fout,status='old')
       do j=1,ntrj
       read(22,'(a130)',end=99) line
!      read(22,'(a130)') line
        if( s_indexi(line,key0).gt.0) then
          read(line,'(i7,e12.5)')nij,deltaH(i,j)
          nlont=nlont+1
        else
!        write(*,*) line
         read(line,'(i7,e12.5,a10,i7)') nij,deltaH(i,j),s1,npath(i,j)
        endif
       enddo
99      continue 
      enddo

!      open(1211,file='resub.sh')
!      do i=1,nfile
!       do j=1,ntrj
!       if(npath(i,j).eq.0) then
!        write(si,'(i4.4)') i
!        call s_blank_delete(si)
!        write(1211,'(a)')' '
!        write(1211,'(a)')'cd '//trim(si)//'/'
!        write(1211,'(a)')'qsub '//'run'//trim(si)
!        write(1211,'(a)')'cd ../'
!        goto 109
!       endif
!       enddo
!109    continue
!      enddo

!      htmax=-1.d9
!      htmin=1.d9
!      do i=1,nfile
!       do j=1,ntrj
!        if(deltaH(i,j).ne.0.0d0)then
!         htmax=max(deltaH(i,j),htmax)
!         htmin=min(deltaH(i,j),htmin)
!        endif
!       enddo
!      enddo
!      write(*,*) htmax,htmin  
!      write(100,*) htmax,htmin  
!      write(100,*)'-------------------------------------------'
!      write(*,*)' Done reading'
      
      write(100,'(a)')'********************************************'
      write(100,*)'non-converged trajectories except channel 1'
      write(100,*)
      do i=1,nfile
       do j=1,ntrj
      pt=npath(i,j)
      if(pt<=zero .or. pt>sumpt)cycle
      np(pt)=np(pt)+one
      if(pt .eq. one)then
         if(dabs(deltaH(i,j)).le.deco)then 
           nc(pt)=nc(pt)+one
         else
           ncx(pt)=ncx(pt)+one
         end if
      else
         if(deltaH(i,j).le.deco)then
           write(100,'(2x,a6,i3,6x,a6,i4,6x,a6,i2)')'nfile=',i,
     &    'ntraj=',j,'npath=',npath(i,j)
           nc(pt)=nc(pt)+one
         else
!           write(100,'(2x,a6,i3,6x,a6,i4,6x,a6,i2)')'nfile=',i,
!     &    'ntraj=',j,'npath=',npath(i,j)
           ncx(pt)=ncx(pt)+one
         end if
       end if  
       end do
      end do
      write(100,'(a)')'********************************************'
      write(100,*) 'criterion =', deco 
      write(100,'(a)')'********************************************'
      do k=one,sumpt
         write(100,'(5x,a,i2,1x,a,i8)')'        total path',k,':',np(k)
         write(100,'(5x,a,i2,1x,a,i8)')'    converged path',k,':',nc(k)
         write(100,'(5x,a,i2,1x,a,i8)')'non-converged path',k,':',ncx(k)
         write(100,'(a)')'********************************************'
      end do
      write(100,'(a,i8)')'         sum of total traj:',sum(np)+nlont
      write(100,'(a,i8)')'        sum of normal traj:',sum(np)
      write(100,'(a,i8)')'     sum of long time traj:',nlont
      write(100,'(a,i8)')'     sum of converged traj:',sum(nc)
      write(100,'(a,i8)')' sum of non-converged traj:',sum(ncx)
      write(100,'(a)')'********************************************'
      close(100)
!      write(*,*)'Done!'
      end program readtrajout

!********************************************************************************

      subroutine s_blank_delete ( s )
      implicit none
!-->-->-->-->-->
      character c
      integer iget
      integer iput
      integer nchar
      character ( len = * ) s
      character, parameter :: TAB = char ( 9 )
!-->-->-->-->-->
      iput = 0
      nchar = len_trim ( s )

      do iget = 1, nchar

        c = s(iget:iget)

        if ( c /= ' ' .and. c /= TAB ) then
          iput = iput + 1
          s(iput:iput) = c
        end if

      end do

      s(iput+1:nchar) = ' '

       return
       end subroutine s_blank_delete

      function s_indexi ( s, sub )
        implicit none
      
        integer i
        integer llen1
        integer llen2
        character ( len = * ) s
        logical s_eqi
        integer s_indexi
        character ( len = * ) sub
     
        s_indexi = 0
       
        llen1 = len_trim ( s )
        llen2 = len_trim ( sub )
    
      !-->  In case S or SUB is blanks, use LEN.
        if ( llen1 == 0 ) then
          llen1 = len ( s )
        end if
       
        if ( llen2 == 0 ) then
          llen2 = len ( sub )
        end if
       
        if ( llen2 > llen1 ) then
          return
        end if
       
        do i = 1, llen1 + 1 - llen2
       
          if ( s_eqi ( s(i:i+llen2-1), sub ) ) then
            s_indexi = i
                return
          end if
                 
         end do
                
        return
      end  function s_indexi 
      
      function s_eqi ( s1, s2 )
        implicit none
     
        character c1
        character c2
        integer i
        integer len1
        integer len2
        integer lenc
        logical s_eqi
        character ( len = * ) s1
        character ( len = * ) s2
    
        len1 = len ( s1 )
        len2 = len ( s2 )
        lenc = min ( len1, len2 )
       
        s_eqi = .false.
        do i = 1, lenc
          c1 = s1(i:i)
          c2 = s2(i:i)
          call ch_cap ( c1 )
          call ch_cap ( c2 )
          if ( c1 /= c2 ) then
            return
          end if
        end do
       
        do i = lenc + 1, len1
          if ( s1(i:i) /= ' ' ) then
           return
          end if
        end do
             
        do i = lenc + 1, len2
           if ( s2(i:i) /= ' ' ) then
                   return
            end if
        end do
         s_eqi = .true.
                     
          return
        end  function s_eqi


      subroutine ch_cap ( c )
        implicit none
   
        character c
        integer itemp
  
        itemp = ichar ( c )
       
        if ( 97 <= itemp .and. itemp <= 122 ) then
          c = char ( itemp - 32 )
        end if
       
        return
      end subroutine ch_cap
  
!******************************************************************************
