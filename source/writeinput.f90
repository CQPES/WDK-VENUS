module commondata

implicit none
character*100 fscript,fcpinp,fsbatch,fcpout,outbmax,f317

end module commondata

program main
use commondata

implicit none
integer i,style,traj,folder,n,j,du
real*8 xseed
real*8 bmax,Et,RMAX,RBAR,S,mA,mB,mC
real*8 dH,cD,cE,cF,cG,cH,cI
character(100) :: ftmp
character(100) :: cmd
character(10) :: opinion
character(80) :: inp_name

if (iargc() .ne. 1) then
    print *, "Usage: ./exe inp_name"
else
    call getarg(1, inp_name)
    inp_name=trim(inp_name)
    print *, "Input name: ", inp_name
end if

! stop

call ReadInputFile(inp_name,style,traj,folder,n,j,Et,bmax,RMAX,RBAR,S,dH,mA,mB,mC,cD,cE,cF,cG,cH,cI)

5 fsbatch='sbatch.sc'
open(105,file=fsbatch)

!########################################################
ftmp='cp /home/jwyang/WDK-VENUS/'   !change for your dict ##
!########################################################

fcpinp='script.cp' 
open(101,file=fcpinp)
write(101,'(a)')trim(ftmp)//'new/venus-E.e ./'
!write(101,'(a)')trim(ftmp)//'biases.txt ./'
!write(101,'(a)')trim(ftmp)//'weights.txt ./'
!write(101,'(a)')trim(ftmp)//'fit-nh4-.coef ./'
!write(101,'(a)')trim(ftmp)//'ReadFort-analysis/readfort999.f ./'
write(101,'(a)')trim(ftmp)//'ReadFort-analysis/readfort2628.py ./'

call init_random_seed()

do i=1,folder !nfile
  call random_number(xseed)
  if (style == 2) then
    style = style - 1
    call writeinp(i,xseed,style,traj,folder,n,j,Et,bmax,inp_name,RMAX,RBAR,S,dH,mA,mB,mC,cD,cE,cF,cG,cH,cI)
    style = style + 1
  else
    call writeinp(i,xseed,style,traj,folder,n,j,Et,bmax,inp_name,RMAX,RBAR,S,dH,mA,mB,mC,cD,cE,cF,cG,cH,cI)
  endif
enddo

write(101,'(a)') ' '
write(101,'(a)') 'cp ./sbatch.sc ./'//trim(inp_name)//'-inpark'
write(101,'(a)') 'cp ./'//trim(inp_name)//' ./'//trim(inp_name)//'-inpark/input'
!write(101,'(a)') 'mv ./readfort999.f ./'//trim(inp_name)//'-inpark'
write(101,'(a)') 'mv ./readfort2628.py ./'//trim(inp_name)//'-inpark'
!write(101,'(a)') 'cp ~/scripts/grep.sh ../'
write(101,'(a)') 'cd '//trim(inp_name)//'-inpark'
write(101,'(a)') 'chmod +x sbatch.sc'
write(101,'(a)') './sbatch.sc > scancel.x'
write(101,'(a)') "sed -i 's/^.\{19\}/scancel /g' scancel.x" 
write(101,'(a)') 'chmod +x scancel.x'

close(101)

call execute_command_line("chmod +x script.cp")
call execute_command_line("./script.cp")
call execute_command_line('sleep 1s')
call execute_command_line("rm -r inp0* run0*")

if (style == 1) then
   open(106,file=trim(inp_name)//'-inpark/test-bmax')
   write(106,'(a,f5.1)') "(*.*)zzz---wait---",bmax
   call execute_command_line('sleep 90s')

   call testbmax(inp_name,traj,du)
   if (du == 0) then
        call execute_command_line("rm -r sbatch.sc script.cp")
        bmax=bmax+0.1d0
        goto 5
    elseif (du == 1) then
        write(106,*) '**finish!**'
        write(106,'(f5.1,I3,I4,f5.1)') Et,n,j,bmax
        open(107,file='../array-bmax.txt',access='append')
        !open(107,file='../array-bmax.txt',status='old',access='append')
        write(107,'(f5.1,I3,I4,f5.1)') Et,n,j,bmax
        close(107)
    endif
else
    stop
endif
end program main


subroutine writeinp(ii,xseed,style,traj,folder,n,j,Et,bmax,inp_name,RMAX,RBAR,S,dH,mA,mB,mC,cD,cE,cF,cG,cH,cI)
implicit none

integer ii,style,traj,n,j,folder
character*100 finp,sii,sx,sy,frun,srun,fout
character*80 inp_name
integer*8 nseed
real*8 xseed
real*8 bmax,Et,RMAX,RBAR,S
real*8 mA,mB,mC,dH
real*8 cD,cE,cF,cG,cH,cI

nseed=1.0d9*dabs(dsqrt(xseed))

write(sii,'(i4.4)')ii
call s_blank_delete(sii)

finp=trim('inp')//trim(sii)

frun=trim('run')//trim(sii)

fout=trim('out')//trim(sii)

call slurmscript(sii,finp,fout,frun)

write(105,'(a)')' '
write(105,'(a)')'cd '//trim(sii)//'/'
write(105,'(a)')'sbatch '//trim(frun)
write(105,'(a)')'cd '//'../'

!open(130,file=frun)

!write(130,'(a)')'#!/bin/bash'
!write(130,'(a)')'#SBATCH -J '//'O3-1AP-'//trim(sii)
!write(130,'(a)')'#SBATCH --time=720:00:00'
!write(130,'(a)')'#SBATCH -N 1 '
!write(130,'(a)')'#SBATCH -p gworkq '
!write(130,'(a)')'#SBATCH --ntasks-per-node=2'
!write(130,'(a)')' '
!write(130,'(a)')'ulimit -d unlimited' 
!write(130,'(a)')'ulimit -s unlimited' 
!write(130,'(a)')'ulimit -t unlimited' 
!write(130,'(a)')'ulimit -v unlimited' 
!write(130,'(a)')' '
!write(130,'(a)')' '
!write(130,'(a)')'cd $SLURM_SUBMIT_DIR'
!write(130,'(a)')'username=`whoami`'
!write(130,'(a)')' '
!write(130,'(a)')' '
!write(130,'(a)')'VENUS_TMPDIR=/tmp/${username}.$SLURM_JOBID'
!write(130,'(a)')'if [ ! -a $VENUS_TMPDIR ]; then'
!write(130,'(a)')'   mkdir -p $VENUS_TMPDIR'
!write(130,'(a)')'fi'
!write(130,'(a)')'export VENUS_TMPDIR'
!write(130,'(a)')' '
!write(130,'(a)')'echo "Starting Venus run at `hostname` on ${SLURM_JOB_NODELIST}:" `date` > ${SLURM_JOB_ID}.log'
!write(130,'(a)')' '
!write(130,'(a)')'srun -n 1 -c $SLURM_NTASKS ./venus-E.e < '//trim(finp)//' > '//trim(fout)
!write(130,'(a)')' '
!write(130,'(a)')'echo "Finished Venus run on ${SLURM_JOB_NODELIST}:" `date` >> ${SLURM_JOB_ID}.log'
!write(130,'(a)')' '
!write(130,'(a)')'rm -rf $VENUS_TMPDIR '

!close(130)

write(101,'(a)') ' '

! write(101,'(a)') 'mkdir -p ./inpark/'//trim(sii)//'/'
write(101,'(a)') 'mkdir -p ./'//trim(inp_name)//'-inpark/'//trim(sii)
! write(101,'(a)') 'cp ./'//trim(finp)//' ./inpark/'//trim(sii)//'/'
!                 cp    finp           inp_name/sii
write(101,'(a)') 'cp '//trim(finp)//' ./'//trim(inp_name)//'-inpark/'//trim(sii)
!write(101,'(a)') 'cp ./venus-E.e ./inpark/'//trim(sii)//'/'
write(101,'(a)') 'cp venus-E.e ./'//trim(inp_name)//'-inpark/'//trim(sii)
!write(101,'(a)') 'cp ./biases.txt ../'//trim(sii)//'/'
!write(101,'(a)') 'cp ./weights.txt ../'//trim(sii)//'/'
!      write(101,'(a)') 'cp ./fit-nh4-.coef ../'//trim(sii)//'/'
write(101,'(a)') 'cp '//trim(frun)//' ./'//trim(inp_name)//'-inpark/'//trim(sii)
  
open(120,file=finp)
write(120,'(a)')'QCT TRAJECTORY CALCULATION'
write(120,'(a)')'ATB--CQU'
write(120,'(a)')'3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,11,0,0,0,0,0,'
write(120,'(a)')'0.0,'   !vzero
write(120,'(a)')'2,0,'   !NSELT,NCHKP
write(120,'(F8.5,",",F8.5,",",F8.5)') mA,mB,mC   !mass weight in au O+O2 = O2+O 15.99491,15.99491,15.99491
write(120,'(I0,",",a)') traj,'1000000,100,200,'   !NT,NS,NIP,NCROT
write(120,'(a)')'0.005'   !time step
write(sx,*)nseed
call s_blank_delete(sx)
sy=trim(sx)

write(120,'(a)')'0,0,'//trim(sy)//','   !NACTA,NACTB,ISEED
!write(120,'(a)')'5,5,'//trim(sy)//','   !NACTA,NACTB,ISEED, thermal sampling
!write(120,'(a)')'0.001,2,'   ! HINC, NPTS
write(120,'(a)')'1,0,'       ! NATOMA, NLINA
write(120,'(a)')'0.0000000000, 0.0000000000, 0.0000000000'!coordinate for O
!write(120,'(a)')'0,0,'   ! diatomic number of vibrational quanta and diatomic number of rotational quanta
! write(120,'(a)')'1000.0,0.2770222,0,0,' ! N,J -->diatomic number of vibrational quanta and diatomic number of rotational quanta
write(120,'(a)')'2,1,'   !NATOMB, NLINB
write(120,'(F13.10,",",F13.10,",",F13.10)') cD,cE,cF !coordinate for O -1.3875305800,0.2567237100,0.0000000000
write(120,'(F13.10,",",F13.10,",",F13.10)') cG,cH,cI !coordinate for O -2.5955305800,0.2567237100,0.0000000000
write(120,'(I0,",",I0)') n, j! 2-atom-n,j/3-atom-ground state, all v=0, increasing frequencies
!write(120,'(a)')'1,106.0,'   ! rotational
! write(120,'(a)')'1000.0,'   ! TVIB -->vibrational temperature for Boltzmann distribution
! write(120,'(a)')'0,1000.0,'   !NROTA, TROTA

write(120,'(a,F4.1,",",F4.1,a)')'1,2,',RMAX,RBAR,',0.0,'!NABJ(1),NABK(1),rmax(1),rbar(1),delH
write(120,'(a,F5.1,",",F4.1)')'1,',Et,S!  NREL,SEREL,s
write(120,'(I0,",",F3.1)') style,bmax ! NOB,BMAX 
write(120,'(a)')'3,' !NPATHS, other reaction paths except for reagent A and reagent B

write(120,'(F4.1,",",F4.1,a)')RMAX,RBAR,',1,2,0.0,' !rmax(2),rbar(2),NATOMA(2),NATOMB(2),delH(2)
write(120,'(a)')'1,4'   !NABJ(2),NABK(2)
write(120,'(a)')'2'   !index for atomic numbers for fragment A
write(120,'(F13.10,",",F13.10,",",F13.10)') cD,cE,cF !xyz for fragment 0
write(120,'(a)')'1,3'   !index for atomic numbers for fragment B
write(120,'(a)')' 0.0000000000, 0.0000000000, 0.0000000000'!xyz for fragment O
write(120,'(F13.10,",",F13.10,",",F13.10)') cG,cH,cI !xyz for fragment O
 
write(120,'(F4.1,",",F4.1,a)')RMAX,RBAR,',1,2,0.0,' !rmax(2),rbar(2),NATOMA(2),NATOMB(2),delH(2)
write(120,'(a)')'1,4'   !NABJ(2),NABK(2)
write(120,'(a)')'3'   !index for atomic numbers for fragment A
write(120,'(F13.10,",",F13.10,",",F13.10)') cG,cH,cI !xyz for fragment O
write(120,'(a)')'1,2'   !index for atomic numbers for fragment B
write(120,'(a)')' 0.0000000000, 0.0000000000, 0.0000000000'!xyz for fragment O
write(120,'(F13.10,",",F13.10,",",F13.10)') cD,cE,cF !xyz for fragment O

write(120,'(F4.1,",",F4.1,a,",",F5.1)')RMAX,RBAR,',1,2',dH !rmax(2),rbar(2),NATOMA(2),NATOMB(2),delH(2)
write(120,'(a)')'1,4'   !NABJ(2),NABK(2)
write(120,'(a)')'1'   !index for atomic numbers for fragment A
write(120,'(a)')' 0.0000000000, 0.0000000000, 0.0000000000'!xyz for fragment O
write(120,'(a)')'2,3'   !index for atomic numbers for fragment B
write(120,'(F13.10,",",F13.10,",",F13.10)') cD,cE,cF !xyz for fragment O
write(120,'(F13.10,",",F13.10,",",F13.10)') cG,cH,cI !xyz for fragment O


! write(120,'(a)')'50.0,10.0,5,0,-2.304,'!RMAX(2),RBAR(2),NATOMA(2),NATOMB(2),delH(2)
! write(120,'(a)')'2,3'   !NABJ(2),NABK(2)
! write(120,'(a)')'1,2,3,4,5'   !index for atomic numbers for fragment A
! write(120,'(a)')'-0.0000000000,-0.0025871634, 1.0409897854'!xyz for fragment H
! write(120,'(a)')'-0.9802655328,-0.0474922017,-0.3471172430'!xyz for fragment H
! write(120,'(a)')' 0.4482563213, 0.8739719257,-0.3448271315'!xyz for fragment H
! write(120,'(a)')' 0.5320092101,-0.8238925467,-0.3490468288'!xyz for fragment H
! write(120,'(a)')' 0.0000000001,-0.0000000010, 0.0000001020'!xyz for fragment N
! write(120,'(a)')'3,4'   !index for atomic numbers for fragment B
! write(120,'(a)')' 6.54709917, 0.00000000,-1.33357692,'   !xyz for fragment SH
! write(120,'(a)')' 7.89038916, 0.00000000,-1.33378692,'   !xyz for fragment SH
 
write(120,'(a)')'1,1,'  !NFQP,NCOOR
write(120,'(a)')'0,0,'  !NFR,NUMR
write(120,'(a)')'0,0,'  !NFB,NUMB
write(120,'(a)')'0,0,'  !NFA,NUMA
write(120,'(a)')'0,0,'  !NFTAU,NUMTAU
write(120,'(a)')'0,0,'  !NFTET,NUMTET
write(120,'(a)')'0,0,'  !NFDH,NUMDH

close(120)

return
end subroutine writeinp

subroutine s_blank_delete ( s )
implicit none
!-->-->-->-->
character c
integer iget
integer iput
integer nchar
character ( len = * ) s
character, parameter :: TAB = char ( 9 )
!-->-->-->-->
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


subroutine init_random_seed()
 integer :: i,n,clock
 integer,dimension(:),allocatable :: seed
 call random_seed(size=n)
 allocate(seed(n))
 call system_clock(count=clock)
 seed=clock+37*(/(i-1,i=1,n)/)
!seed=seed+getpid()
 call  random_seed(PUT=seed) 
 deallocate(seed)
 return
end
