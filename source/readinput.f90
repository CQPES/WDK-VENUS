SUBROUTINE ReadInputFile(input,style,channel,traj,folder,n,j,Et,bmax,RMAX,RBAR,S,dH,mw,xyz)  
  IMPLICIT NONE  
  INTEGER, INTENT(OUT) :: n, j, channel  
  INTEGER, INTENT(OUT) :: style, traj, folder  
  REAL*8, INTENT(OUT) :: Et, RMAX, RBAR, S  
  REAL*8, INTENT(OUT) :: bmax,dH
  REAL*8, INTENT(OUT) :: mw(3)
  REAL*8, INTENT(OUT) :: xyz(3,3)  
  CHARACTER(256) :: line
  CHARACTER(80) :: input
  INTEGER :: io_stat
  LOGICAL :: f_ini,f_channel,f_bmax,f_style,f_folder,f_traj,f_R,f_mass,f_coord,f_deltH 
  
  input = trim(input)  
  
  OPEN(UNIT=1, FILE=input, STATUS='OLD', ACTION='READ')  
  
  f_style = .FALSE.
  f_channel = .FALSE.
  f_mass =  .FALSE.
  f_coord =  .FALSE.
  f_deltH = .FALSE.
  f_traj = .FALSE.  
  f_folder = .FALSE.  
  f_ini = .FALSE.  
  f_R = .FALSE.  
  f_bmax = .FALSE.  
  
  DO  
    READ(1, '(A)', IOSTAT=io_stat) line  
    IF (io_stat /= 0) EXIT  
  
    IF (INDEX(line, '%style') /= 0) THEN  
      READ(1, *) style  
      f_style = .TRUE.
    ELSE IF (INDEX(line, '%channel') /= 0) THEN  
      READ(1, *) channel  
      f_channel = .TRUE.
    ELSE IF (INDEX(line, '%mass') /= 0) THEN  
      READ(1, *) mw(1),mw(2),mw(3)  
      f_mass = .TRUE.
    ELSE IF (INDEX(line, '%coord') /= 0) THEN
      READ(1, *) xyz(:,1)
      READ(1, *) xyz(:,2)
      READ(1, *) xyz(:,3) 
      f_coord = .TRUE.
    ELSE IF (INDEX(line, '%deltH') /= 0) THEN
      READ(1, *) dH
      f_deltH = .TRUE.  
    ELSE IF (INDEX(line, '%traj') /= 0) THEN  
      READ(1, *) traj  
      f_traj = .TRUE.  
    ELSE IF (INDEX(line, '%folder') /= 0) THEN  
      READ(1, *) folder  
      f_folder = .TRUE.  
    ELSE IF (INDEX(line, '%ini') /= 0) THEN  
      READ(1, *) n, j, Et  
      f_ini = .TRUE.  
    ELSE IF (INDEX(line, '%R') /= 0) THEN  
      READ(1, *) S, RMAX, RBAR  
      f_R = .TRUE.  
    ELSE IF (INDEX(line, '%bmax') /= 0) THEN  
      READ(1, *) bmax  
      f_bmax = .TRUE.  
    END IF  
  
    IF (f_style.AND.f_channel.AND.f_mass.AND.f_coord.AND.f_deltH.AND.f_traj.AND.f_folder.AND.f_ini.AND.f_bmax.AND.f_R) EXIT  
  END DO  
  
  CLOSE(UNIT=1)  
END SUBROUTINE ReadInputFile
