subroutine testbmax(input, traj, du)  
  implicit none  
  integer :: i, linum, traj, firstnum, lastnum, du, io_stat  
  character(100) :: line, kt1  
  character(80) :: input  
  logical :: end_of_file  
  
  input = trim(input)  
  
  !open the input file   
8 open(10, file=trim(input)//'-inpark/0001/fort.999', status='old')  
  
  !Reads the contents of input line by line until the end of the file or until a specific line is reading  
  linum = 0  
  end_of_file = .false.  
  do while (.not. end_of_file .AND. linum <= traj)  
    linum = linum + 1  
    if (linum > traj) then  
      !If the maximum number of loops is exceeded, the loop is jumped out.  
      end_of_file = .true.  
      close(10)  
      du = 1  
      exit  
    endif  
    read(10, '(A)', IOSTAT=io_stat) line  
    read(line, '(I7,A21,I8)') firstnum, kt1, lastnum  
  
    !Checking reaction paths
    !if (io_stat >= 0 .AND. lastnum == 2 .OR. lastnum == 3) then  
    if (io_stat >= 0 .AND. lastnum == 4) then  
      call execute_command_line('./'//trim(input)//'-inpark/scancel.x')  
      call execute_command_line('rm -r ./'//trim(input)//'-inpark/0001/*.log')  
      call execute_command_line('rm -r ./'//trim(input)//'-inpark/0001/*.out')  
      du = 0  
      exit  
    !elseif (io_stat >= 0 .AND. lastnum /= 2 .AND. lastnum /= 3) then
    elseif (io_stat >= 0 .AND. lastnum /= 4) then
      if (io_stat > 0 .AND. firstnum < traj) then  
        end_of_file = .true.  
        close(10)  
        call execute_command_line('sleep 5s')  
        goto 8  
      elseif (io_stat > 0 .AND. firstnum == traj) then  
        end_of_file = .true.  
        close(10)  
        du = 1  
        exit  
      endif  
    endif  
  end do  
  
  !close the input file  
  close(10)  
end subroutine testbmax
