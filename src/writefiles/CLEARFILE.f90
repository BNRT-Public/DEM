    SUBROUTINE CLEARFILE
    
    !character(len=256):: input_file, output_file
    !integer :: stat,ppos
    !character(len=3)  :: new_ext="mac"
    !!
    !input_file = 'ANSYS_T'
    !ppos = scan(trim(input_file),".", BACK= .true.)
    !if ( ppos > 0 ) output_file = input_file(1:ppos)//new_ext
    !!
    !WRITE(*,*) output_file
    !
    !open(unit=9999, iostat=stat, file='ACELERATION_T01.dat', status='old')
    !if (stat == 0) close(9999, status='delete')

    
!!! FUNCIONA CON ESTRUCTURA    
    implicit none
  
  character(len=255) :: buffer
  logical :: exist
  integer :: ind
  
  ind = 1
  fileloop: do
    write(buffer,"(a,i8.8,a)") "ANSYS_T01_", ind, ".mac"
    inquire(file=buffer, exist=exist)
    if (.not. exist) then
      write(*,*) "file '", trim(buffer), "' not found, exiting loop"
      exit
    end if
    write(*,*) "file: '", trim(buffer), "' found."
    ind = ind + 1
  end do fileloop
    
    !!!

  
    RETURN
    END