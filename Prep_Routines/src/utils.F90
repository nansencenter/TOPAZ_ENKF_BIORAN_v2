subroutine chk_fexist(fname,exist,fsize)

  implicit none
  character(len=255), intent(in) :: fname
  integer, intent(out) :: fsize
  logical, intent(out) :: exist

  INQUIRE( file=TRIM(fname), exist=exist, size=fsize )

end subroutine chk_fexist

subroutine handle_err_exist(fname,exist)

  implicit none
  character(len=255), intent(in) :: fname
  logical, intent(in) :: exist

  if ( .not. exist ) then
    write(*,'(a)') TRIM(fname)//' does not exist, STOP'
    stop
  end if

end subroutine handle_err_exist

subroutine handle_err_fsize(fname,fsize)

  implicit none
  character(len=255), intent(in) :: fname
  integer, intent(in) :: fsize

  if ( fsize .eq. 0 ) then
    write(*,'(a)') 'WARNING: '//TRIM(fname)//' is empty'
  end if

end subroutine handle_err_fsize

subroutine Get_Line_Numbers( no,filename,lines )
       
  implicit none
  integer :: ierr, il
  character(len=*), intent(in)    :: filename
  integer, intent(in)             :: no
  integer, intent(out)            :: lines
     
  il = 0
  OPEN ( unit=no,file=filename )
  do while( il.ge.0 )
    READ( no,*,iostat=ierr )
    if ( ierr.lt.0 ) exit
    il = il + 1
  enddo
  lines=il
  CLOSE( unit=no )

end subroutine Get_Line_Numbers
