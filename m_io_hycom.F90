! File:        m_io_hycom.F90
!
! Created:     Novermber 2018
!
! Author:      T.Wakamatsu (tsuyoshi.wakamatsu@nersc.no)
!
! Purpose:     I/O module for hycom model 2D variable file (FORTRAN direct access)
!
! Description: This file is designed based on the hycom utilities:
!
!              hycom/MSCPROGS/src/Nersclib/mod_za.F                 ! original I/O modules for hycom file
!              hycom/MSCPROGS/src/Nersclib/mod_grid.F90             ! read regional.grid.a and regional.depth.a
!              hycom/MSCPROGS/src/Conf_grid/Codes/m_grid_to_hycom.F ! variable list
!
! History:     
!
module m_io_hycom
  
  implicit none
  integer, parameter :: STRLEN = 512
  logical, parameter, private :: VERBOSE = .false.

  integer, private :: ncall
  real(kind=4), allocatable, private :: w(:)

contains 

  subroutine open_hycom_var2d(nop,fname,idm,jdm,status)
  implicit none

  integer, intent(in) :: nop, idm, jdm
  character(len=STRLEN), intent(in) :: fname
  character(len=*), intent(in) :: status

  logical :: exa
  integer :: ios, nrecl
  !
  ! n2drec = size of output 2-d array, multiple of 4096
  !
  integer :: n2drec
  n2drec = ((idm*jdm+4095)/4096)*4096

  allocate( w(n2drec) )
  inquire(iolength=nrecl) w

  inquire(file=TRIM(fname),exist=exa)

  if ( exa ) then
    print *,'Open to read: ',TRIM(fname)
    open(nop,file=TRIM(fname),form='unformatted',status=TRIM(status),access='direct',recl=nrecl,action='read',iostat=ios)
  else
    print *,'Failed to open: ',TRIM(fname)
    stop
  endif

  ncall = 0

  end subroutine open_hycom_var2d
    
  subroutine read_hycom_var2d(nop,var2d,wmax,wmin,idm,jdm,iarec)
  implicit none

  integer, intent(in) :: nop, idm, jdm, iarec
  real(kind=4), dimension(idm,jdm), intent(out) :: var2d
  real(kind=4), intent(out) :: wmax, wmin

  integer :: ios, i, j
    !
    ! spval  = data void marker, 2^100 or about 1.2676506e30
    !
  real(kind=4), parameter :: spval=2.0**100

  wmin =  spval 
  wmax = -spval 

  read(nop,rec=iarec,iostat=ios) w

  if( ios .ne. 0 ) then
    print *,'ERROR, iostat = ',INT2(ios)
    stop
  endif
  
  ncall = ncall + 1

  do j= 1,jdm
    do i= 1,idm
      if (w(i+(j-1)*idm).ne.spval) then
        wmin = min( wmin, w(i+(j-1)*idm) )
        wmax = max( wmax, w(i+(j-1)*idm) )
      endif
      var2d(i,j) = w(i+(j-1)*idm)
    enddo
  enddo

  if(ncall == 1) then
    print *, '     (idm,jdm):',INT2(SHAPE(var2d))
  endif
#ifdef BIOGA_CHK
  print *, '  var2d(50,50):',SNGL(var2d(50,50))
  print *, '  varmax      :',SNGL(wmax)
  print *, '  varmin      :',SNGL(wmin)
#endif

  end subroutine read_hycom_var2d

  subroutine close_hycom_var2d(nop)
    implicit none
    integer, intent(in) :: nop
    logical :: openedq
    character(len=STRLEN) :: fname
    
    inquire(unit=nop, opened=openedq, name=fname)

    if (openedq) then
      print *, 'Close: ',TRIM(fname)
      close(nop)
    else
      print *, 'I/O unit: ',INT2(nop),'not connected'
      stop
    endif

    deallocate(w)
    ncall = 0

  end subroutine close_hycom_var2d

end module  m_io_hycom
