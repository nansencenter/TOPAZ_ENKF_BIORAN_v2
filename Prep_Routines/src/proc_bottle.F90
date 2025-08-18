program main
!
! proc_bottle
!
! Author: T.Wakamatsu (tsuyoshi.wakamatsu@nersc.no)
!  
! Usage:
!
! History:
!
!   [2019.09.27] remove NaN data from indivisual BGC data (eg. nit_bottle.txt)
!   [2019.09.24] insitu observation date [yr_in,mo_in,da_in] is replaced by analysis date [yo,mo,da]
!                insitu nutrient data unit [umol/l] is converted ECOSMO unit [mmol/l]
!   [2018.11.01] started
!
  use, intrinsic :: IEEE_ARITHMETIC
  implicit none

  !------------------------------------
  ! common variables (to be placed in a module in future)
  !------------------------------------

  real(kind=8) :: nan
  integer :: nan_count, nobs

  integer :: fsize
  logical :: exist, lprntnt = .true.

  integer :: inp
  real(kind=8) :: per
  character (len=255) :: tmpchar

  logical :: lrenewdat
  character (len=3) :: data_mode ! 'old' or 'new'

  character (len=255) :: fn_bgcin, fn_bgcout
  character (len=255) :: fn_chlout, fn_nitout, fn_silout, fn_phoout, fn_oxyout

  integer :: yr_in, mo_in, da_in, hr_in  ! real date of observation
  integer :: yr, mo, da, hr, julda       ! date of analysis
  character(len=8) :: date_assim = 'YYYYMMDD'

  character(len=15) :: expc             ! EXPO code
  character(len=15) :: prov             ! Provider name e.g. CMEMS
  real(kind=8) :: lon, lat, dep, depth
  real(kind=8) :: chl    , nit   ,  sil    , pho    , oxy
  real(kind=8) :: chl_dat, nit_dat, sil_dat, pho_dat, oxy_dat
  real(kind=8) :: chl_var, nit_var, sil_var, pho_var, oxy_var
  real(kind=8) :: temp, saln

  !-- define range of observed value which is defined in:
  !     m_Generate_element_Si.F90
  !     m_obs.F90

  real(kind=8), parameter :: CHL_MIN =   0.0d0  ! [mg Chl/m3]
  real(kind=8), parameter :: CHL_MAX =  40.0d0  ! [mg Chl/m3]
  real(kind=8), parameter :: NIT_MIN =   0.0d0  ! [mmol N/m3]
  real(kind=8), parameter :: NIT_MAX =  40.0d0  ! [mmol N/m3]
  real(kind=8), parameter :: SIL_MIN =   0.0d0  ! [mmol S/m3]
  real(kind=8), parameter :: SIL_MAX =  40.0d0  ! [mmol S/m3]
  real(kind=8), parameter :: PHO_MIN =   0.0d0  ! [mmol P/m3]
  real(kind=8), parameter :: PHO_MAX =  10.0d0  ! [mmol P/m3]
  real(kind=8), parameter :: OXY_MIN =   0.0d0  ! [ml    /l ]
  real(kind=8), parameter :: OXY_MAX =  30.0d0  ! [ml    /l ]

  !------------------------------------
  ! data-type specific variables
  !------------------------------------
  
  character (len=255) :: wdir = '.'
  character (len=255) :: type = 'bottle'

  !-- unit conversion factors

  real(kind=8), parameter :: C_N   = 6.625 ! Redfield ratio [mol_C/mol_N]  
  real(kind=8), parameter :: C_Si  = 6.625 ! Redfield ratio [mol_C/mol_Si] 
  real(kind=8), parameter :: C_P   = 106.0 ! Redfield ratio [mol_C/mol_P]  
  real(kind=8), parameter :: C_Cmg = 12.01 ! Transfer unit  [mg_C/mmol_C]  

                                                        !DATA         !ECOSMO
  real(kind=8), parameter :: Cfac = 1.0         ! Chl-a [mg Chl/m3] > [mg C/m3]
  real(kind=8), parameter :: Nfac = C_Cmg*C_N   ! NO3   [mmol N/m3] > [mg C/m3]
  real(kind=8), parameter :: Sfac = C_Cmg*C_Si  ! SiO2  [mmol S/m3] > [mg C/m3]
  real(kind=8), parameter :: Pfac = C_Cmg*C_P   ! PO4   [mmol P/m3] > [mg C/m3]
  real(kind=8), parameter :: Ofac = 1.0         ! O2    [ml    /l ] > [ml  /l ]

  !-- limit domain to read data, otherwise too slow

  real(kind=8), parameter :: latmin =  40.0d0 &
                         &,  latmax =  87.75d0 &
                         &,  lonmin =-179.75d0 &
                         &,  lonmax = 179.75d0
  
  !--

  integer :: il, nline, ierr
  character(len=8) :: date_dat = 'YYYYMMDD'
  character(len=255) :: command

  !------------------------------------
  ! data processing starts here
  !------------------------------------

  !-- assign NaN to nan
  if ( lprntnt ) write(*,*) '!-- assign NaN to nan'

  !nan = IEEE_VALUE(1.d0,IEEE_QUIET_NAN)
  nan = IEEE_VALUE(nan,IEEE_QUIET_NAN)
  if ( lprntnt ) print *, 'nan=',nan

  !-- read command line arguments
  if ( lprntnt ) write(*,*) '!-- read command line arguments'

  if ( iargc().ne.3 ) then
    write ( *,'(a)',advance='yes' ) &
     & 'Usage: ./proc_bottle percentage[%] date_assim[yyyymmdd] data_mode[new/old]'
    stop 'Error reading command line arguments'
  endif

  call getarg(1,tmpchar)      !-- read error percentage [0-99]
  call printmsg(lprntnt,tmpchar)
  read(tmpchar,'(i2)') inp

  call getarg(2,tmpchar)      !-- read gregorian date ['yyyymmdd']
  call printmsg(lprntnt,tmpchar)
  read(tmpchar,'(a8)') date_assim

  call getarg(3,tmpchar)      !-- renew data file ['new'] or not ['old']
  call printmsg(lprntnt,tmpchar)
  read(tmpchar,'(a3)') data_mode

  !-- define signal error percentage: 0-99
  if ( lprntnt ) write(*,*) '!-- define signal error percentage: 0-99'

  per = 0.01*inp            !-- [percentage:0-99] > [fraction:0-1]
  write(*,'(x,a17,i2,a1)') 'error varience = ',inp,'%'

  !-- set analysis date (a single day)
  if ( lprntnt ) write(*,*) '!-- set analysis date'

  read( date_assim(1:4), '(i4.4)' ) yr
  read( date_assim(5:6), '(i2.2)' ) mo
  read( date_assim(7:8), '(i2.2)' ) da
  hr = 12

  write(*,'(x,a)') 'analysis time: '//TRIM(date_assim)

  !-- define I/O file names
  if ( lprntnt ) write(*,*) '!-- define I/O file names'

  fn_bgcin=TRIM(wdir)//'/'//TRIM(type)//'/bgc_'//TRIM(date_assim)//'.txt'

  fn_bgcout = TRIM(wdir)//'/TMP/bgc_bottle.txt'
  fn_chlout = TRIM(wdir)//'/TMP/chl_bottle.txt'
  fn_nitout = TRIM(wdir)//'/TMP/nit_bottle.txt'
  fn_silout = TRIM(wdir)//'/TMP/sil_bottle.txt'
  fn_phoout = TRIM(wdir)//'/TMP/pho_bottle.txt'
  fn_oxyout = TRIM(wdir)//'/TMP/oxy_bottle.txt'

  !-- defined data mode
  if ( lprntnt ) write(*,*) '!-- defined data mode'

  if (data_mode.eq.'new') then
    lrenewdat = .true.
  else if (data_mode.eq.'old') then
    lrenewdat = .false.
  else
    print *,'input:'//data_mode//' is not supported, STOP'
    stop
  endif

  !-- check input file
  if ( lprntnt ) write(*,*) '!-- check input files'

  call chk_fexist(fn_bgcin,exist,fsize); call handle_err_exist(fn_bgcin,exist)

  !-- initialize output files

  if ( lrenewdat ) then
    if ( lprntnt ) write(*,*) '!-- initialize output files'
    call SYSTEM('rm -f '//TRIM(fn_bgcout)//'; touch '//TRIM(fn_bgcout))
    call SYSTEM('rm -f '//TRIM(fn_chlout)//'; touch '//TRIM(fn_chlout))
    call SYSTEM('rm -f '//TRIM(fn_nitout)//'; touch '//TRIM(fn_nitout))
    call SYSTEM('rm -f '//TRIM(fn_silout)//'; touch '//TRIM(fn_silout))
    call SYSTEM('rm -f '//TRIM(fn_phoout)//'; touch '//TRIM(fn_phoout))
    call SYSTEM('rm -f '//TRIM(fn_oxyout)//'; touch '//TRIM(fn_oxyout))
  else
    if ( lprntnt ) write(*,*) '!-- append to existing output files'
    call chk_fexist(fn_bgcout,exist,fsize); call handle_err_exist(fn_bgcout,exist)
    call chk_fexist(fn_chlout,exist,fsize); call handle_err_exist(fn_chlout,exist)
    call chk_fexist(fn_nitout,exist,fsize); call handle_err_exist(fn_nitout,exist)
    call chk_fexist(fn_silout,exist,fsize); call handle_err_exist(fn_silout,exist)
    call chk_fexist(fn_phoout,exist,fsize); call handle_err_exist(fn_phoout,exist)
    call chk_fexist(fn_oxyout,exist,fsize); call handle_err_exist(fn_oxyout,exist)
  endif

  !-- now read nutrient value and clean it up
  if ( lprntnt ) write(*,*) '!-- now read nutrient value and clean it up'

  call Get_Line_Numbers(9000,fn_bgcin,nline)
  print *, 'Number of lines in '//TRIM(fn_bgcin)//' file: ', nline

  open(22,file=TRIM(fn_bgcout),status='old',action='write',position='append') 
  open(23,file=TRIM(fn_chlout),status='old',action='write',position='append') 
  open(24,file=TRIM(fn_nitout),status='old',action='write',position='append') 
  open(25,file=TRIM(fn_silout),status='old',action='write',position='append') 
  open(26,file=TRIM(fn_phoout),status='old',action='write',position='append') 
  open(27,file=TRIM(fn_oxyout),status='old',action='write',position='append') 

  open(10,file=TRIM(fn_bgcin),status='old',action='read')

  read(10,*) ! skip header line 1 : variable name
  read(10,*) ! skip header line 2 : variable unit

  nobs = 0

  do il = 3, nline
    read(10,*,iostat=ierr) prov,expc,yr_in,mo_in,da_in,hr_in,lon,lat,depth,temp,saln,oxy,pho,nit,sil,chl
    !write(*,*) lon,lat,depth,chl
    if (ierr .ne. 0) then
      write(*,*) 'IO Error at line:',il+2,'iostat=',ierr
      stop
    endif

    if (lon.ge.lonmin .and. lon.le.lonmax) then
    if (lat.ge.latmin .and. lat.le.latmax) then

    dep=-1.0*depth ! invert depth axis

    !chl = nan ! force in-situ Chl-a to be NaN

    !-- eliminate 'out of range' signal !TW require further sophistication [2018.01.26]
  
    if (chl < CHL_MIN .or. chl > CHL_MAX) chl = nan
    if (nit < NIT_MIN .or. nit > NIT_MAX) nit = nan
    if (sil < SIL_MIN .or. sil > SIL_MAX) sil = nan
    if (pho < PHO_MIN .or. pho > PHO_MAX) pho = nan
    if (oxy < OXY_MIN .or. oxy > OXY_MAX) oxy = nan

    !-- unit conversions for ECOSMO

    chl_dat = chl*Cfac ! Chl-a: Chlorophyll-a     [mg Chl/m3] > [mg C/m3] 
    nit_dat = nit*Nfac ! NO3  : Nitrate           [umol N/m3] > [mg C/m3] 
    sil_dat = sil*Sfac ! SiO2 : Silicate          [umol N/m3] > [mg C/m3] 
    pho_dat = pho*Pfac ! PO4  : Phosphate         [umol N/m3] > [mg C/m3] 
    oxy_dat = oxy*Ofac ! O2   : Dissolved Oxygen  [ml    /l ] > [ml  /l ]

    !-- at least one variable should have finite value to be archived

    nan_count = 0
    if (IEEE_IS_NAN(chl_dat)) nan_count = nan_count + 1
    if (IEEE_IS_NAN(nit_dat)) nan_count = nan_count + 1
    if (IEEE_IS_NAN(sil_dat)) nan_count = nan_count + 1
    if (IEEE_IS_NAN(pho_dat)) nan_count = nan_count + 1
    if (IEEE_IS_NAN(oxy_dat)) nan_count = nan_count + 1

    if (nan_count == 5) cycle

    nobs = nobs + 1

    !-- define error variance !TW require further sophistication in future [2018.11.05]

    chl_var=(per*chl_dat)**2 ! bogus in-situ Chl-a error variance
    nit_var=(per*nit_dat)**2 ! in-situ Nitrate error variance
    sil_var=(per*sil_dat)**2 ! in-situ Silicate error variance
    pho_var=(per*pho_dat)**2 ! in-situ Phosphate error variance
    oxy_var=(per*oxy_dat)**2 ! in-situ Oxygen error variance

    !-- save data

    write(22,6) yr,mo,da,hr,lon,lat,dep,chl_dat,nit_dat,sil_dat,pho_dat,oxy_dat
    if ( IEEE_IS_FINITE(chl_dat) ) write(23,7) lon,lat,dep,chl_dat,chl_var
    if ( IEEE_IS_FINITE(nit_dat) ) write(24,7) lon,lat,dep,nit_dat,nit_var
    if ( IEEE_IS_FINITE(sil_dat) ) write(25,7) lon,lat,dep,sil_dat,sil_var
    if ( IEEE_IS_FINITE(pho_dat) ) write(26,7) lon,lat,dep,pho_dat,pho_var
    if ( IEEE_IS_FINITE(oxy_dat) ) write(27,7) lon,lat,dep,oxy_dat,oxy_var

    endif
    endif

  enddo

  print *, 'Number of available data: ', nobs

  close(10)

  close(22)
  close(23)
  close(24)
  close(25)
  close(26)
  close(27)

6 format(i4,X,i2.2,X,i2.2,X,i2.2,X,f15.6,X,f15.6,X,f8.2,X,f15.10,X,f15.10,X,f15.10,X,f15.10,X,f15.10)
7 format(f15.6,2X,f15.6,X,f8.2,2X,f15.9,X,f20.10)

  contains

    !-- utilities

    subroutine printmsg(lprntnt,string)
      implicit none
      logical         , intent(in) :: lprntnt
      character(len=*), intent(in) :: string

      if ( lprntnt ) write(*,*) TRIM(string)
    end subroutine printmsg

end program main
