program main
!
! proc_satellite
!
! Author: T.Wakamatsu (tsuyoshi.wakamatsu@nersc.no)
!  
! Usage:
!
! History:
!
!   [2018.11.01] started
!   [2020.08.25] Chl-a data > 70N and > September are masked
!
  use            :: netcdf
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

  integer :: yr, mo, da, julda
  character(len=8) :: date_assim = 'YYYYMMDD'

  real(kind=8) :: lon, lat, dep
  real(kind=8) :: chl    , nit    , sil    , pho    , oxy
  real(kind=8) :: chl_dat, nit_dat, sil_dat, pho_dat, oxy_dat
  real(kind=8) :: chl_err, nit_err, sil_err, pho_err, oxy_err
  real(kind=8) :: chl_std, nit_std, sil_std, pho_std, oxy_std
  real(kind=8) :: temp, saln

  !-- define range of observed value which is defined in:
  !     m_Generate_element_Si.F90
  !     m_obs.F90

  real(kind=8), parameter :: CHL_MIN =   0.0d0  ! [mg Chl/m3]
  real(kind=8), parameter :: CHL_MAX =  40.0d0  ! [mg Chl/m3]
  real(kind=8), parameter :: NIT_MIN =   0.0d0  ! [mmol N/m3]
  real(kind=8), parameter :: NIT_MAX =  50.0d0  ! [mmol N/m3]
  real(kind=8), parameter :: SIL_MIN =   0.0d0  ! [mmol S/m3]
  real(kind=8), parameter :: SIL_MAX =  50.0d0  ! [mmol S/m3]
  real(kind=8), parameter :: PHO_MIN =   0.0d0  ! [mmol P/m3]
  real(kind=8), parameter :: PHO_MAX =  10.0d0  ! [mmol P/m3]
  real(kind=8), parameter :: OXY_MIN =   0.0d0  ! [ml    /l ]
  real(kind=8), parameter :: OXY_MAX =  30.0d0  ! [ml    /l ]

  !------------------------------------
  ! data-type specific variables
  !------------------------------------
  
  character (len=255) :: wdir = '.'
  character (len=255) :: type = 'satellite'

  !-- unit conversion factors

  real(kind=8), parameter :: C_N   = 6.625 ! Redfield ratio [mol_C/mol_N]  
  real(kind=8), parameter :: C_Si  = 6.625 ! Redfield ratio [mol_C/mol_Si] 
  real(kind=8), parameter :: C_P   = 106.0 ! Redfield ratio [mol_C/mol_P]  
  real(kind=8), parameter :: C_Cmg = 12.01 ! Transfer unit  [mg_C/mmol_C]  

                                                        !DATA         !ECOSMO
  real(kind=8), parameter :: Cfac = 1.0         ! Chl-a [mg Chl/m3] > [mg Chl/m3]
  real(kind=8), parameter :: Nfac = C_Cmg*C_N   ! NO3   [mmol N/m3] > [mg C/m3]
  real(kind=8), parameter :: Sfac = C_Cmg*C_Si  ! SiO2  [mmol S/m3] > [mg C/m3]
  real(kind=8), parameter :: Pfac = C_Cmg*C_P   ! PO4   [mmol P/m3] > [mg C/m3]
  real(kind=8), parameter :: Ofac = 1.0         ! O2    [ml    /l ] > [ml  /l ]

  !-- limit domain to read data, otherwise too slow

  real(kind=8), parameter :: latmin =  35.0d0  &
                          & ,latmax =  87.75d0 &
                          & ,lonmin =-179.75d0 &
                          & ,lonmax = 179.75d0
  
  !--

  integer :: ncstat, ncid, ncvarid
  integer, dimension(NF90_MAX_VAR_DIMS) :: dimids
  real(kind=8) :: fillval, fillerr, dataval, dataerr
  character (len=255) :: ncfile
  character (len=20) ::  namVar, namErr, namLons, namLats

  integer :: numLats, numLons, ilat, ilon
  integer :: ilatmin, ilatmax, ilonmin, ilonmax
  real(kind=8), dimension(:,:), allocatable :: varval, varerr
  real(kind=8), dimension(:),   allocatable :: varlat, varlon

  !------------------------------------
  ! data processing starts here
  !------------------------------------

  !-- assign NaN to nan
  if ( lprntnt ) write(*,*) '!-- assign NaN to nan'

  nan = IEEE_VALUE(1.d0,IEEE_QUIET_NAN)
  if ( lprntnt ) print *, 'nan=',nan

  !-- read command line arguments
  call printmsg(lprntnt,'!-- read command line arguments')

  if ( iargc().ne.4 ) then
    write ( *,'(a)',advance='yes' ) &
     & 'Usage: ./proc_satellite percentage[%] julda[days since 19500100] date_assim[yyyymmdd] data_mode[new/old]'
    stop 'Error reading command line arguments'
  endif

  call getarg(1,tmpchar)      !-- read error percentage [%:0-99]
  call printmsg(lprntnt,tmpchar)
  read(tmpchar,'(i2)') inp

  call getarg(2,tmpchar)      !-- read julian date since 19500101
  call printmsg(lprntnt,tmpchar)
  read(tmpchar,'(i5)') julda

  call getarg(3,tmpchar)      !-- read analysis date ['yyyymmdd']
  call printmsg(lprntnt,tmpchar)
  read(tmpchar,'(a8)') date_assim

  call getarg(4,tmpchar)      !-- renew data file ['new'] or not ['old']
  call printmsg(lprntnt,tmpchar)
  read(tmpchar,'(a3)') data_mode

  !-- define signal error percentage: 0-99
  call printmsg(lprntnt,'!-- define signal error percentage: 0-99')

  per = 0.01*inp              ! error percentage [fraction:0-1]
  write(*,'(x,a17,i2,a1)') 'error varience = ',inp,'%'

  !-- set analysis date (a single day)
  if ( lprntnt ) write(*,*) '!-- set analysis date'

  read( date_assim(1:4), '(i4.4)' ) yr
  read( date_assim(5:6), '(i2.2)' ) mo
  read( date_assim(7:8), '(i2.2)' ) da
 
  write(*,'(x,a)') 'analysis time: '//TRIM(date_assim)

  !-- define I/O file names
  if ( lprntnt ) write(*,*) '!-- define I/O file names'

  ncfile='chl_'//TRIM(date_assim)//'.nc'

  if ( lprntnt ) write(*,*) 'ncfile :',TRIM(ncfile)
  
  fn_bgcin  = TRIM(wdir)//'/satellite/'//TRIM(ncfile)
  fn_bgcout = TRIM(wdir)//'/TMP/bgc_satellite.txt'
  fn_chlout = TRIM(wdir)//'/TMP/chl_satellite.txt'

  if ( lprntnt ) write(*,*) 'fn_bgcin :',TRIM(fn_bgcin)
  if ( lprntnt ) write(*,*) 'fn_bgcout:',TRIM(fn_bgcout)
  if ( lprntnt ) write(*,*) 'fn_chlout:',TRIM(fn_chlout)

  !-- define data mode
  if ( lprntnt ) write(*,*) '!-- define data mode'

  if (data_mode.eq.'new') then
    lrenewdat = .true.
  else if (data_mode.eq.'old') then
    lrenewdat = .false.
  else
    print *,'input:'//data_mode//' is not supported, STOP'
    stop
  endif

  if ( lprntnt ) write(*,*) 'data_mode:',TRIM(data_mode)

  !-- check input file
  if ( lprntnt ) write(*,*) '!-- check input file'

  call chk_fexist(fn_bgcin,exist,fsize); call handle_err_exist(fn_bgcin,exist)

  !-- initialize output files

  if ( lrenewdat ) then
    if ( lprntnt ) write(*,*) '!-- initialize output files'
    call SYSTEM('rm -f '//TRIM(fn_bgcout)//'; touch '//TRIM(fn_bgcout))
    call SYSTEM('rm -f '//TRIM(fn_chlout)//'; touch '//TRIM(fn_chlout))
  else
    if ( lprntnt ) write(*,*) '!-- append to existing output files'
    call chk_fexist(fn_bgcout,exist,fsize); call handle_err_exist(fn_bgcout,exist)
    call chk_fexist(fn_chlout,exist,fsize); call handle_err_exist(fn_chlout,exist)
  endif

  !-- define netCDF variable names
  if ( lprntnt ) write(*,*) '!-- define netCDF file/variable names'

#if defined OCCCI
  write(*,*) 'process OCCCI satellte ocean color data'

  namVar  = 'chlor_a'
  namErr  = 'chlor_a_log10_rmsd'
  namLons = 'lon'
  namLats = 'lat'
#elif defined OCTAC
  write(*,*) 'process OC_TAC satellte ocean color data'

  namVar  = 'CHL'
  namErr  = 'CHL_error'
  namLons = 'longitude'
  namLats = 'latitude'
#elif defined GLBCLR
  write(*,*) 'process GlobColour satellte ocean color data'

  namVar  = 'CHL1_mean'
  namErr  = 'CHL1_error'
  namLons = 'lon'
  namLats = 'lat'
#endif

  !-- read dimension of data first
  if ( lprntnt ) write(*,*) '!-- read dimension of data first'

  call nf_check( NF90_OPEN( fn_bgcin, nf90_nowrite, ncid ) )

  call nf_check( NF90_INQ_VARID( ncid, namVar, ncvarid ) )
  call nf_check( NF90_INQUIRE_VARIABLE( ncid, ncvarid, dimids = dimids ) )
  call nf_check( NF90_INQUIRE_DIMENSION( ncid, dimids(1), len = numLons) )
  call nf_check( NF90_INQUIRE_DIMENSION( ncid, dimids(2), len = numLats) )

  call nf_check( NF90_CLOSE( ncid ) )

  write(*,*) 'numLons=',numLons
  write(*,*) 'numLats=',numLats

  !-- allocate working arrays
  if ( lprntnt ) write(*,*) '!-- allocate working arrays'

  allocate(varval(numLons,numLats))
  allocate(varerr(numLons,numLats))
  allocate(varlon(numLons))
  allocate(varlat(numLats))

  write(*,*) 'SIZE(varval):',SIZE(varval)
  write(*,*) 'SIZE(varerr):',SIZE(varerr)
  write(*,*) 'SIZE(varlon):',SIZE(varlon)
  write(*,*) 'SIZE(varlat):',SIZE(varlat)

  !-- read coordination next
  if ( lprntnt ) write(*,*) '!-- read coordination next'

  call nf_check( NF90_OPEN( fn_bgcin, nf90_nowrite,ncid ) )

  call nf_check( NF90_INQ_VARID( ncid, namLats, ncvarid ) )
  call nf_check( NF90_GET_VAR( ncid, ncvarid, varlat) )

  call nf_check( NF90_INQ_VARID( ncid, namLons, ncvarid ) )
  call nf_check( NF90_GET_VAR( ncid, ncvarid, varlon) )

  call nf_check( NF90_CLOSE( ncid ) )

  !-- define analysis box on the coordinate indecies
  if ( lprntnt ) write(*,*) '!-- define analysis box on the coordinate indecies'
  !
  ! Note: The following index search works only for the case:
  ! 
  !         Latitude: 90N to 90S (-90N)
  !         Longitude: 180W (-180E) to 180E
  !
  !       e.g.) GlobColour and OC-CCI       
  !

  write(*,*) 'varLat(1)        ', SNGL(varLat(1))
  write(*,*) 'varLat(2)        ', SNGL(varLat(2))
  write(*,*) 'varLat(numLats-1)', SNGL(varLat(numLats-1))
  write(*,*) 'varLat(numLats)  ', SNGL(varLat(numLats))

  write(*,*) 'varLon(1)        ', SNGL(varLon(1))
  write(*,*) 'varLon(2)        ', SNGL(varLon(2))
  write(*,*) 'varLon(numLons-1)', SNGL(varLon(numLons-1))
  write(*,*) 'varLon(numLons)  ', SNGL(varLon(numLons))

  ilatmin = MINLOC( ABS(varLat-latmax), 1 ) ! works for OC-CCI and GlobColour 
  ilatmax = MINLOC( ABS(varLat-latmin), 1 ) ! works for OC-CCI and GlobColour 
  ilonmin = MINLOC( ABS(varLon-lonmin), 1 )
  ilonmax = MINLOC( ABS(varLon-lonmax), 1 )

  write(*,*) '(ilatmin,ilatmax)=',ilatmin, ilatmax, sngl(varLat(ilatmin)), sngl(varLat(ilatmax))
  write(*,*) '(ilonmin,ilonmax)=',ilonmin, ilonmax, sngl(varLon(ilonmin)), sngl(varLon(ilonmax))

  !-- now read Chl-a value and error
  if ( lprntnt ) write(*,*) '!-- now read Chl-a value and error'
  if ( lprntnt ) write(*,*) 'read... '//TRIM(ncfile)

  call nf_check( NF90_OPEN( fn_bgcin, nf90_nowrite, ncid ) )

  call nf_check( NF90_INQ_VARID( ncid, namVar, ncvarid ) )
  call nf_check( NF90_GET_ATT( ncid, ncvarid, '_FillValue', fillval ) )
  call nf_check( NF90_GET_VAR( ncid, ncvarid, varval, start=(/1,1,1/), count=(/numLons,numLats,1/) ) )

  call nf_check( NF90_INQ_VARID( ncid, namErr, ncvarid ) )
  call nf_check( NF90_GET_ATT( ncid, ncvarid, '_FillValue', fillerr ) )
  call nf_check( NF90_GET_VAR( ncid, ncvarid, varerr, start=(/1,1,1/), count=(/numLons,numLats,1/) ) )

  call nf_check( NF90_CLOSE( ncid ) )

  write(*,*) '_FillValue=',fillval

  !-- create observation vector (y) file
  if ( lprntnt ) write(*,*) '!-- create observation vector (y) file'
  ! 
  ! 1. All satelitte nutrients data are assumed to be 'NaN' here.
  ! 2. Satellite Chl-a data measurement error is assumed to be 50% of signal.
  !
  ! [24.01.2018] T.Wakamatsu
  !   Assignment of 'NaN' value to satelitte Chl-a data is somewhat ad-hoc.
  !   Should come up with more elegant method. > done! [26.01.2018]
  !

  open(22,file=TRIM(fn_bgcout),status='old',action='write',position='append')
  open(23,file=TRIM(fn_chlout),status='old',action='write',position='append')  

  nobs = 0

  do ilat = ilatmin, ilatmax
  do ilon = ilonmin, ilonmax
    dataval = varval(ilon,ilat) ! signal
    dataerr = varerr(ilon,ilat) ! error (not used yet)

    if (dataval == fillval .or. dataerr == fillerr) cycle ! only physical data are archived

    chl = dataval
    nit = nan      ! dummy
    sil = nan      ! dummy
    pho = nan      ! dummy
    oxy = nan      ! dummy 

    lon = varLon(ilon)
    lat = varLat(ilat)
    dep = 0.d0         ! satellite data depth is assumed to be 0.0m

    !-- eliminate 'out of range' signal !TW require further sophistication [2018.01.26]
  
    if (chl < CHL_MIN .or. chl > CHL_MAX) chl = nan
    if (nit < NIT_MIN .or. nit > NIT_MAX) nit = nan
    if (sil < SIL_MIN .or. sil > SIL_MAX) sil = nan
    if (pho < PHO_MIN .or. pho > PHO_MAX) pho = nan
    if (oxy < OXY_MIN .or. oxy > OXY_MAX) oxy = nan

    !-- mask Fall bloom signal in high Arctic Ocean [2020.08.25]

    if (mo >= 9 .and. lat >= 70.0) then
      chl = nan
    end if

    !-- unit conversions for ECOSMO

    chl_dat = chl*Cfac ! Chl-a: Chlorophyll-a     [mg Chl/m3]
    nit_dat = nit*Nfac ! NO3  : Nitrate           [umol N/m3]
    sil_dat = sil*Sfac ! SiO2 : Silicate          [umol S/m3]
    pho_dat = pho*Pfac ! PO4  : Phosphate         [umol P/m3]
    oxy_dat = oxy*Ofac ! O2   : Dissolved Oxygen  [ml    /l ]
    
    !-- at least one variable should have finite value to be archived

    nan_count = 0
    if (ISNAN(chl_dat)) nan_count = nan_count + 1
    if (ISNAN(nit_dat)) nan_count = nan_count + 1
    if (ISNAN(sil_dat)) nan_count = nan_count + 1
    if (ISNAN(pho_dat)) nan_count = nan_count + 1
    if (ISNAN(oxy_dat)) nan_count = nan_count + 1

    if (nan_count == 5) cycle

    nobs = nobs + 1

    !-- define error variance !TW require further sophistication [2018.11.05]

    chl_std = per*chl_dat ! satellite Chl-a data error standard deviation
    chl_err = chl_std**2  ! satellite Chl-a data error variance

    !-- save data

    write(22,6) yr,mo,da,lon,lat,dep,chl_dat,nit_dat,sil_dat,pho_dat,oxy_dat
    write(23,7) julda   ,lon,lat,dep,chl_dat,chl_err

  enddo
  enddo

  print *, 'Number of available data: ', nobs

  close(22)
  close(23)

6 format(i4,X,i2.2,X,i2.2,X,f15.6,X,f15.6,X,f8.2,X,f15.10,X,f15.10,X,f15.10,X,f15.10,X,f15.10)
7 format(i5.5,X,f15.6,X,f15.6,X,f8.2,X,f15.9,X,f20.10)
8 format(i5.5,X,f15.6,X,f15.6,X,f8.2,X,f15.10,X,f20.10,X,f20.10)

  contains

    !-- utilities

    subroutine nf_check(status)
      integer, intent(in) :: status

      if(status /= nf90_noerr) then
        write(*,*) TRIM(NF90_STRERROR(status))
        stop 2
      endif
    end subroutine nf_check

    subroutine printmsg(lprntnt,string)
      implicit none
      logical         , intent(in) :: lprntnt
      character(len=*), intent(in) :: string

      if ( lprntnt ) write(*,*) TRIM(string)

    end subroutine printmsg

end program main
