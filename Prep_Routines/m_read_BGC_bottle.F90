! File:          m_read_BGC_bottle.F90 [tsuyoshi]
!                m_read_biology_data.F90 [moha?]
!                m_read_FFI_glider.F90 [source]
!
! Created:       October 2018
!
! Author:        Pavel Sakov 
!		 MoHa Gharamti (adapted to biology)
!                Tsuyoshi Wakamatsu (readapted to aggregated BGC data)
!                NERSC
!
! Purpose:       Read BGC bottle data from text files into TOPAZ system
!
! Description:   Data file(s) are defined by the string in the 4th line of
!                "infile.data". It should have the following format:
!                <BEGIN>
!                BGC
!                CHL | NIT | SIL | PHO | OXY
!                <File name header>
!                <File name>
!                <END>
!
! Note: Unlike m_read_FFI_glider.F90, data are read in the following order:
!
!                lon, lat, depth, data, (error variance)
!
! Modifications: none

module m_read_BGC_bottle
  implicit none

  integer, parameter, private :: STRLEN = 512
  public read_bgc_bottle

contains

  subroutine read_bgc_bottle(fname, obstype, nx, ny, data)
    use mod_measurement
    use m_get_mod_cnfg
    use m_confmap
    use m_oldtonew
    use m_pivotp
    use m_bilincoeff
    implicit none

    character(*), intent(in) :: fname
    character(*), intent(in) :: obstype
    integer, intent(in) :: nx, ny
    type(measurement), allocatable, intent(out) :: data(:)
    
    integer , dimension(nx, ny) :: mask
    real*8, dimension(nx, ny) :: modlat, modlon, depths
    real :: mindx, meandx
    real :: latnew, lonnew

    character(STRLEN) :: record
    integer :: ios
    integer :: r, nr, o, nobs, oo, i, j

    real :: tmp
    type(measurement) :: obs
    type(measurement), allocatable :: tmpdata(:)

    ! count number of records
    !
    open(10, file = trim(fname), access = 'sequential', status = 'old', iostat = ios)
    if (ios /= 0) then
       print *, 'ERROR: read_BGC_bottle(): could not open "', fname, '"'
    end if
    nr = 1
    do while(.true.)
       read(10, *, iostat = ios) record
       if (ios /= 0) then
          exit
       end if
       nr = nr + 1
    end do

    print *, trim(fname), ': ', nr, ' lines'
    if (nr == 0) then
       print *, 'ERROR: read_BGC_bottle(): "', fname, '": empty file?'
       stop
    end if

    allocate(data(nr))

    close(10)

    open(10, file = trim(fname), access = 'sequential', status = 'old')

    nobs = 0
    do r = 1, nr
      read(10, *, iostat = ios) obs%lon, obs%lat, obs%depth, obs%d, obs%var
      !write(*,*) SNGL(obs%lon),SNGL(obs%lat),SNGL(obs%depth),SNGL(obs%d),SNGL(obs%var)
      obs % date = 0 ! assimilate synchronously
      if (obs%date < 0) then
        cycle
      end if
      nobs = nobs + 1
      data(nobs) = obs
    end do

    close(10)

    allocate(tmpdata(1 : nobs))
    tmpdata = data(1 : nobs)
    deallocate(data)
    allocate(data(nobs))
    data = tmpdata
    deallocate(tmpdata)

    if (nobs == 0) then
       print *, 'ERROR: read_BGC_bottle(): "', trim(fname),&
            '": no meaningful data for ', trim(obstype), ' found'
       stop
    end if
    print *, trim(fname), ': ', nobs, ' records for ', trim(obstype)

    data % id = obstype
    data % status = .true.
    data % ns = 0
    data % i_orig_grid = 0

    call confmap_init(nx, ny)
    call get_mod_cnfg(modlon, modlat, depths, mindx, meandx, nx, ny, .false.)

    do o = 1, nobs
       call oldtonew(data(o)%lat, data(o)%lon, latnew, lonnew)
       call pivotp(lonnew, latnew, data(o)%ipiv, data(o)%jpiv)
       if (data(o)%ipiv < 1 .or. data(o)%jpiv < 1 .or. data(o)%ipiv > nx - 1 .or. data(o)%jpiv > ny - 1) then
          data(o) % status = .false.
       else
          call bilincoeff(modlon, modlat, nx, ny, &
                          data(o)%lon, data(o)%lat, data(o)%ipiv, data(o)%jpiv, &
                          data(o)%a1,  data(o)%a2,  data(o)%a3,   data(o)%a4)
          data(o) % status = .true.
       end if

    end do

    ! some basic QC

    !if (trim(obstype) == 'CHL') then
    !   where (data % d < CHL_MIN .or. data % d > CHL_MAX)
    !      data % status = .false.
    !   end where
    !elseif (trim(obstype) == 'NIT') then
    !   where (data % d < NIT_MIN .or. data % d > NIT_MAX)
    !      data % status = .false.
    !   end where
    !elseif (trim(obstype) == 'SIL') then
    !   where (data % d < SIL_MIN .or. data % d > SIL_MAX)
    !      data % status = .false.
    !   end where
    !elseif (trim(obstype) == 'PHO') then
    !   where (data % d < PHO_MIN .or. data % d > PHO_MAX)
    !      data % status = .false.
    !   end where
    !elseif (trim(obstype) == 'OXY') then
    !   where (data % d < OXY_MIN .or. data % d > OXY_MAX)
    !      data % status = .false.
    !   end where
    !end if

    allocate(tmpdata(1 : count(data % status)))
    oo = 0
    do o = 1, nobs
       if (data(o) % status) then
          oo = oo + 1
          tmpdata(oo) = data(o)
       end if
    end do
    nobs = oo

    deallocate(data)

    print *, trim(fname), ': ', nobs, ' records for ', trim(obstype), ' after simple QC'

    allocate(data(nobs))

    data = tmpdata

    deallocate(tmpdata)

  end subroutine read_bgc_bottle

end module m_read_BGC_bottle
