! File:          m_read_CMEMS_CORA.F90
!
! Created:       14 Mar 2025
!
! Author:        Tsuyoshi Wakamatsu
!
module m_read_CMEMS_CORA
  ! Reads CLS SLA data after having read the grid in read_CLS_SST_grid
  implicit none

  integer, parameter, private :: STRLEN =2180
  integer, parameter, private :: MLEV0 = 1000
  real, parameter, private :: SAL_MIN = 16.0     ! tuning back Jan 2024
  real, parameter, private :: SAL_MAX = 37.5
  real, parameter, private :: TEM_MIN = -2.0
  real, parameter, private :: TEM_MAX = 40.0
  real, parameter, private :: DENS_DIFF_MIN = -0.02
  logical, parameter, private :: DISCARD_CLOSE = .false.

  public read_CMEMS_CORA

  private data_inquire
  private data_readfile
  private potential_density
  private grid_readxyz
  private get_symlink_target
  
  contains

  function get_symlink_target(fname) result(target)
    character(len=*), intent(in) :: fname
    character(len=80) :: target, line
    integer :: iunit
    integer :: status
    logical :: file_exists

    target = ''
    inquire(file=trim(fname),exist=file_exists)
    if (.not. file_exists) then
       print *, 'ERROR: File does not exist, STOP'
       stop
    end if
    
    call system("readlink "//trim(fname)//" > output.txt")
    open(unit=iunit, file='output.txt', status='old')
    read(iunit,'(A)',iostat=status) target
    close(iunit)
    call system('rm output.txt')

    if (len_trim(target) == 0) then ! file is not symlink
       target = fname
    endif
  end function get_symlink_target
    
  subroutine read_CMEMS_CORA(fname, obstype, variance, nx, ny, data, datainfo)
  use mod_measurement
  use m_oldtonew
  use m_confmap
  use m_bilincoeff
  use m_pivotp
  use netcdf
  use m_nf90_err
   
  character(*), intent(in) :: fname
  character(*), intent(in) :: obstype
  real, intent(in) :: variance
  integer, intent(in) :: nx, ny
  type(measurement), allocatable, intent(out) :: data(:)
  type(measurement_info), allocatable, intent(out),optional :: datainfo(:)
  
  real(8) :: fillval = 99999.
  integer :: nprof, nlev
  character(len=32), allocatable, dimension(:) :: profname, unique_profname
  real(8), allocatable, dimension(:) :: time, lat, lon
  real(8), allocatable, dimension(:,:) :: pres, temp, psal
  integer(kind=1), allocatable, dimension(:) :: time_qc, pos_qc
  integer(kind=1), allocatable, dimension(:,:) :: pres_qc, temp_qc, psal_qc

  character(len=32) :: floatid
  integer, allocatable, dimension(:) :: mask1d
  integer, allocatable, dimension(:,:) :: mask2d
  real(8) :: rho_prev, rho, rho_inc
  
  integer, allocatable, dimension(:) :: ipiv, jpiv

  integer :: ngood, ndata
  real :: latnew, lonnew
  real(8), dimension(nx,ny) :: modlat, modlon
  real(8), dimension(nx,ny) :: depths

  integer :: p, l, i, j, unique_count, found

  print *, '-----------------------------------------------'
  print *, ' BEGIN read_CMEMS_CORA(): '//trim(get_symlink_target(fname))
  print *, '-----------------------------------------------'
  
  call data_inquire(fname, nprof, nlev)
  print *, '  data size: nprof =', nprof, ', nlev =', nlev

  ! limitation on the maximal levels 
  if (nlev>MLEV0) then
     nlev=MLEV0
     print *, '  nlev exceeds max level = ',MLEV0
     print *, '  data size: nprof =', nprof, ', nlev =', nlev
  endif

  allocate(profname(nprof))
  allocate(unique_profname(nprof))
  allocate(time(nprof))
  allocate(time_qc(nprof))
  allocate(lat(nprof))
  allocate(lon(nprof))
  allocate(pos_qc(nprof))
  allocate(pres(nlev,nprof))
  allocate(pres_qc(nlev,nprof))
  allocate(temp(nlev,nprof))
  allocate(temp_qc(nlev,nprof))
  allocate(psal(nlev,nprof))
  allocate(psal_qc(nlev,nprof))

  allocate(ipiv(nprof))
  allocate(jpiv(nprof))
  allocate(mask1d(nprof))
  allocate(mask2d(nlev,nprof))
    
  pres = fillval
  temp = fillval
  psal = fillval

  !-- read PRES,TEMP,PSAL all
  
  call data_readfile(fname,obstype,time,time_qc,lat,lon,pos_qc,pres,pres_qc,temp,temp_qc,psal,psal_qc,nprof,nlev,profname)

  !-- check unique float names

  unique_count = 0
  unique_profname = ""

  do i = 1, size(profname) ! Loop through the input array
    found = 0              ! Initialize found to 0 (false)  
    ! Check if the current element is already in the unique array
    do j = 1, unique_count
      if (trim(unique_profname(j)) == trim(profname(i))) then
        found = 1 ! Set found to 1 (true) if found
        exit ! Exit the inner loop since it's already found
      end if
    end do
    ! If it's not found, add it to the unique array
    if (found == 0) then
      unique_count = unique_count + 1
      unique_profname(unique_count) = profname(i)
    end if
  end do

  write(*,'(i3,x,a)') unique_count,"unique floats are found:"
  do i = 1, unique_count
    print *, '  ',trim(unique_profname(i))
  end do

  !-- mask < (time_qc)U(pos_qc)U(pivot_qc)U(pres_qc)U(temp_qc)U(psal_qc)U(dpth_qc)
  print *, '----------------'
  print *, ' operational QC '
  print *, '----------------'

  mask1d(:) = 1
  mask2d(:,:) = 1
  call grid_readxyz(nx, ny, modlat, modlon, depths)
  
  ! time_qc
  where (time_qc /= 1) mask1d = 0
  where (spread(mask1d, dim=1, ncopies=nlev) == 0) mask2d = 0
  write(*,'(2x,i4,x,a)') count(mask1d == 1), 'good profiles after time_qc'
  write(*,'(i6,x,a)') count(mask2d == 1), 'good obs after time_qc'

  ! pos_qc
  where (pos_qc /= 1) mask1d = 0
  where (spread(mask1d, dim=1, ncopies=nlev) == 0) mask2d = 0
  write(*,'(2x,i4,x,a)') count(mask1d == 1), 'good profiles after pos_qc'
  write(*,'(i6,x,a)') count(mask2d == 1), 'good obs after pos_qc'
 
  ! profname_qc (BLOCKLIST)
  do p = 1, nprof
     floatid = profname(p)
     if (floatid(1:3) == 'ITP') mask1d = 0 ! Too many convectively unstable layers ditected in ITP profiles
  enddo
  write(*,'(2x,i4,x,a)') count(mask1d == 1), 'good profiles after profname_qc'
  write(*,'(i6,x,a)') count(mask2d == 1), 'good obs after profname_qc'
    
  ! pivot_qc
  ipiv(:) = -999
  jpiv(:) = -999
  call confmap_init(nx, ny)
  do p = 1, nprof
    if (mask1d(p) == 1) then
      call oldtonew(real(lat(p)), real(lon(p)), latnew, lonnew)
      call pivotp(lonnew, latnew, ipiv(p), jpiv(p))
    end if
  end do
  where(ipiv < 2 .or. jpiv < 2 .or. ipiv > nx - 1 .or. jpiv > ny - 1) mask1d = 0
  where (spread(mask1d, dim=1, ncopies=nlev) == 0) mask2d = 0
  write(*,'(2x,i4,x,a)') count(mask1d == 1), 'good profiles after pivot_qc'
  write(*,'(i6,x,a)') count(mask2d == 1), 'good obs after pivot_qc'
    
  ! pres_qc

  !do l = 1, nlev
  !   write(*,*) (pres_qc(l,p),p=1,min(nprof,15))
  !enddo
  
  where (pres_qc /= 1) mask2d = 0
  where (sum(mask2d,dim=1) == 0) mask1d = 0 ! eliminate profile without good data
  write(*,'(2x,i4,x,a)') count(mask1d == 1), 'good profiles after pres_qc'
  write(*,'(i6,x,a)') count(mask2d == 1), 'good obs after pres_qc'
 
  ! temp_qc
  where (temp_qc /= 1) mask2d = 0
  where (sum(mask2d,dim=1) == 0) mask1d = 0 ! eliminate profile without good data
  write(*,'(2x,i4,x,a)') count(mask1d == 1), 'good profiles after temp_qc'
  write(*,'(i6,x,a)') count(mask2d == 1), 'good obs after temp_qc'
  
  ! psal_qc
  where (psal_qc /= 1) mask2d = 0
  where (sum(mask2d,dim=1) == 0) mask1d = 0 ! eliminate profile without good data
  write(*,'(2x,i4,x,a)') count(mask1d == 1), 'good profiles after psal_qc'
  write(*,'(i6,x,a)') count(mask2d == 1), 'good obs after psal_qc'

  !-- range qc

  !-- depth: check for the observation being wet
  do p = 1, nprof
    if (mask1d(p) == 0) cycle
    do l = 1, nlev
       if (mask2d(l, p) == 1) then
          if (pres(l, p) > depths(ipiv(p), jpiv(p)) .or.&
              pres(l, p) > depths(ipiv(p) + 1, jpiv(p)) .or.&
              pres(l, p) > depths(ipiv(p), jpiv(p) + 1) .or.&
              pres(l, p) > depths(ipiv(p) + 1, jpiv(p) + 1)) then
              mask2d(l, p) = 0
          end if
        endif
    end do
  end do
  where (sum(mask2d,dim=1) == 0) mask1d = 0 ! eliminate profile without good data
  write(*,'(2x,i4,x,a)') count(mask1d == 1), 'good profiles after depth range qc'
  write(*,'(i6,x,a)') count(mask2d == 1), 'good obs after depth range qc'
  
  ! psal
  where (psal < SAL_MIN .and. psal > SAL_MAX) mask2d = 0
  where (sum(mask2d,dim=1) == 0) mask1d = 0 ! eliminate profile witout good data
  write(*,'(2x,i4,x,a)') count(mask1d == 1), 'good profiles after psal range qc'
  write(*,'(i6,x,a)') count(mask2d == 1), 'good obs after psal range qc'
  
  ! temp
  where (temp < TEM_MIN .and. temp > TEM_MAX) mask2d = 0
  where (sum(mask2d,dim=1) == 0) mask1d = 0 ! eliminate profile without good data
  write(*,'(2x,i4,x,a)') count(mask1d == 1), 'good profiles after temp range qc'
  write(*,'(i6,x,a)') count(mask2d == 1), 'good obs after temp range qc'

  !-- stability qc

  outer: do p = 1, nprof
     if (mask1d(p) == 0) cycle
     write(*,*) '------------------------'
     write(*,'(2x,a)') profname(p)
     write(*,*) '------------------------'
     rho_prev = 0.
     do l = 1, nlev
        if (mask2d(l,p) == 0.) cycle
        rho = potential_density(temp(l,p),psal(l,p))
        !write(*,'(i3,3(x,f8.5))') l, rho, temp(l,p), psal(l,p)
        if (rho_prev == 0.) then
           write(*,'(a)') 'SKIP'
           rho_prev = rho
           cycle
        end if
        rho_inc = rho - rho_prev
        if (rho_inc < DENS_DIFF_MIN) then
           mask2d(l-1,p) = 0 !
           mask2d(l,p)   = 0 ! eliminate convectively unstable pair
           print *, ' Convectionally unstable profile deteced:'
           print *, '      filename  :',trim(fname)
           print *, '      profile ID:',trim(profname(p))
           print *, '      levels    :',l-1,l
           print *, '   rho increment:',rho_inc
           rho_prev = 0.
        else
           rho_prev = rho
        endif
     end do
     write(*,*)
  end do outer
  where (sum(mask2d,dim=1) == 0) mask1d = 0 ! eliminate profile without good data
  write(*,'(2x,i4,x,a)') count(mask1d == 1), 'good profiles after stability qc'
  write(*,'(i6,x,a)') count(mask2d == 1), 'good obs after stability qc'

  !-- redundancy qc
  ! TW: I do not think this is necessary for CMEMS MY data
  !     see original code in m_read_ifremer_argo.F90
    !
    ! Finally, discard redundant observations
    ! This is a O(n^2) search, which can become a bit long when the number of
    ! examined profiles becomes really large (say, 10^4)
    !

  !-- construct EnKF data

  ngood = count(mask2d == 1)
  allocate(data(ngood))
  if(present(datainfo)) then
     allocate(datainfo(ngood))
  endif
  
  ndata = 0
  do p = 1, nprof
  do l = 1, nlev
     if (mask2d(l,p) == 1) then
        ndata = ndata + 1
        
        if (trim(obstype) == 'SAL') then
           data(ndata) % d = psal(l, p)
        else if (trim(obstype) == 'TEM') then
           data(ndata) % d = temp(l, p)
        end if
        
        data(ndata) % var = variance
        data(ndata) % id = obstype
        data(ndata) % lon = lon(p)
        data(ndata) % lat = lat(p)
        data(ndata) % depth = max(0.0, real(pres(l, p)))
        data(ndata) % ipiv = ipiv(p)
        data(ndata) % jpiv = jpiv(p)
        data(ndata) % ns = 0   ! for a point (not gridded) measurement
        data(ndata) % date = 0 ! assimilate synchronously
          
        call bilincoeff(real(modlon),real(modlat),nx,ny,real(lon(p)),real(lat(p)),ipiv(p), &
                        jpiv(p),data(ndata) % a1,data(ndata) % a2,data(ndata) % a3, &
                        data(ndata) % a4)
        data(ndata) % status = .true. ! (active)
        data(ndata) % i_orig_grid = p
        data(ndata) % j_orig_grid = l
     endif
  end do ! l loop
  end do ! p loop

  if (ndata /= ngood) then
     print *, 'ERROR: read_CMEMS_CORA(): ndata =', ndata,', ngood =',ngood
     stop
  end if

  !-- release memory

  deallocate(profname, time, time_qc,lat, lon, pos_qc)
  deallocate(pres, pres_qc, temp, temp_qc, psal, psal_qc)
  deallocate(mask1d, mask2d)
  
  end subroutine read_CMEMS_CORA

  subroutine data_inquire(fname, nprof, nlev)
    !use ifport
    !use nfw_mod
    use netcdf
    use m_nf90_err

    character(STRLEN), intent(in) :: fname
    integer, intent(inout) :: nprof, nlev

    integer :: ios
    integer :: ncid
    integer :: id, attid

    call nf90_err(NF90_OPEN(trim(fname),NF90_NOWRITE,ncid))

    ! nprof
    !
    call nf90_err(nf90_Inq_Dimid(ncid,'N_PROF',id))
    call nf90_err(nf90_Inquire_Dimension(ncid,id,len=nprof))

    ! nlev
    !
    call nf90_err(nf90_Inq_Dimid(ncid,'N_LEVELS',id))
    call nf90_err(nf90_Inquire_Dimension(ncid,id,len=nlev))
       
    call nf90_err(nf90_close(ncid))
       
  end subroutine data_inquire
  
  subroutine data_readfile(fname,obstype,time,time_qc,lat,lon,pos_qc,pres,pres_qc,temp,temp_qc,psal,psal_qc,nprof,nlev,profname)
    !use ifport
    use netcdf
    use m_nf90_err
    
    character(STRLEN), intent(in) :: fname
    character(*), intent(in) :: obstype
    character(len=32), intent(inout), dimension(:) :: profname
    integer, intent(out) :: nprof, nlev
    real(8), intent(inout), dimension(:) :: time
    integer(kind=1), intent(inout), dimension(:) :: time_qc
    real(8), intent(inout), dimension(:) :: lat, lon
    integer(kind=1), intent(inout), dimension(:) :: pos_qc
    real(8), intent(inout), dimension(:,:) :: pres
    integer(kind=1), intent(inout), dimension(:,:) :: pres_qc
    real(8), intent(inout), dimension(:,:) :: temp
    integer(kind=1), intent(inout), dimension(:,:) :: temp_qc
    real(8), intent(inout), dimension(:,:) :: psal
    integer(kind=1), intent(inout), dimension(:,:) :: psal_qc

    real(8) :: fillval, unitval
    character(STRLEN) :: varname, qcname

    integer :: ncid, varid, attid, dimid, id
    integer :: ndims, dimids(2), dimlen1, dimlen2
    integer :: status

    integer(kind=1), allocatable :: mask1d(:) 
    integer(kind=1), allocatable :: mask2d(:,:) 
    !real(8), allocatable :: var2d(:,:)
    
    print *, "size(time)   : (",size(time),")"
    print *, "size(lat)    : (",size(lat ),")"
    print *, "size(lon)    : (",size(lon ),")"
    print *, "size(pres)   : (",size(pres,1),",",size(pres,2),")"
    
    print *, '  reading "'//trim(fname)//'"'
    
    call nf90_err(NF90_OPEN(trim(fname),NF90_NOWRITE,ncid),'call 01')

    ! nprof
    !
    call nf90_err(nf90_Inq_Dimid(ncid,'N_PROF',id),'call 02')
    call nf90_err(nf90_Inquire_Dimension(ncid,id,len=nprof),'call 03')

    ! nlev
    !
    call nf90_err(nf90_inq_dimid(ncid,'N_LEVELS',id),'call 04')
    call nf90_err(nf90_inquire_dimension(ncid,id,len=nlev),'call 05')

    print *, "(nprof,nlev) : (",nlev,",",nprof,")"
    allocate(mask1d(nprof))
    allocate(mask2d(nlev,nprof))

    ! profname
    !
    call nf90_err(nf90_inq_varid(ncid,'PLATFORM_NUMBER',id),'call 06')
    call nf90_err(nf90_get_var(ncid,id,profname),'call 07')

    ! time
    !
    call nf90_err(nf90_inq_varid(ncid,'TIME',id),'call 08')
    call nf90_err(nf90_get_var(ncid,id,time),'call 09')

    ! time_qc
    !
    call nf90_err(nf90_inq_varid(ncid,'TIME_QC',id),'call 10')
    call nf90_err(nf90_get_var(ncid,id,mask1d),'call 11')
    time_qc(:) = 0
    where (mask1d == 1) time_qc = 1

    ! lat
    !
    call nf90_err(nf90_inq_varid(ncid,'LATITUDE',id),'call 12')
    call nf90_err(nf90_get_var(ncid,id,lat),'call 13')

    ! lon
    !
    call nf90_err(nf90_inq_varid(ncid,'LONGITUDE',id),'call 14')
    call nf90_err(nf90_get_var(ncid,id,lon),'call 15')

    ! pos_qc
    !
    call nf90_err(nf90_inq_varid(ncid,'POSITION_QC',id),'call 16')
    call nf90_err(nf90_get_var(ncid,id,mask1d),'call 17')
    pos_qc(:) = 0
    where (mask1d == 1) pos_qc = 1

    ! pres
    !
    if (nf90_inq_varid(ncid,trim('PRES_ADJUSTED'),id) == 0) then
       varname = 'PRES_ADJUSTED'
       qcname = 'PRES_ADJUSTED_QC'
       print *, 'Read ',trim(varname), trim(qcname)
    else if (nf90_inq_varid(ncid,trim('PRES'),id) == 0) then
       varname = 'PRES'
       qcname = 'PRES_QC'
       print *, 'Read ',trim(varname), trim(qcname)
    else
       stop "ERROR: PRES variable not found, STOP"
    endif

    call nf90_err(nf90_inq_varid(ncid,trim(varname),varid),'call 18a')
    call nf90_err(nf90_get_var(ncid,id,pres),'call 18b')
    call nf90_err(nf90_get_att(ncid,id,TRIM('_FillValue'),fillval),'call 18c')
    call nf90_err(nf90_inq_varid(ncid,trim(qcname),id),'call 18d') ! QC
    call nf90_err(nf90_get_var( ncid,id,mask2d),'call 18e')
    pres_qc(:,:) = 0
    where (mask2d == 1) pres_qc = 1
   
    ! temp
    !
    if (nf90_inq_varid(ncid,trim('TEMP_ADJUSTED'),id) == 0) then
      varname = 'TEMP_ADJUSTED'
      qcname = 'TEMP_ADJUSTED_QC'
    else if (nf90_inq_varid(ncid,trim('TEMP'),id) == 0) then
      varname = 'TEMP'
      qcname = 'TEMP_QC'
    else
      print *, 'ERROR: TEMP variable not found, STOP'
      stop
    endif
    
    call nf90_err(nf90_inq_varid(ncid,trim(varname),varid),'call 19a')
    call nf90_err(nf90_get_var(ncid,varid,temp),'call 19b')
    call nf90_err(nf90_get_att(ncid,varid,TRIM('_FillValue'),fillval),'call 19c')
    call nf90_err(nf90_inq_varid(ncid,trim(qcname),varid),'call 19d') ! QC
    call nf90_err(nf90_get_var(ncid,varid,mask2d),'call 19e')
    temp_qc(:,:) = 0
    where (mask2d == 1) temp_qc = 1

    ! psal
    !
    if (nf90_inq_varid(ncid,trim('PSAL_ADJUSTED'),id) == 0) then
      varname = 'PSAL_ADJUSTED'
      qcname = 'PSAL_ADJUSTED_QC'
    else if (nf90_inq_varid(ncid,trim('PSAL'),id) == 0) then
      varname = 'PSAL'
      qcname = 'PSAL_QC'
    else
      print *, 'ERROR: PSAL variable not found, STOP'
      stop
    endif
    
    call nf90_err(nf90_inq_varid(ncid,trim(varname),varid),'call 20a')
    call nf90_err(nf90_get_var(ncid,varid,psal),'call 20b')
    call nf90_err(nf90_get_att(ncid,varid,TRIM('_FillValue'),fillval),'call 20c')
    call nf90_err(nf90_inq_varid(ncid,trim(qcname),varid),'call 20d') ! QC
    call nf90_err(nf90_get_var(ncid,varid,mask2d),'call 20e')
    psal_qc(:,:) = 0
    where (mask2d == 1) psal_qc = 1
      
    call nf90_err(nf90_close(ncid))

    deallocate(mask1d)
    deallocate(mask2d)
    
  end subroutine data_readfile

  subroutine grid_readxyz(nx, ny, lat, lon, depth)
    integer, intent(in) :: nx, ny
    real(8), dimension(nx, ny), intent(inout) :: lat, lon, depth

    logical :: exists
    character(len = 128) :: fname
    
    fname = 'newpos.uf'
    inquire(file = fname, exist = exists)
    if (.not. exists) then
       print *, 'grid_readxyz(): ERROR: "', trim(fname), '" does not exist'
       stop
    end if
    open(10, file = fname, form = 'unformatted', status = 'old')
    print *, '  grid_readxyz(): reading "', trim(fname), '"...'
    read(10) lat, lon
    close(10)

    write(fname, '(a, i3.3, a, i3.3, a)') 'depths', nx, 'x', ny, '.uf'
    inquire(file = fname, exist = exists)
    if (.not. exists) then
       print*, 'grid_readxyz(): ERROR: "', trim(fname), '" does not exist'
       stop
    end if
    open (unit = 10, file = fname, status = 'old', form = 'unformatted')
    print *, '  grid_readxyz(): reading "', trim(fname), '"...'
    read(10) depth
    close(10)
  end subroutine grid_readxyz

  real(8) function potential_density(T, S)
    real(8), intent(in) :: T, S

    if (T < -2.0d0 .or. T > 40.0d0 .or. S < 0.0d0 .or. S > 42.0d0) then
       potential_density = -999.0d0
       return
    end if

    potential_density =&
         -9.20601d-2&
         + T * (5.10768d-2 + S * (- 3.01036d-3)&
         + T * (- 7.40849d-3 + T * 3.32367d-5 + S * 3.21931d-5))&
         + 8.05999d-1 * S
  end function potential_density

end module m_read_CMEMS_CORA
