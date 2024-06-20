module m_read_ESACCI_SCHL
  ! Reads ESACCI SCHL data after having read the grid in read_ESACCI_grid

  integer, parameter, private :: STRLEN = 512

contains

  subroutine read_ESACCI_SCHL(filename,gr,data)
    use, intrinsic :: ieee_arithmetic
    use mod_measurement
    use mod_grid
    use m_spherdist
    use netcdf
    use nfw_mod
    implicit none

    type (measurement),  intent(inout) :: data(:)
    type (grid),         intent(inout) :: gr ! CLS measurement grid
    character(len=80),   intent(in) :: filename

    ! Variable ids
    integer :: lon_ID, lat_ID,vdat_ID, vstd_ID, vmask_ID
    ! Data arrays
    real*8, allocatable :: dat(:,:), lon(:), lat(:), std(:,:)
    logical, allocatable :: mask(:,:)
    integer :: ncid ! observations
    real*8, dimension(1) :: undef_dat
    real*8 :: nan_value
    integer :: i, j, count1
    real, parameter :: Lscale = 2  !
    real*8, dimension(1) :: scale_factor
    ! filen name
    logical         :: ex

    print *, 'read_ESACCI_SCHL:'

    inquire(file=trim(filename),exist=ex)
    if (ex) then
       ! Reading the observation file 
       call nfw_open(filename, nf_nowrite, ncid)
       ! Get dimension id in netcdf file ...
       !nb total of data
       allocate(lon(gr%nx), lat(gr%ny), dat(gr%nx,gr%ny), std(gr%nx, gr%ny), mask(gr%nx, gr%ny))

       ! Variable ids in netcdf file
       call nfw_inq_varid(filename, ncid,'lon', lon_ID)
       call nfw_get_var_double(filename, ncid, lon_ID, lon)

       call nfw_inq_varid(filename, ncid,'lat', lat_ID)
       call nfw_get_var_double(filename, ncid, lat_ID, lat)

       call nfw_inq_varid(filename, ncid,'chlor_a' ,vdat_ID)
       call nfw_get_var_double(filename, ncid, vdat_ID, dat)
       call nfw_get_att_double(filename, ncid, vdat_ID, '_FillValue', undef_dat(1))

       call nfw_inq_varid(filename, ncid,'chlor_a_log10_rmsd' ,vstd_ID)
       call nfw_get_var_double(filename, ncid, vstd_ID, std)
       !
       call nfw_close(filename, ncid)

       ! deal with _Fillvalue grids

       gr % undef = ieee_value(0.0, ieee_quiet_nan)

       mask = (dat == undef_dat(1) .or. std == undef_dat(1) .or. dat > 20.0 ) ! do not assimilate Chla > 20 mg m-3
       where (mask)
           dat = gr % undef
           std = gr % undef
       end where

       ! convert log10 std to linear std

       std = abs(dat)*(10.0**std - 1.0) ! error information is given by std of log10(chlor_a)

       ! mask for valud data grids
       mask = .not. ieee_is_nan(dat)

       print *, 'Number of NaNs:', count(mask)
       !
       print '(1x, a, 2f10.2)', '    range Lon = ', minval(lon), maxval(lon)
       print '(1x, a, 2f10.2)', '    range Lat = ', minval(lat), maxval(lat)
       print '(1x, a, 2f10.2)', '    range chla (mg m-3) = ', minval(dat,mask), maxval(dat,mask)
       print '(1x, a, 2f10.2)', '    range chla_std (mg m-3) = ', minval(std,mask), maxval(std,mask)

       ! constract EnKF data

       count1=1
       do i=1,gr%nx
          do j=1,gr%ny
            if (mask(i,j)) then ! eliminate NaN data
                   data(count1)%id = 'SCHL'
                   data(count1)%d = dat(i,j)
                   data(count1)%ipiv = count1 !whatever it is filled afterwards
                   data(count1)%jpiv = 1   !whaterver it is filled afterwards
                   data(count1)%lat=lat(j)
                   data(count1)%lon=lon(i)
                   data(count1)%a1 = spherdist(real(lon(i))-.5*gr%dx,real(lat(j))-.5*gr%dy,real(lon(i))+.5*gr%dx,real(lat(j))+.5*gr%dy)
                   data(count1)%ns = 1 ! 1 for data with a spatial extent
                   data(count1)%var = (std(i,j)*Lscale)**2 
                   !data(count1)%var = (abs(dat(i,j)*std(i,j))*Lscale) ** 2 ! convert log10 std to linear std
                   !data(count1)%var = (dat(i,j)*((10.0**std(i,j) - 1.0))*Lscale) ** 2 ! convert log10 std to linear std
                   data(count1)%date = 0
                   data(count1)%depth = 0.0
                   data(count1)%status = .true.
                   count1=count1+1
            endif
          enddo   !i
        enddo    !j
       print*, '    # of obs read = ', count1
       deallocate(lat, lon, dat, mask)
    end if ! ex
    print *, 'MAX var(SCHL) = ', maxval(data % var)
    !print *, 'MAX var(SCHL) = ', maxval(data(1 : count1) % var)
  end subroutine read_ESACCI_SCHL
end module m_read_ESACCI_SCHL
