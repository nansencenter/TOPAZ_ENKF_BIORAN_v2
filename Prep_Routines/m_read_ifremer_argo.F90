! File:          m_read_ifremer_argo.F90
!
! Created:       25 Jan 2008
!
! Author:        Pavel Sakov
!                NERSC
!
! Purpose:       Read Argo data from NetCDF files from IFREMER into TOPAZ
!                system.
!
! Description:   Data file(s) are defined by the string in the 4th line of
!                "infile.data". It should have the following format:
!                <BEGIN>
!                IFREMER
!                SAL | TEM
!                <obs. error variance>
!                <File name(s) or a wildcard>
!                <END>
!                After that:
!                1. all profiles are read into two arrays,
!                   pres(1 : nlev, 1 : nprof) and v(1 : nlev, 1 : nprof), where
!                   nprof is the total number of profiles in all files, and
!                   nlev is the maximum number of horizontal levels for all
!                   profiles;
!                2. bad data (with qc flags other than '1' or '2' is discarded;
!                3. dry or outside locations are discarded
!                4. if there close profiles (in the same grid cell), the best
!                   one (with most data or the most recent) is retained
!
! Modifications: 17/08/2010 PS: skip discarding close profiles
! 1) skipping *_qc=2 in the profile March 2024
!

module m_read_ifremer_argo
  implicit none

  integer, parameter, private :: STRLEN =2180
  integer, parameter, private :: MLEV0 = 1000   ! changed back for NRT profile at Oct2019
  !integer, parameter, private :: MLEV0 = 2000   ! changed for NRT profile Jan2024 
  !integer, parameter, private :: MLEV0 = 4000
  !real, parameter, private :: SAL_MIN = 16.0
  !real, parameter, private :: SAL_MIN = 20.0     ! tuning in March 2020
  real, parameter, private :: SAL_MIN = 16.0     ! tuning back Jan 2024
  real, parameter, private :: SAL_MAX = 37.5
  real, parameter, private :: TEM_MIN = -2.0
  real, parameter, private :: TEM_MAX = 40.0
  real, parameter, private :: DENS_DIFF_MIN = -0.02
  logical, parameter, private :: DISCARD_CLOSE = .false.

  public read_ifremer_argo

  private data_inquire
  private data_readfile
  private potential_density
  private grid_readxyz

contains

  subroutine read_ifremer_argo(fnames, obstype, variance, nx, ny, data,datainfo)
    use mod_measurement
    use m_oldtonew
    use m_confmap
    use m_bilincoeff
    use m_pivotp
    use nfw_mod
    
    character(*), intent(in) :: fnames
    character(*), intent(in) :: obstype
    real, intent(in) :: variance
    integer, intent(in) :: nx, ny
    type(measurement), allocatable, intent(out) :: data(:)
    type(measurement_info), allocatable, intent(out),optional :: datainfo(:)

    character(STRLEN) :: fname
    integer :: nfile, nprof, nlev
    real(8), allocatable :: juld(:)
    character, allocatable :: juld_qc(:)
    real(8), allocatable :: lat(:), lon(:)
    character, allocatable :: pos_qc(:)
    real(8), allocatable :: pres(:,:)
    character, allocatable :: pres_qc(:,:)
    real(8), allocatable :: temp(:,:), salt(:, :)
    character, allocatable :: temp_qc(:,:), psal_qc(:, :)
    integer, allocatable :: ipiv(:), jpiv(:)

    real(8), dimension(nx, ny) :: modlat, modlon
    real(8), dimension(nx, ny) :: depths

    integer :: f, l, p, np
    integer, allocatable :: mask(:)
    integer, allocatable :: mask2(:, :)
    integer, allocatable :: fid(:);
    integer, allocatable :: profid(:)
    integer, allocatable :: done(:)
    real(8) :: zmax, Q, Qbest, rho, rho_prev, rho_inc
    integer :: best
    integer :: p1

    integer ngood, ndata
    real :: latnew, lonnew
    
#if defined (BLACK_PROFILE)
    integer,        allocatable :: mask_0(:), mask_1(:), mask_2(:), mask_3(:)
    character(len=32)    :: Profname(10000)
    character*100        :: Fobslist
    integer,      allocatable           :: Ptrack(:)
    real,         allocatable           :: Pdep_tmp(:),Pdep_min(:),Pdep_max(:)
    character*100,allocatable           :: Pfsur(:)    
    type(measurement_info), allocatable :: Pdatainfo(:)
    integer   :: i,j,k
#endif

    print *, 'BEGIN read_ifremer_argo(): ',trim(fnames)
    call data_inquire(fnames, nfile, nprof, nlev)
    print *, '  overall: nprof =', nprof, ', nlev =', nlev

    ! limitation on the maximal levels 
    if (nlev>MLEV0) then
      nlev=MLEV0
      print *, '  Limited overall: nprof =', nprof, ', nlev =', nlev
    endif

#if defined (BLACK_PROFILE)
    open(20,file="Blacklist_Prof_QC.txt") 

    allocate(Pfsur(nfile),Ptrack(nprof))
    allocate(Pdatainfo(nprof))

#endif
    
    allocate(juld(nprof))
    allocate(juld_qc(nprof))
    allocate(lat(nprof))
    allocate(lon(nprof))
    allocate(pos_qc(nprof))
    allocate(fid(nprof))
    allocate(profid(nprof))
    allocate(pres(nlev, nprof))
    allocate(pres_qc(nlev, nprof))
    allocate(temp(nlev, nprof))
    allocate(salt(nlev, nprof))
    allocate(temp_qc(nlev, nprof))
    allocate(psal_qc(nlev, nprof))

    pres=99999.
    temp=99999.
    salt=99999.

    p = 1
    do f = 1, nfile
#if defined (BLACK_PROFILE)
       call data_readfile(f, trim(obstype), np, juld(p : nprof),&
            juld_qc(p : nprof), lat(p : nprof),&
            lon(p : nprof), pos_qc(p : nprof), pres(1 : nlev, p : nprof),&
            pres_qc(1 : nlev, p : nprof), temp(1 : nlev, p : nprof),&
            temp_qc(1 : nlev, p : nprof), salt(1 : nlev, p : nprof),&
            psal_qc(1 : nlev, p : nprof),Profname(p:nprof),Fobslist)

            allocate(Pdep_tmp(nlev),Pdep_min(np),Pdep_max(np))
            Pdep_tmp=0;
            Pdep_min=0;
            Pdep_max=0;
            do l=1, np
              Pdep_tmp=pres(:,p+l-1)
              Pdep_min(l)=Pdep_tmp(1)
              if(Pdep_tmp(1)>99990) then
                Pdep_max(l)=Pdep_tmp(1);
              else
                where(Pdep_tmp>99990) Pdep_tmp=Pdep_tmp(1)
                Pdep_min(l)=Pdep_tmp(1);
                Pdep_max(l)=maxval(Pdep_tmp);
              endif
            end do 
            Pfsur(f)=Fobslist;   Ptrack(p:p+np-1)=f
            Pdatainfo(p:p+np-1)%id=trim(obstype);
            Pdatainfo(p:p+np-1)%jday=floor(juld(p:nprof));
            Pdatainfo(p:p+np-1)%jtime=juld(p:nprof)-floor(juld(p:nprof));
            Pdatainfo(p:p+np-1)%lon=lon(p:nprof);
            Pdatainfo(p:p+np-1)%lat=lat(p:nprof);
            Pdatainfo(p:p+np-1)%mindep=Pdep_min(1:np)
            Pdatainfo(p:p+np-1)%maxdep=Pdep_max(1:np);
            Pdatainfo(p:p+np-1)%signal=1;
            Pdatainfo(p:p+np-1)%platnum=Profname(p:nprof);
            Pdatainfo(p:p+np-1)%inifile=Fobslist;
            deallocate(Pdep_tmp,Pdep_min,Pdep_max)
#else
       call data_readfile(f, trim(obstype), np, juld(p : nprof),&
            juld_qc(p : nprof), lat(p : nprof),&
            lon(p : nprof), pos_qc(p : nprof), pres(1 : nlev, p : nprof),&
            pres_qc(1 : nlev, p : nprof), temp(1 : nlev, p : nprof),&
            temp_qc(1 : nlev, p : nprof), salt(1 : nlev, p : nprof),&
            psal_qc(1 : nlev, p : nprof))
#endif
       fid(p : p + np - 1) = f
       do l = 1, np
          profid(p + l - 1) = l
       end do
       p = p + np
    end do

    ! mask <- juld_qc, pos_qc, pres_qc, v_qc
    !
    allocate(mask(nprof))
    mask(:) = 1
    allocate(mask2(nlev, nprof))
    mask2(:, :) = 1
#if defined (BLACK_PROFILE)
    ! different signal for QC type:
    allocate(mask_0(nprof),mask_1(nprof),mask_2(nprof),mask_3(nprof))
    mask_0(:) = 0; !1: Out of model domain 
    mask_1(:) = 0; !1: used for location/time QC
    mask_2(:) = 0; !1: Sal and density check 
    mask_3(:) = 0; !1: pre/sal/tem QC 
#endif
    where (juld_qc /= '1') mask = 0
    !where (juld_qc /= '1' .and. juld_qc /= '2') mask = 0
    do p = 1, nprof
       if (mask(p) == 0) then
          mask2(:, p) = 0
       end if
    end do
    print *, '  after examining JULD_QC:'
    print *, '    ', count(mask == 1), ' good profiles'
    print *, '    ', count(mask2 == 1), ' good obs'

    where (pos_qc /= '1' ) mask = 0
    !where (pos_qc /= '1' .and. pos_qc /= '2') mask = 0
    do p = 1, nprof
       if (mask(p) == 0) then
          mask2(:, p) = 0
       end if
    end do
    print *, '  after examining POS_QC:'
    print *, '    ', count(mask == 1), ' good profiles'
    print *, '    ', count(mask2 == 1), ' good obs'

    ! ipiv, jpiv
    !
    allocate(ipiv(nprof))
    allocate(jpiv(nprof))
    ipiv(:) = -999
    jpiv(:) = -999
    call confmap_init(nx, ny)
    do p = 1, nprof
       if (mask(p) == 0) then
          cycle
       end if
       call oldtonew(real(lat(p)), real(lon(p)), latnew, lonnew)
       call pivotp(lonnew, latnew, ipiv(p), jpiv(p))
    end do
    where (ipiv < 2 .or. jpiv < 2 .or. ipiv > nx - 1 .or. jpiv > ny - 1) mask = 0
#if defined (BLACK_PROFILE)
    where (ipiv < 2 .or. jpiv < 2 .or. ipiv > nx - 1 .or. jpiv > ny - 1) mask_0 = 1 
    where (mask_0 == 0 .and. mask == 0) mask_1=1
#endif
    do p = 1, nprof
       if (mask(p) == 0) then
          mask2(:, p) = 0
       end if
    end do
    print *, '  after calculaling pivot points:'
    print *, '    ', count(mask == 1), ' good profiles'
    print *, '    ', count(mask2 == 1), ' good obs'

    !
    ! Now examine 3D quality flags; set the mask for a profile to 0 if there
    ! are no good samples in this profile
    !

    ! pres_qc
    !
    do p = 1, nprof
       do l = 1, nlev
          !if (pres_qc(l, p) /= '1' .and. pres_qc(l, p) /= '2' ) then
          if (pres_qc(l, p) /= '1' ) then
             mask2(l, p) = 0
             continue
          end if
          if (pres(l,p)>12000) then
             mask2(l, p) = 0
          end if
       end do
       if (count(mask2(:, p) == 1) == 0) then
          mask(p) = 0
       end if
    end do
    print *, '  after examining PRES_QC:'
    print *, '    ', count(mask == 1), ' good profiles'
    print *, '    ', count(mask2 == 1), ' good obs'

    ! <data>_qc
    !
    if (trim(obstype) == 'SAL') then
       do p = 1, nprof
          do l = 1, nlev
             !if (psal_qc(l, p) /= '1' .and. psal_qc(l, p) /= '2') then
             if (psal_qc(l, p) /= '1') then
                mask2(l, p) = 0
             end if
          end do
          if (count(mask2(:, p) == 1) == 0) then
             mask(p) = 0
          end if
       end do
    else if (trim(obstype) == 'TEM') then
       do p = 1, nprof
          do l = 1, nlev
             !if (temp_qc(l, p) /= '1' .and. temp_qc(l, p) /= '2') then
             if (temp_qc(l, p) /= '1') then
                mask2(l, p) = 0
             end if
          end do
          if (count(mask2(:, p) == 1) == 0) then
             mask(p) = 0
          end if
       end do
    end if
    print *, '  after examining <data>_QC:'
    print *, '    ', count(mask == 1), ' good profiles'
    print *, '    ', count(mask2 == 1), ' good obs'
#if defined (BLACK_PROFILE)
    where (mask_0 == 0 .and. mask == 0 .and. mask_1 == 0) mask_2=1
#endif

    ! Check for the observation being wet
    !
    call grid_readxyz(nx, ny, modlat, modlon, depths)
    do p = 1, nprof
       if (mask(p) == 0) then
          cycle
       end if
       do l = 1, nlev
          if (mask2(l, p) == 0) then
             cycle
          end if
          if (pres(l, p) > depths(ipiv(p), jpiv(p)) .or.&
               pres(l, p) > depths(ipiv(p) + 1, jpiv(p)) .or.&
               pres(l, p) > depths(ipiv(p), jpiv(p) + 1) .or.&
               pres(l, p) > depths(ipiv(p) + 1, jpiv(p) + 1)) then
             mask2(l, p) = 0
          end if
       end do
       if (count(mask2(:, p) == 1) == 0) then
          mask(p) = 0
       end if
    end do
    print *, '  after examining for wet cells:'
    print *, '    ', count(mask == 1), ' good profiles'
    print *, '    ', count(mask2 == 1), ' good obs'
#if defined (BLACK_PROFILE)
    where (mask_0 == 0 .and. mask == 0 .and. mask_2 == 0 ) mask_1=1
#endif

    ! For salinity, allow SAL_MIN < S < SAL_MAX only in a profile
    !
    do p = 1, nprof
       if (mask(p) == 0) then
          cycle
       end if
       do l = 1, nlev
          if (mask2(l, p) == 0) then
             cycle
          end if
          if ((trim(obstype) == 'TEM' .and.&
               !(temp_qc(l, p) == '1' .or. temp_qc(l, p) == '2')) .and.&
               temp_qc(l, p) == '1' ).and.&
               (temp(l, p) < TEM_MIN .or. temp(l, p) > TEM_MAX)) then
             mask(p) = 0 ! discard the profile
             mask2(:, p) = 0
             exit
          end if
          if ((trim(obstype) == 'SAL' .and.&
               !(psal_qc(l, p) == '1' .or. psal_qc(l, p) == '2')) .and.&
               psal_qc(l, p) == '1' ).and.&
               (salt(l, p) < SAL_MIN .or. salt(l, p) > SAL_MAX)) then
             mask(p) = 0 ! discard the profile
             mask2(:, p) = 0
             exit
          end if
       end do
    end do
    print *, '  after keeping only profiles with salinity within',&
         SAL_MIN, '<= S <=', SAL_MAX, ":"
    print *, '    ', count(mask == 1), ' good profiles'
    print *, '    ', count(mask2 == 1), ' good obs'

    print *, '  discarding convectionally unstable profiles:'
    do p = 1, nprof
       if (mask(p) == 0) then
          cycle
       end if
       rho_prev = -999.0
       do l = 1, nlev
          if (mask2(l, p) == 0 .or.&
               !(temp_qc(l, p) /= '1' .and. temp_qc(l, p) /= '2') .or.&
               !(psal_qc(l, p) /= '1' .and. psal_qc(l, p) /= '2')) then
               temp_qc(l, p) /= '1' .or. psal_qc(l, p) /= '1' ) then
             cycle
          end if
          if (rho_prev == -999.0) then
             rho_prev = potential_density(temp(l, p), salt(l, p))
             cycle
          else
             rho = potential_density(temp(l, p), salt(l, p))
             rho_inc = rho - rho_prev
             if (rho_inc < DENS_DIFF_MIN) then
                open(10, file = 'infiles.txt')
                do f = 1, fid(p)
                   read(10, fmt = '(a)') fname
                end do
                close(10)

                print *, '    ', trim(fname), ':'
                print *, '      profile #', profid(p), '( #', p, ')'
                print *, '      level #', l
                print *, '      rho increment =', rho_inc
                mask(p) = 0 ! discard the profile
                mask2(:, p) = 0
                exit
             end if
             rho_prev = rho
          end if
       end do
    end do
    print *, '  after discarding unstable profiles:'
    print *, '    ', count(mask == 1), ' good profiles'
    print *, '    ', count(mask2 == 1), ' good obs'
#if defined (BLACK_PROFILE)
    where (mask_0 == 0 .and. mask == 0 .and. mask_2 == 0 .and. mask_1==0 ) mask_3=1
#endif
    ! Finally, discard redundant observations
    ! This is a O(n^2) search, which can become a bit long when the number of
    ! examined profiles becomes really large (say, 10^4)
    !
    if (DISCARD_CLOSE) then
       allocate(done(nprof))
       done = 0
       do p = 1, nprof
          if (mask(p) == 0 .or. done(p) == 1) then
             cycle
          end if
          np = 1
          profid(np) = p
          do p1 = p + 1, nprof
             if (ipiv(p1) == ipiv(p) .and. jpiv(p1) == jpiv(p)) then
                np = np + 1
                profid(np) = p1
                done(p1) = 1
             end if
          end do
          if (np > 1) then
             ! for each of close profiles find the depth range, number of points
             ! and the age
             Qbest = 0.0
             do p1 = 1, np
                zmax = 0.0
                ndata = 0
                do l = 1, nlev
                   if (mask2(l, p1) == 1) then
                      ndata = ndata + 1
                      if (pres(l, profid(p1)) > zmax) then
                         zmax =  pres(l, profid(p1))
                      end if
                   end if
                end do
                Q = min(zmax, 400.0) / 400.0 + min(ndata, 10) / 10
                if (Q > Qbest) then
                   best = p1
                end if
             end do
             do p1 = 1, np
                if (p1 == best) then
                   cycle
                end if
                mask(profid(p1)) = 0
                mask2(:, profid(p1)) = 0
             end do
          end if
       end do
       deallocate(done)
       print *, '  after discarding close profiles:'
       print *, '    ', count(mask == 1), ' good profiles'
       print *, '    ', count(mask2 == 1), ' good obs'
    end if ! DISCARD_CLOSE

    ngood = count(mask2 == 1)
    allocate(data(ngood))
    if(present(datainfo)) then
      allocate(datainfo(ngood))
    endif

#if defined (BLACK_PROFILE)
    do p = 1, nprof
      if(mask_0(p)) then
         Pdatainfo(p)%signal=0
      elseif(mask_1(p)) then
         Pdatainfo(p)%signal=-1
      elseif(mask_2(p)) then
         Pdatainfo(p)%signal=-2
      elseif(mask_3(p)) then
         Pdatainfo(p)%signal=-3
      endif 
    end do
#endif

    ndata = 0
    do p = 1, nprof
       if (mask(p) == 0) then
#if defined (BLACK_PROFILE)
       ! output the profile list within the model domain
          if (mask_0(p)==0) then
            !write(20,'(a16,F13.5,2F10.4,2x,a)') trim(Profname(p)),juld(p),lon(p),lat(p),trim(Pfsur(Ptrack(p)))  
            write(20,'(a7,F8.1,F8.4,2F10.3,2F8.1,i5,2x,a10,2x,a)') Pdatainfo(p)%id, &
               Pdatainfo(p)%jday, Pdatainfo(p)%jtime,  &
               Pdatainfo(p)%lon,Pdatainfo(p)%lat, Pdatainfo(p)%mindep, Pdatainfo(p)%maxdep,  &
               Pdatainfo(p)%signal,trim(Pdatainfo(p)%platnum), trim(Pdatainfo(p)%inifile)
          end if
#endif
          cycle
       end if
       do l = 1, nlev
          if (mask2(l, p) == 0) then
             cycle
          end if

          ndata = ndata + 1

          if (ndata > ngood) then
             print *, 'ERROR: read_ifremer_argo(): programming error'
             print *, '  p =', p, ', l =', l
             print *, '  # data =', ndata, ', ngood =', ngood
             stop
          elseif (mod(ndata,10000)==0) then
            print *, 'ndata=',ndata
          end if
       
          ! PS: I guess we should not bother about the cost of the
          ! comparisons below.
          !
          if (trim(obstype) == 'SAL') then
             data(ndata) % d = salt(l, p)
          else if (trim(obstype) == 'TEM') then
             data(ndata) % d = temp(l, p)
          else
             data(ndata) % d = -999.0
          end if
          data(ndata) % var = variance
          data(ndata) % id = obstype
          data(ndata) % lon = lon(p)
          data(ndata) % lat = lat(p)
          data(ndata) % depth = max(0.0, real(pres(l, p)))
          data(ndata) % ipiv = ipiv(p)
          data(ndata) % jpiv = jpiv(p)
          data(ndata) % ns = 0 ! for a point (not gridded) measurement
          data(ndata) % date = 0 ! assimilate synchronously

          call bilincoeff(real(modlon), real(modlat), nx, ny, real(lon(p)), real(lat(p)), ipiv(p),&
               jpiv(p), data(ndata) % a1, data(ndata) % a2, data(ndata) % a3,&
               data(ndata) % a4)

          data(ndata) % status = .true. ! (active)
          data(ndata) % i_orig_grid = p
          data(ndata) % j_orig_grid = l

#if defined (BLACK_PROFILE)
! saving the related measurment information
          datainfo(ndata)%id      =  Pdatainfo(p)%id
          datainfo(ndata)%jday    =  Pdatainfo(p)%jday
          datainfo(ndata)%jtime   =  Pdatainfo(p)%jtime
          datainfo(ndata)%lon     =  Pdatainfo(p)%lon
          datainfo(ndata)%lat     =  Pdatainfo(p)%lat
          datainfo(ndata)%mindep  =  data(ndata)%depth
          datainfo(ndata)%maxdep  =  data(ndata)%depth
          datainfo(ndata)%ipiv    =  data(ndata)%ipiv
          datainfo(ndata)%jpiv    =  data(ndata)%jpiv
          datainfo(ndata)%signal  =  Pdatainfo(p)%signal
          datainfo(ndata)%platnum =  Pdatainfo(p)%platnum
          datainfo(ndata)%inifile =  Pdatainfo(p)%inifile
#endif
       end do
    end do

    if (ndata /= ngood) then
       print *, 'ERROR: read_ifremer_argo(): programming error'
       print *, '  ndata =', ndata, ', ngood =', ngood
       stop
    end if

    deallocate(juld)
    deallocate(juld_qc)
    deallocate(lat)
    deallocate(lon)
    deallocate(pos_qc)
    deallocate(profid)
    deallocate(pres)
    deallocate(pres_qc)
    deallocate(temp)
    deallocate(salt)
    deallocate(temp_qc)
    deallocate(psal_qc)
    deallocate(mask)
    deallocate(mask2)
    deallocate(ipiv)
    deallocate(jpiv)
#if defined (BLACK_PROFILE)
    close(20)
    deallocate(Pfsur,Ptrack)
    deallocate(Pdatainfo)
#endif

    print *, 'END read_ifremer_argo()'

  end subroutine read_ifremer_argo


  subroutine data_inquire(fnames, nfile, nprof, nlev)
    use ifport
    use nfw_mod

    character(*), intent(in) :: fnames
    integer, intent(inout) :: nfile, nprof, nlev

    character(STRLEN) :: command ! (there may be a limit of 80 on some systems)
    character(STRLEN) :: fname
    integer :: ios
    integer :: ncid
    integer :: id

    integer :: nprof_this, nlev_this
    integer :: ns1,ns2,len_this
    integer :: res

    nfile = 0
    nprof = 0
    nlev = 0

    res=system("ls "//trim(fnames)//" > infiles.txt");

    nfile = 0
    open(10, file = 'infiles.txt')
    do while (.true.)
       read(10, fmt = '(a)', iostat = ios) fname
       if (ios /= 0) then
          exit
       end if

       nfile = nfile + 1
       print *, '  file #', nfile, ' = "', trim(fname), '"'

       call nfw_open(fname, nf_nowrite, ncid)

       ! nprof
       !
       call nfw_inq_dimid(fname, ncid, 'N_PROF', id)
       call nfw_inq_dimlen(fname, ncid, id, nprof_this)
      ! print *, '    nprof = ', nprof_this

       ! nlev
       !
       call nfw_inq_dimid(fname, ncid, 'N_LEVELS', id)
       call nfw_inq_dimlen(fname, ncid, id, nlev_this)
       
       nprof = nprof + nprof_this
       if (nlev_this > nlev) then
          nlev = nlev_this
       end if

       call nfw_close(fname, ncid)
    end do
    close(10)
  end subroutine data_inquire


  subroutine data_readfile(fid, obstype, nprof, juld_all, juld_qc_all,&
    lat_all, lon_all, pos_qc_all, pres_all, pres_qc_all, temp_all, temp_qc_all, salt_all, psal_qc_all,Pfname,Pfile)
    use ifport
    use nfw_mod
    integer, intent(in) :: fid
    character(*), intent(in) :: obstype
    integer, intent(inout) :: nprof
    real(8), intent(inout), dimension(:) :: juld_all
    character, intent(inout), dimension(:) :: juld_qc_all
    real(8), intent(inout), dimension(:) :: lat_all, lon_all
    character, intent(inout), dimension(:) :: pos_qc_all
    real(8), intent(inout), dimension(:,:) :: pres_all
    character, intent(inout), dimension(:,:) :: pres_qc_all
    real(8), intent(inout), dimension(:,:) :: temp_all
    character, intent(inout), dimension(:,:) :: temp_qc_all
    real(8), intent(inout), dimension(:,:) :: salt_all
    character, intent(inout), dimension(:,:) :: psal_qc_all
    character(len=32), intent(inout), dimension(:),optional :: Pfname
    character(len=100), intent(inout), optional :: Pfile

    character(STRLEN) :: fname
    integer :: f
    integer :: ncid
    integer :: id
    integer :: nlev,mlev

    real(8), dimension(:,:),allocatable :: tmp_all
    real(8),   dimension(:),allocatable :: tmp_sub
    integer,   dimension(:),allocatable :: mask_sub

    ! dealing with bug to read EASY CORA5.2
    real(8), dimension(1) :: scalefac, fillval, addoffset,varmin,varmax 

    integer :: res

#if defined (BLACK_PROFILE)
    character*100 Fsur
    character*300 str0 
    character*100 str1
   ! integer, external :: strrindex
    
    integer :: S1,S2 

    character*32 :: tmpstr0
    character,dimension(:,:),allocatable ::tmp_str
#endif    
    open(10, file = 'infiles.txt')
    do f = 1, fid
       read(10, fmt = '(a)') fname
    end do
    close(10)

    print *, '  reading "', trim(fname), '"'
#if defined (BLACK_PROFILE)
    ! extract the file name
    res=system("ls -l "//trim(fname)//" | awk '{print $NF}' > name.txt")
    res=system("cat name.txt | cut -d '/' -f9 >1.txt ")  
    ! the value of '-f9' can be changed as the used file link
    open(10,file='1.txt')
      read(10,*) str0
    close(10)
    f=index(str0,'.nc')  
    Pfile=trim(str0(1:f-1))
#endif
    
    call nfw_open(fname, nf_nowrite, ncid)

    ! nprof
    !
    call nfw_inq_dimid(fname, ncid, 'N_PROF', id)
    call nfw_inq_dimlen(fname, ncid, id, nprof)

    ! nlev
    !
    call nfw_inq_dimid(fname, ncid, 'N_LEVELS', id)
    call nfw_inq_dimlen(fname, ncid, id, nlev)

    ! juld
    !
    call nfw_inq_varid(fname, ncid, 'JULD', id)
    call nfw_get_var_double(fname, ncid, id, juld_all(1 : nprof))

    ! juld_qc
    !
    call nfw_inq_varid(fname, ncid, 'JULD_QC', id)
    call nfw_get_var_text(fname, ncid, id, juld_qc_all(1 : nprof))

    ! lat
    !
    call nfw_inq_varid(fname, ncid, 'LATITUDE', id)
    call nfw_get_var_double(fname, ncid, id, lat_all(1 : nprof))

    ! lon
    !
    call nfw_inq_varid(fname, ncid, 'LONGITUDE', id)
    call nfw_get_var_double(fname, ncid, id, lon_all(1 : nprof))

    ! pos_qc
    !
    call nfw_inq_varid(fname, ncid, 'POSITION_QC', id)
    call nfw_get_var_text(fname, ncid, id, pos_qc_all(1 : nprof))
     
    allocate(tmp_all(nlev,nprof))

    mlev=min(MLEV0,nlev)
    allocate(mask_sub(nprof),tmp_sub(nprof))

    ! adjusted by Jiping 28/07/2015
    if (nfw_var_exists(ncid, 'PRES')) then
      ! pres
      !
      call nfw_inq_varid(fname, ncid, 'PRES', id)
      call nfw_get_var_double(fname, ncid, id, tmp_all(1 : nlev, 1 : nprof))
      !print '(20f8.1)',tmp_all(1,:)
      ! dealing with nrt profile in Oct2019
      if (nfw_var_att_exists(ncid,id,'valid_min') .and.nfw_var_att_exists(ncid,id,'valid_max')) then
        call nfw_get_att_double(fname, ncid, id, 'valid_min', varmin)
        call nfw_get_att_double(fname, ncid, id, 'valid_max', varmax)
      else
        varmax=0; varmin=0;
      endif
      !call nfw_get_att_double(fname, ncid, id, '_FillValue', fillval)
      if (nfw_var_att_exists(ncid, id, 'scale_factor')) then
         if (nfw_var_att_exists(ncid, id, 'fill_value')) then
            call nfw_get_att_double(fname, ncid, id, 'fill_value', fillval)
         elseif (nfw_var_att_exists(ncid, id, '_FillValue')) then
            call nfw_get_att_double(fname, ncid, id, '_FillValue', fillval)
         endif
         call nfw_get_att_double(fname, ncid, id, 'scale_factor', scalefac)
         call nfw_get_att_double(fname, ncid, id, 'add_offset', addoffset)
      else
         scalefac(1)=1
         addoffset(1)=0
      endif
      !print '(a20,3F13.3)', trim(fname), fillval(:),scalefac(:),addoffset(:)
      if (varmin(1)<varmax(1).and.varmax(1)>0) then
        do f=1,mlev
          tmp_sub(1:nprof)=scalefac(1)*tmp_all(f,1:nprof)+addoffset(1)
          mask_sub(1:nprof)=0
          where (tmp_sub(:)>=varmin(1).and.tmp_sub(:)<=varmax(1))
             mask_sub(:)=1;
          endwhere
          where (mask_sub/=1)
             tmp_sub=99999.
          endwhere
          pres_all(f,1:nprof)=tmp_sub(:)
      !  stop
        end do
      else
        do f=1,mlev
          pres_all(f,1:nprof)=scalefac(1)*tmp_all(f,1:nprof)+addoffset(1)
        end do
      endif

      ! pres_qc
      !
      call nfw_inq_varid(fname, ncid, 'PRES_QC', id)
      call nfw_get_var_text(fname, ncid, id, pres_qc_all(1 : nlev, 1 : nprof))
    else
      ! pres
      !
      call nfw_inq_varid(fname, ncid, 'DEPH', id)
      call nfw_get_var_double(fname, ncid, id, tmp_all(1 : nlev, 1 : nprof))
      if (nfw_var_att_exists(ncid,id,'valid_min') .and.nfw_var_att_exists(ncid,id,'valid_max')) then
        call nfw_get_att_double(fname, ncid, id, 'valid_min', varmin)
        call nfw_get_att_double(fname, ncid, id, 'valid_max', varmax)
      else
        varmin=0; varmax=0
      endif
      !call nfw_get_att_double(fname, ncid, id, '_FillValue', fillval)
      if (nfw_var_att_exists(ncid, id, 'scale_factor')) then
         if (nfw_var_att_exists(ncid, id, 'fill_value')) then
            call nfw_get_att_double(fname, ncid, id, 'fill_value', fillval)
         elseif (nfw_var_att_exists(ncid, id, '_FillValue')) then
            call nfw_get_att_double(fname, ncid, id, '_FillValue', fillval)
         endif
         call nfw_get_att_double(fname, ncid, id, 'scale_factor', scalefac)
         call nfw_get_att_double(fname, ncid, id, 'add_offset', addoffset)
      else
         scalefac(1)=1
         addoffset(1)=0
      endif
      if (varmin(1)<varmax(1).and.varmax(1)>0) then
        do f=1,mlev
          tmp_sub(1:nprof)=scalefac(1)*tmp_all(f,1:nprof)+addoffset(1)
          mask_sub(1:nprof)=0
          where (tmp_sub(:)>=varmin(1).and.tmp_sub(:)<=varmax(1))
            mask_sub(:)=1;
          endwhere
          where (mask_sub/=1)
            tmp_sub=99999.
          endwhere
          pres_all(f,1:nprof)=tmp_sub(:)
        end do
      else
        do f=1,mlev
          pres_all(f,1:nprof)=scalefac(1)*tmp_all(f,1:nprof)+addoffset(1)
        end do
      endif

      ! pres_qc
      !
      call nfw_inq_varid(fname, ncid, 'DEPH_QC', id)
      call nfw_get_var_text(fname, ncid, id, pres_qc_all(1 : nlev, 1 : nprof))
    endif


    ! temp
    !
    if (nfw_var_exists(ncid, 'TEMP')) then
      call nfw_inq_varid(fname, ncid, 'TEMP', id)
      call nfw_get_var_double(fname, ncid, id, tmp_all(1 : nlev, 1 : nprof))
      if (nfw_var_att_exists(ncid,id,'valid_min') .and.nfw_var_att_exists(ncid,id,'valid_max')) then
        call nfw_get_att_double(fname, ncid, id, 'valid_min', varmin)
        call nfw_get_att_double(fname, ncid, id, 'valid_max', varmax)
      else
        varmin=0; varmax=0;
      endif

      !call nfw_get_att_double(fname, ncid, id, '_FillValue', fillval)
      if (nfw_var_att_exists(ncid, id, 'scale_factor')) then
         if (nfw_var_att_exists(ncid, id, 'fill_value')) then
            call nfw_get_att_double(fname, ncid, id, 'fill_value', fillval(1))
         elseif (nfw_var_att_exists(ncid, id, '_FillValue')) then
            call nfw_get_att_double(fname, ncid, id, '_FillValue', fillval(1))
         endif

        call nfw_get_att_double(fname, ncid, id, 'scale_factor', scalefac(1))
        call nfw_get_att_double(fname, ncid, id, 'add_offset', addoffset(1))
      else
        scalefac(1)=1
        addoffset(1)=0
      endif

      if (varmin(1)<varmax(1).and.varmax(1)>0) then
        do f=1,mlev
          tmp_sub(1:nprof)=scalefac(1)*tmp_all(f,1:nprof)+addoffset(1)
          mask_sub(1:nprof)=0
          where (tmp_sub(:)>=varmin(1).and.tmp_sub(:)<=varmax(1))
            mask_sub(:)=1
          endwhere
          where (mask_sub/=1)
            tmp_sub=99999.
          endwhere
          temp_all(f,1:nprof)=tmp_sub(:)
          !print '(200F13.1)', tmp_sub(:)
        end do
      else
        do f=1,mlev
          temp_all(f,1:nprof)=scalefac(1)*tmp_all(f,1:nprof)+addoffset(1)
        end do
      endif

      ! temp_qc
      !
      call nfw_inq_varid(fname, ncid, 'TEMP_QC', id)
      call nfw_get_var_text(fname, ncid, id, temp_qc_all(1 : nlev, 1 : nprof))
    else
       temp_qc_all = 'E';
    endif

    if (nfw_var_exists(ncid, 'PSAL')) then
       ! psal
       !
       call nfw_inq_varid(fname, ncid, 'PSAL', id)
       call nfw_get_var_double(fname, ncid, id, tmp_all(1 : nlev, 1 : nprof))
       if (nfw_var_att_exists(ncid,id,'valid_min') .and.nfw_var_att_exists(ncid,id,'valid_max')) then
         call nfw_get_att_double(fname, ncid, id, 'valid_min', varmin)
         call nfw_get_att_double(fname, ncid, id, 'valid_max', varmax)
       else
         varmin=0;  varmax=0
       endif
       
       if (nfw_var_att_exists(ncid, id, 'scale_factor')) then
          if (nfw_var_att_exists(ncid, id, 'fill_value')) then
             call nfw_get_att_double(fname, ncid, id, 'fill_value', fillval(1))
          elseif (nfw_var_att_exists(ncid, id, '_FillValue')) then
             call nfw_get_att_double(fname, ncid, id, '_FillValue', fillval(1))
          endif
          call nfw_get_att_double(fname, ncid, id, 'scale_factor', scalefac)
          call nfw_get_att_double(fname, ncid, id, 'add_offset', addoffset)
       else
          scalefac(1)=1
          addoffset(1)=0
       endif

      if (varmin(1)<varmax(1).and.varmax(1)>0) then
        do f=1,mlev
          tmp_sub(1:nprof)=scalefac(1)*tmp_all(f,1:nprof)+addoffset(1)
          mask_sub(1:nprof)=0
          where (tmp_sub(:)>=varmin(1).and.tmp_sub(:)<=varmax(1))
             mask_sub(:)=1
          endwhere
          where (mask_sub/=1)
            tmp_sub=99999.
          endwhere
          salt_all(f,1:nprof)=tmp_sub(:)
        end do
      else
        do f=1,mlev
          salt_all(f,1:nprof)=scalefac(1)*tmp_all(f,1:nprof)+addoffset(1)
        end do
      endif

       ! psal_qc
       !
       call nfw_inq_varid(fname, ncid, 'PSAL_QC', id)
       call nfw_get_var_text(fname, ncid, id, psal_qc_all(1 : nlev, 1 : nprof))
    else
       psal_qc_all = 'E';
    end if


    deallocate(tmp_all)
    deallocate(mask_sub,tmp_sub)

#if defined (BLACK_PROFILE)
    ! PLATFORM_NUMBER
    call nfw_inq_varid(fname,ncid,'PLATFORM_NUMBER',id)
    call nfw_inq_var_strlen(fname,ncid,id,S1,S2)
    allocate(tmp_str(S1,S2));
    call nfw_get_var_strings(fname,ncid,id,tmp_str,S1,S2)
    if(S1==nprof) then
      do f=1,S1
        write(tmpstr0,*) tmp_str(f,:)
        Pfname(f)=tmpstr0
      end do
    else
      do f=1,S2
        write(tmpstr0,*) tmp_str(:,f)
          Pfname(f)=tmpstr0
      end do
    endif
    deallocate(tmp_str)
!    
#endif

    call nfw_close(fname, ncid)
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

end module  m_read_ifremer_argo
