! File:        m_get_mod_cnfg.F90
!
! Created:     Novermber 2018
!
! Author:      T.Wakamatsu (tsuyoshi.wakamatsu@nersc.no)
!
! Purpose:     Read hycom model grid and depth data
!
! Description: This file is designed based on the NERSC hycom utility:
!
!                hycom/MSCPROGS/src/Nersclib/mod_grid.F90
!
!              Read modlon, modlat and depths from hycom configuration files:
!
!                regional.grid.a
!                regional.depth.a
!
!              Note the two utility modules need to be linked at its compilation. See make.inc.
!
!              List of variables in regional.grid.a:
!
!                   1 plon:
!                   2 plat:
!                   3 qlon:
!                   4 qlat:
!                   5 ulon:
!                   6 ulat:
!                   7 vlon:
!                   8 vlat:
!                   9 pang: rotation angle of x-grid direction rel to local lat line
!                  10 scpx: zonal grid distance at p-point
!                  11 scpy: meridional grid distance at p-point
!                  12 scqx: zonal grid distance at q-point (vorticity)
!                  13 scqy: meridional grid distance at q-point
!                  14 scux: zonal grid distance at u-point
!                  15 scuy: meridional grid distance at u-point
!                  16 scvx: zonal grid distance at v-point
!                  17 scvy: meridional grid distance at v-point
!                  18 cori: Coriolis parameter at q-point
!                  19 pasp: v-grid aspect ratios for diffusion (=scpx/scpy)
!
!              List of variables in regional.depth.a:
!
!                   1 depth: 
!
! History:     
!
module m_get_mod_cnfg
  
  implicit none
  logical, parameter, private :: VERBOSE = .false.

contains 

subroutine get_mod_cnfg(plon,plat,depth,mindx,meandx,idm,jdm,lprntmsk)
#if defined (QMPI)
   use qmpi
#else
   use qmpi_fake
#endif
   use m_io_hycom
   implicit none

   integer, intent(in) :: idm,jdm
   logical, intent(in) :: lprntmsk

   real*8, dimension(idm,jdm), intent(out) :: plon, plat, depth
   real,                       intent(out) :: mindx, meandx

   integer :: i, j

   real(kind=4) :: wmin, wmax, dxmin, dxmax, dymin, dymax
   real(kind=4), dimension(:,:), allocatable, save :: var2D, scpy, scpx
   integer, dimension(:,:), allocatable, save :: lmask
   !real(kind=4), dimension(:,:), allocatable, save :: mask, var2D, scpy, scpx

   character(len=10) :: fmt, str

   logical :: exa, exa2
   character(len=512) :: fname, fname2
   integer :: nop=120, ios, nrecl

   allocate(var2D(idm,jdm))
   allocate(scpx(idm,jdm))
   allocate(scpy(idm,jdm))
   allocate(lmask(idm,jdm))

   !-- input files

   fname='regional.grid.a'
   fname2='regional.depth.a'

   !-- Read position from model files

   call open_hycom_var2d(nop,fname,idm,jdm,'old')
   call read_hycom_var2d(nop,var2d,wmax,wmin,idm,jdm,1)  ! plon
   plon = DBLE(var2d)
   call read_hycom_var2d(nop,var2d,wmax,wmin,idm,jdm,2)  ! plat
   plat = DBLE(var2d)
   call close_hycom_var2d(nop)

   !-- Read depths from model files

   call open_hycom_var2d(nop,fname2,idm,jdm,'old')
   call read_hycom_var2d(nop,var2d,wmax,wmin,idm,jdm,1) ! depth
   depth = DBLE(var2d)
   call close_hycom_var2d(nop)

   !-- Print land mask data (0:land, 1:ocean)

   if (lprntmsk) then

   fmt = '(144i1.1)'

   write(*,*) '-------------------------------'
   write(*,*) ' land mask'
   write(*,*) '-------------------------------'
   lmask = INT2(depth)
   do i = 1, idm
     write(*,TRIM(fmt)) lmask(i,1:144)
   enddo
   write(*,*) '-------------------------------'

   endif

   !-- Check grid size

   call open_hycom_var2d(nop,fname,idm,jdm,'old')
   call read_hycom_var2d(nop,var2d,dxmax,dxmin,idm,jdm,10) ! scpx
   scpx = var2d
   call read_hycom_var2d(nop,var2d,dymax,dymin,idm,jdm,11) ! scpy
   scpy = var2d
   call close_hycom_var2d(nop)

   mindx  = min(dxmin, dymin)
   meandx = sum(scpx, mask = depth > 1.0d0 .and. depth < 1.0d25) / real(count(depth > 1.0d0 .and. depth < 1.0d25))

   if (master .and. VERBOSE) then
    print *,'MINIMUM grid size from scpx/scpy : ',mindx
    print *,'   MEAN grid size from scpx/scpy : ',meandx
   end if

   ! Safety check ..
   if (mindx<2000.) then
      if (master .and. VERBOSE) then
         print *,'min grid size lower than safety threshold - fix if you want'
         print *,'(get_mod_cnfg)'
      end if
      call stop_mpi()
   end if

   ! Safety check .. This one is not that critical so the value is set high
   if (mindx>500000.) then
      if (master .and. VERBOSE) then
         print *,'min grid size higher than safety threshold - fix if you want'
         print *,'(get_mod_cnfg)'
      end if
      call stop_mpi()
   end if

   !--

   deallocate(var2d,scpx,scpy,lmask)
   
end subroutine  get_mod_cnfg

end module  m_get_mod_cnfg
