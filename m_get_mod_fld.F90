module m_get_mod_fld
! KAL -- This routine reads one of the fields from the model, specified
! KAL -- by name, vertical level and time level 
! KAL -- This routine is really only effective for the new restart files.
#if defined BIORAN
  ! default settings of BGC Box-Cox transformation
  !
  logical :: lognormal = .true.
#endif  
contains
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

subroutine get_mod_fld(fld,j,cfld,vlevel,tlevel,nx,ny)
#if defined (QMPI)
   use qmpi
#else
   use qmpi_fake
#endif
   implicit none
   integer,      intent(in)            :: nx,ny  ! Grid dimension
   integer,      intent(in)            :: j      ! Ensemble member to read
   real, dimension(nx,ny), intent(out) :: fld    ! output fld
   character(len=*), intent(in)        :: cfld   ! name of fld
   integer, intent(in)                 :: vlevel ! vertical level
   integer, intent(in)                 :: tlevel ! time level

   integer reclICE
   real*8, dimension(nx,ny) :: ficem,hicem,hsnwm,ticem,tsrfm

   logical ex

   character(len=*),parameter :: icefile='forecastICE.uf'

   ! KAL -- shortcut -- the analysis is for observation icec -- this little "if" 
   ! means the  analysis will only work for ice. Add a check though
   if ((trim(cfld)/='icec' .and. trim(cfld)/='hice')  .or. vlevel/=0 .or. tlevel/=1)then
      if (master) print *,'get_mod_fld only works for icec for now '//trim(cfld)
      stop
   end if

!###################################################################
!####################### READ  ICE   MODEL #########################
!###################################################################
#if defined (ICE)
#warning "COMPILING WITH ICE"
   inquire(exist=ex,file=icefile)
   if (.not.ex) then
      if (master) then
         print *,icefile//' does not exist!'
         print *,'(get_mod_fld)'
      end if
      stop
   end if
   inquire(iolength=reclICE)ficem,hicem,hsnwm,ticem,tsrfm  !,iceU,iceV
   open(10,file=icefile,form='unformatted',access='direct',recl=reclICE,action='read')
      read(10,rec=j)ficem,hicem,hsnwm,ticem,tsrfm !,iceU,iceV
      if (trim(cfld)=='icec') fld = ficem
      if (trim(cfld)=='hice') fld = hicem
   close(10)
#else
#warning "COMPILING WITHOUT ICE"
#endif


  return
end subroutine get_mod_fld



! KAL - This is for the new file type
subroutine get_mod_fld_new(memfile,fld,iens,cfld0,vlevel,tlevel,nx,ny,Indfield)
   use mod_raw_io
   use m_get_mod_fld_nc
#if defined (QMPI)
   use qmpi, only : qmpi_proc_num, master,stop_mpi
#else
   use qmpi_fake
#endif
   implicit none
   integer,      intent(in)            :: nx,ny  ! Grid dimension
   integer,      intent(in)            :: iens   ! Ensemble member to read
   real, dimension(nx,ny), intent(out) :: fld    ! output fld
   character(len=*), intent(in)        :: memfile! base name of input files
   character(len=*), intent(in)        :: cfld0   ! name of fld
   integer, intent(in)                 :: vlevel ! vertical level
   integer, intent(in)                 :: tlevel ! time level
   integer, intent(in)                 :: Indfield ! index in the analysisfields

   real*8, dimension(nx,ny) :: readfldr8
   real*4, dimension(nx,ny) :: readfldr4
   real*4:: amin, amax,spval
   real :: bmin, bmax
   integer :: indx
!----------------------------------- 
   character*80 :: cfld,icefile
   
   cfld=trim(cfld0)
#if defined (HYCOM_CICE)
   if (trim(cfld0) == 'SSH') then
      cfld='srfhgt'
   end if
#endif
#if defined (SINGLE_RESTART)
   if (trim(cfld0) == 'icec') then
     cfld='ficem'
   elseif (trim(cfld0) == 'hice') then
     cfld='hicem'
   endif
#endif 
!----------------------------------- 
   ! Dette fordi is-variablane forelobig er paa gammalt format.
   if (trim(cfld) /= 'icec' .and. trim(cfld) /= 'hice' .and. &
       trim(cfld) /= 'hsnwm' .and. trim(cfld) /= 'aicen' .and. &
       trim(cfld) /= 'vicen' .and. trim(cfld) /= 'vsnon' .and. &
       trim(cfld) /= 'ticem' .and. trim(cfld) /= 'tsrfm' .and. & 
       trim(cfld) /= 'sicem' .and. trim(cfld) /= 'tsnom' .and. & 
       trim(cfld) /= 'ficem' .and. trim(cfld) /= 'hicem') then

      ! KAL - 1) f kva index som skal lesast finn vi fraa .b fil (header)
      ! working well for restart file
      indx=0;
      if (Indfield>=0) then
          call rst_index_from_header(trim(memfile)//'.b', & ! filnavn utan extension
                                 cfld               , & ! felt som skal lesast fex saln,temp
                                 vlevel,              & ! vertikalnivaa
                                 tlevel,              & ! time level - kan vere 1 eller 2 - vi bruker 1 foreloepig
                                 indx,                & ! indexen som maa lesas fra data fila
                                 bmin,bmax,           & ! min/max - kan sjekkast mot det som er i datafila
                                 .true. )
      else
          call daily_index_from_header(trim(memfile)//'.b', & ! filnavn utan extension
                                 cfld               , & ! felt som skal lesast fex saln,temp
                                 vlevel,              & ! vertikalnivaa
                                 indx,                & ! indexen som maa lesas fra data fila
                                 bmin,bmax,iens)        ! min/max - kan sjekkast mot det som er i datafila

      endif

      if (indx < 0) then
         if (master) then
            print *, 'ERROR: get_mod_fld_new() to read ', trim(memfile), '.b: "',&
                 trim(cfld), '" not found for ',indx
         end if
         stop
      end if

      ! KAL -- les datafelt vi fann fraa header fila (indx)
      spval=2**100
      call READRAW(readfldr4          ,& ! Midlertidig felt som skal lesast
                   amin, amax         ,& ! max/min fraa data (.a) fila 
                   nx,ny              ,& ! dimensjonar
                   .true.,spval      ,& ! dette brukast for  sette "no value" verdiar
                   trim(memfile)//'.a',& ! fil som skal lesast fraa
                   indx)                 ! index funne over

     ! Sjekk p at vi har lest rett - samanlign max/min fr filene
     if     (abs(amin-bmin).gt.abs(bmin)*1.e-4 .or. &
             abs(bmax-amax).gt.abs(bmax)*1.e-4     ) then
        print *,'Inconsistency between .a and .b files (m_get_mod_fld)'
        print *,'.a : ',amin,amax
        print *,'.b : ',bmin,bmax
        print *,trim(cfld),vlevel,tlevel, indx
        print *,'node ',qmpi_proc_num
        call exit(1)
     end if
#if defined(BIORAN)
     if (lognormal) then
        !
        ! [2019.10.04] TW
        !   Following variable list should be cap of list in analysisfields.in
        ! [2015.06.25] TW
        !   Following variable list should be matched with the list in m_put_mod_fld.F90
        !
        if ( trim(cfld) == 'ECO_fla'  &  ! FLA
        .or. trim(cfld) == 'ECO_dia'  &  ! DIA
        .or. trim(cfld) == 'ECO_micr' &  ! MICRO
        .or. trim(cfld) == 'ECO_meso' &  ! MESO
        .or. trim(cfld) == 'ECO_no3'  &  ! NIT
        .or. trim(cfld) == 'ECO_pho'  &  ! PHO
        .or. trim(cfld) == 'ECO_sil'  &  ! SIL
        .or. trim(cfld) == 'ECO_oxy'  &  ! OXY
        .or. trim(cfld) == 'ECO_flac' &  ! CHLFLA
        .or. trim(cfld) == 'ECO_diac' &  ! CHLDIA
        .or. trim(cfld) == 'ECO_cclc' &  ! CHLCCL
        ) then
           ! Forward Box-Cox transformation (FBCT). See m_put_mod_fld.F90 for the Inverse BCT (IBCT).
           ! [2019.06.07] TW
           !   So far, only the case: lambda=0 (Log-Normal transformation) is available.
           !   Targeted variables should be selected here, but we transform all listed in analysisfields.in so far.
           fld=log(readfldr4)
        else
           fld=readfldr4
        endif
     else
        fld=readfldr4
     endif
#else     
     fld=readfldr4
#endif

   else ! fld = fice, hice
      ! Gammal rutine ja
#if defined(HYCOM_CICE)
      indx=-1
      icefile=trim(memfile)//'.nc'
      call get_mod_fld_nc(trim(icefile), readfldr8, &
                  cfld, vlevel, indx, nx, ny)
      if (indx==-1) then
         icefile='ice_'//trim(memfile)//'.nc'
         call get_mod_fld_nc(trim(icefile), readfldr8, &
                  cfld, vlevel, indx, nx, ny)
      end if
      if (indx < 0) then
         if (master) then
            print *, 'ERROR: get_mod_fld_new(): ', trim(icefile),&
                 trim(cfld), '" not found for ', vlevel
            stop
         end if
         stop
!      else
!         print *, trim(icefile), ' '//trim(cfld), vlevel,Indfield
      end if

#else
      call get_mod_fld(readfldr8,iens,cfld,0,1,nx,ny)
#endif
      fld=readfldr8
   end if


end subroutine



end module m_get_mod_fld


