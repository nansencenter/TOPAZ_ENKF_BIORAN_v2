module m_put_mod_fld
! KAL -- This routine puts one of the fields to the restart file, specified
! KAL -- by name, vertical level and time level. 
! KAL -- Its a bit dangerous to use -- indx must be updated correctly (max one 
! KAL -- increment per call of this routine), otherwise there wil be a 
! KAL -- inconsistency between .a and .b files
#if defined BIORAN
  ! default settings of BGC Box-Cox transformation
  !
  logical :: lognormal = .true.
#endif  
contains
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


! KAL - This is for the new file type
subroutine put_mod_fld(memfile,fld,iens,cfld,vlevel,tlevel,indx,nx,ny)
   use mod_raw_io
   implicit none
   integer, intent(in) :: nx,ny
   integer,                intent(in)  :: iens   ! Ensemble member to read
   real, dimension(nx,ny), intent(in)  :: fld    ! output fld
   character(len=*),       intent(in)  :: memfile! base name of input files
   character(len=8),       intent(in)  :: cfld   ! name of fld
   integer,                intent(in)  :: tlevel ! time level
   integer,                intent(in)  :: vlevel ! vertical level
   integer,                intent(in)  :: indx   ! index into file

   real*4:: amin, amax,spval
   real*4:: writefldr4(nx,ny)
   real*4:: writefldr4_tmpl(nx,ny)
   integer , parameter :: nop=123
   integer :: ios

   spval=2**100

#if defined BIORAN
   if (lognormal) then
      !
      ! [2019.10.04] TW
      !   Following variable list should be cap of list in analysisfields.in
      ! [2015.06.25] TW
      !   Following variable list should be matched with the list in m_get_mod_fld.F90
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
         ! Inverse Box-Cox transformation (IBCT). See m_get_mod_fld.F90 for the Forward BCT (FBCT).
         ! [2019.06.07] TW
         !   So far, only the case: lambda=0 (Log-Normal transformation) is available.
         !   Targeted variables should be selected here, but we transform all listed in analysisfields.in so far.
         writefldr4_tmpl = real(fld, 4)
         writefldr4 = exp(writefldr4_tmpl)
      else
         writefldr4 = real(fld, 4)
      endif
   else
      writefldr4 = real(fld, 4)
   endif
#else
   writefldr4 = real(fld, 4)
#endif  
   ! for debug
   !print *, 'Dump: '//trim(memfile)//' '//trim(cfld), vlevel, tlevel,indx

   ! Write fld into index of .a file -- Direct access file
   !call writeraw(writefldr4,          &! feltet som skal skrivast
   !              amin,amax,           &! min og max (returnerast fra writeraw)
   !              nx,ny,               &! dimensjon
   !              .false.,spval,       &! feltet vil ikkje faa "no-value" verdiar
   !              memfile//'.a',       &! forelopig filnavn...
   !              indx)                 ! indexen i .a fila

   call writeraw(writefldr4,          &! feltet som skal skrivast
                 amin,amax,           &! min og max (returnerast fra writeraw)
                 nx,ny,               &! dimensjon
                 .true.,spval,       &! feltet vil ikkje faa "no-value" verdiar
                 memfile//'.a',       &! forelopig filnavn...
                 indx)                 ! indexen i .a fila

   ! Skriv header -- .b fil -- Hold tunga rett i munnen her og utanfor rutina, 
   ! ellers blir det inkonsistens mellom .a og .b filer - dette vil fangast opp 
   ! av postprosessering. Dette er tungvint men vanskelig aa omgaa fordi
   ! .b-fila er sekvensiell mens .a fila er direct access.
   if (indx==1) then
      ! forste indeks - vi overskriv evt gamle filer
      open(nop,file=memfile//'.b',status='replace')
   else
      ! Ellers legg vi til 
      open(nop,file=memfile//'.b',status='old', position='append')
   end if

   ! Skriv i vei !
   write(nop,4100,iostat=ios) cfld,vlevel,tlevel,amin,amax
   close(nop)

4100  format(a,': layer,tlevel,range = ',i3,i3,2x,1p2e16.7)
!     format (a8,23x,i3,i3,2x,2e16.7)


end subroutine



end module m_put_mod_fld


