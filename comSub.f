!
! ---------------------------------------------------------------------
! "THE BEER-WARE LICENSE" (Revision 42):
! CfKu wrote this file. As long as you retain this notice you can do
! whatever you want with this stuff. If we meet some day, and you
! think this stuff is worth it, you can buy me a beer in return.
! Christof Kuestner
! ---------------------------------------------------------------------
!
!######################################################################!
!#### COMMON_SUB MODULE                                                !
!####                                                                  !
!######################################################################!
! Version | Date            | Creator    | Comment                     !
!######################################################################!
! 1.0     | 2012-02-22      | CfKu       | First release               !
!######################################################################!
      MODULE comSub
      IMPLICIT NONE
!######################################################################!
      !Overload countequal (int, double)
      INTERFACE countequal
         module procedure countequal_int
         module procedure countequal_double
      END INTERFACE
      
      !Overload countvalue (int, double, logical)
      INTERFACE countvalue
         module procedure countvalue_int
         module procedure countvalue_double
         module procedure countvalue_logical
      END INTERFACE
      
      !Overload numb2str (int, double)
      INTERFACE numb2str
         module procedure numb2str_int
         module procedure numb2str_double
      END INTERFACE
      
      !Overload pp [pretty print] (scalar, vector, matrix) 
      INTERFACE pp
         module procedure pp_s
         module procedure pp_i
         module procedure pp_f4
         module procedure pp_f
         module procedure pp_b
         module procedure pp_vi
         module procedure pp_vf4
         module procedure pp_vf
         module procedure pp_vb
         module procedure pp_ai
         module procedure pp_a2f4
         module procedure pp_a2f
         module procedure pp_a3f4
         module procedure pp_a3f
         module procedure pp_a4f4
         module procedure pp_a4f
         module procedure pp_ab
      END INTERFACE
      
!######################################################################!
      CONTAINS
!######################################################################!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      FUNCTION countequal_int (viValues) RESULT(res)
      ! compares all values with each other and returns the amaount of matches
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         IMPLICIT NONE
         INTEGER, INTENT(IN)  :: viValues(:)
         INTEGER              :: res
         
         !### VAR ###!
         integer  :: i,j
         
         !### CODE ###!
         res = 0
         do i = lbound(viValues,1),ubound(viValues,1)
            do j = lbound(viValues,1),ubound(viValues,1)
               if ( (i /= j) .and. (viValues(i) == viValues(j)) ) then
                  res = res + 1
               endif
            enddo
         enddo
      
      END FUNCTION

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      FUNCTION countequal_double (vfValues) RESULT(res)
      ! compares all values with each other and returns the amaount of matches
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         IMPLICIT NONE
         REAL(8), INTENT(IN)  :: vfValues(:)
         INTEGER              :: res
         
         !### VAR ###!
         integer  :: i,j
         
         !### CODE ###!
         res = 0
         do i = lbound(vfValues,1),ubound(vfValues,1)
            do j = i+1,ubound(vfValues,1)
               if ( vfValues(i) == vfValues(j) ) then
                  res = res + 1
               endif
            enddo
         enddo
      
      END FUNCTION

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      FUNCTION countvalue_int (viValues, iValue) RESULT(res)
      ! counts the the Value iValue in viValues
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         IMPLICIT NONE
         INTEGER, INTENT(IN)  :: viValues(:)
         INTEGER, INTENT(IN)  :: iValue
         INTEGER              :: res
         
         !### VAR ###!
         integer  :: i
         
         !### CODE ###!
         res = 0
         do i = lbound(viValues,1),ubound(viValues,1)
            if (viValues(i) == iValue) res = res + 1
         enddo
      
      END FUNCTION

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      FUNCTION countvalue_double (vfValues, fValue) RESULT(res)
      ! counts the the Value fValue in vfValues
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         IMPLICIT NONE
         REAL(8), INTENT(IN)  :: vfValues(:)
         REAL(8), INTENT(IN)  :: fValue
         INTEGER              :: res
         
         !### VAR ###!
         integer  :: i
         
         !### CODE ###!
         res = 0
         do i = lbound(vfValues,1),ubound(vfValues,1)
            if (vfValues(i) == fValue) res = res + 1
         enddo
      
      END FUNCTION

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      FUNCTION countvalue_logical (vbValues, bValue) RESULT(res)
      ! counts the the Value bValue in vbValues
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         IMPLICIT NONE
         LOGICAL, INTENT(IN)  :: vbValues(:)
         LOGICAL, INTENT(IN)  :: bValue
         INTEGER              :: res
         
         !### VAR ###!
         integer  :: i
         
         !### CODE ###!
         res = 0
         do i = lbound(vbValues,1),ubound(vbValues,1)
            if (vbValues(i) == bValue) res = res + 1
         enddo
      
      END FUNCTION

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      FUNCTION numb2str_int (iNumb) RESULT(res)
      ! converts an integer to string
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         IMPLICIT NONE
         INTEGER, INTENT(IN)  :: iNumb
         CHARACTER(1024)      :: res
      
         !### VAR ###!
         
         !### CODE ###!
         write(res,*) iNumb
      
      END FUNCTION

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      FUNCTION numb2str_double (fNumb) RESULT(res)
      ! converts an real(8) to string
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         IMPLICIT NONE
         REAL(8), INTENT(IN)  :: fNumb
         CHARACTER(1024)      :: res
         
         !### VAR ###!
         
         !### CODE ###!
         write(res,*) fNumb
      
      END FUNCTION

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE pp_s (sScalar, sCaptionOPT)
      ! prints a scalar (string)
      !     sScalar        scalar as string
      !     sCaptionOPT    [optional] caption
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         IMPLICIT NONE
         CHARACTER(*), INTENT(IN)            :: sScalar
         CHARACTER(*), INTENT(IN), OPTIONAL  :: sCaptionOPT
      
         !### VAR ###!
         character(1024)   :: sCaption
      
         !### CODE ###!
         if (present(sCaptionOPT)) then
            sCaption = sCaptionOPT
         else
            sCaption = 'String; s'
         endif
         
         write (*,'(a,a)') trim(sCaption), ':'
         write (*,'(a)')
     *      sScalar
         write (*,*)
      
      END SUBROUTINE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE pp_i (iScalar, sCaptionOPT)
      ! prints a scalar (integer)
      !     iScalar        scalar as integer
      !     sCaptionOPT    [optional] caption
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         IMPLICIT NONE
         INTEGER, INTENT(IN)                 :: iScalar
         CHARACTER(*), INTENT(IN), OPTIONAL  :: sCaptionOPT
      
         !### VAR ###!
         character(1024)   :: sCaption
      
         !### CODE ###!
         if (present(sCaptionOPT)) then
            sCaption = sCaptionOPT
         else
            sCaption = 'Integer; i'
         endif
         
         write (*,'(a,a)') trim(sCaption), ':'
         write (*,'(i20)')
     *      iScalar
         write (*,*)
      
      END SUBROUTINE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE pp_f4 (fScalar, sCaptionOPT)
      ! prints a scalar (real single)
      !     fScalar        scalar as double
      !     sCaptionOPT    [optional] caption
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         IMPLICIT NONE
         REAL(4), INTENT(IN)                 :: fScalar
         CHARACTER(*), INTENT(IN), OPTIONAL  :: sCaptionOPT
      
         !### VAR ###!
         character(1024)   :: sCaption
      
         !### CODE ###!
         if (present(sCaptionOPT)) then
            sCaption = sCaptionOPT
         else
            sCaption = 'Double; f'
         endif
         
         write (*,'(a,a)') trim(sCaption), ':'
         write (*,'(f20.10)')
     *      fScalar
         write (*,*)
      
      END SUBROUTINE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE pp_f (fScalar, sCaptionOPT)
      ! prints a scalar (real double)
      !     fScalar        scalar as double
      !     sCaptionOPT    [optional] caption
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         IMPLICIT NONE
         REAL(8), INTENT(IN)                 :: fScalar
         CHARACTER(*), INTENT(IN), OPTIONAL  :: sCaptionOPT
      
         !### VAR ###!
         character(1024)   :: sCaption
      
         !### CODE ###!
         if (present(sCaptionOPT)) then
            sCaption = sCaptionOPT
         else
            sCaption = 'Double; f'
         endif
         
         write (*,'(a,a)') trim(sCaption), ':'
         write (*,'(f20.10)')
     *      fScalar
         write (*,*)
      
      END SUBROUTINE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE pp_b (bScalar, sCaptionOPT)
      ! prints a "scalar" (logical)
      !     bScalar        "scalar" as logical
      !     sCaptionOPT    [optional] caption
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         IMPLICIT NONE
         LOGICAL, INTENT(IN)                 :: bScalar
         CHARACTER(*), INTENT(IN), OPTIONAL  :: sCaptionOPT
      
         !### VAR ###!
         character(1024)   :: sCaption
      
         !### CODE ###!
         if (present(sCaptionOPT)) then
            sCaption = sCaptionOPT
         else
            sCaption = 'Logical; b'
         endif
         
         write (*,'(a,a)') trim(sCaption), ':'
         write (*,'(l5)')
     *      bScalar
         write (*,*)
      
      END SUBROUTINE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE pp_vi (viVec, sCaptionOPT)
      ! prints a vector (integer)
      !     viVec          Vector; Dimension (:)
      !     sCaptionOPT    [optional] caption
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         IMPLICIT NONE
         INTEGER, INTENT(IN)                 :: viVec(:)
         CHARACTER(*), INTENT(IN), OPTIONAL  :: sCaptionOPT
      
         !### VAR ###!
         integer           :: i
         character(1024)   :: sCaption
      
         !### CODE ###!
         if (present(sCaptionOPT)) then
            sCaption = sCaptionOPT
         else
            sCaption = 'iVector; vi'
         endif
         
         write (*,'(a,a,i2,a)')
     *      trim(sCaption), ' [ ', size(viVec,1), ' ]:'
         do i=lbound(viVec,1),ubound(viVec,1)
            write (*,'(i20)')
     *         viVec(i)
         enddo
         write (*,*)
      
      END SUBROUTINE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE pp_vf4 (vfVec, sCaptionOPT)
      ! prints a vector (real single)
      !     vfVec          Vector; Dimension (:)
      !     sCaptionOPT    [optional] caption
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         IMPLICIT NONE
         REAL(4), INTENT(IN)                 :: vfVec(:)
         CHARACTER(*), INTENT(IN), OPTIONAL  :: sCaptionOPT
      
         !### VAR ###!
         integer           :: i
         character(1024)   :: sCaption
      
         !### CODE ###!
         if (present(sCaptionOPT)) then
            sCaption = sCaptionOPT
         else
            sCaption = 'fVector; vf'
         endif
         
         write (*,'(a,a,i2,a)')
     *      trim(sCaption), ' [ ', size(vfVec,1), ' ]:'
         do i=lbound(vfVec,1),ubound(vfVec,1)
            write (*,'(f20.10)')
     *         vfVec(i)
         enddo
         write (*,*)
      
      END SUBROUTINE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE pp_vf (vfVec, sCaptionOPT)
      ! prints a vector (real double)
      !     vfVec          Vector; Dimension (:)
      !     sCaptionOPT    [optional] caption
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         IMPLICIT NONE
         REAL(8), INTENT(IN)                 :: vfVec(:)
         CHARACTER(*), INTENT(IN), OPTIONAL  :: sCaptionOPT
      
         !### VAR ###!
         integer           :: i
         character(1024)   :: sCaption
      
         !### CODE ###!
         if (present(sCaptionOPT)) then
            sCaption = sCaptionOPT
         else
            sCaption = 'fVector; vf'
         endif
         
         write (*,'(a,a,i2,a)')
     *      trim(sCaption), ' [ ', size(vfVec,1), ' ]:'
         do i=lbound(vfVec,1),ubound(vfVec,1)
            write (*,'(f20.10)')
     *         vfVec(i)
         enddo
         write (*,*)
      
      END SUBROUTINE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE pp_vb (vbVec, sCaptionOPT)
      ! prints a vector (logical)
      !     vbVec          Vector; Dimension (:)
      !     sCaptionOPT    [optional] caption
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         IMPLICIT NONE
         LOGICAL, INTENT(IN)                 :: vbVec(:)
         CHARACTER(*), INTENT(IN), OPTIONAL  :: sCaptionOPT
      
         !### VAR ###!
         integer           :: i
         character(1024)   :: sCaption
      
         !### CODE ###!
         if (present(sCaptionOPT)) then
            sCaption = sCaptionOPT
         else
            sCaption = 'bVector; vb'
         endif
         
         write (*,'(a,a,i2,a)')
     *      trim(sCaption), ' [ ', size(vbVec,1), ' ]:'
         do i=lbound(vbVec,1),ubound(vbVec,1)
            write (*,'(l5)')
     *         vbVec(i)
         enddo
         write (*,*)
      
      END SUBROUTINE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE pp_ai (aiMat, sCaptionOPT)
      ! prints a matrix(:,:) (integer)
      !     aiMat          Matrix; Dimension (:,:)
      !     sCaptionOPT    [optional] caption
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         IMPLICIT NONE
         INTEGER, INTENT(IN)                 :: aiMat(:,:)
         CHARACTER(*), INTENT(IN), OPTIONAL  :: sCaptionOPT
      
         !### VAR ###!
         integer           :: i
         character(1024)   :: sCaption
      
         !### CODE ###!
         if (present(sCaptionOPT)) then
            sCaption = sCaptionOPT
         else
            sCaption = 'iMatrix; ai'
         endif
         
         write (*,'(a,a,i2,a,i2,a)')
     *      trim(sCaption), ' [ ', size(aiMat,1), 'x', size(aiMat,2),
     *      ' ]:'
         do i=lbound(aiMat,1),ubound(aiMat,1)
            write (*,'(' // numb2str(size(aiMat,1)) // 'i20)')
     *         aiMat(i,:)
         enddo
         write (*,*)
      
      END SUBROUTINE
      
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE pp_a2f4 (afMat, sCaptionOPT)
      ! prints a matrix(:,:) (real single)
      !     afMat          Matrix; Dimension (:,:)
      !     sCaptionOPT    [optional] caption
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         IMPLICIT NONE
         REAL(4), INTENT(IN)                 :: afMat(:,:)
         CHARACTER(*), INTENT(IN), OPTIONAL  :: sCaptionOPT
      
         !### VAR ###!
         integer           :: i
         character(1024)   :: sCaption
      
         !### CODE ###!
         if (present(sCaptionOPT)) then
            sCaption = sCaptionOPT
         else
            sCaption = 'fMatrix; af'
         endif
         
         write (*,'(a,2(a,i2),a)')
     *      trim(sCaption), ' [ ', size(afMat,1), 'x', size(afMat,2),
     *      ' ]:'
         do i=lbound(afMat,1),ubound(afMat,1)
            write (*,'(' // numb2str(size(afMat,1)) // 'f20.10)')
     *         afMat(i,:)
         enddo
         write (*,*)
      
      END SUBROUTINE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE pp_a2f (afMat, sCaptionOPT)
      ! prints a matrix(:,:) (real double)
      !     afMat          Matrix; Dimension (:,:)
      !     sCaptionOPT    [optional] caption
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         IMPLICIT NONE
         REAL(8), INTENT(IN)                 :: afMat(:,:)
         CHARACTER(*), INTENT(IN), OPTIONAL  :: sCaptionOPT
      
         !### VAR ###!
         integer           :: i
         character(1024)   :: sCaption
      
         !### CODE ###!
         if (present(sCaptionOPT)) then
            sCaption = sCaptionOPT
         else
            sCaption = 'fMatrix; af'
         endif
         
         write (*,'(a,2(a,i2),a)')
     *      trim(sCaption), ' [ ', size(afMat,1), 'x', size(afMat,2),
     *      ' ]:'
         do i=lbound(afMat,1),ubound(afMat,1)
            write (*,'(' // numb2str(size(afMat,1)) // 'f20.10)')
     *         afMat(i,:)
         enddo
         write (*,*)
      
      END SUBROUTINE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE pp_a3f4 (afMat, sCaptionOPT)
      ! prints a matrix(:,:,:) (real single)
      !     afMat          Matrix; Dimension (:,:,:)
      !     sCaptionOPT    [optional] caption
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         IMPLICIT NONE
         REAL(4), INTENT(IN)                 :: afMat(:,:,:)
         CHARACTER(*), INTENT(IN), OPTIONAL  :: sCaptionOPT
      
         !### VAR ###!
         integer           :: i,j
         character(1024)   :: sCaption
      
         !### CODE ###!
         if (present(sCaptionOPT)) then
            sCaption = sCaptionOPT
         else
            sCaption = 'fMatrix; af'
         endif
         
         write (*,'(a,3(a,i2),a)')
     *      trim(sCaption), ' [ ', size(afMat,1), 'x', size(afMat,2),
     *      'x', size(afMat,3), ' ]:'
         do j=lbound(afMat,3),ubound(afMat,3)
            write (*,'(1(a,i2))') ' >> :, :,', j
            do i=lbound(afMat,1),ubound(afMat,1)
               write (*,'(' // numb2str(size(afMat,1)) // 'f20.10)')
     *            afMat(i,:,j)
            enddo
            write (*,*)
         enddo
      
      END SUBROUTINE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE pp_a3f (afMat, sCaptionOPT)
      ! prints a matrix(:,:,:) (real double)
      !     afMat          Matrix; Dimension (:,:,:)
      !     sCaptionOPT    [optional] caption
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         IMPLICIT NONE
         REAL(8), INTENT(IN)                 :: afMat(:,:,:)
         CHARACTER(*), INTENT(IN), OPTIONAL  :: sCaptionOPT
      
         !### VAR ###!
         integer           :: i,j
         character(1024)   :: sCaption
      
         !### CODE ###!
         if (present(sCaptionOPT)) then
            sCaption = sCaptionOPT
         else
            sCaption = 'fMatrix; af'
         endif
         
         write (*,'(a,3(a,i2),a)')
     *      trim(sCaption), ' [ ', size(afMat,1), 'x', size(afMat,2),
     *      'x', size(afMat,3), ' ]:'
         do j=lbound(afMat,3),ubound(afMat,3)
            write (*,'(1(a,i2))') ' >> :, :,', j
            do i=lbound(afMat,1),ubound(afMat,1)
               write (*,'(' // numb2str(size(afMat,1)) // 'f20.10)')
     *            afMat(i,:,j)
            enddo
            write (*,*)
         enddo
      
      END SUBROUTINE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE pp_a4f4 (afMat, sCaptionOPT)
      ! prints a matrix(:,:,:,:) (real single)
      !     afMat          Matrix; Dimension (:,:,:,:)
      !     sCaptionOPT    [optional] caption
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         IMPLICIT NONE
         REAL(4), INTENT(IN)                 :: afMat(:,:,:,:)
         CHARACTER(*), INTENT(IN), OPTIONAL  :: sCaptionOPT
      
         !### VAR ###!
         integer           :: i,j,k
         character(1024)   :: sCaption
      
         !### CODE ###!
         if (present(sCaptionOPT)) then
            sCaption = sCaptionOPT
         else
            sCaption = 'fMatrix; af'
         endif
         
         write (*,'(a,4(a,i2),a)')
     *      trim(sCaption), ' [ ', size(afMat,1), 'x', size(afMat,2),
     *      'x', size(afMat,3), 'x', size(afMat,4), ' ]:'
         do k=lbound(afMat,4),ubound(afMat,4)
            do j=lbound(afMat,3),ubound(afMat,3)
               write (*,'(2(a,i2))') ' >> :, :,',j,',',k
               do i=lbound(afMat,1),ubound(afMat,1)
                  write (*,'(' // numb2str(size(afMat,1)) // 'f20.10)')
     *               afMat(i,:,j,k)
               enddo
               write (*,*)
            enddo
         enddo
      
      END SUBROUTINE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE pp_a4f (afMat, sCaptionOPT)
      ! prints a matrix(:,:,:,:) (real double)
      !     afMat          Matrix; Dimension (:,:,:,:)
      !     sCaptionOPT    [optional] caption
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         IMPLICIT NONE
         REAL(8), INTENT(IN)                 :: afMat(:,:,:,:)
         CHARACTER(*), INTENT(IN), OPTIONAL  :: sCaptionOPT
      
         !### VAR ###!
         integer           :: i,j,k
         character(1024)   :: sCaption
      
         !### CODE ###!
         if (present(sCaptionOPT)) then
            sCaption = sCaptionOPT
         else
            sCaption = 'fMatrix; af'
         endif
         
         write (*,'(a,4(a,i2),a)')
     *      trim(sCaption), ' [ ', size(afMat,1), 'x', size(afMat,2),
     *      'x', size(afMat,3), 'x', size(afMat,4), ' ]:'
         do k=lbound(afMat,4),ubound(afMat,4)
            do j=lbound(afMat,3),ubound(afMat,3)
               write (*,'(2(a,i2))') ' >> :, :,',j,',',k
               do i=lbound(afMat,1),ubound(afMat,1)
                  write (*,'(' // numb2str(size(afMat,1)) // 'f20.10)')
     *               afMat(i,:,j,k)
               enddo
               write (*,*)
            enddo
         enddo
      
      END SUBROUTINE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE pp_ab (abMat, sCaptionOPT)
      ! prints a matrix(:,:) (logical)
      !     abMat          Matrix; Dimension (:,:)
      !     sCaptionOPT    [optional] caption
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         IMPLICIT NONE
         LOGICAL, INTENT(IN)                 :: abMat(:,:)
         CHARACTER(*), INTENT(IN), OPTIONAL  :: sCaptionOPT
      
         !### VAR ###!
         integer           :: i
         character(1024)   :: sCaption
      
         !### CODE ###!
         if (present(sCaptionOPT)) then
            sCaption = sCaptionOPT
         else
            sCaption = 'iMatrix; ai'
         endif
         
         write (*,'(a,a,i2,a,i2,a)')
     *      trim(sCaption), ' [ ', size(abMat,1), 'x', size(abMat,2),
     *      ' ]:'
         do i=lbound(abMat,1),ubound(abMat,1)
            write (*,'(' // numb2str(size(abMat,1)) // 'l5)')
     *         abMat(i,:)
         enddo
         write (*,*)
      
      END SUBROUTINE
!######################################################################!
      END MODULE comSub
!######################################################################!
