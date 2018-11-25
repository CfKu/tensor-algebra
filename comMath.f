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
!#### COMMON_MATH MODULE                                               !
!####                                                                  !
!######################################################################!
! Version | Date            | Creator    | Comment                     !
!######################################################################!
! 1.0     | 2012-02-22      | CfKu       | First release               !
!######################################################################!
      MODULE comMath
      USE comSub
      IMPLICIT NONE
      
!######################################################################!
      !Overload round
      INTERFACE round
         module procedure round_scalar
         module procedure round_vec
         module procedure round_mat
      END INTERFACE

      !Overload trace
      INTERFACE tr
         module procedure trace_int
         module procedure trace_double
      END INTERFACE

      !Overload extract
      INTERFACE extract
         module procedure extract3_6x1_t2f
         module procedure extract3_6x6_t4f
      END INTERFACE
      
      !Overload contract
      INTERFACE contract
         module procedure contract3_t2f_6x1
         module procedure contract3_t4f_6x6
      END INTERFACE
      
      !Overload spectral representation
      INTERFACE spec
         module procedure spectral_ord2
      END INTERFACE
      
      !Overload in1 (first inner product)
      INTERFACE in1
         module procedure in1_ord11
         module procedure in1_ord12
         module procedure in1_ord21
         module procedure in1_ord22
         module procedure in1_ord222
      END INTERFACE

      !Overload in2 (second inner product)
      INTERFACE in2
         module procedure in2_ord22
         module procedure in2_ord24
         module procedure in2_ord42
         module procedure in2_ord44
         module procedure in2_ord444
      END INTERFACE
      
      !Overload dyad (outer product / dyadic product)
      INTERFACE dyad
         module procedure dyad_ord11
         module procedure dyad_ord22
      END INTERFACE

      !Overload inv (matrix inversion)
      INTERFACE inv_sym
         module procedure inv_6x6_sym
      END INTERFACE
      
!######################################################################!
      CONTAINS
!######################################################################!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      FUNCTION round_scalar(fNumb, iDigits) RESULT(res)
      ! rounds fNumb to iDigits decimal digits (scalar)
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         IMPLICIT NONE
         REAL(8), INTENT(IN)  :: fNumb
         INTEGER, INTENT(IN)  :: iDigits
         REAL(8)  :: res
         
         !### VAR ###!
         integer :: iMulDiv
         
         !### CODE ###!
         iMulDiv = 10 ** iDigits
         res = REAL(INT(fNumb * iMulDiv + 0.5d0),8) / iMulDiv
         
      END FUNCTION

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      FUNCTION round_vec(vfNumb, iDigits) RESULT(res)
      ! rounds vfNumb to iDigits decimal digits (vector)
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         IMPLICIT NONE
         REAL(8), INTENT(IN)  :: vfNumb(:)
         INTEGER, INTENT(IN)  :: iDigits
         REAL(8)  :: res(size(vfNumb,1))
         
         !### VAR ###!
         integer :: iMulDiv
         
         !### CODE ###!
         iMulDiv = 10 ** iDigits
         res = REAL(INT(vfNumb * iMulDiv + 0.5d0),8) / iMulDiv
         
      END FUNCTION

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      FUNCTION round_mat(afNumb, iDigits) RESULT(res)
      ! rounds afNumb to iDigits decimal digits (vector)
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         IMPLICIT NONE
         REAL(8), INTENT(IN)  :: afNumb(:,:)
         INTEGER, INTENT(IN)  :: iDigits
         REAL(8)  :: res(size(afNumb,1),size(afNumb,2))
         
         !### VAR ###!
         !### VAR ###!
         integer :: iMulDiv
         
         !### CODE ###!
         iMulDiv = 10 ** iDigits
         res = REAL(INT(afNumb * iMulDiv + 0.5d0),8) / iMulDiv
         
      END FUNCTION

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      FUNCTION unit2(iDm) RESULT(res)
      ! returns an 2nd order unit tensor of dimension iDm (double)
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         IMPLICIT NONE
         INTEGER, INTENT(IN) :: iDm
         REAL(8)             :: res (iDm,iDm)
      
         !### VAR ###!
         integer :: i
         
         !### CODE ###!
         res = 0.0d0
         do i = 1,iDm
            res(i,i) = 1.0d0
         enddo
      
      END FUNCTION

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      FUNCTION unit4 (iDm) RESULT(res)
      ! returns the 4th order symetric unit tensor of dimension iDm (double)
      ! res_ijkl = delta_ik * delta_jl
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         IMPLICIT NONE
         INTEGER, INTENT(IN)  :: iDm
         REAL(8)              :: res (iDm,iDm,iDm,iDm)
         
         !### VAR ###!
         integer  :: i,j,k,l
         real(8)  :: t2f_u2(iDm,iDm)
         
         !### CODE ###!
         t2f_u2 = unit2(iDm)
         do l = 1,iDm
            do k = 1,iDm
               do j = 1,iDm
                  do i = 1,iDm
                     res(i,j,k,l) = t2f_u2(i,k) * t2f_u2(j,l)
                  enddo
               enddo
            enddo
         enddo
      
      END FUNCTION

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      FUNCTION unit4_sym (iDm) RESULT(res)
      ! returns the 4th order symetric unit tensor of dimension iDm (double)
      ! res_ijkl = 0.5 * ( delta_ik * delta_jl + delta_jk * delta_il )
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         IMPLICIT NONE
         INTEGER, INTENT(IN)  :: iDm
         REAL(8)              :: res (iDm,iDm,iDm,iDm)
         
         !### VAR ###!
         integer  :: i,j,k,l
         real(8)  :: t2f_u2(iDm,iDm)
         
         !### CODE ###!
         t2f_u2 = unit2(iDm)
         do l = 1,iDm
            do k = 1,iDm
               do j = 1,iDm
                  do i = 1,iDm
                     res(i,j,k,l) = 0.5d0 * ( t2f_u2(i,k) * t2f_u2(j,l) 
     *                  + t2f_u2(j,k) * t2f_u2(i,l) )
                  enddo
               enddo
            enddo
         enddo
      
      END FUNCTION

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      FUNCTION kronckerdelta (iDm1,iDm2) RESULT(res)
      ! returns the kronecker delta
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         IMPLICIT NONE
         INTEGER, INTENT(IN) :: iDm1, iDm2
         REAL(8)             :: res
      
         !### VAR ###!
         
         !### CODE ###!
         if (iDm1 == iDm2) then
            res = 1.0d0
         else
            res = 0.0d0
         endif
      
      END FUNCTION

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      FUNCTION levicivita (iDmI,iDmJ,iDmK,iDmL) RESULT(res)
      ! returns the Levi-Civita-Symbol (permutation symbol)
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         IMPLICIT NONE
         INTEGER, INTENT(IN)           :: iDmI, iDmJ, iDmK
         INTEGER, INTENT(IN), OPTIONAL :: iDmL
         REAL(8)                       :: res
      
         !### VAR ###!
         
         !### CODE ###!
         if (present(iDmL)) then
            res = (1.0d0 / 12.0d0) *
     *         (iDmI - iDmJ) * (iDmI - iDmK) *
     *         (iDmI - iDmL) * (iDmJ - iDmK) *
     *         (iDmJ - iDmL) * (iDmK - iDmL)
         else
            res = 0.5d0 * (iDmI - iDmJ) * (iDmJ - iDmK) * (iDmK - iDmI)
         endif
      
      END FUNCTION

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      FUNCTION spectral_ord2 (vfEW,afEV) RESULT(res)
      ! spectral representation with vfEW (eigenvalues) and afEV (eigenvectors)
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         IMPLICIT NONE
         REAL(8), INTENT(IN)  :: vfEW(3)
         REAL(8), INTENT(IN)  :: afEV(3,3)
         REAL(8)              :: res(3,3)
      
         !### VAR ###!
         integer  :: i
         real(8)  :: t2f_EVbases (3,3)
         
         !### CODE ###!
         res = 0.0d0
         do i = 1,3
            call dyad(t2f_EVbases, afEV(:,i), afEV(:,i),3)
            res = res + vfEW(i) * t2f_EVbases
         enddo
      
      END FUNCTION

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      FUNCTION trace_int (afMat) RESULT(res)
      ! returns the the trace of afMat [INTEGER]
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         IMPLICIT NONE
         INTEGER, INTENT(IN)  :: afMat(:,:)
         INTEGER              :: res
      
         !### VAR ###!
         integer  :: i
         
         !### CODE ###!
         res = 0
         do i=1,min(size(afMat,1), size(afMat,2))
            res = res + afMat(i,i)
         enddo
      
      END FUNCTION

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      FUNCTION trace_double (afMat) RESULT(res)
      ! returns the the trace of afMat [DOUBLE]
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         IMPLICIT NONE
         REAL(8), INTENT(IN)  :: afMat(:,:)
         REAL(8)              :: res
      
         !### VAR ###!
         integer  :: i
         
         !### CODE ###!
         res = 0.0d0
         do i=1,min(size(afMat,1), size(afMat,2))
            res = res + afMat(i,i)
         enddo
      END FUNCTION
      
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE extract3_6x1_t2f (res, vfVec)
      ! extracts a 6x1 vector: res(3x3) = vfVec(6) [3rd dimension]
      !     vfVec          Vector; Dimension (6)
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         IMPLICIT NONE
         REAL(8), INTENT(OUT) :: res (3,3)
         REAL(8), INTENT(IN)  :: vfVec (6)
         
         !### VAR ###!
         
         !### CODE ###!
         res(1,1) = vfVec(1)
         res(2,1) = vfVec(4)
         res(3,1) = vfVec(6)
         
         res(1,2) = vfVec(4)
         res(2,2) = vfVec(2)
         res(3,2) = vfVec(5)
         
         res(1,3) = vfVec(6)
         res(2,3) = vfVec(5)
         res(3,3) = vfVec(3)
      END SUBROUTINE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE extract3_6x6_t4f (res, afMat)
      ! extracts a 6x1 vector: res(3x3x3x3) = afMat(6x6) [3rd dimension]
      !     afMat          Matrix; Dimension (6x6)
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         IMPLICIT NONE
         REAL(8), INTENT(OUT) :: res (3,3,3,3)
         REAL(8), INTENT(IN)  :: afMat (6,6)
         
         !### VAR ###!
         
         !### CODE ###!
         res(1,1,1,1) = afMat(1,1)
         res(2,1,1,1) = afMat(4,1)
         res(3,1,1,1) = afMat(6,1)
         res(1,2,1,1) = afMat(4,1)
         res(2,2,1,1) = afMat(2,1)
         res(3,2,1,1) = afMat(5,1)
         res(1,3,1,1) = afMat(6,1)
         res(2,3,1,1) = afMat(5,1)
         res(3,3,1,1) = afMat(3,1)

         res(1,1,2,1) = afMat(1,4)
         res(2,1,2,1) = afMat(4,4)
         res(3,1,2,1) = afMat(6,4)
         res(1,2,2,1) = afMat(4,4)
         res(2,2,2,1) = afMat(2,4)
         res(3,2,2,1) = afMat(5,4)
         res(1,3,2,1) = afMat(6,4)
         res(2,3,2,1) = afMat(5,4)
         res(3,3,2,1) = afMat(3,4)

         res(1,1,3,1) = afMat(1,6)
         res(2,1,3,1) = afMat(4,6)
         res(3,1,3,1) = afMat(6,6)
         res(1,2,3,1) = afMat(4,6)
         res(2,2,3,1) = afMat(2,6)
         res(3,2,3,1) = afMat(5,6)
         res(1,3,3,1) = afMat(6,6)
         res(2,3,3,1) = afMat(5,6)
         res(3,3,3,1) = afMat(3,6)

         res(1,1,1,2) = afMat(1,4)
         res(2,1,1,2) = afMat(4,4)
         res(3,1,1,2) = afMat(6,4)
         res(1,2,1,2) = afMat(4,4)
         res(2,2,1,2) = afMat(2,4)
         res(3,2,1,2) = afMat(5,4)
         res(1,3,1,2) = afMat(6,4)
         res(2,3,1,2) = afMat(5,4)
         res(3,3,1,2) = afMat(3,4)

         res(1,1,2,2) = afMat(1,2)
         res(2,1,2,2) = afMat(4,2)
         res(3,1,2,2) = afMat(6,2)
         res(1,2,2,2) = afMat(4,2)
         res(2,2,2,2) = afMat(2,2)
         res(3,2,2,2) = afMat(5,2)
         res(1,3,2,2) = afMat(6,2)
         res(2,3,2,2) = afMat(5,2)
         res(3,3,2,2) = afMat(3,2)

         res(1,1,3,2) = afMat(1,5)
         res(2,1,3,2) = afMat(4,5)
         res(3,1,3,2) = afMat(6,5)
         res(1,2,3,2) = afMat(4,5)
         res(2,2,3,2) = afMat(2,5)
         res(3,2,3,2) = afMat(5,5)
         res(1,3,3,2) = afMat(6,5)
         res(2,3,3,2) = afMat(5,5)
         res(3,3,3,2) = afMat(3,5)

         res(1,1,1,3) = afMat(1,6)
         res(2,1,1,3) = afMat(4,6)
         res(3,1,1,3) = afMat(6,6)
         res(1,2,1,3) = afMat(4,6)
         res(2,2,1,3) = afMat(2,6)
         res(3,2,1,3) = afMat(5,6)
         res(1,3,1,3) = afMat(6,6)
         res(2,3,1,3) = afMat(5,6)
         res(3,3,1,3) = afMat(3,6)

         res(1,1,2,3) = afMat(1,5)
         res(2,1,2,3) = afMat(4,5)
         res(3,1,2,3) = afMat(6,5)
         res(1,2,2,3) = afMat(4,5)
         res(2,2,2,3) = afMat(2,5)
         res(3,2,2,3) = afMat(5,5)
         res(1,3,2,3) = afMat(6,5)
         res(2,3,2,3) = afMat(5,5)
         res(3,3,2,3) = afMat(3,5)

         res(1,1,3,3) = afMat(1,3)
         res(2,1,3,3) = afMat(4,3)
         res(3,1,3,3) = afMat(6,3)
         res(1,2,3,3) = afMat(4,3)
         res(2,2,3,3) = afMat(2,3)
         res(3,2,3,3) = afMat(5,3)
         res(1,3,3,3) = afMat(6,3)
         res(2,3,3,3) = afMat(5,3)
         res(3,3,3,3) = afMat(3,3)
      END SUBROUTINE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE contract3_t2f_6x1 (res, t2f)
      ! contracts a 3x3 tensor: res(6) = t2f(3x3) [3rd dimension]
      !     t2f            Tensor 2nd order; Dimension (DimxDim)
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         IMPLICIT NONE
         REAL(8), INTENT(OUT) :: res (6)
         REAL(8), INTENT(IN)  :: t2f (3,3)
         
         !### VAR ###!
         
         !### CODE ###!
         res(1) = t2f(1,1)
         res(2) = t2f(2,2)
         res(3) = t2f(3,3)
         res(4) = t2f(1,2)
         res(5) = t2f(2,3)
         res(6) = t2f(3,1)
      END SUBROUTINE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE contract3_t4f_6x6 (res, t4f)
      ! contracts a 3x3x3x3 tensor: res(6x6) = t2f(3x3x3x3) [3rd dimension]
      !     t4f            Tensor 4th order; Dimension (DimxDimxDimxDim)
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         IMPLICIT NONE
         REAL(8), INTENT(OUT) :: res (6,6)
         REAL(8), INTENT(IN)  :: t4f (3,3,3,3)
         
         !### VAR ###!
         
         !### CODE ###!
         res(1,1) = t4f(1,1,1,1)
         res(2,1) = t4f(2,2,1,1)
         res(3,1) = t4f(3,3,1,1)
         res(4,1) = t4f(1,2,1,1)
         res(5,1) = t4f(2,3,1,1)
         res(6,1) = t4f(3,1,1,1)
         
         res(1,2) = t4f(1,1,2,2)
         res(2,2) = t4f(2,2,2,2)
         res(3,2) = t4f(3,3,2,2)
         res(4,2) = t4f(1,2,2,2)
         res(5,2) = t4f(2,3,2,2)
         res(6,2) = t4f(3,1,2,2)
         
         res(1,3) = t4f(1,1,3,3)
         res(2,3) = t4f(2,2,3,3)
         res(3,3) = t4f(3,3,3,3)
         res(4,3) = t4f(1,2,3,3)
         res(5,3) = t4f(2,3,3,3)
         res(6,3) = t4f(3,1,3,3)
         
         res(1,4) = t4f(1,1,1,2)
         res(2,4) = t4f(2,2,1,2)
         res(3,4) = t4f(3,3,1,2)
         res(4,4) = t4f(1,2,1,2)
         res(5,4) = t4f(2,3,1,2)
         res(6,4) = t4f(3,1,1,2)
         
         res(1,5) = t4f(1,1,2,3)
         res(2,5) = t4f(2,2,2,3)
         res(3,5) = t4f(3,3,2,3)
         res(4,5) = t4f(1,2,2,3)
         res(5,5) = t4f(2,3,2,3)
         res(6,5) = t4f(3,1,2,3)
         
         res(1,6) = t4f(1,1,3,1)
         res(2,6) = t4f(2,2,3,1)
         res(3,6) = t4f(3,3,3,1)
         res(4,6) = t4f(1,2,3,1)
         res(5,6) = t4f(2,3,3,1)
         res(6,6) = t4f(3,1,3,1)
      END SUBROUTINE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE in1_ord11 (res, t1f_A, t1f_B, iDm)
      ! returns the first inner product of A and B
      ! res = a_i * b_i
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         IMPLICIT NONE
         INTEGER, INTENT(IN)  :: iDm
         REAL(8), INTENT(OUT) :: res
         REAL(8), INTENT(IN)  :: t1f_A (iDm)
         REAL(8), INTENT(IN)  :: t1f_B (iDm)
      
         !### VAR ###!
         integer  :: ii
         
         !### CODE ###!
         res = 0.0d0
         do ii = 1,iDm
            res = res + t1f_A(ii) * t1f_B(ii)
         enddo
      
      END SUBROUTINE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE in1_ord12 (res, t1f_A, t2f_B, iDm)
      ! returns the first inner product of A and B
      ! res_j = a_i * B_ij
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         IMPLICIT NONE
         INTEGER, INTENT(IN)  :: iDm
         REAL(8), INTENT(OUT) :: res (iDm)
         REAL(8), INTENT(IN)  :: t1f_A (iDm)
         REAL(8), INTENT(IN)  :: t2f_B (iDm,iDm)
      
         !### VAR ###!
         integer  :: ii,j
         real(8)  :: fSum
         
         !### CODE ###!
         do j = 1,iDm
            
            fSum = 0.0d0
            do ii = 1,iDm
               fSum = fSum + t1f_A(ii) * t2f_B(ii,j)
            enddo
            res(j) = fSum
            
         enddo
      
      END SUBROUTINE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE in1_ord21 (res, t2f_A, t1f_B, iDm)
      ! returns the first inner product of A and B
      ! res_i = A_ij * b_j
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         IMPLICIT NONE
         INTEGER, INTENT(IN)  :: iDm
         REAL(8), INTENT(OUT) :: res (iDm)
         REAL(8), INTENT(IN)  :: t2f_A (iDm,iDm)
         REAL(8), INTENT(IN)  :: t1f_B (iDm)
      
         !### VAR ###!
         integer  :: i,jj
         real(8)  :: fSum
         
         !### CODE ###!
         do i = 1,iDm
            
            fSum = 0.0d0
            do jj = 1,iDm
               fSum = fSum + t2f_A(i,jj) * t1f_B(jj)
            enddo
            res(i) = fSum
            
         enddo
      
      END SUBROUTINE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE in1_ord22 (res, t2f_A, t2f_B, iDm)
      ! returns the first inner product of A and B
      ! res_ij = A_ik * B_kj
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         IMPLICIT NONE
         INTEGER, INTENT(IN)  :: iDm
         REAL(8), INTENT(OUT) :: res (iDm,iDm)
         REAL(8), INTENT(IN)  :: t2f_A (iDm,iDm)
         REAL(8), INTENT(IN)  :: t2f_B (iDm,iDm)
      
         !### VAR ###!
         integer  :: i,j,kk
         real(8)  :: fSum
         
         !### CODE ###!
         do j = 1,iDm
            do i = 1,iDm
               
               fSum = 0.0d0
               do kk = 1,iDm
                  fSum = fSum + t2f_A(i,kk) * t2f_B(kk,j)
               enddo
               res(i,j) = fSum
               
            enddo
         enddo
      
      END SUBROUTINE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE in1_ord222 (res, t2f_A, t2f_B, t2f_C, iDm)
      ! returns the first inner product of A and B and C
      ! res_ij = A_ik * B_kl * C_lj
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         IMPLICIT NONE
         INTEGER, INTENT(IN)  :: iDm
         REAL(8), INTENT(OUT) :: res (iDm,iDm)
         REAL(8), INTENT(IN)  :: t2f_A (iDm,iDm)
         REAL(8), INTENT(IN)  :: t2f_B (iDm,iDm)
         REAL(8), INTENT(IN)  :: t2f_C (iDm,iDm)
      
         !### VAR ###!
         integer  :: i,j,kk,ll
         real(8)  :: fSum
         
         !### CODE ###!
         do j = 1,iDm
            do i = 1,iDm
               
               fSum = 0.0d0
               do ll = 1,iDm
                  do kk = 1,iDm
                     fSum = fSum + t2f_A(i,kk) * t2f_B(kk,ll) *
     *                  t2f_C(ll,j)
                  enddo
               enddo
               res(i,j) = fSum
               
            enddo
         enddo
      
      END SUBROUTINE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE in2_ord22 (res, t2f_A, t2f_B, iDm)
      ! returns the second inner product of A and B
      ! res = A_ij * B_ij
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         IMPLICIT NONE
         INTEGER, INTENT(IN)  :: iDm
         REAL(8), INTENT(OUT) :: res
         REAL(8), INTENT(IN)  :: t2f_A (iDm,iDm)
         REAL(8), INTENT(IN)  :: t2f_B (iDm,iDm)
      
         !### VAR ###!
         integer  :: ii,jj
         
         !### CODE ###!
         res = 0.0d0
         do jj = 1,iDm
            do ii = 1,iDm
               res = res + t2f_A(ii,jj) * t2f_B(ii,jj)
            enddo
         enddo
      
      END SUBROUTINE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE in2_ord24 (res, t2f_A, t4f_B, iDm)
      ! returns the second inner product of A and B
      ! res_ij = A_kl * B_klij
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         IMPLICIT NONE
         INTEGER, INTENT(IN)  :: iDm
         REAL(8), INTENT(OUT) :: res (iDm,iDm)
         REAL(8), INTENT(IN)  :: t2f_A (iDm,iDm)
         REAL(8), INTENT(IN)  :: t4f_B (iDm,iDm,iDm,iDm)
      
         !### VAR ###!
         integer  :: i,j,kk,ll
         real(8)  :: fSum
         
         !### CODE ###!
         do j = 1,iDm
            do i = 1,iDm
               
               fSum = 0.0d0
               do ll = 1,iDm
                  do kk = 1,iDm
                     fSum = fSum + t2f_A(kk,ll) * t4f_B(kk,ll,i,j)
                  enddo
               enddo
               res(i,j) = fSum
               
            enddo
         enddo
      
      END SUBROUTINE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE in2_ord42 (res, t4f_A, t2f_B, iDm)
      ! returns the second inner product of A and B
      ! res_ij = A_ijkl * B_kl
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         IMPLICIT NONE
         INTEGER, INTENT(IN)  :: iDm
         REAL(8), INTENT(OUT) :: res (iDm,iDm)
         REAL(8), INTENT(IN)  :: t4f_A (iDm,iDm,iDm,iDm)
         REAL(8), INTENT(IN)  :: t2f_B (iDm,iDm)
      
         !### VAR ###!
         integer  :: i,j,kk,ll
         real(8)  :: fSum
         
         !### CODE ###!
         do j = 1,iDm
            do i = 1,iDm
               
               fSum = 0.0d0
               do ll = 1,iDm
                  do kk = 1,iDm
                     fSum = fSum + t4f_A(i,j,kk,ll) * t2f_B(kk,ll)
                  enddo
               enddo
               res(i,j) = fSum
               
            enddo
         enddo
      
      END SUBROUTINE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE in2_ord44 (res, t4f_A, t4f_B, iDm)
      ! returns the second inner product of A and B
      ! res_ijkl = A_ijmn * B_mnkl
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         IMPLICIT NONE
         INTEGER, INTENT(IN)  :: iDm
         REAL(8), INTENT(OUT) :: res (iDm,iDm,iDm,iDm)
         REAL(8), INTENT(IN)  :: t4f_A (iDm,iDm,iDm,iDm)
         REAL(8), INTENT(IN)  :: t4f_B (iDm,iDm,iDm,iDm)
      
         !### VAR ###!
         integer  :: i,j,k,l,mm,nn
         real(8)  :: fSum
         
         !### CODE ###!
         do l = 1,iDm
            do k = 1,iDm
               do j = 1,iDm
                  do i = 1,iDm
                     
                     fSum = 0.0d0
                     do nn = 1,iDm
                        do mm = 1,iDm
                           fSum = fSum +
     *                        t4f_A(i,j,mm,nn) * t4f_B(mm,nn,k,l)
                        enddo
                     enddo
                     res(i,j,k,l) = fSum
                     
                  enddo
               enddo
            enddo
         enddo
      
      END SUBROUTINE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE in2_ord444 (res, t4f_A, t4f_B, t4f_C, iDm)
      ! returns the second inner product of A and B and C
      ! res_ijkl = A_ijmn * B_mnop * C_opkl
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         IMPLICIT NONE
         INTEGER, INTENT(IN)  :: iDm
         REAL(8), INTENT(OUT) :: res (iDm,iDm,iDm,iDm)
         REAL(8), INTENT(IN)  :: t4f_A (iDm,iDm,iDm,iDm)
         REAL(8), INTENT(IN)  :: t4f_B (iDm,iDm,iDm,iDm)
         REAL(8), INTENT(IN)  :: t4f_C (iDm,iDm,iDm,iDm)
      
         !### VAR ###!
         integer  :: i,j,k,l,mm,nn,oo,pp
         real(8)  :: fSum
         
         !### CODE ###!
         do l = 1,iDm
            do k = 1,iDm
               do j = 1,iDm
                  do i = 1,iDm
                     
                     fSum = 0.0d0
                     do pp = 1,iDm
                        do oo = 1,iDm
                           do nn = 1,iDm
                              do mm = 1,iDm
                                 fSum = fSum +
     *                              t4f_A(i,j,mm,nn) *
     *                              t4f_B(mm,nn,oo,pp) *
     *                              t4f_C(oo,pp,k,l)
                              enddo
                           enddo
                        enddo
                     enddo
                     res(i,j,k,l) = fSum
                     
                  enddo
               enddo
            enddo
         enddo
      
      END SUBROUTINE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE dyad_ord11 (res, t1f_A, t1f_B, iDm)
      ! returns the outer/dyadic product of A and B
      ! res_ij = a_i * b_j
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         IMPLICIT NONE
         INTEGER, INTENT(IN)  :: iDm
         REAL(8), INTENT(OUT) :: res (iDm,iDm)
         REAL(8), INTENT(IN)  :: t1f_A (iDm)
         REAL(8), INTENT(IN)  :: t1f_B (iDm)
      
         !### VAR ###!
         integer  :: i,j
         
         !### CODE ###!
         do j = 1,iDm
            do i = 1,iDm
               res(i,j) = t1f_A(i) * t1f_B(j)
            enddo
         enddo
      
      END SUBROUTINE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE dyad_ord22 (res, t2f_A, t2f_B, iDm)
      ! returns the outer/dyadic product of A and B
      ! res_ijkl = A_ij * B_kl
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         IMPLICIT NONE
         INTEGER, INTENT(IN)  :: iDm
         REAL(8), INTENT(OUT) :: res (iDm,iDm,iDm,iDm)
         REAL(8), INTENT(IN)  :: t2f_A (iDm,iDm)
         REAL(8), INTENT(IN)  :: t2f_B (iDm,iDm)
         
         !### VAR ###!
         integer  :: i,j,k,l
         
         !### CODE ###!
         do l = 1,iDm
            do k = 1,iDm
               do j = 1,iDm
                  do i = 1,iDm
                     res(i,j,k,l) = t2f_A(i,j) * t2f_B(k,l)
                  enddo
               enddo
            enddo
         enddo
      
      END SUBROUTINE
      
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE inv_6x6_sym (res, afMat)
      ! returns the inversion of a 6x6 (sym) matrix by using the MKL
      ! subroutine dsytri(uplo,n,a,lda,ipiv,work,info)
      ! res = inv(A) [sym]
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         IMPLICIT NONE
         REAL(8), INTENT(OUT) :: res (6,6)
         REAL(8), INTENT(IN)  :: afMat (6,6)
         
         !### VAR ###!
         character(1), parameter :: uplo = 'U'
         integer                 :: ipiv(6)
         integer                 :: lwork
         real(8),allocatable     :: work(:)
         integer                 :: info
         
         !### CODE ###!
         res = afMat
         
         ! allocate the workspace
         lwork = -1
         allocate(work(1))
         call dsytrf(uplo,6,res,6,ipiv,work,lwork,info)
         lwork = nint(work(1))
         deallocate(work)
         
         ! call Bunch-Kaufman factorization
         allocate(work(lwork))
         call dsytrf(uplo,6,res,6,ipiv,work,lwork,info)
         deallocate(work)
         
         if (info == 0) then
            ! call inversion of a symmetric matrix
            allocate(work(2*6))
            call dsytri(uplo,6,res,6,ipiv,work,info)
            deallocate(work)
            
            if (info /= 0) then
               res = real(info,8)
            else
               ! mirror sym U to L
               res(2,1) = res(1,2)
               res(3,1) = res(1,3)
               res(4,1) = res(1,4)
               res(5,1) = res(1,5)
               res(6,1) = res(1,6)
               res(3,2) = res(2,3)
               res(4,2) = res(2,4)
               res(5,2) = res(2,5)
               res(6,2) = res(2,6)
               res(4,3) = res(3,4)
               res(5,3) = res(3,5)
               res(6,3) = res(3,6)
               res(5,4) = res(4,5)
               res(6,4) = res(4,6)
               res(6,5) = res(5,6)
            endif
         else
            res = real(info,8)
         endif
         
      END SUBROUTINE

!######################################################################!
      END MODULE comMath
!######################################################################!
