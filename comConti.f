!######################################################################!
!#### COMMON_CONTI MODULE                                              !
!####                                                                  !
!######################################################################!
! Version | Date            | Creator    | Comment                     !
!######################################################################!
! 1.0     | 2012-02-22      | CfKu       | First release               !
!######################################################################!
      MODULE comConti
      USE comSub
      USE comMath
      IMPLICIT NONE

!######################################################################!
      
!######################################################################!
      CONTAINS
!######################################################################!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE push_stress_2pk2sig(res, t2f_2pk, t2f_F, f_J, iDm)
      ! push-forward of 2nd-order stress tensor (2.Piola-Kirchhoff -> Cauchy)
      ! sig_ij = 1/J * F_ik * S_kl * F_jl
      !  t2f_F    : deformation gradient F
      !  f_J      : Jacobi-determinant J = det(F)
      !  iDm      : dimension
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         IMPLICIT NONE
         INTEGER, INTENT(IN)  :: iDm
         REAL(8), INTENT(OUT) :: res (iDm,iDm)     ! t2f_sig
         REAL(8), INTENT(IN)  :: t2f_2pk (iDm,iDm)
         REAL(8), INTENT(IN)  :: t2f_F (iDm,iDm)
         REAL(8), INTENT(IN)  :: f_J
         
         !### VAR ###!
         integer  :: i,j,kk,ll
         real(8)  :: fSum
         
         !### CODE ###!
         do j = 1,iDm
            do i = 1,iDm
               
               fSum = 0.0d0
               do ll = 1,iDm
                  do kk = 1,iDm
                     fSum = fSum + t2f_F(i,kk) * t2f_2pk(kk,ll) *
     *                  t2f_F(j,ll)
                  enddo
               enddo
               res(i,j) = fSum / f_J
               
            enddo
         enddo
      END SUBROUTINE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE push_tang_2pk2sig(res, t4f_ref, t2f_F, f_J, iDm)
      ! push-forward of 4th-order material tensor t4f_ref (material to spacial)
      ! res_ijkl = 1/J * F_im * F_jn * F_ko * F_lp * t4f_ref(m,n,o,p)
      !  t2f_F    : deformation gradient F
      !  f_J      : Jacobi-determinant J = det(F)
      !  iDm      : dimension
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         IMPLICIT NONE
         INTEGER, INTENT(IN)  :: iDm
         REAL(8), INTENT(OUT) :: res (iDm,iDm,iDm,iDm)
         REAL(8), INTENT(IN)  :: t4f_ref (iDm,iDm,iDm,iDm)
         REAL(8), INTENT(IN)  :: t2f_F (iDm,iDm)
         REAL(8), INTENT(IN)  :: f_J
         
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
                                 fSum = fSum + t2f_F(i,mm) *
     *                              t2f_F(j,nn) * t2f_F(k,oo) *
     *                              t2f_F(l,pp) * t4f_ref(mm,nn,oo,pp)
                              enddo
                           enddo
                        enddo
                     enddo
                     res(i,j,k,l) = fSum / f_J
                     
                  enddo
               enddo
            enddo
         enddo
      END SUBROUTINE

!######################################################################!
      END MODULE comConti
!######################################################################!
