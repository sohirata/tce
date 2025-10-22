SUBROUTINE np_ccd_e(N,i0,nocc,t2,v2)
IMPLICIT NONE
INTEGER :: N
INTEGER :: nocc
INTEGER :: h3
INTEGER :: h4
INTEGER :: p1
INTEGER :: p2
REAL*8 :: TMP
REAL*8 :: i0(*)
REAL*8 :: t2(*)
REAL*8 :: v2(*)
i0(1)=0.0d0
TMP=0.0d0
DO h3=1,nocc
DO h4=1,nocc
DO p1=nocc+1,N
DO p2=nocc+1,N
TMP=TMP+t2(((((p1-1)*N+p2-1)*N+h3-1)*N+h4))*v2(((((h3-1)*N+h4-1)*N+p1-1)*N+p2))
END DO
END DO
END DO
END DO
i0(1)=i0(1)+(0.25d0)*TMP
RETURN
END SUBROUTINE
