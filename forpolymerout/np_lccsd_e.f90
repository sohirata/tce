SUBROUTINE np_lccsd_e(N,f1,i0,nocc,t1,t2,v2)
IMPLICIT NONE
INTEGER :: N
INTEGER :: nocc
INTEGER :: p2
INTEGER :: h1
INTEGER :: h3
INTEGER :: h4
INTEGER :: p1
REAL*8 :: TMP
REAL*8 :: i0(*)
REAL*8 :: t1(*)
REAL*8 :: f1(*)
REAL*8 :: t2(*)
REAL*8 :: v2(*)
i0(1)=0.0d0
TMP=0.0d0
DO p2=nocc+1,N
DO h1=1,nocc
TMP=TMP+t1(((p2-1)*N+h1))*f1(((h1-1)*N+p2))
END DO
END DO
i0(1)=i0(1)+(1.0d0)*TMP
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
