SUBROUTINE p_lccsd_t1(N,f1,f1e,i0,i0e,nocc,t1,t1e,t2,t2e,v2,v2e)
IMPLICIT NONE
INTEGER :: N
INTEGER :: nocc
INTEGER :: p2
INTEGER :: h1
INTEGER :: h3
INTEGER :: p3
INTEGER :: h4
INTEGER :: p4
INTEGER :: h5
REAL*8 :: i0(*)
REAL*8 :: i0e(*)
REAL*8 :: f1(*)
REAL*8 :: f1e(*)
REAL*8 :: t1(*)
REAL*8 :: t1e(*)
REAL*8 :: v2(*)
REAL*8 :: v2e(*)
REAL*8 :: t2(*)
REAL*8 :: t2e(*)
DO p2=nocc+1,N
DO h1=1,nocc
i0(((p2-1)*N+h1))=(1.0d0)*f1(((p2-1)*N+h1))
END DO
END DO
DO p2=nocc+1,N
DO h1=1,nocc
DO h3=1,nocc
i0(((p2-1)*N+h1))=i0(((p2-1)*N+h1))+(-1.0d0)*t1(((p2-1)*N+h3))*f1(((h3-1)*N+h1))
END DO
END DO
END DO
DO p2=nocc+1,N
DO h1=1,nocc
DO p3=nocc+1,N
i0(((p2-1)*N+h1))=i0(((p2-1)*N+h1))+(1.0d0)*t1(((p3-1)*N+h1))*f1(((p2-1)*N+p3))
END DO
END DO
END DO
DO p2=nocc+1,N
DO h4=1,nocc
DO h1=1,nocc
DO p3=nocc+1,N
v2e(((((h4-1)*N+p2-1)*N+h1-1)*N+p3))= + v2(((((h4-1)*N+p2-1)*N+h1-1)*N+p3))
END DO
END DO
END DO
END DO
DO p2=nocc+1,N
DO h1=1,nocc
DO p3=nocc+1,N
DO h4=1,nocc
i0(((p2-1)*N+h1))=i0(((p2-1)*N+h1))+(-1.0d0)*t1(((p3-1)*N+h4))*v2e(((((h4-1)*N+p2-1)*N+h1-1)*N+p3))
END DO
END DO
END DO
END DO
DO p2=nocc+1,N
DO p4=nocc+1,N
IF (p2>p4) CYCLE
DO h1=1,nocc
DO h3=1,nocc
IF (h1>h3) CYCLE
t2e(((((p4-1)*N+p2-1)*N+h3-1)*N+h1))= + t2(((((p2-1)*N+p4-1)*N+h1-1)*N+h3))
t2e(((((p4-1)*N+p2-1)*N+h1-1)*N+h3))= - t2(((((p2-1)*N+p4-1)*N+h1-1)*N+h3))
t2e(((((p2-1)*N+p4-1)*N+h3-1)*N+h1))= - t2(((((p2-1)*N+p4-1)*N+h1-1)*N+h3))
t2e(((((p2-1)*N+p4-1)*N+h1-1)*N+h3))= + t2(((((p2-1)*N+p4-1)*N+h1-1)*N+h3))
END DO
END DO
END DO
END DO
DO p2=nocc+1,N
DO h1=1,nocc
DO p4=nocc+1,N
DO h3=1,nocc
i0(((p2-1)*N+h1))=i0(((p2-1)*N+h1))+(1.0d0)*t2e(((((p2-1)*N+p4-1)*N+h1-1)*N+h3))*f1(((h3-1)*N+p4))
END DO
END DO
END DO
END DO
DO p2=nocc+1,N
DO p3=nocc+1,N
IF (p2>p3) CYCLE
DO h4=1,nocc
DO h5=1,nocc
IF (h4>h5) CYCLE
t2e(((((p3-1)*N+p2-1)*N+h4-1)*N+h5))= - t2(((((p2-1)*N+p3-1)*N+h4-1)*N+h5))
t2e(((((p2-1)*N+p3-1)*N+h4-1)*N+h5))= + t2(((((p2-1)*N+p3-1)*N+h4-1)*N+h5))
END DO
END DO
END DO
END DO
DO h4=1,nocc
DO h5=1,nocc
IF (h4>h5) CYCLE
DO h1=1,nocc
DO p3=nocc+1,N
v2e(((((h4-1)*N+h5-1)*N+h1-1)*N+p3))= + v2(((((h4-1)*N+h5-1)*N+h1-1)*N+p3))
END DO
END DO
END DO
END DO
DO p2=nocc+1,N
DO h1=1,nocc
DO p3=nocc+1,N
DO h4=1,nocc
DO h5=1,nocc
IF (h4>h5) CYCLE
i0(((p2-1)*N+h1))=i0(((p2-1)*N+h1))+(-1.0d0)*t2e(((((p2-1)*N+p3-1)*N+h4-1)*N+h5))*v2e(((((h4-1)*N+h5-1)*N+h1-1)*N+p3))
END DO
END DO
END DO
END DO
END DO
DO p3=nocc+1,N
DO p4=nocc+1,N
IF (p3>p4) CYCLE
DO h1=1,nocc
DO h5=1,nocc
IF (h1>h5) CYCLE
t2e(((((p3-1)*N+p4-1)*N+h5-1)*N+h1))= - t2(((((p3-1)*N+p4-1)*N+h1-1)*N+h5))
t2e(((((p3-1)*N+p4-1)*N+h1-1)*N+h5))= + t2(((((p3-1)*N+p4-1)*N+h1-1)*N+h5))
END DO
END DO
END DO
END DO
DO p2=nocc+1,N
DO h5=1,nocc
DO p3=nocc+1,N
DO p4=nocc+1,N
IF (p3>p4) CYCLE
v2e(((((h5-1)*N+p2-1)*N+p3-1)*N+p4))= + v2(((((h5-1)*N+p2-1)*N+p3-1)*N+p4))
END DO
END DO
END DO
END DO
DO p2=nocc+1,N
DO h1=1,nocc
DO p3=nocc+1,N
DO p4=nocc+1,N
IF (p3>p4) CYCLE
DO h5=1,nocc
i0(((p2-1)*N+h1))=i0(((p2-1)*N+h1))+(-1.0d0)*t2e(((((p3-1)*N+p4-1)*N+h1-1)*N+h5))*v2e(((((h5-1)*N+p2-1)*N+p3-1)*N+p4))
END DO
END DO
END DO
END DO
END DO
RETURN
END SUBROUTINE
