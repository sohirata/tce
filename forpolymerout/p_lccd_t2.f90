SUBROUTINE p_lccd_t2(N,f1,f1e,i0,i0e,nocc,t2,t2e,v2,v2e)
IMPLICIT NONE
INTEGER :: N
INTEGER :: nocc
INTEGER :: p3
INTEGER :: p4
INTEGER :: h1
INTEGER :: h2
INTEGER :: h5
INTEGER :: p5
INTEGER :: h6
INTEGER :: p6
REAL*8 :: i0(*)
REAL*8 :: i0e(*)
REAL*8 :: v2(*)
REAL*8 :: v2e(*)
REAL*8 :: t2(*)
REAL*8 :: t2e(*)
REAL*8 :: f1(*)
REAL*8 :: f1e(*)
DO p3=nocc+1,N
DO p4=nocc+1,N
IF (p3>p4) CYCLE
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
i0(((((p3-1)*N+p4-1)*N+h1-1)*N+h2))=(1.0d0)*v2(((((p3-1)*N+p4-1)*N+h1-1)*N+h2))
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
DO p3=nocc+1,N
DO p4=nocc+1,N
IF (p3>p4) CYCLE
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
DO h5=1,nocc
i0(((((p3-1)*N+p4-1)*N+h1-1)*N+h2))=i0(((((p3-1)*N+p4-1)*N+h1-1)*N+h2))+(-1.0d0)*t2e(((((p3-1)*N+p4-1)*N+h1-1)*N+h5))*f1(((h5-1)*N+&
&h2))+(1.0d0)*t2e(((((p3-1)*N+p4-1)*N+h2-1)*N+h5))*f1(((h5-1)*N+h1))
END DO
END DO
END DO
END DO
END DO
DO p3=nocc+1,N
DO p5=nocc+1,N
IF (p3>p5) CYCLE
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
t2e(((((p5-1)*N+p3-1)*N+h1-1)*N+h2))= - t2(((((p3-1)*N+p5-1)*N+h1-1)*N+h2))
t2e(((((p3-1)*N+p5-1)*N+h1-1)*N+h2))= + t2(((((p3-1)*N+p5-1)*N+h1-1)*N+h2))
END DO
END DO
END DO
END DO
DO p3=nocc+1,N
DO p4=nocc+1,N
IF (p3>p4) CYCLE
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
DO p5=nocc+1,N
i0(((((p3-1)*N+p4-1)*N+h1-1)*N+h2))=i0(((((p3-1)*N+p4-1)*N+h1-1)*N+h2))+(1.0d0)*t2e(((((p3-1)*N+p5-1)*N+h1-1)*N+h2))*f1(((p4-1)*N+p&
&5))+(-1.0d0)*t2e(((((p4-1)*N+p5-1)*N+h1-1)*N+h2))*f1(((p3-1)*N+p5))
END DO
END DO
END DO
END DO
END DO
DO p3=nocc+1,N
DO p4=nocc+1,N
IF (p3>p4) CYCLE
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
DO h5=1,nocc
DO h6=1,nocc
IF (h5>h6) CYCLE
i0(((((p3-1)*N+p4-1)*N+h1-1)*N+h2))=i0(((((p3-1)*N+p4-1)*N+h1-1)*N+h2))+(1.0d0)*t2(((((p3-1)*N+p4-1)*N+h5-1)*N+h6))*v2(((((h5-1)*N+&
&h6-1)*N+h1-1)*N+h2))
END DO
END DO
END DO
END DO
END DO
END DO
DO p3=nocc+1,N
DO p5=nocc+1,N
IF (p3>p5) CYCLE
DO h1=1,nocc
DO h6=1,nocc
IF (h1>h6) CYCLE
t2e(((((p5-1)*N+p3-1)*N+h6-1)*N+h1))= + t2(((((p3-1)*N+p5-1)*N+h1-1)*N+h6))
t2e(((((p5-1)*N+p3-1)*N+h1-1)*N+h6))= - t2(((((p3-1)*N+p5-1)*N+h1-1)*N+h6))
t2e(((((p3-1)*N+p5-1)*N+h6-1)*N+h1))= - t2(((((p3-1)*N+p5-1)*N+h1-1)*N+h6))
t2e(((((p3-1)*N+p5-1)*N+h1-1)*N+h6))= + t2(((((p3-1)*N+p5-1)*N+h1-1)*N+h6))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h6=1,nocc
DO h2=1,nocc
DO p5=nocc+1,N
v2e(((((h6-1)*N+p4-1)*N+h2-1)*N+p5))= + v2(((((h6-1)*N+p4-1)*N+h2-1)*N+p5))
END DO
END DO
END DO
END DO
DO p3=nocc+1,N
DO p4=nocc+1,N
IF (p3>p4) CYCLE
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
DO p5=nocc+1,N
DO h6=1,nocc
i0(((((p3-1)*N+p4-1)*N+h1-1)*N+h2))=i0(((((p3-1)*N+p4-1)*N+h1-1)*N+h2))+(-1.0d0)*t2e(((((p3-1)*N+p5-1)*N+h1-1)*N+h6))*v2e(((((h6-1)&
&*N+p4-1)*N+h2-1)*N+p5))+(1.0d0)*t2e(((((p3-1)*N+p5-1)*N+h2-1)*N+h6))*v2e(((((h6-1)*N+p4-1)*N+h1-1)*N+p5))+(1.0d0)*t2e(((((p4-1)*N+&
&p5-1)*N+h1-1)*N+h6))*v2e(((((h6-1)*N+p3-1)*N+h2-1)*N+p5))+(-1.0d0)*t2e(((((p4-1)*N+p5-1)*N+h2-1)*N+h6))*v2e(((((h6-1)*N+p3-1)*N+h1&
&-1)*N+p5))
END DO
END DO
END DO
END DO
END DO
END DO
DO p3=nocc+1,N
DO p4=nocc+1,N
IF (p3>p4) CYCLE
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
DO p5=nocc+1,N
DO p6=nocc+1,N
IF (p5>p6) CYCLE
i0(((((p3-1)*N+p4-1)*N+h1-1)*N+h2))=i0(((((p3-1)*N+p4-1)*N+h1-1)*N+h2))+(1.0d0)*t2(((((p5-1)*N+p6-1)*N+h1-1)*N+h2))*v2(((((p3-1)*N+&
&p4-1)*N+p5-1)*N+p6))
END DO
END DO
END DO
END DO
END DO
END DO
RETURN
END SUBROUTINE
