SUBROUTINE p_ccs_t1(N,f1,f1e,i0,i0e,i1,i1e,i2,i2e,nocc,t1,t1e,v2,v2e)
IMPLICIT NONE
INTEGER :: N
INTEGER :: nocc
INTEGER :: p2
INTEGER :: h1
INTEGER :: h7
INTEGER :: p3
INTEGER :: h4
INTEGER :: p8
INTEGER :: h5
INTEGER :: p4
INTEGER :: h6
INTEGER :: p5
REAL*8 :: i0(*)
REAL*8 :: i0e(*)
REAL*8 :: f1(*)
REAL*8 :: f1e(*)
REAL*8 :: t1(*)
REAL*8 :: t1e(*)
REAL*8 :: i1(*)
REAL*8 :: i1e(*)
REAL*8 :: i2(*)
REAL*8 :: i2e(*)
REAL*8 :: v2(*)
REAL*8 :: v2e(*)
DO p2=nocc+1,N
DO h1=1,nocc
i0(((p2-1)*N+h1))=(1.0d0)*f1(((p2-1)*N+h1))
END DO
END DO
DO h7=1,nocc
DO h1=1,nocc
i1(((h7-1)*N+h1))=(1.0d0)*f1(((h7-1)*N+h1))
END DO
END DO
DO h7=1,nocc
DO p8=nocc+1,N
i2(((h7-1)*N+p8))=(1.0d0)*f1(((h7-1)*N+p8))
END DO
END DO
DO h7=1,nocc
DO h6=1,nocc
IF (h6>h7) CYCLE
DO p8=nocc+1,N
DO p5=nocc+1,N
IF (p5>p8) CYCLE
v2e(((((h7-1)*N+h6-1)*N+p8-1)*N+p5))= + v2(((((h6-1)*N+h7-1)*N+p5-1)*N+p8))
v2e(((((h7-1)*N+h6-1)*N+p5-1)*N+p8))= - v2(((((h6-1)*N+h7-1)*N+p5-1)*N+p8))
v2e(((((h6-1)*N+h7-1)*N+p8-1)*N+p5))= - v2(((((h6-1)*N+h7-1)*N+p5-1)*N+p8))
v2e(((((h6-1)*N+h7-1)*N+p5-1)*N+p8))= + v2(((((h6-1)*N+h7-1)*N+p5-1)*N+p8))
END DO
END DO
END DO
END DO
DO h7=1,nocc
DO p8=nocc+1,N
DO p5=nocc+1,N
DO h6=1,nocc
i2(((h7-1)*N+p8))=i2(((h7-1)*N+p8))+(0.9999999999999999d0)*t1(((p5-1)*N+h6))*v2e(((((h6-1)*N+h7-1)*N+p5-1)*N+p8))
END DO
END DO
END DO
END DO
DO h7=1,nocc
DO h1=1,nocc
DO p8=nocc+1,N
i1(((h7-1)*N+h1))=i1(((h7-1)*N+h1))+(1.0d0)*t1(((p8-1)*N+h1))*i2(((h7-1)*N+p8))
END DO
END DO
END DO
DO h7=1,nocc
DO h5=1,nocc
IF (h5>h7) CYCLE
DO h1=1,nocc
DO p4=nocc+1,N
v2e(((((h7-1)*N+h5-1)*N+h1-1)*N+p4))= - v2(((((h5-1)*N+h7-1)*N+h1-1)*N+p4))
v2e(((((h5-1)*N+h7-1)*N+h1-1)*N+p4))= + v2(((((h5-1)*N+h7-1)*N+h1-1)*N+p4))
END DO
END DO
END DO
END DO
DO h7=1,nocc
DO h1=1,nocc
DO p4=nocc+1,N
DO h5=1,nocc
i1(((h7-1)*N+h1))=i1(((h7-1)*N+h1))+(-1.0d0)*t1(((p4-1)*N+h5))*v2e(((((h5-1)*N+h7-1)*N+h1-1)*N+p4))
END DO
END DO
END DO
END DO
DO p2=nocc+1,N
DO h1=1,nocc
DO h7=1,nocc
i0(((p2-1)*N+h1))=i0(((p2-1)*N+h1))+(-1.0d0)*t1(((p2-1)*N+h7))*i1(((h7-1)*N+h1))
END DO
END DO
END DO
DO p2=nocc+1,N
DO p3=nocc+1,N
i1(((p2-1)*N+p3))=(1.0d0)*f1(((p2-1)*N+p3))
END DO
END DO
DO p2=nocc+1,N
DO h5=1,nocc
DO p3=nocc+1,N
DO p4=nocc+1,N
IF (p3>p4) CYCLE
v2e(((((h5-1)*N+p2-1)*N+p4-1)*N+p3))= - v2(((((h5-1)*N+p2-1)*N+p3-1)*N+p4))
v2e(((((h5-1)*N+p2-1)*N+p3-1)*N+p4))= + v2(((((h5-1)*N+p2-1)*N+p3-1)*N+p4))
END DO
END DO
END DO
END DO
DO p2=nocc+1,N
DO p3=nocc+1,N
DO p4=nocc+1,N
DO h5=1,nocc
i1(((p2-1)*N+p3))=i1(((p2-1)*N+p3))+(-1.0d0)*t1(((p4-1)*N+h5))*v2e(((((h5-1)*N+p2-1)*N+p3-1)*N+p4))
END DO
END DO
END DO
END DO
DO p2=nocc+1,N
DO h1=1,nocc
DO p3=nocc+1,N
i0(((p2-1)*N+h1))=i0(((p2-1)*N+h1))+(1.0d0)*t1(((p3-1)*N+h1))*i1(((p2-1)*N+p3))
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
RETURN
END SUBROUTINE
