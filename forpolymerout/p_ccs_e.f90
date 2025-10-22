SUBROUTINE p_ccs_e(N,f1,f1e,i0,i0e,i1,i1e,nocc,t1,t1e,v2,v2e)
IMPLICIT NONE
INTEGER :: N
INTEGER :: nocc
INTEGER :: p5
INTEGER :: h6
INTEGER :: h4
INTEGER :: p3
REAL*8 :: i0(*)
REAL*8 :: i0e(*)
REAL*8 :: t1(*)
REAL*8 :: t1e(*)
REAL*8 :: i1(*)
REAL*8 :: i1e(*)
REAL*8 :: f1(*)
REAL*8 :: f1e(*)
REAL*8 :: v2(*)
REAL*8 :: v2e(*)
DO h6=1,nocc
DO p5=nocc+1,N
i1(((h6-1)*N+p5))=(1.0d0)*f1(((h6-1)*N+p5))
END DO
END DO
DO h6=1,nocc
DO h4=1,nocc
IF (h4>h6) CYCLE
DO p5=nocc+1,N
DO p3=nocc+1,N
IF (p3>p5) CYCLE
v2e(((((h6-1)*N+h4-1)*N+p5-1)*N+p3))= + v2(((((h4-1)*N+h6-1)*N+p3-1)*N+p5))
v2e(((((h6-1)*N+h4-1)*N+p3-1)*N+p5))= - v2(((((h4-1)*N+h6-1)*N+p3-1)*N+p5))
v2e(((((h4-1)*N+h6-1)*N+p5-1)*N+p3))= - v2(((((h4-1)*N+h6-1)*N+p3-1)*N+p5))
v2e(((((h4-1)*N+h6-1)*N+p3-1)*N+p5))= + v2(((((h4-1)*N+h6-1)*N+p3-1)*N+p5))
END DO
END DO
END DO
END DO
DO h6=1,nocc
DO p5=nocc+1,N
DO p3=nocc+1,N
DO h4=1,nocc
i1(((h6-1)*N+p5))=i1(((h6-1)*N+p5))+(0.5d0)*t1(((p3-1)*N+h4))*v2e(((((h4-1)*N+h6-1)*N+p3-1)*N+p5))
END DO
END DO
END DO
END DO
i0(1)=0.0d0
DO p5=nocc+1,N
DO h6=1,nocc
i0(1)=i0(1)+(1.0d0)*t1(((p5-1)*N+h6))*i1(((h6-1)*N+p5))
END DO
END DO
RETURN
END SUBROUTINE
