SUBROUTINE p_ccd_t2_disconnected(N,f1,f1e,i0,i0e,i1,i1e,nocc,t2,t2e,v2,v2e)
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
INTEGER :: p7
INTEGER :: h8
INTEGER :: h7
INTEGER :: p8
REAL*8 :: i0(*)
REAL*8 :: i0e(*)
REAL*8 :: v2(*)
REAL*8 :: v2e(*)
REAL*8 :: t2(*)
REAL*8 :: t2e(*)
REAL*8 :: i1(*)
REAL*8 :: i1e(*)
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
DO h5=1,nocc
DO h1=1,nocc
i1(((h5-1)*N+h1))=(1.0d0)*f1(((h5-1)*N+h1))
END DO
END DO
DO p6=nocc+1,N
DO p7=nocc+1,N
IF (p6>p7) CYCLE
DO h1=1,nocc
DO h8=1,nocc
IF (h1>h8) CYCLE
t2e(((((p6-1)*N+p7-1)*N+h8-1)*N+h1))= - t2(((((p6-1)*N+p7-1)*N+h1-1)*N+h8))
t2e(((((p6-1)*N+p7-1)*N+h1-1)*N+h8))= + t2(((((p6-1)*N+p7-1)*N+h1-1)*N+h8))
END DO
END DO
END DO
END DO
DO h5=1,nocc
DO h8=1,nocc
IF (h5>h8) CYCLE
DO p6=nocc+1,N
DO p7=nocc+1,N
IF (p6>p7) CYCLE
v2e(((((h8-1)*N+h5-1)*N+p6-1)*N+p7))= - v2(((((h5-1)*N+h8-1)*N+p6-1)*N+p7))
v2e(((((h5-1)*N+h8-1)*N+p6-1)*N+p7))= + v2(((((h5-1)*N+h8-1)*N+p6-1)*N+p7))
END DO
END DO
END DO
END DO
DO h5=1,nocc
DO h1=1,nocc
DO p6=nocc+1,N
DO p7=nocc+1,N
IF (p6>p7) CYCLE
DO h8=1,nocc
i1(((h5-1)*N+h1))=i1(((h5-1)*N+h1))+(1.0d0)*t2e(((((p6-1)*N+p7-1)*N+h1-1)*N+h8))*v2e(((((h5-1)*N+h8-1)*N+p6-1)*N+p7))
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
DO p3=nocc+1,N
DO p4=nocc+1,N
IF (p3>p4) CYCLE
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
DO h5=1,nocc
i0(((((p3-1)*N+p4-1)*N+h1-1)*N+h2))=i0(((((p3-1)*N+p4-1)*N+h1-1)*N+h2))+(-1.0d0)*t2e(((((p3-1)*N+p4-1)*N+h1-1)*N+h5))*i1(((h5-1)*N+&
&h2))+(1.0d0)*t2e(((((p3-1)*N+p4-1)*N+h2-1)*N+h5))*i1(((h5-1)*N+h1))
END DO
END DO
END DO
END DO
END DO
DO p3=nocc+1,N
DO p5=nocc+1,N
i1(((p3-1)*N+p5))=(1.0d0)*f1(((p3-1)*N+p5))
END DO
END DO
DO p3=nocc+1,N
DO p6=nocc+1,N
IF (p3>p6) CYCLE
DO h7=1,nocc
DO h8=1,nocc
IF (h7>h8) CYCLE
t2e(((((p6-1)*N+p3-1)*N+h7-1)*N+h8))= - t2(((((p3-1)*N+p6-1)*N+h7-1)*N+h8))
t2e(((((p3-1)*N+p6-1)*N+h7-1)*N+h8))= + t2(((((p3-1)*N+p6-1)*N+h7-1)*N+h8))
END DO
END DO
END DO
END DO
DO h7=1,nocc
DO h8=1,nocc
IF (h7>h8) CYCLE
DO p5=nocc+1,N
DO p6=nocc+1,N
IF (p5>p6) CYCLE
v2e(((((h7-1)*N+h8-1)*N+p6-1)*N+p5))= - v2(((((h7-1)*N+h8-1)*N+p5-1)*N+p6))
v2e(((((h7-1)*N+h8-1)*N+p5-1)*N+p6))= + v2(((((h7-1)*N+h8-1)*N+p5-1)*N+p6))
END DO
END DO
END DO
END DO
DO p3=nocc+1,N
DO p5=nocc+1,N
DO p6=nocc+1,N
DO h7=1,nocc
DO h8=1,nocc
IF (h7>h8) CYCLE
i1(((p3-1)*N+p5))=i1(((p3-1)*N+p5))+(-1.0d0)*t2e(((((p3-1)*N+p6-1)*N+h7-1)*N+h8))*v2e(((((h7-1)*N+h8-1)*N+p5-1)*N+p6))
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
i0(((((p3-1)*N+p4-1)*N+h1-1)*N+h2))=i0(((((p3-1)*N+p4-1)*N+h1-1)*N+h2))+(1.0d0)*t2e(((((p3-1)*N+p5-1)*N+h1-1)*N+h2))*i1(((p4-1)*N+p&
&5))+(-1.0d0)*t2e(((((p4-1)*N+p5-1)*N+h1-1)*N+h2))*i1(((p3-1)*N+p5))
END DO
END DO
END DO
END DO
END DO
DO h5=1,nocc
DO h6=1,nocc
IF (h5>h6) CYCLE
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
i1(((((h5-1)*N+h6-1)*N+h1-1)*N+h2))=(1.0d0)*v2(((((h5-1)*N+h6-1)*N+h1-1)*N+h2))
END DO
END DO
END DO
END DO
DO h5=1,nocc
DO h6=1,nocc
IF (h5>h6) CYCLE
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
DO p7=nocc+1,N
DO p8=nocc+1,N
IF (p7>p8) CYCLE
i1(((((h5-1)*N+h6-1)*N+h1-1)*N+h2))=i1(((((h5-1)*N+h6-1)*N+h1-1)*N+h2))+(1.0d0)*t2(((((p7-1)*N+p8-1)*N+h1-1)*N+h2))*v2(((((h5-1)*N+&
&h6-1)*N+p7-1)*N+p8))
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
DO h5=1,nocc
DO h6=1,nocc
IF (h5>h6) CYCLE
i0(((((p3-1)*N+p4-1)*N+h1-1)*N+h2))=i0(((((p3-1)*N+p4-1)*N+h1-1)*N+h2))+(1.0d0)*t2(((((p3-1)*N+p4-1)*N+h5-1)*N+h6))*i1(((((h5-1)*N+&
&h6-1)*N+h1-1)*N+h2))
END DO
END DO
END DO
END DO
END DO
END DO
DO p3=nocc+1,N
DO h6=1,nocc
DO h1=1,nocc
DO p5=nocc+1,N
v2e(((((h6-1)*N+p3-1)*N+h1-1)*N+p5))= + v2(((((h6-1)*N+p3-1)*N+h1-1)*N+p5))
END DO
END DO
END DO
END DO
DO p3=nocc+1,N
DO h6=1,nocc
DO h1=1,nocc
DO p5=nocc+1,N
i1(((((h6-1)*N+p3-1)*N+h1-1)*N+p5))=(1.0d0)*v2e(((((h6-1)*N+p3-1)*N+h1-1)*N+p5))
END DO
END DO
END DO
END DO
DO p3=nocc+1,N
DO p7=nocc+1,N
IF (p3>p7) CYCLE
DO h1=1,nocc
DO h8=1,nocc
IF (h1>h8) CYCLE
t2e(((((p7-1)*N+p3-1)*N+h8-1)*N+h1))= + t2(((((p3-1)*N+p7-1)*N+h1-1)*N+h8))
t2e(((((p7-1)*N+p3-1)*N+h1-1)*N+h8))= - t2(((((p3-1)*N+p7-1)*N+h1-1)*N+h8))
t2e(((((p3-1)*N+p7-1)*N+h8-1)*N+h1))= - t2(((((p3-1)*N+p7-1)*N+h1-1)*N+h8))
t2e(((((p3-1)*N+p7-1)*N+h1-1)*N+h8))= + t2(((((p3-1)*N+p7-1)*N+h1-1)*N+h8))
END DO
END DO
END DO
END DO
DO h6=1,nocc
DO h8=1,nocc
IF (h6>h8) CYCLE
DO p5=nocc+1,N
DO p7=nocc+1,N
IF (p5>p7) CYCLE
v2e(((((h8-1)*N+h6-1)*N+p7-1)*N+p5))= + v2(((((h6-1)*N+h8-1)*N+p5-1)*N+p7))
v2e(((((h8-1)*N+h6-1)*N+p5-1)*N+p7))= - v2(((((h6-1)*N+h8-1)*N+p5-1)*N+p7))
v2e(((((h6-1)*N+h8-1)*N+p7-1)*N+p5))= - v2(((((h6-1)*N+h8-1)*N+p5-1)*N+p7))
v2e(((((h6-1)*N+h8-1)*N+p5-1)*N+p7))= + v2(((((h6-1)*N+h8-1)*N+p5-1)*N+p7))
END DO
END DO
END DO
END DO
DO p3=nocc+1,N
DO h6=1,nocc
DO h1=1,nocc
DO p5=nocc+1,N
DO p7=nocc+1,N
DO h8=1,nocc
i1(((((h6-1)*N+p3-1)*N+h1-1)*N+p5))=i1(((((h6-1)*N+p3-1)*N+h1-1)*N+p5))+(-0.5d0)*t2e(((((p3-1)*N+p7-1)*N+h1-1)*N+h8))*v2e(((((h6-1)&
&*N+h8-1)*N+p5-1)*N+p7))
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
DO p3=nocc+1,N
DO p4=nocc+1,N
IF (p3>p4) CYCLE
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
DO p5=nocc+1,N
DO h6=1,nocc
i0(((((p3-1)*N+p4-1)*N+h1-1)*N+h2))=i0(((((p3-1)*N+p4-1)*N+h1-1)*N+h2))+(-1.0d0)*t2e(((((p3-1)*N+p5-1)*N+h1-1)*N+h6))*i1(((((h6-1)*&
&N+p4-1)*N+h2-1)*N+p5))+(1.0d0)*t2e(((((p3-1)*N+p5-1)*N+h2-1)*N+h6))*i1(((((h6-1)*N+p4-1)*N+h1-1)*N+p5))+(1.0d0)*t2e(((((p4-1)*N+p5&
&-1)*N+h1-1)*N+h6))*i1(((((h6-1)*N+p3-1)*N+h2-1)*N+p5))+(-1.0d0)*t2e(((((p4-1)*N+p5-1)*N+h2-1)*N+h6))*i1(((((h6-1)*N+p3-1)*N+h1-1)*&
&N+p5))
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
i1(1)=0.0d0
DO p5=nocc+1,N
DO p6=nocc+1,N
IF (p5>p6) CYCLE
DO h7=1,nocc
DO h8=1,nocc
IF (h7>h8) CYCLE
i1(1)=i1(1)+(4.0d0)*t2(((((p5-1)*N+p6-1)*N+h7-1)*N+h8))*v2(((((h7-1)*N+h8-1)*N+p5-1)*N+p6))
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
i0(((((p3-1)*N+p4-1)*N+h1-1)*N+h2))=i0(((((p3-1)*N+p4-1)*N+h1-1)*N+h2))+(0.25d0)*t2(((((p3-1)*N+p4-1)*N+h1-1)*N+h2))*i1(1)
END DO
END DO
END DO
END DO
RETURN
END SUBROUTINE
