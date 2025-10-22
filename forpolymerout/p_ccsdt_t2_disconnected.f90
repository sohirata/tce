SUBROUTINE p_ccsdt_t2_disconnected(N,f1,f1e,i0,i0e,i1,i1e,i2,i2e,i3,i3e,nocc,t1,t1e,t2,t2e,t3,t3e,v2,v2e)
IMPLICIT NONE
INTEGER :: N
INTEGER :: nocc
INTEGER :: p3
INTEGER :: p4
INTEGER :: h1
INTEGER :: h2
INTEGER :: h9
INTEGER :: p5
INTEGER :: h10
INTEGER :: h6
INTEGER :: p6
INTEGER :: p9
INTEGER :: h7
INTEGER :: h8
INTEGER :: p7
INTEGER :: p10
INTEGER :: h5
INTEGER :: p8
REAL*8 :: i0(*)
REAL*8 :: i0e(*)
REAL*8 :: v2(*)
REAL*8 :: v2e(*)
REAL*8 :: t1(*)
REAL*8 :: t1e(*)
REAL*8 :: i1(*)
REAL*8 :: i1e(*)
REAL*8 :: f1(*)
REAL*8 :: f1e(*)
REAL*8 :: i2(*)
REAL*8 :: i2e(*)
REAL*8 :: i3(*)
REAL*8 :: i3e(*)
REAL*8 :: t2(*)
REAL*8 :: t2e(*)
REAL*8 :: t3(*)
REAL*8 :: t3e(*)
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
DO h1=1,nocc
i1(((p3-1)*N+h1))=(1.0d0)*f1(((p3-1)*N+h1))
END DO
END DO
DO h10=1,nocc
DO h1=1,nocc
i2(((h10-1)*N+h1))=(1.0d0)*f1(((h10-1)*N+h1))
END DO
END DO
DO h10=1,nocc
DO p9=nocc+1,N
i3(((h10-1)*N+p9))=(1.0d0)*f1(((h10-1)*N+p9))
END DO
END DO
DO h10=1,nocc
DO h8=1,nocc
IF (h8>h10) CYCLE
DO p9=nocc+1,N
DO p7=nocc+1,N
IF (p7>p9) CYCLE
v2e(((((h10-1)*N+h8-1)*N+p9-1)*N+p7))= + v2(((((h8-1)*N+h10-1)*N+p7-1)*N+p9))
v2e(((((h10-1)*N+h8-1)*N+p7-1)*N+p9))= - v2(((((h8-1)*N+h10-1)*N+p7-1)*N+p9))
v2e(((((h8-1)*N+h10-1)*N+p9-1)*N+p7))= - v2(((((h8-1)*N+h10-1)*N+p7-1)*N+p9))
v2e(((((h8-1)*N+h10-1)*N+p7-1)*N+p9))= + v2(((((h8-1)*N+h10-1)*N+p7-1)*N+p9))
END DO
END DO
END DO
END DO
DO h10=1,nocc
DO p9=nocc+1,N
DO p7=nocc+1,N
DO h8=1,nocc
i3(((h10-1)*N+p9))=i3(((h10-1)*N+p9))+(0.9999999999999999d0)*t1(((p7-1)*N+h8))*v2e(((((h8-1)*N+h10-1)*N+p7-1)*N+p9))
END DO
END DO
END DO
END DO
DO h10=1,nocc
DO h1=1,nocc
DO p9=nocc+1,N
i2(((h10-1)*N+h1))=i2(((h10-1)*N+h1))+(1.0d0)*t1(((p9-1)*N+h1))*i3(((h10-1)*N+p9))
END DO
END DO
END DO
DO h10=1,nocc
DO h7=1,nocc
IF (h7>h10) CYCLE
DO h1=1,nocc
DO p6=nocc+1,N
v2e(((((h10-1)*N+h7-1)*N+h1-1)*N+p6))= - v2(((((h7-1)*N+h10-1)*N+h1-1)*N+p6))
v2e(((((h7-1)*N+h10-1)*N+h1-1)*N+p6))= + v2(((((h7-1)*N+h10-1)*N+h1-1)*N+p6))
END DO
END DO
END DO
END DO
DO h10=1,nocc
DO h1=1,nocc
DO p6=nocc+1,N
DO h7=1,nocc
i2(((h10-1)*N+h1))=i2(((h10-1)*N+h1))+(-1.0d0)*t1(((p6-1)*N+h7))*v2e(((((h7-1)*N+h10-1)*N+h1-1)*N+p6))
END DO
END DO
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
DO h10=1,nocc
DO h8=1,nocc
IF (h8>h10) CYCLE
DO p6=nocc+1,N
DO p7=nocc+1,N
IF (p6>p7) CYCLE
v2e(((((h10-1)*N+h8-1)*N+p6-1)*N+p7))= - v2(((((h8-1)*N+h10-1)*N+p6-1)*N+p7))
v2e(((((h8-1)*N+h10-1)*N+p6-1)*N+p7))= + v2(((((h8-1)*N+h10-1)*N+p6-1)*N+p7))
END DO
END DO
END DO
END DO
DO h10=1,nocc
DO h1=1,nocc
DO p6=nocc+1,N
DO p7=nocc+1,N
IF (p6>p7) CYCLE
DO h8=1,nocc
i2(((h10-1)*N+h1))=i2(((h10-1)*N+h1))+(-1.0d0)*t2e(((((p6-1)*N+p7-1)*N+h1-1)*N+h8))*v2e(((((h8-1)*N+h10-1)*N+p6-1)*N+p7))
END DO
END DO
END DO
END DO
END DO
DO p3=nocc+1,N
DO h1=1,nocc
DO h10=1,nocc
i1(((p3-1)*N+h1))=i1(((p3-1)*N+h1))+(-1.0d0)*t1(((p3-1)*N+h10))*i2(((h10-1)*N+h1))
END DO
END DO
END DO
DO p3=nocc+1,N
DO p5=nocc+1,N
i2(((p3-1)*N+p5))=(1.0d0)*f1(((p3-1)*N+p5))
END DO
END DO
DO p3=nocc+1,N
DO h7=1,nocc
DO p5=nocc+1,N
DO p6=nocc+1,N
IF (p5>p6) CYCLE
v2e(((((h7-1)*N+p3-1)*N+p6-1)*N+p5))= - v2(((((h7-1)*N+p3-1)*N+p5-1)*N+p6))
v2e(((((h7-1)*N+p3-1)*N+p5-1)*N+p6))= + v2(((((h7-1)*N+p3-1)*N+p5-1)*N+p6))
END DO
END DO
END DO
END DO
DO p3=nocc+1,N
DO p5=nocc+1,N
DO p6=nocc+1,N
DO h7=1,nocc
i2(((p3-1)*N+p5))=i2(((p3-1)*N+p5))+(-1.0d0)*t1(((p6-1)*N+h7))*v2e(((((h7-1)*N+p3-1)*N+p5-1)*N+p6))
END DO
END DO
END DO
END DO
DO p3=nocc+1,N
DO h1=1,nocc
DO p5=nocc+1,N
i1(((p3-1)*N+h1))=i1(((p3-1)*N+h1))+(1.0d0)*t1(((p5-1)*N+h1))*i2(((p3-1)*N+p5))
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
DO h1=1,nocc
DO p5=nocc+1,N
DO h6=1,nocc
i1(((p3-1)*N+h1))=i1(((p3-1)*N+h1))+(-1.0d0)*t1(((p5-1)*N+h6))*v2e(((((h6-1)*N+p3-1)*N+h1-1)*N+p5))
END DO
END DO
END DO
END DO
DO h7=1,nocc
DO p9=nocc+1,N
i2(((h7-1)*N+p9))=(1.0d0)*f1(((h7-1)*N+p9))
END DO
END DO
DO h7=1,nocc
DO h6=1,nocc
IF (h6>h7) CYCLE
DO p9=nocc+1,N
DO p5=nocc+1,N
IF (p5>p9) CYCLE
v2e(((((h7-1)*N+h6-1)*N+p9-1)*N+p5))= + v2(((((h6-1)*N+h7-1)*N+p5-1)*N+p9))
v2e(((((h7-1)*N+h6-1)*N+p5-1)*N+p9))= - v2(((((h6-1)*N+h7-1)*N+p5-1)*N+p9))
v2e(((((h6-1)*N+h7-1)*N+p9-1)*N+p5))= - v2(((((h6-1)*N+h7-1)*N+p5-1)*N+p9))
v2e(((((h6-1)*N+h7-1)*N+p5-1)*N+p9))= + v2(((((h6-1)*N+h7-1)*N+p5-1)*N+p9))
END DO
END DO
END DO
END DO
DO h7=1,nocc
DO p9=nocc+1,N
DO p5=nocc+1,N
DO h6=1,nocc
i2(((h7-1)*N+p9))=i2(((h7-1)*N+p9))+(1.0d0)*t1(((p5-1)*N+h6))*v2e(((((h6-1)*N+h7-1)*N+p5-1)*N+p9))
END DO
END DO
END DO
END DO
DO p3=nocc+1,N
DO p9=nocc+1,N
IF (p3>p9) CYCLE
DO h1=1,nocc
DO h7=1,nocc
IF (h1>h7) CYCLE
t2e(((((p9-1)*N+p3-1)*N+h7-1)*N+h1))= + t2(((((p3-1)*N+p9-1)*N+h1-1)*N+h7))
t2e(((((p9-1)*N+p3-1)*N+h1-1)*N+h7))= - t2(((((p3-1)*N+p9-1)*N+h1-1)*N+h7))
t2e(((((p3-1)*N+p9-1)*N+h7-1)*N+h1))= - t2(((((p3-1)*N+p9-1)*N+h1-1)*N+h7))
t2e(((((p3-1)*N+p9-1)*N+h1-1)*N+h7))= + t2(((((p3-1)*N+p9-1)*N+h1-1)*N+h7))
END DO
END DO
END DO
END DO
DO p3=nocc+1,N
DO h1=1,nocc
DO p9=nocc+1,N
DO h7=1,nocc
i1(((p3-1)*N+h1))=i1(((p3-1)*N+h1))+(1.0d0)*t2e(((((p3-1)*N+p9-1)*N+h1-1)*N+h7))*i2(((h7-1)*N+p9))
END DO
END DO
END DO
END DO
DO h6=1,nocc
DO h10=1,nocc
IF (h6>h10) CYCLE
DO h1=1,nocc
DO p9=nocc+1,N
v2e(((((h6-1)*N+h10-1)*N+h1-1)*N+p9))= + v2(((((h6-1)*N+h10-1)*N+h1-1)*N+p9))
END DO
END DO
END DO
END DO
DO h6=1,nocc
DO h10=1,nocc
IF (h6>h10) CYCLE
DO h1=1,nocc
DO p9=nocc+1,N
i2(((((h6-1)*N+h10-1)*N+h1-1)*N+p9))=(-1.0d0)*v2e(((((h6-1)*N+h10-1)*N+h1-1)*N+p9))
END DO
END DO
END DO
END DO
DO h6=1,nocc
DO h10=1,nocc
IF (h6>h10) CYCLE
DO p9=nocc+1,N
DO p5=nocc+1,N
IF (p5>p9) CYCLE
v2e(((((h6-1)*N+h10-1)*N+p9-1)*N+p5))= - v2(((((h6-1)*N+h10-1)*N+p5-1)*N+p9))
v2e(((((h6-1)*N+h10-1)*N+p5-1)*N+p9))= + v2(((((h6-1)*N+h10-1)*N+p5-1)*N+p9))
END DO
END DO
END DO
END DO
DO h6=1,nocc
DO h10=1,nocc
IF (h6>h10) CYCLE
DO h1=1,nocc
DO p9=nocc+1,N
DO p5=nocc+1,N
i2(((((h6-1)*N+h10-1)*N+h1-1)*N+p9))=i2(((((h6-1)*N+h10-1)*N+h1-1)*N+p9))+(-1.0d0)*t1(((p5-1)*N+h1))*v2e(((((h6-1)*N+h10-1)*N+p5-1)&
&*N+p9))
END DO
END DO
END DO
END DO
END DO
DO p3=nocc+1,N
DO p9=nocc+1,N
IF (p3>p9) CYCLE
DO h6=1,nocc
DO h10=1,nocc
IF (h6>h10) CYCLE
t2e(((((p9-1)*N+p3-1)*N+h6-1)*N+h10))= - t2(((((p3-1)*N+p9-1)*N+h6-1)*N+h10))
t2e(((((p3-1)*N+p9-1)*N+h6-1)*N+h10))= + t2(((((p3-1)*N+p9-1)*N+h6-1)*N+h10))
END DO
END DO
END DO
END DO
DO p3=nocc+1,N
DO h1=1,nocc
DO p9=nocc+1,N
DO h6=1,nocc
DO h10=1,nocc
IF (h6>h10) CYCLE
i1(((p3-1)*N+h1))=i1(((p3-1)*N+h1))+(1.0d0)*t2e(((((p3-1)*N+p9-1)*N+h6-1)*N+h10))*i2(((((h6-1)*N+h10-1)*N+h1-1)*N+p9))
END DO
END DO
END DO
END DO
END DO
DO p5=nocc+1,N
DO p6=nocc+1,N
IF (p5>p6) CYCLE
DO h1=1,nocc
DO h7=1,nocc
IF (h1>h7) CYCLE
t2e(((((p5-1)*N+p6-1)*N+h7-1)*N+h1))= - t2(((((p5-1)*N+p6-1)*N+h1-1)*N+h7))
t2e(((((p5-1)*N+p6-1)*N+h1-1)*N+h7))= + t2(((((p5-1)*N+p6-1)*N+h1-1)*N+h7))
END DO
END DO
END DO
END DO
DO p3=nocc+1,N
DO h7=1,nocc
DO p5=nocc+1,N
DO p6=nocc+1,N
IF (p5>p6) CYCLE
v2e(((((h7-1)*N+p3-1)*N+p5-1)*N+p6))= + v2(((((h7-1)*N+p3-1)*N+p5-1)*N+p6))
END DO
END DO
END DO
END DO
DO p3=nocc+1,N
DO h1=1,nocc
DO p5=nocc+1,N
DO p6=nocc+1,N
IF (p5>p6) CYCLE
DO h7=1,nocc
i1(((p3-1)*N+h1))=i1(((p3-1)*N+h1))+(-1.0d0)*t2e(((((p5-1)*N+p6-1)*N+h1-1)*N+h7))*v2e(((((h7-1)*N+p3-1)*N+p5-1)*N+p6))
END DO
END DO
END DO
END DO
END DO
DO p3=nocc+1,N
DO p5=nocc+1,N
DO p6=nocc+1,N
IF (p3>p5) CYCLE
IF (p5>p6) CYCLE
DO h1=1,nocc
DO h7=1,nocc
DO h8=1,nocc
IF (h1>h7) CYCLE
IF (h7>h8) CYCLE
t3e(((((((p5-1)*N+p3-1)*N+p6-1)*N+h7-1)*N+h1-1)*N+h8))= + t3(((((((p3-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h7-1)*N+h8))
t3e(((((((p5-1)*N+p3-1)*N+p6-1)*N+h8-1)*N+h1-1)*N+h7))= - t3(((((((p3-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h7-1)*N+h8))
t3e(((((((p5-1)*N+p3-1)*N+p6-1)*N+h1-1)*N+h7-1)*N+h8))= - t3(((((((p3-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h7-1)*N+h8))
t3e(((((((p6-1)*N+p3-1)*N+p5-1)*N+h7-1)*N+h1-1)*N+h8))= - t3(((((((p3-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h7-1)*N+h8))
t3e(((((((p6-1)*N+p3-1)*N+p5-1)*N+h8-1)*N+h1-1)*N+h7))= + t3(((((((p3-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h7-1)*N+h8))
t3e(((((((p6-1)*N+p3-1)*N+p5-1)*N+h1-1)*N+h7-1)*N+h8))= + t3(((((((p3-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h7-1)*N+h8))
t3e(((((((p3-1)*N+p5-1)*N+p6-1)*N+h7-1)*N+h1-1)*N+h8))= - t3(((((((p3-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h7-1)*N+h8))
t3e(((((((p3-1)*N+p5-1)*N+p6-1)*N+h8-1)*N+h1-1)*N+h7))= + t3(((((((p3-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h7-1)*N+h8))
t3e(((((((p3-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h7-1)*N+h8))= + t3(((((((p3-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h7-1)*N+h8))
END DO
END DO
END DO
END DO
END DO
END DO
DO p3=nocc+1,N
DO h1=1,nocc
DO p5=nocc+1,N
DO p6=nocc+1,N
IF (p5>p6) CYCLE
DO h7=1,nocc
DO h8=1,nocc
IF (h7>h8) CYCLE
i1(((p3-1)*N+h1))=i1(((p3-1)*N+h1))+(1.0d0)*t3e(((((((p3-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h7-1)*N+h8))*v2(((((h7-1)*N+h8-1)*N+p5-1)*N+p&
&6))
END DO
END DO
END DO
END DO
END DO
END DO
DO h6=1,nocc
DO p5=nocc+1,N
i3(((h6-1)*N+p5))=(1.0d0)*f1(((h6-1)*N+p5))
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
DO h6=1,nocc
DO p5=nocc+1,N
DO p7=nocc+1,N
DO h8=1,nocc
i3(((h6-1)*N+p5))=i3(((h6-1)*N+p5))+(0.5d0)*t1(((p7-1)*N+h8))*v2e(((((h6-1)*N+h8-1)*N+p5-1)*N+p7))
END DO
END DO
END DO
END DO
i2(1)=0.0d0
DO p5=nocc+1,N
DO h6=1,nocc
i2(1)=i2(1)+(1.0d0)*t1(((p5-1)*N+h6))*i3(((h6-1)*N+p5))
END DO
END DO
DO p5=nocc+1,N
DO p6=nocc+1,N
IF (p5>p6) CYCLE
DO h7=1,nocc
DO h8=1,nocc
IF (h7>h8) CYCLE
i2(1)=i2(1)+(1.0d0)*t2(((((p5-1)*N+p6-1)*N+h7-1)*N+h8))*v2(((((h7-1)*N+h8-1)*N+p5-1)*N+p6))
END DO
END DO
END DO
END DO
DO p3=nocc+1,N
DO h1=1,nocc
i1(((p3-1)*N+h1))=i1(((p3-1)*N+h1))+(0.5d0)*t1(((p3-1)*N+h1))*i2(1)
END DO
END DO
DO p3=nocc+1,N
DO p4=nocc+1,N
IF (p3>p4) CYCLE
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
i0(((((p3-1)*N+p4-1)*N+h1-1)*N+h2))=i0(((((p3-1)*N+p4-1)*N+h1-1)*N+h2))+(1.0d0)*t1(((p3-1)*N+h1))*i1(((p4-1)*N+h2))+(-1.0d0)*t1(((p&
&3-1)*N+h2))*i1(((p4-1)*N+h1))+(-1.0d0)*t1(((p4-1)*N+h1))*i1(((p3-1)*N+h2))+(1.0d0)*t1(((p4-1)*N+h2))*i1(((p3-1)*N+h1))
END DO
END DO
END DO
END DO
DO p3=nocc+1,N
DO h9=1,nocc
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
v2e(((((h9-1)*N+p3-1)*N+h1-1)*N+h2))= + v2(((((h9-1)*N+p3-1)*N+h1-1)*N+h2))
END DO
END DO
END DO
END DO
DO p3=nocc+1,N
DO h9=1,nocc
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
i1(((((h9-1)*N+p3-1)*N+h1-1)*N+h2))=(1.0d0)*v2e(((((h9-1)*N+p3-1)*N+h1-1)*N+h2))
END DO
END DO
END DO
END DO
DO h6=1,nocc
DO h9=1,nocc
IF (h6>h9) CYCLE
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
i2(((((h6-1)*N+h9-1)*N+h1-1)*N+h2))=(1.0d0)*v2(((((h6-1)*N+h9-1)*N+h1-1)*N+h2))
END DO
END DO
END DO
END DO
DO h6=1,nocc
DO h9=1,nocc
IF (h6>h9) CYCLE
DO h1=1,nocc
DO p7=nocc+1,N
v2e(((((h6-1)*N+h9-1)*N+h1-1)*N+p7))= + v2(((((h6-1)*N+h9-1)*N+h1-1)*N+p7))
END DO
END DO
END DO
END DO
DO h6=1,nocc
DO h9=1,nocc
IF (h6>h9) CYCLE
DO h1=1,nocc
DO p7=nocc+1,N
i3(((((h6-1)*N+h9-1)*N+h1-1)*N+p7))=(1.0d0)*v2e(((((h6-1)*N+h9-1)*N+h1-1)*N+p7))
END DO
END DO
END DO
END DO
DO h6=1,nocc
DO h9=1,nocc
IF (h6>h9) CYCLE
DO p7=nocc+1,N
DO p8=nocc+1,N
IF (p7>p8) CYCLE
v2e(((((h6-1)*N+h9-1)*N+p8-1)*N+p7))= - v2(((((h6-1)*N+h9-1)*N+p7-1)*N+p8))
v2e(((((h6-1)*N+h9-1)*N+p7-1)*N+p8))= + v2(((((h6-1)*N+h9-1)*N+p7-1)*N+p8))
END DO
END DO
END DO
END DO
DO h6=1,nocc
DO h9=1,nocc
IF (h6>h9) CYCLE
DO h1=1,nocc
DO p7=nocc+1,N
DO p8=nocc+1,N
i3(((((h6-1)*N+h9-1)*N+h1-1)*N+p7))=i3(((((h6-1)*N+h9-1)*N+h1-1)*N+p7))+(-0.5d0)*t1(((p8-1)*N+h1))*v2e(((((h6-1)*N+h9-1)*N+p7-1)*N+&
&p8))
END DO
END DO
END DO
END DO
END DO
DO h6=1,nocc
DO h9=1,nocc
IF (h6>h9) CYCLE
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
DO p7=nocc+1,N
i2(((((h6-1)*N+h9-1)*N+h1-1)*N+h2))=i2(((((h6-1)*N+h9-1)*N+h1-1)*N+h2))+(-1.0d0)*t1(((p7-1)*N+h1))*i3(((((h6-1)*N+h9-1)*N+h2-1)*N+p&
&7))+(1.0d0)*t1(((p7-1)*N+h2))*i3(((((h6-1)*N+h9-1)*N+h1-1)*N+p7))
END DO
END DO
END DO
END DO
END DO
DO h6=1,nocc
DO h9=1,nocc
IF (h6>h9) CYCLE
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
DO p7=nocc+1,N
DO p8=nocc+1,N
IF (p7>p8) CYCLE
i2(((((h6-1)*N+h9-1)*N+h1-1)*N+h2))=i2(((((h6-1)*N+h9-1)*N+h1-1)*N+h2))+(1.0d0)*t2(((((p7-1)*N+p8-1)*N+h1-1)*N+h2))*v2(((((h6-1)*N+&
&h9-1)*N+p7-1)*N+p8))
END DO
END DO
END DO
END DO
END DO
END DO
DO h6=1,nocc
DO h9=1,nocc
IF (h6>h9) CYCLE
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
i2e(((((h9-1)*N+h6-1)*N+h1-1)*N+h2))= - i2(((((h6-1)*N+h9-1)*N+h1-1)*N+h2))
i2e(((((h6-1)*N+h9-1)*N+h1-1)*N+h2))= + i2(((((h6-1)*N+h9-1)*N+h1-1)*N+h2))
END DO
END DO
END DO
END DO
DO p3=nocc+1,N
DO h9=1,nocc
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
DO h6=1,nocc
i1(((((h9-1)*N+p3-1)*N+h1-1)*N+h2))=i1(((((h9-1)*N+p3-1)*N+h1-1)*N+h2))+(0.5d0)*t1(((p3-1)*N+h6))*i2e(((((h6-1)*N+h9-1)*N+h1-1)*N+h&
&2))
END DO
END DO
END DO
END DO
END DO
DO p3=nocc+1,N
DO h9=1,nocc
DO h1=1,nocc
DO p6=nocc+1,N
v2e(((((h9-1)*N+p3-1)*N+h1-1)*N+p6))= + v2(((((h9-1)*N+p3-1)*N+h1-1)*N+p6))
END DO
END DO
END DO
END DO
DO p3=nocc+1,N
DO h9=1,nocc
DO h1=1,nocc
DO p6=nocc+1,N
i2(((((h9-1)*N+p3-1)*N+h1-1)*N+p6))=(1.0d0)*v2e(((((h9-1)*N+p3-1)*N+h1-1)*N+p6))
END DO
END DO
END DO
END DO
DO p3=nocc+1,N
DO h9=1,nocc
DO p6=nocc+1,N
DO p7=nocc+1,N
IF (p6>p7) CYCLE
v2e(((((h9-1)*N+p3-1)*N+p7-1)*N+p6))= - v2(((((h9-1)*N+p3-1)*N+p6-1)*N+p7))
v2e(((((h9-1)*N+p3-1)*N+p6-1)*N+p7))= + v2(((((h9-1)*N+p3-1)*N+p6-1)*N+p7))
END DO
END DO
END DO
END DO
DO p3=nocc+1,N
DO h9=1,nocc
DO h1=1,nocc
DO p6=nocc+1,N
DO p7=nocc+1,N
i2(((((h9-1)*N+p3-1)*N+h1-1)*N+p6))=i2(((((h9-1)*N+p3-1)*N+h1-1)*N+p6))+(-0.5d0)*t1(((p7-1)*N+h1))*v2e(((((h9-1)*N+p3-1)*N+p6-1)*N+&
&p7))
END DO
END DO
END DO
END DO
END DO
DO p3=nocc+1,N
DO h9=1,nocc
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
DO p6=nocc+1,N
i1(((((h9-1)*N+p3-1)*N+h1-1)*N+h2))=i1(((((h9-1)*N+p3-1)*N+h1-1)*N+h2))+(-1.0d0)*t1(((p6-1)*N+h1))*i2(((((h9-1)*N+p3-1)*N+h2-1)*N+p&
&6))+(1.0d0)*t1(((p6-1)*N+h2))*i2(((((h9-1)*N+p3-1)*N+h1-1)*N+p6))
END DO
END DO
END DO
END DO
END DO
DO h9=1,nocc
DO p5=nocc+1,N
i2(((h9-1)*N+p5))=(1.0d0)*f1(((h9-1)*N+p5))
END DO
END DO
DO h9=1,nocc
DO h7=1,nocc
IF (h7>h9) CYCLE
DO p5=nocc+1,N
DO p6=nocc+1,N
IF (p5>p6) CYCLE
v2e(((((h9-1)*N+h7-1)*N+p6-1)*N+p5))= + v2(((((h7-1)*N+h9-1)*N+p5-1)*N+p6))
v2e(((((h9-1)*N+h7-1)*N+p5-1)*N+p6))= - v2(((((h7-1)*N+h9-1)*N+p5-1)*N+p6))
v2e(((((h7-1)*N+h9-1)*N+p6-1)*N+p5))= - v2(((((h7-1)*N+h9-1)*N+p5-1)*N+p6))
v2e(((((h7-1)*N+h9-1)*N+p5-1)*N+p6))= + v2(((((h7-1)*N+h9-1)*N+p5-1)*N+p6))
END DO
END DO
END DO
END DO
DO h9=1,nocc
DO p5=nocc+1,N
DO p6=nocc+1,N
DO h7=1,nocc
i2(((h9-1)*N+p5))=i2(((h9-1)*N+p5))+(-1.0d0)*t1(((p6-1)*N+h7))*v2e(((((h7-1)*N+h9-1)*N+p5-1)*N+p6))
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
DO h9=1,nocc
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
DO p5=nocc+1,N
i1(((((h9-1)*N+p3-1)*N+h1-1)*N+h2))=i1(((((h9-1)*N+p3-1)*N+h1-1)*N+h2))+(-1.0d0)*t2e(((((p3-1)*N+p5-1)*N+h1-1)*N+h2))*i2(((h9-1)*N+&
&p5))
END DO
END DO
END DO
END DO
END DO
DO h5=1,nocc
DO h9=1,nocc
IF (h5>h9) CYCLE
DO h1=1,nocc
DO p10=nocc+1,N
v2e(((((h5-1)*N+h9-1)*N+h1-1)*N+p10))= + v2(((((h5-1)*N+h9-1)*N+h1-1)*N+p10))
END DO
END DO
END DO
END DO
DO h5=1,nocc
DO h9=1,nocc
IF (h5>h9) CYCLE
DO h1=1,nocc
DO p10=nocc+1,N
i2(((((h5-1)*N+h9-1)*N+h1-1)*N+p10))=(1.0d0)*v2e(((((h5-1)*N+h9-1)*N+h1-1)*N+p10))
END DO
END DO
END DO
END DO
DO h5=1,nocc
DO h9=1,nocc
IF (h5>h9) CYCLE
DO p10=nocc+1,N
DO p6=nocc+1,N
IF (p6>p10) CYCLE
v2e(((((h5-1)*N+h9-1)*N+p10-1)*N+p6))= - v2(((((h5-1)*N+h9-1)*N+p6-1)*N+p10))
v2e(((((h5-1)*N+h9-1)*N+p6-1)*N+p10))= + v2(((((h5-1)*N+h9-1)*N+p6-1)*N+p10))
END DO
END DO
END DO
END DO
DO h5=1,nocc
DO h9=1,nocc
IF (h5>h9) CYCLE
DO h1=1,nocc
DO p10=nocc+1,N
DO p6=nocc+1,N
i2(((((h5-1)*N+h9-1)*N+h1-1)*N+p10))=i2(((((h5-1)*N+h9-1)*N+h1-1)*N+p10))+(1.0d0)*t1(((p6-1)*N+h1))*v2e(((((h5-1)*N+h9-1)*N+p6-1)*N&
&+p10))
END DO
END DO
END DO
END DO
END DO
DO p3=nocc+1,N
DO p10=nocc+1,N
IF (p3>p10) CYCLE
DO h1=1,nocc
DO h5=1,nocc
IF (h1>h5) CYCLE
t2e(((((p10-1)*N+p3-1)*N+h5-1)*N+h1))= + t2(((((p3-1)*N+p10-1)*N+h1-1)*N+h5))
t2e(((((p10-1)*N+p3-1)*N+h1-1)*N+h5))= - t2(((((p3-1)*N+p10-1)*N+h1-1)*N+h5))
t2e(((((p3-1)*N+p10-1)*N+h5-1)*N+h1))= - t2(((((p3-1)*N+p10-1)*N+h1-1)*N+h5))
t2e(((((p3-1)*N+p10-1)*N+h1-1)*N+h5))= + t2(((((p3-1)*N+p10-1)*N+h1-1)*N+h5))
END DO
END DO
END DO
END DO
DO h5=1,nocc
DO h9=1,nocc
IF (h5>h9) CYCLE
DO h2=1,nocc
DO p10=nocc+1,N
i2e(((((h9-1)*N+h5-1)*N+h2-1)*N+p10))= - i2(((((h5-1)*N+h9-1)*N+h2-1)*N+p10))
i2e(((((h5-1)*N+h9-1)*N+h2-1)*N+p10))= + i2(((((h5-1)*N+h9-1)*N+h2-1)*N+p10))
END DO
END DO
END DO
END DO
DO p3=nocc+1,N
DO h9=1,nocc
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
DO p10=nocc+1,N
DO h5=1,nocc
i1(((((h9-1)*N+p3-1)*N+h1-1)*N+h2))=i1(((((h9-1)*N+p3-1)*N+h1-1)*N+h2))+(1.0d0)*t2e(((((p3-1)*N+p10-1)*N+h1-1)*N+h5))*i2e(((((h5-1)&
&*N+h9-1)*N+h2-1)*N+p10))+(-1.0d0)*t2e(((((p3-1)*N+p10-1)*N+h2-1)*N+h5))*i2e(((((h5-1)*N+h9-1)*N+h1-1)*N+p10))
END DO
END DO
END DO
END DO
END DO
END DO
DO p3=nocc+1,N
DO h9=1,nocc
DO p5=nocc+1,N
DO p6=nocc+1,N
IF (p5>p6) CYCLE
v2e(((((h9-1)*N+p3-1)*N+p5-1)*N+p6))= + v2(((((h9-1)*N+p3-1)*N+p5-1)*N+p6))
END DO
END DO
END DO
END DO
DO p3=nocc+1,N
DO h9=1,nocc
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
DO p5=nocc+1,N
DO p6=nocc+1,N
IF (p5>p6) CYCLE
i1(((((h9-1)*N+p3-1)*N+h1-1)*N+h2))=i1(((((h9-1)*N+p3-1)*N+h1-1)*N+h2))+(1.0d0)*t2(((((p5-1)*N+p6-1)*N+h1-1)*N+h2))*v2e(((((h9-1)*N&
&+p3-1)*N+p5-1)*N+p6))
END DO
END DO
END DO
END DO
END DO
END DO
DO p3=nocc+1,N
DO p5=nocc+1,N
DO p6=nocc+1,N
IF (p3>p5) CYCLE
IF (p5>p6) CYCLE
DO h1=1,nocc
DO h2=1,nocc
DO h7=1,nocc
IF (h1>h2) CYCLE
IF (h2>h7) CYCLE
t3e(((((((p5-1)*N+p3-1)*N+p6-1)*N+h2-1)*N+h7-1)*N+h1))= - t3(((((((p3-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h7))
t3e(((((((p5-1)*N+p3-1)*N+p6-1)*N+h1-1)*N+h7-1)*N+h2))= + t3(((((((p3-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h7))
t3e(((((((p5-1)*N+p3-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h7))= - t3(((((((p3-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h7))
t3e(((((((p6-1)*N+p3-1)*N+p5-1)*N+h2-1)*N+h7-1)*N+h1))= + t3(((((((p3-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h7))
t3e(((((((p6-1)*N+p3-1)*N+p5-1)*N+h1-1)*N+h7-1)*N+h2))= - t3(((((((p3-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h7))
t3e(((((((p6-1)*N+p3-1)*N+p5-1)*N+h1-1)*N+h2-1)*N+h7))= + t3(((((((p3-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h7))
t3e(((((((p3-1)*N+p5-1)*N+p6-1)*N+h2-1)*N+h7-1)*N+h1))= + t3(((((((p3-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h7))
t3e(((((((p3-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h7-1)*N+h2))= - t3(((((((p3-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h7))
t3e(((((((p3-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h7))= + t3(((((((p3-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h7))
END DO
END DO
END DO
END DO
END DO
END DO
DO h9=1,nocc
DO h7=1,nocc
IF (h7>h9) CYCLE
DO p5=nocc+1,N
DO p6=nocc+1,N
IF (p5>p6) CYCLE
v2e(((((h9-1)*N+h7-1)*N+p5-1)*N+p6))= - v2(((((h7-1)*N+h9-1)*N+p5-1)*N+p6))
v2e(((((h7-1)*N+h9-1)*N+p5-1)*N+p6))= + v2(((((h7-1)*N+h9-1)*N+p5-1)*N+p6))
END DO
END DO
END DO
END DO
DO p3=nocc+1,N
DO h9=1,nocc
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
DO p5=nocc+1,N
DO p6=nocc+1,N
IF (p5>p6) CYCLE
DO h7=1,nocc
i1(((((h9-1)*N+p3-1)*N+h1-1)*N+h2))=i1(((((h9-1)*N+p3-1)*N+h1-1)*N+h2))+(1.0d0)*t3e(((((((p3-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h&
&7))*v2e(((((h7-1)*N+h9-1)*N+p5-1)*N+p6))
END DO
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
DO h9=1,nocc
i0(((((p3-1)*N+p4-1)*N+h1-1)*N+h2))=i0(((((p3-1)*N+p4-1)*N+h1-1)*N+h2))+(-1.0d0)*t1(((p3-1)*N+h9))*i1(((((h9-1)*N+p4-1)*N+h1-1)*N+h&
&2))+(1.0d0)*t1(((p4-1)*N+h9))*i1(((((h9-1)*N+p3-1)*N+h1-1)*N+h2))
END DO
END DO
END DO
END DO
END DO
DO p3=nocc+1,N
DO p4=nocc+1,N
IF (p3>p4) CYCLE
DO h1=1,nocc
DO p5=nocc+1,N
v2e(((((p3-1)*N+p4-1)*N+h1-1)*N+p5))= + v2(((((p3-1)*N+p4-1)*N+h1-1)*N+p5))
END DO
END DO
END DO
END DO
DO p3=nocc+1,N
DO p4=nocc+1,N
IF (p3>p4) CYCLE
DO h1=1,nocc
DO p5=nocc+1,N
i1(((((p3-1)*N+p4-1)*N+h1-1)*N+p5))=(1.0d0)*v2e(((((p3-1)*N+p4-1)*N+h1-1)*N+p5))
END DO
END DO
END DO
END DO
DO p3=nocc+1,N
DO p4=nocc+1,N
IF (p3>p4) CYCLE
DO p5=nocc+1,N
DO p6=nocc+1,N
IF (p5>p6) CYCLE
v2e(((((p3-1)*N+p4-1)*N+p6-1)*N+p5))= - v2(((((p3-1)*N+p4-1)*N+p5-1)*N+p6))
v2e(((((p3-1)*N+p4-1)*N+p5-1)*N+p6))= + v2(((((p3-1)*N+p4-1)*N+p5-1)*N+p6))
END DO
END DO
END DO
END DO
DO p3=nocc+1,N
DO p4=nocc+1,N
IF (p3>p4) CYCLE
DO h1=1,nocc
DO p5=nocc+1,N
DO p6=nocc+1,N
i1(((((p3-1)*N+p4-1)*N+h1-1)*N+p5))=i1(((((p3-1)*N+p4-1)*N+h1-1)*N+p5))+(-0.5d0)*t1(((p6-1)*N+h1))*v2e(((((p3-1)*N+p4-1)*N+p5-1)*N+&
&p6))
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
i0(((((p3-1)*N+p4-1)*N+h1-1)*N+h2))=i0(((((p3-1)*N+p4-1)*N+h1-1)*N+h2))+(-1.0d0)*t1(((p5-1)*N+h1))*i1(((((p3-1)*N+p4-1)*N+h2-1)*N+p&
&5))+(1.0d0)*t1(((p5-1)*N+h2))*i1(((((p3-1)*N+p4-1)*N+h1-1)*N+p5))
END DO
END DO
END DO
END DO
END DO
DO h9=1,nocc
DO h1=1,nocc
i1(((h9-1)*N+h1))=(1.0d0)*f1(((h9-1)*N+h1))
END DO
END DO
DO h9=1,nocc
DO p8=nocc+1,N
i2(((h9-1)*N+p8))=(1.0d0)*f1(((h9-1)*N+p8))
END DO
END DO
DO h9=1,nocc
DO h7=1,nocc
IF (h7>h9) CYCLE
DO p8=nocc+1,N
DO p6=nocc+1,N
IF (p6>p8) CYCLE
v2e(((((h9-1)*N+h7-1)*N+p8-1)*N+p6))= + v2(((((h7-1)*N+h9-1)*N+p6-1)*N+p8))
v2e(((((h9-1)*N+h7-1)*N+p6-1)*N+p8))= - v2(((((h7-1)*N+h9-1)*N+p6-1)*N+p8))
v2e(((((h7-1)*N+h9-1)*N+p8-1)*N+p6))= - v2(((((h7-1)*N+h9-1)*N+p6-1)*N+p8))
v2e(((((h7-1)*N+h9-1)*N+p6-1)*N+p8))= + v2(((((h7-1)*N+h9-1)*N+p6-1)*N+p8))
END DO
END DO
END DO
END DO
DO h9=1,nocc
DO p8=nocc+1,N
DO p6=nocc+1,N
DO h7=1,nocc
i2(((h9-1)*N+p8))=i2(((h9-1)*N+p8))+(1.0d0)*t1(((p6-1)*N+h7))*v2e(((((h7-1)*N+h9-1)*N+p6-1)*N+p8))
END DO
END DO
END DO
END DO
DO h9=1,nocc
DO h1=1,nocc
DO p8=nocc+1,N
i1(((h9-1)*N+h1))=i1(((h9-1)*N+h1))+(1.0d0)*t1(((p8-1)*N+h1))*i2(((h9-1)*N+p8))
END DO
END DO
END DO
DO h9=1,nocc
DO h7=1,nocc
IF (h7>h9) CYCLE
DO h1=1,nocc
DO p6=nocc+1,N
v2e(((((h9-1)*N+h7-1)*N+h1-1)*N+p6))= - v2(((((h7-1)*N+h9-1)*N+h1-1)*N+p6))
v2e(((((h7-1)*N+h9-1)*N+h1-1)*N+p6))= + v2(((((h7-1)*N+h9-1)*N+h1-1)*N+p6))
END DO
END DO
END DO
END DO
DO h9=1,nocc
DO h1=1,nocc
DO p6=nocc+1,N
DO h7=1,nocc
i1(((h9-1)*N+h1))=i1(((h9-1)*N+h1))+(-1.0d0)*t1(((p6-1)*N+h7))*v2e(((((h7-1)*N+h9-1)*N+h1-1)*N+p6))
END DO
END DO
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
DO h9=1,nocc
DO h8=1,nocc
IF (h8>h9) CYCLE
DO p6=nocc+1,N
DO p7=nocc+1,N
IF (p6>p7) CYCLE
v2e(((((h9-1)*N+h8-1)*N+p6-1)*N+p7))= - v2(((((h8-1)*N+h9-1)*N+p6-1)*N+p7))
v2e(((((h8-1)*N+h9-1)*N+p6-1)*N+p7))= + v2(((((h8-1)*N+h9-1)*N+p6-1)*N+p7))
END DO
END DO
END DO
END DO
DO h9=1,nocc
DO h1=1,nocc
DO p6=nocc+1,N
DO p7=nocc+1,N
IF (p6>p7) CYCLE
DO h8=1,nocc
i1(((h9-1)*N+h1))=i1(((h9-1)*N+h1))+(-1.0d0)*t2e(((((p6-1)*N+p7-1)*N+h1-1)*N+h8))*v2e(((((h8-1)*N+h9-1)*N+p6-1)*N+p7))
END DO
END DO
END DO
END DO
END DO
DO p3=nocc+1,N
DO p4=nocc+1,N
IF (p3>p4) CYCLE
DO h1=1,nocc
DO h9=1,nocc
IF (h1>h9) CYCLE
t2e(((((p3-1)*N+p4-1)*N+h9-1)*N+h1))= - t2(((((p3-1)*N+p4-1)*N+h1-1)*N+h9))
t2e(((((p3-1)*N+p4-1)*N+h1-1)*N+h9))= + t2(((((p3-1)*N+p4-1)*N+h1-1)*N+h9))
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
DO h9=1,nocc
i0(((((p3-1)*N+p4-1)*N+h1-1)*N+h2))=i0(((((p3-1)*N+p4-1)*N+h1-1)*N+h2))+(-1.0d0)*t2e(((((p3-1)*N+p4-1)*N+h1-1)*N+h9))*i1(((h9-1)*N+&
&h2))+(1.0d0)*t2e(((((p3-1)*N+p4-1)*N+h2-1)*N+h9))*i1(((h9-1)*N+h1))
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
DO h7=1,nocc
DO p5=nocc+1,N
DO p6=nocc+1,N
IF (p5>p6) CYCLE
v2e(((((h7-1)*N+p3-1)*N+p6-1)*N+p5))= - v2(((((h7-1)*N+p3-1)*N+p5-1)*N+p6))
v2e(((((h7-1)*N+p3-1)*N+p5-1)*N+p6))= + v2(((((h7-1)*N+p3-1)*N+p5-1)*N+p6))
END DO
END DO
END DO
END DO
DO p3=nocc+1,N
DO p5=nocc+1,N
DO p6=nocc+1,N
DO h7=1,nocc
i1(((p3-1)*N+p5))=i1(((p3-1)*N+p5))+(-1.0d0)*t1(((p6-1)*N+h7))*v2e(((((h7-1)*N+p3-1)*N+p5-1)*N+p6))
END DO
END DO
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
DO h9=1,nocc
DO h10=1,nocc
IF (h9>h10) CYCLE
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
i1(((((h9-1)*N+h10-1)*N+h1-1)*N+h2))=(1.0d0)*v2(((((h9-1)*N+h10-1)*N+h1-1)*N+h2))
END DO
END DO
END DO
END DO
DO h9=1,nocc
DO h10=1,nocc
IF (h9>h10) CYCLE
DO h1=1,nocc
DO p8=nocc+1,N
v2e(((((h9-1)*N+h10-1)*N+h1-1)*N+p8))= + v2(((((h9-1)*N+h10-1)*N+h1-1)*N+p8))
END DO
END DO
END DO
END DO
DO h9=1,nocc
DO h10=1,nocc
IF (h9>h10) CYCLE
DO h1=1,nocc
DO p8=nocc+1,N
i2(((((h9-1)*N+h10-1)*N+h1-1)*N+p8))=(1.0d0)*v2e(((((h9-1)*N+h10-1)*N+h1-1)*N+p8))
END DO
END DO
END DO
END DO
DO h9=1,nocc
DO h10=1,nocc
IF (h9>h10) CYCLE
DO p8=nocc+1,N
DO p6=nocc+1,N
IF (p6>p8) CYCLE
v2e(((((h9-1)*N+h10-1)*N+p8-1)*N+p6))= - v2(((((h9-1)*N+h10-1)*N+p6-1)*N+p8))
v2e(((((h9-1)*N+h10-1)*N+p6-1)*N+p8))= + v2(((((h9-1)*N+h10-1)*N+p6-1)*N+p8))
END DO
END DO
END DO
END DO
DO h9=1,nocc
DO h10=1,nocc
IF (h9>h10) CYCLE
DO h1=1,nocc
DO p8=nocc+1,N
DO p6=nocc+1,N
i2(((((h9-1)*N+h10-1)*N+h1-1)*N+p8))=i2(((((h9-1)*N+h10-1)*N+h1-1)*N+p8))+(0.5d0)*t1(((p6-1)*N+h1))*v2e(((((h9-1)*N+h10-1)*N+p6-1)*&
&N+p8))
END DO
END DO
END DO
END DO
END DO
DO h9=1,nocc
DO h10=1,nocc
IF (h9>h10) CYCLE
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
DO p8=nocc+1,N
i1(((((h9-1)*N+h10-1)*N+h1-1)*N+h2))=i1(((((h9-1)*N+h10-1)*N+h1-1)*N+h2))+(-1.0d0)*t1(((p8-1)*N+h1))*i2(((((h9-1)*N+h10-1)*N+h2-1)*&
&N+p8))+(1.0d0)*t1(((p8-1)*N+h2))*i2(((((h9-1)*N+h10-1)*N+h1-1)*N+p8))
END DO
END DO
END DO
END DO
END DO
DO h9=1,nocc
DO h10=1,nocc
IF (h9>h10) CYCLE
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
DO p7=nocc+1,N
DO p8=nocc+1,N
IF (p7>p8) CYCLE
i1(((((h9-1)*N+h10-1)*N+h1-1)*N+h2))=i1(((((h9-1)*N+h10-1)*N+h1-1)*N+h2))+(1.0d0)*t2(((((p7-1)*N+p8-1)*N+h1-1)*N+h2))*v2(((((h9-1)*&
&N+h10-1)*N+p7-1)*N+p8))
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
DO h9=1,nocc
DO h10=1,nocc
IF (h9>h10) CYCLE
i0(((((p3-1)*N+p4-1)*N+h1-1)*N+h2))=i0(((((p3-1)*N+p4-1)*N+h1-1)*N+h2))+(1.0d0)*t2(((((p3-1)*N+p4-1)*N+h9-1)*N+h10))*i1(((((h9-1)*N&
&+h10-1)*N+h1-1)*N+h2))
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
DO h6=1,nocc
DO p5=nocc+1,N
DO p7=nocc+1,N
IF (p5>p7) CYCLE
v2e(((((h6-1)*N+p3-1)*N+p7-1)*N+p5))= - v2(((((h6-1)*N+p3-1)*N+p5-1)*N+p7))
v2e(((((h6-1)*N+p3-1)*N+p5-1)*N+p7))= + v2(((((h6-1)*N+p3-1)*N+p5-1)*N+p7))
END DO
END DO
END DO
END DO
DO p3=nocc+1,N
DO h6=1,nocc
DO h1=1,nocc
DO p5=nocc+1,N
DO p7=nocc+1,N
i1(((((h6-1)*N+p3-1)*N+h1-1)*N+p5))=i1(((((h6-1)*N+p3-1)*N+h1-1)*N+p5))+(-1.0d0)*t1(((p7-1)*N+h1))*v2e(((((h6-1)*N+p3-1)*N+p5-1)*N+&
&p7))
END DO
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
DO h10=1,nocc
DO p9=nocc+1,N
i1(((h10-1)*N+p9))=(1.0d0)*f1(((h10-1)*N+p9))
END DO
END DO
DO h10=1,nocc
DO h8=1,nocc
IF (h8>h10) CYCLE
DO p9=nocc+1,N
DO p7=nocc+1,N
IF (p7>p9) CYCLE
v2e(((((h10-1)*N+h8-1)*N+p9-1)*N+p7))= + v2(((((h8-1)*N+h10-1)*N+p7-1)*N+p9))
v2e(((((h10-1)*N+h8-1)*N+p7-1)*N+p9))= - v2(((((h8-1)*N+h10-1)*N+p7-1)*N+p9))
v2e(((((h8-1)*N+h10-1)*N+p9-1)*N+p7))= - v2(((((h8-1)*N+h10-1)*N+p7-1)*N+p9))
v2e(((((h8-1)*N+h10-1)*N+p7-1)*N+p9))= + v2(((((h8-1)*N+h10-1)*N+p7-1)*N+p9))
END DO
END DO
END DO
END DO
DO h10=1,nocc
DO p9=nocc+1,N
DO p7=nocc+1,N
DO h8=1,nocc
i1(((h10-1)*N+p9))=i1(((h10-1)*N+p9))+(1.0d0)*t1(((p7-1)*N+h8))*v2e(((((h8-1)*N+h10-1)*N+p7-1)*N+p9))
END DO
END DO
END DO
END DO
DO p3=nocc+1,N
DO p4=nocc+1,N
DO p9=nocc+1,N
IF (p3>p4) CYCLE
IF (p4>p9) CYCLE
DO h1=1,nocc
DO h2=1,nocc
DO h10=1,nocc
IF (h1>h2) CYCLE
IF (h2>h10) CYCLE
t3e(((((((p4-1)*N+p9-1)*N+p3-1)*N+h2-1)*N+h10-1)*N+h1))= + t3(((((((p3-1)*N+p4-1)*N+p9-1)*N+h1-1)*N+h2-1)*N+h10))
t3e(((((((p4-1)*N+p9-1)*N+p3-1)*N+h1-1)*N+h10-1)*N+h2))= - t3(((((((p3-1)*N+p4-1)*N+p9-1)*N+h1-1)*N+h2-1)*N+h10))
t3e(((((((p4-1)*N+p9-1)*N+p3-1)*N+h1-1)*N+h2-1)*N+h10))= + t3(((((((p3-1)*N+p4-1)*N+p9-1)*N+h1-1)*N+h2-1)*N+h10))
t3e(((((((p3-1)*N+p9-1)*N+p4-1)*N+h2-1)*N+h10-1)*N+h1))= - t3(((((((p3-1)*N+p4-1)*N+p9-1)*N+h1-1)*N+h2-1)*N+h10))
t3e(((((((p3-1)*N+p9-1)*N+p4-1)*N+h1-1)*N+h10-1)*N+h2))= + t3(((((((p3-1)*N+p4-1)*N+p9-1)*N+h1-1)*N+h2-1)*N+h10))
t3e(((((((p3-1)*N+p9-1)*N+p4-1)*N+h1-1)*N+h2-1)*N+h10))= - t3(((((((p3-1)*N+p4-1)*N+p9-1)*N+h1-1)*N+h2-1)*N+h10))
t3e(((((((p3-1)*N+p4-1)*N+p9-1)*N+h2-1)*N+h10-1)*N+h1))= + t3(((((((p3-1)*N+p4-1)*N+p9-1)*N+h1-1)*N+h2-1)*N+h10))
t3e(((((((p3-1)*N+p4-1)*N+p9-1)*N+h1-1)*N+h10-1)*N+h2))= - t3(((((((p3-1)*N+p4-1)*N+p9-1)*N+h1-1)*N+h2-1)*N+h10))
t3e(((((((p3-1)*N+p4-1)*N+p9-1)*N+h1-1)*N+h2-1)*N+h10))= + t3(((((((p3-1)*N+p4-1)*N+p9-1)*N+h1-1)*N+h2-1)*N+h10))
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
DO p9=nocc+1,N
DO h10=1,nocc
i0(((((p3-1)*N+p4-1)*N+h1-1)*N+h2))=i0(((((p3-1)*N+p4-1)*N+h1-1)*N+h2))+(1.0d0)*t3e(((((((p3-1)*N+p4-1)*N+p9-1)*N+h1-1)*N+h2-1)*N+h&
&10))*i1(((h10-1)*N+p9))
END DO
END DO
END DO
END DO
END DO
END DO
DO h6=1,nocc
DO h7=1,nocc
IF (h6>h7) CYCLE
DO h1=1,nocc
DO p5=nocc+1,N
v2e(((((h6-1)*N+h7-1)*N+h1-1)*N+p5))= + v2(((((h6-1)*N+h7-1)*N+h1-1)*N+p5))
END DO
END DO
END DO
END DO
DO h6=1,nocc
DO h7=1,nocc
IF (h6>h7) CYCLE
DO h1=1,nocc
DO p5=nocc+1,N
i1(((((h6-1)*N+h7-1)*N+h1-1)*N+p5))=(1.0d0)*v2e(((((h6-1)*N+h7-1)*N+h1-1)*N+p5))
END DO
END DO
END DO
END DO
DO h6=1,nocc
DO h7=1,nocc
IF (h6>h7) CYCLE
DO p5=nocc+1,N
DO p8=nocc+1,N
IF (p5>p8) CYCLE
v2e(((((h6-1)*N+h7-1)*N+p8-1)*N+p5))= - v2(((((h6-1)*N+h7-1)*N+p5-1)*N+p8))
v2e(((((h6-1)*N+h7-1)*N+p5-1)*N+p8))= + v2(((((h6-1)*N+h7-1)*N+p5-1)*N+p8))
END DO
END DO
END DO
END DO
DO h6=1,nocc
DO h7=1,nocc
IF (h6>h7) CYCLE
DO h1=1,nocc
DO p5=nocc+1,N
DO p8=nocc+1,N
i1(((((h6-1)*N+h7-1)*N+h1-1)*N+p5))=i1(((((h6-1)*N+h7-1)*N+h1-1)*N+p5))+(-1.0d0)*t1(((p8-1)*N+h1))*v2e(((((h6-1)*N+h7-1)*N+p5-1)*N+&
&p8))
END DO
END DO
END DO
END DO
END DO
DO p3=nocc+1,N
DO p4=nocc+1,N
DO p5=nocc+1,N
IF (p3>p4) CYCLE
IF (p4>p5) CYCLE
DO h1=1,nocc
DO h6=1,nocc
DO h7=1,nocc
IF (h1>h6) CYCLE
IF (h6>h7) CYCLE
t3e(((((((p4-1)*N+p5-1)*N+p3-1)*N+h6-1)*N+h1-1)*N+h7))= - t3(((((((p3-1)*N+p4-1)*N+p5-1)*N+h1-1)*N+h6-1)*N+h7))
t3e(((((((p4-1)*N+p5-1)*N+p3-1)*N+h7-1)*N+h1-1)*N+h6))= + t3(((((((p3-1)*N+p4-1)*N+p5-1)*N+h1-1)*N+h6-1)*N+h7))
t3e(((((((p4-1)*N+p5-1)*N+p3-1)*N+h1-1)*N+h6-1)*N+h7))= + t3(((((((p3-1)*N+p4-1)*N+p5-1)*N+h1-1)*N+h6-1)*N+h7))
t3e(((((((p3-1)*N+p5-1)*N+p4-1)*N+h6-1)*N+h1-1)*N+h7))= + t3(((((((p3-1)*N+p4-1)*N+p5-1)*N+h1-1)*N+h6-1)*N+h7))
t3e(((((((p3-1)*N+p5-1)*N+p4-1)*N+h7-1)*N+h1-1)*N+h6))= - t3(((((((p3-1)*N+p4-1)*N+p5-1)*N+h1-1)*N+h6-1)*N+h7))
t3e(((((((p3-1)*N+p5-1)*N+p4-1)*N+h1-1)*N+h6-1)*N+h7))= - t3(((((((p3-1)*N+p4-1)*N+p5-1)*N+h1-1)*N+h6-1)*N+h7))
t3e(((((((p3-1)*N+p4-1)*N+p5-1)*N+h6-1)*N+h1-1)*N+h7))= - t3(((((((p3-1)*N+p4-1)*N+p5-1)*N+h1-1)*N+h6-1)*N+h7))
t3e(((((((p3-1)*N+p4-1)*N+p5-1)*N+h7-1)*N+h1-1)*N+h6))= + t3(((((((p3-1)*N+p4-1)*N+p5-1)*N+h1-1)*N+h6-1)*N+h7))
t3e(((((((p3-1)*N+p4-1)*N+p5-1)*N+h1-1)*N+h6-1)*N+h7))= + t3(((((((p3-1)*N+p4-1)*N+p5-1)*N+h1-1)*N+h6-1)*N+h7))
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
DO h6=1,nocc
DO h7=1,nocc
IF (h6>h7) CYCLE
i0(((((p3-1)*N+p4-1)*N+h1-1)*N+h2))=i0(((((p3-1)*N+p4-1)*N+h1-1)*N+h2))+(-1.0d0)*t3e(((((((p3-1)*N+p4-1)*N+p5-1)*N+h1-1)*N+h6-1)*N+&
&h7))*i1(((((h6-1)*N+h7-1)*N+h2-1)*N+p5))+(1.0d0)*t3e(((((((p3-1)*N+p4-1)*N+p5-1)*N+h2-1)*N+h6-1)*N+h7))*i1(((((h6-1)*N+h7-1)*N+h1-&
&1)*N+p5))
END DO
END DO
END DO
END DO
END DO
END DO
END DO
DO p3=nocc+1,N
DO p5=nocc+1,N
DO p6=nocc+1,N
IF (p3>p5) CYCLE
IF (p5>p6) CYCLE
DO h1=1,nocc
DO h2=1,nocc
DO h7=1,nocc
IF (h1>h2) CYCLE
IF (h2>h7) CYCLE
t3e(((((((p5-1)*N+p3-1)*N+p6-1)*N+h2-1)*N+h7-1)*N+h1))= - t3(((((((p3-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h7))
t3e(((((((p5-1)*N+p3-1)*N+p6-1)*N+h1-1)*N+h7-1)*N+h2))= + t3(((((((p3-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h7))
t3e(((((((p5-1)*N+p3-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h7))= - t3(((((((p3-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h7))
t3e(((((((p6-1)*N+p3-1)*N+p5-1)*N+h2-1)*N+h7-1)*N+h1))= + t3(((((((p3-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h7))
t3e(((((((p6-1)*N+p3-1)*N+p5-1)*N+h1-1)*N+h7-1)*N+h2))= - t3(((((((p3-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h7))
t3e(((((((p6-1)*N+p3-1)*N+p5-1)*N+h1-1)*N+h2-1)*N+h7))= + t3(((((((p3-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h7))
t3e(((((((p3-1)*N+p5-1)*N+p6-1)*N+h2-1)*N+h7-1)*N+h1))= + t3(((((((p3-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h7))
t3e(((((((p3-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h7-1)*N+h2))= - t3(((((((p3-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h7))
t3e(((((((p3-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h7))= + t3(((((((p3-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h7))
END DO
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h7=1,nocc
DO p5=nocc+1,N
DO p6=nocc+1,N
IF (p5>p6) CYCLE
v2e(((((h7-1)*N+p4-1)*N+p5-1)*N+p6))= + v2(((((h7-1)*N+p4-1)*N+p5-1)*N+p6))
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
DO h7=1,nocc
i0(((((p3-1)*N+p4-1)*N+h1-1)*N+h2))=i0(((((p3-1)*N+p4-1)*N+h1-1)*N+h2))+(-1.0d0)*t3e(((((((p3-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+&
&h7))*v2e(((((h7-1)*N+p4-1)*N+p5-1)*N+p6))+(1.0d0)*t3e(((((((p4-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h7))*v2e(((((h7-1)*N+p3-1)*N+p&
&5-1)*N+p6))
END DO
END DO
END DO
END DO
END DO
END DO
END DO
DO h10=1,nocc
DO p9=nocc+1,N
i2(((h10-1)*N+p9))=(1.0d0)*f1(((h10-1)*N+p9))
END DO
END DO
DO h10=1,nocc
DO h8=1,nocc
IF (h8>h10) CYCLE
DO p9=nocc+1,N
DO p7=nocc+1,N
IF (p7>p9) CYCLE
v2e(((((h10-1)*N+h8-1)*N+p9-1)*N+p7))= + v2(((((h8-1)*N+h10-1)*N+p7-1)*N+p9))
v2e(((((h10-1)*N+h8-1)*N+p7-1)*N+p9))= - v2(((((h8-1)*N+h10-1)*N+p7-1)*N+p9))
v2e(((((h8-1)*N+h10-1)*N+p9-1)*N+p7))= - v2(((((h8-1)*N+h10-1)*N+p7-1)*N+p9))
v2e(((((h8-1)*N+h10-1)*N+p7-1)*N+p9))= + v2(((((h8-1)*N+h10-1)*N+p7-1)*N+p9))
END DO
END DO
END DO
END DO
DO h10=1,nocc
DO p9=nocc+1,N
DO p7=nocc+1,N
DO h8=1,nocc
i2(((h10-1)*N+p9))=i2(((h10-1)*N+p9))+(0.5d0)*t1(((p7-1)*N+h8))*v2e(((((h8-1)*N+h10-1)*N+p7-1)*N+p9))
END DO
END DO
END DO
END DO
i1(1)=0.0d0
DO p9=nocc+1,N
DO h10=1,nocc
i1(1)=i1(1)+(1.0d0)*t1(((p9-1)*N+h10))*i2(((h10-1)*N+p9))
END DO
END DO
DO p5=nocc+1,N
DO p6=nocc+1,N
IF (p5>p6) CYCLE
DO h7=1,nocc
DO h8=1,nocc
IF (h7>h8) CYCLE
i1(1)=i1(1)+(1.0d0)*t2(((((p5-1)*N+p6-1)*N+h7-1)*N+h8))*v2(((((h7-1)*N+h8-1)*N+p5-1)*N+p6))
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
i0(((((p3-1)*N+p4-1)*N+h1-1)*N+h2))=i0(((((p3-1)*N+p4-1)*N+h1-1)*N+h2))+(1.0d0)*t2(((((p3-1)*N+p4-1)*N+h1-1)*N+h2))*i1(1)
END DO
END DO
END DO
END DO
RETURN
END SUBROUTINE
