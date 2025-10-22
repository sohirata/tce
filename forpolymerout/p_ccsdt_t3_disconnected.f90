SUBROUTINE p_ccsdt_t3_disconnected(N,f1,f1e,i0,i0e,i1,i1e,i2,i2e,i3,i3e,nocc,t1,t1e,t2,t2e,t3,t3e,v2,v2e)
IMPLICIT NONE
INTEGER :: N
INTEGER :: nocc
INTEGER :: p4
INTEGER :: p5
INTEGER :: p6
INTEGER :: h1
INTEGER :: h2
INTEGER :: h3
INTEGER :: h11
INTEGER :: p12
INTEGER :: p7
INTEGER :: h12
INTEGER :: h8
INTEGER :: p8
INTEGER :: h9
INTEGER :: h10
INTEGER :: h7
INTEGER :: h13
INTEGER :: p11
INTEGER :: p9
INTEGER :: p10
REAL*8 :: i0(*)
REAL*8 :: i0e(*)
REAL*8 :: t1(*)
REAL*8 :: t1e(*)
REAL*8 :: i1(*)
REAL*8 :: i1e(*)
REAL*8 :: v2(*)
REAL*8 :: v2e(*)
REAL*8 :: i2(*)
REAL*8 :: i2e(*)
REAL*8 :: f1(*)
REAL*8 :: f1e(*)
REAL*8 :: i3(*)
REAL*8 :: i3e(*)
REAL*8 :: t2(*)
REAL*8 :: t2e(*)
REAL*8 :: t3(*)
REAL*8 :: t3e(*)
DO p4=nocc+1,N
DO p5=nocc+1,N
IF (p4>p5) CYCLE
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
i1(((((p4-1)*N+p5-1)*N+h1-1)*N+h2))=(1.0d0)*v2(((((p4-1)*N+p5-1)*N+h1-1)*N+h2))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h1=1,nocc
i2(((p4-1)*N+h1))=(1.0d0)*f1(((p4-1)*N+h1))
END DO
END DO
DO h11=1,nocc
DO h1=1,nocc
i3(((h11-1)*N+h1))=(1.0d0)*f1(((h11-1)*N+h1))
END DO
END DO
DO h11=1,nocc
DO h1=1,nocc
DO p7=nocc+1,N
i3(((h11-1)*N+h1))=i3(((h11-1)*N+h1))+(1.0d0)*t1(((p7-1)*N+h1))*f1(((h11-1)*N+p7))
END DO
END DO
END DO
DO h11=1,nocc
DO h9=1,nocc
IF (h9>h11) CYCLE
DO h1=1,nocc
DO p8=nocc+1,N
v2e(((((h11-1)*N+h9-1)*N+h1-1)*N+p8))= - v2(((((h9-1)*N+h11-1)*N+h1-1)*N+p8))
v2e(((((h9-1)*N+h11-1)*N+h1-1)*N+p8))= + v2(((((h9-1)*N+h11-1)*N+h1-1)*N+p8))
END DO
END DO
END DO
END DO
DO h11=1,nocc
DO h1=1,nocc
DO p8=nocc+1,N
DO h9=1,nocc
i3(((h11-1)*N+h1))=i3(((h11-1)*N+h1))+(-1.0d0)*t1(((p8-1)*N+h9))*v2e(((((h9-1)*N+h11-1)*N+h1-1)*N+p8))
END DO
END DO
END DO
END DO
DO p8=nocc+1,N
DO p9=nocc+1,N
IF (p8>p9) CYCLE
DO h1=1,nocc
DO h10=1,nocc
IF (h1>h10) CYCLE
t2e(((((p8-1)*N+p9-1)*N+h10-1)*N+h1))= - t2(((((p8-1)*N+p9-1)*N+h1-1)*N+h10))
t2e(((((p8-1)*N+p9-1)*N+h1-1)*N+h10))= + t2(((((p8-1)*N+p9-1)*N+h1-1)*N+h10))
END DO
END DO
END DO
END DO
DO h11=1,nocc
DO h10=1,nocc
IF (h10>h11) CYCLE
DO p8=nocc+1,N
DO p9=nocc+1,N
IF (p8>p9) CYCLE
v2e(((((h11-1)*N+h10-1)*N+p8-1)*N+p9))= - v2(((((h10-1)*N+h11-1)*N+p8-1)*N+p9))
v2e(((((h10-1)*N+h11-1)*N+p8-1)*N+p9))= + v2(((((h10-1)*N+h11-1)*N+p8-1)*N+p9))
END DO
END DO
END DO
END DO
DO h11=1,nocc
DO h1=1,nocc
DO p8=nocc+1,N
DO p9=nocc+1,N
IF (p8>p9) CYCLE
DO h10=1,nocc
i3(((h11-1)*N+h1))=i3(((h11-1)*N+h1))+(-1.0d0)*t2e(((((p8-1)*N+p9-1)*N+h1-1)*N+h10))*v2e(((((h10-1)*N+h11-1)*N+p8-1)*N+p9))
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h1=1,nocc
DO h11=1,nocc
i2(((p4-1)*N+h1))=i2(((p4-1)*N+h1))+(-1.0d0)*t1(((p4-1)*N+h11))*i3(((h11-1)*N+h1))
END DO
END DO
END DO
DO p4=nocc+1,N
DO p7=nocc+1,N
i3(((p4-1)*N+p7))=(1.0d0)*f1(((p4-1)*N+p7))
END DO
END DO
DO p4=nocc+1,N
DO h9=1,nocc
DO p7=nocc+1,N
DO p8=nocc+1,N
IF (p7>p8) CYCLE
v2e(((((h9-1)*N+p4-1)*N+p8-1)*N+p7))= - v2(((((h9-1)*N+p4-1)*N+p7-1)*N+p8))
v2e(((((h9-1)*N+p4-1)*N+p7-1)*N+p8))= + v2(((((h9-1)*N+p4-1)*N+p7-1)*N+p8))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p7=nocc+1,N
DO p8=nocc+1,N
DO h9=1,nocc
i3(((p4-1)*N+p7))=i3(((p4-1)*N+p7))+(-1.0d0)*t1(((p8-1)*N+h9))*v2e(((((h9-1)*N+p4-1)*N+p7-1)*N+p8))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p8=nocc+1,N
IF (p4>p8) CYCLE
DO h9=1,nocc
DO h10=1,nocc
IF (h9>h10) CYCLE
t2e(((((p8-1)*N+p4-1)*N+h9-1)*N+h10))= - t2(((((p4-1)*N+p8-1)*N+h9-1)*N+h10))
t2e(((((p4-1)*N+p8-1)*N+h9-1)*N+h10))= + t2(((((p4-1)*N+p8-1)*N+h9-1)*N+h10))
END DO
END DO
END DO
END DO
DO h9=1,nocc
DO h10=1,nocc
IF (h9>h10) CYCLE
DO p7=nocc+1,N
DO p8=nocc+1,N
IF (p7>p8) CYCLE
v2e(((((h9-1)*N+h10-1)*N+p8-1)*N+p7))= - v2(((((h9-1)*N+h10-1)*N+p7-1)*N+p8))
v2e(((((h9-1)*N+h10-1)*N+p7-1)*N+p8))= + v2(((((h9-1)*N+h10-1)*N+p7-1)*N+p8))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p7=nocc+1,N
DO p8=nocc+1,N
DO h9=1,nocc
DO h10=1,nocc
IF (h9>h10) CYCLE
i3(((p4-1)*N+p7))=i3(((p4-1)*N+p7))+(-1.0d0)*t2e(((((p4-1)*N+p8-1)*N+h9-1)*N+h10))*v2e(((((h9-1)*N+h10-1)*N+p7-1)*N+p8))
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h1=1,nocc
DO p7=nocc+1,N
i2(((p4-1)*N+h1))=i2(((p4-1)*N+h1))+(1.0d0)*t1(((p7-1)*N+h1))*i3(((p4-1)*N+p7))
END DO
END DO
END DO
DO p4=nocc+1,N
DO h8=1,nocc
DO h1=1,nocc
DO p7=nocc+1,N
v2e(((((h8-1)*N+p4-1)*N+h1-1)*N+p7))= + v2(((((h8-1)*N+p4-1)*N+h1-1)*N+p7))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h1=1,nocc
DO p7=nocc+1,N
DO h8=1,nocc
i2(((p4-1)*N+h1))=i2(((p4-1)*N+h1))+(-1.0d0)*t1(((p7-1)*N+h8))*v2e(((((h8-1)*N+p4-1)*N+h1-1)*N+p7))
END DO
END DO
END DO
END DO
DO h9=1,nocc
DO p11=nocc+1,N
i3(((h9-1)*N+p11))=(1.0d0)*f1(((h9-1)*N+p11))
END DO
END DO
DO h9=1,nocc
DO h8=1,nocc
IF (h8>h9) CYCLE
DO p11=nocc+1,N
DO p7=nocc+1,N
IF (p7>p11) CYCLE
v2e(((((h9-1)*N+h8-1)*N+p11-1)*N+p7))= + v2(((((h8-1)*N+h9-1)*N+p7-1)*N+p11))
v2e(((((h9-1)*N+h8-1)*N+p7-1)*N+p11))= - v2(((((h8-1)*N+h9-1)*N+p7-1)*N+p11))
v2e(((((h8-1)*N+h9-1)*N+p11-1)*N+p7))= - v2(((((h8-1)*N+h9-1)*N+p7-1)*N+p11))
v2e(((((h8-1)*N+h9-1)*N+p7-1)*N+p11))= + v2(((((h8-1)*N+h9-1)*N+p7-1)*N+p11))
END DO
END DO
END DO
END DO
DO h9=1,nocc
DO p11=nocc+1,N
DO p7=nocc+1,N
DO h8=1,nocc
i3(((h9-1)*N+p11))=i3(((h9-1)*N+p11))+(1.0d0)*t1(((p7-1)*N+h8))*v2e(((((h8-1)*N+h9-1)*N+p7-1)*N+p11))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p11=nocc+1,N
IF (p4>p11) CYCLE
DO h1=1,nocc
DO h9=1,nocc
IF (h1>h9) CYCLE
t2e(((((p11-1)*N+p4-1)*N+h9-1)*N+h1))= + t2(((((p4-1)*N+p11-1)*N+h1-1)*N+h9))
t2e(((((p11-1)*N+p4-1)*N+h1-1)*N+h9))= - t2(((((p4-1)*N+p11-1)*N+h1-1)*N+h9))
t2e(((((p4-1)*N+p11-1)*N+h9-1)*N+h1))= - t2(((((p4-1)*N+p11-1)*N+h1-1)*N+h9))
t2e(((((p4-1)*N+p11-1)*N+h1-1)*N+h9))= + t2(((((p4-1)*N+p11-1)*N+h1-1)*N+h9))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h1=1,nocc
DO p11=nocc+1,N
DO h9=1,nocc
i2(((p4-1)*N+h1))=i2(((p4-1)*N+h1))+(1.0d0)*t2e(((((p4-1)*N+p11-1)*N+h1-1)*N+h9))*i3(((h9-1)*N+p11))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p7=nocc+1,N
IF (p4>p7) CYCLE
DO h8=1,nocc
DO h9=1,nocc
IF (h8>h9) CYCLE
t2e(((((p7-1)*N+p4-1)*N+h8-1)*N+h9))= - t2(((((p4-1)*N+p7-1)*N+h8-1)*N+h9))
t2e(((((p4-1)*N+p7-1)*N+h8-1)*N+h9))= + t2(((((p4-1)*N+p7-1)*N+h8-1)*N+h9))
END DO
END DO
END DO
END DO
DO h8=1,nocc
DO h9=1,nocc
IF (h8>h9) CYCLE
DO h1=1,nocc
DO p7=nocc+1,N
v2e(((((h8-1)*N+h9-1)*N+h1-1)*N+p7))= + v2(((((h8-1)*N+h9-1)*N+h1-1)*N+p7))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h1=1,nocc
DO p7=nocc+1,N
DO h8=1,nocc
DO h9=1,nocc
IF (h8>h9) CYCLE
i2(((p4-1)*N+h1))=i2(((p4-1)*N+h1))+(-1.0d0)*t2e(((((p4-1)*N+p7-1)*N+h8-1)*N+h9))*v2e(((((h8-1)*N+h9-1)*N+h1-1)*N+p7))
END DO
END DO
END DO
END DO
END DO
DO p7=nocc+1,N
DO p8=nocc+1,N
IF (p7>p8) CYCLE
DO h1=1,nocc
DO h9=1,nocc
IF (h1>h9) CYCLE
t2e(((((p7-1)*N+p8-1)*N+h9-1)*N+h1))= - t2(((((p7-1)*N+p8-1)*N+h1-1)*N+h9))
t2e(((((p7-1)*N+p8-1)*N+h1-1)*N+h9))= + t2(((((p7-1)*N+p8-1)*N+h1-1)*N+h9))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h9=1,nocc
DO p7=nocc+1,N
DO p8=nocc+1,N
IF (p7>p8) CYCLE
v2e(((((h9-1)*N+p4-1)*N+p7-1)*N+p8))= + v2(((((h9-1)*N+p4-1)*N+p7-1)*N+p8))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h1=1,nocc
DO p7=nocc+1,N
DO p8=nocc+1,N
IF (p7>p8) CYCLE
DO h9=1,nocc
i2(((p4-1)*N+h1))=i2(((p4-1)*N+h1))+(-1.0d0)*t2e(((((p7-1)*N+p8-1)*N+h1-1)*N+h9))*v2e(((((h9-1)*N+p4-1)*N+p7-1)*N+p8))
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p7=nocc+1,N
DO p8=nocc+1,N
IF (p4>p7) CYCLE
IF (p7>p8) CYCLE
DO h1=1,nocc
DO h9=1,nocc
DO h10=1,nocc
IF (h1>h9) CYCLE
IF (h9>h10) CYCLE
t3e(((((((p7-1)*N+p4-1)*N+p8-1)*N+h9-1)*N+h1-1)*N+h10))= + t3(((((((p4-1)*N+p7-1)*N+p8-1)*N+h1-1)*N+h9-1)*N+h10))
t3e(((((((p7-1)*N+p4-1)*N+p8-1)*N+h10-1)*N+h1-1)*N+h9))= - t3(((((((p4-1)*N+p7-1)*N+p8-1)*N+h1-1)*N+h9-1)*N+h10))
t3e(((((((p7-1)*N+p4-1)*N+p8-1)*N+h1-1)*N+h9-1)*N+h10))= - t3(((((((p4-1)*N+p7-1)*N+p8-1)*N+h1-1)*N+h9-1)*N+h10))
t3e(((((((p8-1)*N+p4-1)*N+p7-1)*N+h9-1)*N+h1-1)*N+h10))= - t3(((((((p4-1)*N+p7-1)*N+p8-1)*N+h1-1)*N+h9-1)*N+h10))
t3e(((((((p8-1)*N+p4-1)*N+p7-1)*N+h10-1)*N+h1-1)*N+h9))= + t3(((((((p4-1)*N+p7-1)*N+p8-1)*N+h1-1)*N+h9-1)*N+h10))
t3e(((((((p8-1)*N+p4-1)*N+p7-1)*N+h1-1)*N+h9-1)*N+h10))= + t3(((((((p4-1)*N+p7-1)*N+p8-1)*N+h1-1)*N+h9-1)*N+h10))
t3e(((((((p4-1)*N+p7-1)*N+p8-1)*N+h9-1)*N+h1-1)*N+h10))= - t3(((((((p4-1)*N+p7-1)*N+p8-1)*N+h1-1)*N+h9-1)*N+h10))
t3e(((((((p4-1)*N+p7-1)*N+p8-1)*N+h10-1)*N+h1-1)*N+h9))= + t3(((((((p4-1)*N+p7-1)*N+p8-1)*N+h1-1)*N+h9-1)*N+h10))
t3e(((((((p4-1)*N+p7-1)*N+p8-1)*N+h1-1)*N+h9-1)*N+h10))= + t3(((((((p4-1)*N+p7-1)*N+p8-1)*N+h1-1)*N+h9-1)*N+h10))
END DO
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h1=1,nocc
DO p7=nocc+1,N
DO p8=nocc+1,N
IF (p7>p8) CYCLE
DO h9=1,nocc
DO h10=1,nocc
IF (h9>h10) CYCLE
i2(((p4-1)*N+h1))=i2(((p4-1)*N+h1))+(1.0d0)*t3e(((((((p4-1)*N+p7-1)*N+p8-1)*N+h1-1)*N+h9-1)*N+h10))*v2(((((h9-1)*N+h10-1)*N+p7-1)*N&
&+p8))
END DO
END DO
END DO
END DO
END DO
END DO
i3(1)=0.0d0
DO p7=nocc+1,N
DO h8=1,nocc
i3(1)=i3(1)+(1.0d0)*t1(((p7-1)*N+h8))*f1(((h8-1)*N+p7))
END DO
END DO
DO p7=nocc+1,N
DO p8=nocc+1,N
IF (p7>p8) CYCLE
DO h9=1,nocc
DO h10=1,nocc
IF (h9>h10) CYCLE
i3(1)=i3(1)+(1.0d0)*t2(((((p7-1)*N+p8-1)*N+h9-1)*N+h10))*v2(((((h9-1)*N+h10-1)*N+p7-1)*N+p8))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h1=1,nocc
i2(((p4-1)*N+h1))=i2(((p4-1)*N+h1))+(0.3333333333333333d0)*t1(((p4-1)*N+h1))*i3(1)
END DO
END DO
DO p4=nocc+1,N
DO p5=nocc+1,N
IF (p4>p5) CYCLE
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
i1(((((p4-1)*N+p5-1)*N+h1-1)*N+h2))=i1(((((p4-1)*N+p5-1)*N+h1-1)*N+h2))+(0.5d0)*t1(((p4-1)*N+h1))*i2(((p5-1)*N+h2))+(-0.5d0)*t1(((p&
&4-1)*N+h2))*i2(((p5-1)*N+h1))+(-0.5d0)*t1(((p5-1)*N+h1))*i2(((p4-1)*N+h2))+(0.5d0)*t1(((p5-1)*N+h2))*i2(((p4-1)*N+h1))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h7=1,nocc
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
v2e(((((h7-1)*N+p4-1)*N+h1-1)*N+h2))= + v2(((((h7-1)*N+p4-1)*N+h1-1)*N+h2))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h7=1,nocc
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
i2(((((h7-1)*N+p4-1)*N+h1-1)*N+h2))=(1.0d0)*v2e(((((h7-1)*N+p4-1)*N+h1-1)*N+h2))
END DO
END DO
END DO
END DO
DO h7=1,nocc
DO h8=1,nocc
IF (h7>h8) CYCLE
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
i3(((((h7-1)*N+h8-1)*N+h1-1)*N+h2))=(1.0d0)*v2(((((h7-1)*N+h8-1)*N+h1-1)*N+h2))
END DO
END DO
END DO
END DO
DO h7=1,nocc
DO h8=1,nocc
IF (h7>h8) CYCLE
DO h2=1,nocc
DO p9=nocc+1,N
v2e(((((h7-1)*N+h8-1)*N+h2-1)*N+p9))= + v2(((((h7-1)*N+h8-1)*N+h2-1)*N+p9))
END DO
END DO
END DO
END DO
DO h7=1,nocc
DO h8=1,nocc
IF (h7>h8) CYCLE
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
DO p9=nocc+1,N
i3(((((h7-1)*N+h8-1)*N+h1-1)*N+h2))=i3(((((h7-1)*N+h8-1)*N+h1-1)*N+h2))+(-1.0d0)*t1(((p9-1)*N+h1))*v2e(((((h7-1)*N+h8-1)*N+h2-1)*N+&
&p9))+(1.0d0)*t1(((p9-1)*N+h2))*v2e(((((h7-1)*N+h8-1)*N+h1-1)*N+p9))
END DO
END DO
END DO
END DO
END DO
DO h7=1,nocc
DO h8=1,nocc
IF (h7>h8) CYCLE
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
DO p9=nocc+1,N
DO p10=nocc+1,N
IF (p9>p10) CYCLE
i3(((((h7-1)*N+h8-1)*N+h1-1)*N+h2))=i3(((((h7-1)*N+h8-1)*N+h1-1)*N+h2))+(1.0d0)*t2(((((p9-1)*N+p10-1)*N+h1-1)*N+h2))*v2(((((h7-1)*N&
&+h8-1)*N+p9-1)*N+p10))
END DO
END DO
END DO
END DO
END DO
END DO
DO h7=1,nocc
DO h8=1,nocc
IF (h7>h8) CYCLE
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
i3e(((((h8-1)*N+h7-1)*N+h1-1)*N+h2))= - i3(((((h7-1)*N+h8-1)*N+h1-1)*N+h2))
i3e(((((h7-1)*N+h8-1)*N+h1-1)*N+h2))= + i3(((((h7-1)*N+h8-1)*N+h1-1)*N+h2))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h7=1,nocc
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
DO h8=1,nocc
i2(((((h7-1)*N+p4-1)*N+h1-1)*N+h2))=i2(((((h7-1)*N+p4-1)*N+h1-1)*N+h2))+(-0.5d0)*t1(((p4-1)*N+h8))*i3e(((((h7-1)*N+h8-1)*N+h1-1)*N+&
&h2))
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h7=1,nocc
DO h1=1,nocc
DO p8=nocc+1,N
v2e(((((h7-1)*N+p4-1)*N+h1-1)*N+p8))= + v2(((((h7-1)*N+p4-1)*N+h1-1)*N+p8))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h7=1,nocc
DO h1=1,nocc
DO p8=nocc+1,N
i3(((((h7-1)*N+p4-1)*N+h1-1)*N+p8))=(1.0d0)*v2e(((((h7-1)*N+p4-1)*N+h1-1)*N+p8))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h7=1,nocc
DO p8=nocc+1,N
DO p9=nocc+1,N
IF (p8>p9) CYCLE
v2e(((((h7-1)*N+p4-1)*N+p9-1)*N+p8))= - v2(((((h7-1)*N+p4-1)*N+p8-1)*N+p9))
v2e(((((h7-1)*N+p4-1)*N+p8-1)*N+p9))= + v2(((((h7-1)*N+p4-1)*N+p8-1)*N+p9))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h7=1,nocc
DO h1=1,nocc
DO p8=nocc+1,N
DO p9=nocc+1,N
i3(((((h7-1)*N+p4-1)*N+h1-1)*N+p8))=i3(((((h7-1)*N+p4-1)*N+h1-1)*N+p8))+(-0.5d0)*t1(((p9-1)*N+h1))*v2e(((((h7-1)*N+p4-1)*N+p8-1)*N+&
&p9))
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h7=1,nocc
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
DO p8=nocc+1,N
i2(((((h7-1)*N+p4-1)*N+h1-1)*N+h2))=i2(((((h7-1)*N+p4-1)*N+h1-1)*N+h2))+(-1.0d0)*t1(((p8-1)*N+h1))*i3(((((h7-1)*N+p4-1)*N+h2-1)*N+p&
&8))+(1.0d0)*t1(((p8-1)*N+h2))*i3(((((h7-1)*N+p4-1)*N+h1-1)*N+p8))
END DO
END DO
END DO
END DO
END DO
DO h7=1,nocc
DO h12=1,nocc
IF (h7>h12) CYCLE
DO h1=1,nocc
DO p11=nocc+1,N
v2e(((((h7-1)*N+h12-1)*N+h1-1)*N+p11))= + v2(((((h7-1)*N+h12-1)*N+h1-1)*N+p11))
END DO
END DO
END DO
END DO
DO h7=1,nocc
DO h12=1,nocc
IF (h7>h12) CYCLE
DO h1=1,nocc
DO p11=nocc+1,N
i3(((((h7-1)*N+h12-1)*N+h1-1)*N+p11))=(1.0d0)*v2e(((((h7-1)*N+h12-1)*N+h1-1)*N+p11))
END DO
END DO
END DO
END DO
DO h7=1,nocc
DO h12=1,nocc
IF (h7>h12) CYCLE
DO p11=nocc+1,N
DO p8=nocc+1,N
IF (p8>p11) CYCLE
v2e(((((h7-1)*N+h12-1)*N+p11-1)*N+p8))= - v2(((((h7-1)*N+h12-1)*N+p8-1)*N+p11))
v2e(((((h7-1)*N+h12-1)*N+p8-1)*N+p11))= + v2(((((h7-1)*N+h12-1)*N+p8-1)*N+p11))
END DO
END DO
END DO
END DO
DO h7=1,nocc
DO h12=1,nocc
IF (h7>h12) CYCLE
DO h1=1,nocc
DO p11=nocc+1,N
DO p8=nocc+1,N
i3(((((h7-1)*N+h12-1)*N+h1-1)*N+p11))=i3(((((h7-1)*N+h12-1)*N+h1-1)*N+p11))+(1.0d0)*t1(((p8-1)*N+h1))*v2e(((((h7-1)*N+h12-1)*N+p8-1&
&)*N+p11))
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p11=nocc+1,N
IF (p4>p11) CYCLE
DO h1=1,nocc
DO h12=1,nocc
IF (h1>h12) CYCLE
t2e(((((p11-1)*N+p4-1)*N+h12-1)*N+h1))= + t2(((((p4-1)*N+p11-1)*N+h1-1)*N+h12))
t2e(((((p11-1)*N+p4-1)*N+h1-1)*N+h12))= - t2(((((p4-1)*N+p11-1)*N+h1-1)*N+h12))
t2e(((((p4-1)*N+p11-1)*N+h12-1)*N+h1))= - t2(((((p4-1)*N+p11-1)*N+h1-1)*N+h12))
t2e(((((p4-1)*N+p11-1)*N+h1-1)*N+h12))= + t2(((((p4-1)*N+p11-1)*N+h1-1)*N+h12))
END DO
END DO
END DO
END DO
DO h7=1,nocc
DO h12=1,nocc
IF (h7>h12) CYCLE
DO h2=1,nocc
DO p11=nocc+1,N
i3e(((((h12-1)*N+h7-1)*N+h2-1)*N+p11))= - i3(((((h7-1)*N+h12-1)*N+h2-1)*N+p11))
i3e(((((h7-1)*N+h12-1)*N+h2-1)*N+p11))= + i3(((((h7-1)*N+h12-1)*N+h2-1)*N+p11))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h7=1,nocc
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
DO p11=nocc+1,N
DO h12=1,nocc
i2(((((h7-1)*N+p4-1)*N+h1-1)*N+h2))=i2(((((h7-1)*N+p4-1)*N+h1-1)*N+h2))+(-1.0d0)*t2e(((((p4-1)*N+p11-1)*N+h1-1)*N+h12))*i3e(((((h7-&
&1)*N+h12-1)*N+h2-1)*N+p11))+(1.0d0)*t2e(((((p4-1)*N+p11-1)*N+h2-1)*N+h12))*i3e(((((h7-1)*N+h12-1)*N+h1-1)*N+p11))
END DO
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h7=1,nocc
DO p8=nocc+1,N
DO p9=nocc+1,N
IF (p8>p9) CYCLE
v2e(((((h7-1)*N+p4-1)*N+p8-1)*N+p9))= + v2(((((h7-1)*N+p4-1)*N+p8-1)*N+p9))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h7=1,nocc
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
DO p8=nocc+1,N
DO p9=nocc+1,N
IF (p8>p9) CYCLE
i2(((((h7-1)*N+p4-1)*N+h1-1)*N+h2))=i2(((((h7-1)*N+p4-1)*N+h1-1)*N+h2))+(1.0d0)*t2(((((p8-1)*N+p9-1)*N+h1-1)*N+h2))*v2e(((((h7-1)*N&
&+p4-1)*N+p8-1)*N+p9))
END DO
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p8=nocc+1,N
DO p9=nocc+1,N
IF (p4>p8) CYCLE
IF (p8>p9) CYCLE
DO h1=1,nocc
DO h2=1,nocc
DO h10=1,nocc
IF (h1>h2) CYCLE
IF (h2>h10) CYCLE
t3e(((((((p8-1)*N+p4-1)*N+p9-1)*N+h2-1)*N+h10-1)*N+h1))= - t3(((((((p4-1)*N+p8-1)*N+p9-1)*N+h1-1)*N+h2-1)*N+h10))
t3e(((((((p8-1)*N+p4-1)*N+p9-1)*N+h1-1)*N+h10-1)*N+h2))= + t3(((((((p4-1)*N+p8-1)*N+p9-1)*N+h1-1)*N+h2-1)*N+h10))
t3e(((((((p8-1)*N+p4-1)*N+p9-1)*N+h1-1)*N+h2-1)*N+h10))= - t3(((((((p4-1)*N+p8-1)*N+p9-1)*N+h1-1)*N+h2-1)*N+h10))
t3e(((((((p9-1)*N+p4-1)*N+p8-1)*N+h2-1)*N+h10-1)*N+h1))= + t3(((((((p4-1)*N+p8-1)*N+p9-1)*N+h1-1)*N+h2-1)*N+h10))
t3e(((((((p9-1)*N+p4-1)*N+p8-1)*N+h1-1)*N+h10-1)*N+h2))= - t3(((((((p4-1)*N+p8-1)*N+p9-1)*N+h1-1)*N+h2-1)*N+h10))
t3e(((((((p9-1)*N+p4-1)*N+p8-1)*N+h1-1)*N+h2-1)*N+h10))= + t3(((((((p4-1)*N+p8-1)*N+p9-1)*N+h1-1)*N+h2-1)*N+h10))
t3e(((((((p4-1)*N+p8-1)*N+p9-1)*N+h2-1)*N+h10-1)*N+h1))= + t3(((((((p4-1)*N+p8-1)*N+p9-1)*N+h1-1)*N+h2-1)*N+h10))
t3e(((((((p4-1)*N+p8-1)*N+p9-1)*N+h1-1)*N+h10-1)*N+h2))= - t3(((((((p4-1)*N+p8-1)*N+p9-1)*N+h1-1)*N+h2-1)*N+h10))
t3e(((((((p4-1)*N+p8-1)*N+p9-1)*N+h1-1)*N+h2-1)*N+h10))= + t3(((((((p4-1)*N+p8-1)*N+p9-1)*N+h1-1)*N+h2-1)*N+h10))
END DO
END DO
END DO
END DO
END DO
END DO
DO h7=1,nocc
DO h10=1,nocc
IF (h7>h10) CYCLE
DO p8=nocc+1,N
DO p9=nocc+1,N
IF (p8>p9) CYCLE
v2e(((((h10-1)*N+h7-1)*N+p8-1)*N+p9))= - v2(((((h7-1)*N+h10-1)*N+p8-1)*N+p9))
v2e(((((h7-1)*N+h10-1)*N+p8-1)*N+p9))= + v2(((((h7-1)*N+h10-1)*N+p8-1)*N+p9))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h7=1,nocc
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
DO p8=nocc+1,N
DO p9=nocc+1,N
IF (p8>p9) CYCLE
DO h10=1,nocc
i2(((((h7-1)*N+p4-1)*N+h1-1)*N+h2))=i2(((((h7-1)*N+p4-1)*N+h1-1)*N+h2))+(-1.0d0)*t3e(((((((p4-1)*N+p8-1)*N+p9-1)*N+h1-1)*N+h2-1)*N+&
&h10))*v2e(((((h7-1)*N+h10-1)*N+p8-1)*N+p9))
END DO
END DO
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p5=nocc+1,N
IF (p4>p5) CYCLE
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
DO h7=1,nocc
i1(((((p4-1)*N+p5-1)*N+h1-1)*N+h2))=i1(((((p4-1)*N+p5-1)*N+h1-1)*N+h2))+(-1.0d0)*t1(((p4-1)*N+h7))*i2(((((h7-1)*N+p5-1)*N+h1-1)*N+h&
&2))+(1.0d0)*t1(((p5-1)*N+h7))*i2(((((h7-1)*N+p4-1)*N+h1-1)*N+h2))
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p5=nocc+1,N
IF (p4>p5) CYCLE
DO h1=1,nocc
DO p7=nocc+1,N
v2e(((((p4-1)*N+p5-1)*N+h1-1)*N+p7))= + v2(((((p4-1)*N+p5-1)*N+h1-1)*N+p7))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p5=nocc+1,N
IF (p4>p5) CYCLE
DO h1=1,nocc
DO p7=nocc+1,N
i2(((((p4-1)*N+p5-1)*N+h1-1)*N+p7))=(1.0d0)*v2e(((((p4-1)*N+p5-1)*N+h1-1)*N+p7))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p5=nocc+1,N
IF (p4>p5) CYCLE
DO p7=nocc+1,N
DO p8=nocc+1,N
IF (p7>p8) CYCLE
v2e(((((p4-1)*N+p5-1)*N+p8-1)*N+p7))= - v2(((((p4-1)*N+p5-1)*N+p7-1)*N+p8))
v2e(((((p4-1)*N+p5-1)*N+p7-1)*N+p8))= + v2(((((p4-1)*N+p5-1)*N+p7-1)*N+p8))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p5=nocc+1,N
IF (p4>p5) CYCLE
DO h1=1,nocc
DO p7=nocc+1,N
DO p8=nocc+1,N
i2(((((p4-1)*N+p5-1)*N+h1-1)*N+p7))=i2(((((p4-1)*N+p5-1)*N+h1-1)*N+p7))+(-0.5d0)*t1(((p8-1)*N+h1))*v2e(((((p4-1)*N+p5-1)*N+p7-1)*N+&
&p8))
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p5=nocc+1,N
IF (p4>p5) CYCLE
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
DO p7=nocc+1,N
i1(((((p4-1)*N+p5-1)*N+h1-1)*N+h2))=i1(((((p4-1)*N+p5-1)*N+h1-1)*N+h2))+(-1.0d0)*t1(((p7-1)*N+h1))*i2(((((p4-1)*N+p5-1)*N+h2-1)*N+p&
&7))+(1.0d0)*t1(((p7-1)*N+h2))*i2(((((p4-1)*N+p5-1)*N+h1-1)*N+p7))
END DO
END DO
END DO
END DO
END DO
DO h12=1,nocc
DO h1=1,nocc
i2(((h12-1)*N+h1))=(1.0d0)*f1(((h12-1)*N+h1))
END DO
END DO
DO h12=1,nocc
DO p7=nocc+1,N
i3(((h12-1)*N+p7))=(1.0d0)*f1(((h12-1)*N+p7))
END DO
END DO
DO h12=1,nocc
DO h9=1,nocc
IF (h9>h12) CYCLE
DO p7=nocc+1,N
DO p8=nocc+1,N
IF (p7>p8) CYCLE
v2e(((((h12-1)*N+h9-1)*N+p8-1)*N+p7))= + v2(((((h9-1)*N+h12-1)*N+p7-1)*N+p8))
v2e(((((h12-1)*N+h9-1)*N+p7-1)*N+p8))= - v2(((((h9-1)*N+h12-1)*N+p7-1)*N+p8))
v2e(((((h9-1)*N+h12-1)*N+p8-1)*N+p7))= - v2(((((h9-1)*N+h12-1)*N+p7-1)*N+p8))
v2e(((((h9-1)*N+h12-1)*N+p7-1)*N+p8))= + v2(((((h9-1)*N+h12-1)*N+p7-1)*N+p8))
END DO
END DO
END DO
END DO
DO h12=1,nocc
DO p7=nocc+1,N
DO p8=nocc+1,N
DO h9=1,nocc
i3(((h12-1)*N+p7))=i3(((h12-1)*N+p7))+(-1.0d0)*t1(((p8-1)*N+h9))*v2e(((((h9-1)*N+h12-1)*N+p7-1)*N+p8))
END DO
END DO
END DO
END DO
DO h12=1,nocc
DO h1=1,nocc
DO p7=nocc+1,N
i2(((h12-1)*N+h1))=i2(((h12-1)*N+h1))+(1.0d0)*t1(((p7-1)*N+h1))*i3(((h12-1)*N+p7))
END DO
END DO
END DO
DO h12=1,nocc
DO h8=1,nocc
IF (h8>h12) CYCLE
DO h1=1,nocc
DO p7=nocc+1,N
v2e(((((h12-1)*N+h8-1)*N+h1-1)*N+p7))= - v2(((((h8-1)*N+h12-1)*N+h1-1)*N+p7))
v2e(((((h8-1)*N+h12-1)*N+h1-1)*N+p7))= + v2(((((h8-1)*N+h12-1)*N+h1-1)*N+p7))
END DO
END DO
END DO
END DO
DO h12=1,nocc
DO h1=1,nocc
DO p7=nocc+1,N
DO h8=1,nocc
i2(((h12-1)*N+h1))=i2(((h12-1)*N+h1))+(-1.0d0)*t1(((p7-1)*N+h8))*v2e(((((h8-1)*N+h12-1)*N+h1-1)*N+p7))
END DO
END DO
END DO
END DO
DO p8=nocc+1,N
DO p9=nocc+1,N
IF (p8>p9) CYCLE
DO h1=1,nocc
DO h10=1,nocc
IF (h1>h10) CYCLE
t2e(((((p8-1)*N+p9-1)*N+h10-1)*N+h1))= - t2(((((p8-1)*N+p9-1)*N+h1-1)*N+h10))
t2e(((((p8-1)*N+p9-1)*N+h1-1)*N+h10))= + t2(((((p8-1)*N+p9-1)*N+h1-1)*N+h10))
END DO
END DO
END DO
END DO
DO h12=1,nocc
DO h10=1,nocc
IF (h10>h12) CYCLE
DO p8=nocc+1,N
DO p9=nocc+1,N
IF (p8>p9) CYCLE
v2e(((((h12-1)*N+h10-1)*N+p8-1)*N+p9))= - v2(((((h10-1)*N+h12-1)*N+p8-1)*N+p9))
v2e(((((h10-1)*N+h12-1)*N+p8-1)*N+p9))= + v2(((((h10-1)*N+h12-1)*N+p8-1)*N+p9))
END DO
END DO
END DO
END DO
DO h12=1,nocc
DO h1=1,nocc
DO p8=nocc+1,N
DO p9=nocc+1,N
IF (p8>p9) CYCLE
DO h10=1,nocc
i2(((h12-1)*N+h1))=i2(((h12-1)*N+h1))+(-1.0d0)*t2e(((((p8-1)*N+p9-1)*N+h1-1)*N+h10))*v2e(((((h10-1)*N+h12-1)*N+p8-1)*N+p9))
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p5=nocc+1,N
IF (p4>p5) CYCLE
DO h1=1,nocc
DO h12=1,nocc
IF (h1>h12) CYCLE
t2e(((((p4-1)*N+p5-1)*N+h12-1)*N+h1))= - t2(((((p4-1)*N+p5-1)*N+h1-1)*N+h12))
t2e(((((p4-1)*N+p5-1)*N+h1-1)*N+h12))= + t2(((((p4-1)*N+p5-1)*N+h1-1)*N+h12))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p5=nocc+1,N
IF (p4>p5) CYCLE
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
DO h12=1,nocc
i1(((((p4-1)*N+p5-1)*N+h1-1)*N+h2))=i1(((((p4-1)*N+p5-1)*N+h1-1)*N+h2))+(-1.0d0)*t2e(((((p4-1)*N+p5-1)*N+h1-1)*N+h12))*i2(((h12-1)*&
&N+h2))+(1.0d0)*t2e(((((p4-1)*N+p5-1)*N+h2-1)*N+h12))*i2(((h12-1)*N+h1))
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p12=nocc+1,N
i2(((p4-1)*N+p12))=(1.0d0)*f1(((p4-1)*N+p12))
END DO
END DO
DO h7=1,nocc
DO p12=nocc+1,N
i3(((h7-1)*N+p12))=(1.0d0)*f1(((h7-1)*N+p12))
END DO
END DO
DO h7=1,nocc
DO h9=1,nocc
IF (h7>h9) CYCLE
DO p12=nocc+1,N
DO p8=nocc+1,N
IF (p8>p12) CYCLE
v2e(((((h9-1)*N+h7-1)*N+p12-1)*N+p8))= + v2(((((h7-1)*N+h9-1)*N+p8-1)*N+p12))
v2e(((((h9-1)*N+h7-1)*N+p8-1)*N+p12))= - v2(((((h7-1)*N+h9-1)*N+p8-1)*N+p12))
v2e(((((h7-1)*N+h9-1)*N+p12-1)*N+p8))= - v2(((((h7-1)*N+h9-1)*N+p8-1)*N+p12))
v2e(((((h7-1)*N+h9-1)*N+p8-1)*N+p12))= + v2(((((h7-1)*N+h9-1)*N+p8-1)*N+p12))
END DO
END DO
END DO
END DO
DO h7=1,nocc
DO p12=nocc+1,N
DO p8=nocc+1,N
DO h9=1,nocc
i3(((h7-1)*N+p12))=i3(((h7-1)*N+p12))+(-1.0d0)*t1(((p8-1)*N+h9))*v2e(((((h7-1)*N+h9-1)*N+p8-1)*N+p12))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p12=nocc+1,N
DO h7=1,nocc
i2(((p4-1)*N+p12))=i2(((p4-1)*N+p12))+(-1.0d0)*t1(((p4-1)*N+h7))*i3(((h7-1)*N+p12))
END DO
END DO
END DO
DO p4=nocc+1,N
DO h8=1,nocc
DO p12=nocc+1,N
DO p7=nocc+1,N
IF (p7>p12) CYCLE
v2e(((((h8-1)*N+p4-1)*N+p12-1)*N+p7))= - v2(((((h8-1)*N+p4-1)*N+p7-1)*N+p12))
v2e(((((h8-1)*N+p4-1)*N+p7-1)*N+p12))= + v2(((((h8-1)*N+p4-1)*N+p7-1)*N+p12))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p12=nocc+1,N
DO p7=nocc+1,N
DO h8=1,nocc
i2(((p4-1)*N+p12))=i2(((p4-1)*N+p12))+(1.0d0)*t1(((p7-1)*N+h8))*v2e(((((h8-1)*N+p4-1)*N+p7-1)*N+p12))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p8=nocc+1,N
IF (p4>p8) CYCLE
DO h9=1,nocc
DO h10=1,nocc
IF (h9>h10) CYCLE
t2e(((((p8-1)*N+p4-1)*N+h9-1)*N+h10))= - t2(((((p4-1)*N+p8-1)*N+h9-1)*N+h10))
t2e(((((p4-1)*N+p8-1)*N+h9-1)*N+h10))= + t2(((((p4-1)*N+p8-1)*N+h9-1)*N+h10))
END DO
END DO
END DO
END DO
DO h9=1,nocc
DO h10=1,nocc
IF (h9>h10) CYCLE
DO p12=nocc+1,N
DO p8=nocc+1,N
IF (p8>p12) CYCLE
v2e(((((h9-1)*N+h10-1)*N+p12-1)*N+p8))= - v2(((((h9-1)*N+h10-1)*N+p8-1)*N+p12))
v2e(((((h9-1)*N+h10-1)*N+p8-1)*N+p12))= + v2(((((h9-1)*N+h10-1)*N+p8-1)*N+p12))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p12=nocc+1,N
DO p8=nocc+1,N
DO h9=1,nocc
DO h10=1,nocc
IF (h9>h10) CYCLE
i2(((p4-1)*N+p12))=i2(((p4-1)*N+p12))+(1.0d0)*t2e(((((p4-1)*N+p8-1)*N+h9-1)*N+h10))*v2e(((((h9-1)*N+h10-1)*N+p8-1)*N+p12))
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p12=nocc+1,N
IF (p4>p12) CYCLE
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
t2e(((((p12-1)*N+p4-1)*N+h1-1)*N+h2))= - t2(((((p4-1)*N+p12-1)*N+h1-1)*N+h2))
t2e(((((p4-1)*N+p12-1)*N+h1-1)*N+h2))= + t2(((((p4-1)*N+p12-1)*N+h1-1)*N+h2))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p5=nocc+1,N
IF (p4>p5) CYCLE
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
DO p12=nocc+1,N
i1(((((p4-1)*N+p5-1)*N+h1-1)*N+h2))=i1(((((p4-1)*N+p5-1)*N+h1-1)*N+h2))+(1.0d0)*t2e(((((p4-1)*N+p12-1)*N+h1-1)*N+h2))*i2(((p5-1)*N+&
&p12))+(-1.0d0)*t2e(((((p5-1)*N+p12-1)*N+h1-1)*N+h2))*i2(((p4-1)*N+p12))
END DO
END DO
END DO
END DO
END DO
DO h11=1,nocc
DO h12=1,nocc
IF (h11>h12) CYCLE
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
i2(((((h11-1)*N+h12-1)*N+h1-1)*N+h2))=(1.0d0)*v2(((((h11-1)*N+h12-1)*N+h1-1)*N+h2))
END DO
END DO
END DO
END DO
DO h11=1,nocc
DO h12=1,nocc
IF (h11>h12) CYCLE
DO h1=1,nocc
DO p7=nocc+1,N
v2e(((((h11-1)*N+h12-1)*N+h1-1)*N+p7))= + v2(((((h11-1)*N+h12-1)*N+h1-1)*N+p7))
END DO
END DO
END DO
END DO
DO h11=1,nocc
DO h12=1,nocc
IF (h11>h12) CYCLE
DO h1=1,nocc
DO p7=nocc+1,N
i3(((((h11-1)*N+h12-1)*N+h1-1)*N+p7))=(1.0d0)*v2e(((((h11-1)*N+h12-1)*N+h1-1)*N+p7))
END DO
END DO
END DO
END DO
DO h11=1,nocc
DO h12=1,nocc
IF (h11>h12) CYCLE
DO p7=nocc+1,N
DO p8=nocc+1,N
IF (p7>p8) CYCLE
v2e(((((h11-1)*N+h12-1)*N+p8-1)*N+p7))= - v2(((((h11-1)*N+h12-1)*N+p7-1)*N+p8))
v2e(((((h11-1)*N+h12-1)*N+p7-1)*N+p8))= + v2(((((h11-1)*N+h12-1)*N+p7-1)*N+p8))
END DO
END DO
END DO
END DO
DO h11=1,nocc
DO h12=1,nocc
IF (h11>h12) CYCLE
DO h1=1,nocc
DO p7=nocc+1,N
DO p8=nocc+1,N
i3(((((h11-1)*N+h12-1)*N+h1-1)*N+p7))=i3(((((h11-1)*N+h12-1)*N+h1-1)*N+p7))+(-0.5d0)*t1(((p8-1)*N+h1))*v2e(((((h11-1)*N+h12-1)*N+p7&
&-1)*N+p8))
END DO
END DO
END DO
END DO
END DO
DO h11=1,nocc
DO h12=1,nocc
IF (h11>h12) CYCLE
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
DO p7=nocc+1,N
i2(((((h11-1)*N+h12-1)*N+h1-1)*N+h2))=i2(((((h11-1)*N+h12-1)*N+h1-1)*N+h2))+(-1.0d0)*t1(((p7-1)*N+h1))*i3(((((h11-1)*N+h12-1)*N+h2-&
&1)*N+p7))+(1.0d0)*t1(((p7-1)*N+h2))*i3(((((h11-1)*N+h12-1)*N+h1-1)*N+p7))
END DO
END DO
END DO
END DO
END DO
DO h11=1,nocc
DO h12=1,nocc
IF (h11>h12) CYCLE
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
DO p9=nocc+1,N
DO p10=nocc+1,N
IF (p9>p10) CYCLE
i2(((((h11-1)*N+h12-1)*N+h1-1)*N+h2))=i2(((((h11-1)*N+h12-1)*N+h1-1)*N+h2))+(1.0d0)*t2(((((p9-1)*N+p10-1)*N+h1-1)*N+h2))*v2(((((h11&
&-1)*N+h12-1)*N+p9-1)*N+p10))
END DO
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p5=nocc+1,N
IF (p4>p5) CYCLE
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
DO h11=1,nocc
DO h12=1,nocc
IF (h11>h12) CYCLE
i1(((((p4-1)*N+p5-1)*N+h1-1)*N+h2))=i1(((((p4-1)*N+p5-1)*N+h1-1)*N+h2))+(1.0d0)*t2(((((p4-1)*N+p5-1)*N+h11-1)*N+h12))*i2(((((h11-1)&
&*N+h12-1)*N+h1-1)*N+h2))
END DO
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h13=1,nocc
DO h1=1,nocc
DO p12=nocc+1,N
v2e(((((h13-1)*N+p4-1)*N+h1-1)*N+p12))= + v2(((((h13-1)*N+p4-1)*N+h1-1)*N+p12))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h13=1,nocc
DO h1=1,nocc
DO p12=nocc+1,N
i2(((((h13-1)*N+p4-1)*N+h1-1)*N+p12))=(1.0d0)*v2e(((((h13-1)*N+p4-1)*N+h1-1)*N+p12))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h13=1,nocc
DO p12=nocc+1,N
DO p7=nocc+1,N
IF (p7>p12) CYCLE
v2e(((((h13-1)*N+p4-1)*N+p12-1)*N+p7))= - v2(((((h13-1)*N+p4-1)*N+p7-1)*N+p12))
v2e(((((h13-1)*N+p4-1)*N+p7-1)*N+p12))= + v2(((((h13-1)*N+p4-1)*N+p7-1)*N+p12))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h13=1,nocc
DO h1=1,nocc
DO p12=nocc+1,N
DO p7=nocc+1,N
i2(((((h13-1)*N+p4-1)*N+h1-1)*N+p12))=i2(((((h13-1)*N+p4-1)*N+h1-1)*N+p12))+(1.0d0)*t1(((p7-1)*N+h1))*v2e(((((h13-1)*N+p4-1)*N+p7-1&
&)*N+p12))
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p9=nocc+1,N
IF (p4>p9) CYCLE
DO h1=1,nocc
DO h10=1,nocc
IF (h1>h10) CYCLE
t2e(((((p9-1)*N+p4-1)*N+h10-1)*N+h1))= + t2(((((p4-1)*N+p9-1)*N+h1-1)*N+h10))
t2e(((((p9-1)*N+p4-1)*N+h1-1)*N+h10))= - t2(((((p4-1)*N+p9-1)*N+h1-1)*N+h10))
t2e(((((p4-1)*N+p9-1)*N+h10-1)*N+h1))= - t2(((((p4-1)*N+p9-1)*N+h1-1)*N+h10))
t2e(((((p4-1)*N+p9-1)*N+h1-1)*N+h10))= + t2(((((p4-1)*N+p9-1)*N+h1-1)*N+h10))
END DO
END DO
END DO
END DO
DO h13=1,nocc
DO h10=1,nocc
IF (h10>h13) CYCLE
DO p12=nocc+1,N
DO p9=nocc+1,N
IF (p9>p12) CYCLE
v2e(((((h13-1)*N+h10-1)*N+p12-1)*N+p9))= + v2(((((h10-1)*N+h13-1)*N+p9-1)*N+p12))
v2e(((((h13-1)*N+h10-1)*N+p9-1)*N+p12))= - v2(((((h10-1)*N+h13-1)*N+p9-1)*N+p12))
v2e(((((h10-1)*N+h13-1)*N+p12-1)*N+p9))= - v2(((((h10-1)*N+h13-1)*N+p9-1)*N+p12))
v2e(((((h10-1)*N+h13-1)*N+p9-1)*N+p12))= + v2(((((h10-1)*N+h13-1)*N+p9-1)*N+p12))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h13=1,nocc
DO h1=1,nocc
DO p12=nocc+1,N
DO p9=nocc+1,N
DO h10=1,nocc
i2(((((h13-1)*N+p4-1)*N+h1-1)*N+p12))=i2(((((h13-1)*N+p4-1)*N+h1-1)*N+p12))+(-0.5d0)*t2e(((((p4-1)*N+p9-1)*N+h1-1)*N+h10))*v2e(((((&
&h10-1)*N+h13-1)*N+p9-1)*N+p12))
END DO
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p12=nocc+1,N
IF (p4>p12) CYCLE
DO h1=1,nocc
DO h13=1,nocc
IF (h1>h13) CYCLE
t2e(((((p12-1)*N+p4-1)*N+h13-1)*N+h1))= + t2(((((p4-1)*N+p12-1)*N+h1-1)*N+h13))
t2e(((((p12-1)*N+p4-1)*N+h1-1)*N+h13))= - t2(((((p4-1)*N+p12-1)*N+h1-1)*N+h13))
t2e(((((p4-1)*N+p12-1)*N+h13-1)*N+h1))= - t2(((((p4-1)*N+p12-1)*N+h1-1)*N+h13))
t2e(((((p4-1)*N+p12-1)*N+h1-1)*N+h13))= + t2(((((p4-1)*N+p12-1)*N+h1-1)*N+h13))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p5=nocc+1,N
IF (p4>p5) CYCLE
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
DO p12=nocc+1,N
DO h13=1,nocc
i1(((((p4-1)*N+p5-1)*N+h1-1)*N+h2))=i1(((((p4-1)*N+p5-1)*N+h1-1)*N+h2))+(-1.0d0)*t2e(((((p4-1)*N+p12-1)*N+h1-1)*N+h13))*i2(((((h13-&
&1)*N+p5-1)*N+h2-1)*N+p12))+(1.0d0)*t2e(((((p4-1)*N+p12-1)*N+h2-1)*N+h13))*i2(((((h13-1)*N+p5-1)*N+h1-1)*N+p12))+(1.0d0)*t2e(((((p5&
&-1)*N+p12-1)*N+h1-1)*N+h13))*i2(((((h13-1)*N+p4-1)*N+h2-1)*N+p12))+(-1.0d0)*t2e(((((p5-1)*N+p12-1)*N+h2-1)*N+h13))*i2(((((h13-1)*N&
&+p4-1)*N+h1-1)*N+p12))
END DO
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p5=nocc+1,N
IF (p4>p5) CYCLE
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
DO p7=nocc+1,N
DO p8=nocc+1,N
IF (p7>p8) CYCLE
i1(((((p4-1)*N+p5-1)*N+h1-1)*N+h2))=i1(((((p4-1)*N+p5-1)*N+h1-1)*N+h2))+(1.0d0)*t2(((((p7-1)*N+p8-1)*N+h1-1)*N+h2))*v2(((((p4-1)*N+&
&p5-1)*N+p7-1)*N+p8))
END DO
END DO
END DO
END DO
END DO
END DO
DO h9=1,nocc
DO p11=nocc+1,N
i2(((h9-1)*N+p11))=(1.0d0)*f1(((h9-1)*N+p11))
END DO
END DO
DO h9=1,nocc
DO h8=1,nocc
IF (h8>h9) CYCLE
DO p11=nocc+1,N
DO p7=nocc+1,N
IF (p7>p11) CYCLE
v2e(((((h9-1)*N+h8-1)*N+p11-1)*N+p7))= + v2(((((h8-1)*N+h9-1)*N+p7-1)*N+p11))
v2e(((((h9-1)*N+h8-1)*N+p7-1)*N+p11))= - v2(((((h8-1)*N+h9-1)*N+p7-1)*N+p11))
v2e(((((h8-1)*N+h9-1)*N+p11-1)*N+p7))= - v2(((((h8-1)*N+h9-1)*N+p7-1)*N+p11))
v2e(((((h8-1)*N+h9-1)*N+p7-1)*N+p11))= + v2(((((h8-1)*N+h9-1)*N+p7-1)*N+p11))
END DO
END DO
END DO
END DO
DO h9=1,nocc
DO p11=nocc+1,N
DO p7=nocc+1,N
DO h8=1,nocc
i2(((h9-1)*N+p11))=i2(((h9-1)*N+p11))+(1.0d0)*t1(((p7-1)*N+h8))*v2e(((((h8-1)*N+h9-1)*N+p7-1)*N+p11))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p5=nocc+1,N
DO p11=nocc+1,N
IF (p4>p5) CYCLE
IF (p5>p11) CYCLE
DO h1=1,nocc
DO h2=1,nocc
DO h9=1,nocc
IF (h1>h2) CYCLE
IF (h2>h9) CYCLE
t3e(((((((p5-1)*N+p11-1)*N+p4-1)*N+h2-1)*N+h9-1)*N+h1))= + t3(((((((p4-1)*N+p5-1)*N+p11-1)*N+h1-1)*N+h2-1)*N+h9))
t3e(((((((p5-1)*N+p11-1)*N+p4-1)*N+h1-1)*N+h9-1)*N+h2))= - t3(((((((p4-1)*N+p5-1)*N+p11-1)*N+h1-1)*N+h2-1)*N+h9))
t3e(((((((p5-1)*N+p11-1)*N+p4-1)*N+h1-1)*N+h2-1)*N+h9))= + t3(((((((p4-1)*N+p5-1)*N+p11-1)*N+h1-1)*N+h2-1)*N+h9))
t3e(((((((p4-1)*N+p11-1)*N+p5-1)*N+h2-1)*N+h9-1)*N+h1))= - t3(((((((p4-1)*N+p5-1)*N+p11-1)*N+h1-1)*N+h2-1)*N+h9))
t3e(((((((p4-1)*N+p11-1)*N+p5-1)*N+h1-1)*N+h9-1)*N+h2))= + t3(((((((p4-1)*N+p5-1)*N+p11-1)*N+h1-1)*N+h2-1)*N+h9))
t3e(((((((p4-1)*N+p11-1)*N+p5-1)*N+h1-1)*N+h2-1)*N+h9))= - t3(((((((p4-1)*N+p5-1)*N+p11-1)*N+h1-1)*N+h2-1)*N+h9))
t3e(((((((p4-1)*N+p5-1)*N+p11-1)*N+h2-1)*N+h9-1)*N+h1))= + t3(((((((p4-1)*N+p5-1)*N+p11-1)*N+h1-1)*N+h2-1)*N+h9))
t3e(((((((p4-1)*N+p5-1)*N+p11-1)*N+h1-1)*N+h9-1)*N+h2))= - t3(((((((p4-1)*N+p5-1)*N+p11-1)*N+h1-1)*N+h2-1)*N+h9))
t3e(((((((p4-1)*N+p5-1)*N+p11-1)*N+h1-1)*N+h2-1)*N+h9))= + t3(((((((p4-1)*N+p5-1)*N+p11-1)*N+h1-1)*N+h2-1)*N+h9))
END DO
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p5=nocc+1,N
IF (p4>p5) CYCLE
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
DO p11=nocc+1,N
DO h9=1,nocc
i1(((((p4-1)*N+p5-1)*N+h1-1)*N+h2))=i1(((((p4-1)*N+p5-1)*N+h1-1)*N+h2))+(1.0d0)*t3e(((((((p4-1)*N+p5-1)*N+p11-1)*N+h1-1)*N+h2-1)*N+&
&h9))*i2(((h9-1)*N+p11))
END DO
END DO
END DO
END DO
END DO
END DO
DO h8=1,nocc
DO h12=1,nocc
IF (h8>h12) CYCLE
DO h1=1,nocc
DO p11=nocc+1,N
v2e(((((h8-1)*N+h12-1)*N+h1-1)*N+p11))= + v2(((((h8-1)*N+h12-1)*N+h1-1)*N+p11))
END DO
END DO
END DO
END DO
DO h8=1,nocc
DO h12=1,nocc
IF (h8>h12) CYCLE
DO h1=1,nocc
DO p11=nocc+1,N
i2(((((h8-1)*N+h12-1)*N+h1-1)*N+p11))=(-1.0d0)*v2e(((((h8-1)*N+h12-1)*N+h1-1)*N+p11))
END DO
END DO
END DO
END DO
DO h8=1,nocc
DO h12=1,nocc
IF (h8>h12) CYCLE
DO p11=nocc+1,N
DO p7=nocc+1,N
IF (p7>p11) CYCLE
v2e(((((h8-1)*N+h12-1)*N+p11-1)*N+p7))= - v2(((((h8-1)*N+h12-1)*N+p7-1)*N+p11))
v2e(((((h8-1)*N+h12-1)*N+p7-1)*N+p11))= + v2(((((h8-1)*N+h12-1)*N+p7-1)*N+p11))
END DO
END DO
END DO
END DO
DO h8=1,nocc
DO h12=1,nocc
IF (h8>h12) CYCLE
DO h1=1,nocc
DO p11=nocc+1,N
DO p7=nocc+1,N
i2(((((h8-1)*N+h12-1)*N+h1-1)*N+p11))=i2(((((h8-1)*N+h12-1)*N+h1-1)*N+p11))+(-1.0d0)*t1(((p7-1)*N+h1))*v2e(((((h8-1)*N+h12-1)*N+p7-&
&1)*N+p11))
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p5=nocc+1,N
DO p11=nocc+1,N
IF (p4>p5) CYCLE
IF (p5>p11) CYCLE
DO h1=1,nocc
DO h8=1,nocc
DO h12=1,nocc
IF (h1>h8) CYCLE
IF (h8>h12) CYCLE
t3e(((((((p5-1)*N+p11-1)*N+p4-1)*N+h8-1)*N+h1-1)*N+h12))= - t3(((((((p4-1)*N+p5-1)*N+p11-1)*N+h1-1)*N+h8-1)*N+h12))
t3e(((((((p5-1)*N+p11-1)*N+p4-1)*N+h12-1)*N+h1-1)*N+h8))= + t3(((((((p4-1)*N+p5-1)*N+p11-1)*N+h1-1)*N+h8-1)*N+h12))
t3e(((((((p5-1)*N+p11-1)*N+p4-1)*N+h1-1)*N+h8-1)*N+h12))= + t3(((((((p4-1)*N+p5-1)*N+p11-1)*N+h1-1)*N+h8-1)*N+h12))
t3e(((((((p4-1)*N+p11-1)*N+p5-1)*N+h8-1)*N+h1-1)*N+h12))= + t3(((((((p4-1)*N+p5-1)*N+p11-1)*N+h1-1)*N+h8-1)*N+h12))
t3e(((((((p4-1)*N+p11-1)*N+p5-1)*N+h12-1)*N+h1-1)*N+h8))= - t3(((((((p4-1)*N+p5-1)*N+p11-1)*N+h1-1)*N+h8-1)*N+h12))
t3e(((((((p4-1)*N+p11-1)*N+p5-1)*N+h1-1)*N+h8-1)*N+h12))= - t3(((((((p4-1)*N+p5-1)*N+p11-1)*N+h1-1)*N+h8-1)*N+h12))
t3e(((((((p4-1)*N+p5-1)*N+p11-1)*N+h8-1)*N+h1-1)*N+h12))= - t3(((((((p4-1)*N+p5-1)*N+p11-1)*N+h1-1)*N+h8-1)*N+h12))
t3e(((((((p4-1)*N+p5-1)*N+p11-1)*N+h12-1)*N+h1-1)*N+h8))= + t3(((((((p4-1)*N+p5-1)*N+p11-1)*N+h1-1)*N+h8-1)*N+h12))
t3e(((((((p4-1)*N+p5-1)*N+p11-1)*N+h1-1)*N+h8-1)*N+h12))= + t3(((((((p4-1)*N+p5-1)*N+p11-1)*N+h1-1)*N+h8-1)*N+h12))
END DO
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p5=nocc+1,N
IF (p4>p5) CYCLE
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
DO p11=nocc+1,N
DO h8=1,nocc
DO h12=1,nocc
IF (h8>h12) CYCLE
i1(((((p4-1)*N+p5-1)*N+h1-1)*N+h2))=i1(((((p4-1)*N+p5-1)*N+h1-1)*N+h2))+(1.0d0)*t3e(((((((p4-1)*N+p5-1)*N+p11-1)*N+h1-1)*N+h8-1)*N+&
&h12))*i2(((((h8-1)*N+h12-1)*N+h2-1)*N+p11))+(-1.0d0)*t3e(((((((p4-1)*N+p5-1)*N+p11-1)*N+h2-1)*N+h8-1)*N+h12))*i2(((((h8-1)*N+h12-1&
&)*N+h1-1)*N+p11))
END DO
END DO
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p7=nocc+1,N
DO p8=nocc+1,N
IF (p4>p7) CYCLE
IF (p7>p8) CYCLE
DO h1=1,nocc
DO h2=1,nocc
DO h9=1,nocc
IF (h1>h2) CYCLE
IF (h2>h9) CYCLE
t3e(((((((p7-1)*N+p4-1)*N+p8-1)*N+h2-1)*N+h9-1)*N+h1))= - t3(((((((p4-1)*N+p7-1)*N+p8-1)*N+h1-1)*N+h2-1)*N+h9))
t3e(((((((p7-1)*N+p4-1)*N+p8-1)*N+h1-1)*N+h9-1)*N+h2))= + t3(((((((p4-1)*N+p7-1)*N+p8-1)*N+h1-1)*N+h2-1)*N+h9))
t3e(((((((p7-1)*N+p4-1)*N+p8-1)*N+h1-1)*N+h2-1)*N+h9))= - t3(((((((p4-1)*N+p7-1)*N+p8-1)*N+h1-1)*N+h2-1)*N+h9))
t3e(((((((p8-1)*N+p4-1)*N+p7-1)*N+h2-1)*N+h9-1)*N+h1))= + t3(((((((p4-1)*N+p7-1)*N+p8-1)*N+h1-1)*N+h2-1)*N+h9))
t3e(((((((p8-1)*N+p4-1)*N+p7-1)*N+h1-1)*N+h9-1)*N+h2))= - t3(((((((p4-1)*N+p7-1)*N+p8-1)*N+h1-1)*N+h2-1)*N+h9))
t3e(((((((p8-1)*N+p4-1)*N+p7-1)*N+h1-1)*N+h2-1)*N+h9))= + t3(((((((p4-1)*N+p7-1)*N+p8-1)*N+h1-1)*N+h2-1)*N+h9))
t3e(((((((p4-1)*N+p7-1)*N+p8-1)*N+h2-1)*N+h9-1)*N+h1))= + t3(((((((p4-1)*N+p7-1)*N+p8-1)*N+h1-1)*N+h2-1)*N+h9))
t3e(((((((p4-1)*N+p7-1)*N+p8-1)*N+h1-1)*N+h9-1)*N+h2))= - t3(((((((p4-1)*N+p7-1)*N+p8-1)*N+h1-1)*N+h2-1)*N+h9))
t3e(((((((p4-1)*N+p7-1)*N+p8-1)*N+h1-1)*N+h2-1)*N+h9))= + t3(((((((p4-1)*N+p7-1)*N+p8-1)*N+h1-1)*N+h2-1)*N+h9))
END DO
END DO
END DO
END DO
END DO
END DO
DO p5=nocc+1,N
DO h9=1,nocc
DO p7=nocc+1,N
DO p8=nocc+1,N
IF (p7>p8) CYCLE
v2e(((((h9-1)*N+p5-1)*N+p7-1)*N+p8))= + v2(((((h9-1)*N+p5-1)*N+p7-1)*N+p8))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p5=nocc+1,N
IF (p4>p5) CYCLE
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
DO p7=nocc+1,N
DO p8=nocc+1,N
IF (p7>p8) CYCLE
DO h9=1,nocc
i1(((((p4-1)*N+p5-1)*N+h1-1)*N+h2))=i1(((((p4-1)*N+p5-1)*N+h1-1)*N+h2))+(-1.0d0)*t3e(((((((p4-1)*N+p7-1)*N+p8-1)*N+h1-1)*N+h2-1)*N+&
&h9))*v2e(((((h9-1)*N+p5-1)*N+p7-1)*N+p8))+(1.0d0)*t3e(((((((p5-1)*N+p7-1)*N+p8-1)*N+h1-1)*N+h2-1)*N+h9))*v2e(((((h9-1)*N+p4-1)*N+p&
&7-1)*N+p8))
END DO
END DO
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p5=nocc+1,N
DO p6=nocc+1,N
IF (p4>p5) CYCLE
IF (p5>p6) CYCLE
DO h1=1,nocc
DO h2=1,nocc
DO h3=1,nocc
IF (h1>h2) CYCLE
IF (h2>h3) CYCLE
i0(((((((p4-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h3))=0.0d0
i0(((((((p4-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h3))=i0(((((((p4-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h3))+(1.0d0)*t1(((p4-1)*N+h1&
&))*i1(((((p5-1)*N+p6-1)*N+h2-1)*N+h3))+(-1.0d0)*t1(((p4-1)*N+h2))*i1(((((p5-1)*N+p6-1)*N+h1-1)*N+h3))+(1.0d0)*t1(((p4-1)*N+h3))*i1&
&(((((p5-1)*N+p6-1)*N+h1-1)*N+h2))+(-1.0d0)*t1(((p5-1)*N+h1))*i1(((((p4-1)*N+p6-1)*N+h2-1)*N+h3))+(1.0d0)*t1(((p5-1)*N+h2))*i1(((((&
&p4-1)*N+p6-1)*N+h1-1)*N+h3))+(-1.0d0)*t1(((p5-1)*N+h3))*i1(((((p4-1)*N+p6-1)*N+h1-1)*N+h2))+(1.0d0)*t1(((p6-1)*N+h1))*i1(((((p4-1)&
&*N+p5-1)*N+h2-1)*N+h3))+(-1.0d0)*t1(((p6-1)*N+h2))*i1(((((p4-1)*N+p5-1)*N+h1-1)*N+h3))+(1.0d0)*t1(((p6-1)*N+h3))*i1(((((p4-1)*N+p5&
&-1)*N+h1-1)*N+h2))
END DO
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h1=1,nocc
i1(((p4-1)*N+h1))=(1.0d0)*f1(((p4-1)*N+h1))
END DO
END DO
DO h11=1,nocc
DO h1=1,nocc
i2(((h11-1)*N+h1))=(1.0d0)*f1(((h11-1)*N+h1))
END DO
END DO
DO h11=1,nocc
DO p12=nocc+1,N
i3(((h11-1)*N+p12))=(1.0d0)*f1(((h11-1)*N+p12))
END DO
END DO
DO h11=1,nocc
DO h10=1,nocc
IF (h10>h11) CYCLE
DO p12=nocc+1,N
DO p9=nocc+1,N
IF (p9>p12) CYCLE
v2e(((((h11-1)*N+h10-1)*N+p12-1)*N+p9))= + v2(((((h10-1)*N+h11-1)*N+p9-1)*N+p12))
v2e(((((h11-1)*N+h10-1)*N+p9-1)*N+p12))= - v2(((((h10-1)*N+h11-1)*N+p9-1)*N+p12))
v2e(((((h10-1)*N+h11-1)*N+p12-1)*N+p9))= - v2(((((h10-1)*N+h11-1)*N+p9-1)*N+p12))
v2e(((((h10-1)*N+h11-1)*N+p9-1)*N+p12))= + v2(((((h10-1)*N+h11-1)*N+p9-1)*N+p12))
END DO
END DO
END DO
END DO
DO h11=1,nocc
DO p12=nocc+1,N
DO p9=nocc+1,N
DO h10=1,nocc
i3(((h11-1)*N+p12))=i3(((h11-1)*N+p12))+(0.9999999999999998d0)*t1(((p9-1)*N+h10))*v2e(((((h10-1)*N+h11-1)*N+p9-1)*N+p12))
END DO
END DO
END DO
END DO
DO h11=1,nocc
DO h1=1,nocc
DO p12=nocc+1,N
i2(((h11-1)*N+h1))=i2(((h11-1)*N+h1))+(1.0d0)*t1(((p12-1)*N+h1))*i3(((h11-1)*N+p12))
END DO
END DO
END DO
DO h11=1,nocc
DO h9=1,nocc
IF (h9>h11) CYCLE
DO h1=1,nocc
DO p8=nocc+1,N
v2e(((((h11-1)*N+h9-1)*N+h1-1)*N+p8))= - v2(((((h9-1)*N+h11-1)*N+h1-1)*N+p8))
v2e(((((h9-1)*N+h11-1)*N+h1-1)*N+p8))= + v2(((((h9-1)*N+h11-1)*N+h1-1)*N+p8))
END DO
END DO
END DO
END DO
DO h11=1,nocc
DO h1=1,nocc
DO p8=nocc+1,N
DO h9=1,nocc
i2(((h11-1)*N+h1))=i2(((h11-1)*N+h1))+(-1.0d0)*t1(((p8-1)*N+h9))*v2e(((((h9-1)*N+h11-1)*N+h1-1)*N+p8))
END DO
END DO
END DO
END DO
DO p7=nocc+1,N
DO p8=nocc+1,N
IF (p7>p8) CYCLE
DO h1=1,nocc
DO h9=1,nocc
IF (h1>h9) CYCLE
t2e(((((p7-1)*N+p8-1)*N+h9-1)*N+h1))= - t2(((((p7-1)*N+p8-1)*N+h1-1)*N+h9))
t2e(((((p7-1)*N+p8-1)*N+h1-1)*N+h9))= + t2(((((p7-1)*N+p8-1)*N+h1-1)*N+h9))
END DO
END DO
END DO
END DO
DO h11=1,nocc
DO h9=1,nocc
IF (h9>h11) CYCLE
DO p7=nocc+1,N
DO p8=nocc+1,N
IF (p7>p8) CYCLE
v2e(((((h11-1)*N+h9-1)*N+p7-1)*N+p8))= - v2(((((h9-1)*N+h11-1)*N+p7-1)*N+p8))
v2e(((((h9-1)*N+h11-1)*N+p7-1)*N+p8))= + v2(((((h9-1)*N+h11-1)*N+p7-1)*N+p8))
END DO
END DO
END DO
END DO
DO h11=1,nocc
DO h1=1,nocc
DO p7=nocc+1,N
DO p8=nocc+1,N
IF (p7>p8) CYCLE
DO h9=1,nocc
i2(((h11-1)*N+h1))=i2(((h11-1)*N+h1))+(-1.0d0)*t2e(((((p7-1)*N+p8-1)*N+h1-1)*N+h9))*v2e(((((h9-1)*N+h11-1)*N+p7-1)*N+p8))
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h1=1,nocc
DO h11=1,nocc
i1(((p4-1)*N+h1))=i1(((p4-1)*N+h1))+(-1.0d0)*t1(((p4-1)*N+h11))*i2(((h11-1)*N+h1))
END DO
END DO
END DO
DO p4=nocc+1,N
DO p11=nocc+1,N
i2(((p4-1)*N+p11))=(1.0d0)*f1(((p4-1)*N+p11))
END DO
END DO
DO p4=nocc+1,N
DO h9=1,nocc
DO p11=nocc+1,N
DO p8=nocc+1,N
IF (p8>p11) CYCLE
v2e(((((h9-1)*N+p4-1)*N+p11-1)*N+p8))= - v2(((((h9-1)*N+p4-1)*N+p8-1)*N+p11))
v2e(((((h9-1)*N+p4-1)*N+p8-1)*N+p11))= + v2(((((h9-1)*N+p4-1)*N+p8-1)*N+p11))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p11=nocc+1,N
DO p8=nocc+1,N
DO h9=1,nocc
i2(((p4-1)*N+p11))=i2(((p4-1)*N+p11))+(1.0d0)*t1(((p8-1)*N+h9))*v2e(((((h9-1)*N+p4-1)*N+p8-1)*N+p11))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p7=nocc+1,N
IF (p4>p7) CYCLE
DO h8=1,nocc
DO h9=1,nocc
IF (h8>h9) CYCLE
t2e(((((p7-1)*N+p4-1)*N+h8-1)*N+h9))= - t2(((((p4-1)*N+p7-1)*N+h8-1)*N+h9))
t2e(((((p4-1)*N+p7-1)*N+h8-1)*N+h9))= + t2(((((p4-1)*N+p7-1)*N+h8-1)*N+h9))
END DO
END DO
END DO
END DO
DO h8=1,nocc
DO h9=1,nocc
IF (h8>h9) CYCLE
DO p11=nocc+1,N
DO p7=nocc+1,N
IF (p7>p11) CYCLE
v2e(((((h8-1)*N+h9-1)*N+p11-1)*N+p7))= - v2(((((h8-1)*N+h9-1)*N+p7-1)*N+p11))
v2e(((((h8-1)*N+h9-1)*N+p7-1)*N+p11))= + v2(((((h8-1)*N+h9-1)*N+p7-1)*N+p11))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p11=nocc+1,N
DO p7=nocc+1,N
DO h8=1,nocc
DO h9=1,nocc
IF (h8>h9) CYCLE
i2(((p4-1)*N+p11))=i2(((p4-1)*N+p11))+(1.0d0)*t2e(((((p4-1)*N+p7-1)*N+h8-1)*N+h9))*v2e(((((h8-1)*N+h9-1)*N+p7-1)*N+p11))
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h1=1,nocc
DO p11=nocc+1,N
i1(((p4-1)*N+h1))=i1(((p4-1)*N+h1))+(1.0d0)*t1(((p11-1)*N+h1))*i2(((p4-1)*N+p11))
END DO
END DO
END DO
DO p4=nocc+1,N
DO h8=1,nocc
DO h1=1,nocc
DO p7=nocc+1,N
v2e(((((h8-1)*N+p4-1)*N+h1-1)*N+p7))= + v2(((((h8-1)*N+p4-1)*N+h1-1)*N+p7))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h1=1,nocc
DO p7=nocc+1,N
DO h8=1,nocc
i1(((p4-1)*N+h1))=i1(((p4-1)*N+h1))+(-1.0d0)*t1(((p7-1)*N+h8))*v2e(((((h8-1)*N+p4-1)*N+h1-1)*N+p7))
END DO
END DO
END DO
END DO
DO h8=1,nocc
DO p7=nocc+1,N
i2(((h8-1)*N+p7))=(1.0d0)*f1(((h8-1)*N+p7))
END DO
END DO
DO h8=1,nocc
DO h10=1,nocc
IF (h8>h10) CYCLE
DO p7=nocc+1,N
DO p9=nocc+1,N
IF (p7>p9) CYCLE
v2e(((((h10-1)*N+h8-1)*N+p9-1)*N+p7))= + v2(((((h8-1)*N+h10-1)*N+p7-1)*N+p9))
v2e(((((h10-1)*N+h8-1)*N+p7-1)*N+p9))= - v2(((((h8-1)*N+h10-1)*N+p7-1)*N+p9))
v2e(((((h8-1)*N+h10-1)*N+p9-1)*N+p7))= - v2(((((h8-1)*N+h10-1)*N+p7-1)*N+p9))
v2e(((((h8-1)*N+h10-1)*N+p7-1)*N+p9))= + v2(((((h8-1)*N+h10-1)*N+p7-1)*N+p9))
END DO
END DO
END DO
END DO
DO h8=1,nocc
DO p7=nocc+1,N
DO p9=nocc+1,N
DO h10=1,nocc
i2(((h8-1)*N+p7))=i2(((h8-1)*N+p7))+(1.0d0)*t1(((p9-1)*N+h10))*v2e(((((h8-1)*N+h10-1)*N+p7-1)*N+p9))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p7=nocc+1,N
IF (p4>p7) CYCLE
DO h1=1,nocc
DO h8=1,nocc
IF (h1>h8) CYCLE
t2e(((((p7-1)*N+p4-1)*N+h8-1)*N+h1))= + t2(((((p4-1)*N+p7-1)*N+h1-1)*N+h8))
t2e(((((p7-1)*N+p4-1)*N+h1-1)*N+h8))= - t2(((((p4-1)*N+p7-1)*N+h1-1)*N+h8))
t2e(((((p4-1)*N+p7-1)*N+h8-1)*N+h1))= - t2(((((p4-1)*N+p7-1)*N+h1-1)*N+h8))
t2e(((((p4-1)*N+p7-1)*N+h1-1)*N+h8))= + t2(((((p4-1)*N+p7-1)*N+h1-1)*N+h8))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h1=1,nocc
DO p7=nocc+1,N
DO h8=1,nocc
i1(((p4-1)*N+h1))=i1(((p4-1)*N+h1))+(1.0d0)*t2e(((((p4-1)*N+p7-1)*N+h1-1)*N+h8))*i2(((h8-1)*N+p7))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p7=nocc+1,N
IF (p4>p7) CYCLE
DO h8=1,nocc
DO h9=1,nocc
IF (h8>h9) CYCLE
t2e(((((p7-1)*N+p4-1)*N+h8-1)*N+h9))= - t2(((((p4-1)*N+p7-1)*N+h8-1)*N+h9))
t2e(((((p4-1)*N+p7-1)*N+h8-1)*N+h9))= + t2(((((p4-1)*N+p7-1)*N+h8-1)*N+h9))
END DO
END DO
END DO
END DO
DO h8=1,nocc
DO h9=1,nocc
IF (h8>h9) CYCLE
DO h1=1,nocc
DO p7=nocc+1,N
v2e(((((h8-1)*N+h9-1)*N+h1-1)*N+p7))= + v2(((((h8-1)*N+h9-1)*N+h1-1)*N+p7))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h1=1,nocc
DO p7=nocc+1,N
DO h8=1,nocc
DO h9=1,nocc
IF (h8>h9) CYCLE
i1(((p4-1)*N+h1))=i1(((p4-1)*N+h1))+(-1.0d0)*t2e(((((p4-1)*N+p7-1)*N+h8-1)*N+h9))*v2e(((((h8-1)*N+h9-1)*N+h1-1)*N+p7))
END DO
END DO
END DO
END DO
END DO
DO p7=nocc+1,N
DO p8=nocc+1,N
IF (p7>p8) CYCLE
DO h1=1,nocc
DO h9=1,nocc
IF (h1>h9) CYCLE
t2e(((((p7-1)*N+p8-1)*N+h9-1)*N+h1))= - t2(((((p7-1)*N+p8-1)*N+h1-1)*N+h9))
t2e(((((p7-1)*N+p8-1)*N+h1-1)*N+h9))= + t2(((((p7-1)*N+p8-1)*N+h1-1)*N+h9))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h9=1,nocc
DO p7=nocc+1,N
DO p8=nocc+1,N
IF (p7>p8) CYCLE
v2e(((((h9-1)*N+p4-1)*N+p7-1)*N+p8))= + v2(((((h9-1)*N+p4-1)*N+p7-1)*N+p8))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h1=1,nocc
DO p7=nocc+1,N
DO p8=nocc+1,N
IF (p7>p8) CYCLE
DO h9=1,nocc
i1(((p4-1)*N+h1))=i1(((p4-1)*N+h1))+(-1.0d0)*t2e(((((p7-1)*N+p8-1)*N+h1-1)*N+h9))*v2e(((((h9-1)*N+p4-1)*N+p7-1)*N+p8))
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p7=nocc+1,N
DO p8=nocc+1,N
IF (p4>p7) CYCLE
IF (p7>p8) CYCLE
DO h1=1,nocc
DO h9=1,nocc
DO h10=1,nocc
IF (h1>h9) CYCLE
IF (h9>h10) CYCLE
t3e(((((((p7-1)*N+p4-1)*N+p8-1)*N+h9-1)*N+h1-1)*N+h10))= + t3(((((((p4-1)*N+p7-1)*N+p8-1)*N+h1-1)*N+h9-1)*N+h10))
t3e(((((((p7-1)*N+p4-1)*N+p8-1)*N+h10-1)*N+h1-1)*N+h9))= - t3(((((((p4-1)*N+p7-1)*N+p8-1)*N+h1-1)*N+h9-1)*N+h10))
t3e(((((((p7-1)*N+p4-1)*N+p8-1)*N+h1-1)*N+h9-1)*N+h10))= - t3(((((((p4-1)*N+p7-1)*N+p8-1)*N+h1-1)*N+h9-1)*N+h10))
t3e(((((((p8-1)*N+p4-1)*N+p7-1)*N+h9-1)*N+h1-1)*N+h10))= - t3(((((((p4-1)*N+p7-1)*N+p8-1)*N+h1-1)*N+h9-1)*N+h10))
t3e(((((((p8-1)*N+p4-1)*N+p7-1)*N+h10-1)*N+h1-1)*N+h9))= + t3(((((((p4-1)*N+p7-1)*N+p8-1)*N+h1-1)*N+h9-1)*N+h10))
t3e(((((((p8-1)*N+p4-1)*N+p7-1)*N+h1-1)*N+h9-1)*N+h10))= + t3(((((((p4-1)*N+p7-1)*N+p8-1)*N+h1-1)*N+h9-1)*N+h10))
t3e(((((((p4-1)*N+p7-1)*N+p8-1)*N+h9-1)*N+h1-1)*N+h10))= - t3(((((((p4-1)*N+p7-1)*N+p8-1)*N+h1-1)*N+h9-1)*N+h10))
t3e(((((((p4-1)*N+p7-1)*N+p8-1)*N+h10-1)*N+h1-1)*N+h9))= + t3(((((((p4-1)*N+p7-1)*N+p8-1)*N+h1-1)*N+h9-1)*N+h10))
t3e(((((((p4-1)*N+p7-1)*N+p8-1)*N+h1-1)*N+h9-1)*N+h10))= + t3(((((((p4-1)*N+p7-1)*N+p8-1)*N+h1-1)*N+h9-1)*N+h10))
END DO
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h1=1,nocc
DO p7=nocc+1,N
DO p8=nocc+1,N
IF (p7>p8) CYCLE
DO h9=1,nocc
DO h10=1,nocc
IF (h9>h10) CYCLE
i1(((p4-1)*N+h1))=i1(((p4-1)*N+h1))+(1.0d0)*t3e(((((((p4-1)*N+p7-1)*N+p8-1)*N+h1-1)*N+h9-1)*N+h10))*v2(((((h9-1)*N+h10-1)*N+p7-1)*N&
&+p8))
END DO
END DO
END DO
END DO
END DO
END DO
DO h8=1,nocc
DO p7=nocc+1,N
i3(((h8-1)*N+p7))=(1.0d0)*f1(((h8-1)*N+p7))
END DO
END DO
DO h8=1,nocc
DO h10=1,nocc
IF (h8>h10) CYCLE
DO p7=nocc+1,N
DO p9=nocc+1,N
IF (p7>p9) CYCLE
v2e(((((h10-1)*N+h8-1)*N+p9-1)*N+p7))= + v2(((((h8-1)*N+h10-1)*N+p7-1)*N+p9))
v2e(((((h10-1)*N+h8-1)*N+p7-1)*N+p9))= - v2(((((h8-1)*N+h10-1)*N+p7-1)*N+p9))
v2e(((((h8-1)*N+h10-1)*N+p9-1)*N+p7))= - v2(((((h8-1)*N+h10-1)*N+p7-1)*N+p9))
v2e(((((h8-1)*N+h10-1)*N+p7-1)*N+p9))= + v2(((((h8-1)*N+h10-1)*N+p7-1)*N+p9))
END DO
END DO
END DO
END DO
DO h8=1,nocc
DO p7=nocc+1,N
DO p9=nocc+1,N
DO h10=1,nocc
i3(((h8-1)*N+p7))=i3(((h8-1)*N+p7))+(0.5d0)*t1(((p9-1)*N+h10))*v2e(((((h8-1)*N+h10-1)*N+p7-1)*N+p9))
END DO
END DO
END DO
END DO
i2(1)=0.0d0
DO p7=nocc+1,N
DO h8=1,nocc
i2(1)=i2(1)+(1.0d0)*t1(((p7-1)*N+h8))*i3(((h8-1)*N+p7))
END DO
END DO
DO p7=nocc+1,N
DO p8=nocc+1,N
IF (p7>p8) CYCLE
DO h9=1,nocc
DO h10=1,nocc
IF (h9>h10) CYCLE
i2(1)=i2(1)+(1.0d0)*t2(((((p7-1)*N+p8-1)*N+h9-1)*N+h10))*v2(((((h9-1)*N+h10-1)*N+p7-1)*N+p8))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h1=1,nocc
i1(((p4-1)*N+h1))=i1(((p4-1)*N+h1))+(1.0d0)*t1(((p4-1)*N+h1))*i2(1)
END DO
END DO
DO p4=nocc+1,N
DO p5=nocc+1,N
DO p6=nocc+1,N
IF (p4>p5) CYCLE
IF (p5>p6) CYCLE
DO h1=1,nocc
DO h2=1,nocc
DO h3=1,nocc
IF (h1>h2) CYCLE
IF (h2>h3) CYCLE
i0(((((((p4-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h3))=i0(((((((p4-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h3))+(1.0d0)*t2(((((p4-1)*N+&
&p5-1)*N+h1-1)*N+h2))*i1(((p6-1)*N+h3))+(1.0d0)*t2(((((p4-1)*N+p5-1)*N+h2-1)*N+h3))*i1(((p6-1)*N+h1))+(-1.0d0)*t2(((((p4-1)*N+p5-1)&
&*N+h1-1)*N+h3))*i1(((p6-1)*N+h2))+(1.0d0)*t2(((((p5-1)*N+p6-1)*N+h1-1)*N+h2))*i1(((p4-1)*N+h3))+(1.0d0)*t2(((((p5-1)*N+p6-1)*N+h2-&
&1)*N+h3))*i1(((p4-1)*N+h1))+(-1.0d0)*t2(((((p5-1)*N+p6-1)*N+h1-1)*N+h3))*i1(((p4-1)*N+h2))+(-1.0d0)*t2(((((p4-1)*N+p6-1)*N+h1-1)*N&
&+h2))*i1(((p5-1)*N+h3))+(-1.0d0)*t2(((((p4-1)*N+p6-1)*N+h2-1)*N+h3))*i1(((p5-1)*N+h1))+(1.0d0)*t2(((((p4-1)*N+p6-1)*N+h1-1)*N+h3))&
&*i1(((p5-1)*N+h2))
END DO
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h11=1,nocc
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
v2e(((((h11-1)*N+p4-1)*N+h1-1)*N+h2))= + v2(((((h11-1)*N+p4-1)*N+h1-1)*N+h2))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h11=1,nocc
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
i1(((((h11-1)*N+p4-1)*N+h1-1)*N+h2))=(1.0d0)*v2e(((((h11-1)*N+p4-1)*N+h1-1)*N+h2))
END DO
END DO
END DO
END DO
DO h7=1,nocc
DO h11=1,nocc
IF (h7>h11) CYCLE
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
i2(((((h7-1)*N+h11-1)*N+h1-1)*N+h2))=(1.0d0)*v2(((((h7-1)*N+h11-1)*N+h1-1)*N+h2))
END DO
END DO
END DO
END DO
DO h7=1,nocc
DO h11=1,nocc
IF (h7>h11) CYCLE
DO h1=1,nocc
DO p8=nocc+1,N
v2e(((((h7-1)*N+h11-1)*N+h1-1)*N+p8))= + v2(((((h7-1)*N+h11-1)*N+h1-1)*N+p8))
END DO
END DO
END DO
END DO
DO h7=1,nocc
DO h11=1,nocc
IF (h7>h11) CYCLE
DO h1=1,nocc
DO p8=nocc+1,N
i3(((((h7-1)*N+h11-1)*N+h1-1)*N+p8))=(1.0d0)*v2e(((((h7-1)*N+h11-1)*N+h1-1)*N+p8))
END DO
END DO
END DO
END DO
DO h7=1,nocc
DO h11=1,nocc
IF (h7>h11) CYCLE
DO p8=nocc+1,N
DO p9=nocc+1,N
IF (p8>p9) CYCLE
v2e(((((h7-1)*N+h11-1)*N+p9-1)*N+p8))= - v2(((((h7-1)*N+h11-1)*N+p8-1)*N+p9))
v2e(((((h7-1)*N+h11-1)*N+p8-1)*N+p9))= + v2(((((h7-1)*N+h11-1)*N+p8-1)*N+p9))
END DO
END DO
END DO
END DO
DO h7=1,nocc
DO h11=1,nocc
IF (h7>h11) CYCLE
DO h1=1,nocc
DO p8=nocc+1,N
DO p9=nocc+1,N
i3(((((h7-1)*N+h11-1)*N+h1-1)*N+p8))=i3(((((h7-1)*N+h11-1)*N+h1-1)*N+p8))+(-0.4999999999999999d0)*t1(((p9-1)*N+h1))*v2e(((((h7-1)*N&
&+h11-1)*N+p8-1)*N+p9))
END DO
END DO
END DO
END DO
END DO
DO h7=1,nocc
DO h11=1,nocc
IF (h7>h11) CYCLE
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
DO p8=nocc+1,N
i2(((((h7-1)*N+h11-1)*N+h1-1)*N+h2))=i2(((((h7-1)*N+h11-1)*N+h1-1)*N+h2))+(-1.0d0)*t1(((p8-1)*N+h1))*i3(((((h7-1)*N+h11-1)*N+h2-1)*&
&N+p8))+(1.0d0)*t1(((p8-1)*N+h2))*i3(((((h7-1)*N+h11-1)*N+h1-1)*N+p8))
END DO
END DO
END DO
END DO
END DO
DO h7=1,nocc
DO h11=1,nocc
IF (h7>h11) CYCLE
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
DO p8=nocc+1,N
DO p9=nocc+1,N
IF (p8>p9) CYCLE
i2(((((h7-1)*N+h11-1)*N+h1-1)*N+h2))=i2(((((h7-1)*N+h11-1)*N+h1-1)*N+h2))+(1.0d0)*t2(((((p8-1)*N+p9-1)*N+h1-1)*N+h2))*v2(((((h7-1)*&
&N+h11-1)*N+p8-1)*N+p9))
END DO
END DO
END DO
END DO
END DO
END DO
DO h7=1,nocc
DO h11=1,nocc
IF (h7>h11) CYCLE
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
i2e(((((h11-1)*N+h7-1)*N+h1-1)*N+h2))= - i2(((((h7-1)*N+h11-1)*N+h1-1)*N+h2))
i2e(((((h7-1)*N+h11-1)*N+h1-1)*N+h2))= + i2(((((h7-1)*N+h11-1)*N+h1-1)*N+h2))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h11=1,nocc
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
DO h7=1,nocc
i1(((((h11-1)*N+p4-1)*N+h1-1)*N+h2))=i1(((((h11-1)*N+p4-1)*N+h1-1)*N+h2))+(1.0d0)*t1(((p4-1)*N+h7))*i2e(((((h7-1)*N+h11-1)*N+h1-1)*&
&N+h2))
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h11=1,nocc
DO h1=1,nocc
DO p9=nocc+1,N
v2e(((((h11-1)*N+p4-1)*N+h1-1)*N+p9))= + v2(((((h11-1)*N+p4-1)*N+h1-1)*N+p9))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h11=1,nocc
DO h1=1,nocc
DO p9=nocc+1,N
i2(((((h11-1)*N+p4-1)*N+h1-1)*N+p9))=(1.0d0)*v2e(((((h11-1)*N+p4-1)*N+h1-1)*N+p9))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h11=1,nocc
DO p9=nocc+1,N
DO p8=nocc+1,N
IF (p8>p9) CYCLE
v2e(((((h11-1)*N+p4-1)*N+p9-1)*N+p8))= - v2(((((h11-1)*N+p4-1)*N+p8-1)*N+p9))
v2e(((((h11-1)*N+p4-1)*N+p8-1)*N+p9))= + v2(((((h11-1)*N+p4-1)*N+p8-1)*N+p9))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h11=1,nocc
DO h1=1,nocc
DO p9=nocc+1,N
DO p8=nocc+1,N
i2(((((h11-1)*N+p4-1)*N+h1-1)*N+p9))=i2(((((h11-1)*N+p4-1)*N+h1-1)*N+p9))+(0.5d0)*t1(((p8-1)*N+h1))*v2e(((((h11-1)*N+p4-1)*N+p8-1)*&
&N+p9))
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h11=1,nocc
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
DO p9=nocc+1,N
i1(((((h11-1)*N+p4-1)*N+h1-1)*N+h2))=i1(((((h11-1)*N+p4-1)*N+h1-1)*N+h2))+(-1.0d0)*t1(((p9-1)*N+h1))*i2(((((h11-1)*N+p4-1)*N+h2-1)*&
&N+p9))+(1.0d0)*t1(((p9-1)*N+h2))*i2(((((h11-1)*N+p4-1)*N+h1-1)*N+p9))
END DO
END DO
END DO
END DO
END DO
DO h11=1,nocc
DO p12=nocc+1,N
i2(((h11-1)*N+p12))=(1.0d0)*f1(((h11-1)*N+p12))
END DO
END DO
DO h11=1,nocc
DO h10=1,nocc
IF (h10>h11) CYCLE
DO p12=nocc+1,N
DO p9=nocc+1,N
IF (p9>p12) CYCLE
v2e(((((h11-1)*N+h10-1)*N+p12-1)*N+p9))= + v2(((((h10-1)*N+h11-1)*N+p9-1)*N+p12))
v2e(((((h11-1)*N+h10-1)*N+p9-1)*N+p12))= - v2(((((h10-1)*N+h11-1)*N+p9-1)*N+p12))
v2e(((((h10-1)*N+h11-1)*N+p12-1)*N+p9))= - v2(((((h10-1)*N+h11-1)*N+p9-1)*N+p12))
v2e(((((h10-1)*N+h11-1)*N+p9-1)*N+p12))= + v2(((((h10-1)*N+h11-1)*N+p9-1)*N+p12))
END DO
END DO
END DO
END DO
DO h11=1,nocc
DO p12=nocc+1,N
DO p9=nocc+1,N
DO h10=1,nocc
i2(((h11-1)*N+p12))=i2(((h11-1)*N+p12))+(1.0d0)*t1(((p9-1)*N+h10))*v2e(((((h10-1)*N+h11-1)*N+p9-1)*N+p12))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p12=nocc+1,N
IF (p4>p12) CYCLE
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
t2e(((((p12-1)*N+p4-1)*N+h1-1)*N+h2))= - t2(((((p4-1)*N+p12-1)*N+h1-1)*N+h2))
t2e(((((p4-1)*N+p12-1)*N+h1-1)*N+h2))= + t2(((((p4-1)*N+p12-1)*N+h1-1)*N+h2))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h11=1,nocc
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
DO p12=nocc+1,N
i1(((((h11-1)*N+p4-1)*N+h1-1)*N+h2))=i1(((((h11-1)*N+p4-1)*N+h1-1)*N+h2))+(-1.0d0)*t2e(((((p4-1)*N+p12-1)*N+h1-1)*N+h2))*i2(((h11-1&
&)*N+p12))
END DO
END DO
END DO
END DO
END DO
DO h9=1,nocc
DO h11=1,nocc
IF (h9>h11) CYCLE
DO h1=1,nocc
DO p8=nocc+1,N
v2e(((((h9-1)*N+h11-1)*N+h1-1)*N+p8))= + v2(((((h9-1)*N+h11-1)*N+h1-1)*N+p8))
END DO
END DO
END DO
END DO
DO h9=1,nocc
DO h11=1,nocc
IF (h9>h11) CYCLE
DO h1=1,nocc
DO p8=nocc+1,N
i2(((((h9-1)*N+h11-1)*N+h1-1)*N+p8))=(1.0d0)*v2e(((((h9-1)*N+h11-1)*N+h1-1)*N+p8))
END DO
END DO
END DO
END DO
DO h9=1,nocc
DO h11=1,nocc
IF (h9>h11) CYCLE
DO p8=nocc+1,N
DO p10=nocc+1,N
IF (p8>p10) CYCLE
v2e(((((h9-1)*N+h11-1)*N+p10-1)*N+p8))= - v2(((((h9-1)*N+h11-1)*N+p8-1)*N+p10))
v2e(((((h9-1)*N+h11-1)*N+p8-1)*N+p10))= + v2(((((h9-1)*N+h11-1)*N+p8-1)*N+p10))
END DO
END DO
END DO
END DO
DO h9=1,nocc
DO h11=1,nocc
IF (h9>h11) CYCLE
DO h1=1,nocc
DO p8=nocc+1,N
DO p10=nocc+1,N
i2(((((h9-1)*N+h11-1)*N+h1-1)*N+p8))=i2(((((h9-1)*N+h11-1)*N+h1-1)*N+p8))+(-1.0d0)*t1(((p10-1)*N+h1))*v2e(((((h9-1)*N+h11-1)*N+p8-1&
&)*N+p10))
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p8=nocc+1,N
IF (p4>p8) CYCLE
DO h1=1,nocc
DO h9=1,nocc
IF (h1>h9) CYCLE
t2e(((((p8-1)*N+p4-1)*N+h9-1)*N+h1))= + t2(((((p4-1)*N+p8-1)*N+h1-1)*N+h9))
t2e(((((p8-1)*N+p4-1)*N+h1-1)*N+h9))= - t2(((((p4-1)*N+p8-1)*N+h1-1)*N+h9))
t2e(((((p4-1)*N+p8-1)*N+h9-1)*N+h1))= - t2(((((p4-1)*N+p8-1)*N+h1-1)*N+h9))
t2e(((((p4-1)*N+p8-1)*N+h1-1)*N+h9))= + t2(((((p4-1)*N+p8-1)*N+h1-1)*N+h9))
END DO
END DO
END DO
END DO
DO h9=1,nocc
DO h11=1,nocc
IF (h9>h11) CYCLE
DO h2=1,nocc
DO p8=nocc+1,N
i2e(((((h11-1)*N+h9-1)*N+h2-1)*N+p8))= - i2(((((h9-1)*N+h11-1)*N+h2-1)*N+p8))
i2e(((((h9-1)*N+h11-1)*N+h2-1)*N+p8))= + i2(((((h9-1)*N+h11-1)*N+h2-1)*N+p8))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h11=1,nocc
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
DO p8=nocc+1,N
DO h9=1,nocc
i1(((((h11-1)*N+p4-1)*N+h1-1)*N+h2))=i1(((((h11-1)*N+p4-1)*N+h1-1)*N+h2))+(1.0d0)*t2e(((((p4-1)*N+p8-1)*N+h1-1)*N+h9))*i2e(((((h9-1&
&)*N+h11-1)*N+h2-1)*N+p8))+(-1.0d0)*t2e(((((p4-1)*N+p8-1)*N+h2-1)*N+h9))*i2e(((((h9-1)*N+h11-1)*N+h1-1)*N+p8))
END DO
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h11=1,nocc
DO p8=nocc+1,N
DO p9=nocc+1,N
IF (p8>p9) CYCLE
v2e(((((h11-1)*N+p4-1)*N+p8-1)*N+p9))= + v2(((((h11-1)*N+p4-1)*N+p8-1)*N+p9))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h11=1,nocc
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
DO p8=nocc+1,N
DO p9=nocc+1,N
IF (p8>p9) CYCLE
i1(((((h11-1)*N+p4-1)*N+h1-1)*N+h2))=i1(((((h11-1)*N+p4-1)*N+h1-1)*N+h2))+(1.0d0)*t2(((((p8-1)*N+p9-1)*N+h1-1)*N+h2))*v2e(((((h11-1&
&)*N+p4-1)*N+p8-1)*N+p9))
END DO
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p7=nocc+1,N
DO p8=nocc+1,N
IF (p4>p7) CYCLE
IF (p7>p8) CYCLE
DO h1=1,nocc
DO h2=1,nocc
DO h9=1,nocc
IF (h1>h2) CYCLE
IF (h2>h9) CYCLE
t3e(((((((p7-1)*N+p4-1)*N+p8-1)*N+h2-1)*N+h9-1)*N+h1))= - t3(((((((p4-1)*N+p7-1)*N+p8-1)*N+h1-1)*N+h2-1)*N+h9))
t3e(((((((p7-1)*N+p4-1)*N+p8-1)*N+h1-1)*N+h9-1)*N+h2))= + t3(((((((p4-1)*N+p7-1)*N+p8-1)*N+h1-1)*N+h2-1)*N+h9))
t3e(((((((p7-1)*N+p4-1)*N+p8-1)*N+h1-1)*N+h2-1)*N+h9))= - t3(((((((p4-1)*N+p7-1)*N+p8-1)*N+h1-1)*N+h2-1)*N+h9))
t3e(((((((p8-1)*N+p4-1)*N+p7-1)*N+h2-1)*N+h9-1)*N+h1))= + t3(((((((p4-1)*N+p7-1)*N+p8-1)*N+h1-1)*N+h2-1)*N+h9))
t3e(((((((p8-1)*N+p4-1)*N+p7-1)*N+h1-1)*N+h9-1)*N+h2))= - t3(((((((p4-1)*N+p7-1)*N+p8-1)*N+h1-1)*N+h2-1)*N+h9))
t3e(((((((p8-1)*N+p4-1)*N+p7-1)*N+h1-1)*N+h2-1)*N+h9))= + t3(((((((p4-1)*N+p7-1)*N+p8-1)*N+h1-1)*N+h2-1)*N+h9))
t3e(((((((p4-1)*N+p7-1)*N+p8-1)*N+h2-1)*N+h9-1)*N+h1))= + t3(((((((p4-1)*N+p7-1)*N+p8-1)*N+h1-1)*N+h2-1)*N+h9))
t3e(((((((p4-1)*N+p7-1)*N+p8-1)*N+h1-1)*N+h9-1)*N+h2))= - t3(((((((p4-1)*N+p7-1)*N+p8-1)*N+h1-1)*N+h2-1)*N+h9))
t3e(((((((p4-1)*N+p7-1)*N+p8-1)*N+h1-1)*N+h2-1)*N+h9))= + t3(((((((p4-1)*N+p7-1)*N+p8-1)*N+h1-1)*N+h2-1)*N+h9))
END DO
END DO
END DO
END DO
END DO
END DO
DO h11=1,nocc
DO h9=1,nocc
IF (h9>h11) CYCLE
DO p7=nocc+1,N
DO p8=nocc+1,N
IF (p7>p8) CYCLE
v2e(((((h11-1)*N+h9-1)*N+p7-1)*N+p8))= - v2(((((h9-1)*N+h11-1)*N+p7-1)*N+p8))
v2e(((((h9-1)*N+h11-1)*N+p7-1)*N+p8))= + v2(((((h9-1)*N+h11-1)*N+p7-1)*N+p8))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h11=1,nocc
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
DO p7=nocc+1,N
DO p8=nocc+1,N
IF (p7>p8) CYCLE
DO h9=1,nocc
i1(((((h11-1)*N+p4-1)*N+h1-1)*N+h2))=i1(((((h11-1)*N+p4-1)*N+h1-1)*N+h2))+(1.0d0)*t3e(((((((p4-1)*N+p7-1)*N+p8-1)*N+h1-1)*N+h2-1)*N&
&+h9))*v2e(((((h9-1)*N+h11-1)*N+p7-1)*N+p8))
END DO
END DO
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p5=nocc+1,N
IF (p4>p5) CYCLE
DO h1=1,nocc
DO h11=1,nocc
IF (h1>h11) CYCLE
t2e(((((p4-1)*N+p5-1)*N+h11-1)*N+h1))= - t2(((((p4-1)*N+p5-1)*N+h1-1)*N+h11))
t2e(((((p4-1)*N+p5-1)*N+h1-1)*N+h11))= + t2(((((p4-1)*N+p5-1)*N+h1-1)*N+h11))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p5=nocc+1,N
DO p6=nocc+1,N
IF (p4>p5) CYCLE
IF (p5>p6) CYCLE
DO h1=1,nocc
DO h2=1,nocc
DO h3=1,nocc
IF (h1>h2) CYCLE
IF (h2>h3) CYCLE
DO h11=1,nocc
i0(((((((p4-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h3))=i0(((((((p4-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h3))+(-1.0d0)*t2e(((((p4-1)*&
&N+p5-1)*N+h1-1)*N+h11))*i1(((((h11-1)*N+p6-1)*N+h2-1)*N+h3))+(1.0d0)*t2e(((((p4-1)*N+p5-1)*N+h2-1)*N+h11))*i1(((((h11-1)*N+p6-1)*N&
&+h1-1)*N+h3))+(-1.0d0)*t2e(((((p4-1)*N+p5-1)*N+h3-1)*N+h11))*i1(((((h11-1)*N+p6-1)*N+h1-1)*N+h2))+(-1.0d0)*t2e(((((p5-1)*N+p6-1)*N&
&+h1-1)*N+h11))*i1(((((h11-1)*N+p4-1)*N+h2-1)*N+h3))+(1.0d0)*t2e(((((p5-1)*N+p6-1)*N+h2-1)*N+h11))*i1(((((h11-1)*N+p4-1)*N+h1-1)*N+&
&h3))+(-1.0d0)*t2e(((((p5-1)*N+p6-1)*N+h3-1)*N+h11))*i1(((((h11-1)*N+p4-1)*N+h1-1)*N+h2))+(1.0d0)*t2e(((((p4-1)*N+p6-1)*N+h1-1)*N+h&
&11))*i1(((((h11-1)*N+p5-1)*N+h2-1)*N+h3))+(-1.0d0)*t2e(((((p4-1)*N+p6-1)*N+h2-1)*N+h11))*i1(((((h11-1)*N+p5-1)*N+h1-1)*N+h3))+(1.0&
&d0)*t2e(((((p4-1)*N+p6-1)*N+h3-1)*N+h11))*i1(((((h11-1)*N+p5-1)*N+h1-1)*N+h2))
END DO
END DO
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p5=nocc+1,N
IF (p4>p5) CYCLE
DO h1=1,nocc
DO p12=nocc+1,N
v2e(((((p4-1)*N+p5-1)*N+h1-1)*N+p12))= + v2(((((p4-1)*N+p5-1)*N+h1-1)*N+p12))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p5=nocc+1,N
IF (p4>p5) CYCLE
DO h1=1,nocc
DO p12=nocc+1,N
i1(((((p4-1)*N+p5-1)*N+h1-1)*N+p12))=(1.0d0)*v2e(((((p4-1)*N+p5-1)*N+h1-1)*N+p12))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p5=nocc+1,N
IF (p4>p5) CYCLE
DO p12=nocc+1,N
DO p8=nocc+1,N
IF (p8>p12) CYCLE
v2e(((((p4-1)*N+p5-1)*N+p12-1)*N+p8))= - v2(((((p4-1)*N+p5-1)*N+p8-1)*N+p12))
v2e(((((p4-1)*N+p5-1)*N+p8-1)*N+p12))= + v2(((((p4-1)*N+p5-1)*N+p8-1)*N+p12))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p5=nocc+1,N
IF (p4>p5) CYCLE
DO h1=1,nocc
DO p12=nocc+1,N
DO p8=nocc+1,N
i1(((((p4-1)*N+p5-1)*N+h1-1)*N+p12))=i1(((((p4-1)*N+p5-1)*N+h1-1)*N+p12))+(1.0d0)*t1(((p8-1)*N+h1))*v2e(((((p4-1)*N+p5-1)*N+p8-1)*N&
&+p12))
END DO
END DO
END DO
END DO
END DO
DO h8=1,nocc
DO h9=1,nocc
IF (h8>h9) CYCLE
DO h1=1,nocc
DO p12=nocc+1,N
v2e(((((h8-1)*N+h9-1)*N+h1-1)*N+p12))= + v2(((((h8-1)*N+h9-1)*N+h1-1)*N+p12))
END DO
END DO
END DO
END DO
DO h8=1,nocc
DO h9=1,nocc
IF (h8>h9) CYCLE
DO h1=1,nocc
DO p12=nocc+1,N
i2(((((h8-1)*N+h9-1)*N+h1-1)*N+p12))=(1.0d0)*v2e(((((h8-1)*N+h9-1)*N+h1-1)*N+p12))
END DO
END DO
END DO
END DO
DO h8=1,nocc
DO h9=1,nocc
IF (h8>h9) CYCLE
DO p12=nocc+1,N
DO p10=nocc+1,N
IF (p10>p12) CYCLE
v2e(((((h8-1)*N+h9-1)*N+p12-1)*N+p10))= - v2(((((h8-1)*N+h9-1)*N+p10-1)*N+p12))
v2e(((((h8-1)*N+h9-1)*N+p10-1)*N+p12))= + v2(((((h8-1)*N+h9-1)*N+p10-1)*N+p12))
END DO
END DO
END DO
END DO
DO h8=1,nocc
DO h9=1,nocc
IF (h8>h9) CYCLE
DO h1=1,nocc
DO p12=nocc+1,N
DO p10=nocc+1,N
i2(((((h8-1)*N+h9-1)*N+h1-1)*N+p12))=i2(((((h8-1)*N+h9-1)*N+h1-1)*N+p12))+(1.0d0)*t1(((p10-1)*N+h1))*v2e(((((h8-1)*N+h9-1)*N+p10-1)&
&*N+p12))
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p5=nocc+1,N
IF (p4>p5) CYCLE
DO h1=1,nocc
DO p12=nocc+1,N
DO h8=1,nocc
DO h9=1,nocc
IF (h8>h9) CYCLE
i1(((((p4-1)*N+p5-1)*N+h1-1)*N+p12))=i1(((((p4-1)*N+p5-1)*N+h1-1)*N+p12))+(1.0d0)*t2(((((p4-1)*N+p5-1)*N+h8-1)*N+h9))*i2(((((h8-1)*&
&N+h9-1)*N+h1-1)*N+p12))
END DO
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p8=nocc+1,N
IF (p4>p8) CYCLE
DO h1=1,nocc
DO h9=1,nocc
IF (h1>h9) CYCLE
t2e(((((p8-1)*N+p4-1)*N+h9-1)*N+h1))= + t2(((((p4-1)*N+p8-1)*N+h1-1)*N+h9))
t2e(((((p8-1)*N+p4-1)*N+h1-1)*N+h9))= - t2(((((p4-1)*N+p8-1)*N+h1-1)*N+h9))
t2e(((((p4-1)*N+p8-1)*N+h9-1)*N+h1))= - t2(((((p4-1)*N+p8-1)*N+h1-1)*N+h9))
t2e(((((p4-1)*N+p8-1)*N+h1-1)*N+h9))= + t2(((((p4-1)*N+p8-1)*N+h1-1)*N+h9))
END DO
END DO
END DO
END DO
DO p5=nocc+1,N
DO h9=1,nocc
DO p12=nocc+1,N
DO p8=nocc+1,N
IF (p8>p12) CYCLE
v2e(((((h9-1)*N+p5-1)*N+p12-1)*N+p8))= - v2(((((h9-1)*N+p5-1)*N+p8-1)*N+p12))
v2e(((((h9-1)*N+p5-1)*N+p8-1)*N+p12))= + v2(((((h9-1)*N+p5-1)*N+p8-1)*N+p12))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p5=nocc+1,N
IF (p4>p5) CYCLE
DO h1=1,nocc
DO p12=nocc+1,N
DO p8=nocc+1,N
DO h9=1,nocc
i1(((((p4-1)*N+p5-1)*N+h1-1)*N+p12))=i1(((((p4-1)*N+p5-1)*N+h1-1)*N+p12))+(1.0d0)*t2e(((((p4-1)*N+p8-1)*N+h1-1)*N+h9))*v2e(((((h9-1&
&)*N+p5-1)*N+p8-1)*N+p12))+(-1.0d0)*t2e(((((p5-1)*N+p8-1)*N+h1-1)*N+h9))*v2e(((((h9-1)*N+p4-1)*N+p8-1)*N+p12))
END DO
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p5=nocc+1,N
DO p7=nocc+1,N
IF (p4>p5) CYCLE
IF (p5>p7) CYCLE
DO h1=1,nocc
DO h8=1,nocc
DO h9=1,nocc
IF (h1>h8) CYCLE
IF (h8>h9) CYCLE
t3e(((((((p5-1)*N+p7-1)*N+p4-1)*N+h8-1)*N+h1-1)*N+h9))= - t3(((((((p4-1)*N+p5-1)*N+p7-1)*N+h1-1)*N+h8-1)*N+h9))
t3e(((((((p5-1)*N+p7-1)*N+p4-1)*N+h9-1)*N+h1-1)*N+h8))= + t3(((((((p4-1)*N+p5-1)*N+p7-1)*N+h1-1)*N+h8-1)*N+h9))
t3e(((((((p5-1)*N+p7-1)*N+p4-1)*N+h1-1)*N+h8-1)*N+h9))= + t3(((((((p4-1)*N+p5-1)*N+p7-1)*N+h1-1)*N+h8-1)*N+h9))
t3e(((((((p4-1)*N+p7-1)*N+p5-1)*N+h8-1)*N+h1-1)*N+h9))= + t3(((((((p4-1)*N+p5-1)*N+p7-1)*N+h1-1)*N+h8-1)*N+h9))
t3e(((((((p4-1)*N+p7-1)*N+p5-1)*N+h9-1)*N+h1-1)*N+h8))= - t3(((((((p4-1)*N+p5-1)*N+p7-1)*N+h1-1)*N+h8-1)*N+h9))
t3e(((((((p4-1)*N+p7-1)*N+p5-1)*N+h1-1)*N+h8-1)*N+h9))= - t3(((((((p4-1)*N+p5-1)*N+p7-1)*N+h1-1)*N+h8-1)*N+h9))
t3e(((((((p4-1)*N+p5-1)*N+p7-1)*N+h8-1)*N+h1-1)*N+h9))= - t3(((((((p4-1)*N+p5-1)*N+p7-1)*N+h1-1)*N+h8-1)*N+h9))
t3e(((((((p4-1)*N+p5-1)*N+p7-1)*N+h9-1)*N+h1-1)*N+h8))= + t3(((((((p4-1)*N+p5-1)*N+p7-1)*N+h1-1)*N+h8-1)*N+h9))
t3e(((((((p4-1)*N+p5-1)*N+p7-1)*N+h1-1)*N+h8-1)*N+h9))= + t3(((((((p4-1)*N+p5-1)*N+p7-1)*N+h1-1)*N+h8-1)*N+h9))
END DO
END DO
END DO
END DO
END DO
END DO
DO h8=1,nocc
DO h9=1,nocc
IF (h8>h9) CYCLE
DO p12=nocc+1,N
DO p7=nocc+1,N
IF (p7>p12) CYCLE
v2e(((((h8-1)*N+h9-1)*N+p12-1)*N+p7))= - v2(((((h8-1)*N+h9-1)*N+p7-1)*N+p12))
v2e(((((h8-1)*N+h9-1)*N+p7-1)*N+p12))= + v2(((((h8-1)*N+h9-1)*N+p7-1)*N+p12))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p5=nocc+1,N
IF (p4>p5) CYCLE
DO h1=1,nocc
DO p12=nocc+1,N
DO p7=nocc+1,N
DO h8=1,nocc
DO h9=1,nocc
IF (h8>h9) CYCLE
i1(((((p4-1)*N+p5-1)*N+h1-1)*N+p12))=i1(((((p4-1)*N+p5-1)*N+h1-1)*N+p12))+(1.0d0)*t3e(((((((p4-1)*N+p5-1)*N+p7-1)*N+h1-1)*N+h8-1)*N&
&+h9))*v2e(((((h8-1)*N+h9-1)*N+p7-1)*N+p12))
END DO
END DO
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p12=nocc+1,N
IF (p4>p12) CYCLE
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
t2e(((((p12-1)*N+p4-1)*N+h1-1)*N+h2))= - t2(((((p4-1)*N+p12-1)*N+h1-1)*N+h2))
t2e(((((p4-1)*N+p12-1)*N+h1-1)*N+h2))= + t2(((((p4-1)*N+p12-1)*N+h1-1)*N+h2))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p5=nocc+1,N
DO p6=nocc+1,N
IF (p4>p5) CYCLE
IF (p5>p6) CYCLE
DO h1=1,nocc
DO h2=1,nocc
DO h3=1,nocc
IF (h1>h2) CYCLE
IF (h2>h3) CYCLE
DO p12=nocc+1,N
i0(((((((p4-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h3))=i0(((((((p4-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h3))+(-1.0d0)*t2e(((((p4-1)*&
&N+p12-1)*N+h1-1)*N+h2))*i1(((((p5-1)*N+p6-1)*N+h3-1)*N+p12))+(-1.0d0)*t2e(((((p4-1)*N+p12-1)*N+h2-1)*N+h3))*i1(((((p5-1)*N+p6-1)*N&
&+h1-1)*N+p12))+(1.0d0)*t2e(((((p4-1)*N+p12-1)*N+h1-1)*N+h3))*i1(((((p5-1)*N+p6-1)*N+h2-1)*N+p12))+(1.0d0)*t2e(((((p5-1)*N+p12-1)*N&
&+h1-1)*N+h2))*i1(((((p4-1)*N+p6-1)*N+h3-1)*N+p12))+(1.0d0)*t2e(((((p5-1)*N+p12-1)*N+h2-1)*N+h3))*i1(((((p4-1)*N+p6-1)*N+h1-1)*N+p1&
&2))+(-1.0d0)*t2e(((((p5-1)*N+p12-1)*N+h1-1)*N+h3))*i1(((((p4-1)*N+p6-1)*N+h2-1)*N+p12))+(-1.0d0)*t2e(((((p6-1)*N+p12-1)*N+h1-1)*N+&
&h2))*i1(((((p4-1)*N+p5-1)*N+h3-1)*N+p12))+(-1.0d0)*t2e(((((p6-1)*N+p12-1)*N+h2-1)*N+h3))*i1(((((p4-1)*N+p5-1)*N+h1-1)*N+p12))+(1.0&
&d0)*t2e(((((p6-1)*N+p12-1)*N+h1-1)*N+h3))*i1(((((p4-1)*N+p5-1)*N+h2-1)*N+p12))
END DO
END DO
END DO
END DO
END DO
END DO
END DO
DO h11=1,nocc
DO h1=1,nocc
i1(((h11-1)*N+h1))=(1.0d0)*f1(((h11-1)*N+h1))
END DO
END DO
DO h11=1,nocc
DO p10=nocc+1,N
i2(((h11-1)*N+p10))=(1.0d0)*f1(((h11-1)*N+p10))
END DO
END DO
DO h11=1,nocc
DO h9=1,nocc
IF (h9>h11) CYCLE
DO p10=nocc+1,N
DO p8=nocc+1,N
IF (p8>p10) CYCLE
v2e(((((h11-1)*N+h9-1)*N+p10-1)*N+p8))= + v2(((((h9-1)*N+h11-1)*N+p8-1)*N+p10))
v2e(((((h11-1)*N+h9-1)*N+p8-1)*N+p10))= - v2(((((h9-1)*N+h11-1)*N+p8-1)*N+p10))
v2e(((((h9-1)*N+h11-1)*N+p10-1)*N+p8))= - v2(((((h9-1)*N+h11-1)*N+p8-1)*N+p10))
v2e(((((h9-1)*N+h11-1)*N+p8-1)*N+p10))= + v2(((((h9-1)*N+h11-1)*N+p8-1)*N+p10))
END DO
END DO
END DO
END DO
DO h11=1,nocc
DO p10=nocc+1,N
DO p8=nocc+1,N
DO h9=1,nocc
i2(((h11-1)*N+p10))=i2(((h11-1)*N+p10))+(1.0d0)*t1(((p8-1)*N+h9))*v2e(((((h9-1)*N+h11-1)*N+p8-1)*N+p10))
END DO
END DO
END DO
END DO
DO h11=1,nocc
DO h1=1,nocc
DO p10=nocc+1,N
i1(((h11-1)*N+h1))=i1(((h11-1)*N+h1))+(1.0d0)*t1(((p10-1)*N+h1))*i2(((h11-1)*N+p10))
END DO
END DO
END DO
DO h11=1,nocc
DO h9=1,nocc
IF (h9>h11) CYCLE
DO h1=1,nocc
DO p8=nocc+1,N
v2e(((((h11-1)*N+h9-1)*N+h1-1)*N+p8))= - v2(((((h9-1)*N+h11-1)*N+h1-1)*N+p8))
v2e(((((h9-1)*N+h11-1)*N+h1-1)*N+p8))= + v2(((((h9-1)*N+h11-1)*N+h1-1)*N+p8))
END DO
END DO
END DO
END DO
DO h11=1,nocc
DO h1=1,nocc
DO p8=nocc+1,N
DO h9=1,nocc
i1(((h11-1)*N+h1))=i1(((h11-1)*N+h1))+(-1.0d0)*t1(((p8-1)*N+h9))*v2e(((((h9-1)*N+h11-1)*N+h1-1)*N+p8))
END DO
END DO
END DO
END DO
DO p8=nocc+1,N
DO p9=nocc+1,N
IF (p8>p9) CYCLE
DO h1=1,nocc
DO h10=1,nocc
IF (h1>h10) CYCLE
t2e(((((p8-1)*N+p9-1)*N+h10-1)*N+h1))= - t2(((((p8-1)*N+p9-1)*N+h1-1)*N+h10))
t2e(((((p8-1)*N+p9-1)*N+h1-1)*N+h10))= + t2(((((p8-1)*N+p9-1)*N+h1-1)*N+h10))
END DO
END DO
END DO
END DO
DO h11=1,nocc
DO h10=1,nocc
IF (h10>h11) CYCLE
DO p8=nocc+1,N
DO p9=nocc+1,N
IF (p8>p9) CYCLE
v2e(((((h11-1)*N+h10-1)*N+p8-1)*N+p9))= - v2(((((h10-1)*N+h11-1)*N+p8-1)*N+p9))
v2e(((((h10-1)*N+h11-1)*N+p8-1)*N+p9))= + v2(((((h10-1)*N+h11-1)*N+p8-1)*N+p9))
END DO
END DO
END DO
END DO
DO h11=1,nocc
DO h1=1,nocc
DO p8=nocc+1,N
DO p9=nocc+1,N
IF (p8>p9) CYCLE
DO h10=1,nocc
i1(((h11-1)*N+h1))=i1(((h11-1)*N+h1))+(-1.0d0)*t2e(((((p8-1)*N+p9-1)*N+h1-1)*N+h10))*v2e(((((h10-1)*N+h11-1)*N+p8-1)*N+p9))
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p5=nocc+1,N
DO p6=nocc+1,N
IF (p4>p5) CYCLE
IF (p5>p6) CYCLE
DO h1=1,nocc
DO h2=1,nocc
DO h11=1,nocc
IF (h1>h2) CYCLE
IF (h2>h11) CYCLE
t3e(((((((p4-1)*N+p5-1)*N+p6-1)*N+h2-1)*N+h11-1)*N+h1))= + t3(((((((p4-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h11))
t3e(((((((p4-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h11-1)*N+h2))= - t3(((((((p4-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h11))
t3e(((((((p4-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h11))= + t3(((((((p4-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h11))
END DO
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p5=nocc+1,N
DO p6=nocc+1,N
IF (p4>p5) CYCLE
IF (p5>p6) CYCLE
DO h1=1,nocc
DO h2=1,nocc
DO h3=1,nocc
IF (h1>h2) CYCLE
IF (h2>h3) CYCLE
DO h11=1,nocc
i0(((((((p4-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h3))=i0(((((((p4-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h3))+(-1.0d0)*t3e(((((((p4-1&
&)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h11))*i1(((h11-1)*N+h3))+(-1.0d0)*t3e(((((((p4-1)*N+p5-1)*N+p6-1)*N+h2-1)*N+h3-1)*N+h11))*i1((&
&(h11-1)*N+h1))+(1.0d0)*t3e(((((((p4-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h3-1)*N+h11))*i1(((h11-1)*N+h2))
END DO
END DO
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p7=nocc+1,N
i1(((p4-1)*N+p7))=(1.0d0)*f1(((p4-1)*N+p7))
END DO
END DO
DO p4=nocc+1,N
DO h9=1,nocc
DO p7=nocc+1,N
DO p8=nocc+1,N
IF (p7>p8) CYCLE
v2e(((((h9-1)*N+p4-1)*N+p8-1)*N+p7))= - v2(((((h9-1)*N+p4-1)*N+p7-1)*N+p8))
v2e(((((h9-1)*N+p4-1)*N+p7-1)*N+p8))= + v2(((((h9-1)*N+p4-1)*N+p7-1)*N+p8))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p7=nocc+1,N
DO p8=nocc+1,N
DO h9=1,nocc
i1(((p4-1)*N+p7))=i1(((p4-1)*N+p7))+(-1.0d0)*t1(((p8-1)*N+h9))*v2e(((((h9-1)*N+p4-1)*N+p7-1)*N+p8))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p8=nocc+1,N
IF (p4>p8) CYCLE
DO h9=1,nocc
DO h10=1,nocc
IF (h9>h10) CYCLE
t2e(((((p8-1)*N+p4-1)*N+h9-1)*N+h10))= - t2(((((p4-1)*N+p8-1)*N+h9-1)*N+h10))
t2e(((((p4-1)*N+p8-1)*N+h9-1)*N+h10))= + t2(((((p4-1)*N+p8-1)*N+h9-1)*N+h10))
END DO
END DO
END DO
END DO
DO h9=1,nocc
DO h10=1,nocc
IF (h9>h10) CYCLE
DO p7=nocc+1,N
DO p8=nocc+1,N
IF (p7>p8) CYCLE
v2e(((((h9-1)*N+h10-1)*N+p8-1)*N+p7))= - v2(((((h9-1)*N+h10-1)*N+p7-1)*N+p8))
v2e(((((h9-1)*N+h10-1)*N+p7-1)*N+p8))= + v2(((((h9-1)*N+h10-1)*N+p7-1)*N+p8))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p7=nocc+1,N
DO p8=nocc+1,N
DO h9=1,nocc
DO h10=1,nocc
IF (h9>h10) CYCLE
i1(((p4-1)*N+p7))=i1(((p4-1)*N+p7))+(-1.0d0)*t2e(((((p4-1)*N+p8-1)*N+h9-1)*N+h10))*v2e(((((h9-1)*N+h10-1)*N+p7-1)*N+p8))
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p5=nocc+1,N
DO p7=nocc+1,N
IF (p4>p5) CYCLE
IF (p5>p7) CYCLE
DO h1=1,nocc
DO h2=1,nocc
DO h3=1,nocc
IF (h1>h2) CYCLE
IF (h2>h3) CYCLE
t3e(((((((p5-1)*N+p7-1)*N+p4-1)*N+h1-1)*N+h2-1)*N+h3))= + t3(((((((p4-1)*N+p5-1)*N+p7-1)*N+h1-1)*N+h2-1)*N+h3))
t3e(((((((p4-1)*N+p7-1)*N+p5-1)*N+h1-1)*N+h2-1)*N+h3))= - t3(((((((p4-1)*N+p5-1)*N+p7-1)*N+h1-1)*N+h2-1)*N+h3))
t3e(((((((p4-1)*N+p5-1)*N+p7-1)*N+h1-1)*N+h2-1)*N+h3))= + t3(((((((p4-1)*N+p5-1)*N+p7-1)*N+h1-1)*N+h2-1)*N+h3))
END DO
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p5=nocc+1,N
DO p6=nocc+1,N
IF (p4>p5) CYCLE
IF (p5>p6) CYCLE
DO h1=1,nocc
DO h2=1,nocc
DO h3=1,nocc
IF (h1>h2) CYCLE
IF (h2>h3) CYCLE
DO p7=nocc+1,N
i0(((((((p4-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h3))=i0(((((((p4-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h3))+(1.0d0)*t3e(((((((p4-1)&
&*N+p5-1)*N+p7-1)*N+h1-1)*N+h2-1)*N+h3))*i1(((p6-1)*N+p7))+(1.0d0)*t3e(((((((p5-1)*N+p6-1)*N+p7-1)*N+h1-1)*N+h2-1)*N+h3))*i1(((p4-1&
&)*N+p7))+(-1.0d0)*t3e(((((((p4-1)*N+p6-1)*N+p7-1)*N+h1-1)*N+h2-1)*N+h3))*i1(((p5-1)*N+p7))
END DO
END DO
END DO
END DO
END DO
END DO
END DO
DO h11=1,nocc
DO h12=1,nocc
IF (h11>h12) CYCLE
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
i1(((((h11-1)*N+h12-1)*N+h1-1)*N+h2))=(1.0d0)*v2(((((h11-1)*N+h12-1)*N+h1-1)*N+h2))
END DO
END DO
END DO
END DO
DO h11=1,nocc
DO h12=1,nocc
IF (h11>h12) CYCLE
DO h1=1,nocc
DO p10=nocc+1,N
v2e(((((h11-1)*N+h12-1)*N+h1-1)*N+p10))= + v2(((((h11-1)*N+h12-1)*N+h1-1)*N+p10))
END DO
END DO
END DO
END DO
DO h11=1,nocc
DO h12=1,nocc
IF (h11>h12) CYCLE
DO h1=1,nocc
DO p10=nocc+1,N
i2(((((h11-1)*N+h12-1)*N+h1-1)*N+p10))=(1.0d0)*v2e(((((h11-1)*N+h12-1)*N+h1-1)*N+p10))
END DO
END DO
END DO
END DO
DO h11=1,nocc
DO h12=1,nocc
IF (h11>h12) CYCLE
DO p10=nocc+1,N
DO p8=nocc+1,N
IF (p8>p10) CYCLE
v2e(((((h11-1)*N+h12-1)*N+p10-1)*N+p8))= - v2(((((h11-1)*N+h12-1)*N+p8-1)*N+p10))
v2e(((((h11-1)*N+h12-1)*N+p8-1)*N+p10))= + v2(((((h11-1)*N+h12-1)*N+p8-1)*N+p10))
END DO
END DO
END DO
END DO
DO h11=1,nocc
DO h12=1,nocc
IF (h11>h12) CYCLE
DO h1=1,nocc
DO p10=nocc+1,N
DO p8=nocc+1,N
i2(((((h11-1)*N+h12-1)*N+h1-1)*N+p10))=i2(((((h11-1)*N+h12-1)*N+h1-1)*N+p10))+(0.5d0)*t1(((p8-1)*N+h1))*v2e(((((h11-1)*N+h12-1)*N+p&
&8-1)*N+p10))
END DO
END DO
END DO
END DO
END DO
DO h11=1,nocc
DO h12=1,nocc
IF (h11>h12) CYCLE
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
DO p10=nocc+1,N
i1(((((h11-1)*N+h12-1)*N+h1-1)*N+h2))=i1(((((h11-1)*N+h12-1)*N+h1-1)*N+h2))+(-1.0d0)*t1(((p10-1)*N+h1))*i2(((((h11-1)*N+h12-1)*N+h2&
&-1)*N+p10))+(1.0d0)*t1(((p10-1)*N+h2))*i2(((((h11-1)*N+h12-1)*N+h1-1)*N+p10))
END DO
END DO
END DO
END DO
END DO
DO h11=1,nocc
DO h12=1,nocc
IF (h11>h12) CYCLE
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
DO p9=nocc+1,N
DO p10=nocc+1,N
IF (p9>p10) CYCLE
i1(((((h11-1)*N+h12-1)*N+h1-1)*N+h2))=i1(((((h11-1)*N+h12-1)*N+h1-1)*N+h2))+(1.0d0)*t2(((((p9-1)*N+p10-1)*N+h1-1)*N+h2))*v2(((((h11&
&-1)*N+h12-1)*N+p9-1)*N+p10))
END DO
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p5=nocc+1,N
DO p6=nocc+1,N
IF (p4>p5) CYCLE
IF (p5>p6) CYCLE
DO h1=1,nocc
DO h11=1,nocc
DO h12=1,nocc
IF (h1>h11) CYCLE
IF (h11>h12) CYCLE
t3e(((((((p4-1)*N+p5-1)*N+p6-1)*N+h11-1)*N+h1-1)*N+h12))= - t3(((((((p4-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h11-1)*N+h12))
t3e(((((((p4-1)*N+p5-1)*N+p6-1)*N+h12-1)*N+h1-1)*N+h11))= + t3(((((((p4-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h11-1)*N+h12))
t3e(((((((p4-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h11-1)*N+h12))= + t3(((((((p4-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h11-1)*N+h12))
END DO
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p5=nocc+1,N
DO p6=nocc+1,N
IF (p4>p5) CYCLE
IF (p5>p6) CYCLE
DO h1=1,nocc
DO h2=1,nocc
DO h3=1,nocc
IF (h1>h2) CYCLE
IF (h2>h3) CYCLE
DO h11=1,nocc
DO h12=1,nocc
IF (h11>h12) CYCLE
i0(((((((p4-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h3))=i0(((((((p4-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h3))+(1.0d0)*t3e(((((((p4-1)&
&*N+p5-1)*N+p6-1)*N+h1-1)*N+h11-1)*N+h12))*i1(((((h11-1)*N+h12-1)*N+h2-1)*N+h3))+(-1.0d0)*t3e(((((((p4-1)*N+p5-1)*N+p6-1)*N+h2-1)*N&
&+h11-1)*N+h12))*i1(((((h11-1)*N+h12-1)*N+h1-1)*N+h3))+(1.0d0)*t3e(((((((p4-1)*N+p5-1)*N+p6-1)*N+h3-1)*N+h11-1)*N+h12))*i1(((((h11-&
&1)*N+h12-1)*N+h1-1)*N+h2))
END DO
END DO
END DO
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h8=1,nocc
DO h1=1,nocc
DO p7=nocc+1,N
v2e(((((h8-1)*N+p4-1)*N+h1-1)*N+p7))= + v2(((((h8-1)*N+p4-1)*N+h1-1)*N+p7))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h8=1,nocc
DO h1=1,nocc
DO p7=nocc+1,N
i1(((((h8-1)*N+p4-1)*N+h1-1)*N+p7))=(1.0d0)*v2e(((((h8-1)*N+p4-1)*N+h1-1)*N+p7))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h8=1,nocc
DO p7=nocc+1,N
DO p9=nocc+1,N
IF (p7>p9) CYCLE
v2e(((((h8-1)*N+p4-1)*N+p9-1)*N+p7))= - v2(((((h8-1)*N+p4-1)*N+p7-1)*N+p9))
v2e(((((h8-1)*N+p4-1)*N+p7-1)*N+p9))= + v2(((((h8-1)*N+p4-1)*N+p7-1)*N+p9))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h8=1,nocc
DO h1=1,nocc
DO p7=nocc+1,N
DO p9=nocc+1,N
i1(((((h8-1)*N+p4-1)*N+h1-1)*N+p7))=i1(((((h8-1)*N+p4-1)*N+h1-1)*N+p7))+(-1.0d0)*t1(((p9-1)*N+h1))*v2e(((((h8-1)*N+p4-1)*N+p7-1)*N+&
&p9))
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p9=nocc+1,N
IF (p4>p9) CYCLE
DO h1=1,nocc
DO h10=1,nocc
IF (h1>h10) CYCLE
t2e(((((p9-1)*N+p4-1)*N+h10-1)*N+h1))= + t2(((((p4-1)*N+p9-1)*N+h1-1)*N+h10))
t2e(((((p9-1)*N+p4-1)*N+h1-1)*N+h10))= - t2(((((p4-1)*N+p9-1)*N+h1-1)*N+h10))
t2e(((((p4-1)*N+p9-1)*N+h10-1)*N+h1))= - t2(((((p4-1)*N+p9-1)*N+h1-1)*N+h10))
t2e(((((p4-1)*N+p9-1)*N+h1-1)*N+h10))= + t2(((((p4-1)*N+p9-1)*N+h1-1)*N+h10))
END DO
END DO
END DO
END DO
DO h8=1,nocc
DO h10=1,nocc
IF (h8>h10) CYCLE
DO p7=nocc+1,N
DO p9=nocc+1,N
IF (p7>p9) CYCLE
v2e(((((h10-1)*N+h8-1)*N+p9-1)*N+p7))= + v2(((((h8-1)*N+h10-1)*N+p7-1)*N+p9))
v2e(((((h10-1)*N+h8-1)*N+p7-1)*N+p9))= - v2(((((h8-1)*N+h10-1)*N+p7-1)*N+p9))
v2e(((((h8-1)*N+h10-1)*N+p9-1)*N+p7))= - v2(((((h8-1)*N+h10-1)*N+p7-1)*N+p9))
v2e(((((h8-1)*N+h10-1)*N+p7-1)*N+p9))= + v2(((((h8-1)*N+h10-1)*N+p7-1)*N+p9))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h8=1,nocc
DO h1=1,nocc
DO p7=nocc+1,N
DO p9=nocc+1,N
DO h10=1,nocc
i1(((((h8-1)*N+p4-1)*N+h1-1)*N+p7))=i1(((((h8-1)*N+p4-1)*N+h1-1)*N+p7))+(-1.0d0)*t2e(((((p4-1)*N+p9-1)*N+h1-1)*N+h10))*v2e(((((h8-1&
&)*N+h10-1)*N+p7-1)*N+p9))
END DO
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p5=nocc+1,N
DO p7=nocc+1,N
IF (p4>p5) CYCLE
IF (p5>p7) CYCLE
DO h1=1,nocc
DO h2=1,nocc
DO h8=1,nocc
IF (h1>h2) CYCLE
IF (h2>h8) CYCLE
t3e(((((((p5-1)*N+p7-1)*N+p4-1)*N+h2-1)*N+h8-1)*N+h1))= + t3(((((((p4-1)*N+p5-1)*N+p7-1)*N+h1-1)*N+h2-1)*N+h8))
t3e(((((((p5-1)*N+p7-1)*N+p4-1)*N+h1-1)*N+h8-1)*N+h2))= - t3(((((((p4-1)*N+p5-1)*N+p7-1)*N+h1-1)*N+h2-1)*N+h8))
t3e(((((((p5-1)*N+p7-1)*N+p4-1)*N+h1-1)*N+h2-1)*N+h8))= + t3(((((((p4-1)*N+p5-1)*N+p7-1)*N+h1-1)*N+h2-1)*N+h8))
t3e(((((((p4-1)*N+p7-1)*N+p5-1)*N+h2-1)*N+h8-1)*N+h1))= - t3(((((((p4-1)*N+p5-1)*N+p7-1)*N+h1-1)*N+h2-1)*N+h8))
t3e(((((((p4-1)*N+p7-1)*N+p5-1)*N+h1-1)*N+h8-1)*N+h2))= + t3(((((((p4-1)*N+p5-1)*N+p7-1)*N+h1-1)*N+h2-1)*N+h8))
t3e(((((((p4-1)*N+p7-1)*N+p5-1)*N+h1-1)*N+h2-1)*N+h8))= - t3(((((((p4-1)*N+p5-1)*N+p7-1)*N+h1-1)*N+h2-1)*N+h8))
t3e(((((((p4-1)*N+p5-1)*N+p7-1)*N+h2-1)*N+h8-1)*N+h1))= + t3(((((((p4-1)*N+p5-1)*N+p7-1)*N+h1-1)*N+h2-1)*N+h8))
t3e(((((((p4-1)*N+p5-1)*N+p7-1)*N+h1-1)*N+h8-1)*N+h2))= - t3(((((((p4-1)*N+p5-1)*N+p7-1)*N+h1-1)*N+h2-1)*N+h8))
t3e(((((((p4-1)*N+p5-1)*N+p7-1)*N+h1-1)*N+h2-1)*N+h8))= + t3(((((((p4-1)*N+p5-1)*N+p7-1)*N+h1-1)*N+h2-1)*N+h8))
END DO
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p5=nocc+1,N
DO p6=nocc+1,N
IF (p4>p5) CYCLE
IF (p5>p6) CYCLE
DO h1=1,nocc
DO h2=1,nocc
DO h3=1,nocc
IF (h1>h2) CYCLE
IF (h2>h3) CYCLE
DO p7=nocc+1,N
DO h8=1,nocc
i0(((((((p4-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h3))=i0(((((((p4-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h3))+(-1.0d0)*t3e(((((((p4-1&
&)*N+p5-1)*N+p7-1)*N+h1-1)*N+h2-1)*N+h8))*i1(((((h8-1)*N+p6-1)*N+h3-1)*N+p7))+(-1.0d0)*t3e(((((((p4-1)*N+p5-1)*N+p7-1)*N+h2-1)*N+h3&
&-1)*N+h8))*i1(((((h8-1)*N+p6-1)*N+h1-1)*N+p7))+(1.0d0)*t3e(((((((p4-1)*N+p5-1)*N+p7-1)*N+h1-1)*N+h3-1)*N+h8))*i1(((((h8-1)*N+p6-1)&
&*N+h2-1)*N+p7))+(-1.0d0)*t3e(((((((p5-1)*N+p6-1)*N+p7-1)*N+h1-1)*N+h2-1)*N+h8))*i1(((((h8-1)*N+p4-1)*N+h3-1)*N+p7))+(-1.0d0)*t3e((&
&(((((p5-1)*N+p6-1)*N+p7-1)*N+h2-1)*N+h3-1)*N+h8))*i1(((((h8-1)*N+p4-1)*N+h1-1)*N+p7))+(1.0d0)*t3e(((((((p5-1)*N+p6-1)*N+p7-1)*N+h1&
&-1)*N+h3-1)*N+h8))*i1(((((h8-1)*N+p4-1)*N+h2-1)*N+p7))+(1.0d0)*t3e(((((((p4-1)*N+p6-1)*N+p7-1)*N+h1-1)*N+h2-1)*N+h8))*i1(((((h8-1)&
&*N+p5-1)*N+h3-1)*N+p7))+(1.0d0)*t3e(((((((p4-1)*N+p6-1)*N+p7-1)*N+h2-1)*N+h3-1)*N+h8))*i1(((((h8-1)*N+p5-1)*N+h1-1)*N+p7))+(-1.0d0&
&)*t3e(((((((p4-1)*N+p6-1)*N+p7-1)*N+h1-1)*N+h3-1)*N+h8))*i1(((((h8-1)*N+p5-1)*N+h2-1)*N+p7))
END DO
END DO
END DO
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p7=nocc+1,N
DO p8=nocc+1,N
IF (p4>p7) CYCLE
IF (p7>p8) CYCLE
DO h1=1,nocc
DO h2=1,nocc
DO h3=1,nocc
IF (h1>h2) CYCLE
IF (h2>h3) CYCLE
t3e(((((((p7-1)*N+p4-1)*N+p8-1)*N+h1-1)*N+h2-1)*N+h3))= - t3(((((((p4-1)*N+p7-1)*N+p8-1)*N+h1-1)*N+h2-1)*N+h3))
t3e(((((((p8-1)*N+p4-1)*N+p7-1)*N+h1-1)*N+h2-1)*N+h3))= + t3(((((((p4-1)*N+p7-1)*N+p8-1)*N+h1-1)*N+h2-1)*N+h3))
t3e(((((((p4-1)*N+p7-1)*N+p8-1)*N+h1-1)*N+h2-1)*N+h3))= + t3(((((((p4-1)*N+p7-1)*N+p8-1)*N+h1-1)*N+h2-1)*N+h3))
END DO
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p5=nocc+1,N
DO p6=nocc+1,N
IF (p4>p5) CYCLE
IF (p5>p6) CYCLE
DO h1=1,nocc
DO h2=1,nocc
DO h3=1,nocc
IF (h1>h2) CYCLE
IF (h2>h3) CYCLE
DO p7=nocc+1,N
DO p8=nocc+1,N
IF (p7>p8) CYCLE
i0(((((((p4-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h3))=i0(((((((p4-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h3))+(1.0d0)*t3e(((((((p4-1)&
&*N+p7-1)*N+p8-1)*N+h1-1)*N+h2-1)*N+h3))*v2(((((p5-1)*N+p6-1)*N+p7-1)*N+p8))+(-1.0d0)*t3e(((((((p5-1)*N+p7-1)*N+p8-1)*N+h1-1)*N+h2-&
&1)*N+h3))*v2(((((p4-1)*N+p6-1)*N+p7-1)*N+p8))+(1.0d0)*t3e(((((((p6-1)*N+p7-1)*N+p8-1)*N+h1-1)*N+h2-1)*N+h3))*v2(((((p4-1)*N+p5-1)*&
&N+p7-1)*N+p8))
END DO
END DO
END DO
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h12=1,nocc
DO h1=1,nocc
DO p11=nocc+1,N
v2e(((((h12-1)*N+p4-1)*N+h1-1)*N+p11))= + v2(((((h12-1)*N+p4-1)*N+h1-1)*N+p11))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h12=1,nocc
DO h1=1,nocc
DO p11=nocc+1,N
i2(((((h12-1)*N+p4-1)*N+h1-1)*N+p11))=(1.0d0)*v2e(((((h12-1)*N+p4-1)*N+h1-1)*N+p11))
END DO
END DO
END DO
END DO
DO h8=1,nocc
DO h12=1,nocc
IF (h8>h12) CYCLE
DO h1=1,nocc
DO p11=nocc+1,N
v2e(((((h8-1)*N+h12-1)*N+h1-1)*N+p11))= + v2(((((h8-1)*N+h12-1)*N+h1-1)*N+p11))
END DO
END DO
END DO
END DO
DO h8=1,nocc
DO h12=1,nocc
IF (h8>h12) CYCLE
DO h1=1,nocc
DO p11=nocc+1,N
i3(((((h8-1)*N+h12-1)*N+h1-1)*N+p11))=(1.0d0)*v2e(((((h8-1)*N+h12-1)*N+h1-1)*N+p11))
END DO
END DO
END DO
END DO
DO h8=1,nocc
DO h12=1,nocc
IF (h8>h12) CYCLE
DO p11=nocc+1,N
DO p9=nocc+1,N
IF (p9>p11) CYCLE
v2e(((((h8-1)*N+h12-1)*N+p11-1)*N+p9))= - v2(((((h8-1)*N+h12-1)*N+p9-1)*N+p11))
v2e(((((h8-1)*N+h12-1)*N+p9-1)*N+p11))= + v2(((((h8-1)*N+h12-1)*N+p9-1)*N+p11))
END DO
END DO
END DO
END DO
DO h8=1,nocc
DO h12=1,nocc
IF (h8>h12) CYCLE
DO h1=1,nocc
DO p11=nocc+1,N
DO p9=nocc+1,N
i3(((((h8-1)*N+h12-1)*N+h1-1)*N+p11))=i3(((((h8-1)*N+h12-1)*N+h1-1)*N+p11))+(1.0d0)*t1(((p9-1)*N+h1))*v2e(((((h8-1)*N+h12-1)*N+p9-1&
&)*N+p11))
END DO
END DO
END DO
END DO
END DO
DO h8=1,nocc
DO h12=1,nocc
IF (h8>h12) CYCLE
DO h1=1,nocc
DO p11=nocc+1,N
i3e(((((h12-1)*N+h8-1)*N+h1-1)*N+p11))= - i3(((((h8-1)*N+h12-1)*N+h1-1)*N+p11))
i3e(((((h8-1)*N+h12-1)*N+h1-1)*N+p11))= + i3(((((h8-1)*N+h12-1)*N+h1-1)*N+p11))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h12=1,nocc
DO h1=1,nocc
DO p11=nocc+1,N
DO h8=1,nocc
i2(((((h12-1)*N+p4-1)*N+h1-1)*N+p11))=i2(((((h12-1)*N+p4-1)*N+h1-1)*N+p11))+(0.5d0)*t1(((p4-1)*N+h8))*i3e(((((h8-1)*N+h12-1)*N+h1-1&
&)*N+p11))
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h12=1,nocc
DO p11=nocc+1,N
DO p8=nocc+1,N
IF (p8>p11) CYCLE
v2e(((((h12-1)*N+p4-1)*N+p11-1)*N+p8))= - v2(((((h12-1)*N+p4-1)*N+p8-1)*N+p11))
v2e(((((h12-1)*N+p4-1)*N+p8-1)*N+p11))= + v2(((((h12-1)*N+p4-1)*N+p8-1)*N+p11))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h12=1,nocc
DO h1=1,nocc
DO p11=nocc+1,N
DO p8=nocc+1,N
i2(((((h12-1)*N+p4-1)*N+h1-1)*N+p11))=i2(((((h12-1)*N+p4-1)*N+h1-1)*N+p11))+(1.0d0)*t1(((p8-1)*N+h1))*v2e(((((h12-1)*N+p4-1)*N+p8-1&
&)*N+p11))
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p8=nocc+1,N
IF (p4>p8) CYCLE
DO h1=1,nocc
DO h9=1,nocc
IF (h1>h9) CYCLE
t2e(((((p8-1)*N+p4-1)*N+h9-1)*N+h1))= + t2(((((p4-1)*N+p8-1)*N+h1-1)*N+h9))
t2e(((((p8-1)*N+p4-1)*N+h1-1)*N+h9))= - t2(((((p4-1)*N+p8-1)*N+h1-1)*N+h9))
t2e(((((p4-1)*N+p8-1)*N+h9-1)*N+h1))= - t2(((((p4-1)*N+p8-1)*N+h1-1)*N+h9))
t2e(((((p4-1)*N+p8-1)*N+h1-1)*N+h9))= + t2(((((p4-1)*N+p8-1)*N+h1-1)*N+h9))
END DO
END DO
END DO
END DO
DO h12=1,nocc
DO h9=1,nocc
IF (h9>h12) CYCLE
DO p11=nocc+1,N
DO p8=nocc+1,N
IF (p8>p11) CYCLE
v2e(((((h12-1)*N+h9-1)*N+p11-1)*N+p8))= + v2(((((h9-1)*N+h12-1)*N+p8-1)*N+p11))
v2e(((((h12-1)*N+h9-1)*N+p8-1)*N+p11))= - v2(((((h9-1)*N+h12-1)*N+p8-1)*N+p11))
v2e(((((h9-1)*N+h12-1)*N+p11-1)*N+p8))= - v2(((((h9-1)*N+h12-1)*N+p8-1)*N+p11))
v2e(((((h9-1)*N+h12-1)*N+p8-1)*N+p11))= + v2(((((h9-1)*N+h12-1)*N+p8-1)*N+p11))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h12=1,nocc
DO h1=1,nocc
DO p11=nocc+1,N
DO p8=nocc+1,N
DO h9=1,nocc
i2(((((h12-1)*N+p4-1)*N+h1-1)*N+p11))=i2(((((h12-1)*N+p4-1)*N+h1-1)*N+p11))+(-1.0d0)*t2e(((((p4-1)*N+p8-1)*N+h1-1)*N+h9))*v2e(((((h&
&9-1)*N+h12-1)*N+p8-1)*N+p11))
END DO
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p11=nocc+1,N
IF (p4>p11) CYCLE
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
t2e(((((p11-1)*N+p4-1)*N+h1-1)*N+h2))= - t2(((((p4-1)*N+p11-1)*N+h1-1)*N+h2))
t2e(((((p4-1)*N+p11-1)*N+h1-1)*N+h2))= + t2(((((p4-1)*N+p11-1)*N+h1-1)*N+h2))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p5=nocc+1,N
IF (p4>p5) CYCLE
DO h12=1,nocc
DO h1=1,nocc
DO h2=1,nocc
DO h3=1,nocc
IF (h1>h2) CYCLE
IF (h2>h3) CYCLE
i1(((((((h12-1)*N+p4-1)*N+p5-1)*N+h1-1)*N+h2-1)*N+h3))=0.0d0
DO p11=nocc+1,N
i1(((((((h12-1)*N+p4-1)*N+p5-1)*N+h1-1)*N+h2-1)*N+h3))=i1(((((((h12-1)*N+p4-1)*N+p5-1)*N+h1-1)*N+h2-1)*N+h3))+(-0.16666666666666666&
&d0)*t2e(((((p4-1)*N+p11-1)*N+h1-1)*N+h2))*i2(((((h12-1)*N+p5-1)*N+h3-1)*N+p11))+(-0.16666666666666666d0)*t2e(((((p4-1)*N+p11-1)*N+&
&h2-1)*N+h3))*i2(((((h12-1)*N+p5-1)*N+h1-1)*N+p11))+(0.16666666666666666d0)*t2e(((((p4-1)*N+p11-1)*N+h1-1)*N+h3))*i2(((((h12-1)*N+p&
&5-1)*N+h2-1)*N+p11))+(0.16666666666666666d0)*t2e(((((p5-1)*N+p11-1)*N+h1-1)*N+h2))*i2(((((h12-1)*N+p4-1)*N+h3-1)*N+p11))+(0.166666&
&66666666666d0)*t2e(((((p5-1)*N+p11-1)*N+h2-1)*N+h3))*i2(((((h12-1)*N+p4-1)*N+h1-1)*N+p11))+(-0.16666666666666666d0)*t2e(((((p5-1)*&
&N+p11-1)*N+h1-1)*N+h3))*i2(((((h12-1)*N+p4-1)*N+h2-1)*N+p11))
END DO
END DO
END DO
END DO
END DO
END DO
END DO
DO h12=1,nocc
DO p7=nocc+1,N
i2(((h12-1)*N+p7))=(1.0d0)*f1(((h12-1)*N+p7))
END DO
END DO
DO h12=1,nocc
DO h9=1,nocc
IF (h9>h12) CYCLE
DO p7=nocc+1,N
DO p8=nocc+1,N
IF (p7>p8) CYCLE
v2e(((((h12-1)*N+h9-1)*N+p8-1)*N+p7))= + v2(((((h9-1)*N+h12-1)*N+p7-1)*N+p8))
v2e(((((h12-1)*N+h9-1)*N+p7-1)*N+p8))= - v2(((((h9-1)*N+h12-1)*N+p7-1)*N+p8))
v2e(((((h9-1)*N+h12-1)*N+p8-1)*N+p7))= - v2(((((h9-1)*N+h12-1)*N+p7-1)*N+p8))
v2e(((((h9-1)*N+h12-1)*N+p7-1)*N+p8))= + v2(((((h9-1)*N+h12-1)*N+p7-1)*N+p8))
END DO
END DO
END DO
END DO
DO h12=1,nocc
DO p7=nocc+1,N
DO p8=nocc+1,N
DO h9=1,nocc
i2(((h12-1)*N+p7))=i2(((h12-1)*N+p7))+(-1.0d0)*t1(((p8-1)*N+h9))*v2e(((((h9-1)*N+h12-1)*N+p7-1)*N+p8))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p5=nocc+1,N
DO p7=nocc+1,N
IF (p4>p5) CYCLE
IF (p5>p7) CYCLE
DO h1=1,nocc
DO h2=1,nocc
DO h3=1,nocc
IF (h1>h2) CYCLE
IF (h2>h3) CYCLE
t3e(((((((p5-1)*N+p7-1)*N+p4-1)*N+h1-1)*N+h2-1)*N+h3))= + t3(((((((p4-1)*N+p5-1)*N+p7-1)*N+h1-1)*N+h2-1)*N+h3))
t3e(((((((p4-1)*N+p7-1)*N+p5-1)*N+h1-1)*N+h2-1)*N+h3))= - t3(((((((p4-1)*N+p5-1)*N+p7-1)*N+h1-1)*N+h2-1)*N+h3))
t3e(((((((p4-1)*N+p5-1)*N+p7-1)*N+h1-1)*N+h2-1)*N+h3))= + t3(((((((p4-1)*N+p5-1)*N+p7-1)*N+h1-1)*N+h2-1)*N+h3))
END DO
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p5=nocc+1,N
IF (p4>p5) CYCLE
DO h12=1,nocc
DO h1=1,nocc
DO h2=1,nocc
DO h3=1,nocc
IF (h1>h2) CYCLE
IF (h2>h3) CYCLE
DO p7=nocc+1,N
i1(((((((h12-1)*N+p4-1)*N+p5-1)*N+h1-1)*N+h2-1)*N+h3))=i1(((((((h12-1)*N+p4-1)*N+p5-1)*N+h1-1)*N+h2-1)*N+h3))+(-0.16666666666666666&
&d0)*t3e(((((((p4-1)*N+p5-1)*N+p7-1)*N+h1-1)*N+h2-1)*N+h3))*i2(((h12-1)*N+p7))
END DO
END DO
END DO
END DO
END DO
END DO
END DO
DO h7=1,nocc
DO h12=1,nocc
IF (h7>h12) CYCLE
DO h1=1,nocc
DO p11=nocc+1,N
v2e(((((h7-1)*N+h12-1)*N+h1-1)*N+p11))= + v2(((((h7-1)*N+h12-1)*N+h1-1)*N+p11))
END DO
END DO
END DO
END DO
DO h7=1,nocc
DO h12=1,nocc
IF (h7>h12) CYCLE
DO h1=1,nocc
DO p11=nocc+1,N
i2(((((h7-1)*N+h12-1)*N+h1-1)*N+p11))=(1.0d0)*v2e(((((h7-1)*N+h12-1)*N+h1-1)*N+p11))
END DO
END DO
END DO
END DO
DO h7=1,nocc
DO h12=1,nocc
IF (h7>h12) CYCLE
DO p11=nocc+1,N
DO p8=nocc+1,N
IF (p8>p11) CYCLE
v2e(((((h7-1)*N+h12-1)*N+p11-1)*N+p8))= - v2(((((h7-1)*N+h12-1)*N+p8-1)*N+p11))
v2e(((((h7-1)*N+h12-1)*N+p8-1)*N+p11))= + v2(((((h7-1)*N+h12-1)*N+p8-1)*N+p11))
END DO
END DO
END DO
END DO
DO h7=1,nocc
DO h12=1,nocc
IF (h7>h12) CYCLE
DO h1=1,nocc
DO p11=nocc+1,N
DO p8=nocc+1,N
i2(((((h7-1)*N+h12-1)*N+h1-1)*N+p11))=i2(((((h7-1)*N+h12-1)*N+h1-1)*N+p11))+(1.0d0)*t1(((p8-1)*N+h1))*v2e(((((h7-1)*N+h12-1)*N+p8-1&
&)*N+p11))
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p5=nocc+1,N
DO p11=nocc+1,N
IF (p4>p5) CYCLE
IF (p5>p11) CYCLE
DO h1=1,nocc
DO h2=1,nocc
DO h7=1,nocc
IF (h1>h2) CYCLE
IF (h2>h7) CYCLE
t3e(((((((p5-1)*N+p11-1)*N+p4-1)*N+h2-1)*N+h7-1)*N+h1))= + t3(((((((p4-1)*N+p5-1)*N+p11-1)*N+h1-1)*N+h2-1)*N+h7))
t3e(((((((p5-1)*N+p11-1)*N+p4-1)*N+h1-1)*N+h7-1)*N+h2))= - t3(((((((p4-1)*N+p5-1)*N+p11-1)*N+h1-1)*N+h2-1)*N+h7))
t3e(((((((p5-1)*N+p11-1)*N+p4-1)*N+h1-1)*N+h2-1)*N+h7))= + t3(((((((p4-1)*N+p5-1)*N+p11-1)*N+h1-1)*N+h2-1)*N+h7))
t3e(((((((p4-1)*N+p11-1)*N+p5-1)*N+h2-1)*N+h7-1)*N+h1))= - t3(((((((p4-1)*N+p5-1)*N+p11-1)*N+h1-1)*N+h2-1)*N+h7))
t3e(((((((p4-1)*N+p11-1)*N+p5-1)*N+h1-1)*N+h7-1)*N+h2))= + t3(((((((p4-1)*N+p5-1)*N+p11-1)*N+h1-1)*N+h2-1)*N+h7))
t3e(((((((p4-1)*N+p11-1)*N+p5-1)*N+h1-1)*N+h2-1)*N+h7))= - t3(((((((p4-1)*N+p5-1)*N+p11-1)*N+h1-1)*N+h2-1)*N+h7))
t3e(((((((p4-1)*N+p5-1)*N+p11-1)*N+h2-1)*N+h7-1)*N+h1))= + t3(((((((p4-1)*N+p5-1)*N+p11-1)*N+h1-1)*N+h2-1)*N+h7))
t3e(((((((p4-1)*N+p5-1)*N+p11-1)*N+h1-1)*N+h7-1)*N+h2))= - t3(((((((p4-1)*N+p5-1)*N+p11-1)*N+h1-1)*N+h2-1)*N+h7))
t3e(((((((p4-1)*N+p5-1)*N+p11-1)*N+h1-1)*N+h2-1)*N+h7))= + t3(((((((p4-1)*N+p5-1)*N+p11-1)*N+h1-1)*N+h2-1)*N+h7))
END DO
END DO
END DO
END DO
END DO
END DO
DO h7=1,nocc
DO h12=1,nocc
IF (h7>h12) CYCLE
DO h3=1,nocc
DO p11=nocc+1,N
i2e(((((h12-1)*N+h7-1)*N+h3-1)*N+p11))= - i2(((((h7-1)*N+h12-1)*N+h3-1)*N+p11))
i2e(((((h7-1)*N+h12-1)*N+h3-1)*N+p11))= + i2(((((h7-1)*N+h12-1)*N+h3-1)*N+p11))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p5=nocc+1,N
IF (p4>p5) CYCLE
DO h12=1,nocc
DO h1=1,nocc
DO h2=1,nocc
DO h3=1,nocc
IF (h1>h2) CYCLE
IF (h2>h3) CYCLE
DO p11=nocc+1,N
DO h7=1,nocc
i1(((((((h12-1)*N+p4-1)*N+p5-1)*N+h1-1)*N+h2-1)*N+h3))=i1(((((((h12-1)*N+p4-1)*N+p5-1)*N+h1-1)*N+h2-1)*N+h3))+(0.16666666666666666d&
&0)*t3e(((((((p4-1)*N+p5-1)*N+p11-1)*N+h1-1)*N+h2-1)*N+h7))*i2e(((((h7-1)*N+h12-1)*N+h3-1)*N+p11))+(0.16666666666666666d0)*t3e(((((&
&((p4-1)*N+p5-1)*N+p11-1)*N+h2-1)*N+h3-1)*N+h7))*i2e(((((h7-1)*N+h12-1)*N+h1-1)*N+p11))+(-0.16666666666666666d0)*t3e(((((((p4-1)*N+&
&p5-1)*N+p11-1)*N+h1-1)*N+h3-1)*N+h7))*i2e(((((h7-1)*N+h12-1)*N+h2-1)*N+p11))
END DO
END DO
END DO
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p7=nocc+1,N
DO p8=nocc+1,N
IF (p4>p7) CYCLE
IF (p7>p8) CYCLE
DO h1=1,nocc
DO h2=1,nocc
DO h3=1,nocc
IF (h1>h2) CYCLE
IF (h2>h3) CYCLE
t3e(((((((p7-1)*N+p4-1)*N+p8-1)*N+h1-1)*N+h2-1)*N+h3))= - t3(((((((p4-1)*N+p7-1)*N+p8-1)*N+h1-1)*N+h2-1)*N+h3))
t3e(((((((p8-1)*N+p4-1)*N+p7-1)*N+h1-1)*N+h2-1)*N+h3))= + t3(((((((p4-1)*N+p7-1)*N+p8-1)*N+h1-1)*N+h2-1)*N+h3))
t3e(((((((p4-1)*N+p7-1)*N+p8-1)*N+h1-1)*N+h2-1)*N+h3))= + t3(((((((p4-1)*N+p7-1)*N+p8-1)*N+h1-1)*N+h2-1)*N+h3))
END DO
END DO
END DO
END DO
END DO
END DO
DO p5=nocc+1,N
DO h12=1,nocc
DO p7=nocc+1,N
DO p8=nocc+1,N
IF (p7>p8) CYCLE
v2e(((((h12-1)*N+p5-1)*N+p7-1)*N+p8))= + v2(((((h12-1)*N+p5-1)*N+p7-1)*N+p8))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p5=nocc+1,N
IF (p4>p5) CYCLE
DO h12=1,nocc
DO h1=1,nocc
DO h2=1,nocc
DO h3=1,nocc
IF (h1>h2) CYCLE
IF (h2>h3) CYCLE
DO p7=nocc+1,N
DO p8=nocc+1,N
IF (p7>p8) CYCLE
i1(((((((h12-1)*N+p4-1)*N+p5-1)*N+h1-1)*N+h2-1)*N+h3))=i1(((((((h12-1)*N+p4-1)*N+p5-1)*N+h1-1)*N+h2-1)*N+h3))+(0.16666666666666666d&
&0)*t3e(((((((p4-1)*N+p7-1)*N+p8-1)*N+h1-1)*N+h2-1)*N+h3))*v2e(((((h12-1)*N+p5-1)*N+p7-1)*N+p8))+(-0.16666666666666666d0)*t3e((((((&
&(p5-1)*N+p7-1)*N+p8-1)*N+h1-1)*N+h2-1)*N+h3))*v2e(((((h12-1)*N+p4-1)*N+p7-1)*N+p8))
END DO
END DO
END DO
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p9=nocc+1,N
DO p10=nocc+1,N
IF (p4>p9) CYCLE
IF (p9>p10) CYCLE
DO h1=1,nocc
DO h2=1,nocc
DO h3=1,nocc
IF (h1>h2) CYCLE
IF (h2>h3) CYCLE
t3e(((((((p9-1)*N+p4-1)*N+p10-1)*N+h1-1)*N+h2-1)*N+h3))= - t3(((((((p4-1)*N+p9-1)*N+p10-1)*N+h1-1)*N+h2-1)*N+h3))
t3e(((((((p10-1)*N+p4-1)*N+p9-1)*N+h1-1)*N+h2-1)*N+h3))= + t3(((((((p4-1)*N+p9-1)*N+p10-1)*N+h1-1)*N+h2-1)*N+h3))
t3e(((((((p4-1)*N+p9-1)*N+p10-1)*N+h1-1)*N+h2-1)*N+h3))= + t3(((((((p4-1)*N+p9-1)*N+p10-1)*N+h1-1)*N+h2-1)*N+h3))
END DO
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h8=1,nocc
DO h12=1,nocc
IF (h8>h12) CYCLE
DO h1=1,nocc
DO h2=1,nocc
DO h3=1,nocc
IF (h1>h2) CYCLE
IF (h2>h3) CYCLE
i2(((((((h8-1)*N+h12-1)*N+p4-1)*N+h1-1)*N+h2-1)*N+h3))=0.0d0
DO p9=nocc+1,N
DO p10=nocc+1,N
IF (p9>p10) CYCLE
i2(((((((h8-1)*N+h12-1)*N+p4-1)*N+h1-1)*N+h2-1)*N+h3))=i2(((((((h8-1)*N+h12-1)*N+p4-1)*N+h1-1)*N+h2-1)*N+h3))+(2.0d0)*t3e(((((((p4-&
&1)*N+p9-1)*N+p10-1)*N+h1-1)*N+h2-1)*N+h3))*v2(((((h8-1)*N+h12-1)*N+p9-1)*N+p10))
END DO
END DO
END DO
END DO
END DO
END DO
END DO
END DO
DO p5=nocc+1,N
DO h8=1,nocc
DO h12=1,nocc
IF (h8>h12) CYCLE
DO h1=1,nocc
DO h2=1,nocc
DO h3=1,nocc
IF (h1>h2) CYCLE
IF (h2>h3) CYCLE
i2e(((((((h12-1)*N+h8-1)*N+p5-1)*N+h1-1)*N+h2-1)*N+h3))= - i2(((((((h8-1)*N+h12-1)*N+p5-1)*N+h1-1)*N+h2-1)*N+h3))
i2e(((((((h8-1)*N+h12-1)*N+p5-1)*N+h1-1)*N+h2-1)*N+h3))= + i2(((((((h8-1)*N+h12-1)*N+p5-1)*N+h1-1)*N+h2-1)*N+h3))
END DO
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p5=nocc+1,N
IF (p4>p5) CYCLE
DO h12=1,nocc
DO h1=1,nocc
DO h2=1,nocc
DO h3=1,nocc
IF (h1>h2) CYCLE
IF (h2>h3) CYCLE
DO h8=1,nocc
i1(((((((h12-1)*N+p4-1)*N+p5-1)*N+h1-1)*N+h2-1)*N+h3))=i1(((((((h12-1)*N+p4-1)*N+p5-1)*N+h1-1)*N+h2-1)*N+h3))+(-0.04166666666666666&
&4d0)*t1(((p4-1)*N+h8))*i2e(((((((h8-1)*N+h12-1)*N+p5-1)*N+h1-1)*N+h2-1)*N+h3))+(0.041666666666666664d0)*t1(((p5-1)*N+h8))*i2e(((((&
&((h8-1)*N+h12-1)*N+p4-1)*N+h1-1)*N+h2-1)*N+h3))
END DO
END DO
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p5=nocc+1,N
DO p6=nocc+1,N
IF (p4>p5) CYCLE
IF (p5>p6) CYCLE
DO h1=1,nocc
DO h2=1,nocc
DO h3=1,nocc
IF (h1>h2) CYCLE
IF (h2>h3) CYCLE
DO h12=1,nocc
i0(((((((p4-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h3))=i0(((((((p4-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h3))+(6.0d0)*t1(((p4-1)*N+h1&
&2))*i1(((((((h12-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h3))+(-6.0d0)*t1(((p5-1)*N+h12))*i1(((((((h12-1)*N+p4-1)*N+p6-1)*N+h1-1)*N+h&
&2-1)*N+h3))+(6.0d0)*t1(((p6-1)*N+h12))*i1(((((((h12-1)*N+p4-1)*N+p5-1)*N+h1-1)*N+h2-1)*N+h3))
END DO
END DO
END DO
END DO
END DO
END DO
END DO
DO h12=1,nocc
DO p11=nocc+1,N
i2(((h12-1)*N+p11))=(1.0d0)*f1(((h12-1)*N+p11))
END DO
END DO
DO h12=1,nocc
DO h10=1,nocc
IF (h10>h12) CYCLE
DO p11=nocc+1,N
DO p9=nocc+1,N
IF (p9>p11) CYCLE
v2e(((((h12-1)*N+h10-1)*N+p11-1)*N+p9))= + v2(((((h10-1)*N+h12-1)*N+p9-1)*N+p11))
v2e(((((h12-1)*N+h10-1)*N+p9-1)*N+p11))= - v2(((((h10-1)*N+h12-1)*N+p9-1)*N+p11))
v2e(((((h10-1)*N+h12-1)*N+p11-1)*N+p9))= - v2(((((h10-1)*N+h12-1)*N+p9-1)*N+p11))
v2e(((((h10-1)*N+h12-1)*N+p9-1)*N+p11))= + v2(((((h10-1)*N+h12-1)*N+p9-1)*N+p11))
END DO
END DO
END DO
END DO
DO h12=1,nocc
DO p11=nocc+1,N
DO p9=nocc+1,N
DO h10=1,nocc
i2(((h12-1)*N+p11))=i2(((h12-1)*N+p11))+(0.5d0)*t1(((p9-1)*N+h10))*v2e(((((h10-1)*N+h12-1)*N+p9-1)*N+p11))
END DO
END DO
END DO
END DO
i1(1)=0.0d0
DO p11=nocc+1,N
DO h12=1,nocc
i1(1)=i1(1)+(1.0d0)*t1(((p11-1)*N+h12))*i2(((h12-1)*N+p11))
END DO
END DO
DO p7=nocc+1,N
DO p8=nocc+1,N
IF (p7>p8) CYCLE
DO h9=1,nocc
DO h10=1,nocc
IF (h9>h10) CYCLE
i1(1)=i1(1)+(1.0d0)*t2(((((p7-1)*N+p8-1)*N+h9-1)*N+h10))*v2(((((h9-1)*N+h10-1)*N+p7-1)*N+p8))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p5=nocc+1,N
DO p6=nocc+1,N
IF (p4>p5) CYCLE
IF (p5>p6) CYCLE
DO h1=1,nocc
DO h2=1,nocc
DO h3=1,nocc
IF (h1>h2) CYCLE
IF (h2>h3) CYCLE
i0(((((((p4-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h3))=i0(((((((p4-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h3))+(1.0d0)*t3(((((((p4-1)*&
&N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h3))*i1(1)
END DO
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p7=nocc+1,N
DO p8=nocc+1,N
IF (p4>p7) CYCLE
IF (p7>p8) CYCLE
DO h1=1,nocc
DO h2=1,nocc
DO h3=1,nocc
IF (h1>h2) CYCLE
IF (h2>h3) CYCLE
t3e(((((((p7-1)*N+p4-1)*N+p8-1)*N+h1-1)*N+h2-1)*N+h3))= - t3(((((((p4-1)*N+p7-1)*N+p8-1)*N+h1-1)*N+h2-1)*N+h3))
t3e(((((((p8-1)*N+p4-1)*N+p7-1)*N+h1-1)*N+h2-1)*N+h3))= + t3(((((((p4-1)*N+p7-1)*N+p8-1)*N+h1-1)*N+h2-1)*N+h3))
t3e(((((((p4-1)*N+p7-1)*N+p8-1)*N+h1-1)*N+h2-1)*N+h3))= + t3(((((((p4-1)*N+p7-1)*N+p8-1)*N+h1-1)*N+h2-1)*N+h3))
END DO
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h9=1,nocc
DO h10=1,nocc
IF (h9>h10) CYCLE
DO h1=1,nocc
DO h2=1,nocc
DO h3=1,nocc
IF (h1>h2) CYCLE
IF (h2>h3) CYCLE
i1(((((((h9-1)*N+h10-1)*N+p4-1)*N+h1-1)*N+h2-1)*N+h3))=0.0d0
DO p7=nocc+1,N
DO p8=nocc+1,N
IF (p7>p8) CYCLE
i1(((((((h9-1)*N+h10-1)*N+p4-1)*N+h1-1)*N+h2-1)*N+h3))=i1(((((((h9-1)*N+h10-1)*N+p4-1)*N+h1-1)*N+h2-1)*N+h3))+(2.0d0)*t3e(((((((p4-&
&1)*N+p7-1)*N+p8-1)*N+h1-1)*N+h2-1)*N+h3))*v2(((((h9-1)*N+h10-1)*N+p7-1)*N+p8))
END DO
END DO
END DO
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p5=nocc+1,N
DO p6=nocc+1,N
IF (p4>p5) CYCLE
IF (p5>p6) CYCLE
DO h1=1,nocc
DO h2=1,nocc
DO h3=1,nocc
IF (h1>h2) CYCLE
IF (h2>h3) CYCLE
DO h9=1,nocc
DO h10=1,nocc
IF (h9>h10) CYCLE
i0(((((((p4-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h3))=i0(((((((p4-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h3))+(0.5d0)*t2(((((p4-1)*N+&
&p5-1)*N+h9-1)*N+h10))*i1(((((((h9-1)*N+h10-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h3))+(0.5d0)*t2(((((p5-1)*N+p6-1)*N+h9-1)*N+h10))*i1((((((&
&(h9-1)*N+h10-1)*N+p4-1)*N+h1-1)*N+h2-1)*N+h3))+(-0.5d0)*t2(((((p4-1)*N+p6-1)*N+h9-1)*N+h10))*i1(((((((h9-1)*N+h10-1)*N+p5-1)*N+h1-&
&1)*N+h2-1)*N+h3))
END DO
END DO
END DO
END DO
END DO
END DO
END DO
END DO
RETURN
END SUBROUTINE
