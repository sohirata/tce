SUBROUTINE p_m_ccsdt_t3_disconnected(N,f1,f1e,i0,i0e,i1,i1e,i2,i2e,i3,i3e,m,nocc,t1,t1e,t2,t2e,t3,t3e,v2,v2e)
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
INTEGER :: m(*)
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
i1(((((m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h2)))=(1.0d0)*v2(((((m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h2)))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h1=1,nocc
i2(((m(p4)-1)*N+m(h1)))=(1.0d0)*f1(((m(p4)-1)*N+m(h1)))
END DO
END DO
DO h11=1,nocc
DO h1=1,nocc
i3(((m(h11)-1)*N+m(h1)))=(1.0d0)*f1(((m(h11)-1)*N+m(h1)))
END DO
END DO
DO h11=1,nocc
DO h1=1,nocc
DO p7=nocc+1,N
i3(((m(h11)-1)*N+m(h1)))=i3(((m(h11)-1)*N+m(h1)))+(1.0d0)*t1(((m(p7)-1)*N+m(h1)))*f1(((m(h11)-1)*N+m(p7)))
END DO
END DO
END DO
DO h11=1,nocc
DO h9=1,nocc
IF (h9>h11) CYCLE
DO h1=1,nocc
DO p8=nocc+1,N
v2e(((((m(h11)-1)*N+m(h9)-1)*N+m(h1)-1)*N+m(p8)))= - v2(((((m(h9)-1)*N+m(h11)-1)*N+m(h1)-1)*N+m(p8)))
v2e(((((m(h9)-1)*N+m(h11)-1)*N+m(h1)-1)*N+m(p8)))= + v2(((((m(h9)-1)*N+m(h11)-1)*N+m(h1)-1)*N+m(p8)))
END DO
END DO
END DO
END DO
DO h11=1,nocc
DO h1=1,nocc
DO p8=nocc+1,N
DO h9=1,nocc
i3(((m(h11)-1)*N+m(h1)))=i3(((m(h11)-1)*N+m(h1)))+(-1.0d0)*t1(((m(p8)-1)*N+m(h9)))*v2e(((((m(h9)-1)*N+m(h11)-1)*N+m(h1)-1)*N+m(p8)))
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
t2e(((((m(p8)-1)*N+m(p9)-1)*N+m(h10)-1)*N+m(h1)))= - t2(((((m(p8)-1)*N+m(p9)-1)*N+m(h1)-1)*N+m(h10)))
t2e(((((m(p8)-1)*N+m(p9)-1)*N+m(h1)-1)*N+m(h10)))= + t2(((((m(p8)-1)*N+m(p9)-1)*N+m(h1)-1)*N+m(h10)))
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
v2e(((((m(h11)-1)*N+m(h10)-1)*N+m(p8)-1)*N+m(p9)))= - v2(((((m(h10)-1)*N+m(h11)-1)*N+m(p8)-1)*N+m(p9)))
v2e(((((m(h10)-1)*N+m(h11)-1)*N+m(p8)-1)*N+m(p9)))= + v2(((((m(h10)-1)*N+m(h11)-1)*N+m(p8)-1)*N+m(p9)))
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
i3(((m(h11)-1)*N+m(h1)))=i3(((m(h11)-1)*N+m(h1)))+(-1.0d0)*t2e(((((m(p8)-1)*N+m(p9)-1)*N+m(h1)-1)*N+m(h10)))*v2e(((((m(h10)-1)*N+m(&
&h11)-1)*N+m(p8)-1)*N+m(p9)))
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h1=1,nocc
DO h11=1,nocc
i2(((m(p4)-1)*N+m(h1)))=i2(((m(p4)-1)*N+m(h1)))+(-1.0d0)*t1(((m(p4)-1)*N+m(h11)))*i3(((m(h11)-1)*N+m(h1)))
END DO
END DO
END DO
DO p4=nocc+1,N
DO p7=nocc+1,N
i3(((m(p4)-1)*N+m(p7)))=(1.0d0)*f1(((m(p4)-1)*N+m(p7)))
END DO
END DO
DO p4=nocc+1,N
DO h9=1,nocc
DO p7=nocc+1,N
DO p8=nocc+1,N
IF (p7>p8) CYCLE
v2e(((((m(h9)-1)*N+m(p4)-1)*N+m(p8)-1)*N+m(p7)))= - v2(((((m(h9)-1)*N+m(p4)-1)*N+m(p7)-1)*N+m(p8)))
v2e(((((m(h9)-1)*N+m(p4)-1)*N+m(p7)-1)*N+m(p8)))= + v2(((((m(h9)-1)*N+m(p4)-1)*N+m(p7)-1)*N+m(p8)))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p7=nocc+1,N
DO p8=nocc+1,N
DO h9=1,nocc
i3(((m(p4)-1)*N+m(p7)))=i3(((m(p4)-1)*N+m(p7)))+(-1.0d0)*t1(((m(p8)-1)*N+m(h9)))*v2e(((((m(h9)-1)*N+m(p4)-1)*N+m(p7)-1)*N+m(p8)))
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
t2e(((((m(p8)-1)*N+m(p4)-1)*N+m(h9)-1)*N+m(h10)))= - t2(((((m(p4)-1)*N+m(p8)-1)*N+m(h9)-1)*N+m(h10)))
t2e(((((m(p4)-1)*N+m(p8)-1)*N+m(h9)-1)*N+m(h10)))= + t2(((((m(p4)-1)*N+m(p8)-1)*N+m(h9)-1)*N+m(h10)))
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
v2e(((((m(h9)-1)*N+m(h10)-1)*N+m(p8)-1)*N+m(p7)))= - v2(((((m(h9)-1)*N+m(h10)-1)*N+m(p7)-1)*N+m(p8)))
v2e(((((m(h9)-1)*N+m(h10)-1)*N+m(p7)-1)*N+m(p8)))= + v2(((((m(h9)-1)*N+m(h10)-1)*N+m(p7)-1)*N+m(p8)))
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
i3(((m(p4)-1)*N+m(p7)))=i3(((m(p4)-1)*N+m(p7)))+(-1.0d0)*t2e(((((m(p4)-1)*N+m(p8)-1)*N+m(h9)-1)*N+m(h10)))*v2e(((((m(h9)-1)*N+m(h10&
&)-1)*N+m(p7)-1)*N+m(p8)))
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h1=1,nocc
DO p7=nocc+1,N
i2(((m(p4)-1)*N+m(h1)))=i2(((m(p4)-1)*N+m(h1)))+(1.0d0)*t1(((m(p7)-1)*N+m(h1)))*i3(((m(p4)-1)*N+m(p7)))
END DO
END DO
END DO
DO p4=nocc+1,N
DO h8=1,nocc
DO h1=1,nocc
DO p7=nocc+1,N
v2e(((((m(h8)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(p7)))= + v2(((((m(h8)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(p7)))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h1=1,nocc
DO p7=nocc+1,N
DO h8=1,nocc
i2(((m(p4)-1)*N+m(h1)))=i2(((m(p4)-1)*N+m(h1)))+(-1.0d0)*t1(((m(p7)-1)*N+m(h8)))*v2e(((((m(h8)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(p7)))
END DO
END DO
END DO
END DO
DO h9=1,nocc
DO p11=nocc+1,N
i3(((m(h9)-1)*N+m(p11)))=(1.0d0)*f1(((m(h9)-1)*N+m(p11)))
END DO
END DO
DO h9=1,nocc
DO h8=1,nocc
IF (h8>h9) CYCLE
DO p11=nocc+1,N
DO p7=nocc+1,N
IF (p7>p11) CYCLE
v2e(((((m(h9)-1)*N+m(h8)-1)*N+m(p11)-1)*N+m(p7)))= + v2(((((m(h8)-1)*N+m(h9)-1)*N+m(p7)-1)*N+m(p11)))
v2e(((((m(h9)-1)*N+m(h8)-1)*N+m(p7)-1)*N+m(p11)))= - v2(((((m(h8)-1)*N+m(h9)-1)*N+m(p7)-1)*N+m(p11)))
v2e(((((m(h8)-1)*N+m(h9)-1)*N+m(p11)-1)*N+m(p7)))= - v2(((((m(h8)-1)*N+m(h9)-1)*N+m(p7)-1)*N+m(p11)))
v2e(((((m(h8)-1)*N+m(h9)-1)*N+m(p7)-1)*N+m(p11)))= + v2(((((m(h8)-1)*N+m(h9)-1)*N+m(p7)-1)*N+m(p11)))
END DO
END DO
END DO
END DO
DO h9=1,nocc
DO p11=nocc+1,N
DO p7=nocc+1,N
DO h8=1,nocc
i3(((m(h9)-1)*N+m(p11)))=i3(((m(h9)-1)*N+m(p11)))+(1.0d0)*t1(((m(p7)-1)*N+m(h8)))*v2e(((((m(h8)-1)*N+m(h9)-1)*N+m(p7)-1)*N+m(p11)))
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
t2e(((((m(p11)-1)*N+m(p4)-1)*N+m(h9)-1)*N+m(h1)))= + t2(((((m(p4)-1)*N+m(p11)-1)*N+m(h1)-1)*N+m(h9)))
t2e(((((m(p11)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h9)))= - t2(((((m(p4)-1)*N+m(p11)-1)*N+m(h1)-1)*N+m(h9)))
t2e(((((m(p4)-1)*N+m(p11)-1)*N+m(h9)-1)*N+m(h1)))= - t2(((((m(p4)-1)*N+m(p11)-1)*N+m(h1)-1)*N+m(h9)))
t2e(((((m(p4)-1)*N+m(p11)-1)*N+m(h1)-1)*N+m(h9)))= + t2(((((m(p4)-1)*N+m(p11)-1)*N+m(h1)-1)*N+m(h9)))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h1=1,nocc
DO p11=nocc+1,N
DO h9=1,nocc
i2(((m(p4)-1)*N+m(h1)))=i2(((m(p4)-1)*N+m(h1)))+(1.0d0)*t2e(((((m(p4)-1)*N+m(p11)-1)*N+m(h1)-1)*N+m(h9)))*i3(((m(h9)-1)*N+m(p11)))
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
t2e(((((m(p7)-1)*N+m(p4)-1)*N+m(h8)-1)*N+m(h9)))= - t2(((((m(p4)-1)*N+m(p7)-1)*N+m(h8)-1)*N+m(h9)))
t2e(((((m(p4)-1)*N+m(p7)-1)*N+m(h8)-1)*N+m(h9)))= + t2(((((m(p4)-1)*N+m(p7)-1)*N+m(h8)-1)*N+m(h9)))
END DO
END DO
END DO
END DO
DO h8=1,nocc
DO h9=1,nocc
IF (h8>h9) CYCLE
DO h1=1,nocc
DO p7=nocc+1,N
v2e(((((m(h8)-1)*N+m(h9)-1)*N+m(h1)-1)*N+m(p7)))= + v2(((((m(h8)-1)*N+m(h9)-1)*N+m(h1)-1)*N+m(p7)))
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
i2(((m(p4)-1)*N+m(h1)))=i2(((m(p4)-1)*N+m(h1)))+(-1.0d0)*t2e(((((m(p4)-1)*N+m(p7)-1)*N+m(h8)-1)*N+m(h9)))*v2e(((((m(h8)-1)*N+m(h9)-&
&1)*N+m(h1)-1)*N+m(p7)))
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
t2e(((((m(p7)-1)*N+m(p8)-1)*N+m(h9)-1)*N+m(h1)))= - t2(((((m(p7)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(h9)))
t2e(((((m(p7)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(h9)))= + t2(((((m(p7)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(h9)))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h9=1,nocc
DO p7=nocc+1,N
DO p8=nocc+1,N
IF (p7>p8) CYCLE
v2e(((((m(h9)-1)*N+m(p4)-1)*N+m(p7)-1)*N+m(p8)))= + v2(((((m(h9)-1)*N+m(p4)-1)*N+m(p7)-1)*N+m(p8)))
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
i2(((m(p4)-1)*N+m(h1)))=i2(((m(p4)-1)*N+m(h1)))+(-1.0d0)*t2e(((((m(p7)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(h9)))*v2e(((((m(h9)-1)*N+m(p4)-&
&1)*N+m(p7)-1)*N+m(p8)))
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
t3e(((((((m(p7)-1)*N+m(p4)-1)*N+m(p8)-1)*N+m(h9)-1)*N+m(h1)-1)*N+m(h10)))= + t3(((((((m(p4)-1)*N+m(p7)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m&
&(h9)-1)*N+m(h10)))
t3e(((((((m(p7)-1)*N+m(p4)-1)*N+m(p8)-1)*N+m(h10)-1)*N+m(h1)-1)*N+m(h9)))= - t3(((((((m(p4)-1)*N+m(p7)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m&
&(h9)-1)*N+m(h10)))
t3e(((((((m(p7)-1)*N+m(p4)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(h9)-1)*N+m(h10)))= - t3(((((((m(p4)-1)*N+m(p7)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m&
&(h9)-1)*N+m(h10)))
t3e(((((((m(p8)-1)*N+m(p4)-1)*N+m(p7)-1)*N+m(h9)-1)*N+m(h1)-1)*N+m(h10)))= - t3(((((((m(p4)-1)*N+m(p7)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m&
&(h9)-1)*N+m(h10)))
t3e(((((((m(p8)-1)*N+m(p4)-1)*N+m(p7)-1)*N+m(h10)-1)*N+m(h1)-1)*N+m(h9)))= + t3(((((((m(p4)-1)*N+m(p7)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m&
&(h9)-1)*N+m(h10)))
t3e(((((((m(p8)-1)*N+m(p4)-1)*N+m(p7)-1)*N+m(h1)-1)*N+m(h9)-1)*N+m(h10)))= + t3(((((((m(p4)-1)*N+m(p7)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m&
&(h9)-1)*N+m(h10)))
t3e(((((((m(p4)-1)*N+m(p7)-1)*N+m(p8)-1)*N+m(h9)-1)*N+m(h1)-1)*N+m(h10)))= - t3(((((((m(p4)-1)*N+m(p7)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m&
&(h9)-1)*N+m(h10)))
t3e(((((((m(p4)-1)*N+m(p7)-1)*N+m(p8)-1)*N+m(h10)-1)*N+m(h1)-1)*N+m(h9)))= + t3(((((((m(p4)-1)*N+m(p7)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m&
&(h9)-1)*N+m(h10)))
t3e(((((((m(p4)-1)*N+m(p7)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(h9)-1)*N+m(h10)))= + t3(((((((m(p4)-1)*N+m(p7)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m&
&(h9)-1)*N+m(h10)))
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
i2(((m(p4)-1)*N+m(h1)))=i2(((m(p4)-1)*N+m(h1)))+(1.0d0)*t3e(((((((m(p4)-1)*N+m(p7)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(h9)-1)*N+m(h10)))*v&
&2(((((m(h9)-1)*N+m(h10)-1)*N+m(p7)-1)*N+m(p8)))
END DO
END DO
END DO
END DO
END DO
END DO
i3(1)=0.0d0
DO p7=nocc+1,N
DO h8=1,nocc
i3(1)=i3(1)+(1.0d0)*t1(((m(p7)-1)*N+m(h8)))*f1(((m(h8)-1)*N+m(p7)))
END DO
END DO
DO p7=nocc+1,N
DO p8=nocc+1,N
IF (p7>p8) CYCLE
DO h9=1,nocc
DO h10=1,nocc
IF (h9>h10) CYCLE
i3(1)=i3(1)+(1.0d0)*t2(((((m(p7)-1)*N+m(p8)-1)*N+m(h9)-1)*N+m(h10)))*v2(((((m(h9)-1)*N+m(h10)-1)*N+m(p7)-1)*N+m(p8)))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h1=1,nocc
i2(((m(p4)-1)*N+m(h1)))=i2(((m(p4)-1)*N+m(h1)))+(0.3333333333333333d0)*t1(((m(p4)-1)*N+m(h1)))*i3(1)
END DO
END DO
DO p4=nocc+1,N
DO p5=nocc+1,N
IF (p4>p5) CYCLE
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
i1(((((m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h2)))=i1(((((m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h2)))+(0.5d0)*t1(((m(p4)-1)*N+m(h1)))*i2(&
&((m(p5)-1)*N+m(h2)))+(-0.5d0)*t1(((m(p4)-1)*N+m(h2)))*i2(((m(p5)-1)*N+m(h1)))+(-0.5d0)*t1(((m(p5)-1)*N+m(h1)))*i2(((m(p4)-1)*N+m(h&
&2)))+(0.5d0)*t1(((m(p5)-1)*N+m(h2)))*i2(((m(p4)-1)*N+m(h1)))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h7=1,nocc
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
v2e(((((m(h7)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))= + v2(((((m(h7)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h7=1,nocc
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
i2(((((m(h7)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))=(1.0d0)*v2e(((((m(h7)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))
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
i3(((((m(h7)-1)*N+m(h8)-1)*N+m(h1)-1)*N+m(h2)))=(1.0d0)*v2(((((m(h7)-1)*N+m(h8)-1)*N+m(h1)-1)*N+m(h2)))
END DO
END DO
END DO
END DO
DO h7=1,nocc
DO h8=1,nocc
IF (h7>h8) CYCLE
DO h2=1,nocc
DO p9=nocc+1,N
v2e(((((m(h7)-1)*N+m(h8)-1)*N+m(h2)-1)*N+m(p9)))= + v2(((((m(h7)-1)*N+m(h8)-1)*N+m(h2)-1)*N+m(p9)))
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
i3(((((m(h7)-1)*N+m(h8)-1)*N+m(h1)-1)*N+m(h2)))=i3(((((m(h7)-1)*N+m(h8)-1)*N+m(h1)-1)*N+m(h2)))+(-1.0d0)*t1(((m(p9)-1)*N+m(h1)))*v2&
&e(((((m(h7)-1)*N+m(h8)-1)*N+m(h2)-1)*N+m(p9)))+(1.0d0)*t1(((m(p9)-1)*N+m(h2)))*v2e(((((m(h7)-1)*N+m(h8)-1)*N+m(h1)-1)*N+m(p9)))
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
i3(((((m(h7)-1)*N+m(h8)-1)*N+m(h1)-1)*N+m(h2)))=i3(((((m(h7)-1)*N+m(h8)-1)*N+m(h1)-1)*N+m(h2)))+(1.0d0)*t2(((((m(p9)-1)*N+m(p10)-1)&
&*N+m(h1)-1)*N+m(h2)))*v2(((((m(h7)-1)*N+m(h8)-1)*N+m(p9)-1)*N+m(p10)))
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
i3e(((((m(h8)-1)*N+m(h7)-1)*N+m(h1)-1)*N+m(h2)))= - i3(((((m(h7)-1)*N+m(h8)-1)*N+m(h1)-1)*N+m(h2)))
i3e(((((m(h7)-1)*N+m(h8)-1)*N+m(h1)-1)*N+m(h2)))= + i3(((((m(h7)-1)*N+m(h8)-1)*N+m(h1)-1)*N+m(h2)))
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
i2(((((m(h7)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))=i2(((((m(h7)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))+(-0.5d0)*t1(((m(p4)-1)*N+m(h8)))*i3&
&e(((((m(h7)-1)*N+m(h8)-1)*N+m(h1)-1)*N+m(h2)))
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h7=1,nocc
DO h1=1,nocc
DO p8=nocc+1,N
v2e(((((m(h7)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(p8)))= + v2(((((m(h7)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(p8)))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h7=1,nocc
DO h1=1,nocc
DO p8=nocc+1,N
i3(((((m(h7)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(p8)))=(1.0d0)*v2e(((((m(h7)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(p8)))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h7=1,nocc
DO p8=nocc+1,N
DO p9=nocc+1,N
IF (p8>p9) CYCLE
v2e(((((m(h7)-1)*N+m(p4)-1)*N+m(p9)-1)*N+m(p8)))= - v2(((((m(h7)-1)*N+m(p4)-1)*N+m(p8)-1)*N+m(p9)))
v2e(((((m(h7)-1)*N+m(p4)-1)*N+m(p8)-1)*N+m(p9)))= + v2(((((m(h7)-1)*N+m(p4)-1)*N+m(p8)-1)*N+m(p9)))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h7=1,nocc
DO h1=1,nocc
DO p8=nocc+1,N
DO p9=nocc+1,N
i3(((((m(h7)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(p8)))=i3(((((m(h7)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(p8)))+(-0.5d0)*t1(((m(p9)-1)*N+m(h1)))*v2&
&e(((((m(h7)-1)*N+m(p4)-1)*N+m(p8)-1)*N+m(p9)))
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
i2(((((m(h7)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))=i2(((((m(h7)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))+(-1.0d0)*t1(((m(p8)-1)*N+m(h1)))*i3&
&(((((m(h7)-1)*N+m(p4)-1)*N+m(h2)-1)*N+m(p8)))+(1.0d0)*t1(((m(p8)-1)*N+m(h2)))*i3(((((m(h7)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(p8)))
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
v2e(((((m(h7)-1)*N+m(h12)-1)*N+m(h1)-1)*N+m(p11)))= + v2(((((m(h7)-1)*N+m(h12)-1)*N+m(h1)-1)*N+m(p11)))
END DO
END DO
END DO
END DO
DO h7=1,nocc
DO h12=1,nocc
IF (h7>h12) CYCLE
DO h1=1,nocc
DO p11=nocc+1,N
i3(((((m(h7)-1)*N+m(h12)-1)*N+m(h1)-1)*N+m(p11)))=(1.0d0)*v2e(((((m(h7)-1)*N+m(h12)-1)*N+m(h1)-1)*N+m(p11)))
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
v2e(((((m(h7)-1)*N+m(h12)-1)*N+m(p11)-1)*N+m(p8)))= - v2(((((m(h7)-1)*N+m(h12)-1)*N+m(p8)-1)*N+m(p11)))
v2e(((((m(h7)-1)*N+m(h12)-1)*N+m(p8)-1)*N+m(p11)))= + v2(((((m(h7)-1)*N+m(h12)-1)*N+m(p8)-1)*N+m(p11)))
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
i3(((((m(h7)-1)*N+m(h12)-1)*N+m(h1)-1)*N+m(p11)))=i3(((((m(h7)-1)*N+m(h12)-1)*N+m(h1)-1)*N+m(p11)))+(1.0d0)*t1(((m(p8)-1)*N+m(h1)))&
&*v2e(((((m(h7)-1)*N+m(h12)-1)*N+m(p8)-1)*N+m(p11)))
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
t2e(((((m(p11)-1)*N+m(p4)-1)*N+m(h12)-1)*N+m(h1)))= + t2(((((m(p4)-1)*N+m(p11)-1)*N+m(h1)-1)*N+m(h12)))
t2e(((((m(p11)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h12)))= - t2(((((m(p4)-1)*N+m(p11)-1)*N+m(h1)-1)*N+m(h12)))
t2e(((((m(p4)-1)*N+m(p11)-1)*N+m(h12)-1)*N+m(h1)))= - t2(((((m(p4)-1)*N+m(p11)-1)*N+m(h1)-1)*N+m(h12)))
t2e(((((m(p4)-1)*N+m(p11)-1)*N+m(h1)-1)*N+m(h12)))= + t2(((((m(p4)-1)*N+m(p11)-1)*N+m(h1)-1)*N+m(h12)))
END DO
END DO
END DO
END DO
DO h7=1,nocc
DO h12=1,nocc
IF (h7>h12) CYCLE
DO h2=1,nocc
DO p11=nocc+1,N
i3e(((((m(h12)-1)*N+m(h7)-1)*N+m(h2)-1)*N+m(p11)))= - i3(((((m(h7)-1)*N+m(h12)-1)*N+m(h2)-1)*N+m(p11)))
i3e(((((m(h7)-1)*N+m(h12)-1)*N+m(h2)-1)*N+m(p11)))= + i3(((((m(h7)-1)*N+m(h12)-1)*N+m(h2)-1)*N+m(p11)))
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
i2(((((m(h7)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))=i2(((((m(h7)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))+(-1.0d0)*t2e(((((m(p4)-1)*N+m(p11)-&
&1)*N+m(h1)-1)*N+m(h12)))*i3e(((((m(h7)-1)*N+m(h12)-1)*N+m(h2)-1)*N+m(p11)))+(1.0d0)*t2e(((((m(p4)-1)*N+m(p11)-1)*N+m(h2)-1)*N+m(h1&
&2)))*i3e(((((m(h7)-1)*N+m(h12)-1)*N+m(h1)-1)*N+m(p11)))
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
v2e(((((m(h7)-1)*N+m(p4)-1)*N+m(p8)-1)*N+m(p9)))= + v2(((((m(h7)-1)*N+m(p4)-1)*N+m(p8)-1)*N+m(p9)))
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
i2(((((m(h7)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))=i2(((((m(h7)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))+(1.0d0)*t2(((((m(p8)-1)*N+m(p9)-1)*&
&N+m(h1)-1)*N+m(h2)))*v2e(((((m(h7)-1)*N+m(p4)-1)*N+m(p8)-1)*N+m(p9)))
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
t3e(((((((m(p8)-1)*N+m(p4)-1)*N+m(p9)-1)*N+m(h2)-1)*N+m(h10)-1)*N+m(h1)))= - t3(((((((m(p4)-1)*N+m(p8)-1)*N+m(p9)-1)*N+m(h1)-1)*N+m&
&(h2)-1)*N+m(h10)))
t3e(((((((m(p8)-1)*N+m(p4)-1)*N+m(p9)-1)*N+m(h1)-1)*N+m(h10)-1)*N+m(h2)))= + t3(((((((m(p4)-1)*N+m(p8)-1)*N+m(p9)-1)*N+m(h1)-1)*N+m&
&(h2)-1)*N+m(h10)))
t3e(((((((m(p8)-1)*N+m(p4)-1)*N+m(p9)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h10)))= - t3(((((((m(p4)-1)*N+m(p8)-1)*N+m(p9)-1)*N+m(h1)-1)*N+m&
&(h2)-1)*N+m(h10)))
t3e(((((((m(p9)-1)*N+m(p4)-1)*N+m(p8)-1)*N+m(h2)-1)*N+m(h10)-1)*N+m(h1)))= + t3(((((((m(p4)-1)*N+m(p8)-1)*N+m(p9)-1)*N+m(h1)-1)*N+m&
&(h2)-1)*N+m(h10)))
t3e(((((((m(p9)-1)*N+m(p4)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(h10)-1)*N+m(h2)))= - t3(((((((m(p4)-1)*N+m(p8)-1)*N+m(p9)-1)*N+m(h1)-1)*N+m&
&(h2)-1)*N+m(h10)))
t3e(((((((m(p9)-1)*N+m(p4)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h10)))= + t3(((((((m(p4)-1)*N+m(p8)-1)*N+m(p9)-1)*N+m(h1)-1)*N+m&
&(h2)-1)*N+m(h10)))
t3e(((((((m(p4)-1)*N+m(p8)-1)*N+m(p9)-1)*N+m(h2)-1)*N+m(h10)-1)*N+m(h1)))= + t3(((((((m(p4)-1)*N+m(p8)-1)*N+m(p9)-1)*N+m(h1)-1)*N+m&
&(h2)-1)*N+m(h10)))
t3e(((((((m(p4)-1)*N+m(p8)-1)*N+m(p9)-1)*N+m(h1)-1)*N+m(h10)-1)*N+m(h2)))= - t3(((((((m(p4)-1)*N+m(p8)-1)*N+m(p9)-1)*N+m(h1)-1)*N+m&
&(h2)-1)*N+m(h10)))
t3e(((((((m(p4)-1)*N+m(p8)-1)*N+m(p9)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h10)))= + t3(((((((m(p4)-1)*N+m(p8)-1)*N+m(p9)-1)*N+m(h1)-1)*N+m&
&(h2)-1)*N+m(h10)))
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
v2e(((((m(h10)-1)*N+m(h7)-1)*N+m(p8)-1)*N+m(p9)))= - v2(((((m(h7)-1)*N+m(h10)-1)*N+m(p8)-1)*N+m(p9)))
v2e(((((m(h7)-1)*N+m(h10)-1)*N+m(p8)-1)*N+m(p9)))= + v2(((((m(h7)-1)*N+m(h10)-1)*N+m(p8)-1)*N+m(p9)))
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
i2(((((m(h7)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))=i2(((((m(h7)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))+(-1.0d0)*t3e(((((((m(p4)-1)*N+m(p8)&
&-1)*N+m(p9)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h10)))*v2e(((((m(h7)-1)*N+m(h10)-1)*N+m(p8)-1)*N+m(p9)))
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
i1(((((m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h2)))=i1(((((m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h2)))+(-1.0d0)*t1(((m(p4)-1)*N+m(h7)))*i2&
&(((((m(h7)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h2)))+(1.0d0)*t1(((m(p5)-1)*N+m(h7)))*i2(((((m(h7)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))
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
v2e(((((m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(p7)))= + v2(((((m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(p7)))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p5=nocc+1,N
IF (p4>p5) CYCLE
DO h1=1,nocc
DO p7=nocc+1,N
i2(((((m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(p7)))=(1.0d0)*v2e(((((m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(p7)))
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
v2e(((((m(p4)-1)*N+m(p5)-1)*N+m(p8)-1)*N+m(p7)))= - v2(((((m(p4)-1)*N+m(p5)-1)*N+m(p7)-1)*N+m(p8)))
v2e(((((m(p4)-1)*N+m(p5)-1)*N+m(p7)-1)*N+m(p8)))= + v2(((((m(p4)-1)*N+m(p5)-1)*N+m(p7)-1)*N+m(p8)))
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
i2(((((m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(p7)))=i2(((((m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(p7)))+(-0.5d0)*t1(((m(p8)-1)*N+m(h1)))*v2&
&e(((((m(p4)-1)*N+m(p5)-1)*N+m(p7)-1)*N+m(p8)))
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
i1(((((m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h2)))=i1(((((m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h2)))+(-1.0d0)*t1(((m(p7)-1)*N+m(h1)))*i2&
&(((((m(p4)-1)*N+m(p5)-1)*N+m(h2)-1)*N+m(p7)))+(1.0d0)*t1(((m(p7)-1)*N+m(h2)))*i2(((((m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(p7)))
END DO
END DO
END DO
END DO
END DO
DO h12=1,nocc
DO h1=1,nocc
i2(((m(h12)-1)*N+m(h1)))=(1.0d0)*f1(((m(h12)-1)*N+m(h1)))
END DO
END DO
DO h12=1,nocc
DO p7=nocc+1,N
i3(((m(h12)-1)*N+m(p7)))=(1.0d0)*f1(((m(h12)-1)*N+m(p7)))
END DO
END DO
DO h12=1,nocc
DO h9=1,nocc
IF (h9>h12) CYCLE
DO p7=nocc+1,N
DO p8=nocc+1,N
IF (p7>p8) CYCLE
v2e(((((m(h12)-1)*N+m(h9)-1)*N+m(p8)-1)*N+m(p7)))= + v2(((((m(h9)-1)*N+m(h12)-1)*N+m(p7)-1)*N+m(p8)))
v2e(((((m(h12)-1)*N+m(h9)-1)*N+m(p7)-1)*N+m(p8)))= - v2(((((m(h9)-1)*N+m(h12)-1)*N+m(p7)-1)*N+m(p8)))
v2e(((((m(h9)-1)*N+m(h12)-1)*N+m(p8)-1)*N+m(p7)))= - v2(((((m(h9)-1)*N+m(h12)-1)*N+m(p7)-1)*N+m(p8)))
v2e(((((m(h9)-1)*N+m(h12)-1)*N+m(p7)-1)*N+m(p8)))= + v2(((((m(h9)-1)*N+m(h12)-1)*N+m(p7)-1)*N+m(p8)))
END DO
END DO
END DO
END DO
DO h12=1,nocc
DO p7=nocc+1,N
DO p8=nocc+1,N
DO h9=1,nocc
i3(((m(h12)-1)*N+m(p7)))=i3(((m(h12)-1)*N+m(p7)))+(-1.0d0)*t1(((m(p8)-1)*N+m(h9)))*v2e(((((m(h9)-1)*N+m(h12)-1)*N+m(p7)-1)*N+m(p8)))
END DO
END DO
END DO
END DO
DO h12=1,nocc
DO h1=1,nocc
DO p7=nocc+1,N
i2(((m(h12)-1)*N+m(h1)))=i2(((m(h12)-1)*N+m(h1)))+(1.0d0)*t1(((m(p7)-1)*N+m(h1)))*i3(((m(h12)-1)*N+m(p7)))
END DO
END DO
END DO
DO h12=1,nocc
DO h8=1,nocc
IF (h8>h12) CYCLE
DO h1=1,nocc
DO p7=nocc+1,N
v2e(((((m(h12)-1)*N+m(h8)-1)*N+m(h1)-1)*N+m(p7)))= - v2(((((m(h8)-1)*N+m(h12)-1)*N+m(h1)-1)*N+m(p7)))
v2e(((((m(h8)-1)*N+m(h12)-1)*N+m(h1)-1)*N+m(p7)))= + v2(((((m(h8)-1)*N+m(h12)-1)*N+m(h1)-1)*N+m(p7)))
END DO
END DO
END DO
END DO
DO h12=1,nocc
DO h1=1,nocc
DO p7=nocc+1,N
DO h8=1,nocc
i2(((m(h12)-1)*N+m(h1)))=i2(((m(h12)-1)*N+m(h1)))+(-1.0d0)*t1(((m(p7)-1)*N+m(h8)))*v2e(((((m(h8)-1)*N+m(h12)-1)*N+m(h1)-1)*N+m(p7)))
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
t2e(((((m(p8)-1)*N+m(p9)-1)*N+m(h10)-1)*N+m(h1)))= - t2(((((m(p8)-1)*N+m(p9)-1)*N+m(h1)-1)*N+m(h10)))
t2e(((((m(p8)-1)*N+m(p9)-1)*N+m(h1)-1)*N+m(h10)))= + t2(((((m(p8)-1)*N+m(p9)-1)*N+m(h1)-1)*N+m(h10)))
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
v2e(((((m(h12)-1)*N+m(h10)-1)*N+m(p8)-1)*N+m(p9)))= - v2(((((m(h10)-1)*N+m(h12)-1)*N+m(p8)-1)*N+m(p9)))
v2e(((((m(h10)-1)*N+m(h12)-1)*N+m(p8)-1)*N+m(p9)))= + v2(((((m(h10)-1)*N+m(h12)-1)*N+m(p8)-1)*N+m(p9)))
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
i2(((m(h12)-1)*N+m(h1)))=i2(((m(h12)-1)*N+m(h1)))+(-1.0d0)*t2e(((((m(p8)-1)*N+m(p9)-1)*N+m(h1)-1)*N+m(h10)))*v2e(((((m(h10)-1)*N+m(&
&h12)-1)*N+m(p8)-1)*N+m(p9)))
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
t2e(((((m(p4)-1)*N+m(p5)-1)*N+m(h12)-1)*N+m(h1)))= - t2(((((m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h12)))
t2e(((((m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h12)))= + t2(((((m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h12)))
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
i1(((((m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h2)))=i1(((((m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h2)))+(-1.0d0)*t2e(((((m(p4)-1)*N+m(p5)-1&
&)*N+m(h1)-1)*N+m(h12)))*i2(((m(h12)-1)*N+m(h2)))+(1.0d0)*t2e(((((m(p4)-1)*N+m(p5)-1)*N+m(h2)-1)*N+m(h12)))*i2(((m(h12)-1)*N+m(h1)))
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p12=nocc+1,N
i2(((m(p4)-1)*N+m(p12)))=(1.0d0)*f1(((m(p4)-1)*N+m(p12)))
END DO
END DO
DO h7=1,nocc
DO p12=nocc+1,N
i3(((m(h7)-1)*N+m(p12)))=(1.0d0)*f1(((m(h7)-1)*N+m(p12)))
END DO
END DO
DO h7=1,nocc
DO h9=1,nocc
IF (h7>h9) CYCLE
DO p12=nocc+1,N
DO p8=nocc+1,N
IF (p8>p12) CYCLE
v2e(((((m(h9)-1)*N+m(h7)-1)*N+m(p12)-1)*N+m(p8)))= + v2(((((m(h7)-1)*N+m(h9)-1)*N+m(p8)-1)*N+m(p12)))
v2e(((((m(h9)-1)*N+m(h7)-1)*N+m(p8)-1)*N+m(p12)))= - v2(((((m(h7)-1)*N+m(h9)-1)*N+m(p8)-1)*N+m(p12)))
v2e(((((m(h7)-1)*N+m(h9)-1)*N+m(p12)-1)*N+m(p8)))= - v2(((((m(h7)-1)*N+m(h9)-1)*N+m(p8)-1)*N+m(p12)))
v2e(((((m(h7)-1)*N+m(h9)-1)*N+m(p8)-1)*N+m(p12)))= + v2(((((m(h7)-1)*N+m(h9)-1)*N+m(p8)-1)*N+m(p12)))
END DO
END DO
END DO
END DO
DO h7=1,nocc
DO p12=nocc+1,N
DO p8=nocc+1,N
DO h9=1,nocc
i3(((m(h7)-1)*N+m(p12)))=i3(((m(h7)-1)*N+m(p12)))+(-1.0d0)*t1(((m(p8)-1)*N+m(h9)))*v2e(((((m(h7)-1)*N+m(h9)-1)*N+m(p8)-1)*N+m(p12)))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p12=nocc+1,N
DO h7=1,nocc
i2(((m(p4)-1)*N+m(p12)))=i2(((m(p4)-1)*N+m(p12)))+(-1.0d0)*t1(((m(p4)-1)*N+m(h7)))*i3(((m(h7)-1)*N+m(p12)))
END DO
END DO
END DO
DO p4=nocc+1,N
DO h8=1,nocc
DO p12=nocc+1,N
DO p7=nocc+1,N
IF (p7>p12) CYCLE
v2e(((((m(h8)-1)*N+m(p4)-1)*N+m(p12)-1)*N+m(p7)))= - v2(((((m(h8)-1)*N+m(p4)-1)*N+m(p7)-1)*N+m(p12)))
v2e(((((m(h8)-1)*N+m(p4)-1)*N+m(p7)-1)*N+m(p12)))= + v2(((((m(h8)-1)*N+m(p4)-1)*N+m(p7)-1)*N+m(p12)))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p12=nocc+1,N
DO p7=nocc+1,N
DO h8=1,nocc
i2(((m(p4)-1)*N+m(p12)))=i2(((m(p4)-1)*N+m(p12)))+(1.0d0)*t1(((m(p7)-1)*N+m(h8)))*v2e(((((m(h8)-1)*N+m(p4)-1)*N+m(p7)-1)*N+m(p12)))
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
t2e(((((m(p8)-1)*N+m(p4)-1)*N+m(h9)-1)*N+m(h10)))= - t2(((((m(p4)-1)*N+m(p8)-1)*N+m(h9)-1)*N+m(h10)))
t2e(((((m(p4)-1)*N+m(p8)-1)*N+m(h9)-1)*N+m(h10)))= + t2(((((m(p4)-1)*N+m(p8)-1)*N+m(h9)-1)*N+m(h10)))
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
v2e(((((m(h9)-1)*N+m(h10)-1)*N+m(p12)-1)*N+m(p8)))= - v2(((((m(h9)-1)*N+m(h10)-1)*N+m(p8)-1)*N+m(p12)))
v2e(((((m(h9)-1)*N+m(h10)-1)*N+m(p8)-1)*N+m(p12)))= + v2(((((m(h9)-1)*N+m(h10)-1)*N+m(p8)-1)*N+m(p12)))
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
i2(((m(p4)-1)*N+m(p12)))=i2(((m(p4)-1)*N+m(p12)))+(1.0d0)*t2e(((((m(p4)-1)*N+m(p8)-1)*N+m(h9)-1)*N+m(h10)))*v2e(((((m(h9)-1)*N+m(h1&
&0)-1)*N+m(p8)-1)*N+m(p12)))
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
t2e(((((m(p12)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))= - t2(((((m(p4)-1)*N+m(p12)-1)*N+m(h1)-1)*N+m(h2)))
t2e(((((m(p4)-1)*N+m(p12)-1)*N+m(h1)-1)*N+m(h2)))= + t2(((((m(p4)-1)*N+m(p12)-1)*N+m(h1)-1)*N+m(h2)))
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
i1(((((m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h2)))=i1(((((m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h2)))+(1.0d0)*t2e(((((m(p4)-1)*N+m(p12)-1&
&)*N+m(h1)-1)*N+m(h2)))*i2(((m(p5)-1)*N+m(p12)))+(-1.0d0)*t2e(((((m(p5)-1)*N+m(p12)-1)*N+m(h1)-1)*N+m(h2)))*i2(((m(p4)-1)*N+m(p12)))
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
i2(((((m(h11)-1)*N+m(h12)-1)*N+m(h1)-1)*N+m(h2)))=(1.0d0)*v2(((((m(h11)-1)*N+m(h12)-1)*N+m(h1)-1)*N+m(h2)))
END DO
END DO
END DO
END DO
DO h11=1,nocc
DO h12=1,nocc
IF (h11>h12) CYCLE
DO h1=1,nocc
DO p7=nocc+1,N
v2e(((((m(h11)-1)*N+m(h12)-1)*N+m(h1)-1)*N+m(p7)))= + v2(((((m(h11)-1)*N+m(h12)-1)*N+m(h1)-1)*N+m(p7)))
END DO
END DO
END DO
END DO
DO h11=1,nocc
DO h12=1,nocc
IF (h11>h12) CYCLE
DO h1=1,nocc
DO p7=nocc+1,N
i3(((((m(h11)-1)*N+m(h12)-1)*N+m(h1)-1)*N+m(p7)))=(1.0d0)*v2e(((((m(h11)-1)*N+m(h12)-1)*N+m(h1)-1)*N+m(p7)))
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
v2e(((((m(h11)-1)*N+m(h12)-1)*N+m(p8)-1)*N+m(p7)))= - v2(((((m(h11)-1)*N+m(h12)-1)*N+m(p7)-1)*N+m(p8)))
v2e(((((m(h11)-1)*N+m(h12)-1)*N+m(p7)-1)*N+m(p8)))= + v2(((((m(h11)-1)*N+m(h12)-1)*N+m(p7)-1)*N+m(p8)))
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
i3(((((m(h11)-1)*N+m(h12)-1)*N+m(h1)-1)*N+m(p7)))=i3(((((m(h11)-1)*N+m(h12)-1)*N+m(h1)-1)*N+m(p7)))+(-0.5d0)*t1(((m(p8)-1)*N+m(h1))&
&)*v2e(((((m(h11)-1)*N+m(h12)-1)*N+m(p7)-1)*N+m(p8)))
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
i2(((((m(h11)-1)*N+m(h12)-1)*N+m(h1)-1)*N+m(h2)))=i2(((((m(h11)-1)*N+m(h12)-1)*N+m(h1)-1)*N+m(h2)))+(-1.0d0)*t1(((m(p7)-1)*N+m(h1))&
&)*i3(((((m(h11)-1)*N+m(h12)-1)*N+m(h2)-1)*N+m(p7)))+(1.0d0)*t1(((m(p7)-1)*N+m(h2)))*i3(((((m(h11)-1)*N+m(h12)-1)*N+m(h1)-1)*N+m(p7&
&)))
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
i2(((((m(h11)-1)*N+m(h12)-1)*N+m(h1)-1)*N+m(h2)))=i2(((((m(h11)-1)*N+m(h12)-1)*N+m(h1)-1)*N+m(h2)))+(1.0d0)*t2(((((m(p9)-1)*N+m(p10&
&)-1)*N+m(h1)-1)*N+m(h2)))*v2(((((m(h11)-1)*N+m(h12)-1)*N+m(p9)-1)*N+m(p10)))
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
i1(((((m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h2)))=i1(((((m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h2)))+(1.0d0)*t2(((((m(p4)-1)*N+m(p5)-1)*&
&N+m(h11)-1)*N+m(h12)))*i2(((((m(h11)-1)*N+m(h12)-1)*N+m(h1)-1)*N+m(h2)))
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
v2e(((((m(h13)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(p12)))= + v2(((((m(h13)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(p12)))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h13=1,nocc
DO h1=1,nocc
DO p12=nocc+1,N
i2(((((m(h13)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(p12)))=(1.0d0)*v2e(((((m(h13)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(p12)))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h13=1,nocc
DO p12=nocc+1,N
DO p7=nocc+1,N
IF (p7>p12) CYCLE
v2e(((((m(h13)-1)*N+m(p4)-1)*N+m(p12)-1)*N+m(p7)))= - v2(((((m(h13)-1)*N+m(p4)-1)*N+m(p7)-1)*N+m(p12)))
v2e(((((m(h13)-1)*N+m(p4)-1)*N+m(p7)-1)*N+m(p12)))= + v2(((((m(h13)-1)*N+m(p4)-1)*N+m(p7)-1)*N+m(p12)))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h13=1,nocc
DO h1=1,nocc
DO p12=nocc+1,N
DO p7=nocc+1,N
i2(((((m(h13)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(p12)))=i2(((((m(h13)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(p12)))+(1.0d0)*t1(((m(p7)-1)*N+m(h1)))&
&*v2e(((((m(h13)-1)*N+m(p4)-1)*N+m(p7)-1)*N+m(p12)))
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
t2e(((((m(p9)-1)*N+m(p4)-1)*N+m(h10)-1)*N+m(h1)))= + t2(((((m(p4)-1)*N+m(p9)-1)*N+m(h1)-1)*N+m(h10)))
t2e(((((m(p9)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h10)))= - t2(((((m(p4)-1)*N+m(p9)-1)*N+m(h1)-1)*N+m(h10)))
t2e(((((m(p4)-1)*N+m(p9)-1)*N+m(h10)-1)*N+m(h1)))= - t2(((((m(p4)-1)*N+m(p9)-1)*N+m(h1)-1)*N+m(h10)))
t2e(((((m(p4)-1)*N+m(p9)-1)*N+m(h1)-1)*N+m(h10)))= + t2(((((m(p4)-1)*N+m(p9)-1)*N+m(h1)-1)*N+m(h10)))
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
v2e(((((m(h13)-1)*N+m(h10)-1)*N+m(p12)-1)*N+m(p9)))= + v2(((((m(h10)-1)*N+m(h13)-1)*N+m(p9)-1)*N+m(p12)))
v2e(((((m(h13)-1)*N+m(h10)-1)*N+m(p9)-1)*N+m(p12)))= - v2(((((m(h10)-1)*N+m(h13)-1)*N+m(p9)-1)*N+m(p12)))
v2e(((((m(h10)-1)*N+m(h13)-1)*N+m(p12)-1)*N+m(p9)))= - v2(((((m(h10)-1)*N+m(h13)-1)*N+m(p9)-1)*N+m(p12)))
v2e(((((m(h10)-1)*N+m(h13)-1)*N+m(p9)-1)*N+m(p12)))= + v2(((((m(h10)-1)*N+m(h13)-1)*N+m(p9)-1)*N+m(p12)))
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
i2(((((m(h13)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(p12)))=i2(((((m(h13)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(p12)))+(-0.5d0)*t2e(((((m(p4)-1)*N+m(p&
&9)-1)*N+m(h1)-1)*N+m(h10)))*v2e(((((m(h10)-1)*N+m(h13)-1)*N+m(p9)-1)*N+m(p12)))
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
t2e(((((m(p12)-1)*N+m(p4)-1)*N+m(h13)-1)*N+m(h1)))= + t2(((((m(p4)-1)*N+m(p12)-1)*N+m(h1)-1)*N+m(h13)))
t2e(((((m(p12)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h13)))= - t2(((((m(p4)-1)*N+m(p12)-1)*N+m(h1)-1)*N+m(h13)))
t2e(((((m(p4)-1)*N+m(p12)-1)*N+m(h13)-1)*N+m(h1)))= - t2(((((m(p4)-1)*N+m(p12)-1)*N+m(h1)-1)*N+m(h13)))
t2e(((((m(p4)-1)*N+m(p12)-1)*N+m(h1)-1)*N+m(h13)))= + t2(((((m(p4)-1)*N+m(p12)-1)*N+m(h1)-1)*N+m(h13)))
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
i1(((((m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h2)))=i1(((((m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h2)))+(-1.0d0)*t2e(((((m(p4)-1)*N+m(p12)-&
&1)*N+m(h1)-1)*N+m(h13)))*i2(((((m(h13)-1)*N+m(p5)-1)*N+m(h2)-1)*N+m(p12)))+(1.0d0)*t2e(((((m(p4)-1)*N+m(p12)-1)*N+m(h2)-1)*N+m(h13&
&)))*i2(((((m(h13)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(p12)))+(1.0d0)*t2e(((((m(p5)-1)*N+m(p12)-1)*N+m(h1)-1)*N+m(h13)))*i2(((((m(h13)-1)*&
&N+m(p4)-1)*N+m(h2)-1)*N+m(p12)))+(-1.0d0)*t2e(((((m(p5)-1)*N+m(p12)-1)*N+m(h2)-1)*N+m(h13)))*i2(((((m(h13)-1)*N+m(p4)-1)*N+m(h1)-1&
&)*N+m(p12)))
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
i1(((((m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h2)))=i1(((((m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h2)))+(1.0d0)*t2(((((m(p7)-1)*N+m(p8)-1)*&
&N+m(h1)-1)*N+m(h2)))*v2(((((m(p4)-1)*N+m(p5)-1)*N+m(p7)-1)*N+m(p8)))
END DO
END DO
END DO
END DO
END DO
END DO
DO h9=1,nocc
DO p11=nocc+1,N
i2(((m(h9)-1)*N+m(p11)))=(1.0d0)*f1(((m(h9)-1)*N+m(p11)))
END DO
END DO
DO h9=1,nocc
DO h8=1,nocc
IF (h8>h9) CYCLE
DO p11=nocc+1,N
DO p7=nocc+1,N
IF (p7>p11) CYCLE
v2e(((((m(h9)-1)*N+m(h8)-1)*N+m(p11)-1)*N+m(p7)))= + v2(((((m(h8)-1)*N+m(h9)-1)*N+m(p7)-1)*N+m(p11)))
v2e(((((m(h9)-1)*N+m(h8)-1)*N+m(p7)-1)*N+m(p11)))= - v2(((((m(h8)-1)*N+m(h9)-1)*N+m(p7)-1)*N+m(p11)))
v2e(((((m(h8)-1)*N+m(h9)-1)*N+m(p11)-1)*N+m(p7)))= - v2(((((m(h8)-1)*N+m(h9)-1)*N+m(p7)-1)*N+m(p11)))
v2e(((((m(h8)-1)*N+m(h9)-1)*N+m(p7)-1)*N+m(p11)))= + v2(((((m(h8)-1)*N+m(h9)-1)*N+m(p7)-1)*N+m(p11)))
END DO
END DO
END DO
END DO
DO h9=1,nocc
DO p11=nocc+1,N
DO p7=nocc+1,N
DO h8=1,nocc
i2(((m(h9)-1)*N+m(p11)))=i2(((m(h9)-1)*N+m(p11)))+(1.0d0)*t1(((m(p7)-1)*N+m(h8)))*v2e(((((m(h8)-1)*N+m(h9)-1)*N+m(p7)-1)*N+m(p11)))
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
t3e(((((((m(p5)-1)*N+m(p11)-1)*N+m(p4)-1)*N+m(h2)-1)*N+m(h9)-1)*N+m(h1)))= + t3(((((((m(p4)-1)*N+m(p5)-1)*N+m(p11)-1)*N+m(h1)-1)*N+&
&m(h2)-1)*N+m(h9)))
t3e(((((((m(p5)-1)*N+m(p11)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h9)-1)*N+m(h2)))= - t3(((((((m(p4)-1)*N+m(p5)-1)*N+m(p11)-1)*N+m(h1)-1)*N+&
&m(h2)-1)*N+m(h9)))
t3e(((((((m(p5)-1)*N+m(p11)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h9)))= + t3(((((((m(p4)-1)*N+m(p5)-1)*N+m(p11)-1)*N+m(h1)-1)*N+&
&m(h2)-1)*N+m(h9)))
t3e(((((((m(p4)-1)*N+m(p11)-1)*N+m(p5)-1)*N+m(h2)-1)*N+m(h9)-1)*N+m(h1)))= - t3(((((((m(p4)-1)*N+m(p5)-1)*N+m(p11)-1)*N+m(h1)-1)*N+&
&m(h2)-1)*N+m(h9)))
t3e(((((((m(p4)-1)*N+m(p11)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h9)-1)*N+m(h2)))= + t3(((((((m(p4)-1)*N+m(p5)-1)*N+m(p11)-1)*N+m(h1)-1)*N+&
&m(h2)-1)*N+m(h9)))
t3e(((((((m(p4)-1)*N+m(p11)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h9)))= - t3(((((((m(p4)-1)*N+m(p5)-1)*N+m(p11)-1)*N+m(h1)-1)*N+&
&m(h2)-1)*N+m(h9)))
t3e(((((((m(p4)-1)*N+m(p5)-1)*N+m(p11)-1)*N+m(h2)-1)*N+m(h9)-1)*N+m(h1)))= + t3(((((((m(p4)-1)*N+m(p5)-1)*N+m(p11)-1)*N+m(h1)-1)*N+&
&m(h2)-1)*N+m(h9)))
t3e(((((((m(p4)-1)*N+m(p5)-1)*N+m(p11)-1)*N+m(h1)-1)*N+m(h9)-1)*N+m(h2)))= - t3(((((((m(p4)-1)*N+m(p5)-1)*N+m(p11)-1)*N+m(h1)-1)*N+&
&m(h2)-1)*N+m(h9)))
t3e(((((((m(p4)-1)*N+m(p5)-1)*N+m(p11)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h9)))= + t3(((((((m(p4)-1)*N+m(p5)-1)*N+m(p11)-1)*N+m(h1)-1)*N+&
&m(h2)-1)*N+m(h9)))
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
i1(((((m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h2)))=i1(((((m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h2)))+(1.0d0)*t3e(((((((m(p4)-1)*N+m(p5)-&
&1)*N+m(p11)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h9)))*i2(((m(h9)-1)*N+m(p11)))
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
v2e(((((m(h8)-1)*N+m(h12)-1)*N+m(h1)-1)*N+m(p11)))= + v2(((((m(h8)-1)*N+m(h12)-1)*N+m(h1)-1)*N+m(p11)))
END DO
END DO
END DO
END DO
DO h8=1,nocc
DO h12=1,nocc
IF (h8>h12) CYCLE
DO h1=1,nocc
DO p11=nocc+1,N
i2(((((m(h8)-1)*N+m(h12)-1)*N+m(h1)-1)*N+m(p11)))=(-1.0d0)*v2e(((((m(h8)-1)*N+m(h12)-1)*N+m(h1)-1)*N+m(p11)))
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
v2e(((((m(h8)-1)*N+m(h12)-1)*N+m(p11)-1)*N+m(p7)))= - v2(((((m(h8)-1)*N+m(h12)-1)*N+m(p7)-1)*N+m(p11)))
v2e(((((m(h8)-1)*N+m(h12)-1)*N+m(p7)-1)*N+m(p11)))= + v2(((((m(h8)-1)*N+m(h12)-1)*N+m(p7)-1)*N+m(p11)))
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
i2(((((m(h8)-1)*N+m(h12)-1)*N+m(h1)-1)*N+m(p11)))=i2(((((m(h8)-1)*N+m(h12)-1)*N+m(h1)-1)*N+m(p11)))+(-1.0d0)*t1(((m(p7)-1)*N+m(h1))&
&)*v2e(((((m(h8)-1)*N+m(h12)-1)*N+m(p7)-1)*N+m(p11)))
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
t3e(((((((m(p5)-1)*N+m(p11)-1)*N+m(p4)-1)*N+m(h8)-1)*N+m(h1)-1)*N+m(h12)))= - t3(((((((m(p4)-1)*N+m(p5)-1)*N+m(p11)-1)*N+m(h1)-1)*N&
&+m(h8)-1)*N+m(h12)))
t3e(((((((m(p5)-1)*N+m(p11)-1)*N+m(p4)-1)*N+m(h12)-1)*N+m(h1)-1)*N+m(h8)))= + t3(((((((m(p4)-1)*N+m(p5)-1)*N+m(p11)-1)*N+m(h1)-1)*N&
&+m(h8)-1)*N+m(h12)))
t3e(((((((m(p5)-1)*N+m(p11)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h8)-1)*N+m(h12)))= + t3(((((((m(p4)-1)*N+m(p5)-1)*N+m(p11)-1)*N+m(h1)-1)*N&
&+m(h8)-1)*N+m(h12)))
t3e(((((((m(p4)-1)*N+m(p11)-1)*N+m(p5)-1)*N+m(h8)-1)*N+m(h1)-1)*N+m(h12)))= + t3(((((((m(p4)-1)*N+m(p5)-1)*N+m(p11)-1)*N+m(h1)-1)*N&
&+m(h8)-1)*N+m(h12)))
t3e(((((((m(p4)-1)*N+m(p11)-1)*N+m(p5)-1)*N+m(h12)-1)*N+m(h1)-1)*N+m(h8)))= - t3(((((((m(p4)-1)*N+m(p5)-1)*N+m(p11)-1)*N+m(h1)-1)*N&
&+m(h8)-1)*N+m(h12)))
t3e(((((((m(p4)-1)*N+m(p11)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h8)-1)*N+m(h12)))= - t3(((((((m(p4)-1)*N+m(p5)-1)*N+m(p11)-1)*N+m(h1)-1)*N&
&+m(h8)-1)*N+m(h12)))
t3e(((((((m(p4)-1)*N+m(p5)-1)*N+m(p11)-1)*N+m(h8)-1)*N+m(h1)-1)*N+m(h12)))= - t3(((((((m(p4)-1)*N+m(p5)-1)*N+m(p11)-1)*N+m(h1)-1)*N&
&+m(h8)-1)*N+m(h12)))
t3e(((((((m(p4)-1)*N+m(p5)-1)*N+m(p11)-1)*N+m(h12)-1)*N+m(h1)-1)*N+m(h8)))= + t3(((((((m(p4)-1)*N+m(p5)-1)*N+m(p11)-1)*N+m(h1)-1)*N&
&+m(h8)-1)*N+m(h12)))
t3e(((((((m(p4)-1)*N+m(p5)-1)*N+m(p11)-1)*N+m(h1)-1)*N+m(h8)-1)*N+m(h12)))= + t3(((((((m(p4)-1)*N+m(p5)-1)*N+m(p11)-1)*N+m(h1)-1)*N&
&+m(h8)-1)*N+m(h12)))
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
i1(((((m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h2)))=i1(((((m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h2)))+(1.0d0)*t3e(((((((m(p4)-1)*N+m(p5)-&
&1)*N+m(p11)-1)*N+m(h1)-1)*N+m(h8)-1)*N+m(h12)))*i2(((((m(h8)-1)*N+m(h12)-1)*N+m(h2)-1)*N+m(p11)))+(-1.0d0)*t3e(((((((m(p4)-1)*N+m(&
&p5)-1)*N+m(p11)-1)*N+m(h2)-1)*N+m(h8)-1)*N+m(h12)))*i2(((((m(h8)-1)*N+m(h12)-1)*N+m(h1)-1)*N+m(p11)))
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
t3e(((((((m(p7)-1)*N+m(p4)-1)*N+m(p8)-1)*N+m(h2)-1)*N+m(h9)-1)*N+m(h1)))= - t3(((((((m(p4)-1)*N+m(p7)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(&
&h2)-1)*N+m(h9)))
t3e(((((((m(p7)-1)*N+m(p4)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(h9)-1)*N+m(h2)))= + t3(((((((m(p4)-1)*N+m(p7)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(&
&h2)-1)*N+m(h9)))
t3e(((((((m(p7)-1)*N+m(p4)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h9)))= - t3(((((((m(p4)-1)*N+m(p7)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(&
&h2)-1)*N+m(h9)))
t3e(((((((m(p8)-1)*N+m(p4)-1)*N+m(p7)-1)*N+m(h2)-1)*N+m(h9)-1)*N+m(h1)))= + t3(((((((m(p4)-1)*N+m(p7)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(&
&h2)-1)*N+m(h9)))
t3e(((((((m(p8)-1)*N+m(p4)-1)*N+m(p7)-1)*N+m(h1)-1)*N+m(h9)-1)*N+m(h2)))= - t3(((((((m(p4)-1)*N+m(p7)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(&
&h2)-1)*N+m(h9)))
t3e(((((((m(p8)-1)*N+m(p4)-1)*N+m(p7)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h9)))= + t3(((((((m(p4)-1)*N+m(p7)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(&
&h2)-1)*N+m(h9)))
t3e(((((((m(p4)-1)*N+m(p7)-1)*N+m(p8)-1)*N+m(h2)-1)*N+m(h9)-1)*N+m(h1)))= + t3(((((((m(p4)-1)*N+m(p7)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(&
&h2)-1)*N+m(h9)))
t3e(((((((m(p4)-1)*N+m(p7)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(h9)-1)*N+m(h2)))= - t3(((((((m(p4)-1)*N+m(p7)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(&
&h2)-1)*N+m(h9)))
t3e(((((((m(p4)-1)*N+m(p7)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h9)))= + t3(((((((m(p4)-1)*N+m(p7)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(&
&h2)-1)*N+m(h9)))
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
v2e(((((m(h9)-1)*N+m(p5)-1)*N+m(p7)-1)*N+m(p8)))= + v2(((((m(h9)-1)*N+m(p5)-1)*N+m(p7)-1)*N+m(p8)))
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
i1(((((m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h2)))=i1(((((m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h2)))+(-1.0d0)*t3e(((((((m(p4)-1)*N+m(p7)&
&-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h9)))*v2e(((((m(h9)-1)*N+m(p5)-1)*N+m(p7)-1)*N+m(p8)))+(1.0d0)*t3e(((((((m(p5)-1)*N+m(p7)&
&-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h9)))*v2e(((((m(h9)-1)*N+m(p4)-1)*N+m(p7)-1)*N+m(p8)))
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
i0(((((((m(p4)-1)*N+m(p5)-1)*N+m(p6)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h3)))=0.0d0
i0(((((((m(p4)-1)*N+m(p5)-1)*N+m(p6)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h3)))=i0(((((((m(p4)-1)*N+m(p5)-1)*N+m(p6)-1)*N+m(h1)-1)*N+m(h2)-&
&1)*N+m(h3)))+(1.0d0)*t1(((m(p4)-1)*N+m(h1)))*i1(((((m(p5)-1)*N+m(p6)-1)*N+m(h2)-1)*N+m(h3)))+(-1.0d0)*t1(((m(p4)-1)*N+m(h2)))*i1((&
&(((m(p5)-1)*N+m(p6)-1)*N+m(h1)-1)*N+m(h3)))+(1.0d0)*t1(((m(p4)-1)*N+m(h3)))*i1(((((m(p5)-1)*N+m(p6)-1)*N+m(h1)-1)*N+m(h2)))+(-1.0d&
&0)*t1(((m(p5)-1)*N+m(h1)))*i1(((((m(p4)-1)*N+m(p6)-1)*N+m(h2)-1)*N+m(h3)))+(1.0d0)*t1(((m(p5)-1)*N+m(h2)))*i1(((((m(p4)-1)*N+m(p6)&
&-1)*N+m(h1)-1)*N+m(h3)))+(-1.0d0)*t1(((m(p5)-1)*N+m(h3)))*i1(((((m(p4)-1)*N+m(p6)-1)*N+m(h1)-1)*N+m(h2)))+(1.0d0)*t1(((m(p6)-1)*N+&
&m(h1)))*i1(((((m(p4)-1)*N+m(p5)-1)*N+m(h2)-1)*N+m(h3)))+(-1.0d0)*t1(((m(p6)-1)*N+m(h2)))*i1(((((m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m&
&(h3)))+(1.0d0)*t1(((m(p6)-1)*N+m(h3)))*i1(((((m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h2)))
END DO
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h1=1,nocc
i1(((m(p4)-1)*N+m(h1)))=(1.0d0)*f1(((m(p4)-1)*N+m(h1)))
END DO
END DO
DO h11=1,nocc
DO h1=1,nocc
i2(((m(h11)-1)*N+m(h1)))=(1.0d0)*f1(((m(h11)-1)*N+m(h1)))
END DO
END DO
DO h11=1,nocc
DO p12=nocc+1,N
i3(((m(h11)-1)*N+m(p12)))=(1.0d0)*f1(((m(h11)-1)*N+m(p12)))
END DO
END DO
DO h11=1,nocc
DO h10=1,nocc
IF (h10>h11) CYCLE
DO p12=nocc+1,N
DO p9=nocc+1,N
IF (p9>p12) CYCLE
v2e(((((m(h11)-1)*N+m(h10)-1)*N+m(p12)-1)*N+m(p9)))= + v2(((((m(h10)-1)*N+m(h11)-1)*N+m(p9)-1)*N+m(p12)))
v2e(((((m(h11)-1)*N+m(h10)-1)*N+m(p9)-1)*N+m(p12)))= - v2(((((m(h10)-1)*N+m(h11)-1)*N+m(p9)-1)*N+m(p12)))
v2e(((((m(h10)-1)*N+m(h11)-1)*N+m(p12)-1)*N+m(p9)))= - v2(((((m(h10)-1)*N+m(h11)-1)*N+m(p9)-1)*N+m(p12)))
v2e(((((m(h10)-1)*N+m(h11)-1)*N+m(p9)-1)*N+m(p12)))= + v2(((((m(h10)-1)*N+m(h11)-1)*N+m(p9)-1)*N+m(p12)))
END DO
END DO
END DO
END DO
DO h11=1,nocc
DO p12=nocc+1,N
DO p9=nocc+1,N
DO h10=1,nocc
i3(((m(h11)-1)*N+m(p12)))=i3(((m(h11)-1)*N+m(p12)))+(0.9999999999999998d0)*t1(((m(p9)-1)*N+m(h10)))*v2e(((((m(h10)-1)*N+m(h11)-1)*N&
&+m(p9)-1)*N+m(p12)))
END DO
END DO
END DO
END DO
DO h11=1,nocc
DO h1=1,nocc
DO p12=nocc+1,N
i2(((m(h11)-1)*N+m(h1)))=i2(((m(h11)-1)*N+m(h1)))+(1.0d0)*t1(((m(p12)-1)*N+m(h1)))*i3(((m(h11)-1)*N+m(p12)))
END DO
END DO
END DO
DO h11=1,nocc
DO h9=1,nocc
IF (h9>h11) CYCLE
DO h1=1,nocc
DO p8=nocc+1,N
v2e(((((m(h11)-1)*N+m(h9)-1)*N+m(h1)-1)*N+m(p8)))= - v2(((((m(h9)-1)*N+m(h11)-1)*N+m(h1)-1)*N+m(p8)))
v2e(((((m(h9)-1)*N+m(h11)-1)*N+m(h1)-1)*N+m(p8)))= + v2(((((m(h9)-1)*N+m(h11)-1)*N+m(h1)-1)*N+m(p8)))
END DO
END DO
END DO
END DO
DO h11=1,nocc
DO h1=1,nocc
DO p8=nocc+1,N
DO h9=1,nocc
i2(((m(h11)-1)*N+m(h1)))=i2(((m(h11)-1)*N+m(h1)))+(-1.0d0)*t1(((m(p8)-1)*N+m(h9)))*v2e(((((m(h9)-1)*N+m(h11)-1)*N+m(h1)-1)*N+m(p8)))
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
t2e(((((m(p7)-1)*N+m(p8)-1)*N+m(h9)-1)*N+m(h1)))= - t2(((((m(p7)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(h9)))
t2e(((((m(p7)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(h9)))= + t2(((((m(p7)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(h9)))
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
v2e(((((m(h11)-1)*N+m(h9)-1)*N+m(p7)-1)*N+m(p8)))= - v2(((((m(h9)-1)*N+m(h11)-1)*N+m(p7)-1)*N+m(p8)))
v2e(((((m(h9)-1)*N+m(h11)-1)*N+m(p7)-1)*N+m(p8)))= + v2(((((m(h9)-1)*N+m(h11)-1)*N+m(p7)-1)*N+m(p8)))
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
i2(((m(h11)-1)*N+m(h1)))=i2(((m(h11)-1)*N+m(h1)))+(-1.0d0)*t2e(((((m(p7)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(h9)))*v2e(((((m(h9)-1)*N+m(h1&
&1)-1)*N+m(p7)-1)*N+m(p8)))
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h1=1,nocc
DO h11=1,nocc
i1(((m(p4)-1)*N+m(h1)))=i1(((m(p4)-1)*N+m(h1)))+(-1.0d0)*t1(((m(p4)-1)*N+m(h11)))*i2(((m(h11)-1)*N+m(h1)))
END DO
END DO
END DO
DO p4=nocc+1,N
DO p11=nocc+1,N
i2(((m(p4)-1)*N+m(p11)))=(1.0d0)*f1(((m(p4)-1)*N+m(p11)))
END DO
END DO
DO p4=nocc+1,N
DO h9=1,nocc
DO p11=nocc+1,N
DO p8=nocc+1,N
IF (p8>p11) CYCLE
v2e(((((m(h9)-1)*N+m(p4)-1)*N+m(p11)-1)*N+m(p8)))= - v2(((((m(h9)-1)*N+m(p4)-1)*N+m(p8)-1)*N+m(p11)))
v2e(((((m(h9)-1)*N+m(p4)-1)*N+m(p8)-1)*N+m(p11)))= + v2(((((m(h9)-1)*N+m(p4)-1)*N+m(p8)-1)*N+m(p11)))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p11=nocc+1,N
DO p8=nocc+1,N
DO h9=1,nocc
i2(((m(p4)-1)*N+m(p11)))=i2(((m(p4)-1)*N+m(p11)))+(1.0d0)*t1(((m(p8)-1)*N+m(h9)))*v2e(((((m(h9)-1)*N+m(p4)-1)*N+m(p8)-1)*N+m(p11)))
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
t2e(((((m(p7)-1)*N+m(p4)-1)*N+m(h8)-1)*N+m(h9)))= - t2(((((m(p4)-1)*N+m(p7)-1)*N+m(h8)-1)*N+m(h9)))
t2e(((((m(p4)-1)*N+m(p7)-1)*N+m(h8)-1)*N+m(h9)))= + t2(((((m(p4)-1)*N+m(p7)-1)*N+m(h8)-1)*N+m(h9)))
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
v2e(((((m(h8)-1)*N+m(h9)-1)*N+m(p11)-1)*N+m(p7)))= - v2(((((m(h8)-1)*N+m(h9)-1)*N+m(p7)-1)*N+m(p11)))
v2e(((((m(h8)-1)*N+m(h9)-1)*N+m(p7)-1)*N+m(p11)))= + v2(((((m(h8)-1)*N+m(h9)-1)*N+m(p7)-1)*N+m(p11)))
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
i2(((m(p4)-1)*N+m(p11)))=i2(((m(p4)-1)*N+m(p11)))+(1.0d0)*t2e(((((m(p4)-1)*N+m(p7)-1)*N+m(h8)-1)*N+m(h9)))*v2e(((((m(h8)-1)*N+m(h9)&
&-1)*N+m(p7)-1)*N+m(p11)))
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h1=1,nocc
DO p11=nocc+1,N
i1(((m(p4)-1)*N+m(h1)))=i1(((m(p4)-1)*N+m(h1)))+(1.0d0)*t1(((m(p11)-1)*N+m(h1)))*i2(((m(p4)-1)*N+m(p11)))
END DO
END DO
END DO
DO p4=nocc+1,N
DO h8=1,nocc
DO h1=1,nocc
DO p7=nocc+1,N
v2e(((((m(h8)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(p7)))= + v2(((((m(h8)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(p7)))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h1=1,nocc
DO p7=nocc+1,N
DO h8=1,nocc
i1(((m(p4)-1)*N+m(h1)))=i1(((m(p4)-1)*N+m(h1)))+(-1.0d0)*t1(((m(p7)-1)*N+m(h8)))*v2e(((((m(h8)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(p7)))
END DO
END DO
END DO
END DO
DO h8=1,nocc
DO p7=nocc+1,N
i2(((m(h8)-1)*N+m(p7)))=(1.0d0)*f1(((m(h8)-1)*N+m(p7)))
END DO
END DO
DO h8=1,nocc
DO h10=1,nocc
IF (h8>h10) CYCLE
DO p7=nocc+1,N
DO p9=nocc+1,N
IF (p7>p9) CYCLE
v2e(((((m(h10)-1)*N+m(h8)-1)*N+m(p9)-1)*N+m(p7)))= + v2(((((m(h8)-1)*N+m(h10)-1)*N+m(p7)-1)*N+m(p9)))
v2e(((((m(h10)-1)*N+m(h8)-1)*N+m(p7)-1)*N+m(p9)))= - v2(((((m(h8)-1)*N+m(h10)-1)*N+m(p7)-1)*N+m(p9)))
v2e(((((m(h8)-1)*N+m(h10)-1)*N+m(p9)-1)*N+m(p7)))= - v2(((((m(h8)-1)*N+m(h10)-1)*N+m(p7)-1)*N+m(p9)))
v2e(((((m(h8)-1)*N+m(h10)-1)*N+m(p7)-1)*N+m(p9)))= + v2(((((m(h8)-1)*N+m(h10)-1)*N+m(p7)-1)*N+m(p9)))
END DO
END DO
END DO
END DO
DO h8=1,nocc
DO p7=nocc+1,N
DO p9=nocc+1,N
DO h10=1,nocc
i2(((m(h8)-1)*N+m(p7)))=i2(((m(h8)-1)*N+m(p7)))+(1.0d0)*t1(((m(p9)-1)*N+m(h10)))*v2e(((((m(h8)-1)*N+m(h10)-1)*N+m(p7)-1)*N+m(p9)))
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
t2e(((((m(p7)-1)*N+m(p4)-1)*N+m(h8)-1)*N+m(h1)))= + t2(((((m(p4)-1)*N+m(p7)-1)*N+m(h1)-1)*N+m(h8)))
t2e(((((m(p7)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h8)))= - t2(((((m(p4)-1)*N+m(p7)-1)*N+m(h1)-1)*N+m(h8)))
t2e(((((m(p4)-1)*N+m(p7)-1)*N+m(h8)-1)*N+m(h1)))= - t2(((((m(p4)-1)*N+m(p7)-1)*N+m(h1)-1)*N+m(h8)))
t2e(((((m(p4)-1)*N+m(p7)-1)*N+m(h1)-1)*N+m(h8)))= + t2(((((m(p4)-1)*N+m(p7)-1)*N+m(h1)-1)*N+m(h8)))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h1=1,nocc
DO p7=nocc+1,N
DO h8=1,nocc
i1(((m(p4)-1)*N+m(h1)))=i1(((m(p4)-1)*N+m(h1)))+(1.0d0)*t2e(((((m(p4)-1)*N+m(p7)-1)*N+m(h1)-1)*N+m(h8)))*i2(((m(h8)-1)*N+m(p7)))
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
t2e(((((m(p7)-1)*N+m(p4)-1)*N+m(h8)-1)*N+m(h9)))= - t2(((((m(p4)-1)*N+m(p7)-1)*N+m(h8)-1)*N+m(h9)))
t2e(((((m(p4)-1)*N+m(p7)-1)*N+m(h8)-1)*N+m(h9)))= + t2(((((m(p4)-1)*N+m(p7)-1)*N+m(h8)-1)*N+m(h9)))
END DO
END DO
END DO
END DO
DO h8=1,nocc
DO h9=1,nocc
IF (h8>h9) CYCLE
DO h1=1,nocc
DO p7=nocc+1,N
v2e(((((m(h8)-1)*N+m(h9)-1)*N+m(h1)-1)*N+m(p7)))= + v2(((((m(h8)-1)*N+m(h9)-1)*N+m(h1)-1)*N+m(p7)))
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
i1(((m(p4)-1)*N+m(h1)))=i1(((m(p4)-1)*N+m(h1)))+(-1.0d0)*t2e(((((m(p4)-1)*N+m(p7)-1)*N+m(h8)-1)*N+m(h9)))*v2e(((((m(h8)-1)*N+m(h9)-&
&1)*N+m(h1)-1)*N+m(p7)))
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
t2e(((((m(p7)-1)*N+m(p8)-1)*N+m(h9)-1)*N+m(h1)))= - t2(((((m(p7)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(h9)))
t2e(((((m(p7)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(h9)))= + t2(((((m(p7)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(h9)))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h9=1,nocc
DO p7=nocc+1,N
DO p8=nocc+1,N
IF (p7>p8) CYCLE
v2e(((((m(h9)-1)*N+m(p4)-1)*N+m(p7)-1)*N+m(p8)))= + v2(((((m(h9)-1)*N+m(p4)-1)*N+m(p7)-1)*N+m(p8)))
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
i1(((m(p4)-1)*N+m(h1)))=i1(((m(p4)-1)*N+m(h1)))+(-1.0d0)*t2e(((((m(p7)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(h9)))*v2e(((((m(h9)-1)*N+m(p4)-&
&1)*N+m(p7)-1)*N+m(p8)))
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
t3e(((((((m(p7)-1)*N+m(p4)-1)*N+m(p8)-1)*N+m(h9)-1)*N+m(h1)-1)*N+m(h10)))= + t3(((((((m(p4)-1)*N+m(p7)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m&
&(h9)-1)*N+m(h10)))
t3e(((((((m(p7)-1)*N+m(p4)-1)*N+m(p8)-1)*N+m(h10)-1)*N+m(h1)-1)*N+m(h9)))= - t3(((((((m(p4)-1)*N+m(p7)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m&
&(h9)-1)*N+m(h10)))
t3e(((((((m(p7)-1)*N+m(p4)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(h9)-1)*N+m(h10)))= - t3(((((((m(p4)-1)*N+m(p7)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m&
&(h9)-1)*N+m(h10)))
t3e(((((((m(p8)-1)*N+m(p4)-1)*N+m(p7)-1)*N+m(h9)-1)*N+m(h1)-1)*N+m(h10)))= - t3(((((((m(p4)-1)*N+m(p7)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m&
&(h9)-1)*N+m(h10)))
t3e(((((((m(p8)-1)*N+m(p4)-1)*N+m(p7)-1)*N+m(h10)-1)*N+m(h1)-1)*N+m(h9)))= + t3(((((((m(p4)-1)*N+m(p7)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m&
&(h9)-1)*N+m(h10)))
t3e(((((((m(p8)-1)*N+m(p4)-1)*N+m(p7)-1)*N+m(h1)-1)*N+m(h9)-1)*N+m(h10)))= + t3(((((((m(p4)-1)*N+m(p7)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m&
&(h9)-1)*N+m(h10)))
t3e(((((((m(p4)-1)*N+m(p7)-1)*N+m(p8)-1)*N+m(h9)-1)*N+m(h1)-1)*N+m(h10)))= - t3(((((((m(p4)-1)*N+m(p7)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m&
&(h9)-1)*N+m(h10)))
t3e(((((((m(p4)-1)*N+m(p7)-1)*N+m(p8)-1)*N+m(h10)-1)*N+m(h1)-1)*N+m(h9)))= + t3(((((((m(p4)-1)*N+m(p7)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m&
&(h9)-1)*N+m(h10)))
t3e(((((((m(p4)-1)*N+m(p7)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(h9)-1)*N+m(h10)))= + t3(((((((m(p4)-1)*N+m(p7)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m&
&(h9)-1)*N+m(h10)))
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
i1(((m(p4)-1)*N+m(h1)))=i1(((m(p4)-1)*N+m(h1)))+(1.0d0)*t3e(((((((m(p4)-1)*N+m(p7)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(h9)-1)*N+m(h10)))*v&
&2(((((m(h9)-1)*N+m(h10)-1)*N+m(p7)-1)*N+m(p8)))
END DO
END DO
END DO
END DO
END DO
END DO
DO h8=1,nocc
DO p7=nocc+1,N
i3(((m(h8)-1)*N+m(p7)))=(1.0d0)*f1(((m(h8)-1)*N+m(p7)))
END DO
END DO
DO h8=1,nocc
DO h10=1,nocc
IF (h8>h10) CYCLE
DO p7=nocc+1,N
DO p9=nocc+1,N
IF (p7>p9) CYCLE
v2e(((((m(h10)-1)*N+m(h8)-1)*N+m(p9)-1)*N+m(p7)))= + v2(((((m(h8)-1)*N+m(h10)-1)*N+m(p7)-1)*N+m(p9)))
v2e(((((m(h10)-1)*N+m(h8)-1)*N+m(p7)-1)*N+m(p9)))= - v2(((((m(h8)-1)*N+m(h10)-1)*N+m(p7)-1)*N+m(p9)))
v2e(((((m(h8)-1)*N+m(h10)-1)*N+m(p9)-1)*N+m(p7)))= - v2(((((m(h8)-1)*N+m(h10)-1)*N+m(p7)-1)*N+m(p9)))
v2e(((((m(h8)-1)*N+m(h10)-1)*N+m(p7)-1)*N+m(p9)))= + v2(((((m(h8)-1)*N+m(h10)-1)*N+m(p7)-1)*N+m(p9)))
END DO
END DO
END DO
END DO
DO h8=1,nocc
DO p7=nocc+1,N
DO p9=nocc+1,N
DO h10=1,nocc
i3(((m(h8)-1)*N+m(p7)))=i3(((m(h8)-1)*N+m(p7)))+(0.5d0)*t1(((m(p9)-1)*N+m(h10)))*v2e(((((m(h8)-1)*N+m(h10)-1)*N+m(p7)-1)*N+m(p9)))
END DO
END DO
END DO
END DO
i2(1)=0.0d0
DO p7=nocc+1,N
DO h8=1,nocc
i2(1)=i2(1)+(1.0d0)*t1(((m(p7)-1)*N+m(h8)))*i3(((m(h8)-1)*N+m(p7)))
END DO
END DO
DO p7=nocc+1,N
DO p8=nocc+1,N
IF (p7>p8) CYCLE
DO h9=1,nocc
DO h10=1,nocc
IF (h9>h10) CYCLE
i2(1)=i2(1)+(1.0d0)*t2(((((m(p7)-1)*N+m(p8)-1)*N+m(h9)-1)*N+m(h10)))*v2(((((m(h9)-1)*N+m(h10)-1)*N+m(p7)-1)*N+m(p8)))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h1=1,nocc
i1(((m(p4)-1)*N+m(h1)))=i1(((m(p4)-1)*N+m(h1)))+(1.0d0)*t1(((m(p4)-1)*N+m(h1)))*i2(1)
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
i0(((((((m(p4)-1)*N+m(p5)-1)*N+m(p6)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h3)))=i0(((((((m(p4)-1)*N+m(p5)-1)*N+m(p6)-1)*N+m(h1)-1)*N+m(h2)-&
&1)*N+m(h3)))+(1.0d0)*t2(((((m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h2)))*i1(((m(p6)-1)*N+m(h3)))+(1.0d0)*t2(((((m(p4)-1)*N+m(p5)-1)*N+&
&m(h2)-1)*N+m(h3)))*i1(((m(p6)-1)*N+m(h1)))+(-1.0d0)*t2(((((m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h3)))*i1(((m(p6)-1)*N+m(h2)))+(1.0d0&
&)*t2(((((m(p5)-1)*N+m(p6)-1)*N+m(h1)-1)*N+m(h2)))*i1(((m(p4)-1)*N+m(h3)))+(1.0d0)*t2(((((m(p5)-1)*N+m(p6)-1)*N+m(h2)-1)*N+m(h3)))*&
&i1(((m(p4)-1)*N+m(h1)))+(-1.0d0)*t2(((((m(p5)-1)*N+m(p6)-1)*N+m(h1)-1)*N+m(h3)))*i1(((m(p4)-1)*N+m(h2)))+(-1.0d0)*t2(((((m(p4)-1)*&
&N+m(p6)-1)*N+m(h1)-1)*N+m(h2)))*i1(((m(p5)-1)*N+m(h3)))+(-1.0d0)*t2(((((m(p4)-1)*N+m(p6)-1)*N+m(h2)-1)*N+m(h3)))*i1(((m(p5)-1)*N+m&
&(h1)))+(1.0d0)*t2(((((m(p4)-1)*N+m(p6)-1)*N+m(h1)-1)*N+m(h3)))*i1(((m(p5)-1)*N+m(h2)))
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
v2e(((((m(h11)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))= + v2(((((m(h11)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h11=1,nocc
DO h1=1,nocc
DO h2=1,nocc
IF (h1>h2) CYCLE
i1(((((m(h11)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))=(1.0d0)*v2e(((((m(h11)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))
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
i2(((((m(h7)-1)*N+m(h11)-1)*N+m(h1)-1)*N+m(h2)))=(1.0d0)*v2(((((m(h7)-1)*N+m(h11)-1)*N+m(h1)-1)*N+m(h2)))
END DO
END DO
END DO
END DO
DO h7=1,nocc
DO h11=1,nocc
IF (h7>h11) CYCLE
DO h1=1,nocc
DO p8=nocc+1,N
v2e(((((m(h7)-1)*N+m(h11)-1)*N+m(h1)-1)*N+m(p8)))= + v2(((((m(h7)-1)*N+m(h11)-1)*N+m(h1)-1)*N+m(p8)))
END DO
END DO
END DO
END DO
DO h7=1,nocc
DO h11=1,nocc
IF (h7>h11) CYCLE
DO h1=1,nocc
DO p8=nocc+1,N
i3(((((m(h7)-1)*N+m(h11)-1)*N+m(h1)-1)*N+m(p8)))=(1.0d0)*v2e(((((m(h7)-1)*N+m(h11)-1)*N+m(h1)-1)*N+m(p8)))
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
v2e(((((m(h7)-1)*N+m(h11)-1)*N+m(p9)-1)*N+m(p8)))= - v2(((((m(h7)-1)*N+m(h11)-1)*N+m(p8)-1)*N+m(p9)))
v2e(((((m(h7)-1)*N+m(h11)-1)*N+m(p8)-1)*N+m(p9)))= + v2(((((m(h7)-1)*N+m(h11)-1)*N+m(p8)-1)*N+m(p9)))
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
i3(((((m(h7)-1)*N+m(h11)-1)*N+m(h1)-1)*N+m(p8)))=i3(((((m(h7)-1)*N+m(h11)-1)*N+m(h1)-1)*N+m(p8)))+(-0.4999999999999999d0)*t1(((m(p9&
&)-1)*N+m(h1)))*v2e(((((m(h7)-1)*N+m(h11)-1)*N+m(p8)-1)*N+m(p9)))
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
i2(((((m(h7)-1)*N+m(h11)-1)*N+m(h1)-1)*N+m(h2)))=i2(((((m(h7)-1)*N+m(h11)-1)*N+m(h1)-1)*N+m(h2)))+(-1.0d0)*t1(((m(p8)-1)*N+m(h1)))*&
&i3(((((m(h7)-1)*N+m(h11)-1)*N+m(h2)-1)*N+m(p8)))+(1.0d0)*t1(((m(p8)-1)*N+m(h2)))*i3(((((m(h7)-1)*N+m(h11)-1)*N+m(h1)-1)*N+m(p8)))
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
i2(((((m(h7)-1)*N+m(h11)-1)*N+m(h1)-1)*N+m(h2)))=i2(((((m(h7)-1)*N+m(h11)-1)*N+m(h1)-1)*N+m(h2)))+(1.0d0)*t2(((((m(p8)-1)*N+m(p9)-1&
&)*N+m(h1)-1)*N+m(h2)))*v2(((((m(h7)-1)*N+m(h11)-1)*N+m(p8)-1)*N+m(p9)))
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
i2e(((((m(h11)-1)*N+m(h7)-1)*N+m(h1)-1)*N+m(h2)))= - i2(((((m(h7)-1)*N+m(h11)-1)*N+m(h1)-1)*N+m(h2)))
i2e(((((m(h7)-1)*N+m(h11)-1)*N+m(h1)-1)*N+m(h2)))= + i2(((((m(h7)-1)*N+m(h11)-1)*N+m(h1)-1)*N+m(h2)))
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
i1(((((m(h11)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))=i1(((((m(h11)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))+(1.0d0)*t1(((m(p4)-1)*N+m(h7)))*i&
&2e(((((m(h7)-1)*N+m(h11)-1)*N+m(h1)-1)*N+m(h2)))
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h11=1,nocc
DO h1=1,nocc
DO p9=nocc+1,N
v2e(((((m(h11)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(p9)))= + v2(((((m(h11)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(p9)))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h11=1,nocc
DO h1=1,nocc
DO p9=nocc+1,N
i2(((((m(h11)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(p9)))=(1.0d0)*v2e(((((m(h11)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(p9)))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h11=1,nocc
DO p9=nocc+1,N
DO p8=nocc+1,N
IF (p8>p9) CYCLE
v2e(((((m(h11)-1)*N+m(p4)-1)*N+m(p9)-1)*N+m(p8)))= - v2(((((m(h11)-1)*N+m(p4)-1)*N+m(p8)-1)*N+m(p9)))
v2e(((((m(h11)-1)*N+m(p4)-1)*N+m(p8)-1)*N+m(p9)))= + v2(((((m(h11)-1)*N+m(p4)-1)*N+m(p8)-1)*N+m(p9)))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h11=1,nocc
DO h1=1,nocc
DO p9=nocc+1,N
DO p8=nocc+1,N
i2(((((m(h11)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(p9)))=i2(((((m(h11)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(p9)))+(0.5d0)*t1(((m(p8)-1)*N+m(h1)))*v&
&2e(((((m(h11)-1)*N+m(p4)-1)*N+m(p8)-1)*N+m(p9)))
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
i1(((((m(h11)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))=i1(((((m(h11)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))+(-1.0d0)*t1(((m(p9)-1)*N+m(h1)))*&
&i2(((((m(h11)-1)*N+m(p4)-1)*N+m(h2)-1)*N+m(p9)))+(1.0d0)*t1(((m(p9)-1)*N+m(h2)))*i2(((((m(h11)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(p9)))
END DO
END DO
END DO
END DO
END DO
DO h11=1,nocc
DO p12=nocc+1,N
i2(((m(h11)-1)*N+m(p12)))=(1.0d0)*f1(((m(h11)-1)*N+m(p12)))
END DO
END DO
DO h11=1,nocc
DO h10=1,nocc
IF (h10>h11) CYCLE
DO p12=nocc+1,N
DO p9=nocc+1,N
IF (p9>p12) CYCLE
v2e(((((m(h11)-1)*N+m(h10)-1)*N+m(p12)-1)*N+m(p9)))= + v2(((((m(h10)-1)*N+m(h11)-1)*N+m(p9)-1)*N+m(p12)))
v2e(((((m(h11)-1)*N+m(h10)-1)*N+m(p9)-1)*N+m(p12)))= - v2(((((m(h10)-1)*N+m(h11)-1)*N+m(p9)-1)*N+m(p12)))
v2e(((((m(h10)-1)*N+m(h11)-1)*N+m(p12)-1)*N+m(p9)))= - v2(((((m(h10)-1)*N+m(h11)-1)*N+m(p9)-1)*N+m(p12)))
v2e(((((m(h10)-1)*N+m(h11)-1)*N+m(p9)-1)*N+m(p12)))= + v2(((((m(h10)-1)*N+m(h11)-1)*N+m(p9)-1)*N+m(p12)))
END DO
END DO
END DO
END DO
DO h11=1,nocc
DO p12=nocc+1,N
DO p9=nocc+1,N
DO h10=1,nocc
i2(((m(h11)-1)*N+m(p12)))=i2(((m(h11)-1)*N+m(p12)))+(1.0d0)*t1(((m(p9)-1)*N+m(h10)))*v2e(((((m(h10)-1)*N+m(h11)-1)*N+m(p9)-1)*N+m(p&
&12)))
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
t2e(((((m(p12)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))= - t2(((((m(p4)-1)*N+m(p12)-1)*N+m(h1)-1)*N+m(h2)))
t2e(((((m(p4)-1)*N+m(p12)-1)*N+m(h1)-1)*N+m(h2)))= + t2(((((m(p4)-1)*N+m(p12)-1)*N+m(h1)-1)*N+m(h2)))
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
i1(((((m(h11)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))=i1(((((m(h11)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))+(-1.0d0)*t2e(((((m(p4)-1)*N+m(p12&
&)-1)*N+m(h1)-1)*N+m(h2)))*i2(((m(h11)-1)*N+m(p12)))
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
v2e(((((m(h9)-1)*N+m(h11)-1)*N+m(h1)-1)*N+m(p8)))= + v2(((((m(h9)-1)*N+m(h11)-1)*N+m(h1)-1)*N+m(p8)))
END DO
END DO
END DO
END DO
DO h9=1,nocc
DO h11=1,nocc
IF (h9>h11) CYCLE
DO h1=1,nocc
DO p8=nocc+1,N
i2(((((m(h9)-1)*N+m(h11)-1)*N+m(h1)-1)*N+m(p8)))=(1.0d0)*v2e(((((m(h9)-1)*N+m(h11)-1)*N+m(h1)-1)*N+m(p8)))
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
v2e(((((m(h9)-1)*N+m(h11)-1)*N+m(p10)-1)*N+m(p8)))= - v2(((((m(h9)-1)*N+m(h11)-1)*N+m(p8)-1)*N+m(p10)))
v2e(((((m(h9)-1)*N+m(h11)-1)*N+m(p8)-1)*N+m(p10)))= + v2(((((m(h9)-1)*N+m(h11)-1)*N+m(p8)-1)*N+m(p10)))
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
i2(((((m(h9)-1)*N+m(h11)-1)*N+m(h1)-1)*N+m(p8)))=i2(((((m(h9)-1)*N+m(h11)-1)*N+m(h1)-1)*N+m(p8)))+(-1.0d0)*t1(((m(p10)-1)*N+m(h1)))&
&*v2e(((((m(h9)-1)*N+m(h11)-1)*N+m(p8)-1)*N+m(p10)))
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
t2e(((((m(p8)-1)*N+m(p4)-1)*N+m(h9)-1)*N+m(h1)))= + t2(((((m(p4)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(h9)))
t2e(((((m(p8)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h9)))= - t2(((((m(p4)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(h9)))
t2e(((((m(p4)-1)*N+m(p8)-1)*N+m(h9)-1)*N+m(h1)))= - t2(((((m(p4)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(h9)))
t2e(((((m(p4)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(h9)))= + t2(((((m(p4)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(h9)))
END DO
END DO
END DO
END DO
DO h9=1,nocc
DO h11=1,nocc
IF (h9>h11) CYCLE
DO h2=1,nocc
DO p8=nocc+1,N
i2e(((((m(h11)-1)*N+m(h9)-1)*N+m(h2)-1)*N+m(p8)))= - i2(((((m(h9)-1)*N+m(h11)-1)*N+m(h2)-1)*N+m(p8)))
i2e(((((m(h9)-1)*N+m(h11)-1)*N+m(h2)-1)*N+m(p8)))= + i2(((((m(h9)-1)*N+m(h11)-1)*N+m(h2)-1)*N+m(p8)))
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
i1(((((m(h11)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))=i1(((((m(h11)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))+(1.0d0)*t2e(((((m(p4)-1)*N+m(p8)-&
&1)*N+m(h1)-1)*N+m(h9)))*i2e(((((m(h9)-1)*N+m(h11)-1)*N+m(h2)-1)*N+m(p8)))+(-1.0d0)*t2e(((((m(p4)-1)*N+m(p8)-1)*N+m(h2)-1)*N+m(h9))&
&)*i2e(((((m(h9)-1)*N+m(h11)-1)*N+m(h1)-1)*N+m(p8)))
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
v2e(((((m(h11)-1)*N+m(p4)-1)*N+m(p8)-1)*N+m(p9)))= + v2(((((m(h11)-1)*N+m(p4)-1)*N+m(p8)-1)*N+m(p9)))
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
i1(((((m(h11)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))=i1(((((m(h11)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))+(1.0d0)*t2(((((m(p8)-1)*N+m(p9)-1&
&)*N+m(h1)-1)*N+m(h2)))*v2e(((((m(h11)-1)*N+m(p4)-1)*N+m(p8)-1)*N+m(p9)))
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
t3e(((((((m(p7)-1)*N+m(p4)-1)*N+m(p8)-1)*N+m(h2)-1)*N+m(h9)-1)*N+m(h1)))= - t3(((((((m(p4)-1)*N+m(p7)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(&
&h2)-1)*N+m(h9)))
t3e(((((((m(p7)-1)*N+m(p4)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(h9)-1)*N+m(h2)))= + t3(((((((m(p4)-1)*N+m(p7)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(&
&h2)-1)*N+m(h9)))
t3e(((((((m(p7)-1)*N+m(p4)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h9)))= - t3(((((((m(p4)-1)*N+m(p7)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(&
&h2)-1)*N+m(h9)))
t3e(((((((m(p8)-1)*N+m(p4)-1)*N+m(p7)-1)*N+m(h2)-1)*N+m(h9)-1)*N+m(h1)))= + t3(((((((m(p4)-1)*N+m(p7)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(&
&h2)-1)*N+m(h9)))
t3e(((((((m(p8)-1)*N+m(p4)-1)*N+m(p7)-1)*N+m(h1)-1)*N+m(h9)-1)*N+m(h2)))= - t3(((((((m(p4)-1)*N+m(p7)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(&
&h2)-1)*N+m(h9)))
t3e(((((((m(p8)-1)*N+m(p4)-1)*N+m(p7)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h9)))= + t3(((((((m(p4)-1)*N+m(p7)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(&
&h2)-1)*N+m(h9)))
t3e(((((((m(p4)-1)*N+m(p7)-1)*N+m(p8)-1)*N+m(h2)-1)*N+m(h9)-1)*N+m(h1)))= + t3(((((((m(p4)-1)*N+m(p7)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(&
&h2)-1)*N+m(h9)))
t3e(((((((m(p4)-1)*N+m(p7)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(h9)-1)*N+m(h2)))= - t3(((((((m(p4)-1)*N+m(p7)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(&
&h2)-1)*N+m(h9)))
t3e(((((((m(p4)-1)*N+m(p7)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h9)))= + t3(((((((m(p4)-1)*N+m(p7)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(&
&h2)-1)*N+m(h9)))
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
v2e(((((m(h11)-1)*N+m(h9)-1)*N+m(p7)-1)*N+m(p8)))= - v2(((((m(h9)-1)*N+m(h11)-1)*N+m(p7)-1)*N+m(p8)))
v2e(((((m(h9)-1)*N+m(h11)-1)*N+m(p7)-1)*N+m(p8)))= + v2(((((m(h9)-1)*N+m(h11)-1)*N+m(p7)-1)*N+m(p8)))
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
i1(((((m(h11)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))=i1(((((m(h11)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))+(1.0d0)*t3e(((((((m(p4)-1)*N+m(p7&
&)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h9)))*v2e(((((m(h9)-1)*N+m(h11)-1)*N+m(p7)-1)*N+m(p8)))
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
t2e(((((m(p4)-1)*N+m(p5)-1)*N+m(h11)-1)*N+m(h1)))= - t2(((((m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h11)))
t2e(((((m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h11)))= + t2(((((m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h11)))
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
i0(((((((m(p4)-1)*N+m(p5)-1)*N+m(p6)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h3)))=i0(((((((m(p4)-1)*N+m(p5)-1)*N+m(p6)-1)*N+m(h1)-1)*N+m(h2)-&
&1)*N+m(h3)))+(-1.0d0)*t2e(((((m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h11)))*i1(((((m(h11)-1)*N+m(p6)-1)*N+m(h2)-1)*N+m(h3)))+(1.0d0)*t&
&2e(((((m(p4)-1)*N+m(p5)-1)*N+m(h2)-1)*N+m(h11)))*i1(((((m(h11)-1)*N+m(p6)-1)*N+m(h1)-1)*N+m(h3)))+(-1.0d0)*t2e(((((m(p4)-1)*N+m(p5&
&)-1)*N+m(h3)-1)*N+m(h11)))*i1(((((m(h11)-1)*N+m(p6)-1)*N+m(h1)-1)*N+m(h2)))+(-1.0d0)*t2e(((((m(p5)-1)*N+m(p6)-1)*N+m(h1)-1)*N+m(h1&
&1)))*i1(((((m(h11)-1)*N+m(p4)-1)*N+m(h2)-1)*N+m(h3)))+(1.0d0)*t2e(((((m(p5)-1)*N+m(p6)-1)*N+m(h2)-1)*N+m(h11)))*i1(((((m(h11)-1)*N&
&+m(p4)-1)*N+m(h1)-1)*N+m(h3)))+(-1.0d0)*t2e(((((m(p5)-1)*N+m(p6)-1)*N+m(h3)-1)*N+m(h11)))*i1(((((m(h11)-1)*N+m(p4)-1)*N+m(h1)-1)*N&
&+m(h2)))+(1.0d0)*t2e(((((m(p4)-1)*N+m(p6)-1)*N+m(h1)-1)*N+m(h11)))*i1(((((m(h11)-1)*N+m(p5)-1)*N+m(h2)-1)*N+m(h3)))+(-1.0d0)*t2e((&
&(((m(p4)-1)*N+m(p6)-1)*N+m(h2)-1)*N+m(h11)))*i1(((((m(h11)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h3)))+(1.0d0)*t2e(((((m(p4)-1)*N+m(p6)-1)*&
&N+m(h3)-1)*N+m(h11)))*i1(((((m(h11)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h2)))
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
v2e(((((m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(p12)))= + v2(((((m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(p12)))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p5=nocc+1,N
IF (p4>p5) CYCLE
DO h1=1,nocc
DO p12=nocc+1,N
i1(((((m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(p12)))=(1.0d0)*v2e(((((m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(p12)))
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
v2e(((((m(p4)-1)*N+m(p5)-1)*N+m(p12)-1)*N+m(p8)))= - v2(((((m(p4)-1)*N+m(p5)-1)*N+m(p8)-1)*N+m(p12)))
v2e(((((m(p4)-1)*N+m(p5)-1)*N+m(p8)-1)*N+m(p12)))= + v2(((((m(p4)-1)*N+m(p5)-1)*N+m(p8)-1)*N+m(p12)))
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
i1(((((m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(p12)))=i1(((((m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(p12)))+(1.0d0)*t1(((m(p8)-1)*N+m(h1)))*v&
&2e(((((m(p4)-1)*N+m(p5)-1)*N+m(p8)-1)*N+m(p12)))
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
v2e(((((m(h8)-1)*N+m(h9)-1)*N+m(h1)-1)*N+m(p12)))= + v2(((((m(h8)-1)*N+m(h9)-1)*N+m(h1)-1)*N+m(p12)))
END DO
END DO
END DO
END DO
DO h8=1,nocc
DO h9=1,nocc
IF (h8>h9) CYCLE
DO h1=1,nocc
DO p12=nocc+1,N
i2(((((m(h8)-1)*N+m(h9)-1)*N+m(h1)-1)*N+m(p12)))=(1.0d0)*v2e(((((m(h8)-1)*N+m(h9)-1)*N+m(h1)-1)*N+m(p12)))
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
v2e(((((m(h8)-1)*N+m(h9)-1)*N+m(p12)-1)*N+m(p10)))= - v2(((((m(h8)-1)*N+m(h9)-1)*N+m(p10)-1)*N+m(p12)))
v2e(((((m(h8)-1)*N+m(h9)-1)*N+m(p10)-1)*N+m(p12)))= + v2(((((m(h8)-1)*N+m(h9)-1)*N+m(p10)-1)*N+m(p12)))
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
i2(((((m(h8)-1)*N+m(h9)-1)*N+m(h1)-1)*N+m(p12)))=i2(((((m(h8)-1)*N+m(h9)-1)*N+m(h1)-1)*N+m(p12)))+(1.0d0)*t1(((m(p10)-1)*N+m(h1)))*&
&v2e(((((m(h8)-1)*N+m(h9)-1)*N+m(p10)-1)*N+m(p12)))
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
i1(((((m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(p12)))=i1(((((m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(p12)))+(1.0d0)*t2(((((m(p4)-1)*N+m(p5)-1&
&)*N+m(h8)-1)*N+m(h9)))*i2(((((m(h8)-1)*N+m(h9)-1)*N+m(h1)-1)*N+m(p12)))
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
t2e(((((m(p8)-1)*N+m(p4)-1)*N+m(h9)-1)*N+m(h1)))= + t2(((((m(p4)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(h9)))
t2e(((((m(p8)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h9)))= - t2(((((m(p4)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(h9)))
t2e(((((m(p4)-1)*N+m(p8)-1)*N+m(h9)-1)*N+m(h1)))= - t2(((((m(p4)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(h9)))
t2e(((((m(p4)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(h9)))= + t2(((((m(p4)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(h9)))
END DO
END DO
END DO
END DO
DO p5=nocc+1,N
DO h9=1,nocc
DO p12=nocc+1,N
DO p8=nocc+1,N
IF (p8>p12) CYCLE
v2e(((((m(h9)-1)*N+m(p5)-1)*N+m(p12)-1)*N+m(p8)))= - v2(((((m(h9)-1)*N+m(p5)-1)*N+m(p8)-1)*N+m(p12)))
v2e(((((m(h9)-1)*N+m(p5)-1)*N+m(p8)-1)*N+m(p12)))= + v2(((((m(h9)-1)*N+m(p5)-1)*N+m(p8)-1)*N+m(p12)))
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
i1(((((m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(p12)))=i1(((((m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(p12)))+(1.0d0)*t2e(((((m(p4)-1)*N+m(p8)-&
&1)*N+m(h1)-1)*N+m(h9)))*v2e(((((m(h9)-1)*N+m(p5)-1)*N+m(p8)-1)*N+m(p12)))+(-1.0d0)*t2e(((((m(p5)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(h9))&
&)*v2e(((((m(h9)-1)*N+m(p4)-1)*N+m(p8)-1)*N+m(p12)))
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
t3e(((((((m(p5)-1)*N+m(p7)-1)*N+m(p4)-1)*N+m(h8)-1)*N+m(h1)-1)*N+m(h9)))= - t3(((((((m(p4)-1)*N+m(p5)-1)*N+m(p7)-1)*N+m(h1)-1)*N+m(&
&h8)-1)*N+m(h9)))
t3e(((((((m(p5)-1)*N+m(p7)-1)*N+m(p4)-1)*N+m(h9)-1)*N+m(h1)-1)*N+m(h8)))= + t3(((((((m(p4)-1)*N+m(p5)-1)*N+m(p7)-1)*N+m(h1)-1)*N+m(&
&h8)-1)*N+m(h9)))
t3e(((((((m(p5)-1)*N+m(p7)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h8)-1)*N+m(h9)))= + t3(((((((m(p4)-1)*N+m(p5)-1)*N+m(p7)-1)*N+m(h1)-1)*N+m(&
&h8)-1)*N+m(h9)))
t3e(((((((m(p4)-1)*N+m(p7)-1)*N+m(p5)-1)*N+m(h8)-1)*N+m(h1)-1)*N+m(h9)))= + t3(((((((m(p4)-1)*N+m(p5)-1)*N+m(p7)-1)*N+m(h1)-1)*N+m(&
&h8)-1)*N+m(h9)))
t3e(((((((m(p4)-1)*N+m(p7)-1)*N+m(p5)-1)*N+m(h9)-1)*N+m(h1)-1)*N+m(h8)))= - t3(((((((m(p4)-1)*N+m(p5)-1)*N+m(p7)-1)*N+m(h1)-1)*N+m(&
&h8)-1)*N+m(h9)))
t3e(((((((m(p4)-1)*N+m(p7)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h8)-1)*N+m(h9)))= - t3(((((((m(p4)-1)*N+m(p5)-1)*N+m(p7)-1)*N+m(h1)-1)*N+m(&
&h8)-1)*N+m(h9)))
t3e(((((((m(p4)-1)*N+m(p5)-1)*N+m(p7)-1)*N+m(h8)-1)*N+m(h1)-1)*N+m(h9)))= - t3(((((((m(p4)-1)*N+m(p5)-1)*N+m(p7)-1)*N+m(h1)-1)*N+m(&
&h8)-1)*N+m(h9)))
t3e(((((((m(p4)-1)*N+m(p5)-1)*N+m(p7)-1)*N+m(h9)-1)*N+m(h1)-1)*N+m(h8)))= + t3(((((((m(p4)-1)*N+m(p5)-1)*N+m(p7)-1)*N+m(h1)-1)*N+m(&
&h8)-1)*N+m(h9)))
t3e(((((((m(p4)-1)*N+m(p5)-1)*N+m(p7)-1)*N+m(h1)-1)*N+m(h8)-1)*N+m(h9)))= + t3(((((((m(p4)-1)*N+m(p5)-1)*N+m(p7)-1)*N+m(h1)-1)*N+m(&
&h8)-1)*N+m(h9)))
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
v2e(((((m(h8)-1)*N+m(h9)-1)*N+m(p12)-1)*N+m(p7)))= - v2(((((m(h8)-1)*N+m(h9)-1)*N+m(p7)-1)*N+m(p12)))
v2e(((((m(h8)-1)*N+m(h9)-1)*N+m(p7)-1)*N+m(p12)))= + v2(((((m(h8)-1)*N+m(h9)-1)*N+m(p7)-1)*N+m(p12)))
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
i1(((((m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(p12)))=i1(((((m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(p12)))+(1.0d0)*t3e(((((((m(p4)-1)*N+m(p5&
&)-1)*N+m(p7)-1)*N+m(h1)-1)*N+m(h8)-1)*N+m(h9)))*v2e(((((m(h8)-1)*N+m(h9)-1)*N+m(p7)-1)*N+m(p12)))
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
t2e(((((m(p12)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))= - t2(((((m(p4)-1)*N+m(p12)-1)*N+m(h1)-1)*N+m(h2)))
t2e(((((m(p4)-1)*N+m(p12)-1)*N+m(h1)-1)*N+m(h2)))= + t2(((((m(p4)-1)*N+m(p12)-1)*N+m(h1)-1)*N+m(h2)))
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
i0(((((((m(p4)-1)*N+m(p5)-1)*N+m(p6)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h3)))=i0(((((((m(p4)-1)*N+m(p5)-1)*N+m(p6)-1)*N+m(h1)-1)*N+m(h2)-&
&1)*N+m(h3)))+(-1.0d0)*t2e(((((m(p4)-1)*N+m(p12)-1)*N+m(h1)-1)*N+m(h2)))*i1(((((m(p5)-1)*N+m(p6)-1)*N+m(h3)-1)*N+m(p12)))+(-1.0d0)*&
&t2e(((((m(p4)-1)*N+m(p12)-1)*N+m(h2)-1)*N+m(h3)))*i1(((((m(p5)-1)*N+m(p6)-1)*N+m(h1)-1)*N+m(p12)))+(1.0d0)*t2e(((((m(p4)-1)*N+m(p1&
&2)-1)*N+m(h1)-1)*N+m(h3)))*i1(((((m(p5)-1)*N+m(p6)-1)*N+m(h2)-1)*N+m(p12)))+(1.0d0)*t2e(((((m(p5)-1)*N+m(p12)-1)*N+m(h1)-1)*N+m(h2&
&)))*i1(((((m(p4)-1)*N+m(p6)-1)*N+m(h3)-1)*N+m(p12)))+(1.0d0)*t2e(((((m(p5)-1)*N+m(p12)-1)*N+m(h2)-1)*N+m(h3)))*i1(((((m(p4)-1)*N+m&
&(p6)-1)*N+m(h1)-1)*N+m(p12)))+(-1.0d0)*t2e(((((m(p5)-1)*N+m(p12)-1)*N+m(h1)-1)*N+m(h3)))*i1(((((m(p4)-1)*N+m(p6)-1)*N+m(h2)-1)*N+m&
&(p12)))+(-1.0d0)*t2e(((((m(p6)-1)*N+m(p12)-1)*N+m(h1)-1)*N+m(h2)))*i1(((((m(p4)-1)*N+m(p5)-1)*N+m(h3)-1)*N+m(p12)))+(-1.0d0)*t2e((&
&(((m(p6)-1)*N+m(p12)-1)*N+m(h2)-1)*N+m(h3)))*i1(((((m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(p12)))+(1.0d0)*t2e(((((m(p6)-1)*N+m(p12)-1)&
&*N+m(h1)-1)*N+m(h3)))*i1(((((m(p4)-1)*N+m(p5)-1)*N+m(h2)-1)*N+m(p12)))
END DO
END DO
END DO
END DO
END DO
END DO
END DO
DO h11=1,nocc
DO h1=1,nocc
i1(((m(h11)-1)*N+m(h1)))=(1.0d0)*f1(((m(h11)-1)*N+m(h1)))
END DO
END DO
DO h11=1,nocc
DO p10=nocc+1,N
i2(((m(h11)-1)*N+m(p10)))=(1.0d0)*f1(((m(h11)-1)*N+m(p10)))
END DO
END DO
DO h11=1,nocc
DO h9=1,nocc
IF (h9>h11) CYCLE
DO p10=nocc+1,N
DO p8=nocc+1,N
IF (p8>p10) CYCLE
v2e(((((m(h11)-1)*N+m(h9)-1)*N+m(p10)-1)*N+m(p8)))= + v2(((((m(h9)-1)*N+m(h11)-1)*N+m(p8)-1)*N+m(p10)))
v2e(((((m(h11)-1)*N+m(h9)-1)*N+m(p8)-1)*N+m(p10)))= - v2(((((m(h9)-1)*N+m(h11)-1)*N+m(p8)-1)*N+m(p10)))
v2e(((((m(h9)-1)*N+m(h11)-1)*N+m(p10)-1)*N+m(p8)))= - v2(((((m(h9)-1)*N+m(h11)-1)*N+m(p8)-1)*N+m(p10)))
v2e(((((m(h9)-1)*N+m(h11)-1)*N+m(p8)-1)*N+m(p10)))= + v2(((((m(h9)-1)*N+m(h11)-1)*N+m(p8)-1)*N+m(p10)))
END DO
END DO
END DO
END DO
DO h11=1,nocc
DO p10=nocc+1,N
DO p8=nocc+1,N
DO h9=1,nocc
i2(((m(h11)-1)*N+m(p10)))=i2(((m(h11)-1)*N+m(p10)))+(1.0d0)*t1(((m(p8)-1)*N+m(h9)))*v2e(((((m(h9)-1)*N+m(h11)-1)*N+m(p8)-1)*N+m(p10&
&)))
END DO
END DO
END DO
END DO
DO h11=1,nocc
DO h1=1,nocc
DO p10=nocc+1,N
i1(((m(h11)-1)*N+m(h1)))=i1(((m(h11)-1)*N+m(h1)))+(1.0d0)*t1(((m(p10)-1)*N+m(h1)))*i2(((m(h11)-1)*N+m(p10)))
END DO
END DO
END DO
DO h11=1,nocc
DO h9=1,nocc
IF (h9>h11) CYCLE
DO h1=1,nocc
DO p8=nocc+1,N
v2e(((((m(h11)-1)*N+m(h9)-1)*N+m(h1)-1)*N+m(p8)))= - v2(((((m(h9)-1)*N+m(h11)-1)*N+m(h1)-1)*N+m(p8)))
v2e(((((m(h9)-1)*N+m(h11)-1)*N+m(h1)-1)*N+m(p8)))= + v2(((((m(h9)-1)*N+m(h11)-1)*N+m(h1)-1)*N+m(p8)))
END DO
END DO
END DO
END DO
DO h11=1,nocc
DO h1=1,nocc
DO p8=nocc+1,N
DO h9=1,nocc
i1(((m(h11)-1)*N+m(h1)))=i1(((m(h11)-1)*N+m(h1)))+(-1.0d0)*t1(((m(p8)-1)*N+m(h9)))*v2e(((((m(h9)-1)*N+m(h11)-1)*N+m(h1)-1)*N+m(p8)))
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
t2e(((((m(p8)-1)*N+m(p9)-1)*N+m(h10)-1)*N+m(h1)))= - t2(((((m(p8)-1)*N+m(p9)-1)*N+m(h1)-1)*N+m(h10)))
t2e(((((m(p8)-1)*N+m(p9)-1)*N+m(h1)-1)*N+m(h10)))= + t2(((((m(p8)-1)*N+m(p9)-1)*N+m(h1)-1)*N+m(h10)))
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
v2e(((((m(h11)-1)*N+m(h10)-1)*N+m(p8)-1)*N+m(p9)))= - v2(((((m(h10)-1)*N+m(h11)-1)*N+m(p8)-1)*N+m(p9)))
v2e(((((m(h10)-1)*N+m(h11)-1)*N+m(p8)-1)*N+m(p9)))= + v2(((((m(h10)-1)*N+m(h11)-1)*N+m(p8)-1)*N+m(p9)))
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
i1(((m(h11)-1)*N+m(h1)))=i1(((m(h11)-1)*N+m(h1)))+(-1.0d0)*t2e(((((m(p8)-1)*N+m(p9)-1)*N+m(h1)-1)*N+m(h10)))*v2e(((((m(h10)-1)*N+m(&
&h11)-1)*N+m(p8)-1)*N+m(p9)))
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
t3e(((((((m(p4)-1)*N+m(p5)-1)*N+m(p6)-1)*N+m(h2)-1)*N+m(h11)-1)*N+m(h1)))= + t3(((((((m(p4)-1)*N+m(p5)-1)*N+m(p6)-1)*N+m(h1)-1)*N+m&
&(h2)-1)*N+m(h11)))
t3e(((((((m(p4)-1)*N+m(p5)-1)*N+m(p6)-1)*N+m(h1)-1)*N+m(h11)-1)*N+m(h2)))= - t3(((((((m(p4)-1)*N+m(p5)-1)*N+m(p6)-1)*N+m(h1)-1)*N+m&
&(h2)-1)*N+m(h11)))
t3e(((((((m(p4)-1)*N+m(p5)-1)*N+m(p6)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h11)))= + t3(((((((m(p4)-1)*N+m(p5)-1)*N+m(p6)-1)*N+m(h1)-1)*N+m&
&(h2)-1)*N+m(h11)))
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
i0(((((((m(p4)-1)*N+m(p5)-1)*N+m(p6)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h3)))=i0(((((((m(p4)-1)*N+m(p5)-1)*N+m(p6)-1)*N+m(h1)-1)*N+m(h2)-&
&1)*N+m(h3)))+(-1.0d0)*t3e(((((((m(p4)-1)*N+m(p5)-1)*N+m(p6)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h11)))*i1(((m(h11)-1)*N+m(h3)))+(-1.0d0)*&
&t3e(((((((m(p4)-1)*N+m(p5)-1)*N+m(p6)-1)*N+m(h2)-1)*N+m(h3)-1)*N+m(h11)))*i1(((m(h11)-1)*N+m(h1)))+(1.0d0)*t3e(((((((m(p4)-1)*N+m(&
&p5)-1)*N+m(p6)-1)*N+m(h1)-1)*N+m(h3)-1)*N+m(h11)))*i1(((m(h11)-1)*N+m(h2)))
END DO
END DO
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p7=nocc+1,N
i1(((m(p4)-1)*N+m(p7)))=(1.0d0)*f1(((m(p4)-1)*N+m(p7)))
END DO
END DO
DO p4=nocc+1,N
DO h9=1,nocc
DO p7=nocc+1,N
DO p8=nocc+1,N
IF (p7>p8) CYCLE
v2e(((((m(h9)-1)*N+m(p4)-1)*N+m(p8)-1)*N+m(p7)))= - v2(((((m(h9)-1)*N+m(p4)-1)*N+m(p7)-1)*N+m(p8)))
v2e(((((m(h9)-1)*N+m(p4)-1)*N+m(p7)-1)*N+m(p8)))= + v2(((((m(h9)-1)*N+m(p4)-1)*N+m(p7)-1)*N+m(p8)))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p7=nocc+1,N
DO p8=nocc+1,N
DO h9=1,nocc
i1(((m(p4)-1)*N+m(p7)))=i1(((m(p4)-1)*N+m(p7)))+(-1.0d0)*t1(((m(p8)-1)*N+m(h9)))*v2e(((((m(h9)-1)*N+m(p4)-1)*N+m(p7)-1)*N+m(p8)))
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
t2e(((((m(p8)-1)*N+m(p4)-1)*N+m(h9)-1)*N+m(h10)))= - t2(((((m(p4)-1)*N+m(p8)-1)*N+m(h9)-1)*N+m(h10)))
t2e(((((m(p4)-1)*N+m(p8)-1)*N+m(h9)-1)*N+m(h10)))= + t2(((((m(p4)-1)*N+m(p8)-1)*N+m(h9)-1)*N+m(h10)))
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
v2e(((((m(h9)-1)*N+m(h10)-1)*N+m(p8)-1)*N+m(p7)))= - v2(((((m(h9)-1)*N+m(h10)-1)*N+m(p7)-1)*N+m(p8)))
v2e(((((m(h9)-1)*N+m(h10)-1)*N+m(p7)-1)*N+m(p8)))= + v2(((((m(h9)-1)*N+m(h10)-1)*N+m(p7)-1)*N+m(p8)))
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
i1(((m(p4)-1)*N+m(p7)))=i1(((m(p4)-1)*N+m(p7)))+(-1.0d0)*t2e(((((m(p4)-1)*N+m(p8)-1)*N+m(h9)-1)*N+m(h10)))*v2e(((((m(h9)-1)*N+m(h10&
&)-1)*N+m(p7)-1)*N+m(p8)))
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
t3e(((((((m(p5)-1)*N+m(p7)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h3)))= + t3(((((((m(p4)-1)*N+m(p5)-1)*N+m(p7)-1)*N+m(h1)-1)*N+m(&
&h2)-1)*N+m(h3)))
t3e(((((((m(p4)-1)*N+m(p7)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h3)))= - t3(((((((m(p4)-1)*N+m(p5)-1)*N+m(p7)-1)*N+m(h1)-1)*N+m(&
&h2)-1)*N+m(h3)))
t3e(((((((m(p4)-1)*N+m(p5)-1)*N+m(p7)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h3)))= + t3(((((((m(p4)-1)*N+m(p5)-1)*N+m(p7)-1)*N+m(h1)-1)*N+m(&
&h2)-1)*N+m(h3)))
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
i0(((((((m(p4)-1)*N+m(p5)-1)*N+m(p6)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h3)))=i0(((((((m(p4)-1)*N+m(p5)-1)*N+m(p6)-1)*N+m(h1)-1)*N+m(h2)-&
&1)*N+m(h3)))+(1.0d0)*t3e(((((((m(p4)-1)*N+m(p5)-1)*N+m(p7)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h3)))*i1(((m(p6)-1)*N+m(p7)))+(1.0d0)*t3e(&
&((((((m(p5)-1)*N+m(p6)-1)*N+m(p7)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h3)))*i1(((m(p4)-1)*N+m(p7)))+(-1.0d0)*t3e(((((((m(p4)-1)*N+m(p6)-1&
&)*N+m(p7)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h3)))*i1(((m(p5)-1)*N+m(p7)))
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
i1(((((m(h11)-1)*N+m(h12)-1)*N+m(h1)-1)*N+m(h2)))=(1.0d0)*v2(((((m(h11)-1)*N+m(h12)-1)*N+m(h1)-1)*N+m(h2)))
END DO
END DO
END DO
END DO
DO h11=1,nocc
DO h12=1,nocc
IF (h11>h12) CYCLE
DO h1=1,nocc
DO p10=nocc+1,N
v2e(((((m(h11)-1)*N+m(h12)-1)*N+m(h1)-1)*N+m(p10)))= + v2(((((m(h11)-1)*N+m(h12)-1)*N+m(h1)-1)*N+m(p10)))
END DO
END DO
END DO
END DO
DO h11=1,nocc
DO h12=1,nocc
IF (h11>h12) CYCLE
DO h1=1,nocc
DO p10=nocc+1,N
i2(((((m(h11)-1)*N+m(h12)-1)*N+m(h1)-1)*N+m(p10)))=(1.0d0)*v2e(((((m(h11)-1)*N+m(h12)-1)*N+m(h1)-1)*N+m(p10)))
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
v2e(((((m(h11)-1)*N+m(h12)-1)*N+m(p10)-1)*N+m(p8)))= - v2(((((m(h11)-1)*N+m(h12)-1)*N+m(p8)-1)*N+m(p10)))
v2e(((((m(h11)-1)*N+m(h12)-1)*N+m(p8)-1)*N+m(p10)))= + v2(((((m(h11)-1)*N+m(h12)-1)*N+m(p8)-1)*N+m(p10)))
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
i2(((((m(h11)-1)*N+m(h12)-1)*N+m(h1)-1)*N+m(p10)))=i2(((((m(h11)-1)*N+m(h12)-1)*N+m(h1)-1)*N+m(p10)))+(0.5d0)*t1(((m(p8)-1)*N+m(h1)&
&))*v2e(((((m(h11)-1)*N+m(h12)-1)*N+m(p8)-1)*N+m(p10)))
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
i1(((((m(h11)-1)*N+m(h12)-1)*N+m(h1)-1)*N+m(h2)))=i1(((((m(h11)-1)*N+m(h12)-1)*N+m(h1)-1)*N+m(h2)))+(-1.0d0)*t1(((m(p10)-1)*N+m(h1)&
&))*i2(((((m(h11)-1)*N+m(h12)-1)*N+m(h2)-1)*N+m(p10)))+(1.0d0)*t1(((m(p10)-1)*N+m(h2)))*i2(((((m(h11)-1)*N+m(h12)-1)*N+m(h1)-1)*N+m&
&(p10)))
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
i1(((((m(h11)-1)*N+m(h12)-1)*N+m(h1)-1)*N+m(h2)))=i1(((((m(h11)-1)*N+m(h12)-1)*N+m(h1)-1)*N+m(h2)))+(1.0d0)*t2(((((m(p9)-1)*N+m(p10&
&)-1)*N+m(h1)-1)*N+m(h2)))*v2(((((m(h11)-1)*N+m(h12)-1)*N+m(p9)-1)*N+m(p10)))
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
t3e(((((((m(p4)-1)*N+m(p5)-1)*N+m(p6)-1)*N+m(h11)-1)*N+m(h1)-1)*N+m(h12)))= - t3(((((((m(p4)-1)*N+m(p5)-1)*N+m(p6)-1)*N+m(h1)-1)*N+&
&m(h11)-1)*N+m(h12)))
t3e(((((((m(p4)-1)*N+m(p5)-1)*N+m(p6)-1)*N+m(h12)-1)*N+m(h1)-1)*N+m(h11)))= + t3(((((((m(p4)-1)*N+m(p5)-1)*N+m(p6)-1)*N+m(h1)-1)*N+&
&m(h11)-1)*N+m(h12)))
t3e(((((((m(p4)-1)*N+m(p5)-1)*N+m(p6)-1)*N+m(h1)-1)*N+m(h11)-1)*N+m(h12)))= + t3(((((((m(p4)-1)*N+m(p5)-1)*N+m(p6)-1)*N+m(h1)-1)*N+&
&m(h11)-1)*N+m(h12)))
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
i0(((((((m(p4)-1)*N+m(p5)-1)*N+m(p6)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h3)))=i0(((((((m(p4)-1)*N+m(p5)-1)*N+m(p6)-1)*N+m(h1)-1)*N+m(h2)-&
&1)*N+m(h3)))+(1.0d0)*t3e(((((((m(p4)-1)*N+m(p5)-1)*N+m(p6)-1)*N+m(h1)-1)*N+m(h11)-1)*N+m(h12)))*i1(((((m(h11)-1)*N+m(h12)-1)*N+m(h&
&2)-1)*N+m(h3)))+(-1.0d0)*t3e(((((((m(p4)-1)*N+m(p5)-1)*N+m(p6)-1)*N+m(h2)-1)*N+m(h11)-1)*N+m(h12)))*i1(((((m(h11)-1)*N+m(h12)-1)*N&
&+m(h1)-1)*N+m(h3)))+(1.0d0)*t3e(((((((m(p4)-1)*N+m(p5)-1)*N+m(p6)-1)*N+m(h3)-1)*N+m(h11)-1)*N+m(h12)))*i1(((((m(h11)-1)*N+m(h12)-1&
&)*N+m(h1)-1)*N+m(h2)))
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
v2e(((((m(h8)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(p7)))= + v2(((((m(h8)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(p7)))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h8=1,nocc
DO h1=1,nocc
DO p7=nocc+1,N
i1(((((m(h8)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(p7)))=(1.0d0)*v2e(((((m(h8)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(p7)))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h8=1,nocc
DO p7=nocc+1,N
DO p9=nocc+1,N
IF (p7>p9) CYCLE
v2e(((((m(h8)-1)*N+m(p4)-1)*N+m(p9)-1)*N+m(p7)))= - v2(((((m(h8)-1)*N+m(p4)-1)*N+m(p7)-1)*N+m(p9)))
v2e(((((m(h8)-1)*N+m(p4)-1)*N+m(p7)-1)*N+m(p9)))= + v2(((((m(h8)-1)*N+m(p4)-1)*N+m(p7)-1)*N+m(p9)))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h8=1,nocc
DO h1=1,nocc
DO p7=nocc+1,N
DO p9=nocc+1,N
i1(((((m(h8)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(p7)))=i1(((((m(h8)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(p7)))+(-1.0d0)*t1(((m(p9)-1)*N+m(h1)))*v2&
&e(((((m(h8)-1)*N+m(p4)-1)*N+m(p7)-1)*N+m(p9)))
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
t2e(((((m(p9)-1)*N+m(p4)-1)*N+m(h10)-1)*N+m(h1)))= + t2(((((m(p4)-1)*N+m(p9)-1)*N+m(h1)-1)*N+m(h10)))
t2e(((((m(p9)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h10)))= - t2(((((m(p4)-1)*N+m(p9)-1)*N+m(h1)-1)*N+m(h10)))
t2e(((((m(p4)-1)*N+m(p9)-1)*N+m(h10)-1)*N+m(h1)))= - t2(((((m(p4)-1)*N+m(p9)-1)*N+m(h1)-1)*N+m(h10)))
t2e(((((m(p4)-1)*N+m(p9)-1)*N+m(h1)-1)*N+m(h10)))= + t2(((((m(p4)-1)*N+m(p9)-1)*N+m(h1)-1)*N+m(h10)))
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
v2e(((((m(h10)-1)*N+m(h8)-1)*N+m(p9)-1)*N+m(p7)))= + v2(((((m(h8)-1)*N+m(h10)-1)*N+m(p7)-1)*N+m(p9)))
v2e(((((m(h10)-1)*N+m(h8)-1)*N+m(p7)-1)*N+m(p9)))= - v2(((((m(h8)-1)*N+m(h10)-1)*N+m(p7)-1)*N+m(p9)))
v2e(((((m(h8)-1)*N+m(h10)-1)*N+m(p9)-1)*N+m(p7)))= - v2(((((m(h8)-1)*N+m(h10)-1)*N+m(p7)-1)*N+m(p9)))
v2e(((((m(h8)-1)*N+m(h10)-1)*N+m(p7)-1)*N+m(p9)))= + v2(((((m(h8)-1)*N+m(h10)-1)*N+m(p7)-1)*N+m(p9)))
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
i1(((((m(h8)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(p7)))=i1(((((m(h8)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(p7)))+(-1.0d0)*t2e(((((m(p4)-1)*N+m(p9)-1&
&)*N+m(h1)-1)*N+m(h10)))*v2e(((((m(h8)-1)*N+m(h10)-1)*N+m(p7)-1)*N+m(p9)))
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
t3e(((((((m(p5)-1)*N+m(p7)-1)*N+m(p4)-1)*N+m(h2)-1)*N+m(h8)-1)*N+m(h1)))= + t3(((((((m(p4)-1)*N+m(p5)-1)*N+m(p7)-1)*N+m(h1)-1)*N+m(&
&h2)-1)*N+m(h8)))
t3e(((((((m(p5)-1)*N+m(p7)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h8)-1)*N+m(h2)))= - t3(((((((m(p4)-1)*N+m(p5)-1)*N+m(p7)-1)*N+m(h1)-1)*N+m(&
&h2)-1)*N+m(h8)))
t3e(((((((m(p5)-1)*N+m(p7)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h8)))= + t3(((((((m(p4)-1)*N+m(p5)-1)*N+m(p7)-1)*N+m(h1)-1)*N+m(&
&h2)-1)*N+m(h8)))
t3e(((((((m(p4)-1)*N+m(p7)-1)*N+m(p5)-1)*N+m(h2)-1)*N+m(h8)-1)*N+m(h1)))= - t3(((((((m(p4)-1)*N+m(p5)-1)*N+m(p7)-1)*N+m(h1)-1)*N+m(&
&h2)-1)*N+m(h8)))
t3e(((((((m(p4)-1)*N+m(p7)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h8)-1)*N+m(h2)))= + t3(((((((m(p4)-1)*N+m(p5)-1)*N+m(p7)-1)*N+m(h1)-1)*N+m(&
&h2)-1)*N+m(h8)))
t3e(((((((m(p4)-1)*N+m(p7)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h8)))= - t3(((((((m(p4)-1)*N+m(p5)-1)*N+m(p7)-1)*N+m(h1)-1)*N+m(&
&h2)-1)*N+m(h8)))
t3e(((((((m(p4)-1)*N+m(p5)-1)*N+m(p7)-1)*N+m(h2)-1)*N+m(h8)-1)*N+m(h1)))= + t3(((((((m(p4)-1)*N+m(p5)-1)*N+m(p7)-1)*N+m(h1)-1)*N+m(&
&h2)-1)*N+m(h8)))
t3e(((((((m(p4)-1)*N+m(p5)-1)*N+m(p7)-1)*N+m(h1)-1)*N+m(h8)-1)*N+m(h2)))= - t3(((((((m(p4)-1)*N+m(p5)-1)*N+m(p7)-1)*N+m(h1)-1)*N+m(&
&h2)-1)*N+m(h8)))
t3e(((((((m(p4)-1)*N+m(p5)-1)*N+m(p7)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h8)))= + t3(((((((m(p4)-1)*N+m(p5)-1)*N+m(p7)-1)*N+m(h1)-1)*N+m(&
&h2)-1)*N+m(h8)))
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
i0(((((((m(p4)-1)*N+m(p5)-1)*N+m(p6)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h3)))=i0(((((((m(p4)-1)*N+m(p5)-1)*N+m(p6)-1)*N+m(h1)-1)*N+m(h2)-&
&1)*N+m(h3)))+(-1.0d0)*t3e(((((((m(p4)-1)*N+m(p5)-1)*N+m(p7)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h8)))*i1(((((m(h8)-1)*N+m(p6)-1)*N+m(h3)-&
&1)*N+m(p7)))+(-1.0d0)*t3e(((((((m(p4)-1)*N+m(p5)-1)*N+m(p7)-1)*N+m(h2)-1)*N+m(h3)-1)*N+m(h8)))*i1(((((m(h8)-1)*N+m(p6)-1)*N+m(h1)-&
&1)*N+m(p7)))+(1.0d0)*t3e(((((((m(p4)-1)*N+m(p5)-1)*N+m(p7)-1)*N+m(h1)-1)*N+m(h3)-1)*N+m(h8)))*i1(((((m(h8)-1)*N+m(p6)-1)*N+m(h2)-1&
&)*N+m(p7)))+(-1.0d0)*t3e(((((((m(p5)-1)*N+m(p6)-1)*N+m(p7)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h8)))*i1(((((m(h8)-1)*N+m(p4)-1)*N+m(h3)-1&
&)*N+m(p7)))+(-1.0d0)*t3e(((((((m(p5)-1)*N+m(p6)-1)*N+m(p7)-1)*N+m(h2)-1)*N+m(h3)-1)*N+m(h8)))*i1(((((m(h8)-1)*N+m(p4)-1)*N+m(h1)-1&
&)*N+m(p7)))+(1.0d0)*t3e(((((((m(p5)-1)*N+m(p6)-1)*N+m(p7)-1)*N+m(h1)-1)*N+m(h3)-1)*N+m(h8)))*i1(((((m(h8)-1)*N+m(p4)-1)*N+m(h2)-1)&
&*N+m(p7)))+(1.0d0)*t3e(((((((m(p4)-1)*N+m(p6)-1)*N+m(p7)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h8)))*i1(((((m(h8)-1)*N+m(p5)-1)*N+m(h3)-1)*&
&N+m(p7)))+(1.0d0)*t3e(((((((m(p4)-1)*N+m(p6)-1)*N+m(p7)-1)*N+m(h2)-1)*N+m(h3)-1)*N+m(h8)))*i1(((((m(h8)-1)*N+m(p5)-1)*N+m(h1)-1)*N&
&+m(p7)))+(-1.0d0)*t3e(((((((m(p4)-1)*N+m(p6)-1)*N+m(p7)-1)*N+m(h1)-1)*N+m(h3)-1)*N+m(h8)))*i1(((((m(h8)-1)*N+m(p5)-1)*N+m(h2)-1)*N&
&+m(p7)))
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
t3e(((((((m(p7)-1)*N+m(p4)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h3)))= - t3(((((((m(p4)-1)*N+m(p7)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(&
&h2)-1)*N+m(h3)))
t3e(((((((m(p8)-1)*N+m(p4)-1)*N+m(p7)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h3)))= + t3(((((((m(p4)-1)*N+m(p7)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(&
&h2)-1)*N+m(h3)))
t3e(((((((m(p4)-1)*N+m(p7)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h3)))= + t3(((((((m(p4)-1)*N+m(p7)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(&
&h2)-1)*N+m(h3)))
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
i0(((((((m(p4)-1)*N+m(p5)-1)*N+m(p6)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h3)))=i0(((((((m(p4)-1)*N+m(p5)-1)*N+m(p6)-1)*N+m(h1)-1)*N+m(h2)-&
&1)*N+m(h3)))+(1.0d0)*t3e(((((((m(p4)-1)*N+m(p7)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h3)))*v2(((((m(p5)-1)*N+m(p6)-1)*N+m(p7)-1&
&)*N+m(p8)))+(-1.0d0)*t3e(((((((m(p5)-1)*N+m(p7)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h3)))*v2(((((m(p4)-1)*N+m(p6)-1)*N+m(p7)-1&
&)*N+m(p8)))+(1.0d0)*t3e(((((((m(p6)-1)*N+m(p7)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h3)))*v2(((((m(p4)-1)*N+m(p5)-1)*N+m(p7)-1)&
&*N+m(p8)))
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
v2e(((((m(h12)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(p11)))= + v2(((((m(h12)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(p11)))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h12=1,nocc
DO h1=1,nocc
DO p11=nocc+1,N
i2(((((m(h12)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(p11)))=(1.0d0)*v2e(((((m(h12)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(p11)))
END DO
END DO
END DO
END DO
DO h8=1,nocc
DO h12=1,nocc
IF (h8>h12) CYCLE
DO h1=1,nocc
DO p11=nocc+1,N
v2e(((((m(h8)-1)*N+m(h12)-1)*N+m(h1)-1)*N+m(p11)))= + v2(((((m(h8)-1)*N+m(h12)-1)*N+m(h1)-1)*N+m(p11)))
END DO
END DO
END DO
END DO
DO h8=1,nocc
DO h12=1,nocc
IF (h8>h12) CYCLE
DO h1=1,nocc
DO p11=nocc+1,N
i3(((((m(h8)-1)*N+m(h12)-1)*N+m(h1)-1)*N+m(p11)))=(1.0d0)*v2e(((((m(h8)-1)*N+m(h12)-1)*N+m(h1)-1)*N+m(p11)))
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
v2e(((((m(h8)-1)*N+m(h12)-1)*N+m(p11)-1)*N+m(p9)))= - v2(((((m(h8)-1)*N+m(h12)-1)*N+m(p9)-1)*N+m(p11)))
v2e(((((m(h8)-1)*N+m(h12)-1)*N+m(p9)-1)*N+m(p11)))= + v2(((((m(h8)-1)*N+m(h12)-1)*N+m(p9)-1)*N+m(p11)))
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
i3(((((m(h8)-1)*N+m(h12)-1)*N+m(h1)-1)*N+m(p11)))=i3(((((m(h8)-1)*N+m(h12)-1)*N+m(h1)-1)*N+m(p11)))+(1.0d0)*t1(((m(p9)-1)*N+m(h1)))&
&*v2e(((((m(h8)-1)*N+m(h12)-1)*N+m(p9)-1)*N+m(p11)))
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
i3e(((((m(h12)-1)*N+m(h8)-1)*N+m(h1)-1)*N+m(p11)))= - i3(((((m(h8)-1)*N+m(h12)-1)*N+m(h1)-1)*N+m(p11)))
i3e(((((m(h8)-1)*N+m(h12)-1)*N+m(h1)-1)*N+m(p11)))= + i3(((((m(h8)-1)*N+m(h12)-1)*N+m(h1)-1)*N+m(p11)))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h12=1,nocc
DO h1=1,nocc
DO p11=nocc+1,N
DO h8=1,nocc
i2(((((m(h12)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(p11)))=i2(((((m(h12)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(p11)))+(0.5d0)*t1(((m(p4)-1)*N+m(h8)))&
&*i3e(((((m(h8)-1)*N+m(h12)-1)*N+m(h1)-1)*N+m(p11)))
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
v2e(((((m(h12)-1)*N+m(p4)-1)*N+m(p11)-1)*N+m(p8)))= - v2(((((m(h12)-1)*N+m(p4)-1)*N+m(p8)-1)*N+m(p11)))
v2e(((((m(h12)-1)*N+m(p4)-1)*N+m(p8)-1)*N+m(p11)))= + v2(((((m(h12)-1)*N+m(p4)-1)*N+m(p8)-1)*N+m(p11)))
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO h12=1,nocc
DO h1=1,nocc
DO p11=nocc+1,N
DO p8=nocc+1,N
i2(((((m(h12)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(p11)))=i2(((((m(h12)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(p11)))+(1.0d0)*t1(((m(p8)-1)*N+m(h1)))&
&*v2e(((((m(h12)-1)*N+m(p4)-1)*N+m(p8)-1)*N+m(p11)))
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
t2e(((((m(p8)-1)*N+m(p4)-1)*N+m(h9)-1)*N+m(h1)))= + t2(((((m(p4)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(h9)))
t2e(((((m(p8)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h9)))= - t2(((((m(p4)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(h9)))
t2e(((((m(p4)-1)*N+m(p8)-1)*N+m(h9)-1)*N+m(h1)))= - t2(((((m(p4)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(h9)))
t2e(((((m(p4)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(h9)))= + t2(((((m(p4)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(h9)))
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
v2e(((((m(h12)-1)*N+m(h9)-1)*N+m(p11)-1)*N+m(p8)))= + v2(((((m(h9)-1)*N+m(h12)-1)*N+m(p8)-1)*N+m(p11)))
v2e(((((m(h12)-1)*N+m(h9)-1)*N+m(p8)-1)*N+m(p11)))= - v2(((((m(h9)-1)*N+m(h12)-1)*N+m(p8)-1)*N+m(p11)))
v2e(((((m(h9)-1)*N+m(h12)-1)*N+m(p11)-1)*N+m(p8)))= - v2(((((m(h9)-1)*N+m(h12)-1)*N+m(p8)-1)*N+m(p11)))
v2e(((((m(h9)-1)*N+m(h12)-1)*N+m(p8)-1)*N+m(p11)))= + v2(((((m(h9)-1)*N+m(h12)-1)*N+m(p8)-1)*N+m(p11)))
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
i2(((((m(h12)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(p11)))=i2(((((m(h12)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(p11)))+(-1.0d0)*t2e(((((m(p4)-1)*N+m(p&
&8)-1)*N+m(h1)-1)*N+m(h9)))*v2e(((((m(h9)-1)*N+m(h12)-1)*N+m(p8)-1)*N+m(p11)))
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
t2e(((((m(p11)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))= - t2(((((m(p4)-1)*N+m(p11)-1)*N+m(h1)-1)*N+m(h2)))
t2e(((((m(p4)-1)*N+m(p11)-1)*N+m(h1)-1)*N+m(h2)))= + t2(((((m(p4)-1)*N+m(p11)-1)*N+m(h1)-1)*N+m(h2)))
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
i1(((((((m(h12)-1)*N+m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h3)))=0.0d0
DO p11=nocc+1,N
i1(((((((m(h12)-1)*N+m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h3)))=i1(((((((m(h12)-1)*N+m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h2&
&)-1)*N+m(h3)))+(-0.16666666666666666d0)*t2e(((((m(p4)-1)*N+m(p11)-1)*N+m(h1)-1)*N+m(h2)))*i2(((((m(h12)-1)*N+m(p5)-1)*N+m(h3)-1)*N&
&+m(p11)))+(-0.16666666666666666d0)*t2e(((((m(p4)-1)*N+m(p11)-1)*N+m(h2)-1)*N+m(h3)))*i2(((((m(h12)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(p1&
&1)))+(0.16666666666666666d0)*t2e(((((m(p4)-1)*N+m(p11)-1)*N+m(h1)-1)*N+m(h3)))*i2(((((m(h12)-1)*N+m(p5)-1)*N+m(h2)-1)*N+m(p11)))+(&
&0.16666666666666666d0)*t2e(((((m(p5)-1)*N+m(p11)-1)*N+m(h1)-1)*N+m(h2)))*i2(((((m(h12)-1)*N+m(p4)-1)*N+m(h3)-1)*N+m(p11)))+(0.1666&
&6666666666666d0)*t2e(((((m(p5)-1)*N+m(p11)-1)*N+m(h2)-1)*N+m(h3)))*i2(((((m(h12)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(p11)))+(-0.166666666&
&66666666d0)*t2e(((((m(p5)-1)*N+m(p11)-1)*N+m(h1)-1)*N+m(h3)))*i2(((((m(h12)-1)*N+m(p4)-1)*N+m(h2)-1)*N+m(p11)))
END DO
END DO
END DO
END DO
END DO
END DO
END DO
DO h12=1,nocc
DO p7=nocc+1,N
i2(((m(h12)-1)*N+m(p7)))=(1.0d0)*f1(((m(h12)-1)*N+m(p7)))
END DO
END DO
DO h12=1,nocc
DO h9=1,nocc
IF (h9>h12) CYCLE
DO p7=nocc+1,N
DO p8=nocc+1,N
IF (p7>p8) CYCLE
v2e(((((m(h12)-1)*N+m(h9)-1)*N+m(p8)-1)*N+m(p7)))= + v2(((((m(h9)-1)*N+m(h12)-1)*N+m(p7)-1)*N+m(p8)))
v2e(((((m(h12)-1)*N+m(h9)-1)*N+m(p7)-1)*N+m(p8)))= - v2(((((m(h9)-1)*N+m(h12)-1)*N+m(p7)-1)*N+m(p8)))
v2e(((((m(h9)-1)*N+m(h12)-1)*N+m(p8)-1)*N+m(p7)))= - v2(((((m(h9)-1)*N+m(h12)-1)*N+m(p7)-1)*N+m(p8)))
v2e(((((m(h9)-1)*N+m(h12)-1)*N+m(p7)-1)*N+m(p8)))= + v2(((((m(h9)-1)*N+m(h12)-1)*N+m(p7)-1)*N+m(p8)))
END DO
END DO
END DO
END DO
DO h12=1,nocc
DO p7=nocc+1,N
DO p8=nocc+1,N
DO h9=1,nocc
i2(((m(h12)-1)*N+m(p7)))=i2(((m(h12)-1)*N+m(p7)))+(-1.0d0)*t1(((m(p8)-1)*N+m(h9)))*v2e(((((m(h9)-1)*N+m(h12)-1)*N+m(p7)-1)*N+m(p8)))
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
t3e(((((((m(p5)-1)*N+m(p7)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h3)))= + t3(((((((m(p4)-1)*N+m(p5)-1)*N+m(p7)-1)*N+m(h1)-1)*N+m(&
&h2)-1)*N+m(h3)))
t3e(((((((m(p4)-1)*N+m(p7)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h3)))= - t3(((((((m(p4)-1)*N+m(p5)-1)*N+m(p7)-1)*N+m(h1)-1)*N+m(&
&h2)-1)*N+m(h3)))
t3e(((((((m(p4)-1)*N+m(p5)-1)*N+m(p7)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h3)))= + t3(((((((m(p4)-1)*N+m(p5)-1)*N+m(p7)-1)*N+m(h1)-1)*N+m(&
&h2)-1)*N+m(h3)))
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
i1(((((((m(h12)-1)*N+m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h3)))=i1(((((((m(h12)-1)*N+m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h2&
&)-1)*N+m(h3)))+(-0.16666666666666666d0)*t3e(((((((m(p4)-1)*N+m(p5)-1)*N+m(p7)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h3)))*i2(((m(h12)-1)*N+&
&m(p7)))
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
v2e(((((m(h7)-1)*N+m(h12)-1)*N+m(h1)-1)*N+m(p11)))= + v2(((((m(h7)-1)*N+m(h12)-1)*N+m(h1)-1)*N+m(p11)))
END DO
END DO
END DO
END DO
DO h7=1,nocc
DO h12=1,nocc
IF (h7>h12) CYCLE
DO h1=1,nocc
DO p11=nocc+1,N
i2(((((m(h7)-1)*N+m(h12)-1)*N+m(h1)-1)*N+m(p11)))=(1.0d0)*v2e(((((m(h7)-1)*N+m(h12)-1)*N+m(h1)-1)*N+m(p11)))
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
v2e(((((m(h7)-1)*N+m(h12)-1)*N+m(p11)-1)*N+m(p8)))= - v2(((((m(h7)-1)*N+m(h12)-1)*N+m(p8)-1)*N+m(p11)))
v2e(((((m(h7)-1)*N+m(h12)-1)*N+m(p8)-1)*N+m(p11)))= + v2(((((m(h7)-1)*N+m(h12)-1)*N+m(p8)-1)*N+m(p11)))
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
i2(((((m(h7)-1)*N+m(h12)-1)*N+m(h1)-1)*N+m(p11)))=i2(((((m(h7)-1)*N+m(h12)-1)*N+m(h1)-1)*N+m(p11)))+(1.0d0)*t1(((m(p8)-1)*N+m(h1)))&
&*v2e(((((m(h7)-1)*N+m(h12)-1)*N+m(p8)-1)*N+m(p11)))
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
t3e(((((((m(p5)-1)*N+m(p11)-1)*N+m(p4)-1)*N+m(h2)-1)*N+m(h7)-1)*N+m(h1)))= + t3(((((((m(p4)-1)*N+m(p5)-1)*N+m(p11)-1)*N+m(h1)-1)*N+&
&m(h2)-1)*N+m(h7)))
t3e(((((((m(p5)-1)*N+m(p11)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h7)-1)*N+m(h2)))= - t3(((((((m(p4)-1)*N+m(p5)-1)*N+m(p11)-1)*N+m(h1)-1)*N+&
&m(h2)-1)*N+m(h7)))
t3e(((((((m(p5)-1)*N+m(p11)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h7)))= + t3(((((((m(p4)-1)*N+m(p5)-1)*N+m(p11)-1)*N+m(h1)-1)*N+&
&m(h2)-1)*N+m(h7)))
t3e(((((((m(p4)-1)*N+m(p11)-1)*N+m(p5)-1)*N+m(h2)-1)*N+m(h7)-1)*N+m(h1)))= - t3(((((((m(p4)-1)*N+m(p5)-1)*N+m(p11)-1)*N+m(h1)-1)*N+&
&m(h2)-1)*N+m(h7)))
t3e(((((((m(p4)-1)*N+m(p11)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h7)-1)*N+m(h2)))= + t3(((((((m(p4)-1)*N+m(p5)-1)*N+m(p11)-1)*N+m(h1)-1)*N+&
&m(h2)-1)*N+m(h7)))
t3e(((((((m(p4)-1)*N+m(p11)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h7)))= - t3(((((((m(p4)-1)*N+m(p5)-1)*N+m(p11)-1)*N+m(h1)-1)*N+&
&m(h2)-1)*N+m(h7)))
t3e(((((((m(p4)-1)*N+m(p5)-1)*N+m(p11)-1)*N+m(h2)-1)*N+m(h7)-1)*N+m(h1)))= + t3(((((((m(p4)-1)*N+m(p5)-1)*N+m(p11)-1)*N+m(h1)-1)*N+&
&m(h2)-1)*N+m(h7)))
t3e(((((((m(p4)-1)*N+m(p5)-1)*N+m(p11)-1)*N+m(h1)-1)*N+m(h7)-1)*N+m(h2)))= - t3(((((((m(p4)-1)*N+m(p5)-1)*N+m(p11)-1)*N+m(h1)-1)*N+&
&m(h2)-1)*N+m(h7)))
t3e(((((((m(p4)-1)*N+m(p5)-1)*N+m(p11)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h7)))= + t3(((((((m(p4)-1)*N+m(p5)-1)*N+m(p11)-1)*N+m(h1)-1)*N+&
&m(h2)-1)*N+m(h7)))
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
i2e(((((m(h12)-1)*N+m(h7)-1)*N+m(h3)-1)*N+m(p11)))= - i2(((((m(h7)-1)*N+m(h12)-1)*N+m(h3)-1)*N+m(p11)))
i2e(((((m(h7)-1)*N+m(h12)-1)*N+m(h3)-1)*N+m(p11)))= + i2(((((m(h7)-1)*N+m(h12)-1)*N+m(h3)-1)*N+m(p11)))
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
i1(((((((m(h12)-1)*N+m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h3)))=i1(((((((m(h12)-1)*N+m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h2&
&)-1)*N+m(h3)))+(0.16666666666666666d0)*t3e(((((((m(p4)-1)*N+m(p5)-1)*N+m(p11)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h7)))*i2e(((((m(h7)-1)*&
&N+m(h12)-1)*N+m(h3)-1)*N+m(p11)))+(0.16666666666666666d0)*t3e(((((((m(p4)-1)*N+m(p5)-1)*N+m(p11)-1)*N+m(h2)-1)*N+m(h3)-1)*N+m(h7))&
&)*i2e(((((m(h7)-1)*N+m(h12)-1)*N+m(h1)-1)*N+m(p11)))+(-0.16666666666666666d0)*t3e(((((((m(p4)-1)*N+m(p5)-1)*N+m(p11)-1)*N+m(h1)-1)&
&*N+m(h3)-1)*N+m(h7)))*i2e(((((m(h7)-1)*N+m(h12)-1)*N+m(h2)-1)*N+m(p11)))
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
t3e(((((((m(p7)-1)*N+m(p4)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h3)))= - t3(((((((m(p4)-1)*N+m(p7)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(&
&h2)-1)*N+m(h3)))
t3e(((((((m(p8)-1)*N+m(p4)-1)*N+m(p7)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h3)))= + t3(((((((m(p4)-1)*N+m(p7)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(&
&h2)-1)*N+m(h3)))
t3e(((((((m(p4)-1)*N+m(p7)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h3)))= + t3(((((((m(p4)-1)*N+m(p7)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(&
&h2)-1)*N+m(h3)))
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
v2e(((((m(h12)-1)*N+m(p5)-1)*N+m(p7)-1)*N+m(p8)))= + v2(((((m(h12)-1)*N+m(p5)-1)*N+m(p7)-1)*N+m(p8)))
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
i1(((((((m(h12)-1)*N+m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h3)))=i1(((((((m(h12)-1)*N+m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h2&
&)-1)*N+m(h3)))+(0.16666666666666666d0)*t3e(((((((m(p4)-1)*N+m(p7)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h3)))*v2e(((((m(h12)-1)*&
&N+m(p5)-1)*N+m(p7)-1)*N+m(p8)))+(-0.16666666666666666d0)*t3e(((((((m(p5)-1)*N+m(p7)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h3)))*&
&v2e(((((m(h12)-1)*N+m(p4)-1)*N+m(p7)-1)*N+m(p8)))
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
t3e(((((((m(p9)-1)*N+m(p4)-1)*N+m(p10)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h3)))= - t3(((((((m(p4)-1)*N+m(p9)-1)*N+m(p10)-1)*N+m(h1)-1)*N+&
&m(h2)-1)*N+m(h3)))
t3e(((((((m(p10)-1)*N+m(p4)-1)*N+m(p9)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h3)))= + t3(((((((m(p4)-1)*N+m(p9)-1)*N+m(p10)-1)*N+m(h1)-1)*N+&
&m(h2)-1)*N+m(h3)))
t3e(((((((m(p4)-1)*N+m(p9)-1)*N+m(p10)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h3)))= + t3(((((((m(p4)-1)*N+m(p9)-1)*N+m(p10)-1)*N+m(h1)-1)*N+&
&m(h2)-1)*N+m(h3)))
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
i2(((((((m(h8)-1)*N+m(h12)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h3)))=0.0d0
DO p9=nocc+1,N
DO p10=nocc+1,N
IF (p9>p10) CYCLE
i2(((((((m(h8)-1)*N+m(h12)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h3)))=i2(((((((m(h8)-1)*N+m(h12)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2&
&)-1)*N+m(h3)))+(2.0d0)*t3e(((((((m(p4)-1)*N+m(p9)-1)*N+m(p10)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h3)))*v2(((((m(h8)-1)*N+m(h12)-1)*N+m(p&
&9)-1)*N+m(p10)))
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
i2e(((((((m(h12)-1)*N+m(h8)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h3)))= - i2(((((((m(h8)-1)*N+m(h12)-1)*N+m(p5)-1)*N+m(h1)-1)*N+&
&m(h2)-1)*N+m(h3)))
i2e(((((((m(h8)-1)*N+m(h12)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h3)))= + i2(((((((m(h8)-1)*N+m(h12)-1)*N+m(p5)-1)*N+m(h1)-1)*N+&
&m(h2)-1)*N+m(h3)))
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
i1(((((((m(h12)-1)*N+m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h3)))=i1(((((((m(h12)-1)*N+m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h2&
&)-1)*N+m(h3)))+(-0.041666666666666664d0)*t1(((m(p4)-1)*N+m(h8)))*i2e(((((((m(h8)-1)*N+m(h12)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h2)-1)*N&
&+m(h3)))+(0.041666666666666664d0)*t1(((m(p5)-1)*N+m(h8)))*i2e(((((((m(h8)-1)*N+m(h12)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h3)))
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
i0(((((((m(p4)-1)*N+m(p5)-1)*N+m(p6)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h3)))=i0(((((((m(p4)-1)*N+m(p5)-1)*N+m(p6)-1)*N+m(h1)-1)*N+m(h2)-&
&1)*N+m(h3)))+(6.0d0)*t1(((m(p4)-1)*N+m(h12)))*i1(((((((m(h12)-1)*N+m(p5)-1)*N+m(p6)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h3)))+(-6.0d0)*t1&
&(((m(p5)-1)*N+m(h12)))*i1(((((((m(h12)-1)*N+m(p4)-1)*N+m(p6)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h3)))+(6.0d0)*t1(((m(p6)-1)*N+m(h12)))*i&
&1(((((((m(h12)-1)*N+m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h3)))
END DO
END DO
END DO
END DO
END DO
END DO
END DO
DO h12=1,nocc
DO p11=nocc+1,N
i2(((m(h12)-1)*N+m(p11)))=(1.0d0)*f1(((m(h12)-1)*N+m(p11)))
END DO
END DO
DO h12=1,nocc
DO h10=1,nocc
IF (h10>h12) CYCLE
DO p11=nocc+1,N
DO p9=nocc+1,N
IF (p9>p11) CYCLE
v2e(((((m(h12)-1)*N+m(h10)-1)*N+m(p11)-1)*N+m(p9)))= + v2(((((m(h10)-1)*N+m(h12)-1)*N+m(p9)-1)*N+m(p11)))
v2e(((((m(h12)-1)*N+m(h10)-1)*N+m(p9)-1)*N+m(p11)))= - v2(((((m(h10)-1)*N+m(h12)-1)*N+m(p9)-1)*N+m(p11)))
v2e(((((m(h10)-1)*N+m(h12)-1)*N+m(p11)-1)*N+m(p9)))= - v2(((((m(h10)-1)*N+m(h12)-1)*N+m(p9)-1)*N+m(p11)))
v2e(((((m(h10)-1)*N+m(h12)-1)*N+m(p9)-1)*N+m(p11)))= + v2(((((m(h10)-1)*N+m(h12)-1)*N+m(p9)-1)*N+m(p11)))
END DO
END DO
END DO
END DO
DO h12=1,nocc
DO p11=nocc+1,N
DO p9=nocc+1,N
DO h10=1,nocc
i2(((m(h12)-1)*N+m(p11)))=i2(((m(h12)-1)*N+m(p11)))+(0.5d0)*t1(((m(p9)-1)*N+m(h10)))*v2e(((((m(h10)-1)*N+m(h12)-1)*N+m(p9)-1)*N+m(p&
&11)))
END DO
END DO
END DO
END DO
i1(1)=0.0d0
DO p11=nocc+1,N
DO h12=1,nocc
i1(1)=i1(1)+(1.0d0)*t1(((m(p11)-1)*N+m(h12)))*i2(((m(h12)-1)*N+m(p11)))
END DO
END DO
DO p7=nocc+1,N
DO p8=nocc+1,N
IF (p7>p8) CYCLE
DO h9=1,nocc
DO h10=1,nocc
IF (h9>h10) CYCLE
i1(1)=i1(1)+(1.0d0)*t2(((((m(p7)-1)*N+m(p8)-1)*N+m(h9)-1)*N+m(h10)))*v2(((((m(h9)-1)*N+m(h10)-1)*N+m(p7)-1)*N+m(p8)))
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
i0(((((((m(p4)-1)*N+m(p5)-1)*N+m(p6)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h3)))=i0(((((((m(p4)-1)*N+m(p5)-1)*N+m(p6)-1)*N+m(h1)-1)*N+m(h2)-&
&1)*N+m(h3)))+(1.0d0)*t3(((((((m(p4)-1)*N+m(p5)-1)*N+m(p6)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h3)))*i1(1)
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
t3e(((((((m(p7)-1)*N+m(p4)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h3)))= - t3(((((((m(p4)-1)*N+m(p7)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(&
&h2)-1)*N+m(h3)))
t3e(((((((m(p8)-1)*N+m(p4)-1)*N+m(p7)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h3)))= + t3(((((((m(p4)-1)*N+m(p7)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(&
&h2)-1)*N+m(h3)))
t3e(((((((m(p4)-1)*N+m(p7)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h3)))= + t3(((((((m(p4)-1)*N+m(p7)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(&
&h2)-1)*N+m(h3)))
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
i1(((((((m(h9)-1)*N+m(h10)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h3)))=0.0d0
DO p7=nocc+1,N
DO p8=nocc+1,N
IF (p7>p8) CYCLE
i1(((((((m(h9)-1)*N+m(h10)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h3)))=i1(((((((m(h9)-1)*N+m(h10)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2&
&)-1)*N+m(h3)))+(2.0d0)*t3e(((((((m(p4)-1)*N+m(p7)-1)*N+m(p8)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h3)))*v2(((((m(h9)-1)*N+m(h10)-1)*N+m(p7&
&)-1)*N+m(p8)))
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
i0(((((((m(p4)-1)*N+m(p5)-1)*N+m(p6)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h3)))=i0(((((((m(p4)-1)*N+m(p5)-1)*N+m(p6)-1)*N+m(h1)-1)*N+m(h2)-&
&1)*N+m(h3)))+(0.5d0)*t2(((((m(p4)-1)*N+m(p5)-1)*N+m(h9)-1)*N+m(h10)))*i1(((((((m(h9)-1)*N+m(h10)-1)*N+m(p6)-1)*N+m(h1)-1)*N+m(h2)-&
&1)*N+m(h3)))+(0.5d0)*t2(((((m(p5)-1)*N+m(p6)-1)*N+m(h9)-1)*N+m(h10)))*i1(((((((m(h9)-1)*N+m(h10)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)-&
&1)*N+m(h3)))+(-0.5d0)*t2(((((m(p4)-1)*N+m(p6)-1)*N+m(h9)-1)*N+m(h10)))*i1(((((((m(h9)-1)*N+m(h10)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h2)&
&-1)*N+m(h3)))
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
