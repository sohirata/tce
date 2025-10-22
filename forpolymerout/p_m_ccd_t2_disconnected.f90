SUBROUTINE p_m_ccd_t2_disconnected(N,f1,f1e,i0,i0e,i1,i1e,m,nocc,t2,t2e,v2,v2e)
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
INTEGER :: m(*)
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
i0(((((m(p3)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))=(1.0d0)*v2(((((m(p3)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))
END DO
END DO
END DO
END DO
DO h5=1,nocc
DO h1=1,nocc
i1(((m(h5)-1)*N+m(h1)))=(1.0d0)*f1(((m(h5)-1)*N+m(h1)))
END DO
END DO
DO p6=nocc+1,N
DO p7=nocc+1,N
IF (p6>p7) CYCLE
DO h1=1,nocc
DO h8=1,nocc
IF (h1>h8) CYCLE
t2e(((((m(p6)-1)*N+m(p7)-1)*N+m(h8)-1)*N+m(h1)))= - t2(((((m(p6)-1)*N+m(p7)-1)*N+m(h1)-1)*N+m(h8)))
t2e(((((m(p6)-1)*N+m(p7)-1)*N+m(h1)-1)*N+m(h8)))= + t2(((((m(p6)-1)*N+m(p7)-1)*N+m(h1)-1)*N+m(h8)))
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
v2e(((((m(h8)-1)*N+m(h5)-1)*N+m(p6)-1)*N+m(p7)))= - v2(((((m(h5)-1)*N+m(h8)-1)*N+m(p6)-1)*N+m(p7)))
v2e(((((m(h5)-1)*N+m(h8)-1)*N+m(p6)-1)*N+m(p7)))= + v2(((((m(h5)-1)*N+m(h8)-1)*N+m(p6)-1)*N+m(p7)))
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
i1(((m(h5)-1)*N+m(h1)))=i1(((m(h5)-1)*N+m(h1)))+(1.0d0)*t2e(((((m(p6)-1)*N+m(p7)-1)*N+m(h1)-1)*N+m(h8)))*v2e(((((m(h5)-1)*N+m(h8)-1&
&)*N+m(p6)-1)*N+m(p7)))
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
t2e(((((m(p3)-1)*N+m(p4)-1)*N+m(h5)-1)*N+m(h1)))= - t2(((((m(p3)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h5)))
t2e(((((m(p3)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h5)))= + t2(((((m(p3)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h5)))
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
i0(((((m(p3)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))=i0(((((m(p3)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))+(-1.0d0)*t2e(((((m(p3)-1)*N+m(p4)-1&
&)*N+m(h1)-1)*N+m(h5)))*i1(((m(h5)-1)*N+m(h2)))+(1.0d0)*t2e(((((m(p3)-1)*N+m(p4)-1)*N+m(h2)-1)*N+m(h5)))*i1(((m(h5)-1)*N+m(h1)))
END DO
END DO
END DO
END DO
END DO
DO p3=nocc+1,N
DO p5=nocc+1,N
i1(((m(p3)-1)*N+m(p5)))=(1.0d0)*f1(((m(p3)-1)*N+m(p5)))
END DO
END DO
DO p3=nocc+1,N
DO p6=nocc+1,N
IF (p3>p6) CYCLE
DO h7=1,nocc
DO h8=1,nocc
IF (h7>h8) CYCLE
t2e(((((m(p6)-1)*N+m(p3)-1)*N+m(h7)-1)*N+m(h8)))= - t2(((((m(p3)-1)*N+m(p6)-1)*N+m(h7)-1)*N+m(h8)))
t2e(((((m(p3)-1)*N+m(p6)-1)*N+m(h7)-1)*N+m(h8)))= + t2(((((m(p3)-1)*N+m(p6)-1)*N+m(h7)-1)*N+m(h8)))
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
v2e(((((m(h7)-1)*N+m(h8)-1)*N+m(p6)-1)*N+m(p5)))= - v2(((((m(h7)-1)*N+m(h8)-1)*N+m(p5)-1)*N+m(p6)))
v2e(((((m(h7)-1)*N+m(h8)-1)*N+m(p5)-1)*N+m(p6)))= + v2(((((m(h7)-1)*N+m(h8)-1)*N+m(p5)-1)*N+m(p6)))
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
i1(((m(p3)-1)*N+m(p5)))=i1(((m(p3)-1)*N+m(p5)))+(-1.0d0)*t2e(((((m(p3)-1)*N+m(p6)-1)*N+m(h7)-1)*N+m(h8)))*v2e(((((m(h7)-1)*N+m(h8)-&
&1)*N+m(p5)-1)*N+m(p6)))
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
t2e(((((m(p5)-1)*N+m(p3)-1)*N+m(h1)-1)*N+m(h2)))= - t2(((((m(p3)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h2)))
t2e(((((m(p3)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h2)))= + t2(((((m(p3)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h2)))
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
i0(((((m(p3)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))=i0(((((m(p3)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))+(1.0d0)*t2e(((((m(p3)-1)*N+m(p5)-1)&
&*N+m(h1)-1)*N+m(h2)))*i1(((m(p4)-1)*N+m(p5)))+(-1.0d0)*t2e(((((m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h2)))*i1(((m(p3)-1)*N+m(p5)))
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
i1(((((m(h5)-1)*N+m(h6)-1)*N+m(h1)-1)*N+m(h2)))=(1.0d0)*v2(((((m(h5)-1)*N+m(h6)-1)*N+m(h1)-1)*N+m(h2)))
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
i1(((((m(h5)-1)*N+m(h6)-1)*N+m(h1)-1)*N+m(h2)))=i1(((((m(h5)-1)*N+m(h6)-1)*N+m(h1)-1)*N+m(h2)))+(1.0d0)*t2(((((m(p7)-1)*N+m(p8)-1)*&
&N+m(h1)-1)*N+m(h2)))*v2(((((m(h5)-1)*N+m(h6)-1)*N+m(p7)-1)*N+m(p8)))
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
i0(((((m(p3)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))=i0(((((m(p3)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))+(1.0d0)*t2(((((m(p3)-1)*N+m(p4)-1)*&
&N+m(h5)-1)*N+m(h6)))*i1(((((m(h5)-1)*N+m(h6)-1)*N+m(h1)-1)*N+m(h2)))
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
v2e(((((m(h6)-1)*N+m(p3)-1)*N+m(h1)-1)*N+m(p5)))= + v2(((((m(h6)-1)*N+m(p3)-1)*N+m(h1)-1)*N+m(p5)))
END DO
END DO
END DO
END DO
DO p3=nocc+1,N
DO h6=1,nocc
DO h1=1,nocc
DO p5=nocc+1,N
i1(((((m(h6)-1)*N+m(p3)-1)*N+m(h1)-1)*N+m(p5)))=(1.0d0)*v2e(((((m(h6)-1)*N+m(p3)-1)*N+m(h1)-1)*N+m(p5)))
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
t2e(((((m(p7)-1)*N+m(p3)-1)*N+m(h8)-1)*N+m(h1)))= + t2(((((m(p3)-1)*N+m(p7)-1)*N+m(h1)-1)*N+m(h8)))
t2e(((((m(p7)-1)*N+m(p3)-1)*N+m(h1)-1)*N+m(h8)))= - t2(((((m(p3)-1)*N+m(p7)-1)*N+m(h1)-1)*N+m(h8)))
t2e(((((m(p3)-1)*N+m(p7)-1)*N+m(h8)-1)*N+m(h1)))= - t2(((((m(p3)-1)*N+m(p7)-1)*N+m(h1)-1)*N+m(h8)))
t2e(((((m(p3)-1)*N+m(p7)-1)*N+m(h1)-1)*N+m(h8)))= + t2(((((m(p3)-1)*N+m(p7)-1)*N+m(h1)-1)*N+m(h8)))
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
v2e(((((m(h8)-1)*N+m(h6)-1)*N+m(p7)-1)*N+m(p5)))= + v2(((((m(h6)-1)*N+m(h8)-1)*N+m(p5)-1)*N+m(p7)))
v2e(((((m(h8)-1)*N+m(h6)-1)*N+m(p5)-1)*N+m(p7)))= - v2(((((m(h6)-1)*N+m(h8)-1)*N+m(p5)-1)*N+m(p7)))
v2e(((((m(h6)-1)*N+m(h8)-1)*N+m(p7)-1)*N+m(p5)))= - v2(((((m(h6)-1)*N+m(h8)-1)*N+m(p5)-1)*N+m(p7)))
v2e(((((m(h6)-1)*N+m(h8)-1)*N+m(p5)-1)*N+m(p7)))= + v2(((((m(h6)-1)*N+m(h8)-1)*N+m(p5)-1)*N+m(p7)))
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
i1(((((m(h6)-1)*N+m(p3)-1)*N+m(h1)-1)*N+m(p5)))=i1(((((m(h6)-1)*N+m(p3)-1)*N+m(h1)-1)*N+m(p5)))+(-0.5d0)*t2e(((((m(p3)-1)*N+m(p7)-1&
&)*N+m(h1)-1)*N+m(h8)))*v2e(((((m(h6)-1)*N+m(h8)-1)*N+m(p5)-1)*N+m(p7)))
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
t2e(((((m(p5)-1)*N+m(p3)-1)*N+m(h6)-1)*N+m(h1)))= + t2(((((m(p3)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h6)))
t2e(((((m(p5)-1)*N+m(p3)-1)*N+m(h1)-1)*N+m(h6)))= - t2(((((m(p3)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h6)))
t2e(((((m(p3)-1)*N+m(p5)-1)*N+m(h6)-1)*N+m(h1)))= - t2(((((m(p3)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h6)))
t2e(((((m(p3)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h6)))= + t2(((((m(p3)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h6)))
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
i0(((((m(p3)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))=i0(((((m(p3)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))+(-1.0d0)*t2e(((((m(p3)-1)*N+m(p5)-1&
&)*N+m(h1)-1)*N+m(h6)))*i1(((((m(h6)-1)*N+m(p4)-1)*N+m(h2)-1)*N+m(p5)))+(1.0d0)*t2e(((((m(p3)-1)*N+m(p5)-1)*N+m(h2)-1)*N+m(h6)))*i1&
&(((((m(h6)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(p5)))+(1.0d0)*t2e(((((m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h6)))*i1(((((m(h6)-1)*N+m(p3)-1)*&
&N+m(h2)-1)*N+m(p5)))+(-1.0d0)*t2e(((((m(p4)-1)*N+m(p5)-1)*N+m(h2)-1)*N+m(h6)))*i1(((((m(h6)-1)*N+m(p3)-1)*N+m(h1)-1)*N+m(p5)))
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
i0(((((m(p3)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))=i0(((((m(p3)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))+(1.0d0)*t2(((((m(p5)-1)*N+m(p6)-1)*&
&N+m(h1)-1)*N+m(h2)))*v2(((((m(p3)-1)*N+m(p4)-1)*N+m(p5)-1)*N+m(p6)))
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
i1(1)=i1(1)+(4.0d0)*t2(((((m(p5)-1)*N+m(p6)-1)*N+m(h7)-1)*N+m(h8)))*v2(((((m(h7)-1)*N+m(h8)-1)*N+m(p5)-1)*N+m(p6)))
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
i0(((((m(p3)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))=i0(((((m(p3)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))+(0.25d0)*t2(((((m(p3)-1)*N+m(p4)-1)&
&*N+m(h1)-1)*N+m(h2)))*i1(1)
END DO
END DO
END DO
END DO
RETURN
END SUBROUTINE
