SUBROUTINE p_m_ccs_t1_disconnected(N,f1,f1e,i0,i0e,i1,i1e,i2,i2e,m,nocc,t1,t1e,v2,v2e)
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
INTEGER :: m(*)
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
i0(((m(p2)-1)*N+m(h1)))=(1.0d0)*f1(((m(p2)-1)*N+m(h1)))
END DO
END DO
DO h7=1,nocc
DO h1=1,nocc
i1(((m(h7)-1)*N+m(h1)))=(1.0d0)*f1(((m(h7)-1)*N+m(h1)))
END DO
END DO
DO h7=1,nocc
DO p8=nocc+1,N
i2(((m(h7)-1)*N+m(p8)))=(1.0d0)*f1(((m(h7)-1)*N+m(p8)))
END DO
END DO
DO h7=1,nocc
DO h6=1,nocc
IF (h6>h7) CYCLE
DO p8=nocc+1,N
DO p5=nocc+1,N
IF (p5>p8) CYCLE
v2e(((((m(h7)-1)*N+m(h6)-1)*N+m(p8)-1)*N+m(p5)))= + v2(((((m(h6)-1)*N+m(h7)-1)*N+m(p5)-1)*N+m(p8)))
v2e(((((m(h7)-1)*N+m(h6)-1)*N+m(p5)-1)*N+m(p8)))= - v2(((((m(h6)-1)*N+m(h7)-1)*N+m(p5)-1)*N+m(p8)))
v2e(((((m(h6)-1)*N+m(h7)-1)*N+m(p8)-1)*N+m(p5)))= - v2(((((m(h6)-1)*N+m(h7)-1)*N+m(p5)-1)*N+m(p8)))
v2e(((((m(h6)-1)*N+m(h7)-1)*N+m(p5)-1)*N+m(p8)))= + v2(((((m(h6)-1)*N+m(h7)-1)*N+m(p5)-1)*N+m(p8)))
END DO
END DO
END DO
END DO
DO h7=1,nocc
DO p8=nocc+1,N
DO p5=nocc+1,N
DO h6=1,nocc
i2(((m(h7)-1)*N+m(p8)))=i2(((m(h7)-1)*N+m(p8)))+(0.9999999999999999d0)*t1(((m(p5)-1)*N+m(h6)))*v2e(((((m(h6)-1)*N+m(h7)-1)*N+m(p5)-&
&1)*N+m(p8)))
END DO
END DO
END DO
END DO
DO h7=1,nocc
DO h1=1,nocc
DO p8=nocc+1,N
i1(((m(h7)-1)*N+m(h1)))=i1(((m(h7)-1)*N+m(h1)))+(1.0d0)*t1(((m(p8)-1)*N+m(h1)))*i2(((m(h7)-1)*N+m(p8)))
END DO
END DO
END DO
DO h7=1,nocc
DO h5=1,nocc
IF (h5>h7) CYCLE
DO h1=1,nocc
DO p4=nocc+1,N
v2e(((((m(h7)-1)*N+m(h5)-1)*N+m(h1)-1)*N+m(p4)))= - v2(((((m(h5)-1)*N+m(h7)-1)*N+m(h1)-1)*N+m(p4)))
v2e(((((m(h5)-1)*N+m(h7)-1)*N+m(h1)-1)*N+m(p4)))= + v2(((((m(h5)-1)*N+m(h7)-1)*N+m(h1)-1)*N+m(p4)))
END DO
END DO
END DO
END DO
DO h7=1,nocc
DO h1=1,nocc
DO p4=nocc+1,N
DO h5=1,nocc
i1(((m(h7)-1)*N+m(h1)))=i1(((m(h7)-1)*N+m(h1)))+(-1.0d0)*t1(((m(p4)-1)*N+m(h5)))*v2e(((((m(h5)-1)*N+m(h7)-1)*N+m(h1)-1)*N+m(p4)))
END DO
END DO
END DO
END DO
DO p2=nocc+1,N
DO h1=1,nocc
DO h7=1,nocc
i0(((m(p2)-1)*N+m(h1)))=i0(((m(p2)-1)*N+m(h1)))+(-1.0d0)*t1(((m(p2)-1)*N+m(h7)))*i1(((m(h7)-1)*N+m(h1)))
END DO
END DO
END DO
DO p2=nocc+1,N
DO p3=nocc+1,N
i1(((m(p2)-1)*N+m(p3)))=(1.0d0)*f1(((m(p2)-1)*N+m(p3)))
END DO
END DO
DO p2=nocc+1,N
DO h5=1,nocc
DO p3=nocc+1,N
DO p4=nocc+1,N
IF (p3>p4) CYCLE
v2e(((((m(h5)-1)*N+m(p2)-1)*N+m(p4)-1)*N+m(p3)))= - v2(((((m(h5)-1)*N+m(p2)-1)*N+m(p3)-1)*N+m(p4)))
v2e(((((m(h5)-1)*N+m(p2)-1)*N+m(p3)-1)*N+m(p4)))= + v2(((((m(h5)-1)*N+m(p2)-1)*N+m(p3)-1)*N+m(p4)))
END DO
END DO
END DO
END DO
DO p2=nocc+1,N
DO p3=nocc+1,N
DO p4=nocc+1,N
DO h5=1,nocc
i1(((m(p2)-1)*N+m(p3)))=i1(((m(p2)-1)*N+m(p3)))+(-1.0d0)*t1(((m(p4)-1)*N+m(h5)))*v2e(((((m(h5)-1)*N+m(p2)-1)*N+m(p3)-1)*N+m(p4)))
END DO
END DO
END DO
END DO
DO p2=nocc+1,N
DO h1=1,nocc
DO p3=nocc+1,N
i0(((m(p2)-1)*N+m(h1)))=i0(((m(p2)-1)*N+m(h1)))+(1.0d0)*t1(((m(p3)-1)*N+m(h1)))*i1(((m(p2)-1)*N+m(p3)))
END DO
END DO
END DO
DO p2=nocc+1,N
DO h4=1,nocc
DO h1=1,nocc
DO p3=nocc+1,N
v2e(((((m(h4)-1)*N+m(p2)-1)*N+m(h1)-1)*N+m(p3)))= + v2(((((m(h4)-1)*N+m(p2)-1)*N+m(h1)-1)*N+m(p3)))
END DO
END DO
END DO
END DO
DO p2=nocc+1,N
DO h1=1,nocc
DO p3=nocc+1,N
DO h4=1,nocc
i0(((m(p2)-1)*N+m(h1)))=i0(((m(p2)-1)*N+m(h1)))+(-1.0d0)*t1(((m(p3)-1)*N+m(h4)))*v2e(((((m(h4)-1)*N+m(p2)-1)*N+m(h1)-1)*N+m(p3)))
END DO
END DO
END DO
END DO
DO h4=1,nocc
DO p3=nocc+1,N
i2(((m(h4)-1)*N+m(p3)))=(1.0d0)*f1(((m(h4)-1)*N+m(p3)))
END DO
END DO
DO h4=1,nocc
DO h6=1,nocc
IF (h4>h6) CYCLE
DO p3=nocc+1,N
DO p5=nocc+1,N
IF (p3>p5) CYCLE
v2e(((((m(h6)-1)*N+m(h4)-1)*N+m(p5)-1)*N+m(p3)))= + v2(((((m(h4)-1)*N+m(h6)-1)*N+m(p3)-1)*N+m(p5)))
v2e(((((m(h6)-1)*N+m(h4)-1)*N+m(p3)-1)*N+m(p5)))= - v2(((((m(h4)-1)*N+m(h6)-1)*N+m(p3)-1)*N+m(p5)))
v2e(((((m(h4)-1)*N+m(h6)-1)*N+m(p5)-1)*N+m(p3)))= - v2(((((m(h4)-1)*N+m(h6)-1)*N+m(p3)-1)*N+m(p5)))
v2e(((((m(h4)-1)*N+m(h6)-1)*N+m(p3)-1)*N+m(p5)))= + v2(((((m(h4)-1)*N+m(h6)-1)*N+m(p3)-1)*N+m(p5)))
END DO
END DO
END DO
END DO
DO h4=1,nocc
DO p3=nocc+1,N
DO p5=nocc+1,N
DO h6=1,nocc
i2(((m(h4)-1)*N+m(p3)))=i2(((m(h4)-1)*N+m(p3)))+(0.5d0)*t1(((m(p5)-1)*N+m(h6)))*v2e(((((m(h4)-1)*N+m(h6)-1)*N+m(p3)-1)*N+m(p5)))
END DO
END DO
END DO
END DO
i1(1)=0.0d0
DO p3=nocc+1,N
DO h4=1,nocc
i1(1)=i1(1)+(1.0d0)*t1(((m(p3)-1)*N+m(h4)))*i2(((m(h4)-1)*N+m(p3)))
END DO
END DO
DO p2=nocc+1,N
DO h1=1,nocc
i0(((m(p2)-1)*N+m(h1)))=i0(((m(p2)-1)*N+m(h1)))+(1.0d0)*t1(((m(p2)-1)*N+m(h1)))*i1(1)
END DO
END DO
RETURN
END SUBROUTINE
