SUBROUTINE p_m_lccd_t2(N,f1,f1e,i0,i0e,m,nocc,t2,t2e,v2,v2e)
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
INTEGER :: m(*)
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
i0(((((m(p3)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))=(1.0d0)*v2(((((m(p3)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h2)))
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
&)*N+m(h1)-1)*N+m(h5)))*f1(((m(h5)-1)*N+m(h2)))+(1.0d0)*t2e(((((m(p3)-1)*N+m(p4)-1)*N+m(h2)-1)*N+m(h5)))*f1(((m(h5)-1)*N+m(h1)))
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
&*N+m(h1)-1)*N+m(h2)))*f1(((m(p4)-1)*N+m(p5)))+(-1.0d0)*t2e(((((m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h2)))*f1(((m(p3)-1)*N+m(p5)))
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
&N+m(h5)-1)*N+m(h6)))*v2(((((m(h5)-1)*N+m(h6)-1)*N+m(h1)-1)*N+m(h2)))
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
DO p4=nocc+1,N
DO h6=1,nocc
DO h2=1,nocc
DO p5=nocc+1,N
v2e(((((m(h6)-1)*N+m(p4)-1)*N+m(h2)-1)*N+m(p5)))= + v2(((((m(h6)-1)*N+m(p4)-1)*N+m(h2)-1)*N+m(p5)))
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
&)*N+m(h1)-1)*N+m(h6)))*v2e(((((m(h6)-1)*N+m(p4)-1)*N+m(h2)-1)*N+m(p5)))+(1.0d0)*t2e(((((m(p3)-1)*N+m(p5)-1)*N+m(h2)-1)*N+m(h6)))*v&
&2e(((((m(h6)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(p5)))+(1.0d0)*t2e(((((m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h6)))*v2e(((((m(h6)-1)*N+m(p3)-&
&1)*N+m(h2)-1)*N+m(p5)))+(-1.0d0)*t2e(((((m(p4)-1)*N+m(p5)-1)*N+m(h2)-1)*N+m(h6)))*v2e(((((m(h6)-1)*N+m(p3)-1)*N+m(h1)-1)*N+m(p5)))
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
RETURN
END SUBROUTINE
