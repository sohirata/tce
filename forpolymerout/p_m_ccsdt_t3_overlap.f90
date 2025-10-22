SUBROUTINE p_m_ccsdt_t3_overlap(N,i0,i0e,i1,i1e,m,nocc,t1,t1e,t2,t2e,t3,t3e)
IMPLICIT NONE
INTEGER :: N
INTEGER :: nocc
INTEGER :: p4
INTEGER :: p5
INTEGER :: p6
INTEGER :: h1
INTEGER :: h2
INTEGER :: h3
INTEGER :: m(*)
REAL*8 :: i0(*)
REAL*8 :: i0e(*)
REAL*8 :: t3(*)
REAL*8 :: t3e(*)
REAL*8 :: t2(*)
REAL*8 :: t2e(*)
REAL*8 :: t1(*)
REAL*8 :: t1e(*)
REAL*8 :: i1(*)
REAL*8 :: i1e(*)
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
i0(((((((m(p4)-1)*N+m(p5)-1)*N+m(p6)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h3)))=(1.0d0)*t3(((((((m(p4)-1)*N+m(p5)-1)*N+m(p6)-1)*N+m(h1)-1)*&
&N+m(h2)-1)*N+m(h3)))
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
i0(((((((m(p4)-1)*N+m(p5)-1)*N+m(p6)-1)*N+m(h1)-1)*N+m(h2)-1)*N+m(h3)))=i0(((((((m(p4)-1)*N+m(p5)-1)*N+m(p6)-1)*N+m(h1)-1)*N+m(h2)-&
&1)*N+m(h3)))+(1.0d0)*t2(((((m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h2)))*t1(((m(p6)-1)*N+m(h3)))+(1.0d0)*t2(((((m(p4)-1)*N+m(p5)-1)*N+&
&m(h2)-1)*N+m(h3)))*t1(((m(p6)-1)*N+m(h1)))+(-1.0d0)*t2(((((m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h3)))*t1(((m(p6)-1)*N+m(h2)))+(1.0d0&
&)*t2(((((m(p5)-1)*N+m(p6)-1)*N+m(h1)-1)*N+m(h2)))*t1(((m(p4)-1)*N+m(h3)))+(1.0d0)*t2(((((m(p5)-1)*N+m(p6)-1)*N+m(h2)-1)*N+m(h3)))*&
&t1(((m(p4)-1)*N+m(h1)))+(-1.0d0)*t2(((((m(p5)-1)*N+m(p6)-1)*N+m(h1)-1)*N+m(h3)))*t1(((m(p4)-1)*N+m(h2)))+(-1.0d0)*t2(((((m(p4)-1)*&
&N+m(p6)-1)*N+m(h1)-1)*N+m(h2)))*t1(((m(p5)-1)*N+m(h3)))+(-1.0d0)*t2(((((m(p4)-1)*N+m(p6)-1)*N+m(h2)-1)*N+m(h3)))*t1(((m(p5)-1)*N+m&
&(h1)))+(1.0d0)*t2(((((m(p4)-1)*N+m(p6)-1)*N+m(h1)-1)*N+m(h3)))*t1(((m(p5)-1)*N+m(h2)))
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
i1(((((m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h2)))=0.0d0
i1(((((m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h2)))=i1(((((m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h2)))+(-0.25d0)*t1(((m(p4)-1)*N+m(h1)))*t&
&1(((m(p5)-1)*N+m(h2)))+(0.25d0)*t1(((m(p4)-1)*N+m(h2)))*t1(((m(p5)-1)*N+m(h1)))+(0.25d0)*t1(((m(p5)-1)*N+m(h1)))*t1(((m(p4)-1)*N+m&
&(h2)))+(-0.25d0)*t1(((m(p5)-1)*N+m(h2)))*t1(((m(p4)-1)*N+m(h1)))
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
&1)*N+m(h3)))+(-0.6666666666666666d0)*t1(((m(p4)-1)*N+m(h1)))*i1(((((m(p5)-1)*N+m(p6)-1)*N+m(h2)-1)*N+m(h3)))+(0.6666666666666666d0&
&)*t1(((m(p4)-1)*N+m(h2)))*i1(((((m(p5)-1)*N+m(p6)-1)*N+m(h1)-1)*N+m(h3)))+(-0.6666666666666666d0)*t1(((m(p4)-1)*N+m(h3)))*i1(((((m&
&(p5)-1)*N+m(p6)-1)*N+m(h1)-1)*N+m(h2)))+(0.6666666666666666d0)*t1(((m(p5)-1)*N+m(h1)))*i1(((((m(p4)-1)*N+m(p6)-1)*N+m(h2)-1)*N+m(h&
&3)))+(-0.6666666666666666d0)*t1(((m(p5)-1)*N+m(h2)))*i1(((((m(p4)-1)*N+m(p6)-1)*N+m(h1)-1)*N+m(h3)))+(0.6666666666666666d0)*t1(((m&
&(p5)-1)*N+m(h3)))*i1(((((m(p4)-1)*N+m(p6)-1)*N+m(h1)-1)*N+m(h2)))+(-0.6666666666666666d0)*t1(((m(p6)-1)*N+m(h1)))*i1(((((m(p4)-1)*&
&N+m(p5)-1)*N+m(h2)-1)*N+m(h3)))+(0.6666666666666666d0)*t1(((m(p6)-1)*N+m(h2)))*i1(((((m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h3)))+(-0&
&.6666666666666666d0)*t1(((m(p6)-1)*N+m(h3)))*i1(((((m(p4)-1)*N+m(p5)-1)*N+m(h1)-1)*N+m(h2)))
END DO
END DO
END DO
END DO
END DO
END DO
RETURN
END SUBROUTINE
