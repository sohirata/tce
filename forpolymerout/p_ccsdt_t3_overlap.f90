SUBROUTINE p_ccsdt_t3_overlap(N,i0,i0e,i1,i1e,nocc,t1,t1e,t2,t2e,t3,t3e)
IMPLICIT NONE
INTEGER :: N
INTEGER :: nocc
INTEGER :: p4
INTEGER :: p5
INTEGER :: p6
INTEGER :: h1
INTEGER :: h2
INTEGER :: h3
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
i0(((((((p4-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h3))=(1.0d0)*t3(((((((p4-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h3))
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
i0(((((((p4-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h3))=i0(((((((p4-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h3))+(1.0d0)*t2(((((p4-1)*N+&
&p5-1)*N+h1-1)*N+h2))*t1(((p6-1)*N+h3))+(1.0d0)*t2(((((p4-1)*N+p5-1)*N+h2-1)*N+h3))*t1(((p6-1)*N+h1))+(-1.0d0)*t2(((((p4-1)*N+p5-1)&
&*N+h1-1)*N+h3))*t1(((p6-1)*N+h2))+(1.0d0)*t2(((((p5-1)*N+p6-1)*N+h1-1)*N+h2))*t1(((p4-1)*N+h3))+(1.0d0)*t2(((((p5-1)*N+p6-1)*N+h2-&
&1)*N+h3))*t1(((p4-1)*N+h1))+(-1.0d0)*t2(((((p5-1)*N+p6-1)*N+h1-1)*N+h3))*t1(((p4-1)*N+h2))+(-1.0d0)*t2(((((p4-1)*N+p6-1)*N+h1-1)*N&
&+h2))*t1(((p5-1)*N+h3))+(-1.0d0)*t2(((((p4-1)*N+p6-1)*N+h2-1)*N+h3))*t1(((p5-1)*N+h1))+(1.0d0)*t2(((((p4-1)*N+p6-1)*N+h1-1)*N+h3))&
&*t1(((p5-1)*N+h2))
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
i1(((((p4-1)*N+p5-1)*N+h1-1)*N+h2))=0.0d0
i1(((((p4-1)*N+p5-1)*N+h1-1)*N+h2))=i1(((((p4-1)*N+p5-1)*N+h1-1)*N+h2))+(-0.25d0)*t1(((p4-1)*N+h1))*t1(((p5-1)*N+h2))+(0.25d0)*t1((&
&(p4-1)*N+h2))*t1(((p5-1)*N+h1))+(0.25d0)*t1(((p5-1)*N+h1))*t1(((p4-1)*N+h2))+(-0.25d0)*t1(((p5-1)*N+h2))*t1(((p4-1)*N+h1))
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
i0(((((((p4-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h3))=i0(((((((p4-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h3))+(-0.6666666666666666d0)&
&*t1(((p4-1)*N+h1))*i1(((((p5-1)*N+p6-1)*N+h2-1)*N+h3))+(0.6666666666666666d0)*t1(((p4-1)*N+h2))*i1(((((p5-1)*N+p6-1)*N+h1-1)*N+h3)&
&)+(-0.6666666666666666d0)*t1(((p4-1)*N+h3))*i1(((((p5-1)*N+p6-1)*N+h1-1)*N+h2))+(0.6666666666666666d0)*t1(((p5-1)*N+h1))*i1(((((p4&
&-1)*N+p6-1)*N+h2-1)*N+h3))+(-0.6666666666666666d0)*t1(((p5-1)*N+h2))*i1(((((p4-1)*N+p6-1)*N+h1-1)*N+h3))+(0.6666666666666666d0)*t1&
&(((p5-1)*N+h3))*i1(((((p4-1)*N+p6-1)*N+h1-1)*N+h2))+(-0.6666666666666666d0)*t1(((p6-1)*N+h1))*i1(((((p4-1)*N+p5-1)*N+h2-1)*N+h3))+&
&(0.6666666666666666d0)*t1(((p6-1)*N+h2))*i1(((((p4-1)*N+p5-1)*N+h1-1)*N+h3))+(-0.6666666666666666d0)*t1(((p6-1)*N+h3))*i1(((((p4-1&
&)*N+p5-1)*N+h1-1)*N+h2))
END DO
END DO
END DO
END DO
END DO
END DO
RETURN
END SUBROUTINE
