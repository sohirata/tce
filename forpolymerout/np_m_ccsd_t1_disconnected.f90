SUBROUTINE np_m_ccsd_t1_disconnected(N,f1,i0,i1,i2,m,nocc,t1,t2,v2)
IMPLICIT NONE
INTEGER :: N
INTEGER :: nocc
INTEGER :: p2
INTEGER :: h1
INTEGER :: h7
INTEGER :: p3
INTEGER :: h4
INTEGER :: p7
INTEGER :: h8
INTEGER :: h5
INTEGER :: p4
INTEGER :: p8
INTEGER :: h6
INTEGER :: p5
INTEGER :: p6
INTEGER :: m(*)
REAL*8 :: TMP
REAL*8 :: i0(*)
REAL*8 :: f1(*)
REAL*8 :: t1(*)
REAL*8 :: i1(*)
REAL*8 :: i2(*)
REAL*8 :: v2(*)
REAL*8 :: t2(*)
DO p2=nocc+1,N
DO h1=1,nocc
i0(((m(p2)-1)*N+m(h1)))=0.0d0
END DO
END DO
DO p2=nocc+1,N
DO h1=1,nocc
TMP=0.0d0
TMP=TMP+f1(((m(p2)-1)*N+m(h1)))
i0(((m(p2)-1)*N+m(h1)))=i0(((m(p2)-1)*N+m(h1)))+(1.0d0)*TMP
END DO
END DO
DO h7=1,nocc
DO h1=1,nocc
i1(((m(h7)-1)*N+m(h1)))=0.0d0
END DO
END DO
DO h7=1,nocc
DO h1=1,nocc
TMP=0.0d0
TMP=TMP+f1(((m(h7)-1)*N+m(h1)))
i1(((m(h7)-1)*N+m(h1)))=i1(((m(h7)-1)*N+m(h1)))+(1.0d0)*TMP
END DO
END DO
DO h7=1,nocc
DO p8=nocc+1,N
i2(((m(h7)-1)*N+m(p8)))=0.0d0
END DO
END DO
DO h7=1,nocc
DO p8=nocc+1,N
TMP=0.0d0
TMP=TMP+f1(((m(h7)-1)*N+m(p8)))
i2(((m(h7)-1)*N+m(p8)))=i2(((m(h7)-1)*N+m(p8)))+(1.0d0)*TMP
END DO
END DO
DO h7=1,nocc
DO p8=nocc+1,N
TMP=0.0d0
DO h6=1,nocc
DO p5=nocc+1,N
TMP=TMP+t1(((m(p5)-1)*N+m(h6)))*v2(((((m(h6)-1)*N+m(h7)-1)*N+m(p5)-1)*N+m(p8)))
END DO
END DO
i2(((m(h7)-1)*N+m(p8)))=i2(((m(h7)-1)*N+m(p8)))+(0.9999999999999999d0)*TMP
END DO
END DO
DO h7=1,nocc
DO h1=1,nocc
TMP=0.0d0
DO p8=nocc+1,N
TMP=TMP+t1(((m(p8)-1)*N+m(h1)))*i2(((m(h7)-1)*N+m(p8)))
END DO
i1(((m(h7)-1)*N+m(h1)))=i1(((m(h7)-1)*N+m(h1)))+(1.0d0)*TMP
END DO
END DO
DO h7=1,nocc
DO h1=1,nocc
TMP=0.0d0
DO h5=1,nocc
DO p4=nocc+1,N
TMP=TMP+t1(((m(p4)-1)*N+m(h5)))*v2(((((m(h5)-1)*N+m(h7)-1)*N+m(h1)-1)*N+m(p4)))
END DO
END DO
i1(((m(h7)-1)*N+m(h1)))=i1(((m(h7)-1)*N+m(h1)))+(-1.0d0)*TMP
END DO
END DO
DO h7=1,nocc
DO h1=1,nocc
TMP=0.0d0
DO h5=1,nocc
DO p3=nocc+1,N
DO p4=nocc+1,N
TMP=TMP+t2(((((m(p3)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h5)))*v2(((((m(h5)-1)*N+m(h7)-1)*N+m(p3)-1)*N+m(p4)))
END DO
END DO
END DO
i1(((m(h7)-1)*N+m(h1)))=i1(((m(h7)-1)*N+m(h1)))+(-0.5d0)*TMP
END DO
END DO
DO p2=nocc+1,N
DO h1=1,nocc
TMP=0.0d0
DO h7=1,nocc
TMP=TMP+t1(((m(p2)-1)*N+m(h7)))*i1(((m(h7)-1)*N+m(h1)))
END DO
i0(((m(p2)-1)*N+m(h1)))=i0(((m(p2)-1)*N+m(h1)))+(-1.0d0)*TMP
END DO
END DO
DO p2=nocc+1,N
DO p3=nocc+1,N
i1(((m(p2)-1)*N+m(p3)))=0.0d0
END DO
END DO
DO p2=nocc+1,N
DO p3=nocc+1,N
TMP=0.0d0
TMP=TMP+f1(((m(p2)-1)*N+m(p3)))
i1(((m(p2)-1)*N+m(p3)))=i1(((m(p2)-1)*N+m(p3)))+(1.0d0)*TMP
END DO
END DO
DO p2=nocc+1,N
DO p3=nocc+1,N
TMP=0.0d0
DO h5=1,nocc
DO p4=nocc+1,N
TMP=TMP+t1(((m(p4)-1)*N+m(h5)))*v2(((((m(h5)-1)*N+m(p2)-1)*N+m(p3)-1)*N+m(p4)))
END DO
END DO
i1(((m(p2)-1)*N+m(p3)))=i1(((m(p2)-1)*N+m(p3)))+(-1.0d0)*TMP
END DO
END DO
DO p2=nocc+1,N
DO h1=1,nocc
TMP=0.0d0
DO p3=nocc+1,N
TMP=TMP+t1(((m(p3)-1)*N+m(h1)))*i1(((m(p2)-1)*N+m(p3)))
END DO
i0(((m(p2)-1)*N+m(h1)))=i0(((m(p2)-1)*N+m(h1)))+(1.0d0)*TMP
END DO
END DO
DO p2=nocc+1,N
DO h1=1,nocc
TMP=0.0d0
DO h4=1,nocc
DO p3=nocc+1,N
TMP=TMP+t1(((m(p3)-1)*N+m(h4)))*v2(((((m(h4)-1)*N+m(p2)-1)*N+m(h1)-1)*N+m(p3)))
END DO
END DO
i0(((m(p2)-1)*N+m(h1)))=i0(((m(p2)-1)*N+m(h1)))+(-1.0d0)*TMP
END DO
END DO
DO h8=1,nocc
DO p7=nocc+1,N
i1(((m(h8)-1)*N+m(p7)))=0.0d0
END DO
END DO
DO h8=1,nocc
DO p7=nocc+1,N
TMP=0.0d0
TMP=TMP+f1(((m(h8)-1)*N+m(p7)))
i1(((m(h8)-1)*N+m(p7)))=i1(((m(h8)-1)*N+m(p7)))+(1.0d0)*TMP
END DO
END DO
DO h8=1,nocc
DO p7=nocc+1,N
TMP=0.0d0
DO h6=1,nocc
DO p5=nocc+1,N
TMP=TMP+t1(((m(p5)-1)*N+m(h6)))*v2(((((m(h6)-1)*N+m(h8)-1)*N+m(p5)-1)*N+m(p7)))
END DO
END DO
i1(((m(h8)-1)*N+m(p7)))=i1(((m(h8)-1)*N+m(p7)))+(1.0d0)*TMP
END DO
END DO
DO p2=nocc+1,N
DO h1=1,nocc
TMP=0.0d0
DO p7=nocc+1,N
DO h8=1,nocc
TMP=TMP+t2(((((m(p2)-1)*N+m(p7)-1)*N+m(h1)-1)*N+m(h8)))*i1(((m(h8)-1)*N+m(p7)))
END DO
END DO
i0(((m(p2)-1)*N+m(h1)))=i0(((m(p2)-1)*N+m(h1)))+(1.0d0)*TMP
END DO
END DO
DO h4=1,nocc
DO h5=1,nocc
DO h1=1,nocc
DO p3=nocc+1,N
i1(((((m(h4)-1)*N+m(h5)-1)*N+m(h1)-1)*N+m(p3)))=0.0d0
END DO
END DO
END DO
END DO
DO h4=1,nocc
DO h5=1,nocc
DO h1=1,nocc
DO p3=nocc+1,N
TMP=0.0d0
TMP=TMP+v2(((((m(h4)-1)*N+m(h5)-1)*N+m(h1)-1)*N+m(p3)))
i1(((((m(h4)-1)*N+m(h5)-1)*N+m(h1)-1)*N+m(p3)))=i1(((((m(h4)-1)*N+m(h5)-1)*N+m(h1)-1)*N+m(p3)))+(1.0d0)*TMP
END DO
END DO
END DO
END DO
DO h4=1,nocc
DO h5=1,nocc
DO h1=1,nocc
DO p3=nocc+1,N
TMP=0.0d0
DO p6=nocc+1,N
TMP=TMP+t1(((m(p6)-1)*N+m(h1)))*v2(((((m(h4)-1)*N+m(h5)-1)*N+m(p3)-1)*N+m(p6)))
END DO
i1(((((m(h4)-1)*N+m(h5)-1)*N+m(h1)-1)*N+m(p3)))=i1(((((m(h4)-1)*N+m(h5)-1)*N+m(h1)-1)*N+m(p3)))+(-1.0d0)*TMP
END DO
END DO
END DO
END DO
DO p2=nocc+1,N
DO h1=1,nocc
TMP=0.0d0
DO h4=1,nocc
DO h5=1,nocc
DO p3=nocc+1,N
TMP=TMP+t2(((((m(p2)-1)*N+m(p3)-1)*N+m(h4)-1)*N+m(h5)))*i1(((((m(h4)-1)*N+m(h5)-1)*N+m(h1)-1)*N+m(p3)))
END DO
END DO
END DO
i0(((m(p2)-1)*N+m(h1)))=i0(((m(p2)-1)*N+m(h1)))+(-0.5d0)*TMP
END DO
END DO
DO p2=nocc+1,N
DO h1=1,nocc
TMP=0.0d0
DO h5=1,nocc
DO p3=nocc+1,N
DO p4=nocc+1,N
TMP=TMP+t2(((((m(p3)-1)*N+m(p4)-1)*N+m(h1)-1)*N+m(h5)))*v2(((((m(h5)-1)*N+m(p2)-1)*N+m(p3)-1)*N+m(p4)))
END DO
END DO
END DO
i0(((m(p2)-1)*N+m(h1)))=i0(((m(p2)-1)*N+m(h1)))+(-0.5d0)*TMP
END DO
END DO
DO h4=1,nocc
DO p3=nocc+1,N
i2(((m(h4)-1)*N+m(p3)))=0.0d0
END DO
END DO
DO h4=1,nocc
DO p3=nocc+1,N
TMP=0.0d0
TMP=TMP+f1(((m(h4)-1)*N+m(p3)))
i2(((m(h4)-1)*N+m(p3)))=i2(((m(h4)-1)*N+m(p3)))+(1.0d0)*TMP
END DO
END DO
DO h4=1,nocc
DO p3=nocc+1,N
TMP=0.0d0
DO h6=1,nocc
DO p5=nocc+1,N
TMP=TMP+t1(((m(p5)-1)*N+m(h6)))*v2(((((m(h4)-1)*N+m(h6)-1)*N+m(p3)-1)*N+m(p5)))
END DO
END DO
i2(((m(h4)-1)*N+m(p3)))=i2(((m(h4)-1)*N+m(p3)))+(0.5d0)*TMP
END DO
END DO
i1(1)=0.0d0
TMP=0.0d0
DO h4=1,nocc
DO p3=nocc+1,N
TMP=TMP+t1(((m(p3)-1)*N+m(h4)))*i2(((m(h4)-1)*N+m(p3)))
END DO
END DO
i1(1)=i1(1)+(1.0d0)*TMP
TMP=0.0d0
DO h5=1,nocc
DO h6=1,nocc
DO p3=nocc+1,N
DO p4=nocc+1,N
TMP=TMP+t2(((((m(p3)-1)*N+m(p4)-1)*N+m(h5)-1)*N+m(h6)))*v2(((((m(h5)-1)*N+m(h6)-1)*N+m(p3)-1)*N+m(p4)))
END DO
END DO
END DO
END DO
i1(1)=i1(1)+(0.25d0)*TMP
DO p2=nocc+1,N
DO h1=1,nocc
TMP=0.0d0
TMP=TMP+t1(((m(p2)-1)*N+m(h1)))*i1(1)
i0(((m(p2)-1)*N+m(h1)))=i0(((m(p2)-1)*N+m(h1)))+(1.0d0)*TMP
END DO
END DO
DO p2=nocc+1,N
DO h1=1,nocc
TMP = 0.0d0
TMP = TMP + i0(((m(p2)-1)*N+m(h1)))
i0(((m(p2)-1)*N+m(h1))) = TMP / 1.0d0
END DO
END DO
RETURN
END SUBROUTINE
