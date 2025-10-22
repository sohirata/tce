SUBROUTINE np_ccsdt_t3(N,f1,i0,i1,i2,i3,nocc,t1,t2,t3,v2)
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
INTEGER :: p9
INTEGER :: p10
INTEGER :: p11
REAL*8 :: TMP
REAL*8 :: i0(*)
REAL*8 :: t2(*)
REAL*8 :: i1(*)
REAL*8 :: v2(*)
REAL*8 :: t1(*)
REAL*8 :: i2(*)
REAL*8 :: i3(*)
REAL*8 :: f1(*)
REAL*8 :: t3(*)
DO h11=1,nocc
DO p4=nocc+1,N
DO h1=1,nocc
DO h2=1,nocc
i1(((((h11-1)*N+p4-1)*N+h1-1)*N+h2))=0.0d0
END DO
END DO
END DO
END DO
DO h11=1,nocc
DO p4=nocc+1,N
DO h1=1,nocc
DO h2=1,nocc
TMP=0.0d0
TMP=TMP+v2(((((h11-1)*N+p4-1)*N+h1-1)*N+h2))
i1(((((h11-1)*N+p4-1)*N+h1-1)*N+h2))=i1(((((h11-1)*N+p4-1)*N+h1-1)*N+h2))+(1.0d0)*TMP
END DO
END DO
END DO
END DO
DO h10=1,nocc
DO h11=1,nocc
DO h1=1,nocc
DO h2=1,nocc
i2(((((h10-1)*N+h11-1)*N+h1-1)*N+h2))=0.0d0
END DO
END DO
END DO
END DO
DO h10=1,nocc
DO h11=1,nocc
DO h1=1,nocc
DO h2=1,nocc
TMP=0.0d0
TMP=TMP+v2(((((h10-1)*N+h11-1)*N+h1-1)*N+h2))
i2(((((h10-1)*N+h11-1)*N+h1-1)*N+h2))=i2(((((h10-1)*N+h11-1)*N+h1-1)*N+h2))+(1.0d0)*TMP
END DO
END DO
END DO
END DO
DO h10=1,nocc
DO h11=1,nocc
DO h1=1,nocc
DO p7=nocc+1,N
i3(((((h10-1)*N+h11-1)*N+h1-1)*N+p7))=0.0d0
END DO
END DO
END DO
END DO
DO h10=1,nocc
DO h11=1,nocc
DO h1=1,nocc
DO p7=nocc+1,N
TMP=0.0d0
TMP=TMP+v2(((((h10-1)*N+h11-1)*N+h1-1)*N+p7))
i3(((((h10-1)*N+h11-1)*N+h1-1)*N+p7))=i3(((((h10-1)*N+h11-1)*N+h1-1)*N+p7))+(1.0d0)*TMP
END DO
END DO
END DO
END DO
DO h10=1,nocc
DO h11=1,nocc
DO h1=1,nocc
DO p7=nocc+1,N
TMP=0.0d0
DO p8=nocc+1,N
TMP=TMP+t1(((p8-1)*N+h1))*v2(((((h10-1)*N+h11-1)*N+p7-1)*N+p8))
END DO
i3(((((h10-1)*N+h11-1)*N+h1-1)*N+p7))=i3(((((h10-1)*N+h11-1)*N+h1-1)*N+p7))+(-0.5d0)*TMP
END DO
END DO
END DO
END DO
DO h10=1,nocc
DO h11=1,nocc
DO h1=1,nocc
DO h2=1,nocc
TMP=0.0d0
DO p7=nocc+1,N
TMP=TMP+t1(((p7-1)*N+h1))*i3(((((h10-1)*N+h11-1)*N+h2-1)*N+p7))
END DO
i2(((((h10-1)*N+h11-1)*N+h1-1)*N+h2))=i2(((((h10-1)*N+h11-1)*N+h1-1)*N+h2))+(-1.0d0)*TMP
i2(((((h10-1)*N+h11-1)*N+h2-1)*N+h1))=i2(((((h10-1)*N+h11-1)*N+h2-1)*N+h1))+(1.0d0)*TMP
END DO
END DO
END DO
END DO
DO h10=1,nocc
DO h11=1,nocc
DO h1=1,nocc
DO h2=1,nocc
TMP=0.0d0
DO p8=nocc+1,N
DO p9=nocc+1,N
TMP=TMP+t2(((((p8-1)*N+p9-1)*N+h1-1)*N+h2))*v2(((((h10-1)*N+h11-1)*N+p8-1)*N+p9))
END DO
END DO
i2(((((h10-1)*N+h11-1)*N+h1-1)*N+h2))=i2(((((h10-1)*N+h11-1)*N+h1-1)*N+h2))+(0.5d0)*TMP
END DO
END DO
END DO
END DO
DO h11=1,nocc
DO p4=nocc+1,N
DO h1=1,nocc
DO h2=1,nocc
TMP=0.0d0
DO h10=1,nocc
TMP=TMP+t1(((p4-1)*N+h10))*i2(((((h10-1)*N+h11-1)*N+h1-1)*N+h2))
END DO
i1(((((h11-1)*N+p4-1)*N+h1-1)*N+h2))=i1(((((h11-1)*N+p4-1)*N+h1-1)*N+h2))+(1.0d0)*TMP
END DO
END DO
END DO
END DO
DO h11=1,nocc
DO p4=nocc+1,N
DO h1=1,nocc
DO p9=nocc+1,N
i2(((((h11-1)*N+p4-1)*N+h1-1)*N+p9))=0.0d0
END DO
END DO
END DO
END DO
DO h11=1,nocc
DO p4=nocc+1,N
DO h1=1,nocc
DO p9=nocc+1,N
TMP=0.0d0
TMP=TMP+v2(((((h11-1)*N+p4-1)*N+h1-1)*N+p9))
i2(((((h11-1)*N+p4-1)*N+h1-1)*N+p9))=i2(((((h11-1)*N+p4-1)*N+h1-1)*N+p9))+(1.0d0)*TMP
END DO
END DO
END DO
END DO
DO h11=1,nocc
DO p4=nocc+1,N
DO h1=1,nocc
DO p9=nocc+1,N
TMP=0.0d0
DO p8=nocc+1,N
TMP=TMP+t1(((p8-1)*N+h1))*v2(((((h11-1)*N+p4-1)*N+p8-1)*N+p9))
END DO
i2(((((h11-1)*N+p4-1)*N+h1-1)*N+p9))=i2(((((h11-1)*N+p4-1)*N+h1-1)*N+p9))+(0.5d0)*TMP
END DO
END DO
END DO
END DO
DO h11=1,nocc
DO p4=nocc+1,N
DO h1=1,nocc
DO h2=1,nocc
TMP=0.0d0
DO p9=nocc+1,N
TMP=TMP+t1(((p9-1)*N+h1))*i2(((((h11-1)*N+p4-1)*N+h2-1)*N+p9))
END DO
i1(((((h11-1)*N+p4-1)*N+h1-1)*N+h2))=i1(((((h11-1)*N+p4-1)*N+h1-1)*N+h2))+(-1.0d0)*TMP
i1(((((h11-1)*N+p4-1)*N+h2-1)*N+h1))=i1(((((h11-1)*N+p4-1)*N+h2-1)*N+h1))+(1.0d0)*TMP
END DO
END DO
END DO
END DO
DO h11=1,nocc
DO p7=nocc+1,N
i2(((h11-1)*N+p7))=0.0d0
END DO
END DO
DO h11=1,nocc
DO p7=nocc+1,N
TMP=0.0d0
TMP=TMP+f1(((h11-1)*N+p7))
i2(((h11-1)*N+p7))=i2(((h11-1)*N+p7))+(1.0d0)*TMP
END DO
END DO
DO h11=1,nocc
DO p7=nocc+1,N
TMP=0.0d0
DO h10=1,nocc
DO p9=nocc+1,N
TMP=TMP+t1(((p9-1)*N+h10))*v2(((((h10-1)*N+h11-1)*N+p7-1)*N+p9))
END DO
END DO
i2(((h11-1)*N+p7))=i2(((h11-1)*N+p7))+(-1.0d0)*TMP
END DO
END DO
DO h11=1,nocc
DO p4=nocc+1,N
DO h1=1,nocc
DO h2=1,nocc
TMP=0.0d0
DO p7=nocc+1,N
TMP=TMP+t2(((((p4-1)*N+p7-1)*N+h1-1)*N+h2))*i2(((h11-1)*N+p7))
END DO
i1(((((h11-1)*N+p4-1)*N+h1-1)*N+h2))=i1(((((h11-1)*N+p4-1)*N+h1-1)*N+h2))+(-1.0d0)*TMP
END DO
END DO
END DO
END DO
DO h9=1,nocc
DO h11=1,nocc
DO h1=1,nocc
DO p8=nocc+1,N
i2(((((h9-1)*N+h11-1)*N+h1-1)*N+p8))=0.0d0
END DO
END DO
END DO
END DO
DO h9=1,nocc
DO h11=1,nocc
DO h1=1,nocc
DO p8=nocc+1,N
TMP=0.0d0
TMP=TMP+v2(((((h9-1)*N+h11-1)*N+h1-1)*N+p8))
i2(((((h9-1)*N+h11-1)*N+h1-1)*N+p8))=i2(((((h9-1)*N+h11-1)*N+h1-1)*N+p8))+(1.0d0)*TMP
END DO
END DO
END DO
END DO
DO h9=1,nocc
DO h11=1,nocc
DO h1=1,nocc
DO p8=nocc+1,N
TMP=0.0d0
DO p10=nocc+1,N
TMP=TMP+t1(((p10-1)*N+h1))*v2(((((h9-1)*N+h11-1)*N+p8-1)*N+p10))
END DO
i2(((((h9-1)*N+h11-1)*N+h1-1)*N+p8))=i2(((((h9-1)*N+h11-1)*N+h1-1)*N+p8))+(-1.0d0)*TMP
END DO
END DO
END DO
END DO
DO h11=1,nocc
DO p4=nocc+1,N
DO h1=1,nocc
DO h2=1,nocc
TMP=0.0d0
DO h9=1,nocc
DO p8=nocc+1,N
TMP=TMP+t2(((((p4-1)*N+p8-1)*N+h1-1)*N+h9))*i2(((((h9-1)*N+h11-1)*N+h2-1)*N+p8))
END DO
END DO
i1(((((h11-1)*N+p4-1)*N+h1-1)*N+h2))=i1(((((h11-1)*N+p4-1)*N+h1-1)*N+h2))+(1.0d0)*TMP
i1(((((h11-1)*N+p4-1)*N+h2-1)*N+h1))=i1(((((h11-1)*N+p4-1)*N+h2-1)*N+h1))+(-1.0d0)*TMP
END DO
END DO
END DO
END DO
DO h11=1,nocc
DO p4=nocc+1,N
DO h1=1,nocc
DO h2=1,nocc
TMP=0.0d0
DO p8=nocc+1,N
DO p9=nocc+1,N
TMP=TMP+t2(((((p8-1)*N+p9-1)*N+h1-1)*N+h2))*v2(((((h11-1)*N+p4-1)*N+p8-1)*N+p9))
END DO
END DO
i1(((((h11-1)*N+p4-1)*N+h1-1)*N+h2))=i1(((((h11-1)*N+p4-1)*N+h1-1)*N+h2))+(0.5d0)*TMP
END DO
END DO
END DO
END DO
DO h11=1,nocc
DO p4=nocc+1,N
DO h1=1,nocc
DO h2=1,nocc
TMP=0.0d0
DO h9=1,nocc
DO p7=nocc+1,N
DO p8=nocc+1,N
TMP=TMP+t3(((((((p4-1)*N+p7-1)*N+p8-1)*N+h1-1)*N+h2-1)*N+h9))*v2(((((h9-1)*N+h11-1)*N+p7-1)*N+p8))
END DO
END DO
END DO
i1(((((h11-1)*N+p4-1)*N+h1-1)*N+h2))=i1(((((h11-1)*N+p4-1)*N+h1-1)*N+h2))+(0.5d0)*TMP
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p5=nocc+1,N
DO p6=nocc+1,N
DO h1=1,nocc
DO h2=1,nocc
DO h3=1,nocc
i0(((((((p4-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h3))=0.0d0
END DO
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p5=nocc+1,N
DO p6=nocc+1,N
DO h1=1,nocc
DO h2=1,nocc
DO h3=1,nocc
TMP=0.0d0
DO h11=1,nocc
TMP=TMP+t2(((((p4-1)*N+p5-1)*N+h1-1)*N+h11))*i1(((((h11-1)*N+p6-1)*N+h2-1)*N+h3))
END DO
i0(((((((p4-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h3))=i0(((((((p4-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h3))+(-1.0d0)*TMP
i0(((((((p4-1)*N+p5-1)*N+p6-1)*N+h2-1)*N+h1-1)*N+h3))=i0(((((((p4-1)*N+p5-1)*N+p6-1)*N+h2-1)*N+h1-1)*N+h3))+(1.0d0)*TMP
i0(((((((p4-1)*N+p5-1)*N+p6-1)*N+h2-1)*N+h3-1)*N+h1))=i0(((((((p4-1)*N+p5-1)*N+p6-1)*N+h2-1)*N+h3-1)*N+h1))+(-1.0d0)*TMP
i0(((((((p6-1)*N+p4-1)*N+p5-1)*N+h1-1)*N+h2-1)*N+h3))=i0(((((((p6-1)*N+p4-1)*N+p5-1)*N+h1-1)*N+h2-1)*N+h3))+(-1.0d0)*TMP
i0(((((((p6-1)*N+p4-1)*N+p5-1)*N+h2-1)*N+h1-1)*N+h3))=i0(((((((p6-1)*N+p4-1)*N+p5-1)*N+h2-1)*N+h1-1)*N+h3))+(1.0d0)*TMP
i0(((((((p6-1)*N+p4-1)*N+p5-1)*N+h2-1)*N+h3-1)*N+h1))=i0(((((((p6-1)*N+p4-1)*N+p5-1)*N+h2-1)*N+h3-1)*N+h1))+(-1.0d0)*TMP
i0(((((((p4-1)*N+p6-1)*N+p5-1)*N+h1-1)*N+h2-1)*N+h3))=i0(((((((p4-1)*N+p6-1)*N+p5-1)*N+h1-1)*N+h2-1)*N+h3))+(1.0d0)*TMP
i0(((((((p4-1)*N+p6-1)*N+p5-1)*N+h2-1)*N+h1-1)*N+h3))=i0(((((((p4-1)*N+p6-1)*N+p5-1)*N+h2-1)*N+h1-1)*N+h3))+(-1.0d0)*TMP
i0(((((((p4-1)*N+p6-1)*N+p5-1)*N+h2-1)*N+h3-1)*N+h1))=i0(((((((p4-1)*N+p6-1)*N+p5-1)*N+h2-1)*N+h3-1)*N+h1))+(1.0d0)*TMP
END DO
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p5=nocc+1,N
DO h1=1,nocc
DO p12=nocc+1,N
i1(((((p4-1)*N+p5-1)*N+h1-1)*N+p12))=0.0d0
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p5=nocc+1,N
DO h1=1,nocc
DO p12=nocc+1,N
TMP=0.0d0
TMP=TMP+v2(((((p4-1)*N+p5-1)*N+h1-1)*N+p12))
i1(((((p4-1)*N+p5-1)*N+h1-1)*N+p12))=i1(((((p4-1)*N+p5-1)*N+h1-1)*N+p12))+(1.0d0)*TMP
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p5=nocc+1,N
DO h1=1,nocc
DO p12=nocc+1,N
TMP=0.0d0
DO p8=nocc+1,N
TMP=TMP+t1(((p8-1)*N+h1))*v2(((((p4-1)*N+p5-1)*N+p8-1)*N+p12))
END DO
i1(((((p4-1)*N+p5-1)*N+h1-1)*N+p12))=i1(((((p4-1)*N+p5-1)*N+h1-1)*N+p12))+(1.0d0)*TMP
END DO
END DO
END DO
END DO
DO h8=1,nocc
DO h9=1,nocc
DO h1=1,nocc
DO p12=nocc+1,N
i2(((((h8-1)*N+h9-1)*N+h1-1)*N+p12))=0.0d0
END DO
END DO
END DO
END DO
DO h8=1,nocc
DO h9=1,nocc
DO h1=1,nocc
DO p12=nocc+1,N
TMP=0.0d0
TMP=TMP+v2(((((h8-1)*N+h9-1)*N+h1-1)*N+p12))
i2(((((h8-1)*N+h9-1)*N+h1-1)*N+p12))=i2(((((h8-1)*N+h9-1)*N+h1-1)*N+p12))+(1.0d0)*TMP
END DO
END DO
END DO
END DO
DO h8=1,nocc
DO h9=1,nocc
DO h1=1,nocc
DO p12=nocc+1,N
TMP=0.0d0
DO p10=nocc+1,N
TMP=TMP+t1(((p10-1)*N+h1))*v2(((((h8-1)*N+h9-1)*N+p10-1)*N+p12))
END DO
i2(((((h8-1)*N+h9-1)*N+h1-1)*N+p12))=i2(((((h8-1)*N+h9-1)*N+h1-1)*N+p12))+(1.0d0)*TMP
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p5=nocc+1,N
DO h1=1,nocc
DO p12=nocc+1,N
TMP=0.0d0
DO h8=1,nocc
DO h9=1,nocc
TMP=TMP+t2(((((p4-1)*N+p5-1)*N+h8-1)*N+h9))*i2(((((h8-1)*N+h9-1)*N+h1-1)*N+p12))
END DO
END DO
i1(((((p4-1)*N+p5-1)*N+h1-1)*N+p12))=i1(((((p4-1)*N+p5-1)*N+h1-1)*N+p12))+(0.5d0)*TMP
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p5=nocc+1,N
DO h1=1,nocc
DO p12=nocc+1,N
TMP=0.0d0
DO h9=1,nocc
DO p8=nocc+1,N
TMP=TMP+t2(((((p4-1)*N+p8-1)*N+h1-1)*N+h9))*v2(((((h9-1)*N+p5-1)*N+p8-1)*N+p12))
END DO
END DO
i1(((((p4-1)*N+p5-1)*N+h1-1)*N+p12))=i1(((((p4-1)*N+p5-1)*N+h1-1)*N+p12))+(1.0d0)*TMP
i1(((((p5-1)*N+p4-1)*N+h1-1)*N+p12))=i1(((((p5-1)*N+p4-1)*N+h1-1)*N+p12))+(-1.0d0)*TMP
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p5=nocc+1,N
DO h1=1,nocc
DO p12=nocc+1,N
TMP=0.0d0
DO h8=1,nocc
DO h9=1,nocc
DO p7=nocc+1,N
TMP=TMP+t3(((((((p4-1)*N+p5-1)*N+p7-1)*N+h1-1)*N+h8-1)*N+h9))*v2(((((h8-1)*N+h9-1)*N+p7-1)*N+p12))
END DO
END DO
END DO
i1(((((p4-1)*N+p5-1)*N+h1-1)*N+p12))=i1(((((p4-1)*N+p5-1)*N+h1-1)*N+p12))+(0.5d0)*TMP
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p5=nocc+1,N
DO p6=nocc+1,N
DO h1=1,nocc
DO h2=1,nocc
DO h3=1,nocc
TMP=0.0d0
DO p12=nocc+1,N
TMP=TMP+t2(((((p4-1)*N+p12-1)*N+h1-1)*N+h2))*i1(((((p5-1)*N+p6-1)*N+h3-1)*N+p12))
END DO
i0(((((((p4-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h3))=i0(((((((p4-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h3))+(-1.0d0)*TMP
i0(((((((p4-1)*N+p5-1)*N+p6-1)*N+h3-1)*N+h1-1)*N+h2))=i0(((((((p4-1)*N+p5-1)*N+p6-1)*N+h3-1)*N+h1-1)*N+h2))+(-1.0d0)*TMP
i0(((((((p4-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h3-1)*N+h2))=i0(((((((p4-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h3-1)*N+h2))+(1.0d0)*TMP
i0(((((((p5-1)*N+p4-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h3))=i0(((((((p5-1)*N+p4-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h3))+(1.0d0)*TMP
i0(((((((p5-1)*N+p4-1)*N+p6-1)*N+h3-1)*N+h1-1)*N+h2))=i0(((((((p5-1)*N+p4-1)*N+p6-1)*N+h3-1)*N+h1-1)*N+h2))+(1.0d0)*TMP
i0(((((((p5-1)*N+p4-1)*N+p6-1)*N+h1-1)*N+h3-1)*N+h2))=i0(((((((p5-1)*N+p4-1)*N+p6-1)*N+h1-1)*N+h3-1)*N+h2))+(-1.0d0)*TMP
i0(((((((p5-1)*N+p6-1)*N+p4-1)*N+h1-1)*N+h2-1)*N+h3))=i0(((((((p5-1)*N+p6-1)*N+p4-1)*N+h1-1)*N+h2-1)*N+h3))+(-1.0d0)*TMP
i0(((((((p5-1)*N+p6-1)*N+p4-1)*N+h3-1)*N+h1-1)*N+h2))=i0(((((((p5-1)*N+p6-1)*N+p4-1)*N+h3-1)*N+h1-1)*N+h2))+(-1.0d0)*TMP
i0(((((((p5-1)*N+p6-1)*N+p4-1)*N+h1-1)*N+h3-1)*N+h2))=i0(((((((p5-1)*N+p6-1)*N+p4-1)*N+h1-1)*N+h3-1)*N+h2))+(1.0d0)*TMP
END DO
END DO
END DO
END DO
END DO
END DO
DO h11=1,nocc
DO h1=1,nocc
i1(((h11-1)*N+h1))=0.0d0
END DO
END DO
DO h11=1,nocc
DO h1=1,nocc
TMP=0.0d0
TMP=TMP+f1(((h11-1)*N+h1))
i1(((h11-1)*N+h1))=i1(((h11-1)*N+h1))+(1.0d0)*TMP
END DO
END DO
DO h11=1,nocc
DO p10=nocc+1,N
i2(((h11-1)*N+p10))=0.0d0
END DO
END DO
DO h11=1,nocc
DO p10=nocc+1,N
TMP=0.0d0
TMP=TMP+f1(((h11-1)*N+p10))
i2(((h11-1)*N+p10))=i2(((h11-1)*N+p10))+(1.0d0)*TMP
END DO
END DO
DO h11=1,nocc
DO p10=nocc+1,N
TMP=0.0d0
DO h9=1,nocc
DO p8=nocc+1,N
TMP=TMP+t1(((p8-1)*N+h9))*v2(((((h9-1)*N+h11-1)*N+p8-1)*N+p10))
END DO
END DO
i2(((h11-1)*N+p10))=i2(((h11-1)*N+p10))+(1.0d0)*TMP
END DO
END DO
DO h11=1,nocc
DO h1=1,nocc
TMP=0.0d0
DO p10=nocc+1,N
TMP=TMP+t1(((p10-1)*N+h1))*i2(((h11-1)*N+p10))
END DO
i1(((h11-1)*N+h1))=i1(((h11-1)*N+h1))+(1.0d0)*TMP
END DO
END DO
DO h11=1,nocc
DO h1=1,nocc
TMP=0.0d0
DO h9=1,nocc
DO p8=nocc+1,N
TMP=TMP+t1(((p8-1)*N+h9))*v2(((((h9-1)*N+h11-1)*N+h1-1)*N+p8))
END DO
END DO
i1(((h11-1)*N+h1))=i1(((h11-1)*N+h1))+(-1.0d0)*TMP
END DO
END DO
DO h11=1,nocc
DO h1=1,nocc
TMP=0.0d0
DO h10=1,nocc
DO p8=nocc+1,N
DO p9=nocc+1,N
TMP=TMP+t2(((((p8-1)*N+p9-1)*N+h1-1)*N+h10))*v2(((((h10-1)*N+h11-1)*N+p8-1)*N+p9))
END DO
END DO
END DO
i1(((h11-1)*N+h1))=i1(((h11-1)*N+h1))+(-0.5d0)*TMP
END DO
END DO
DO p4=nocc+1,N
DO p5=nocc+1,N
DO p6=nocc+1,N
DO h1=1,nocc
DO h2=1,nocc
DO h3=1,nocc
TMP=0.0d0
DO h11=1,nocc
TMP=TMP+t3(((((((p4-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h11))*i1(((h11-1)*N+h3))
END DO
i0(((((((p4-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h3))=i0(((((((p4-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h3))+(-1.0d0)*TMP
i0(((((((p4-1)*N+p5-1)*N+p6-1)*N+h3-1)*N+h1-1)*N+h2))=i0(((((((p4-1)*N+p5-1)*N+p6-1)*N+h3-1)*N+h1-1)*N+h2))+(-1.0d0)*TMP
i0(((((((p4-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h3-1)*N+h2))=i0(((((((p4-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h3-1)*N+h2))+(1.0d0)*TMP
END DO
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p7=nocc+1,N
i1(((p4-1)*N+p7))=0.0d0
END DO
END DO
DO p4=nocc+1,N
DO p7=nocc+1,N
TMP=0.0d0
TMP=TMP+f1(((p4-1)*N+p7))
i1(((p4-1)*N+p7))=i1(((p4-1)*N+p7))+(1.0d0)*TMP
END DO
END DO
DO p4=nocc+1,N
DO p7=nocc+1,N
TMP=0.0d0
DO h9=1,nocc
DO p8=nocc+1,N
TMP=TMP+t1(((p8-1)*N+h9))*v2(((((h9-1)*N+p4-1)*N+p7-1)*N+p8))
END DO
END DO
i1(((p4-1)*N+p7))=i1(((p4-1)*N+p7))+(-1.0d0)*TMP
END DO
END DO
DO p4=nocc+1,N
DO p7=nocc+1,N
TMP=0.0d0
DO h9=1,nocc
DO h10=1,nocc
DO p8=nocc+1,N
TMP=TMP+t2(((((p4-1)*N+p8-1)*N+h9-1)*N+h10))*v2(((((h9-1)*N+h10-1)*N+p7-1)*N+p8))
END DO
END DO
END DO
i1(((p4-1)*N+p7))=i1(((p4-1)*N+p7))+(-0.5d0)*TMP
END DO
END DO
DO p4=nocc+1,N
DO p5=nocc+1,N
DO p6=nocc+1,N
DO h1=1,nocc
DO h2=1,nocc
DO h3=1,nocc
TMP=0.0d0
DO p7=nocc+1,N
TMP=TMP+t3(((((((p4-1)*N+p5-1)*N+p7-1)*N+h1-1)*N+h2-1)*N+h3))*i1(((p6-1)*N+p7))
END DO
i0(((((((p4-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h3))=i0(((((((p4-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h3))+(1.0d0)*TMP
i0(((((((p6-1)*N+p4-1)*N+p5-1)*N+h1-1)*N+h2-1)*N+h3))=i0(((((((p6-1)*N+p4-1)*N+p5-1)*N+h1-1)*N+h2-1)*N+h3))+(1.0d0)*TMP
i0(((((((p4-1)*N+p6-1)*N+p5-1)*N+h1-1)*N+h2-1)*N+h3))=i0(((((((p4-1)*N+p6-1)*N+p5-1)*N+h1-1)*N+h2-1)*N+h3))+(-1.0d0)*TMP
END DO
END DO
END DO
END DO
END DO
END DO
DO h11=1,nocc
DO h12=1,nocc
DO h1=1,nocc
DO h2=1,nocc
i1(((((h11-1)*N+h12-1)*N+h1-1)*N+h2))=0.0d0
END DO
END DO
END DO
END DO
DO h11=1,nocc
DO h12=1,nocc
DO h1=1,nocc
DO h2=1,nocc
TMP=0.0d0
TMP=TMP+v2(((((h11-1)*N+h12-1)*N+h1-1)*N+h2))
i1(((((h11-1)*N+h12-1)*N+h1-1)*N+h2))=i1(((((h11-1)*N+h12-1)*N+h1-1)*N+h2))+(1.0d0)*TMP
END DO
END DO
END DO
END DO
DO h11=1,nocc
DO h12=1,nocc
DO h1=1,nocc
DO p10=nocc+1,N
i2(((((h11-1)*N+h12-1)*N+h1-1)*N+p10))=0.0d0
END DO
END DO
END DO
END DO
DO h11=1,nocc
DO h12=1,nocc
DO h1=1,nocc
DO p10=nocc+1,N
TMP=0.0d0
TMP=TMP+v2(((((h11-1)*N+h12-1)*N+h1-1)*N+p10))
i2(((((h11-1)*N+h12-1)*N+h1-1)*N+p10))=i2(((((h11-1)*N+h12-1)*N+h1-1)*N+p10))+(1.0d0)*TMP
END DO
END DO
END DO
END DO
DO h11=1,nocc
DO h12=1,nocc
DO h1=1,nocc
DO p10=nocc+1,N
TMP=0.0d0
DO p8=nocc+1,N
TMP=TMP+t1(((p8-1)*N+h1))*v2(((((h11-1)*N+h12-1)*N+p8-1)*N+p10))
END DO
i2(((((h11-1)*N+h12-1)*N+h1-1)*N+p10))=i2(((((h11-1)*N+h12-1)*N+h1-1)*N+p10))+(0.5d0)*TMP
END DO
END DO
END DO
END DO
DO h11=1,nocc
DO h12=1,nocc
DO h1=1,nocc
DO h2=1,nocc
TMP=0.0d0
DO p10=nocc+1,N
TMP=TMP+t1(((p10-1)*N+h1))*i2(((((h11-1)*N+h12-1)*N+h2-1)*N+p10))
END DO
i1(((((h11-1)*N+h12-1)*N+h1-1)*N+h2))=i1(((((h11-1)*N+h12-1)*N+h1-1)*N+h2))+(-1.0d0)*TMP
i1(((((h11-1)*N+h12-1)*N+h2-1)*N+h1))=i1(((((h11-1)*N+h12-1)*N+h2-1)*N+h1))+(1.0d0)*TMP
END DO
END DO
END DO
END DO
DO h11=1,nocc
DO h12=1,nocc
DO h1=1,nocc
DO h2=1,nocc
TMP=0.0d0
DO p9=nocc+1,N
DO p10=nocc+1,N
TMP=TMP+t2(((((p9-1)*N+p10-1)*N+h1-1)*N+h2))*v2(((((h11-1)*N+h12-1)*N+p9-1)*N+p10))
END DO
END DO
i1(((((h11-1)*N+h12-1)*N+h1-1)*N+h2))=i1(((((h11-1)*N+h12-1)*N+h1-1)*N+h2))+(0.5d0)*TMP
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p5=nocc+1,N
DO p6=nocc+1,N
DO h1=1,nocc
DO h2=1,nocc
DO h3=1,nocc
TMP=0.0d0
DO h11=1,nocc
DO h12=1,nocc
TMP=TMP+t3(((((((p4-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h11-1)*N+h12))*i1(((((h11-1)*N+h12-1)*N+h2-1)*N+h3))
END DO
END DO
i0(((((((p4-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h3))=i0(((((((p4-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h3))+(0.5d0)*TMP
i0(((((((p4-1)*N+p5-1)*N+p6-1)*N+h2-1)*N+h1-1)*N+h3))=i0(((((((p4-1)*N+p5-1)*N+p6-1)*N+h2-1)*N+h1-1)*N+h3))+(-0.5d0)*TMP
i0(((((((p4-1)*N+p5-1)*N+p6-1)*N+h2-1)*N+h3-1)*N+h1))=i0(((((((p4-1)*N+p5-1)*N+p6-1)*N+h2-1)*N+h3-1)*N+h1))+(0.5d0)*TMP
END DO
END DO
END DO
END DO
END DO
END DO
DO h8=1,nocc
DO p4=nocc+1,N
DO h1=1,nocc
DO p7=nocc+1,N
i1(((((h8-1)*N+p4-1)*N+h1-1)*N+p7))=0.0d0
END DO
END DO
END DO
END DO
DO h8=1,nocc
DO p4=nocc+1,N
DO h1=1,nocc
DO p7=nocc+1,N
TMP=0.0d0
TMP=TMP+v2(((((h8-1)*N+p4-1)*N+h1-1)*N+p7))
i1(((((h8-1)*N+p4-1)*N+h1-1)*N+p7))=i1(((((h8-1)*N+p4-1)*N+h1-1)*N+p7))+(1.0d0)*TMP
END DO
END DO
END DO
END DO
DO h8=1,nocc
DO p4=nocc+1,N
DO h1=1,nocc
DO p7=nocc+1,N
TMP=0.0d0
DO p9=nocc+1,N
TMP=TMP+t1(((p9-1)*N+h1))*v2(((((h8-1)*N+p4-1)*N+p7-1)*N+p9))
END DO
i1(((((h8-1)*N+p4-1)*N+h1-1)*N+p7))=i1(((((h8-1)*N+p4-1)*N+h1-1)*N+p7))+(-1.0d0)*TMP
END DO
END DO
END DO
END DO
DO h8=1,nocc
DO p4=nocc+1,N
DO h1=1,nocc
DO p7=nocc+1,N
TMP=0.0d0
DO h10=1,nocc
DO p9=nocc+1,N
TMP=TMP+t2(((((p4-1)*N+p9-1)*N+h1-1)*N+h10))*v2(((((h8-1)*N+h10-1)*N+p7-1)*N+p9))
END DO
END DO
i1(((((h8-1)*N+p4-1)*N+h1-1)*N+p7))=i1(((((h8-1)*N+p4-1)*N+h1-1)*N+p7))+(-1.0d0)*TMP
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p5=nocc+1,N
DO p6=nocc+1,N
DO h1=1,nocc
DO h2=1,nocc
DO h3=1,nocc
TMP=0.0d0
DO h8=1,nocc
DO p7=nocc+1,N
TMP=TMP+t3(((((((p4-1)*N+p5-1)*N+p7-1)*N+h1-1)*N+h2-1)*N+h8))*i1(((((h8-1)*N+p6-1)*N+h3-1)*N+p7))
END DO
END DO
i0(((((((p4-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h3))=i0(((((((p4-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h3))+(-1.0d0)*TMP
i0(((((((p4-1)*N+p5-1)*N+p6-1)*N+h3-1)*N+h1-1)*N+h2))=i0(((((((p4-1)*N+p5-1)*N+p6-1)*N+h3-1)*N+h1-1)*N+h2))+(-1.0d0)*TMP
i0(((((((p4-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h3-1)*N+h2))=i0(((((((p4-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h3-1)*N+h2))+(1.0d0)*TMP
i0(((((((p6-1)*N+p4-1)*N+p5-1)*N+h1-1)*N+h2-1)*N+h3))=i0(((((((p6-1)*N+p4-1)*N+p5-1)*N+h1-1)*N+h2-1)*N+h3))+(-1.0d0)*TMP
i0(((((((p6-1)*N+p4-1)*N+p5-1)*N+h3-1)*N+h1-1)*N+h2))=i0(((((((p6-1)*N+p4-1)*N+p5-1)*N+h3-1)*N+h1-1)*N+h2))+(-1.0d0)*TMP
i0(((((((p6-1)*N+p4-1)*N+p5-1)*N+h1-1)*N+h3-1)*N+h2))=i0(((((((p6-1)*N+p4-1)*N+p5-1)*N+h1-1)*N+h3-1)*N+h2))+(1.0d0)*TMP
i0(((((((p4-1)*N+p6-1)*N+p5-1)*N+h1-1)*N+h2-1)*N+h3))=i0(((((((p4-1)*N+p6-1)*N+p5-1)*N+h1-1)*N+h2-1)*N+h3))+(1.0d0)*TMP
i0(((((((p4-1)*N+p6-1)*N+p5-1)*N+h3-1)*N+h1-1)*N+h2))=i0(((((((p4-1)*N+p6-1)*N+p5-1)*N+h3-1)*N+h1-1)*N+h2))+(1.0d0)*TMP
i0(((((((p4-1)*N+p6-1)*N+p5-1)*N+h1-1)*N+h3-1)*N+h2))=i0(((((((p4-1)*N+p6-1)*N+p5-1)*N+h1-1)*N+h3-1)*N+h2))+(-1.0d0)*TMP
END DO
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p5=nocc+1,N
DO p6=nocc+1,N
DO h1=1,nocc
DO h2=1,nocc
DO h3=1,nocc
TMP=0.0d0
DO p7=nocc+1,N
DO p8=nocc+1,N
TMP=TMP+t3(((((((p4-1)*N+p7-1)*N+p8-1)*N+h1-1)*N+h2-1)*N+h3))*v2(((((p5-1)*N+p6-1)*N+p7-1)*N+p8))
END DO
END DO
i0(((((((p4-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h3))=i0(((((((p4-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h3))+(0.5d0)*TMP
i0(((((((p5-1)*N+p4-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h3))=i0(((((((p5-1)*N+p4-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h3))+(-0.5d0)*TMP
i0(((((((p5-1)*N+p6-1)*N+p4-1)*N+h1-1)*N+h2-1)*N+h3))=i0(((((((p5-1)*N+p6-1)*N+p4-1)*N+h1-1)*N+h2-1)*N+h3))+(0.5d0)*TMP
END DO
END DO
END DO
END DO
END DO
END DO
DO h12=1,nocc
DO p4=nocc+1,N
DO h1=1,nocc
DO p10=nocc+1,N
i2(((((h12-1)*N+p4-1)*N+h1-1)*N+p10))=0.0d0
END DO
END DO
END DO
END DO
DO h12=1,nocc
DO p4=nocc+1,N
DO h1=1,nocc
DO p10=nocc+1,N
TMP=0.0d0
TMP=TMP+v2(((((h12-1)*N+p4-1)*N+h1-1)*N+p10))
i2(((((h12-1)*N+p4-1)*N+h1-1)*N+p10))=i2(((((h12-1)*N+p4-1)*N+h1-1)*N+p10))+(1.0d0)*TMP
END DO
END DO
END DO
END DO
DO h11=1,nocc
DO h12=1,nocc
DO h1=1,nocc
DO p10=nocc+1,N
i3(((((h11-1)*N+h12-1)*N+h1-1)*N+p10))=0.0d0
END DO
END DO
END DO
END DO
DO h11=1,nocc
DO h12=1,nocc
DO h1=1,nocc
DO p10=nocc+1,N
TMP=0.0d0
TMP=TMP+v2(((((h11-1)*N+h12-1)*N+h1-1)*N+p10))
i3(((((h11-1)*N+h12-1)*N+h1-1)*N+p10))=i3(((((h11-1)*N+h12-1)*N+h1-1)*N+p10))+(1.0d0)*TMP
END DO
END DO
END DO
END DO
DO h11=1,nocc
DO h12=1,nocc
DO h1=1,nocc
DO p10=nocc+1,N
TMP=0.0d0
DO p7=nocc+1,N
TMP=TMP+t1(((p7-1)*N+h1))*v2(((((h11-1)*N+h12-1)*N+p7-1)*N+p10))
END DO
i3(((((h11-1)*N+h12-1)*N+h1-1)*N+p10))=i3(((((h11-1)*N+h12-1)*N+h1-1)*N+p10))+(1.0d0)*TMP
END DO
END DO
END DO
END DO
DO h12=1,nocc
DO p4=nocc+1,N
DO h1=1,nocc
DO p10=nocc+1,N
TMP=0.0d0
DO h11=1,nocc
TMP=TMP+t1(((p4-1)*N+h11))*i3(((((h11-1)*N+h12-1)*N+h1-1)*N+p10))
END DO
i2(((((h12-1)*N+p4-1)*N+h1-1)*N+p10))=i2(((((h12-1)*N+p4-1)*N+h1-1)*N+p10))+(0.5d0)*TMP
END DO
END DO
END DO
END DO
DO h12=1,nocc
DO p4=nocc+1,N
DO h1=1,nocc
DO p10=nocc+1,N
TMP=0.0d0
DO p7=nocc+1,N
TMP=TMP+t1(((p7-1)*N+h1))*v2(((((h12-1)*N+p4-1)*N+p7-1)*N+p10))
END DO
i2(((((h12-1)*N+p4-1)*N+h1-1)*N+p10))=i2(((((h12-1)*N+p4-1)*N+h1-1)*N+p10))+(1.0d0)*TMP
END DO
END DO
END DO
END DO
DO h12=1,nocc
DO p4=nocc+1,N
DO h1=1,nocc
DO p10=nocc+1,N
TMP=0.0d0
DO h9=1,nocc
DO p8=nocc+1,N
TMP=TMP+t2(((((p4-1)*N+p8-1)*N+h1-1)*N+h9))*v2(((((h9-1)*N+h12-1)*N+p8-1)*N+p10))
END DO
END DO
i2(((((h12-1)*N+p4-1)*N+h1-1)*N+p10))=i2(((((h12-1)*N+p4-1)*N+h1-1)*N+p10))+(-1.0d0)*TMP
END DO
END DO
END DO
END DO
DO h12=1,nocc
DO p4=nocc+1,N
DO p5=nocc+1,N
DO h1=1,nocc
DO h2=1,nocc
DO h3=1,nocc
i1(((((((h12-1)*N+p4-1)*N+p5-1)*N+h1-1)*N+h2-1)*N+h3))=0.0d0
END DO
END DO
END DO
END DO
END DO
END DO
DO h12=1,nocc
DO p4=nocc+1,N
DO p5=nocc+1,N
DO h1=1,nocc
DO h2=1,nocc
DO h3=1,nocc
TMP=0.0d0
DO p10=nocc+1,N
TMP=TMP+t2(((((p4-1)*N+p10-1)*N+h1-1)*N+h2))*i2(((((h12-1)*N+p5-1)*N+h3-1)*N+p10))
END DO
i1(((((((h12-1)*N+p4-1)*N+p5-1)*N+h1-1)*N+h2-1)*N+h3))=i1(((((((h12-1)*N+p4-1)*N+p5-1)*N+h1-1)*N+h2-1)*N+h3))+(-0.16666666666666666&
&d0)*TMP
i1(((((((h12-1)*N+p4-1)*N+p5-1)*N+h3-1)*N+h1-1)*N+h2))=i1(((((((h12-1)*N+p4-1)*N+p5-1)*N+h3-1)*N+h1-1)*N+h2))+(-0.16666666666666666&
&d0)*TMP
i1(((((((h12-1)*N+p4-1)*N+p5-1)*N+h1-1)*N+h3-1)*N+h2))=i1(((((((h12-1)*N+p4-1)*N+p5-1)*N+h1-1)*N+h3-1)*N+h2))+(0.16666666666666666d&
&0)*TMP
i1(((((((h12-1)*N+p5-1)*N+p4-1)*N+h1-1)*N+h2-1)*N+h3))=i1(((((((h12-1)*N+p5-1)*N+p4-1)*N+h1-1)*N+h2-1)*N+h3))+(0.16666666666666666d&
&0)*TMP
i1(((((((h12-1)*N+p5-1)*N+p4-1)*N+h3-1)*N+h1-1)*N+h2))=i1(((((((h12-1)*N+p5-1)*N+p4-1)*N+h3-1)*N+h1-1)*N+h2))+(0.16666666666666666d&
&0)*TMP
i1(((((((h12-1)*N+p5-1)*N+p4-1)*N+h1-1)*N+h3-1)*N+h2))=i1(((((((h12-1)*N+p5-1)*N+p4-1)*N+h1-1)*N+h3-1)*N+h2))+(-0.16666666666666666&
&d0)*TMP
END DO
END DO
END DO
END DO
END DO
END DO
DO h12=1,nocc
DO p7=nocc+1,N
i2(((h12-1)*N+p7))=0.0d0
END DO
END DO
DO h12=1,nocc
DO p7=nocc+1,N
TMP=0.0d0
TMP=TMP+f1(((h12-1)*N+p7))
i2(((h12-1)*N+p7))=i2(((h12-1)*N+p7))+(1.0d0)*TMP
END DO
END DO
DO h12=1,nocc
DO p7=nocc+1,N
TMP=0.0d0
DO h9=1,nocc
DO p8=nocc+1,N
TMP=TMP+t1(((p8-1)*N+h9))*v2(((((h9-1)*N+h12-1)*N+p7-1)*N+p8))
END DO
END DO
i2(((h12-1)*N+p7))=i2(((h12-1)*N+p7))+(-1.0d0)*TMP
END DO
END DO
DO h12=1,nocc
DO p4=nocc+1,N
DO p5=nocc+1,N
DO h1=1,nocc
DO h2=1,nocc
DO h3=1,nocc
TMP=0.0d0
DO p7=nocc+1,N
TMP=TMP+t3(((((((p4-1)*N+p5-1)*N+p7-1)*N+h1-1)*N+h2-1)*N+h3))*i2(((h12-1)*N+p7))
END DO
i1(((((((h12-1)*N+p4-1)*N+p5-1)*N+h1-1)*N+h2-1)*N+h3))=i1(((((((h12-1)*N+p4-1)*N+p5-1)*N+h1-1)*N+h2-1)*N+h3))+(-0.16666666666666666&
&d0)*TMP
END DO
END DO
END DO
END DO
END DO
END DO
DO h9=1,nocc
DO h12=1,nocc
DO h1=1,nocc
DO p11=nocc+1,N
i2(((((h9-1)*N+h12-1)*N+h1-1)*N+p11))=0.0d0
END DO
END DO
END DO
END DO
DO h9=1,nocc
DO h12=1,nocc
DO h1=1,nocc
DO p11=nocc+1,N
TMP=0.0d0
TMP=TMP+v2(((((h9-1)*N+h12-1)*N+h1-1)*N+p11))
i2(((((h9-1)*N+h12-1)*N+h1-1)*N+p11))=i2(((((h9-1)*N+h12-1)*N+h1-1)*N+p11))+(1.0d0)*TMP
END DO
END DO
END DO
END DO
DO h9=1,nocc
DO h12=1,nocc
DO h1=1,nocc
DO p11=nocc+1,N
TMP=0.0d0
DO p7=nocc+1,N
TMP=TMP+t1(((p7-1)*N+h1))*v2(((((h9-1)*N+h12-1)*N+p7-1)*N+p11))
END DO
i2(((((h9-1)*N+h12-1)*N+h1-1)*N+p11))=i2(((((h9-1)*N+h12-1)*N+h1-1)*N+p11))+(1.0d0)*TMP
END DO
END DO
END DO
END DO
DO h12=1,nocc
DO p4=nocc+1,N
DO p5=nocc+1,N
DO h1=1,nocc
DO h2=1,nocc
DO h3=1,nocc
TMP=0.0d0
DO h9=1,nocc
DO p11=nocc+1,N
TMP=TMP+t3(((((((p4-1)*N+p5-1)*N+p11-1)*N+h1-1)*N+h2-1)*N+h9))*i2(((((h9-1)*N+h12-1)*N+h3-1)*N+p11))
END DO
END DO
i1(((((((h12-1)*N+p4-1)*N+p5-1)*N+h1-1)*N+h2-1)*N+h3))=i1(((((((h12-1)*N+p4-1)*N+p5-1)*N+h1-1)*N+h2-1)*N+h3))+(0.16666666666666666d&
&0)*TMP
i1(((((((h12-1)*N+p4-1)*N+p5-1)*N+h3-1)*N+h1-1)*N+h2))=i1(((((((h12-1)*N+p4-1)*N+p5-1)*N+h3-1)*N+h1-1)*N+h2))+(0.16666666666666666d&
&0)*TMP
i1(((((((h12-1)*N+p4-1)*N+p5-1)*N+h1-1)*N+h3-1)*N+h2))=i1(((((((h12-1)*N+p4-1)*N+p5-1)*N+h1-1)*N+h3-1)*N+h2))+(-0.16666666666666666&
&d0)*TMP
END DO
END DO
END DO
END DO
END DO
END DO
DO h12=1,nocc
DO p4=nocc+1,N
DO p5=nocc+1,N
DO h1=1,nocc
DO h2=1,nocc
DO h3=1,nocc
TMP=0.0d0
DO p7=nocc+1,N
DO p8=nocc+1,N
TMP=TMP+t3(((((((p4-1)*N+p7-1)*N+p8-1)*N+h1-1)*N+h2-1)*N+h3))*v2(((((h12-1)*N+p5-1)*N+p7-1)*N+p8))
END DO
END DO
i1(((((((h12-1)*N+p4-1)*N+p5-1)*N+h1-1)*N+h2-1)*N+h3))=i1(((((((h12-1)*N+p4-1)*N+p5-1)*N+h1-1)*N+h2-1)*N+h3))+(0.08333333333333333d&
&0)*TMP
i1(((((((h12-1)*N+p5-1)*N+p4-1)*N+h1-1)*N+h2-1)*N+h3))=i1(((((((h12-1)*N+p5-1)*N+p4-1)*N+h1-1)*N+h2-1)*N+h3))+(-0.08333333333333333&
&d0)*TMP
END DO
END DO
END DO
END DO
END DO
END DO
DO h8=1,nocc
DO h12=1,nocc
DO p4=nocc+1,N
DO h1=1,nocc
DO h2=1,nocc
DO h3=1,nocc
i2(((((((h8-1)*N+h12-1)*N+p4-1)*N+h1-1)*N+h2-1)*N+h3))=0.0d0
END DO
END DO
END DO
END DO
END DO
END DO
DO h8=1,nocc
DO h12=1,nocc
DO p4=nocc+1,N
DO h1=1,nocc
DO h2=1,nocc
DO h3=1,nocc
TMP=0.0d0
DO p9=nocc+1,N
DO p10=nocc+1,N
TMP=TMP+t3(((((((p4-1)*N+p9-1)*N+p10-1)*N+h1-1)*N+h2-1)*N+h3))*v2(((((h8-1)*N+h12-1)*N+p9-1)*N+p10))
END DO
END DO
i2(((((((h8-1)*N+h12-1)*N+p4-1)*N+h1-1)*N+h2-1)*N+h3))=i2(((((((h8-1)*N+h12-1)*N+p4-1)*N+h1-1)*N+h2-1)*N+h3))+(-1.0d0)*TMP
END DO
END DO
END DO
END DO
END DO
END DO
DO h12=1,nocc
DO p4=nocc+1,N
DO p5=nocc+1,N
DO h1=1,nocc
DO h2=1,nocc
DO h3=1,nocc
TMP=0.0d0
DO h8=1,nocc
TMP=TMP+t1(((p4-1)*N+h8))*i2(((((((h8-1)*N+h12-1)*N+p5-1)*N+h1-1)*N+h2-1)*N+h3))
END DO
i1(((((((h12-1)*N+p4-1)*N+p5-1)*N+h1-1)*N+h2-1)*N+h3))=i1(((((((h12-1)*N+p4-1)*N+p5-1)*N+h1-1)*N+h2-1)*N+h3))+(0.041666666666666664&
&d0)*TMP
i1(((((((h12-1)*N+p5-1)*N+p4-1)*N+h1-1)*N+h2-1)*N+h3))=i1(((((((h12-1)*N+p5-1)*N+p4-1)*N+h1-1)*N+h2-1)*N+h3))+(-0.04166666666666666&
&4d0)*TMP
END DO
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p5=nocc+1,N
DO p6=nocc+1,N
DO h1=1,nocc
DO h2=1,nocc
DO h3=1,nocc
TMP=0.0d0
DO h12=1,nocc
TMP=TMP+t1(((p4-1)*N+h12))*i1(((((((h12-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h3))
END DO
i0(((((((p4-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h3))=i0(((((((p4-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h3))+(6.0d0)*TMP
i0(((((((p5-1)*N+p4-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h3))=i0(((((((p5-1)*N+p4-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h3))+(-6.0d0)*TMP
i0(((((((p5-1)*N+p6-1)*N+p4-1)*N+h1-1)*N+h2-1)*N+h3))=i0(((((((p5-1)*N+p6-1)*N+p4-1)*N+h1-1)*N+h2-1)*N+h3))+(6.0d0)*TMP
END DO
END DO
END DO
END DO
END DO
END DO
DO h9=1,nocc
DO h10=1,nocc
DO p4=nocc+1,N
DO h1=1,nocc
DO h2=1,nocc
DO h3=1,nocc
i1(((((((h9-1)*N+h10-1)*N+p4-1)*N+h1-1)*N+h2-1)*N+h3))=0.0d0
END DO
END DO
END DO
END DO
END DO
END DO
DO h9=1,nocc
DO h10=1,nocc
DO p4=nocc+1,N
DO h1=1,nocc
DO h2=1,nocc
DO h3=1,nocc
TMP=0.0d0
DO p7=nocc+1,N
DO p8=nocc+1,N
TMP=TMP+t3(((((((p4-1)*N+p7-1)*N+p8-1)*N+h1-1)*N+h2-1)*N+h3))*v2(((((h9-1)*N+h10-1)*N+p7-1)*N+p8))
END DO
END DO
i1(((((((h9-1)*N+h10-1)*N+p4-1)*N+h1-1)*N+h2-1)*N+h3))=i1(((((((h9-1)*N+h10-1)*N+p4-1)*N+h1-1)*N+h2-1)*N+h3))+(1.0d0)*TMP
END DO
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p5=nocc+1,N
DO p6=nocc+1,N
DO h1=1,nocc
DO h2=1,nocc
DO h3=1,nocc
TMP=0.0d0
DO h9=1,nocc
DO h10=1,nocc
TMP=TMP+t2(((((p4-1)*N+p5-1)*N+h9-1)*N+h10))*i1(((((((h9-1)*N+h10-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h3))
END DO
END DO
i0(((((((p4-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h3))=i0(((((((p4-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h3))+(0.25d0)*TMP
i0(((((((p6-1)*N+p4-1)*N+p5-1)*N+h1-1)*N+h2-1)*N+h3))=i0(((((((p6-1)*N+p4-1)*N+p5-1)*N+h1-1)*N+h2-1)*N+h3))+(0.25d0)*TMP
i0(((((((p4-1)*N+p6-1)*N+p5-1)*N+h1-1)*N+h2-1)*N+h3))=i0(((((((p4-1)*N+p6-1)*N+p5-1)*N+h1-1)*N+h2-1)*N+h3))+(-0.25d0)*TMP
END DO
END DO
END DO
END DO
END DO
END DO
DO p4=nocc+1,N
DO p5=nocc+1,N
DO p6=nocc+1,N
DO h1=1,nocc
DO h2=1,nocc
DO h3=1,nocc
TMP = 0.0d0
TMP = TMP + i0(((((((p6-1)*N+p5-1)*N+p4-1)*N+h3-1)*N+h2-1)*N+h1))
TMP = TMP - i0(((((((p6-1)*N+p5-1)*N+p4-1)*N+h2-1)*N+h3-1)*N+h1))
TMP = TMP + i0(((((((p6-1)*N+p5-1)*N+p4-1)*N+h2-1)*N+h1-1)*N+h3))
TMP = TMP - i0(((((((p6-1)*N+p5-1)*N+p4-1)*N+h3-1)*N+h1-1)*N+h2))
TMP = TMP + i0(((((((p6-1)*N+p5-1)*N+p4-1)*N+h1-1)*N+h3-1)*N+h2))
TMP = TMP - i0(((((((p6-1)*N+p5-1)*N+p4-1)*N+h1-1)*N+h2-1)*N+h3))
TMP = TMP - i0(((((((p5-1)*N+p6-1)*N+p4-1)*N+h3-1)*N+h2-1)*N+h1))
TMP = TMP + i0(((((((p5-1)*N+p6-1)*N+p4-1)*N+h2-1)*N+h3-1)*N+h1))
TMP = TMP - i0(((((((p5-1)*N+p6-1)*N+p4-1)*N+h2-1)*N+h1-1)*N+h3))
TMP = TMP + i0(((((((p5-1)*N+p6-1)*N+p4-1)*N+h3-1)*N+h1-1)*N+h2))
TMP = TMP - i0(((((((p5-1)*N+p6-1)*N+p4-1)*N+h1-1)*N+h3-1)*N+h2))
TMP = TMP + i0(((((((p5-1)*N+p6-1)*N+p4-1)*N+h1-1)*N+h2-1)*N+h3))
TMP = TMP + i0(((((((p5-1)*N+p4-1)*N+p6-1)*N+h3-1)*N+h2-1)*N+h1))
TMP = TMP - i0(((((((p5-1)*N+p4-1)*N+p6-1)*N+h2-1)*N+h3-1)*N+h1))
TMP = TMP + i0(((((((p5-1)*N+p4-1)*N+p6-1)*N+h2-1)*N+h1-1)*N+h3))
TMP = TMP - i0(((((((p5-1)*N+p4-1)*N+p6-1)*N+h3-1)*N+h1-1)*N+h2))
TMP = TMP + i0(((((((p5-1)*N+p4-1)*N+p6-1)*N+h1-1)*N+h3-1)*N+h2))
TMP = TMP - i0(((((((p5-1)*N+p4-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h3))
TMP = TMP - i0(((((((p6-1)*N+p4-1)*N+p5-1)*N+h3-1)*N+h2-1)*N+h1))
TMP = TMP + i0(((((((p6-1)*N+p4-1)*N+p5-1)*N+h2-1)*N+h3-1)*N+h1))
TMP = TMP - i0(((((((p6-1)*N+p4-1)*N+p5-1)*N+h2-1)*N+h1-1)*N+h3))
TMP = TMP + i0(((((((p6-1)*N+p4-1)*N+p5-1)*N+h3-1)*N+h1-1)*N+h2))
TMP = TMP - i0(((((((p6-1)*N+p4-1)*N+p5-1)*N+h1-1)*N+h3-1)*N+h2))
TMP = TMP + i0(((((((p6-1)*N+p4-1)*N+p5-1)*N+h1-1)*N+h2-1)*N+h3))
TMP = TMP + i0(((((((p4-1)*N+p6-1)*N+p5-1)*N+h3-1)*N+h2-1)*N+h1))
TMP = TMP - i0(((((((p4-1)*N+p6-1)*N+p5-1)*N+h2-1)*N+h3-1)*N+h1))
TMP = TMP + i0(((((((p4-1)*N+p6-1)*N+p5-1)*N+h2-1)*N+h1-1)*N+h3))
TMP = TMP - i0(((((((p4-1)*N+p6-1)*N+p5-1)*N+h3-1)*N+h1-1)*N+h2))
TMP = TMP + i0(((((((p4-1)*N+p6-1)*N+p5-1)*N+h1-1)*N+h3-1)*N+h2))
TMP = TMP - i0(((((((p4-1)*N+p6-1)*N+p5-1)*N+h1-1)*N+h2-1)*N+h3))
TMP = TMP - i0(((((((p4-1)*N+p5-1)*N+p6-1)*N+h3-1)*N+h2-1)*N+h1))
TMP = TMP + i0(((((((p4-1)*N+p5-1)*N+p6-1)*N+h2-1)*N+h3-1)*N+h1))
TMP = TMP - i0(((((((p4-1)*N+p5-1)*N+p6-1)*N+h2-1)*N+h1-1)*N+h3))
TMP = TMP + i0(((((((p4-1)*N+p5-1)*N+p6-1)*N+h3-1)*N+h1-1)*N+h2))
TMP = TMP - i0(((((((p4-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h3-1)*N+h2))
TMP = TMP + i0(((((((p4-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h3))
i0(((((((p4-1)*N+p5-1)*N+p6-1)*N+h1-1)*N+h2-1)*N+h3)) = TMP / 36.0d0
END DO
END DO
END DO
END DO
END DO
END DO
RETURN
END SUBROUTINE
