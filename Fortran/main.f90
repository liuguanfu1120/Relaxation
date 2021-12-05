MODULE sfroid_data
	USE nrtype
	INTEGER(I4B), PARAMETER :: M=4000
	REAL(SP) :: anorm,h
	REAL(SP), DIMENSION(M) :: x
END MODULE sfroid_data

	PROGRAM sfroid
	USE nrtype; USE nrutil, ONLY : arth
	USE nr, ONLY : solvde   !,plgndr,
	USE sfroid_data
	IMPLICIT NONE
	INTEGER(I4B), PARAMETER :: NE=3,NB=1
	INTEGER(I4B) :: itmax
	INTEGER(I4B), DIMENSION(NE) :: indexv
	REAL(SP) :: conv,slowc
	REAL(SP), DIMENSION(NE) :: scalv
	REAL(SP), DIMENSION(NE,M) :: y
    INTEGER(I4B) :: iloop=1
    REAL :: start,finish
    call cpu_time(start)
        itmax=1000
	conv=5.0e-5_sp
	slowc=1.0
	h=2.0_sp/(M-1)
	indexv(1:3)=(/1,2,3/)
	x(1:M)=arth(0,1,M)*h-1.0_sp
    y(1,1:M) = 1.0_sp
    y(2,1:M) = 2.0_sp
    y(3,1:M)= 4.0_sp
    scalv(1:3)=(/2.0_sp,4.0_sp,6.0_sp/)
    open(unit=10,file='y0.txt',status='unknown')
    do iloop=1,M
    write(unit=10,fmt=*)y(1,iloop),y(2,iloop),y(3,iloop)
    end do
	call solvde(itmax,conv,slowc,scalv,indexv,NB,y)
    write(unit=6,fmt=*)"The k square is "
    write (unit=6,fmt=*)y(3,1)
    open(unit=10,file='y.txt',status='unknown')
    do iloop=1,M
    write(unit=10,fmt=*)y(1,iloop),y(2,iloop),y(3,iloop)
    end do
    close(10)
    call cpu_time(finish)
    print'("Time = ",f20.8,"seconds.")',finish-start
END PROGRAM sfroid
