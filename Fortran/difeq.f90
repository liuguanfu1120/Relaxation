	SUBROUTINE difeq(k,k1,k2,jsf,is1,isf,indexv,s,y)
	USE nrtype
	USE sfroid_data
	IMPLICIT NONE
	INTEGER(I4B), INTENT(IN) :: is1,isf,jsf,k,k1,k2
	INTEGER(I4B), DIMENSION(:), INTENT(IN) :: indexv
	REAL(SP), DIMENSION(:,:), INTENT(OUT) :: s
	REAL(SP), DIMENSION(:,:), INTENT(IN) :: y
	INTEGER(I4B), DIMENSION(3) :: indexv3
	indexv3(1:3)=3+indexv(1:3)
	if (k == k1) then
		s(3,indexv3(1:3))= (/ 1.0_sp, 0.0_sp, 0.0_sp /)
		s(3,jsf)=y(1,1)
	else if (k > k2) then
		s(1,indexv3(1:3))= (/ 0.0_sp,1.0_sp,0.0_sp /)
		s(1,jsf)=y(2,M)-5
		s(2,indexv3(1:3))=(/ 1.0_sp, 0.0_sp, 0.0_sp /)
		s(2,jsf)=y(1,M)
	else
		s(1,indexv(1:3))=(/ -1.0_sp, -0.5_sp*h, 0.0_sp /)
		s(1,indexv3(1:3))=(/ 1.0_sp, -0.5_sp*h, 0.0_sp /)
		s(2,indexv(1:3))=(/0.25_sp*h*(y(3,k)+y(3,k-1)),-1.0_sp,  0.25_sp*h*(y(1,k)+y(1,k-1))/)
        s(2,indexv3(1:3))=s(2,indexv(1:3))
		s(2,indexv3(2))=s(2,indexv3(2))+2.0_sp
		s(3,indexv(1:3))=(/ 0.0_sp, 0.0_sp, -1.0_sp /)
		s(3,indexv3(1:3))=(/ 0.0_sp, 0.0_sp, 1.0_sp /)
		s(1,jsf)=y(1,k)-y(1,k-1)-0.5_sp*h*(y(2,k)+y(2,k-1))
		s(2,jsf)=y(2,k)-y(2,k-1)+0.5_sp*h*(y(3,k)+y(3,k-1))&
                *0.5_sp*(y(1,k)+y(1,k-1))
		s(3,jsf)=y(3,k)-y(3,k-1)
	end if
	END SUBROUTINE difeq
