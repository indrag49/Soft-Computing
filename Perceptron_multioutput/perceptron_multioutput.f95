! AND, OR and ANDNOT gate implementations
program perceptron_multioutput
parameter(n=2, kx=4, m=3)
integer:: x(kx, n), y(kx, m), w(n+1, m)
integer:: i, j, k, m1, theta, alpha, yin, yout
open(1, file="perceptron2", status="old")
open(2, file="perceptron2_y", status="old")
open(3, file="perceptron2_out", status="unknown")

do i=1, kx
read(1, *)(x(i,j), j=1, n)
enddo

do i=1, kx
read(2, *)(y(i,j), j=1, m)
enddo

do i=1,n+1
do j=1,m
w(i,j)=0
enddo
enddo

alpha=1
theta=0

do m1=1, m
4 k=1
check=0
5 yin=w(n+1, m1)
do i=1,n
yin=yin+x(k,i)*w(i,m1)
enddo

yout=f(yin, theta)
if(yout .ne. y(k, m1)) then
do i=1, n
w(i,m1)=w(i,m1)+alpha*y(k,m1)*x(k,i)
enddo
w(n+1, m1)=w(n+1, m1)+alpha*y(k,m1)
else
check=check+1
endif
write(3, *)(x(k, j), j=1,n), y(n,m1), yin, yout
k=k+1
if (k<=kx) goto 5
write(3, *) " "
if (check<kx) goto 4
write(3, *) "-------------"
enddo
end program perceptron_multioutput

function f(x1, theta)
integer:: x1, res, theta
if(x1>theta) then
res=1
else if(x1 .ge. -theta .and. x1 .le. theta) then
res=0
else 
res=-1
end if
f=res
end function f