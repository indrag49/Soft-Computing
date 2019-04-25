program perceptron_singleoutput
parameter(n=4, kx=4)
integer:: i, j, k, theta, check, yin, yout
real:: alpha
integer::x(kx, n), y(kx), w(n+1)
open(1, file="perceptron1", status="old")
open(2, file="perceptron1_y", status="old")
open(3, file="perceptron1_output", status="unknown")
  
do i=1, kx
read(1, *)(x(i, j), j=1, n)
enddo

do i=1, kx
read(2, *) y(i)
enddo

do i=1, n+1
w(i)=0
enddo

alpha=1 !the learning rate
theta=0.2 !the threshold

4 k=1
check=0
5 yin=w(n+1)
do i=1,n
yin=yin+x(k,i)*w(i)
enddo
yout=f(yin, theta)
if(yout .ne. y(k))then
do i=1,n
w(i)=w(i)+alpha*y(k)*x(k,i)
enddo
w(n+1)=w(n+1)+alpha*y(k)
else
check=check+1
endif
write(3, *)(x(k, j), j=1,n), y(n), yin, yout
k=k+1
if(k<=kx) goto 5
write(3, *) " "
if (check<kx) goto 4
end program perceptron_singleoutput

function f(x, theta)
integer:: x, theta, res
if(x>theta) then
res=1
else if(x .ge. -theta .and. x .le. theta) then
res=0
else 
res=-1
end if
f=res
end function f
  