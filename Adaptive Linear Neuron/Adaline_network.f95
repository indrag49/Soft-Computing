program Adaline
parameter(n=2, kx=4)
integer::i, j, k, x(kx, n), y(kx)
real::alpha, E, error, l, w(n+1), yin
open(1, file="Adaline", status="old")
open(2, file="Adaline_y", status="old")
open(3, file="Adaline_output", status="unknown")

do i=1, kx
read(1, *) (x(i,j), j=1, n)
enddo

alpha=0.1
error=1.4

do i=1, kx
read(2, *) y(i)
enddo

do i=1, n+1
w(i)=0.1
enddo

4 k=1
E=0.
5 yin=w(n+1)
do i=1, n
yin=yin+x(k,i)*w(i)
enddo
l=y(k)-yin
y=f(yin)
do i=1, n
w(i)=w(i)+alpha*l*x(k,i)
enddo
w(n+1)=w(n+1)+alpha*l
E=E+l**2
write(3, *) (x(k,i), i=1,n), yin, y, l, (w(i), i=1, n+1), l**2
k=k+1
if (k<=kx) goto 5
write(3, *) "Total error for epoch", E
write(3, *) "------------------------"
if (E>error) goto 4
end program Adaline

function f(x)
real::x
if(x>=0)then
f=1
else
f=-1
endif
end function 