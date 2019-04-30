program Madaline
parameter(n=2, kx=4, m=2)
integer::i, j, k, ii, m1, x(kx, n), y(kx), yout
real::z(m), w(n+1, m), zout(m), v(m+1), yin, alpha
open(1, file="Madaline_x", status="old")
open(2, file="Madaline_y", status="old")
open(3, file="Madaline_weight_matrix", status="old")
open(4, file="Madaline_v", status="old")
open(5, file="Madaline_output", status="unknown")

do i=1, kx
read(1, *)(x(i,j), j=1,n)
enddo

do i=1, kx
read(2, *)y(i)
enddo

do i=1, n+1
read(3, *)(w(i,j), j=1, m)
enddo

do i=1, m+1
read(4, *) v(i)
enddo

alpha=0.5

do ii=1, 10
k=1
6 do m1=1, m
z(m1)=w(n+1, m1)
do i=1,n
z(m1)=z(m1)+x(k,i)*w(i, m1)
enddo
zout(m1)=f(z(m1))
enddo
yin=v(m+1)
do i=1, m
yin=yin+zout(i)*v(i)
enddo
yout=f(yin)
if(yout .ne. y(k)) then
if(y(k)==1) then
do j=1, m
if(abs(z(j))<0.7)then
do i=1, n
w(i,j)=w(i,j)+alpha*(y(k)-z(j))*x(k,i)
enddo
w(n+1, j)=w(n+1, j)+alpha*(y(k)-z(j))
endif
enddo
elseif (y(k)==-1) then
do j=1, m
if(z(j)>0.0)then
do i=1, n
w(i, j)=w(i, j)+alpha*(y(k)-z(j))*x(k, i)
enddo
w(n+1, j)=w(n+1, j)+alpha*(y(k)-z(j))
endif
enddo
endif
endif
write(5, *) (z(m1), m1=1,m), (zout(m1), m1=1, m), yin, yout
k=k+1 
if (k<=kx) goto 6
write(5, *) "--------------------------------------"
enddo
end program Madaline

function f(x)
real::x
integer::r
if(x>=0.) then
r=1
elseif (x<0.) then
r=-1
endif
f=int(r)
end function f