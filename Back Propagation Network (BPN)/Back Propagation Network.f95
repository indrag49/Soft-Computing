program Back_Propagation
parameter(kx=2, ky=2)
integer::x(kx), i, j
real::w(kx+1, ky), v(ky+1), alpha, del_w(kx+1, ky), deltav(ky), delta_out(ky), del_v(ky+1), zin(ky), z(ky), del, yin, t

del=0.0
alpha=0.25
yin=0.0

open(1, file="BPN_x", status="old")
open(2, file="BPN_y", status="old")
open(3, file="BPN_w", status="old")
open(4, file="BPN_v", status="old")
open(5, file="BPN_output_w", status="unknown")
open(6, file="BPN_output_v", status="unknown")

do i=1, kx
read(1, *)x(i)
enddo

read(2, *)t

do i=1, kx+1
read(3, *) (w(i,j), j=1, ky)
enddo

do i=1, ky+1
read(4, *)v(i)
enddo

do i=1, kx+1
do j=1, ky
del_w(i,j)=0.0
enddo
enddo

do i=1, ky+1
del_v(i)=0.0
enddo

do i=1, ky
zin(i)=0.0
z(i)=0.0
deltav(i)=0.0
delta_out(i)=0.0
enddo

do i=1, ky
zin(i)=w(kx+1, i)
do j=1, kx
zin(i)=zin(i)+x(j)*w(j, i)
enddo
z(i)=f(zin(i))
enddo
yin=v(ky+1)
do i=1, ky
yin=yin+z(i)*v(i)
enddo
yout=f(yin)
del=(t-yout)*f(yin)*(1-f(yin))
del_v(ky+1)=alpha*del
do i=1, ky
del_v(i)=alpha*del*z(i)
enddo
do i=1, ky
deltav(i)=del*del_v(i)
enddo
do i=1, ky
delta_out(i)=deltav(i)*f(zin(i))*(1-f(zin(i)))
enddo
do j=1, ky
do i=1, kx
del_w(i, j)=alpha*delta_out(j)*x(i)
enddo
del_w(kx+1, j)=alpha*delta_out(j)
enddo

do i=1, kx+1
do j=1, ky
w(i, j)=w(i, j)+del_w(i, j)
enddo
enddo
do i=1, ky+1
v(i)=v(i)+del_v(i)
enddo

do i=1, kx+1
write(5, *)(w(i,j), j=1, ky)
enddo

do i=1, ky+1
write(6, *)v(i)
enddo

end program Back_Propagation

function f(x)
real::x
f=2./(1+exp(-x))-1.
end function f