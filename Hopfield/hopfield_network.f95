program Hopfield_Network
parameter(n = 3, kx = 5)
integer :: i, j, k, l, x(n, kx), x_test(n, kx), w(kx, kx), wT(kx, kx), E(n), s(kx), t(kx), s1(kx), sm, unit(3), y(kx), wj(kx)

open(1, file = "hopfield_inputs.html", status = "old")
open(2, file = "hopfield_tests.html", status = "old")
open(3, file = "weights.html", status = "unknown")
open(4, file = "energy.html", status = "unknown")
open(5, file = "hopfield_units.html", status = "old")
open(6, file = "energy_2.html", status = "unknown")

do i = 1, n
read(1, *) (x(i, j), j = 1, kx)
read(2, *) (x_test(i, j), j = 1, kx)
enddo

do i = 1, kx
do j = 1, kx
w(i, j) = 0
enddo
enddo

do i = 1, n
do j = 1, kx
s(j) = x(i, j)
t(j) = x(i, j)
enddo
do k = 1, kx
do l = 1, kx
if (k == l) then
w(k, l) = 0
else
w(k, l) = w(k, l) + s(k) * t(l)
endif
enddo
enddo
enddo

do i = 1, kx
do j = 1, kx
wT(i, j) = w(j, i)
enddo
enddo

do i = 1, kx
write(3, *) (wT(i, j), j = 1, kx)
enddo

do i = 1, n
E(i) = 0
do j = 1, kx
s(j) = x(i, j)
enddo
do k = 1, kx
sm = 0
do l = 1, kx
sm = sm + wT(k, l) * s(l)
enddo
s1(k) = sm
enddo
do k = 1, kx
E(i) = E(i) + s(k) * s1(k)
enddo
E(i) = -0.5*E(i)
enddo

do i = 1, n
write(4, *) E(i)
enddo

! Now applying test patterns
do i = 1, n
read(5, *) unit(i)
enddo

do i = 1, n
E(i) = 0
do j = 1, kx
s(j) = x_test(i, j)
wj(j) = wT(j, unit(i))
enddo
sm = x_test(i, unit(i))
do k = 1, kx
sm = sm + s(k)*wj(k)
enddo
s(unit(i)) = f(s(unit(i)), sm)
!write(6, *) s(unit(i))
do k = 1, kx
sm = 0
do l = 1, kx
sm = sm + wT(k, l) * s(l)
enddo
s1(k) = sm
enddo
do k = 1, kx
E(i) = E(i) + s(k) * s1(k)
enddo
E(i) = -0.5*E(i)
enddo

do i = 1, n
write(6, *) E(i)
enddo


end program Hopfield_Network

function f(z, x)
integer :: z, x
if (x > 0) then
f = 1
else if (x == 0) then
f = z
else
f = -1
endif
end function f