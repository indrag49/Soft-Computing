program Heteroassociative_Memory_Network
parameter(n = 4, kx = 4, ky = 2)
integer :: i, j, k, l, m, s(n, kx), t(n, ky), w(kx, ky), s1(kx), t1(ky), ac(kx), out(ky), s

open(1, file = "heteroassociative_inputs.html", status = "old")
open(2, file = "heteroassociative_targets.html", status = "old")
open(3, file = "heteroassociative_weight.html", status = "unknown")
open(4, file = "heteroassociative_activations.html", status = "old")
open(5, file = "heteroassociative_outputs.html", status = "unknown")

do i = 1, n
read(1, *) (s(i, j), j = 1, kx)
enddo

do i = 1, n
read(2, *) (t(i, j), j = 1, ky)
enddo

do i = 1, kx
do j = 1, ky
w(i, j) = 0
enddo
enddo

do i = 1, n
do j = 1, kx
s1(j) = s(i, j)
enddo
do k = 1, ky
t1(k) = t(i, k)
enddo
do l = 1, kx
do m = 1, ky
w(l, m) = w(l, m) + s1(l) * t1(m)
enddo
enddo
enddo

do i = 1, kx
write(3, *) (w(i, j), j = 1, ky)
enddo

do i = 1, kx
read(4, *) (ac(i))
enddo

do j = 1, ky
s = 0
do i = 1, kx
s = s + ac(i) * w(i, j)
enddo
out(j) = f(s)
enddo

do i = 1, ky
write(5, *) out(i)
enddo



end program Heteroassociative_Memory_Network

function f(x)
integer :: x
if (x > 0) then
f = 1
else if (x == 0) then
f = 0
else
f = -1
endif
end function f