program Mexican_Hat_Network
parameter(n = 7, kx = 2)
integer :: i, j, k, R(kx), R1, R2, t, tmax = 5
real :: s(n), C(kx), C1, C2, x(n), s1, s2, s3

open(1, file = "Mexican_hat_external_signal.html", status = "old")
open(2, file = "Mexican_hat_radii.html", status = "old")
open(3, file = "Mexican_hat_weights.html", status = "old")
open(4, file = "Mexican_hat_outputs.html", status = "unknown")

do i = 1, n
read(1, *) s(i)
enddo

do i = 1, kx
read(2, *) R(i)
read(3, *) C(i)
enddo

do t = 1, tmax
write (4, *) "t = ", t
do i = 1, n
s1 = 0.
s2 = 0.
s3 = 0.
do k = - R(1), R(1)
if (i + k > 0 .and. i + k .le. n) then
s1 = s1 + s(i + k)
endif
enddo
do k = - R(2), - R(1) - 1 
if (i + k > 0 .and. i + k .le. n) then
s2 = s2 + s(i + k)
endif
enddo
do k = R(1) + 1, R(2)
if (i + k > 0 .and. i + k .le. n) then
s3 = s3 + s(i + k)
endif
enddo
x(i) = C(1) * s1 + C(2) * s2 + C(2) * s3
enddo
do i = 1, n
s(i) = f(x(i))
write(4, *) s(i)
enddo
write(4, *) "----------------------------"
enddo

end program Mexican_Hat_Network

function f(x)
real :: x
if (x < 0) then 
f = 0.
else if (0 .le. x .and. x .le. 2) then
f = x
else 
f = 2.
endif
end function f 

