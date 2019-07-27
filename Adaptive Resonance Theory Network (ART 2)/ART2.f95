program ART2
parameter(kx = 2, ky = 3)
integer :: i, j, k, epoch = 1, reset = 1, winning_ind
real :: a = 10, b = 10, c = 0.1, d = 0.9, e = 0, rho = 0.9, theta, b1(ky), t(ky), alpha = 0.6
real :: x(kx, ky), u(ky), w(ky), p(ky), x1(ky), q(ky), v(ky), y(ky), r(ky)

open(1, file = "ART2_inputs.html", status = "old")
open(2, file = "ART2_outputs1.html", status = "unknown")
open(3, file = "ART3_outputs2.html", status = "unknown")

theta = 1 / sqrt(3.)
do i = 1, kx
read(1, *) (x(i, j), j = 1, ky)
enddo

do j = 1, ky
b1(j) = 1 / ((1 - d) * sqrt(1. * ky))
t(j) = 0.
enddo

do j = 1, ky
u(j) = 0
w(j) = x(1, j)
p(j) = 0
q(j) = 0
enddo
do j = 1, ky
x1(j) = x(1, j) / (e + sum(w))
v(j) = f(x1(j), theta) + b * f(q(j), theta)
enddo

write(2, *) "u = "
write(2, *) (u(j), j = 1, ky)
write(2, *) "w = "
write(2, *) (w(j), j = 1, ky)
write(2, *) "p = "
write(2, *) (p(j), j = 1, ky)
write(2, *) "q = "
write(2, *) (q(j), j = 1, ky)
write(2, *) "x = "
write(2, *) (x1(j), j = 1, ky)
write(2, *) "v = "
write(2, *) (v(j), j = 1, ky)


5 write(3, *) "Epoch =", epoch 
do i = 1, kx
!Update F1 unit activations again
do j = 1, ky
u(j) = v(j) / (e + sum(v))
w(j) = x(i, j) + a * u(j)
p(j) = u(j)
enddo
do j = 1, ky
x1(j) = w(j) / (e + sum(w))
q(j) = p(j) / (e + sum(p))
v(j) = f(x1(j), theta) + b * f(q(j), theta)
enddo
!Calculate signals to F2 units
do j = 1, ky
y(j) = b1(j) * p(j)
enddo
winning_ind = maxloc(y)
4 reset = 1
do j = 1, ky
u(j) = v(j) / (e + sum(v))
p(j) = u(j) + d * t(j)
enddo
do j = 1, ky
r(j) = (u(j) + c * p(j)) / (e + sum(u) + c * sum(p))
enddo
if(sum(r) < (rho - e)) then
y(winning_ind) = -1
goto 4
endif
do j = 1, ky
w(j) = s(j) + a * u(j)
q(j) = p(j) / (e + sum(p))
enddo
do j = 1, ky
x1(j) = w(j) / (e + sum(w))
v(j) = f(x1(j), theta) + b * f(q(j), theta)
enddo
t(winning_ind) = alpha * d * u(winning_ind) + ((1 + alpha * d * (d - 1)) * t(winning_ind))
b1(winning_ind) = alpha * d * u(winning_ind) + ((1 + alpha * d * (d - 1)) * b1(winning_ind))
write(2, *) (t(j), j = 1, ky)
write(3, *) (b1(j), j = 1, ky)
write(2, *) " "
write(3, *) " "
do j = 1, ky
u(j) = v(j) / (e + sum(v))
w(j) = x(i, j) + a * u(j)
p(j) = u(j) + d * t(j)
enddo
do j = 1, ky
x1(j) = w(j) / (e + sum(w))
q(j) = p(j) / (e + sum(p))
v(j) = f(x1(j), theta) + b * f(q(j), theta)
enddo 
enddo
epoch = epoch + 1
if (epoch < 10) then
goto 5
endif


end program ART2

function f(m, theta)
real :: m, theta
if (m .ge. theta) then
f = m
else
f = 0
endif
end function f