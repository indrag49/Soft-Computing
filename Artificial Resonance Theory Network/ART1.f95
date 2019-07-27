program ART1
parameter(kx = 4, ky = 3)
integer :: i, j, k, x(kx, kx), x1(kx), x1_s, epoch = 1, s, winning_ind, reset, t(ky, kx)
real :: b(kx, ky), b1(kx, ky), alpha, rho, y, D(ky)

open(1, file = "ART1_inputs.html", status = "old")
open(2, file = "ART1_vigilance_parameter.html", status = "old")
open(3, file = "ART1_learning_rate.html", status = "old")
open(4, file = "ART1_outputs1.html", status = "unknown")
open(5, file = "ART1_outputs2.html", status = "unknown")

do i = 1, kx
read(1, *) (x(i, j), j = 1, kx)
enddo

read(2, *) rho
read(3, *) alpha

do i = 1, kx
do j = 1, ky
b(i, j) = 1./(1 + kx)
enddo
enddo

do i = 1, ky
do j = 1, kx
t(i, j) = 1
enddo
enddo

7 write(4, *) "Epoch =", epoch
write(5, *) "Epoch =", epoch  
do i = 1, kx
s = 0
do j = 1, kx
s = s + x(i, j)
enddo
6 reset = 1
do j = 1, ky
D(j) = 0
do k = 1, kx
D(j) = D(j) + b(k, j)*x(i, k)
enddo
enddo  
winning_ind = maxloc(D)
write(4, *) "J =", winning_ind
!if (D(winning_ind) .ne. -1) then
x1_s = 0
do j = 1, kx
x1(j) = x(i, j) * t(winning_ind, j)
x1_s = x1_s + x1(j)
enddo
if (x1_s*1./s < rho) then
D(winning_ind) = -1
goto 6
endif
do j = 1, kx
b(j, winning_ind) = alpha * x1(j)/(alpha - 1 + x1_s)
enddo
do j = 1, kx
t(winning_ind, j) = x1(j)
enddo
do j = 1, kx
write(4, *) (b(j, k), k = 1, ky)
enddo
do j = 1, ky
write(5, *) (t(j, k), k = 1, kx)
enddo
write(4, *) " "
write(5, *) " "
!endif
enddo
epoch = epoch + 1
if (epoch < 10) then
goto 7
endif

end program ART1