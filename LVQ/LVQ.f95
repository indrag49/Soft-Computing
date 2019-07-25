program LVQ
parameter(n = 3, kx = 4, ky = 2)
integer :: i, j, k, x(n, kx), winning_ind, epoch = 1
real :: w(kx, ky), alpha, s, D(ky), c(n)

open(1, file = "LVQ_inputs.html", status = "old")
open(2, file = "LVQ_learning_rate.html", status ="old")
open(3, file = "LVQ_weights.html", status = "old")
open(4, file = "LVQ_classes.html", status = "old")
open(5, file = "LVQ_outputs.html", status = "unknown")

do i = 1, n
read(1, *) (x(i, j), j = 1, kx)
read(4, *) c(i)
enddo

read(2, *) alpha

do i = 1, kx
read(3, *) (w(i, j), j = 1, ky)
enddo

6 write(5, *) "Epoch = ", epoch
do i = 1, n
do j = 1, ky
s = 0.
do k = 1, kx
s = s + (w(k, j) - x(i, k))**2
enddo
D(j) = s
enddo
do j = 1, ky - 1
if (D(j) < D(j + 1)) then
winning_ind = j
else
winning_ind = j + 1
endif
enddo
if (winning_ind .ne. c(i)) then
do j = 1, kx
w(j, winning_ind) = w(j, winning_ind) - alpha * (x(i, j) - w(j, winning_ind))
enddo
else
do j = 1, kx
w(j, winning_ind) = w(j, winning_ind) + alpha * (x(i, j) - w(j, winning_ind))
enddo
endif
do j = 1, kx
write(5, *) (w(j, k), k = 1, ky)
enddo
write(5, *) " "
enddo
epoch = epoch + 1
alpha = 0.5 * alpha
if (epoch .le. 5000) then
goto 6
endif

end program LVQ