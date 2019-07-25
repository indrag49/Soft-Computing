program CPN
parameter(kx = 4, ky = 2)
integer :: i, j, k, x(kx), y(ky), winning_ind, epoch = 1, count
real :: V(kx, ky), W(ky, ky), V1(kx, ky), alpha, D(ky), s, a

open(1, file = "Forwardonly_CPN_inputs.html", status = "old")
open(2, file = "Forwardonly_CPN_clusters.html", status = "old")
open(3, file = "Forwardonly_CPN_learning_rate.html", status = "old")
open(4, file = "Forwardonly_CPN_weights1.html", status = "old")
open(5, file = "Forwardonly_CPN_weights2.html", status = "old")
open(6, file = "Forwardonly_CPN_outputs1.html", status = "unknown")
open(7, file = "Forwardonly_CPN_outputs2.html", status = "unknown")

do i = 1, kx
read(1, *) x(i)
read(4, *) (V(i, j), j = 1, ky)
enddo

do i = 1, kx
do j = 1, ky
V1(i, j) = V(i, j)
enddo
enddo

do i = 1, ky
read(2, *) y(i)
read(5, *) (W(i, j), j = 1, ky)
enddo

read(3, *) alpha
a = alpha

! Phase 1 of training
8 count = 0 
write(6, *) "Epoch = ", epoch
do j = 1, ky
s = 0.
do i = 1, kx
s = s + (x(i) - V(i, j))**2
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
do i = 1, kx
V(i, winning_ind) = V(i, winning_ind) + alpha * (x(i) - V(i, winning_ind))
enddo
do j = 1, kx
write(6, *) (V(j, k), k = 1, ky)
enddo
write(6, *) " "
epoch = epoch + 1
alpha = 0.5 * alpha
do i = 1, kx
do j = 1, ky
if (V(i, j) == V1(i, j)) then
count = count + 1
endif
enddo
enddo
if (count < kx * ky) then 
do i = 1, kx
do j = 1, ky
V1(i, j) = V(i, j)
enddo
enddo
goto 8
endif

! Phase 2 of training
epoch = 1
9 count = 0 
write(6, *) "Epoch = ", epoch
write(7, *) "Epoch = ", epoch
do j = 1, ky
s = 0.
do i = 1, kx
s = s + (x(i) - V(i, j))**2
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
do i = 1, kx
V(i, winning_ind) = V(i, winning_ind) + a * (x(i) - V(i, winning_ind))
enddo
do i = 1, ky
W(i, winning_ind) = W(i, winning_ind) + a * (y(i) - W(i, winning_ind))
enddo
do j = 1, kx
write(6, *) (V(j, k), k = 1, ky)
enddo
do j = 1, ky
write(7, *) (W(j, k), k = 1, ky)
enddo
write(6, *) " "
write(7, *) " "
epoch = epoch + 1
a = 0.5 * a
do i = 1, kx
do j = 1, ky
if (V(i, j) == V1(i, j)) then
count = count + 1
endif
enddo
enddo
if (count < kx * ky) then 
do i = 1, kx
do j = 1, ky
V1(i, j) = V(i, j)
enddo
enddo
goto 9
endif

end program CPN
 