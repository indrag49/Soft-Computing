program Maxnet
parameter(n = 4)
integer :: i, j, k, count
real :: a(n), epsilon, s, b(n)

open(1, file = "Maxnet_inputs.html", status = "old")
open(2, file = "Maxnet_inhibitory_weight.html", status = "old")
open(3, file = "Maxnet_outputs.html", status = "unknown")

do i = 1, n
read(1, *) a(i)
enddo

read(2, *) epsilon

4 count = 0
do j = 1, n
s = 0
do k = 1, n
if (k .ne. j) then
s = s + a(k)
endif
enddo
b(j) = f(a(j) - epsilon * s)
enddo
do i = 1, n
a(i) = b(i)
enddo
write(3, *) (a(i), i = 1, n)
write(3, *) "------------------------------------"
do i = 1, n
if (a(i) > 0) then
count = count + 1
endif
enddo
if (count > 1) then
goto 4
endif



end program Maxnet

function f(x)
real :: x
if (x > 0) then 
f = x
else
f = 0
endif
end function f