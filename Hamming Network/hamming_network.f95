program Hamming_Network
parameter(n = 4, kx = 4, ky = 2)
integer :: i, j, k, x(n, kx)
real :: e(kx, ky), w(kx, ky), y(kx, ky), b(ky), s

open(1, file = "hamming_network_exemplar_vectors.html", status = "old")
open(2, file = "hamming_network_inputs.html", status = "old")
open(3, file = "hamming_network_outputs.html", status = "unknown")

do i = 1, kx
read(1, *) (e(i, j), j = 1, ky)
enddo

do i = 1, n
read(2, *) (x(i, j), j = 1, kx)
enddo

do i = 1, kx
do j = 1, ky
w(i, j) = e(i, j)/2.
enddo
enddo

do j = 1, ky
b(j) = n/2.
enddo

do i = 1, n
do j = 1, ky
s = b(j)
do k = 1, kx
s = s + x(i, k) * w(k, j)
enddo
y(i, j) = s
enddo
enddo

do i = 1, kx
write(3, *) (y(i, j), j = 1, ky)
enddo

end program Hamming_Network