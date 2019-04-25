program Hebb_Network
parameter(n=9, kx=2)
integer::i, j,k, x(kx, n), y(kx), w(n+1)
open(1, file="Hebb1", status="old")
open(2, file="Hebb_y", status="old")
open(3, file="Hebb_output", status="unknown")

do i=1,kx
read(1,*)(x(i,j), j=1, n)
enddo

do j=1,kx
read(2,*)y(j)
enddo

do i=1, n+1
w(i)=0
enddo

k=1
4 do i=1, n
w(i)=w(i)+x(k,i)*y(k)
enddo
w(10)=w(10)+y(k)
k=k+1
if(k<=2) goto 4

do i=1, 10
write(3, *)w(i)
enddo
  
end program Hebb_Network