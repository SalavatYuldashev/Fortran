real(R_) A(N, N)

forall(integer :: i = 1:N) &
      A(i, i) = 0

! Непрофессионально.
forall(integer :: i = 1:N, j = 1:N, i == j) &
      A(i, j) = 0

! Непрофессионально.
do i = 1:N
   do j = 1:N
      if (i == j) &
         A(i, j) = 0
   end do
end do

