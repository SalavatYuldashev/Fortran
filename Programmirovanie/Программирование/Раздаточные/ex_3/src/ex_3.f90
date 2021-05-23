program exercise_3
   implicit none
   integer, parameter      :: R_ = 4
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt", E_ = "UTF-8"
   integer                 :: In = 0, Out = 0, N = 0, i = 0
   real(R_)                :: P = 1
   real(R_), allocatable   :: B(:) 

   open (file=input_file, encoding=E_, newunit=In)
      read (In, *) N
      allocate (B(N))

      ! В императивонм стиле:      
      do i = 1, N
         read (In, *) B(i)
      end do

      ! В регулярном стиле:
      ! read (In, *) B
   close (In)

   ! В императивном стиле:
   do i = 1, N
      P = P * B(i)
   end do

   ! В регулярном стиле:
   ! P = Product(B)

   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, "(i0)") Len
      write (Out, "(f0.2)") B
      write (Out, *)
      write (Out, "('Product = ', f0.2)") P
   close (Out)
end program exercise_3
