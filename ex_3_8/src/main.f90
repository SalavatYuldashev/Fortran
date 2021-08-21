program exercise_3
   use Environment
   
   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0 , N = 0, M = 0
   real(R_)                :: T = 0
   real(R_), allocatable   :: B(:,:) 

   open (file=input_file, newunit=In)
      read (In, *) N, M
      allocate (B(N,M))
   close (In)
   
   CALL RANDOM_NUMBER(B)
   B = B * 100
   T = sum(B)
 
   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, *)
      write (Out,fmt="(a10,i2,a3,i2)") "Matrica A ", N, " x ", M
      write (Out, *)
      write (Out, "('Tz A = ', f0.2)") T
   close (Out)
end program exercise_3
