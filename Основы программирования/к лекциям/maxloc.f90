integer  :: i = 0, &
            Index(1) = 0, &
            Index2(2) = 0,
            max_loc = 0
real     :: A(N) = [1. 4, 2, 6, 9, 6, 9], &
            B(N, N) = 0

Index = MaxLoc(A) ! Index = [5]
! Size(Index) == rank(A)

Index2 = MaxLoc(B) ! Index2 = [2, 4]
! Size(Index2) == rank(B)

max_loc = MaxLoc(A) ! max_loc = [5]
! Ошибка: rank(0) = rank(1)

max_loc = MaxLoc(A, dim=1) ! max_loc = 5

Index2(1) = MaxLoc(B, dim=1) ! Index2(1) = 2
Index2(2) = MaxLoc(B, dim=2) ! Index2(1) = 4


max_loc = MaxLoc(A, dim=1, back=.true.) ! max_loc = 7
