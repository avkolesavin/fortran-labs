program pr7
  implicit none

  100 FORMAT (a)

  type MatrixElement
    INTEGER x, y
  end type MatrixElement

  INTEGER :: arr(10, 10), i, j, k, zeroesCount = 0
  REAL :: rnd
  type(MatrixElement), allocatable :: zeroes(:), tmp(:)

  do i = lbound(arr, 1), ubound(arr, 1)
    do j = lbound(arr, 2), ubound(arr, 2)
      call random_number(rnd)
      arr(i, j) = int(rnd * 12 - 6)

      if(arr(i,j) == 0) then
        zeroesCount = zeroesCount + 1

        if(allocated(zeroes)) then
          allocate(tmp(ubound(zeroes, 1)))
          do k = lbound(tmp, 1), ubound(tmp, 1)
            tmp(k) = zeroes(k)
          end do
          deallocate(zeroes)
        end if

        allocate(zeroes(zeroesCount))
        if (allocated(tmp)) then
          do k = lbound(tmp, 1), ubound(tmp, 1)
            zeroes(k) = tmp(k)
          end do
          deallocate(tmp)
        end if

        zeroes(zeroesCount)%y = i
        zeroes(zeroesCount)%x = j

      end if
    end do
  end do

  print 100, 'Сгенерированная матрица:'
  do i = lbound(arr, 1), ubound(arr, 1)
    print '(10(i2, 1x))', arr(i, :)
  end do

  print 100, repeat('-', 30)

  print 100, 'Нули находятся на позициях:'
  do i = lbound(zeroes, 1), ubound(zeroes, 1)
    print '(i0, ", ", i0)', zeroes(i)%y, zeroes(i)%x
  end do
  
end program pr7