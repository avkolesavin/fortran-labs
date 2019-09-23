program pr7
  implicit none

  100 FORMAT (a)
  200 FORMAT (a, $)

  INTEGER :: n, i, j, val = 1, deep
  INTEGER, allocatable :: arr(:, :)

  print 200, 'Введите размер квадратной матрицы (от 3 до 10): '
  read *, n

  if(n < 3 .or. n > 10) then
    print 100, 'Неверный размер матрицы'
    stop
  end if

  allocate(arr(n, n))
  arr = 0

  do deep = 0, ceiling(n / 2.0)
    do j = lbound(arr, 1) + deep, ubound(arr, 1) - deep
      arr(lbound(arr, 1) + deep, j) = val
      val = val + 1
    end do

    do i = lbound(arr, 1) + 1 + deep, ubound(arr, 1) - 1 - deep
      arr(i, ubound(arr, 1) - deep) = val
      val = val + 1
    end do
    
    do j = ubound(arr, 1) - deep, lbound(arr, 1) + deep, -1
      arr(ubound(arr, 1) - deep, j) = val
      val = val + 1
    end do

    do i = ubound(arr, 1) - 1 - deep, lbound(arr, 1) + 1 + deep, -1
      arr(i, lbound(arr, 1) + deep) = val
      val = val + 1
    end do

  end do

  print 100, 'Сгенерированная матрица:'
  do i = lbound(arr, 1), ubound(arr, 1)
    print '(10(i3, 1x))', arr(i, :)
  end do
  
end program pr7