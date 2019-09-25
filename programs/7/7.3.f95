program pr7
  implicit none

  100 FORMAT (a)
  200 FORMAT (a, $)

  INTEGER :: i, j, n, offset, val = 1
  INTEGER, allocatable :: arr(:, :)
  REAL :: mid, skip
  
  print 200, 'Введите размер квадратной матрицы (от 3 до 10): '
  read *, n
  
  if(n < 3 .or. n > 10) then
    print 100, 'Неверный размер матрицы'
    stop
  end if
  
  allocate(arr(n, n))
  arr = 0

  mid = ceiling(n / 2.0)
  if(mod(n, 2) == 0) then
    skip = mid - 0.5 
  else
    skip = mid - 1.0
  end if

  do i = lbound(arr, 1), ubound(arr, 1)
    offset = int(mid - int(abs(skip)))

    if(i == mid) then
      val = val + 1
    end if

    do j = lbound(arr, 2) + offset, ubound(arr, 2) - offset
      arr(i, j) = val
    end do
    skip = skip - 1.0
  end do
  
  if(mod(n, 2) == 0) then
    skip = mid - 0.5 
  else
    skip = mid - 1.0
  end if

  val = val + 1
  do j = lbound(arr, 2), ubound(arr, 2)
    offset = int(mid - int(abs(skip)))

    if(j == mid) then
      val = val + 1
    end if

    do i = lbound(arr, 1) + offset, ubound(arr, 1) - offset
      arr(i, j) = val
    end do
    skip = skip - 1.0
  end do

  do i = lbound(arr, 1), ubound(arr, 1)
    print '(10i2)', arr(i, :)
  end do
  
end program pr7