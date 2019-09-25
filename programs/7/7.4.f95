program pr7
  implicit none

  100 FORMAT (a)
  200 FORMAT (a, $)

  INTEGER :: i, j, n, jStart = 1, val = 1
  INTEGER, allocatable :: arr(:, :)
  
  print 200, 'Введите размер квадратной матрицы (от 3 до 10): '
  read *, n
  
  if(n < 3 .or. n > 10) then
    print 100, 'Неверный размер матрицы'
    stop
  end if
  
  allocate(arr(n, n))

  i = lbound(arr, 1)
  j = lbound(arr, 2)
  do while(i <= ubound(arr, 1) .and. j <= ubound(arr, 2))
      if (j < lbound(arr, 2)) then
          jStart = jStart + 1
          j = jStart
          i = lbound(arr, 1)
      end if

    arr(i, j) = val
    val = val + 1

    i = i + 1
    j = j - 1
    
    if(i > ubound(arr, 1)) then
      i = i - ubound(arr, 1) + j + 1
      j = ubound(arr, 2)
    end if

  end do

  do i = lbound(arr, 1), ubound(arr, 1)
    print '(10i4)', arr(i, :)
  end do
  
end program pr7