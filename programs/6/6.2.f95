program pr6
  implicit none

  100 FORMAT (a)
  200 FORMAT (a, $)
  300 FORMAT (a, i0, a, i0)

  INTEGER, allocatable :: arr(:)
  INTEGER :: n, i, &
            minInd, minVal, maxInd, maxVal

  REAL :: rnd

  print 200, 'Введите количество элементов массива (минимум 1): '
  read *, n

  if(n < 2) then
    print 100, 'Значение должно быть больше!'
    stop
  end if

  if(n > 1000) then
    print 100, 'Не так много пожалуйста'
    stop
  end if

  print '(a, i0, a)', 'Будет создан массив из ', n, ' элементов!'

  allocate(arr(n))

  do i = lbound(arr, 1), ubound(arr, 1)
    call random_number(rnd)
    arr(i) = int(rnd * 100)
  end do

  minInd = lbound(arr, 1)
  minVal = arr(lbound(arr, 1))
  maxInd = lbound(arr, 1)
  maxVal = arr(lbound(arr, 1))

  do i = lbound(arr, 1), ubound(arr, 1)
    if(maxVal < arr(i)) then
      maxVal = arr(i)
      maxInd = i
    end if

    if(minVal > arr(i)) then
      minVal = arr(i)
      minInd = i
    end if
  end do

  deallocate(arr)

  print 100, repeat('-', 50)
  print '(a, 1000i3)', 'Сгенерированный массив: ', arr
  print 100, repeat('-', 50)
  print 300, 'Минимальный элемент ', minInd, '-й, со значением ', minVal
  print 300, 'Максимальный элемент ', maxInd, '-й, со значением ', maxVal
  print '(a, i0)', 'Элементов между максимальным и минимальным: ', abs(minInd - maxInd) - 1
  
end program pr6