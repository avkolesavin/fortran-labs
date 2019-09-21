program pr6
  implicit none

  100 FORMAT (a)
  200 FORMAT (a, i0)

  INTEGER, allocatable :: arr(:)
  INTEGER :: n, i, posCount = 0, negCount = 0

  REAL :: rnd

  print '(a, $)', 'Введите количество элементов массива (минимум 1): '
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
    arr(i) = int(rnd * 200 - 100)
    
    if(arr(i) < 0) then
      negCount = negCount + 1
    else
      posCount = posCount + 1
    end if
  end do

  print 100, repeat('-', 50)
  print '(a, 1000i4)', 'Сгенерированный массив: ', arr
  print 100, repeat('-', 50)

  print 200, 'Количество положительных элементов в массиве: ', posCount
  print 200, 'Количество отрицательных элементов в массиве: ', negCount
  
end program pr6