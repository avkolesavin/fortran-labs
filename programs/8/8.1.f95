program pr8
  implicit none

  100 FORMAT (a)
  200 FORMAT (a, $)
  300 FORMAT (a, ' элемент ', i0, '-й со значением ', f12.10)
  400 FORMAT (10f7.4)
  500 FORMAT ('Алгоритм ', a, ' использует ' i0, ' перестановок')

  REAL, allocatable :: arr(:), arr_(:)
  REAL :: rnd, tmp, minVal, maxVal
  INTEGER :: i, j, n, minInd = 0, maxInd = 0, bubbleOps = 0, insertOps = 0
  LOGICAL :: sorted = .false.

  print 200, 'Введите размер массива от 5 до 100 элементов: '
  read *, n

  if(n < 5 .or. n > 100) then
    print 100, 'Введен неверный размер массива!'
    stop
  end if

  print 100

  allocate(arr(n))

  do i = lbound(arr, 1), ubound(arr, 1)
    call random_number(rnd)
    arr(i) = rnd

    if (i == lbound(arr,1)) then
      minVal = rnd
      minVal = rnd
      cycle
    end if

    if(rnd > maxVal) then
      maxVal = rnd
      maxInd = i
    end if

    if(arr(i) < minVal) then
      minVal = rnd
      minInd = i
    end if
  end do

  print '(a, i0, a)', 'Сгенерированный массив из ', n, ' элементов:' 
  print 100, repeat('-', 40)
  print 400, arr
  print 100, repeat('-', 40)
  print 300, 'Максимальный', maxInd, maxVal
  print 300, 'Минимальный', minInd, minVal

  print 100

  !Сортировка

  arr_ = arr
  !Пузырьком
  print 100, 'Сортировка пузырьком: '
  do while (.not.sorted)
    sorted = .true.

    do i = lbound(arr_, 1), ubound(arr_, 1) - 1
      if(arr_(i) > arr_(i + 1)) then
        tmp = arr_(i)
        arr_(i) = arr_(i + 1)
        arr_(i + 1) = tmp

        bubbleOps = bubbleOps + 1

        sorted = .false.
      end if
    end do
  end do

  print 100, 'Отсортрованный массив по возрастанию (пузырьком):'
  print 100, repeat('-', 40)
  print 400, arr_

  print 100
  
  arr_ = arr
  ! Вставками
  print 100, 'Сортровка вставками: '
  do i = lbound(arr_, 1), ubound(arr_, 1)
    maxInd = i
    do j = i, ubound(arr_, 1)
      if (arr_(j) < arr_(maxInd)) then
        maxInd = j
      end if
    end do
    
    tmp = arr_(i)
    arr_(i) = arr_(maxInd)
    arr_(maxInd) = tmp

    insertOps = insertOps + 1
  end do

  print 100, 'Отсортрованный массив по врзрастанию (вставками):'
  
  print 100, repeat('-', 40)
  print 400, arr_

  print 100
  print 500, 'пузырком', bubbleOps
  print 500, 'вставками', insertOps
  
end program pr8