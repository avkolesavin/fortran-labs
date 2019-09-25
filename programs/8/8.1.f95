program pr8
  implicit none

  100 FORMAT (a)
  200 FORMAT (a, $)
  300 FORMAT (a, ' элемент ', i0, '-й со значением ', f12.10)
  400 FORMAT (10f13.10)

  REAL, allocatable :: arr(:)
  REAL :: rnd, tmp, minVal, maxVal
  INTEGER :: i, j, n, minInd = 0, maxInd = 0
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

  !Пузырьком
  ! do while (.not.sorted)
  !   sorted = .true.

  !   do i = lbound(arr, 1), ubound(arr, 1) - 1
  !     if(arr(i) > arr(i + 1)) then
  !       tmp = arr(i)
  !       arr(i) = arr(i + 1)
  !       arr(i + 1) = tmp

  !       sorted = .false.
  !     end if
  !   end do
  ! end do

  ! print 100, 'Отсортрованный массив по возрастанию (пузырьком):'
  
  ! Вставками
  ! do i = lbound(arr, 1), ubound(arr, 1)
  !   maxInd = i
  !   do j = i, ubound(arr, 1)
  !     if (arr(j) > arr(maxInd)) then
  !       maxInd = j
  !     end if
  !   end do
    
  !   tmp = arr(i)
  !   arr(i) = arr(maxInd)
  !   arr(maxInd) = tmp
  ! end do

  ! print 100, 'Отсортрованный массив по убыванию (вставками):'
  print 100, repeat('-', 40)
  print 400, arr
  
end program pr8