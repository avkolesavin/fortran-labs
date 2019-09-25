program pr8
  implicit none

  100 FORMAT (a)
  200 FORMAT (a, $)
  400 FORMAT (10f13.10)

  REAL, allocatable :: arr(:)
  REAL :: rnd
  INTEGER :: i, r, n, ranges(10)
  ranges = 0

  print 200, 'Введите размер массива от 5 до 5000 элементов: '
  read *, n

  if(n < 5 .or. n > 5000) then
    print 100, 'Введен неверный размер массива!'
    stop
  end if

  print 100

  allocate(arr(n))

  do i = lbound(arr, 1), ubound(arr, 1)
    call random_number(rnd)
    arr(i) = rnd

    r = int(arr(i) / 0.1 + 1)
    ranges(r) = ranges(r) + 1
  end do

  if (n <= 100) then
    print '(a, i0, a)', 'Сгенерированный массив из ', n, ' элементов:' 
    print 100, repeat('-', 40)
    print 400, arr
    print 100, repeat('-', 40)
  else
    print 100, 'Массив не отображается потому что он слишком большой'
    print 100
  end if

  do r = lbound(ranges, 1), ubound(ranges, 1)
      print '("[", f0.1, ", ", f0.1, "): ", i0)', r/10.0, r/10.0 + 0.1, ranges(r) 
    end do
  
end program pr8