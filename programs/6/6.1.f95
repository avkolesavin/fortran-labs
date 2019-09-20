program pr6
  implicit none

  INTEGER :: arr(10), i, sum = 0
  REAL :: rnd

  do i = 1, int(size(arr))
    call random_number(rnd)
    arr(i) = int(rnd * 10)
    sum = sum + arr(i)
  end do

  print '(a, 10i2.1)', 'Сгенерированный массив: ', arr
  print '(a, i0)', 'Сумма элементов массива: ', sum
  
end program pr6