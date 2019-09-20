program pr5
  implicit none

  INTEGER :: val, posCount = 0

  print '(a)', 'Вводите числа, пока не введете отрицательное число:'
  read *, val

  do while (val >= 0)
    posCount = posCount + 1

    read *, val
  end do

  print '(a, i0, a)', 'Было введено ', posCount, ' положительных чисел'
  
end program pr5