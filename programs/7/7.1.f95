program pr7
  implicit none

  100 FORMAT (a)
  200 FORMAT (a, " по модулю элемент имеет координаты (", i0, ",", i0, ")")

  type MatrixElement
    INTEGER x, y
  end type MatrixElement

  INTEGER :: i, j
  REAL :: arr(5,7), rnd
  type(MatrixElement) :: minInd, maxInd

  minInd%x = lbound(arr, 2)
  minInd%y = lbound(arr, 1)
  maxInd%x = lbound(arr, 2)
  maxInd%y = lbound(arr, 1)

  do i = lbound(arr, 1), ubound(arr, 1)
    do j = lbound(arr, 2), ubound(arr, 2)
      call random_number(rnd)
      arr(i, j) = rnd * 2 - 1

      if (abs(arr(i, j)) > abs(arr(maxInd%y, maxInd%x))) then
        maxInd%y = i
        maxInd%x = j
      end if

      if (abs(arr(i, j)) < abs(arr(minInd%y, minInd%x))) then
        minInd%y = i
        minInd%x = j
      end if
    end do
  end do

  print 100, 'Сгенерированная матрица:'
  do i = lbound(arr, 1), ubound(arr, 1)
    print '(7f14.10)', arr(i, :)
  end do

  print 100, repeat('-', 30)
  print 200, 'Наибольший', maxInd%y, maxInd%x
  print 200, 'Наименьший', minInd%y, minInd%x
  
end program pr7