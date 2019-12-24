program pr10
  implicit none
  real, parameter :: PI = atan(1.0) * 4
  real :: rangeStart = 0.0, rangeEnd = 1.0, tableStep, &
    maxNoise, rnd, numerator, denominator, sum
  real, allocatable :: funcTable(:,:), avgTable(:, :), tmpTable(:, :), p(:)
  integer :: i, k, j, nodesAmount = 10

  tableStep = (rangeEnd - rangeStart) / (nodesAmount - 1)
  maxNoise = tableStep / 3

  allocate(funcTable(0:nodesAmount - 1, 0:1))
  allocate(avgTable(0:nodesAmount - 2, 0:1))
  allocate(tmpTable(0:nodesAmount - 2, 0:nodesAmount - 1))
  allocate(p(0:nodesAmount - 2))

  funcTable(:, 1) = 0
  funcTable(:, 1) = 0
  p(:) = 0
  
  do i = lbound(funcTable, 1), ubound(funcTable, 1)
    call random_number(rnd)

    if(i < ubound(funcTable, 1)) then
      funcTable(i, 0) = tableStep * i - maxNoise * rnd

      if(funcTable(i, 0) < 0) then
        funcTable(i, 0) = 0
      end if
    else
      funcTable(i, 0) = rangeEnd
    end if
    funcTable(i, 1) = f(funcTable(i, 0))
  end do 
  
  ! Заполнение таблицы серединных интервалов
  do k = lbound(avgTable, 1), ubound(avgTable, 1)
    avgTable(k, 0) = (funcTable(k, 0) + funcTable(k + 1, 0)) / 2
    avgTable(k, 1) = f(avgTable(k, 0))
  end do

  ! Заполнение вспомогательной таблицы (k, i)
  do k = lbound(tmpTable, 1), ubound(tmpTable, 1) ! 0:n-2
    do i = lbound(tmpTable, 2), ubound(tmpTable, 2) ! 0:n-1
      numerator = 1
      denominator = 1
      do j = lbound(funcTable, 1), ubound(funcTable, 1) ! 0:n-1
        if(.not.(i == j)) then
          numerator = numerator * (avgTable(k, 0) - funcTable(j, 0))
          denominator = denominator * (funcTable(i, 0) - funcTable(j, 0))
        end if
      end do
      tmpTable(k, i) = numerator / denominator
    end do
  end do

  do k = 0, nodesAmount - 2
    sum=0
    do i = 0, nodesAmount - 1
        sum = sum + funcTable(i, 1) * tmpTable(k, i)
    end do
    p(k) = sum
  end do    
  
  print *, 'Интерполяция по Лагранжу:'
  call printResults(avgTable, p)

  !--------
  contains
  real function f(x)
    real, intent(in) :: x

    f = sin(x)
  end function f
  
  subroutine printTable(funcTable)
    real, allocatable :: funcTable(:, :)

    print *, repeat('-', 20)
    do i = lbound(funcTable, 1), ubound(funcTable, 1)
      print '(2f9.6)', funcTable(i, :)
    end do
  end subroutine printTable

  subroutine printResults(originTable, resultVector)
    real, allocatable :: originTable(:, :), resultVector(:)

    print '(3x, a, 3(13xa))', 'x', 'y', 'y2', 'd'
    print '(2xa)', repeat('-', 55)
    do i = lbound(resultVector, 1), ubound(resultVector, 1)
      print '(2x, 4(f10.7, 2x"|"1x))', originTable(i, 0), originTable(i, 1), resultVector(i), originTable(i, 1) - resultVector(i) 
    end do
    print '(2xa)', repeat('-', 55)
  end subroutine printResults
end program pr10