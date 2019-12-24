
program pr10
  implicit none

  type ResultRow
    real :: x
    real :: y
  end type ResultRow

  type(ResultRow), allocatable :: table(:)
  integer, parameter :: n = 6
  real :: xStart = 0, xEnd = 1.0, result
  real :: h 
  
  integer :: i
  real :: xTmp
  
  h = (xEnd - xStart) / (real(n) - 1)
  
  allocate(table(0:n-1))

  do i = lbound(table, 1), ubound(table, 1)
    xTmp = real(xStart + h * (i))

    table(i)%x = xTmp
    table(i)%y = f(xTmp)
  end do
  
  print *, 'Результат функции:'
  print '(f8.5)', f(0.75)
  print *, 'Результат интерполяции:'
  print '(f8.5)', P(table, 0.75, 3, h)

  contains
  !-------------------------------------
  real function P(t, x, n, h)
    type(ResultRow), dimension(0:) :: t
    real :: x, result, h, mult
    integer :: index, i, j, n
    index = 0

    ! Перебираем таблицу чтобы найти в каком промежутке x
    do i = lbound(t, 1), ubound(t, 1) - n + 1
      if(x >= t(i)%x .and. x < t(i + 1)%x) then
        index = i
      end if 
    end do

    result = calcA(t, 0, index, h)

    if(n > 0)then
      do i = 1, n
        mult = 1

        do j = index, (i + index - 1)
          mult = mult * (x - t(j)%x)
        end do

        result = result + calcA(t, i, index, h) * mult
      end do
    end if

    P = result
  end function P

  real function calcA(t, n, i, h)
  implicit none

  type(ResultRow), dimension(0:) :: t
  integer, intent(in) :: n, i
  real :: h

  if(n == 0) then
    calcA = t(i)%y
    return
  end if

  calcA = deltaY(t, n, i) / (fact(n) * h**n)
  end function calcA

  recursive real function deltaY(t, p, i) result (r) ! +
    implicit none

    type(ResultRow), dimension(0:) :: t
    integer, intent(in) :: i, p

    if(size(t) < p + i) then
      print *, "i value out of array range"
      stop
    end if

    if(p == 0)then
      r = t(i)%y
      return
    end if

    if(p == 1) then
      r = t(i + 1)%y - t(i)%y
      return
    end if

    r = deltaY(t, p - 1, i + 1) - deltaY(t, p - 1, i)
  
  end function deltaY

  recursive integer function fact(n) result (r) ! +
  implicit none
  
  integer n

  if(n == 1) then
    r = 1
    return
  end if

  r = n * fact(n - 1)
  end function fact

real function f(x)
  implicit none
  real :: x

  f = sin(x)
end function f
  
end program pr10



