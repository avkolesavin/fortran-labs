program pr9
  implicit none

  REAL :: f, mid, xMid, e, a = 1, b = 4, &
        delta = 0.0001, deltaSpeed = 0.0001
  CHARACTER(*), parameter :: func = '(x - 1)^2 - 2', int = '(1, 4)'
  ! CHARACTER(*), parameter :: func = '-cos(πx/2) + 2x', int = '(0, 1)'

  if(f(a) == 0) then
    call printAnswer(func, int, a)
  end if

  if(f(b) == 0) then
    call printAnswer(func, int, b)
  end if

  do
    xMid = mid(a, b)

    if(f(xMid) == 0) then
      call printAnswer(func, int, mid(a, b))
    end if

    if(f(a) * f(xMid) < 0) then
      b = xMid
    else
      a = xMid
    end if

    if(abs(b - a) < delta) then
      e = mid(a, b)
      if(f(e) < e) then
        call printAnswer(func, int, e)
      else
        delta = delta - deltaSpeed
      end if
    end if
  end do

end program pr9

function f(x)
  implicit none

  REAL :: f, x
  REAL, parameter :: PI = atan(1.0) * 4
  f = (x - 1) ** 2 - 2
  ! f = -cos(PI * x / 2) + 2 * x
end function f

function mid(a, b)
  implicit none
  REAL :: mid, a, b
  mid = (a + b) / 2
end function mid

subroutine printAnswer(func, int, r)
  CHARACTER(*), intent(in) :: func, int
  REAL, intent(in) :: r
  print '("Корень функции ", a, " на интервале ", a, " = ", f6.4)', func, int, r
  stop
end subroutine printAnswer