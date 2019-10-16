program pr9
  implicit none

  REAL :: a = 1.0, b = 4.0, &
          e = 0, d = 1.0e-5, ds = 1.0e-7, &
          xn, xn_
  INTEGER :: count = 0
  REAL :: f
  CHARACTER(*), parameter :: func = '(x - 1)ˆ2 - 2'

  if(f(a) == 0) then
    call printAnswer(func, a, b, a, 1)
  end if

  if(f(b) == 0) then
    call printAnswer(func, a, b, b, 1)
  end if

  xn = a
  
  do 
    xn_ = xn
    xn = xn_ - (f(xn_) / (f(b) - f(xn_))) * (b - xn_)

    count = count + 1

    if((abs(xn_ - xn) < d)) then
      call printAnswer(func, a, b, xn, count)
    else
      d = d - ds
    end if

  end do
  
end program pr9

function f(x)
  REAL :: f, x

  f = (x - 1)**2 - 2
end function f

subroutine printAnswer(func, a, b, r, count)
  CHARACTER(*), intent(in) :: func
  REAL, intent(in) :: r, a, b
  INTEGER :: count
  print '("Корень функции ", a, " на интервале [", f5.2, ", " f5.2, "] = ", f6.4)', &
        func, a, b, r
  print '("Поиск корня использовал ", i0, " повторений")', count
  stop
end subroutine printAnswer