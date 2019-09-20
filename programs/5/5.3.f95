program pr5
  implicit none

  100 FORMAT (a, $)

  INTEGER :: a, b, a_, b_, m, squares = 0

  print 100, 'Введите размер стороны a: '
  read *, a
  print 100, 'Введите размер стороны b: '
  read *, b

  a_ = a
  b_ = b

  do
    if (a > b) then
      m = mod(a, b)
      squares = squares + a / b
      a = b
      b = m
    else if (a < b) then
      m = mod(b, a)
      squares = squares + b / a
      b = m
    end if

    if (m == 0) then
      exit
    end if
  end do

  print '(a, i0, a, i0)', "Прямоугольник со сторонами a=", a_, " и b=", b_
  print '(a, i0, a)', 'содержит ', squares, ' квадратов'
  
end program pr5