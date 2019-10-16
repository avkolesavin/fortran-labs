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

  print '(a, i0, a, i0, a)', "Прямоугольник со сторонами a=", a_, " и b=", b_, ' содержит'

  do
    if (a > b) then
      m = mod(a, b)
      squares = squares + a / b
      print '(i0, " квадратов со сторонами ", i0, "х", i0)',  a / b, b, b
      a = b
      b = m
    else if (a < b) then
      m = mod(b, a)
      squares = squares + b / a
      print '(i0, " квадратов со сторонами ", i0, "х", i0)', b / a, a, a

      b = m
    end if

    if (m == 0) then
      exit
    end if
  end do

  print '(a, i0, a)', 'содержит ', squares, ' квадратов'
  
end program pr5