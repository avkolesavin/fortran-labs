program pr3
  implicit none

  REAL :: a, b, c, max, min, mid

  print '(a)', 'Введите 3 числа:'
  read *, a, b, c

  if (a > b)  then
    if (a > c) then
      max = a
      if(b > c) then
        mid = b
        min = c
      else
        mid = c
        min = b
      endif
    else
      max = c
      mid = a
      min = b
    endif
  else if(b > c) then
      max = b
      if (c > a) then
        mid = c
        min = a
      else
        mid = a 
        min = c
      endif
    else
      max = c
      mid = b
      min = a
  endif
  
  100 FORMAT (a, f0.5)

  print 100, 'Максимальное значение: ', max
  print 100, 'Среднее значение: ', mid
  print 100, 'Минимальное значение: ', min

  print '(a)', 'Конец'
  
end program pr3