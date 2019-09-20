program pr3
  implicit none

  100 FORMAT (a, 1x, f0.5)

  REAL :: a, b, c

  print '(a)', 'Введите 3 числа:'
  read *, a, b, c

  if (a > b)  then
    if (a > c) then
      print 100, 'Наибольшее число:', a
    else
      print 100, 'Наибольшее число:', c
    endif
  else if(b > c) then
      print 100, 'Наибольшее число:', b
    else
      print 100, 'Наибольшее число:', c
  endif

  !print *, max(a, b, c)

  print '(a)', 'Конец'
  
end program pr3