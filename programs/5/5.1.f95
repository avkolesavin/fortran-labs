program pr5
  implicit none

  100 FORMAT (a)
  200 FORMAT (a, $)

  INTEGER :: a, d, n, i, sum = 0

  print 100, 'Программа вычисляет сумму элементов последовательности:'
  print 100, 'a, a + d, a + 2d, ..., a + (n - 1)d'
  print 200, 'Введите значение для a: '
  read *, a
  print 200, 'Введите значение для d: '
  read *, d
  print 200, 'Введите значение для n: '
  read *, n

  print '(a, i0, a, $)', 'Сумма ', n, ' элементов последовательности = '

  do i = 1, n
    sum = sum + (a + (i - 1) * d)
  end do

  print '(i0)', sum
  
end program pr5