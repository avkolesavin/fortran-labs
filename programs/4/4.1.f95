program pr4
  implicit none
  100 FORMAT (i0, "-й месяц - ", a)

  INTEGER :: month

  print '(a, $)', 'Введите номер месяца: '
  read *, month

  select case (month)
  case (1:2, 12)
    print 100, month, 'Зима'
  case (3:5)
    print 100, month, 'Весна'
  case (6:8)
    print 100, month, 'Лето'
  case (9:11)
    print 100, month, 'Осень'
  case default
    print '(a)', 'Нет такого месяца'
  end select
  
end program pr4