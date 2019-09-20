program pr4
  implicit none

  100 FORMAT ("Человеку ", i0, 1x, a)

  INTEGER :: age, m

  print '(a, $)', 'Введите возраст человека от 1 до 100: '
  read *, age

  if(age < 1 .or. age > 100) then
    print '(a)', 'Вы ввели неверный возраст!'
    stop
  end if

  m = mod(age, 10)

  if(age>=10 .and. age <= 20) then
    m = 0
  end if

  select case (m)
  case(1)
    print 100, age, 'год'
  case(2:4)
    print 100, age, 'года'
  case(5:10, 0)
    print 100, age, 'лет'
  case default
    print '(a)', 'Что-то пошло нетак...'
  end select
  
end program pr4