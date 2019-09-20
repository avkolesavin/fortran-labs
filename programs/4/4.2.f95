program pr4
  implicit none

  100 FORMAT (a, $)
  200 FORMAT (a)

  REAL :: rN
  INTEGER :: iN

  print 100, 'Нажмите Enter чтобы запустить лотерею '
  read *

  call sleep(1)
  print 100, 'Достаем ваш номер'

  print 100, '.'
  call sleep(1)
  print 100, '.'
  call sleep(1)
  print 200, '.'
  call sleep(1)

  call random_number(rN)
  rN = rN * 12 + 1
  iN = int(rN)

  ! print
  print '(a, i0)', 'Вам выпал номер ', iN
  call sleep(1)

  print 100, 'Вы выиграли'
  print 100, '.'
  call sleep(1)
  print 100, '.'
  call sleep(1)
  print 200, '.'
  call sleep(1)

  select case (iN)
  case (1)
    
    print 200, 'КВАРТИРУ В МОСКВЕ!'
  case (2)
    ! print 100, 'Вы выиграли... '
    ! call sleep(2)
    print 200, 'ААААВТОМОБИЛЬ!'
  case (3)
    ! print 100, 'Вы выиграли... '
    ! call sleep(2)
    print 200, 'ТУРИСТИЧЕСКУЮ ПУТЕВКУ!'
  case (4)
    ! print 100, 'Вы выиграли... '
    ! call sleep(2)
    print 200, 'ЗУБНУЮ ЩЕТКУ!'
  case default
    ! print 200, 'Вы выиграли... '
    ! call sleep(2)
    print 200, 'К сожалению вы ничего не выиграли'
  end select 
  
end program pr4