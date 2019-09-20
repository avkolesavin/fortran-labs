program pr3
  implicit none

  100 FORMAT (a, $)
  200 FORMAT (a)
  300 FORMAT ("Y = ", f0.5)

  REAL :: x

  print 100, 'Введите значение X в пределах -9 ≤ X < 3: '
  read *, x

  if(-9 <= x .and. x < -6) then
    print 200, 'X оказался в пределе: -9 ≤ x < -6'
    print 200, 'Y будет рассчитан по формуле: -3 + √(9 - (9 + x)ˆ2)'
    print 300, -3 + sqrt(9 - (9 + x)**2)
  else if (-6 <= x .and. x < -3) then
    print 200, 'X оказался в пределе: -6 ≤ x < -3'
    print 200, 'Y будет рассчитан по формуле: 3 + x'
    print 300, 3 + x
  else if (-3 <= x .and. x < 0) then
    print 200, 'X оказался в пределе: -3 ≤ x < 0'
    print 200, 'Y будет рассчитан по формуле: √((3+x)ˆ2)'
    print 300, sqrt((3 + x)**2)
  else if (0 <= x .and. x < 3) then
    print 200, 'X оказался в пределе: 0 ≤ x < 3'
    print 200, 'Y будет рассчитан по формуле: 3 - x'
    print 300, 3 - x
  else
    print 200, 'Недопустимое значение для X!'
  end if
  
end program pr3