program pr2
    implicit none

    100 FORMAT (/, a, /, a)
    200 FORMAT (a, " x = ", f0.10)

    REAL, parameter :: PI = 4.0 * atan(1.0)
    REAL :: a = 4, b = 3, c = 2, d = 1

    print 100, 'Алгебраические выражения', repeat('-', 30)
    print 200, '1)', ((a + b) * (b + c) + d) * (a - b)
    print 200, '2)', ((a / b + 1) * (b / a - 1) - 1) / (a + b)
    print 200, '8)', (sqrt(a) + sqrt(b)) / a
    print 200, '9)', 1 + (b**2 + c**2 - a**2) / (2 * b * c)
    print 200, '10)', (a**2 - 1) / (sqrt(a) - sqrt(b))
    
    print 100, 'Тригонометрические выражения', repeat('-', 30)
    a = PI / 4
    print 200, '3)', cos(a) + sin(a) + cos(3 * a) + sin(3 * a)
    print 200, '4)', (sin(2 * a) + sin(5 * a) - sin(3 * a)) / (cos(a) + 1 - 2 * sin(2 * a)**2)

    a = PI / 2
    print 200, '5)', sin(a)**2 + cos(a)**2
    print 200, '6)', cos(3*PI/4 + 3 * a / 2) / sin(3*PI/4 + 3 * a / 2)

    a = PI
    print 200, '7)', (1 - tan(a)) / (1 + tan(a))
  
end program pr2