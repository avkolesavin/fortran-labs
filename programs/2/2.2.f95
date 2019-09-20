program pr2
    implicit none

    100 FORMAT (a, i0)

    INTEGER :: x, res1, res2, res3

    print '(a)', 'Введите значение X:'
    read *, x
    
    ! res1 = 2^x4 - 3x^3 + 4x^2 - 5x + 6
    res1 = x * (-5 + x * (4 + x * (2 * x - 3))) + 6
    print 100, '1) ', res1
    
    ! res2 = 1 - 2x + 3x^2 - 4x^3
    res2 = 1 + x * (-2 + 3 * x - 4 * x * x)
    print 100, '2) ', res2
    
    ! res3 = 1 + 2x + 3x^2 + 4x^3
    res3 = 1 + x * (2 + 3 * x + 4 * x * x)
    print 100, '3) ', res3
end program pr2