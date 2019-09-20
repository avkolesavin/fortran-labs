program pr2
    implicit none

    INTEGER :: sum_res, sub_res, mult_res
    REAL :: a, b, div_res
    100 FORMAT (f0.1, 1x, a, 1x, f0.1, " = ", i0)

    print '(a)', 'Введите число a: '
    read *, a
    print '(a)', 'Введите число b: '
    read *, b

    sum_res = int(a + b)
    sub_res = int(a - b)
    mult_res = int(a * b)
    div_res = a / b
    
    print '(/, a, /, a)', 'Результат:', repeat('-', 20)
    print 100, a, '+', b, sum_res
    print 100, a, '-', b, sub_res
    print 100, a, '*', b, mult_res
    print '(f0.1, 1x, a, 1x, f0.1, " = ", f0.10)', a, '/', b, div_res
end program pr2