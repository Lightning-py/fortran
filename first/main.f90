program first
    implicit none

    integer :: m, n, k;
    real :: z, numerator, denominator;

    read *, m, n, k;

    if (n == 0 .or. m == 0 .or. k == 0) then 
        write (*, *) "как минимум одно из чисел равно нулю, не могу вычислить"
    else 
        numerator = (float(m) / float(n) - float(n) / float(k));
        denominator = (float(m) / float(n) + float(k) / float(m));

        if (abs(denominator) < 1e-5) then
            write (*, *) "Знаменатель равен нулю, не могу вычислить"
        else
            write (*, *) numerator / denominator
        end if
    end if


end program first