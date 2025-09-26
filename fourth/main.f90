program fourth
    implicit none

    integer, parameter :: dp = kind(1.0d0)

    real(dp) :: pi
    real(dp) :: eps, counter, sum1, sum2, sum_second, sum_third
    logical :: flag

    integer*8 :: count

    eps = 0.1   

    counter = 1
    sum1 = 0
    sum2 = 0

    pi = 3.1415926535

    ! 4 * (1 - 1/3 + 1/5 - 1/7 + ...)

    flag = .FALSE.

    do while (abs(sum1 - sum2 - pi) > eps)
        if (flag) then
            ! value = value - 4.0 / counter
            sum2 = sum2 + 4.0 / counter
        else
            ! value = value + 4.0 / counter
            sum1 = sum1 + 4.0 / counter
        end if

        counter = counter + 2
        flag = .not. flag
    end do
        
    write (*, *) "sum = ", sum1 - sum2, " ; steps = ", int(counter) / 2


    ! 3 + 4 * (1 / (2 * 3 * 4) - 1 / (4 * 5 * 6) + ...)

    sum_second = 0
    counter = 2.0
    flag = .FALSE.


    do while (abs(3.0 + 4.0 * sum_second - pi) > eps)
        if (flag) then
            sum_second = sum_second - 1.0 / (counter * (counter + 1) * (counter + 2))
        else
            sum_second = sum_second + 1.0 / (counter * (counter + 1) * (counter + 2))
        end if

        counter = counter + 2
        flag = .not. flag

    end do
    
    write (*, *) "sum = ", 3.0 + 4.0 * sum_second, " ; steps = ", int(counter) / 2 - 1

    ! sqrt(6 * (1 + 1/2**2 + 1/3**2 + 1/4**2 + ...))

    sum_third = 0.0
    count = 1

    do while (abs( sqrt(sum_third * 6.0) - pi) > eps )
        sum_third = sum_third + 1 / real(count * count)
        count = count + 1
    end do
    
    write (*, *) "sum = ", sqrt(6 * sum_third), " ; steps = ", int(count) - 1

end program fourth