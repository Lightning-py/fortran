program hello_world
    implicit none

    real :: x_i, a, h, sum
    integer :: n, i

    a = -6.6
    h = 0.33
    n = 48

    sum = 0

    x_i = a + 10 * h
    do i = 10, n
        sum = sum + f(x_i)
        print *, x_i, f(x_i), sum
        x_i = x_i + h
    end do

    print *, "Sum", sum

contains 
    function f(x) result(value)
        real :: x, value
        real, parameter :: pi = 3.141592653589793

        if (abs(x) < 1e-5) then
            value = 6.166177
        else if (x < 0) then
            value = (2.0 / 3.0) ** (n / 7.0) * -abs(x - 2) ** (1.0 / 5) * cos(x * 30.0 * pi / 180.0)
            ! value = cos(x * 30.0 * pi / 180.0)
            ! value = x * 30.0 * pi / 180.0
        else
            if (x >= 1) then
                ! value = (x - 1.0) ** (1.0 / 3.0) * sin(x * 60.0 * pi / 180.0)
                value = sin(x * 60.0 * pi / 180.0)
            else
                ! value = -abs(x - 1.0) ** (1.0 / 3.0) * sin(x * 60.0 * pi / 180.0)
                value = sin(x * 60.0 * pi / 180.0)
            end if
        end if

    end function f


end program hello_world