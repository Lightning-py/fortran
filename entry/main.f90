program hello_world
    implicit none

    integer, parameter :: sp = selected_real_kind(6, 37)

    real(sp) :: x_i, a, h, sum
    integer :: n, i

    a = -6.6_sp
    h = 0.33_sp
    n = 48

    sum = 0

    x_i = a + 10 * h
    do i = 10, n
        sum = sum + f(x_i)
        print *, i, x_i, f(x_i), sum
        x_i = x_i + h
    end do

    print *, "Sum", sum

contains 
    function f(x) result(value)
        integer, parameter :: sp = selected_real_kind(6, 37)

        real(sp) :: x, value
        real(sp), parameter :: pi = 3.141592653589793_sp


        if (abs(x) < 1e-5) then
            value = 6.166177_sp
        else if (x < 0.0_sp) then
            value = (2.0_sp / 3.0_sp) ** (n / 7.0_sp) * -abs(x - 2_sp) ** (1.0_sp / 5.0_sp) * cos(x * pi / 6.0_sp)
        else
            if (x >= 1.0_sp) then
                value = sin(x * pi / 3.0_sp)
            else
                value = sin(x * pi / 3.0_sp)
            end if
        end if

    end function f


end program hello_world