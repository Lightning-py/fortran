program seventh
    implicit none

    integer, parameter :: dp = kind(1.0d0)

    real(dp) :: xstart, xend, xstep, ystart, yend, ystep, x, y, eps

    eps = 0.001

    xstart = -1.0
    xend = 1.0
    xstep = 0.1

    ystart = -1.25
    yend = 0.25
    ystep = 0.05

    write (*, '(A30)', advance='no') "x/y"
    do y = ystart, yend + ystep, ystep
        write(*, '(4F30.5)', advance='no') y
    end do
    write (*, *)

    do x = xstart, xend + xstep, xstep
        write (*, '(8F30.5)', advance='no') x 
        do y = ystart, yend + ystep, ystep
            if ((abs(x) < eps) .or. (abs(y) < eps) .or. (x * y <= 0)) then
                write (*, '(A30)', advance='no') "None"
            else
                write (*, '(4F30.5)', advance='no') sinh(2 * x / y) * cos(y / x) - log10(x * y)
            end if
        end do
        write (*, *)
    end do




end program seventh