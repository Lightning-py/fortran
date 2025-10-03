program seventh
    implicit none

    integer, parameter :: dp = kind(1.0d0)

    real(dp) :: xstart, xend, xstep, ystart, yend, ystep, x, y, eps, value
    integer :: in_field, fields, fields_printed, i, printed


    eps = 0.001

    xstart = -1.0
    xend = 1.0
    xstep = 0.1

    ystart = -1.25
    yend = 0.25
    ystep = 0.05

    in_field = 8
    fields = int(ceiling(((yend - ystart) / ystep) /  in_field))
    fields_printed = 0

    write (*, *) fields


    ystart = ystart - ystep

    do fields_printed = 0, fields
        write (*, '(A7, 2X)', advance='no') "x/y"

        printed = 0

        do y = ystart + (fields_printed * (in_field - 1) + 1) * ystep, ystart + (fields_printed + 1) * (in_field - 1) * ystep, &
            ystep
            printed = printed + 1
            write(*, '(A, F9.2, 1X)', advance='no') "|", y
            if (abs(y - yend) < 1e-5) exit
        end do

        write (*, "(A)") "|"

        do i = 0, printed
                write (*, "(A)", advance='no') "-----------"
        end do
        write (*, '(/)', advance='no')

        do x = xstart, xend + xstep, xstep
            write (*, '("|", F7.3, 1X)', advance='no') x 

            
            do y = ystart + (fields_printed * (in_field - 1) + 1) * ystep, ystart + (fields_printed + 1) * (in_field - 1) * ystep, &
                ystep
                
                if ((abs(x) < eps) .or. (abs(y) < eps) .or. (x * y <= 0)) then
                    write (*, '(A, 3X, A4, 3X)', advance='no') "|", "...."
                else
                    value = sinh(2 * x / y) * cos(y / x) - log10(x * y)
                    if (value > 1e5) then
                        write (*, '(A, E9.1, 1X)', advance='no') "|", value
                    else
                        write (*, '(A, F9.2, 1X)', advance='no') "|", value
                    end if
                end if
            
            
                if (abs(y - yend) < 1e-5) exit
            end do
        
            write (*, '(A, /)', advance='no') "|"

            do i = 0, printed
                write (*, "(A)", advance='no') "-----------"
            end do

            write (*, '(/)', advance='no')
        
        end do

        write (*, '(/, A, /)', advance='no') "Press Enter..."
        
        read (*, *)
    end do




end program seventh
