program fith
    implicit none

    integer, dimension(10) :: A, B, C
    integer i, count_b, count_c

    A = [-1, 2, 3, 4, -5, 6, -7, -8, -9, 0]

    count_b = 1
    count_c = 1

    do i = 1, 10
        if (A(i) > 0) then
            B(count_b) = A(i)
            count_b = count_b + 1
        else if (A(i) < 0) then
            C(count_c) = A(i)
            count_c = count_c + 1
        end if
    end do

    write (*, *) "B", count_b - 1

    do i = 1, count_b + 1
        write (*, *) B(i)
    end do

    write (*, *) "C", count_c - 1

    do i = 1, count_c + 1
        write (*, *) C(i)
    end do


end program fith