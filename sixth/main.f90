program sixth
    implicit none

    integer, parameter :: dp = kind(1.0d0)

    integer, allocatable :: arr(:, :)
    integer n, err, temp, i, j, sum, sum2
    logical flag, flag_2

    flag = .true.
    
    read (*, *) n

    allocate(arr(n, n), stat=err)
    if (err .ne. 0) stop 

    do i = 1, n
        do j = 1, n
            flag_2 = .false.
            do 
                if (flag_2) then
                    write (*, *) "введите заново"
                end if

                read (*, *) temp
                flag_2 = .true.
                if (temp > 0) exit
            end do 

            arr(i, j) = temp
        end do
    end do

    temp = n * (n + 1) / 2

    write (*, *) "матрица"
    do i = 1, n
        write (*, '(5I5)') (arr(i, j), j = 1, n)
    end do

    do i = 1, n
        sum = 0
        sum2 = 0
        do j = 1, n
            sum = sum + arr(i, j)
        end do

        do j = 1, n
            sum2 = sum2 + arr(j, i)
        end do


        if (sum .ne. temp .or. sum2 .ne. temp) then
            flag = .false.
            exit
        end if
    end do

    write (*, *) flag

    deallocate(arr)

end program sixth