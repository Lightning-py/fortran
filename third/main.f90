program third
    implicit none

    integer :: a, d1, d2, d3, d4

    read *, a;
    a = abs(a)

    d4 = mod(a, 10);
    d3 = mod(a / 10, 10);
    d2 = mod(a / 100, 10);
    d1 = mod(a / 1000, 10);

    if (d1 == 0 .or. (a - (d1 * 1000 + d2 * 100 + d3 * 10 + d4)) .ne. 0) then
        write (*, *) "число не четырехзначное"
        stop
    end if

    if (d1 + d2 == d3 + d4 .or. d1 + d3 == d2 + d4 .or. d1 + d4 == d2 + d3) then
        write (*, *) "можно составить сумму"
    else
        write (*, *) "сумму составить нельзя"
    end if

end program third