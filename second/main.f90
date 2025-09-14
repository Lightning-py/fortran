program second
    implicit none

    real :: ax, ay, bx, by, cx, cy, dx, dy, center_x, center_y, temp_x, temp_y

    read *, ax, ay, bx, by

    if (ax == cx .and. ay == cy) then 
        write (*, *) "Точки совпадают, не могу вычислить"
    else
        center_x = (ax + bx) / 2
        center_y = (ay + by) / 2

        temp_x = ax - center_x
        temp_y = ay - center_y

        bx = center_x - temp_y
        by = center_y + temp_x

        dx = center_x + temp_y
        dy = center_y - temp_x

        write (*, *) "(", bx, "; ", by, "), (", dx, "; ", dy, ")" 
    end if

end program second