    Program Centroid

    implicit none
    real x, y, sumX, sumY
    integer i, total_points, ios

    open(unit=10,file='0007-0759auto.txt')

    do i=1,10000
        read(10,*,iostat=ios)x,y
        if (ios/=0) exit
        sumX = sumX + x
        sumY = sumY + y
        total_points = total_points + 1
    enddo

    sumX = sumX/total_points
    sumY = sumY/total_points

    print*, sumX, sumY
    close (10)

    end program Centroid