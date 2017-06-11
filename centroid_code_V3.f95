    Program Centroid

      implicit none
      integer max_roi
      parameter (max_roi=10000)

    real x(max_roi), y(max_roi)
!First file=1, Second file=2
    real sumX1, sumY1, Center_X1, Center_Y1
    real sumX2, sumY2, Center_X2, Center_Y2
    integer i, total_points1, total_points2, ios

!First File
    open(unit=10,file='0007-0759auto.txt')
        do i=1,max_roi
                read(10,*,end=1000)x(i),y(i)
        enddo
1000    continue
        close (unit=10)

    total_points1 = i - 1

    sumX1 = 0.0
    sumY1 = 0.0
    do i=1,total_points1
        sumX1 = sumX1 + x(i)
        sumY1 = sumY1 + y(i)
    enddo

    Center_X1 = sumX1/total_points1
    Center_Y1 = sumY1/total_points1

    print*, Center_X1, Center_Y1

!Second File
    open(unit=20,file='0007-0759.txt')
        do i=1,max_roi
            read(20,*,end=2000)x(i),y(i)
        enddo
2000    continue
        close (unit=20)

    total_points2 = i - 1

    sumX2 = 0.0
    sumY2 = 0.0
    do i=1,total_points2
        sumX2 = sumX2 + x(i)
        sumY2 = sumY2 + y(i)
    enddo

    Center_X2 = sumX2/total_points2
    Center_Y2 = sumY2/total_points2

    print*, Center_X2, Center_Y2

    end program Centroid