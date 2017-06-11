    Program Centroid

      implicit none
      integer max_roi
      parameter (max_roi=10000)

    real x(max_roi), y(max_roi), sumX, sumY, Center_X, Center_Y
    integer i, total_points, ios

      open(unit=10,file='0007-0759auto.txt')
            do i=1,max_roi
                  read(10,*,end=1000)x(i),y(i)
            enddo
1000  continue
      close (unit=10)

      total_points = i - 1

      sumX = 0.0
      sumY = 0.0
      do i=1,total_points
        sumX = sumX + x(i)
        sumY = sumY + y(i)
      enddo

    Center_X = sumX/total_points
    Center_Y = sumY/total_points

    print*, Center_X, Center_Y

    end program Centroid